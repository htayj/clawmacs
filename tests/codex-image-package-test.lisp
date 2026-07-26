(in-package :clawmacs/tests)

(in-suite media-package-suite)

(defmacro with-codex-image-function-override ((name lambda-list &body implementation)
                                               &body body)
  "Temporarily replace NAME with a deterministic adapter test seam."
  `(let ((original-function (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name) (lambda ,lambda-list ,@implementation))
            ,@body)
       (setf (symbol-function ',name) original-function))))

(defmacro with-codex-image-package-state (&body body)
  "Load the isolated bundled subscription adapter without live credentials."
  `(with-media-package-state
     (set-package-enablement-scope "codex-image" :global)
     (load-active-packages)
     (let ((clawmacs::*codex-auth-path*
             (merge-pathnames "auth.json" clawmacs::*tool-working-directory*)))
       ,@body)))

(defun codex-image-test-auth (&key (mode :chatgpt) (account-id "acct-test"))
  "Return an auth.json-compatible deterministic credential fixture."
  (clawmacs::openai-codex-auth-payload
   :auth-mode mode
   :openai-api-key (and (eq mode :api-key) "sk-not-for-images")
   :id-token "header.eyJjaGF0Z3B0X2FjY291bnRfaWQiOiJhY2N0In0.signature"
   :access-token "header.payload.signature"
   :refresh-token "refresh-token"
   :account-id account-id
   :last-refresh (clawmacs::current-rfc3339-timestamp)))

(defun codex-image-write-auth (auth)
  "Persist AUTH only into this test's temporary Codex auth store."
  (clawmacs::save-openai-codex-auth-json auth))

(defparameter *codex-image-success-json*
  "{\"data\":[{\"b64_json\":\"iVBORw0KGgo=\",\"revised_prompt\":\"revised\"}]}"
  "Tiny PNG-signature response fixture encoded as strict standard Base64.")

(defun codex-image-start-test-request (&key references)
  "Start one direct image adapter request with an optional reference list."
  (clawmacs::codex-image-start
   (clawmacs:make-media-generation-request :image "draw a stable test"
                                           :referenced-image-paths references)))

(test codex-image-package-registers-a-subscription-only-provider
  "The package owns a media provider and does not expose transport knobs."
  (with-codex-image-package-state
    (let ((provider (clawmacs:find-media-provider "codex-image")))
      (is-true provider)
      (is (equal '(:image) (clawmacs:media-provider-kinds provider)))
      (is (string= "codex-image" (clawmacs:media-default-provider)))
      (is (search "Codex subscription image generation"
                  (render-package-prompt-sections))))))

(test codex-image-rejects-api-and-static-token-paths-before-http
  "Only ChatGPT auth.json credentials can reach the image backend."
  (with-codex-image-package-state
    (let ((http-calls 0)
          (static-lookups 0))
      (codex-image-write-auth (codex-image-test-auth :mode :api-key))
      (with-codex-image-function-override (drakma:http-request (&rest _args)
                                             (declare (ignore _args))
                                             (incf http-calls)
                                             (error "HTTP must not be called"))
        (let ((outcome (codex-image-start-test-request)))
          (is (eq :failed (clawmacs:media-provider-outcome-status outcome)))
          (is (search "subscription auth" (clawmacs:media-provider-outcome-public-error outcome)
                      :test #'char-equal))
          (is (= 0 http-calls))))
      ;; A valid ChatGPT auth.json must win by construction: this narrow resolver
      ;; never consults the legacy static token source at all.
      (codex-image-write-auth (codex-image-test-auth))
      (with-codex-image-function-override (clawmacs::read-provider-file-token (&rest _args)
                                             (declare (ignore _args))
                                             (incf static-lookups)
                                             "static-api-key")
        (with-codex-image-function-override (drakma:http-request (url &rest args)
                                               (declare (ignore url args))
                                               (incf http-calls)
                                               (values *codex-image-success-json* 200 nil))
          (is (eq :succeeded
                  (clawmacs:media-provider-outcome-status
                   (codex-image-start-test-request))))))
      (is (= 0 static-lookups))
      (is (= 1 http-calls)))))

(test codex-image-requires-chatgpt-account-id-before-http
  "Malformed auth.json has a clear pre-network account-id failure."
  (with-codex-image-package-state
    (let ((http-calls 0))
      (codex-image-write-auth (codex-image-test-auth :account-id nil))
      ;; Remove the ID-token too, so the resolver cannot derive an account ID.
      (let ((auth (clawmacs::read-openai-codex-auth-json)))
        (setf (cdr (assoc :id--token (cdr (assoc :tokens auth)))) nil)
        (codex-image-write-auth auth))
      (with-codex-image-function-override (drakma:http-request (&rest _args)
                                             (declare (ignore _args))
                                             (incf http-calls)
                                             (error "HTTP must not be called"))
        (let ((outcome (codex-image-start-test-request)))
          (is (eq :failed (clawmacs:media-provider-outcome-status outcome)))
          (is (search "account ID" (clawmacs:media-provider-outcome-public-error outcome)
                      :test #'char-equal))))
      (is (= 0 http-calls)))))

(test codex-image-mirrors-generation-and-edit-transport-shapes
  "Generation and reference edits use their distinct Codex endpoints and bodies."
  (with-codex-image-package-state
    (codex-image-write-auth (codex-image-test-auth))
    (let ((seen nil)
          (reference (merge-pathnames "reference.png" clawmacs::*tool-working-directory*)))
      (with-open-file (stream reference :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede :if-does-not-exist :create)
        (write-sequence #(137 80 78 71) stream))
      (with-codex-image-function-override (drakma:http-request (url &rest args)
                                             (push (list url args) seen)
                                             (values *codex-image-success-json* 200 nil))
        (is (eq :succeeded (clawmacs:media-provider-outcome-status
                            (codex-image-start-test-request))))
        (is (eq :succeeded (clawmacs:media-provider-outcome-status
                            (codex-image-start-test-request
                             :references (list (namestring (truename reference))))))))
      (let* ((edit (first seen))
             (generation (second seen))
             (generation-args (second generation))
             (edit-args (second edit))
             (generation-body (getf generation-args :content))
             (edit-body (getf edit-args :content))
             (edit-json (cl-json:decode-json-from-string edit-body))
             (edit-images (clawmacs::artifactum-json-value edit-json "images"))
             (headers (getf generation-args :additional-headers)))
        (is (search "/images/generations" (first generation)))
        (is (search "/images/edits" (first edit)))
        (is (search "\"model\":\"gpt-image-2\"" generation-body))
        (is (search "\"background\":\"auto\"" generation-body))
        (is (search "\"quality\":\"auto\"" generation-body))
        (is (search "\"size\":\"auto\"" generation-body))
        (is (= 1 (length edit-images)))
        (is (string= "data:image/png;base64,iVBORw==" (elt edit-images 0)))
        (is (search "Bearer header.payload.signature"
                    (cdr (assoc "Authorization" headers :test #'string=))))
        (is (string= "codex_cli_rs"
                     (cdr (assoc "originator" headers :test #'string=))))
        (is (string= "acct-test"
                     (cdr (assoc "ChatGPT-Account-ID" headers :test #'string=))))))))

(test codex-image-refreshes-proactively-and-once-after-401
  "Stale auth refreshes before connect; a 401 performs exactly one refresh."
  (with-codex-image-package-state
    (codex-image-write-auth (codex-image-test-auth))
    (let ((refreshes 0) (calls 0))
      (with-codex-image-function-override (clawmacs::openai-codex-chatgpt-auth-stale-p (_auth)
                                             (declare (ignore _auth)) t)
        (with-codex-image-function-override (clawmacs::refresh-openai-codex-auth-json (&optional _auth)
                                               (declare (ignore _auth))
                                               (incf refreshes)
                                               (codex-image-test-auth))
          (with-codex-image-function-override (drakma:http-request (&rest _args)
                                                 (declare (ignore _args))
                                                 (incf calls)
                                                 (values *codex-image-success-json* 200 nil))
            (is (eq :succeeded (clawmacs:media-provider-outcome-status
                                (codex-image-start-test-request)))))))
      (is (= 1 refreshes))
      (is (= 1 calls)))
    (let ((refreshes 0) (calls 0))
      (with-codex-image-function-override (clawmacs::refresh-openai-codex-auth-json (&optional _auth)
                                             (declare (ignore _auth))
                                             (incf refreshes)
                                             (codex-image-test-auth))
        (with-codex-image-function-override (drakma:http-request (&rest _args)
                                               (declare (ignore _args))
                                               (incf calls)
                                               (if (= calls 1)
                                                   (values "unauthorized" 401 nil)
                                                   (values *codex-image-success-json* 200 nil)))
          (is (eq :succeeded (clawmacs:media-provider-outcome-status
                              (codex-image-start-test-request))))))
      (is (= 1 refreshes))
      (is (= 2 calls)))))

(test codex-image-reports-refresh-transient-and-cancellation-failures-safely
  "No raw response/token text escapes failed adapter outcomes."
  (with-codex-image-package-state
    (codex-image-write-auth (codex-image-test-auth))
    (with-codex-image-function-override (drakma:http-request (&rest _args)
                                           (declare (ignore _args))
                                           (values "not authorized" 401 nil))
      (with-codex-image-function-override (clawmacs::refresh-openai-codex-auth-json (&optional _auth)
                                             (declare (ignore _auth)) nil)
        (let ((outcome (codex-image-start-test-request)))
          (is (eq :failed (clawmacs:media-provider-outcome-status outcome)))
          (is (string= "Codex image auth refresh failed."
                       (clawmacs:media-provider-outcome-public-error outcome))))))
    (let ((calls 0)
          (clawmacs::*provider-http-max-retries* 1)
          (clawmacs::*provider-http-sleep-function* (lambda (_seconds)
                                                        (declare (ignore _seconds)) nil)))
      (with-codex-image-function-override (drakma:http-request (&rest _args)
                                             (declare (ignore _args))
                                             (incf calls)
                                             (values "temporary" 503 nil))
        (let ((outcome (codex-image-start-test-request)))
          (is (eq :failed (clawmacs:media-provider-outcome-status outcome)))
          (is (search "transient retries" (clawmacs:media-provider-outcome-public-error outcome)
                      :test #'char-equal))))
      (is (= 2 calls)))
    ;; A cancellation delivered through the published media operation stops a
    ;; transient retry before its backoff can issue a second connection.
    (let ((http-calls 0)
          (clawmacs::*provider-http-max-retries* 2)
          (clawmacs::*provider-http-sleep-function* (lambda (_seconds)
                                                        (declare (ignore _seconds)) nil))
          (request (clawmacs:make-media-generation-request :image "cancel retry"))
          (operation nil))
      (with-codex-image-function-override (drakma:http-request (&rest _args)
                                             (declare (ignore _args))
                                             (incf http-calls)
                                             (clawmacs:cancel-media-operation operation)
                                             (values "temporary" 503 nil))
        (progn
          (setf operation (clawmacs::begin-media-operation request))
          (clawmacs::run-media-operation operation)
          (is (eq :cancelled (clawmacs:media-operation-status operation)))))
      (is (= 1 http-calls)))
    ;; The media-level callback cancels a published pending operation before
    ;; its adapter can connect at all.
    (let ((http-calls 0)
          (request (clawmacs:make-media-generation-request :image "cancel pending")))
      (with-codex-image-function-override (drakma:http-request (&rest _args)
                                             (declare (ignore _args))
                                             (incf http-calls)
                                             (error "must not connect"))
        (let ((operation (clawmacs::begin-media-operation request)))
          (clawmacs:cancel-media-operation operation)
          (clawmacs::run-media-operation operation)
          (is (eq :cancelled (clawmacs:media-operation-status operation))))
      (is (= 0 http-calls))))))

(test codex-image-validates-response-base64-and-persists-safe-artifact-provenance
  "Malformed response data fails safely; success is persisted through Artifactum."
  (with-codex-image-package-state
    (codex-image-write-auth (codex-image-test-auth))
    (dolist (body '("{}"
                    "{\"data\":[{\"b64_json\":\"AAAA_A==\"}]}"
                    "{\"data\":[{\"b64_json\":\"abc\"}]}"))
      (with-codex-image-function-override (drakma:http-request (&rest _args)
                                             (declare (ignore _args))
                                             (values body 200 nil))
        (let ((outcome (codex-image-start-test-request)))
          (is (eq :failed (clawmacs:media-provider-outcome-status outcome)))
          (is (or (search "invalid" ; the exact body is never exposed
                         (clawmacs:media-provider-outcome-public-error outcome)
                         :test #'char-equal)
                  (search "did not contain"
                          (clawmacs:media-provider-outcome-public-error outcome)
                          :test #'char-equal))))))
    (let ((buffer (make-media-test-buffer "codex-image-artifact")))
      (with-codex-image-function-override (drakma:http-request (&rest _args)
                                             (declare (ignore _args))
                                             (values *codex-image-success-json* 200 nil))
        (let* ((request (clawmacs:make-media-generation-request :image "safe artifact"))
               (operation (clawmacs:start-media-operation request))
               (records (clawmacs:persist-media-operation-assets buffer operation))
               (record (first records)))
          (is (eq :succeeded (clawmacs:media-operation-status operation)))
          (is (probe-file (pathname (getf record :path))))
          (is (search "codex-image-request-" (getf record :name)))
          (is (string= "gpt-image-2"
                       (clawmacs::artifactum-json-value (getf record :metadata) "model")))
          (is (string= "codex-image"
                       (clawmacs::artifactum-json-value (getf record :metadata) "provider")))
          (is (string= (clawmacs:media-operation-id operation)
                       (clawmacs::artifactum-json-value (getf record :provenance)
                                                         "operation_id"))))))))
