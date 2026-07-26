(in-package :clawmacs)

;;; Codex subscription image adapter
;;;
;;; This adapter intentionally follows Codex CLI's internal ChatGPT backend
;;; route, rather than OpenAI's separately billed public Images API.  The
;;; route is not a public third-party API contract; see docs/MEDIA-GENERATION.md.

(defparameter +codex-image-model+ "gpt-image-2"
  "The image model selected by current Codex CLI image generation.")

(defparameter +codex-image-provider-id+ "codex-image"
  "Stable provider id for the bundled Codex subscription image adapter.")

(define-condition codex-image-error (error)
  ((category :initarg :category :reader codex-image-error-category)
   (message :initarg :message :reader codex-image-error-message))
  (:report (lambda (condition stream)
             (write-string (codex-image-error-message condition) stream))))

(defun codex-image-fail (category control &rest arguments)
  "Signal a public-safe adapter error in CATEGORY without provider response data."
  (error 'codex-image-error
         :category category
         :message (apply #'format nil control arguments)))

(defun codex-image-operation-for-request (request)
  "Return the live media operation owning REQUEST, when one has been published."
  (bt:with-lock-held (*media-provider-registry-lock*)
    (loop :for operation :being :the :hash-values :of *media-operation-registry*
          :when (eq request (media-operation-request operation))
            :return operation)))

(defun codex-image-request-cancelled-p (request)
  "Return true when REQUEST's media operation was cancelled by its tool owner."
  (let ((operation (codex-image-operation-for-request request)))
    (and operation
         (bt:with-lock-held (*media-provider-registry-lock*)
           (eq (media-operation-status operation) :cancelled)))))

(defun codex-image-cancelled-p (request)
  "Return true when REQUEST's published media operation was cancelled."
  (codex-image-request-cancelled-p request))

(defun codex-image-check-cancelled (request)
  "Signal a terminal public cancellation before opening or retrying a request."
  (when (codex-image-cancelled-p request)
    (codex-image-fail :cancelled "Codex image request was cancelled.")))

(defun codex-image-resolve-auth (&key (refresh-if-needed t))
  "Resolve only ChatGPT subscription credentials for image generation."
  (handler-case
      (resolve-openai-codex-chatgpt-image-auth :refresh-if-needed refresh-if-needed)
    (codex-image-error (condition) (error condition))
    (error (condition)
      (let ((message (princ-to-string condition)))
        (cond
          ((search "refresh failed" message :test #'char-equal)
           (codex-image-fail :refresh-failed "Codex image auth refresh failed."))
          ((search "account_id" message :test #'char-equal)
           (codex-image-fail :auth-unavailable
                             "Codex image auth is unavailable: ChatGPT account ID is missing."))
          ((or (search "API-key" message :test #'char-equal)
               (search "API key" message :test #'char-equal))
           (codex-image-fail :subscription-required
                             "Codex image generation requires ChatGPT subscription auth; API keys are not used."))
          (t
           (codex-image-fail :auth-unavailable
                             "Codex image auth is unavailable. Sign in to Codex with ChatGPT.")))))))

(defun codex-image-endpoint (auth suffix)
  "Return the version-sensitive Codex image endpoint named by SUFFIX."
  (format nil "~A/images/~A"
          (string-right-trim "/" (getf auth :base-url)) suffix))

(defun codex-image-standard-base64-char-value (character)
  "Return CHARACTER's standard RFC 4648 Base64 value, or NIL.

URL-safe `-' and `_' are intentionally rejected: image responses must contain
strict standard Base64 in `b64_json'."
  (cond
    ((and (char>= character #\A) (char<= character #\Z))
     (- (char-code character) (char-code #\A)))
    ((and (char>= character #\a) (char<= character #\z))
     (+ 26 (- (char-code character) (char-code #\a))))
    ((and (char>= character #\0) (char<= character #\9))
     (+ 52 (- (char-code character) (char-code #\0))))
    ((char= character #\+) 62)
    ((char= character #\/) 63)
    (t nil)))

(defun codex-image-standard-base64-decode (text)
  "Strictly decode standard padded Base64 TEXT into an octet vector."
  (unless (and (stringp text) (plusp (length text))
               (zerop (mod (length text) 4)))
    (codex-image-fail :invalid-response
                      "Codex image response contained invalid Base64 data."))
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    (loop :for offset :from 0 :below (length text) :by 4
          :for a := (codex-image-standard-base64-char-value (char text offset))
          :for b := (codex-image-standard-base64-char-value (char text (+ offset 1)))
          :for c-char := (char text (+ offset 2))
          :for d-char := (char text (+ offset 3))
          :for c := (unless (char= c-char #\=)
                      (codex-image-standard-base64-char-value c-char))
          :for d := (unless (char= d-char #\=)
                      (codex-image-standard-base64-char-value d-char))
          :for final-p := (= (+ offset 4) (length text))
          :do (unless (and a b
                           (or c (char= c-char #\=))
                           (or d (char= d-char #\=))
                           (or (not (char= c-char #\=))
                               (and (char= d-char #\=) final-p))
                           (or (not (char= d-char #\=)) final-p))
                (codex-image-fail :invalid-response
                                  "Codex image response contained invalid Base64 data."))
              (when (and (char= c-char #\=) (not (char= d-char #\=)))
                (codex-image-fail :invalid-response
                                  "Codex image response contained invalid Base64 data."))
              ;; RFC 4648 canonical padding: unused low bits must be zero.
              (when (and (char= c-char #\=)
                         (not (zerop (logand b #x0f))))
                (codex-image-fail :invalid-response
                                  "Codex image response contained invalid Base64 data."))
              (when (and (char= d-char #\=) c
                         (not (zerop (logand c #x03))))
                (codex-image-fail :invalid-response
                                  "Codex image response contained invalid Base64 data."))
              (let ((value (logior (ash a 18) (ash b 12)
                                   (ash (or c 0) 6) (or d 0))))
                (vector-push-extend (ldb (byte 8 16) value) octets)
                (when c
                  (vector-push-extend (ldb (byte 8 8) value) octets))
                (when d
                  (vector-push-extend (ldb (byte 8 0) value) octets))))
    octets))

(defun codex-image-standard-base64-encode (octets)
  "Encode OCTETS as padded standard Base64 for a data URL reference image."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (out)
      (loop :for index :from 0 :below (length octets) :by 3
            :for remaining := (- (length octets) index)
            :for a := (aref octets index)
            :for b := (if (> remaining 1) (aref octets (+ index 1)) 0)
            :for c := (if (> remaining 2) (aref octets (+ index 2)) 0)
            :for value := (logior (ash a 16) (ash b 8) c)
            :do (write-char (char alphabet (ldb (byte 6 18) value)) out)
                (write-char (char alphabet (ldb (byte 6 12) value)) out)
                (write-char (if (> remaining 1)
                                (char alphabet (ldb (byte 6 6) value))
                                #\=)
                            out)
                (write-char (if (> remaining 2)
                                (char alphabet (ldb (byte 6 0) value))
                                #\=)
                            out)))))

(defun codex-image-reference-mime-type (pathname)
  "Infer a conservative image MIME type from PATHNAME's extension."
  (let ((type (string-downcase (or (pathname-type pathname) ""))))
    (cond
      ((string= type "png") "image/png")
      ((or (string= type "jpg") (string= type "jpeg")) "image/jpeg")
      ((string= type "webp") "image/webp")
      ((string= type "gif") "image/gif")
      (t "application/octet-stream"))))

(defun codex-image-read-octets (pathname)
  "Read a local reference image as a fresh unsigned-byte vector."
  (with-open-file (stream pathname :direction :input
                                  :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun codex-image-data-url (path)
  "Return a data URL carrying the local absolute reference image PATH."
  (let* ((pathname (pathname path))
         (mime-type (codex-image-reference-mime-type pathname))
         (encoded (codex-image-standard-base64-encode
                   (codex-image-read-octets pathname))))
    (format nil "data:~A;base64,~A" mime-type encoded)))

(defun codex-image-request-body (request)
  "Build the current Codex-compatible generation or edit JSON request body."
  (let ((references (media-request-referenced-image-paths request)))
    (api-json-encode
     (if references
         `((:model . ,+codex-image-model+)
           (:prompt . ,(media-request-prompt request))
           (:images . ,(coerce
                        (mapcar (lambda (path)
                                  `((:image--url . ,(codex-image-data-url path))))
                                references)
                        'vector)))
         `((:model . ,+codex-image-model+)
           (:prompt . ,(media-request-prompt request))
           (:background . "auto")
           (:quality . "auto")
           (:size . "auto"))))))

(defun codex-image-request-suffix (request)
  "Return the Codex image endpoint suffix required for REQUEST."
  (if (media-request-referenced-image-paths request) "edits" "generations"))

(defun codex-image-force-refresh-auth ()
  "Refresh only ChatGPT subscription auth after an image endpoint 401."
  (handler-case
      (or (refresh-openai-codex-chatgpt-image-auth)
          (codex-image-fail :refresh-failed "Codex image auth refresh failed."))
    (codex-image-error (condition) (error condition))
    (error ()
      (codex-image-fail :refresh-failed "Codex image auth refresh failed."))))

(defun codex-image-http-request (auth request-body suffix request &key (allow-refresh t))
  "POST a narrow Codex image request, with one 401 refresh and safe retries."
  (codex-image-check-cancelled request)
  (multiple-value-bind (body status headers)
      (handler-case
          (provider-http-request-with-retries
           "Codex subscription image request"
           (lambda ()
             (codex-image-check-cancelled request)
             (drakma:http-request
              (codex-image-endpoint auth suffix)
              :method :post
              :content-type "application/json"
              :additional-headers (openai-codex-request-headers auth)
              :external-format-in :utf-8
              :content request-body
              :want-stream nil
              :force-binary t
              :connection-timeout *provider-http-connection-timeout-seconds*))
           :cancel-p (lambda () (codex-image-cancelled-p request)))
        (codex-image-error (condition) (error condition))
        (error ()
          (codex-image-fail :transient-exhausted
                            "Codex image request failed after transient retries.")))
    (cond
      ((codex-image-cancelled-p request)
       (codex-image-fail :cancelled "Codex image request was cancelled."))
      ((null status)
       (codex-image-fail :cancelled "Codex image request was cancelled."))
      ((and (= status 401) allow-refresh)
       (codex-image-http-request (codex-image-force-refresh-auth) request-body suffix request
                                 :allow-refresh nil))
      ((= status 200)
       (values body headers auth))
      ((provider-http-retryable-status-p status)
       (codex-image-fail :transient-exhausted
                         "Codex image request exhausted transient retries."))
      (t
       (codex-image-fail :provider-rejected
                         "Codex image provider rejected the request (HTTP ~D)." status)))))

(defun codex-image-response-asset (response request)
  "Convert a successful Codex JSON RESPONSE into one durable PNG media asset."
  (let* ((data (cdr (assoc :data response)))
         (first-item (and (or (vectorp data) (listp data))
                          (elt data 0)))
         (encoded (and first-item (alist-string-value first-item :b64--json)))
         (revised-prompt (and first-item
                              (alist-string-value first-item :revised--prompt))))
    (unless encoded
      (codex-image-fail :invalid-response
                        "Codex image response did not contain a PNG artifact."))
    (values
     (make-media-asset
      :name (format nil "codex-image-~A.png" (media-request-id request))
      :mime-type "image/png"
      :octets (codex-image-standard-base64-decode encoded)
      :metadata `((:model . ,+codex-image-model+)
                  (:transport . "chatgpt-codex-subscription")
                  (:source . "codex-image")))
     revised-prompt)))

(defun codex-image-start (request)
  "Generate or edit exactly one image through the user's Codex subscription."
  ;; The media package publishes an operation before invoking START-FN and
  ;; installs a cancellation callback in the background tool worker.  Query
  ;; that published operation before connect and at every retry boundary.
  (handler-case
      (progn
        (codex-image-check-cancelled request)
        (let* ((auth (codex-image-resolve-auth))
               (body (codex-image-request-body request))
               (suffix (codex-image-request-suffix request)))
          (multiple-value-bind (response-body _headers final-auth)
              (codex-image-http-request auth body suffix request)
            (declare (ignore _headers final-auth))
            (handler-case
                (let ((response (api-json-decode (http-body-string response-body)))
                      (asset nil)
                      (revised-prompt nil))
                  (multiple-value-setq (asset revised-prompt)
                    (codex-image-response-asset response request))
                  (make-media-provider-outcome
                   :status :succeeded
                   :assets (list asset)
                   :revised-prompt revised-prompt))
              (codex-image-error (condition) (error condition))
              (error ()
                (codex-image-fail :invalid-response
                                  "Codex image response was invalid."))))))
    (codex-image-error (condition)
      (make-media-provider-outcome :status :failed
                                   :public-error (codex-image-error-message condition)))
    (error ()
      (make-media-provider-outcome :status :failed
                                   :public-error "Codex image request failed."))))

(defun codex-image-cancel (_operation)
  "Acknowledge cooperative cancellation of a synchronous Codex image request."
  (declare (ignore _operation))
  (make-media-provider-outcome :status :cancelled))

(register-media-provider +codex-image-provider-id+ '(:image) #'codex-image-start
                         :cancel-fn #'codex-image-cancel)
(unless (media-default-provider)
  (set-media-default-provider +codex-image-provider-id+))

(register-package-prompt-section
 "codex-image"
 "## Codex subscription image generation

- `media_generate_image` uses the configured `codex-image` provider by default
  when this package is enabled. It creates one durable PNG through the user's
  existing Codex ChatGPT subscription; the model cannot select a provider,
  model, billing route, or destination.
- It accepts `prompt` and optional `referenced_image_paths` (up to five
  absolute local images). References select the edit route. Results are stored
  in Artifactum, not rendered as live McCLIM bitmaps yet.
- This adapter does not use an OpenAI API key and has no automatic paid API
  fallback. Treat provider availability as version-sensitive."
 :title "Codex subscription image generation"
 :package "codex-image")
