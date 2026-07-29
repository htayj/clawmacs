(in-package :rplaca/tests)

(in-suite media-package-suite)

(defmacro with-media-package-state (&body body)
  "Run BODY with isolated media, package, tool, and session registries."
  `(with-package-state-override ((default-package-test-channels))
     (let* ((*sessions-dir* (temp-session-test-directory "media"))
            (rplaca::*tool-working-directory* *sessions-dir*)
            (rplaca::*buffer-ring* nil)
            (rplaca::*buffer-counter* 0)
            (rplaca::*default-keymap* nil)
            (rplaca::*scratch-keymap* nil)
            (rplaca::*file-keymap* nil)
            (rplaca::*command-table* (make-hash-table :test #'eq))
            (rplaca::*extended-docs* (make-hash-table :test #'eq))
            (rplaca::*agent-tool-metadata-table* (make-hash-table :test #'eq))
            (rplaca::*agent-tool-name-table* (make-hash-table :test #'equal))
            (rplaca::*tool-table* (make-hash-table :test #'equal))
            (rplaca::*slash-command-table* (make-hash-table :test #'equal))
            (rplaca::*media-provider-registry* (make-hash-table :test #'equal))
            (rplaca::*media-operation-registry* (make-hash-table :test #'equal))
            (rplaca::*media-provider-registry-lock*
             (bt:make-lock "test media provider registry"))
            (rplaca::*media-default-provider* nil)
            (rplaca::*media-operation-counter* 0))
       (set-package-enablement-scope "media" :global)
       (load-active-packages)
       (rplaca::init-default-keymap)
       ,@body)))

(defun make-media-test-buffer (label)
  "Return a persistent chat buffer suitable for durable generated media."
  (let ((root (merge-pathnames (format nil "~A/" label)
                               rplaca::*tool-working-directory*)))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (let ((buffer (rplaca::make-chat-buffer label :working-directory root)))
      (setf (rplaca::buffer-keymap buffer) rplaca::*default-keymap*)
      (add-buffer-to-ring buffer)
      (switch-to-buffer buffer)
      buffer)))

(defun media-test-tool-result (args)
  "Execute the media image tool and decode its Lisp data result."
  (nth-value 0
    (rplaca::lisp-data-read
     (rplaca:execute-tool "media_generate_image" args))))

(defun media-test-png-asset ()
  "Return a deterministic tiny PNG-shaped binary asset for provider tests."
  (rplaca:make-media-asset
   :name "fake-image.png"
   :mime-type "image/png"
   :octets (make-array 8 :element-type '(unsigned-byte 8)
                       :initial-contents '(137 80 78 71 13 10 26 10))
   :metadata '((:seed . 7))))

(defun media-test-create-fifo (path)
  "Create PATH as a FIFO when the test host provides mkfifo, else return NIL."
  (handler-case
      (progn
        (uiop:run-program (list "mkfifo" (namestring path))
                          :output nil :error-output nil)
        (probe-file path))
    (error () nil)))

(test media-package-registers-a-background-image-tool
  "The bundled package exposes one narrow background image-generation tool."
  (with-media-package-state
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (definition (find "media_generate_image" tools
                             :key (lambda (tool) (cdr (assoc :name tool)))
                             :test #'string=))
           (metadata (rplaca:find-agent-tool-metadata
                      'rplaca::media-generate-image-tool)))
      (is-true definition)
      (is (eq :background (rplaca:agent-tool-metadata-execution metadata)))
      (is (search "Generated media with media" (render-package-prompt-sections)))
      (is (not (find "provider"
                     (rplaca:agent-tool-metadata-args metadata)
                     :key (lambda (arg) (getf arg :name))
                     :test #'string=))))))

(test media-provider-registry-enforces-capabilities-and-default-selection
  "Providers are package-owned and selected through trusted configuration."
  (with-media-package-state
    (let ((provider
            (rplaca:register-media-provider
             "fake" '(:image)
             (lambda (_request)
               (declare (ignore _request))
               (rplaca:make-media-provider-outcome
                :status :succeeded
                :assets (list (media-test-png-asset)))))))
      (is (string= "fake" (rplaca:media-provider-id provider)))
      (is (equal '(:image) (rplaca:media-provider-kinds provider)))
      (is (eq provider (rplaca:find-media-provider "FAKE")))
      (signals error (rplaca:set-media-default-provider "missing"))
      (is (string= "fake" (rplaca:set-media-default-provider "fake")))
      (is (string= "fake" (rplaca:media-default-provider)))
      (signals error
        (rplaca:make-media-generation-request :audio "not supported")))))

(test media-provider-owner-and-running-job-invariants-are-canonical
  "Provider ownership and asynchronous backend identifiers cannot be ambiguous."
  (with-media-package-state
    (let ((provider
            (rplaca:register-media-provider
             "owner-fake" '(:image)
             (lambda (_request)
               (declare (ignore _request))
               (rplaca:make-media-provider-outcome :status :failed))
             :package " Media ")))
      (is (string= "media" (rplaca:media-provider-package provider)))
      (is (equal '("owner-fake")
                 (rplaca:remove-media-providers-for-package "MEDIA")))
      (is-false (rplaca:find-media-provider "owner-fake")))
    (signals error
      (rplaca:register-media-provider
       "bad-owner" '(:image) (lambda (_request) (declare (ignore _request)))
       :package "   "))
    (rplaca:register-media-provider
     "missing-job" '(:image)
     (lambda (_request)
       (declare (ignore _request))
       (rplaca:make-media-provider-outcome :status :running :backend-id "  ")))
    (rplaca:set-media-default-provider "missing-job")
    (let ((operation (rplaca:start-media-operation
                      (rplaca:make-media-generation-request :image "job"))))
      (is (eq :failed (rplaca:media-operation-status operation)))
      (is (search "backend id" (rplaca:media-operation-error operation))))))

(test media-request-constructor-enforces-regular-reference-files
  "Direct callers cannot bypass the tool's bounded regular-file reference rule."
  (with-media-package-state
    (let* ((root (temp-package-test-directory "media-reference"))
           (regular (merge-pathnames "reference.png" root))
           (directory (merge-pathnames "directory/" root))
           (fifo (merge-pathnames "reference.fifo" root)))
      (ensure-directories-exist (merge-pathnames #P".keep" root))
      (with-open-file (stream regular :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede
                               :if-does-not-exist :create)
        (write-sequence #(1 2 3) stream))
      (ensure-directories-exist (merge-pathnames #P".keep" directory))
      (let ((request (rplaca:make-media-generation-request
                      :image "regular reference"
                      :referenced-image-paths (vector (namestring regular)))))
        (is (equal (list (namestring (truename regular)))
                   (rplaca:media-request-referenced-image-paths request))))
      (signals error
        (rplaca:make-media-generation-request
         :image "relative" :referenced-image-paths #("relative.png")))
      (signals error
        (rplaca:make-media-generation-request
         :image "directory" :referenced-image-paths (vector (namestring directory))))
      (signals error
        (rplaca:make-media-generation-request
         :image "empty" :referenced-image-paths #()))
      (signals error
        (rplaca:make-media-generation-request
         :image "too many"
         :referenced-image-paths (make-array 6 :initial-element (namestring regular))))
      (when (media-test-create-fifo fifo)
        (unwind-protect
             (signals error
               (rplaca:make-media-generation-request
                :image "fifo" :referenced-image-paths (vector (namestring fifo))))
          (ignore-errors (delete-file fifo)))))))

(test media-image-tool-validates-input-and-persists-successful-assets
  "The tool accepts only prompt plus up to five existing absolute references."
  (with-media-package-state
    (let* ((buffer (make-media-test-buffer "success"))
           (reference (merge-pathnames "reference.png"
                                       (rplaca:buffer-working-directory buffer)))
           (directory-reference (merge-pathnames "directory-reference/"
                                                 (rplaca:buffer-working-directory buffer)))
           (seen-request nil))
      (with-open-file (stream reference
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-sequence #(1 2 3) stream))
      (rplaca:register-media-provider
       "fake" '(:image)
       (lambda (request)
         (setf seen-request request)
         (rplaca:make-media-provider-outcome
          :status :succeeded
          :assets (list (media-test-png-asset))
          :revised-prompt "a revised fake image"
          :backend-id "fake-1")))
      (rplaca:set-media-default-provider "fake")
      (let* ((rplaca::*current-tool-buffer* buffer)
             (result (media-test-tool-result
                      `((:prompt . " draw a test image ")
                        (:referenced_image_paths . ,(vector (namestring reference))))))
             (records (rplaca:artifactum-session-records buffer))
             (artifact (first (coerce (getf result :artifacts) 'list))))
        (is (string= "succeeded" (getf result :status)))
        (is (string= "fake-1" (getf result :backend-id)))
        (is (string= "draw a test image"
                     (rplaca:media-request-prompt seen-request)))
        (is (equal (list (namestring (truename reference)))
                   (rplaca:media-request-referenced-image-paths seen-request)))
        (is (= 1 (length records)))
        (is (string= "generated-media" (getf artifact :kind)))
        (is (probe-file (pathname (getf artifact :path))))
        (is (string= "fake"
                     (rplaca::artifactum-json-value
                      (getf artifact :metadata) "provider")))
        (signals error
          (media-test-tool-result
           '((:prompt . "x") (:provider . "fake"))))
        (signals error
          (media-test-tool-result
           '((:prompt . "x") (:referenced_image_paths . #("relative.png")))))
        (ensure-directories-exist (merge-pathnames #P".keep" directory-reference))
        (signals error
          (media-test-tool-result
           `((:prompt . "x")
             (:referenced_image_paths . ,(vector (namestring directory-reference))))))
        (signals error
          (media-test-tool-result
           `((:prompt . "x")
             (:referenced_image_paths . ,(make-array 6 :initial-element
                                                       (namestring reference))))))))))

(test media-operation-supports-running-poll-and-cancel-lifecycles
  "The same contract handles future asynchronous video providers."
  (with-media-package-state
    (let ((poll-count 0)
          (cancel-count 0))
      (rplaca:register-media-provider
       "async-fake" '(:image :video)
       (lambda (_request)
         (declare (ignore _request))
         (rplaca:make-media-provider-outcome :status :running :backend-id "job-1"))
       :poll-fn (lambda (_operation)
                  (declare (ignore _operation))
                  (incf poll-count)
                  (rplaca:make-media-provider-outcome
                   :status :succeeded :backend-id "job-1"
                   :assets (list (media-test-png-asset))))
       :cancel-fn (lambda (_operation)
                    (declare (ignore _operation))
                    (incf cancel-count)
                    (rplaca:make-media-provider-outcome
                     :status :cancelled :backend-id "job-1")))
      (rplaca:set-media-default-provider "async-fake")
      (let ((operation (rplaca:start-media-operation
                        (rplaca:make-media-generation-request :video "animate"))))
        (is (eq :running (rplaca:media-operation-status operation)))
        (is (eq operation (rplaca:find-media-operation
                           (rplaca:media-operation-id operation))))
        (is (eq operation (rplaca:poll-media-operation operation)))
        (is (= 1 poll-count))
        (is (eq :succeeded (rplaca:media-operation-status operation))))
      (let ((operation (rplaca:start-media-operation
                        (rplaca:make-media-generation-request :image "cancel"))))
        (is (eq :running (rplaca:media-operation-status operation)))
        (is (eq operation (rplaca:cancel-media-operation
                           (rplaca:media-operation-id operation))))
        (is (= 1 cancel-count))
        (is (eq :cancelled (rplaca:media-operation-status operation)))))))

(test media-cancellation-bridges-the-managed-background-tool-boundary
  "Interactive cancellation invokes provider cancellation before a late start settles."
  (with-media-package-state
    (let ((entered (bt:make-semaphore :name "media-start-entered"))
          (release (bt:make-semaphore :name "media-start-release"))
          (cancelled (bt:make-semaphore :name "media-cancelled"))
          (start-count 0)
          (cancel-count 0)
          (cancelled-operation nil)
          (buffer (make-media-test-buffer "cancel-bridge")))
      (rplaca:register-media-provider
       "cancel-fake" '(:image)
       (lambda (_request)
         (declare (ignore _request))
         (incf start-count)
         (bt:signal-semaphore entered)
         (unless (bt:wait-on-semaphore release :timeout 5.0)
           (error "Timed out releasing media start."))
         (rplaca:make-media-provider-outcome
          :status :succeeded :assets (list (media-test-png-asset))))
       :cancel-fn (lambda (operation)
                    (setf cancelled-operation operation)
                    (incf cancel-count)
                    (bt:signal-semaphore cancelled)
                    (rplaca:make-media-provider-outcome :status :cancelled)))
      (rplaca:set-media-default-provider "cancel-fake")
      (let ((state (rplaca::start-interactive-tool-execution
                    buffer "media_generate_image"
                    '((:prompt . "cancel this image")) "media-cancel-1")))
        (unwind-protect
             (progn
               (is-true state)
               (is-true (bt:wait-on-semaphore entered :timeout 2.0))
               (rplaca::cancel-interactive-tool-execution state)
               (is-true (bt:wait-on-semaphore cancelled :timeout 2.0))
               (is (= 1 start-count))
               (is (= 1 cancel-count))
               (is (eq :cancelled
                       (rplaca:media-operation-status cancelled-operation)))
               ;; The late successful start result loses deterministically to
               ;; cancellation and cannot create an Artifactum artifact.
               (bt:signal-semaphore release)
               (loop :repeat 400
                     :while (bt:thread-alive-p
                             (rplaca::interactive-tool-execution-worker state))
                     :do (sleep 0.005))
               (is (eq :cancelled
                       (rplaca:media-operation-status cancelled-operation)))
               (is (null (rplaca:artifactum-session-records buffer))))
          (bt:signal-semaphore release))))))

(test media-cancellation-before-run-skips-the-provider-start
  "A published pending operation can be cancelled before any provider call."
  (with-media-package-state
    (let ((starts 0))
      (rplaca:register-media-provider
       "pending-cancel" '(:image)
       (lambda (_request)
         (declare (ignore _request))
         (incf starts)
         (rplaca:make-media-provider-outcome
          :status :succeeded :assets (list (media-test-png-asset)))))
      (rplaca:set-media-default-provider "pending-cancel")
      (let ((operation (rplaca::begin-media-operation
                        (rplaca:make-media-generation-request :image "do not start"))))
        (rplaca:cancel-media-operation operation)
        (rplaca::run-media-operation operation)
        (is (eq :cancelled (rplaca:media-operation-status operation)))
        (is (= 0 starts))))))

(test media-artifact-write-failures-have-a-stable-media-category
  "Artifactum write failures retain media operation context for a tool result."
  (with-media-package-state
    (let ((buffer (make-media-test-buffer "artifact-write-failure")))
      (rplaca:register-media-provider
       "write-fail" '(:image)
       (lambda (_request)
         (declare (ignore _request))
         (rplaca:make-media-provider-outcome
          :status :succeeded :assets (list (media-test-png-asset)))))
      (rplaca:set-media-default-provider "write-fail")
      (let* ((operation (rplaca:start-media-operation
                         (rplaca:make-media-generation-request :image "write fails")))
             (original (symbol-function 'rplaca:artifactum-create-from-octets)))
        (unwind-protect
             (progn
               (setf (symbol-function 'rplaca:artifactum-create-from-octets)
                     (lambda (&rest _arguments)
                       (declare (ignore _arguments))
                       (error "simulated artifact storage failure")))
               (handler-case
                   (progn
                     (rplaca:persist-media-operation-assets buffer operation)
                     (fail "Expected a media artifact write failure."))
                 (rplaca::media-artifact-write-failed (condition)
                   (is (eq operation
                           (rplaca::media-artifact-write-failed-operation condition)))
                   (is (search "simulated artifact storage failure"
                               (format nil "~A" condition))))))
          (setf (symbol-function 'rplaca:artifactum-create-from-octets)
                original))))))

(test media-provider-cleanup-follows-package-reset-and-reload
  "Package lifecycle reset removes provider registrations owned by that package."
  (with-media-package-state
    (let ((rplaca::*current-rplaca-package* "media"))
      (rplaca:register-media-provider
       "owned-fake" '(:image)
       (lambda (_request)
         (declare (ignore _request))
         (rplaca:make-media-provider-outcome :status :failed
                                                :public-error "not used"))))
    (rplaca:set-media-default-provider "owned-fake")
    (is (rplaca:find-media-provider "owned-fake"))
    (rplaca::reset-package-runtime-state "media")
    (is-false (rplaca:find-media-provider "owned-fake"))
    (is-false (rplaca:media-default-provider))))

(test media-provider-failures-are-returned-as-operation-data
  "Provider exceptions become public failed outcomes instead of crashing a tool worker."
  (with-media-package-state
    (rplaca:register-media-provider
     "broken" '(:image)
     (lambda (_request)
       (declare (ignore _request))
       (error "intentional fake failure")))
    (rplaca:set-media-default-provider "broken")
    (let ((operation (rplaca:start-media-operation
                      (rplaca:make-media-generation-request :image "broken"))))
      (is (eq :failed (rplaca:media-operation-status operation)))
      (is (search "intentional fake failure" (rplaca:media-operation-error operation))))))
