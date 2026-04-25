(in-package :clawmacs/tests)

(in-suite interop-suite)

(defun interop-temp-directory (label)
  "Return a fresh temporary directory for interop tests."
  (make-pathname
   :directory
   (list :absolute "tmp"
         (format nil "clawmacs-interop-~A-~36R-~36R-~A"
                 label
                 (get-universal-time)
                 (get-internal-real-time)
                 (gensym)))))

(defun interop-temp-file (label filename)
  "Return a fresh temporary file path for interop tests."
  (let ((root (uiop:ensure-directory-pathname
               (interop-temp-directory label))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (merge-pathnames filename root)))

(defmacro with-interop-function-override ((name lambda-list &body implementation)
                                          &body body)
  `(let ((original-function (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name)
                  (lambda ,lambda-list
                    ,@implementation))
            ,@body)
       (setf (symbol-function ',name) original-function))))

(defun interop-default-package-test-channels ()
  "Return the bundled package channels for isolated interop tests."
  (list (clawmacs:make-package-channel
         :name "default"
         :root clawmacs:*default-package-channel-directory*
         :description "Bundled Clawmacs packages"
         :source :builtin)))

(defmacro with-interop-test-state (() &body body)
  `(let* ((*sessions-dir* (uiop:ensure-directory-pathname
                           (interop-temp-directory "sessions")))
          (guard-path (interop-temp-file "guard" "guard.json"))
          (package-root (uiop:ensure-directory-pathname
                         (interop-temp-directory "packages")))
          (clawmacs::*approval-policy-path* guard-path)
          (clawmacs::*approval-policy-registry* nil)
          (clawmacs::*approval-policy-project-registry-cache*
            (make-hash-table :test #'equal))
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json" package-root))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels*
           (interop-default-package-test-channels))
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
         (clawmacs::*project-registry* (make-hash-table :test #'equal))
         (clawmacs::*buffer-ring* nil)
         (clawmacs::*buffer-counter* 0)
         (clawmacs::*tool-table* (make-hash-table :test #'equal))
         (clawmacs::*temporary-tool-table* nil)
         (clawmacs::*active-tool-names* nil)
         (clawmacs::*interop-thread-table* (make-hash-table :test #'equal))
         (clawmacs::*interop-turn-table* (make-hash-table :test #'equal))
         (clawmacs::*pipeline-definition-registry*
           (make-hash-table :test #'equal))
          (clawmacs::*pipeline-test-profile-registry*
           (make-hash-table :test #'equal)))
     (ensure-directories-exist (merge-pathnames #P".keep" *sessions-dir*))
     (ensure-directories-exist (merge-pathnames #P".keep" package-root))
     (clawmacs::init-default-keymap)
     (clawmacs::init-global-faces)
     (clawmacs::init-tools)
     ,@body))

(defun interop-wait-for-turn-status (turn-id statuses &key (timeout-seconds 2.0))
  "Wait until TURN-ID reaches one of STATUSES, then return its summary."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds internal-time-units-per-second)))))
    (loop
      (let* ((summary (clawmacs:read-interop-turn turn-id))
             (status (getf summary :status)))
        (when (member status statuses :test #'string=)
          (return summary)))
      (when (>= (get-internal-real-time) deadline)
        (error "Timed out waiting for turn ~A to reach one of ~S"
               turn-id statuses))
      (sleep 0.02))))

(defun interop-wait-for-event-type (events-var event-type &key (timeout-seconds 2.0))
  "Wait until EVENTS-VAR contains EVENT-TYPE and return a snapshot."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds internal-time-units-per-second)))))
    (loop
      (let ((snapshot (funcall events-var)))
        (when (find event-type snapshot
                    :key (lambda (event) (getf event :event))
                    :test #'string=)
          (return snapshot)))
      (when (>= (get-internal-real-time) deadline)
        (error "Timed out waiting for event ~A" event-type))
      (sleep 0.02))))

(defun interop-json-object-value (object key)
  "Return KEY from decoded JSON OBJECT."
  (cdr (assoc key object)))

(test normalize-output-schema-loads-inline-json-and-files
  "Structured-output schemas accept both inline JSON and file paths."
  (let* ((path (interop-temp-file "schema" "schema.json"))
         (json "{\"type\":\"object\",\"required\":[\"status\"]}"))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string json stream))
    (let ((inline (clawmacs:normalize-output-schema json))
          (from-file (clawmacs:normalize-output-schema (namestring path))))
      (is (equal inline from-file))
      (is (string= "object"
                   (interop-json-object-value inline :type))))))

(test parse-and-validate-structured-output-signals-readable-errors
  "Invalid structured output raises a typed validation error with the source text."
  (handler-case
      (progn
        (clawmacs:parse-and-validate-structured-output
         "{\"summary\":\"missing status\"}"
         "{\"type\":\"object\",\"properties\":{\"summary\":{\"type\":\"string\"},\"status\":{\"type\":\"string\"}},\"required\":[\"summary\",\"status\"],\"additionalProperties\":false}")
        (fail "Expected structured-output validation error"))
    (clawmacs:structured-output-validation-error (condition)
      (is (search "Missing required property"
                  (clawmacs:structured-output-validation-error-reason
                   condition)))
      (is (string= "{\"summary\":\"missing status\"}"
                   (clawmacs:structured-output-validation-error-text
                    condition))))))

(test run-single-prompt-attaches-structured-output
  "Single-turn prompt runs validate and attach structured output."
  (with-interop-test-state ()
    (with-interop-function-override
        (clawmacs::provider-request-streaming
         (provider messages callback
                   &key model max-tokens tools reasoning-effort system-prompt)
         (declare (ignore provider messages model max-tokens tools
                          reasoning-effort system-prompt))
         (let ((state (clawmacs::make-stream-state)))
           (bt:with-lock-held ((clawmacs::stream-state-lock state))
             (setf (clawmacs::stream-state-text state) "{\"status\":\"ok\"}"))
           (funcall callback state)
           (bt:with-lock-held ((clawmacs::stream-state-lock state))
             (setf (clawmacs::stream-state-content-blocks state)
                   (reverse
                    (list (clawmacs::canonical-text-block
                           "{\"status\":\"ok\"}")))
                   (clawmacs::stream-state-usage state)
                   '((:input-tokens . 3) (:output-tokens . 5))
                   (clawmacs::stream-state-stop-reason state) "end_turn"
                   (clawmacs::stream-state-done-p state) t))
           state))
      (let* ((events nil)
             (result
               (clawmacs:run-single-prompt
                "Return JSON."
                :provider :zai
                :model "glm-5"
                :output-schema
                "{\"type\":\"object\",\"properties\":{\"status\":{\"type\":\"string\"}},\"required\":[\"status\"],\"additionalProperties\":false}"
                :event-callback (lambda (event)
                                  (push event events)))))
        (is (string= "{\"status\":\"ok\"}"
                     (clawmacs:prompt-run-result-final-text result)))
        (let ((structured (clawmacs:prompt-run-result-structured-output result)))
          (is (string= "ok"
                       (interop-json-object-value structured :status))))
        (is (equal '("assistant.chunk")
                   (mapcar (lambda (event)
                             (getf event :event))
                           (nreverse events))))))))

(test run-pipeline-prompt-attaches-stage-structured-output
  "Pipeline stage schemas propagate parsed JSON through the final prompt result."
  (with-interop-test-state ()
    (clawmacs:register-pipeline-definition
     "interop-schema"
     :stages
     (list
      '(:name "plan"
        :prompt "Return a JSON object."
        :output-schema "{\"type\":\"object\",\"properties\":{\"status\":{\"type\":\"string\"}},\"required\":[\"status\"],\"additionalProperties\":false}")))
    (with-interop-function-override
        (clawmacs::provider-request-streaming
         (provider messages callback
                   &key model max-tokens tools reasoning-effort system-prompt)
         (declare (ignore provider messages callback model max-tokens tools
                          reasoning-effort system-prompt))
         (let ((state (clawmacs::make-stream-state)))
           (bt:with-lock-held ((clawmacs::stream-state-lock state))
             (setf (clawmacs::stream-state-content-blocks state)
                   (reverse
                    (list (clawmacs::canonical-text-block
                           "{\"status\":\"pipeline-ok\"}")))
                   (clawmacs::stream-state-stop-reason state) "end_turn"
                   (clawmacs::stream-state-done-p state) t))
           state))
      (let* ((result (clawmacs:run-pipeline-prompt
                      "Do the work."
                      "interop-schema"
                      :provider :zai
                      :model "glm-5"))
             (structured (clawmacs:prompt-run-result-structured-output result)))
        (is (string= "pipeline-ok"
                     (interop-json-object-value structured :status)))))))

(test handle-interop-request-supports-thread-lifecycle
  "The interop request handler exposes initialize plus thread lifecycle methods."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "project")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd))))
      (declare (ignore _keep))
      (with-interop-function-override
          (clawmacs::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (let ((state (clawmacs::make-stream-state)))
             (bt:with-lock-held ((clawmacs::stream-state-lock state))
               (setf (clawmacs::stream-state-content-blocks state)
                     (reverse
                      (list (clawmacs::canonical-text-block "interop complete")))
                     (clawmacs::stream-state-usage state)
                     '((:input-tokens . 2) (:output-tokens . 4))
                     (clawmacs::stream-state-stop-reason state) "end_turn"
                     (clawmacs::stream-state-done-p state) t))
             state))
        (let* ((initialize
                 (clawmacs:handle-interop-request
                  '((:method . "initialize"))))
               (started
                 (clawmacs:handle-interop-request
                  `((:method . "thread.start")
                    (:params . ((:session-name . "interop-thread")
                                (:cwd . ,(namestring cwd))
                                (:model-provider . "zai")
                                (:model . "glm-5"))))))
               (thread-id (getf started :id))
               (run-result
                 (clawmacs:handle-interop-request
                  `((:method . "thread.run")
                    (:params . ((:thread-id . ,thread-id)
                                (:input . "Continue"))))))
               (read-result
                 (clawmacs:handle-interop-request
                  `((:method . "thread.read")
                    (:params . ((:thread-id . ,thread-id)
                                (:include-turns . t))))))
               (list-result
                 (clawmacs:handle-interop-request
                  `((:method . "thread.list")
                    (:params . ((:cwd . ,(namestring cwd)))))))
               (forked
                 (clawmacs:handle-interop-request
                  `((:method . "thread.fork")
                    (:params . ((:thread-id . ,thread-id)
                                (:name . "forked-thread"))))))
               (resumed
                 (clawmacs:handle-interop-request
                  `((:method . "thread.resume")
                    (:params . ((:thread-id . ,thread-id)))))))
          (is (= 1 (getf initialize :protocol-version)))
          (is (string= "clawmacs-app-server"
                       (getf (getf initialize :server-info) :name)))
          (is (string= "interop complete"
                       (getf run-result :final-response)))
          (is (= 2 (length (coerce (getf read-result :items) 'list))))
          (is (find thread-id
                    (coerce (getf list-result :threads) 'list)
                    :key (lambda (entry) (getf entry :id))
                    :test #'string=))
          (is (not (string= thread-id (getf forked :id))))
          (is (string= thread-id (getf resumed :id))))))))

(test run-interop-thread-streams-assistant-chunks
  "Synchronous interop thread runs emit assistant chunk events and completion."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "stream-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (events nil))
      (declare (ignore _keep))
      (with-interop-function-override
          (clawmacs::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages model max-tokens tools
                            reasoning-effort system-prompt))
           (let ((state (clawmacs::make-stream-state)))
             (bt:with-lock-held ((clawmacs::stream-state-lock state))
               (setf (clawmacs::stream-state-text state) "partial"))
             (funcall callback state)
             (bt:with-lock-held ((clawmacs::stream-state-lock state))
               (setf (clawmacs::stream-state-content-blocks state)
                     (reverse (list (clawmacs::canonical-text-block
                                     "final answer")))
                     (clawmacs::stream-state-stop-reason state) "end_turn"
                     (clawmacs::stream-state-done-p state) t))
             state))
        (let* ((thread (clawmacs:start-interop-thread
                        :session-name "stream-thread"
                        :cwd cwd
                        :provider "zai"
                        :model "glm-5"))
               (result (clawmacs:run-interop-thread
                        (clawmacs:interop-thread-id thread)
                        "Stream a reply."
                        :event-callback (lambda (event)
                                          (push event events))))
               (ordered-events (nreverse events)))
          (is (string= "final answer"
                       (clawmacs:prompt-run-result-final-text result)))
          (is (equal '("turn.started" "assistant.chunk" "turn.completed")
                     (mapcar (lambda (event) (getf event :event))
                             ordered-events))))))))

(test run-interop-thread-streams-tool-events
  "Synchronous interop thread runs emit tool call/result events."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "tool-stream-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (events nil)
           (request-count 0))
      (declare (ignore _keep))
      (with-interop-function-override
          (clawmacs::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (incf request-count)
           (if (= request-count 1)
               (let ((state (clawmacs::make-stream-state)))
                 (bt:with-lock-held ((clawmacs::stream-state-lock state))
                   (setf (clawmacs::stream-state-content-blocks state)
                         (reverse
                          (list
                           `((:type . "tool_use")
                             (:id . "call-1")
                             (:name . "lisp_eval")
                             (:input . ((:code . "(+ 2 3)"))))))
                         (clawmacs::stream-state-stop-reason state) "tool_use"
                         (clawmacs::stream-state-done-p state) t))
                 state)
               (let ((state (clawmacs::make-stream-state)))
                 (bt:with-lock-held ((clawmacs::stream-state-lock state))
                   (setf (clawmacs::stream-state-content-blocks state)
                         (reverse (list (clawmacs::canonical-text-block
                                         "done")))
                         (clawmacs::stream-state-stop-reason state) "end_turn"
                         (clawmacs::stream-state-done-p state) t))
                 state)))
        (let* ((thread (clawmacs:start-interop-thread
                        :session-name "tool-stream-thread"
                        :cwd cwd
                        :provider "zai"
                        :model "glm-5"))
               (result (clawmacs:run-interop-thread
                        (clawmacs:interop-thread-id thread)
                        "Use one tool."
                        :auto-approve-tools-p t
                        :event-callback (lambda (event)
                                          (push event events))))
               (ordered-events (nreverse events)))
          (is (string= "done"
                       (clawmacs:prompt-run-result-final-text result)))
          (is (equal '("turn.started"
                       "tool.call"
                       "tool.result"
                       "turn.completed")
                     (mapcar (lambda (event) (getf event :event))
                             ordered-events))))))))

(test interop-local-client-dispatches-through-the-request-handler
  "The minimal Lisp client exposes the same request surface in-process."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "client-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd))))
      (declare (ignore _keep))
      (let* ((client (clawmacs:make-interop-local-client))
             (started
               (clawmacs:interop-client-call
                client
                "thread.start"
                `((:session-name . "client-thread")
                  (:cwd . ,(namestring cwd))
                  (:ephemeral . t))))
             (listed
               (clawmacs:interop-client-call client "thread.list")))
        (is (string= "client-thread" (getf started :session-name)))
        (is (find (getf started :id)
                  (coerce (getf listed :threads) 'list)
                  :key (lambda (entry) (getf entry :id))
                  :test #'string=))))))

(test start-interop-turn-streams-events-and-supports-interrupt
  "Async turns emit ordered events and can be interrupted through the turn API."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "turn-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (events-lock (bt:make-lock "interop-test-events"))
           (events nil)
           (request-count 0))
      (declare (ignore _keep))
      (with-interop-function-override
          (clawmacs::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort
                            system-prompt))
           (incf request-count)
           (let ((state (clawmacs::make-stream-state)))
             state))
        (let* ((thread (clawmacs:start-interop-thread
                        :session-name "async-thread"
                        :cwd cwd
                        :provider "zai"
                        :model "glm-5"))
               (turn (clawmacs:start-interop-turn
                      (clawmacs:interop-thread-id thread)
                      "Work until interrupted."
                      :auto-approve-tools-p t
                      :event-callback (lambda (event)
                                        (bt:with-lock-held (events-lock)
                                          (push event events)))))
               (turn-id (clawmacs:interop-turn-id turn)))
          (interop-wait-for-turn-status turn-id '("running"))
          (interop-wait-for-event-type
           (lambda ()
             (bt:with-lock-held (events-lock)
               (copy-list events)))
           "turn.started")
          (clawmacs:interrupt-interop-turn turn-id)
          (let* ((summary (interop-wait-for-turn-status turn-id '("interrupted")))
                 (ordered-events
                   (nreverse
                    (bt:with-lock-held (events-lock)
                      (copy-list events))))
                 (event-types (mapcar (lambda (event) (getf event :event))
                                      ordered-events))
                 (completion (find "turn.completed"
                                   ordered-events
                                   :key (lambda (event) (getf event :event))
                                   :test #'string=)))
            (is (equal '("turn.started"
                         "turn.interrupted"
                         "turn.completed")
                       event-types))
            (is (string= "interrupted" (getf summary :status)))
            (is (string= "cancelled" (getf completion :stop-reason)))))))))
