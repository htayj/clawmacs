(in-package :rplaca/tests)

(in-suite interop-suite)

(defun interop-temp-directory (label)
  "Return a fresh temporary directory for interop tests."
  (make-pathname
   :directory
   (list :absolute "tmp"
         (format nil "rplaca-interop-~A-~36R-~36R-~A"
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

(defun wait-for-interop-test-thread-exit (thread &key (timeout 2.0))
  "Wait boundedly for THREAD to exit, joining it only after settlement."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop :while (and (bt:thread-alive-p thread)
                      (< (get-internal-real-time) deadline))
          :do (sleep 0.005))
    (unless (bt:thread-alive-p thread)
      (bt:join-thread thread)
      t)))

(defun interop-default-package-test-channels ()
  "Return the bundled package channels for isolated interop tests."
  (list (rplaca:make-package-channel
         :name "default"
         :root rplaca:*default-package-channel-directory*
         :description "Bundled RPLACA packages"
         :source :builtin)))

(defmacro with-interop-test-state (() &body body)
  `(let* ((*sessions-dir* (uiop:ensure-directory-pathname
                           (interop-temp-directory "sessions")))
          (package-root (uiop:ensure-directory-pathname
                         (interop-temp-directory "packages")))
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json" package-root))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels*
           (interop-default-package-test-channels))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
         (rplaca::*project-registry* (make-hash-table :test #'equal))
         (rplaca::*buffer-ring* nil)
         (rplaca::*buffer-counter* 0)
         (rplaca::*tool-table* (make-hash-table :test #'equal))
         (rplaca::*temporary-tool-table* nil)
         (rplaca::*active-tool-names* nil)
         (rplaca::*interop-thread-table* (make-hash-table :test #'equal))
         (rplaca::*interop-turn-table* (make-hash-table :test #'equal))
         (rplaca::*interop-terminal-turn-history-limit* 128)
         (rplaca::*interop-idle-thread-history-limit* 32)
         (rplaca::*interop-registry-sequence-counter* 0)
         (rplaca::*interop-runtime-operations* (make-hash-table :test #'eq))
         (rplaca::*pipeline-definition-registry*
           (make-hash-table :test #'equal))
          (rplaca::*pipeline-test-profile-registry*
           (make-hash-table :test #'equal)))
     (ensure-directories-exist (merge-pathnames #P".keep" *sessions-dir*))
     (ensure-directories-exist (merge-pathnames #P".keep" package-root))
     (rplaca::init-default-keymap)
     (rplaca::init-tools)
     ,@body))

(test interop-history-pruning-preserves-live-work-and-releases-evictions
  "Registry bounds preserve live work while evictions drop heavy resources."
  (with-interop-test-state ()
    (let* ((rplaca::*interop-terminal-turn-history-limit* 1)
           (rplaca::*interop-idle-thread-history-limit* 1)
           (active-buffer
             (rplaca::make-chat-buffer
              "active-retention" :session-persistence-mode :ephemeral))
           (old-buffer
             (rplaca::make-chat-buffer
              "old-retention" :session-persistence-mode :ephemeral))
           (recent-buffer
             (rplaca::make-chat-buffer
              "recent-retention" :session-persistence-mode :ephemeral))
           (active-thread
             (rplaca::make-interop-thread-from-buffer
              active-buffer :id "thread-active" :ephemeral-p t :register-p nil))
           (old-thread-result (list :large "old-thread-result"))
           (old-thread
             (rplaca::make-interop-thread-from-buffer
              old-buffer :id "thread-old" :ephemeral-p t :register-p nil))
           (recent-thread
             (rplaca::make-interop-thread-from-buffer
              recent-buffer :id "thread-recent" :ephemeral-p t
              :register-p nil))
           (active-turn
             (rplaca::make-interop-turn
              :id "turn-active"
              :thread-id "thread-active"
              :status :running
              :input "active input"
              :runner-installed-p t
              :runner-finished-p nil))
           (old-turn-result (list :large "old-turn-result"))
           (old-turn
             (rplaca::make-interop-turn
              :id "turn-old"
              :thread-id "turn-history-thread"
              :status :failed
              :input "old input"
              :output-schema (list :large "old schema")
              :result old-turn-result
              :error "old failure"
              :current-stream-state (list :old-stream)
              :event-callback (lambda (_event) (declare (ignore _event)))
              :runner-installed-p t
              :runner-finished-p t))
           (recent-turn
             (rplaca::make-interop-turn
              :id "turn-recent"
              :thread-id "turn-history-thread"
              :status :failed
              :input "recent input"
              :error "recent failure"
              :runner-installed-p t
              :runner-finished-p t)))
      (setf (rplaca::interop-thread-last-result old-thread)
            old-thread-result)
      (mapc #'rplaca::register-interop-thread
            (list active-thread old-thread recent-thread))
      (rplaca::reserve-interop-thread-execution active-thread "turn-active")
      (mapc #'rplaca::register-interop-turn
            (list active-turn old-turn recent-turn))
      (unwind-protect
           (progn
             (multiple-value-bind (turn-count thread-count)
                 (rplaca::prune-interop-registries)
               (is (= 1 turn-count))
               (is (= 1 thread-count)))
             (is (eq active-turn
                     (rplaca::find-interop-turn "turn-active")))
             (is (eq recent-turn
                     (rplaca::find-interop-turn "turn-recent")))
             (is-false (rplaca::find-interop-turn "turn-old"))
             (is (= 2 (hash-table-count rplaca::*interop-turn-table*)))
             (is (= 1
                    (count-if
                     (lambda (turn)
                       (bt:with-lock-held ((rplaca::interop-turn-lock turn))
                         (rplaca::settled-interop-turn-p turn)))
                     (rplaca::interop-turn-registry-snapshot))))
             (is-true
              (rplaca::interop-turn-retained-resources-released-p old-turn))
             (is (null (rplaca::interop-turn-input old-turn)))
             (is (null (rplaca::interop-turn-output-schema old-turn)))
             (is (null (rplaca::interop-turn-result old-turn)))
             (is (null (rplaca::interop-turn-error old-turn)))
             (is (null (rplaca::interop-turn-current-stream-state old-turn)))
             (is (null (rplaca::interop-turn-event-callback old-turn)))
             (let ((summary (rplaca:read-interop-turn "turn-recent")))
               (is (string= "failed" (getf summary :status)))
               (is (string= "recent failure" (getf summary :error))))
             (is (eq active-thread
                     (rplaca::find-live-interop-thread "thread-active")))
             (is (eq recent-thread
                     (rplaca::find-live-interop-thread "thread-recent")))
             (is-false (rplaca::find-live-interop-thread "thread-old"))
             (is (= 2 (hash-table-count rplaca::*interop-thread-table*)))
             (is-true
              (rplaca::interop-thread-retained-resources-released-p
               old-thread))
             (is (null (rplaca::interop-thread-buffer old-thread)))
             (is (null (rplaca::interop-thread-last-result old-thread)))
             (is-true (rplaca::buffer-disposed-p old-buffer))
             (is-false (rplaca::buffer-disposed-p active-buffer))
             (is-false (rplaca::buffer-disposed-p recent-buffer))
             (is-true
              (rplaca::interop-thread-execution-reserved-p active-thread)))
        (rplaca::release-interop-thread-execution
         active-thread "turn-active")))))

(test interop-construction-and-reservation-use-safe-reload-admission
  "Reload ownership rejects starts and the outer gate precedes publication."
  (with-interop-test-state ()
    (let ((rplaca::*safe-reload-active-request* :reload-owned))
      (signals rplaca:runtime-admission-closed
        (rplaca:start-interop-thread
         :session-name "refused-start" :ephemeral t))
      (is (= 0 (hash-table-count rplaca::*interop-thread-table*))))
    (let* ((live
             (rplaca:start-interop-thread
              :session-name "admission-live" :ephemeral t))
           (thread-id (rplaca:interop-thread-id live))
           (unpublished
             (rplaca::make-interop-thread :id "refused-publication"))
           (unpublished-turn
             (rplaca::make-interop-turn
              :id "refused-turn" :thread-id thread-id :status :queued)))
      (let ((rplaca::*safe-reload-active-request* :reload-owned))
        (signals rplaca:runtime-admission-closed
          (rplaca:resume-interop-thread "missing-session"))
        (signals rplaca:runtime-admission-closed
          (rplaca:fork-interop-thread thread-id))
        (signals rplaca:runtime-admission-closed
          (rplaca::reserve-interop-thread-execution live "refused-owner"))
        (signals rplaca:runtime-admission-closed
          (rplaca:start-interop-turn thread-id "Refuse turn"))
        (signals rplaca:runtime-admission-closed
          (rplaca::register-interop-thread unpublished))
        (signals rplaca:runtime-admission-closed
          (rplaca::register-interop-turn unpublished-turn)))
      (is-false (rplaca::interop-thread-execution-reserved-p live))
      (is (= 1 (hash-table-count rplaca::*interop-thread-table*)))
      (is (= 0 (hash-table-count rplaca::*interop-turn-table*)))
      (let ((thread-table rplaca::*interop-thread-table*)
            (turn-table rplaca::*interop-turn-table*)
            (started (bt:make-semaphore :name "interop-admission-started"))
            (finished (bt:make-semaphore :name "interop-admission-finished"))
            (created nil)
            (worker-error nil)
            (worker nil)
            (lock-held-p nil))
        (unwind-protect
             (progn
               (setf lock-held-p
                     (bt:acquire-lock rplaca::*safe-reload-lock* nil))
               (is-true lock-held-p)
               (setf worker
                     (bt:make-thread
                      (lambda ()
                        (let ((rplaca::*interop-thread-table* thread-table)
                              (rplaca::*interop-turn-table* turn-table))
                          (bt:signal-semaphore started)
                          (handler-case
                              (setf created
                                    (rplaca:start-interop-thread
                                     :session-name "gated-start"
                                     :ephemeral t))
                            (error (condition)
                              (setf worker-error condition)))
                          (bt:signal-semaphore finished)))
                      :name "interop-admission-publication-test"))
               (is (bt:wait-on-semaphore started :timeout 2.0))
               (is (null (bt:wait-on-semaphore finished :timeout 0.05)))
               (is (= 1 (hash-table-count thread-table)))
               (bt:release-lock rplaca::*safe-reload-lock*)
               (setf lock-held-p nil)
               (bt:join-thread worker)
               (setf worker nil)
               (is (null worker-error))
               (is (rplaca::interop-thread-p created))
               (is (= 2 (hash-table-count thread-table))))
          (when lock-held-p
            (bt:release-lock rplaca::*safe-reload-lock*))
          (when worker
            (bt:join-thread worker)))))))

(test async-turn-completion-enforces-terminal-history-limit
  "Runner settlement itself evicts terminal history beyond the configured cap."
  (with-interop-test-state ()
    (let* ((rplaca::*interop-terminal-turn-history-limit* 1)
           (thread
             (rplaca:start-interop-thread
              :session-name "async-history-limit" :ephemeral t))
           (thread-id (rplaca:interop-thread-id thread))
           (first nil)
           (second nil)
           ;; Bordeaux worker threads do not inherit the test's dynamic
           ;; registry bindings.  Make the worker's pruning call operate on
           ;; the isolated registries whose contents this test asserts.
           (turn-table rplaca::*interop-turn-table*)
           (thread-table rplaca::*interop-thread-table*))
      (with-interop-function-override
          (rplaca::make-interop-turn-runner-thread (function name)
           (let ((sequence-counter
                   rplaca::*interop-registry-sequence-counter*))
             (bt:make-thread
              (lambda ()
                (let ((rplaca::*interop-turn-table* turn-table)
                      (rplaca::*interop-thread-table* thread-table)
                      (rplaca::*interop-terminal-turn-history-limit* 1)
                      (rplaca::*interop-registry-sequence-counter*
                        sequence-counter))
                  (funcall function)))
              :name name)))
        (with-interop-function-override
            (rplaca::provider-request-streaming
             (provider messages callback
                       &key model max-tokens tools reasoning-effort system-prompt)
             (declare (ignore provider messages callback model max-tokens tools
                              reasoning-effort system-prompt))
             (make-completed-interop-test-stream-state "history complete"))
          (setf first (rplaca:start-interop-turn thread-id "First turn"))
          (bt:join-thread (rplaca::interop-turn-runner-thread first))
          (is (= 1 (hash-table-count rplaca::*interop-turn-table*)))
          (setf second (rplaca:start-interop-turn thread-id "Second turn"))
          (bt:join-thread (rplaca::interop-turn-runner-thread second))
          (is (= 1 (hash-table-count rplaca::*interop-turn-table*)))
          (is-false
           (rplaca::find-interop-turn (rplaca:interop-turn-id first)))
          (is (eq second
                  (rplaca::find-interop-turn
                   (rplaca:interop-turn-id second))))
          (is-true
           (rplaca::interop-turn-retained-resources-released-p first))
          (is (string= "succeeded"
                       (getf (rplaca:read-interop-turn
                              (rplaca:interop-turn-id second))
                             :status))))))))

(test async-turn-final-pruning-holds-settlement-admission
  "The runner cannot reopen reload admission before terminal pruning finishes."
  (with-interop-test-state ()
    (let* ((thread
             (rplaca:start-interop-thread
              :session-name "async-settlement-gate" :ephemeral t))
           (thread-id (rplaca:interop-thread-id thread))
           (provider-entered
             (bt:make-semaphore :name "settlement-provider-entered"))
           (release-provider
             (bt:make-semaphore :name "settlement-provider-release"))
           (cleanup-entered
             (bt:make-semaphore :name "settlement-cleanup-entered"))
           (release-cleanup
             (bt:make-semaphore :name "settlement-cleanup-release"))
           (turn nil)
           (original-prune
             (symbol-function 'rplaca::prune-interop-registries)))
      (with-interop-function-override
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (bt:signal-semaphore provider-entered)
           (unless (bt:wait-on-semaphore release-provider :timeout 5.0)
             (error "Timed out releasing settlement provider"))
           (make-completed-interop-test-stream-state "settlement complete"))
        (with-interop-function-override
            (rplaca::prune-interop-registries (&key protected-thread)
             (when (and turn
                        (bt:with-lock-held
                            ((rplaca::interop-turn-lock turn))
                          (rplaca::interop-turn-runner-finished-p turn)))
               (bt:signal-semaphore cleanup-entered)
               (unless (bt:wait-on-semaphore release-cleanup :timeout 5.0)
                 (error "Timed out releasing terminal history pruning")))
             (funcall original-prune :protected-thread protected-thread))
          (unwind-protect
               (progn
                 (setf turn
                       (rplaca:start-interop-turn
                        thread-id "Hold final pruning"))
                 (is (bt:wait-on-semaphore provider-entered :timeout 2.0))
                 (bt:signal-semaphore release-provider)
                 (is (bt:wait-on-semaphore cleanup-entered :timeout 2.0))
                 (let ((acquired
                         (bt:acquire-lock rplaca::*safe-reload-lock* nil)))
                   (when acquired
                     (bt:release-lock rplaca::*safe-reload-lock*))
                   (is (null acquired)))
                 (bt:signal-semaphore release-cleanup)
                 (bt:join-thread
                  (rplaca::interop-turn-runner-thread turn))
                 (let ((acquired
                         (bt:acquire-lock rplaca::*safe-reload-lock* nil)))
                   (is-true acquired)
                   (when acquired
                     (bt:release-lock rplaca::*safe-reload-lock*)))
                 (is-true
                  (rplaca::interop-turn-runner-finished-p turn)))
            (bt:signal-semaphore release-provider)
            (bt:signal-semaphore release-cleanup)
            (when (and turn
                       (bt:thread-alive-p
                        (rplaca::interop-turn-runner-thread turn)))
              (bt:join-thread
               (rplaca::interop-turn-runner-thread turn)))))))))

(defun interop-wait-for-turn-status (turn-id statuses &key (timeout-seconds 2.0))
  "Wait until TURN-ID reaches one of STATUSES, then return its summary."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds internal-time-units-per-second)))))
    (loop
      (let* ((summary (rplaca:read-interop-turn turn-id))
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

(defun make-completed-interop-test-stream-state (&optional (text "done"))
  "Return a terminal stream state containing one assistant text block."
  (let ((state (rplaca::make-stream-state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-content-blocks state)
            (reverse (list (rplaca::canonical-text-block text)))
            (rplaca::stream-state-stop-reason state) "end_turn"
            (rplaca::stream-state-done-p state) t))
    state))

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
    (let ((inline (rplaca:normalize-output-schema json))
          (from-file (rplaca:normalize-output-schema (namestring path))))
      (is (equal inline from-file))
      (is (string= "object"
                   (interop-json-object-value inline :type))))))

(test parse-and-validate-structured-output-signals-readable-errors
  "Invalid structured output raises a typed validation error with the source text."
  (handler-case
      (progn
        (rplaca:parse-and-validate-structured-output
         "{\"summary\":\"missing status\"}"
         "{\"type\":\"object\",\"properties\":{\"summary\":{\"type\":\"string\"},\"status\":{\"type\":\"string\"}},\"required\":[\"summary\",\"status\"],\"additionalProperties\":false}")
        (fail "Expected structured-output validation error"))
    (rplaca:structured-output-validation-error (condition)
      (is (search "Missing required property"
                  (rplaca:structured-output-validation-error-reason
                   condition)))
      (is (string= "{\"summary\":\"missing status\"}"
                   (rplaca:structured-output-validation-error-text
                    condition))))))

(test run-single-prompt-attaches-structured-output
  "Single-turn prompt runs validate and attach structured output."
  (with-interop-test-state ()
    (with-interop-function-override
        (rplaca::provider-request-streaming
         (provider messages callback
                   &key model max-tokens tools reasoning-effort system-prompt)
         (declare (ignore provider messages model max-tokens tools
                          reasoning-effort system-prompt))
         (let ((state (rplaca::make-stream-state)))
           (bt:with-lock-held ((rplaca::stream-state-lock state))
             (setf (rplaca::stream-state-text state) "{\"status\":\"ok\"}"))
           (funcall callback state)
           (bt:with-lock-held ((rplaca::stream-state-lock state))
             (setf (rplaca::stream-state-content-blocks state)
                   (reverse
                    (list (rplaca::canonical-text-block
                           "{\"status\":\"ok\"}")))
                   (rplaca::stream-state-usage state)
                   '((:input-tokens . 3) (:output-tokens . 5))
                   (rplaca::stream-state-stop-reason state) "end_turn"
                   (rplaca::stream-state-done-p state) t))
           state))
      (let* ((events nil)
             (result
               (rplaca:run-single-prompt
                "Return JSON."
                :provider :zai
                :model "glm-5"
                :output-schema
                "{\"type\":\"object\",\"properties\":{\"status\":{\"type\":\"string\"}},\"required\":[\"status\"],\"additionalProperties\":false}"
                :event-callback (lambda (event)
                                  (push event events)))))
        (is (string= "{\"status\":\"ok\"}"
                     (rplaca:prompt-run-result-final-text result)))
        (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
        (let ((structured (rplaca:prompt-run-result-structured-output result)))
          (is (string= "ok"
                       (interop-json-object-value structured :status))))
        (is (equal '("assistant.chunk")
                   (mapcar (lambda (event)
                             (getf event :event))
                           (nreverse events))))))))

(test blocked-prompt-event-callback-cannot-retain-provider-reader
  "A never-returning public event callback cannot own the provider reader."
  (with-interop-test-state ()
    (let ((callback-entered
            (bt:make-semaphore :name "blocked prompt callback entered"))
          (callback-release
            (bt:make-semaphore :name "release blocked prompt callback"))
          (state nil)
          (result nil))
      (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
      (unwind-protect
           (with-interop-function-override
               (rplaca::provider-request-streaming
                (provider messages callback
                          &key model max-tokens tools reasoning-effort
                            system-prompt)
                (declare (ignore provider messages model max-tokens tools
                                 reasoning-effort system-prompt))
                (setf state
                      (rplaca::make-stream-state :callback callback))
                (rplaca::start-stream-state-reader-worker
                 state callback "blocked-prompt-callback-provider"
                 (lambda (worker-state)
                   (rplaca::call-with-active-stream-state
                    worker-state
                    (lambda (locked-state)
                      (setf (rplaca::stream-state-text locked-state)
                            "callback-safe")))
                   (rplaca::maybe-call-streaming-callback
                    callback worker-state)
                   (rplaca::transition-stream-state-to-terminal
                    worker-state
                    :stop-reason "end_turn"
                    :update
                    (lambda (locked-state)
                      (setf (rplaca::stream-state-content-blocks locked-state)
                            (list (rplaca::canonical-text-block
                                   "callback-safe")))))))
                state)
             (setf result
                   (rplaca:run-single-prompt
                    "Prove callback isolation."
                    :provider :zai
                    :model "glm-5"
                    :event-callback
                    (lambda (event)
                      (when (string= "assistant.chunk" (getf event :event))
                        (bt:signal-semaphore callback-entered)
                        (bt:wait-on-semaphore callback-release)))))
             (is (string= "callback-safe"
                          (rplaca:prompt-run-result-final-text result)))
             (is-true (bt:wait-on-semaphore callback-entered :timeout 2.0))
             ;; Prompt completion joined and cleared the reader even though the
             ;; copied public callback delivery is still deliberately blocked.
             (is (null (rplaca::stream-state-reader-thread state)))
             (is (= 1 (rplaca::runtime-callback-dispatch-pending-count)))
             (is (eq :external-callback
                     (rplaca::safe-reload-process-runtime-activity))))
        (bt:signal-semaphore callback-release)
        (is-true
         (rplaca::wait-for-runtime-callback-dispatch-idle :timeout 2.0))))))

(test runtime-callback-payload-copy-preserves-cycles
  "Callback handoff copies cyclic mutable payloads without recursing forever."
  (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
  (let* ((payload (cons :cycle nil))
         (received nil)
         (proxy
           (rplaca::make-bounded-runtime-callback
            (lambda (value)
              (setf received value))
            :label "cyclic callback payload test")))
    (setf (cdr payload) payload)
    (is-true (funcall proxy payload))
    (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
    (is (consp received))
    (is (not (eq payload received)))
    (is (eq received (cdr received)))))

(test runtime-callback-copy-failure-is-a-contained-refusal
  "A payload outside the copy budget never escapes into its runtime caller."
  (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
  (let* ((rplaca::*runtime-callback-copy-node-limit* 0)
         (delivered-p nil)
         (dropped-before
           (rplaca::runtime-callback-dispatch-dropped-count))
         (proxy
           (rplaca::make-bounded-runtime-callback
            (lambda (_value)
              (declare (ignore _value))
              (setf delivered-p t))
            :label "copy refusal test")))
    (is-false (funcall proxy (list :too-large)))
    (is-false delivered-p)
    (is (= (1+ dropped-before)
           (rplaca::runtime-callback-dispatch-dropped-count)))))

(test runtime-callback-copy-aggregate-budget-bounds-flat-containers
  "Flat strings, vectors, and hash entries consume the aggregate copy budget."
  (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
  (let* ((rplaca::*runtime-callback-copy-element-limit* 2)
         (delivered-count 0)
         (dropped-before
           (rplaca::runtime-callback-dispatch-dropped-count))
         (proxy
           (rplaca::make-bounded-runtime-callback
            (lambda (_value)
              (declare (ignore _value))
              (incf delivered-count))
            :label "aggregate copy refusal test"))
         (table (make-hash-table :test #'equal :size 100000)))
    (setf (gethash "one" table) 1
          (gethash "two" table) 2)
    (is-false (funcall proxy "abc"))
    (is-false (funcall proxy #(1 2 3)))
    (is-false (funcall proxy table))
    (is (zerop delivered-count))
    (is (= (+ 3 dropped-before)
           (rplaca::runtime-callback-dispatch-dropped-count)))))

(test runtime-callback-copy-does-not-reproduce-sparse-hash-capacity
  "A sparse callback hash is copied according to entries, not reserved size."
  (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
  (let* ((source (make-hash-table :test #'equal :size 100000))
         (received nil)
         (proxy
           (rplaca::make-bounded-runtime-callback
            (lambda (value)
              (setf received value))
            :label "sparse hash callback test")))
    (setf (gethash "key" source) "value")
    (is-true (funcall proxy source))
    (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
    (is (hash-table-p received))
    (is (string= "value" (gethash "key" received)))
    (is (< (hash-table-size received) (hash-table-size source)))))

(test runtime-callback-lane-is-ordered-bounded-and-observable
  "A blocked lane retains a bounded FIFO and reports its newest refusal."
  (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
  (let* ((rplaca::*runtime-callback-dispatch-queue-limit* 1)
         (callback-entered
           (bt:make-semaphore :name "bounded callback lane entered"))
         (callback-release
           (bt:make-semaphore :name "release bounded callback lane"))
         (values-lock (bt:make-lock "bounded callback values"))
         (debug-lock (bt:make-lock "bounded callback debug events"))
         (delivered nil)
         (debug-events nil)
         (dropped-before
           (rplaca::runtime-callback-dispatch-dropped-count))
         (proxy
           (rplaca::make-bounded-runtime-callback
            (lambda (value)
              (bt:with-lock-held (values-lock)
                (push value delivered))
              (when (= value 1)
                (bt:signal-semaphore callback-entered)
                (bt:wait-on-semaphore callback-release)))
            :label "bounded ordered callback test")))
    (with-interop-function-override
        (rplaca::file-debug-event (event-name &rest payload)
         (bt:with-lock-held (debug-lock)
           (push (cons event-name payload) debug-events)))
      (unwind-protect
           (progn
             (is-true (funcall proxy 1))
             (is-true
              (bt:wait-on-semaphore callback-entered :timeout 2.0))
             (is-true (funcall proxy 2))
             (is-false (funcall proxy 3))
             (is (= (1+ dropped-before)
                    (rplaca::runtime-callback-dispatch-dropped-count)))
             (is (= 2
                    (rplaca::runtime-callback-dispatch-pending-count))))
        (bt:signal-semaphore callback-release))
      (is-true
       (rplaca::wait-for-runtime-callback-dispatch-idle :timeout 2.0))
      (is (equal '(1 2)
                 (nreverse
                  (bt:with-lock-held (values-lock)
                    (copy-list delivered)))))
      (is (find "runtime-callback-dropped"
                (bt:with-lock-held (debug-lock)
                  (copy-list debug-events))
                :key #'car
                :test #'string=)))))

(test run-pipeline-prompt-attaches-stage-structured-output
  "Pipeline stage schemas propagate parsed JSON through the final prompt result."
  (with-interop-test-state ()
    (rplaca:register-pipeline-definition
     "interop-schema"
     :stages
     (list
      '(:name "plan"
        :prompt "Return a JSON object."
        :output-schema "{\"type\":\"object\",\"properties\":{\"status\":{\"type\":\"string\"}},\"required\":[\"status\"],\"additionalProperties\":false}")))
    (with-interop-function-override
        (rplaca::provider-request-streaming
         (provider messages callback
                   &key model max-tokens tools reasoning-effort system-prompt)
         (declare (ignore provider messages callback model max-tokens tools
                          reasoning-effort system-prompt))
         (let ((state (rplaca::make-stream-state)))
           (bt:with-lock-held ((rplaca::stream-state-lock state))
             (setf (rplaca::stream-state-content-blocks state)
                   (reverse
                    (list (rplaca::canonical-text-block
                           "{\"status\":\"pipeline-ok\"}")))
                   (rplaca::stream-state-stop-reason state) "end_turn"
                   (rplaca::stream-state-done-p state) t))
           state))
      (let* ((result (rplaca:run-pipeline-prompt
                      "Do the work."
                      "interop-schema"
                      :provider :zai
                      :model "glm-5"))
             (structured (rplaca:prompt-run-result-structured-output result)))
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
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (let ((state (rplaca::make-stream-state)))
             (bt:with-lock-held ((rplaca::stream-state-lock state))
               (setf (rplaca::stream-state-content-blocks state)
                     (reverse
                      (list (rplaca::canonical-text-block "interop complete")))
                     (rplaca::stream-state-usage state)
                     '((:input-tokens . 2) (:output-tokens . 4))
                     (rplaca::stream-state-stop-reason state) "end_turn"
                     (rplaca::stream-state-done-p state) t))
             state))
        (let* ((initialize
                 (rplaca:handle-interop-request
                  '((:method . "initialize"))))
               (started
                 (rplaca:handle-interop-request
                  `((:method . "thread.start")
                    (:params . ((:session-name . "interop-thread")
                                (:cwd . ,(namestring cwd))
                                (:model-provider . "zai")
                                (:model . "glm-5"))))))
               (thread-id (getf started :id))
               (run-result
                 (rplaca:handle-interop-request
                  `((:method . "thread.run")
                    (:params . ((:thread-id . ,thread-id)
                                (:input . "Continue"))))))
               (read-result
                 (rplaca:handle-interop-request
                  `((:method . "thread.read")
                    (:params . ((:thread-id . ,thread-id)
                                (:include-turns . t))))))
               (list-result
                 (rplaca:handle-interop-request
                  `((:method . "thread.list")
                    (:params . ((:cwd . ,(namestring cwd)))))))
               (forked
                 (rplaca:handle-interop-request
                  `((:method . "thread.fork")
                    (:params . ((:thread-id . ,thread-id)
                                (:name . "forked-thread"))))))
               (resumed
                 (rplaca:handle-interop-request
                  `((:method . "thread.resume")
                    (:params . ((:thread-id . ,thread-id)))))))
          (is (= 1 (getf initialize :protocol-version)))
          (is (string= "rplaca-app-server"
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
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages model max-tokens tools
                            reasoning-effort system-prompt))
           (let ((state (rplaca::make-stream-state)))
             (bt:with-lock-held ((rplaca::stream-state-lock state))
               (setf (rplaca::stream-state-text state) "partial"))
             (funcall callback state)
             (bt:with-lock-held ((rplaca::stream-state-lock state))
               (setf (rplaca::stream-state-content-blocks state)
                     (reverse (list (rplaca::canonical-text-block
                                     "final answer")))
                     (rplaca::stream-state-stop-reason state) "end_turn"
                     (rplaca::stream-state-done-p state) t))
             state))
        (let* ((thread (rplaca:start-interop-thread
                        :session-name "stream-thread"
                        :cwd cwd
                        :provider "zai"
                        :model "glm-5"))
               (result (rplaca:run-interop-thread
                        (rplaca:interop-thread-id thread)
                        "Stream a reply."
                        :event-callback (lambda (event)
                                          (push event events)))))
          (is (string= "final answer"
                       (rplaca:prompt-run-result-final-text result)))
          (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
          (let ((ordered-events (nreverse events)))
            (is (equal '("turn.started" "assistant.chunk" "turn.completed")
                       (mapcar (lambda (event) (getf event :event))
                               ordered-events)))))))))

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
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (incf request-count)
           (if (= request-count 1)
               (let ((state (rplaca::make-stream-state)))
                 (bt:with-lock-held ((rplaca::stream-state-lock state))
                   (setf (rplaca::stream-state-content-blocks state)
                         (reverse
                          (list
                           `((:type . "tool_use")
                             (:id . "call-1")
                             (:name . "lisp_eval")
                             (:input . ((:code . "(+ 2 3)"))))))
                         (rplaca::stream-state-stop-reason state) "tool_use"
                         (rplaca::stream-state-done-p state) t))
                 state)
               (let ((state (rplaca::make-stream-state)))
                 (bt:with-lock-held ((rplaca::stream-state-lock state))
                   (setf (rplaca::stream-state-content-blocks state)
                         (reverse (list (rplaca::canonical-text-block
                                         "done")))
                         (rplaca::stream-state-stop-reason state) "end_turn"
                         (rplaca::stream-state-done-p state) t))
                 state)))
        (let* ((thread (rplaca:start-interop-thread
                        :session-name "tool-stream-thread"
                        :cwd cwd
                        :provider "zai"
                        :model "glm-5"))
               (result (rplaca:run-interop-thread
                        (rplaca:interop-thread-id thread)
                        "Use one tool."
                        :event-callback (lambda (event)
                                          (push event events)))))
          (is (string= "done"
                       (rplaca:prompt-run-result-final-text result)))
          (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
          (let ((ordered-events (nreverse events)))
            (is (equal '("turn.started"
                         "tool.call"
                         "tool.result"
                         "turn.completed")
                       (mapcar (lambda (event) (getf event :event))
                               ordered-events)))))))))

(test interop-local-client-dispatches-through-the-request-handler
  "The minimal Lisp client exposes the same request surface in-process."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "client-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd))))
      (declare (ignore _keep))
      (let* ((client (rplaca:make-interop-local-client))
             (started
               (rplaca:interop-client-call
                client
                "thread.start"
                `((:session-name . "client-thread")
                  (:cwd . ,(namestring cwd))
                  (:ephemeral . t))))
             (listed
               (rplaca:interop-client-call client "thread.list")))
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
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort
                            system-prompt))
           (incf request-count)
           (let ((state (rplaca::make-stream-state)))
             state))
        (let* ((thread (rplaca:start-interop-thread
                        :session-name "async-thread"
                        :cwd cwd
                        :provider "zai"
                        :model "glm-5"))
               (turn (rplaca:start-interop-turn
                      (rplaca:interop-thread-id thread)
                      "Work until interrupted."
                      :event-callback (lambda (event)
                                        (bt:with-lock-held (events-lock)
                                          (push event events)))))
               (turn-id (rplaca:interop-turn-id turn)))
          (interop-wait-for-turn-status turn-id '("running"))
          (interop-wait-for-event-type
           (lambda ()
             (bt:with-lock-held (events-lock)
               (copy-list events)))
           "turn.started")
          (rplaca:interrupt-interop-turn turn-id)
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

(test concurrent-turn-start-admits-exactly-one-turn-per-thread
  "The active-turn check and registration are one atomic operation."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "turn-race-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (thread (rplaca:start-interop-thread
                    :session-name "turn-race-thread"
                    :cwd cwd
                    :provider "zai"
                    :model "glm-5"))
           (thread-id (rplaca:interop-thread-id thread))
           (thread-table rplaca::*interop-thread-table*)
           (turn-table rplaca::*interop-turn-table*)
           (start-ready (bt:make-semaphore :name "turn-start-ready"))
           (start-gate (bt:make-semaphore :name "turn-start-gate"))
           (second-registration (bt:make-semaphore
                                 :name "second-turn-registration"))
           (registration-lock (bt:make-lock "turn-registration-test"))
           (registration-count 0)
           (result-lock (bt:make-lock "turn-start-results"))
           (successes nil)
           (failures nil)
           (attempts nil)
           (original-register
             (symbol-function 'rplaca::register-interop-turn)))
      (declare (ignore _keep))
      (with-interop-function-override
          (rplaca::register-interop-turn (candidate)
           ;; A split check/register implementation is forced to let both
           ;; contenders finish their check before either inserts its turn.
           ;; The atomic production path never calls this separate-registration
           ;; helper.
           (let ((registration-index
                   (bt:with-lock-held (registration-lock)
                     (incf registration-count))))
             (if (= registration-index 1)
                 (unless (bt:wait-on-semaphore second-registration
                                               :timeout 2.0)
                   (error "Timed out waiting for the competing registration"))
                 (bt:signal-semaphore second-registration)))
           (funcall original-register candidate))
        (with-interop-function-override
            (rplaca::provider-request-streaming
             (provider messages callback
                       &key model max-tokens tools reasoning-effort system-prompt)
             (declare (ignore provider messages callback model max-tokens tools
                              reasoning-effort system-prompt))
             (rplaca::make-stream-state))
          (labels ((attempt-turn-start ()
                     (let ((rplaca::*interop-thread-table* thread-table)
                           (rplaca::*interop-turn-table* turn-table))
                       (bt:signal-semaphore start-ready)
                       (unless (bt:wait-on-semaphore start-gate :timeout 2.0)
                         (error "Timed out waiting for the turn-start gate"))
                       (handler-case
                           (let ((turn (rplaca:start-interop-turn
                                        thread-id "Race this turn.")))
                             (bt:with-lock-held (result-lock)
                               (push turn successes)))
                         (error (condition)
                           (bt:with-lock-held (result-lock)
                             (push condition failures)))))))
            (setf attempts
                  (list (bt:make-thread #'attempt-turn-start
                                        :name "interop-turn-race-1")
                        (bt:make-thread #'attempt-turn-start
                                        :name "interop-turn-race-2")))
            (is (bt:wait-on-semaphore start-ready :timeout 2.0))
            (is (bt:wait-on-semaphore start-ready :timeout 2.0))
            (bt:signal-semaphore start-gate :count 2)
            (dolist (attempt attempts)
              (bt:join-thread attempt))
            (is (= 1 (length successes)))
            (is (= 1 (length failures)))
            (is (search "already has an active turn"
                        (format nil "~A" (first failures))))
            (let* ((winner (first successes))
                   (winner-id (rplaca:interop-turn-id winner)))
              (rplaca:interrupt-interop-turn winner-id)
              (interop-wait-for-turn-status winner-id '("interrupted"))
              (bt:join-thread (rplaca::interop-turn-runner-thread winner)))))))))

(test interop-registry-operations-use-one-lock-and-return-valid-snapshots
  "Registry reads, writes, and membership snapshots share the registry lock."
  (with-interop-test-state ()
    (let* ((thread-table rplaca::*interop-thread-table*)
           (turn-table rplaca::*interop-turn-table*)
           (existing-thread (rplaca::make-interop-thread :id "thread-1"))
           (added-thread (rplaca::make-interop-thread :id "thread-2"))
           (existing-turn (rplaca::make-interop-turn
                           :id "turn-1" :thread-id "thread-1"
                           :status :succeeded))
           (added-turn (rplaca::make-interop-turn
                        :id "turn-2" :thread-id "thread-2"
                        :status :queued))
           (started (bt:make-semaphore :name "registry-operation-started"))
           (finished (bt:make-semaphore :name "registry-operation-finished"))
           (reader-result nil)
           (writer nil)
           (reader nil))
      (rplaca::register-interop-thread existing-thread)
      (rplaca::register-interop-turn existing-turn)
      (bt:with-lock-held (rplaca::*interop-registry-lock*)
        (setf writer
              (bt:make-thread
               (lambda ()
                 (let ((rplaca::*interop-thread-table* thread-table)
                       (rplaca::*interop-turn-table* turn-table))
                   (bt:signal-semaphore started)
                   (rplaca::register-interop-thread added-thread)
                   (rplaca::register-interop-turn added-turn)
                   (bt:signal-semaphore finished)))
               :name "interop-registry-writer")
              reader
              (bt:make-thread
               (lambda ()
                 (let ((rplaca::*interop-thread-table* thread-table)
                       (rplaca::*interop-turn-table* turn-table))
                   (bt:signal-semaphore started)
                   (setf reader-result
                         (list
                          :thread (rplaca::find-live-interop-thread "thread-1")
                          :turn (rplaca::find-interop-turn "turn-1")
                          :threads (rplaca::interop-thread-registry-snapshot)
                          :turns (rplaca::interop-turn-registry-snapshot)))
                   (bt:signal-semaphore finished)))
               :name "interop-registry-reader"))
        (is (bt:wait-on-semaphore started :timeout 2.0))
        (is (bt:wait-on-semaphore started :timeout 2.0))
        (is (null (bt:wait-on-semaphore finished :timeout 0.05))))
      (bt:join-thread writer)
      (bt:join-thread reader)
      (is (eq existing-thread (getf reader-result :thread)))
      (is (eq existing-turn (getf reader-result :turn)))
      (is (every #'rplaca::interop-thread-p
                 (getf reader-result :threads)))
      (is (every #'rplaca::interop-turn-p
                 (getf reader-result :turns)))
      (is (= 2 (hash-table-count thread-table)))
      (is (= 2 (hash-table-count turn-table)))
      (is (= 2 (length (rplaca::interop-thread-registry-snapshot))))
      (is (= 2 (length (rplaca::interop-turn-registry-snapshot)))))))

(test concurrent-synchronous-runs-share-one-thread-reservation
  "A blocked synchronous run rejects a second synchronous buffer mutation."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "sync-reservation-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (thread (rplaca:start-interop-thread
                    :session-name "sync-reservation"
                    :cwd cwd
                    :ephemeral t
                    :provider "zai"
                    :model "glm-5"))
           (thread-id (rplaca:interop-thread-id thread))
           (thread-table rplaca::*interop-thread-table*)
           (turn-table rplaca::*interop-turn-table*)
           (provider-entered (bt:make-semaphore :name "sync-provider-entered"))
           (provider-release (bt:make-semaphore :name "sync-provider-release"))
           (provider-lock (bt:make-lock "sync-provider-count"))
           (provider-call-count 0)
           (first-result nil)
           (first-error nil)
           (second-error nil)
           (worker nil))
      (declare (ignore _keep))
      (with-interop-function-override
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (bt:with-lock-held (provider-lock)
             (incf provider-call-count))
           (bt:signal-semaphore provider-entered)
           (unless (bt:wait-on-semaphore provider-release :timeout 5.0)
             (error "Timed out waiting to release the synchronous provider"))
           (make-completed-interop-test-stream-state "first complete"))
        (unwind-protect
             (progn
               (setf worker
                     (bt:make-thread
                      (lambda ()
                        (let ((rplaca::*interop-thread-table* thread-table)
                              (rplaca::*interop-turn-table* turn-table))
                          (handler-case
                              (setf first-result
                                    (rplaca:run-interop-thread
                                     thread-id "First run"))
                            (error (condition)
                              (setf first-error condition)))))
                      :name "interop-sync-reservation-owner"))
               (is (bt:wait-on-semaphore provider-entered :timeout 2.0))
               (is-true
                (rplaca::interop-thread-execution-reserved-p thread))
               (handler-case
                   (rplaca:run-interop-thread thread-id "Second run")
                 (error (condition)
                   (setf second-error condition)))
               (is (typep second-error 'error))
               (is (search "already has an active turn"
                           (format nil "~A" second-error)))
               (is (= 1 (bt:with-lock-held (provider-lock)
                          provider-call-count)))
               (bt:signal-semaphore provider-release)
               (bt:join-thread worker)
               (setf worker nil)
               (is (null first-error))
               (is (string= "first complete"
                            (rplaca:prompt-run-result-final-text
                             first-result)))
               (is-false
                (rplaca::interop-thread-execution-reserved-p thread)))
          (bt:signal-semaphore provider-release)
          (when worker
            (bt:join-thread worker)))))))

(test asynchronous-turn-reservation-blocks-and-then-releases-sync-run
  "An async turn excludes thread.run until its outer cleanup releases ownership."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "async-sync-reservation-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (thread (rplaca:start-interop-thread
                    :session-name "async-sync-reservation"
                    :cwd cwd
                    :ephemeral t
                    :provider "zai"
                    :model "glm-5"))
           (thread-id (rplaca:interop-thread-id thread))
           (provider-entered (bt:make-semaphore :name "async-provider-entered"))
           (provider-lock (bt:make-lock "async-provider-count"))
           (provider-call-count 0)
           (turn nil)
           (sync-error nil))
      (declare (ignore _keep))
      (with-interop-function-override
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (let ((call-index
                   (bt:with-lock-held (provider-lock)
                     (incf provider-call-count))))
             (if (= call-index 1)
                 (progn
                   (bt:signal-semaphore provider-entered)
                   (rplaca::make-stream-state))
                 (make-completed-interop-test-stream-state "after async"))))
        (unwind-protect
             (progn
               (setf turn (rplaca:start-interop-turn
                           thread-id "Hold the async reservation"))
               (is (bt:wait-on-semaphore provider-entered :timeout 2.0))
               (interop-wait-for-turn-status
                (rplaca:interop-turn-id turn) '("running"))
               (handler-case
                   (rplaca:run-interop-thread thread-id "Must be rejected")
                 (error (condition)
                   (setf sync-error condition)))
               (is (typep sync-error 'error))
               (is (search "already has an active turn"
                           (format nil "~A" sync-error)))
               (is (= 1 (bt:with-lock-held (provider-lock)
                          provider-call-count)))
               (rplaca:interrupt-interop-turn
                (rplaca:interop-turn-id turn))
               (interop-wait-for-turn-status
                (rplaca:interop-turn-id turn) '("interrupted"))
               (bt:join-thread (rplaca::interop-turn-runner-thread turn))
               (is-false
                (rplaca::interop-thread-execution-reserved-p thread))
               (let ((result
                       (rplaca:run-interop-thread
                        thread-id "Run after async cleanup")))
                 (is (string= "after async"
                              (rplaca:prompt-run-result-final-text result))))
               (is (= 2 (bt:with-lock-held (provider-lock)
                          provider-call-count)))
               (setf turn nil))
          (when turn
            (ignore-errors
              (rplaca:interrupt-interop-turn
               (rplaca:interop-turn-id turn)))
            (let ((runner (rplaca::interop-turn-runner-thread turn)))
              (when runner
                (bt:join-thread runner)))))))))

(test concurrent-persistent-start-is-single-flight-before-buffer-construction
  "Two starts for one session return one object and construct one live buffer."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "start-race-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (session (rplaca::load-or-create-session
                     "start-race" :working-directory cwd))
           (thread-table rplaca::*interop-thread-table*)
           (turn-table rplaca::*interop-turn-table*)
           (first-load-entered
             (bt:make-semaphore :name "start-race-first-load"))
           (release-first-load
             (bt:make-semaphore :name "start-race-release-load"))
           (state-lock (bt:make-lock "start-race-state"))
           (load-count 0)
           (buffer-count 0)
           (results nil)
           (errors nil)
           (workers nil)
           (original-make-chat-buffer
             (symbol-function 'rplaca::make-chat-buffer)))
      (declare (ignore _keep))
      (with-interop-function-override
          (rplaca::load-or-create-session (name &key working-directory)
           (declare (ignore name working-directory))
           (let ((index (bt:with-lock-held (state-lock)
                          (incf load-count))))
             (when (= index 1)
               (bt:signal-semaphore first-load-entered)
               (unless (bt:wait-on-semaphore release-first-load :timeout 5.0)
                 (error "Timed out releasing first persistent start")))
             session))
        (with-interop-function-override
            (rplaca::make-chat-buffer (name &rest arguments)
             (bt:with-lock-held (state-lock)
               (incf buffer-count))
             (apply original-make-chat-buffer name arguments))
          (labels ((attempt-start ()
                     (let ((rplaca::*interop-thread-table* thread-table)
                           (rplaca::*interop-turn-table* turn-table))
                       (handler-case
                           (let ((result
                                   (rplaca:start-interop-thread
                                    :session-name "start-race"
                                    :cwd cwd
                                    :provider "zai"
                                    :model "glm-5")))
                             (bt:with-lock-held (state-lock)
                               (push result results)))
                         (error (condition)
                           (bt:with-lock-held (state-lock)
                             (push condition errors)))))))
            (unwind-protect
                 (progn
                   (setf workers
                         (list
                          (bt:make-thread #'attempt-start
                                          :name "interop-start-race-1")
                          (bt:make-thread #'attempt-start
                                          :name "interop-start-race-2")))
                   (is-true
                    (bt:wait-on-semaphore first-load-entered :timeout 2.0))
                   ;; The second caller is now either waiting for the start
                   ;; single-flight or about to do so.
                   (sleep 0.02)
                   (bt:signal-semaphore release-first-load)
                   (dolist (worker workers)
                     (bt:join-thread worker))
                   (setf workers nil)
                   (is (null errors))
                   (is (= 2 (length results)))
                   (is (eq (first results) (second results)))
                   (is (= 1 buffer-count))
                   (is (= 1 (hash-table-count thread-table)))
                   (is (eq (first results)
                           (gethash (rplaca:interop-thread-id
                                     (first results))
                                    thread-table))))
              (bt:signal-semaphore release-first-load)
              (dolist (worker workers)
                (bt:join-thread worker)))))))))

(test concurrent-resume-is-single-flight-before-session-buffer-construction
  "Concurrent resumes converge before a second mutable session buffer exists."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "resume-race-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (source (rplaca:start-interop-thread
                    :session-name "resume-race"
                    :cwd cwd
                    :provider "zai"
                    :model "glm-5"))
           (thread-id (rplaca:interop-thread-id source))
           (session (buffer-session (rplaca:interop-thread-buffer source)))
           (thread-table rplaca::*interop-thread-table*)
           (turn-table rplaca::*interop-turn-table*)
           (load-ready (bt:make-semaphore :name "resume-load-ready"))
           (load-gate (bt:make-semaphore :name "resume-load-gate"))
           (state-lock (bt:make-lock "resume-race-state"))
           (created-buffers nil)
           (results nil)
           (errors nil)
           (workers nil))
      (declare (ignore _keep))
      (clrhash thread-table)
      (with-interop-function-override
          (rplaca::load-session (designator &key agent-name)
           (declare (ignore designator))
           (let ((buffer
                   (rplaca::make-chat-buffer
                    "resume-race"
                    :agent-name agent-name
                    :working-directory (rplaca:session-working-directory
                                        session)
                    :session session
                    :session-persistence-mode :persistent)))
             (bt:with-lock-held (state-lock)
               (push buffer created-buffers))
             (bt:signal-semaphore load-ready)
             (unless (bt:wait-on-semaphore load-gate :timeout 5.0)
               (error "Timed out releasing serialized resume load"))
             buffer))
        (labels ((attempt-resume ()
                   (let ((rplaca::*interop-thread-table* thread-table)
                         (rplaca::*interop-turn-table* turn-table))
                     (handler-case
                         (let ((result
                                 (rplaca:resume-interop-thread thread-id)))
                           (bt:with-lock-held (state-lock)
                             (push result results)))
                       (error (condition)
                         (bt:with-lock-held (state-lock)
                           (push condition errors)))))))
          (unwind-protect
               (progn
                 (setf workers
                       (list
                        (bt:make-thread #'attempt-resume
                                        :name "interop-resume-race-1")
                        (bt:make-thread #'attempt-resume
                                        :name "interop-resume-race-2")))
                 (is (bt:wait-on-semaphore load-ready :timeout 2.0))
                 (sleep 0.02)
                 (bt:signal-semaphore load-gate)
                 (dolist (worker workers)
                   (bt:join-thread worker))
                 (setf workers nil)
                 (is (null errors))
                 (is (= 2 (length results)))
                 (is (eq (first results) (second results)))
                 (is (= 1 (hash-table-count thread-table)))
                 (is (eq (first results)
                         (gethash thread-id thread-table)))
                 (is (= 1 (length created-buffers)))
                 (is-false
                  (rplaca:buffer-disposed-p
                   (rplaca:interop-thread-buffer (first results)))))
            (bt:signal-semaphore load-gate)
            (dolist (worker workers)
              (bt:join-thread worker))))))))

(test interrupt-cancels-outside-turn-lock-and-allows-reentrant-read
  "Stream cancellation may synchronously read its turn without lock inversion."
  (with-interop-test-state ()
    (let* ((turn-table rplaca::*interop-turn-table*)
           (stream (rplaca::make-stream-state))
           (events nil)
           (turn (rplaca::make-interop-turn
                  :id "reentrant-interrupt-turn"
                  :thread-id "reentrant-thread"
                  :status :running
                  :created-at (get-universal-time)
                  :updated-at (get-universal-time)
                  :current-stream-state stream
                  :event-callback (lambda (event) (push event events))))
           (read-finished (bt:make-semaphore :name "interrupt-read-finished"))
           (reader nil)
           (reader-summary nil)
           (reader-error nil)
           (read-during-cancel-p nil)
           (cancel-stop-reason nil))
      (rplaca::register-interop-turn turn)
      (with-interop-function-override
          (rplaca::cancel-stream-state (state &key stop-reason)
           (declare (ignore state))
           (setf cancel-stop-reason stop-reason
                 reader
                 (bt:make-thread
                  (lambda ()
                    (let ((rplaca::*interop-turn-table* turn-table))
                      (handler-case
                          (setf reader-summary
                                (rplaca:read-interop-turn
                                 "reentrant-interrupt-turn"))
                        (error (condition)
                          (setf reader-error condition)))
                      (bt:signal-semaphore read-finished)))
                  :name "interop-interrupt-reentrant-reader"))
           (setf read-during-cancel-p
                 (not (null (bt:wait-on-semaphore read-finished
                                                   :timeout 1.0))))
           t)
        (let ((summary
                (rplaca:interrupt-interop-turn
                 "reentrant-interrupt-turn")))
          (when reader
            (bt:join-thread reader))
          (is-true read-during-cancel-p)
          (is (null reader-error))
          (is (string= "running" (getf reader-summary :status)))
          (is (string= "cancelled" cancel-stop-reason))
          (is-true (getf summary :interrupt-requested-p))
          (is (equal '("turn.interrupted")
                     (mapcar (lambda (event) (getf event :event)) events))))))))

(test async-runner-creation-failure-terminalizes-turn-and-releases-thread
  "A spawn failure leaves a failed diagnostic turn and an idle live thread."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "spawn-failure-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (thread (rplaca:start-interop-thread
                    :session-name "spawn-failure"
                    :cwd cwd
                    :ephemeral t
                    :provider "zai"
                    :model "glm-5"))
           (thread-id (rplaca:interop-thread-id thread))
           (callback-entered
             (bt:make-semaphore :name "spawn failure callback entered"))
           (callback-release
             (bt:make-semaphore :name "release spawn failure callback"))
           (events-lock (bt:make-lock "spawn failure callback events"))
           (events nil)
           (start-error nil))
      (declare (ignore _keep))
      (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
      (unwind-protect
           (progn
             (with-interop-function-override
                 (rplaca::make-interop-turn-runner-thread (function name)
                  (declare (ignore function name))
                  (error "forced interop runner creation failure"))
               (handler-case
                   (rplaca:start-interop-turn
                    thread-id "This cannot start"
                    :event-callback
                    (lambda (event)
                      (bt:with-lock-held (events-lock)
                        (push (rplaca::copy-runtime-owned-data event) events))
                      (bt:signal-semaphore callback-entered)
                      (bt:wait-on-semaphore callback-release)))
                 (error (condition)
                   (setf start-error condition))))
             ;; Runner construction and thread ownership settle synchronously;
             ;; the bounded public callback is deliberately not joined.
             (is (typep start-error 'error))
             (is (search "forced interop runner creation failure"
                         (format nil "~A" start-error)))
             (is-true
              (bt:wait-on-semaphore callback-entered :timeout 2.0))
             (is (= 1 (hash-table-count rplaca::*interop-turn-table*)))
             (let* ((turn (first (rplaca::interop-turn-registry-snapshot)))
                    (summary (rplaca:read-interop-turn
                              (rplaca:interop-turn-id turn))))
               (is (string= "failed" (getf summary :status)))
               (is (search "forced interop runner creation failure"
                           (getf summary :error)))
               (is (getf summary :finished-at))
               (is (null (rplaca::interop-turn-runner-thread turn))))
             (is-false
              (rplaca::interop-thread-execution-reserved-p thread))
             (is (= 1
                    (rplaca::runtime-callback-dispatch-pending-count)))
             (is (equal
                  '("turn.failed")
                  (mapcar
                   (lambda (event) (getf event :event))
                   (nreverse
                    (bt:with-lock-held (events-lock)
                      (copy-list events)))))))
        (bt:signal-semaphore callback-release)
        (is-true
         (rplaca::wait-for-runtime-callback-dispatch-idle :timeout 2.0)))
      (with-interop-function-override
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (make-completed-interop-test-stream-state "retry succeeded"))
        (let ((result
                (rplaca:run-interop-thread thread-id "Retry synchronously")))
          (is (string= "retry succeeded"
                       (rplaca:prompt-run-result-final-text result))))))))

(test async-interrupt-at-prompt-loop-gap-prevents-tool-and-provider-work
  "An interrupt after stream completion is observed before the tool loop."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "turn-gap-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (thread (rplaca:start-interop-thread
                    :session-name "turn-gap-interrupt"
                    :cwd cwd
                    :ephemeral t
                    :provider "zai"
                    :model "glm-5"))
           (thread-id (rplaca:interop-thread-id thread))
           (response-ready (bt:make-semaphore :name "turn-gap-response-ready"))
           (release-response (bt:make-semaphore :name "turn-gap-release"))
           (events-lock (bt:make-lock "turn-gap-events"))
           (events nil)
           (provider-call-count 0)
           (turn nil)
           (original-prompt-request-once
             (symbol-function 'rplaca::prompt-request-once)))
      (declare (ignore _keep))
      (with-interop-function-override
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (incf provider-call-count)
           (if (= provider-call-count 1)
               (let ((state (rplaca::make-stream-state)))
                 (bt:with-lock-held ((rplaca::stream-state-lock state))
                   (setf (rplaca::stream-state-content-blocks state)
                         (reverse
                          (list
                           `((:type . "tool_use")
                             (:id . "gap-call")
                             (:name . "lisp_eval")
                             (:input . ((:code . "(+ 2 3)"))))))
                         (rplaca::stream-state-stop-reason state) "tool_use"
                         (rplaca::stream-state-done-p state) t))
                 state)
               (make-completed-interop-test-stream-state
                "work continued after interruption")))
        (with-interop-function-override
            (rplaca::prompt-request-once
             (buffer &key event-callback stream-state-callback
                          cancel-requested-p)
             (let ((values
                     (multiple-value-list
                      (funcall original-prompt-request-once
                               buffer
                               :event-callback event-callback
                               :stream-state-callback stream-state-callback
                               :cancel-requested-p cancel-requested-p))))
               (when (= provider-call-count 1)
                 (bt:signal-semaphore response-ready)
                 (unless (bt:wait-on-semaphore release-response :timeout 5.0)
                   (error "Timed out releasing completed interop response")))
               (values-list values)))
          (unwind-protect
               (progn
                 (setf turn
                       (rplaca:start-interop-turn
                        thread-id
                        "Interrupt after the first provider response."
                        :event-callback
                        (lambda (event)
                          (bt:with-lock-held (events-lock)
                            (push event events)))))
                 (is (bt:wait-on-semaphore response-ready :timeout 2.0))
                 (rplaca:interrupt-interop-turn
                  (rplaca:interop-turn-id turn))
                 (bt:signal-semaphore release-response)
                 (bt:join-thread (rplaca::interop-turn-runner-thread turn))
                 (let* ((summary
                          (rplaca:read-interop-turn
                           (rplaca:interop-turn-id turn)))
                        (ordered-events
                          (nreverse
                           (bt:with-lock-held (events-lock)
                             (copy-list events)))))
                   (is (string= "interrupted" (getf summary :status)))
                   (is (string= "cancelled" (getf summary :stop-reason)))
                   (is (= 1 provider-call-count))
                   (is (equal '("turn.started"
                                "turn.interrupted"
                                "turn.completed")
                              (mapcar (lambda (event) (getf event :event))
                                      ordered-events)))
                   (is-false
                    (find "tool.call" ordered-events
                          :key (lambda (event) (getf event :event))
                          :test #'string=))
                   (is-false
                    (rplaca::interop-thread-execution-reserved-p thread)))
                 (setf turn nil))
            (bt:signal-semaphore release-response)
            (when turn
              (ignore-errors
                (rplaca:interrupt-interop-turn
                 (rplaca:interop-turn-id turn)))
              (let ((runner (rplaca::interop-turn-runner-thread turn)))
                (when runner
                  (bt:join-thread runner))))))))))

(test async-failure-callback-error-is-contained-and-runner-settles
  "A failing dispatched callback cannot replace the runner's own failure."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "callback-failure-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (thread (rplaca:start-interop-thread
                    :session-name "callback-failure"
                    :cwd cwd
                    :ephemeral t
                    :provider "zai"
                    :model "glm-5"))
           (thread-id (rplaca:interop-thread-id thread))
           (callback-count 0)
           (escaped-condition nil)
           (turn nil)
           (original-thread-constructor
             (symbol-function 'rplaca::make-interop-turn-runner-thread)))
      (declare (ignore _keep))
      (with-interop-function-override
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (error "forced provider failure"))
        (with-interop-function-override
            (rplaca::make-interop-turn-runner-thread (function name)
             (funcall original-thread-constructor
                      (lambda ()
                        (handler-case
                            (funcall function)
                          (condition (condition)
                            (setf escaped-condition condition))))
                      name))
          (setf turn
                (rplaca:start-interop-turn
                 thread-id
                 "Fail independently of callback delivery."
                 :event-callback
                 (lambda (event)
                   (incf callback-count)
                   (error "forced event callback failure for ~A"
                          (getf event :event)))))
          (bt:join-thread (rplaca::interop-turn-runner-thread turn))))
      (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
      (let ((summary
              (rplaca:read-interop-turn
               (rplaca:interop-turn-id turn))))
        (is (null escaped-condition))
        (is (= 2 callback-count))
        (is (string= "failed" (getf summary :status)))
        (is (search "forced provider failure"
                    (getf summary :error)))
        (is (getf summary :finished-at))
        (is-false (rplaca::interop-thread-execution-reserved-p thread))))))

(test blocked-terminal-callback-cannot-retain-async-runner
  "A terminal client callback may block forever without owning the turn runner."
  (with-interop-test-state ()
    (let* ((cwd (uiop:ensure-directory-pathname
                 (interop-temp-directory "blocked-terminal-callback-cwd")))
           (_keep (ensure-directories-exist (merge-pathnames #P".keep" cwd)))
           (callback-entered
             (bt:make-semaphore :name "blocked terminal callback entered"))
           (callback-release
             (bt:make-semaphore :name "release blocked terminal callback"))
           (events-lock (bt:make-lock "blocked terminal callback events"))
           (events nil)
           (turn nil))
      (declare (ignore _keep))
      (is-true (rplaca::wait-for-runtime-callback-dispatch-idle))
      (with-interop-function-override
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (make-completed-interop-test-stream-state "terminal-safe"))
        (let* ((thread (rplaca:start-interop-thread
                        :session-name "blocked-terminal-callback"
                        :cwd cwd
                        :ephemeral t
                        :provider "zai"
                        :model "glm-5"))
               (thread-id (rplaca:interop-thread-id thread)))
          (unwind-protect
               (progn
                 (setf turn
                       (rplaca:start-interop-turn
                        thread-id
                        "Finish despite the terminal callback."
                        :event-callback
                        (lambda (event)
                          (bt:with-lock-held (events-lock)
                            (push (rplaca::copy-runtime-owned-data event)
                                  events))
                          (when (string= "turn.completed" (getf event :event))
                            (bt:signal-semaphore callback-entered)
                            (bt:wait-on-semaphore callback-release)))))
                 (is-true
                  (bt:wait-on-semaphore callback-entered :timeout 2.0))
                 (let ((runner
                         (rplaca::interop-turn-runner-thread turn)))
                   (is-true
                    (wait-for-interop-test-thread-exit runner :timeout 2.0)))
                 (let ((summary
                         (rplaca:read-interop-turn
                          (rplaca:interop-turn-id turn))))
                   (is (string= "succeeded" (getf summary :status)))
                   (is (string= "terminal-safe"
                                (getf summary :final-response))))
                 (is-true
                  (bt:with-lock-held ((rplaca::interop-turn-lock turn))
                    (rplaca::interop-turn-runner-finished-p turn)))
                 (is-false
                  (rplaca::interop-thread-execution-reserved-p thread))
                 (is (eq :external-callback
                         (rplaca::safe-reload-process-runtime-activity)))
                 (is (equal '("turn.started" "turn.completed")
                            (mapcar (lambda (event) (getf event :event))
                                    (nreverse
                                     (bt:with-lock-held (events-lock)
                                       (copy-list events)))))))
            (bt:signal-semaphore callback-release)
            (is-true
             (rplaca::wait-for-runtime-callback-dispatch-idle
              :timeout 2.0))))))))
