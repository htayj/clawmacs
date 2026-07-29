(in-package :rplaca/tests)

(in-suite subagent-package-suite)

(defmacro with-subagent-package-function-override ((name lambda-list &body implementation)
                                                   &body body)
  `(let ((original-function (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name)
                  (lambda ,lambda-list
                    ,@implementation))
            ,@body)
       (setf (symbol-function ',name) original-function))))

(defmacro with-subagent-package-state (&body body)
  "Run BODY with isolated package, agent, subagent, and tool registries."
  `(let* ((root (temp-package-test-directory "subagent-config"))
          (rplaca::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (rplaca::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (rplaca::*tool-table* (make-hash-table :test #'equal))
          (rplaca::*agent-definition-registry*
           (make-hash-table :test #'equal))
          (rplaca::*subagent-handle-counter* 0)
          (rplaca::*subagent-handles* (make-hash-table :test #'equal))
          (rplaca::*subagent-terminal-history-limit* 64)
          (rplaca::*subagent-terminal-sequence-counter* 0)
          (rplaca::*synchronous-subagent-runs*
           (make-hash-table :test #'eq))
          (rplaca::*subagent-registry-lock*
           (bt:make-lock "test-subagent-package-registry"))
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels* (default-package-test-channels))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
          (rplaca::*enabled-builtin-packages* nil))
     ,@body))

(test settled-subagent-history-is-bounded-without-evicting-active-work
  "Pruning retains active handles and releases resources held by old history."
  (with-subagent-package-state
    (let* ((rplaca::*subagent-terminal-history-limit* 1)
           (running
             (rplaca::make-subagent-handle
              :id "subagent-running"
              :prompt "running prompt"
              :status :running
              :thread (list :running-thread)
              :started-at 1))
           (cancelling
             (rplaca::make-subagent-handle
              :id "subagent-cancelling"
              :prompt "cancelling prompt"
              :status :cancelling
              :thread (list :cancelling-thread)
              :cancel-requested-p t
              :started-at 2))
           (old-result (list :large "old-result"))
           (old-thread (list :old-thread))
           (old
             (rplaca::make-subagent-handle
              :id "subagent-old"
              :prompt "old prompt"
              :status :succeeded
              :result old-result
              :error "old error metadata"
              :thread old-thread
              :started-at 3))
           (recent-result (list :large "recent-result"))
           (recent
             (rplaca::make-subagent-handle
              :id "subagent-recent"
              :prompt "recent prompt"
              :status :succeeded
              :result recent-result
              :thread (list :recent-thread)
              :started-at 4)))
      (mapc #'rplaca::register-subagent-handle
            (list running cancelling old))
      (rplaca::finish-subagent-worker old)
      (rplaca::register-subagent-handle recent)
      (rplaca::finish-subagent-worker recent)
      (is (eq running (rplaca:find-subagent "subagent-running")))
      (is (eq cancelling (rplaca:find-subagent "subagent-cancelling")))
      (is (eq recent (rplaca:find-subagent "subagent-recent")))
      (is-false (rplaca:find-subagent "subagent-old"))
      (is (= 3 (hash-table-count rplaca::*subagent-handles*)))
      (is (= 1
             (count-if #'rplaca::settled-subagent-handle-p
                       (rplaca:list-subagents))))
      (is-true
       (rplaca::subagent-handle-retained-resources-released-p old))
      (is (null (rplaca::subagent-handle-prompt old)))
      (is (null (rplaca::subagent-handle-result old)))
      (is (null (rplaca::subagent-handle-error old)))
      (is (null (rplaca::subagent-handle-thread old)))
      (is (equal '(:running-thread)
                 (rplaca::subagent-handle-thread running)))
      (is (equal '(:cancelling-thread)
                 (rplaca::subagent-handle-thread cancelling)))
      (multiple-value-bind (result status handle)
          (rplaca:wait-subagent recent :timeout 0)
        (is (eq recent handle))
        (is (eq :succeeded status))
        (is (eq recent-result result))))))

(test subagent-starts-participate-in-safe-reload-admission
  "Direct runs are observable and reload ownership rejects all new starts."
  (with-subagent-package-state
    (let ((entered (bt:make-semaphore :name "sync-subagent-entered"))
          (release (bt:make-semaphore :name "sync-subagent-release"))
          (runs rplaca::*synchronous-subagent-runs*)
          (worker nil)
          (result nil)
          (worker-error nil)
          (prompt-call-count 0))
      (with-subagent-package-function-override
          (rplaca::run-single-prompt (prompt &rest arguments)
           (declare (ignore prompt arguments))
           (incf prompt-call-count)
           (bt:signal-semaphore entered)
           (unless (bt:wait-on-semaphore release :timeout 5.0)
             (error "Timed out releasing synchronous subagent test run"))
           :synchronous-result)
        (unwind-protect
             (progn
               (setf worker
                     (bt:make-thread
                      (lambda ()
                        (let ((rplaca::*synchronous-subagent-runs* runs))
                          (handler-case
                              (setf result
                                    (rplaca:run-subagent "Hold admission"))
                            (error (condition)
                              (setf worker-error condition)))))
                      :name "synchronous-subagent-admission-test"))
               (is (bt:wait-on-semaphore entered :timeout 2.0))
               (is (= 1
                      (rplaca:active-synchronous-subagent-run-count)))
               (bt:signal-semaphore release)
               (bt:join-thread worker)
               (setf worker nil)
               (is (null worker-error))
               (is (eq :synchronous-result result))
               (is (= 0
                      (rplaca:active-synchronous-subagent-run-count))))
          (bt:signal-semaphore release)
          (when worker
            (bt:join-thread worker))))
      (let ((rplaca::*safe-reload-active-request* :reload-owned))
        (signals rplaca:runtime-admission-closed
          (rplaca:run-subagent "Refuse direct run"))
        (signals rplaca:runtime-admission-closed
          (rplaca:run-subagent-async "Refuse async run")))
      (is (= 1 prompt-call-count))
      (is (= 0 rplaca::*subagent-handle-counter*))
      (is (= 0 (hash-table-count rplaca::*subagent-handles*)))
      (is (= 0 (rplaca:active-synchronous-subagent-run-count))))))

(defun load-test-subagent-package ()
  "Enable and load the bundled subagent package."
  (set-package-enablement-scope "subagent" :global)
  (load-active-packages))

(defun subagent-package-tool-result (tool-name args)
  "Execute TOOL-NAME with ARGS and read its Lisp data result."
  (nth-value 0
    (rplaca::lisp-data-read
     (rplaca:execute-tool tool-name args))))

(defun make-subagent-package-completed-stream-state-response
    (stop-reason content-blocks &optional usage)
  "Return a completed stream state fixture for subagent package tests."
  (let ((state (rplaca::make-stream-state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-stop-reason state) stop-reason
            (rplaca::stream-state-content-blocks state)
            (reverse content-blocks)
            (rplaca::stream-state-usage state) usage
            (rplaca::stream-state-done-p state) t))
    state))

(test subagent-package-registers-agent-tools-and-prompt
  "Enabling subagent exposes package-scoped provider tools and prompt guidance."
  (with-subagent-package-state
    (load-test-subagent-package)
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<))
           (prompt (render-package-prompt-sections)))
      (dolist (name '("subagent_cancel" "subagent_run" "subagent_start"
                      "subagent_status" "subagent_wait"))
        (is (member name tool-names :test #'string=)))
      (is (search "Delegation with subagent" prompt))
      (is (search "subagent_run" prompt))
      (is (search "agent_spec" prompt))
      (is (search "fully custom transient agent" prompt))
      (is (search "Prefer these tools over `lisp_eval`" prompt)))))

(test subagent-run-tool-supports-registered-agent-and-overrides
  "subagent_run can delegate to registered agents with routing overrides."
  (with-subagent-package-state
    (let ((seen-provider nil)
          (seen-model nil)
          (seen-think-level nil))
      (rplaca:register-agent-definition
       "researcher"
       :provider :zai
       :model "glm-5"
       :personality-prompt "research personality")
      (load-test-subagent-package)
      (with-subagent-package-function-override
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore messages callback max-tokens tools system-prompt))
           (setf seen-provider provider
                 seen-model model
                 seen-think-level reasoning-effort)
           (make-subagent-package-completed-stream-state-response
            "end_turn"
            (list (rplaca::canonical-text-block "delegated answer"))))
        (rplaca::init-default-keymap)
        (let* ((result (subagent-package-tool-result
                        "subagent_run"
                        '(:prompt "Research this"
                          :agent "researcher"
                          :provider "openai-codex"
                          :model "gpt-5.3-codex"
                          :think_level "high")))
               (run-result (getf result :result)))
          (is (getf result :ok))
          (is (eq :openai-codex seen-provider))
          (is (string= "gpt-5.3-codex" seen-model))
          (is (string= "high" seen-think-level))
          (is (string= "researcher" (getf run-result :agent-name)))
          (is (string= "delegated answer" (getf run-result :final-text))))))))

(test subagent-run-tool-supports-custom-transient-agent
  "subagent_run can create transient custom agents without registering them."
  (with-subagent-package-state
    (let ((seen-system-prompt nil))
      (load-test-subagent-package)
      (with-subagent-package-function-override (rplaca::load-boot-files ()
                                                nil)
        (with-subagent-package-function-override
            (rplaca::provider-request-streaming
             (provider messages callback
                       &key model max-tokens tools reasoning-effort system-prompt)
             (declare (ignore provider messages callback model max-tokens tools
                              reasoning-effort))
             (setf seen-system-prompt system-prompt)
             (make-subagent-package-completed-stream-state-response
              "end_turn"
              (list (rplaca::canonical-text-block "custom answer"))))
          (rplaca::init-default-keymap)
          (let* ((result (subagent-package-tool-result
                          "subagent_run"
                          '(:prompt "Use custom instructions"
                            :agent_spec ((:name . "temporary-doc-agent")
                                         (:provider . "zai")
                                         (:model . "glm-5")
                                         (:core_prompt . "TEMP CORE")
                                         (:personality_prompt
                                          . "TEMP PERSONALITY")))))
                 (run-result (getf result :result)))
            (is (getf result :ok))
            (is (search "TEMP CORE" seen-system-prompt))
            (is (search "TEMP PERSONALITY" seen-system-prompt))
            (is (null (rplaca:find-agent-definition "temporary-doc-agent")))
            (is (string= "custom answer" (getf run-result :final-text)))))))))

(test subagent-async-tools-start-status-wait-and-cancel
  "subagent_start/status/wait/cancel expose the background handle lifecycle."
  (with-subagent-package-state
    (load-test-subagent-package)
    (with-subagent-package-function-override
        (rplaca::provider-request-streaming
         (provider messages callback
                   &key model max-tokens tools reasoning-effort system-prompt)
         (declare (ignore provider messages callback model max-tokens tools
                          reasoning-effort system-prompt))
         (make-subagent-package-completed-stream-state-response
          "end_turn"
          (list (rplaca::canonical-text-block "async answer"))))
      (rplaca::init-default-keymap)
      (let* ((started (subagent-package-tool-result
                       "subagent_start"
                       '(:prompt "Do async work"
                         :agent "async-agent"
                         :provider "zai"
                         :model "glm-5")))
             (started-subagent (getf started :subagent))
             (id (getf started-subagent :id)))
        (is (getf started :ok))
        (is (string= "subagent-1" id))
        (is (eq :running (getf started-subagent :status)))
        (let* ((listed (subagent-package-tool-result "subagent_status" nil))
               (subagents (getf listed :subagents)))
          (is (getf listed :ok))
          (is (= 1 (length subagents)))
          (is (string= id (getf (aref subagents 0) :id))))
        (let* ((waited (subagent-package-tool-result
                        "subagent_wait"
                        `(:id ,id :timeout 2)))
               (waited-subagent (getf waited :subagent))
               (run-result (getf waited-subagent :result)))
          (is (getf waited :ok))
          (is (eq :succeeded (getf waited :status)))
          (is (getf waited-subagent :done-p))
          (is (string= "async answer" (getf run-result :final-text)))))
      (with-subagent-package-function-override
          (rplaca::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (sleep 0.1)
           (make-subagent-package-completed-stream-state-response
            "end_turn"
            (list (rplaca::canonical-text-block "late answer"))))
        (let* ((started (subagent-package-tool-result
                         "subagent_start"
                         '(:prompt "Cancel me"
                           :provider "zai"
                           :model "glm-5")))
               (id (getf (getf started :subagent) :id))
               (cancelled (subagent-package-tool-result
                           "subagent_cancel"
                           `(:id ,id))))
          (is (getf cancelled :ok))
          (is (member (getf (getf cancelled :subagent) :status)
                      '(:cancelling :cancelled)))
          ;; Keep the provider override installed until the cancelled worker has
          ;; actually unwound.  This guards against cross-test/provider leakage.
          (let ((waited (subagent-package-tool-result
                         "subagent_wait"
                         `(:id ,id :timeout 2))))
            (is (eq :cancelled (getf waited :status)))
            (is (getf (getf waited :subagent) :done-p))))))))
