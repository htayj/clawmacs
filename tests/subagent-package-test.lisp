(in-package :clawmacs/tests)

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
          (clawmacs::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (clawmacs::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (clawmacs::*tool-table* (make-hash-table :test #'equal))
          (clawmacs::*agent-definition-registry*
           (make-hash-table :test #'equal))
          (clawmacs::*subagent-handle-counter* 0)
          (clawmacs::*subagent-handles* (make-hash-table :test #'equal))
          (clawmacs::*subagent-registry-lock*
           (bt:make-lock "test-subagent-package-registry"))
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels* (default-package-test-channels))
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
          (clawmacs::*enabled-builtin-packages* nil))
     ,@body))

(defun load-test-subagent-package ()
  "Enable and load the bundled subagent package."
  (set-package-enablement-scope "subagent" :global)
  (load-active-packages))

(defun subagent-package-tool-result (tool-name args)
  "Execute TOOL-NAME with ARGS and read its Lisp data result."
  (nth-value 0
    (clawmacs::lisp-data-read
     (clawmacs:execute-tool tool-name args))))

(defun make-subagent-package-completed-stream-state-response
    (stop-reason content-blocks &optional usage)
  "Return a completed stream state fixture for subagent package tests."
  (let ((state (clawmacs::make-stream-state)))
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (setf (clawmacs::stream-state-stop-reason state) stop-reason
            (clawmacs::stream-state-content-blocks state)
            (reverse content-blocks)
            (clawmacs::stream-state-usage state) usage
            (clawmacs::stream-state-done-p state) t))
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
        (is (member name tool-names :test #'string=))
        (is-false (clawmacs::tool-requires-permission-p name)))
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
      (clawmacs:register-agent-definition
       "researcher"
       :provider :zai
       :model "glm-5"
       :personality-prompt "research personality")
      (load-test-subagent-package)
      (with-subagent-package-function-override
          (clawmacs::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore messages callback max-tokens tools system-prompt))
           (setf seen-provider provider
                 seen-model model
                 seen-think-level reasoning-effort)
           (make-subagent-package-completed-stream-state-response
            "end_turn"
            (list (clawmacs::canonical-text-block "delegated answer"))))
        (clawmacs::init-default-keymap)
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
      (with-subagent-package-function-override (clawmacs::load-boot-files ()
                                                nil)
        (with-subagent-package-function-override
            (clawmacs::provider-request-streaming
             (provider messages callback
                       &key model max-tokens tools reasoning-effort system-prompt)
             (declare (ignore provider messages callback model max-tokens tools
                              reasoning-effort))
             (setf seen-system-prompt system-prompt)
             (make-subagent-package-completed-stream-state-response
              "end_turn"
              (list (clawmacs::canonical-text-block "custom answer"))))
          (clawmacs::init-default-keymap)
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
            (is (null (clawmacs:find-agent-definition "temporary-doc-agent")))
            (is (string= "custom answer" (getf run-result :final-text)))))))))

(test subagent-async-tools-start-status-wait-and-cancel
  "subagent_start/status/wait/cancel expose the background handle lifecycle."
  (with-subagent-package-state
    (load-test-subagent-package)
    (with-subagent-package-function-override
        (clawmacs::provider-request-streaming
         (provider messages callback
                   &key model max-tokens tools reasoning-effort system-prompt)
         (declare (ignore provider messages callback model max-tokens tools
                          reasoning-effort system-prompt))
         (make-subagent-package-completed-stream-state-response
          "end_turn"
          (list (clawmacs::canonical-text-block "async answer"))))
      (clawmacs::init-default-keymap)
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
          (clawmacs::provider-request-streaming
           (provider messages callback
                     &key model max-tokens tools reasoning-effort system-prompt)
           (declare (ignore provider messages callback model max-tokens tools
                            reasoning-effort system-prompt))
           (sleep 0.1)
           (make-subagent-package-completed-stream-state-response
            "end_turn"
            (list (clawmacs::canonical-text-block "late answer"))))
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
          (is (eq :cancelled (getf (getf cancelled :subagent) :status))))))))

(test subagent-package-rejects-agent-auto-approval-escalation
  "Agent callers cannot use subagent tools to auto-approve permissioned tools."
  (with-subagent-package-state
    (load-test-subagent-package)
    (let ((*current-caller* :coder))
      (signals error
        (clawmacs:execute-tool
         "subagent_run"
         '(:prompt "Try escalation"
           :provider "zai"
           :model "glm-5"
           :auto_approve_tools t))))))
