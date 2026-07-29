(in-package :rplaca/tests)

(in-suite modelaria-package-suite)

(defmacro with-modelaria-function-override ((name lambda-list &body implementation)
                                            &body body)
  `(let ((original-function (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name)
                  (lambda ,lambda-list
                    ,@implementation))
            ,@body)
       (setf (symbol-function ',name) original-function))))

(defmacro with-modelaria-test-state (&body body)
  `(with-package-state-override ((default-package-test-channels))
     (let* ((global-root (temp-package-test-directory "modelaria-global"))
            (rplaca::*modelaria-global-config-path*
             (merge-pathnames "modelaria.json"
                              (uiop:ensure-directory-pathname global-root)))
            (rplaca::*buffer-ring* nil)
            (rplaca::*buffer-counter* 0)
            (rplaca::*default-keymap* nil)
            (rplaca::*scratch-keymap* nil)
            (rplaca::*file-keymap* nil)
            (rplaca::*command-table* (make-hash-table :test #'eq))
            (rplaca::*extended-docs* (make-hash-table :test #'eq))
            (rplaca::*agent-tool-metadata-table*
             (make-hash-table :test #'eq))
            (rplaca::*agent-tool-name-table*
             (make-hash-table :test #'equal))
            (rplaca::*slash-command-table* (make-hash-table :test #'equal))
            (rplaca::*after-send-message-hook* nil))
       (set-package-enablement-scope "modelaria" :global)
       (load-active-packages)
       (rplaca::init-default-keymap)
       ,@body)))

(defun make-modelaria-test-buffer (label &key (working-directory nil))
  "Return a chat buffer for modelaria tests."
  (let ((root (or working-directory
                  (temp-package-test-directory
                   (format nil "modelaria-buffer-~A" label)))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (let ((buffer (rplaca::make-chat-buffer label
                                              :working-directory root)))
      (setf (rplaca::buffer-keymap buffer) rplaca::*default-keymap*)
      buffer)))

(test modelaria-package-registers-tools-prompt-and-commands
  "Enabling modelaria exposes its tools, prompt section, and commands."
  (with-modelaria-test-state
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (mapcar (lambda (tool) (cdr (assoc :name tool))) tools))
           (prompt (render-package-prompt-sections))
           (commands (list-available-commands)))
      (is (member "modelaria_role_status" tool-names :test #'string=))
      (is (member "modelaria_session_usage" tool-names :test #'string=))
      (is (search "Model roles with modelaria" prompt))
      (is (member 'rplaca::modelaria-select-session-role-command
                  commands :test #'eq))
      (is (member 'rplaca::modelaria-cycle-role-command
                  commands :test #'eq))
      (is (member 'rplaca::modelaria-show-usage-command
                  commands :test #'eq)))))

(test modelaria-resolves-scoped-roles-and-next-turn-precedence
  "Project and session roles resolve in scope order, while next-turn wins."
  (with-modelaria-test-state
    (let* ((project-root (temp-package-test-directory "modelaria-project"))
           (buffer (make-modelaria-test-buffer "modelaria-scope"
                                               :working-directory project-root)))
      (ensure-directories-exist (merge-pathnames #P".keep" project-root))
      (rplaca::save-modelaria-global-config
       '(:active-role "cheap" :role-set ("default" "cheap") :service-tier "auto"))
      (rplaca::save-modelaria-project-config
       buffer
       '(:active-role "review" :role-set ("review" "slow") :service-tier "priority"))
      (multiple-value-bind (provider model think-level)
          (rplaca::resolve-buffer-provider-and-model buffer)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.6-sol" model))
        (is (string= "high" think-level))
        (is (string= "priority"
                     (rplaca::resolve-buffer-service-tier buffer))))
      (rplaca::set-buffer-provider-override buffer :openai-codex)
      (rplaca::set-buffer-model-override buffer "gpt-5.2-codex")
      (multiple-value-bind (_provider model _think)
          (rplaca::resolve-buffer-provider-and-model buffer)
        (declare (ignore _provider _think))
        (is (string= "gpt-5.2-codex" model)))
      (setf (rplaca::buffer-next-turn-model-role-override buffer) "slow")
      (multiple-value-bind (_provider model think-level)
          (rplaca::resolve-buffer-provider-and-model buffer)
        (declare (ignore _provider))
        (is (string= "gpt-5.6-sol" model))
        (is (string= "max" think-level)))
      (setf (rplaca::buffer-next-turn-model-role-override buffer) nil)
      (rplaca::clear-buffer-provider-override buffer)
      (rplaca::clear-buffer-model-override buffer)
      (rplaca::set-buffer-model-role-override buffer "slow")
      (multiple-value-bind (_provider model think-level)
          (rplaca::resolve-buffer-provider-and-model buffer)
        (declare (ignore _provider))
        (is (string= "gpt-5.6-sol" model))
        (is (string= "max" think-level))))))

(test modelaria-cycle-uses-effective-role-set
  "Cycling uses the scoped role set rather than the flat global model list."
  (with-modelaria-test-state
    (let ((buffer (make-modelaria-test-buffer "modelaria-cycle")))
      (rplaca::set-buffer-model-role-set-override buffer '("cheap" "slow"))
      (rplaca::set-buffer-model-role-override buffer "cheap")
      (rplaca::modelaria-cycle-role-command buffer)
      (is (string= "slow" (rplaca::buffer-model-role-override buffer)))
      (rplaca::modelaria-cycle-role-command buffer)
      (is (string= "cheap" (rplaca::buffer-model-role-override buffer))))))

(test modelaria-built-in-roles-use-gpt-5.6-family
  "Built-in roles retain their quality and cost intent on GPT-5.6."
  (multiple-value-bind (provider model think-level)
      (rplaca::modelaria-role-routing
       "cheap" :openai-codex "gpt-5.6-sol" nil)
    (is (eq :openai-codex provider))
    (is (string= "gpt-5.6-luna" model))
    (is (string= "low" think-level)))
  (multiple-value-bind (provider model think-level)
      (rplaca::modelaria-role-routing
       "plan" :openai-codex "gpt-5.6-sol" nil)
    (is (eq :openai-codex provider))
    (is (string= "gpt-5.6-sol" model))
    (is (string= "high" think-level)))
  (multiple-value-bind (provider model think-level)
      (rplaca::modelaria-role-routing
       "slow" :openai-codex "gpt-5.6-sol" nil)
    (is (eq :openai-codex provider))
    (is (string= "gpt-5.6-sol" model))
    (is (string= "max" think-level)))
  (multiple-value-bind (provider model think-level)
      (rplaca::modelaria-role-routing
       "cheap" :openrouter "openai/gpt-5.6-sol" nil)
    (is (eq :openrouter provider))
    (is (string= "openai/gpt-5.6-luna" model))
    (is (null think-level))))

(test modelaria-usage-report-estimates-cost-and-context-pressure
  "Usage reports aggregate token/cache counts, cost, and context pressure."
  (with-modelaria-test-state
    (let ((buffer (make-modelaria-test-buffer "modelaria-usage")))
      (rplaca::set-buffer-provider-override buffer :openai-codex)
      (rplaca::set-buffer-model-override buffer "gpt-5.4")
      (rplaca::set-buffer-service-tier-override buffer "priority")
      (setf (buffer-token-count buffer) 50000
            (buffer-context-limit buffer) 100000)
      (let ((message (rplaca::buffer-insert-read-only-message
                      buffer :assistant "usage message")))
        (rplaca::put-message-metadata
         message
         :provider :openai-codex
         :model "gpt-5.4"
         :input-tokens 1000
         :cached-input-tokens 400
         :uncached-input-tokens 600
         :output-tokens 200
         :total-tokens 1200))
      (let* ((report (rplaca::modelaria-session-usage-report buffer))
             (usage (getf report :usage)))
        (is (= 1000 (getf usage :input-tokens)))
        (is (= 400 (getf usage :cached-input-tokens)))
        (is (not (null (getf report :estimated-cost))))
        (is (= 0.5f0 (getf report :context-pressure)))
        (is (search "Estimated cost"
                    (rplaca::modelaria-format-usage-help buffer)))))))

(test modelaria-routing-flows-through-prompt-subagent-and-pipeline
  "Prompt runs, subagents, and pipelines can carry model roles and service tiers."
  (with-modelaria-test-state
    (let ((prompt-capture nil)
          (subagent-capture nil)
          (pipeline-capture nil))
      (with-modelaria-function-override
          (rplaca::run-prompt-with-buffer
           (buffer prompt custom-tool-definitions
                   max-tool-iterations
                   tool-names tool-names-supplied-p
                   &key output-schema event-callback stream-state-callback
                     cancel-requested-p)
           (declare (ignore prompt custom-tool-definitions
                            max-tool-iterations
                            tool-names
                            tool-names-supplied-p
                            output-schema
                            event-callback
                            stream-state-callback
                            cancel-requested-p))
           (setf prompt-capture
                 (list :role (rplaca::buffer-model-role-override buffer)
                       :tier (rplaca::buffer-service-tier-override buffer)))
           (rplaca::make-prompt-run-result
            :prompt "p"
            :final-text "ok"
            :tool-events nil
            :reasoning-blocks nil
            :agent-name (rplaca::buffer-agent-name buffer)
            :provider :openai-codex
            :model "gpt-5.4"
            :think-level "high"
            :service-tier (rplaca::buffer-service-tier-override buffer)
            :iterations 1
            :stop-reason "end_turn"))
        (rplaca::run-single-prompt "hi"
                                     :model-role "review"
                                     :service-tier "priority")
        (is (equal '(:role "review" :tier "priority") prompt-capture)))
      (with-modelaria-function-override
          (rplaca::run-single-prompt (prompt &rest args)
           (declare (ignore prompt))
           (setf subagent-capture args)
           (rplaca::make-prompt-run-result
            :prompt "p"
            :final-text "ok"
            :tool-events nil
            :reasoning-blocks nil
            :agent-name "worker"
            :provider :openai-codex
            :model "gpt-5.4"
            :think-level "high"
            :service-tier "flex"
            :iterations 1
            :stop-reason "end_turn"))
        (rplaca::run-subagent "inspect"
                                :model-role "plan"
                                :service-tier "flex")
        (is (member :model-role subagent-capture))
        (is (string= "plan" (getf subagent-capture :model-role)))
        (is (string= "flex" (getf subagent-capture :service-tier))))
      (with-modelaria-function-override
          (rplaca::run-prompt-with-buffer
           (buffer prompt custom-tool-definitions
                   max-tool-iterations
                   tool-names tool-names-supplied-p
                   &key output-schema event-callback stream-state-callback
                     cancel-requested-p)
           (declare (ignore prompt custom-tool-definitions
                            max-tool-iterations
                            tool-names
                            tool-names-supplied-p
                            output-schema
                            event-callback
                            stream-state-callback
                            cancel-requested-p))
           (setf pipeline-capture
                 (list :role (rplaca::buffer-model-role-override buffer)
                       :tier (rplaca::buffer-service-tier-override buffer)))
           (rplaca::make-prompt-run-result
            :prompt "p"
            :final-text "ok"
            :tool-events nil
            :reasoning-blocks nil
            :agent-name (rplaca::buffer-agent-name buffer)
            :provider :openai-codex
            :model "gpt-5.1-codex-max"
            :think-level "high"
            :service-tier (rplaca::buffer-service-tier-override buffer)
            :iterations 1
            :stop-reason "end_turn"))
        (let* ((buffer (make-modelaria-test-buffer "modelaria-pipeline"))
               (definition
                 (rplaca::make-pipeline-definition
                  :name "modelaria-pipeline"
                  :stages (list (rplaca::make-pipeline-stage
                                 :name "plan"
                                 :prompt "plan"
                                 :model-role "slow"
                                 :service-tier "priority"))
                  :entry-stage "plan"))
               (context (rplaca::make-pipeline-context
                         :definition definition
                         :original-prompt "orig"
                         :buffer buffer
                         :stage-results nil)))
          (rplaca::run-pipeline-stage-on-buffer context
                                                  (first (rplaca::pipeline-definition-stages
                                                          definition)))
          (is (equal '(:role "slow" :tier "priority") pipeline-capture)))))))
