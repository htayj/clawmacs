(in-package :clawmacs)

(defvar *subagent-tool-default-wait-timeout* 60
  "Default seconds that subagent_wait blocks before returning :TIMEOUT.")

(defun subagent-tool-blank-string-p (value)
  "Return true when VALUE is NIL or contains only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun subagent-tool-string (value field-name &key allow-nil)
  "Normalize VALUE as a string argument named FIELD-NAME."
  (cond
    ((null value)
     (if allow-nil
         nil
         (error "~A is required." field-name)))
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 value)))
       (cond
         ((and (not allow-nil)
               (subagent-tool-blank-string-p trimmed))
          (error "~A must be a non-empty string." field-name))
         ((subagent-tool-blank-string-p trimmed) nil)
         (t trimmed))))
    ((symbolp value)
     (subagent-tool-string (string-downcase (symbol-name value))
                           field-name
                           :allow-nil allow-nil))
    (t
     (error "~A must be a string, got ~S." field-name value))))

(defun subagent-tool-positive-integer (value field-name default)
  "Return VALUE as a positive integer, or DEFAULT when VALUE is NIL."
  (cond
    ((null value) default)
    ((and (integerp value) (plusp value)) value)
    (t
     (error "~A must be a positive integer, got ~S." field-name value))))

(defun subagent-tool-nonnegative-real (value field-name default)
  "Return VALUE as a non-negative real number, or DEFAULT when VALUE is NIL."
  (cond
    ((null value) default)
    ((and (realp value) (not (minusp value))) value)
    (t
     (error "~A must be a non-negative number, got ~S." field-name value))))

(defun subagent-tool-arg-value (args &rest keys)
  "Return values VALUE and SUPPLIED-P for the first matching key in ARGS."
  (dolist (key keys (values nil nil))
    (multiple-value-bind (value supplied-p)
        (tool-argument-value args key)
      (when supplied-p
        (return-from subagent-tool-arg-value (values value t))))))

(defun subagent-tool-object-p (value)
  "Return true when VALUE can be read as a Lisp data object argument."
  (and (listp value)
       (or (null value)
           (keywordp (first value))
           (consp (first value)))))

(defun subagent-tool-agent-spec (args)
  "Return an optional nested agent spec from ARGS."
  (multiple-value-bind (value supplied-p)
      (subagent-tool-arg-value args :agent-spec "agent_spec")
    (when supplied-p
      (unless (subagent-tool-object-p value)
        (error "agent_spec must be an object."))
      (return-from subagent-tool-agent-spec value)))
  (multiple-value-bind (value supplied-p)
      (subagent-tool-arg-value args :agent "agent")
    (when (and supplied-p (subagent-tool-object-p value))
      value)))

(defun subagent-tool-field-value (args spec &rest keys)
  "Return field value from SPEC first, then top-level ARGS."
  (when spec
    (multiple-value-bind (value supplied-p)
        (apply #'subagent-tool-arg-value spec keys)
      (when supplied-p
        (return-from subagent-tool-field-value (values value t)))))
  (apply #'subagent-tool-arg-value args keys))

(defun subagent-tool-string-field (args spec field-name keys &key allow-nil)
  "Return a normalized string field from SPEC or ARGS."
  (multiple-value-bind (value supplied-p)
      (apply #'subagent-tool-field-value args spec keys)
    (if supplied-p
        (subagent-tool-string value field-name :allow-nil allow-nil)
        (if allow-nil
            nil
            (error "~A is required." field-name)))))

(defun subagent-tool-boolean-field (args spec keys default)
  "Return a boolean field from SPEC or ARGS."
  (multiple-value-bind (value supplied-p)
      (apply #'subagent-tool-field-value args spec keys)
    (if supplied-p
        (not (null value))
        default)))

(defun subagent-tool-agent-name (args spec)
  "Return the requested subagent name."
  (let ((from-spec
          (when spec
            (subagent-tool-string-field
             args spec "agent name" '(:name "name" :agent-name "agent_name")
             :allow-nil t))))
    (or from-spec
        (multiple-value-bind (agent supplied-p)
            (subagent-tool-arg-value args :agent "agent" :agent-name "agent_name")
          (when (and supplied-p (not (subagent-tool-object-p agent)))
            (subagent-tool-string agent "agent" :allow-nil t)))
        *default-subagent-name*)))

(defun subagent-tool-tool-names (args spec)
  "Return values TOOL-NAMES and SUPPLIED-P for optional tool allowlists."
  (multiple-value-bind (value supplied-p)
      (subagent-tool-field-value args spec
                                 :tool-names "tool_names"
                                 :tools "tools")
    (if supplied-p
        (let ((names (normalize-tool-name-list value)))
          (values names (not (null names))))
        (values nil nil))))

(defun subagent-tool-auto-approve-tools-p (args spec)
  "Return the requested auto-approve setting, rejecting agent escalation."
  (let ((value (subagent-tool-boolean-field
                args spec
                '(:auto-approve-tools "auto_approve_tools")
                nil)))
    (when (and value (not (eq *current-caller* :user)))
      (error "auto_approve_tools can only be true for direct user calls."))
    value))

(defun subagent-tool-run-arguments (args)
  "Return values PROMPT and keyword arguments for RUN-SUBAGENT."
  (let* ((spec (subagent-tool-agent-spec args))
         (prompt (subagent-tool-string-field
                  args spec "prompt" '(:prompt "prompt")))
         (agent-name (subagent-tool-agent-name args spec))
         (provider (subagent-tool-string-field
                    args spec "provider" '(:provider "provider")
                    :allow-nil t))
         (model (subagent-tool-string-field
                 args spec "model" '(:model "model")
                 :allow-nil t))
         (think-level (subagent-tool-string-field
                       args spec "think_level"
                       '(:think-level "think_level" "reasoning_effort")
                       :allow-nil t))
         (core-prompt (subagent-tool-string-field
                       args spec "core_prompt"
                       '(:core-prompt "core_prompt" "system_prompt")
                       :allow-nil t))
         (personality-prompt
           (subagent-tool-string-field
            args spec "personality_prompt"
            '(:personality-prompt "personality_prompt")
            :allow-nil t))
         (max-tool-iterations
           (subagent-tool-positive-integer
            (nth-value 0
              (subagent-tool-field-value args spec
                                         :max-tool-iterations
                                         "max_tool_iterations"))
            "max_tool_iterations"
            *prompt-max-tool-iterations*))
         (auto-approve-tools-p
           (subagent-tool-auto-approve-tools-p args spec))
         (run-args (list :agent-name agent-name
                         :provider provider
                         :model model
                         :think-level think-level
                         :core-prompt core-prompt
                         :personality-prompt personality-prompt
                         :max-tool-iterations max-tool-iterations
                         :auto-approve-tools-p auto-approve-tools-p)))
    (multiple-value-bind (tool-names supplied-p)
        (subagent-tool-tool-names args spec)
      (when supplied-p
        (setf run-args (append run-args (list :tool-names tool-names)))))
    (values prompt run-args)))

(defun subagent-tool-provider-name (provider)
  "Return PROVIDER as a lowercase provider name string."
  (and provider
       (string-downcase (symbol-name provider))))

(defun subagent-tool-event-data (event)
  "Return EVENT as Lisp-data-friendly plist."
  (list :id (prompt-tool-event-id event)
        :name (prompt-tool-event-name event)
        :input (prompt-tool-event-input event)
        :result-text (prompt-tool-event-result-text event)
        :display (prompt-tool-event-display event)
        :denied-p (prompt-tool-event-denied-p event)))

(defun subagent-tool-result-data (result)
  "Return RESULT as Lisp-data-friendly plist."
  (when result
    (list :prompt (prompt-run-result-prompt result)
          :final-text (prompt-run-result-final-text result)
          :agent-name (prompt-run-result-agent-name result)
          :provider (subagent-tool-provider-name
                     (prompt-run-result-provider result))
          :model (prompt-run-result-model result)
          :think-level (prompt-run-result-think-level result)
          :iterations (prompt-run-result-iterations result)
          :stop-reason (prompt-run-result-stop-reason result)
          :usage (copy-list (prompt-run-result-usage result))
          :tool-events (coerce (mapcar #'subagent-tool-event-data
                                        (prompt-run-result-tool-events result))
                               'vector)
          :reasoning (coerce (copy-list
                              (prompt-run-result-reasoning-blocks result))
                             'vector))))

(defun subagent-tool-handle (id)
  "Return subagent handle ID or signal a clear tool error."
  (or (find-subagent id)
      (error "Unknown subagent id: ~A" id)))

(defun subagent-tool-handle-data (handle &key include-result)
  "Return HANDLE as Lisp-data-friendly plist."
  (let ((snapshot (subagent-snapshot handle)))
    (if include-result
        (setf (getf snapshot :result)
              (subagent-tool-result-data (getf snapshot :result)))
        (remf snapshot :result))
    snapshot))

(defun subagent-tool-run (args)
  "Run a synchronous subagent and return its result."
  (multiple-value-bind (prompt run-args)
      (subagent-tool-run-arguments args)
    (lisp-data-string
     (list :ok t
           :result (subagent-tool-result-data
                    (apply #'run-subagent prompt run-args))))))

(defun subagent-tool-start (args)
  "Start an asynchronous subagent and return its handle id and status."
  (multiple-value-bind (prompt run-args)
      (subagent-tool-run-arguments args)
    (let ((handle (apply #'run-subagent-async prompt run-args)))
      (lisp-data-string
       (list :ok t
             :subagent (subagent-tool-handle-data handle))))))

(defun subagent-tool-status (args)
  "Return one subagent status, or all known subagent statuses when ID is omitted."
  (let ((id (subagent-tool-string
             (tool-arg args :id "id")
             "id"
             :allow-nil t))
        (include-result-p
          (subagent-tool-boolean-field
           args nil '(:include-result "include_result") nil)))
    (lisp-data-string
     (if id
         (list :ok t
               :subagent (subagent-tool-handle-data
                          (subagent-tool-handle id)
                          :include-result include-result-p))
         (list :ok t
               :subagents (coerce
                           (mapcar (lambda (handle)
                                     (subagent-tool-handle-data
                                      handle
                                      :include-result include-result-p))
                                   (list-subagents))
                           'vector))))))

(defun subagent-tool-wait (args)
  "Wait for a background subagent to finish and return its status/result."
  (let* ((id (subagent-tool-string (tool-arg args :id "id") "id"))
         (timeout (subagent-tool-nonnegative-real
                   (tool-arg args :timeout "timeout")
                   "timeout"
                   *subagent-tool-default-wait-timeout*))
         (handle (subagent-tool-handle id)))
    (multiple-value-bind (_result status returned-handle)
        (wait-subagent handle :timeout timeout)
      (declare (ignore _result))
      (lisp-data-string
       (list :ok (eq status :succeeded)
             :status status
             :subagent (subagent-tool-handle-data
                        returned-handle
                        :include-result (eq status :succeeded)))))))

(defun subagent-tool-cancel (args)
  "Cancel a background subagent."
  (let* ((id (subagent-tool-string (tool-arg args :id "id") "id"))
         (handle (cancel-subagent (subagent-tool-handle id))))
    (lisp-data-string
     (list :ok t
           :subagent (subagent-tool-handle-data handle)))))

(register-package-prompt-section
 "subagent"
 "## Delegation with subagent

- Use `subagent_run` when you need another agent to complete a bounded task
  before you continue. It returns the final text, metadata, and any tool events.
- Use `subagent_start` for background or parallel work, then use
  `subagent_status` or `subagent_wait` with the returned `id`.
- Use `agent` to name an existing registered Clawmacs agent. Add `provider`,
  `model`, `think_level`, `core_prompt`, `personality_prompt`, or `tool_names`
  to override that agent for this run only. You can also group those fields in
  an `agent_spec` object.
- For a fully custom transient agent, pass a new `agent` name plus
  `core_prompt` and/or `personality_prompt`. This does not register or mutate
  the agent definition.
- Use `tool_names` to constrain the subagent's tools. Omit it to use the
  selected agent's default tools. Permission-gated tools are denied in
  non-interactive subagents unless the user directly requested auto-approval.
- Prefer these tools over `lisp_eval` for subagent delegation."
 :title "Delegation with subagent"
 :package "subagent")

(deftool subagent-tool-run
  :name "subagent_run"
  :description "Run a Clawmacs subagent synchronously. Use agent for an existing agent, or provide core_prompt/personality_prompt/routing overrides for a custom transient agent."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((prompt :type "string"
                 :description "Task prompt for the subagent.")
         (agent :type "string" :required nil
                :description "Existing agent name, or a transient custom agent name.")
         (agent-spec :type "object" :required nil
                     :description "Optional object with name, provider, model, think_level, core_prompt, personality_prompt, tool_names, and max_tool_iterations overrides.")
         (provider :type "string" :required nil
                   :description "Provider override such as openai-codex, zai, or openrouter.")
         (model :type "string" :required nil
                :description "Model override for this subagent run.")
         (think-level :type "string" :required nil
                      :description "Reasoning effort override, also accepted as think_level or reasoning_effort.")
         (core-prompt :type "string" :required nil
                      :description "Core/system prompt override for a custom or existing agent.")
         (personality-prompt :type "string" :required nil
                             :description "Personality prompt override for a custom or existing agent.")
         (tool-names :type "array" :required nil
                     :items ((:type . "string"))
                     :description "Optional exact tool allowlist for the subagent.")
         (max-tool-iterations :type "integer" :required nil
                              :description "Maximum provider tool-use loop iterations for the subagent.")))

(deftool subagent-tool-start
  :name "subagent_start"
  :description "Start a Clawmacs subagent asynchronously and return a handle id for later status, wait, or cancel calls."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((prompt :type "string"
                 :description "Task prompt for the subagent.")
         (agent :type "string" :required nil
                :description "Existing agent name, or a transient custom agent name.")
         (agent-spec :type "object" :required nil
                     :description "Optional object with name, provider, model, think_level, core_prompt, personality_prompt, tool_names, and max_tool_iterations overrides.")
         (provider :type "string" :required nil
                   :description "Provider override such as openai-codex, zai, or openrouter.")
         (model :type "string" :required nil
                :description "Model override for this subagent run.")
         (think-level :type "string" :required nil
                      :description "Reasoning effort override, also accepted as think_level or reasoning_effort.")
         (core-prompt :type "string" :required nil
                      :description "Core/system prompt override for a custom or existing agent.")
         (personality-prompt :type "string" :required nil
                             :description "Personality prompt override for a custom or existing agent.")
         (tool-names :type "array" :required nil
                     :items ((:type . "string"))
                     :description "Optional exact tool allowlist for the subagent.")
         (max-tool-iterations :type "integer" :required nil
                              :description "Maximum provider tool-use loop iterations for the subagent.")))

(deftool subagent-tool-status
  :name "subagent_status"
  :description "Inspect one background subagent by id, or list all known background subagents when id is omitted."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((id :type "string" :required nil
             :description "Subagent id returned by subagent_start.")
         (include-result :type "boolean" :required nil
                         :description "When true, include summarized final result data for completed subagents.")))

(deftool subagent-tool-wait
  :name "subagent_wait"
  :description "Wait for a background subagent to finish and return its status and final result when it succeeds."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((id :type "string"
             :description "Subagent id returned by subagent_start.")
         (timeout :type "number" :required nil
                  :description "Seconds to wait before returning :timeout. Defaults to 60.")))

(deftool subagent-tool-cancel
  :name "subagent_cancel"
  :description "Cooperatively cancel a background subagent by id."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((id :type "string"
             :description "Subagent id returned by subagent_start.")))
