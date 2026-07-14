(in-package :clawmacs)

(defvar *modelaria-package-name* "modelaria"
  "Bundled package name for scoped model routing and usage reporting.")

(defun modelaria-current-buffer ()
  "Return the current tool buffer, or the current UI buffer."
  (or *current-tool-buffer*
      (current-buffer)))

(defun modelaria-role-items (buffer &key include-clear-p)
  "Return minibuffer items for model role selection in BUFFER."
  (multiple-value-bind (active-role active-scope)
      (modelaria-buffer-effective-role buffer)
    (let ((roles (remove-duplicates
                  (append (copy-list *modelaria-built-in-role-order*)
                          (copy-list (buffer-model-role-set-override buffer)))
                  :test #'string=)))
      (append
       (when include-clear-p
         (list (list :role "default"
                     :display "default  [built-in] Use the normal agent/session routing."
                     :active-p (string= active-role "default")
                     :match-text "default normal agent session")))
       (loop :for role :in roles
             :unless (and include-clear-p (string= role "default"))
               :collect
               (list :role role
                     :active-p (string= role active-role)
                     :display (format nil "~A  [~(~A~)] ~A"
                                      role
                                      (if (string= role active-role)
                                          active-scope
                                          :available)
                                      (modelaria-role-description role))
                     :match-text (format nil "~A ~A"
                                         role
                                         (modelaria-role-description role))))))))

(defun modelaria-insert-routing-message (buffer prefix)
  "Insert a routing status message into BUFFER with PREFIX."
  (multiple-value-bind (provider model think-level)
      (resolve-buffer-provider-and-model buffer)
    (multiple-value-bind (role scope)
        (modelaria-buffer-effective-role buffer)
      (buffer-insert-system-message
       buffer
       (format nil "[~A role=~A [~(~A~)] -> ~(~A~)/~A~@[ think ~A~]~@[ tier ~A~]]"
               prefix
               role
               scope
               provider
               model
               think-level
               (resolve-buffer-service-tier buffer))))))

(defun modelaria-record-buffer-routing (buffer)
  "Persist BUFFER's current routing state into its session tree, when present."
  (when (buffer-session buffer)
    (multiple-value-bind (provider model think-level)
        (resolve-buffer-provider-and-model buffer)
      (multiple-value-bind (role scope)
          (modelaria-buffer-effective-role buffer)
        (declare (ignore scope))
        (record-session-model-change
         (buffer-session buffer)
         provider
         model
         :think-level think-level
         :role role
         :service-tier (resolve-buffer-service-tier buffer))))))

(defun modelaria-apply-session-role (buffer role)
  "Apply session-scoped ROLE to BUFFER."
  (clear-buffer-provider-override buffer)
  (clear-buffer-model-override buffer)
  (clear-buffer-think-level-override buffer)
  (set-buffer-model-role-override buffer role)
  (setf (buffer-next-turn-model-role-override buffer) nil)
  (modelaria-record-buffer-routing buffer)
  (modelaria-insert-routing-message buffer "Model role set")
  buffer)

(defun modelaria-select-session-role-command (buffer)
  "Select the current buffer/session model role."
  (minibuffer-activate
   "Session Model Role"
   (modelaria-role-items buffer :include-clear-p t)
   (lambda (item)
     (modelaria-apply-session-role buffer (getf item :role))))
  (preselect-minibuffer-active-item
   (modelaria-role-items buffer :include-clear-p t)))
(defcommand modelaria-select-session-role-command)

(defun modelaria-cycle-role-command (buffer)
  "Cycle BUFFER through its effective model-role set."
  (multiple-value-bind (roles scope)
      (modelaria-buffer-effective-role-set buffer)
    (declare (ignore scope))
    (multiple-value-bind (current-role current-scope)
        (modelaria-buffer-effective-role buffer)
      (declare (ignore current-scope))
      (let* ((effective (or roles (copy-list *modelaria-built-in-role-order*)))
             (index (or (position current-role effective :test #'string=) -1))
             (next-role (nth (mod (1+ index) (length effective)) effective)))
        (modelaria-apply-session-role buffer next-role)))))
(defcommand modelaria-cycle-role-command)

(defun modelaria-set-next-turn-role-command (buffer)
  "Set a one-turn model role for BUFFER and clear it after the next send."
  (minibuffer-activate
   "Next Turn Role"
   (modelaria-role-items buffer :include-clear-p t)
   (lambda (item)
     (setf (buffer-next-turn-model-role-override buffer)
           (normalize-model-role-override (getf item :role)))
     (modelaria-insert-routing-message buffer "Next turn role set")))
  (preselect-minibuffer-active-item
   (modelaria-role-items buffer :include-clear-p t)))
(defcommand modelaria-set-next-turn-role-command)

(defun modelaria-parse-role-set-input (input)
  "Parse INPUT into an ordered unique list of model roles."
  (let* ((pieces (uiop:split-string
                  (substitute #\Space #\, (or input ""))
                  :separator '(#\Space #\Tab #\Newline #\Return)))
         (roles (normalize-modelaria-role-set pieces)))
    (if roles
        roles
        (copy-list *modelaria-built-in-role-order*))))

(defun modelaria-set-session-role-set-command (buffer)
  "Prompt for BUFFER's session-local role set as comma-separated names."
  (minibuffer-prompt
   "Session Role Set"
   (lambda (input)
     (set-buffer-model-role-set-override buffer
                                         (modelaria-parse-role-set-input input))
     (buffer-insert-system-message
      buffer
      (format nil "[Session role set: ~{~A~^, ~}]"
              (buffer-model-role-set-override buffer))))))
(defcommand modelaria-set-session-role-set-command)

(defun modelaria-set-project-role-command (buffer)
  "Select BUFFER's project default role."
  (let ((config (modelaria-project-config buffer)))
    (unless config
      (buffer-insert-system-message buffer "[No project directory for project role config.]")
      (return-from modelaria-set-project-role-command nil))
    (minibuffer-activate
     "Project Model Role"
     (modelaria-role-items buffer :include-clear-p t)
     (lambda (item)
       (let ((updated (copy-list config)))
         (setf (getf updated :active-role)
               (normalize-modelaria-role-name (getf item :role)))
         (save-modelaria-project-config buffer updated)
         (modelaria-insert-routing-message buffer "Project role set"))))
    (preselect-minibuffer-active-item
     (modelaria-role-items buffer :include-clear-p t))))
(defcommand modelaria-set-project-role-command)

(defun modelaria-set-project-role-set-command (buffer)
  "Prompt for BUFFER's project-local role set as comma-separated names."
  (let ((config (or (modelaria-project-config buffer)
                    (list :active-role nil :role-set nil :service-tier nil))))
    (unless (modelaria-project-config-path buffer)
      (buffer-insert-system-message buffer "[No project directory for project role config.]")
      (return-from modelaria-set-project-role-set-command nil))
    (minibuffer-prompt
     "Project Role Set"
     (lambda (input)
       (let ((updated (copy-list config)))
         (setf (getf updated :role-set)
               (modelaria-parse-role-set-input input))
         (save-modelaria-project-config buffer updated)
         (buffer-insert-system-message
          buffer
          (format nil "[Project role set: ~{~A~^, ~}]"
                  (getf updated :role-set))))))))
(defcommand modelaria-set-project-role-set-command)

(defun modelaria-service-tier-items (buffer)
  "Return minibuffer items for service-tier selection in BUFFER."
  (let ((active (or (resolve-buffer-service-tier buffer) "auto")))
    (mapcar (lambda (tier)
              (list :service-tier tier
                    :active-p (string= tier active)
                    :display (format nil "~A~@[  [active]~]"
                                     tier
                                     (and (string= tier active) t))
                    :match-text tier))
            '("auto" "default" "flex" "priority"))))

(defun modelaria-set-service-tier-command (buffer)
  "Select BUFFER's service-tier preference."
  (minibuffer-activate
   "Service Tier"
   (modelaria-service-tier-items buffer)
   (lambda (item)
     (set-buffer-service-tier-override buffer (getf item :service-tier))
     (modelaria-record-buffer-routing buffer)
     (modelaria-insert-routing-message buffer "Service tier set")))
  (preselect-minibuffer-active-item (modelaria-service-tier-items buffer)))
(defcommand modelaria-set-service-tier-command)

(defun modelaria-format-usage-help (buffer)
  "Return BUFFER's usage report as help text."
  (let* ((report (modelaria-session-usage-report buffer))
         (usage (getf report :usage))
         (cost (getf report :estimated-cost))
         (pressure (getf report :context-pressure)))
    (format nil
            "Model Routing~%============~%~%Role: ~A [~(~A~)]~%Role set [~(~A~)]: ~{~A~^, ~}~%Provider/model: ~(~A~)/~A~%Thinking: ~A~%Service tier: ~A~%~%Usage~%=====~%~A~%~@[Estimated cost: ~$~%~]~@[Context pressure: ~,1F%%~%~]"
            (getf report :role)
            (getf report :role-scope)
            (getf report :role-set-scope)
            (or (getf report :role-set) '("default"))
            (getf report :provider)
            (getf report :model)
            (or (getf report :think-level) "default")
            (or (getf report :service-tier) "auto")
            (or (format-token-usage-summary usage) "tokens: unavailable")
            cost
            (and pressure (* 100.0 pressure)))))

(defun modelaria-show-usage-command (buffer)
  "Open a help buffer describing BUFFER's routing, usage, and cost estimate."
  (switch-to-buffer
   (make-help-buffer "*help:modelaria-usage*"
                     (modelaria-format-usage-help buffer))))
(defcommand modelaria-show-usage-command)

(defun modelaria-role-status-tool (_args)
  "Return effective routing state for the current buffer."
  (declare (ignore _args))
  (lisp-data-string
   (modelaria-session-usage-report
    (or (modelaria-current-buffer)
        (error "No current buffer for modelaria_role_status.")))))

(deftool modelaria-role-status-tool
  :name "modelaria_role_status"
  :description "Return the current buffer's effective model role, role set, routing, service tier, and usage summary."
  :args ())

(defun modelaria-session-usage-tool (_args)
  "Return the current buffer's usage report."
  (declare (ignore _args))
  (lisp-data-string
   (getf (modelaria-session-usage-report
          (or (modelaria-current-buffer)
              (error "No current buffer for modelaria_session_usage.")))
         :usage)))

(deftool modelaria-session-usage-tool
  :name "modelaria_session_usage"
  :description "Return the current buffer's aggregate token and cache usage."
  :args ())

(defun modelaria-clear-next-turn-role-after-send (_buffer _input _result)
  "Clear one-turn model role overrides after a send."
  (declare (ignore _input _result))
  (when _buffer
    (setf (buffer-next-turn-model-role-override _buffer) nil)))

(register-package-prompt-section
 "modelaria"
 "## Model roles with modelaria

- Use `modelaria_role_status` to inspect the current role, routing, service
  tier, and session usage.
- Use `modelaria_session_usage` when you only need token/cache totals.
- Named roles let the user scope routing per session or project and use a
  one-turn override without permanently changing the saved session."
 :title "Model roles with modelaria"
 :package "modelaria")

(add-hook '*after-send-message-hook* 'modelaria-clear-next-turn-role-after-send
          :append t)
