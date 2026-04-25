(in-package :clawmacs)

(defvar *modelaria-global-config-path*
  (merge-pathnames #P".clawmacs.d/modelaria.json" (user-homedir-pathname))
  "User config path for modelaria scope defaults.")

(defvar *modelaria-project-config-filename* ".clawmacs-modelaria.json"
  "Project-local modelaria config filename.")

(defparameter *modelaria-built-in-role-order*
  '("default" "cheap" "plan" "review" "slow")
  "Built-in ordered model roles used when no scoped role set is configured.")

(defparameter *modelaria-openai-price-table*
  '(("gpt-5.4" :input 2.50d0 :cached-input 0.25d0 :output 15.00d0)
    ("gpt-5.2-codex" :input 1.75d0 :cached-input 0.175d0 :output 14.00d0)
    ("gpt-5.2" :input 1.75d0 :cached-input 0.175d0 :output 14.00d0)
    ("gpt-5.1-codex-max" :input 1.25d0 :cached-input 0.125d0 :output 10.00d0))
  "Approximate OpenAI token prices per 1M tokens for usage reporting.")

(defun modelaria-package-active-p (&optional buffer)
  "Return true when the modelaria package is active for BUFFER."
  (package-active-p "modelaria" :buffer buffer))

(defun modelaria-blank-string-p (value)
  "Return true when VALUE is NIL or ASCII-whitespace only."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (string value))))))

(defun normalize-modelaria-role-name (value)
  "Normalize VALUE into a stable modelaria role name, or NIL."
  (unless (modelaria-blank-string-p value)
    (string-downcase
     (string-trim '(#\Space #\Tab #\Newline #\Return)
                  (string value)))))

(defun normalize-modelaria-role-set (value)
  "Normalize VALUE into an ordered unique role-name list."
  (let ((roles nil))
    (dolist (raw (or value '()) (nreverse roles))
      (let ((role (normalize-modelaria-role-name raw)))
        (when (and role
                   (not (member role roles :test #'string=)))
          (push role roles))))))

(defun normalize-modelaria-service-tier (value)
  "Normalize VALUE into an allowed service-tier string, or NIL."
  (let ((normalized (and value
                         (string-downcase
                          (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (string value))))))
    (cond
      ((or (null normalized) (zerop (length normalized))) nil)
      ((member normalized '("auto" "default" "flex" "priority")
               :test #'string=)
       normalized)
      (t
       (error "Unsupported service tier ~S. Expected auto, default, flex, or priority."
              value)))))

(defun modelaria-json-string-key (key)
  "Return KEY as a lowercase config field name.
Hyphens and underscores are treated as equivalent."
  (let ((raw (string-downcase
              (etypecase key
                (string key)
                (symbol (symbol-name key))))))
    (with-output-to-string (out)
      (let ((last-separator-p nil))
        (loop :for ch :across raw
              :do (if (or (char= ch #\-) (char= ch #\_))
                      (unless last-separator-p
                        (write-char #\_ out)
                        (setf last-separator-p t))
                      (progn
                        (write-char ch out)
                        (setf last-separator-p nil))))))))

(defun modelaria-json-value (alist key)
  "Return KEY's value from decoded JSON ALIST."
  (let ((name (string-downcase key)))
    (loop :for (entry-key . entry-value) :in alist
          :when (string= name (modelaria-json-string-key entry-key))
            :return entry-value)))

(defun normalize-modelaria-scope-config (data)
  "Normalize decoded config DATA into a plist."
  (let ((active-role (normalize-modelaria-role-name
                      (modelaria-json-value data "active_role")))
        (role-set (normalize-modelaria-role-set
                   (coerce (or (modelaria-json-value data "role_set") #())
                           'list)))
        (service-tier (ignore-errors
                        (normalize-modelaria-service-tier
                         (modelaria-json-value data "service_tier")))))
    (list :active-role active-role
          :role-set role-set
          :service-tier service-tier)))

(defun read-modelaria-scope-config (path)
  "Read modelaria config from PATH and return a normalized plist."
  (when (probe-file path)
    (let ((cl-json:*json-array-type* 'vector))
      (normalize-modelaria-scope-config
       (cl-json:decode-json-from-string (uiop:read-file-string path))))))

(defun write-modelaria-scope-config (path config)
  "Persist normalized CONFIG plist to PATH."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string
     (cl-json:encode-json-to-string
      `((:active_role . ,(getf config :active-role))
        (:role_set . ,(coerce (copy-list (getf config :role-set)) 'vector))
        (:service_tier . ,(getf config :service-tier))))
     stream))
  path)

(defun modelaria-global-config ()
  "Return normalized global modelaria config."
  (or (read-modelaria-scope-config *modelaria-global-config-path*)
      (list :active-role nil
            :role-set nil
            :service-tier nil)))

(defun save-modelaria-global-config (config)
  "Persist normalized global modelaria CONFIG."
  (write-modelaria-scope-config *modelaria-global-config-path* config))

(defun modelaria-project-config-path (&optional buffer)
  "Return BUFFER's project-local modelaria config path, or NIL."
  (let* ((root (and buffer (buffer-working-directory buffer)))
         (dir (and root
                   (ignore-errors
                     (uiop:ensure-directory-pathname root)))))
    (when (and dir (uiop:directory-exists-p dir))
      (merge-pathnames *modelaria-project-config-filename* dir))))

(defun modelaria-project-config (&optional buffer)
  "Return normalized project-local modelaria config for BUFFER, or NIL."
  (let ((path (modelaria-project-config-path buffer)))
    (and path (or (read-modelaria-scope-config path)
                  (list :active-role nil
                        :role-set nil
                        :service-tier nil)))))

(defun save-modelaria-project-config (buffer config)
  "Persist normalized project-local modelaria CONFIG for BUFFER."
  (let ((path (or (modelaria-project-config-path buffer)
                  (error "No project directory for buffer ~A." (buffer-name buffer)))))
    (write-modelaria-scope-config path config)))

(defun modelaria-buffer-direct-routing-overrides-p (buffer)
  "Return true when BUFFER has explicit direct provider/model/think overrides."
  (or (buffer-provider-override buffer)
      (buffer-model-override buffer)
      (buffer-think-level-override buffer)))

(defun modelaria-buffer-effective-role-set (buffer)
  "Return BUFFER's ordered role set and its source scope."
  (cond
    ((buffer-model-role-set-override buffer)
     (values (copy-list (buffer-model-role-set-override buffer)) :session))
    ((let ((project (modelaria-project-config buffer)))
       (and project (getf project :role-set)))
     (values (copy-list (getf (modelaria-project-config buffer) :role-set))
             :project))
    ((getf (modelaria-global-config) :role-set)
     (values (copy-list (getf (modelaria-global-config) :role-set)) :global))
    (t
     (values (copy-list *modelaria-built-in-role-order*) :builtin))))

(defun modelaria-buffer-effective-role (buffer)
  "Return BUFFER's effective role name and scope."
  (cond
    ((buffer-next-turn-model-role-override buffer)
     (values (buffer-next-turn-model-role-override buffer) :next-turn))
    ((buffer-model-role-override buffer)
     (values (buffer-model-role-override buffer) :session))
    ((let ((project (modelaria-project-config buffer)))
       (and project (getf project :active-role)))
     (values (getf (modelaria-project-config buffer) :active-role) :project))
    ((getf (modelaria-global-config) :active-role)
     (values (getf (modelaria-global-config) :active-role) :global))
    (t
     (values "default" :builtin))))

(defun modelaria-role-description (role)
  "Return a concise description for built-in ROLE."
  (cond
    ((string= role "default")
     "Use the normal agent/session routing.")
    ((string= role "cheap")
     "Prefer a lower-cost coding model when the provider supports it.")
    ((string= role "plan")
     "Prefer a deliberate planner-grade model with higher reasoning.")
    ((string= role "review")
     "Prefer a careful reviewer-grade model with higher reasoning.")
    ((string= role "slow")
     "Prefer the slowest, highest-effort model available for the provider.")
    (t
     "Custom model-routing role.")))

(defun modelaria-role-routing (role provider model think-level)
  "Return routing values for ROLE over PROVIDER/MODEL/THINK-LEVEL."
  (let ((normalized (or (normalize-modelaria-role-name role) "default")))
    (cond
      ((string= normalized "cheap")
       (cond
         ((eq provider :openrouter)
          (values provider "openai/gpt-4o" nil))
         (t
          (values :openai-codex "gpt-5.2-codex" "low"))))
      ((string= normalized "plan")
       (cond
         ((eq provider :openrouter)
          (values provider "openai/gpt-5.2" nil))
         (t
          (values :openai-codex "gpt-5.4" "high"))))
      ((string= normalized "review")
       (cond
         ((eq provider :openrouter)
          (values provider "openai/gpt-5.2" nil))
         (t
          (values :openai-codex "gpt-5.4" "high"))))
      ((string= normalized "slow")
       (cond
         ((eq provider :openrouter)
          (values provider "openai/gpt-5.1" nil))
         (t
          (values :openai-codex "gpt-5.1-codex-max" "high"))))
      (t
       (values provider model think-level)))))

(defun apply-modelaria-routing (buffer provider model think-level)
  "Return effective routing for BUFFER over base PROVIDER/MODEL/THINK-LEVEL."
  (if (not (modelaria-package-active-p buffer))
      (values provider model think-level)
      (multiple-value-bind (role scope)
          (modelaria-buffer-effective-role buffer)
        (declare (ignore scope))
        (if (and (not (string= role "default"))
                 (or (eq (nth-value 1 (modelaria-buffer-effective-role buffer))
                         :next-turn)
                     (not (modelaria-buffer-direct-routing-overrides-p buffer))))
            (modelaria-role-routing role provider model think-level)
            (values provider model think-level)))))

(defun resolve-buffer-service-tier (buffer)
  "Return BUFFER's effective service-tier preference, or NIL."
  (when (modelaria-package-active-p buffer)
    (or (buffer-service-tier-override buffer)
        (let ((project (modelaria-project-config buffer)))
          (and project (getf project :service-tier)))
        (getf (modelaria-global-config) :service-tier))))

(defun service-tier-cost-multiplier (service-tier)
  "Return the billing multiplier for SERVICE-TIER."
  (let ((tier (ignore-errors
                (normalize-modelaria-service-tier service-tier))))
    (cond
      ((or (null tier) (string= tier "auto") (string= tier "default")) 1.0d0)
      ((string= tier "flex") 0.5d0)
      ((string= tier "priority") 2.0d0)
      (t 1.0d0))))

(defun modelaria-price-spec (provider model)
  "Return a pricing plist for PROVIDER and MODEL, or NIL."
  (cond
    ((eq provider :openai-codex)
     (cdr (assoc model *modelaria-openai-price-table* :test #'string=)))
    (t nil)))

(defun modelaria-estimated-usage-cost (usage provider model &key service-tier)
  "Return approximate dollar cost for USAGE on PROVIDER/MODEL, or NIL."
  (let ((price (modelaria-price-spec provider model)))
    (when price
      (let* ((input (or (getf usage :uncached-input-tokens)
                        (getf usage :input-tokens)
                        0))
             (cached (or (getf usage :cached-input-tokens) 0))
             (output (or (getf usage :output-tokens) 0))
             (multiplier (service-tier-cost-multiplier service-tier)))
        (* multiplier
           (+ (* (/ input 1000000.0d0) (getf price :input 0.0d0))
              (* (/ cached 1000000.0d0) (getf price :cached-input 0.0d0))
              (* (/ output 1000000.0d0) (getf price :output 0.0d0))))))))

(defun modelaria-session-usage-report (buffer)
  "Return a plist summarizing BUFFER's current routing and session usage."
  (multiple-value-bind (provider model think-level)
      (resolve-buffer-provider-and-model buffer)
    (multiple-value-bind (role scope)
        (modelaria-buffer-effective-role buffer)
      (multiple-value-bind (role-set role-set-scope)
          (modelaria-buffer-effective-role-set buffer)
        (let* ((service-tier (resolve-buffer-service-tier buffer))
               (usage (buffer-session-usage buffer))
               (context-limit (buffer-context-limit buffer))
               (context-pressure
                 (and (plusp context-limit)
                      (/ (float (buffer-token-count buffer))
                         context-limit)))
               (estimated-cost
                 (and usage
                      (modelaria-estimated-usage-cost usage provider model
                                                      :service-tier service-tier))))
          (list :provider provider
                :model model
                :think-level think-level
                :role role
                :role-scope scope
                :role-set (copy-list role-set)
                :role-set-scope role-set-scope
                :service-tier service-tier
                :usage (and usage (copy-list usage))
                :context-pressure context-pressure
                :estimated-cost estimated-cost))))))
