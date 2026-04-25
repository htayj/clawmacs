(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Pipeline Data
;;; --------------------------------------------------------------------------

(defstruct pipeline-stage
  "One deterministic stage in an agent pipeline."
  name
  agent-name
  prompt
  next
  provider
  model
  think-level
  model-role
  service-tier
  tool-names
  tool-names-supplied-p
  package-names
  skill-names
  max-tool-iterations
  auto-approve-tools-p
  output-schema
  output-parser
  runner)

(defstruct pipeline-definition
  "Deterministic graph of agent prompt stages."
  name
  description
  stages
  entry-stage
  max-steps
  max-tool-iterations
  auto-approve-tools-p)

(defstruct pipeline-context
  "Mutable context accumulated while a pipeline is running."
  definition
  original-prompt
  buffer
  stage-results)

(defstruct pipeline-stage-result
  "Result of one deterministic pipeline stage."
  stage-name
  prompt
  result
  parsed-output
  status
  error
  started-at
  finished-at)

(defstruct pipeline-run-result
  "Result of an entire deterministic pipeline run."
  pipeline-name
  original-prompt
  stage-results
  final-stage-result
  status
  error)

(defvar *pipeline-definition-registry* (make-hash-table :test #'equal)
  "Programmatic deterministic pipeline definitions keyed by normalized name.")

(defstruct pipeline-test-profile
  "A named deterministic test profile runnable from a pipeline stage."
  name
  description
  command)

(defvar *pipeline-test-profile-registry* (make-hash-table :test #'equal)
  "Deterministic pipeline test profiles keyed by normalized name.")

;;; --------------------------------------------------------------------------
;;; Deterministic Pipelines
;;; --------------------------------------------------------------------------

(defun normalize-pipeline-name (name &optional (field-name "pipeline name"))
  "Normalize NAME into a stable pipeline or stage lookup key."
  (unless name
    (error "~A is required." field-name))
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (string name))))
    (when (blank-string-p trimmed)
      (error "~A must be non-empty." field-name))
    (string-downcase trimmed)))

(defun pipeline-name-or-nil (name)
  "Normalize NAME as a pipeline/stage name, treating NIL and stop markers as NIL."
  (cond
    ((null name) nil)
    ((member name '(:stop :done :end) :test #'eq) nil)
    (t (normalize-pipeline-name name "pipeline route target"))))

(defun normalize-pipeline-test-profile-name (name)
  "Normalize NAME into a stable pipeline test profile key."
  (normalize-pipeline-name name "pipeline test profile name"))

(defun pipeline-plist-value (plist &rest keys)
  "Return values VALUE and SUPPLIED-P for the first present key in PLIST."
  (dolist (key keys (values nil nil))
    (when (member key plist :test #'eq)
      (return (values (getf plist key) t)))))

(defun normalize-pipeline-skill-name-list (skill-names)
  "Normalize SKILL-NAMES into a duplicate-free list.
NIL means no explicit skill injection list."
  (when skill-names
    (let ((names (cond
                   ((or (stringp skill-names)
                        (symbolp skill-names))
                    (list skill-names))
                   ((vectorp skill-names)
                    (coerce skill-names 'list))
                   ((listp skill-names)
                    skill-names)
                   (t
                    (error "Skill names must be a string, symbol, list, vector, or NIL")))))
      (remove-duplicates
       (mapcar #'normalize-skill-name names)
       :test #'string=))))

(defun pipeline-stage-list-spec-value (value normalizer)
  "Normalize VALUE with NORMALIZER unless VALUE is a function."
  (if (functionp value)
      value
      (funcall normalizer value)))

(defun normalize-pipeline-stage-spec (spec)
  "Normalize SPEC into a PIPELINE-STAGE."
  (cond
    ((pipeline-stage-p spec) spec)
    ((and (listp spec) spec)
     (let* ((plist (if (keywordp (first spec))
                       spec
                       (list* :name (first spec) (rest spec))))
            (name (or (getf plist :name)
                      (error "Pipeline stage requires :name: ~S" spec))))
       (multiple-value-bind (tool-names tool-names-supplied-p)
           (pipeline-plist-value plist :tool-names :tools)
         (make-pipeline-stage
          :name (normalize-pipeline-name name "pipeline stage name")
          :agent-name (let ((agent-name (or (getf plist :agent-name)
                                            (getf plist :agent))))
                        (and agent-name (princ-to-string agent-name)))
          :prompt (getf plist :prompt)
          :next (getf plist :next)
          :provider (getf plist :provider)
          :model (getf plist :model)
          :think-level (or (getf plist :think-level)
                           (getf plist :reasoning-effort))
          :model-role (or (getf plist :model-role)
                          (getf plist :model_role))
          :service-tier (or (getf plist :service-tier)
                            (getf plist :service_tier))
          :tool-names (and tool-names-supplied-p
                           (pipeline-stage-list-spec-value
                            tool-names
                            #'normalize-tool-name-list))
          :tool-names-supplied-p tool-names-supplied-p
          :package-names
          (pipeline-stage-list-spec-value
           (or (getf plist :package-names)
               (getf plist :packages))
           #'normalize-package-name-list)
          :skill-names
          (pipeline-stage-list-spec-value
           (or (getf plist :skill-names)
               (getf plist :skills))
           #'normalize-pipeline-skill-name-list)
          :max-tool-iterations (getf plist :max-tool-iterations)
          :auto-approve-tools-p
          (not (null (getf plist :auto-approve-tools-p)))
          :output-schema (or (getf plist :output-schema)
                             (getf plist :output_schema)
                             (getf plist :schema))
          :output-parser (or (getf plist :output-parser)
                             (getf plist :parse-output)
                             (getf plist :parser))
          :runner (or (getf plist :runner)
                      (getf plist :run)
                      (getf plist :execute))))))
    (t
     (error "Pipeline stage must be a PIPELINE-STAGE or plist, got ~S" spec))))

(defun normalize-pipeline-stages (stages)
  "Return normalized STAGES as a list of PIPELINE-STAGE values."
  (let ((stage-list (cond
                      ((vectorp stages) (coerce stages 'list))
                      ((listp stages) stages)
                      (t
                       (error "Pipeline :stages must be a list or vector.")))))
    (unless stage-list
      (error "Pipeline requires at least one stage."))
    (let ((normalized (mapcar #'normalize-pipeline-stage-spec stage-list))
          (seen (make-hash-table :test #'equal)))
      (dolist (stage normalized)
        (let ((name (pipeline-stage-name stage)))
          (when (gethash name seen)
            (error "Duplicate pipeline stage name ~S." name))
          (setf (gethash name seen) t)))
      normalized)))

(defun register-pipeline-definition
    (name &key description stages entry-stage (max-steps 20)
            max-tool-iterations auto-approve-tools-p)
  "Register a deterministic agent pipeline.
STAGES is a list of stage plists. Each stage accepts :NAME, :AGENT,
:PROMPT, :NEXT, routing overrides, :TOOL-NAMES, :PACKAGES, and
:MAX-TOOL-ITERATIONS. :PROMPT and :NEXT may be functions for custom
deterministic behavior."
  (let* ((normalized-name (normalize-pipeline-name name))
         (normalized-stages (normalize-pipeline-stages stages))
         (entry (normalize-pipeline-name
                 (or entry-stage
                     (pipeline-stage-name (first normalized-stages)))
                 "pipeline entry stage")))
    (unless (find entry normalized-stages
                  :key #'pipeline-stage-name
                  :test #'string=)
      (error "Pipeline ~A entry stage ~A is not defined."
             normalized-name entry))
    (unless (and (integerp max-steps) (plusp max-steps))
      (error "Pipeline ~A max-steps must be a positive integer." normalized-name))
    (let ((definition
            (make-pipeline-definition
             :name normalized-name
             :description (or description "")
             :stages normalized-stages
             :entry-stage entry
             :max-steps max-steps
             :max-tool-iterations max-tool-iterations
             :auto-approve-tools-p (not (null auto-approve-tools-p)))))
      (setf (gethash normalized-name *pipeline-definition-registry*)
            definition)
      definition)))

(defun define-pipeline (name &rest options)
  "Register a deterministic agent pipeline from Lisp configuration."
  (apply #'register-pipeline-definition name options))

(defmacro defpipeline (name &rest options)
  "Define a deterministic pipeline using a symbol or string NAME."
  `(define-pipeline ,(if (symbolp name)
                         (string-downcase (symbol-name name))
                         name)
     ,@options))

(defun find-pipeline-definition (name)
  "Return the registered pipeline named NAME, or NIL."
  (when name
    (gethash (normalize-pipeline-name name)
             *pipeline-definition-registry*)))

(defun list-pipeline-definitions ()
  "Return registered pipeline definitions sorted by name."
  (let ((definitions nil))
    (maphash (lambda (_name definition)
               (declare (ignore _name))
               (push definition definitions))
             *pipeline-definition-registry*)
    (sort definitions #'string< :key #'pipeline-definition-name)))

(defun register-pipeline-test-profile (name &key description command)
  "Register a deterministic test profile runnable from a pipeline stage."
  (let ((normalized-name (normalize-pipeline-test-profile-name name)))
    (unless command
      (error "Pipeline test profile ~A requires :command." normalized-name))
    (setf (gethash normalized-name *pipeline-test-profile-registry*)
          (make-pipeline-test-profile
           :name normalized-name
           :description (or description "")
           :command command))))

(defun define-pipeline-test-profile (name &rest options)
  "Register a deterministic test profile from Lisp configuration."
  (apply #'register-pipeline-test-profile name options))

(defun find-pipeline-test-profile (name)
  "Return the deterministic test profile named NAME, or NIL."
  (when name
    (gethash (normalize-pipeline-test-profile-name name)
             *pipeline-test-profile-registry*)))

(defun list-pipeline-test-profiles ()
  "Return registered deterministic test profiles sorted by name."
  (let ((profiles nil))
    (maphash (lambda (_name profile)
               (declare (ignore _name))
               (push profile profiles))
             *pipeline-test-profile-registry*)
    (sort profiles #'string< :key #'pipeline-test-profile-name)))

(defun ensure-pipeline-definition (pipeline)
  "Return PIPELINE as a definition or signal a clear error."
  (cond
    ((pipeline-definition-p pipeline) pipeline)
    ((find-pipeline-definition pipeline))
    (t
     (error "Unknown pipeline: ~A" pipeline))))

(defun pipeline-stage-by-name (definition stage-name)
  "Return STAGE-NAME from DEFINITION or signal a clear error."
  (or (find (normalize-pipeline-name stage-name "pipeline stage name")
            (pipeline-definition-stages definition)
            :key #'pipeline-stage-name
            :test #'string=)
      (error "Pipeline ~A has no stage named ~A."
             (pipeline-definition-name definition)
             stage-name)))

(defun pipeline-stage-result-final-text (stage-result)
  "Return STAGE-RESULT's final text, or NIL when it failed before a result."
  (let ((result (and stage-result
                     (pipeline-stage-result-result stage-result))))
    (and result
         (prompt-run-result-final-text result))))

(defun pipeline-stage-result-succeeded-p (stage-result)
  "Return true when STAGE-RESULT completed without provider/tool errors."
  (eq :succeeded (pipeline-stage-result-status stage-result)))

(defun pipeline-last-stage-result (context)
  "Return the most recent stage result in CONTEXT."
  (car (last (pipeline-context-stage-results context))))

(defun pipeline-stage-output (context stage-name)
  "Return the most recent final text for STAGE-NAME in CONTEXT."
  (let ((target (normalize-pipeline-name stage-name "pipeline stage name")))
    (loop :for result :in (reverse (pipeline-context-stage-results context))
          :when (string= target (pipeline-stage-result-stage-name result))
            :return (pipeline-stage-result-final-text result))))

(defun pipeline-stage-parsed-output (context stage-name)
  "Return the most recent parsed output for STAGE-NAME in CONTEXT."
  (let ((target (normalize-pipeline-name stage-name "pipeline stage name")))
    (loop :for result :in (reverse (pipeline-context-stage-results context))
          :when (string= target (pipeline-stage-result-stage-name result))
            :return (pipeline-stage-result-parsed-output result))))

(defun replace-all-substrings (string needle replacement)
  "Return STRING with every NEEDLE replaced by REPLACEMENT."
  (let ((source (or string ""))
        (target (or needle ""))
        (value (or replacement "")))
    (when (zerop (length target))
      (return-from replace-all-substrings source))
    (with-output-to-string (out)
      (loop :with start := 0
            :for pos := (search target source :start2 start)
            :do (cond
                  (pos
                   (write-string source out :start start :end pos)
                   (write-string value out)
                   (setf start (+ pos (length target))))
                  (t
                   (write-string source out :start start)
                   (return)))))))

(defun render-pipeline-prompt-template (template context)
  "Expand simple pipeline placeholders in TEMPLATE."
  (let* ((previous (pipeline-last-stage-result context))
         (rendered (replace-all-substrings
                    template
                    "{{input}}"
                    (pipeline-context-original-prompt context)))
         (rendered (replace-all-substrings
                    rendered
                    "{{previous}}"
                    (or (pipeline-stage-result-final-text previous) ""))))
    (dolist (result (reverse (pipeline-context-stage-results context)) rendered)
      (let ((placeholder (format nil "{{stage:~A}}"
                                 (pipeline-stage-result-stage-name result))))
        (setf rendered
              (replace-all-substrings
               rendered
               placeholder
               (or (pipeline-stage-result-final-text result) "")))))))

(defun pipeline-stage-prompt-text (stage context)
  "Return the prompt text for STAGE in CONTEXT."
  (let ((prompt (pipeline-stage-prompt stage)))
    (cond
      ((functionp prompt)
       (let ((value (funcall prompt context)))
         (if (stringp value)
             value
             (princ-to-string value))))
      ((stringp prompt)
       (render-pipeline-prompt-template prompt context))
      ((null prompt)
       (render-pipeline-prompt-template "{{input}}" context))
      (t
       (princ-to-string prompt)))))

(defun pipeline-stage-next-name (stage context stage-result)
  "Return the next stage name for STAGE, or NIL to stop."
  (let ((next (pipeline-stage-next stage)))
    (pipeline-name-or-nil
     (cond
       ((functionp next)
        (funcall next context stage-result))
       (t next)))))

(defun effective-pipeline-stage-max-tool-iterations
    (stage definition caller-max-tool-iterations)
  "Return the effective tool iteration limit for STAGE."
  (or (pipeline-stage-max-tool-iterations stage)
      caller-max-tool-iterations
      (pipeline-definition-max-tool-iterations definition)
      *prompt-max-tool-iterations*))

(defun effective-pipeline-stage-auto-approve-tools-p
    (stage definition caller-auto-approve-tools-p)
  "Return true when STAGE should auto-approve permissioned tools."
  (or (pipeline-stage-auto-approve-tools-p stage)
      caller-auto-approve-tools-p
      (pipeline-definition-auto-approve-tools-p definition)))

(defun resolve-pipeline-stage-value (value context stage)
  "Resolve VALUE for STAGE in CONTEXT, calling functions when needed."
  (if (functionp value)
      (funcall value context stage)
      value))

(defun effective-pipeline-stage-tool-names (stage context)
  "Return the effective tool allowlist for STAGE."
  (let ((value (resolve-pipeline-stage-value
                (pipeline-stage-tool-names stage)
                context
                stage)))
    (if (pipeline-stage-tool-names-supplied-p stage)
        (normalize-tool-name-list value)
        nil)))

(defun effective-pipeline-stage-package-names
    (stage context original-package-names)
  "Return package names active for STAGE."
  (remove-duplicates
   (append (normalize-package-name-list
            (resolve-pipeline-stage-value
             (pipeline-stage-package-names stage)
             context
             stage))
           original-package-names)
   :test #'string=))

(defun effective-pipeline-stage-skill-names (stage context)
  "Return enabled skill names explicitly injected for STAGE."
  (let ((value (resolve-pipeline-stage-value
                (pipeline-stage-skill-names stage)
                context
                stage)))
    (remove-if-not
     (lambda (name)
       (ignore-errors (find-skill name)))
     (normalize-pipeline-skill-name-list value))))

(defun insert-pipeline-stage-context-message (buffer stage-name prompt)
  "Insert PROMPT as a provider-visible context message for STAGE-NAME."
  (buffer-insert-context-message
   buffer
   (format nil "[pipeline stage: ~A]~%~A" stage-name prompt)))

(defun insert-pipeline-stage-skill-context-messages (buffer skill-names)
  "Insert explicit contextual skill instructions for SKILL-NAMES."
  (dolist (name skill-names)
    (let ((skill (ignore-errors (find-skill name))))
      (when skill
        (buffer-insert-context-message
         buffer
         (render-skill-instructions-block skill))))))

(defun apply-pipeline-stage-output-parser (stage context stage-result)
  "Parse STAGE-RESULT using STAGE's output parser when one is configured."
  (let ((parser (pipeline-stage-output-parser stage))
        (schema (pipeline-stage-output-schema stage)))
    (cond
      (parser
       (let ((text (or (pipeline-stage-result-final-text stage-result) "")))
         (setf (pipeline-stage-result-parsed-output stage-result)
               (funcall parser text context stage stage-result))
         stage-result))
      (schema
       (or (and (pipeline-stage-result-result stage-result)
                (setf (pipeline-stage-result-parsed-output stage-result)
                      (prompt-run-result-structured-output
                       (pipeline-stage-result-result stage-result))))
           (apply-output-schema-to-pipeline-stage-result stage-result schema))
       stage-result)
      (t
       stage-result))))

(defun normalize-pipeline-stage-runner-result
    (runner-result stage-name prompt started-at finished-at)
  "Normalize RUNNER-RESULT into a PIPELINE-STAGE-RESULT."
  (cond
    ((pipeline-stage-result-p runner-result)
     (unless (pipeline-stage-result-stage-name runner-result)
       (setf (pipeline-stage-result-stage-name runner-result) stage-name))
     (unless (pipeline-stage-result-prompt runner-result)
       (setf (pipeline-stage-result-prompt runner-result) prompt))
     (unless (pipeline-stage-result-started-at runner-result)
       (setf (pipeline-stage-result-started-at runner-result) started-at))
     (unless (pipeline-stage-result-finished-at runner-result)
       (setf (pipeline-stage-result-finished-at runner-result) finished-at))
     runner-result)
    ((prompt-run-result-p runner-result)
     (make-pipeline-stage-result
      :stage-name stage-name
      :prompt prompt
      :result runner-result
      :status :succeeded
      :started-at started-at
      :finished-at finished-at))
    ((stringp runner-result)
     (make-pipeline-stage-result
      :stage-name stage-name
      :prompt prompt
      :result (make-prompt-run-result
               :prompt prompt
               :final-text runner-result
               :tool-events nil
               :reasoning-blocks nil
               :agent-name stage-name
               :iterations 1
               :stop-reason "pipeline_stage_complete")
      :status :succeeded
      :started-at started-at
      :finished-at finished-at))
    (t
     (error "Pipeline stage runner for ~A returned unsupported value ~S."
            stage-name runner-result))))

(defun run-pipeline-stage-on-buffer
    (context stage &key max-tool-iterations auto-approve-tools-p event-callback)
  "Run STAGE against CONTEXT's buffer and return a PIPELINE-STAGE-RESULT."
  (let* ((definition (pipeline-context-definition context))
         (buffer (pipeline-context-buffer context))
         (prompt (pipeline-stage-prompt-text stage context))
         (stage-name (pipeline-stage-name stage))
         (started-at (get-universal-time))
         (old-agent-name (buffer-agent-name buffer))
         (old-provider (buffer-provider-override buffer))
         (old-model (buffer-model-override buffer))
         (old-think-level (buffer-think-level-override buffer))
         (old-model-role (buffer-model-role-override buffer))
         (old-service-tier (buffer-service-tier-override buffer))
         (old-packages (copy-list (buffer-enabled-packages buffer)))
         (skill-names (effective-pipeline-stage-skill-names stage context)))
    (insert-pipeline-stage-skill-context-messages buffer skill-names)
    (insert-pipeline-stage-context-message buffer stage-name prompt)
    (unwind-protect
         (handler-case
             (progn
               (setf (buffer-agent-name buffer)
                     (or (pipeline-stage-agent-name stage)
                         old-agent-name)
                     (buffer-provider-override buffer)
                     (or (and (pipeline-stage-provider stage)
                              (normalize-provider
                               (pipeline-stage-provider stage)))
                         old-provider)
                     (buffer-model-override buffer)
                     (or (pipeline-stage-model stage)
                         old-model)
                     (buffer-think-level-override buffer)
                     (or (normalize-think-level-override
                          (pipeline-stage-think-level stage))
                         old-think-level)
                     (buffer-model-role-override buffer)
                     (or (normalize-model-role-override
                          (pipeline-stage-model-role stage))
                         old-model-role)
                     (buffer-service-tier-override buffer)
                     (or (normalize-service-tier-override
                          (pipeline-stage-service-tier stage))
                         old-service-tier)
                     (buffer-enabled-packages buffer)
                     (effective-pipeline-stage-package-names
                      stage context old-packages))
               (let* ((runner (pipeline-stage-runner stage))
                      (result (if runner
                                  (normalize-pipeline-stage-runner-result
                                   (funcall runner
                                            context
                                            stage
                                            prompt)
                                   stage-name
                                   prompt
                                   started-at
                                   (get-universal-time))
                                  (make-pipeline-stage-result
                                   :stage-name stage-name
                                   :prompt prompt
                                   :result
                                   (run-prompt-with-buffer
                                    buffer
                                    prompt
                                    nil
                                    (effective-pipeline-stage-max-tool-iterations
                                     stage definition max-tool-iterations)
                                   (effective-pipeline-stage-auto-approve-tools-p
                                     stage definition auto-approve-tools-p)
                                    (effective-pipeline-stage-tool-names
                                     stage context)
                                    (pipeline-stage-tool-names-supplied-p stage)
                                    :output-schema
                                    (pipeline-stage-output-schema stage)
                                    :event-callback event-callback)
                                   :status :succeeded
                                   :started-at started-at
                                   :finished-at (get-universal-time)))))
                 (apply-pipeline-stage-output-parser stage context result)))
           (error (condition)
             (make-pipeline-stage-result
              :stage-name stage-name
              :prompt prompt
              :status :failed
              :error (format nil "~A" condition)
              :started-at started-at
              :finished-at (get-universal-time))))
      (setf (buffer-agent-name buffer) old-agent-name
            (buffer-provider-override buffer) old-provider
            (buffer-model-override buffer) old-model
            (buffer-think-level-override buffer) old-think-level
            (buffer-model-role-override buffer) old-model-role
            (buffer-service-tier-override buffer) old-service-tier
            (buffer-enabled-packages buffer) old-packages))))

(defun pipeline-stage-working-directory (context)
  "Return the working directory used for deterministic pipeline commands."
  (let ((buffer (pipeline-context-buffer context)))
    (or (and buffer
             (buffer-working-directory buffer)
             (uiop:directory-exists-p
              (uiop:ensure-directory-pathname
               (buffer-working-directory buffer))))
        (uiop:ensure-directory-pathname (truename ".")))))

(defun pipeline-command-display-string (command)
  "Return COMMAND as a readable shell-ish string."
  (cond
    ((stringp command) command)
    ((listp command)
     (format nil "~{~A~^ ~}" command))
    (t
     (princ-to-string command))))

(defun run-pipeline-command (command &key directory)
  "Run COMMAND in DIRECTORY and return a plist result."
  (let ((working-directory
          (uiop:ensure-directory-pathname
           (or directory (truename ".")))))
    (multiple-value-bind (stdout stderr exit-code)
        (if (stringp command)
            (uiop:run-program (list "sh" "-lc" command)
                              :directory working-directory
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
            (uiop:run-program command
                              :directory working-directory
                              :output :string
                              :error-output :string
                              :ignore-error-status t))
      (list :command (pipeline-command-display-string command)
            :directory (namestring working-directory)
            :exit-code exit-code
            :stdout stdout
            :stderr stderr
            :passed-p (zerop exit-code)))))

(defun pipeline-test-profile-command-value (profile directory)
  "Return PROFILE's command in DIRECTORY."
  (let ((command (pipeline-test-profile-command profile)))
    (if (functionp command)
        (funcall command directory)
        command)))

(defun normalize-pipeline-test-profile-list (profile-names)
  "Normalize PROFILE-NAMES into a duplicate-free list."
  (when profile-names
    (let ((names (cond
                   ((or (stringp profile-names)
                        (symbolp profile-names))
                    (list profile-names))
                   ((vectorp profile-names)
                    (coerce profile-names 'list))
                   ((listp profile-names)
                    profile-names)
                   (t
                    (error "Pipeline test profiles must be a string, symbol, list, vector, or NIL")))))
      (remove-duplicates
       (mapcar #'normalize-pipeline-test-profile-name names)
       :test #'string=))))

(defun run-pipeline-test-profiles (profile-names &key directory)
  "Run PROFILE-NAMES sequentially and return a structured summary plist."
  (let* ((names (normalize-pipeline-test-profile-list profile-names))
         (working-directory
           (uiop:ensure-directory-pathname
            (or directory (truename "."))))
         (results nil))
    (dolist (name names)
      (let ((profile (find-pipeline-test-profile name)))
        (unless profile
          (error "Unknown pipeline test profile: ~A" name))
        (push (append (list :name (pipeline-test-profile-name profile))
                      (run-pipeline-command
                       (pipeline-test-profile-command-value
                        profile working-directory)
                       :directory working-directory))
              results)))
    (let* ((ordered-results (nreverse results))
           (passed-p (every (lambda (result) (getf result :passed-p))
                            ordered-results))
           (summary
             (with-output-to-string (out)
               (dolist (result ordered-results)
                 (format out "~A: ~:[FAIL~;PASS~] (exit ~D)~%"
                         (getf result :name)
                         (getf result :passed-p)
                         (getf result :exit-code)))
               (format out "~%Overall: ~:[FAILED~;PASSED~]"
                       passed-p))))
      (list :passed-p passed-p
            :directory (namestring working-directory)
            :profiles (copy-list names)
            :results ordered-results
            :summary summary))))

(defun pipeline-run-result-final-text (run-result)
  "Return RUN-RESULT's final stage text or error text."
  (let ((stage-result (pipeline-run-result-final-stage-result run-result)))
    (or (and stage-result
             (pipeline-stage-result-final-text stage-result))
        (pipeline-run-result-error run-result)
        "")))

(defun pipeline-run-result->prompt-run-result (run-result)
  "Summarize RUN-RESULT as a PROMPT-RUN-RESULT for prompt-mode output."
  (let ((tool-events nil)
        (reasoning nil)
        (usage nil)
        (iterations 0)
        (final-stage (pipeline-run-result-final-stage-result run-result))
        (final-result nil))
    (dolist (stage-result (pipeline-run-result-stage-results run-result))
      (let ((result (pipeline-stage-result-result stage-result)))
        (when result
          (setf tool-events
                (append tool-events
                        (copy-list (prompt-run-result-tool-events result)))
                reasoning
                (append reasoning
                        (copy-list
                         (prompt-run-result-reasoning-blocks result)))
                usage
                (merge-token-usage usage
                                   (prompt-run-result-usage result))
                iterations
                (+ iterations
                   (or (prompt-run-result-iterations result) 0))))))
    (setf final-result (and final-stage
                            (pipeline-stage-result-result final-stage)))
    (make-prompt-run-result
     :prompt (pipeline-run-result-original-prompt run-result)
     :final-text (pipeline-run-result-final-text run-result)
     :tool-events tool-events
     :reasoning-blocks reasoning
     :agent-name (pipeline-run-result-pipeline-name run-result)
     :provider (and final-result
                    (prompt-run-result-provider final-result))
     :model (and final-result
                 (prompt-run-result-model final-result))
     :think-level (and final-result
                       (prompt-run-result-think-level final-result))
     :structured-output (and final-result
                             (prompt-run-result-structured-output final-result))
     :iterations iterations
     :stop-reason (if (eq :succeeded (pipeline-run-result-status run-result))
                      "pipeline_complete"
                      "pipeline_failed")
     :usage usage
     :session-name (and final-result
                        (prompt-run-result-session-name final-result))
     :session-id (and final-result
                      (prompt-run-result-session-id final-result)))))

(defun run-pipeline-on-buffer
    (pipeline prompt &key buffer max-tool-iterations auto-approve-tools-p
                    event-callback)
  "Run PIPELINE deterministically against BUFFER for PROMPT."
  (let* ((definition (ensure-pipeline-definition pipeline))
         (context (make-pipeline-context
                   :definition definition
                   :original-prompt prompt
                   :buffer (or buffer
                               (error "run-pipeline-on-buffer requires :buffer"))
                   :stage-results nil))
         (current-stage-name (pipeline-definition-entry-stage definition))
         (step-count 0)
         (final-stage-result nil))
    (handler-case
        (loop
          (when (>= step-count (pipeline-definition-max-steps definition))
            (let ((message (format nil "Pipeline ~A exceeded max steps (~D)."
                                   (pipeline-definition-name definition)
                                   (pipeline-definition-max-steps definition))))
              (return-from run-pipeline-on-buffer
                (make-pipeline-run-result
                 :pipeline-name (pipeline-definition-name definition)
                 :original-prompt prompt
                 :stage-results (pipeline-context-stage-results context)
                 :final-stage-result final-stage-result
                 :status :failed
                 :error message))))
          (incf step-count)
          (let* ((stage (pipeline-stage-by-name definition current-stage-name))
                 (stage-result
                   (run-pipeline-stage-on-buffer
                    context stage
                    :max-tool-iterations max-tool-iterations
                    :auto-approve-tools-p auto-approve-tools-p
                    :event-callback event-callback)))
            (setf (pipeline-context-stage-results context)
                  (append (pipeline-context-stage-results context)
                          (list stage-result))
                  final-stage-result stage-result)
            (let ((next-stage-name
                    (pipeline-stage-next-name stage context stage-result)))
              (cond
                (next-stage-name
                 (setf current-stage-name next-stage-name))
                (t
                 (return
                   (make-pipeline-run-result
                    :pipeline-name (pipeline-definition-name definition)
                    :original-prompt prompt
                    :stage-results (pipeline-context-stage-results context)
                    :final-stage-result stage-result
                    :status (pipeline-stage-result-status stage-result)
                    :error (pipeline-stage-result-error stage-result))))))))
      (error (condition)
        (make-pipeline-run-result
         :pipeline-name (pipeline-definition-name definition)
         :original-prompt prompt
         :stage-results (pipeline-context-stage-results context)
         :final-stage-result final-stage-result
         :status :failed
         :error (format nil "~A" condition))))))

(defun run-pipeline-prompt (prompt pipeline-name
                            &key session-name
                              (agent-name *default-agent-name*)
                              provider model think-level
                              model-role service-tier
                              output-schema
                              (session-persistence-mode
                               *default-buffer-session-persistence-mode*)
                              (max-tool-iterations
                               *prompt-max-tool-iterations*)
                              auto-approve-tools-p
                              package-names
                              event-callback)
  "Run PROMPT through deterministic PIPELINE-NAME and return a prompt result."
  (when (blank-string-p prompt)
    (error "Prompt must be non-empty"))
  (let ((definition (ensure-pipeline-definition pipeline-name))
        (buf (if session-name
                 (let ((session-buffer
                         (make-session-prompt-buffer session-name agent-name)))
                   (append-session-prompt-input session-buffer prompt)
                   session-buffer)
                 (make-prompt-buffer
                  prompt agent-name
                  :session-persistence-mode session-persistence-mode))))
    (maybe-apply-prompt-routing-overrides buf provider model think-level
                                          :model-role model-role
                                          :service-tier service-tier)
    (when package-names
      (setf (buffer-enabled-packages buf)
            (normalize-package-name-list package-names)))
    (set-buffer-pipeline buf (pipeline-definition-name definition))
    (let ((run-result
            (run-pipeline-on-buffer definition
                                    prompt
                                    :buffer buf
                                    :max-tool-iterations max-tool-iterations
                                    :auto-approve-tools-p
                                    auto-approve-tools-p
                                    :event-callback event-callback)))
      (when (eq :failed (pipeline-run-result-status run-result))
        (let ((error-text (pipeline-run-result-error run-result)))
          (unless (blank-string-p error-text)
            (buffer-insert-system-message
             buf
            (format nil "[Pipeline error: ~A]" error-text)))))
      (apply-output-schema-to-prompt-run-result
       (pipeline-run-result->prompt-run-result run-result)
       output-schema))))

(defun run-pipeline-for-buffer (buffer prompt)
  "Run BUFFER's active pipeline for PROMPT, recording stage messages in BUFFER."
  (let ((pipeline-name (buffer-pipeline-name buffer)))
    (unless pipeline-name
      (error "Buffer ~A does not have an active pipeline."
             (buffer-name buffer)))
    (let ((definition (find-pipeline-definition pipeline-name)))
      (unless definition
        (setf (buffer-status buffer) :error)
        (buffer-insert-system-message
         buffer
         (format nil "[Pipeline error: unknown pipeline ~A]" pipeline-name))
        (return-from run-pipeline-for-buffer nil))
      (setf (buffer-status buffer) :thinking)
      (let ((run-result (run-pipeline-on-buffer definition
                                                prompt
                                                :buffer buffer)))
        (cond
          ((eq :succeeded (pipeline-run-result-status run-result))
           (setf (buffer-status buffer) :idle))
          (t
           (setf (buffer-status buffer) :error)
           (buffer-insert-system-message
            buffer
            (format nil "[Pipeline error: ~A]"
                    (or (pipeline-run-result-error run-result)
                        "pipeline failed")))))
        run-result))))

(defun buffer-has-active-pipeline-p (buffer)
  "Return true when BUFFER names a registered deterministic pipeline."
  (and (buffer-pipeline-name buffer)
       (find-pipeline-definition (buffer-pipeline-name buffer))))
