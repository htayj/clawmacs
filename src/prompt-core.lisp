(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Prompt Run Data
;;; --------------------------------------------------------------------------

(defvar *prompt-max-tool-iterations* 20
  "Default maximum tool-call turns allowed during one non-interactive prompt run.")

(defparameter +prompt-default-provider+ "openai-codex"
  "Default provider used by prompt.sh when no agent or provider is specified.")

(defparameter +prompt-default-model+ "gpt-5.3-codex"
  "Default model used by prompt.sh when no agent or model is specified.")

(defparameter +session-prompt-default-session-name+ "clawmacs:session-prompt"
  "Default saved session name used by session-prompt.sh.")

(defstruct prompt-tool-event
  "A tool call/result pair captured during non-interactive prompt execution."
  id
  name
  input
  result-text
  display
  denied-p)

(defstruct prompt-run-result
  "Result returned by RUN-SINGLE-PROMPT."
  prompt
  final-text
  tool-events
  reasoning-blocks
  structured-output
  agent-name
  provider
  model
  think-level
  service-tier
  iterations
  stop-reason
  usage
  session-name
  session-id)

(defstruct prompt-options
  "Command-line options for CLAWMACS-PROMPT-MAIN."
  prompt
  (agent-name *default-agent-name*)
  provider
  model
  think-level
  model-role
  service-tier
  (show-tools-p nil :type boolean)
  (show-reasoning-p nil :type boolean)
  (show-metadata-p nil :type boolean)
  (json-p nil :type boolean)
  (jsonl-p nil :type boolean)
  output-schema
  (auto-approve-tools-p nil :type boolean)
  (max-tool-iterations *prompt-max-tool-iterations* :type integer)
  (skill-roots nil :type list)
  (packages nil :type list)
  session-name
  (continue-session-p nil :type boolean)
  (ephemeral-p nil :type boolean)
  pipeline-name
  debug-log-path
  (isolated-p nil :type boolean)
  (inhibit-user-init-p nil :type boolean)
  (help-p nil :type boolean))

(defun default-prompt-working-directory ()
  "Return the working directory for one-shot prompt buffers."
  (normalize-buffer-working-directory
   (cond
     ((and (boundp '*current-tool-buffer*) *current-tool-buffer*)
      (buffer-working-directory *current-tool-buffer*))
     (t
      (truename ".")))))

(define-condition prompt-run-error (error)
  ((message :initarg :message :reader prompt-run-error-message)
   (tool-events :initarg :tool-events
                :initform nil
                :reader prompt-run-error-tool-events)
   (iterations :initarg :iterations
               :initform 0
               :reader prompt-run-error-iterations)
   (provider :initarg :provider
             :initform nil
             :reader prompt-run-error-provider)
   (model :initarg :model
          :initform nil
          :reader prompt-run-error-model)
   (think-level :initarg :think-level
                :initform nil
                :reader prompt-run-error-think-level))
  (:report (lambda (condition stream)
             (format stream "~A" (prompt-run-error-message condition)))))

(defun prompt-run-tool-names (result)
  "Return tool names used by RESULT in chronological order."
  (mapcar #'prompt-tool-event-name
          (prompt-run-result-tool-events result)))

(defun prompt-run-tool-count (result &optional tool-name)
  "Count tool events in RESULT, optionally restricted to TOOL-NAME."
  (let ((events (prompt-run-result-tool-events result)))
    (if tool-name
        (count (normalize-tool-name tool-name)
               events
               :key (lambda (event)
                      (normalize-tool-name
                       (prompt-tool-event-name event)))
               :test #'string=)
        (length events))))

(defun prompt-run-used-tool-p (result tool-name)
  "Return true when RESULT includes at least one TOOL-NAME call."
  (plusp (prompt-run-tool-count result tool-name)))
