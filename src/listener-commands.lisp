(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Listener command dispatch: read-frame-command and todo-6 commands.
;; --------------------------------------------------------------------------
;;;
;;; read-frame-command accepts command-form-or-prose (todo 4) and maps each
;;; listener-input-token kind to a command form.  This file defines the hidden
;;; no-op/error/shell/mode commands, com-eval, and com-say.

(defun listener-token->command (token frame stream)
  "Map a listener-input-token to the command form for read-frame-command.

Pure function: no side effects, no input editing.  The :command case uses
climi::ensure-complete-command (intentional McCLIM core dependency, matching the
McCLIM Listener) to complete partial commands from the frame's command table."
  (let ((kind (listener-input-token-kind token))
        (value (listener-input-token-value token))
        (source (listener-input-token-source token)))
    (case kind
      ((:form :eval-form) (list 'com-eval value source))
      (:command (climi::ensure-complete-command
                 value (clim:frame-command-table frame) stream))
      (:prose (list 'com-say source))
      (:enter-say (list 'com-set-input-mode :say))
      (:exit-say (list 'com-set-input-mode :eval))
      (:shell (list 'com-run-shell source))
      (:no-op (list 'com-no-op))
      (:error (list 'com-report-input-error value))
      (t (error "Unknown listener input token kind: ~S" kind)))))

(defmethod clim:read-frame-command ((frame rplaca-listener)
                                    &key (stream *standard-input*))
  "Accept a command-form-or-prose token and map it to the next command form.

Uses *command-dispatchers* '(#\,) so comma is the command prefix.  Reader errors
remain input-editor/accept concerns; this method does not catch arbitrary
conditions (matching McCLIM Listener behavior at frames.lisp:506)."
  (let* ((*package* (listener-context-package
                     (rplaca-listener-context frame)))
         (clim:*command-dispatchers* '(#\,))
         (token (clim:accept 'command-form-or-prose
                             :stream stream :prompt nil)))
    (listener-token->command token frame stream)))

;;; --------------------------------------------------------------------------
;;; Hidden commands (no menu exposure, no user-facing name).
;;; --------------------------------------------------------------------------

(define-rplaca-listener-command (com-no-op :name nil) ()
  "Hidden: do nothing and return through normal command execution so the loop
re-prompts.  Used for empty/whitespace-only input."
  nil)

(define-rplaca-listener-command (com-report-input-error :name nil)
    ((hint string))
  "Hidden: write the input error hint to the interactor."
  (let ((stream (clim:frame-standard-output clim:*application-frame*)))
    (write-string hint stream)
    (terpri stream)))

;;; --------------------------------------------------------------------------
;;; Shell command (delegates to the todo-3 bounded helper).
;;; --------------------------------------------------------------------------

(define-rplaca-listener-command (com-run-shell :name nil)
    ((source string))
  "Hidden: run a shell command via the bounded run-listener-shell-command helper,
writing stdout/stderr/exit-status to the interactor.  Never mutates global cwd."
  (let* ((frame clim:*application-frame*)
         (buffer (rplaca-listener-conversation-buffer frame))
         (directory (uiop:ensure-directory-pathname
                     (buffer-working-directory buffer)))
         (result (run-listener-shell-command
                  source directory)))
    (let ((stream (clim:frame-standard-output frame)))
      (let ((stdout (getf result :stdout))
            (stderr (getf result :stderr))
            (exit-code (getf result :exit-code)))
        (when (and stdout (plusp (length stdout)))
          (write-string stdout stream))
        (when (and stderr (plusp (length stderr)))
          (write-string stderr stream))
        (when (/= 0 exit-code)
          (format stream "~&[exit ~A]~%" exit-code))))))

;;; --------------------------------------------------------------------------
;;; Input mode setter (used by enter-say/exit-say token kinds).
;;; --------------------------------------------------------------------------

(define-rplaca-listener-command (com-set-input-mode :name nil)
    ((mode (member :eval :say)))
  "Hidden: update the frame's listener-context input mode immutably so the next
prompt reflects the new mode."
  (let ((frame clim:*application-frame*))
    (setf (rplaca-listener-context frame)
          (listener-context-set-input-mode
           (rplaca-listener-context frame) mode))))

;;; --------------------------------------------------------------------------
;;; Say core: literal prose submission and one final inline assistant turn.
;;; --------------------------------------------------------------------------

(defun listener-write-input-error (frame condition)
  (let ((stream (clim:frame-standard-output frame)))
    (format stream "~A~%" condition)))

(defun listener-say-busy-p (frame buffer)
  (or (not (eq :live (rplaca-listener-liveness frame)))
      (buffer-agent-busy-p buffer)))

(defun require-listener-agent-turn-await-boundary ()
  (unless (fboundp 'await-listener-agent-turn)
    (error "AWAIT-LISTENER-AGENT-TURN is not installed; todo9 must provide the listener settlement boundary."))
  (symbol-function 'await-listener-agent-turn))

(defun run-listener-say-turn (frame buffer source)
  "Expand SOURCE, submit it once, await settlement, and emit one final turn."
  (when (listener-say-busy-p frame buffer)
    (listener-write-input-error frame "An agent turn is already active.")
    (return-from run-listener-say-turn nil))
  (let* ((context (rplaca-listener-context frame))
         (package (find-package (listener-context-package-name context)))
         (expanded
           (handler-case
               (expand-prose-interpolations source package #'eval)
             (prose-interpolation-error (condition)
               (listener-write-input-error frame condition)
               (return-from run-listener-say-turn nil)))))
    (let ((boundary nil)
          (captured-result nil)
          (captured-result-p nil))
      (labels ((capture-completion (hook-buffer hook-text result)
                 (when (and (eq buffer hook-buffer)
                            (string= expanded hook-text))
                   (setf captured-result result
                         captured-result-p t)
                   (when boundary
                     (setf (listener-turn-boundary-completion-result boundary)
                           result
                           (listener-turn-boundary-completion-result-p boundary)
                           t)))))
        (add-hook '*after-send-message-hook* #'capture-completion :append t)
        (unwind-protect
             (multiple-value-bind
                   (dispatch-result accepted-p turn-boundary)
                 (send-prose-message buffer expanded)
               (unless accepted-p
                 (return-from run-listener-say-turn nil))
               (setf boundary turn-boundary)
               (when captured-result-p
                 (setf (listener-turn-boundary-completion-result boundary)
                       captured-result
                       (listener-turn-boundary-completion-result-p boundary)
                       t))
               (let* ((await (require-listener-agent-turn-await-boundary))
                      (turn
                        (handler-case
                            (progn
                              (funcall await frame buffer dispatch-result)
                              (make-settled-listener-assistant-turn
                               buffer boundary :complete))
                          (prompt-run-error (condition)
                            (make-settled-listener-assistant-turn
                             buffer boundary :error
                             :condition condition
                             :primary-text (format nil "[Error: ~A]"
                                                   condition)))
                          (prompt-run-cancelled (condition)
                            (declare (ignore condition))
                            (make-settled-listener-assistant-turn
                             buffer boundary :cancelled
                             :primary-text "[Response cancelled.]")))))
                 (setf (rplaca-listener-pending-assistant-turn frame) turn)
                 (emit-listener-assistant-turn frame turn)))
          (remove-hook '*after-send-message-hook* #'capture-completion))))))

(define-rplaca-listener-command (com-say) ((source prose))
  "Send literal prose to the active listener's agent and own its inline turn."
  (let* ((frame clim:*application-frame*)
         (buffer (rplaca-listener-conversation-buffer frame)))
    (run-listener-say-turn frame buffer source)))

;;; --------------------------------------------------------------------------
;;; Eval core (todo 7): com-eval with bounded output, value presentations,
;;; REPL history, ASK-AGENT restart, and context synchronization.
;;; --------------------------------------------------------------------------

(defparameter +listener-eval-output-limit+ 20000)
(defparameter +listener-eval-truncation-marker+ " [...truncated]")

(defclass listener-bounded-output (sb-gray:fundamental-character-output-stream)
  ((target :initarg :target :reader bounded-target)
   (remaining :initarg :remaining :accessor bounded-remaining)
   (marker-len :initform (length +listener-eval-truncation-marker+)
               :reader bounded-marker-len)
   (marker-p :initform nil :accessor bounded-marker-p)))

(defmethod sb-gray:stream-write-char ((stream listener-bounded-output) char)
  (with-slots (target remaining marker-len marker-p) stream
    (cond
      ((> remaining marker-len)
       (write-char char target) (decf remaining))
      ((not marker-p)
       (setf marker-p t)
       (write-string +listener-eval-truncation-marker+ target)
       (setf remaining 0))
      (t nil))))

(defmethod sb-gray:stream-write-string ((stream listener-bounded-output)
                                         string &optional (start 0) end)
  (let* ((end (or end (length string)))
         (len (- end start)))
    (with-slots (target remaining marker-len marker-p) stream
      (unless marker-p
        (let* ((allowance (max 0 (- remaining marker-len)))
               (count (min len allowance)))
          (when (plusp count)
            (write-string string target :start start :end (+ start count))
            (decf remaining count))
          (when (> len count)
            (setf marker-p t)
            (write-string +listener-eval-truncation-marker+ target)
            (decf remaining marker-len))))))
  string)

(defmethod sb-gray:stream-line-column ((stream listener-bounded-output)) nil)
(defmethod sb-gray:stream-finish-output ((stream listener-bounded-output))
  (finish-output (bounded-target stream)))
(defmethod sb-gray:stream-force-output ((stream listener-bounded-output))
  (force-output (bounded-target stream)))

(defun listener-sync-eval-context (frame package directory)
  (setf (rplaca-listener-context frame)
        (listener-context-set-package
         (rplaca-listener-context frame) (package-name package)))
  (setf (buffer-working-directory (rplaca-listener-conversation-buffer frame))
        directory))

(defun listener-update-repl-history (form values)
  "Shuffle +/+++ and */*** unconditionally per the standard REPL contract.
Zero values sets * to NIL and still shifts ** /***."
  (setq cl:+++ cl:++ cl:++ cl:+ cl:+ form
        cl:/// cl:// cl:// cl:/ cl:/ values)
  (setq cl:*** cl:** cl:** cl:* cl:* (first values)))

(defun listener-eval-form (form source-text)
  "Core eval logic: evaluate FORM, present values, update history, offer ASK-AGENT.
Returns the source-text if ASK-AGENT was invoked, or NIL otherwise."
  (let* ((frame clim:*application-frame*)
         (interactor (clim:frame-standard-output frame))
         (bounded (make-instance 'listener-bounded-output
                                  :target interactor
                                  :remaining +listener-eval-output-limit+))
         (ask-tag (gensym "ASK-AGENT-TRANSFER")))
    (let ((*package* (listener-context-package
                      (rplaca-listener-context frame)))
          (*default-pathname-defaults*
            (listener-context-current-directory
             (rplaca-listener-context frame)
             (buffer-working-directory
              (rplaca-listener-conversation-buffer frame)))))
      (unwind-protect
           (catch ask-tag
             (let ((*standard-output* bounded)
                   (*error-output* bounded)
                   (*trace-output* bounded)
                   (*standard-input* interactor)
                   (cl:- form))
               (handler-bind
                   ((unbound-variable
                      (lambda (condition)
                        (restart-case
                            (invoke-debugger condition)
                          (ask-agent ()
                            :report "Ask the agent about this"
                            (throw ask-tag source-text)))))
                    (undefined-function
                      (lambda (condition)
                        (restart-case
                            (invoke-debugger condition)
                          (ask-agent ()
                            :report "Ask the agent about this"
                            (throw ask-tag source-text))))))
                 (let ((values (multiple-value-list (eval form))))
                   (listener-update-repl-history form values)
                   (let ((*print-length* 100)
                         (*print-level* 8)
                         (*print-circle* t)
                         (*print-readably* nil)
                         (*print-pretty* nil)
                         (*print-escape* t))
                     (dolist (value values)
                       (clim:with-output-as-presentation
                           (interactor value 'clim:expression)
                         (let ((*standard-output* bounded))
                           (prin1 value)
                           (terpri)))))))
             nil))
        (listener-sync-eval-context frame
                                    *package*
                                    *default-pathname-defaults*)))))

(define-rplaca-listener-command (com-eval) ((form t) (source-text string))
  (let ((ask-source (listener-eval-form form source-text)))
    (when (and ask-source (fboundp 'com-say))
      (funcall (symbol-function 'com-say) ask-source))))

;;; --------------------------------------------------------------------------
;;; Todo 10: Lisp/Say mode commands, Stop Response, and Compose.
;;; --------------------------------------------------------------------------

(define-rplaca-listener-command (com-lisp-mode :name "Lisp Mode")
    ()
  "Switch the listener to :eval input mode.  The prompt (and wholine, on the
next command-loop redisplay) reflect the new mode."
  (let ((frame clim:*application-frame*))
    (setf (rplaca-listener-context frame)
          (listener-context-set-input-mode
           (rplaca-listener-context frame) :eval))))

(define-rplaca-listener-command (com-say-mode :name "Say Mode")
    ()
  "Switch the listener to :say input mode."
  (let ((frame clim:*application-frame*))
    (setf (rplaca-listener-context frame)
          (listener-context-set-input-mode
           (rplaca-listener-context frame) :say))))

(define-rplaca-listener-command (com-stop-response :name "Stop Response")
    ()
  "Stop the active provider/tool response once, when one is busy.  Idempotent:
a second invocation finds nothing owned and is a no-op.  This is a defensive
escape hatch for busy-but-not-awaiting states; it does not duplicate the todo9
Esc/gesture cancellation that owns the in-await wait loop."
  (let* ((frame clim:*application-frame*)
         (buffer (rplaca-listener-conversation-buffer frame)))
    (when (and (buffer-agent-busy-p buffer)
               (not (buffer-runtime-stopping-p buffer)))
      (stop-streaming-response buffer))))

(declaim (notinline listener-read-compose-text))

(defun listener-read-compose-text (frame)
  "Open a multiline compose dialog and return the entered string, or NIL.

The default path is an accepting-values dialog with a text-editor pane so the
user can enter multiline prose that the single-line interactor rejects.  This
is the only seam between the dialog surface and the one-shot submission; tests
override it to drive com-say headlessly.  Drei is not used here to keep the
dialog McCLIM-native and dependency-light for this todo."
  (let ((stream (clim:frame-standard-output frame))
        (text nil))
    (handler-case
        (clim:accepting-values (stream :own-window t
                                       :label "Compose prose")
          (setf text
                 (clim:accept 'clim:string
                              :stream stream
                              :prompt "Prose"
                              :view clim:+text-editor-view+)))
      (error () nil))
    text))

(defun listener-compose-multiline-prose (frame)
  "Read multiline prose, reject blank/busy, and hand it to com-say exactly
once.  Blank and busy are rejected before any interpolation or send."
  (let ((buffer (rplaca-listener-conversation-buffer frame)))
    (when (listener-say-busy-p frame buffer)
      (listener-write-input-error frame "An agent turn is already active.")
      (return-from listener-compose-multiline-prose nil))
    (let ((text (listener-read-compose-text frame)))
      (cond
        ((or (null text) (blank-string-p text))
         (listener-write-input-error frame "Compose text is blank."))
        (t
         ;; Resolve through SYMBOL-FUNCTION so the same stub surface the rest
         ;; of the listener uses (see com-eval's ask-agent handoff) applies.
         (funcall (symbol-function 'com-say) text))))))

(define-rplaca-listener-command (com-compose :name "Compose")
    ()
  "Open a multiline compose dialog and submit the result once to com-say."
  (let ((frame clim:*application-frame*))
    (listener-compose-multiline-prose frame)))

(defun listener-command-buffer-directory (frame)
  (listener-context-current-directory
   (rplaca-listener-context frame)
   (buffer-working-directory (rplaca-listener-conversation-buffer frame))))

(defun listener-command-pathname (frame pathname)
  (merge-pathnames pathname (listener-command-buffer-directory frame)))

(defun listener-write-expression (stream object)
  (clim:with-output-as-presentation (stream object 'clim:expression)
    (prin1 object stream))
  (terpri stream))

(defun listener-command-line-names (table)
  (let ((names nil))
    (clim:map-over-command-table-commands
     (lambda (command)
       (let ((name (clim:command-line-name-for-command
                    command table :errorp nil)))
         (when name
           (push name names))))
     table)
    names))

(define-rplaca-listener-command (com-push-directory :name "Push Directory")
    ((directory clim:pathname))
  (let* ((frame clim:*application-frame*)
         (buffer (rplaca-listener-conversation-buffer frame))
         (context (rplaca-listener-context frame)))
    (multiple-value-bind (updated target)
        (listener-command-pushd
         context
         (listener-command-buffer-directory frame)
         (listener-command-pathname frame directory))
      (setf (rplaca-listener-context frame) updated
            (buffer-working-directory buffer) target)
      (format (clim:frame-standard-output frame)
              "Directory: ~A~%" (namestring target)))))

(define-rplaca-listener-command (com-pop-directory :name "Pop Directory") ()
  (let* ((frame clim:*application-frame*)
         (buffer (rplaca-listener-conversation-buffer frame)))
    (multiple-value-bind (updated target)
        (listener-command-popd (rplaca-listener-context frame))
      (setf (rplaca-listener-context frame) updated
            (buffer-working-directory buffer) target)
      (format (clim:frame-standard-output frame)
              "Directory: ~A~%" (namestring target)))))

(define-rplaca-listener-command
    (com-display-directory-stack :name "Display Directory Stack") ()
  (let* ((frame clim:*application-frame*)
         (stream (clim:frame-standard-output frame))
         (directories (listener-command-dirs
                       (rplaca-listener-context frame))))
    (if directories
        (progn
          (write-line "Directory stack:" stream)
          (loop :for directory :in directories
                :for index :from 0
                :do (format stream "~D: " index)
                    (clim:present directory 'clim:pathname :stream stream)
                    (terpri stream)))
        (write-line "Directory stack is empty." stream))))

(define-rplaca-listener-command (com-apropos :name "Apropos")
    ((text clim:string))
  (let* ((frame clim:*application-frame*)
         (stream (clim:frame-standard-output frame)))
    (dolist (symbol (listener-context-apropos
                     (rplaca-listener-context frame) text))
      (listener-write-expression stream symbol))))

(define-rplaca-listener-command (com-describe :name "Describe")
    ((object clim:form))
  (let* ((frame clim:*application-frame*)
         (stream (clim:frame-standard-output frame))
         (context (rplaca-listener-context frame)))
    (listener-write-expression stream object)
    (write-string (listener-context-describe context object) stream)))

(define-rplaca-listener-command (com-inspect :name "Inspect")
    ((form clim:form))
  (let* ((frame clim:*application-frame*)
         (stream (clim:frame-standard-output frame))
         (value (listener-context-inspect
                 (rplaca-listener-context frame) form)))
    (listener-write-expression stream value)))

(define-rplaca-listener-command (com-load-file :name "Load File")
    ((pathname clim:pathname))
  (let* ((frame clim:*application-frame*)
         (file (listener-context-load-file
                (rplaca-listener-context frame)
                (listener-command-pathname frame pathname))))
    (format (clim:frame-standard-output frame)
            "Loaded: ~A~%" (namestring file))))

(define-rplaca-listener-command (com-compile-file :name "Compile File")
    ((pathname clim:pathname))
  (let ((frame clim:*application-frame*))
    (multiple-value-bind (output warnings-p failure-p)
        (listener-context-compile-file
         (rplaca-listener-context frame)
         (listener-command-pathname frame pathname))
      (format (clim:frame-standard-output frame)
              "Compiled: ~A~@[ (warnings)~]~@[ (failed)~]~%"
              (and output (namestring output)) warnings-p failure-p))))

(define-rplaca-listener-command (com-in-package :name "In Package")
    ((package rplaca-package))
  (let ((frame clim:*application-frame*))
    (setf (rplaca-listener-context frame)
          (listener-context-in-package
           (rplaca-listener-context frame) package))
    (format (clim:frame-standard-output frame)
            "Package set to ~A~%" (package-name package))))

(define-rplaca-listener-command (com-room :name "Room") ()
  (let* ((frame clim:*application-frame*)
         (report (listener-context-room (rplaca-listener-context frame))))
    (write-string report (clim:frame-standard-output frame))))

(define-rplaca-listener-command (com-help-commands :name "Help Commands") ()
  (let* ((frame clim:*application-frame*)
         (table (clim:frame-command-table frame))
         (text (listener-context-help-commands
                (rplaca-listener-context frame)
                (listener-command-line-names table))))
    (write-string text (clim:frame-standard-output frame))))

;;; Listener-native parity selectors.  Candidate construction delegates to the
;;; established item builders; acceptance is owned by CLIM presentations in the
;;; listener interactor and never activates the legacy minibuffer.

(defun listener-model-choice-entries (buffer)
  (sort-models-by-recency
   (build-model-selector-items (available-models-for-selector buffer))))

(defun listener-think-level-choice-entries (buffer)
  (copy-list (or (available-think-levels-for-selector buffer) nil)))

(defun listener-buffer-choice-entries (buffer)
  (declare (ignore buffer))
  (mapcar (lambda (candidate)
            (list :buffer candidate
                  :display (buffer-name candidate)))
          (copy-list *buffer-ring*)))

(defun listener-session-choice-entries (buffer)
  (declare (ignore buffer))
  (mapcar (lambda (record)
            (append (copy-list record)
                    (list :display (session-selector-display-text record))))
          (listener-saved-session-records)))

(defun listener-skill-choice-entries (buffer)
  (declare (ignore buffer))
  (skill-selector-items :include-disabled t :include-enabled-marker t))

(defun listener-project-choice-entries (buffer)
  (project-selector-items (and buffer (buffer-project-name buffer))))

(defun listener-file-choice-entries (buffer)
  (let ((project (and buffer (current-buffer-project buffer))))
    (and project (project-file-selector-items project))))

(defun listener-selector-buffer (frame)
  (rplaca-listener-conversation-buffer frame))

(define-rplaca-listener-command (com-model :name "Model")
    ((choice listener-model-choice))
  (let* ((frame clim:*application-frame*)
         (buffer (listener-selector-buffer frame))
         (provider (getf choice :provider))
         (model (getf choice :model)))
    (multiple-value-bind (think-status think-level)
        (apply-buffer-model-selection buffer provider model)
      (record-model-selection-history (getf choice :display))
      (insert-model-selection-message
       buffer provider model think-status think-level))))

(define-rplaca-listener-command (com-think-level :name "Think-Level")
    ((choice listener-think-level-choice))
  (apply-buffer-think-level-selection
   (listener-selector-buffer clim:*application-frame*) choice))

(define-rplaca-listener-command (com-buffer :name "Buffer")
    ((choice listener-buffer-choice))
  (listener-activate-session-buffer clim:*application-frame*
                                    (if (typep choice 'buffer)
                                        choice
                                        (getf choice :buffer))))

(define-rplaca-listener-command (com-skill :name "Skill")
    ((choice listener-skill-choice))
  (let* ((buffer (listener-selector-buffer clim:*application-frame*))
         (skill (getf choice :skill))
         (enabled-p (not (skill-enabled-p skill))))
    (set-skill-enabled skill enabled-p)
    (buffer-insert-system-message
     buffer
     (format nil "[Skill ~A ~A]"
             (skill-name skill) (if enabled-p "enabled" "disabled")))
    enabled-p))

(define-rplaca-listener-command (com-package :name "Package")
    ((package rplaca-package))
  (let ((frame clim:*application-frame*))
    (setf (rplaca-listener-context frame)
          (listener-context-in-package
           (rplaca-listener-context frame) package))
    package))

(define-rplaca-listener-command (com-project :name "Project")
    ((choice listener-project-choice))
  (let* ((buffer (listener-selector-buffer clim:*application-frame*))
         (project (getf choice :project)))
    (setf (buffer-project-name buffer) (project-name project)
          (buffer-working-directory buffer) (project-root project))
    project))

(define-rplaca-listener-command (com-file :name "File")
    ((choice listener-file-choice))
  (project-open-file (getf choice :project) (getf choice :path)))

(define-rplaca-listener-command (com-safe-reload :name "Safe Reload") ()
  (let ((frame clim:*application-frame*))
    (start-interactive-safe-reload (listener-selector-buffer frame) frame)))

(defun listener-show-text-detail (frame text)
  (set-rplaca-listener-selected-detail frame text :text)
  (listener-set-details-layout frame)
  text)

(defun listener-frame-command-help-text (frame)
  (listener-context-help-commands
   (rplaca-listener-context frame)
   (listener-command-line-names (clim:frame-command-table frame))))

(defun listener-info-text (location)
  (info-document-display-text (info-fetch-location location)))

(define-rplaca-listener-command (com-help :name "Help") ()
  (let ((frame clim:*application-frame*))
    (listener-show-text-detail frame (listener-frame-command-help-text frame))))

(define-rplaca-listener-command (com-manual :name "Manual") ()
  (let ((frame clim:*application-frame*))
    (listener-show-text-detail
     frame (listener-info-text (resolve-rplaca-info-location "Top")))))

(define-rplaca-listener-command (com-info :name "Info") ()
  (let ((frame clim:*application-frame*))
    (listener-show-text-detail
     frame (listener-info-text (info-directory-location)))))

(define-rplaca-listener-command
    (com-reload-appearance :name "Reload Appearance") ()
  (listener-adopt-current-appearance clim:*application-frame*))

(define-rplaca-listener-command (com-new-session :name "New Session") ()
  "Create and activate a fresh persistent listener conversation."
  (listener-create-session clim:*application-frame*))

(define-rplaca-listener-command (com-list-sessions :name "List Sessions") ()
  "Display saved sessions as presentations that can be selected to resume."
  (let ((records (listener-saved-session-records)))
    (display-listener-session-list
     records
     (clim:frame-standard-output clim:*application-frame*))))

(define-rplaca-listener-command (com-resume-session :name "Resume Session")
    ((record saved-listener-session))
  "Load and activate an existing persistent listener conversation."
  (listener-resume-session clim:*application-frame* record))

(clim:define-presentation-to-command-translator
    com-resume-listener-session-translator
    (saved-listener-session com-resume-session rplaca-listener
     :gesture :select
     :documentation "Resume session"
     :pointer-documentation "Resume this session"
     :menu t)
    (object)
  (list object))
