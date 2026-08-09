(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Buffer
;;; --------------------------------------------------------------------------

(defvar *default-context-limit* 200000
  "Default token context window size for new buffers.")

(defvar *default-agent-name* "agent"
  "Default agent name for new buffers. Affects modeline display and agent-defaults resolution.")

(defvar *default-show-tool-results* t
  "When nil, new buffers hide tool-result messages by default.")

(defvar *default-collapse-tool-activity* t
  "When non-nil, new buffers collapse consecutive tool calls/results in transcript display.")

(defvar *default-show-reasoning-output* nil
  "When non-nil, new buffers show provider-supplied reasoning blocks by default.")

(defvar *default-show-metadata-output* nil
  "When non-nil, new buffers show provider/response metadata by default.")

(defvar *default-pipeline-name* nil
  "When non-nil, new chat buffers run this pipeline when the user sends input.")

(defvar *default-buffer-session-persistence-mode* :persistent
  "Default session persistence mode for newly created buffers.")

(defvar *buffer-system-prompt-display-enabled* t
  "When non-nil, interactive chat buffers show a synthetic system-prompt header.")

(defvar *scratch-buffer-name* "*scratch*"
  "Name used for the process-local scratch buffer.")

(defvar *scratch-buffer-initial-text* ""
  "Initial text inserted into the scratch buffer when it is created.")

(defvar *current-rplaca-package* nil
  "Package name dynamically bound while loading a package entrypoint.")

(defvar *buffer-display-wakeup-hook* nil
  "Internal UI wakeups for buffer display changes.

Unlike `*after-buffer-display-change-hook*', this is not an extension point.
Its functions may be called by managed workers and therefore may only enqueue
an application event for the owning frame; they must not inspect or mutate
application state on the caller's thread.")

(defun maybe-run-hook-with-args (hook-var &rest args)
  "Run HOOK-VAR with ARGS when the hooks system has been loaded."
  (when (and (boundp hook-var)
             (fboundp 'run-hook-with-args))
    (apply (symbol-function 'run-hook-with-args) hook-var args)))

(defun wake-buffer-display-change (buf reason)
  "Queue internal UI wakeups for BUF without invoking extension observers."
  (when buf
    (maybe-run-hook-with-args '*buffer-display-wakeup-hook* buf reason))
  buf)

(defun notify-buffer-display-change (buf reason)
  "Queue BUF redisplay and notify application observers of REASON."
  (when buf
    (wake-buffer-display-change buf reason)
    (maybe-run-hook-with-args '*after-buffer-display-change-hook* buf reason))
  buf)

(defstruct buffer-type
  "A registered buffer kind and its optional UI presentation hooks."
  (name nil :type (or null keyword))
  (description nil :type (or null string))
  (major-mode nil :type (or null string))
  (document-p nil :type boolean)
  presentation-function
  input-presentation-function
  serialize-state-function
  restore-state-function
  (package nil :type (or null string)))

(declaim (ftype (function (t) keyword) normalize-buffer-kind)
         (ftype (function (t) string) buffer-kind-default-major-mode)
         (ftype (function (t t) string) normalize-buffer-major-mode)
         (ftype (function (t) (or null string)) normalize-buffer-type-package-name)
         (ftype (function (t t) (or null function symbol))
                normalize-buffer-type-function)
         (ftype (function (t) (or null function symbol))
                buffer-type-state-serializer
                buffer-type-state-restorer)
         (ftype (function () hash-table) make-buffer-type-registry))

(defun normalize-buffer-kind (kind)
  "Normalize KIND into the keyword stored in BUFFER-KIND."
  (cond
    ((keywordp kind) kind)
    ((symbolp kind) (intern (string-upcase (symbol-name kind)) :keyword))
    ((stringp kind)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) kind)))
       (unless (plusp (length trimmed))
         (error "Buffer kind must not be blank."))
       (intern (string-upcase trimmed) :keyword)))
    (t
     (error "Buffer kind must be a keyword, symbol, or string: ~S" kind))))

(defun buffer-kind-default-major-mode (kind)
  "Return the default major-mode string for normalized KIND."
  (string-downcase (symbol-name (normalize-buffer-kind kind))))

(defun normalize-buffer-major-mode (major-mode kind)
  "Return a non-empty major-mode string for KIND."
  (cond
    ((null major-mode)
     (buffer-kind-default-major-mode kind))
    ((and (stringp major-mode)
          (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      major-mode))))
     major-mode)
    ((symbolp major-mode)
     (string-downcase (symbol-name major-mode)))
    (t
     (error "Major mode must be a string, symbol, or NIL: ~S" major-mode))))

(defun normalize-buffer-type-package-name (package)
  "Normalize PACKAGE ownership metadata for a buffer type."
  (cond
    ((null package) nil)
    ((stringp package)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) package)))
       (and (plusp (length trimmed))
            (string-downcase trimmed))))
    ((symbolp package)
     (string-downcase (symbol-name package)))
    (t
     (error "Buffer type package must be a string, symbol, or NIL: ~S" package))))

(defun normalize-buffer-type-function (function role)
  "Validate FUNCTION as an optional function designator for ROLE."
  (cond
    ((null function) nil)
    ((functionp function) function)
    ((and (symbolp function) (fboundp function)) function)
    (t
     (error "Buffer type ~A must be an existing function designator: ~S"
            role
            function))))

(defun buffer-type-state-serializer (type)
  "Return TYPE's optional persistence serializer function."
  (and type (buffer-type-serialize-state-function type)))

(defun buffer-type-state-restorer (type)
  "Return TYPE's optional persistence restore function."
  (and type (buffer-type-restore-state-function type)))

(defun make-buffer-type-registry ()
  "Return a fresh buffer type registry seeded with built-in buffer kinds."
  (let ((registry (make-hash-table :test #'eq)))
    (flet ((install (name description major-mode document-p)
             (setf (gethash name registry)
                   (make-buffer-type
                    :name name
                    :description description
                    :major-mode major-mode
                    :document-p document-p
                    :package nil))))
      (install :chat "Default agent conversation buffer." "chat" nil)
      (install :help "Read-only help buffer." "help" nil)
      (install :info "Read-only Info manual browser." "info" nil)
      (install :font-editor "Interactive CADR-style bitmap font editor." "font-editor" nil)
      (install :scratch "Editable scratch buffer." "scratch" t)
      (install :file "Project-backed editable file buffer." "file" t))
    registry))

(defvar *buffer-type-registry* (make-buffer-type-registry)
  "Registry of known buffer kinds.

Packages may add entries with REGISTER-BUFFER-TYPE or DEFINE-BUFFER-TYPE.
Registered presentation functions are retained as metadata for future
interfaces.")

(defvar *process-buffer-type-registry* *buffer-type-registry*
  "Process-global buffer type registry, distinct from dynamic test bindings.")

(defvar *buffer-type-registry-lock*
  (bt:make-lock "rplaca buffer type registry")
  "Lock guarding process-global buffer types and presentation providers.")

(defun call-with-buffer-type-registry-lock
    (function &optional (table *buffer-type-registry*))
  "Call FUNCTION under the buffer registry lock when TABLE is process-global.

FUNCTION must do bounded registry or provider-list access only.  Presentation
functions and package visibility checks must run after this lock is released."
  (if (eq table *process-buffer-type-registry*)
      (bt:with-lock-held (*buffer-type-registry-lock*)
        (funcall function))
      (funcall function)))

(defun buffer-type-registry-snapshot (&optional (table *buffer-type-registry*))
  "Return a stable alist snapshot of buffer type TABLE."
  (call-with-buffer-type-registry-lock
   (lambda ()
     (let ((entries nil))
       (maphash (lambda (name type)
                  (push (cons name type) entries))
                table)
       entries))
   table))

(defun remove-buffer-types-for-package (package-name)
  "Atomically remove buffer types owned by PACKAGE-NAME."
  (let ((name (normalize-buffer-type-package-name package-name)))
    (when name
      (call-with-buffer-type-registry-lock
       (lambda ()
         (let ((removed nil))
           (maphash
            (lambda (kind type)
              (when (string= name (or (buffer-type-package type) ""))
                (push type removed)
                (remhash kind *buffer-type-registry*)))
            *buffer-type-registry*)
           (nreverse removed)))
       *buffer-type-registry*))))

(defstruct buffer-input-presentation-provider
  "Package-owned input overlay presenter for an existing buffer kind."
  (kind nil :type (or null keyword))
  function
  (package nil :type (or null string)))

(defvar *buffer-input-presentation-providers* nil
  "Package-owned input presentation providers for existing buffer kinds.")

(defun buffer-input-presentation-provider-snapshot ()
  "Return a stable copy of the current input presentation provider list."
  (call-with-buffer-type-registry-lock
   (lambda () (copy-list *buffer-input-presentation-providers*))
   *buffer-type-registry*))

(defun register-buffer-input-presentation-provider
    (kind function &key (package nil package-supplied-p))
  "Register FUNCTION as a package-owned input overlay for buffer KIND."
  (when (and *current-rplaca-package*
             (not (package-resource-type-allowed-p :buffer-type)))
    (return-from register-buffer-input-presentation-provider nil))
  (let* ((normalized-kind (normalize-buffer-kind kind))
         (normalized-function
           (normalize-buffer-type-function function :input-presentation-provider))
         (owner (normalize-buffer-type-package-name
                 (cond
                   (package-supplied-p package)
                   (*current-rplaca-package*)
                   (t nil))))
         (provider (make-buffer-input-presentation-provider
                    :kind normalized-kind
                    :function normalized-function
                    :package owner)))
    (call-with-buffer-type-registry-lock
     (lambda ()
       (setf *buffer-input-presentation-providers*
             (cons
              provider
              (remove-if
               (lambda (existing)
                 (and
                  (eq normalized-kind
                      (buffer-input-presentation-provider-kind existing))
                  (equal owner
                         (buffer-input-presentation-provider-package existing))
                  (eq normalized-function
                      (buffer-input-presentation-provider-function existing))))
               *buffer-input-presentation-providers*))))
     *buffer-type-registry*)
    provider))

(defun remove-buffer-input-presentation-providers-for-package (package-name)
  "Remove input presentation providers owned by PACKAGE-NAME."
  (let ((name (normalize-buffer-type-package-name package-name)))
    (call-with-buffer-type-registry-lock
     (lambda ()
       (setf *buffer-input-presentation-providers*
             (remove-if
              (lambda (provider)
                (equal name
                       (buffer-input-presentation-provider-package provider)))
              *buffer-input-presentation-providers*)))
     *buffer-type-registry*)))

(defun register-buffer-type
    (name
     &key
       (description nil description-supplied-p)
       (major-mode nil major-mode-supplied-p)
       (document-p nil document-p-supplied-p)
       (presentation-function nil presentation-function-supplied-p)
       (input-presentation-function nil input-presentation-function-supplied-p)
       (serialize-state-function nil serialize-state-function-supplied-p)
       (restore-state-function nil restore-state-function-supplied-p)
       (package nil package-supplied-p))
  "Register NAME as a buffer type and return its BUFFER-TYPE metadata.

  PRESENTATION-FUNCTION and INPUT-PRESENTATION-FUNCTION are retained as optional
interface metadata. Package entrypoints normally leave PACKAGE unset; it
defaults to the package currently being loaded by the RPLACA package manager."
  (when (and *current-rplaca-package*
             (not (package-resource-type-allowed-p :buffer-type)))
    (return-from register-buffer-type nil))
  (let* ((kind (normalize-buffer-kind name))
         (existing
           (call-with-buffer-type-registry-lock
            (lambda () (gethash kind *buffer-type-registry*))
            *buffer-type-registry*))
         (current-owner
           (normalize-buffer-type-package-name *current-rplaca-package*))
         (owner (normalize-buffer-type-package-name
                 (cond
                   (package-supplied-p package)
                   (current-owner)
                   (existing (buffer-type-package existing))
                   (t nil))))
         (type (make-buffer-type
                :name kind
                :description
                (or (and description-supplied-p description)
                    (and existing (buffer-type-description existing))
                    "")
                :major-mode
                (normalize-buffer-major-mode
                 (cond
                   (major-mode-supplied-p major-mode)
                   (existing (buffer-type-major-mode existing))
                   (t nil))
                 kind)
                :document-p
                (if document-p-supplied-p
                    (not (null document-p))
                    (and existing (buffer-type-document-p existing)))
                :presentation-function
                (normalize-buffer-type-function
                 (cond
                   (presentation-function-supplied-p presentation-function)
                   (existing (buffer-type-presentation-function existing))
                   (t nil))
                                                :presentation-function)
                :input-presentation-function
                (normalize-buffer-type-function
                 (cond
                   (input-presentation-function-supplied-p
                    input-presentation-function)
                   (existing (buffer-type-input-presentation-function existing))
                   (t nil))
                                                :input-presentation-function)
                :serialize-state-function
                (normalize-buffer-type-function
                 (cond
                   (serialize-state-function-supplied-p
                    serialize-state-function)
                   (existing (buffer-type-serialize-state-function existing))
                   (t nil))
                 :serialize-state-function)
                :restore-state-function
                (normalize-buffer-type-function
                 (cond
                   (restore-state-function-supplied-p restore-state-function)
                   (existing (buffer-type-restore-state-function existing))
                   (t nil))
                 :restore-state-function)
                :package owner)))
    (call-with-buffer-type-registry-lock
     (lambda ()
       (setf (gethash kind *buffer-type-registry*) type))
     *buffer-type-registry*)
    type))

(defmacro define-buffer-type (name &rest options)
  "Define a RPLACA buffer type.

This is the package-facing form for registering a buffer kind, its default
major-mode label, and optional McCLIM presentation functions."
  `(register-buffer-type ',name ,@options))

(defun find-buffer-type (name)
  "Return the registered BUFFER-TYPE for NAME, or NIL."
  (let ((kind (normalize-buffer-kind name)))
    (call-with-buffer-type-registry-lock
     (lambda () (gethash kind *buffer-type-registry*))
     *buffer-type-registry*)))

(defun list-buffer-types ()
  "Return registered buffer types sorted by kind name."
  (sort (mapcar #'cdr (buffer-type-registry-snapshot))
        #'string<
        :key (lambda (type)
               (symbol-name (buffer-type-name type)))))

(defun buffer-type-for-buffer (buf)
  "Return the registered BUFFER-TYPE for BUF, or NIL."
  (find-buffer-type (buffer-kind buf)))

(defun buffer-presentation-function (buf)
  "Return BUF's registered presentation function, if any."
  (let ((type (buffer-type-for-buffer buf)))
    (and type (buffer-type-presentation-function type))))

(defun buffer-input-presentation-function (buf)
  "Return BUF's registered input presentation function, if any."
  (let ((type (buffer-type-for-buffer buf)))
    (and type (buffer-type-input-presentation-function type))))

(defun buffer-input-presentation-provider-active-p (provider buf)
  "Return true when PROVIDER should render for BUF."
  (let ((package (buffer-input-presentation-provider-package provider)))
    (and (eq (buffer-kind buf)
             (buffer-input-presentation-provider-kind provider))
         (or (null package)
             (package-active-p package :buffer buf)))))

(defun buffer-input-presentation-functions (buf)
  "Return input presentation functions active for BUF."
  (remove nil
          (append
           (list (buffer-input-presentation-function buf))
           (mapcar #'buffer-input-presentation-provider-function
                   (remove-if-not
                    (lambda (provider)
                      (buffer-input-presentation-provider-active-p provider buf))
                     (reverse
                      (buffer-input-presentation-provider-snapshot)))))))

(defvar *buffer-input-presentation-text* nil)

(defun buffer-input-presentation-text (buffer)
  "Return the text visible to input presentation providers for BUFFER."
  (or *buffer-input-presentation-text*
      (and buffer (message-text (buffer-input-message buffer)))
      ""))

(defun buffer-state-serializer (buf)
  "Return BUF's optional persistence serializer function."
  (let ((type (buffer-type-for-buffer buf)))
    (and type (buffer-type-state-serializer type))))

(defun buffer-state-restorer (buf)
  "Return BUF's optional persistence restore function."
  (let ((type (buffer-type-for-buffer buf)))
    (and type (buffer-type-state-restorer type))))

(defun serialize-buffer-extra-state (buf)
  "Return BUF's optional buffer-type-specific persistence state."
  (let ((serializer (buffer-state-serializer buf)))
    (when serializer
      (funcall serializer buf))))

(defun restore-buffer-extra-state (buf state)
  "Restore buffer-type-specific STATE into BUF when supported."
  (let ((restorer (buffer-state-restorer buf)))
    (when (and restorer state)
      (funcall restorer buf state)))
  buf)

(defun normalize-buffer-working-directory (value)
  "Return VALUE as a directory pathname suitable for BUFFER-WORKING-DIRECTORY."
  (let ((path (cond
                ((pathnamep value) value)
                ((stringp value) (pathname value))
                ((null value) (truename "."))
                (t (error "Invalid buffer working directory: ~S" value)))))
    (uiop:ensure-directory-pathname path)))

(defclass buffer ()
  ((name              :initarg :name
                      :accessor buffer-name
                      :type string
                      :documentation "Display name for this buffer (e.g. \"session-01\").")
   (first-message     :initarg :first-message
                      :accessor buffer-first-message
                      :type message
                      :documentation "First message in the buffer's doubly-linked list.")
   (last-message      :initarg :last-message
                      :accessor buffer-last-message
                      :type message
                      :documentation "Last message in the buffer (always the editable input message).")
   (agent-name        :initarg :agent-name
                      :accessor buffer-agent-name
                      :initform "agent"
                      :type string
                      :documentation "Name of the AI agent for this buffer (e.g. \"agent\").")
   (kind              :initarg :kind
                      :accessor buffer-kind
                      :initform :chat
                      :type keyword
                      :documentation "Buffer kind. Built-ins include :chat, :help, :info, :scratch, and :file.")
   (working-directory :initarg :working-directory
                      :accessor buffer-working-directory
                      :initform (truename ".")
                      :type pathname
                      :documentation "Working directory for shell commands and file operations.")
   (project-name      :initarg :project-name
                      :accessor buffer-project-name
                      :initform nil
                      :type (or null string)
                      :documentation "Project name associated with this buffer, when any.")
   (resource-path     :initarg :resource-path
                      :accessor buffer-resource-path
                      :initform nil
                      :type (or null string)
                      :documentation "Project-relative resource path associated with this buffer, when any.")
   (original-text     :initarg :original-text
                      :accessor buffer-original-text
                      :initform ""
                      :type string
                      :documentation "Last saved text for project-backed editable buffers.")
   (dirty-p           :initarg :dirty-p
                      :accessor buffer-dirty-p
                      :initform nil
                      :type boolean
                      :documentation "True when a project-backed editable buffer has unsaved changes.")
   (token-count       :initarg :token-count
                      :accessor buffer-token-count
                      :initform 0
                      :type integer
                      :documentation "Running count of tokens used in this buffer's conversation.")
   (context-limit     :initarg :context-limit
                      :accessor buffer-context-limit
                      :initform 200000
                      :type integer
                      :documentation "Maximum token context window size for the model.")
   (status            :initarg :status
                       :accessor buffer-status
                       :initform :idle
                       :type keyword
                       :documentation "Current buffer state: :idle, :thinking, :streaming, :error, :question, or :oauth.")
   (provider-override :initarg :provider-override
                      :accessor buffer-provider-override
                      :initform nil
                      :type (or null keyword)
                      :documentation "When non-nil, overrides the agent's default provider (e.g. :zai).")
   (model-override    :initarg :model-override
                      :accessor buffer-model-override
                      :initform nil
                      :type (or null string)
                      :documentation "When non-nil, overrides the agent's default model name.")
   (think-level-override :initarg :think-level-override
                         :accessor buffer-think-level-override
                         :initform nil
                         :type (or null string)
                         :documentation "When non-nil, overrides the model's default reasoning effort.")
   (model-role-override :initarg :model-role-override
                        :accessor buffer-model-role-override
                        :initform nil
                        :type (or null string)
                        :documentation "When non-nil, applies a named model-routing role for this buffer/session.")
   (model-role-set-override :initarg :model-role-set-override
                            :accessor buffer-model-role-set-override
                            :initform nil
                            :type list
                            :documentation "Ordered named model-routing roles used when cycling models within this buffer/session.")
   (next-turn-model-role-override :initarg :next-turn-model-role-override
                                  :accessor buffer-next-turn-model-role-override
                                  :initform nil
                                  :type (or null string)
                                  :documentation "One-turn named model-routing role override cleared after the next send.")
   (service-tier-override :initarg :service-tier-override
                          :accessor buffer-service-tier-override
                          :initform nil
                          :type (or null string)
                          :documentation "When non-nil, prefers a provider service tier such as default, flex, or priority.")
   (pipeline-name :initarg :pipeline-name
                  :accessor buffer-pipeline-name
                  :initform *default-pipeline-name*
                  :type (or null string)
                  :documentation "Optional deterministic pipeline run instead of a normal single agent response.")
   (enabled-packages :initarg :enabled-packages
                     :accessor buffer-enabled-packages
                     :initform nil
                     :type list
                     :documentation "Package names explicitly enabled for this buffer.")
   (session           :initarg :session
                      :accessor buffer-session
                      :initform nil
                      :type (or null session)
                      :documentation "Persistent session metadata for chat buffers, when attached.")
   (session-persistence-mode :initarg :session-persistence-mode
                             :accessor buffer-session-persistence-mode
                             :initform :persistent
                             :type keyword
                             :documentation "Controls whether the buffer should attach and autosave a durable session. Supported values are :persistent and :ephemeral.")
   (keymap            :initarg :keymap
                      :accessor buffer-keymap
                      :initform nil
                      :documentation "Keymap for this buffer. Falls back to *default-keymap*.")
   (scroll-offset    :initarg :scroll-offset
                      :accessor buffer-scroll-offset
                      :initform 0
                      :type integer
                      :documentation "Number of visual rows scrolled up from the bottom.
0 means auto-scroll (latest messages visible).")
   (show-tool-results-p :initarg :show-tool-results-p
                         :accessor buffer-show-tool-results-p
                         :initform *default-show-tool-results*
                         :type boolean
                         :documentation "When nil, tool-result messages are hidden from display.")
   (collapse-tool-activity-p :initarg :collapse-tool-activity-p
                             :accessor buffer-collapse-tool-activity-p
                             :initform *default-collapse-tool-activity*
                             :type boolean
                             :documentation "When non-nil, consecutive tool call/result messages are summarized in display.")
   (show-reasoning-p    :initarg :show-reasoning-p
                         :accessor buffer-show-reasoning-p
                         :initform *default-show-reasoning-output*
                         :type boolean
                         :documentation "When nil, provider-supplied reasoning blocks are hidden from display.")
   (show-metadata-p     :initarg :show-metadata-p
                         :accessor buffer-show-metadata-p
                         :initform *default-show-metadata-output*
                         :type boolean
                         :documentation "When nil, provider/response metadata is hidden from display.")
   (pending-stream      :initarg :pending-stream
                         :accessor buffer-pending-stream
                         :initform nil
                         :documentation "When non-nil, holds a stream-state for an in-progress streaming response.")
   (pending-tool-execution :initarg :pending-tool-execution
                           :accessor buffer-pending-tool-execution
                           :initform nil
                           :documentation "Managed worker state for one interactive tool call. Only the frame process applies its result to this buffer.")
   (pending-interactive-operation
    :initarg :pending-interactive-operation
    :accessor buffer-pending-interactive-operation
    :initform nil
    :documentation "Managed worker state for one shell, pipeline, or compaction action. Only the frame process applies its result to this buffer.")
   (runtime-lock :reader buffer-runtime-lock
                 :initform (bt:make-lock "rplaca buffer runtime")
                 :documentation "Lock serializing provider/tool ownership and teardown transitions for this buffer.")
   (runtime-condition :reader buffer-runtime-condition
                      :initform (bt:make-condition-variable
                                 :name "rplaca buffer runtime condition")
                      :documentation "Condition variable for application/teardown phase changes protected by RUNTIME-LOCK.")
   (runtime-application :accessor buffer-runtime-application
                        :initform nil
                        :documentation "Unique terminal-completion context while the frame process applies a stream or tool result.")
   (runtime-teardown :accessor buffer-runtime-teardown
                     :initform nil
                     :documentation "Single-flight teardown context retained until appliers and owned workers settle.")
   (runtime-generation :initarg :runtime-generation
                       :accessor buffer-runtime-generation
                       :initform 0
                       :type integer
                       :documentation "Generation token invalidating late provider and tool callbacks after teardown.")
   (runtime-start-generation :accessor buffer-runtime-start-generation
                             :initform nil
                             :type (or null integer)
                             :documentation "Generation reserved while a provider stream or OAuth flow is being prepared but has not yet published its state.")
   (runtime-start-owner :accessor buffer-runtime-start-owner
                        :initform nil
                        :documentation "Thread holding RUNTIME-START-GENERATION until publication or cancellation cleanup.")
   (runtime-tool-cancellation-p :accessor buffer-runtime-tool-cancellation-p
                                :initform nil
                                :type boolean
                                :documentation "True while Stop owns the pending tool queue and is recording protocol-completing cancellation results.")
   (runtime-stopping-p :accessor buffer-runtime-stopping-p
                       :initform nil
                       :type boolean
                       :documentation "True while owned runtime operations are being detached and cancelled.")
   (runtime-stopped-notification-p
    :accessor buffer-runtime-stopped-notification-p
    :initform nil
    :type boolean
    :documentation "True when teardown completion awaits delivery on the owning CLIM frame process.")
   (disposing-p        :accessor buffer-disposing-p
                       :initform nil
                       :type boolean
                       :documentation "True while permanent buffer disposal is in progress.")
   (disposed-p         :initarg :disposed-p
                       :accessor buffer-disposed-p
                       :initform nil
                       :type boolean
                       :documentation "True after the buffer has been removed permanently from the buffer ring.")
    (streaming-message   :initarg :streaming-message
                         :accessor buffer-streaming-message
                         :initform nil
                         :type (or null message)
                         :documentation "The message being updated by streaming. Updated in-place as tokens arrive.")
   (stashed-input       :initarg :stashed-input
                         :accessor buffer-stashed-input
                         :initform nil
                         :type (or null string)
                         :documentation "User input temporarily stashed while a tool-call sequence runs.")
   (pending-tool-calls  :initarg :pending-tool-calls
                         :accessor buffer-pending-tool-calls
                         :initform nil
                         :documentation "List of tool_use blocks awaiting sequential execution.")
   (tool-call-results   :initarg :tool-call-results
                         :accessor buffer-tool-call-results
                         :initform nil
                         :documentation "Accumulated results from tool calls in the current sequence.")
   (user-input-pending  :initarg :user-input-pending
                        :accessor buffer-user-input-pending
                        :initform nil
                        :documentation "When non-nil, an alist describing a pending structured user-input request.")
   (queued-steering-messages :initarg :queued-steering-messages
                             :accessor buffer-queued-steering-messages
                             :initform nil
                             :type list
                             :documentation "Queued steering messages to inject before the next LLM turn once the current run reaches a safe boundary.")
   (queued-follow-up-messages :initarg :queued-follow-up-messages
                              :accessor buffer-queued-follow-up-messages
                              :initform nil
                              :type list
                              :documentation "Queued follow-up messages to inject only after the agent would otherwise stop.")
   (major-mode          :initarg :major-mode
                         :accessor buffer-major-mode
                         :initform "chat"
                         :type string
                         :documentation "The major mode name for modeline display (e.g. \"chat\", \"help\")."))
  (:documentation
   "A chat buffer containing a doubly-linked list of messages.
The last message is always the input message (read-only-p = nil).
Invariant: last-message and input-message always refer to the same object."))

(defun buffer-input-message (buf)
  "Return the input message (alias for last-message).
Enforces the invariant that it is not read-only."
  (let ((msg (buffer-last-message buf)))
    (assert (not (message-read-only-p msg)) ()
            "Invariant violated: input message is read-only")
    msg))

(declaim (ftype (function (string &key (:agent-name string)
                                       (:kind (or keyword symbol string))
                                       (:working-directory pathname)
                                       (:project-name (or null string))
                                       (:resource-path (or null string))
                                       (:original-text string)
                                       (:dirty-p boolean)
                                       (:context-limit integer)
                                       (:pipeline-name (or null string))
                                      (:enabled-packages list)
                                      (:session (or null session))
                                      (:session-persistence-mode keyword)
                                      (:major-mode (or null string symbol)))
                          buffer)
                make-buffer))
(defun make-buffer (name &key (agent-name *default-agent-name*)
                              (kind :chat)
                              (working-directory (truename "."))
                              project-name
                              resource-path
                              (original-text "")
                              (dirty-p nil)
                              (context-limit *default-context-limit*)
                              (pipeline-name *default-pipeline-name*)
                              (enabled-packages nil)
                              (session nil)
                              (session-persistence-mode
                               *default-buffer-session-persistence-mode*)
                              major-mode)
  "Create a new buffer with a single empty input message."
  (let* ((normalized-kind (normalize-buffer-kind kind))
         (normalized-session-mode
           (normalize-buffer-session-persistence-mode
            session-persistence-mode))
         (type (find-buffer-type normalized-kind))
         (resolved-major-mode
           (or (and major-mode
                    (normalize-buffer-major-mode major-mode normalized-kind))
               (and type (buffer-type-major-mode type))
               (buffer-kind-default-major-mode normalized-kind)))
         (input-msg (make-message :user))
         (buf (make-instance 'buffer
                :name name
                :first-message input-msg
                :last-message input-msg
                :agent-name agent-name
                :kind normalized-kind
                :working-directory working-directory
                :project-name project-name
                :resource-path resource-path
                :original-text original-text
                :dirty-p dirty-p
                :context-limit context-limit
                :pipeline-name pipeline-name
                :enabled-packages (copy-list enabled-packages)
                :session session
                :session-persistence-mode normalized-session-mode
                :major-mode resolved-major-mode)))
    (maybe-run-hook-with-args '*after-buffer-create-hook* buf)
    buf))

(declaim (ftype (function (t) keyword) normalize-buffer-session-persistence-mode)
         (ftype (function (buffer) boolean) buffer-persistent-session-p
                buffer-ephemeral-p)
         (ftype (function (t) keyword) normalize-buffer-queued-message-kind)
         (ftype (function (t string &key (:timestamp t)) list)
                make-buffer-queued-message)
         (ftype (function (buffer) list) buffer-queued-messages
                clear-buffer-queued-messages
                restore-buffer-queued-messages-to-input)
         (ftype (function (buffer) integer) buffer-queued-message-count)
         (ftype (function (buffer) boolean) buffer-has-queued-messages-p)
         (ftype (function (buffer t string &key (:timestamp t)) list)
                queue-buffer-message)
         (ftype (function (buffer) (or null list))
                dequeue-buffer-steering-message
                dequeue-buffer-follow-up-message))

(defun normalize-buffer-session-persistence-mode (value)
  "Normalize VALUE to a supported buffer session persistence mode."
  (let ((mode (if (keywordp value)
                  value
                  (normalize-buffer-kind value))))
    (case mode
      (:persistent :persistent)
      (:ephemeral :ephemeral)
      (t (error "Unsupported session persistence mode: ~S" value)))))

(defun buffer-persistent-session-p (buf)
  "Return true when BUF should attach and autosave a durable session."
  (eq (buffer-session-persistence-mode buf) :persistent))

(defun buffer-ephemeral-p (buf)
  "Return true when BUF should not attach or autosave a durable session."
  (eq (buffer-session-persistence-mode buf) :ephemeral))

(declaim (ftype (function (buffer) boolean) scratch-buffer-p))
(defun scratch-buffer-p (buf)
  "Return true when BUF is the process-local scratch buffer."
  (eq (buffer-kind buf) :scratch))

(declaim (ftype (function (buffer) boolean) file-buffer-p))
(defun file-buffer-p (buf)
  "Return true when BUF is a project-backed editable file buffer."
  (eq (buffer-kind buf) :file))

(declaim (ftype (function (buffer) boolean) help-buffer-p))
(defun help-buffer-p (buf)
  "Return true when BUF is a read-only help buffer."
  (eq (buffer-kind buf) :help))

(declaim (ftype (function (buffer) boolean) info-buffer-p))

(declaim (ftype (function (buffer) boolean) document-buffer-p))
(defun document-buffer-p (buf)
  "Return true when BUF is an editable document buffer rather than a chat buffer."
  (or (let ((type (buffer-type-for-buffer buf)))
        (and type (buffer-type-document-p type)))
      (scratch-buffer-p buf)
      (file-buffer-p buf)))

(declaim (ftype (function (buffer) boolean) buffer-llm-running-p))
(defun buffer-llm-running-p (buf)
  "Return true while BUF owns or is settling an agent runtime turn."
  (not (null (and buf
                  (bt:with-lock-held ((buffer-runtime-lock buf))
                    (or (buffer-pending-stream buf)
                        (buffer-pending-tool-execution buf)
                        (buffer-pending-interactive-operation buf)
                        (buffer-runtime-application buf)
                        (buffer-runtime-teardown buf)
                        (buffer-runtime-start-generation buf)
                        (buffer-runtime-start-owner buf)
                        (buffer-runtime-stopping-p buf)
                        (buffer-runtime-stopped-notification-p buf)
                        (buffer-disposing-p buf)))))))

(declaim (ftype (function (buffer) boolean) buffer-interaction-pending-p))
(defun buffer-interaction-pending-p (buf)
  "Return true when BUF is waiting on user interaction before continuing."
  (not (null (and buf
                  (buffer-user-input-pending buf)))))

(declaim (ftype (function (buffer) boolean) buffer-agent-busy-p))
(defun buffer-agent-busy-p (buf)
  "Return true when BUF cannot immediately accept a new direct user turn."
  (not (null (and buf
                  (or (buffer-llm-running-p buf)
                      (buffer-interaction-pending-p buf)
                      (buffer-pending-tool-calls buf))))))

(defun normalize-buffer-queued-message-kind (kind)
  "Normalize KIND to one of the supported queued message kinds."
  (case kind
    (:steering :steering)
    (:follow-up :follow-up)
    (t (error "Unsupported queued message kind: ~S" kind))))

(defun make-buffer-queued-message (kind text &key timestamp)
  "Return a queued message plist for KIND and TEXT."
  (list :kind (normalize-buffer-queued-message-kind kind)
        :text text
        :timestamp (or timestamp (get-universal-time))))

(defun buffer-queued-messages (buf)
  "Return BUF's queued steering and follow-up messages as one list."
  (append (copy-list (buffer-queued-steering-messages buf))
          (copy-list (buffer-queued-follow-up-messages buf))))

(defun buffer-queued-message-count (buf)
  "Return the total number of queued steering and follow-up messages in BUF."
  (+ (length (buffer-queued-steering-messages buf))
     (length (buffer-queued-follow-up-messages buf))))

(defun buffer-has-queued-messages-p (buf)
  "Return true when BUF has any queued steering or follow-up messages."
  (plusp (buffer-queued-message-count buf)))

(defun queue-buffer-message (buf kind text &key timestamp)
  "Queue TEXT on BUF as KIND and return the queued entry."
  (let* ((normalized-kind (normalize-buffer-queued-message-kind kind))
         (entry (make-buffer-queued-message normalized-kind text
                                           :timestamp timestamp)))
    (ecase normalized-kind
      (:steering
       (setf (buffer-queued-steering-messages buf)
             (append (buffer-queued-steering-messages buf) (list entry))))
      (:follow-up
       (setf (buffer-queued-follow-up-messages buf)
             (append (buffer-queued-follow-up-messages buf) (list entry)))))
    (notify-buffer-display-change buf :queued-message)
    entry))

(defun dequeue-buffer-steering-message (buf)
  "Pop and return BUF's next queued steering message, or NIL."
  (let ((queue (buffer-queued-steering-messages buf)))
    (when queue
      (let ((entry (first queue)))
        (setf (buffer-queued-steering-messages buf) (rest queue))
        (notify-buffer-display-change buf :queued-message)
        entry))))

(defun dequeue-buffer-follow-up-message (buf)
  "Pop and return BUF's next queued follow-up message, or NIL."
  (let ((queue (buffer-queued-follow-up-messages buf)))
    (when queue
      (let ((entry (first queue)))
        (setf (buffer-queued-follow-up-messages buf) (rest queue))
        (notify-buffer-display-change buf :queued-message)
        entry))))

(defun clear-buffer-queued-messages (buf)
  "Remove and return all queued steering and follow-up messages from BUF."
  (let ((messages (buffer-queued-messages buf)))
    (setf (buffer-queued-steering-messages buf) nil
          (buffer-queued-follow-up-messages buf) nil)
    (notify-buffer-display-change buf :queued-message)
    messages))

(defun restore-buffer-queued-messages-to-input (buf)
  "Restore BUF's queued messages into the current input editor and clear them.
Returns the restored messages."
  (let ((messages (clear-buffer-queued-messages buf)))
    (when messages
      (set-message-text
       (buffer-input-message buf)
       (format nil "~{~A~^~%~%~}"
               (mapcar (lambda (entry)
                         (or (getf entry :text) ""))
                       messages)))
      (mark-buffer-dirty buf))
    messages))

(defvar *suppress-session-transcript-recording* nil
  "When non-nil, buffer message helpers do not append transcript events.")

(defvar *suppress-session-autosave* nil
  "When non-nil, buffer message helpers do not refresh session snapshots.")

(defun copy-runtime-owned-data (value)
  "Recursively copy mutable provider/tool data into a new runtime owner.

Conses, strings, vectors, and hash tables are copied.  Symbols, numbers,
pathnames, functions, and other conventionally immutable leaf objects may be
shared.  Provider payloads are acyclic data, so cycle preservation is not
required here."
  (typecase value
    (string (copy-seq value))
    (cons
     (cons (copy-runtime-owned-data (car value))
           (copy-runtime-owned-data (cdr value))))
    (vector
     (let ((copy (make-array (length value))))
       (loop :for index :below (length value)
             :do (setf (aref copy index)
                       (copy-runtime-owned-data (aref value index))))
       copy))
    (hash-table
     (let ((copy (make-hash-table
                  :test (hash-table-test value)
                  :size (hash-table-size value)
                  :rehash-size (hash-table-rehash-size value)
                  :rehash-threshold (hash-table-rehash-threshold value))))
       (maphash (lambda (key item)
                  (setf (gethash (copy-runtime-owned-data key) copy)
                        (copy-runtime-owned-data item)))
                value)
       copy))
    (t value)))

(defstruct (buffer-message-insertion-effect
            (:constructor make-buffer-message-insertion-effect
                (sender text raw-content metadata timestamp record-p run-hook-p)))
  "Immutable description of one worker-requested buffer message insertion."
  (sender :system :type keyword :read-only t)
  (text "" :type string :read-only t)
  (raw-content nil :type list :read-only t)
  (metadata nil :type list :read-only t)
  (timestamp nil :type (or null integer) :read-only t)
  (record-p t :type boolean :read-only t)
  (run-hook-p t :type boolean :read-only t))

(defvar *buffer-message-effect-recorder* nil
  "Dynamically bound worker callback for deferred buffer message effects.
When bound, BUFFER-INSERT-READ-ONLY-MESSAGE mutates only its detached buffer and
records an immutable insertion effect.  Transcript writes, hooks, autosave, and
display notification are deferred until the frame process applies the effect.")

(defun attach-buffer-session (buf session)
  "Attach SESSION to BUF and return BUF."
  (setf (buffer-session buf) session)
  buf)

(defun ensure-buffer-session (buf)
  "Ensure BUF has persistent session metadata when it is a chat buffer."
  (when (and buf
             (not (document-buffer-p buf))
             (buffer-persistent-session-p buf)
             (null (buffer-session buf)))
    (attach-buffer-session buf
                           (load-or-create-session
                            (buffer-name buf)
                            :working-directory (buffer-working-directory buf))))
  (buffer-session buf))

(defun autosave-session-snapshot (buf)
  "Refresh BUF's session snapshot when automatic persistence is active."
  (when (and buf
             (not *suppress-session-autosave*)
             (not (document-buffer-p buf))
             (buffer-persistent-session-p buf)
             (buffer-session buf))
    (handler-case
        (save-session buf)
      (error (e)
        (format *error-output*
                "~&;; Warning: failed to autosave session ~A: ~A~%"
                (buffer-name buf)
                e))))
  buf)

(defun record-buffer-message (buf msg)
  "Record MSG in BUF's transcript when session recording is active."
  (when (and buf
             msg
             (not *suppress-session-transcript-recording*)
             (buffer-persistent-session-p buf)
             (buffer-session buf))
    (record-session-message (buffer-session buf) msg)
    (autosave-session-snapshot buf))
  msg)

(defun buffer-clear-history-before-input (buf)
  "Remove all finalized history messages from BUF, preserving the input."
  (let ((input (buffer-input-message buf)))
    (setf (message-prev input) nil
          (buffer-first-message buf) input))
  buf)

(declaim (ftype (function (buffer) fixnum) buffer-message-count))
(defun buffer-message-count (buf)
  "Count the number of messages in BUF."
  (loop :for current := (buffer-first-message buf) :then (message-next current)
        :while current
        :unless (buffer-ephemeral-display-message-p current)
        :count t))

(declaim (ftype (function (buffer keyword) buffer) set-buffer-provider-override))
(defun set-buffer-provider-override (buf provider)
  "Set BUF's provider override to PROVIDER and return BUF."
  (setf (buffer-provider-override buf) provider)
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer string) buffer) set-buffer-model-override))
(defun set-buffer-model-override (buf model)
  "Set BUF's model override to MODEL and return BUF."
  (setf (buffer-model-override buf) model)
  (notify-buffer-display-change buf :routing)
  buf)

(defun normalize-think-level-override (value)
  "Normalize VALUE for storage as a think-level override."
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (string value))))
      (when (plusp (length trimmed))
        (string-downcase trimmed)))))

(declaim (ftype (function (buffer string) buffer) set-buffer-think-level-override))
(defun set-buffer-think-level-override (buf think-level)
  "Set BUF's think-level override to THINK-LEVEL and return BUF."
  (setf (buffer-think-level-override buf)
        (normalize-think-level-override think-level))
  (notify-buffer-display-change buf :routing)
  buf)

(defun normalize-model-role-override (value)
  "Normalize VALUE for storage as a model-role override."
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (string-downcase (string value)))))
      (when (plusp (length trimmed))
        trimmed))))

(declaim (ftype (function (buffer (or null string symbol)) buffer)
                set-buffer-model-role-override))
(defun set-buffer-model-role-override (buf role)
  "Set BUF's model-role override to ROLE and return BUF."
  (setf (buffer-model-role-override buf)
        (normalize-model-role-override role))
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer list) buffer) set-buffer-model-role-set-override))
(defun set-buffer-model-role-set-override (buf roles)
  "Set BUF's ordered model-role set override to ROLES and return BUF."
  (setf (buffer-model-role-set-override buf)
        (remove nil
                (mapcar #'normalize-model-role-override roles)
                :test #'equal))
  (notify-buffer-display-change buf :routing)
  buf)

(defun normalize-service-tier-override (value)
  "Normalize VALUE for storage as a service-tier override."
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (string-downcase (string value)))))
      (when (plusp (length trimmed))
        trimmed))))

(declaim (ftype (function (buffer (or null string symbol)) buffer)
                set-buffer-service-tier-override))
(defun set-buffer-service-tier-override (buf service-tier)
  "Set BUF's service-tier override to SERVICE-TIER and return BUF."
  (setf (buffer-service-tier-override buf)
        (normalize-service-tier-override service-tier))
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-provider-override))
(defun clear-buffer-provider-override (buf)
  "Clear BUF's provider override and return BUF."
  (setf (buffer-provider-override buf) nil)
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-model-override))
(defun clear-buffer-model-override (buf)
  "Clear BUF's model override and return BUF."
  (setf (buffer-model-override buf) nil)
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-think-level-override))
(defun clear-buffer-think-level-override (buf)
  "Clear BUF's think-level override and return BUF."
  (setf (buffer-think-level-override buf) nil)
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-model-role-override))
(defun clear-buffer-model-role-override (buf)
  "Clear BUF's model-role override and return BUF."
  (setf (buffer-model-role-override buf) nil)
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-model-role-set-override))
(defun clear-buffer-model-role-set-override (buf)
  "Clear BUF's model-role set override and return BUF."
  (setf (buffer-model-role-set-override buf) nil)
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-service-tier-override))
(defun clear-buffer-service-tier-override (buf)
  "Clear BUF's service-tier override and return BUF."
  (setf (buffer-service-tier-override buf) nil)
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-routing-overrides))
(defun clear-buffer-routing-overrides (buf)
  "Clear BUF's provider, model, think, role, and service-tier overrides."
  (clear-buffer-provider-override buf)
  (clear-buffer-model-override buf)
  (clear-buffer-think-level-override buf)
  (clear-buffer-model-role-override buf)
  (clear-buffer-service-tier-override buf)
  buf)

(declaim (ftype (function (buffer (or null string symbol)) buffer)
                set-buffer-pipeline))
(defun set-buffer-pipeline (buf pipeline-name)
  "Set BUF's deterministic pipeline name, or clear it when PIPELINE-NAME is NIL."
  (let ((trimmed (and pipeline-name
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (string pipeline-name)))))
    (setf (buffer-pipeline-name buf)
          (and trimmed
               (plusp (length trimmed))
               (string-downcase trimmed))))
  (notify-buffer-display-change buf :routing)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-pipeline))
(defun clear-buffer-pipeline (buf)
  "Clear BUF's deterministic pipeline."
  (setf (buffer-pipeline-name buf) nil)
  (notify-buffer-display-change buf :routing)
  buf)

;;; --------------------------------------------------------------------------
;;; Buffer Ring
;;; --------------------------------------------------------------------------

(defvar *buffer-ring* nil
  "List of all buffers. The first element is the current buffer.")

(defvar *buffer-counter* 0
  "Counter for generating unique buffer names.")

(defun current-buffer ()
  "Return the current buffer (first in the ring)."
  (first *buffer-ring*))

(defun add-buffer-to-ring (buf)
  "Add BUF to the front of the buffer ring."
  (push buf *buffer-ring*)
  buf)

(defun switch-to-buffer (buf)
  "Make BUF the current buffer by moving it to the front of the ring."
  (setf *buffer-ring* (cons buf (remove buf *buffer-ring*)))
  buf)

(defgeneric cancel-buffer-runtime-operations (buf)
  (:documentation
   "Cancel provider/tool/OAuth operations owned by BUF and invalidate callbacks."))

(defun dispose-buffer (buf)
  "Permanently dispose BUF and cancel all runtime resources it owns."
  (let ((already-disposing-p nil))
    (bt:with-lock-held ((buffer-runtime-lock buf))
      (when (buffer-disposed-p buf)
        (return-from dispose-buffer buf))
      (if (buffer-disposing-p buf)
          (setf already-disposing-p t)
          (setf (buffer-disposing-p buf) t)))
    ;; Disposal is single-flight.  A concurrent or reentrant caller observes
    ;; the existing transition; its owner (or terminal applier) completes it.
    (when already-disposing-p
      (return-from dispose-buffer buf)))
  (let ((returned-p nil)
        (settled-p nil))
    (unwind-protect
         (multiple-value-bind (ignored settled)
             (cancel-buffer-runtime-operations buf)
           (declare (ignore ignored))
           (setf returned-p t
                 settled-p settled))
      (bt:with-lock-held ((buffer-runtime-lock buf))
        (cond
          (settled-p
           (setf (buffer-disposing-p buf) nil
                 (buffer-disposed-p buf) t)
           (bt:condition-notify (buffer-runtime-condition buf)))
          ((not returned-p)
           ;; A failed cancellation did not transfer finalization ownership.
           ;; Preserve the buffer for a later retry.
           (setf (buffer-disposing-p buf) nil)
           (bt:condition-notify (buffer-runtime-condition buf)))))))
  buf)

(defun kill-buffer-from-ring (buf)
  "Remove BUF from the buffer ring. Returns the new current buffer or nil."
  (unless (and buf (scratch-buffer-p buf))
    (when buf
      (dispose-buffer buf))
    (setf *buffer-ring* (remove buf *buffer-ring*)))
  (first *buffer-ring*))

(defun find-buffer-by-name (name)
  "Find a buffer in the ring by name. Returns the buffer or nil."
  (find name *buffer-ring* :key #'buffer-name :test #'string=))

(defun scratch-buffer ()
  "Return the loaded scratch buffer, or nil when it has not been created yet."
  (find-if #'scratch-buffer-p *buffer-ring*))

(defun next-buffer-name ()
  "Generate the next unique buffer name."
  (incf *buffer-counter*)
  (format nil "session-~A" *buffer-counter*))

(defun buffer-names ()
  "Return a list of all buffer names in the ring."
  (mapcar #'buffer-name *buffer-ring*))

(defun scratch-buffer-text (&optional (buf (scratch-buffer)))
  "Return BUF's editable scratch text, or nil when no scratch buffer is loaded."
  (when buf
    (unless (scratch-buffer-p buf)
      (error "Not a scratch buffer: ~A" (buffer-name buf)))
    (message-text (buffer-input-message buf))))

(defun (setf scratch-buffer-text) (text &optional (buf (scratch-buffer)))
  "Replace BUF's editable scratch text with TEXT."
  (unless buf
    (error "No scratch buffer is loaded"))
  (unless (scratch-buffer-p buf)
    (error "Not a scratch buffer: ~A" (buffer-name buf)))
  (set-message-text (buffer-input-message buf) text))

(defun file-buffer-text (&optional (buf (current-buffer)))
  "Return BUF's editable file text, or nil when BUF is nil."
  (when buf
    (unless (file-buffer-p buf)
      (error "Not a file buffer: ~A" (buffer-name buf)))
    (message-text (buffer-input-message buf))))

(defun (setf file-buffer-text) (text &optional (buf (current-buffer)))
  "Replace BUF's editable file text and update its dirty state."
  (unless buf
    (error "No file buffer is current"))
  (unless (file-buffer-p buf)
    (error "Not a file buffer: ~A" (buffer-name buf)))
  (set-message-text (buffer-input-message buf) text)
  (setf (buffer-dirty-p buf)
        (not (string= text (buffer-original-text buf))))
  text)

(declaim (ftype (function (&optional (or null buffer)) boolean)
                file-buffer-dirty-p))
(defun file-buffer-dirty-p (&optional (buf (current-buffer)))
  "Return true when BUF is a project-backed file buffer with unsaved changes."
  (when buf
    (unless (file-buffer-p buf)
      (error "Not a file buffer: ~A" (buffer-name buf)))
    (buffer-dirty-p buf)))

(defun mark-buffer-dirty (buf)
  "Mark BUF dirty when it is a project-backed file buffer."
  (when (file-buffer-p buf)
    (setf (buffer-dirty-p buf) t))
  (notify-buffer-display-change buf :dirty)
  buf)

;;; --------------------------------------------------------------------------
;;; Buffer Operations
;;; --------------------------------------------------------------------------

(declaim (ftype (function (buffer) buffer) buffer-finalize-input))
(defun buffer-finalize-input (buf)
  "Finalize the current input message: make it read-only, timestamp it,
and create a new empty input message at the tail."
  (let ((input (buffer-input-message buf)))
    (setf (message-read-only-p input) t
          (message-timestamp input) (get-universal-time)
          (message-sender input) :user)
    (maybe-run-hook-with-args '*after-message-insert-hook* buf input)
    (let ((*suppress-session-autosave* t))
      (record-buffer-message buf input))
    (let ((new-input (make-message :user)))
      (setf (message-prev new-input) input
            (message-next input) new-input
            (buffer-last-message buf) new-input))
    (autosave-session-snapshot buf))
  (notify-buffer-display-change buf :message)
  buf)

(defun whitespace-char-p (char)
  "Return T when CHAR is treated as command-word whitespace."
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Newline)
      (char= char #\Return)))

(defun split-command-words (text)
  "Split TEXT into whitespace-delimited words."
  (let ((words nil)
        (len (length text))
        (start 0))
    (loop
      (let ((word-start (position-if-not #'whitespace-char-p text :start start)))
        (unless word-start
          (return (nreverse words)))
        (let ((word-end (or (position-if #'whitespace-char-p text :start word-start)
                            len)))
          (push (subseq text word-start word-end) words)
          (setf start word-end))))))

(defun buffer-previous-user-command-text (buf)
  "Return the latest finalized user message text in BUF, or nil."
  (loop :for msg := (message-prev (buffer-input-message buf)) :then (message-prev msg)
        :while msg
        :for text := (message-text msg)
        :when (and (message-read-only-p msg)
                   (eq (message-sender msg) :user)
                   (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
          :return text
        :finally (return nil)))

(defun buffer-previous-command-first-argument (buf)
  "Return previous command's first argument (second word), or nil."
  (let* ((command (buffer-previous-user-command-text buf))
         (words (and command (split-command-words command))))
    (second words)))

(defun buffer-previous-command-last-argument (buf)
  "Return previous command's last argument, or nil when unavailable."
  (let* ((command (buffer-previous-user-command-text buf))
         (words (and command (split-command-words command))))
    (car (last words))))

(defun insert-message-before-input (buf msg)
  "Insert MSG before BUF's editable input message."
  (let* ((input (buffer-input-message buf))
         (before-input (message-prev input)))
    (setf (message-prev msg) before-input
          (message-next msg) input
          (message-prev input) msg)
    (if before-input
        (setf (message-next before-input) msg)
        (setf (buffer-first-message buf) msg)))
  msg)

(defun buffer-remove-message (buf msg)
  "Remove MSG from BUF's linked list and return BUF.

The input message is never removed."
  (when (and buf msg)
    (when (eq msg (buffer-input-message buf))
      (error "Cannot remove the input message from a buffer."))
    (let ((prev (message-prev msg))
          (next (message-next msg)))
      (when prev
        (setf (message-next prev) next))
      (when next
        (setf (message-prev next) prev))
      (when (eq msg (buffer-first-message buf))
        (setf (buffer-first-message buf) next))
      (when (eq msg (buffer-last-message buf))
        (setf (buffer-last-message buf) prev))
      (setf (message-prev msg) nil
            (message-next msg) nil)
      (notify-buffer-display-change buf :message)
      (autosave-session-snapshot buf)))
  buf)

(defun buffer-remove-messages-if (buf predicate)
  "Remove every finalized message in BUF for which PREDICATE returns true."
  (loop :for msg := (buffer-first-message buf)
        :then next
        :for next := (and msg (message-next msg))
        :while (and msg (not (eq msg (buffer-input-message buf))))
        :when (funcall predicate msg)
          :do (buffer-remove-message buf msg))
  buf)

(defun buffer-insert-read-only-message
    (buf sender text &key raw-content metadata timestamp
                      (record-p t) (run-hook-p t) (notify-p t))
  "Create a read-only SENDER message with TEXT before BUF's input message."
  (let* ((deferred-p (not (null *buffer-message-effect-recorder*)))
         (msg (make-message sender :read-only-p t)))
    (set-message-text msg text)
    (setf (message-timestamp msg) (or timestamp (get-universal-time)))
    (when raw-content
      (setf (message-raw-content msg) (copy-runtime-owned-data raw-content)))
    (when metadata
      (setf (message-metadata msg) (copy-runtime-owned-data metadata)))
    (insert-message-before-input buf msg)
    (if deferred-p
        (funcall
         *buffer-message-effect-recorder*
         (make-buffer-message-insertion-effect
          sender
          (copy-seq (or text ""))
          (copy-runtime-owned-data raw-content)
          (copy-runtime-owned-data metadata)
          (message-timestamp msg)
          record-p
          run-hook-p))
        (progn
          (when run-hook-p
            (maybe-run-hook-with-args '*after-message-insert-hook* buf msg))
          (when record-p
            (record-buffer-message buf msg))
          (when notify-p
            (notify-buffer-display-change buf :message))))
    msg))

(defun apply-buffer-message-insertion-effect (buf effect)
  "Apply deferred message insertion EFFECT to live BUF in the frame process."
  (check-type effect buffer-message-insertion-effect)
  (buffer-insert-read-only-message
   buf
   (buffer-message-insertion-effect-sender effect)
   (buffer-message-insertion-effect-text effect)
   :raw-content (copy-runtime-owned-data
                 (buffer-message-insertion-effect-raw-content effect))
   :metadata (copy-runtime-owned-data
              (buffer-message-insertion-effect-metadata effect))
   :timestamp (buffer-message-insertion-effect-timestamp effect)
   :record-p (buffer-message-insertion-effect-record-p effect)
   :run-hook-p (buffer-message-insertion-effect-run-hook-p effect)))

(defun copy-session-for-tool-execution (session)
  "Return detached SESSION metadata with its own synchronization lock."
  (when session
    (%make-session
     (copy-seq (session-name session))
     (copy-seq (session-id session))
     (session-directory session)
     (session-manifest-path session)
     (session-transcript-directory session)
     (session-current-transcript-index session)
     (session-current-transcript-path session)
     (session-created-at session)
     :updated-at (session-updated-at session)
     :current-leaf-id (and (session-current-leaf-id session)
                           (copy-seq (session-current-leaf-id session)))
     :parent-session (and (session-parent-session session)
                          (copy-seq (session-parent-session session)))
     :working-directory (session-working-directory session)
     :display-name (and (session-display-name session)
                        (copy-seq (session-display-name session))))))

(defun clone-message-into-tool-buffer (snapshot message)
  "Copy finalized MESSAGE into detached tool SNAPSHOT without side effects."
  (let ((copy (make-message (message-sender message) :read-only-p t)))
    (set-message-text copy (message-text message))
    (setf (message-timestamp copy) (message-timestamp message)
          (message-raw-content copy)
          (copy-runtime-owned-data (message-raw-content message))
          (message-metadata copy)
          (copy-runtime-owned-data (message-metadata message))
          (message-entry-id copy) (and (message-entry-id message)
                                       (copy-seq (message-entry-id message)))
          (message-parent-entry-id copy)
          (and (message-parent-entry-id message)
               (copy-seq (message-parent-entry-id message))))
    (insert-message-before-input snapshot copy)))

(defun make-tool-execution-buffer-snapshot (buf)
  "Return a detached snapshot suitable for one background tool invocation.

The snapshot shares no message, session lock, or mutable queue/runtime state
with BUF.  Background tools may inspect and mutate it freely; only explicitly
recorded immutable effects can later cross back to the live frame buffer."
  (let* ((input (make-message :user))
         (snapshot
           (make-instance
            'buffer
            :name (copy-seq (buffer-name buf))
            :first-message input
            :last-message input
            :agent-name (copy-seq (buffer-agent-name buf))
            :kind (buffer-kind buf)
            :working-directory (buffer-working-directory buf)
            :project-name (and (buffer-project-name buf)
                               (copy-seq (buffer-project-name buf)))
            :resource-path (and (buffer-resource-path buf)
                                (copy-seq (buffer-resource-path buf)))
            :original-text (copy-seq (buffer-original-text buf))
            :dirty-p (buffer-dirty-p buf)
            :context-limit (buffer-context-limit buf)
            :pipeline-name (and (buffer-pipeline-name buf)
                                (copy-seq (buffer-pipeline-name buf)))
            :enabled-packages (copy-list (buffer-enabled-packages buf))
            :session (copy-session-for-tool-execution (buffer-session buf))
            :session-persistence-mode :ephemeral
            :major-mode (copy-seq (buffer-major-mode buf)))))
    (loop :for message := (buffer-first-message buf)
            :then (message-next message)
          :while (and message (not (eq message (buffer-input-message buf))))
          :do (clone-message-into-tool-buffer snapshot message))
    (set-message-text (buffer-input-message snapshot)
                      (message-text (buffer-input-message buf)))
    (setf (buffer-token-count snapshot) (buffer-token-count buf)
          (buffer-status snapshot) (buffer-status buf)
          (buffer-provider-override snapshot) (buffer-provider-override buf)
          (buffer-model-override snapshot) (buffer-model-override buf)
          (buffer-think-level-override snapshot)
          (buffer-think-level-override buf)
          (buffer-model-role-override snapshot)
          (buffer-model-role-override buf)
          (buffer-model-role-set-override snapshot)
          (copy-list (buffer-model-role-set-override buf))
          (buffer-next-turn-model-role-override snapshot)
          (buffer-next-turn-model-role-override buf)
          (buffer-service-tier-override snapshot)
          (buffer-service-tier-override buf)
          (buffer-show-tool-results-p snapshot) (buffer-show-tool-results-p buf)
          (buffer-collapse-tool-activity-p snapshot)
          (buffer-collapse-tool-activity-p buf)
          (buffer-show-reasoning-p snapshot) (buffer-show-reasoning-p buf)
          (buffer-show-metadata-p snapshot) (buffer-show-metadata-p buf))
    snapshot))

(defun buffer-insert-agent-message
    (buf text &key (record-p t) raw-content metadata (run-hook-p t))
  "Create a read-only agent message with TEXT before BUF's input message."
  (let ((agent-keyword (intern (string-upcase (buffer-agent-name buf)) :keyword)))
    (buffer-insert-read-only-message
     buf agent-keyword text
     :raw-content raw-content
     :metadata metadata
     :record-p record-p
     :run-hook-p run-hook-p)))

(defun buffer-insert-context-message
    (buf text &key (record-p t) raw-content metadata (run-hook-p t))
  "Create a read-only context message with TEXT before BUF's input message.
Context messages are sent to providers as user-context messages."
  (buffer-insert-read-only-message
   buf :context text
   :raw-content raw-content
   :metadata metadata
   :record-p record-p
   :run-hook-p run-hook-p))

(defun buffer-insert-system-message
    (buf text &key (record-p t) raw-content metadata (run-hook-p t))
  "Create a read-only display-only system message before BUF's input message."
  (buffer-insert-read-only-message
   buf :system text
   :raw-content raw-content
   :metadata metadata
   :record-p record-p
   :run-hook-p run-hook-p))

(defun buffer-ephemeral-display-message-p (msg)
  "Return true when MSG is a synthetic display-only helper message."
  (let ((metadata (and msg (message-metadata msg))))
    (and metadata
         (message-metadata-value metadata :ephemeral-display)
         t)))

(defun buffer-system-prompt-display-message-p (msg)
  "Return true when MSG is the synthetic system-prompt header message."
  (let ((metadata (and msg (message-metadata msg))))
    (and (eq (message-sender msg) :system)
         metadata
         (message-metadata-value metadata :system-prompt-display)
         t)))

(defun find-buffer-system-prompt-display-message (buf)
  "Return BUF's synthetic system-prompt header message, or NIL."
  (loop :for msg := (buffer-first-message buf) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buf))))
        :thereis (and (buffer-system-prompt-display-message-p msg) msg)))

(defun buffer-system-prompt-display-text (buf)
  "Return the display text for BUF's synthetic system-prompt header, or NIL."
  (when (and *buffer-system-prompt-display-enabled*
             (eq (buffer-kind buf) :chat))
    (let ((builder (and (fboundp 'build-agent-system-prompt)
                        (symbol-function 'build-agent-system-prompt))))
      (when builder
        (handler-case
            (let ((prompt (funcall builder (buffer-agent-name buf) :buffer buf)))
              (unless (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                  (or prompt ""))))
                (format nil "[System Prompt]~%~%~A" prompt)))
          (error (condition)
            (when (fboundp 'file-debug-log)
              (ignore-errors
                (funcall (symbol-function 'file-debug-log)
                         "system-prompt-display"
                         "failed to build system prompt for ~A: ~A"
                         (buffer-name buf)
                         condition)))
            nil))))))

(defun unlink-message-from-buffer (buf msg)
  "Detach MSG from BUF without transcript or autosave side effects."
  (when (and buf msg)
    (let ((prev (message-prev msg))
          (next (message-next msg)))
      (when prev
        (setf (message-next prev) next))
      (when next
        (setf (message-prev next) prev))
      (when (eq msg (buffer-first-message buf))
        (setf (buffer-first-message buf) next))
      (when (eq msg (buffer-last-message buf))
        (setf (buffer-last-message buf) prev))
      (setf (message-prev msg) nil
            (message-next msg) nil)))
  msg)

(defun insert-message-at-buffer-start (buf msg)
  "Insert MSG as BUF's first finalized message."
  (let ((first (buffer-first-message buf)))
    (setf (message-prev msg) nil
          (message-next msg) first)
    (when first
      (setf (message-prev first) msg))
    (setf (buffer-first-message buf) msg)
    (when (null (buffer-last-message buf))
      (setf (buffer-last-message buf) msg)))
  msg)

(defun sync-buffer-system-prompt-display (buf)
  "Ensure BUF shows the current full system prompt at the top of chat buffers.

The header message is synthetic: it is visible in the buffer, but it is not
recorded into transcripts or saved snapshots."
  (when buf
    (let* ((text (buffer-system-prompt-display-text buf))
           (existing (find-buffer-system-prompt-display-message buf))
           (changed-p nil))
      (cond
        ((null text)
         (when existing
           (unlink-message-from-buffer buf existing)
           (setf changed-p t)))
        ((null existing)
         (let ((msg (make-message :system :read-only-p t)))
           (set-message-text msg text)
           (setf (message-timestamp msg) (get-universal-time)
                 (message-metadata msg)
                 '((:system-prompt-display . t)
                   (:ephemeral-display . t)))
           (insert-message-at-buffer-start buf msg)
           (setf changed-p t)))
        (t
         (unless (eq existing (buffer-first-message buf))
           (unlink-message-from-buffer buf existing)
           (insert-message-at-buffer-start buf existing)
           (setf changed-p t))
         (unless (string= text (message-text existing))
           (set-message-text existing text)
           (setf (message-timestamp existing) (get-universal-time))
           (setf changed-p t))
         (setf (message-metadata existing)
               '((:system-prompt-display . t)
                 (:ephemeral-display . t)))
         nil))
      (when changed-p
        (notify-buffer-display-change buf :system-prompt))
      buf)))

(defun ensure-default-keymap-initialized ()
  "Ensure the default keymap exists when keymap code is loaded."
  (when (and (boundp '*default-keymap*)
             (null (symbol-value '*default-keymap*))
             (fboundp 'init-default-keymap))
    (funcall (symbol-function 'init-default-keymap)))
  (and (boundp '*default-keymap*)
       (symbol-value '*default-keymap*)))

(defun ensure-scratch-keymap-initialized ()
  "Ensure the scratch keymap exists when keymap code is loaded."
  (ensure-default-keymap-initialized)
  (when (and (boundp '*scratch-keymap*)
             (null (symbol-value '*scratch-keymap*))
             (fboundp 'init-scratch-keymap))
    (funcall (symbol-function 'init-scratch-keymap)))
  (or (and (boundp '*scratch-keymap*)
           (symbol-value '*scratch-keymap*))
      (ensure-default-keymap-initialized)))

(defun ensure-file-keymap-initialized ()
  "Ensure the file buffer keymap exists when keymap code is loaded."
  (ensure-default-keymap-initialized)
  (when (and (boundp '*file-keymap*)
             (null (symbol-value '*file-keymap*))
             (fboundp 'init-file-keymap))
    (funcall (symbol-function 'init-file-keymap)))
  (or (and (boundp '*file-keymap*)
           (symbol-value '*file-keymap*))
      (ensure-default-keymap-initialized)))

(defun initialize-buffer-display-defaults (buf &key keymap)
  "Install KEYMAP on BUF when the keymap system is loaded."
  (setf (buffer-keymap buf)
        (or keymap
            (ensure-default-keymap-initialized)))
  buf)

(defun make-chat-buffer
    (name &key (agent-name *default-agent-name*)
               (working-directory (truename "."))
               (session nil)
               (session-persistence-mode
                *default-buffer-session-persistence-mode*)
               (add-to-ring-p nil))
  "Create a chat buffer with default faces, keymap, and optional ring entry."
  (let* ((normalized-session-mode
           (normalize-buffer-session-persistence-mode
            session-persistence-mode))
         (effective-session
           (or session
               (and (eq normalized-session-mode :persistent)
                    (load-or-create-session name))))
         (buf (make-buffer name
                           :agent-name agent-name
                           :working-directory working-directory
                           :session-persistence-mode normalized-session-mode
                           :session effective-session)))
    (initialize-buffer-display-defaults buf)
    (sync-buffer-system-prompt-display buf)
    (when add-to-ring-p
      (add-buffer-to-ring buf))
    buf))

(defun make-help-buffer (name content)
  "Create a help buffer with NAME containing CONTENT as read-only text."
  (let ((buf (make-buffer name :agent-name "help" :kind :help)))
    (initialize-buffer-display-defaults buf)
    (setf (buffer-major-mode buf) "help"
          (buffer-scroll-offset buf) most-positive-fixnum)
    (buffer-insert-agent-message buf content)
    (add-buffer-to-ring buf)
    buf))

(defun help-buffer-text (buf)
  "Return the read-only help text stored in BUF."
  (unless (help-buffer-p buf)
    (error "Not a help buffer: ~S" buf))
  (let ((msg (message-prev (buffer-input-message buf))))
    (if msg
        (message-text msg)
        "")))

(defun ensure-scratch-buffer ()
  "Ensure the process-local scratch buffer is loaded in the buffer ring.
The current buffer remains current when a current buffer already exists."
  (or (scratch-buffer)
      (let* ((current (current-buffer))
             (buf (make-buffer *scratch-buffer-name*
                               :agent-name "scratch"
                               :kind :scratch
                               :working-directory (truename "."))))
        (initialize-buffer-display-defaults
         buf
         :keymap (ensure-scratch-keymap-initialized))
        (setf (buffer-major-mode buf) "scratch")
        (setf (scratch-buffer-text buf) *scratch-buffer-initial-text*)
        (add-buffer-to-ring buf)
        (when current
          (switch-to-buffer current))
        buf)))

;;; --------------------------------------------------------------------------
;;; Session Persistence
;;; --------------------------------------------------------------------------

(defun session-path (session-name &key (root *sessions-dir*))
  "Return the file path for a session by name."
  (merge-pathnames (format nil "~A.json" session-name) root))

(defun session-sidecar-manifest-path (session-name &key (root *sessions-dir*))
  "Return SESSION-NAME's sidecar manifest path."
  (merge-pathnames "session.json"
                   (session-sidecar-directory session-name :root root)))

(defun serialize-message (msg)
  "Serialize a message to an alist for JSON encoding."
  `((:entry-id . ,(message-entry-id msg))
    (:parent-entry-id . ,(message-parent-entry-id msg))
    (:sender . ,(symbol-name (message-sender msg)))
    (:text . ,(message-text msg))
    (:timestamp . ,(message-timestamp msg))
    (:read-only-p . ,(message-read-only-p msg))
    ,@(when (message-raw-content msg)
        `((:raw-content . ,(coerce (message-raw-content msg) 'vector))))
    ,@(when (message-metadata msg)
        `((:metadata . ,(message-metadata msg))))))

(defun serialize-buffer (buf)
  "Serialize a buffer's conversation to JSON-ready alist."
  (let ((messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :when (and (message-read-only-p msg)
                     (not (buffer-ephemeral-display-message-p msg)))
          :do (push (serialize-message msg) messages))
    `((:name . ,(buffer-name buf))
      ,@(when (and (buffer-session buf)
                   (session-display-name (buffer-session buf)))
          `((:display-name . ,(session-display-name (buffer-session buf)))))
      ,@(when (buffer-session buf)
          `((:session-id . ,(session-id (buffer-session buf)))
            (:created-at . ,(session-created-at (buffer-session buf)))
            (:updated-at . ,(session-updated-at (buffer-session buf)))))
      (:agent-name . ,(buffer-agent-name buf))
      (:kind . ,(symbol-name (buffer-kind buf)))
      (:session-persistence-mode
       . ,(symbol-name (buffer-session-persistence-mode buf)))
      (:major-mode . ,(buffer-major-mode buf))
      (:working-directory
       . ,(namestring (uiop:ensure-directory-pathname
                       (buffer-working-directory buf))))
      (:provider-override . ,(buffer-provider-override buf))
      (:model-override . ,(buffer-model-override buf))
      (:think-level-override . ,(buffer-think-level-override buf))
      (:model-role-override . ,(buffer-model-role-override buf))
      (:service-tier-override . ,(buffer-service-tier-override buf))
      (:pipeline-name . ,(buffer-pipeline-name buf))
      ,@(when (buffer-model-role-set-override buf)
          `((:model-role-set-override
             . ,(coerce (copy-list (buffer-model-role-set-override buf))
                        'vector))))
      ,@(when (buffer-user-input-pending buf)
          `((:user-input-pending . ,(buffer-user-input-pending buf))))
      ,@(when (buffer-queued-steering-messages buf)
          `((:queued-steering-messages
             . ,(coerce (copy-list (buffer-queued-steering-messages buf))
                        'vector))))
      ,@(when (buffer-queued-follow-up-messages buf)
          `((:queued-follow-up-messages
             . ,(coerce (copy-list (buffer-queued-follow-up-messages buf))
                        'vector))))
      (:enabled-packages . ,(coerce (copy-list (buffer-enabled-packages buf))
                                    'vector))
      ,@(let ((buffer-state (serialize-buffer-extra-state buf)))
          (when buffer-state
            `((:buffer-state . ,buffer-state))))
      (:messages . ,(coerce (nreverse messages) 'vector)))))

(defun save-session (buf)
  "Save the buffer's conversation to a session file."
  (let ((session (buffer-session buf)))
    (when (and session (buffer-persistent-session-p buf))
      (setf (session-working-directory session)
            (normalize-buffer-working-directory
             (buffer-working-directory buf)))
      (write-session-manifest session)))
  (when (buffer-persistent-session-p buf)
    (let ((path (session-path (buffer-name buf))))
      (ensure-directories-exist path)
      (with-open-file (s path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string (cl-json:encode-json-to-string (serialize-buffer buf)) s))
      (maybe-run-hook-with-args '*after-session-save-hook* buf path)
      path)))

(defun replay-serialized-message (buf msg-data)
  "Insert one serialized message into BUF without transcript side effects."
  (let* ((sender-str (or (cdr (assoc :sender msg-data)) "SYSTEM"))
         (text (cdr (assoc :text msg-data)))
         (raw-content (cdr (assoc :raw-content msg-data)))
         (metadata (cdr (assoc :metadata msg-data)))
         (entry-id (cdr (assoc :entry-id msg-data)))
         (parent-entry-id (cdr (assoc :parent-entry-id msg-data)))
         (sender-kw (intern sender-str :keyword))
         (msg (buffer-insert-read-only-message
               buf sender-kw (or text "")
               :raw-content (and raw-content
                                 (normalize-legacy-raw-content raw-content))
               :metadata metadata
               :timestamp (cdr (assoc :timestamp msg-data))
               :record-p nil
               :run-hook-p nil)))
    (setf (message-entry-id msg) entry-id
          (message-parent-entry-id msg) parent-entry-id)
    msg))

(defun replay-serialized-messages (buf messages)
  "Replay serialized MESSAGES into BUF without transcript or autosave writes."
  (let ((*suppress-session-transcript-recording* t)
        (*suppress-session-autosave* t))
    (loop :for msg-data :in (coerce (or messages #()) 'list)
          :do (replay-serialized-message buf msg-data)))
  buf)

(defun replace-buffer-history-with-serialized-messages
    (buf messages &key input-text (autosave-p t))
  "Replace BUF history with serialized MESSAGES and optional INPUT-TEXT."
  (let ((*suppress-session-transcript-recording* t)
        (*suppress-session-autosave* t))
    (buffer-clear-history-before-input buf)
    (replay-serialized-messages buf messages)
    (when input-text
      (set-message-text (buffer-input-message buf) input-text)))
  (when autosave-p
    (autosave-session-snapshot buf))
  buf)

(defun buffer-tree-backed-history-p (buf)
  "Return true when BUF's loaded history points at transcript tree entries."
  (loop :for msg := (buffer-first-message buf) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buf))))
        :thereis (message-entry-id msg)))

(defun apply-session-branch-state-to-buffer (buf session &key overwrite-nil-p)
  "Apply SESSION's branch-local provider state to BUF.
When OVERWRITE-NIL-P is false, NIL branch values leave snapshot metadata alone."
  (multiple-value-bind (provider model think-level)
      (session-branch-state session)
    (when (or provider overwrite-nil-p)
      (setf (buffer-provider-override buf) provider))
    (when (or model overwrite-nil-p)
      (setf (buffer-model-override buf) model))
    (when (or think-level overwrite-nil-p)
      (setf (buffer-think-level-override buf) think-level)))
  buf)

(defun session-transcript-message-events (session)
  "Return durable serialized messages for SESSION's active branch."
  (session-active-branch-message-events session))

(defun load-session-snapshot
    (session-name path agent-name &key source-root (root *sessions-dir*))
  "Load SESSION-NAME from snapshot PATH."
  (let ((cl-json:*json-array-type* 'vector))
    (let* ((json-str (uiop:read-file-string path))
           (data (cl-json:decode-json-from-string json-str))
           (name (or (cdr (assoc :name data)) session-name))
           (display-name
             (normalize-session-display-name (cdr (assoc :display-name data))))
           (agent (or (cdr (assoc :agent-name data)) agent-name))
           (kind (cdr (assoc :kind data)))
           (major-mode (cdr (assoc :major-mode data)))
           (working-directory
             (normalize-buffer-working-directory
              (cdr (assoc :working-directory data))))
           (provider-override (cdr (assoc :provider-override data)))
           (model-override (cdr (assoc :model-override data)))
           (think-level-override (cdr (assoc :think-level-override data)))
           (model-role-override (cdr (assoc :model-role-override data)))
           (model-role-set-override (cdr (assoc :model-role-set-override data)))
           (service-tier-override (cdr (assoc :service-tier-override data)))
           (pipeline-name (cdr (assoc :pipeline-name data)))
           (user-input-pending (cdr (assoc :user-input-pending data)))
           (queued-steering-messages
             (cdr (assoc :queued-steering-messages data)))
           (queued-follow-up-messages
             (cdr (assoc :queued-follow-up-messages data)))
           (session-persistence-mode
             (cdr (assoc :session-persistence-mode data)))
           (enabled-packages (cdr (assoc :enabled-packages data)))
           (buffer-state (cdr (assoc :buffer-state data)))
           (messages (cdr (assoc :messages data)))
           (buf (make-buffer name :agent-name agent
                                  :kind (or kind :chat)
                                  :working-directory working-directory
                                  :major-mode major-mode
                                  :session-persistence-mode
                                  (normalize-buffer-session-persistence-mode
                                   (or session-persistence-mode
                                       :persistent))
                                  :session (load-or-create-session
                                            name
                                            :working-directory
                                            working-directory
                                            :display-name display-name
                                            :source-root source-root
                                            :root root))))
      (setf (buffer-provider-override buf)
            (and provider-override
                 (ignore-errors
                   (normalize-provider provider-override)))
            (buffer-model-override buf)
            model-override
            (buffer-think-level-override buf)
            (normalize-think-level-override think-level-override)
            (buffer-model-role-override buf)
            (normalize-model-role-override model-role-override)
            (buffer-model-role-set-override buf)
            (remove nil
                    (mapcar #'normalize-model-role-override
                            (coerce (or model-role-set-override #()) 'list))
                    :test #'equal)
            (buffer-service-tier-override buf)
            (normalize-service-tier-override service-tier-override)
            (buffer-pipeline-name buf)
            (and (stringp pipeline-name)
                 (plusp (length (string-trim
                                  '(#\Space #\Tab #\Newline #\Return)
                                  pipeline-name)))
                 (string-downcase pipeline-name))
            (buffer-user-input-pending buf)
            user-input-pending
            (buffer-queued-steering-messages buf)
            (copy-list (coerce (or queued-steering-messages #()) 'list))
            (buffer-queued-follow-up-messages buf)
            (copy-list (coerce (or queued-follow-up-messages #()) 'list))
            (buffer-enabled-packages buf)
            (loop :for package :in (coerce (or enabled-packages #()) 'list)
                  :when (stringp package)
                    :collect package))
      (replay-serialized-messages buf messages)
      (if (and (eq (buffer-kind buf) :listener)
               (fboundp 'restore-retired-listener-buffer-state))
          (funcall (symbol-function 'restore-retired-listener-buffer-state)
                   buf buffer-state)
          (restore-buffer-extra-state buf buffer-state))
      (ignore-errors
        (reconcile-buffer-think-level-override buf))
      (sync-buffer-system-prompt-display buf)
      buf)))

(defun load-session-sidecar
    (session-name &key (agent-name *default-agent-name*)
                         (root *sessions-dir*) source-root)
  "Load SESSION-NAME from its transcript sidecar."
  (let* ((source-root (or source-root root))
         (root (if (equal (pathname root) (pathname +legacy-sessions-dir+))
                   (materialize-legacy-sessions-before-mutation)
                   root))
         (manifest-path (session-sidecar-manifest-path session-name :root root))
         (manifest (read-session-manifest manifest-path)))
    (unless manifest
      (return-from load-session-sidecar nil))
    (when (typep manifest 'session-manifest-parse-error)
      (error manifest))
    (let* ((name (or (cdr (assoc :name manifest)) session-name))
           (display-name
             (normalize-session-display-name
              (cdr (assoc :display-name manifest))))
           (session (load-or-create-session name
                                            :display-name display-name
                                            :source-root source-root
                                            :root root))
           (buf (make-buffer name :agent-name agent-name
                                  :working-directory
                                  (session-working-directory session)
                                  :session-persistence-mode :persistent
                                  :session session)))
      (replay-serialized-messages
       buf
       (session-transcript-message-events session))
      (apply-session-branch-state-to-buffer buf session :overwrite-nil-p t)
      (sync-buffer-system-prompt-display buf)
      (autosave-session-snapshot buf)
      buf)))

(defun load-session (session-designator &key (agent-name *default-agent-name*))
  "Load a saved session into a new buffer. Returns the buffer or nil.
SESSION-DESIGNATOR may be an exact session name, a unique id prefix, or a
snapshot/manifest path."
  (let* ((record (resolve-saved-session-record session-designator))
         (session-name (or (and record (getf record :session-name))
                           session-designator))
         (path (or (and record (getf record :path))
                   (and (stringp session-name)
                        (session-path session-name))))
         (source (and record (getf record :source)))
         (source-root (and record (getf record :source-root)))
         (storage-root
           (cond
             ((null source-root) *sessions-dir*)
             ((equal (pathname source-root)
                     (pathname +legacy-sessions-dir+))
              (materialize-legacy-sessions-before-mutation))
             (t source-root)))
         (buf (cond
                ((or (eq source :snapshot)
                     (and (null source)
                          path
                          (probe-file path)
                          (not (eq source :sidecar))))
                 (let ((snapshot
                         (load-session-snapshot session-name
                                                (or path
                                                    (session-path session-name))
                                                agent-name
                                                :source-root source-root
                                                :root storage-root)))
                   (when snapshot
                     (let* ((session (buffer-session snapshot))
                            (sidecar-messages
                              (and session
                                   (session-transcript-message-events
                                    session))))
                       (if (buffer-tree-backed-history-p snapshot)
                           (when sidecar-messages
                             (replace-buffer-history-with-serialized-messages
                              snapshot sidecar-messages :autosave-p nil)
                             (apply-session-branch-state-to-buffer
                              snapshot session))
                           (when session
                             (set-session-current-leaf session nil)))))
                   snapshot))
                ((eq source :sidecar)
                 (load-session-sidecar session-name
                                       :agent-name agent-name
                                       :root source-root
                                       :source-root source-root))
                (t
                 (load-session-sidecar session-name :agent-name agent-name)))))
    (when buf
      (sync-buffer-system-prompt-display buf)
      (maybe-run-hook-with-args '*after-session-load-hook* buf session-name))
    buf))

(defun saved-session-snapshot-names (&optional (root (selected-sessions-read-root)))
  "Return session names with legacy JSON snapshots."
  (when (probe-file root)
    (mapcar #'pathname-name
            (directory (merge-pathnames "*.json" root)))))

(defun saved-session-sidecar-names (&optional (root (selected-sessions-read-root)))
  "Return session names with transcript sidecar manifests."
  (when (probe-file root)
    (loop :for path :in (directory (merge-pathnames #P"*/session.json"
                                                    root))
          :for manifest := (read-session-manifest path)
          :for name := (and (listp manifest)
                            (cdr (assoc :name manifest)))
          :when (typep manifest 'session-manifest-parse-error)
            :do (warn "Failed to parse session manifest ~A: ~A"
                      (session-manifest-parse-error-path manifest)
                      (session-manifest-parse-error-cause manifest))
          :when (and (stringp name)
                     (plusp (length name)))
            :collect name)))

(defun session-record-display-name (record)
  "Return RECORD's display name or its session name."
  (or (getf record :display-name)
      (getf record :session-name)))

(defun read-session-record-from-snapshot (session-name path)
  "Return a session listing record from a legacy JSON snapshot."
  (let ((cl-json:*json-array-type* 'vector))
    (handler-case
        (let* ((data (cl-json:decode-json-from-string
                      (uiop:read-file-string path)))
               (display-name
                 (normalize-session-display-name (cdr (assoc :display-name data)))))
          (list :session-name (or (cdr (assoc :name data)) session-name)
                :display-name display-name
                :session-id (cdr (assoc :session-id data))
                :working-directory
                (cdr (assoc :working-directory data))
                :updated-at (cdr (assoc :updated-at data))
                :created-at (cdr (assoc :created-at data))
                :path path
                :source :snapshot))
      (error (condition)
        (warn "Failed to parse session snapshot ~A: ~A" path condition)
        nil))))

(defun read-session-record-from-sidecar (session-name path)
  "Return a session listing record from a sidecar manifest."
  (let ((manifest (read-session-manifest path)))
    (cond
      ((null manifest) nil)
      ((typep manifest 'session-manifest-parse-error)
       (warn "Failed to parse session manifest ~A: ~A"
             (session-manifest-parse-error-path manifest)
             (session-manifest-parse-error-cause manifest))
       nil)
      (t
       (list :session-name (or (cdr (assoc :name manifest)) session-name)
             :display-name
             (normalize-session-display-name (cdr (assoc :display-name manifest)))
             :session-id (cdr (assoc :id manifest))
             :working-directory (cdr (assoc :working-directory manifest))
             :updated-at (cdr (assoc :updated-at manifest))
             :created-at (cdr (assoc :created-at manifest))
             :path path
             :source :sidecar)))))

(defun list-saved-session-records ()
  "Return saved session records with display names and metadata."
  (let ((root (selected-sessions-read-root)))
    (when (probe-file root)
      (let ((records nil))
        (dolist (session-name (list-saved-sessions))
          (let* ((snapshot-path (session-path session-name :root root))
                 (sidecar-path
                   (session-sidecar-manifest-path session-name :root root))
               (snapshot (and (probe-file snapshot-path)
                              (read-session-record-from-snapshot
                               session-name snapshot-path)))
               (sidecar (and (probe-file sidecar-path)
                             (read-session-record-from-sidecar
                              session-name sidecar-path)))
               (display-name (or (getf snapshot :display-name)
                                 (getf sidecar :display-name)
                                 session-name))
               (working-directory
                 (or (getf snapshot :working-directory)
                     (getf sidecar :working-directory)))
               (updated-at (or (getf snapshot :updated-at)
                               (getf sidecar :updated-at)))
               (created-at (or (getf snapshot :created-at)
                               (getf sidecar :created-at)))
               (path (or (getf snapshot :path)
                         (getf sidecar :path)
                         snapshot-path))
               (source (or (getf snapshot :source)
                           (getf sidecar :source)
                           :unknown)))
          (push (list :session-name session-name
                      :display-name display-name
                      :session-id (or (getf snapshot :session-id)
                                      (getf sidecar :session-id))
                      :working-directory working-directory
                      :updated-at updated-at
                      :created-at created-at
                      :path path
                      :source-root root
                      :source source)
                records)))
        (sort records
              #'string<
              :key (lambda (record)
                     (session-record-display-name record)))))))

(defun normalize-session-record-working-directory (value)
  "Return VALUE as a comparable absolute directory namestring, or NIL."
  (when value
    (let* ((path (normalize-session-working-directory value))
           (resolved (or (ignore-errors (truename path)) path)))
      (namestring (uiop:ensure-directory-pathname resolved)))))

(defun session-record-timestamp (record)
  "Return RECORD's best available timestamp for recency sorting."
  (or (getf record :updated-at)
      (getf record :created-at)
      0))

(defun session-record-with-source-root (record root)
  "Return RECORD annotated with its discovery ROOT, or NIL."
  (and record
       (append record (list :source-root
                            (uiop:ensure-directory-pathname root)))))

(defun explicit-session-record-from-path (designator)
  "Return a saved-session record for DESIGNATOR when it names a session path."
  (let* ((path (probe-file designator))
         (directory (and path (uiop:directory-pathname-p path))))
    (cond
      ((null path) nil)
      (directory
       (let ((manifest-path (merge-pathnames #P"session.json" path)))
         (and (probe-file manifest-path)
              (session-record-with-source-root
               (read-session-record-from-sidecar "" manifest-path)
               (uiop:pathname-parent-directory-pathname path)))))
      ((string= (pathname-name path) "session")
       (session-record-with-source-root
        (read-session-record-from-sidecar "" path)
        (uiop:pathname-parent-directory-pathname
         (uiop:pathname-directory-pathname path))))
      (t
       (session-record-with-source-root
        (read-session-record-from-snapshot (pathname-name path) path)
        (uiop:pathname-directory-pathname path))))))

(defun resolve-saved-session-record (designator)
  "Resolve DESIGNATOR to one saved-session record, or NIL when not found.
DESIGNATOR may be an exact session name, a unique session-id prefix, or a
snapshot/manifest path."
  (when (and (stringp designator)
             (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         designator))))
    (or (explicit-session-record-from-path designator)
        (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     designator))
               (records (or (list-saved-session-records) nil))
               (exact (find trimmed records
                            :key (lambda (record)
                                   (getf record :session-name))
                            :test #'string=)))
          (or exact
              (let ((matches
                      (remove-if-not
                       (lambda (record)
                         (let ((session-id (getf record :session-id)))
                           (and (stringp session-id)
                                (<= (length trimmed) (length session-id))
                                (string-equal trimmed
                                              (subseq session-id 0
                                                      (length trimmed))))))
                       records)))
                (cond
                  ((null matches) nil)
                  ((null (rest matches)) (first matches))
                  (t
                   (error "Session id prefix ~A is ambiguous: ~{~A~^, ~}"
                          trimmed
                          (mapcar (lambda (record)
                                    (getf record :session-name))
                                  matches))))))))))

(defun most-recent-saved-session-record (&key working-directory)
  "Return the most recent saved-session record, optionally scoped to WORKING-DIRECTORY."
  (let* ((target-directory
           (normalize-session-record-working-directory working-directory))
         (records
           (if target-directory
               (remove-if-not
                (lambda (record)
                  (let ((record-directory
                          (normalize-session-record-working-directory
                           (getf record :working-directory))))
                    (and record-directory
                         (string= record-directory target-directory))))
                (or (list-saved-session-records) nil))
               (or (list-saved-session-records) nil))))
    (car (sort (copy-list records) #'> :key #'session-record-timestamp))))

(defun most-recent-saved-session-name (&key working-directory)
  "Return the session name for the most recent saved session."
  (let ((record (most-recent-saved-session-record
                 :working-directory working-directory)))
    (and record (getf record :session-name))))

(defun list-saved-sessions ()
  "Return a list of saved session names."
  (let ((root (selected-sessions-read-root)))
    (when (probe-file root)
      (sort (remove-duplicates
             (append (saved-session-snapshot-names root)
                     (saved-session-sidecar-names root))
             :test #'string=)
            #'string<))))
