(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Special Variables
;;; --------------------------------------------------------------------------

(defvar *current-caller* :user
  "The current caller context. Bound to :USER for interactive use,
or an agent keyword (e.g., :CODER) during agent command dispatch.")

;;; --------------------------------------------------------------------------
;;; Command Metadata
;;; --------------------------------------------------------------------------

(defstruct command-metadata
  "Metadata for a registered command."
  (name        (error "name required")       :type symbol   :read-only t)
  (permission  :user-only                    :type keyword  :read-only t)
  (docstring   ""                            :type string   :read-only t)
  (keybindings nil                           :type list     :read-only t)
  (lambda-list '(buffer)                     :type list     :read-only t)
  (interactive-spec t                        :type t        :read-only t))

(defvar *command-table* (make-hash-table :test #'eq)
  "Global table mapping command symbols to command-metadata.")

;;; --------------------------------------------------------------------------
;;; Command Validation Helpers
;;; --------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun command-lambda-list-keyword-p (symbol)
    "Return T when SYMBOL is a lambda-list keyword unsupported by defcommand."
    (member symbol '(&optional &rest &body &key &allow-other-keys
                     &aux &whole &environment)
            :test #'eq))

  (defun validate-command-lambda-list (name lambda-list)
    "Validate that LAMBDA-LIST is a defcommand-compatible required arg list."
    (unless (and (listp lambda-list) (plusp (length lambda-list)))
      (error "defcommand ~A requires a non-empty lambda list." name))
    (dolist (item lambda-list)
      (unless (symbolp item)
        (error "defcommand ~A only supports symbol parameters, got ~S."
               name item))
      (when (command-lambda-list-keyword-p item)
        (error "defcommand ~A does not support lambda-list keyword ~S."
               name item)))
    lambda-list)

  (defun default-command-prompt (arg-name)
    "Return the default minibuffer prompt string for ARG-NAME."
    (string-capitalize
     (substitute #\Space #\-
                 (string-downcase (symbol-name arg-name)))))

  (defun normalize-command-interactive-entry (command-name arg-name entry)
    "Normalize one interactive ENTRY for ARG-NAME on COMMAND-NAME."
    (unless (and (consp entry) (symbolp (first entry)))
      (error "defcommand ~A interactive entry for ~A must start with the argument name, got ~S."
             command-name arg-name entry))
    (let ((entry-name (first entry))
          (plist (rest entry)))
      (unless (eq entry-name arg-name)
        (error "defcommand ~A interactive entry ~S does not match argument ~S."
               command-name entry-name arg-name))
      (unless (evenp (length plist))
        (error "defcommand ~A interactive entry for ~A has an odd plist: ~S."
               command-name arg-name entry))
      (loop :for rest :on plist :by #'cddr
            :for key := (first rest)
            :unless (member key '(:prompt :reader) :test #'eq)
              :do (error "defcommand ~A interactive entry for ~A has unsupported key ~S."
                         command-name arg-name key))
      (let* ((prompt (if (member :prompt plist :test #'eq)
                         (string (getf plist :prompt))
                         (default-command-prompt arg-name)))
             (reader (when (member :reader plist :test #'eq)
                       (getf plist :reader))))
        (list :name arg-name
              :prompt prompt
              :reader reader))))

  (defun normalize-command-interactive-spec (name lambda-list interactive supplied-p)
    "Normalize the interactive metadata for NAME and LAMBDA-LIST."
    (let ((args (rest lambda-list)))
      (cond
        ((not supplied-p)
         (if args nil t))
        ((null interactive)
         nil)
        ((eq interactive t)
         (when args
           (error "defcommand ~A requires an interactive arg spec for parameters ~S."
                  name args))
         t)
        ((listp interactive)
         (unless (= (length interactive) (length args))
           (error "defcommand ~A interactive spec count ~D does not match argument count ~D."
                  name (length interactive) (length args)))
         (mapcar (lambda (arg entry)
                   (normalize-command-interactive-entry name arg entry))
                 args interactive))
        (t
         (error "defcommand ~A interactive spec must be T, NIL, or a list, got ~S."
                name interactive))))))

;;; --------------------------------------------------------------------------
;;; Conditions
;;; --------------------------------------------------------------------------

(define-condition permission-denied (error)
  ((command :initarg :command :reader permission-denied-command
            :documentation "The command symbol that was denied."))
  (:documentation "Signaled when a command is invoked by a caller who lacks permission.")
  (:report (lambda (c stream)
             (format stream "Permission denied: ~A is not available to ~A"
                     (permission-denied-command c) *current-caller*))))

(define-condition permission-required (error)
  ((command :initarg :command :reader permission-required-command
            :documentation "The command symbol that requires approval."))
  (:documentation "Signaled when a command requires explicit user approval before execution.")
  (:report (lambda (c stream)
             (format stream "Permission required: ~A needs approval for ~A"
                     *current-caller* (permission-required-command c)))))

;;; --------------------------------------------------------------------------
;;; Access Control
;;; --------------------------------------------------------------------------

(declaim (ftype (function (symbol) (values)) check-permission))
(defun check-permission (command-name)
  "Check whether *CURRENT-CALLER* has permission to execute COMMAND-NAME.
Signals PERMISSION-DENIED or PERMISSION-REQUIRED as appropriate."
  (let* ((metadata (gethash command-name *command-table*))
         (permission (command-metadata-permission metadata)))
    (ecase permission
      (:agent-allowed
       (values))
      (:agent-with-permission
       (unless (eq *current-caller* :user)
         (restart-case
             (error 'permission-required :command command-name)
           (grant-permission ()
             :report "Grant permission for this command"
             (values))
           (deny-permission ()
             :report "Deny permission for this command"
             (error 'permission-denied :command command-name)))))
      (:user-only
       (unless (eq *current-caller* :user)
         (error 'permission-denied :command command-name))
       (values)))))

;;; --------------------------------------------------------------------------
;;; Command Listing
;;; --------------------------------------------------------------------------

(declaim (ftype (function () list) list-available-commands))
(defun list-available-commands ()
  "Return a list of command symbols available to *CURRENT-CALLER*.
Filters out :USER-ONLY commands when caller is not :USER."
  (let ((result nil))
    (maphash (lambda (name metadata)
               (let ((perm (command-metadata-permission metadata)))
                 (when (or (eq *current-caller* :user)
                           (not (eq perm :user-only)))
                   (push name result))))
             *command-table*)
    result))

(declaim (ftype (function (symbol) list) command-required-arguments))
(defun command-required-arguments (command-name)
  "Return the required non-buffer arguments for COMMAND-NAME."
  (let ((metadata (gethash command-name *command-table*)))
    (when metadata
      (rest (command-metadata-lambda-list metadata)))))

(declaim (ftype (function (symbol) t) command-interactive-p))
(defun command-interactive-p (command-name)
  "Return the interactive metadata for COMMAND-NAME, or NIL when absent."
  (let ((metadata (gethash command-name *command-table*)))
    (when metadata
      (command-metadata-interactive-spec metadata))))

(declaim (ftype (function () list) list-interactive-commands))
(defun list-interactive-commands ()
  "Return interactive commands available to *CURRENT-CALLER*."
  (remove-if-not #'command-interactive-p (list-available-commands)))

(declaim (ftype (function (t) function) resolve-command-interactive-reader))
(defun resolve-command-interactive-reader (reader)
  "Resolve READER into a callable function for interactive prompts."
  (cond
    ((null reader) #'identity)
    ((functionp reader) reader)
    ((symbolp reader) (fdefinition reader))
    ((and (consp reader)
          (eq (first reader) 'function)
          (symbolp (second reader)))
     (fdefinition (second reader)))
    (t
     (error "Invalid interactive reader designator: ~S" reader))))

;;; --------------------------------------------------------------------------
;;; defcommand Macro
;;; --------------------------------------------------------------------------

(defmacro defcommand (name (&key (permission :user-only)
                                 (keys nil)
                                 (interactive nil interactive-supplied-p))
                      docstring lambda-list &body body)
  "Define a command as a generic function with access control.

Expands to:
1. Registration of command metadata in *command-table*
2. A generic function definition
3. An :around method that checks permissions
4. A primary method with BODY

Example:
  (defcommand send-message (:permission :user-only :keys ((#\\Return)))
    \"Send the current input.\"
    (buffer)
    (buffer-finalize-input buffer))"
  (let* ((lambda-list (validate-command-lambda-list name lambda-list))
         (interactive-spec
           (normalize-command-interactive-spec name lambda-list
                                               interactive interactive-supplied-p))
         (buffer-var (first lambda-list))
         (other-args (rest lambda-list))
         (meta-var (gensym "META")))
    `(progn
       ;; Register metadata
       (let ((,meta-var (make-command-metadata
                         :name ',name
                         :permission ,permission
                         :docstring ,docstring
                         :keybindings ',keys
                         :lambda-list ',lambda-list
                         :interactive-spec ',interactive-spec)))
         (setf (gethash ',name *command-table*) ,meta-var))

       ;; Define the generic function (idempotent in CLOS)
       (defgeneric ,name ,lambda-list
         (:documentation ,docstring))

       ;; Define the access control :around method
       (defmethod ,name :around ((,buffer-var buffer) ,@other-args)
         ,@(when other-args `((declare (ignorable ,@other-args))))
         (check-permission ',name)
         (call-next-method))

       ;; Define the primary method
       (defmethod ,name ((,buffer-var buffer) ,@other-args)
         ,@body)

       ',name)))

;;; --------------------------------------------------------------------------
;;; Extended Documentation System
;;; --------------------------------------------------------------------------

(defvar *extended-docs* (make-hash-table :test #'eq)
  "Hash table mapping symbols to extended documentation plists.
Each entry is a plist with optional keys:
  :usage        — parameter types and example call
  :returns      — return type and example value
  :see-also     — list of related symbols
  :category     — category string for grouping
  :side-effects — description of mutations, I/O, or global state changes")

(defmacro defdoc (name &key category usage returns see-also side-effects)
  "Define extended documentation for SYMBOL.
Stores a plist in *extended-docs* keyed by the symbol.

Example:
  (defdoc make-buffer
    :category \"buffer\"
    :usage \"(make-buffer NAME &key :agent-name :working-directory)\"
    :returns \"buffer — A new buffer object with a single empty input message.\"
    :see-also (buffer buffer-name add-buffer-to-ring)
    :side-effects \"Allocates a new buffer with an empty input message.\")"
  `(setf (gethash ',name *extended-docs*)
         (list ,@(when category `(:category ,category))
               ,@(when usage `(:usage ,usage))
               ,@(when returns `(:returns ,returns))
               ,@(when see-also `(:see-also ',see-also))
               ,@(when side-effects `(:side-effects ,side-effects)))))

(defun extended-doc (symbol &optional key)
  "Return the extended documentation for SYMBOL.
If KEY is provided (e.g. :usage, :returns), return just that property value.
Without KEY, returns the full plist or NIL if no extended doc exists."
  (let ((doc (gethash symbol *extended-docs*)))
    (if key
        (getf doc key)
        doc)))
