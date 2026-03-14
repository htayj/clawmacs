(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Special Variables
;;; --------------------------------------------------------------------------

(defvar *current-caller* :user
  "The current caller context. Bound to :USER for interactive use,
or an agent keyword (e.g., :CLAUDE-OPUS) during agent command dispatch.")

(defvar *sandbox-root* nil
  "When non-nil, restricts file operations to this directory subtree.")

;;; --------------------------------------------------------------------------
;;; Command Metadata
;;; --------------------------------------------------------------------------

(defstruct command-metadata
  "Metadata for a registered command."
  (name        (error "name required")       :type symbol   :read-only t)
  (permission  :user-only                    :type keyword  :read-only t)
  (docstring   ""                            :type string   :read-only t)
  (keybindings nil                           :type list     :read-only t))

(defvar *command-table* (make-hash-table :test #'eq)
  "Global table mapping command symbols to command-metadata.")

;;; --------------------------------------------------------------------------
;;; Conditions
;;; --------------------------------------------------------------------------

(define-condition permission-denied (error)
  ((command :initarg :command :reader permission-denied-command))
  (:report (lambda (c stream)
             (format stream "Permission denied: ~A is not available to ~A"
                     (permission-denied-command c) *current-caller*))))

(define-condition permission-required (error)
  ((command :initarg :command :reader permission-required-command))
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

;;; --------------------------------------------------------------------------
;;; defcommand Macro
;;; --------------------------------------------------------------------------

(defmacro defcommand (name (&key (permission :user-only) (keys nil))
                      docstring (buffer-var) &body body)
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
  (let ((meta-var (gensym "META")))
    `(progn
       ;; Register metadata
       (let ((,meta-var (make-command-metadata
                         :name ',name
                         :permission ,permission
                         :docstring ,docstring
                         :keybindings ',keys)))
         (setf (gethash ',name *command-table*) ,meta-var))

       ;; Define the generic function (idempotent in CLOS)
       (defgeneric ,name (,buffer-var)
         (:documentation ,docstring))

       ;; Define the access control :around method
       (defmethod ,name :around ((,buffer-var buffer))
         (check-permission ',name)
         (call-next-method))

       ;; Define the primary method
       (defmethod ,name ((,buffer-var buffer))
         ,@body)

       ',name)))
