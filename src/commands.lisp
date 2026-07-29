(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Special Variables
;;; --------------------------------------------------------------------------

(defvar *current-caller* :user
  "The current caller context. Bound to :USER for interactive use,
or an agent keyword (e.g., :CODER) during agent tool dispatch.")

;;; --------------------------------------------------------------------------
;;; Command Metadata
;;; --------------------------------------------------------------------------

(defstruct command-metadata
  "Metadata for a registered command."
  (name        (error "name required")       :type symbol   :read-only t)
  (docstring   ""                            :type string   :read-only t)
  (keybindings nil                           :type list     :read-only t)
  (lambda-list '(buffer)                     :type list     :read-only t)
  (prompts     nil                           :type list     :read-only t)
  (package     nil                           :type (or null string) :read-only t))

(defvar *command-table* (make-hash-table :test #'eq)
  "Global table mapping command symbols to command-metadata.")

(defvar *process-command-table* *command-table*
  "Process-global command table, distinct from dynamic test bindings.")

(defvar *command-registry-lock*
  (bt:make-lock "rplaca command registry")
  "Lock guarding bounded access to the process-global command table.")

(defun call-with-command-registry-lock (function &optional
                                                   (table *command-table*))
  "Call FUNCTION under the command lock when TABLE is process-global.

FUNCTION must only perform bounded table access.  Package visibility checks,
UI work, and extension callbacks belong after the lock has been released."
  (if (eq table *process-command-table*)
      (bt:with-lock-held (*command-registry-lock*)
        (funcall function))
      (funcall function)))

(defun command-registry-snapshot (&optional (table *command-table*))
  "Return a stable alist snapshot of command TABLE."
  (call-with-command-registry-lock
   (lambda ()
     (let ((entries nil))
       (maphash (lambda (name metadata)
                  (push (cons name metadata) entries))
                table)
       entries))
   table))

(defun find-command-metadata (name)
  "Return command metadata registered for NAME, or NIL."
  (call-with-command-registry-lock
   (lambda () (gethash name *command-table*))
   *command-table*))

(defun remove-command-metadata-for-package (package-name)
  "Atomically remove command metadata owned by PACKAGE-NAME."
  (call-with-command-registry-lock
   (lambda ()
     (let ((removed nil))
       (maphash
        (lambda (name metadata)
          (when (string= package-name
                         (or (command-metadata-package metadata) ""))
            (push metadata removed)
            (remhash name *command-table*)))
        *command-table*)
       (nreverse removed)))
   *command-table*))

;;; --------------------------------------------------------------------------
;;; Command Validation Helpers
;;; --------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun command-lambda-list-keyword-p (symbol)
    "Return T when SYMBOL is a lambda-list keyword unsupported by defcommand."
    (member symbol '(&optional &rest &body &key &allow-other-keys
                     &aux &whole &environment)
            :test #'eq))

  (defun function-lambda-list (symbol)
    "Return SYMBOL's runtime function lambda list, or NIL when unavailable."
    (handler-case
        (let ((fn (fdefinition symbol)))
          (cond
            ((typep fn 'generic-function)
             #+sbcl (sb-mop:generic-function-lambda-list fn)
             #-sbcl nil)
            (t
             #+sbcl (sb-introspect:function-lambda-list symbol)
             #-sbcl nil)))
      (error () nil)))

  (defun command-function-lambda-list (name)
    "Return NAME's function lambda list or signal a defcommand error."
    (unless (fboundp name)
      (error "defcommand ~A requires a separately defined function." name))
    (or (function-lambda-list name)
        (error "defcommand ~A could not determine the function lambda list."
               name)))

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

  (defun normalize-command-prompt-entry (command-name arg-name entry)
    "Normalize one prompt ENTRY for ARG-NAME on COMMAND-NAME."
    (unless (and (consp entry) (symbolp (first entry)))
      (error "defcommand ~A prompt entry for ~A must start with the argument name, got ~S."
             command-name arg-name entry))
    (let ((entry-name (first entry))
          (plist (rest entry)))
      (unless (eq entry-name arg-name)
        (error "defcommand ~A prompt entry ~S does not match argument ~S."
               command-name entry-name arg-name))
      (unless (evenp (length plist))
        (error "defcommand ~A prompt entry for ~A has an odd plist: ~S."
               command-name arg-name entry))
      (loop :for rest :on plist :by #'cddr
            :for key := (first rest)
            :unless (member key '(:prompt :reader) :test #'eq)
              :do (error "defcommand ~A prompt entry for ~A has unsupported key ~S."
                         command-name arg-name key))
      (let* ((prompt (if (member :prompt plist :test #'eq)
                         (string (getf plist :prompt))
                         (default-command-prompt arg-name)))
             (reader (when (member :reader plist :test #'eq)
                       (getf plist :reader))))
        (list :name arg-name
              :prompt prompt
              :reader reader))))

  (defun normalize-command-prompts (name lambda-list prompts supplied-p)
    "Normalize minibuffer prompt metadata for NAME and LAMBDA-LIST."
    (let ((args (rest lambda-list)))
      (cond
        ((not supplied-p)
         (when args
           (error "defcommand ~A requires :PROMPTS metadata for parameters ~S."
                  name args))
         nil)
        ((listp prompts)
         (unless (= (length prompts) (length args))
           (error "defcommand ~A prompt spec count ~D does not match argument count ~D."
                  name (length prompts) (length args)))
         (mapcar (lambda (arg entry)
                   (normalize-command-prompt-entry name arg entry))
                 args prompts))
        (t
         (error "defcommand ~A :PROMPTS must be a list, got ~S."
                name prompts))))))

;;; --------------------------------------------------------------------------
;;; Command Listing
;;; --------------------------------------------------------------------------

(defun command-metadata-visible-p (metadata &key buffer agent-name include-inactive)
  "Return true when METADATA should be visible in the current package context."
  (let ((package (command-metadata-package metadata)))
    (or include-inactive
        (null package)
        (package-active-p package :buffer buffer :agent-name agent-name))))

(defun list-available-commands (&key buffer agent-name include-inactive)
  "Return registered command symbols visible in the package context."
  (loop :for (name . metadata) :in (command-registry-snapshot)
        :when (command-metadata-visible-p
               metadata
               :buffer buffer
               :agent-name agent-name
               :include-inactive include-inactive)
          :collect name))

(declaim (ftype (function (symbol) list) command-required-arguments))
(defun command-required-arguments (command-name)
  "Return the required non-buffer arguments for COMMAND-NAME."
  (let ((metadata (find-command-metadata command-name)))
    (when metadata
      (rest (command-metadata-lambda-list metadata)))))

(declaim (ftype (function (t) function) resolve-command-prompt-reader))
(defun resolve-command-prompt-reader (reader)
  "Resolve READER into a callable function for minibuffer prompts."
  (cond
    ((null reader) #'identity)
    ((functionp reader) reader)
    ((symbolp reader) (fdefinition reader))
    ((and (consp reader)
          (eq (first reader) 'function)
          (symbolp (second reader)))
     (fdefinition (second reader)))
    (t
     (error "Invalid command prompt reader designator: ~S" reader))))

;;; --------------------------------------------------------------------------
;;; defcommand Macro
;;; --------------------------------------------------------------------------

(defun register-command-metadata (name &key keys
                                            (prompts nil prompts-supplied-p)
                                            docstring)
  "Register existing function NAME as a user command."
  (when (and *current-rplaca-package*
             (not (package-resource-type-allowed-p :command)))
    (return-from register-command-metadata nil))
  (let* ((lambda-list
           (validate-command-lambda-list
            name (command-function-lambda-list name)))
         (prompts
           (normalize-command-prompts name lambda-list
                                      prompts prompts-supplied-p))
         (metadata (make-command-metadata
                    :name name
                    :docstring (or docstring
                                   (documentation name 'function)
                                   "")
                    :keybindings keys
                    :lambda-list lambda-list
                    :prompts prompts
                    :package *current-rplaca-package*)))
    (call-with-command-registry-lock
     (lambda ()
       (setf (gethash name *command-table*) metadata))
     *command-table*)
    metadata))

(defmacro defcommand (name &rest command-spec)
  "Register existing function NAME as an interactive command.

Example:
  (defun send-message (buffer)
    \"Send the current input.\"
    ...)
  (defcommand send-message :keys (#\\Return))"
  (unless (symbolp name)
    (error "defcommand requires a symbol name, got ~S." name))
  (unless (evenp (length command-spec))
    (error "defcommand ~A metadata has an odd plist: ~S."
           name command-spec))
  (loop :for tail :on command-spec :by #'cddr
        :for key := (first tail)
        :unless (member key '(:keys :prompts :docstring) :test #'eq)
          :do (error "defcommand ~A has unsupported key ~S." name key))
  `(register-command-metadata
    ',name
    ,@(loop :for (key value) :on command-spec :by #'cddr
            :append (list key `',value))))

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

(defvar *process-extended-docs* *extended-docs*
  "Process-global extended-doc table, distinct from dynamic test bindings.")

(defvar *extended-doc-registry-lock*
  (bt:make-lock "rplaca extended documentation registry")
  "Lock guarding bounded access to the process-global extended-doc table.")

(defun call-with-extended-doc-registry-lock
    (function &optional (table *extended-docs*))
  "Call FUNCTION under the extended-doc lock when TABLE is process-global."
  (if (eq table *process-extended-docs*)
      (bt:with-lock-held (*extended-doc-registry-lock*)
        (funcall function))
      (funcall function)))

(defun extended-doc-registry-snapshot (&optional (table *extended-docs*))
  "Return a stable alist snapshot of extended documentation TABLE."
  (call-with-extended-doc-registry-lock
   (lambda ()
     (let ((entries nil))
       (maphash (lambda (symbol doc)
                  (push (cons symbol doc) entries))
                table)
       entries))
   table))

(defun register-extended-doc (symbol doc)
  "Publish DOC for SYMBOL in the current extended-doc registry."
  (call-with-extended-doc-registry-lock
   (lambda ()
     (setf (gethash symbol *extended-docs*) doc))
   *extended-docs*)
  doc)

(defun remove-extended-docs-for-package (package-name)
  "Atomically remove extended docs owned by PACKAGE-NAME."
  (call-with-extended-doc-registry-lock
   (lambda ()
     (let ((removed nil))
       (maphash
        (lambda (symbol doc)
          (when (string= package-name (or (getf doc :package) ""))
            (push (cons symbol doc) removed)
            (remhash symbol *extended-docs*)))
        *extended-docs*)
       (nreverse removed)))
   *extended-docs*))

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
  `(let ((doc
           (when (or (null *current-rplaca-package*)
                     (package-resource-type-allowed-p :doc))
             (register-extended-doc
              ',name
              (append
               (list ,@(when category `(:category ,category))
                     ,@(when usage `(:usage ,usage))
                     ,@(when returns `(:returns ,returns))
                     ,@(when see-also `(:see-also ',see-also))
                     ,@(when side-effects `(:side-effects ,side-effects)))
               (when *current-rplaca-package*
                 (list :package *current-rplaca-package*)))))))
     doc))

(defun extended-doc (symbol &optional key)
  "Return the extended documentation for SYMBOL.
If KEY is provided (e.g. :usage, :returns), return just that property value.
Without KEY, returns the full plist or NIL if no extended doc exists."
  (let ((doc (call-with-extended-doc-registry-lock
              (lambda () (gethash symbol *extended-docs*))
              *extended-docs*)))
    (if key
        (getf doc key)
        doc)))
