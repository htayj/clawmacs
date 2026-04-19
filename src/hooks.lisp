(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Hooks
;;; --------------------------------------------------------------------------

(defstruct hook-metadata
  "Metadata for a user-customizable hook variable."
  (name      (error "name required") :type symbol :read-only t)
  (args      nil                     :type list   :read-only t)
  (docstring ""                      :type string :read-only t))

(defvar *hook-metadata-table* (make-hash-table :test #'eq)
  "Global table mapping hook variable symbols to hook metadata.")

(defun register-hook-metadata (name args docstring)
  "Register NAME as a hook variable that receives ARGS."
  (check-type name symbol)
  (let ((metadata (make-hook-metadata :name name
                                      :args args
                                      :docstring (or docstring ""))))
    (setf (gethash name *hook-metadata-table*) metadata)
    metadata))

(defmacro defhook (name args &optional docstring)
  "Define NAME as a hook variable and register its argument metadata."
  (unless (symbolp name)
    (error "defhook requires a symbol name, got ~S." name))
  (unless (listp args)
    (error "defhook ~A requires a list of argument names, got ~S."
           name args))
  `(progn
     (defvar ,name nil ,@(when docstring (list docstring)))
     (register-hook-metadata ',name ',args ,docstring)))

(defun find-hook-metadata (hook-var)
  "Return metadata for HOOK-VAR, or NIL when HOOK-VAR is not registered."
  (gethash hook-var *hook-metadata-table*))

(defun list-hooks ()
  "Return registered hook metadata sorted by hook variable name."
  (let ((hooks nil))
    (maphash (lambda (_name metadata)
               (declare (ignore _name))
               (push metadata hooks))
             *hook-metadata-table*)
    (sort hooks #'string< :key (lambda (metadata)
                                 (symbol-name
                                  (hook-metadata-name metadata))))))

(defun add-hook (hook-var function &key append)
  "Add FUNCTION to the hook list stored in HOOK-VAR and return FUNCTION.
HOOK-VAR should name a special variable containing a list of function
designators. When APPEND is non-nil, add FUNCTION at the end instead of the
front."
  (check-type hook-var symbol)
  (let ((hooks (symbol-value hook-var)))
    (unless (member function hooks :test #'eq)
      (setf (symbol-value hook-var)
            (if append
                (append hooks (list function))
                (cons function hooks)))))
  function)

(defun remove-hook (hook-var function)
  "Remove FUNCTION from the hook list stored in HOOK-VAR.
Returns FUNCTION."
  (check-type hook-var symbol)
  (setf (symbol-value hook-var)
        (remove function (symbol-value hook-var) :test #'eq))
  function)

(defun resolve-hook-function (hook)
  "Resolve HOOK to a function object."
  (etypecase hook
    (function hook)
    (symbol (symbol-function hook))))

(defun maybe-log-hook-error (hook hook-name condition)
  "Log hook CONDITION when file logging is available."
  (when (fboundp 'file-debug-log)
    (ignore-errors
      (funcall (symbol-function 'file-debug-log)
               "hook"
               "error running hook ~S from ~S: ~A"
               hook hook-name condition))))

(defun call-hook-safely (hook hook-name &rest args)
  "Invoke HOOK with ARGS, reporting and logging errors without aborting."
  (handler-case
      (apply (resolve-hook-function hook) args)
    (error (e)
      (format *error-output*
              "~&;; Warning: error running hook ~S from ~S:~%;; ~A~%"
              hook hook-name e)
      (maybe-log-hook-error hook hook-name e)
      nil)))

(defun run-hook-list (hook-name hooks &rest args)
  "Run HOOKS with ARGS, catching and reporting individual hook errors."
  (dolist (hook hooks)
    (apply #'call-hook-safely hook hook-name args))
  nil)

(defun run-hook-with-args (hook-var &rest args)
  "Run every function in HOOK-VAR with ARGS."
  (check-type hook-var symbol)
  (apply #'run-hook-list hook-var (symbol-value hook-var) args))

(defun run-hooks (&rest hook-vars)
  "Run each hook variable in HOOK-VARS with no arguments."
  (dolist (hook-var hook-vars)
    (run-hook-with-args hook-var))
  nil)

(defhook *startup-hook* ()
  "List of functions run after init.lisp loads and before McCLIM startup.")

(defhook *initial-buffer-hook* (buffer)
  "List of functions run with the initial buffer after it is created.")

(defhook *before-command-hook* (buffer command)
  "List of functions run before an interactive command is invoked.")

(defhook *after-command-hook* (buffer command result)
  "List of functions run after an interactive command returns normally.")

(defhook *before-tool-hook* (tool-name args)
  "List of functions run before an agent tool is executed.")

(defhook *after-tool-hook* (tool-name args result)
  "List of functions run after an agent tool returns normally.")

(defhook *before-send-message-hook* (buffer input-text)
  "List of functions run before a non-empty chat input is sent.")

(defhook *after-send-message-hook* (buffer input-text result)
  "List of functions run after a non-empty chat input send returns normally.")

(defhook *after-buffer-create-hook* (buffer)
  "List of functions run after a buffer object is created.")

(defhook *after-message-insert-hook* (buffer message)
  "List of functions run after a read-only message is inserted into a buffer.")

(defhook *after-buffer-display-change-hook* (buffer reason)
  "List of functions run after buffer state that affects display changes.")

(defhook *after-provider-response-hook* (buffer response provider model usage)
  "List of functions run after a provider response has been received.")

(defhook *package-enablement-changed-hook* (package scope buffer agent-name)
  "List of functions run after a package enablement scope changes.")

(defhook *after-session-save-hook* (buffer path)
  "List of functions run after a session snapshot is saved.")

(defhook *after-session-load-hook* (buffer session-name)
  "List of functions run after a session is loaded into a buffer.")

;;; --------------------------------------------------------------------------
;;; Advice
;;; --------------------------------------------------------------------------

(defstruct advice-entry
  "One advice registration for an advised function."
  (name       (error "name required") :type t       :read-only t)
  (where      (error "where required") :type keyword :read-only t)
  (designator (error "designator required") :type t :read-only t))

(defstruct advice-state
  "Internal state for a function whose fdefinition has been advised."
  (original-function (error "original-function required") :type function)
  (entries nil :type list))

(defvar *advice-table* (make-hash-table :test #'eq)
  "Global table mapping advised function symbols to advice state.")

(defun validate-advice-target (symbol)
  "Signal an error unless SYMBOL can be advised as an ordinary function."
  (check-type symbol symbol)
  (when (macro-function symbol)
    (error "Cannot advise macro ~A as an ordinary function." symbol))
  (unless (fboundp symbol)
    (error "Cannot advise undefined function ~A." symbol))
  symbol)

(defun validate-advice-where (where)
  "Signal an error unless WHERE is a supported advice position."
  (unless (member where '(:before :after :around) :test #'eq)
    (error "Unsupported advice position ~S. Expected :BEFORE, :AFTER, or :AROUND."
           where))
  where)

(defun resolve-advice-function (designator)
  "Resolve DESIGNATOR to a callable advice function."
  (cond
    ((functionp designator) designator)
    ((symbolp designator) (symbol-function designator))
    ((and (consp designator)
          (eq (first designator) 'function)
          (symbolp (second designator)))
     (symbol-function (second designator)))
    (t
     (error "Invalid advice function designator: ~S" designator))))

(defun default-advice-name (designator)
  "Return the default advice name for DESIGNATOR."
  (if (and (consp designator)
           (eq (first designator) 'function)
           (symbolp (second designator)))
      (second designator)
      designator))

(defun advice-entry-matches-p (entry name-or-function)
  "Return true when ENTRY matches NAME-OR-FUNCTION."
  (or (eql (advice-entry-name entry) name-or-function)
      (equal (advice-entry-name entry) name-or-function)
      (eql (advice-entry-designator entry) name-or-function)
      (equal (advice-entry-designator entry) name-or-function)))

(defun call-advice-entry (entry &rest args)
  "Call ENTRY's advice function with ARGS."
  (apply (resolve-advice-function (advice-entry-designator entry)) args))

(defun advice-entries-by-position (state where)
  "Return STATE advice entries registered for WHERE."
  (remove-if-not (lambda (entry)
                   (eq where (advice-entry-where entry)))
                 (advice-state-entries state)))

(defun call-advised-function (symbol state args)
  "Call SYMBOL's original function through STATE's advice chain."
  (declare (ignore symbol))
  (let* ((before-entries (advice-entries-by-position state :before))
         (after-entries (advice-entries-by-position state :after))
         (around-entries (advice-entries-by-position state :around))
         (next (lambda (&rest next-args)
                 (apply (advice-state-original-function state) next-args))))
    (dolist (entry (reverse around-entries))
      (let ((advice entry)
            (previous-next next))
        (setf next
              (lambda (&rest next-args)
                (apply #'call-advice-entry
                       advice previous-next next-args)))))
    (dolist (entry before-entries)
      (apply #'call-advice-entry entry args))
    (let ((values (multiple-value-list (apply next args))))
      (dolist (entry after-entries)
        (apply #'call-advice-entry entry (first values) args))
      (values-list values))))

(defun install-advised-function (symbol state)
  "Replace SYMBOL's fdefinition with the advice dispatcher for STATE."
  (setf (fdefinition symbol)
        (lambda (&rest args)
          (call-advised-function symbol state args)))
  state)

(defun ensure-advice-state (symbol)
  "Return SYMBOL's advice state, installing the dispatcher when needed."
  (or (gethash symbol *advice-table*)
      (let ((state (make-advice-state
                    :original-function (fdefinition symbol))))
        (setf (gethash symbol *advice-table*) state)
        (install-advised-function symbol state))))

(defun add-advice (symbol where advice &key name append)
  "Add ADVICE around SYMBOL at WHERE and return the registered advice entry."
  (validate-advice-target symbol)
  (validate-advice-where where)
  (resolve-advice-function advice)
  (let* ((state (ensure-advice-state symbol))
         (entry-name (or name (default-advice-name advice)))
         (entry (make-advice-entry :name entry-name
                                   :where where
                                   :designator advice))
         (entries (remove-if (lambda (existing)
                               (advice-entry-matches-p existing entry-name))
                             (advice-state-entries state))))
    (setf (advice-state-entries state)
          (if append
              (append entries (list entry))
              (cons entry entries)))
    entry))

(defun remove-advice (symbol name-or-function)
  "Remove advice from SYMBOL by advice name or function designator."
  (let ((state (gethash symbol *advice-table*)))
    (when state
      (let ((removed nil))
        (setf (advice-state-entries state)
              (remove-if (lambda (entry)
                           (when (advice-entry-matches-p entry
                                                          name-or-function)
                             (push entry removed)
                             t))
                         (advice-state-entries state)))
        (unless (advice-state-entries state)
          (setf (fdefinition symbol) (advice-state-original-function state))
          (remhash symbol *advice-table*))
        (nreverse removed)))))

(defun advice-member-p (symbol name-or-function)
  "Return the matching advice entry for SYMBOL, or NIL."
  (let ((state (gethash symbol *advice-table*)))
    (and state
         (find-if (lambda (entry)
                    (advice-entry-matches-p entry name-or-function))
                  (advice-state-entries state)))))

(defun list-advices (symbol)
  "Return SYMBOL's advice entries in invocation order."
  (let ((state (gethash symbol *advice-table*)))
    (and state (copy-list (advice-state-entries state)))))

(defun clear-advices (symbol)
  "Remove all advice from SYMBOL and restore its original function."
  (let ((state (gethash symbol *advice-table*)))
    (when state
      (setf (fdefinition symbol) (advice-state-original-function state))
      (remhash symbol *advice-table*)
      t)))

(defmacro defadvice (symbol name where lambda-list &body body)
  "Define NAME as an advice function and add it to SYMBOL at WHERE."
  (unless (symbolp symbol)
    (error "defadvice requires a target symbol, got ~S." symbol))
  (unless (symbolp name)
    (error "defadvice requires an advice function name, got ~S." name))
  `(progn
     (defun ,name ,lambda-list ,@body)
     (add-advice ',symbol ,where ',name :name ',name)))
