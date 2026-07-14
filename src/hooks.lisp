(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Hooks
;;; --------------------------------------------------------------------------

(defstruct hook-metadata
  "Metadata for a user-customizable hook variable."
  (name      (error "name required") :type symbol :read-only t)
  (args      nil                     :type list   :read-only t)
  (docstring ""                      :type string :read-only t)
  (package   nil :type (or null string) :read-only t))

(defvar *hook-metadata-table* (make-hash-table :test #'eq)
  "Global table mapping hook variable symbols to hook metadata.")

(defvar *process-hook-metadata-table* *hook-metadata-table*
  "Process-global hook metadata table, distinct from dynamic test bindings.")

(defvar *hook-registry-lock*
  (bt:make-lock "clawmacs hook registry")
  "Lock guarding process-global hook metadata, members, and ownership records.")

(defun call-with-hook-registry-lock
    (function &optional (table *hook-metadata-table*))
  "Call FUNCTION under the hook lock when TABLE is process-global.

FUNCTION must perform only bounded metadata, ownership, or hook-list access.
Hook resolution and invocation must happen after the lock has been released."
  (if (eq table *process-hook-metadata-table*)
      (bt:with-lock-held (*hook-registry-lock*)
        (funcall function))
      (funcall function)))

(defun hook-metadata-registry-snapshot (&optional (table *hook-metadata-table*))
  "Return a stable alist snapshot of hook metadata TABLE."
  (call-with-hook-registry-lock
   (lambda ()
     (let ((entries nil))
       (maphash (lambda (name metadata)
                  (push (cons name metadata) entries))
                table)
       entries))
   table))

(defun hook-member-snapshot (hook-var)
  "Return a stable copy of the hook member list named by HOOK-VAR."
  (call-with-hook-registry-lock
   (lambda () (copy-list (symbol-value hook-var)))
   *hook-metadata-table*))

(defstruct package-hook-registration
  "One hook function installed by a package entrypoint."
  hook-var
  function
  package)

(defvar *package-hook-registrations* nil
  "Ownership records for package functions added to existing hook variables.")

(defun register-hook-metadata (name args docstring)
  "Register NAME as a hook variable that receives ARGS."
  (check-type name symbol)
  (when (and *current-clawmacs-package*
             (not (package-resource-type-allowed-p :hook)))
    (return-from register-hook-metadata nil))
  (let ((metadata (make-hook-metadata
                   :name name
                   :args args
                   :docstring (or docstring "")
                   :package (and *current-clawmacs-package*
                                 (manifest-package-name
                                  *current-clawmacs-package*)))))
    (call-with-hook-registry-lock
     (lambda ()
       (setf (gethash name *hook-metadata-table*) metadata))
     *hook-metadata-table*)
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
  (call-with-hook-registry-lock
   (lambda () (gethash hook-var *hook-metadata-table*))
   *hook-metadata-table*))

(defun list-hooks ()
  "Return registered hook metadata sorted by hook variable name."
  (sort (mapcar #'cdr (hook-metadata-registry-snapshot))
        #'string< :key (lambda (metadata)
                         (symbol-name
                          (hook-metadata-name metadata)))))

(defun add-hook (hook-var function &key append)
  "Add FUNCTION to the hook list stored in HOOK-VAR and return FUNCTION.
HOOK-VAR should name a special variable containing a list of function
designators. When APPEND is non-nil, add FUNCTION at the end instead of the
front."
  (check-type hook-var symbol)
  (when (and *current-clawmacs-package*
             (not (package-resource-type-allowed-p :hook)))
    (return-from add-hook function))
  (let ((package (and *current-clawmacs-package*
                      (manifest-package-name *current-clawmacs-package*))))
    (call-with-hook-registry-lock
     (lambda ()
       (let ((hooks (symbol-value hook-var)))
         (unless (member function hooks :test #'eq)
           (setf (symbol-value hook-var)
                 (if append
                     (append hooks (list function))
                     (cons function hooks)))
           (when package
             (push (make-package-hook-registration
                    :hook-var hook-var
                    :function function
                    :package package)
                   *package-hook-registrations*)))))
     *hook-metadata-table*))
  function)

(defun remove-hook (hook-var function)
  "Remove FUNCTION from the hook list stored in HOOK-VAR.
Returns FUNCTION."
  (check-type hook-var symbol)
  (call-with-hook-registry-lock
   (lambda ()
     (setf (symbol-value hook-var)
           (remove function (symbol-value hook-var) :test #'eq)
           *package-hook-registrations*
           (remove-if
            (lambda (registration)
              (and (eq hook-var
                       (package-hook-registration-hook-var registration))
                   (eq function
                       (package-hook-registration-function registration))))
            *package-hook-registrations*)))
   *hook-metadata-table*)
  function)

(defun remove-package-hook-registrations (package-name)
  "Remove hook metadata and hook members owned by PACKAGE-NAME."
  (let ((name (manifest-package-name package-name)))
    (call-with-hook-registry-lock
     (lambda ()
       (let ((removed nil)
             (metadata-names nil))
         (dolist (registration *package-hook-registrations*)
           (when (string= name
                          (or (package-hook-registration-package registration)
                              ""))
             (let ((hook-var
                     (package-hook-registration-hook-var registration))
                   (function
                     (package-hook-registration-function registration)))
               (when (boundp hook-var)
                 (setf (symbol-value hook-var)
                       (remove function (symbol-value hook-var) :test #'eq)))
               (push registration removed))))
         (setf *package-hook-registrations*
               (remove name *package-hook-registrations*
                       :key #'package-hook-registration-package
                       :test #'string=))
         (maphash
          (lambda (hook-var metadata)
            (when (string= name (or (hook-metadata-package metadata) ""))
              (push hook-var metadata-names)))
          *hook-metadata-table*)
         (dolist (hook-var metadata-names)
           (remhash hook-var *hook-metadata-table*))
         (nreverse removed)))
     *hook-metadata-table*)))

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
  (apply #'run-hook-list hook-var (hook-member-snapshot hook-var) args))

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

(defhook *session-share-hook* (buffer export-info)
  "List of functions run by the built-in session share hook handler.
Each function receives BUFFER and EXPORT-INFO, and may return a share result.")

;;; --------------------------------------------------------------------------
;;; Advice
;;; --------------------------------------------------------------------------

(defstruct advice-entry
  "One advice registration for an advised function."
  (name       (error "name required") :type t       :read-only t)
  (where      (error "where required") :type keyword :read-only t)
  (designator (error "designator required") :type t :read-only t)
  (package    nil :type (or null string) :read-only t))

(defstruct advice-state
  "Internal state for a function whose fdefinition has been advised."
  (original-function (error "original-function required") :type function)
  (installed-function nil :type (or null function))
  (entries nil :type list))

(defvar *advice-table* (make-hash-table :test #'eq)
  "Global table mapping advised function symbols to advice state.")

(defvar *process-advice-table* *advice-table*
  "Process-global advice table, distinct from dynamic test bindings.")

(defvar *advice-registry-lock*
  (bt:make-lock "clawmacs advice registry")
  "Lock guarding process-global advice table and advice-state mutation.")

(defun call-with-advice-registry-lock
    (function &optional (table *advice-table*))
  "Call FUNCTION under the advice lock when TABLE is process-global.

FUNCTION must only access advice bookkeeping or install/restore an fdefinition;
advice resolution and invocation must occur after the lock has been released."
  (if (eq table *process-advice-table*)
      (bt:with-lock-held (*advice-registry-lock*)
        (funcall function))
      (funcall function)))

(defun copy-advice-state-snapshot (state)
  "Return a stable copy of mutable advice STATE."
  (make-advice-state
   :original-function (advice-state-original-function state)
   :installed-function (advice-state-installed-function state)
   :entries (copy-list (advice-state-entries state))))

(defun advice-registry-snapshot (&optional (table *advice-table*))
  "Return a stable alist snapshot of advice TABLE and its mutable states."
  (call-with-advice-registry-lock
   (lambda ()
     (let ((entries nil))
       (maphash (lambda (symbol state)
                  (push (cons symbol (copy-advice-state-snapshot state))
                        entries))
                table)
       entries))
   table))

(defun advice-state-call-snapshot (state)
  "Return STATE's original function and a stable entry list for invocation."
  (call-with-advice-registry-lock
   (lambda ()
     (values (advice-state-original-function state)
             (copy-list (advice-state-entries state))))
   *advice-table*))

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

(defun advice-entries-by-position (entries where)
  "Return advice ENTRIES registered for WHERE."
  (remove-if-not (lambda (entry)
                   (eq where (advice-entry-where entry)))
                 entries))

(defun call-advised-function (symbol state args)
  "Call SYMBOL's original function through STATE's advice chain."
  (declare (ignore symbol))
  (multiple-value-bind (original-function entries)
      (advice-state-call-snapshot state)
    (let* ((before-entries (advice-entries-by-position entries :before))
           (after-entries (advice-entries-by-position entries :after))
           (around-entries (advice-entries-by-position entries :around))
           (next (lambda (&rest next-args)
                   (apply original-function next-args))))
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
        (values-list values)))))

(defun install-advised-function (symbol state)
  "Replace SYMBOL's fdefinition with the advice dispatcher for STATE.

The caller must hold the applicable advice registry lock."
  (let ((dispatcher
          (lambda (&rest args)
            (call-advised-function symbol state args))))
    (setf (advice-state-installed-function state) dispatcher
          (fdefinition symbol) dispatcher))
  state)

(defun reconcile-advice-state (symbol state)
  "Adopt a redefined SYMBOL as STATE's new original and reinstall advice.

ASDF and package reloads may replace a target fdefinition without first
clearing Clawmacs advice bookkeeping.  Comparing the exact installed dispatcher
prevents later cleanup from restoring a stale pre-reload function.  The caller
must hold the applicable advice registry lock."
  (let ((current (fdefinition symbol)))
    (unless (eq current (advice-state-installed-function state))
      (setf (advice-state-original-function state) current)
      (install-advised-function symbol state)))
  state)

(defun ensure-advice-state-locked (symbol)
  "Return SYMBOL's advice state while the applicable advice lock is held."
  (let ((state (gethash symbol *advice-table*)))
    (if state
        (reconcile-advice-state symbol state)
        (let ((new-state (make-advice-state
                          :original-function (fdefinition symbol))))
          (setf (gethash symbol *advice-table*) new-state)
          (install-advised-function symbol new-state)))))

(defun add-advice (symbol where advice &key name append)
  "Add ADVICE around SYMBOL at WHERE and return the registered advice entry."
  (validate-advice-target symbol)
  (validate-advice-where where)
  (when (and *current-clawmacs-package*
             (not (package-resource-type-allowed-p :advice)))
    (return-from add-advice nil))
  (resolve-advice-function advice)
  (let* ((entry-name (or name (default-advice-name advice)))
         (entry (make-advice-entry
                 :name entry-name
                 :where where
                 :designator advice
                 :package (and *current-clawmacs-package*
                               (manifest-package-name
                                *current-clawmacs-package*)))))
    (call-with-advice-registry-lock
     (lambda ()
       (let* ((state (ensure-advice-state-locked symbol))
              (entries
                (remove-if
                 (lambda (existing)
                   (advice-entry-matches-p existing entry-name))
                 (advice-state-entries state))))
         (setf (advice-state-entries state)
               (if append
                   (append entries (list entry))
                   (cons entry entries)))))
     *advice-table*)
    entry))

(defun remove-advice (symbol name-or-function)
  "Remove advice from SYMBOL by advice name or function designator."
  (call-with-advice-registry-lock
   (lambda ()
     (let ((state (gethash symbol *advice-table*)))
       (when state
         (let ((removed nil))
           (setf (advice-state-entries state)
                 (remove-if
                  (lambda (entry)
                    (when (advice-entry-matches-p entry name-or-function)
                      (push entry removed)
                      t))
                  (advice-state-entries state)))
           (cond
             ((advice-state-entries state)
              (reconcile-advice-state symbol state))
             (t
              (when (eq (fdefinition symbol)
                        (advice-state-installed-function state))
                (setf (fdefinition symbol)
                      (advice-state-original-function state)))
              (remhash symbol *advice-table*)))
           (nreverse removed)))))
   *advice-table*))

(defun advice-member-p (symbol name-or-function)
  "Return the matching advice entry for SYMBOL, or NIL."
  (call-with-advice-registry-lock
   (lambda ()
     (let ((state (gethash symbol *advice-table*)))
       (and state
            (find-if (lambda (entry)
                       (advice-entry-matches-p entry name-or-function))
                     (advice-state-entries state)))))
   *advice-table*))

(defun list-advices (symbol)
  "Return SYMBOL's advice entries in invocation order."
  (call-with-advice-registry-lock
   (lambda ()
     (let ((state (gethash symbol *advice-table*)))
       (and state (copy-list (advice-state-entries state)))))
   *advice-table*))

(defun clear-advices (symbol)
  "Remove all advice from SYMBOL and restore its original function."
  (call-with-advice-registry-lock
   (lambda ()
     (let ((state (gethash symbol *advice-table*)))
       (when state
         (when (eq (fdefinition symbol)
                   (advice-state-installed-function state))
           (setf (fdefinition symbol) (advice-state-original-function state)))
         (remhash symbol *advice-table*)
         t)))
   *advice-table*))

(defun remove-package-advices (package-name)
  "Remove every advice entry owned by PACKAGE-NAME and restore empty targets."
  (let ((name (manifest-package-name package-name)))
    (call-with-advice-registry-lock
     (lambda ()
       (let ((removed nil)
             (targets nil))
         (maphash (lambda (symbol _state)
                    (declare (ignore _state))
                    (push symbol targets))
                  *advice-table*)
         (dolist (symbol targets)
           (let ((state (gethash symbol *advice-table*)))
             (when state
               (let ((owned
                       (remove-if-not
                        (lambda (entry)
                          (string= name
                                   (or (advice-entry-package entry) "")))
                        (advice-state-entries state))))
                 (when owned
                   (setf (advice-state-entries state)
                         (remove name (advice-state-entries state)
                                 :key #'advice-entry-package
                                 :test #'string=))
                   (setf removed (nconc removed (copy-list owned)))
                   (cond
                     ((advice-state-entries state)
                      (reconcile-advice-state symbol state))
                     (t
                      (when (eq (fdefinition symbol)
                                (advice-state-installed-function state))
                        (setf (fdefinition symbol)
                              (advice-state-original-function state)))
                      (remhash symbol *advice-table*))))))))
         removed))
     *advice-table*)))

(defmacro defadvice (symbol name where lambda-list &body body)
  "Define NAME as an advice function and add it to SYMBOL at WHERE."
  (unless (symbolp symbol)
    (error "defadvice requires a target symbol, got ~S." symbol))
  (unless (symbolp name)
    (error "defadvice requires an advice function name, got ~S." name))
  `(progn
     (defun ,name ,lambda-list ,@body)
     (add-advice ',symbol ,where ',name :name ',name)))
