(in-package :clawmacs)

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

(defstruct agent-tool-metadata
  "Metadata for a Lisp function exposed as a provider-callable agent tool."
  (symbol      (error "symbol required")     :type symbol   :read-only t)
  (name        ""                            :type string   :read-only t)
  (description ""                            :type string   :read-only t)
  (args        nil                           :type list     :read-only t)
  (input-schema nil                          :type list     :read-only t)
  (permission  :agent-allowed                :type keyword  :read-only t)
  (call-style  :positional                   :type keyword  :read-only t)
  (approval-display-fn nil                   :type t        :read-only t)
  (command-p   nil                           :type boolean  :read-only t)
  (lambda-list nil                           :type list     :read-only t)
  (package     nil                           :type (or null string) :read-only t))

(defvar *agent-tool-metadata-table* (make-hash-table :test #'eq)
  "Global table mapping Lisp symbols to AGENT-TOOL-METADATA.")

(defvar *agent-tool-name-table* (make-hash-table :test #'equal)
  "Global table mapping provider tool names to owning Lisp symbols.")

(defun agent-tool-blank-string-p (value)
  "Return true when VALUE is NIL or contains only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun normalize-agent-tool-name (tool-name)
  "Normalize TOOL-NAME into the provider-facing tool name string."
  (let* ((raw (etypecase tool-name
                (string tool-name)
                (symbol (symbol-name tool-name))))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (when (agent-tool-blank-string-p trimmed)
      (error "Tool name must be non-empty"))
    (string-downcase
     (substitute #\_ #\- trimmed))))

(defun normalize-agent-tool-schema-type (value)
  "Normalize a Lisp type designator into the provider schema type string."
  (cond
    ((null value) "string")
    ((stringp value) value)
    ((symbolp value) (string-downcase (symbol-name value)))
    (t (format nil "~(~A~)" value))))

(defun normalize-agent-tool-arg-spec (tool-symbol arg-spec)
  "Normalize one explicit argument spec for TOOL-SYMBOL."
  (unless (and (consp arg-spec)
               (not (keywordp (first arg-spec))))
    (error "Tool ~A argument spec must start with an argument name, got ~S."
           tool-symbol arg-spec))
  (let ((arg-name (first arg-spec))
        (plist (rest arg-spec)))
    (unless (evenp (length plist))
      (error "Tool ~A argument spec for ~A has an odd plist: ~S."
             tool-symbol arg-name arg-spec))
    (loop :for tail :on plist :by #'cddr
          :for key := (first tail)
          :unless (member key '(:type :description :required :items) :test #'eq)
            :do (error "Tool ~A argument spec for ~A has unsupported key ~S."
                       tool-symbol arg-name key))
    (list :name (tool-key-name arg-name)
          :type (normalize-agent-tool-schema-type (getf plist :type))
          :items (getf plist :items)
          :description (or (getf plist :description) "")
          :required (if (member :required plist :test #'eq)
                        (not (null (getf plist :required)))
                        t))))

(defun agent-tool-arg-schema-property (arg)
  "Return the provider schema property for normalized ARG metadata."
  (let ((property `((:type . ,(getf arg :type)))))
    (when (getf arg :items)
      (setf property (append property `((:items . ,(getf arg :items))))))
    (if (agent-tool-blank-string-p (getf arg :description))
        property
        (append property
                `((:description . ,(getf arg :description)))))))

(defun agent-tool-input-schema (args)
  "Return the provider input schema for normalized ARGS metadata."
  (let ((required (loop :for arg :in args
                        :when (getf arg :required)
                          :collect (getf arg :name)))
        (properties (loop :for arg :in args
                          :collect (cons (getf arg :name)
                                         (agent-tool-arg-schema-property arg)))))
    `((:type . "object")
      (:properties . ,properties)
      (:required . ,(coerce required 'vector)))))

(defun normalize-agent-tool-call-style (tool-symbol value command-p)
  "Normalize the call style for TOOL-SYMBOL."
  (let ((style (or value (if command-p :command :positional))))
    (unless (member style '(:raw-args :positional :keyword :command) :test #'eq)
      (error "Tool ~A has unsupported call style ~S." tool-symbol style))
    (when (and command-p (not (eq style :command)))
      (error "Command tool ~A must use :COMMAND call style." tool-symbol))
    style))

(defun validate-agent-tool-args (tool-symbol tool-spec)
  "Return the explicit :ARGS value from TOOL-SPEC or signal a clear error."
  (unless (member :args tool-spec :test #'eq)
    (error "Tool ~A requires explicit :ARGS metadata." tool-symbol))
  (let ((args (getf tool-spec :args)))
    (unless (listp args)
      (error "Tool ~A :ARGS must be a list, got ~S." tool-symbol args))
    args))

(defun normalize-agent-tool-spec (symbol tool-spec
                                  &key command-p lambda-list docstring)
  "Normalize explicit tool metadata into AGENT-TOOL-METADATA."
  (unless (and (listp tool-spec) (or (null tool-spec) (keywordp (first tool-spec))))
    (error "Tool metadata for ~A must be a keyword plist, got ~S."
           symbol tool-spec))
  (unless (evenp (length tool-spec))
    (error "Tool metadata for ~A has an odd plist: ~S." symbol tool-spec))
  (loop :for tail :on tool-spec :by #'cddr
        :for key := (first tail)
        :unless (member key '(:name :description :permission :args
                              :call-style :approval-display-fn)
                        :test #'eq)
          :do (error "Tool metadata for ~A has unsupported key ~S."
                     symbol key))
  (let* ((raw-args (validate-agent-tool-args symbol tool-spec))
         (args (mapcar (lambda (arg)
                         (normalize-agent-tool-arg-spec symbol arg))
                       raw-args))
         (permission (or (getf tool-spec :permission) :agent-allowed))
         (description (or (getf tool-spec :description)
                          docstring
                          (documentation symbol 'function)
                          ""))
         (call-style
           (normalize-agent-tool-call-style
            symbol (getf tool-spec :call-style) command-p)))
    (unless (member permission '(:agent-allowed :agent-with-permission :user-only)
                    :test #'eq)
      (error "Tool ~A has unsupported permission ~S." symbol permission))
    (when (and command-p lambda-list
               (/= (length args) (length (rest lambda-list))))
      (error "Command tool ~A argument count ~D does not match command parameters ~D."
             symbol (length args) (length (rest lambda-list))))
    (make-agent-tool-metadata
     :symbol symbol
     :name (normalize-agent-tool-name (or (getf tool-spec :name) symbol))
     :description description
     :args args
     :input-schema (agent-tool-input-schema args)
     :permission permission
     :call-style call-style
     :approval-display-fn (getf tool-spec :approval-display-fn)
     :command-p (not (null command-p))
     :lambda-list lambda-list
     :package *current-clawmacs-package*)))

(defun call-agent-tool-provider-bridge (metadata)
  "Register METADATA with the provider tool table when that bridge is loaded."
  (let ((bridge (and (fboundp 'register-agent-tool-provider-definition)
                     (symbol-function 'register-agent-tool-provider-definition))))
    (when bridge
      (funcall bridge metadata))))

(defun remove-agent-tool-provider-entry (name)
  "Remove NAME from the provider tool table when the table is loaded."
  (when (and (boundp '*tool-table*)
             (hash-table-p (symbol-value '*tool-table*)))
    (remhash name (symbol-value '*tool-table*))))

(defun unregister-agent-tool-metadata (symbol)
  "Remove SYMBOL's agent tool metadata and provider entry, when present."
  (let ((metadata (gethash symbol *agent-tool-metadata-table*)))
    (when metadata
      (remhash (agent-tool-metadata-name metadata) *agent-tool-name-table*)
      (remhash symbol *agent-tool-metadata-table*)
      (remove-agent-tool-provider-entry (agent-tool-metadata-name metadata)))
    metadata))

(defun register-agent-tool-metadata (symbol tool-spec
                                     &key command-p lambda-list docstring)
  "Register SYMBOL as an agent tool from explicit TOOL-SPEC metadata."
  (if (null tool-spec)
      (unregister-agent-tool-metadata symbol)
      (let* ((metadata (normalize-agent-tool-spec
                        symbol tool-spec
                        :command-p command-p
                        :lambda-list lambda-list
                        :docstring docstring))
             (name (agent-tool-metadata-name metadata))
             (owner (gethash name *agent-tool-name-table*)))
        (when (and owner (not (eq owner symbol)))
          (error "Tool name ~S is already registered for ~A, cannot reuse it for ~A."
                 name owner symbol))
        (let ((previous (gethash symbol *agent-tool-metadata-table*)))
          (when (and previous
                     (not (string= name (agent-tool-metadata-name previous))))
            (remhash (agent-tool-metadata-name previous) *agent-tool-name-table*)
            (remove-agent-tool-provider-entry
             (agent-tool-metadata-name previous))))
        (setf (gethash symbol *agent-tool-metadata-table*) metadata
              (gethash name *agent-tool-name-table*) symbol)
        (call-agent-tool-provider-bridge metadata)
        metadata)))

(defun find-agent-tool-metadata (symbol-or-name)
  "Return agent tool metadata by Lisp symbol or provider tool name."
  (cond
    ((symbolp symbol-or-name)
     (or (gethash symbol-or-name *agent-tool-metadata-table*)
         (let ((owner (gethash (normalize-agent-tool-name symbol-or-name)
                               *agent-tool-name-table*)))
           (and owner (gethash owner *agent-tool-metadata-table*)))))
    ((stringp symbol-or-name)
     (let ((owner (gethash (normalize-agent-tool-name symbol-or-name)
                           *agent-tool-name-table*)))
       (and owner (gethash owner *agent-tool-metadata-table*))))
    (t nil)))

(defun list-agent-tool-metadata ()
  "Return registered agent tool metadata sorted by provider tool name."
  (let ((metadata nil))
    (maphash (lambda (_symbol value)
               (declare (ignore _symbol))
               (push value metadata))
             *agent-tool-metadata-table*)
    (sort metadata #'string< :key #'agent-tool-metadata-name)))

(defun ensure-agent-tool-function-defined (symbol)
  "Signal an error unless SYMBOL names a callable tool function."
  (unless (fboundp symbol)
    (error "deftool ~A requires a separately defined function." symbol))
  symbol)

(defmacro deftool (symbol &rest tool-spec)
  "Register an existing function SYMBOL as a provider-callable agent tool.

When SYMBOL names a registered command, command call style is inferred: provider
arguments map to the command's non-BUFFER parameters, and the current tool
buffer is supplied automatically during execution."
  (unless (symbolp symbol)
    (error "deftool requires a symbol name, got ~S." symbol))
  (unless tool-spec
    (error "deftool ~A requires explicit metadata." symbol))
  (unless (evenp (length tool-spec))
    (error "deftool ~A metadata has an odd plist: ~S." symbol tool-spec))
  (let ((metadata-var (gensym "COMMAND-METADATA")))
    `(let ((,metadata-var (gethash ',symbol *command-table*)))
       (ensure-agent-tool-function-defined ',symbol)
       (register-agent-tool-metadata
        ',symbol ',tool-spec
        :command-p (not (null ,metadata-var))
        :lambda-list (and ,metadata-var
                          (command-metadata-lambda-list ,metadata-var))
        :docstring (or (and ,metadata-var
                            (command-metadata-docstring ,metadata-var))
                       (documentation ',symbol 'function))))))

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
  (let ((result nil))
    (maphash (lambda (name metadata)
               (when (command-metadata-visible-p
                      metadata
                      :buffer buffer
                      :agent-name agent-name
                      :include-inactive include-inactive)
                 (push name result)))
             *command-table*)
    result))

(declaim (ftype (function (symbol) list) command-required-arguments))
(defun command-required-arguments (command-name)
  "Return the required non-buffer arguments for COMMAND-NAME."
  (let ((metadata (gethash command-name *command-table*)))
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
                    :package *current-clawmacs-package*)))
    (setf (gethash name *command-table*) metadata)
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
  `(let ((doc (setf (gethash ',name *extended-docs*)
                    (append
                     (list ,@(when category `(:category ,category))
                           ,@(when usage `(:usage ,usage))
                           ,@(when returns `(:returns ,returns))
                           ,@(when see-also `(:see-also ',see-also))
                           ,@(when side-effects `(:side-effects ,side-effects)))
                     (when *current-clawmacs-package*
                       (list :package *current-clawmacs-package*))))))
     doc))

(defun extended-doc (symbol &optional key)
  "Return the extended documentation for SYMBOL.
If KEY is provided (e.g. :usage, :returns), return just that property value.
Without KEY, returns the full plist or NIL if no extended doc exists."
  (let ((doc (gethash symbol *extended-docs*)))
    (if key
        (getf doc key)
        doc)))
