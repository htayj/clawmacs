(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Tool Registry
;;; --------------------------------------------------------------------------

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

(defun normalize-tool-permission (value &key allow-inherit-p (context "Tool permission"))
  "Normalize VALUE into a supported tool permission keyword.
When ALLOW-INHERIT-P is true, NIL and \"inherit\" map to NIL."
  (let ((normalized
          (cond
            ((null value) (and allow-inherit-p nil))
            ((keywordp value) value)
            ((symbolp value)
             (intern (string-upcase (symbol-name value)) :keyword))
            ((stringp value)
             (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          value))
                    (token (substitute #\- #\_ trimmed)))
               (when (agent-tool-blank-string-p token)
                 (error "~A must not be blank." context))
               (intern (string-upcase token) :keyword)))
            (t
             (error "~A must be a keyword, symbol, string, or NIL: ~S"
                    context
                    value)))))
    (cond
      ((and allow-inherit-p (or (null normalized) (eq normalized :inherit)))
       nil)
      ((member normalized '(:agent-allowed :agent-with-permission :user-only)
               :test #'eq)
       normalized)
      (t
       (error "~A has unsupported value ~S." context value)))))

(defun tool-permission-json-value (permission)
  "Return PERMISSION as the persisted JSON string."
  (if permission
      (string-downcase (symbol-name permission))
      "inherit"))

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
         (permission (normalize-tool-permission
                      (or (getf tool-spec :permission) :agent-allowed)
                      :context (format nil "Tool ~A permission" symbol)))
         (description (or (getf tool-spec :description)
                          docstring
                          (documentation symbol 'function)
                          ""))
         (call-style
           (normalize-agent-tool-call-style
            symbol (getf tool-spec :call-style) command-p)))
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

(defstruct tool-definition
  "Definition of a tool that can be called by an agent."
  (name        ""              :type string   :read-only t)
  (description ""              :type string   :read-only t)
  (input-schema nil            :type list     :read-only t)
  (permission  :agent-allowed  :type keyword  :read-only t)
  (execute-fn  nil             :type (or null function))
  ;; Optional function (args) -> string-or-nil for extra approval context
  (approval-display-fn nil     :type (or null function))
  (package nil                 :type (or null string) :read-only t))

(defstruct (subagent-tool
            (:constructor %make-subagent-tool
                (&key name description input-schema permission execute-fn
                      approval-display-fn)))
  "Temporary tool definition passed to a subagent run."
  (name        ""              :type string   :read-only t)
  (description ""              :type string   :read-only t)
  (input-schema nil            :type list     :read-only t)
  (permission  :agent-allowed  :type keyword  :read-only t)
  (execute-fn  nil             :type (or null function))
  (approval-display-fn nil     :type (or null function)))

(defvar *tool-table* (make-hash-table :test #'equal)
  "Global table mapping tool name strings to tool-definition structs.")

(defvar *active-tool-names* nil
  "Dynamic tool allowlist for the current agent run.
NIL means all tools visible to the caller are available.")

(defvar *temporary-tool-table* nil
  "Dynamic table mapping tool names to temporary tool definitions.
Temporary tools override same-named global tools for the dynamic extent.")

(defvar *current-tool-buffer* nil
  "Dynamic buffer passed to command-style provider tools during execution.")

(defvar *approval-policy-path*
  (merge-pathnames #P".config/clawmacs/guard.json" (user-homedir-pathname))
  "Path to the persisted user-scoped tool approval policy.")

(defvar *approval-policy-registry* nil
  "Memoized user approval policy registry.")

(defvar *http-fetch-max-chars* 50000
  "Default maximum characters returned by http_fetch.")

(defvar *http-connection-timeout* 15
  "Connection timeout in seconds for HTTP requests.")

(defvar *http-user-agent* "Clawmacs/0.1"
  "User-Agent header for HTTP requests.")

(defvar *shell-exec-default-timeout* 30
  "Default timeout in seconds for shell_exec.")

(defvar *shell-exec-poll-interval* 0.05
  "Polling interval in seconds while waiting for shell_exec to finish.")

(defparameter *built-in-tool-names*
  '("lisp_eval")
  "Names reserved for core Clawmacs provider tools.
INIT-TOOLS removes these entries before re-registering tagged tools, so
user-added tools stored in *tool-table* are left intact.")

(defun make-approval-policy-registry ()
  "Create an empty in-memory approval policy registry."
  (list :default nil
        :tools (make-hash-table :test #'equal)))

(defun approval-policy-tools (registry)
  "Return the per-tool permission table stored in REGISTRY."
  (getf registry :tools))

(defun approval-policy-default-permission ()
  "Return the current default approval override permission, or NIL."
  (ensure-approval-policy-loaded)
  (getf *approval-policy-registry* :default))

(defun approval-policy-tool-permission (name)
  "Return the approval override permission for tool NAME, or NIL."
  (ensure-approval-policy-loaded)
  (gethash (normalize-tool-name name)
           (approval-policy-tools *approval-policy-registry*)))

(defun set-approval-policy-default-permission (permission)
  "Set the default approval override PERMISSION in memory."
  (ensure-approval-policy-loaded)
  (setf (getf *approval-policy-registry* :default)
        (normalize-tool-permission permission
                                   :allow-inherit-p t
                                   :context "Guard policy default"))
  (approval-policy-default-permission))

(defun set-approval-policy-tool-permission (name permission)
  "Set or clear the approval override PERMISSION for tool NAME in memory."
  (ensure-approval-policy-loaded)
  (let* ((tools (approval-policy-tools *approval-policy-registry*))
         (normalized-name (normalize-tool-name name))
         (normalized-permission
           (normalize-tool-permission permission
                                      :allow-inherit-p t
                                      :context
                                      (format nil "Guard policy override for ~A"
                                              normalized-name))))
    (if normalized-permission
        (setf (gethash normalized-name tools) normalized-permission)
        (remhash normalized-name tools))
    normalized-permission))

(defun load-approval-policy ()
  "Load and memoize the persisted user approval policy."
  (let ((registry (make-approval-policy-registry)))
    (when (probe-file *approval-policy-path*)
      (handler-case
          (let* ((json (uiop:read-file-string *approval-policy-path*))
                 (data (cl-json:decode-json-from-string json))
                 (default (lookup-json-value data "default"))
                 (tool-overrides (lookup-json-value data "tools"))
                 (tools (approval-policy-tools registry)))
            (setf (getf registry :default)
                  (normalize-tool-permission default
                                             :allow-inherit-p t
                                             :context "Guard policy default"))
            (dolist (entry tool-overrides)
              (let* ((name (json-key-string (car entry)))
                     (permission (normalize-tool-permission
                                  (cdr entry)
                                  :allow-inherit-p t
                                  :context
                                  (format nil "Guard policy override for ~A"
                                          name))))
                (when permission
                  (setf (gethash (normalize-tool-name name) tools)
                        permission)))))
        (error (condition)
          (warn "Failed to load guard policy from ~A: ~A"
                *approval-policy-path*
                condition))))
    (setf *approval-policy-registry* registry)))

(defun ensure-approval-policy-loaded ()
  "Load the approval policy when it has not been memoized yet."
  (unless *approval-policy-registry*
    (load-approval-policy))
  *approval-policy-registry*)

(defun save-approval-policy ()
  "Persist the current approval policy registry to disk."
  (ensure-approval-policy-loaded)
  (let ((payload
          `((:version . 1)
            (:default . ,(tool-permission-json-value
                          (approval-policy-default-permission)))
            (:tools
             . ,(let ((entries nil))
                  (maphash
                   (lambda (name permission)
                     (push `(,name . ,(tool-permission-json-value permission))
                           entries))
                   (approval-policy-tools *approval-policy-registry*))
                  (nreverse entries))))))
    (ensure-directories-exist *approval-policy-path*)
    (with-open-file (stream *approval-policy-path*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (api-json-encode payload) stream))
    *approval-policy-path*))

(defun effective-tool-permission (definition-or-name)
  "Return the approval-effective permission for DEFINITION-OR-NAME."
  (let* ((definition (if (tool-definition-p definition-or-name)
                         definition-or-name
                         (effective-tool-definition definition-or-name)))
         (static-permission (and definition
                                 (tool-definition-permission definition))))
    (if definition
        (or (approval-policy-tool-permission (tool-definition-name definition))
            (approval-policy-default-permission)
            static-permission)
        nil)))

(defun make-subagent-tool (&key name description input-schema
                             ((:schema schema-arg) nil)
                             (permission :agent-allowed)
                             execute-fn
                             ((:function function-arg) nil)
                             approval-display-fn)
  "Build a temporary tool definition suitable for RUN-SUBAGENT.
SCHEMA is accepted as an alias for INPUT-SCHEMA.  EXECUTE-FN or FUNCTION must
be a function accepting one argument: the decoded tool input alist."
  (let ((fn (or execute-fn function-arg))
        (effective-schema (or input-schema schema-arg)))
    (unless name
      (error "Temporary subagent tools require :name"))
    (unless description
      (error "Temporary subagent tools require :description"))
    (unless effective-schema
      (error "Temporary subagent tools require :input-schema or :schema"))
    (unless fn
      (error "Temporary subagent tools require :execute-fn or :function"))
    (%make-subagent-tool :name (normalize-tool-name name)
                         :description description
                         :input-schema effective-schema
                         :permission (normalize-tool-permission
                                      permission
                                      :context
                                      (format nil "Temporary tool ~A permission"
                                              name))
                         :execute-fn fn
                         :approval-display-fn approval-display-fn)))

(defun subagent-tool->tool-definition (tool)
  "Convert temporary TOOL into a TOOL-DEFINITION."
  (make-tool-definition :name (subagent-tool-name tool)
                        :description (subagent-tool-description tool)
                        :input-schema (subagent-tool-input-schema tool)
                        :permission (subagent-tool-permission tool)
                        :execute-fn (subagent-tool-execute-fn tool)
                        :approval-display-fn
                        (subagent-tool-approval-display-fn tool)))

(defun plist-subagent-tool-p (tool)
  "Return true when TOOL appears to be a plist temporary tool definition."
  (and (listp tool)
       (keywordp (first tool))
       (or (getf tool :name)
           (getf tool :description)
           (getf tool :input-schema)
           (getf tool :schema)
           (getf tool :execute-fn)
           (getf tool :function))))

(defun normalize-subagent-tool (tool)
  "Normalize TOOL into a TOOL-DEFINITION.
TOOL may be a SUBAGENT-TOOL, a TOOL-DEFINITION, or a plist accepted by
MAKE-SUBAGENT-TOOL."
  (cond
    ((tool-definition-p tool)
     (make-tool-definition :name (normalize-tool-name
                                  (tool-definition-name tool))
                           :description (tool-definition-description tool)
                           :input-schema (tool-definition-input-schema tool)
                           :permission (tool-definition-permission tool)
                           :execute-fn (tool-definition-execute-fn tool)
                           :approval-display-fn
                           (tool-definition-approval-display-fn tool)
                           :package (tool-definition-package tool)))
    ((subagent-tool-p tool)
     (subagent-tool->tool-definition tool))
    ((plist-subagent-tool-p tool)
     (subagent-tool->tool-definition
      (apply #'make-subagent-tool tool)))
    (t
     (error "Unsupported temporary subagent tool definition: ~S" tool))))

(defun normalize-subagent-tools (tools)
  "Return a list of normalized temporary tool definitions."
  (mapcar #'normalize-subagent-tool tools))

(defun make-temporary-tool-table (tools)
  "Build a temporary tool table from normalized or plist TOOLS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (definition (normalize-subagent-tools tools) table)
      (setf (gethash (tool-definition-name definition) table)
            definition))))

(defun effective-tool-definition (name)
  "Return the effective tool definition for NAME.
Temporary dynamic tools override process-global registered tools."
  (let ((normalized-name (normalize-tool-name name)))
    (or (and *temporary-tool-table*
             (gethash normalized-name *temporary-tool-table*))
        (gethash normalized-name *tool-table*))))

(defun map-effective-tool-definitions (function)
  "Call FUNCTION with every effective tool definition.
Temporary tools are visited first and same-named global tools are suppressed."
  (let ((seen (make-hash-table :test #'equal)))
    (when *temporary-tool-table*
      (maphash (lambda (name definition)
                 (setf (gethash name seen) t)
                 (funcall function name definition))
               *temporary-tool-table*))
    (maphash (lambda (name definition)
               (unless (gethash name seen)
                 (funcall function name definition)))
             *tool-table*)))

(defun register-tool (name description schema permission execute-fn
                      &key approval-display-fn package)
  "Register a tool in *tool-table*.
APPROVAL-DISPLAY-FN, if provided, is called with (args) during permission
approval to generate extra display context (e.g., file diffs). PACKAGE records
the owning Clawmacs package for package-scoped tools."
  (let ((normalized-name (normalize-tool-name name)))
    (setf (gethash normalized-name *tool-table*)
          (make-tool-definition :name normalized-name
                                :description description
                                :input-schema schema
                                :permission (normalize-tool-permission
                                             permission
                                             :context
                                             (format nil "Tool ~A permission"
                                                     normalized-name))
                                :execute-fn execute-fn
                                :approval-display-fn approval-display-fn
                                :package package))))

(defun tool-argument-value (args name)
  "Return values VALUE and SUPPLIED-P for NAME in Lisp data ARGS."
  (loop :for (key . value) :in (tool-args-alist args)
        :when (tool-key= key name)
          :return (values value t)
        :finally (return (values nil nil))))

(defun agent-tool-arg-keyword (name)
  "Return NAME as a keyword symbol suitable for APPLY keyword calls."
  (intern (string-upcase name) :keyword))

(defun collect-agent-tool-positional-args (metadata args)
  "Collect provider ARGS in METADATA argument order for positional calls."
  (let ((values nil)
        (omitting-optional-tail-p nil))
    (dolist (arg (agent-tool-metadata-args metadata) (nreverse values))
      (let ((name (getf arg :name)))
        (multiple-value-bind (value supplied-p)
            (tool-argument-value args name)
          (cond
            (supplied-p
             (when omitting-optional-tail-p
               (error "Optional tool argument ~A was supplied after an omitted optional argument."
                      name))
             (push value values))
            ((getf arg :required)
             (error "Tool ~A requires argument ~A."
                    (agent-tool-metadata-name metadata) name))
            (t
             (setf omitting-optional-tail-p t))))))))

(defun collect-agent-tool-keyword-args (metadata args)
  "Collect provider ARGS as keyword/value pairs for keyword calls."
  (let ((values nil))
    (dolist (arg (agent-tool-metadata-args metadata) (nreverse values))
      (let ((name (getf arg :name)))
        (multiple-value-bind (value supplied-p)
            (tool-argument-value args name)
          (cond
            (supplied-p
             (push (agent-tool-arg-keyword name) values)
             (push value values))
            ((getf arg :required)
             (error "Tool ~A requires argument ~A."
                    (agent-tool-metadata-name metadata) name))))))))

(defun validate-agent-tool-required-args (metadata args)
  "Signal when any required argument in METADATA is absent from ARGS."
  (dolist (arg (agent-tool-metadata-args metadata))
    (when (getf arg :required)
      (multiple-value-bind (_value supplied-p)
          (tool-argument-value args (getf arg :name))
        (declare (ignore _value))
        (unless supplied-p
          (error "Tool ~A requires argument ~A."
                 (agent-tool-metadata-name metadata)
                 (getf arg :name)))))))

(defun resolve-agent-tool-function (symbol)
  "Return SYMBOL's function binding or signal a clear tool error."
  (unless (fboundp symbol)
    (error "Tool function ~A is not defined." symbol))
  (fdefinition symbol))

(defun resolve-agent-tool-function-designator (designator)
  "Resolve a tool function designator."
  (cond
    ((null designator) nil)
    ((functionp designator) designator)
    ((symbolp designator) (resolve-agent-tool-function designator))
    ((and (consp designator)
          (eq (first designator) 'function)
          (symbolp (second designator)))
     (resolve-agent-tool-function (second designator)))
    (t
     (error "Invalid tool function designator: ~S" designator))))

(defun agent-tool-values-result-string (values)
  "Convert multiple return VALUES from a tagged tool into a tool result string."
  (cond
    ((and (= 1 (length values))
          (stringp (first values)))
     (first values))
    ((= 1 (length values))
     (lisp-data-string (first values)))
    (t
     (lisp-data-string (list :values (length values)
                             :result values)))))

(defun execute-agent-tool-metadata (metadata args)
  "Execute a tagged Lisp function tool with provider ARGS."
  (let ((fn (resolve-agent-tool-function
             (agent-tool-metadata-symbol metadata))))
    (agent-tool-values-result-string
     (multiple-value-list
      (ecase (agent-tool-metadata-call-style metadata)
        (:raw-args
         (validate-agent-tool-required-args metadata args)
         (funcall fn args))
        (:positional
         (apply fn (collect-agent-tool-positional-args metadata args)))
        (:keyword
         (apply fn (collect-agent-tool-keyword-args metadata args)))
        (:command
         (unless *current-tool-buffer*
           (error "Command tool ~A requires an active buffer."
                  (agent-tool-metadata-name metadata)))
         (apply fn *current-tool-buffer*
                (collect-agent-tool-positional-args metadata args))))))))

(defun register-agent-tool-provider-definition (metadata)
  "Register AGENT-TOOL-METADATA in the provider tool table."
  (register-tool
   (agent-tool-metadata-name metadata)
   (agent-tool-metadata-description metadata)
   (agent-tool-metadata-input-schema metadata)
   (agent-tool-metadata-permission metadata)
   (lambda (args)
     (execute-agent-tool-metadata metadata args))
   :approval-display-fn
   (resolve-agent-tool-function-designator
    (agent-tool-metadata-approval-display-fn metadata))
   :package (agent-tool-metadata-package metadata)))

(defun register-agent-tool-provider-definitions ()
  "Register process-global tagged agent tools in the provider tool table.
Package-owned tools are registered when their package entrypoint is loaded."
  (dolist (metadata (list-agent-tool-metadata))
    (unless (agent-tool-metadata-package metadata)
      (register-agent-tool-provider-definition metadata))))

(defun register-package-agent-tool-provider-definitions (package-name)
  "Register provider tools owned by PACKAGE-NAME."
  (let ((name (manifest-package-name package-name)))
    (when name
      (dolist (metadata (list-agent-tool-metadata))
        (when (string= name (or (agent-tool-metadata-package metadata) ""))
          (register-agent-tool-provider-definition metadata))))))

(defun tool-allowed-for-active-run-p (name)
  "Return true when NAME is allowed by *ACTIVE-TOOL-NAMES*."
  (or (null *active-tool-names*)
      (member (normalize-tool-name name) *active-tool-names* :test #'string=)))

(defun caller-agent-name ()
  "Return an agent-name string derived from *CURRENT-CALLER*, or NIL."
  (unless (eq *current-caller* :user)
    (string-downcase (symbol-name *current-caller*))))

(defun tool-visible-in-package-context-p (definition &key buffer agent-name)
  "Return true when DEFINITION is visible for the active package context."
  (let ((package (tool-definition-package definition)))
    (or (null package)
        (package-active-p package
                          :buffer buffer
                          :agent-name (or agent-name
                                          (and buffer
                                               (buffer-agent-name buffer))
                                          (caller-agent-name))))))

(defun tool-visible-to-caller-p (definition)
  "Return true when DEFINITION is visible to *CURRENT-CALLER*."
  (let ((perm (effective-tool-permission definition)))
    (or (eq *current-caller* :user)
        (eq perm :agent-allowed)
        (eq perm :agent-with-permission))))

(defun tool-definitions-for-api (&key buffer agent-name)
  "Return a vector of clawmacs tool definitions for provider adapters.
Only includes tools visible to the current *current-caller*."
  (let ((tools nil))
    (map-effective-tool-definitions
     (lambda (name def)
       (declare (ignore name))
       (when (and (tool-visible-to-caller-p def)
                  (tool-visible-in-package-context-p
                   def
                   :buffer buffer
                   :agent-name agent-name)
                  (tool-allowed-for-active-run-p
                   (tool-definition-name def)))
         (push `((:name . ,(tool-definition-name def))
                 (:description . ,(tool-definition-description def))
                 (:input--schema . ,(tool-definition-input-schema def)))
               tools))))
    (coerce (sort tools #'string<
                  :key (lambda (tool)
                         (cdr (assoc :name tool))))
            'vector)))

(defun rendered-tool-description (tool)
  "Return TOOL's provider name and description values."
  (values (cdr (assoc :name tool))
          (cdr (assoc :description tool))))

(defun render-agent-tools-section
    (&optional (tools (coerce (tool-definitions-for-api) 'list)))
  "Render the active provider tool discovery section for the system prompt."
  (when tools
    (let ((sorted-tools (sort (copy-list tools) #'string<
                              :key (lambda (tool)
                                     (cdr (assoc :name tool))))))
      (with-output-to-string (s)
        (format s "<tools>~%")
        (format s "## Tools~%")
        (format s "Use the provider tools for normal actions. Tool inputs and tool results use Lisp data mode with keyword arguments.~%")
        (format s "Use `lisp_eval` for testing, live system updates, introspection, or defining helper tools when no exposed tool fits.~%")
        (format s "### Available tools~%")
        (dolist (tool sorted-tools)
          (multiple-value-bind (name description)
              (rendered-tool-description tool)
            (format s "- ~A: ~A~%" name description)))
        (format s "</tools>")))))

(defun tool-requires-permission-p (name &key buffer)
  "Return T if tool NAME requires user permission."
  (declare (ignore buffer))
  (let ((def (effective-tool-definition name)))
    (and def (eq :agent-with-permission (effective-tool-permission def)))))

(defun execute-tool (name args)
  "Execute tool NAME with ARGS (an alist of parameter values).
Returns a string result or signals an error."
  (unless (tool-allowed-for-active-run-p name)
    (error "Tool ~A is not allowed for this agent" name))
  (let* ((normalized-name (normalize-tool-name name))
         (def (effective-tool-definition normalized-name)))
    (unless def
      (error "Unknown tool: ~A" normalized-name))
    (unless (tool-visible-in-package-context-p
             def
             :buffer *current-tool-buffer*
             :agent-name (caller-agent-name))
      (error "Tool ~A belongs to an inactive package" normalized-name))
    (let ((perm (effective-tool-permission def)))
      (ecase perm
        (:agent-allowed t)
        (:agent-with-permission t)  ; caller is responsible for approval check
        (:user-only
         (unless (eq *current-caller* :user)
           (error "Tool ~A is user-only" normalized-name)))))
    (run-hook-with-args '*before-tool-hook* normalized-name args)
    (let ((result (funcall (tool-definition-execute-fn def) args)))
      (run-hook-with-args '*after-tool-hook* normalized-name args result)
      result)))

(defun format-tool-call-sexpr (name args)
  "Format a tool call as a raw s-expression string.
E.g., (lisp_eval :code \"(list-functions)\")"
  (let ((arg-alist (tool-args-alist args)))
    (with-output-to-string (s)
      (format s "(~A" name)
      (loop :for (k . v) :in arg-alist
            :do (format s " :~A ~S" (tool-key-name k) v))
      (write-char #\) s))))

(defun tool-schema-property (key schema-props)
  "Return KEY's property metadata from SCHEMA-PROPS."
  (loop :for (schema-key . property) :in schema-props
        :when (tool-key= key schema-key)
          :return property))

(defun format-tool-call-expanded (name args)
  "Format a tool call with expanded parameter descriptions.
E.g., (lisp_eval
        :code \"(list-functions)\")  ; The Common Lisp code to evaluate"
  (let ((def (effective-tool-definition name))
        (schema-props nil)
        (arg-alist (tool-args-alist args)))
    ;; Extract property descriptions from schema
    (when def
      (let ((schema (tool-definition-input-schema def)))
        (when schema
          (let ((props (cdr (assoc :properties schema))))
            (when props
              (setf schema-props props))))))
    (with-output-to-string (s)
      (format s "(~A" name)
      (loop :for (k . v) :in arg-alist
            :for param-name := (tool-key-name k)
            :for desc := (let ((prop (tool-schema-property k schema-props)))
                           (when prop (cdr (assoc :description prop))))
            :do (format s "~%  :~A ~S" param-name v)
                (when desc
                  (format s "  ; ~A" desc)))
      (write-char #\) s))))

(defun tool-approval-extra-display (name args)
  "Get extra display text for a tool's approval prompt.
Returns a string or nil. Calls the tool's approval-display-fn if set."
  (let ((def (effective-tool-definition name)))
    (when (and def (tool-definition-approval-display-fn def))
      (handler-case
          (funcall (tool-definition-approval-display-fn def) args)
        (error () nil)))))

;;; --------------------------------------------------------------------------
;;; HTTP Fetch Tool
;;; --------------------------------------------------------------------------

(defun execute-http-fetch (args)
  "Fetch content from an HTTP/HTTPS URL. Returns the response body as text."
  (let ((url (cdr (assoc :url args)))
        (max-chars (or (cdr (assoc :max--chars args)) *http-fetch-max-chars*)))
    (unless url
      (error "url parameter is required"))
    (unless (or (alexandria:starts-with-subseq "http://" url)
                (alexandria:starts-with-subseq "https://" url))
      (error "Only http:// and https:// URLs are supported, got: ~A" url))
    (multiple-value-bind (body status-code)
        (drakma:http-request url
                             :method :get
                             :want-stream nil
                             :force-binary nil
                             :connection-timeout *http-connection-timeout*
                             :user-agent *http-user-agent*)
      (let ((body-string (if (stringp body)
                             body
                             (flexi-streams:octets-to-string
                              body :external-format :utf-8))))
        (let ((truncated (if (> (length body-string) max-chars)
                             (subseq body-string 0 max-chars)
                             body-string)))
          (cl-json:encode-json-to-string
           `((:url . ,url)
             (:status . ,status-code)
             (:length . ,(length body-string))
             (:truncated . ,(> (length body-string) max-chars))
             (:content . ,truncated))))))))

;;; --------------------------------------------------------------------------
;;; Shell Exec Tool
;;; --------------------------------------------------------------------------

(defun execute-shell-exec (args)
  "Execute a shell command within the sandbox directory."
  (let* ((command (cdr (assoc :command args)))
         (timeout (or (cdr (assoc :timeout args)) *shell-exec-default-timeout*))
         (sandbox (or *sandbox-root* (truename "."))))
    (unless command
      (error "command parameter is required"))
    (let* ((stdout-path
             (merge-pathnames
              (format nil "clawmacs-shell-exec-~A.stdout" (gensym))
              #P"/tmp/"))
           (stderr-path
             (merge-pathnames
              (format nil "clawmacs-shell-exec-~A.stderr" (gensym))
              #P"/tmp/"))
           (deadline
             (when timeout
               (+ (get-internal-real-time)
                  (round (* timeout internal-time-units-per-second))))))
      (unwind-protect
           (let ((process
                   (sb-ext:run-program "/bin/sh"
                                       (list "-c" command)
                                       :wait nil
                                       :directory sandbox
                                       :output stdout-path
                                       :error stderr-path
                                       :if-output-exists :supersede
                                       :if-error-exists :supersede))
                 (timed-out-p nil))
            (labels ((process-finished-p ()
                        (not (eql (sb-ext:process-status process) :running)))
                      (read-temp-file (pathname)
                        (if (probe-file pathname)
                            (uiop:read-file-string pathname)
                            "")))
               (loop while (not (process-finished-p)) do
                 (when (and deadline
                            (>= (get-internal-real-time) deadline))
                   (setf timed-out-p t)
                   (ignore-errors (sb-ext:process-kill process 15))
                   (loop repeat 10
                         while (not (process-finished-p))
                         do (sleep *shell-exec-poll-interval*))
                   (when (not (process-finished-p))
                     (ignore-errors (sb-ext:process-kill process 9))
                     (loop repeat 10
                           while (not (process-finished-p))
                           do (sleep *shell-exec-poll-interval*))))
                 (unless (process-finished-p)
                   (sleep *shell-exec-poll-interval*)))
               (let ((stdout (read-temp-file stdout-path))
                     (stderr (read-temp-file stderr-path))
                     (exit-code (and (not timed-out-p)
                                     (ignore-errors
                                      (sb-ext:process-exit-code process)))))
                 (cl-json:encode-json-to-string
                  `((:command . ,command)
                    (:exit--code . ,exit-code)
                    (:timed--out . ,timed-out-p)
                    (:stdout . ,stdout)
                    (:stderr . ,stderr))))))
        (ignore-errors (delete-file stdout-path))
        (ignore-errors (delete-file stderr-path))))))

;;; --------------------------------------------------------------------------
;;; Tool Registration
;;; --------------------------------------------------------------------------

(deftool execute-lisp-eval
  :name "lisp_eval"
  :description "Evaluate one Common Lisp form in the running clawmacs process for testing, introspection, live system updates, or defining helper tools."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((code :type "string"
               :description "Lisp data :code, one Common Lisp form to read and evaluate.")
         (package :type "string"
                  :required nil
                  :description "Lisp data :package, the package name used while reading and evaluating :code. Default: CLAWMACS.")))

(defun init-tools ()
  "Register the default clawmacs built-in tools.
This removes any previously registered built-in entries, then re-registers
tagged agent tools. User-added tools remain untouched."

  (dolist (name *built-in-tool-names*)
    (remhash name *tool-table*))

  (setf *lisp-eval-default-package* "CLAWMACS")
  (register-agent-tool-provider-definitions))
