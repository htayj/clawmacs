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
  (when (and *current-clawmacs-package*
             (not (package-resource-type-allowed-p :tool)))
    (return-from register-agent-tool-metadata nil))
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
       (when (or (null *current-clawmacs-package*)
                 (package-resource-type-allowed-p :tool))
         (register-agent-tool-metadata
          ',symbol ',tool-spec
          :command-p (not (null ,metadata-var))
          :lambda-list (and ,metadata-var
                            (command-metadata-lambda-list ,metadata-var))
          :docstring (or (and ,metadata-var
                              (command-metadata-docstring ,metadata-var))
                         (documentation ',symbol 'function)))))))

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

(defvar *approval-policy-project-registry-cache* (make-hash-table :test #'equal)
  "Memoized project-local approval policy registries keyed by pathname.")

(defvar *approval-policy-isolation-root* nil
  "When non-nil, redirect project-local guard state under this root.
Used by isolated prompt runs so project guard edits and overrides never touch
the live working tree.")

(defvar *approval-policy-network-dependent-tools*
  '("http_fetch" "netcons_run" "netcons_search" "netcons_open" "netcons_find")
  "Names treated as network-dependent for guard policy checks.")

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
  '("lisp_eval" "recovery_list")
  "Names reserved for core Clawmacs provider tools.
INIT-TOOLS removes these entries before re-registering tagged tools, so
user-added tools stored in *tool-table* are left intact.")

(defun make-approval-policy-registry ()
  "Create an empty in-memory approval policy registry."
  (list :default nil
        :tools (make-hash-table :test #'equal)
        :sandbox-default nil
        :sandbox-tools (make-hash-table :test #'equal)
        :network-default nil
        :network-tools (make-hash-table :test #'equal)
        :working-directory-default nil
        :working-directory-tools (make-hash-table :test #'equal)
        :history nil))

(defun approval-policy-tools (registry)
  "Return the per-tool permission table stored in REGISTRY."
  (getf registry :tools))

(defun approval-policy-sandbox-tools (registry)
  "Return the per-tool sandbox preset table stored in REGISTRY."
  (getf registry :sandbox-tools))

(defun approval-policy-network-tools (registry)
  "Return the per-tool network toggle table stored in REGISTRY."
  (getf registry :network-tools))

(defun approval-policy-working-directory-tools (registry)
  "Return the per-tool working-directory table stored in REGISTRY."
  (getf registry :working-directory-tools))

(defun approval-policy-history (registry)
  "Return the audit history stored in REGISTRY."
  (getf registry :history))

(defun setf-approval-policy-history (registry history)
  "Store HISTORY in REGISTRY and return it."
  (setf (getf registry :history) history))

(defun approval-policy-sandbox-default (registry)
  "Return the default sandbox preset in REGISTRY."
  (getf registry :sandbox-default))

(defun approval-policy-network-default (registry)
  "Return the default network toggle in REGISTRY."
  (getf registry :network-default))

(defun approval-policy-working-directory-default (registry)
  "Return the default working-directory policy in REGISTRY."
  (getf registry :working-directory-default))

(defun approval-policy-registry-for-path (path)
  "Return the approval policy registry associated with PATH."
  (if (string= (approval-policy-cache-key path)
               (approval-policy-cache-key *approval-policy-path*))
      (ensure-approval-policy-loaded)
      (approval-policy-load-from-path-or-cache path)))

(defun approval-policy-path-for-registry (&key buffer directory)
  "Return the approval policy pathname for BUFFER or DIRECTORY."
  (cond
    (directory (approval-policy-path-for-directory directory))
    (buffer (approval-policy-path-for-buffer buffer))
    (t *approval-policy-path*)))

(defun approval-policy-registry-for-context (&key buffer directory)
  "Return the registry for BUFFER or DIRECTORY, defaulting to the user policy."
  (let ((path (approval-policy-path-for-registry :buffer buffer
                                                 :directory directory)))
    (approval-policy-registry-for-path path)))

(defun approval-policy-user-registry-fallback (path)
  "Return the user registry when PATH is not the user policy path."
  (unless (string= (approval-policy-cache-key path)
                   (approval-policy-cache-key *approval-policy-path*))
    (ensure-approval-policy-loaded)))

(defun approval-policy-default-permission (&key buffer directory)
  "Return the default approval override permission for the selected policy."
  (let ((registry (approval-policy-registry-for-context :buffer buffer
                                                        :directory directory)))
    (getf registry :default)))

(defun approval-policy-tool-permission (name &key buffer directory)
  "Return the approval override permission for tool NAME in the selected policy."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path))
         (user-registry (approval-policy-user-registry-fallback path))
         (normalized-name (normalize-tool-name name)))
    (or (gethash normalized-name (approval-policy-tools registry))
        (and user-registry
             (gethash normalized-name
                      (approval-policy-tools user-registry))))))

(defun approval-policy-sandbox-permission (name &key buffer directory)
  "Return the sandbox preset override for tool NAME in the selected policy."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path))
         (user-registry (approval-policy-user-registry-fallback path))
         (normalized-name (normalize-tool-name name)))
    (or (gethash normalized-name (approval-policy-sandbox-tools registry))
        (and user-registry
             (gethash normalized-name
                      (approval-policy-sandbox-tools user-registry))))))

(defun approval-policy-working-directory-permission (name &key buffer directory)
  "Return the working-directory override for tool NAME in the selected policy."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path))
         (user-registry (approval-policy-user-registry-fallback path))
         (normalized-name (normalize-tool-name name)))
    (or (gethash normalized-name
                 (approval-policy-working-directory-tools registry))
        (and user-registry
             (gethash normalized-name
                      (approval-policy-working-directory-tools
                       user-registry))))))

(defun approval-policy-default-sandbox-permission (&key buffer directory)
  "Return the default sandbox preset for the selected policy."
  (approval-policy-sandbox-default
   (approval-policy-registry-for-context :buffer buffer
                                         :directory directory)))

(defun approval-policy-default-working-directory-permission
    (&key buffer directory)
  "Return the default working-directory preset for the selected policy."
  (approval-policy-working-directory-default
   (approval-policy-registry-for-context :buffer buffer
                                         :directory directory)))

(defun approval-policy-default-network-permission (&key buffer directory)
  "Return the default network toggle for the selected policy."
  (approval-policy-network-default
   (approval-policy-registry-for-context :buffer buffer
                                         :directory directory)))

(defun approval-policy-json-key-string (key)
  "Return KEY as a normalized approval policy JSON key string."
  (with-output-to-string (stream)
    (let ((text (json-key-string key))
          (index 0)
          (length 0))
      (setf length (length text))
      (loop
        (when (>= index length)
          (return))
        (let ((char (char text index)))
          (if (and (char= char #\_)
                   (< (1+ index) length)
                   (char= (char text (1+ index)) #\_))
              (progn
                (write-char #\_ stream)
                (incf index 2))
              (progn
                (write-char char stream)
                (incf index 1))))))))

(defun approval-policy-history-entries (&key buffer directory)
  "Return a copy of the recorded approval audit entries for the selected policy."
  (copy-tree
   (approval-policy-history
    (approval-policy-registry-for-context :buffer buffer
                                          :directory directory))))

(defun approval-policy-network-permission (name &key buffer directory)
  "Return the network toggle override for tool NAME in the selected policy."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path))
         (user-registry (approval-policy-user-registry-fallback path))
         (normalized-name (normalize-tool-name name)))
    (or (gethash normalized-name (approval-policy-network-tools registry))
        (and user-registry
             (gethash normalized-name
                      (approval-policy-network-tools user-registry))))))

(defun set-approval-policy-default-permission (permission &key buffer directory)
  "Set the default approval override PERMISSION in the selected policy."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path)))
    (setf (getf registry :default)
          (normalize-tool-permission permission
                                     :allow-inherit-p t
                                     :context "Guard policy default"))
    (save-approval-policy :buffer buffer :directory directory)
    (approval-policy-default-permission :buffer buffer :directory directory)))

(defun set-approval-policy-tool-permission (name permission
                                           &key buffer directory)
  "Set or clear the approval override PERMISSION for tool NAME."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path))
         (tools (approval-policy-tools registry))
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
    (save-approval-policy :buffer buffer :directory directory)
    normalized-permission))

(defun set-approval-policy-sandbox-permission (name permission
                                              &key buffer directory)
  "Set or clear the sandbox preset PERMISSION for tool NAME."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path))
         (tools (approval-policy-sandbox-tools registry))
         (normalized-name (normalize-tool-name name))
         (normalized-permission
           (normalize-approval-preset permission
                                      :allow-inherit-p t
                                      :context
                                      (format nil "Guard sandbox override for ~A"
                                              normalized-name))))
    (if normalized-permission
        (setf (gethash normalized-name tools) normalized-permission)
        (remhash normalized-name tools))
    (save-approval-policy :buffer buffer :directory directory)
    normalized-permission))

(defun set-approval-policy-network-default (permission &key buffer directory)
  "Set the default network toggle PERMISSION in the selected policy."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path)))
    (setf (getf registry :network-default)
          (normalize-network-toggle permission
                                    :allow-inherit-p t
                                    :context "Guard network default"))
    (save-approval-policy :buffer buffer :directory directory)
    (approval-policy-default-network-permission
     :buffer buffer
     :directory directory)))

(defun set-approval-policy-network-permission (name permission
                                               &key buffer directory)
  "Set or clear the network toggle PERMISSION for tool NAME."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path))
         (tools (approval-policy-network-tools registry))
         (normalized-name (normalize-tool-name name))
         (normalized-permission
           (normalize-network-toggle permission
                                     :allow-inherit-p t
                                     :context
                                     (format nil "Guard network override for ~A"
                                             normalized-name))))
    (if normalized-permission
        (setf (gethash normalized-name tools) normalized-permission)
        (remhash normalized-name tools))
    (save-approval-policy :buffer buffer :directory directory)
    normalized-permission))

(defun set-approval-policy-working-directory-permission (name permission
                                                         &key buffer directory)
  "Set or clear the working-directory preset PERMISSION for tool NAME."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path))
         (tools (approval-policy-working-directory-tools registry))
         (normalized-name (normalize-tool-name name))
         (normalized-permission
           (normalize-approval-preset permission
                                      :allow-inherit-p t
                                      :context
                                      (format nil "Guard working-directory override for ~A"
                                              normalized-name))))
    (if normalized-permission
        (setf (gethash normalized-name tools) normalized-permission)
        (remhash normalized-name tools))
    (save-approval-policy :buffer buffer :directory directory)
    normalized-permission))

(defun set-approval-policy-default-sandbox-permission (permission
                                                       &key buffer directory)
  "Set the default sandbox preset PERMISSION in the selected policy."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path)))
    (setf (getf registry :sandbox-default)
          (normalize-approval-preset permission
                                     :allow-inherit-p t
                                     :context "Guard sandbox default"))
    (save-approval-policy :buffer buffer :directory directory)
    (approval-policy-default-sandbox-permission
     :buffer buffer
     :directory directory)))

(defun set-approval-policy-default-working-directory-permission
    (permission &key buffer directory)
  "Set the default working-directory preset PERMISSION in the selected policy."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path)))
    (setf (getf registry :working-directory-default)
          (normalize-approval-preset permission
                                     :allow-inherit-p t
                                     :context "Guard working-directory default"))
    (save-approval-policy :buffer buffer :directory directory)
    (approval-policy-default-working-directory-permission
     :buffer buffer
     :directory directory)))

(defun set-approval-policy-default-network-permission (permission
                                                       &key buffer directory)
  "Set the default network toggle PERMISSION in the selected policy."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path)))
    (setf (getf registry :network-default)
          (normalize-network-toggle permission
                                    :allow-inherit-p t
                                    :context "Guard network default"))
    (save-approval-policy :buffer buffer :directory directory)
    (approval-policy-default-network-permission
     :buffer buffer
     :directory directory)))

(defun normalize-approval-preset (value &key (allow-inherit-p t) (context "Approval preset"))
  "Normalize VALUE into a sandbox or working-directory preset keyword."
  (let ((normalized
          (cond
            ((null value) (and allow-inherit-p nil))
            ((keywordp value) value)
            ((symbolp value)
             (intern (string-upcase (symbol-name value)) :keyword))
            ((stringp value)
             (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
               (when (zerop (length trimmed))
                 (error "~A must not be blank." context))
               (intern (string-upcase (substitute #\- #\_ trimmed)) :keyword)))
            (t
             (error "~A must be a keyword, symbol, string, or NIL: ~S"
                    context value)))))
    (when (and allow-inherit-p (or (null normalized) (eq normalized :inherit)))
      (return-from normalize-approval-preset nil))
    (unless (member normalized '(:read-only :workspace-write :full-access
                                  :project-root :workspace :any)
                    :test #'eq)
      (error "~A has unsupported value ~S." context value))
    normalized))

(defun approval-policy-path-for-directory (directory)
  "Return the project-local approval policy path for DIRECTORY."
  (let* ((base-directory
           (if *approval-policy-isolation-root*
               (merge-pathnames
                (format nil "projects/~36R/" ; stable enough for one run
                        (sxhash
                         (namestring
                          (uiop:ensure-directory-pathname directory))))
                (uiop:ensure-directory-pathname
                 *approval-policy-isolation-root*))
               (uiop:ensure-directory-pathname directory))))
    (merge-pathnames #P".clawmacs.d/guard.json" base-directory)))

(defun approval-policy-path-for-buffer (&optional buffer)
  "Return the approval policy path relevant to BUFFER or the user default."
  (if buffer
      (approval-policy-path-for-directory (buffer-working-directory buffer))
      *approval-policy-path*))

(defun approval-policy-cache-key (path)
  "Return a stable cache key for PATH."
  (namestring (uiop:ensure-directory-pathname path)))

(defun approval-policy-json-sequence->list (value)
  "Normalize JSON array VALUE into a Common Lisp list."
  (cond
    ((null value) nil)
    ((listp value) value)
    ((vectorp value) (coerce value 'list))
    (t (list value))))

(defun approval-policy-json-raw-string (key)
  "Return KEY as a decoded JSON key string suitable for normalization."
  (typecase key
    (string key)
    (symbol (json-key-string key))
    (t (princ-to-string key))))

(defun approval-policy-json-key-token (key)
  "Return KEY as a lower-case token with separators stripped."
  (let ((raw (approval-policy-json-raw-string key)))
    (with-output-to-string (stream)
      (loop :for char :across raw
            :when (alphanumericp char)
              :do (write-char (char-downcase char) stream)))))

(defun approval-policy-json-key-name (key)
  "Return KEY as a normalized guard JSON key string."
  (let ((raw (approval-policy-json-raw-string key)))
    (with-output-to-string (stream)
      (loop :with pending-separator-p := nil
            :with wrote-any-p := nil
            :for char :across raw
            :do (cond
                  ((char= char #\_)
                   (when wrote-any-p
                     (write-char #\_ stream)
                     (setf wrote-any-p t))
                   (setf pending-separator-p nil))
                  ((or (char= char #\-) (char= char #\Space))
                   (setf pending-separator-p t))
                  (t
                   (when (and pending-separator-p wrote-any-p)
                     (write-char #\- stream))
                   (setf pending-separator-p nil)
                   (write-char (char-downcase char) stream)
                   (setf wrote-any-p t)))))))

(defun approval-policy-lookup-json-value (alist key)
  "Find KEY in decoded guard JSON ALIST, accepting string or symbol keys."
  (let ((token (approval-policy-json-key-token key)))
    (loop :for (entry-key . value) :in alist
          :when (string= token (approval-policy-json-key-token entry-key))
            :do (return value))))

(defun approval-policy-tool-name-from-json-key (key)
  "Return a canonical tool name for a decoded guard JSON KEY."
  (let ((token (approval-policy-json-key-token key)))
    (or (loop :for name :being :the :hash-keys :of *tool-table*
              :for normalized := (normalize-tool-name name)
              :when (string= token (approval-policy-json-key-token normalized))
                :do (return normalized))
        (normalize-tool-name
         (approval-policy-json-key-name key)))))

(defun approval-policy-json-entry->alist (entry)
  "Normalize one decoded guard audit ENTRY into a keyword alist."
  (flet ((canonical-entry-key (entry-key)
           (let ((token (approval-policy-json-key-token entry-key)))
             (cond
               ((string= token "timestamp") :timestamp)
               ((string= token "buffer") :buffer)
               ((string= token "workingdirectory") :working-directory)
               ((string= token "toolname") :tool-name)
               ((string= token "decision") :decision)
               ((string= token "policy") :policy)
               ((string= token "reason") :reason)
               ((string= token "entry") :entry)
               (t
                (intern (string-upcase (approval-policy-json-key-name entry-key))
                        :keyword))))))
    (loop :for (entry-key . value) :in (approval-policy-json-sequence->list entry)
          :collect (cons (canonical-entry-key entry-key) value))))

(defun approval-policy-load-from-path (path)
  "Load an approval policy registry from PATH."
  (let ((registry (make-approval-policy-registry)))
    (when (probe-file path)
      (handler-case
          (let* ((json (uiop:read-file-string path))
                 (data (api-json-decode json))
                 (default (approval-policy-lookup-json-value data "default"))
                 (tool-overrides (approval-policy-lookup-json-value data "tools"))
                 (sandbox-default
                   (approval-policy-lookup-json-value data "sandbox-default"))
                 (sandbox-overrides
                   (approval-policy-lookup-json-value data "sandbox-tools"))
                 (network-default
                   (approval-policy-lookup-json-value data "network-default"))
                 (network-overrides
                   (approval-policy-lookup-json-value data "network-tools"))
                 (working-default
                   (approval-policy-lookup-json-value data "working-directory-default"))
                 (working-overrides
                   (approval-policy-lookup-json-value data "working-directory-tools"))
                 (history (approval-policy-lookup-json-value data "history"))
                 (tools (approval-policy-tools registry))
                 (sandbox-tools (approval-policy-sandbox-tools registry))
                 (network-tools (approval-policy-network-tools registry))
                 (working-tools (approval-policy-working-directory-tools registry)))
            (setf (getf registry :default)
                  (normalize-tool-permission default
                                             :allow-inherit-p t
                                             :context "Guard policy default"))
            (setf (getf registry :sandbox-default)
                  (normalize-approval-preset sandbox-default
                                             :allow-inherit-p t
                                             :context "Guard sandbox default"))
            (setf (getf registry :network-default)
                  (normalize-network-toggle network-default
                                            :allow-inherit-p t
                                            :context "Guard network default"))
            (setf (getf registry :working-directory-default)
                  (normalize-approval-preset working-default
                                             :allow-inherit-p t
                                             :context "Guard working-directory default"))
            (dolist (entry (approval-policy-json-sequence->list tool-overrides))
              (let* ((name (approval-policy-tool-name-from-json-key (car entry)))
                     (permission (normalize-tool-permission
                                  (cdr entry)
                                  :allow-inherit-p t
                                  :context
                                  (format nil "Guard policy override for ~A"
                                          name))))
                (when permission
                  (setf (gethash (normalize-tool-name name) tools)
                        permission))))
            (dolist (entry (approval-policy-json-sequence->list sandbox-overrides))
              (let* ((name (approval-policy-tool-name-from-json-key (car entry)))
                     (preset (normalize-approval-preset
                              (cdr entry)
                              :allow-inherit-p t
                              :context
                              (format nil "Guard sandbox override for ~A"
                                      name))))
                (when preset
                  (setf (gethash (normalize-tool-name name) sandbox-tools)
                        preset))))
            (dolist (entry (approval-policy-json-sequence->list network-overrides))
              (let* ((name (approval-policy-tool-name-from-json-key (car entry)))
                     (toggle (normalize-network-toggle
                              (cdr entry)
                              :allow-inherit-p t
                              :context
                              (format nil "Guard network override for ~A"
                                      name))))
                (when toggle
                  (setf (gethash (normalize-tool-name name) network-tools)
                        toggle))))
            (dolist (entry (approval-policy-json-sequence->list working-overrides))
              (let* ((name (approval-policy-tool-name-from-json-key (car entry)))
                     (preset (normalize-approval-preset
                              (cdr entry)
                              :allow-inherit-p t
                              :context
                              (format nil "Guard working-directory override for ~A"
                                      name))))
                (when preset
                  (setf (gethash (normalize-tool-name name) working-tools)
                        preset))))
            (let ((history-list
                    (mapcar #'approval-policy-json-entry->alist
                            (approval-policy-json-sequence->list history))))
              (when history-list
                (setf (getf registry :history) history-list))))
        (error (condition)
          (warn "Failed to load guard policy from ~A: ~A"
                path
                condition))))
    registry))

(defun approval-policy-load-from-path-or-cache (path)
  "Load and memoize an approval policy registry for PATH."
  (let* ((key (approval-policy-cache-key path))
         (cached (gethash key *approval-policy-project-registry-cache*)))
    (or cached
        (setf (gethash key *approval-policy-project-registry-cache*)
              (approval-policy-load-from-path path)))))

(defun approval-policy-registry-for-buffer (&optional buffer)
  "Return the approval policy registry relevant to BUFFER, if any."
  (when buffer
    (approval-policy-load-from-path-or-cache
     (approval-policy-path-for-buffer buffer))))

(defun approval-policy-record-history-entry
    (buffer tool-name decision &key policy entry reason directory)
  "Record one approval decision in the policy history and notify hooks."
  (let* ((working-directory
           (or (and buffer
                    (buffer-working-directory buffer)
                    (namestring (buffer-working-directory buffer)))
               (and directory
                    (namestring
                     (uiop:ensure-directory-pathname directory)))))
         (registry (approval-policy-registry-for-context :buffer buffer
                                                         :directory directory))
         (audit-entry
           `((:timestamp . ,(get-universal-time))
             (:buffer . ,(and buffer (buffer-name buffer)))
             (:working-directory . ,working-directory)
             (:tool-name . ,(normalize-tool-name tool-name))
             (:decision . ,(string-downcase (symbol-name decision)))
             (:policy . ,(and policy (string-downcase (symbol-name policy))))
             (:reason . ,reason)
             (:entry . ,entry)))
         (history (cons audit-entry (approval-policy-history registry))))
    (setf (getf registry :history)
          (subseq history 0 (min (length history) 200)))
    (save-approval-policy :buffer buffer :directory directory)
    (maybe-run-hook-with-args '*approval-review-hook*
                              buffer
                              (normalize-tool-name tool-name)
                              decision
                              policy
                              audit-entry)
    audit-entry))

(defun approval-policy->json-payload (registry)
  "Return REGISTRY as an API-ready JSON alist."
  `((:version . 3)
    (:default . ,(tool-permission-json-value
                  (getf registry :default)))
    (:tools . ,(let ((entries nil))
                 (maphash
                  (lambda (name permission)
                    (push `(,name . ,(tool-permission-json-value permission))
                          entries))
                  (approval-policy-tools registry))
                 (nreverse entries)))
    (:sandbox-default . ,(tool-permission-json-value
                          (approval-policy-sandbox-default registry)))
    (:sandbox-tools . ,(let ((entries nil))
                         (maphash
                          (lambda (name preset)
                            (push `(,name . ,(tool-permission-json-value preset))
                                  entries))
                          (approval-policy-sandbox-tools registry))
                         (nreverse entries)))
    (:network-default . ,(tool-permission-json-value
                           (approval-policy-network-default registry)))
    (:network-tools . ,(let ((entries nil))
                         (maphash
                          (lambda (name toggle)
                            (push `(,name . ,(tool-permission-json-value toggle))
                                  entries))
                          (approval-policy-network-tools registry))
                         (nreverse entries)))
    (:working-directory-default . ,(tool-permission-json-value
                                    (approval-policy-working-directory-default
                                     registry)))
    (:working-directory-tools . ,(let ((entries nil))
                                   (maphash
                                    (lambda (name preset)
                                      (push `(,name . ,(tool-permission-json-value preset))
                                            entries))
                                    (approval-policy-working-directory-tools registry))
                                   (nreverse entries)))
    (:history . ,(coerce (reverse (approval-policy-history registry)) 'vector))))

(defun load-approval-policy ()
  "Load and memoize the persisted user approval policy."
  (setf *approval-policy-registry*
        (approval-policy-load-from-path *approval-policy-path*))
  *approval-policy-registry*)

(defun ensure-approval-policy-loaded ()
  "Load the approval policy when it has not been memoized yet."
  (unless *approval-policy-registry*
    (load-approval-policy))
  *approval-policy-registry*)

(defun save-approval-policy (&key buffer directory)
  "Persist the current approval policy registry to disk."
  (let* ((path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (registry (approval-policy-registry-for-path path))
         (payload (approval-policy->json-payload registry)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (api-json-encode payload) stream))
    (when (string= (approval-policy-cache-key path)
                   (approval-policy-cache-key *approval-policy-path*))
      (setf *approval-policy-registry* registry))
    path))

(defun effective-tool-permission (definition-or-name &key buffer directory)
  "Return the approval-effective permission for DEFINITION-OR-NAME."
  (let* ((definition (if (tool-definition-p definition-or-name)
                         definition-or-name
                         (effective-tool-definition definition-or-name)))
         (name (or (and definition (tool-definition-name definition))
                   (and definition-or-name
                        (normalize-tool-name definition-or-name))))
         (static-permission (and definition
                                 (tool-definition-permission definition)))
         (project-registry
           (approval-policy-registry-for-context :buffer buffer
                                                 :directory directory))
         (user-registry (ensure-approval-policy-loaded))
         (tool-project-permission
           (and name project-registry
                (gethash name (approval-policy-tools project-registry))))
         (tool-user-permission
           (and name
                (gethash name (approval-policy-tools user-registry))))
         (default-project-permission
           (and project-registry (getf project-registry :default)))
         (default-user-permission
           (getf user-registry :default)))
    (if definition
        (or tool-project-permission
            tool-user-permission
            default-project-permission
            default-user-permission
            static-permission)
        nil)))

(defun effective-tool-sandbox-permission (definition-or-name
                                          &key buffer directory)
  "Return the effective sandbox preset for DEFINITION-OR-NAME."
  (effective-tool-sandbox-preset definition-or-name
                                 :buffer buffer
                                 :directory directory))

(defun effective-tool-working-directory-permission (definition-or-name
                                                    &key buffer directory)
  "Return the effective working-directory preset for DEFINITION-OR-NAME."
  (effective-tool-working-directory-policy definition-or-name
                                           :buffer buffer
                                           :directory directory))

(defun normalize-network-toggle (value &key (allow-inherit-p t) (context "Network toggle"))
  "Normalize VALUE into a network toggle keyword."
  (let ((normalized
          (cond
            ((null value) (and allow-inherit-p nil))
            ((keywordp value) value)
            ((symbolp value)
             (intern (string-upcase (symbol-name value)) :keyword))
            ((stringp value)
             (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
               (when (zerop (length trimmed))
                 (error "~A must not be blank." context))
               (intern (string-upcase (substitute #\- #\_ trimmed)) :keyword)))
            ((eq value t) :allow)
            (t
             (error "~A must be a keyword, symbol, string, boolean, or NIL: ~S"
                    context value)))))
    (when (and allow-inherit-p (or (null normalized) (eq normalized :inherit)))
      (return-from normalize-network-toggle nil))
    (unless (member normalized '(:allow :deny) :test #'eq)
      (error "~A has unsupported value ~S." context value))
    normalized))

(defun effective-tool-sandbox-preset (definition-or-name &key buffer directory)
  "Return the effective sandbox preset for DEFINITION-OR-NAME."
  (let* ((definition (if (tool-definition-p definition-or-name)
                         definition-or-name
                         (effective-tool-definition definition-or-name)))
         (name (or (and definition (tool-definition-name definition))
                   (and definition-or-name
                        (normalize-tool-name definition-or-name))))
         (project-registry
           (approval-policy-registry-for-context :buffer buffer
                                                 :directory directory))
         (user-registry (ensure-approval-policy-loaded)))
    (or (and name project-registry
             (gethash name (approval-policy-sandbox-tools project-registry)))
        (and name
             (gethash name (approval-policy-sandbox-tools user-registry)))
        (and project-registry
             (approval-policy-sandbox-default project-registry))
        (approval-policy-sandbox-default user-registry)
        :workspace-write)))

(defun effective-tool-network-toggle (definition-or-name &key buffer directory)
  "Return the effective network toggle for DEFINITION-OR-NAME."
  (let* ((definition (if (tool-definition-p definition-or-name)
                         definition-or-name
                         (effective-tool-definition definition-or-name)))
         (name (or (and definition (tool-definition-name definition))
                   (and definition-or-name
                        (normalize-tool-name definition-or-name))))
         (project-registry
           (approval-policy-registry-for-context :buffer buffer
                                                 :directory directory))
         (user-registry (ensure-approval-policy-loaded)))
    (or (and name project-registry
             (gethash name (approval-policy-network-tools project-registry)))
        (and name
             (gethash name (approval-policy-network-tools user-registry)))
        (and project-registry (approval-policy-network-default project-registry))
        (approval-policy-network-default user-registry)
        :allow)))

(defun effective-tool-working-directory-policy (definition-or-name
                                              &key buffer directory)
  "Return the effective working-directory policy for DEFINITION-OR-NAME."
  (let* ((definition (if (tool-definition-p definition-or-name)
                         definition-or-name
                         (effective-tool-definition definition-or-name)))
         (name (or (and definition (tool-definition-name definition))
                   (and definition-or-name
                        (normalize-tool-name definition-or-name))))
         (project-registry
           (approval-policy-registry-for-context :buffer buffer
                                                 :directory directory))
         (user-registry (ensure-approval-policy-loaded)))
    (or (and name project-registry
             (gethash name
                      (approval-policy-working-directory-tools project-registry)))
        (and name
             (gethash name
                      (approval-policy-working-directory-tools user-registry)))
        (and project-registry
             (approval-policy-working-directory-default project-registry))
        (approval-policy-working-directory-default user-registry)
        :any)))

(defun buffer-declared-project-root (buffer)
  "Return BUFFER's declared project root when a project is attached."
  (when buffer
    (let ((project-name (buffer-project-name buffer)))
      (when project-name
        (let ((project (find-project project-name)))
          (and project (project-root project)))))))

(defun approval-policy-path-within-root-p (root path)
  "Return true when PATH is inside ROOT."
  (and root path
       (let ((root-path (uiop:ensure-directory-pathname root))
             (target-path (uiop:ensure-directory-pathname path)))
         (path-under-directory-p root-path target-path))))

(defun approval-policy-tool-working-directory-allowed-p (definition-or-name
                                                        &key buffer directory)
  "Return true when DEFINITION-OR-NAME may run in the selected directory."
  (let ((policy (effective-tool-working-directory-policy definition-or-name
                                                        :buffer buffer
                                                        :directory directory)))
    (case policy
      ((nil :any) t)
      (:workspace
       (approval-policy-path-within-root-p (truename ".")
                                           (or directory
                                               (and buffer
                                                    (buffer-working-directory buffer)))))
      (:project-root
       (let ((allowed-root (or (buffer-declared-project-root buffer)
                               (and buffer (buffer-working-directory buffer)))))
         (approval-policy-path-within-root-p
          allowed-root
          (or directory
              (and buffer (buffer-working-directory buffer))))))
      (:read-only
       (let ((name (if (tool-definition-p definition-or-name)
                       (tool-definition-name definition-or-name)
                       (normalize-tool-name definition-or-name))))
         (member name '("read" "find" "grep" "git_log" "git_status" "git_show"
                        "git_diff" "git_branch" "git_remote" "http_fetch")
                 :test #'string=)))
      (t t))))

(defun tool-mutates-state-p (name)
  "Return true when tool NAME is treated as mutating for sandbox checks."
  (member (normalize-tool-name name)
          '("write" "edit" "shell_exec" "git_add" "git_commit" "git_push")
          :test #'string=))

(defun approval-policy-tool-sandbox-allowed-p (definition-or-name
                                              &key buffer directory)
  "Return true when DEFINITION-OR-NAME is allowed by the sandbox preset."
  (let ((preset (effective-tool-sandbox-preset definition-or-name
                                               :buffer buffer
                                               :directory directory)))
    (cond
      ((or (null preset) (eq preset :full-access)) t)
      ((eq preset :read-only)
       (not (tool-mutates-state-p
             (if (tool-definition-p definition-or-name)
                 (tool-definition-name definition-or-name)
                 definition-or-name))))
      ((eq preset :workspace-write) t)
      (t t))))

(defun tool-network-dependent-p (name)
  "Return true when tool NAME needs network access."
  (member (normalize-tool-name name)
          *approval-policy-network-dependent-tools*
          :test #'string=))

(defun approval-policy-tool-network-allowed-p (definition-or-name
                                               &key buffer directory)
  "Return true when DEFINITION-OR-NAME is allowed network access."
  (let ((toggle (effective-tool-network-toggle definition-or-name
                                               :buffer buffer
                                               :directory directory)))
    (cond
      ((or (null toggle) (eq toggle :allow)) t)
      ((eq toggle :deny)
       (let ((name (if (tool-definition-p definition-or-name)
                       (tool-definition-name definition-or-name)
                       (normalize-tool-name definition-or-name))))
         (not (tool-network-dependent-p name))))
      (t t))))

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

(defun tool-visible-to-caller-p (definition &key buffer directory)
  "Return true when DEFINITION is visible to *CURRENT-CALLER*."
  (let ((perm (effective-tool-permission definition
                                         :buffer buffer
                                         :directory directory)))
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
       (when (and (tool-visible-to-caller-p def :buffer buffer)
                  (tool-visible-in-package-context-p
                   def
                   :buffer buffer
                   :agent-name agent-name)
                  (approval-policy-tool-sandbox-allowed-p
                   def
                   :buffer buffer)
                  (approval-policy-tool-network-allowed-p
                   def
                   :buffer buffer)
                  (approval-policy-tool-working-directory-allowed-p
                   def
                   :buffer buffer)
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
  (let ((def (effective-tool-definition name)))
    (and def
         (eq :agent-with-permission
             (effective-tool-permission def :buffer buffer)))))

(defvar *tool-execution-journal-max-chars* 20000
  "Maximum result/error characters retained in durable tool execution events.")

(defun bounded-tool-execution-string (value)
  "Return VALUE as a bounded string for durable tool execution events."
  (let ((text (cond
                ((null value) "")
                ((stringp value) value)
                (t (lisp-data-string value)))))
    (if (> (length text) *tool-execution-journal-max-chars*)
        (concatenate 'string
                     (subseq text 0 *tool-execution-journal-max-chars*)
                     (format nil "~%[truncated at ~D characters]"
                             *tool-execution-journal-max-chars*))
        text)))

(defun condition-type-name (condition)
  "Return a readable type name for CONDITION."
  (let ((type (type-of condition)))
    (if (symbolp type)
        (format nil "~A" type)
        (prin1-to-string type))))

(defun tool-execution-caller-name ()
  "Return the current tool caller as a durable string."
  (if (and (boundp '*current-caller*) *current-caller*)
      (string-downcase (symbol-name *current-caller*))
      "unknown"))

(defun record-tool-execution-event (buffer event)
  "Durably record one tool execution EVENT for BUFFER when possible.
Failures while journaling are logged but never abort tool execution."
  (handler-case
      (progn
        (when buffer
          (ensure-buffer-session buffer)
          (when (buffer-session buffer)
            (append-session-event (buffer-session buffer) event)))
        (file-debug-log "tool-execution"
                        "~A"
                        (bounded-tool-execution-string event))
        event)
    (error (condition)
      (file-debug-log "tool-execution-journal-error"
                      "failed to journal tool event: ~A"
                      condition)
      nil)))

(defun tool-execution-event (phase tool-name args
                             &key buffer tool-id status result condition reason)
  "Return a durable event describing one tool execution PHASE."
  (declare (ignore buffer))
  `((:event . "tool-execution")
    (:phase . ,phase)
    (:tool-name . ,(normalize-tool-name tool-name))
    ,@(when tool-id `((:tool-id . ,tool-id)))
    (:caller . ,(tool-execution-caller-name))
    (:timestamp . ,(get-universal-time))
    (:input . ,(bounded-tool-execution-string args))
    ,@(when status `((:status . ,status)))
    ,@(when reason `((:reason . ,(bounded-tool-execution-string reason))))
    ,@(when result `((:result . ,(bounded-tool-execution-string result))))
    ,@(when condition
        `((:condition-type . ,(condition-type-name condition))
          (:condition-message . ,(bounded-tool-execution-string
                                  (format nil "~A" condition)))))))

(defun file-checkpoint-tool-p (tool-name)
  "Return true when TOOL-NAME is a built-in file mutation tool."
  (member (normalize-tool-name tool-name) '("write" "edit") :test #'string=))

(defun file-checkpoint-path-argument (args)
  "Return the path argument from ARGS, if any."
  (ignore-errors
    (tool-arg args :path "path")))

(defun file-checkpoint-content-hash (content)
  "Return a stable hexadecimal FNV-1a hash for CONTENT."
  (let ((hash #xCBF29CE484222325)
        (prime #x100000001B3))
    (loop :for char :across (or content "")
          :do (setf hash (logand #xffffffffffffffff
                                 (* (logxor hash (char-code char)) prime))))
    (format nil "~16,'0X" hash)))

(defun file-checkpoint-snapshot (pathname)
  "Return a compact text snapshot for PATHNAME."
  (let ((exists-p (and pathname (probe-file pathname))))
    (if exists-p
        (handler-case
            (let* ((content (uiop:read-file-string pathname))
                   (size (length content)))
              (list :exists-p t
                    :size size
                    :hash (file-checkpoint-content-hash content)
                    :content content))
          (error (condition)
            (list :exists-p t
                  :read-error (format nil "~A" condition))))
        (list :exists-p nil))))

(defun file-checkpoint-snapshot-public (snapshot)
  "Return SNAPSHOT without full file content for durable events."
  (list :exists-p (getf snapshot :exists-p)
        :size (getf snapshot :size)
        :hash (getf snapshot :hash)
        :read-error (getf snapshot :read-error)))

(defun file-checkpoint-snapshot-event-fields (prefix snapshot)
  "Return flat durable event fields for SNAPSHOT under PREFIX."
  (let ((name (string-downcase (symbol-name prefix))))
    (flet ((key (suffix)
             (intern (string-upcase (format nil "~A-~A" name suffix)) :keyword)))
      `((,(key "exists-p") . ,(getf snapshot :exists-p))
        (,(key "size") . ,(getf snapshot :size))
        (,(key "hash") . ,(getf snapshot :hash))
        (,(key "read-error") . ,(getf snapshot :read-error))))))

(defun file-checkpoint-diff (before after)
  "Return a bounded diff from BEFORE to AFTER snapshots when readable."
  (let ((before-content (and (getf before :exists-p)
                             (getf before :content)))
        (after-content (and (getf after :exists-p)
                            (getf after :content))))
    (when (or before-content after-content)
      (bounded-tool-execution-string
       (compute-simple-diff before-content (or after-content ""))))))

(defun file-checkpoint-context (tool-name args)
  "Return checkpoint context for TOOL-NAME and ARGS, or NIL."
  (when (file-checkpoint-tool-p tool-name)
    (let ((path (file-checkpoint-path-argument args)))
      (when path
        (handler-case
            (let ((resolved (validate-sandbox-path path)))
              (list :path path
                    :resolved-path (namestring resolved)))
          (error (condition)
            (list :path path
                  :resolve-error (format nil "~A" condition))))))))

(defun record-file-checkpoint-event
    (buffer phase tool-name args context before
            &key tool-id after status condition)
  "Record one before/after file checkpoint event for a mutating tool."
  (declare (ignore args))
  (when context
    (record-tool-execution-event
     buffer
     `((:event . "file-checkpoint")
       (:phase . ,phase)
       (:tool-name . ,(normalize-tool-name tool-name))
       ,@(when tool-id `((:tool-id . ,tool-id)))
       (:caller . ,(tool-execution-caller-name))
       (:timestamp . ,(get-universal-time))
       (:path . ,(getf context :path))
       ,@(when (getf context :resolved-path)
           `((:resolved-path . ,(getf context :resolved-path))))
       ,@(when (getf context :resolve-error)
           `((:resolve-error . ,(getf context :resolve-error))))
       ,@(when status `((:status . ,status)))
       ,@(when before
           (file-checkpoint-snapshot-event-fields :before before))
       ,@(when after
           (file-checkpoint-snapshot-event-fields :after after))
       ,@(when (and before after)
           `((:diff . ,(file-checkpoint-diff before after))))
       ,@(when condition
           `((:condition-type . ,(condition-type-name condition))
             (:condition-message . ,(bounded-tool-execution-string
                                     (format nil "~A" condition)))))))))

(defun lisp-eval-tool-p (tool-name)
  "Return true when TOOL-NAME is the live Lisp evaluation tool."
  (string= "lisp_eval" (normalize-tool-name tool-name)))

(defun lisp-eval-recovery-mode (args)
  "Return the lisp_eval mode as a durable string."
  (let ((mode (or (ignore-errors (tool-arg args :mode "mode")) "live")))
    (cond
      ((symbolp mode) (string-downcase (symbol-name mode)))
      ((stringp mode) (string-downcase mode))
      (t (bounded-tool-execution-string mode)))))

(defun lisp-eval-recovery-context (tool-name args)
  "Return semantic recovery context for lisp_eval ARGS, or NIL."
  (when (lisp-eval-tool-p tool-name)
    (list :mode (lisp-eval-recovery-mode args)
          :package (or (ignore-errors (tool-arg args :package "package"))
                       *lisp-eval-default-package*)
          :code (or (ignore-errors (tool-arg args :code "code")) ""))))

(defun record-lisp-eval-recovery-event
    (buffer phase tool-name args context &key tool-id status result condition)
  "Record semantic lisp_eval recovery events around live or worker evals.
The before event is intentionally durable before evaluation starts, so startup
recovery can detect an interrupted live eval without reducing eval capability."
  (declare (ignore args))
  (when context
    (record-tool-execution-event
     buffer
     `((:event . "lisp-eval-checkpoint")
       (:phase . ,phase)
       (:tool-name . ,(normalize-tool-name tool-name))
       ,@(when tool-id `((:tool-id . ,tool-id)))
       (:caller . ,(tool-execution-caller-name))
       (:timestamp . ,(get-universal-time))
       (:mode . ,(getf context :mode))
       (:package . ,(getf context :package))
       (:code . ,(bounded-tool-execution-string (getf context :code)))
       ,@(when status `((:status . ,status)))
       ,@(when result
           `((:result . ,(bounded-tool-execution-string result))))
       ,@(when condition
           `((:condition-type . ,(condition-type-name condition))
             (:condition-message . ,(bounded-tool-execution-string
                                     (format nil "~A" condition)))))))))

(defun execute-tool-safely (name args &key buffer tool-id denied-reason)
  "Execute tool NAME with ARGS and return a provider-compatible result string.
All agent tool paths should use this wrapper so tool start/result/error events
are journaled before control returns to the provider loop.  DENIED-REASON records
a denied tool call without invoking the tool, preserving the same durable event
shape as successful and failed executions."
  (let* ((buf (or buffer *current-tool-buffer*))
         (eval-context (and (not denied-reason)
                            (lisp-eval-recovery-context name args)))
         (checkpoint-context (and (not denied-reason)
                                  (file-checkpoint-context name args)))
         (checkpoint-before
           (and checkpoint-context
                (getf checkpoint-context :resolved-path)
                (file-checkpoint-snapshot
                 (pathname (getf checkpoint-context :resolved-path))))))
    (record-tool-execution-event
     buf
     (tool-execution-event "start" name args
                           :buffer buf
                           :tool-id tool-id))
    (when eval-context
      (record-lisp-eval-recovery-event
       buf "before" name args eval-context
       :tool-id tool-id))
    (when checkpoint-context
      (record-file-checkpoint-event
       buf "before" name args checkpoint-context checkpoint-before
       :tool-id tool-id))
    (cond
      (denied-reason
       (let ((result (tool-denied-result-data denied-reason)))
         (record-tool-execution-event
          buf
          (tool-execution-event "result" name args
                                :buffer buf
                                :tool-id tool-id
                                :status "denied"
                                :reason denied-reason
                                :result result))
         result))
      (t
       (handler-case
           (let ((result (execute-tool name args)))
             (when eval-context
               (record-lisp-eval-recovery-event
                buf "after" name args eval-context
                :tool-id tool-id
                :status "ok"
                :result result))
             (when checkpoint-context
               (record-file-checkpoint-event
                buf "after" name args checkpoint-context checkpoint-before
                :tool-id tool-id
                :status "ok"
                :after (and (getf checkpoint-context :resolved-path)
                            (file-checkpoint-snapshot
                             (pathname (getf checkpoint-context
                                             :resolved-path))))))
             (record-tool-execution-event
              buf
              (tool-execution-event "result" name args
                                    :buffer buf
                                    :tool-id tool-id
                                    :status "ok"
                                    :result result))
             result)
         (error (condition)
           (let ((result (tool-error-result-data condition)))
             (when eval-context
               (record-lisp-eval-recovery-event
                buf "after" name args eval-context
                :tool-id tool-id
                :status "error"
                :result result
                :condition condition))
             (when checkpoint-context
               (record-file-checkpoint-event
                buf "after" name args checkpoint-context checkpoint-before
                :tool-id tool-id
                :status "error"
                :after (and (getf checkpoint-context :resolved-path)
                            (file-checkpoint-snapshot
                             (pathname (getf checkpoint-context
                                             :resolved-path))))
                :condition condition))
             (record-tool-execution-event
              buf
              (tool-execution-event "result" name args
                                    :buffer buf
                                    :tool-id tool-id
                                    :status "error"
                                    :result result
                                    :condition condition))
             result)))))))

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
    (unless (approval-policy-tool-sandbox-allowed-p
             def
             :buffer *current-tool-buffer*)
      (approval-policy-record-history-entry
       *current-tool-buffer* normalized-name :denied
       :policy (effective-tool-sandbox-preset def
                                              :buffer *current-tool-buffer*)
       :reason "Denied by sandbox preset"
       :entry args)
      (error "Tool ~A is not allowed by the sandbox preset" normalized-name))
    (unless (approval-policy-tool-network-allowed-p
             def
             :buffer *current-tool-buffer*)
      (approval-policy-record-history-entry
       *current-tool-buffer* normalized-name :denied
       :policy (effective-tool-network-toggle def
                                             :buffer *current-tool-buffer*)
       :reason "Denied by network policy"
       :entry args)
      (error "Tool ~A is not allowed by the network policy" normalized-name))
    (unless (approval-policy-tool-working-directory-allowed-p
             def
             :buffer *current-tool-buffer*)
      (approval-policy-record-history-entry
       *current-tool-buffer* normalized-name :denied
       :policy (effective-tool-working-directory-policy
                def :buffer *current-tool-buffer*)
       :reason "Denied by working-directory policy"
       :entry args)
      (error "Tool ~A is not allowed in the current working directory"
             normalized-name))
    (let ((perm (effective-tool-permission def :buffer *current-tool-buffer*)))
      (ecase perm
        (:agent-allowed t)
        (:agent-with-permission t)  ; caller is responsible for approval check
        (:user-only
         (unless (eq *current-caller* :user)
           (approval-policy-record-history-entry
            *current-tool-buffer* normalized-name :denied
            :policy perm
            :reason "Denied by approval policy"
            :entry args)
           (error "Tool ~A is user-only" normalized-name)))))
    (approval-policy-record-history-entry
     *current-tool-buffer* normalized-name :allowed
     :policy (effective-tool-permission def :buffer *current-tool-buffer*)
     :reason "Tool execution allowed"
     :entry args)
    (run-hook-with-args '*before-tool-hook* normalized-name args)
    (let ((result (funcall (tool-definition-execute-fn def) args)))
      (run-hook-with-args '*after-tool-hook* normalized-name args result)
      result)))

(defun approval-policy-entry-summary (entry)
  "Return a readable summary for an approval audit ENTRY."
  (with-output-to-string (s)
    (format s "timestamp=~A buffer=~A cwd=~A tool=~A decision=~A policy=~A"
            (cdr (assoc :timestamp entry))
            (or (cdr (assoc :buffer entry)) "unknown")
            (or (cdr (assoc :working-directory entry)) "unknown")
            (or (cdr (assoc :tool-name entry)) "unknown")
            (or (cdr (assoc :decision entry)) "unknown")
            (or (cdr (assoc :policy entry)) "unknown"))
    (when (cdr (assoc :reason entry))
      (format s " reason=~A" (cdr (assoc :reason entry))))
    (when (cdr (assoc :entry entry))
      (format s "~%  entry: ~S" (cdr (assoc :entry entry))))))

(defun approval-policy-audit-value-string (value)
  "Return VALUE as a lower-case audit display string."
  (if value
      (string-downcase (symbol-name value))
      "inherit"))

(defun approval-policy-overrides-section-to-string (title table)
  "Return one guard policy override section."
  (let ((entries nil))
    (maphash (lambda (name value)
               (push (cons name value) entries))
             table)
    (with-output-to-string (s)
      (when entries
        (format s "~A~%" title)
        (dolist (entry (sort entries #'string< :key #'car))
          (format s "  ~A => ~A~%"
                  (car entry)
                  (approval-policy-audit-value-string
                   (cdr entry))))))))

(defun approval-policy-history-to-string (&key buffer directory (limit 25))
  "Return a human-readable summary of the selected approval policy."
  (let* ((registry (approval-policy-registry-for-context :buffer buffer
                                                         :directory directory))
         (path (approval-policy-path-for-registry :buffer buffer
                                                  :directory directory))
         (history (approval-policy-history registry))
         (limit (max 0 limit)))
    (with-output-to-string (s)
      (format s "Approval Policy~%")
      (format s "Path: ~A~%" (or (ignore-errors (namestring path)) path))
      (when buffer
        (format s "Buffer: ~A~%" (buffer-name buffer)))
      (format s "Default permission: ~A~%"
              (approval-policy-audit-value-string
               (or (getf registry :default) :agent-allowed)))
      (format s "Sandbox default: ~A~%"
              (approval-policy-audit-value-string
               (or (approval-policy-sandbox-default registry)
                   :workspace-write)))
      (format s "Network default: ~A~%"
               (approval-policy-audit-value-string
                (or (approval-policy-network-default registry) :allow)))
      (format s "Working-directory default: ~A~%"
               (approval-policy-audit-value-string
                (or (approval-policy-working-directory-default registry)
                    :project-root)))
      (write-string (approval-policy-overrides-section-to-string
                     "Approval overrides:"
                     (approval-policy-tools registry))
                    s)
      (write-string (approval-policy-overrides-section-to-string
                     "Sandbox overrides:"
                     (approval-policy-sandbox-tools registry))
                    s)
      (write-string (approval-policy-overrides-section-to-string
                     "Network overrides:"
                     (approval-policy-network-tools registry))
                    s)
      (write-string (approval-policy-overrides-section-to-string
                     "Working-directory overrides:"
                     (approval-policy-working-directory-tools registry))
                    s)
      (if history
          (progn
            (format s "Recent decisions:~%")
            (dolist (entry (subseq history 0 (min limit (length history))))
              (format s "  - ~A~%" (approval-policy-entry-summary entry))))
          (format s "Recent decisions: none~%")))))

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

(defun recovery-event-value (event key)
  "Return KEY's decoded JSON value from EVENT."
  (cdr (assoc key event)))

(defun recovery-event-kind-p (event kind)
  "Return true when EVENT matches recovery event KIND."
  (let ((event-name (recovery-event-value event :event)))
    (or (string= kind "all")
        (and (string= kind "tool")
             (string= event-name "tool-execution"))
        (and (string= kind "file")
             (string= event-name "file-checkpoint"))
        (and (string= kind "lisp-eval")
             (string= event-name "lisp-eval-checkpoint")))))

(defun recovery-event-key (event)
  "Return a stable-enough pairing key for before/after recovery events."
  (or (recovery-event-value event :tool-id)
      (format nil "~A/~A/~A"
              (recovery-event-value event :tool-name)
              (recovery-event-value event :timestamp)
              (recovery-event-value event :code))))

(defun recovery-summarize-event (event index)
  "Return a compact Lisp data summary for recovery EVENT."
  (list :index index
        :event (recovery-event-value event :event)
        :phase (recovery-event-value event :phase)
        :status (recovery-event-value event :status)
        :tool-name (recovery-event-value event :tool-name)
        :tool-id (recovery-event-value event :tool-id)
        :timestamp (recovery-event-value event :timestamp)
        :mode (recovery-event-value event :mode)
        :package (recovery-event-value event :package)
        :path (recovery-event-value event :path)
        :code (recovery-event-value event :code)
        :condition-message (recovery-event-value event :condition-message)
        :diff (recovery-event-value event :diff)))

(defun recovery-pending-lisp-evals (events)
  "Return lisp_eval before checkpoints without a matching after checkpoint."
  (let ((pending (make-hash-table :test #'equal)))
    (dolist (event events)
      (when (string= "lisp-eval-checkpoint"
                     (or (recovery-event-value event :event) ""))
        (let ((key (recovery-event-key event))
              (phase (recovery-event-value event :phase)))
          (cond
            ((string= phase "before")
             (setf (gethash key pending) event))
            ((string= phase "after")
             (remhash key pending))))))
    (let ((result nil))
      (maphash (lambda (_ event)
                 (declare (ignore _))
                 (push event result))
               pending)
      (sort result #'< :key (lambda (event)
                              (or (recovery-event-value event :timestamp) 0))))))

(defun recovery-relevant-events (events kind)
  "Return transcript EVENTS relevant to recovery KIND."
  (remove-if-not (lambda (event)
                   (recovery-event-kind-p event kind))
                 events))

(defun execute-recovery-list (args)
  "List recent recovery journal events for the current buffer session."
  (let* ((buffer (or *current-tool-buffer*
                     (ignore-errors (current-buffer))))
         (limit (or (tool-arg args :limit "limit") 20))
         (kind (string-downcase (or (tool-arg args :kind "kind") "all")))
         (pending-only (tool-arg args :pending-only "pending-only")))
    (unless buffer
      (error "recovery_list requires an active buffer"))
    (unless (member kind '("all" "tool" "file" "lisp-eval") :test #'string=)
      (error "kind must be one of all, tool, file, or lisp-eval"))
    (ensure-buffer-session buffer)
    (let* ((events (session-transcript-events (buffer-session buffer)))
           (pending (recovery-pending-lisp-evals events))
           (relevant (if pending-only
                         pending
                         (recovery-relevant-events events kind)))
           (recent (subseq (reverse relevant)
                           0
                           (min (max 0 limit) (length relevant)))))
      (lisp-data-string
       (list :session-id (session-id (buffer-session buffer))
             :kind kind
             :pending-lisp-evals
             (mapcar #'recovery-event-key pending)
             :event-count (length relevant)
             :events (loop :for event :in recent
                           :for index :from 0
                           :collect (recovery-summarize-event event index)))))))

(deftool execute-recovery-list
  :name "recovery_list"
  :description "List recent durable tool, file checkpoint, and lisp_eval recovery journal events for the current session. Use this after errors, crashes, interrupted live evals, or risky self-modification."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((kind :type "string"
               :required nil
               :description "Optional filter: all, tool, file, or lisp-eval. Default: all.")
         (limit :type "integer"
                :required nil
                :description "Maximum number of recent events to return. Default: 20.")
         (pending-only :type "boolean"
                       :required nil
                       :description "If true, return only lisp_eval before checkpoints without matching after checkpoints.")))

(deftool execute-lisp-eval
  :name "lisp_eval"
  :description "Evaluate one Common Lisp form in the running clawmacs process for testing, introspection, live system updates, or defining helper tools."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((code :type "string"
               :description "Lisp data :code, one Common Lisp form to read and evaluate.")
         (package :type "string"
                  :required nil
                  :description "Lisp data :package, the package name used while reading and evaluating :code. Default: CLAWMACS.")
         (mode :type "string"
               :required nil
               :description "Optional execution mode: live evaluates in the running Clawmacs image; isolated evaluates in a fresh SBCL worker process. Default: live.")
         (timeout :type "integer"
                  :required nil
                  :description "Timeout in seconds for isolated mode. Default: 10.")))

(defun init-tools ()
  "Register the default clawmacs built-in tools.
This removes any previously registered built-in entries, then re-registers
tagged agent tools. User-added tools remain untouched."

  (dolist (name *built-in-tool-names*)
    (remhash name *tool-table*))

  (setf *lisp-eval-default-package* "CLAWMACS")
  (register-agent-tool-provider-definitions))
