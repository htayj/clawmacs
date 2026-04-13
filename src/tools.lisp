(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Tool Registry
;;; --------------------------------------------------------------------------

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

(defvar *http-fetch-max-chars* 50000
  "Default maximum characters returned by http_fetch.")

(defvar *http-connection-timeout* 15
  "Connection timeout in seconds for HTTP requests.")

(defvar *http-user-agent* "Clawmacs/0.1"
  "User-Agent header for HTTP requests.")

(defvar *shell-exec-default-timeout* 30
  "Default timeout in seconds for shell_exec.")

(defparameter *built-in-tool-names*
  '("lisp_eval")
  "Names reserved for core Clawmacs provider tools.
INIT-TOOLS removes these entries before re-registering tagged tools, so
user-added tools stored in *tool-table* are left intact.")

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
                         :permission permission
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
                                :permission permission
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
  (let ((perm (tool-definition-permission definition)))
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
    (coerce tools 'vector)))

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

(defun tool-requires-permission-p (name)
  "Return T if tool NAME requires user permission."
  (let ((def (effective-tool-definition name)))
    (and def (eq :agent-with-permission (tool-definition-permission def)))))

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
    (let ((perm (tool-definition-permission def)))
      (ecase perm
        (:agent-allowed t)
        (:agent-with-permission t)  ; caller is responsible for approval check
        (:user-only
         (unless (eq *current-caller* :user)
           (error "Tool ~A is user-only" normalized-name)))))
    (funcall (tool-definition-execute-fn def) args)))

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
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program (list "sh" "-c" command)
                          :directory sandbox
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore timeout)) ; TODO: implement actual timeout
      (cl-json:encode-json-to-string
       `((:command . ,command)
         (:exit--code . ,exit-code)
         (:stdout . ,stdout)
         (:stderr . ,stderr))))))

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
