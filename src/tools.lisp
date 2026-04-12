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
  (approval-display-fn nil     :type (or null function)))

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

(defvar *http-fetch-max-chars* 50000
  "Default maximum characters returned by http_fetch.")

(defvar *http-connection-timeout* 15
  "Connection timeout in seconds for HTTP requests.")

(defvar *http-user-agent* "Clawmacs/0.1"
  "User-Agent header for HTTP requests.")

(defvar *shell-exec-default-timeout* 30
  "Default timeout in seconds for shell_exec.")

(defparameter *built-in-tool-names*
  '("http_fetch" "file_read" "file_write" "file_edit" "shell_exec"
    "read" "find" "grep" "write" "edit" "lisp_eval")
  "Names reserved for clawmacs built-in tools.
INIT-TOOLS removes these entries before re-registering the default built-ins,
so user-added tools stored in *tool-table* are left intact.")

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
                           (tool-definition-approval-display-fn tool)))
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
                      &key approval-display-fn)
  "Register a tool in *tool-table*.
APPROVAL-DISPLAY-FN, if provided, is called with (args) during permission
approval to generate extra display context (e.g., file diffs)."
  (let ((normalized-name (normalize-tool-name name)))
    (setf (gethash normalized-name *tool-table*)
          (make-tool-definition :name normalized-name
                                :description description
                                :input-schema schema
                                :permission permission
                                :execute-fn execute-fn
                                :approval-display-fn approval-display-fn))))

(defun tool-allowed-for-active-run-p (name)
  "Return true when NAME is allowed by *ACTIVE-TOOL-NAMES*."
  (or (null *active-tool-names*)
      (member (normalize-tool-name name) *active-tool-names* :test #'string=)))

(defun tool-visible-to-caller-p (definition)
  "Return true when DEFINITION is visible to *CURRENT-CALLER*."
  (let ((perm (tool-definition-permission definition)))
    (or (eq *current-caller* :user)
        (eq perm :agent-allowed)
        (eq perm :agent-with-permission))))

(defun tool-definitions-for-api ()
  "Return a vector of clawmacs tool definitions for provider adapters.
Only includes tools visible to the current *current-caller*."
  (let ((tools nil))
    (map-effective-tool-definitions
     (lambda (name def)
       (declare (ignore name))
       (when (and (tool-visible-to-caller-p def)
                  (tool-allowed-for-active-run-p
                   (tool-definition-name def)))
         (push `((:name . ,(tool-definition-name def))
                 (:description . ,(tool-definition-description def))
                 (:input--schema . ,(tool-definition-input-schema def)))
               tools))))
    (coerce tools 'vector)))

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

(defun init-tools ()
  "Register the default clawmacs built-in tools.
This removes any previously registered built-in entries, then re-registers
read, find, grep, write, edit, and lisp_eval. User-added tools remain untouched."

  (dolist (name *built-in-tool-names*)
    (remhash name *tool-table*))

  (setf *lisp-eval-default-package* "CLAWMACS")
  (dolist (spec (lispi:default-tool-specs))
    (apply #'register-tool
           (getf spec :name)
           (getf spec :description)
           (getf spec :schema)
           (getf spec :permission)
           (getf spec :execute-fn)
           (when (getf spec :approval-display-fn)
             (list :approval-display-fn
                   (getf spec :approval-display-fn))))))
