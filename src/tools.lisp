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
  (execute-fn  nil             :type (or null function)))

(defvar *tool-table* (make-hash-table :test #'equal)
  "Global table mapping tool name strings to tool-definition structs.")

(defun register-tool (name description schema permission execute-fn)
  "Register a tool in *tool-table*."
  (setf (gethash name *tool-table*)
        (make-tool-definition :name name
                              :description description
                              :input-schema schema
                              :permission permission
                              :execute-fn execute-fn)))

(defun tool-definitions-for-api ()
  "Return a vector of tool definitions formatted for the Anthropic API.
Only includes tools visible to the current *current-caller*."
  (let ((tools nil))
    (maphash (lambda (name def)
               (declare (ignore name))
               (let ((perm (tool-definition-permission def)))
                 (when (or (eq *current-caller* :user)
                           (eq perm :agent-allowed)
                           (eq perm :agent-with-permission))
                   (push `((:name . ,(tool-definition-name def))
                           (:description . ,(tool-definition-description def))
                           (:input--schema . ,(tool-definition-input-schema def)))
                         tools))))
             *tool-table*)
    (coerce tools 'vector)))

(defun tool-requires-permission-p (name)
  "Return T if tool NAME requires user permission."
  (let ((def (gethash name *tool-table*)))
    (and def (eq :agent-with-permission (tool-definition-permission def)))))

(defun execute-tool (name args)
  "Execute tool NAME with ARGS (an alist of parameter values).
Returns a string result or signals an error."
  (let ((def (gethash name *tool-table*)))
    (unless def
      (error "Unknown tool: ~A" name))
    (let ((perm (tool-definition-permission def)))
      (ecase perm
        (:agent-allowed t)
        (:agent-with-permission t)  ; caller is responsible for approval check
        (:user-only
         (unless (eq *current-caller* :user)
           (error "Tool ~A is user-only" name)))))
    (funcall (tool-definition-execute-fn def) args)))

(defun format-tool-call-sexpr (name args)
  "Format a tool call as a raw s-expression string.
E.g., (shell_exec :command \"ls -la\")"
  (with-output-to-string (s)
    (format s "(~A" name)
    (loop :for (k . v) :in args
          :do (format s " :~A ~S"
                      (string-downcase (symbol-name k)) v))
    (write-char #\) s)))

(defun format-tool-call-expanded (name args)
  "Format a tool call with expanded parameter descriptions.
E.g., (shell_exec
        :command \"ls -la\"  ; The shell command to execute
        :timeout 30)         ; Timeout in seconds"
  (let ((def (gethash name *tool-table*))
        (schema-props nil))
    ;; Extract property descriptions from schema
    (when def
      (let ((schema (tool-definition-input-schema def)))
        (when schema
          (let ((props (cdr (assoc :properties schema))))
            (when props
              (setf schema-props props))))))
    (with-output-to-string (s)
      (format s "(~A" name)
      (loop :for (k . v) :in args
            :for param-name := (string-downcase (symbol-name k))
            :for desc := (let ((prop (cdr (assoc k schema-props))))
                           (when prop (cdr (assoc :description prop))))
            :do (format s "~%  :~A ~S" param-name v)
                (when desc
                  (format s "  ; ~A" desc)))
      (write-char #\) s))))

;;; --------------------------------------------------------------------------
;;; Sandbox Path Validation
;;; --------------------------------------------------------------------------

(defun validate-sandbox-path (path)
  "Validate that PATH is within *sandbox-root*. Returns the resolved pathname.
Signals an error if the path escapes the sandbox."
  (let* ((sandbox (or *sandbox-root* (truename ".")))
         (resolved (merge-pathnames (pathname path) sandbox))
         (resolved-str (namestring (truename resolved)))
         (sandbox-str (namestring sandbox)))
    (unless (alexandria:starts-with-subseq sandbox-str resolved-str)
      (error "Path ~A is outside the sandbox (~A)" path sandbox-str))
    resolved))

;;; --------------------------------------------------------------------------
;;; HTTP Fetch Tool
;;; --------------------------------------------------------------------------

(defun execute-http-fetch (args)
  "Fetch content from an HTTP/HTTPS URL. Returns the response body as text."
  (let ((url (cdr (assoc :url args)))
        (max-chars (or (cdr (assoc :max--chars args)) 50000)))
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
                             :connection-timeout 15
                             :user-agent "Clawmacs/0.1")
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
;;; File Read Tool
;;; --------------------------------------------------------------------------

(defun execute-file-read (args)
  "Read a file within the sandbox. Returns file contents as text."
  (let* ((path (cdr (assoc :path args)))
         (offset (or (cdr (assoc :offset args)) 0))
         (limit (or (cdr (assoc :limit args)) 10000))
         (resolved (validate-sandbox-path path)))
    (unless path
      (error "path parameter is required"))
    (unless (probe-file resolved)
      (error "File not found: ~A" path))
    (let* ((content (uiop:read-file-string resolved))
           (lines (loop :for start := 0 :then (1+ pos)
                        :for pos := (position #\Newline content :start start)
                        :collect (subseq content start (or pos (length content)))
                        :while pos))
           (total-lines (length lines))
           (selected (subseq lines
                             (min offset total-lines)
                             (min (+ offset limit) total-lines)))
           (result (format nil "~{~A~^~%~}" selected)))
      (cl-json:encode-json-to-string
       `((:path . ,path)
         (:total--lines . ,total-lines)
         (:offset . ,offset)
         (:lines--returned . ,(length selected))
         (:content . ,result))))))

;;; --------------------------------------------------------------------------
;;; File Write Tool
;;; --------------------------------------------------------------------------

(defun execute-file-write (args)
  "Write content to a file within the sandbox."
  (let* ((path (cdr (assoc :path args)))
         (content (cdr (assoc :content args)))
         (resolved (validate-sandbox-path path)))
    (unless path
      (error "path parameter is required"))
    (unless content
      (error "content parameter is required"))
    (ensure-directories-exist resolved)
    (with-open-file (s resolved
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-string content s))
    (cl-json:encode-json-to-string
     `((:path . ,path)
       (:bytes--written . ,(length content))
       (:status . "ok")))))

;;; --------------------------------------------------------------------------
;;; Shell Exec Tool
;;; --------------------------------------------------------------------------

(defun execute-shell-exec (args)
  "Execute a shell command within the sandbox directory."
  (let* ((command (cdr (assoc :command args)))
         (timeout (or (cdr (assoc :timeout args)) 30))
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
;;; Lisp Eval Tool
;;; --------------------------------------------------------------------------

(defun execute-lisp-eval (args)
  "Evaluate arbitrary Common Lisp code. Returns the result as a string."
  (let* ((code (cdr (assoc :code args)))
         (package-name (or (cdr (assoc :package args)) "CLAWMACS")))
    (unless code
      (error "code parameter is required"))
    (let ((*package* (or (find-package (string-upcase package-name))
                         (find-package :cl-user))))
      (handler-case
          (let* ((form (read-from-string code))
                 (results (multiple-value-list (eval form)))
                 (output (format nil "~{~S~^~%~}" results)))
            (cl-json:encode-json-to-string
             `((:code . ,code)
               (:result . ,output)
               (:values . ,(length results)))))
        (error (e)
          (cl-json:encode-json-to-string
           `((:code . ,code)
             (:error . ,(format nil "~A" e)))))))))

;;; --------------------------------------------------------------------------
;;; Tool Registration
;;; --------------------------------------------------------------------------

(defun init-tools ()
  "Register all built-in tools."

  (register-tool
   "http_fetch"
   "Fetch content from an HTTP or HTTPS URL. Returns the response body as text with metadata."
   `((:type . "object")
     (:properties
      . ((:url . ((:type . "string")
                  (:description . "The HTTP or HTTPS URL to fetch.")))
         (:max--chars . ((:type . "integer")
                         (:description . "Maximum characters to return. Default 50000.")
                         (:minimum . 100)))))
     (:required . #("url")))
   :agent-allowed
   #'execute-http-fetch)

  (register-tool
   "file_read"
   "Read a file from the working directory. Returns the file contents with line offset support for reading large files in chunks."
   `((:type . "object")
     (:properties
      . ((:path . ((:type . "string")
                   (:description . "File path relative to the working directory.")))
         (:offset . ((:type . "integer")
                     (:description . "Line number to start reading from (0-indexed). Default 0.")))
         (:limit . ((:type . "integer")
                    (:description . "Maximum number of lines to return. Default 10000.")))))
     (:required . #("path")))
   :agent-allowed
   #'execute-file-read)

  (register-tool
   "file_write"
   "Write content to a file in the working directory. Creates parent directories if needed. Overwrites existing files."
   `((:type . "object")
     (:properties
      . ((:path . ((:type . "string")
                   (:description . "File path relative to the working directory.")))
         (:content . ((:type . "string")
                      (:description . "The content to write to the file.")))))
     (:required . #("path" "content")))
   :agent-with-permission
   #'execute-file-write)

  (register-tool
   "shell_exec"
   "Execute a shell command in the working directory. Returns stdout, stderr, and exit code."
   `((:type . "object")
     (:properties
      . ((:command . ((:type . "string")
                      (:description . "The shell command to execute.")))
         (:timeout . ((:type . "integer")
                      (:description . "Timeout in seconds. Default 30.")))))
     (:required . #("command")))
   :agent-with-permission
   #'execute-shell-exec)

  (register-tool
   "lisp_eval"
   "Evaluate arbitrary Common Lisp code in the clawmacs process. Returns the result of evaluation. Use this for computation, data transformation, or interacting with the running system."
   `((:type . "object")
     (:properties
      . ((:code . ((:type . "string")
                   (:description . "The Common Lisp code to evaluate.")))
         (:package . ((:type . "string")
                      (:description . "Package to evaluate in. Default: CLAWMACS.")))))
     (:required . #("code")))
   :agent-allowed
   #'execute-lisp-eval))
