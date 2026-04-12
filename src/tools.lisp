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

(defvar *file-read-default-limit* 10000
  "Default maximum lines returned by file_read.")

(defvar *shell-exec-default-timeout* 30
  "Default timeout in seconds for shell_exec.")

(defvar *diff-display-max-lines* 40
  "Maximum diff lines shown in the approval UI.")

(defparameter *built-in-tool-names*
  '("http_fetch" "file_read" "file_write" "file_edit" "shell_exec" "lisp_eval")
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
  (with-output-to-string (s)
    (format s "(~A" name)
    (loop :for (k . v) :in args
          :do (format s " :~A ~S"
                      (string-downcase (symbol-name k)) v))
    (write-char #\) s)))

(defun format-tool-call-expanded (name args)
  "Format a tool call with expanded parameter descriptions.
E.g., (lisp_eval
        :code \"(list-functions)\")  ; The Common Lisp code to evaluate"
  (let ((def (effective-tool-definition name))
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
;;; File Diff for Approval Display
;;; --------------------------------------------------------------------------

(defun compute-simple-diff (old-text new-text)
  "Compute a simple line-by-line diff between OLD-TEXT and NEW-TEXT.
Returns a string with +/- prefixed lines. Not a full unified diff,
just enough to show what changes."
  (let ((old-lines (if old-text
                       (loop :for start := 0 :then (1+ pos)
                             :for pos := (position #\Newline old-text :start start)
                             :collect (subseq old-text start (or pos (length old-text)))
                             :while pos)
                       nil))
        (new-lines (loop :for start := 0 :then (1+ pos)
                         :for pos := (position #\Newline new-text :start start)
                         :collect (subseq new-text start (or pos (length new-text)))
                         :while pos)))
    (with-output-to-string (s)
      (cond
        ;; New file
        ((null old-lines)
         (format s "--- (new file)~%+++ ~A~%" "new")
         (dolist (line new-lines)
           (format s "+~A~%" line)))
        ;; Show removed and added lines
        (t
         (format s "--- old~%+++ new~%")
         ;; Simple approach: show lines unique to old as -, unique to new as +,
         ;; common lines as context
         (let ((max-lines *diff-display-max-lines*)
               (old-set (make-hash-table :test #'equal))
               (new-set (make-hash-table :test #'equal)))
           (dolist (l old-lines) (setf (gethash l old-set) t))
           (dolist (l new-lines) (setf (gethash l new-set) t))
           ;; Show old lines not in new
           (let ((removed (remove-if (lambda (l) (gethash l new-set)) old-lines)))
             (dolist (line (subseq removed 0 (min (length removed) max-lines)))
               (format s "-~A~%" line)))
           ;; Show new lines not in old
           (let ((added (remove-if (lambda (l) (gethash l old-set)) new-lines)))
             (dolist (line (subseq added 0 (min (length added) max-lines)))
               (format s "+~A~%" line)))))))))

(defun tool-approval-extra-display (name args)
  "Get extra display text for a tool's approval prompt.
Returns a string or nil. Calls the tool's approval-display-fn if set."
  (let ((def (effective-tool-definition name)))
    (when (and def (tool-definition-approval-display-fn def))
      (handler-case
          (funcall (tool-definition-approval-display-fn def) args)
        (error () nil)))))

;;; --------------------------------------------------------------------------
;;; Sandbox Path Validation
;;; --------------------------------------------------------------------------

(defun validate-sandbox-path (path)
  "Validate that PATH is within *sandbox-root*. Returns the resolved pathname.
Signals an error if the path escapes the sandbox.
Works for both existing and not-yet-existing files."
  (let* ((sandbox (or *sandbox-root* (truename ".")))
         (resolved (merge-pathnames (pathname path) sandbox))
         ;; For validation, resolve the directory part (which must exist)
         ;; and check the full path string for sandbox containment.
         (dir (make-pathname :directory (pathname-directory resolved)
                             :device (pathname-device resolved)))
         (dir-str (namestring (if (probe-file dir)
                                  (truename dir)
                                  dir)))
         (sandbox-str (namestring sandbox)))
    (unless (alexandria:starts-with-subseq sandbox-str dir-str)
      (error "Path ~A is outside the sandbox (~A)" path sandbox-str))
    resolved))

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
;;; File Read Tool
;;; --------------------------------------------------------------------------

(defun execute-file-read (args)
  "Read a file within the sandbox. Returns file contents as text."
  (let* ((path (cdr (assoc :path args)))
         (offset (or (cdr (assoc :offset args)) 0))
         (limit (or (cdr (assoc :limit args)) *file-read-default-limit*))
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
;;; File Write Tool (create new or append, never overwrite)
;;; --------------------------------------------------------------------------

(defun execute-file-write (args)
  "Write content to a file within the sandbox.
If the file does not exist, creates it. If it exists, appends to it.
Never overwrites existing content."
  (let* ((path (cdr (assoc :path args)))
         (content (cdr (assoc :content args)))
         (resolved (validate-sandbox-path path)))
    (unless path
      (error "path parameter is required"))
    (unless content
      (error "content parameter is required"))
    (ensure-directories-exist resolved)
    (let ((existed (probe-file resolved)))
      (with-open-file (s resolved
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
        (write-string content s))
      (cl-json:encode-json-to-string
       `((:path . ,path)
         (:bytes--written . ,(length content))
         (:mode . ,(if existed "appended" "created"))
         (:status . "ok"))))))

(defun file-write-approval-display (args)
  "Approval display for file_write: shows what will be appended or created."
  (let* ((path (cdr (assoc :path args)))
         (new-content (cdr (assoc :content args)))
         (sandbox (or *sandbox-root* (truename ".")))
         (resolved (merge-pathnames (pathname path) sandbox)))
    (handler-case
        (if (probe-file resolved)
            (format nil "--- APPENDING to existing file: ~A ---~%~{+~A~^~%~}"
                    path
                    (loop :for start := 0 :then (1+ pos)
                          :for pos := (position #\Newline (or new-content "") :start start)
                          :collect (subseq (or new-content "") start (or pos (length (or new-content ""))))
                          :while pos))
            (format nil "--- CREATING new file: ~A ---~%~{+~A~^~%~}"
                    path
                    (loop :for start := 0 :then (1+ pos)
                          :for pos := (position #\Newline (or new-content "") :start start)
                          :collect (subseq (or new-content "") start (or pos (length (or new-content ""))))
                          :while pos)))
      (error () nil))))

;;; --------------------------------------------------------------------------
;;; File Edit Tool (search-and-replace)
;;; --------------------------------------------------------------------------

(defun execute-file-edit (args)
  "Edit a file by replacing the first occurrence of old_text with new_text.
The file must exist. old_text must be found exactly once."
  (let* ((path (cdr (assoc :path args)))
         (old-text (cdr (assoc :old--text args)))
         (new-text (cdr (assoc :new--text args)))
         (resolved (validate-sandbox-path path)))
    (unless path
      (error "path parameter is required"))
    (unless old-text
      (error "old_text parameter is required"))
    (unless new-text
      (error "new_text parameter is required (use empty string to delete)"))
    (unless (probe-file resolved)
      (error "File not found: ~A" path))
    (let* ((content (uiop:read-file-string resolved))
           (pos (search old-text content))
           (count (loop :for start := 0 :then (+ p (length old-text))
                        :for p := (search old-text content :start2 start)
                        :while p :count t)))
      (unless pos
        (error "old_text not found in ~A" path))
      (when (> count 1)
        (error "old_text found ~A times in ~A (must be unique). Provide more context."
               count path))
      ;; Perform the replacement
      (let ((new-content (concatenate 'string
                                      (subseq content 0 pos)
                                      new-text
                                      (subseq content (+ pos (length old-text))))))
        (with-open-file (s resolved
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (write-string new-content s))
        (cl-json:encode-json-to-string
         `((:path . ,path)
           (:old--text--length . ,(length old-text))
           (:new--text--length . ,(length new-text))
           (:status . "ok")))))))

(defun file-edit-approval-display (args)
  "Approval display for file_edit: shows diff of the replacement."
  (let* ((path (cdr (assoc :path args)))
         (old-text (cdr (assoc :old--text args)))
         (new-text (cdr (assoc :new--text args)))
         (sandbox (or *sandbox-root* (truename ".")))
         (resolved (merge-pathnames (pathname path) sandbox)))
    (handler-case
        (when (probe-file resolved)
          (let* ((content (uiop:read-file-string resolved))
                 (pos (search old-text content)))
            (when pos
              (let ((new-content (concatenate 'string
                                              (subseq content 0 pos)
                                              new-text
                                              (subseq content (+ pos (length old-text))))))
                (compute-simple-diff content new-content)))))
      (error () nil))))

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
;;; Lisp Eval Tool
;;; --------------------------------------------------------------------------

(defstruct lisp-eval-record
  "One captured lisp_eval execution record."
  code
  package
  result
  stdout
  stderr
  condition
  timestamp)

(defvar *last-eval-result* nil
  "Multiple-value list from the last successful lisp_eval execution.")

(defvar *last-eval-condition* nil
  "Condition object from the last failed lisp_eval execution, or NIL.")

(defvar *lisp-eval-history* nil
  "Newest-first list of captured lisp_eval execution records.")

(defvar *lisp-eval-history-limit* 50
  "Maximum number of lisp_eval records retained in memory.")

(defvar *lisp-eval-max-output-chars* 10000
  "Maximum characters retained per lisp_eval result/stdout/stderr field.
This mirrors Codex-style tool truncation: large tool results should stay
bounded for the model, while agents can request smaller focused outputs.")

(defvar *lisp-eval-truncation-guidance*
  "use narrower project-read-file-lines/search/outline selectors or pass max_chars for a smaller focused result; do not repeat broad calls after truncation"
  "Guidance appended to truncated lisp_eval fields.")

(defvar *lisp-eval-error-guidance*
  "Do not repeat the same failing eval. Inspect the error, discover exact symbols/selectors with help/search functions, and retry with a smaller verified call."
  "Default guidance included when lisp_eval returns an error.")

(defun requested-lisp-eval-output-limit (args)
  "Return the per-field lisp_eval output limit requested by ARGS.
The request may tighten, but not exceed, *LISP-EVAL-MAX-OUTPUT-CHARS*."
  (let ((requested (cdr (assoc :max--chars args))))
    (if (and (integerp requested)
             (plusp requested))
        (min requested *lisp-eval-max-output-chars*)
        *lisp-eval-max-output-chars*)))

(defun truncate-lisp-eval-text (text &optional (max-chars *lisp-eval-max-output-chars*))
  "Return values TRUNCATED-TEXT and TRUNCATED-P for TEXT.
Long text is middle-truncated so the model sees both the start and the end."
  (let ((string (or text "")))
    (if (> (length string) max-chars)
        (let* ((total (length string))
               (initial-notice
                 (format nil
                         "~%[truncated: omitted ~D of ~D characters; ~A]~%"
                         0
                         total
                         *lisp-eval-truncation-guidance*))
               (initial-available (- max-chars (length initial-notice)))
               (omitted (- total (max 0 initial-available)))
               (notice (format nil
                               "~%[truncated: omitted ~D of ~D characters; ~A]~%"
                               omitted
                               total
                               *lisp-eval-truncation-guidance*))
               (body-available (- max-chars (length notice))))
          (if (plusp body-available)
              (let* ((head-length (floor body-available 2))
                     (tail-length (- body-available head-length))
                     (tail-start (- total tail-length)))
                (values (concatenate 'string
                                     (subseq string 0 head-length)
                                     notice
                                     (subseq string tail-start))
                        t))
              (values (subseq notice 0 (min (length notice) max-chars))
                      t)))
        (values string nil))))

(defun push-lisp-eval-record (record)
  "Push RECORD into *LISP-EVAL-HISTORY* and enforce the retention limit."
  (push record *lisp-eval-history*)
  (when (> (length *lisp-eval-history*) *lisp-eval-history-limit*)
    (setf *lisp-eval-history*
          (subseq *lisp-eval-history* 0 *lisp-eval-history-limit*)))
  record)

(defun lisp-eval-preview (value &optional (max-length 160))
  "Return a compact printed preview for VALUE."
  (let ((text (handler-case
                  (let ((*print-length* 20)
                        (*print-level* 4)
                        (*print-circle* t)
                        (*print-pretty* nil))
                    (prin1-to-string value))
                (error (condition)
                  (format nil "#<error printing value: ~A>" condition)))))
    (if (> (length text) max-length)
        (concatenate 'string (subseq text 0 max-length) "...")
        text)))

(defun lisp-eval-condition-guidance (condition-text)
  "Return model-facing recovery guidance for CONDITION-TEXT."
  (let ((lower (string-downcase (or condition-text ""))))
    (cond
      ((search "no sexed form matches selector" lower)
       "The sexed selector did not match. Do not guess selectors or symbol names. Use sexed-project-outline-to-string/sexed-outline-to-string or sexed-find-forms with :limit, then retry with exact :id or verified :head/:name.")
      ((search "ambiguous" lower)
       "The selector matched more than one target. Retry with one returned :id value, or add :name/:depth/:nth after verifying the outline.")
      ((or (search "undefined function" lower)
           (search "is undefined" lower))
       "Do not guess Clawmacs symbol names. Use apropos-list, find-symbol, list-functions, or describe-function-to-string to discover the exact exported function before retrying.")
      ((or (search "invalid number of arguments" lower)
           (search "too many arguments" lower)
           (search "too few arguments" lower))
       "Check the callee before retrying. Use describe-function-to-string, extended-doc, documentation, or function-lambda-expression to verify the lambda list.")
      (t
       *lisp-eval-error-guidance*))))

(defun eval-history-to-string (&key (limit 10))
  "Return a compact newest-first summary of lisp_eval history."
  (with-output-to-string (out)
    (if *lisp-eval-history*
        (loop :for record :in *lisp-eval-history*
              :for index :from 1
              :while (<= index limit)
              :do (format out "~D. package=~A time=~A~%   code: ~A~%"
                          index
                          (lisp-eval-record-package record)
                          (lisp-eval-record-timestamp record)
                          (lisp-eval-preview
                           (lisp-eval-record-code record)))
                  (if (lisp-eval-record-condition record)
                      (format out "   error: ~A~%"
                              (lisp-eval-record-condition record))
                      (format out "   result: ~A~%"
                              (lisp-eval-preview
                               (lisp-eval-record-result record))))
                  (when (plusp (length (or (lisp-eval-record-stdout record)
                                           "")))
                    (format out "   stdout: ~A~%"
                            (lisp-eval-preview
                             (lisp-eval-record-stdout record))))
                  (when (plusp (length (or (lisp-eval-record-stderr record)
                                           "")))
                    (format out "   stderr: ~A~%"
                            (lisp-eval-preview
                             (lisp-eval-record-stderr record)))))
        (format out "No lisp_eval history captured.~%"))))

(defun execute-lisp-eval (args)
  "Evaluate arbitrary Common Lisp code. Returns the result as a string."
  (let* ((code (cdr (assoc :code args)))
         (package-name (or (cdr (assoc :package args)) "CLAWMACS"))
         (max-chars (requested-lisp-eval-output-limit args)))
    (unless code
      (error "code parameter is required"))
    (let ((package (or (find-package (string-upcase package-name))
                       (find-package :cl-user)))
          (stdout-stream (make-string-output-stream))
          (stderr-stream (make-string-output-stream))
          (results nil)
          (result-output "")
          (condition-text nil)
          (truncated-fields nil))
      (handler-case
          (let ((*package* package)
                (*standard-output* stdout-stream)
                (*trace-output* stderr-stream)
                (*error-output* stderr-stream))
            (let ((form (read-from-string code)))
              (setf results (multiple-value-list (eval form))
                    *last-eval-result* results
                    *last-eval-condition* nil
                    result-output (format nil "~{~S~^~%~}" results))))
        (error (condition)
          (setf *last-eval-result* nil
                *last-eval-condition* condition
                condition-text (format nil "~A" condition))))
      (let ((stdout (get-output-stream-string stdout-stream))
            (stderr (get-output-stream-string stderr-stream)))
        (multiple-value-bind (result-text result-truncated-p)
            (truncate-lisp-eval-text result-output max-chars)
          (multiple-value-bind (stdout-text stdout-truncated-p)
              (truncate-lisp-eval-text stdout max-chars)
            (multiple-value-bind (stderr-text stderr-truncated-p)
                (truncate-lisp-eval-text stderr max-chars)
              (setf truncated-fields
                    (remove nil
                            (list (when result-truncated-p "result")
                                  (when stdout-truncated-p "stdout")
                                  (when stderr-truncated-p "stderr"))))
              (push-lisp-eval-record
               (make-lisp-eval-record :code code
                                      :package (package-name package)
                                      :result results
                                      :stdout stdout-text
                                      :stderr stderr-text
                                      :condition condition-text
                                      :timestamp (get-universal-time)))
              (cl-json:encode-json-to-string
               `((:code . ,code)
                 (:result . ,result-text)
                 (:values . ,(length results))
                 (:stdout . ,stdout-text)
                 (:stderr . ,stderr-text)
                 (:limit . ,max-chars)
                 (:truncated . ,(coerce truncated-fields 'vector))
                 ,@(when truncated-fields
                     `((:truncation--notice . ,*lisp-eval-truncation-guidance*)))
                 ,@(when condition-text
                     `((:error . ,condition-text)
                       (:error--guidance . ,(lisp-eval-condition-guidance
                                             condition-text)))))))))))))

;;; --------------------------------------------------------------------------
;;; Tool Registration
;;; --------------------------------------------------------------------------

(defun init-tools ()
  "Register the default clawmacs built-in tools.
This removes any previously registered built-in entries, then re-registers
only lisp_eval. User-added tools remain untouched."

  (dolist (name *built-in-tool-names*)
    (remhash name *tool-table*))

  (register-tool
   "lisp_eval"
   "Evaluate arbitrary Common Lisp code in the clawmacs process. Returns the result of evaluation. Use this for computation, data transformation, or interacting with the running system. If a result includes error_guidance or truncation_notice, follow it before retrying."
   `((:type . "object")
     (:properties
      . ((:code . ((:type . "string")
                   (:description . "The Common Lisp code to evaluate.")))
         (:package . ((:type . "string")
                      (:description . "Package to evaluate in. Default: CLAWMACS.")))
         (:max--chars . ((:type . "integer")
                         (:description . "Optional per-field output limit. Can only lower the configured lisp_eval maximum.")))))
     (:required . #("code")))
   :agent-allowed
   #'execute-lisp-eval))
