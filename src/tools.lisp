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

(defvar *tool-table* (make-hash-table :test #'equal)
  "Global table mapping tool name strings to tool-definition structs.")

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

(defun register-tool (name description schema permission execute-fn
                      &key approval-display-fn)
  "Register a tool in *tool-table*.
APPROVAL-DISPLAY-FN, if provided, is called with (args) during permission
approval to generate extra display context (e.g., file diffs)."
  (setf (gethash name *tool-table*)
        (make-tool-definition :name name
                              :description description
                              :input-schema schema
                              :permission permission
                              :execute-fn execute-fn
                              :approval-display-fn approval-display-fn)))

(defun tool-definitions-for-api ()
  "Return a vector of clawmacs tool definitions for provider adapters.
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
  (let ((def (gethash name *tool-table*)))
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
  "Register the default clawmacs built-in tools.
This removes any previously registered built-in entries, then re-registers
only lisp_eval. User-added tools remain untouched."

  (dolist (name *built-in-tool-names*)
    (remhash name *tool-table*))

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
