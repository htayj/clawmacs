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

(defun execute-tool (name args)
  "Execute tool NAME with ARGS (an alist of parameter values).
Returns a string result or signals an error."
  (let ((def (gethash name *tool-table*)))
    (unless def
      (error "Unknown tool: ~A" name))
    ;; Check permission
    (let ((perm (tool-definition-permission def)))
      (ecase perm
        (:agent-allowed t)
        (:agent-with-permission
         ;; For v1, agent-with-permission tools are auto-approved.
         ;; TODO: Add interactive approval UI.
         t)
        (:user-only
         (unless (eq *current-caller* :user)
           (error "Tool ~A is user-only" name)))))
    (funcall (tool-definition-execute-fn def) args)))

;;; --------------------------------------------------------------------------
;;; HTTP Fetch Tool
;;; --------------------------------------------------------------------------

(defun execute-http-fetch (args)
  "Fetch content from an HTTP/HTTPS URL. Returns the response body as text."
  (let ((url (cdr (assoc :url args)))
        (max-chars (or (cdr (assoc :max--chars args)) 50000)))
    (unless url
      (error "url parameter is required"))
    ;; Validate scheme
    (unless (or (alexandria:starts-with-subseq "http://" url)
                (alexandria:starts-with-subseq "https://" url))
      (error "Only http:// and https:// URLs are supported, got: ~A" url))
    ;; Fetch
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

(defun init-tools ()
  "Register all built-in tools."
  (register-tool
   "http_fetch"
   "Fetch content from an HTTP or HTTPS URL. Returns the response body as text with metadata. Use this to retrieve web pages, API responses, or any HTTP-accessible content."
   `((:type . "object")
     (:properties
      . ((:url . ((:type . "string")
                  (:description . "The HTTP or HTTPS URL to fetch.")))
         (:max--chars . ((:type . "integer")
                         (:description . "Maximum characters of response body to return. Default 50000.")
                         (:minimum . 100)))))
     (:required . #("url")))
   :agent-allowed
   #'execute-http-fetch))
