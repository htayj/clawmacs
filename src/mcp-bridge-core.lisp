(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; MCP Bridge Core
;;; --------------------------------------------------------------------------

(defstruct mcp-server-config
  "Persisted configuration for one external MCP server."
  name
  description
  transport
  command
  args
  url
  headers
  (enabled-p t :type boolean)
  default-tool-permission
  tool-permissions)

(defstruct mcp-external-tool
  "One externally discovered MCP tool mapped into the local tool registry."
  server-name
  tool-name
  provider-name
  description
  input-schema
  permission)

(defvar *mcp-server-configuration-path*
  (merge-pathnames #P".clawmacs.d/mcp-servers.json" (user-homedir-pathname))
  "Path to persisted MCP server configuration.")

(defvar *mcp-server-registry* nil
  "Memoized equal-test hash table of MCP-SERVER-CONFIG entries keyed by name.")

(defvar *mcp-external-tool-table* (make-hash-table :test #'equal)
  "Live registry of discovered MCP external tools keyed by provider name.")

(defun mcp-blank-string-p (value)
  "Return true when VALUE is NIL or only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun mcp-trim (value)
  "Return VALUE as a trimmed string, or NIL when blank."
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (string value))))
      (unless (zerop (length trimmed))
        trimmed))))

(defun mcp-json-object-p (value)
  "Return true when VALUE looks like a decoded JSON object alist."
  (and (listp value)
       (every #'consp value)))

(defun mcp-json-array-list (value)
  "Return VALUE as a list when it is a decoded JSON array."
  (cond
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t nil)))

(defun mcp-json-value (object &rest keys)
  "Return the first present value from OBJECT under KEYS."
  (when (mcp-json-object-p object)
    (loop :for key :in keys
          :for cell := (find key object :key #'car :test #'tool-key=)
          :when cell
            :return (cdr cell))))

(defun mcp-json-ready (value)
  "Recursively normalize VALUE into a JSON-ready shape."
  (cond
    ((stringp value) value)
    ((vectorp value) (map 'vector #'mcp-json-ready value))
    ((and (listp value) (every #'consp value))
     (mapcar (lambda (entry)
               (cons (car entry)
                     (mcp-json-ready (cdr entry))))
             value))
    ((tool-plist-p value)
     (loop :for (key item) :on value :by #'cddr
           :collect (cons key (mcp-json-ready item))))
    ((listp value)
     (coerce (mapcar #'mcp-json-ready value) 'vector))
    (t
     value)))

(defun mcp-normalize-server-name (name)
  "Return NAME as a normalized non-empty MCP server name."
  (let ((trimmed (mcp-trim name)))
    (unless trimmed
      (error "MCP server name must be non-empty."))
    (package-identifier-string trimmed)))

(defun mcp-normalize-transport (value)
  "Normalize VALUE to a supported transport keyword."
  (let ((trimmed (string-downcase (or (mcp-trim value) ""))))
    (cond
      ((string= trimmed "stdio") :stdio)
      ((string= trimmed "http") :http)
      (t
       (error "Unsupported MCP transport ~S." value)))))

(defun mcp-normalize-string-list (value field-name)
  "Normalize VALUE into a list of strings for FIELD-NAME."
  (cond
    ((null value) nil)
    ((stringp value)
     (list value))
    ((vectorp value)
     (mapcar (lambda (item)
               (or (mcp-trim item)
                   (error "~A entries must be non-empty strings." field-name)))
             (coerce value 'list)))
    ((listp value)
     (mapcar (lambda (item)
               (or (mcp-trim item)
                   (error "~A entries must be non-empty strings." field-name)))
             value))
    (t
     (error "~A must be a list/vector of strings, got ~S." field-name value))))

(defun mcp-normalize-string-alist (value field-name)
  "Normalize VALUE into an alist of string pairs for FIELD-NAME."
  (cond
    ((null value) nil)
    ((mcp-json-object-p value)
     (loop :for (key . item) :in value
           :collect (cons (or (mcp-trim key)
                              (error "~A keys must be non-empty strings." field-name))
                          (or (mcp-trim item)
                              (error "~A values must be non-empty strings." field-name)))))
    ((vectorp value)
     (mcp-normalize-string-alist (coerce value 'list) field-name))
    ((listp value)
     (loop :for entry :in value
           :collect
           (cond
             ((and (consp entry)
                   (mcp-trim (car entry))
                   (mcp-trim (cdr entry)))
              (cons (mcp-trim (car entry))
                    (mcp-trim (cdr entry))))
             (t
              (error "~A entries must be (key . value) string pairs, got ~S."
                     field-name entry)))))
    (t
     (error "~A must be a JSON object or list of pairs, got ~S."
            field-name value))))

(defun mcp-normalize-tool-permission-alist (value)
  "Normalize VALUE into an alist of tool-name to permission keyword."
  (cond
    ((null value) nil)
    ((mcp-json-object-p value)
     (loop :for (key . item) :in value
           :collect (cons (or (mcp-trim key)
                              (error "MCP tool permission key must be non-empty."))
                          (normalize-tool-permission
                           item
                           :allow-inherit-p t
                           :context (format nil "MCP tool permission for ~A" key)))))
    ((vectorp value)
     (mcp-normalize-tool-permission-alist (coerce value 'list)))
    ((listp value)
     (loop :for entry :in value
           :collect
           (cond
             ((consp entry)
              (cons (or (mcp-trim (car entry))
                        (error "MCP tool permission key must be non-empty."))
                    (normalize-tool-permission
                     (cdr entry)
                     :allow-inherit-p t
                     :context (format nil "MCP tool permission for ~A"
                                      (car entry)))))
             (t
              (error "Invalid MCP tool permission entry ~S." entry)))))
    (t
     (error "MCP tool permissions must be an object or list of pairs, got ~S."
            value))))

(defun make-mcp-server-registry ()
  "Return a fresh equal-test MCP server registry."
  (make-hash-table :test #'equal))

(defun mcp-server-config-record (config)
  "Return CONFIG as a JSON-ready alist."
  (list
   (cons :name (mcp-server-config-name config))
   (cons :description (or (mcp-server-config-description config) ""))
   (cons :transport (string-downcase
                     (symbol-name (mcp-server-config-transport config))))
   (cons :enabled (mcp-server-config-enabled-p config))
   (cons :default_permission
         (tool-permission-json-value
          (mcp-server-config-default-tool-permission config)))
   (cons :tool_permissions
         (loop :for (tool-name . permission)
                 :in (sort (copy-list (mcp-server-config-tool-permissions config))
                           #'string<
                           :key #'car)
               :collect (cons tool-name
                              (tool-permission-json-value permission))))
   (cons :headers
         (loop :for (name . value)
                 :in (sort (copy-list (mcp-server-config-headers config))
                           #'string<
                           :key #'car)
               :collect (cons name value)))
   (cons :command (or (mcp-server-config-command config) ""))
   (cons :args (coerce (copy-list (mcp-server-config-args config)) 'vector))
   (cons :url (or (mcp-server-config-url config) ""))))

(defun load-mcp-server-configurations ()
  "Load and memoize configured MCP servers."
  (let ((registry (make-mcp-server-registry)))
    (when (probe-file *mcp-server-configuration-path*)
      (handler-case
          (let* ((data (api-json-decode
                        (uiop:read-file-string *mcp-server-configuration-path*)))
                 (servers (mcp-json-array-list
                           (or (mcp-json-value data :servers)
                               (mcp-json-value data :mcp_servers)))))
            (dolist (entry servers)
              (let* ((name (mcp-normalize-server-name
                            (or (mcp-json-value entry :name)
                                (mcp-json-value entry "name"))))
                     (config
                       (make-mcp-server-config
                        :name name
                        :description (or (mcp-trim
                                          (mcp-json-value entry :description
                                                          "description"))
                                         "")
                        :transport (mcp-normalize-transport
                                    (mcp-json-value entry :transport "transport"))
                        :command (mcp-trim (mcp-json-value entry :command "command"))
                        :args (mcp-normalize-string-list
                               (mcp-json-value entry :args "args")
                               "MCP args")
                        :url (mcp-trim (mcp-json-value entry :url "url"))
                        :headers (mcp-normalize-string-alist
                                  (mcp-json-value entry :headers "headers")
                                  "MCP headers")
                        :enabled-p (if (member :enabled entry :key #'car :test #'tool-key=)
                                       (not (null (mcp-json-value entry :enabled "enabled")))
                                       t)
                        :default-tool-permission
                        (normalize-tool-permission
                         (or (mcp-json-value entry :default_permission
                                             "default_permission"
                                             :default-permission
                                             "default-permission")
                             :agent-with-permission)
                         :allow-inherit-p t
                         :context (format nil "MCP default permission for ~A"
                                          name))
                        :tool-permissions
                        (mcp-normalize-tool-permission-alist
                         (mcp-json-value entry :tool_permissions
                                         "tool_permissions"
                                         :tool-permissions
                                         "tool-permissions")))))
                (setf (gethash name registry) config))))
        (error (condition)
          (warn "Failed to load MCP server configuration from ~A: ~A"
                *mcp-server-configuration-path*
                condition))))
    (setf *mcp-server-registry* registry)))

(defun ensure-mcp-server-configurations-loaded ()
  "Return the memoized MCP server registry, loading it if needed."
  (or *mcp-server-registry*
      (load-mcp-server-configurations)))

(defun save-mcp-server-configurations ()
  "Persist configured MCP servers."
  (let ((records nil))
    (maphash (lambda (_name config)
               (declare (ignore _name))
               (push (mcp-server-config-record config) records))
             (ensure-mcp-server-configurations-loaded))
    (ensure-directories-exist *mcp-server-configuration-path*)
    (with-open-file (stream *mcp-server-configuration-path*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string
       (api-json-encode
        (list (cons :servers
                    (coerce (sort records #'string<
                                  :key (lambda (record)
                                         (cdr (assoc :name record))))
                            'vector))))
       stream))
    *mcp-server-configuration-path*))

(defun list-mcp-server-configs ()
  "Return configured MCP servers sorted by name."
  (let ((configs nil))
    (maphash (lambda (_name config)
               (declare (ignore _name))
               (push config configs))
             (ensure-mcp-server-configurations-loaded))
    (sort configs #'string< :key #'mcp-server-config-name)))

(defun find-mcp-server-config (name)
  "Return the configured MCP server named NAME, or NIL."
  (and name
       (gethash (mcp-normalize-server-name name)
                (ensure-mcp-server-configurations-loaded))))

(defun register-mcp-server-config (name &key description transport command args
                                        url headers enabled-p
                                        (default-tool-permission
                                          :agent-with-permission)
                                        tool-permissions
                                        (save-p t))
  "Store NAME as a configured MCP server and return its normalized config."
  (let* ((normalized-name (mcp-normalize-server-name name))
         (normalized-transport (mcp-normalize-transport transport))
         (config (make-mcp-server-config
                  :name normalized-name
                  :description (or (mcp-trim description) "")
                  :transport normalized-transport
                  :command (mcp-trim command)
                  :args (mcp-normalize-string-list args "MCP args")
                  :url (mcp-trim url)
                  :headers (mcp-normalize-string-alist headers "MCP headers")
                  :enabled-p (if (null enabled-p) t (not (null enabled-p)))
                  :default-tool-permission
                  (normalize-tool-permission
                   default-tool-permission
                   :allow-inherit-p t
                   :context (format nil "MCP default permission for ~A"
                                    normalized-name))
                  :tool-permissions
                  (mcp-normalize-tool-permission-alist tool-permissions))))
    (ecase normalized-transport
      (:stdio
       (unless (mcp-server-config-command config)
         (error "MCP stdio server ~A requires :command." normalized-name)))
      (:http
       (unless (mcp-server-config-url config)
         (error "MCP HTTP server ~A requires :url." normalized-name))))
    (setf (gethash normalized-name (ensure-mcp-server-configurations-loaded))
          config)
    (when save-p
      (save-mcp-server-configurations))
    config))

(defun remove-mcp-server-config (name &key (save-p t))
  "Remove configured MCP server NAME and return true when present."
  (let ((removed-p
          (remhash (mcp-normalize-server-name name)
                   (ensure-mcp-server-configurations-loaded))))
    (when (and removed-p save-p)
      (save-mcp-server-configurations))
    removed-p))

(defun set-mcp-server-enabled-p (name enabled-p &key (save-p t))
  "Set configured MCP server NAME enabled state to ENABLED-P."
  (let ((config (or (find-mcp-server-config name)
                    (error "Unknown MCP server: ~A" name))))
    (setf (mcp-server-config-enabled-p config) (not (null enabled-p)))
    (when save-p
      (save-mcp-server-configurations))
    (mcp-server-config-enabled-p config)))

(defun mcp-server-tool-permission (server-name tool-name)
  "Return the persisted permission override for TOOL-NAME on SERVER-NAME."
  (let ((config (find-mcp-server-config server-name)))
    (and config
         (cdr (assoc (or (mcp-trim tool-name) "")
                     (mcp-server-config-tool-permissions config)
                     :test #'string=)))))

(defun set-mcp-server-default-permission (server-name permission
                                          &key (save-p t))
  "Set SERVER-NAME's default tool permission override."
  (let ((config (or (find-mcp-server-config server-name)
                    (error "Unknown MCP server: ~A" server-name))))
    (setf (mcp-server-config-default-tool-permission config)
          (normalize-tool-permission
           permission
           :allow-inherit-p t
           :context (format nil "MCP default permission for ~A"
                            (mcp-server-config-name config))))
    (when save-p
      (save-mcp-server-configurations))
    (mcp-server-config-default-tool-permission config)))

(defun set-mcp-server-tool-permission (server-name tool-name permission
                                       &key (save-p t))
  "Set TOOL-NAME's default permission override on SERVER-NAME."
  (let* ((config (or (find-mcp-server-config server-name)
                     (error "Unknown MCP server: ~A" server-name)))
         (normalized-tool (or (mcp-trim tool-name)
                              (error "MCP tool name must be non-empty.")))
         (normalized-permission
           (normalize-tool-permission
            permission
            :allow-inherit-p t
            :context (format nil "MCP tool permission for ~A/~A"
                             (mcp-server-config-name config)
                             normalized-tool))))
    (setf (mcp-server-config-tool-permissions config)
          (cons (cons normalized-tool normalized-permission)
                (remove normalized-tool
                        (mcp-server-config-tool-permissions config)
                        :key #'car
                        :test #'string=)))
    (when save-p
      (save-mcp-server-configurations))
    normalized-permission))

(defun mcp-server-effective-tool-permission (server-name tool-name)
  "Return the effective bridge-level default permission for TOOL-NAME."
  (let ((config (or (find-mcp-server-config server-name)
                    (error "Unknown MCP server: ~A" server-name))))
    (or (mcp-server-tool-permission server-name tool-name)
        (mcp-server-config-default-tool-permission config)
        :agent-with-permission)))

(defun mcp-request-working-directory (&optional designator)
  "Return the working directory used for MCP operations."
  (let ((pathname (or (typecase designator
                        (buffer (buffer-working-directory designator))
                        (pathname designator)
                        (string (pathname designator))
                        (t nil))
                      (and (boundp '*current-tool-buffer*)
                           *current-tool-buffer*
                           (buffer-working-directory *current-tool-buffer*))
                      (truename "."))))
    (uiop:ensure-directory-pathname pathname)))

(defun mcp-json-rpc-request (id method &optional params)
  "Return ID/METHOD/PARAMS as a JSON-RPC request object."
  (append
   (list (cons :jsonrpc "2.0")
         (cons :id id)
         (cons :method method))
   (when params
     (list (cons :params params)))))

(defun mcp-json-rpc-notification (method &optional params)
  "Return METHOD/PARAMS as a JSON-RPC notification object."
  (append
   (list (cons :jsonrpc "2.0")
         (cons :method method))
   (when params
     (list (cons :params params)))))

(defun mcp-json-rpc-initialize-params ()
  "Return the initialize payload used by the MCP bridge."
  (list
   (cons :protocolVersion "2024-11-05")
   (cons :clientInfo
         (list (cons :name "clawmacs")
               (cons :version (clawmacs-system-version))))
   (cons :capabilities (list))))

(defun mcp-split-lines (text)
  "Split TEXT into newline-delimited lines."
  (loop :for start := 0 :then (1+ position)
        :for position := (position #\Newline text :start start)
        :collect (subseq text start (or position (length text)))
        :while position))

(defun mcp-json-rpc-response-for-id (text id)
  "Return the decoded JSON-RPC response in TEXT matching ID."
  (loop :for line :in (mcp-split-lines (or text ""))
        :for trimmed := (string-trim '(#\Space #\Tab #\Newline #\Return) line)
        :unless (mcp-blank-string-p trimmed)
          :do (let ((decoded (ignore-errors (api-json-decode trimmed))))
                (when (and (mcp-json-object-p decoded)
                           (equal id (mcp-json-value decoded :id "id")))
                  (return decoded)))))

(defun mcp-json-rpc-result (response)
  "Return RESPONSE's result value or signal its JSON-RPC error."
  (let ((error-object (mcp-json-value response :error "error")))
    (when error-object
      (error "MCP request failed: ~A"
             (or (mcp-json-value error-object :message "message")
                 error-object))))
  (or (mcp-json-value response :result "result")
      (error "Malformed MCP response: missing result.")))

(defun mcp-stdio-request-input (method params)
  "Return the line-delimited stdio payload for METHOD and PARAMS."
  (with-output-to-string (stream)
    (dolist (request (list (mcp-json-rpc-request 1 "initialize"
                                                 (mcp-json-rpc-initialize-params))
                           (mcp-json-rpc-notification "notifications/initialized"
                                                      (list))
                           (mcp-json-rpc-request 2 method params)))
      (write-string (api-json-encode (mcp-json-ready request)) stream)
      (terpri stream))))

(defun mcp-stdio-json-rpc-request (config method params &key working-directory)
  "Perform one JSON-RPC stdio request against CONFIG."
  (let* ((argv (cons (mcp-server-config-command config)
                     (copy-list (mcp-server-config-args config))))
         (request-body (mcp-stdio-request-input method params)))
    (with-input-from-string (input request-body)
      (multiple-value-bind (stdout stderr exit-code)
          (uiop:run-program argv
                            :directory (mcp-request-working-directory
                                        working-directory)
                            :input input
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (let ((response (mcp-json-rpc-response-for-id stdout 2)))
          (cond
            (response
             (mcp-json-rpc-result response))
            ((zerop exit-code)
             (error "MCP stdio server ~A returned no JSON-RPC response."
                    (mcp-server-config-name config)))
            (t
             (error "MCP stdio server ~A failed (exit ~D): ~A~@[~%~A~]"
                    (mcp-server-config-name config)
                    exit-code
                    (or stdout "")
                    (and (plusp (length (or stderr ""))) stderr)))))))))

(defun mcp-http-json-rpc-request (config method params)
  "Perform one JSON-RPC HTTP request against CONFIG."
  (let* ((body (api-json-encode
                (mcp-json-ready
                 (mcp-json-rpc-request 1 method params))))
         (headers (append
                   '(("content-type" . "application/json"))
                   (copy-list (mcp-server-config-headers config)))))
    (multiple-value-bind (response-body status-code _headers)
        (provider-http-request-with-retries
         (format nil "mcp-~A" (mcp-server-config-name config))
         (lambda ()
           (drakma:http-request
            (mcp-server-config-url config)
            :method :post
            :content-type "application/json"
            :content body
            :additional-headers headers
            :want-stream nil
            :force-binary nil)))
      (declare (ignore _headers))
      (unless (and (integerp status-code)
                   (<= 200 status-code 299))
        (error "MCP HTTP server ~A returned HTTP ~A: ~A"
               (mcp-server-config-name config)
               status-code
               (http-body-string response-body)))
      (mcp-json-rpc-result
       (api-json-decode (http-body-string response-body))))))

(defun mcp-server-json-rpc-request (server method &optional params
                                    &key working-directory)
  "Perform METHOD/PARAMS against configured MCP SERVER."
  (let ((config (typecase server
                  (mcp-server-config server)
                  (t (or (find-mcp-server-config server)
                         (error "Unknown MCP server: ~A" server))))))
    (ecase (mcp-server-config-transport config)
      (:stdio
       (mcp-stdio-json-rpc-request config method params
                                   :working-directory working-directory))
      (:http
       (mcp-http-json-rpc-request config method params)))))

(defun mcp-tool-input-schema (tool)
  "Return TOOL's provider input schema, defaulting to an object."
  (or (mcp-json-value tool :inputSchema "inputSchema"
                      :input_schema "input_schema"
                      :input--schema "input--schema")
      '((:type . "object")
        (:properties . ())
        (:required . #()))))

(defun mcp-provider-tool-name (server-name tool-name)
  "Return the local provider tool name for SERVER-NAME and TOOL-NAME."
  (normalize-tool-name
   (format nil "mcp_~A_~A"
           (mcp-normalize-server-name server-name)
           (package-identifier-string tool-name))))

(defun mcp-discovered-tool (server-name tool)
  "Return TOOL as an MCP-EXTERNAL-TOOL record."
  (let* ((tool-name (or (mcp-trim (mcp-json-value tool :name "name"))
                        (error "MCP tool from ~A is missing :name." server-name)))
         (provider-name (mcp-provider-tool-name server-name tool-name))
         (permission (mcp-server-effective-tool-permission server-name tool-name)))
    (make-mcp-external-tool
     :server-name (mcp-normalize-server-name server-name)
     :tool-name tool-name
     :provider-name provider-name
     :description (or (mcp-trim (mcp-json-value tool :description "description"))
                      (format nil "External MCP tool ~A from ~A."
                              tool-name server-name))
     :input-schema (mcp-tool-input-schema tool)
     :permission permission)))

(defun mcp-list-server-tools (server-name &key working-directory)
  "Return discovered external tools for SERVER-NAME."
  (let* ((result (mcp-server-json-rpc-request
                  server-name
                  "tools/list"
                  nil
                  :working-directory working-directory))
         (tools (mcp-json-array-list (mcp-json-value result :tools "tools"))))
    (mapcar (lambda (tool)
              (mcp-discovered-tool server-name tool))
            tools)))

(defun mcp-call-tool-text (content)
  "Return CONTENT as concatenated text blocks when available."
  (with-output-to-string (stream)
    (dolist (block (mcp-json-array-list content))
      (let ((text (mcp-json-value block :text "text")))
        (when text
          (write-string text stream)
          (unless (char= #\Newline
                         (char text (1- (length text))))
            (terpri stream)))))))

(defun mcp-call-external-tool (server-name tool-name arguments
                               &key working-directory)
  "Call external MCP TOOL-NAME on SERVER-NAME with ARGUMENTS."
  (let* ((result (mcp-server-json-rpc-request
                  server-name
                  "tools/call"
                  (list (cons :name tool-name)
                        (cons :arguments (mcp-json-ready arguments)))
                  :working-directory working-directory))
         (content (mcp-json-value result :content "content"))
         (structured-content
           (mcp-json-value result :structuredContent "structuredContent"
                           :structured_content "structured_content"))
         (is-error-p (not (null (mcp-json-value result :isError "isError"
                                                :is_error "is_error")))))
    (lisp-data-string
     (list :ok (not is-error-p)
           :server (mcp-normalize-server-name server-name)
           :tool tool-name
           :text (mcp-call-tool-text content)
           :content content
           :structured-content structured-content
           :is-error is-error-p))))

(defun registered-tool-definitions-for-package (package-name)
  "Return raw tool-definition entries owned by PACKAGE-NAME."
  (let ((normalized (manifest-package-name package-name))
        (entries nil))
    (maphash (lambda (name definition)
               (when (string= normalized (or (tool-definition-package definition) ""))
                 (push (cons name definition) entries)))
             *tool-table*)
    (sort entries #'string< :key #'car)))

(defun remove-registered-tool-definitions-for-package (package-name)
  "Remove raw tool-definition entries owned by PACKAGE-NAME."
  (dolist (entry (registered-tool-definitions-for-package package-name))
    (remhash (car entry) *tool-table*)))

(defun clear-mcp-external-tool-registrations ()
  "Remove all currently mapped external MCP tools."
  (maphash (lambda (provider-name tool)
             (declare (ignore tool))
             (remhash provider-name *tool-table*)
             (setf *approval-policy-network-dependent-tools*
                   (remove provider-name
                           *approval-policy-network-dependent-tools*
                           :test #'string=)))
           *mcp-external-tool-table*)
  (clrhash *mcp-external-tool-table*)
  t)

(defun register-mcp-external-tool (tool)
  "Register MCP-EXTERNAL-TOOL TOOL into the raw provider tool table."
  (let ((provider-name (mcp-external-tool-provider-name tool)))
    (register-tool
     provider-name
     (mcp-external-tool-description tool)
     (mcp-external-tool-input-schema tool)
     (or (mcp-external-tool-permission tool) :agent-with-permission)
     (lambda (args)
       (mcp-call-external-tool
        (mcp-external-tool-server-name tool)
        (mcp-external-tool-tool-name tool)
        (tool-args-alist args)
        :working-directory *current-tool-buffer*))
     :package "mcp-bridge")
    (setf (gethash provider-name *mcp-external-tool-table*) tool)
    (pushnew provider-name *approval-policy-network-dependent-tools*
             :test #'string=)
    tool))

(defun refresh-mcp-tool-registrations (&key buffer)
  "Refresh local provider tool mappings from enabled MCP servers."
  (clear-mcp-external-tool-registrations)
  (let ((reports nil))
    (dolist (config (list-mcp-server-configs))
      (let ((name (mcp-server-config-name config)))
        (if (not (mcp-server-config-enabled-p config))
            (push (list :server name
                        :status :disabled
                        :tool-count 0)
                  reports)
            (handler-case
                (let ((tools (mcp-list-server-tools name :working-directory buffer)))
                  (dolist (tool tools)
                    (register-mcp-external-tool tool))
                  (push (list :server name
                              :status :ok
                              :tool-count (length tools))
                        reports))
              (error (condition)
                (push (list :server name
                            :status :error
                            :tool-count 0
                            :error (format nil "~A" condition))
                      reports))))))
    (nreverse reports)))

(defun mcp-tool-summary (tool)
  "Return TOOL as an agent-readable plist."
  (list :server (mcp-external-tool-server-name tool)
        :tool-name (mcp-external-tool-tool-name tool)
        :provider-name (mcp-external-tool-provider-name tool)
        :description (mcp-external-tool-description tool)
        :permission (and (mcp-external-tool-permission tool)
                         (string-downcase
                          (symbol-name (mcp-external-tool-permission tool))))))

(defun list-mcp-external-tools ()
  "Return mapped external MCP tools sorted by provider name."
  (let ((tools nil))
    (maphash (lambda (_name tool)
               (declare (ignore _name))
               (push tool tools))
             *mcp-external-tool-table*)
    (sort tools #'string< :key #'mcp-external-tool-provider-name)))

(defun mcp-list-server-resources (server-name &key working-directory)
  "Return resources advertised by SERVER-NAME."
  (let* ((result (mcp-server-json-rpc-request
                  server-name
                  "resources/list"
                  nil
                  :working-directory working-directory))
         (resources (mcp-json-array-list
                     (mcp-json-value result :resources "resources"))))
    (mapcar (lambda (resource)
              (list :server (mcp-normalize-server-name server-name)
                    :uri (or (mcp-trim (mcp-json-value resource :uri "uri"))
                             "")
                    :name (or (mcp-trim (mcp-json-value resource :name "name"))
                              "")
                    :description (or (mcp-trim
                                      (mcp-json-value resource :description
                                                      "description"))
                                     "")
                    :mime-type (mcp-trim (mcp-json-value resource :mimeType
                                                         "mimeType"
                                                         :mime_type
                                                         "mime_type"))))
            resources)))

(defun mcp-resource-content-text (entry)
  "Return ENTRY's text payload, or a readable placeholder for binary content."
  (or (mcp-json-value entry :text "text")
      (let ((blob (mcp-json-value entry :blob "blob")))
        (and blob
             (format nil "[binary blob: ~A chars]" (length (princ-to-string blob)))))))

(defun mcp-read-server-resource (server-name uri &key working-directory)
  "Read URI from SERVER-NAME and return an agent-readable plist."
  (let* ((result (mcp-server-json-rpc-request
                  server-name
                  "resources/read"
                  (list (cons :uri uri))
                  :working-directory working-directory))
         (contents (mcp-json-array-list (mcp-json-value result :contents "contents")))
         (primary (first contents))
         (text (with-output-to-string (stream)
                 (dolist (entry contents)
                   (let ((entry-text (mcp-resource-content-text entry)))
                     (when entry-text
                       (write-string entry-text stream)
                       (terpri stream))))))
         (mime-type (and primary
                         (mcp-json-value primary :mimeType "mimeType"
                                         :mime_type "mime_type"))))
    (list :server (mcp-normalize-server-name server-name)
          :uri uri
          :mime-type mime-type
          :text text
          :contents (coerce contents 'vector))))

(defun mcp-bridge-doctor-report (&key buffer)
  "Return health records for configured MCP servers."
  (let ((working-directory (mcp-request-working-directory buffer)))
    (loop :for config :in (list-mcp-server-configs)
          :collect
          (let ((name (mcp-server-config-name config)))
            (cond
              ((not (mcp-server-config-enabled-p config))
               (list :name name
                     :transport (mcp-server-config-transport config)
                     :enabled-p nil
                     :status :disabled
                     :tool-count 0))
              (t
               (handler-case
                   (let ((tools (mcp-list-server-tools
                                 name
                                 :working-directory working-directory))
                         (resources (ignore-errors
                                      (mcp-list-server-resources
                                       name
                                       :working-directory working-directory))))
                     (list :name name
                           :transport (mcp-server-config-transport config)
                           :enabled-p t
                           :status :ok
                           :tool-count (length tools)
                           :resource-count (length resources)))
                 (error (condition)
                   (list :name name
                         :transport (mcp-server-config-transport config)
                         :enabled-p t
                         :status :error
                         :tool-count 0
                         :error (format nil "~A" condition))))))))))

(defun mcp-tool-permission-lines (config)
  "Return sorted display lines for CONFIG's tool permission overrides."
  (let ((lines nil))
    (dolist (entry (copy-list (mcp-server-config-tool-permissions config)))
      (push (format nil "~A -> ~A"
                    (car entry)
                    (guard-policy-value-string (cdr entry)))
            lines))
    (sort lines #'string<)))

(defun mcp-server-status-entry (config)
  "Return CONFIG as a summary plist for user-facing status views."
  (list :name (mcp-server-config-name config)
        :transport (mcp-server-config-transport config)
        :enabled-p (mcp-server-config-enabled-p config)
        :description (or (mcp-server-config-description config) "")
        :default-permission (mcp-server-config-default-tool-permission config)
        :tool-permission-count (length (mcp-server-config-tool-permissions config))
        :mapped-tool-count
        (count (mcp-server-config-name config)
               (list-mcp-external-tools)
               :key #'mcp-external-tool-server-name
               :test #'string=)))

(defun mcp-server-status-to-string (&key buffer)
  "Return a help-buffer summary of configured MCP integrations."
  (declare (ignore buffer))
  (with-output-to-string (stream)
    (format stream "MCP Integrations~%~%")
    (dolist (config (list-mcp-server-configs))
      (let ((status (mcp-server-status-entry config)))
        (format stream "- [~A] ~A (~(~A~))~%"
                (if (getf status :enabled-p) "enabled" "disabled")
                (getf status :name)
                (getf status :transport))
        (when (plusp (length (getf status :description)))
          (format stream "  ~A~%" (getf status :description)))
        (format stream "  default permission: ~A~%"
                (guard-policy-value-string (getf status :default-permission)))
        (format stream "  mapped tools: ~D~%"
                (getf status :mapped-tool-count))
        (format stream "  tool overrides: ~D~%"
                (getf status :tool-permission-count))
        (case (getf status :transport)
          (:stdio
           (format stream "  command: ~A~@[ ~{~A~^ ~}~]~%"
                   (or (mcp-server-config-command config) "")
                   (mcp-server-config-args config)))
          (:http
           (format stream "  url: ~A~%"
                   (or (mcp-server-config-url config) ""))))
        (terpri stream)))))

(defun describe-mcp-server-to-string (server-name &key buffer)
  "Return a detailed help-buffer description for SERVER-NAME."
  (let* ((config (or (find-mcp-server-config server-name)
                     (error "Unknown MCP server: ~A" server-name)))
         (tools (handler-case
                    (mcp-list-server-tools
                     (mcp-server-config-name config)
                     :working-directory buffer)
                  (error () nil)))
         (resources (handler-case
                        (mcp-list-server-resources
                         (mcp-server-config-name config)
                         :working-directory buffer)
                      (error () nil))))
    (with-output-to-string (stream)
      (format stream "MCP Server: ~A~%~%" (mcp-server-config-name config))
      (format stream "Enabled: ~A~%" (if (mcp-server-config-enabled-p config)
                                         "yes"
                                         "no"))
      (format stream "Transport: ~(~A~)~%" (mcp-server-config-transport config))
      (format stream "Default permission: ~A~%"
              (guard-policy-value-string
               (mcp-server-config-default-tool-permission config)))
      (when (plusp (length (or (mcp-server-config-description config) "")))
        (format stream "~%~A~%" (mcp-server-config-description config)))
      (case (mcp-server-config-transport config)
        (:stdio
         (format stream "~%Command: ~A~@[ ~{~A~^ ~}~]~%"
                 (or (mcp-server-config-command config) "")
                 (mcp-server-config-args config)))
        (:http
         (format stream "~%URL: ~A~%"
                 (or (mcp-server-config-url config) ""))))
      (let ((tool-lines (mcp-tool-permission-lines config)))
        (format stream "~%Permission overrides:~%")
        (if tool-lines
            (dolist (line tool-lines)
              (format stream "  ~A~%" line))
            (format stream "  (none)~%")))
      (format stream "~%Discovered tools:~%")
      (if tools
          (dolist (tool tools)
            (format stream "  - ~A -> ~A (~A)~%"
                    (mcp-external-tool-tool-name tool)
                    (mcp-external-tool-provider-name tool)
                    (guard-policy-value-string
                     (mcp-external-tool-permission tool))))
          (format stream "  (none or unavailable)~%"))
      (format stream "~%Resources:~%")
      (if resources
          (dolist (resource resources)
            (format stream "  - ~A~@[ (~A)~]~%"
                    (getf resource :uri)
                    (and (plusp (length (or (getf resource :name) "")))
                         (getf resource :name))))
          (format stream "  (none or unavailable)~%")))))

(defun mcp-bridge-doctor-to-string (&key buffer)
  "Return a help-buffer doctor summary for configured MCP integrations."
  (with-output-to-string (stream)
    (format stream "MCP Doctor Report~%~%")
    (dolist (entry (mcp-bridge-doctor-report :buffer buffer))
      (format stream "- ~A: ~(~A~)~@[ (~(~A~))~]~%"
              (getf entry :name)
              (getf entry :status)
              (getf entry :transport))
      (when (getf entry :enabled-p)
        (format stream "  tools: ~D~%" (or (getf entry :tool-count) 0))
        (when (getf entry :resource-count)
          (format stream "  resources: ~D~%" (getf entry :resource-count))))
      (when (getf entry :error)
        (format stream "  error: ~A~%" (getf entry :error)))
      (terpri stream))))

(defun mcp-percent-encode (text)
  "Percent-encode TEXT for use in an MCP resource mention."
  (with-output-to-string (stream)
    (loop :for char :across (or text "")
          :do (if (or (alphanumericp char)
                      (find char "-_.~" :test #'char=))
                  (write-char char stream)
                  (format stream "%~2,'0X" (char-code char))))))

(defun mcp-percent-decode (text)
  "Decode percent-encoded TEXT."
  (with-output-to-string (stream)
    (loop :for index := 0 :then (1+ index)
          :while (< index (length text))
          :do (let ((char (char text index)))
                (if (and (char= char #\%)
                         (<= (+ index 2) (1- (length text))))
                    (let ((code (parse-integer text
                                               :start (1+ index)
                                               :end (+ index 3)
                                               :radix 16)))
                      (write-char (code-char code) stream)
                      (incf index 2))
                    (write-char char stream))))))

(defun mcp-resource-mention-text (server-name uri &key name)
  "Return a linked mention string for SERVER-NAME and URI."
  (let ((label (package-identifier-string
                (or (mcp-trim name)
                    (mcp-trim uri)
                    (mcp-normalize-server-name server-name)))))
    (format nil "[$~A](mcp://~A?uri=~A)"
            label
            (mcp-normalize-server-name server-name)
            (mcp-percent-encode uri))))

(defun parse-linked-mcp-resource-mention (text start)
  "Parse a linked MCP resource mention in TEXT at START."
  (multiple-value-bind (_name path end)
      (parse-linked-skill-mention text start)
    (declare (ignore _name))
    (when (and end
               path
               (alexandria:starts-with-subseq "mcp://" path))
      (let* ((without-scheme (subseq path (length "mcp://")))
             (query-start (position #\? without-scheme))
             (server (and query-start
                          (subseq without-scheme 0 query-start)))
             (query (and query-start
                         (subseq without-scheme (1+ query-start))))
             (uri-pos (and query (search "uri=" query :test #'char-equal)))
             (uri (and uri-pos
                       (subseq query (+ uri-pos (length "uri="))))))
        (when (and (not (mcp-blank-string-p server))
                   (not (mcp-blank-string-p uri)))
          (values (mcp-normalize-server-name server)
                  (mcp-percent-decode uri)
                  end))))))

(defun collect-mcp-resource-mentions (text)
  "Return linked MCP resource mentions from TEXT."
  (let ((mentions nil)
        (index 0))
    (loop :while (< index (length text))
          :do (multiple-value-bind (server uri end)
                  (parse-linked-mcp-resource-mention text index)
                (if end
                    (progn
                      (push (list :server server :uri uri) mentions)
                      (setf index end))
                    (incf index))))
    (nreverse mentions)))

(defun mcp-resource-context-string (resource)
  "Return RESOURCE as an injected user-context block."
  (with-output-to-string (stream)
    (format stream "<mcp_resource>~%")
    (format stream "<server>~A</server>~%" (getf resource :server))
    (format stream "<uri>~A</uri>~%" (getf resource :uri))
    (when (getf resource :mime-type)
      (format stream "<mime_type>~A</mime_type>~%" (getf resource :mime-type)))
    (format stream "<content>~%~A~%</content>~%" (or (getf resource :text) ""))
    (format stream "</mcp_resource>")))

(defun mcp-resource-injection-messages (text)
  "Return linked MCP resource contexts explicitly mentioned by TEXT."
  (let ((messages nil))
    (dolist (mention (collect-mcp-resource-mentions text))
      (handler-case
          (push (mcp-resource-context-string
                 (mcp-read-server-resource (getf mention :server)
                                           (getf mention :uri)))
                messages)
        (error (condition)
          (push (format nil "<mcp_resource_error>~%<server>~A</server>~%<uri>~A</uri>~%<message>~A</message>~%</mcp_resource_error>"
                        (getf mention :server)
                        (getf mention :uri)
                        condition)
                messages))))
    (nreverse messages)))
