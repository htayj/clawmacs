(in-package :rplaca/tests)

(in-suite mcp-bridge-package-suite)

(defmacro with-mcp-bridge-package-state (&body body)
  "Run BODY with isolated MCP bridge, package, and tool state."
  `(let* ((root (temp-package-test-directory "mcp-bridge-config"))
          (rplaca::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (rplaca::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (rplaca::*tool-table* (make-hash-table :test #'equal))
          (rplaca::*command-table* (make-hash-table :test #'eq))
          (rplaca::*extended-docs* (make-hash-table :test #'eq))
          (rplaca::*slash-command-table* (make-hash-table :test #'equal))
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels* (default-package-test-channels))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
          (rplaca::*enabled-builtin-packages* nil)
          (rplaca::*buffer-type-registry*
           (rplaca::make-buffer-type-registry))
          (rplaca::*mcp-server-configuration-path*
           (merge-pathnames "mcp-servers.json" root))
          (rplaca::*mcp-server-registry* nil)
          (rplaca::*mcp-external-tool-table* (make-hash-table :test #'equal)))
     ,@body))

(defun load-test-mcp-bridge-package ()
  "Enable and load the bundled MCP bridge package."
  (set-package-enablement-scope "mcp-bridge" :global)
  (load-active-packages))

(defun write-mcp-bridge-stdio-server-script ()
  "Write a deterministic fake stdio MCP server and return its pathname."
  (let ((path (merge-pathnames "fake_mcp_stdio_server.py"
                               (temp-package-test-directory "mcp-stdio"))))
    (write-test-file
     path
     "#!/usr/bin/env python3
import json
import sys

TOOLS = [
    {
        \"name\": \"echo\",
        \"description\": \"Echo a value\",
        \"inputSchema\": {
            \"type\": \"object\",
            \"properties\": {
                \"value\": {
                    \"type\": \"string\",
                    \"description\": \"Value to echo\"
                }
            },
            \"required\": [\"value\"]
        }
    }
]

RESOURCES = [
    {
        \"uri\": \"memory://demo\",
        \"name\": \"Demo Resource\",
        \"description\": \"One demo MCP resource\",
        \"mimeType\": \"text/plain\"
    }
]

def reply(req_id, result=None, error=None):
    payload = {\"jsonrpc\": \"2.0\", \"id\": req_id}
    if error is not None:
        payload[\"error\"] = {\"code\": -32000, \"message\": error}
    else:
        payload[\"result\"] = result
    print(json.dumps(payload), flush=True)

for raw in sys.stdin:
    raw = raw.strip()
    if not raw:
        continue
    request = json.loads(raw)
    method = request.get(\"method\")
    if method == \"initialize\":
        reply(request.get(\"id\"), {
            \"protocolVersion\": \"2024-11-05\",
            \"serverInfo\": {\"name\": \"demo\", \"version\": \"0.1\"},
            \"capabilities\": {\"tools\": {}, \"resources\": {}}
        })
    elif method == \"notifications/initialized\":
        continue
    elif method == \"tools/list\":
        reply(request.get(\"id\"), {\"tools\": TOOLS})
    elif method == \"tools/call\":
        params = request.get(\"params\", {})
        arguments = params.get(\"arguments\", {})
        value = arguments.get(\"value\", \"\")
        reply(request.get(\"id\"), {
            \"content\": [{\"type\": \"text\", \"text\": f\"ECHO:{value}\"}],
            \"structuredContent\": {\"echo\": value}
        })
    elif method == \"resources/list\":
        reply(request.get(\"id\"), {\"resources\": RESOURCES})
    elif method == \"resources/read\":
        uri = request.get(\"params\", {}).get(\"uri\", \"\")
        reply(request.get(\"id\"), {
            \"contents\": [{
                \"uri\": uri,
                \"mimeType\": \"text/plain\",
                \"text\": \"Demo resource body\"
            }]
        })
    else:
        reply(request.get(\"id\"), error=f\"unknown method: {method}\")
")
    path))

(defun mcp-bridge-tool-result (tool-name args)
  "Execute TOOL-NAME with ARGS and read its Lisp-data result."
  (nth-value 0
    (rplaca::lisp-data-read
     (rplaca:execute-tool tool-name args))))

(defun mcp-bridge-tool-names (&optional buffer)
  "Return visible tool names for BUFFER."
  (let ((*current-caller* :agent))
    (sort (mapcar (lambda (tool)
                    (cdr (assoc :name tool)))
                  (coerce (tool-definitions-for-api :buffer buffer) 'list))
          #'string<)))

(defmacro with-mcp-http-stubs (&body body)
  "Run BODY with deterministic HTTP MCP responses."
  `(let ((old-provider (symbol-function 'rplaca::provider-http-request-with-retries))
         (old-http (symbol-function 'drakma:http-request)))
     (unwind-protect
          (progn
            (setf (symbol-function 'rplaca::provider-http-request-with-retries)
                  (lambda (_label thunk)
                    (funcall thunk)))
            (setf (symbol-function 'drakma:http-request)
                  (lambda (_url &rest args &key content &allow-other-keys)
                    (declare (ignore _url args))
                    (let* ((request (api-json-decode content))
                           (method (rplaca::mcp-json-value request :method "method"))
                           (result
                             (cond
                               ((string= method "tools/list")
                                (list (cons :tools
                                            (vector
                                             (list (cons :name "echo")
                                                   (cons :description "Echo")
                                                   (cons :inputSchema
                                                         '((:type . "object")
                                                           (:properties . ())
                                                           (:required . #()))))
                                             (list (cons :name "admin")
                                                   (cons :description "Admin")
                                                   (cons :inputSchema
                                                         '((:type . "object")
                                                           (:properties . ())
                                                           (:required . #()))))))))
                               ((string= method "resources/list")
                                (list (cons :resources #())))
                               (t
                                (list (cons :content #()))))))
                      (values
                       (api-json-encode
                        (list (cons :jsonrpc "2.0")
                              (cons :id 1)
                              (cons :result result)))
                       200
                       nil))))
            ,@body)
       (setf (symbol-function 'rplaca::provider-http-request-with-retries)
             old-provider
             (symbol-function 'drakma:http-request)
             old-http))))

(test mcp-bridge-package-registers-built-in-tools-and-prompt
  "Enabling mcp-bridge exposes its built-in discovery tools and prompt section."
  (with-mcp-bridge-package-state
    (load-test-mcp-bridge-package)
    (let ((tool-names (mcp-bridge-tool-names))
          (prompt (render-package-prompt-sections)))
      (is (equal '("mcp_doctor"
                   "mcp_list_resources"
                   "mcp_list_servers"
                   "mcp_list_tools"
                   "mcp_read_resource"
                   "mcp_refresh_tools")
                 tool-names))
      (is (search "External MCP integrations" prompt))
      (is (search "mcp_list_servers" prompt))
      (is (search "mcp_list_resources" prompt)))))

(test mcp-bridge-stdio-discovery-registers-tools-and-resources
  "A configured stdio MCP server maps tools and resources into RPLACA."
  (with-mcp-bridge-package-state
    (let ((script (write-mcp-bridge-stdio-server-script)))
      (rplaca::register-mcp-server-config
       "demo"
       :transport :stdio
       :command "python3"
       :args (list (namestring script)))
      (load-test-mcp-bridge-package)
      (is (member "mcp_demo_echo" (mcp-bridge-tool-names) :test #'string=))
      (let ((result (mcp-bridge-tool-result "mcp_demo_echo"
                                            '((:value . "pong")))))
        (is (getf result :ok))
        (is (string= "demo" (getf result :server)))
        (is (string= "echo" (getf result :tool)))
        (is (search "ECHO:pong" (getf result :text))))
      (let* ((resources (mcp-bridge-tool-result "mcp_list_resources" '()))
             (entries (coerce (getf resources :resources) 'list)))
        (is (= 1 (length entries)))
        (is (string= "memory://demo" (getf (first entries) :uri))))
      (let ((resource (mcp-bridge-tool-result
                       "mcp_read_resource"
                       '((:server . "demo")
                         (:uri . "memory://demo")))))
        (is (string= "Demo resource body" (string-trim '(#\Newline)
                                                       (getf resource :text))))))))

(test mcp-bridge-legacy-permission-json-is-warned-ignored-and-stripped
  "Legacy permission fields warn, do not constrain tools, and vanish on save."
  (with-mcp-bridge-package-state
    (write-test-file
     rplaca::*mcp-server-configuration-path*
     "{\"servers\":[{\"name\":\"httpdemo\",\"description\":\"legacy config\",\"transport\":\"http\",\"url\":\"http://example.test/mcp\",\"enabled\":true,\"default_permission\":\"user-only\",\"tool_permissions\":{\"echo\":\"agent-with-permission\"}}]}")
    (let ((warnings nil))
      (handler-bind
          ((warning
             (lambda (condition)
               (push (princ-to-string condition) warnings)
               (muffle-warning condition))))
        (rplaca::load-mcp-server-configurations))
      (is-true
       (some (lambda (message)
               (search "obsolete MCP permission fields" message
                       :test #'char-equal))
             warnings)))
    (let ((config (rplaca::find-mcp-server-config "httpdemo")))
      (is-true config)
      (is (eq :http (rplaca::mcp-server-config-transport config)))
      (is (string= "http://example.test/mcp"
                   (rplaca::mcp-server-config-url config))))
    (with-mcp-http-stubs
      (load-test-mcp-bridge-package)
      (is (member "mcp_httpdemo_echo" (mcp-bridge-tool-names) :test #'string=))
      (is (member "mcp_httpdemo_admin" (mcp-bridge-tool-names) :test #'string=)))
    (rplaca::save-mcp-server-configurations)
    (let ((saved (uiop:read-file-string
                  rplaca::*mcp-server-configuration-path*)))
      (is-false (search "permission" saved :test #'char-equal)))))

(test mcp-bridge-resource-mentions-inject-provider-context
  "Linked MCP resource mentions inject resource content before the user message."
  (with-mcp-bridge-package-state
    (let* ((script (write-mcp-bridge-stdio-server-script))
           (buffer (make-buffer "mcp-resource-chat")))
      (rplaca::register-mcp-server-config
       "demo"
       :transport :stdio
       :command "python3"
       :args (list (namestring script)))
      (load-test-mcp-bridge-package)
      (set-message-text
       (buffer-input-message buffer)
       (format nil "Please inspect ~A"
               (rplaca::mcp-resource-mention-text "demo" "memory://demo"
                                                   :name "Demo Resource")))
      (buffer-finalize-input buffer)
      (let* ((messages (build-conversation-messages buffer))
             (first-content (cdr (assoc :content (first messages))))
             (second-content (cdr (assoc :content (second messages))))
             (first-text (cdr (assoc :text (aref first-content 0))))
             (second-text (cdr (assoc :text (aref second-content 0)))))
        (is (= 2 (length messages)))
        (is (search "<mcp_resource>" first-text))
        (is (search "Demo resource body" first-text))
        (is (search "Please inspect" second-text))))))

(test mcp-bridge-doctor-reports-failing-server-without-stale-tools
  "Broken MCP servers are reported cleanly and do not leave stale mapped tools."
  (with-mcp-bridge-package-state
    (rplaca::register-mcp-server-config
     "broken"
     :transport :stdio
     :command "/definitely/missing/mcp-server")
    (load-test-mcp-bridge-package)
    (let* ((report (rplaca::mcp-bridge-doctor-report))
           (entry (first report)))
      (is (= 1 (length report)))
      (is (string= "broken" (getf entry :name)))
      (is (eq :error (getf entry :status)))
      (is (stringp (getf entry :error)))
      (is (null (rplaca::list-mcp-external-tools))))))
