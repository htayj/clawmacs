(in-package :clawmacs/tests)

(in-suite mcp-bridge-package-suite)

(defmacro with-mcp-bridge-package-state (&body body)
  "Run BODY with isolated MCP bridge, package, tool, and approval state."
  `(let* ((root (temp-package-test-directory "mcp-bridge-config"))
          (clawmacs::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (clawmacs::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (clawmacs::*tool-table* (make-hash-table :test #'equal))
          (clawmacs::*command-table* (make-hash-table :test #'eq))
          (clawmacs::*extended-docs* (make-hash-table :test #'eq))
          (clawmacs::*slash-command-table* (make-hash-table :test #'equal))
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels* (default-package-test-channels))
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
          (clawmacs::*enabled-builtin-packages* nil)
          (clawmacs::*buffer-type-registry*
           (clawmacs::make-buffer-type-registry))
          (clawmacs::*mcp-server-configuration-path*
           (merge-pathnames "mcp-servers.json" root))
          (clawmacs::*mcp-server-registry* nil)
          (clawmacs::*mcp-external-tool-table* (make-hash-table :test #'equal))
          (clawmacs::*approval-policy-path*
           (merge-pathnames "guard.json" root))
          (clawmacs::*approval-policy-registry* nil)
          (clawmacs::*approval-policy-project-registry-cache*
           (make-hash-table :test #'equal))
          (clawmacs::*approval-policy-network-dependent-tools*
           '("http_fetch" "netcons_run" "netcons_search" "netcons_open"
             "netcons_find")))
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
    (clawmacs::lisp-data-read
     (clawmacs:execute-tool tool-name args))))

(defun mcp-bridge-tool-names (&optional buffer)
  "Return visible tool names for BUFFER."
  (let ((*current-caller* :agent))
    (sort (mapcar (lambda (tool)
                    (cdr (assoc :name tool)))
                  (coerce (tool-definitions-for-api :buffer buffer) 'list))
          #'string<)))

(defmacro with-mcp-http-stubs (&body body)
  "Run BODY with deterministic HTTP MCP responses."
  `(let ((old-provider (symbol-function 'clawmacs::provider-http-request-with-retries))
         (old-http (symbol-function 'drakma:http-request)))
     (unwind-protect
          (progn
            (setf (symbol-function 'clawmacs::provider-http-request-with-retries)
                  (lambda (_label thunk)
                    (funcall thunk)))
            (setf (symbol-function 'drakma:http-request)
                  (lambda (_url &rest args &key content &allow-other-keys)
                    (declare (ignore _url args))
                    (let* ((request (api-json-decode content))
                           (method (clawmacs::mcp-json-value request :method "method"))
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
       (setf (symbol-function 'clawmacs::provider-http-request-with-retries)
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
  "A configured stdio MCP server maps tools and resources into Clawmacs."
  (with-mcp-bridge-package-state
    (let ((script (write-mcp-bridge-stdio-server-script)))
      (clawmacs::register-mcp-server-config
       "demo"
       :transport :stdio
       :command "python3"
       :args (list (namestring script))
       :default-tool-permission :agent-allowed)
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

(test mcp-bridge-http-configuration-applies-per-tool-permissions
  "HTTP-backed MCP discovery honors server and per-tool default permissions."
  (with-mcp-bridge-package-state
    (clawmacs::register-mcp-server-config
     "httpdemo"
     :transport :http
     :url "http://example.test/mcp"
     :default-tool-permission :agent-with-permission
     :tool-permissions '(("echo" . :agent-allowed)))
    (with-mcp-http-stubs
      (load-test-mcp-bridge-package)
      (is (member "mcp_httpdemo_echo" (mcp-bridge-tool-names) :test #'string=))
      (is (member "mcp_httpdemo_admin" (mcp-bridge-tool-names) :test #'string=))
      (is-false (tool-requires-permission-p "mcp_httpdemo_echo"))
      (is (tool-requires-permission-p "mcp_httpdemo_admin")))))

(test mcp-bridge-resource-mentions-inject-provider-context
  "Linked MCP resource mentions inject resource content before the user message."
  (with-mcp-bridge-package-state
    (let* ((script (write-mcp-bridge-stdio-server-script))
           (buffer (make-buffer "mcp-resource-chat")))
      (clawmacs::register-mcp-server-config
       "demo"
       :transport :stdio
       :command "python3"
       :args (list (namestring script))
       :default-tool-permission :agent-allowed)
      (load-test-mcp-bridge-package)
      (set-message-text
       (buffer-input-message buffer)
       (format nil "Please inspect ~A"
               (clawmacs::mcp-resource-mention-text "demo" "memory://demo"
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
    (clawmacs::register-mcp-server-config
     "broken"
     :transport :stdio
     :command "/definitely/missing/mcp-server"
     :default-tool-permission :agent-allowed)
    (load-test-mcp-bridge-package)
    (let* ((report (clawmacs::mcp-bridge-doctor-report))
           (entry (first report)))
      (is (= 1 (length report)))
      (is (string= "broken" (getf entry :name)))
      (is (eq :error (getf entry :status)))
      (is (stringp (getf entry :error)))
      (is (null (clawmacs::list-mcp-external-tools))))))
