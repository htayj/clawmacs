(in-package :rplaca)

(defun mcp-bridge-refresh-runtime (&optional buffer)
  "Refresh mapped MCP external tools for BUFFER's working directory."
  (refresh-mcp-tool-registrations :buffer buffer))

(defun mcp-bridge-list-servers-tool (args)
  "Return configured MCP servers and mapped tools."
  (declare (ignore args))
  (lisp-data-string
   (list :servers
         (coerce
          (mapcar (lambda (config)
                    (mcp-server-status-entry config))
                  (list-mcp-server-configs))
          'vector)
         :tools
         (coerce (mapcar #'mcp-tool-summary
                         (list-mcp-external-tools))
                 'vector))))

(defun mcp-bridge-list-tools-tool (args)
  "Return mapped MCP external tool metadata."
  (declare (ignore args))
  (lisp-data-string
   (list :tools
         (coerce (mapcar #'mcp-tool-summary
                         (list-mcp-external-tools))
                 'vector))))

(defun mcp-bridge-list-resources-tool (args)
  "Return resources advertised by one or more configured MCP servers."
  (let* ((server-name (tool-arg args :server "server"))
         (configs (if (mcp-blank-string-p server-name)
                      (remove-if-not #'mcp-server-config-enabled-p
                                     (list-mcp-server-configs))
                      (list (or (find-mcp-server-config server-name)
                                (error "Unknown MCP server: ~A" server-name)))))
         (resources nil))
    (dolist (config configs)
      (dolist (resource (mcp-list-server-resources
                         (mcp-server-config-name config)
                         :working-directory *current-tool-buffer*))
        (push resource resources)))
    (lisp-data-string
     (list :resources (coerce (nreverse resources) 'vector)))))

(defun mcp-bridge-read-resource-tool (args)
  "Read one advertised MCP resource."
  (let* ((server-name (tool-arg args :server "server"))
         (uri (tool-arg args :uri "uri")))
    (lisp-data-string
     (mcp-read-server-resource server-name uri
                               :working-directory *current-tool-buffer*))))

(defun mcp-bridge-refresh-tools-tool (args)
  "Refresh mapped MCP external tools."
  (declare (ignore args))
  (lisp-data-string
   (list :reports (coerce (mcp-bridge-refresh-runtime *current-tool-buffer*)
                          'vector)
         :tools (coerce (mapcar #'mcp-tool-summary
                                (list-mcp-external-tools))
                        'vector))))

(defun mcp-bridge-doctor-tool (args)
  "Return health information for configured MCP servers."
  (declare (ignore args))
  (lisp-data-string
   (list :servers (coerce (mcp-bridge-doctor-report
                           :buffer *current-tool-buffer*)
                          'vector))))

(register-package-prompt-section
 "mcp-bridge"
 "## External MCP integrations

- Enabled MCP servers expose their external tools directly as normal provider
  tools. Those mapped tool names use the `mcp_<server>_<tool>` form.
- Use `mcp_list_servers` or `mcp_list_tools` when you need to discover which
  external MCP integrations are currently available.
- Use `mcp_list_resources` and `mcp_read_resource` for MCP resources instead of
  asking the user to paste them manually.
- Users can insert linked MCP resource mentions into chat input. When a message
  contains a linked `mcp://` resource mention, RPLACA injects that resource's
  content into provider-visible context for the turn."
 :title "External MCP integrations"
 :package "mcp-bridge")

(deftool mcp-bridge-list-servers-tool
  :name "mcp_list_servers"
  :description "List configured MCP servers plus currently mapped external tools."
  :call-style :raw-args
  :args ())

(deftool mcp-bridge-list-tools-tool
  :name "mcp_list_tools"
  :description "List currently mapped external MCP tools."
  :call-style :raw-args
  :args ())

(deftool mcp-bridge-list-resources-tool
  :name "mcp_list_resources"
  :description "List advertised resources from one enabled MCP server or all enabled MCP servers."
  :call-style :raw-args
  :args ((server :type "string" :required nil
                 :description "Optional MCP server name. Defaults to all enabled servers.")))

(deftool mcp-bridge-read-resource-tool
  :name "mcp_read_resource"
  :description "Read one resource from a configured MCP server."
  :call-style :raw-args
  :args ((server :type "string"
                 :description "Configured MCP server name.")
         (uri :type "string"
              :description "Exact MCP resource URI.")))

(deftool mcp-bridge-refresh-tools-tool
  :name "mcp_refresh_tools"
  :description "Refresh mapped external MCP tools from the currently enabled MCP servers."
  :call-style :raw-args
  :execution :frame
  :args ())

(deftool mcp-bridge-doctor-tool
  :name "mcp_doctor"
  :description "Inspect the health of configured MCP servers."
  :call-style :raw-args
  :args ())

(defun make-mcp-server-selector-item (config)
  "Return CONFIG as a minibuffer selector item."
  (let* ((name (mcp-server-config-name config))
         (enabled-p (mcp-server-config-enabled-p config))
         (description (or (mcp-server-config-description config) "")))
    (list :server-name name
          :display (format nil "[~A] ~A (~(~A~))~@[ - ~A~]"
                           (if enabled-p "enabled" "disabled")
                           name
                           (mcp-server-config-transport config)
                           (and (plusp (length description)) description))
          :match-text (format nil "~A ~A ~(~A~) ~A"
                              name
                              (if enabled-p "enabled" "disabled")
                              (mcp-server-config-transport config)
                              description))))

(defun mcp-server-selector-items (&key enabled-p)
  "Return selector items for configured MCP servers."
  (let ((configs (list-mcp-server-configs)))
    (when enabled-p
      (setf configs (remove-if-not
                     (lambda (config)
                       (if enabled-p
                           (mcp-server-config-enabled-p config)
                           (not (mcp-server-config-enabled-p config))))
                     configs)))
    (mapcar #'make-mcp-server-selector-item configs)))

(defun mcp-bridge-open-help-buffer (name content)
  "Open or refresh help buffer NAME with CONTENT."
  (let ((existing (find-buffer-by-name name)))
    (if existing
        (progn
          (set-message-text (message-prev (buffer-input-message existing))
                            content)
          (switch-to-buffer existing))
        (switch-to-buffer (make-help-buffer name content)))))

(defun mcp-bridge-list-servers-command (buffer)
  "Open a help buffer listing configured MCP integrations."
  (declare (ignore buffer))
  (mcp-bridge-open-help-buffer "*help:mcp-servers*"
                               (mcp-server-status-to-string)))
(defcommand mcp-bridge-list-servers-command)

(defun mcp-bridge-doctor-command (buffer)
  "Open a help buffer with MCP doctor output."
  (mcp-bridge-open-help-buffer "*help:mcp-doctor*"
                               (mcp-bridge-doctor-to-string :buffer buffer)))
(defcommand mcp-bridge-doctor-command)

(defun mcp-bridge-inspect-server-command (buffer)
  "Select one MCP server and open a detailed help buffer."
  (let ((items (mcp-server-selector-items)))
    (if items
        (minibuffer-activate
         "Inspect MCP Server" items
         (lambda (item)
           (let ((server-name (getf item :server-name)))
             (mcp-bridge-open-help-buffer
              (format nil "*help:mcp:~A*" server-name)
              (describe-mcp-server-to-string server-name :buffer buffer)))))
        (buffer-insert-system-message buffer "[No configured MCP servers.]"))))
(defcommand mcp-bridge-inspect-server-command)

(defun mcp-bridge-enable-server-command (buffer)
  "Select one disabled MCP server and enable it."
  (let* ((items (remove-if (lambda (item)
                             (mcp-server-config-enabled-p
                              (find-mcp-server-config (getf item :server-name))))
                           (mcp-server-selector-items))))
    (if items
        (minibuffer-activate
         "Enable MCP Server" items
         (lambda (item)
           (let ((server-name (getf item :server-name)))
             (set-mcp-server-enabled-p server-name t)
             (mcp-bridge-refresh-runtime buffer)
             (buffer-insert-system-message
              buffer
              (format nil "[Enabled MCP server ~A]" server-name)))))
        (buffer-insert-system-message buffer "[No disabled MCP servers.]"))))
(defcommand mcp-bridge-enable-server-command)

(defun mcp-bridge-disable-server-command (buffer)
  "Select one enabled MCP server and disable it."
  (let* ((items (remove-if-not (lambda (item)
                                 (mcp-server-config-enabled-p
                                  (find-mcp-server-config (getf item :server-name))))
                               (mcp-server-selector-items))))
    (if items
        (minibuffer-activate
         "Disable MCP Server" items
         (lambda (item)
           (let ((server-name (getf item :server-name)))
             (set-mcp-server-enabled-p server-name nil)
             (mcp-bridge-refresh-runtime buffer)
             (buffer-insert-system-message
              buffer
              (format nil "[Disabled MCP server ~A]" server-name)))))
        (buffer-insert-system-message buffer "[No enabled MCP servers.]"))))
(defcommand mcp-bridge-disable-server-command)

(defun mcp-resource-selector-items (&optional buffer)
  "Return minibuffer selector items for advertised MCP resources."
  (let ((items nil))
    (dolist (config (remove-if-not #'mcp-server-config-enabled-p
                                   (list-mcp-server-configs)))
      (dolist (resource (ignore-errors
                          (mcp-list-server-resources
                           (mcp-server-config-name config)
                           :working-directory buffer)))
        (push (list :server (getf resource :server)
                    :uri (getf resource :uri)
                    :name (getf resource :name)
                    :display (format nil "~A  ~A~@[ - ~A~]"
                                     (getf resource :server)
                                     (getf resource :uri)
                                     (and (plusp (length (or (getf resource :name) "")))
                                          (getf resource :name)))
                    :match-text (format nil "~A ~A ~A"
                                        (getf resource :server)
                                        (getf resource :uri)
                                        (or (getf resource :name) "")))
              items)))
    (sort items #'string< :key (lambda (item)
                                 (format nil "~A ~A"
                                         (getf item :server)
                                         (getf item :uri))))))

(defun mcp-bridge-insert-resource-command (buffer)
  "Insert a linked MCP resource mention into BUFFER's input."
  (let ((items (mcp-resource-selector-items buffer)))
    (if items
        (minibuffer-activate
         "Insert MCP Resource" items
         (lambda (item)
           (message-insert-string
            (buffer-input-message buffer)
            (mcp-resource-mention-text
             (getf item :server)
             (getf item :uri)
             :name (or (getf item :name)
                       (getf item :uri))))
           (mark-buffer-dirty buffer)))
        (buffer-insert-system-message
         buffer
         "[No resources available from enabled MCP servers.]"))))
(defcommand mcp-bridge-insert-resource-command)

(mcp-bridge-refresh-runtime)
