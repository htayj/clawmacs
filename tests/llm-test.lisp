(in-package :clawmacs/tests)

(in-suite llm-suite)

(defclass unprintable-eval-value () ())

(defmethod print-object ((object unprintable-eval-value) stream)
  (declare (ignore object stream))
  (error "cannot print eval value"))

(defun make-unprintable-eval-value ()
  "Return an object whose printer signals for lisp_eval tests."
  (make-instance 'unprintable-eval-value))

(defun temp-test-token-path (provider)
  (let* ((base (make-pathname :directory (list :absolute "tmp"
                                               (format nil "clawmacs-llm-tests-~A"
                                                       (list (get-universal-time)
                                                             (get-internal-real-time)
                                                             (gensym))))))
         (filename (ecase provider
                     (:openai-codex "openai-codex-token")
                     (:zai "zai-api-key")
                     (:openrouter "openrouter-api-key"))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames filename base)))

(defmacro with-provider-token-path-overrides ((_removed-provider-path openai-codex-path &optional zai-path) &body body)
  (declare (ignore _removed-provider-path))
  `(let ((original-provider-token-path
           (symbol-function 'clawmacs::provider-token-path)))
     (unwind-protect
          (progn
            (setf (symbol-function 'clawmacs::provider-token-path)
                  (lambda (provider)
                    (case provider
                      (:openai-codex ,openai-codex-path)
                      (:zai ,(or zai-path '(funcall original-provider-token-path provider)))
                      (otherwise
                       (funcall original-provider-token-path provider)))))
            ,@body)
       (setf (symbol-function 'clawmacs::provider-token-path)
             original-provider-token-path))))

(defun temp-agent-defaults-path ()
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "clawmacs-agent-defaults-~A"
                                                      (list (get-universal-time)
                                                            (get-internal-real-time)
                                                            (gensym)))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames "agent-defaults.json" base)))

(defmacro with-agent-defaults-path-override ((path) &body body)
  `(let ((clawmacs::*agent-defaults-path* ,path)
         (clawmacs::*agent-defaults-registry* nil))
     ,@body))

(defun temp-approval-policy-path ()
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "clawmacs-guard-policy-~A"
                                                      (list (get-universal-time)
                                                            (get-internal-real-time)
                                                            (gensym)))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames "guard.json" base)))

(defun temp-package-test-directory (label)
  (make-pathname :directory (list :absolute "tmp"
                                  (format nil "clawmacs-package-tests-~A-~36R-~36R-~A"
                                          label
                                          (get-universal-time)
                                          (get-internal-real-time)
                                          (gensym)))))

(defmacro with-approval-policy-path-override ((path) &body body)
  `(let ((clawmacs::*approval-policy-path* ,path)
         (clawmacs::*approval-policy-registry* nil)
         (clawmacs::*approval-policy-project-registry-cache*
           (make-hash-table :test #'equal)))
     ,@body))

(defun default-package-test-channels ()
  (list (clawmacs:make-package-channel
         :name "default"
         :root clawmacs:*default-package-channel-directory*
         :description "Bundled Clawmacs packages"
         :source :builtin)))

(defmacro with-agent-definition-registry-override (() &body body)
  `(let ((clawmacs::*agent-definition-registry* (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-subagent-registry-override (() &body body)
  `(let ((clawmacs::*subagent-handle-counter* 0)
         (clawmacs::*subagent-handles* (make-hash-table :test #'equal))
         (clawmacs::*subagent-registry-lock*
           (bt:make-lock "test-subagent-registry")))
     ,@body))

(defmacro with-pipeline-definition-registry-override (() &body body)
  `(let ((clawmacs::*pipeline-definition-registry*
           (make-hash-table :test #'equal))
         (clawmacs::*pipeline-test-profile-registry*
           (make-hash-table :test #'equal))
         (clawmacs:*default-pipeline-name* nil))
     ,@body))

(defun temp-codex-auth-path ()
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "clawmacs-codex-auth-~A"
                                                      (list (get-universal-time)
                                                            (get-internal-real-time)
                                                            (gensym)))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames "auth.json" base)))

(defmacro with-codex-auth-path-override ((path) &body body)
  `(let ((clawmacs::*codex-auth-path* ,path))
     ,@body))

(defun write-agent-defaults-file (path json)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string json stream)))

(defmacro with-function-override ((name lambda-list &body implementation) &body body)
  `(let ((original-function (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name)
                  (lambda ,lambda-list
                    ,@implementation))
            ,@body)
       (setf (symbol-function ',name) original-function))))

(defun write-test-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(defmacro with-tool-table-restored (&body body)
  `(let* ((snapshot (make-hash-table :test (hash-table-test clawmacs::*tool-table*)))
          (agent-tool-snapshot
            (make-hash-table
             :test (hash-table-test clawmacs::*agent-tool-metadata-table*)))
          (agent-tool-name-snapshot
            (make-hash-table
             :test (hash-table-test clawmacs::*agent-tool-name-table*)))
          (package-test-root (temp-package-test-directory "llm-package-config"))
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json"
                            (uiop:ensure-directory-pathname package-test-root)))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels* (default-package-test-channels))
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
          (*sessions-dir* (temp-session-test-directory "llm-sessions"))
          (clawmacs::*buffer-ring* nil)
          (clawmacs::*buffer-counter* 0)
          (clawmacs::*startup-hook* nil)
          (clawmacs::*initial-buffer-hook* nil)
          (clawmacs::*before-command-hook* nil)
          (clawmacs::*after-command-hook* nil)
          (clawmacs::*before-tool-hook* nil)
          (clawmacs::*after-tool-hook* nil)
          (clawmacs::*before-send-message-hook* nil)
          (clawmacs::*after-send-message-hook* nil)
          (clawmacs::*after-buffer-create-hook* nil)
          (clawmacs::*after-provider-response-hook* nil)
          (clawmacs::*approval-policy-path* (temp-approval-policy-path))
          (clawmacs::*approval-policy-isolation-root*
           (temp-package-test-directory "approval-policy-isolation"))
          (clawmacs::*approval-policy-registry* nil)
          (clawmacs::*approval-policy-project-registry-cache*
           (make-hash-table :test #'equal)))
     (maphash (lambda (key value)
                (setf (gethash key snapshot) value))
              clawmacs::*tool-table*)
     (maphash (lambda (key value)
                (setf (gethash key agent-tool-snapshot) value))
              clawmacs::*agent-tool-metadata-table*)
     (maphash (lambda (key value)
                (setf (gethash key agent-tool-name-snapshot) value))
              clawmacs::*agent-tool-name-table*)
     (unwind-protect
          (progn
            ,@body)
       (clrhash clawmacs::*tool-table*)
       (maphash (lambda (key value)
                  (setf (gethash key clawmacs::*tool-table*) value))
                snapshot)
       (clrhash clawmacs::*agent-tool-metadata-table*)
       (maphash (lambda (key value)
                  (setf (gethash key clawmacs::*agent-tool-metadata-table*)
                        value))
                agent-tool-snapshot)
       (clrhash clawmacs::*agent-tool-name-table*)
       (maphash (lambda (key value)
                  (setf (gethash key clawmacs::*agent-tool-name-table*) value))
                agent-tool-name-snapshot))))

(defun initialize-test-tools ()
  "Initialize the tool table with the bundled lispi package enabled."
  (clawmacs::init-tools)
  (clawmacs:set-package-enablement-scope "lispi" :global)
  (clawmacs:load-active-packages))

(defun append-test-user-message (buf text)
  (clawmacs::set-message-text (buffer-input-message buf) text)
  (buffer-finalize-input buf)
  (message-prev (buffer-input-message buf)))

(defun test-buffer-history-messages (buf)
  (loop :for msg := (buffer-first-message buf) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buf))))
        :unless (clawmacs::buffer-ephemeral-display-message-p msg)
        :collect msg))

(defun test-buffer-history-senders (buf)
  (mapcar #'message-sender (test-buffer-history-messages buf)))

(defun make-completed-stream-state-response (stop-reason content-blocks
                                             &optional usage)
  (let ((state (clawmacs::make-stream-state)))
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (setf (clawmacs::stream-state-stop-reason state) stop-reason
            (clawmacs::stream-state-content-blocks state) (reverse content-blocks)
            (clawmacs::stream-state-usage state) usage
            (clawmacs::stream-state-done-p state) t))
    state))

(defun write-codex-auth-json (path payload)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (clawmacs::api-json-encode payload) stream)))

(defun make-codex-chatgpt-auth-payload (&key
                                          (access-token "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig")
                                          (refresh-token "chatgpt-refresh")
                                          (account-id "acct_123")
                                          (id-token "id-token-placeholder")
                                          openai-api-key
                                          (last-refresh "2026-04-08T12:00:00Z"))
  `((:auth--mode . "chatgpt")
    (:openai--api--key . ,openai-api-key)
    (:tokens . ((:id--token . ,id-token)
                (:access--token . ,access-token)
                (:refresh--token . ,refresh-token)
                (:account--id . ,account-id)))
    (:last--refresh . ,last-refresh)))

(defun make-codex-api-key-auth-payload (&key
                                          (api-key "sk-test-api-key")
                                          (last-refresh "2026-04-08T12:00:00Z"))
  `((:openai--api--key . ,api-key)
    (:last--refresh . ,last-refresh)))

(test provider-token-paths
  "Provider token paths are provider-specific."
  (let ((home (user-homedir-pathname)))
    (is (equal (merge-pathnames #P".config/clawmacs/openai-codex-token" home)
               (clawmacs::provider-token-path :openai-codex)))
    (is (equal (merge-pathnames #P".config/clawmacs/zai-api-key" home)
               (clawmacs::provider-token-path :zai)))
    (is (equal (merge-pathnames #P".config/clawmacs/openrouter-api-key" home)
               (clawmacs::provider-token-path :openrouter)))))

(test provider-token-path-unknown-provider
  "Unknown providers signal a clear error."
  (signals error
    (clawmacs::provider-token-path :unknown-provider)))

(test init-tools-registers-pi-style-tools-by-default
  "Enabling the bundled lispi package exposes file tools beside lisp_eval."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (initialize-test-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool) (cdr (assoc :name tool))) tools)
                             #'string<)))
      (is (equal '("edit" "find" "grep" "lisp_eval" "read" "write")
                 tool-names))
      (is (string= "CLAWMACS" clawmacs:*lisp-eval-default-package*))
      (dolist (name '("read" "find" "grep" "write" "edit" "lisp_eval"))
        (is (not (null (gethash name clawmacs::*tool-table*))))
        (is-false (clawmacs::tool-requires-permission-p name)))
      (is (null (gethash "http_fetch" clawmacs::*tool-table*)))
      (is (null (gethash "file_read" clawmacs::*tool-table*)))
      (is (null (gethash "file_write" clawmacs::*tool-table*)))
      (is (null (gethash "file_edit" clawmacs::*tool-table*)))
      (is (null (gethash "shell_exec" clawmacs::*tool-table*))))))

(test tool-definitions-for-api-returns-stable-name-order
  "Provider tool definitions are sorted by name for deterministic prompts."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (initialize-test-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
           (tool-names (mapcar (lambda (tool)
                                 (cdr (assoc :name tool)))
                               tools)))
      (is (equal '("edit" "find" "grep" "lisp_eval" "read" "write")
                 tool-names)))))

(test approval-policy-round-trips-default-and-tool-overrides
  "Guard policy JSON persists default and per-tool permission overrides."
  (let ((path (temp-approval-policy-path)))
    (with-approval-policy-path-override (path)
      (clawmacs::set-approval-policy-default-permission
       :agent-with-permission)
      (clawmacs::set-approval-policy-tool-permission "write" :user-only)
      (clawmacs::save-approval-policy)
      (setf clawmacs::*approval-policy-registry* nil)
      (clawmacs::load-approval-policy)
      (is (eq :agent-with-permission
              (clawmacs:approval-policy-default-permission)))
      (is (eq :user-only
              (clawmacs:approval-policy-tool-permission "write")))
      (is (null (clawmacs:approval-policy-tool-permission "read"))))))

(test approval-policy-precedence-is-tool-override-then-default-then-static
  "Guard policy overlay precedence is tool override, then default, then static metadata."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (initialize-test-tools)
    (let ((path (temp-approval-policy-path)))
      (with-approval-policy-path-override (path)
        (is (eq :agent-allowed
                (clawmacs:effective-tool-permission "lisp_eval")))
        (clawmacs::set-approval-policy-default-permission :user-only)
        (clawmacs::set-approval-policy-tool-permission "read" :agent-allowed)
        (is (eq :agent-allowed
                (clawmacs:effective-tool-permission "read")))
        (is (eq :user-only
                (clawmacs:effective-tool-permission "lisp_eval")))))))

(test approval-policy-user-only-overrides-hide-tools-from-agent-discovery
  "User-only overrides remove tools from agent-visible provider discovery."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (initialize-test-tools)
    (let ((path (temp-approval-policy-path)))
      (with-approval-policy-path-override (path)
        (clawmacs::set-approval-policy-tool-permission "write" :user-only)
        (let* ((*current-caller* :agent)
               (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
               (tool-names (mapcar (lambda (tool)
                                     (cdr (assoc :name tool)))
                                   tools)))
          (is-false (member "write" tool-names :test #'string=)))
        (let* ((*current-caller* :user)
               (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
               (tool-names (mapcar (lambda (tool)
                                     (cdr (assoc :name tool)))
                                   tools)))
          (is (member "write" tool-names :test #'string=)))))))

(test approval-policy-overrides-prompt-and-interactive-tool-permission-flow
  "Guard policy overrides feed both prompt-mode denial and interactive approval."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (initialize-test-tools)
    (let ((path (temp-approval-policy-path)))
      (with-approval-policy-path-override (path)
        (clawmacs::set-approval-policy-tool-permission "write"
                                                       :agent-with-permission)
        (let ((tool-use (clawmacs::canonical-tool-use-block
                         "call-1"
                         "write"
                         '((:path . "/tmp/guard-policy.txt")
                           (:content . "guarded"))))
              (buf (make-buffer "guard-policy" :agent-name "agent")))
          (multiple-value-bind (result event)
              (clawmacs::execute-prompt-tool-call buf tool-use :agent nil)
            (is (search "DENIED" (cdr (assoc :display result))))
            (is (clawmacs:prompt-tool-event-denied-p event)))
          (clawmacs::begin-tool-approval buf (list tool-use))
          (is (eq :approval (buffer-status buf)))
          (is (string= "write"
                       (cdr (assoc :tool-name
                                   (buffer-approval-pending buf))))))))))

(test mcclim-compose-approval-continues-lisp-eval-tool-loop
  "The McCLIM compose helper can approve a pending lisp_eval call and continue."
  (with-tool-table-restored
    (initialize-test-tools)
    (clawmacs::set-approval-policy-tool-permission
     "lisp_eval" :agent-with-permission)
    (let ((request-count 0))
      (with-function-override (clawmacs::resolve-buffer-provider-and-model
                               (buffer)
                               (declare (ignore buffer))
                               (values :zai "glm-test" nil))
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                             reasoning-effort system-prompt
                                             service-tier)
                                 (declare (ignore provider messages callback
                                                  model max-tokens tools
                                                  reasoning-effort
                                                  system-prompt service-tier))
                                 (incf request-count)
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "tool_use"
                                      (list
                                       (clawmacs::canonical-tool-use-block
                                        "call-1"
                                        "lisp_eval"
                                        '((:code . "(+ 1 1)")
                                          (:package . "CLAWMACS")))))
                                     (make-completed-stream-state-response
                                      "end_turn"
                                      (list
                                       (clawmacs::canonical-text-block
                                        "the result is 2")))))
          (let ((buf (make-buffer "mcclim-approval" :agent-name "agent")))
            (clawmacs::start-streaming-response buf)
            (is (= 1 request-count))
            (is-true (clawmacs::update-streaming-response buf))
            (is (eq :approval (buffer-status buf)))
            (is (string= "lisp_eval"
                         (cdr (assoc :tool-name
                                     (buffer-approval-pending buf)))))
            (is-true (clawmacs::handle-chat-compose-text buf ""))
            (is (= 2 request-count))
            (is (null (buffer-approval-pending buf)))
            (is-false (clawmacs::update-streaming-response buf))
            (let ((messages (test-buffer-history-messages buf)))
              (is (member :tool-result
                          (mapcar #'message-sender messages)))
              (is (some (lambda (text)
                          (and (search ";; lisp_eval" text)
                               (search "2" text)))
                        (mapcar #'message-text messages)))
              (is (some (lambda (text)
                          (search "the result is 2" text))
                        (mapcar #'message-text messages))))))))))

(defun test-command-table-key-command (table key-character)
  "Return the inherited command bound to KEY-CHARACTER in command TABLE."
  (let ((event (make-instance 'clim:key-press-event
                              :sheet nil
                              :x 0
                              :y 0
                              :key-name nil
                              :key-character key-character
                              :modifier-state (clim:make-modifier-state))))
    (let ((item (esa::find-gestures-with-inheritance (list event) table)))
      (and item (clim:command-menu-item-value item)))))

(test mcclim-compose-return-keystrokes-submit-message
  "The McCLIM chat frame binds Return/Newline to submit the message."
  (let* ((buf (make-buffer "mcclim-ret-submit" :agent-name "agent"))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buf))
         (table (clim:find-command-table 'clawmacs::clawmacs-chat-frame))
         (menu-table (clawmacs::make-chat-menu-bar-command-table frame))
         (drei-order-table
           (clim:make-command-table
            nil
            :inherit-from (list menu-table 'drei:editor-table))))
    (is (clim:command-accessible-in-command-table-p
         'clawmacs::com-chat-submit-compose
         table))
    (is (equal '(clawmacs::com-chat-submit-compose)
               (test-command-table-key-command table #\Return)))
    (is (equal '(clawmacs::com-chat-submit-compose)
               (test-command-table-key-command table #\Newline)))
    (is (equal '(clawmacs::com-chat-submit-compose)
               (test-command-table-key-command menu-table #\Return)))
    (is (equal '(clawmacs::com-chat-submit-compose)
               (test-command-table-key-command drei-order-table #\Return)))))

(test mcclim-chat-menu-bar-exposes-toolbar-commands
  "The McCLIM chat frame exposes its MVP toolbar through command-table menus."
  (let* ((menu-table (clawmacs::make-chat-menu-bar-command-table))
         (chat-menu (clim:find-menu-item "Chat" menu-table :errorp nil))
         (view-menu (clim:find-menu-item "View" menu-table :errorp nil))
         (skills-menu (clim:find-menu-item "Skills" menu-table :errorp nil))
         (packages-menu (clim:find-menu-item "Packages" menu-table :errorp nil)))
    (is (not (null chat-menu)))
    (is (not (null view-menu)))
    (is (not (null skills-menu)))
    (is (not (null packages-menu)))
    (is (eq :menu (clim:command-menu-item-type chat-menu)))
    (is (eq :menu (clim:command-menu-item-type view-menu)))
    (is (eq :menu (clim:command-menu-item-type skills-menu)))
    (is (eq :menu (clim:command-menu-item-type packages-menu)))
    (let ((chat-table (clim:command-menu-item-value chat-menu))
          (view-table (clim:command-menu-item-value view-menu)))
      (flet ((menu-command (name table)
               (let ((item (clim:find-menu-item name table :errorp nil)))
                 (and item (clim:command-menu-item-value item)))))
        (is (eq 'clawmacs::com-chat-stop-response
                (menu-command "Stop Response" chat-table)))
        (is (eq 'clawmacs::com-chat-toggle-tool-results
                (menu-command "✓ Tool Results" view-table)))
        (is (eq 'clawmacs::com-chat-toggle-reasoning-output
                (menu-command "  Reasoning Output" view-table)))
        (is (eq 'clawmacs::com-chat-toggle-metadata-output
                (menu-command "  Metadata Output" view-table)))
        (is (eq 'clawmacs::com-chat-toggle-debug-mode
                (menu-command "  Debug Mode" view-table)))))
    (dolist (command '(clawmacs::com-chat-stop-response
                       clawmacs::com-chat-toggle-tool-results
                       clawmacs::com-chat-toggle-reasoning-output
                       clawmacs::com-chat-toggle-metadata-output
                       clawmacs::com-chat-toggle-debug-mode
                       clawmacs::com-chat-submit-compose
                       clawmacs::com-chat-toggle-skill
                       clawmacs::com-chat-toggle-package))
      (is (clim:command-accessible-in-command-table-p
           command menu-table)))))

(defun test-command-table-menu-labels (table)
  "Return menu labels from TABLE in display order."
  (let ((labels nil))
    (clim:map-over-command-table-menu-items
     (lambda (name keystroke item)
       (declare (ignore keystroke item))
       (push name labels))
     table
     :inherited nil)
    (nreverse labels)))

(defun test-chat-menu-submenu (table name)
  "Return the submenu table named NAME from TABLE."
  (clim:command-menu-item-value
   (clim:find-menu-item name table :errorp t)))

(test mcclim-chat-view-menu-shows-checkmarks-and-refreshes-frame-state
  "The McCLIM view menu shows checkmarks and refreshes after toggles."
  (let* ((buf (make-buffer "view-toolbar" :agent-name "agent"))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buf)))
    (let ((*debug-mode* nil))
      (clawmacs::refresh-chat-frame-menu-bar frame)
      (let ((view-menu (test-chat-menu-submenu
                        (clim:frame-command-table frame)
                        "View")))
        (is (member "✓ Tool Results"
                    (test-command-table-menu-labels view-menu)
                    :test #'string=))
        (is (member "  Reasoning Output"
                    (test-command-table-menu-labels view-menu)
                    :test #'string=))
        (is (member "  Metadata Output"
                    (test-command-table-menu-labels view-menu)
                    :test #'string=))
        (is (member "  Debug Mode"
                    (test-command-table-menu-labels view-menu)
                    :test #'string=)))
      (clawmacs::run-chat-frame-buffer-command
       frame
       #'clawmacs::toggle-reasoning-output-command)
      (clawmacs::run-chat-frame-buffer-command
       frame
       #'clawmacs::toggle-metadata-output-command)
      (clawmacs::run-chat-frame-buffer-command
       frame
       #'clawmacs::toggle-debug-mode-command)
      (let ((view-menu (test-chat-menu-submenu
                        (clim:frame-command-table frame)
                        "View")))
        (is (member "✓ Tool Results"
                    (test-command-table-menu-labels view-menu)
                    :test #'string=))
        (is (member "✓ Reasoning Output"
                    (test-command-table-menu-labels view-menu)
                    :test #'string=))
        (is (member "✓ Metadata Output"
                    (test-command-table-menu-labels view-menu)
                    :test #'string=))
        (is (member "✓ Debug Mode"
                    (test-command-table-menu-labels view-menu)
                    :test #'string=))))))

(test mcclim-chat-skills-menu-toggles-enabled-state
  "The McCLIM skills menu shows checkmarks and toggles persisted skill state."
  (with-isolated-skills (root)
    (write-demo-skill root :name "demo")
    (register-skill-root root)
    (let* ((buf (make-buffer "skill-toolbar"))
           (skill (first (list-skills :include-disabled t)))
           (key (clawmacs::skill-path-key skill)))
      (let ((menu-table (clawmacs::make-chat-menu-bar-command-table buf)))
        (is (member "✓ demo"
                    (test-command-table-menu-labels
                     (test-chat-menu-submenu menu-table "Skills"))
                    :test #'string=)))
      (is-false (clawmacs::toggle-chat-skill-for-buffer buf key))
      (let ((menu-table (clawmacs::make-chat-menu-bar-command-table buf)))
        (is (member "  demo"
                    (test-command-table-menu-labels
                     (test-chat-menu-submenu menu-table "Skills"))
                    :test #'string=)))
      (is-false (skill-enabled-p
                 (clawmacs::find-skill-by-path key :include-disabled t)))
      (is (search "[Skill demo disabled]"
                  (message-text (message-prev (buffer-input-message buf))))))))

(test mcclim-chat-frame-menu-tables-are-frame-local
  "Each McCLIM chat frame owns package menu state for its own buffer."
  (with-package-state-override ((default-package-test-channels))
    (let* ((enabled-buffer (make-buffer "enabled-package-toolbar"
                                        :agent-name "agent"))
           (default-buffer (make-buffer "default-package-toolbar"
                                        :agent-name "agent"))
           (enabled-frame (clim:make-application-frame
                           'clawmacs::clawmacs-chat-frame
                           :buffer enabled-buffer))
           (default-frame (clim:make-application-frame
                           'clawmacs::clawmacs-chat-frame
                           :buffer default-buffer)))
      (clawmacs::set-buffer-package-name-enabled enabled-buffer "sexed" t)
      (clawmacs::refresh-chat-frame-menu-bar enabled-frame)
      (clawmacs::refresh-chat-frame-menu-bar default-frame)
      (is (not (eq (clim:frame-command-table enabled-frame)
                   (clim:frame-command-table default-frame))))
      (is (member "✓ sexed"
                  (test-command-table-menu-labels
                   (test-chat-menu-submenu
                    (clim:frame-command-table enabled-frame)
                    "Packages"))
                  :test #'string=))
      (is (member "  sexed"
                  (test-command-table-menu-labels
                   (test-chat-menu-submenu
                    (clim:frame-command-table default-frame)
                    "Packages"))
                  :test #'string=))
      (is (member "✓ sexed"
                  (test-command-table-menu-labels
                   (test-chat-menu-submenu
                    (clim:frame-command-table enabled-frame)
                    "Packages"))
                  :test #'string=)))))

(test mcclim-chat-packages-menu-toggles-buffer-package-state
  "The McCLIM packages menu toggles current-buffer package enablement."
  (with-package-state-override ((default-package-test-channels))
    (let ((buf (make-buffer "package-toolbar" :agent-name "agent")))
      (let ((menu-table (clawmacs::make-chat-menu-bar-command-table buf)))
        (is (member "  sexed"
                    (test-command-table-menu-labels
                     (test-chat-menu-submenu menu-table "Packages"))
                    :test #'string=)))
      (is-true (clawmacs::toggle-chat-package-for-buffer buf "sexed"))
      (let ((menu-table (clawmacs::make-chat-menu-bar-command-table buf)))
        (is (member "✓ sexed"
                    (test-command-table-menu-labels
                     (test-chat-menu-submenu menu-table "Packages"))
                    :test #'string=)))
      (is (member "sexed" (buffer-enabled-packages buf) :test #'string=))
      (is-false (clawmacs::package-enabled-globally-p "sexed"))
      (is-false (clawmacs::toggle-chat-package-for-buffer buf "sexed"))
      (let ((menu-table (clawmacs::make-chat-menu-bar-command-table buf)))
        (is (member "  sexed"
                    (test-command-table-menu-labels
                     (test-chat-menu-submenu menu-table "Packages"))
                    :test #'string=)))
      (is-false (member "sexed" (buffer-enabled-packages buf)
                        :test #'string=)))))

(test approval-policy-round-trips-sandbox-and-working-directory-settings
  "Guard policy JSON persists sandbox and working-directory defaults and overrides."
  (let ((path (temp-approval-policy-path)))
    (with-approval-policy-path-override (path)
      (clawmacs::set-approval-policy-default-sandbox-permission :workspace-write)
      (clawmacs::set-approval-policy-default-working-directory-permission
       :workspace)
      (clawmacs::set-approval-policy-sandbox-permission "write" :full-access)
      (clawmacs::set-approval-policy-working-directory-permission
       "write" :project-root)
      (clawmacs::save-approval-policy)
      (setf clawmacs::*approval-policy-registry* nil)
      (clawmacs::load-approval-policy)
      (is (eq :workspace-write
              (clawmacs:approval-policy-default-sandbox-permission)))
      (is (eq :workspace
              (clawmacs:approval-policy-default-working-directory-permission)))
      (is (eq :full-access
              (clawmacs:approval-policy-sandbox-permission "write")))
      (is (eq :project-root
              (clawmacs:approval-policy-working-directory-permission "write"))))))

(test approval-policy-project-settings-override-user-settings
  "Project-local guard settings override user policy for effective sandbox/workdir resolution."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (clawmacs::register-tool "guard-test-tool"
                             "Guard test tool"
                             '((:type . "object"))
                             :agent-allowed
                             (lambda (_args)
                               (declare (ignore _args))
                               "ok"))
    (let* ((path (temp-approval-policy-path))
           (project-root
             (make-pathname
              :directory (list :absolute "tmp"
                               (format nil "clawmacs-guard-project-~A"
                                       (gensym)))))
           (buffer (make-buffer "guard-project"
                                :working-directory project-root)))
      (with-approval-policy-path-override (path)
        (clawmacs::set-approval-policy-default-sandbox-permission :read-only)
        (clawmacs::set-approval-policy-default-working-directory-permission
         :workspace)
        (clawmacs::set-approval-policy-default-sandbox-permission
         :workspace-write
         :directory project-root)
        (clawmacs::set-approval-policy-default-working-directory-permission
         :project-root
         :directory project-root)
        (clawmacs::set-approval-policy-sandbox-permission
         "guard-test-tool" :full-access :directory project-root)
        (clawmacs::set-approval-policy-working-directory-permission
         "guard-test-tool" :any :directory project-root)
        (is (eq :full-access
                (clawmacs::effective-tool-sandbox-permission
                 "guard-test-tool" :buffer buffer)))
        (is (eq :any
                (clawmacs::effective-tool-working-directory-permission
                 "guard-test-tool" :buffer buffer)))
        (is (eq :workspace-write
                (clawmacs::effective-tool-sandbox-permission
                 "lisp_eval" :buffer buffer)))
        (is (eq :project-root
                (clawmacs::effective-tool-working-directory-permission
                 "lisp_eval" :buffer buffer)))))))

(test approval-policy-history-is-recorded-and-review-hook-runs
  "Guard history entries persist and notify approval review hooks."
  (let ((path (temp-approval-policy-path))
        (hook-calls nil))
    (with-approval-policy-path-override (path)
      (let* ((buffer (make-buffer "guard-history"
                                  :working-directory
                                  (uiop:pathname-directory-pathname path)))
             (*approval-review-hook*
               (list (lambda (buf tool-name decision policy entry)
                       (push (list :buffer (and buf (buffer-name buf))
                                   :tool tool-name
                                   :decision decision
                                   :policy policy
                                   :entry entry)
                             hook-calls)))))
        (clawmacs::approval-policy-record-history-entry
         buffer "write" :approve
         :policy :agent-with-permission
         :entry "write allowed")
        (let ((history (clawmacs:approval-policy-history-entries
                        :buffer buffer)))
          (is (plusp (length history)))
          (is (string= "write"
                       (cdr (assoc :tool-name (first history)))))
          (is (string= "approve"
                       (cdr (assoc :decision (first history)))))
          (is (string= "write allowed"
                       (cdr (assoc :entry (first history))))))
        (is (= 1 (length hook-calls)))
        (is (equal "guard-history"
                   (getf (first hook-calls) :buffer)))
        (is (equal "write" (getf (first hook-calls) :tool)))))))

(test init-tools-hides-lispi-tools-until-package-enabled
  "init-tools exposes built-in lisp_eval without lispi package tools."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (clawmacs::init-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<)))
      (is (equal '("lisp_eval") tool-names))
      (is (not (null (gethash "lisp_eval" clawmacs::*tool-table*))))
      (is-false (member "read" tool-names :test #'string=)))))

(test init-tools-only-reserves-lisp-eval
  "User tools may use Lispi names when Lispi is not active."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (clawmacs::register-tool
     "read"
     "User read tool."
     '((:type . "object")
       (:properties . nil))
     :agent-allowed
     (lambda (args)
       (declare (ignore args))
       "user-read"))
    (clawmacs::init-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<)))
      (is (equal '("lisp_eval" "read") tool-names))
      (is (string= "user-read"
                   (clawmacs:execute-tool "read" nil))))))

(test direct-tools-with-lispi-names-are-not-package-scoped
  "A direct user tool named like a Lispi tool remains visible without Lispi."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (clawmacs::init-tools)
    (clawmacs:set-package-enablement-scope "lispi" :global)
    (clawmacs:load-active-packages)
    (clawmacs:set-package-enablement-scope "lispi" :default)
    (clawmacs::register-tool
     "read"
     "User read tool."
     '((:type . "object")
       (:properties . nil))
     :agent-allowed
     (lambda (args)
       (declare (ignore args))
       "user-read"))
    (clawmacs::init-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<)))
      (is (equal '("lisp_eval" "read") tool-names))
      (is (string= "user-read"
                   (clawmacs:execute-tool "read" nil))))))

(test tool-definitions->responses-tools-encodes-empty-properties-as-object
  "Zero-arg tool schemas encode JSON object properties as `{}`, not `null`."
  (let* ((tools (vector
                 '((:name . "artifactum_list")
                   (:description . "List artifacts.")
                   (:input--schema
                    (:type . "object")
                    (:properties)
                    (:required . #())))))
         (json (clawmacs:api-json-encode
                (clawmacs::tool-definitions->responses-tools tools))))
    (is (search "\"properties\":{}" json))
    (is-false (search "\"properties\":null" json))))

(test tool-definitions->openai-tools-encodes-empty-properties-as-object
  "Chat-completions tool schemas also encode empty properties as `{}`."
  (let* ((tools (vector
                 '((:name . "artifactum_list")
                   (:description . "List artifacts.")
                   (:input--schema
                    (:type . "object")
                    (:properties)
                    (:required . #())))))
         (json (clawmacs:api-json-encode
                (clawmacs::tool-definitions->openai-tools tools))))
    (is (search "\"properties\":{}" json))
    (is-false (search "\"properties\":null" json))))

(test load-active-packages-reregisters-active-package-tools
  "Active package loading restores package tools after init-tools resets core."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (clawmacs::init-tools)
    (clawmacs:set-package-enablement-scope "lispi" :global)
    (clawmacs:load-active-packages)
    (is (not (null (gethash "read" clawmacs::*tool-table*))))
    (clrhash clawmacs::*tool-table*)
    (clawmacs::init-tools)
    (is (null (gethash "read" clawmacs::*tool-table*)))
    (clawmacs:load-active-packages)
    (let* ((*current-caller* :user)
           (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<)))
      (is (member "read" tool-names :test #'string=))
      (is (member "lisp_eval" tool-names :test #'string=)))))

(test init-tools-preserves-custom-tools
  "init-tools resets built-ins without wiping user-added tools."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (clawmacs::register-tool
     "custom_probe"
     "Custom probe tool."
     '((:type . "object")
       (:properties . ((:payload . ((:type . "string"))))))
     :agent-allowed
     (lambda (args)
       (declare (ignore args))
       "{\"ok\":true}"))
    (initialize-test-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool) (cdr (assoc :name tool))) tools)
                             #'string<)))
      (is (equal '("custom_probe" "edit" "find" "grep" "lisp_eval" "read" "write")
                 tool-names))
      (is (not (null (gethash "custom_probe" clawmacs::*tool-table*))))
      (is (not (null (gethash "lisp_eval" clawmacs::*tool-table*)))))))

(test default-file-tools-read-write-edit-plain-text
  "The default file tools accept Lisp data and mutate sandboxed text files."
  (with-tool-table-restored
    (let* ((root (uiop:ensure-directory-pathname
                  (temp-package-test-directory "file-tools")))
           (file (merge-pathnames "nested/demo.txt" root)))
      (ensure-directories-exist (merge-pathnames #P".keep" root))
      (let ((clawmacs::*sandbox-root* root))
        (initialize-test-tools)
        (let ((write-result
                (clawmacs:execute-tool
                 "write"
                 '(:path "nested/demo.txt"
                   :content "alpha
beta
gamma"))))
          (is (search "Successfully wrote" write-result))
          (is (string= "alpha
beta
gamma"
                       (uiop:read-file-string file))))
        (let ((read-result
                (clawmacs:execute-tool
                 "read"
                 '(:path "nested/demo.txt"
                   :limit 2))))
          (is (search "alpha" read-result))
          (is (search "beta" read-result))
          (is (search "Use offset=3 to continue" read-result)))
        (let ((edit-result
                (clawmacs:execute-tool
                 "edit"
                 '(:path "nested/demo.txt"
                   :old-text "beta"
                   :new-text "BETA"))))
          (is (search "Successfully replaced text" edit-result))
          (is (search "+BETA" edit-result))
          (is (string= "alpha
BETA
gamma"
                       (uiop:read-file-string file))))
        (let ((delete-result
                (clawmacs:execute-tool
                 "edit"
                 '(:path "nested/demo.txt"
                   :old-text "gamma"
                   :new-text ""))))
          (is (search "Successfully replaced text" delete-result))
          (is (string= "alpha
BETA
"
                       (uiop:read-file-string file))))
        (clawmacs:execute-tool
         "write"
         '(:path "nested/demo.txt"
           :content "reset"))
        (is (string= "reset" (uiop:read-file-string file)))))))

(test default-search-tools-find-files-and-grep-contents
  "find searches filenames and grep searches file contents inside the sandbox."
  (with-tool-table-restored
    (let* ((root (uiop:ensure-directory-pathname
                  (temp-package-test-directory "search-tools")))
           (source (merge-pathnames "src/alpha.lisp" root))
           (text (merge-pathnames "src/beta.txt" root))
           (ignored (merge-pathnames "node_modules/ignored.txt" root)))
      (ensure-directories-exist source)
      (ensure-directories-exist ignored)
      (write-test-file source "(defun alpha () :ok)
")
      (write-test-file text "intro
needle here
")
      (write-test-file ignored "needle should not be seen
")
      (let ((clawmacs::*sandbox-root* root))
        (initialize-test-tools)
        (let ((find-result (clawmacs:execute-tool
                            "find"
                            '(:pattern "*.lisp"))))
          (is (search "src/alpha.lisp" find-result))
          (is-false (search "src/beta.txt" find-result)))
        (let ((find-result (clawmacs:execute-tool
                            "find"
                            '(:pattern "beta"
                              :ignore-case t))))
          (is (search "src/beta.txt" find-result)))
        (let ((grep-result (clawmacs:execute-tool
                            "grep"
                            '(:pattern "needle"
                              :glob "*.txt"))))
          (is (search "src/beta.txt:2:needle here" grep-result))
          (is-false (search "node_modules" grep-result)))))))

(test lispi-package-exposes-default-tool-implementations
  "The default read/write/edit implementations live in the lispi package."
  (let* ((specs (lispi:default-tool-specs))
         (names (sort (mapcar (lambda (spec)
                                (getf spec :name))
                              specs)
                      #'string<)))
    (is (equal '("edit" "find" "grep" "read" "write") names)))
  (let* ((root (uiop:ensure-directory-pathname
                (temp-package-test-directory "lispi-tools")))
         (file (merge-pathnames "demo.txt" root)))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (let ((lispi:*sandbox-root* root))
      (is (search "Successfully wrote"
                  (lispi:execute-write
                   '(:path "demo.txt"
                     :content "one
two"))))
      (is (search "Use offset=2 to continue"
                  (lispi:execute-read
                   '(:path "demo.txt"
                     :limit 1))))
      (is (search "demo.txt"
                  (lispi:execute-find
                   '(:pattern "demo"))))
      (is (search "demo.txt:2:two"
                  (lispi:execute-grep
                   '(:pattern "two"))))
      (is (search "Successfully replaced text"
                  (lispi:execute-edit
                   '(:path "demo.txt"
                     :old-text "two"
                     :new-text "TWO"))))
      (is (string= "one
TWO"
                   (uiop:read-file-string file))))))

(test default-write-and-edit-tools-reject-unbalanced-parentheses
  "write and edit fail before touching disk when content would be unbalanced."
  (with-tool-table-restored
    (let* ((root (uiop:ensure-directory-pathname
                  (temp-package-test-directory "tool-paren-balance")))
           (file (merge-pathnames "sample.lisp" root))
           (balanced-with-ignored-parens
             (format nil
                     "(list \"(\" #\\) ; ignored )~% #| ignored ) #| nested ( |# |# :ok)")))
      (ensure-directories-exist (merge-pathnames #P".keep" root))
      (let ((clawmacs::*sandbox-root* root))
        (initialize-test-tools)
        (clawmacs:execute-tool
         "write"
         `(:path "sample.lisp"
           :content ,balanced-with-ignored-parens))
        (is (string= balanced-with-ignored-parens
                     (uiop:read-file-string file)))
        (signals error
          (clawmacs:execute-tool
           "write"
           '(:path "sample.lisp"
             :content "(defun broken ()")))
        (is (string= balanced-with-ignored-parens
                     (uiop:read-file-string file)))
        (clawmacs:execute-tool
         "write"
         '(:path "sample.lisp"
           :content "(defun foo () (+ 1 2))"))
        (signals error
          (clawmacs:execute-tool
           "edit"
           '(:path "sample.lisp"
             :old-text "(+ 1 2)"
             :new-text "(list 1 2")))
        (is (string= "(defun foo () (+ 1 2))"
                     (uiop:read-file-string file)))))))

(test default-edit-tool-rejects-missing-and-duplicate-old-text
  "edit requires :old-text to be present exactly once."
  (with-tool-table-restored
    (let* ((root (uiop:ensure-directory-pathname
                  (temp-package-test-directory "edit-tool-errors")))
           (file (merge-pathnames "sample.txt" root)))
      (ensure-directories-exist (merge-pathnames #P".keep" root))
      (write-test-file file "same
same
")
      (let ((clawmacs::*sandbox-root* root))
        (initialize-test-tools)
        (signals error
          (clawmacs:execute-tool
           "edit"
           '(:path "sample.txt"
             :old-text "missing"
             :new-text "replacement")))
        (signals error
          (clawmacs:execute-tool
           "edit"
           '(:path "sample.txt"
             :old-text "same"
             :new-text "replacement")))
        (signals error
          (clawmacs:execute-tool
           "edit"
           '(:path "sample.txt"
             :old-text ""
             :new-text "replacement")))))))

(test shell-exec-honors-timeout-and-returns-output
  "shell_exec records stdout/stderr and exit status on a fast command."
  (let* ((root (uiop:ensure-directory-pathname
                (temp-package-test-directory "shell-exec-fast"))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (let ((clawmacs::*sandbox-root* root))
      (let* ((result-json
               (clawmacs::execute-shell-exec
                '((:command . "printf 'hello'")
                  (:timeout . 5))))
             (result (clawmacs::api-json-decode result-json)))
        (is (string= "printf 'hello'" (cdr (assoc :command result))))
        (is (equal 0 (cdr (assoc :exit--code result))))
        (is-false (cdr (assoc :timed--out result)))
        (is (string= "hello" (cdr (assoc :stdout result))))
        (is (string= "" (cdr (assoc :stderr result))))))))

(test shell-exec-times-out-and-kills-the-process
  "shell_exec reports timeout when the command outlives its deadline."
  (let* ((root (uiop:ensure-directory-pathname
                (temp-package-test-directory "shell-exec-timeout"))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (let ((clawmacs::*sandbox-root* root))
      (let* ((result-json
               (clawmacs::execute-shell-exec
                '((:command . "sleep 2")
                  (:timeout . 0.2))))
             (result (clawmacs::api-json-decode result-json)))
        (is (string= "sleep 2" (cdr (assoc :command result))))
        (is (cdr (assoc :timed--out result)))
        (is-false (cdr (assoc :exit--code result)))
        (is (string= "" (cdr (assoc :stdout result))))
        (is (string= "" (cdr (assoc :stderr result))))))))

(test build-system-prompt-is-compact-and-pi-style
  "The default system prompt lists the active provider tool surface."
  (with-tool-table-restored
    (initialize-test-tools)
    (with-function-override (clawmacs::load-boot-files ()
                              nil)
      (with-package-state-override ((default-package-test-channels))
        (clawmacs:set-package-enablement-scope "lispi" :global)
        (clawmacs:load-autoload-packages)
        (is-false (search "## Structural editing with sexed"
                          clawmacs::*default-core-system-prompt*))
        (let ((prompt (clawmacs::build-system-prompt)))
          (is (search "operating inside clawmacs" prompt))
          (is (search "## Tools" prompt))
          (is (search "- read: Read the contents of a text file" prompt))
          (is (search "- find: Search for files" prompt))
          (is (search "- grep: Search file contents" prompt))
          (is (search "- write: Create or overwrite a text file" prompt))
          (is (search "- edit: Edit a text file" prompt))
          (is (search "- lisp_eval: Evaluate one Common Lisp form" prompt))
          (is (search "Tool calls and tool results use Lisp data mode" prompt))
          (is (search ":old-text" prompt))
          (is (search ":new-text" prompt))
          (is (search "Prefer provider tools for normal work" prompt))
          (is (search "Use find to locate files by name" prompt))
          (is (search "Use grep to locate literal text" prompt))
          (is (search "Use `lisp_eval` for testing" prompt))
          (is (search "Current date:" prompt))
          (is (search "Current working directory:" prompt))
          (is (search (clawmacs::current-system-prompt-date) prompt))
          (is-false (search "only built-in tool available by default" prompt))
          (is-false (search "## Subagents" prompt))
          (is-false (search "Project file-buffer example" prompt))
          (is-false (search "Use the `sexed-*` functions for Lisp source edits" prompt))
          (is-false (search "(sexed-outline-to-string TEXT :max-depth 2)" prompt))
          (is-false (search "(sexed-replace-project-form \"PROJECT\" \"PATH\" SELECTOR NEW-TEXT)" prompt))
          (is-false (search "(sexed-stage-replace-project-form \"PROJECT\" \"PATH\" SELECTOR NEW-TEXT)" prompt))
          (is-false (search "(sexed-replace-scratch-form SELECTOR NEW-TEXT)" prompt))
          (is-false (search "(sexed-init-outline-to-string :max-depth 3)" prompt))
          (is-false (search "Do not try to set" prompt))
          (is-false (search "Message adapters such as `sexed-replace-message-form` take a `message` object" prompt))
          (is-false (search "fetching URLs, reading/writing files, running shell commands" prompt))
          (is-false (search "http_fetch" prompt))
          (is-false (search "shell_exec" prompt))
          (is-false (search "file_read" prompt)))))))

(test package-deftool-appears-in-system-prompt
  "Package entrypoints can register provider tools by evaluating deftool."
  (let* ((channel-root
           (make-package-channel-root
            :label "package-tool-channel"
            :package-name "package-tool"
            :manifest "(:name \"package-tool\"
 :description \"Package tool\"
 :entrypoint \"entry.lisp\"
 :autoload t)"
            :entrypoint-content
            "(defun package-tool-probe (value)
  (format nil \"package=~A\" value))
(deftool package-tool-probe
  :name \"package_probe\"
  :description \"Probe tool from a package.\"
  :args ((value :type \"string\" :description \"Value to echo.\")))")))
    (let ((clawmacs::*agent-tool-metadata-table* (make-hash-table :test #'eq))
          (clawmacs::*agent-tool-name-table* (make-hash-table :test #'equal)))
      (with-tool-table-restored
        (clrhash clawmacs::*tool-table*)
        (with-package-state-override (nil)
          (clawmacs:register-package-channel "custom" channel-root
                                             :description "Custom channel")
          (clawmacs:load-clawmacs-package "package-tool")
          (let* ((inactive-tools (coerce (clawmacs::tool-definitions-for-api) 'list))
                 (inactive-tool-names (mapcar (lambda (tool)
                                                (cdr (assoc :name tool)))
                                              inactive-tools)))
            (is-false (member "package_probe" inactive-tool-names :test #'string=))
            (signals error
              (clawmacs:execute-tool "package_probe" '(:value "nope"))))
          (clawmacs:set-package-enablement-scope "package-tool" :global)
          (clawmacs:load-active-packages)
          (let* ((tools (coerce (clawmacs::tool-definitions-for-api) 'list))
                 (tool-names (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools))
                 (prompt (clawmacs::build-system-prompt)))
            (is (member "package_probe" tool-names :test #'string=))
            (is (search "- package_probe: Probe tool from a package." prompt))
            (is (string= "package=ok"
                         (clawmacs:execute-tool "package_probe"
                                                '(:value "ok"))))))))))

(test register-agent-definition-round-trips-through-registry
  "Programmatic agent definitions can be registered, replaced, found, and listed."
  (with-agent-definition-registry-override ()
    (let ((first (clawmacs:register-agent-definition
                  "Pair"
                  :provider :openai-codex
                  :model "gpt-5.4"
                  :think-level "high"
                  :core-prompt "pair core"
                  :personality-prompt "pair personality"
                  :tool-names '("lisp_eval" doc-lookup))))
      (is (string= "Pair" (clawmacs:agent-definition-name first)))
      (is (eq :openai-codex (clawmacs:agent-definition-provider first)))
      (is (string= "high" (clawmacs:agent-definition-think-level first)))
      (is (equal '("lisp_eval" "doc_lookup")
                 (clawmacs:agent-definition-tool-names first))))
    (is (string= "pair core"
                 (clawmacs:agent-definition-core-prompt
                  (clawmacs:find-agent-definition "pair"))))
    (is (string= "pair personality"
                 (clawmacs:agent-definition-personality-prompt
                  (clawmacs:find-agent-definition "pair"))))
    (clawmacs:register-agent-definition "Writer" :personality-prompt "writer personality")
    (clawmacs:register-agent-definition "pair" :provider :zai :model "glm-5")
    (let* ((found (clawmacs:find-agent-definition "PAIR"))
           (listed (clawmacs:list-agent-definitions)))
      (is (eq :zai (clawmacs:agent-definition-provider found)))
      (is (string= "glm-5" (clawmacs:agent-definition-model found)))
      (is (equal '("pair" "Writer")
                 (mapcar #'clawmacs:agent-definition-name listed))))))

(test register-pipeline-definition-round-trips-through-registry
  "Programmatic pipeline definitions normalize stages and can be listed."
  (with-pipeline-definition-registry-override ()
    (let ((definition
            (clawmacs:define-pipeline
             "Plan Build Test"
             :description "plan then build"
             :entry-stage "plan"
             :max-steps 4
             :stages '((:name "plan"
                        :agent "planner"
                        :prompt "Plan {{input}}"
                        :next "build")
                       (:name build
                        :agent "builder"
                        :prompt "Build {{stage:plan}}")))))
      (is (string= "plan build test"
                   (clawmacs:pipeline-definition-name definition)))
      (is (string= "plan"
                   (clawmacs:pipeline-definition-entry-stage definition)))
      (is (= 2 (length (clawmacs:pipeline-definition-stages definition))))
      (is (eq definition
              (clawmacs:find-pipeline-definition "PLAN BUILD TEST")))
      (is (equal '("plan build test")
                 (mapcar #'clawmacs:pipeline-definition-name
                         (clawmacs:list-pipeline-definitions)))))))

(test run-pipeline-prompt-pipes-stage-output
  "Pipeline stages can template the original prompt and prior stage output."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (request-payloads nil))
    (with-agent-defaults-path-override (path)
      (with-pipeline-definition-registry-override ()
        (let ((*sessions-dir* (temp-session-test-directory "pipeline-prompt")))
          (clawmacs:define-pipeline
           "plan-build"
           :stages '((:name "plan"
                      :agent "planner"
                      :prompt "Plan this request: {{input}}"
                      :next "build")
                     (:name "build"
                      :agent "builder"
                      :prompt "Build from this plan: {{stage:plan}}")))
          (with-function-override (clawmacs::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider callback model
                                                    max-tokens tools
                                                    reasoning-effort
                                                    system-prompt))
                                   (incf request-count)
                                   (push (clawmacs::api-json-encode messages)
                                         request-payloads)
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (clawmacs::canonical-text-block
                                           (if (= request-count 1)
                                               "PLAN OK"
                                               "BUILD OK")))))
            (clawmacs::init-default-keymap)
            (initialize-test-tools)
            (let ((result (clawmacs:run-pipeline-prompt
                           "ship fizzbuzz"
                           "plan-build"
                           :provider :zai
                           :model "glm-5")))
              (is (= 2 request-count))
              (is (string= "BUILD OK"
                           (clawmacs:prompt-run-result-final-text result)))
              (is (string= "plan-build"
                           (clawmacs:prompt-run-result-agent-name result)))
              (is (= 2 (clawmacs:prompt-run-result-iterations result)))
              (let ((payloads (nreverse request-payloads)))
                (is (search "Plan this request: ship fizzbuzz"
                            (first payloads)))
                (is (search "Build from this plan: PLAN OK"
                            (second payloads)))))))))))

(test run-pipeline-on-buffer-supports-decision-loops
  "A stage :next function can deterministically route back to an earlier stage."
  (let ((path (temp-agent-defaults-path))
        (responses '("PLAN 1" "FAIL tests" "PLAN 2" "PASS tests"))
        (stage-order nil))
    (with-agent-defaults-path-override (path)
      (with-pipeline-definition-registry-override ()
        (let ((*sessions-dir* (temp-session-test-directory "pipeline-loop")))
          (clawmacs:define-pipeline
           "repair"
           :max-steps 6
           :stages `((:name "plan"
                      :agent "planner"
                      :prompt "Plan: {{input}} {{previous}}"
                      :next "test")
                     (:name "test"
                      :agent "tester"
                      :prompt "Test the latest plan: {{stage:plan}}"
                      :next ,(lambda (_context result)
                               (declare (ignore _context))
                               (if (search "FAIL"
                                           (or (clawmacs:pipeline-stage-result-final-text
                                                result)
                                               "")
                                           :test #'char-equal)
                                   "plan"
                                   nil)))))
          (with-function-override (clawmacs::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider messages callback
                                                    model max-tokens tools
                                                    reasoning-effort
                                                    system-prompt))
                                   (let ((text (pop responses)))
                                     (make-completed-stream-state-response
                                      "end_turn"
                                      (list (clawmacs::canonical-text-block
                                             text)))))
            (clawmacs::init-default-keymap)
            (initialize-test-tools)
            (let* ((buf (clawmacs::make-prompt-buffer "fix failing tests"
                                                       "agent"))
                   (result (clawmacs:run-pipeline-on-buffer
                            "repair"
                            "fix failing tests"
                            :buffer buf)))
              (setf stage-order
                    (mapcar #'clawmacs:pipeline-stage-result-stage-name
                            (clawmacs:pipeline-run-result-stage-results
                             result)))
              (is (equal '("plan" "test" "plan" "test")
                         stage-order))
              (is (eq :succeeded
                      (clawmacs:pipeline-run-result-status result)))
              (is (string= "PASS tests"
                           (clawmacs:pipeline-run-result-final-text
                            result))))))))))

(test run-pipeline-test-profiles-captures-command-results
  "Deterministic pipeline test profiles run shell commands and summarize failures."
  (with-pipeline-definition-registry-override ()
    (clawmacs:register-pipeline-test-profile
     "pass"
     :description "Passing probe"
     :command "printf 'ok\\n'")
    (clawmacs:register-pipeline-test-profile
     "fail"
     :description "Failing probe"
     :command "printf 'nope\\n' >&2; exit 3")
    (let ((report (clawmacs:run-pipeline-test-profiles
                   '("pass" "fail")
                   :directory (uiop:ensure-directory-pathname (truename ".")))))
      (is-false (getf report :passed-p))
      (is (equal '("pass" "fail") (getf report :profiles)))
      (is (= 2 (length (getf report :results))))
      (is (search "pass: PASS" (getf report :summary) :test #'char-equal))
      (is (search "fail: FAIL" (getf report :summary) :test #'char-equal))
      (is (string= "ok
" (getf (first (getf report :results)) :stdout)))
      (is (= 3 (getf (second (getf report :results)) :exit-code))))))

(test bundled-self-modify-pipeline-loops-and-injects-selected-packages-and-skills
  "The shipped self-modify pipeline reparses plans after failing tests and injects selected package/skill context."
  (let ((path (temp-agent-defaults-path))
        (responses nil)
        (test-reports nil)
        (captured-calls nil))
    (with-isolated-skills (root)
      root
      (register-skill-definition
       "demo-skill"
       :description "Demo self-modify skill"
       :contents "---\nname: demo-skill\ndescription: Demo self-modify skill\n---\nUse the DEMO-SKILL marker when this skill is injected.\n")
      (with-agent-defaults-path-override (path)
        (with-approval-policy-path-override ((temp-approval-policy-path))
          (let ((clawmacs::*approval-policy-isolation-root*
                  (temp-package-test-directory
                   "self-modify-approval-isolation")))
            (with-pipeline-definition-registry-override ()
              (let ((clawmacs::*agent-definition-registry*
                      (make-hash-table :test #'equal))
                    (clawmacs::*tool-table*
                      (make-hash-table :test #'equal))
                    (clawmacs::*agent-tool-metadata-table*
                      (make-hash-table :test #'eq))
                    (clawmacs::*agent-tool-name-table*
                      (make-hash-table :test #'equal))
                    (clawmacs::*command-table*
                      (make-hash-table :test #'eq))
                    (clawmacs::*extended-docs*
                      (make-hash-table :test #'eq))
                    (clawmacs::*slash-command-table*
                      (make-hash-table :test #'equal))
                    (clawmacs::*compaction-point* nil))
                (with-package-state-override ((default-package-test-channels))
              (setf responses
                    (list
                     (list :kind :text
                           :text "{\"plan\":\"Round 1 plan\",\"implementation\":\"Implement round 1\",\"packages\":[\"sexed\"],\"skills\":[\"demo-skill\"],\"tests\":[\"unit\"],\"docs\":\"Update docs\",\"update_init\":false,\"init\":\"\"}")
                     (list :kind :text
                           :text "IMPLEMENT ROUND 1")
                     (list :kind :tool
                           :id "toolu_test_1"
                           :name "prove_run"
                           :input '((:methods . #("unit"))))
                     (list :kind :text
                           :text "{\"passed\":false,\"summary\":\"unit failed\",\"feedback\":\"unit failed on first run\",\"tests\":[\"unit\"]}")
                     (list :kind :text
                           :text "{\"plan\":\"Round 2 plan\",\"implementation\":\"Implement round 2\",\"packages\":[\"sexed\"],\"skills\":[\"demo-skill\"],\"tests\":[\"unit\"],\"docs\":\"Refresh docs\",\"update_init\":true,\"init\":\"Enable the new workflow in init.\"}")
                     (list :kind :text
                           :text "IMPLEMENT ROUND 2")
                     (list :kind :tool
                           :id "toolu_test_2"
                           :name "prove_run"
                           :input '((:methods . #("unit"))))
                     (list :kind :text
                           :text "{\"passed\":true,\"summary\":\"unit passed\",\"feedback\":\"unit passed on second run\",\"tests\":[\"unit\"]}")
                     (list :kind :text :text "DOCS DONE")
                     (list :kind :text :text "INIT DONE")
                     (list :kind :text :text "INIT DONE"))
                    test-reports
                    (list
                     (list :passed-p nil
                           :profiles '("unit")
                           :results (list (list :name "unit"
                                                :command "unit"
                                                :directory "/tmp"
                                                :exit-code 1
                                                :stdout "failing output"
                                                :stderr "failing stderr"
                                                :passed-p nil))
                           :summary "unit: FAIL (exit 1)\n\nOverall: FAILED")
                     (list :passed-p t
                           :profiles '("unit")
                           :results (list (list :name "unit"
                                                :command "unit"
                                                :directory "/tmp"
                                                :exit-code 0
                                                :stdout "passing output"
                                                :stderr ""
                                                :passed-p t))
                           :summary "unit: PASS (exit 0)\n\nOverall: PASSED")))
              (clawmacs:set-package-enablement-scope "pipelines" :global)
              (clawmacs:load-autoload-packages)
              (with-function-override
                  (clawmacs:run-pipeline-test-profiles
                   (profile-names &key directory)
                   (declare (ignore directory))
                   (is (equal '("unit") profile-names))
                   (or (pop test-reports)
                       (error "no more deterministic test reports")))
                (with-function-override
                    (clawmacs::provider-request-streaming
                     (provider messages callback
                               &key model max-tokens tools
                                 reasoning-effort system-prompt)
                     (declare (ignore provider callback model
                                      max-tokens
                                      reasoning-effort))
                     (let ((response (or (pop responses)
                                         (error "no more provider responses"))))
                       (push (list :messages messages
                                   :messages-json (clawmacs::api-json-encode messages)
                                   :tool-names (mapcar (lambda (tool)
                                                         (cdr (assoc :name tool)))
                                                       (if (vectorp tools)
                                                           (coerce tools 'list)
                                                           tools))
                                   :system-prompt system-prompt)
                             captured-calls)
                       (ecase (getf response :kind)
                         (:text
                          (make-completed-stream-state-response
                           "end_turn"
                           (list (clawmacs::canonical-text-block
                                  (getf response :text)))))
                         (:tool
                          (make-completed-stream-state-response
                           "tool_use"
                           (list (clawmacs::canonical-tool-use-block
                                  (getf response :id)
                                  (getf response :name)
                                  (getf response :input))))))))
                  (clawmacs::init-default-keymap)
                  (initialize-test-tools)
                  (let* ((buf (clawmacs::make-prompt-buffer
                               "build a self-modifying workflow"
                               "agent"))
                         (result (clawmacs:run-pipeline-on-buffer
                                  "self-modify"
                                  "build a self-modifying workflow"
                                  :buffer buf))
                         (stage-order
                           (mapcar #'clawmacs:pipeline-stage-result-stage-name
                                   (clawmacs:pipeline-run-result-stage-results
                                    result)))
                         (calls (nreverse captured-calls))
                         (first-implement (second calls))
                         (first-test (third calls))
                         (second-plan (fifth calls))
                         (init-call (first (last calls))))
                    (is (equal '("plan" "implement" "test"
                                 "plan" "implement" "test"
                                 "docs" "init")
                               stage-order))
                    (is (eq :succeeded
                            (clawmacs:pipeline-run-result-status result)))
                    (is (string= "INIT DONE"
                                 (clawmacs:pipeline-run-result-final-text
                                  result)))
                    (is (search "Structural editing with sexed"
                                (getf first-implement :system-prompt)
                                :test #'char-equal))
                    (is (search "<skill>"
                                (getf first-implement :messages-json)
                                :test #'char-equal))
                    (is (search "DEMO-SKILL marker"
                                (getf first-implement :messages-json)
                                :test #'char-equal))
                    (is (search "Self-testing with prove"
                                (getf first-test :system-prompt)
                                :test #'char-equal))
                    (is (equal '("prove_list_methods" "prove_run")
                               (getf first-test :tool-names)))
                    (is (search "unit failed on first run"
                                (getf second-plan :messages-json)
                                :test #'char-equal))
                    (is (search (namestring clawmacs::*user-init-file*)
                                (princ-to-string (getf init-call :messages))
                                :test #'char-equal))))))))))))))

(test run-pipeline-on-buffer-reports-invalid-route
  "Pipeline route errors are returned as failed pipeline results."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-pipeline-definition-registry-override ()
        (let ((*sessions-dir* (temp-session-test-directory "pipeline-route")))
          (clawmacs:define-pipeline
           "bad-route"
           :stages '((:name "start"
                      :prompt "Start: {{input}}"
                      :next "missing")))
          (with-function-override (clawmacs::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider messages callback
                                                    model max-tokens tools
                                                    reasoning-effort
                                                    system-prompt))
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (clawmacs::canonical-text-block
                                           "DONE"))))
            (clawmacs::init-default-keymap)
            (initialize-test-tools)
            (let* ((buf (clawmacs::make-prompt-buffer "go" "agent"))
                   (result (clawmacs:run-pipeline-on-buffer
                            "bad-route"
                            "go"
                            :buffer buf)))
              (is (eq :failed
                      (clawmacs:pipeline-run-result-status result)))
              (is (= 1
                     (length (clawmacs:pipeline-run-result-stage-results
                              result))))
              (is (search "no stage named missing"
                          (clawmacs:pipeline-run-result-error result)
                          :test #'char-equal)))))))))

(test send-message-runs-active-buffer-pipeline
  "Interactive send-message dispatches to a buffer pipeline when one is set."
  (let ((path (temp-agent-defaults-path))
        (request-payload nil))
    (with-agent-defaults-path-override (path)
      (with-pipeline-definition-registry-override ()
        (let ((*sessions-dir* (temp-session-test-directory "pipeline-send")))
          (clawmacs:define-pipeline
           "one-stage"
           :stages '((:name "stage"
                      :agent "pipeline-agent"
                      :prompt "Pipeline saw: {{input}}")))
          (with-function-override (clawmacs::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider callback model
                                                    max-tokens tools
                                                    reasoning-effort
                                                    system-prompt))
                                   (setf request-payload
                                         (clawmacs::api-json-encode messages))
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (clawmacs::canonical-text-block
                                           "PIPELINE DONE"))))
            (clawmacs::init-default-keymap)
            (initialize-test-tools)
            (let ((buf (make-buffer "pipeline-chat"
                                    :pipeline-name "one-stage")))
              (clawmacs:set-buffer-provider-override buf :zai)
              (clawmacs:set-buffer-model-override buf "glm-5")
              (clawmacs::set-message-text (buffer-input-message buf)
                                          "hello pipeline")
              (clawmacs::send-message buf)
              (is (string= "PIPELINE DONE"
                           (message-text
                            (message-prev (buffer-input-message buf)))))
              (is (eq :idle (buffer-status buf)))
              (is (search "Pipeline saw: hello pipeline"
                          request-payload)))))))))

(test build-agent-system-prompt-composes-boot-core-and-personality
  "Agent prompts are composed in boot -> core -> personality -> runtime order."
  (with-isolated-skills (root)
    root
    (with-agent-definition-registry-override ()
      (clawmacs:register-agent-definition
       "pair"
       :core-prompt "PAIR CORE"
       :personality-prompt "PAIR PERSONALITY")
      (with-function-override (clawmacs::load-boot-files ()
                                "BOOT PREFIX")
        (let* ((prompt (clawmacs:build-agent-system-prompt "pair"))
               (boot-pos (search "BOOT PREFIX" prompt))
               (core-pos (search "PAIR CORE" prompt))
               (personality-pos (search "PAIR PERSONALITY" prompt))
               (date-pos (search "Current date:" prompt)))
          (is (not (null boot-pos)))
          (is (not (null core-pos)))
          (is (not (null personality-pos)))
          (is (not (null date-pos)))
          (is (< boot-pos core-pos personality-pos date-pos)))))))

(test load-boot-files-injects-ancestor-agents-md-instructions
  "Boot-file loading discovers AGENTS.md from the active directory ancestry."
  (let* ((root (temp-package-test-directory "agents-injection"))
         (nested (merge-pathnames #P"src/ui/" root))
         (root-agents (merge-pathnames "AGENTS.md" root))
         (nested-agents (merge-pathnames "AGENTS.md" nested)))
    (ensure-directories-exist (merge-pathnames #P".keep" nested))
    (write-test-file root-agents "ROOT AGENTS MARKER")
    (write-test-file nested-agents "NESTED AGENTS MARKER")
    (let* ((clawmacs::*boot-file-names* '("AGENTS.md"))
           (instructions (clawmacs:load-boot-files :directory nested))
           (root-pos (search "ROOT AGENTS MARKER" instructions))
           (nested-pos (search "NESTED AGENTS MARKER" instructions)))
      (is (search "# AGENTS.md instructions for " instructions))
      (is (search "<INSTRUCTIONS>" instructions))
      (is (search "</INSTRUCTIONS>" instructions))
      (is (not (null root-pos)))
      (is (not (null nested-pos)))
      (is (< root-pos nested-pos)))))

(test build-agent-system-prompt-injects-buffer-working-directory-agents-md
  "System prompts use the buffer working directory for AGENTS.md injection."
  (with-tool-table-restored
    (with-isolated-skills (skills-root)
      skills-root
      (with-package-state-override (nil)
        (let* ((root (temp-package-test-directory "buffer-agents-injection"))
               (nested (merge-pathnames #P"project/subdir/" root))
               (agents-path (merge-pathnames "AGENTS.md" root)))
          (ensure-directories-exist (merge-pathnames #P".keep" nested))
          (write-test-file agents-path "BUFFER WORKING DIRECTORY AGENTS")
          (let* ((clawmacs::*boot-file-names* '("AGENTS.md"))
                 (clawmacs::*default-core-system-prompt* "CORE")
                 (clawmacs::*default-personality-prompt* "PERSONALITY")
                 (clawmacs::*buffer-system-prompt-display-enabled* nil)
                 (buf (make-chat-buffer
                       "agents-buffer"
                       :working-directory nested
                       :session-persistence-mode :ephemeral))
                 (prompt (clawmacs:build-agent-system-prompt "agent"
                                                             :buffer buf)))
            (is (search "BUFFER WORKING DIRECTORY AGENTS" prompt))
            (is (search (namestring nested) prompt))
            (let ((clawmacs::*current-tool-buffer* buf))
              (is (equal (buffer-working-directory buf)
                         (clawmacs::default-prompt-working-directory))))))))))

(test build-agent-system-prompt-falls-back-to-default-components
  "Missing agent prompt slots fall back to the default core and personality prompts."
  (with-agent-definition-registry-override ()
    (let ((clawmacs::*default-personality-prompt* "DEFAULT PERSONALITY"))
      (clawmacs:register-agent-definition "writer" :personality-prompt "WRITER PERSONALITY")
      (with-function-override (clawmacs::load-boot-files ()
                                nil)
        (let ((prompt (clawmacs:build-agent-system-prompt "writer")))
          (is (search "Tool calls and tool results use Lisp data mode" prompt))
          (is (search "WRITER PERSONALITY" prompt))
          (is-false (search "DEFAULT PERSONALITY" prompt))))
      (with-function-override (clawmacs::load-boot-files ()
                                nil)
        (let ((prompt (clawmacs:build-agent-system-prompt "missing")))
          (is (search "Tool calls and tool results use Lisp data mode" prompt))
          (is (search "DEFAULT PERSONALITY" prompt)))))))

(test parse-clawmacs-prompt-args-supports-routing-and-output-options
  "The one-shot prompt parser accepts routing, visibility, and prompt text."
  (let ((options (clawmacs::parse-clawmacs-prompt-args
                  '("--agent" "writer"
                    "--provider" "openai-codex"
                    "--model" "gpt-5.4"
                    "--think" "high"
                    "--model-role" "review"
                    "--service-tier" "priority"
                    "--show-tools"
                    "--show-reasoning"
                    "--show-metadata"
                    "--clean-build"
                    "--isolated"
                    "--json"
                    "--package" "sexed"
                    "--package" "lispi"
                    "--session" "cache-probe"
                    "--continue"
                    "--ephemeral"
                    "--pipeline" "plan-build"
                    "--max-tool-iterations" "7"
                    "summarize" "this"))))
    (is (string= "writer" (clawmacs::prompt-options-agent-name options)))
    (is (string= "openai-codex" (clawmacs::prompt-options-provider options)))
    (is (string= "gpt-5.4" (clawmacs::prompt-options-model options)))
    (is (string= "high" (clawmacs::prompt-options-think-level options)))
    (is (string= "review" (clawmacs::prompt-options-model-role options)))
    (is (string= "priority" (clawmacs::prompt-options-service-tier options)))
    (is (clawmacs::prompt-options-show-tools-p options))
    (is (clawmacs::prompt-options-show-reasoning-p options))
    (is (clawmacs::prompt-options-show-metadata-p options))
    (is (clawmacs::prompt-options-json-p options))
    (is (clawmacs::prompt-options-isolated-p options))
    (is (equal '("sexed" "lispi")
               (clawmacs::prompt-options-packages options)))
    (is (string= "cache-probe"
                 (clawmacs::prompt-options-session-name options)))
    (is (clawmacs::prompt-options-continue-session-p options))
    (is (clawmacs::prompt-options-ephemeral-p options))
    (is (string= "plan-build"
                 (clawmacs::prompt-options-pipeline-name options)))
    (is (= 7 (clawmacs::prompt-options-max-tool-iterations options)))
    (is (string= "summarize this" (clawmacs::prompt-options-prompt options)))))

(test parse-clawmacs-prompt-args-supports-no-session-alias
  "The prompt parser accepts --no-session as an explicit ephemeral alias."
  (let ((options (clawmacs::parse-clawmacs-prompt-args
                  '("--no-session" "say" "hello"))))
    (is (clawmacs::prompt-options-ephemeral-p options))
    (is (string= "say hello" (clawmacs::prompt-options-prompt options)))))

(test parse-clawmacs-prompt-args-supports-jsonl-and-output-schema
  "Prompt parser accepts JSONL streaming and structured-output schema flags."
  (let ((options (clawmacs::parse-clawmacs-prompt-args
                  '("--jsonl"
                    "--output-schema" "{\"type\":\"object\"}"
                    "emit" "json"))))
    (is (clawmacs::prompt-options-jsonl-p options))
    (is (string= "{\"type\":\"object\"}"
                 (clawmacs::prompt-options-output-schema options)))
    (is (string= "emit json" (clawmacs::prompt-options-prompt options)))))

(test write-prompt-run-result-jsonl-emits-turn-completed-record
  "JSONL prompt output writes a turn-completed record with structured output."
  (let* ((result (clawmacs::make-prompt-run-result
                  :prompt "emit json"
                  :final-text "{\"status\":\"ok\"}"
                  :structured-output '((:status . "ok"))
                  :agent-name "writer"
                  :provider :zai
                  :model "glm-5"
                  :iterations 1
                  :stop-reason "end_turn"))
         (options (clawmacs::make-prompt-options :jsonl-p t))
         (output (with-output-to-string (stream)
                   (let ((*standard-output* stream))
                     (clawmacs::write-prompt-run-result result options)))))
    (is (search "\"event\":\"turn.completed\"" output))
    (is (search "\"final_text\":\"{\\\"status\\\":\\\"ok\\\"}\"" output))
    (is (search "\"structured_output\":{\"status\":\"ok\"}" output))))

(test run-prompt-options-resolves-continue-session-for-current-directory
  "Prompt options can resume the most recent saved session for the cwd."
  (let* ((*sessions-dir* (temp-session-test-directory "continue-prompt"))
         (working-directory (temp-session-test-directory "continue-prompt-cwd"))
         (session (load-or-create-session "continued-session"
                                          :working-directory working-directory))
         (buf (make-buffer "continued-session"
                           :agent-name "echo"
                           :working-directory working-directory
                           :session session))
         (captured nil))
    (ensure-directories-exist (merge-pathnames #P".keep" working-directory))
    (save-session buf)
    (let ((*default-pathname-defaults* working-directory))
      (with-function-override (clawmacs::run-session-prompt
                               (prompt &key session-name &allow-other-keys)
                               (setf captured (list prompt session-name))
                               :continued)
        (let ((result (clawmacs::run-prompt-options
                       (clawmacs::make-prompt-options
                        :prompt "continue now"
                        :continue-session-p t))))
          (is (eq :continued result))
          (is (equal '("continue now" "continued-session")
                     captured)))))))

(test run-prompt-options-ephemeral-ignores-session-routing
  "Ephemeral prompt options bypass saved-session routing and run in no-session mode."
  (let ((captured nil))
    (with-function-override (clawmacs::run-single-prompt
                             (prompt &key session-persistence-mode
                                     session-name continue-session-p
                                     &allow-other-keys)
                             (declare (ignore session-name continue-session-p))
                             (setf captured (list prompt session-persistence-mode))
                             :ephemeral)
      (with-function-override (clawmacs::run-session-prompt
                               (prompt &rest args)
                               (declare (ignore prompt args))
                               :unexpected-session)
        (let ((result (clawmacs::run-prompt-options
                       (clawmacs::make-prompt-options
                        :prompt "no session"
                        :session-name "saved-session"
                        :continue-session-p t
                        :ephemeral-p t))))
          (is (eq :ephemeral result))
          (is (equal '("no session" :ephemeral)
                     captured)))))))

(test run-prompt-options-honors-ephemeral-mode-without-creating-session
  "Explicit ephemeral prompt mode keeps one-shot runs out of the session store."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (seen-persistence-mode nil)
        (*sessions-dir* (temp-session-test-directory "ephemeral-prompt")))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback
                                                  model max-tokens tools
                                                  reasoning-effort
                                                  system-prompt))
                                 (incf request-count)
                                 (setf seen-persistence-mode
                                       clawmacs::*default-buffer-session-persistence-mode*)
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (clawmacs::canonical-text-block
                                         "ephemeral answer"))))
          (clawmacs::init-default-keymap)
          (initialize-test-tools)
          (let* ((options (clawmacs::parse-clawmacs-prompt-args
                           '("--ephemeral"
                             "--provider" "zai"
                             "--model" "glm-5"
                             "ephemeral" "prompt")))
                 (result (clawmacs::run-prompt-options options)))
            (is (= 1 request-count))
            (is (eq :ephemeral seen-persistence-mode))
            (is (string= "ephemeral answer"
                         (clawmacs::prompt-run-result-final-text result)))
            (is-false (probe-file (clawmacs::session-path "clawmacs:prompt")))
            (is-false (probe-file
                       (clawmacs::session-sidecar-manifest-path
                        "clawmacs:prompt")))))))))

(test default-session-prompt-session-name-prefers-most-recent-current-directory
  "session-prompt defaults to the newest saved session for the current cwd."
  (let* ((*sessions-dir* (temp-session-test-directory "session-prompt-default"))
         (working-directory (temp-session-test-directory "session-prompt-default-cwd"))
         (session (load-or-create-session "cwd-default"
                                          :working-directory working-directory))
         (buf (make-buffer "cwd-default"
                           :agent-name "echo"
                           :working-directory working-directory
                           :session session)))
    (ensure-directories-exist (merge-pathnames #P".keep" working-directory))
    (save-session buf)
    (let ((*default-pathname-defaults* working-directory))
      (is (string= "cwd-default"
                   (clawmacs::default-session-prompt-session-name))))))

(test parse-clawmacs-prompt-args-defaults-to-codex-for-plain-prompt-sh
  "Plain prompt.sh runs default to Codex 5.3 without overriding explicit routing."
  (let ((options (clawmacs::parse-clawmacs-prompt-args
                  '("summarize" "this"))))
    (is (string= "openai-codex"
                 (clawmacs::prompt-options-provider options)))
    (is (string= "gpt-5.3-codex"
                 (clawmacs::prompt-options-model options))))
  (let ((options (clawmacs::parse-clawmacs-prompt-args
                  '("--agent" "writer" "summarize" "this"))))
    (is (null (clawmacs::prompt-options-provider options)))
    (is (null (clawmacs::prompt-options-model options))))
  (let ((options (clawmacs::parse-clawmacs-prompt-args
                  '("--provider" "zai" "summarize" "this"))))
    (is (string= "zai" (clawmacs::prompt-options-provider options)))
    (is (null (clawmacs::prompt-options-model options))))
  (let ((options (clawmacs::parse-clawmacs-prompt-args
                  '("--model" "custom-codex" "summarize" "this"))))
    (is (string= "openai-codex"
                 (clawmacs::prompt-options-provider options)))
    (is (string= "custom-codex"
                 (clawmacs::prompt-options-model options)))))

(test prompt-usage-string-docs-prompt-sh-codex-default
  "prompt.sh help renders its default provider/model without FORMAT errors."
  (let ((usage (clawmacs::prompt-usage-string)))
    (is (search "Default without --agent: openai-codex" usage))
    (is (search "Default without --agent: gpt-5.3-codex" usage))
    (is (search "--model-role ROLE" usage))
    (is (search "--service-tier TIER" usage))
    (is (search "--package NAME" usage))
    (is (search "--session NAME" usage))
    (is (search "--ephemeral" usage))
    (is (search "--pipeline NAME" usage))
    (is (search "Skip ~/.clawmacs.d/init.lisp" usage))))

(test compaction-threshold-policy
  "Compaction thresholds are configurable as nil, ratios, integers, or functions."
  (let ((buf (make-buffer "compact" :context-limit 1000)))
    (let ((clawmacs::*compaction-point* nil))
      (is (null (clawmacs:compaction-threshold-tokens buf :estimate 42))))
    (let ((clawmacs::*compaction-point* 9/10))
      (is (= 900 (clawmacs:compaction-threshold-tokens buf :estimate 42))))
    (let ((clawmacs::*compaction-point* 1234))
      (is (= 1234 (clawmacs:compaction-threshold-tokens buf :estimate 42))))
    (let ((clawmacs::*compaction-point*
            (lambda (buffer estimate limit)
              (declare (ignore buffer estimate))
              (/ limit 4))))
      (is (= 250 (clawmacs:compaction-threshold-tokens buf :estimate 42))))
    (let ((clawmacs::*compaction-point*
            (lambda (buffer estimate limit)
              (declare (ignore buffer limit))
              (>= estimate 42))))
      (is (= 42 (clawmacs:compaction-threshold-tokens buf :estimate 42))))))

(test maybe-compact-buffer-runs-custom-function-only-at-threshold
  "maybe-compact-buffer calls the configured function only when needed."
  (let ((buf (make-buffer "compact")))
    (append-test-user-message buf "hello")
    (let ((calls 0)
          (reasons nil))
      (let ((clawmacs::*compaction-point* 1000000)
            (clawmacs::*compaction-function*
              (lambda (buffer &key reason)
                (declare (ignore buffer))
                (incf calls)
                (push reason reasons)
                t)))
        (multiple-value-bind (compacted-p estimate threshold)
            (clawmacs:maybe-compact-buffer buf :reason :too-small)
          (declare (ignore estimate threshold))
          (is-false compacted-p)
          (is (= 0 calls))))
      (let ((clawmacs::*compaction-point* 1)
            (clawmacs::*compaction-function*
              (lambda (buffer &key reason)
                (declare (ignore buffer))
                (incf calls)
                (push reason reasons)
                t)))
        (multiple-value-bind (compacted-p estimate threshold)
            (clawmacs:maybe-compact-buffer buf :reason :large-enough)
          (declare (ignore estimate threshold))
          (is-true compacted-p)
          (is (= 1 calls))
          (is (equal '(:large-enough) reasons)))))))

(test default-compact-buffer-replaces-history-with-summary-and-recent-users
  "Default compaction summarizes old history without exposing tools."
  (let ((buf (make-buffer "compact" :agent-name "agent" :context-limit 100000))
        (captured-messages nil)
        (captured-tools nil)
        (captured-system-prompt nil))
    (append-test-user-message buf "first user context")
    (buffer-insert-agent-message buf "assistant work that should be summarized")
    (append-test-user-message buf "latest user request")
    (with-function-override (clawmacs::provider-request-streaming
                             (provider messages callback
                                       &key model max-tokens tools
                                       reasoning-effort system-prompt)
                             (declare (ignore provider callback model
                                              max-tokens reasoning-effort))
                             (setf captured-messages messages
                                   captured-tools tools
                                   captured-system-prompt system-prompt)
                             (make-completed-stream-state-response
                              "end_turn"
                              (list (clawmacs::canonical-text-block
                                     "summary body"))))
      (let ((clawmacs::*compaction-preserved-user-message-token-limit* 1000))
        (is (eq buf (clawmacs:default-compact-buffer buf :reason :manual)))))
    (is (equal '(:compaction-summary :user :user :system)
               (test-buffer-history-senders buf)))
    (let ((history (test-buffer-history-messages buf)))
      (is (search clawmacs:*compaction-summary-prefix*
                  (message-text (first history))))
      (is (search "summary body" (message-text (first history))))
      (is (string= "first user context" (message-text (second history))))
      (is (string= "latest user request" (message-text (third history))))
      (is (search "Conversation compacted" (message-text (fourth history)))))
    (is (= 0 (length captured-tools)))
    (is (stringp captured-system-prompt))
    (let* ((prompt-message (car (last captured-messages)))
           (content (cdr (assoc :content prompt-message)))
           (block (aref content 0)))
      (is (string= clawmacs:*compaction-prompt*
                   (cdr (assoc :text block)))))
    (let ((provider-messages (clawmacs:build-conversation-messages buf)))
      (is (every (lambda (message)
                   (string= "user" (cdr (assoc :role message))))
                 provider-messages))
      (is (not (search "Conversation compacted"
                       (clawmacs:api-json-encode provider-messages)))))))

(test default-compact-buffer-rotates-session-transcript
  "Compaction starts a new transcript segment referencing the previous one."
  (let* ((*sessions-dir* (temp-session-test-directory "compact-rotate"))
         (session (load-or-create-session "compact-rotate"))
         (buf (make-buffer "compact-rotate"
                           :agent-name "agent"
                           :context-limit 100000
                           :session session)))
    (append-test-user-message buf "first user context")
    (buffer-insert-agent-message buf "assistant work that should be summarized")
    (append-test-user-message buf "latest user request")
    (let ((previous-path (session-current-transcript-path session)))
      (with-function-override (clawmacs::provider-request-streaming
                               (provider messages callback
                                         &key model max-tokens tools
                                         reasoning-effort system-prompt)
                               (declare (ignore provider messages callback model
                                                max-tokens tools reasoning-effort
                                                system-prompt))
                               (make-completed-stream-state-response
                                "end_turn"
                                (list (clawmacs::canonical-text-block
                                       "summary body"))))
        (let ((clawmacs::*compaction-preserved-user-message-token-limit* 1000))
          (is (eq buf (clawmacs:default-compact-buffer
                       buf
                       :reason :manual)))))
      (is (not (equal (namestring previous-path)
                      (namestring (session-current-transcript-path session)))))
      (let* ((old-events (read-jsonl-events previous-path))
             (new-events (session-current-events session))
             (first-new (first new-events))
             (message-events
               (remove-if-not (lambda (event)
                                (string= "message"
                                         (event-value event :event)))
                              new-events)))
        (is (= 4 (length old-events)))
        (is (string= "previous-transcript"
                     (event-value first-new :event)))
        (is (string= (clawmacs::session-path-string previous-path)
                     (event-value first-new :previous-transcript-path)))
        (is (find "COMPACTION-SUMMARY" message-events
                  :key (lambda (event)
                         (event-value event :sender))
                  :test #'string=))
        (is (find "SYSTEM" message-events
                  :key (lambda (event)
                         (event-value event :sender))
                  :test #'string=))))))

(test send-message-compacts-before-finalizing-current-input
  "Interactive sending runs pre-send compaction while current input is editable."
  (let ((buf (make-buffer "compact-send"))
        (saw-input nil)
        (saw-read-only nil)
        (sent-p nil))
    (clawmacs::set-message-text (buffer-input-message buf)
                                "current user request")
    (with-function-override (clawmacs::send-to-agent-with-context (buffer)
                              (setf sent-p t)
                              buffer)
      (let ((clawmacs::*compaction-point* 1)
            (clawmacs::*compaction-function*
              (lambda (buffer &key reason)
                (declare (ignore reason))
                (setf saw-input (message-text (buffer-input-message buffer))
                      saw-read-only (message-read-only-p
                                     (buffer-input-message buffer)))
                buffer)))
        (clawmacs::send-message buf)))
    (is (string= "current user request" saw-input))
    (is-false saw-read-only)
    (is-true sent-p)
    (is (string= "current user request"
                 (message-text (message-prev (buffer-input-message buf)))))))

(test run-single-prompt-compacts-before-provider-request
  "Prompt mode applies compaction before sending provider requests."
  (let ((compacted-p nil)
        (provider-requested-p nil))
    (with-function-override (clawmacs::provider-request-streaming
                             (provider messages callback
                                       &key model max-tokens tools
                                       reasoning-effort system-prompt)
                             (declare (ignore provider messages callback model
                                              max-tokens tools reasoning-effort
                                              system-prompt))
                             (setf provider-requested-p t)
                             (make-completed-stream-state-response
                              "end_turn"
                              (list (clawmacs::canonical-text-block
                                     "final after compaction"))))
      (let ((clawmacs::*compaction-point* 1)
            (clawmacs::*compaction-function*
              (lambda (buffer &key reason)
                (declare (ignore buffer))
                (when (eq reason :prompt-request)
                  (setf compacted-p t))
                t)))
        (let ((result (clawmacs:run-single-prompt
                       "hello"
                       :provider :zai
                       :model "glm-5")))
          (is-true compacted-p)
          (is-true provider-requested-p)
          (is (string= "final after compaction"
                       (clawmacs:prompt-run-result-final-text result))))))))

(test make-prompt-buffer-attaches-session-transcript
  "Prompt-mode buffers are real sessions and record their seeded user prompt."
  (let* ((*sessions-dir* (temp-session-test-directory "prompt-buffer"))
         (buf (clawmacs::make-prompt-buffer "hello from prompt" "agent"))
         (session (buffer-session buf)))
    (is (not (null session)))
    (is (probe-file (session-current-transcript-path session)))
    (let* ((events (session-current-events session))
           (message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            events))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "USER" (event-value event :sender)))
      (is (string= "hello from prompt" (event-value event :text))))))

(test run-single-prompt-returns-final-response
  "Non-interactive prompt mode returns a final assistant response without a UI."
  (let ((path (temp-agent-defaults-path))
        (seen-provider nil)
        (seen-model nil)
        (seen-messages nil))
    (with-agent-defaults-path-override (path)
      (with-function-override (clawmacs::provider-request-streaming
                               (provider messages callback
                                         &key model max-tokens tools
                                         reasoning-effort system-prompt)
                               (declare (ignore callback))
                               (declare (ignore max-tokens tools reasoning-effort
                                                system-prompt))
                               (setf seen-provider provider
                                     seen-model model
                                     seen-messages messages)
                               (make-completed-stream-state-response
                                "end_turn"
                                (list (clawmacs::canonical-text-block "final answer")
                                      (clawmacs::canonical-reasoning-block
                                       "provider reasoning summary"))))
        (clawmacs::init-default-keymap)
        (initialize-test-tools)
        (let ((result (clawmacs:run-single-prompt
                       "Say hello"
                       :provider :zai
                       :model "glm-5")))
          (is (eq :zai seen-provider))
          (is (string= "glm-5" seen-model))
          (is (= 1 (length seen-messages)))
          (is (string= "final answer"
                       (clawmacs:prompt-run-result-final-text result)))
          (is (equal '("provider reasoning summary")
                     (clawmacs:prompt-run-result-reasoning-blocks result)))
          (is (= 1 (clawmacs:prompt-run-result-iterations result)))
          (is (null (clawmacs:prompt-run-result-tool-events result))))))))

(test run-single-prompt-executes-lisp-eval-tool-loop
  "Prompt mode executes lisp_eval tool calls and continues with tool results."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (second-request-messages nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore callback))
                                 (declare (ignore provider model max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (incf request-count)
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "tool_use"
                                      (list
                                       (clawmacs::canonical-tool-use-block
                                        "call-1"
                                        "lisp_eval"
                                        '((:code . "(+ 2 3)"))))
                                      '(:input-tokens 100
                                        :output-tokens 10
                                        :total-tokens 110
                                        :cached-input-tokens 64
                                        :uncached-input-tokens 36
                                        :cache-hit-rate 0.64))
                                     (progn
                                       (setf second-request-messages messages)
                                       (make-completed-stream-state-response
                                        "end_turn"
                                        (list (clawmacs::canonical-text-block
                                               "the result is 5"))
                                        '(:input-tokens 120
                                          :output-tokens 20
                                          :total-tokens 140
                                          :cached-input-tokens 80
                                          :uncached-input-tokens 40
                                          :cache-hit-rate 0.6666667)))))
          (clawmacs::init-default-keymap)
          (initialize-test-tools)
          (let* ((result (clawmacs:run-single-prompt
                          "Compute two plus three"
                          :provider :zai
                          :model "glm-5"))
                 (events (clawmacs:prompt-run-result-tool-events result))
                 (event (first events)))
            (is (= 2 request-count))
            (is (= 3 (length second-request-messages)))
            (is (string= "the result is 5"
                         (clawmacs:prompt-run-result-final-text result)))
            (is (= 2 (clawmacs:prompt-run-result-iterations result)))
            (let ((usage (clawmacs:prompt-run-result-usage result)))
              (is (= 220 (getf usage :input-tokens)))
              (is (= 30 (getf usage :output-tokens)))
              (is (= 250 (getf usage :total-tokens)))
              (is (= 144 (getf usage :cached-input-tokens)))
              (is (= 76 (getf usage :uncached-input-tokens)))
              (is (< (abs (- (getf usage :cache-hit-rate)
                              (/ 144.0 220)))
                     0.0001))
              (let ((json-usage
                      (cdr (assoc :usage
                                  (clawmacs::prompt-run-result-json result)))))
                (is (= 144 (cdr (assoc :cached--input--tokens
                                        json-usage))))))
            (is (= 1 (length events)))
            (is (string= "lisp_eval" (clawmacs:prompt-tool-event-name event)))
            (is (search "(+ 2 3)" (clawmacs:prompt-tool-event-display event)))
            (is (search "5" (clawmacs:prompt-tool-event-result-text event)))
            (let* ((tool-result-message (third second-request-messages))
                   (content (coerce (cdr (assoc :content tool-result-message))
                                    'list))
                   (tool-result (first content)))
              (is (string= "user" (cdr (assoc :role tool-result-message))))
              (is (string= "tool_result" (cdr (assoc :type tool-result))))
              (is (string= "call-1"
                           (cdr (assoc :tool--use--id tool-result))))
              (is (search "5" (cdr (assoc :content tool-result)))))))))))

(test run-session-prompt-reuses-saved-session-context
  "Session prompt mode reloads prior turns before sending the next prompt."
  (let ((path (temp-agent-defaults-path))
        (*sessions-dir* (temp-session-test-directory "session-prompt"))
        (request-count 0)
        (second-request-messages nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore callback))
                                 (declare (ignore provider model max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (incf request-count)
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "end_turn"
                                      (list (clawmacs::canonical-text-block
                                             "first answer")))
                                     (progn
                                       (setf second-request-messages messages)
                                       (make-completed-stream-state-response
                                        "end_turn"
                                        (list (clawmacs::canonical-text-block
                                               "second answer"))))))
          (clawmacs::init-default-keymap)
          (initialize-test-tools)
          (let* ((first (clawmacs:run-session-prompt
                         "First prompt"
                         :session-name "cache-probe"
                         :provider :zai
                         :model "glm-5"))
                 (second (clawmacs:run-session-prompt
                          "Second prompt"
                          :session-name "cache-probe"
                          :provider :zai
                          :model "glm-5")))
            (is (string= "first answer"
                         (clawmacs:prompt-run-result-final-text first)))
            (is (string= "second answer"
                         (clawmacs:prompt-run-result-final-text second)))
            (is (= 2 request-count))
            (is (= 3 (length second-request-messages)))
            (is (string= "First prompt"
                         (clawmacs::content-text-blocks
                          (coerce (cdr (assoc :content
                                              (first second-request-messages)))
                                  'list))))
            (is (string= "first answer"
                         (clawmacs::content-text-blocks
                          (coerce (cdr (assoc :content
                                              (second second-request-messages)))
                                  'list))))
            (is (string= "Second prompt"
                         (clawmacs::content-text-blocks
                          (coerce (cdr (assoc :content
                                              (third second-request-messages)))
                                  'list))))
            (let ((loaded (load-session "cache-probe")))
              (is (equal '(:user :agent :user :agent)
                         (test-buffer-history-senders loaded))))))))))

(test run-session-prompt-binds-openai-prompt-cache-controls
  "OpenAI session prompt mode sends a stable cache key for the saved session."
  (let ((path (temp-agent-defaults-path))
        (*sessions-dir* (temp-session-test-directory "session-prompt-cache"))
        (captured-cache-key nil)
        (captured-cache-retention nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore messages callback max-tokens
                                                  tools reasoning-effort
                                                  system-prompt))
                                 (is (eq :openai-codex provider))
                                 (is (string= "gpt-5.4" model))
                                 (setf captured-cache-key
                                       clawmacs::*openai-codex-prompt-cache-key*
                                       captured-cache-retention
                                       clawmacs::*openai-codex-prompt-cache-retention*)
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (clawmacs::canonical-text-block
                                         "cached"))))
          (clawmacs::init-default-keymap)
          (initialize-test-tools)
          (let ((result (clawmacs:run-session-prompt
                         "Cache probe"
                         :session-name "cache-probe"
                         :provider :openai-codex
                         :model "gpt-5.4")))
            (is (string= "cached"
                         (clawmacs:prompt-run-result-final-text result)))
            (is (string= (format nil "clawmacs-agent-~(~A~)"
                                  (clawmacs::session-name-hash
                                   "cache-probe"))
                         captured-cache-key))
            (is (null captured-cache-retention))))))))

(test run-subagent-uses-registered-agent-and-routing-overrides
  "run-subagent can delegate to a registered agent with explicit routing overrides."
  (let ((path (temp-agent-defaults-path))
        (seen-provider nil)
        (seen-model nil)
        (seen-think-level nil))
    (with-agent-defaults-path-override (path)
      (with-agent-definition-registry-override ()
        (clawmacs:register-agent-definition
         "researcher"
         :provider :zai
         :model "glm-5"
         :personality-prompt "research personality")
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore messages callback max-tokens
                                                  tools system-prompt))
                                 (setf seen-provider provider
                                       seen-model model
                                       seen-think-level reasoning-effort)
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (clawmacs::canonical-text-block
                                         "delegated answer"))))
          (clawmacs::init-default-keymap)
          (initialize-test-tools)
          (let ((result (clawmacs:run-subagent
                         "Research this"
                         :agent-name "researcher"
                         :provider :openai-codex
                         :model "gpt-5.3-codex"
                         :think-level "high")))
            (is (eq :openai-codex seen-provider))
            (is (string= "gpt-5.3-codex" seen-model))
            (is (string= "high" seen-think-level))
            (is (string= "researcher"
                         (clawmacs:prompt-run-result-agent-name result)))
            (is (string= "delegated answer"
                         (clawmacs:prompt-run-result-final-text result)))))))))

(test run-subagent-uses-transient-prompts-without-registering-agent
  "Custom subagent prompts are dynamically scoped and do not mutate the registry."
  (let ((path (temp-agent-defaults-path))
        (seen-system-prompt nil))
    (with-agent-defaults-path-override (path)
      (with-agent-definition-registry-override ()
        (with-function-override (clawmacs::load-boot-files ()
                                  nil)
          (with-function-override (clawmacs::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider messages callback model
                                                    max-tokens tools reasoning-effort))
                                   (setf seen-system-prompt system-prompt)
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (clawmacs::canonical-text-block
                                           "custom answer"))))
            (clawmacs::init-default-keymap)
            (initialize-test-tools)
            (let ((result (clawmacs:run-subagent
                           "Use a custom prompt"
                           :agent-name "temporary-doc-agent"
                           :provider :zai
                           :model "glm-5"
                           :core-prompt "TEMP CORE"
                           :personality-prompt "TEMP PERSONALITY")))
              (is (search "TEMP CORE" seen-system-prompt))
              (is (search "TEMP PERSONALITY" seen-system-prompt))
              (is (null (clawmacs:find-agent-definition
                         "temporary-doc-agent")))
              (is (string= "custom answer"
                           (clawmacs:prompt-run-result-final-text result))))))))))

(test run-subagent-uses-agent-tool-allowlist-and-explicit-overrides
  "Agent tool defaults constrain request tools; explicit subagent tool names override them."
  (let ((path (temp-agent-defaults-path))
        (captured-tool-names nil))
    (with-agent-defaults-path-override (path)
      (with-agent-definition-registry-override ()
        (with-tool-table-restored
          (clrhash clawmacs::*tool-table*)
          (initialize-test-tools)
          (clawmacs:register-tool
           "doc_lookup"
           "Look up docs."
           '((:type . "object")
             (:properties . ((:query . ((:type . "string"))))))
           :agent-allowed
           (lambda (args)
             (declare (ignore args))
             "{\"doc\":\"ok\"}"))
          (clawmacs:register-agent-definition
           "docs"
           :provider :zai
           :model "glm-5"
           :tool-names '("doc_lookup"))
          (with-function-override (clawmacs::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider messages callback model
                                                    max-tokens reasoning-effort
                                                    system-prompt))
                                   (setf captured-tool-names
                                         (mapcar (lambda (tool)
                                                   (cdr (assoc :name tool)))
                                                 (coerce tools 'list)))
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (clawmacs::canonical-text-block
                                           "done"))))
            (clawmacs::init-default-keymap)
            (clawmacs:run-subagent "Find docs" :agent-name "docs")
            (is (equal '("doc_lookup") captured-tool-names))
            (setf captured-tool-names nil)
            (clawmacs:run-subagent
             "Use lisp instead"
             :agent-name "docs"
             :tool-names '("lisp_eval"))
            (is (equal '("lisp_eval") captured-tool-names))))))))

(test run-subagent-custom-tools-are-temporary-and-executable
  "Custom subagent tools are exposed only for the run and record tool evidence."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (captured-tool-names nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (clrhash clawmacs::*tool-table*)
        (initialize-test-tools)
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens reasoning-effort
                                                  system-prompt))
                                 (incf request-count)
                                 (when (= request-count 1)
                                   (setf captured-tool-names
                                         (mapcar (lambda (tool)
                                                   (cdr (assoc :name tool)))
                                                 (coerce tools 'list))))
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "tool_use"
                                      (list
                                       (clawmacs::canonical-tool-use-block
                                        "call-custom"
                                        "custom_echo"
                                        '((:payload . "ok")))))
                                     (make-completed-stream-state-response
                                      "end_turn"
                                      (list (clawmacs::canonical-text-block
                                             "custom done")))))
          (clawmacs::init-default-keymap)
          (let* ((tool (clawmacs:make-subagent-tool
                        :name "custom_echo"
                        :description "Echo a payload."
                        :input-schema
                        '((:type . "object")
                          (:properties . ((:payload . ((:type . "string")))))
                          (:required . #("payload")))
                        :execute-fn
                        (lambda (args)
                          (format nil "echo=~A" (cdr (assoc :payload args))))))
                 (result (clawmacs:run-subagent
                          "Use the custom tool"
                          :agent-name "custom-tool-agent"
                          :provider :zai
                          :model "glm-5"
                          :custom-tools (list tool)))
                 (events (clawmacs:prompt-run-result-tool-events result))
                 (event (first events)))
            (is (equal '("custom_echo") captured-tool-names))
            (is (= 2 request-count))
            (is (string= "custom done"
                         (clawmacs:prompt-run-result-final-text result)))
            (is (= 1 (length events)))
            (is (string= "custom_echo"
                         (clawmacs:prompt-tool-event-name event)))
            (is (search "echo=ok"
                        (clawmacs:prompt-tool-event-result-text event)))
            (is (null (gethash "custom_echo" clawmacs::*tool-table*)))))))))

(test run-subagent-custom-tool-plists-and-explicit-tool-names
  "Custom tool plists normalize correctly and explicit tool names can mix scopes."
  (let ((path (temp-agent-defaults-path))
        (captured-tool-names nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (clrhash clawmacs::*tool-table*)
        (initialize-test-tools)
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens reasoning-effort
                                                  system-prompt))
                                 (setf captured-tool-names
                                       (sort
                                        (mapcar (lambda (tool)
                                                  (cdr (assoc :name tool)))
                                                (coerce tools 'list))
                                        #'string<))
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (clawmacs::canonical-text-block
                                         "done"))))
          (clawmacs::init-default-keymap)
          (clawmacs:run-subagent
           "Use available tools"
           :agent-name "custom-tool-agent"
           :provider :zai
           :model "glm-5"
           :custom-tools
           (list (list :name "custom_plist"
                       :description "Plist-defined tool."
                       :schema '((:type . "object"))
                       :execute-fn (lambda (args)
                                     (declare (ignore args))
                                     "plist")))
           :tool-names '("custom_plist" "lisp_eval"))
          (is (equal '("custom_plist" "lisp_eval")
                     captured-tool-names))
          (is (null (gethash "custom_plist" clawmacs::*tool-table*))))))))

(test run-subagent-records-unallowed-tool-call-as-tool-result-error
  "A provider cannot execute a tool outside the subagent allowlist."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (second-request-messages nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (clrhash clawmacs::*tool-table*)
        (initialize-test-tools)
        (clawmacs:register-tool
         "doc_lookup"
         "Look up docs."
         '((:type . "object")
           (:properties . ((:query . ((:type . "string"))))))
         :agent-allowed
         (lambda (args)
           (declare (ignore args))
           "{\"doc\":\"ok\"}"))
        (clawmacs:register-tool
         "write_probe"
         "A tool this subagent must not call."
         '((:type . "object")
           (:properties . ((:payload . ((:type . "string"))))))
         :agent-allowed
         (lambda (args)
           (declare (ignore args))
           "{\"wrote\":true}"))
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider callback model
                                                  max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (incf request-count)
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "tool_use"
                                      (list
                                       (clawmacs::canonical-tool-use-block
                                        "call-write"
                                        "write_probe"
                                        '((:payload . "bad")))))
                                     (progn
                                       (setf second-request-messages messages)
                                       (make-completed-stream-state-response
                                        "end_turn"
                                        (list (clawmacs::canonical-text-block
                                               "handled denial"))))))
          (clawmacs::init-default-keymap)
          (let* ((result (clawmacs:run-subagent
                          "Try the wrong tool"
                          :agent-name "docs"
                          :provider :zai
                          :model "glm-5"
                          :tool-names '("doc_lookup")))
                 (events (clawmacs:prompt-run-result-tool-events result))
                 (event (first events))
                 (tool-result-message (third second-request-messages))
                 (tool-result (first (coerce
                                      (cdr (assoc :content
                                                  tool-result-message))
                                      'list))))
            (is (= 2 request-count))
            (is (= 1 (length events)))
            (is (string= "write_probe"
                         (clawmacs:prompt-tool-event-name event)))
            (is (search "not allowed"
                        (clawmacs:prompt-tool-event-result-text event)))
            (is (search "not allowed"
                        (cdr (assoc :content tool-result))))
            (is (string= "handled denial"
                         (clawmacs:prompt-run-result-final-text result)))))))))

(test run-subagent-async-waits-and-registers-result
  "Async subagents return a handle, register it, and preserve final results."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-subagent-registry-override ()
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (clawmacs::canonical-text-block
                                         "async answer"))))
          (clawmacs::init-default-keymap)
          (initialize-test-tools)
          (let ((handle (clawmacs:run-subagent-async
                         "Do async work"
                         :agent-name "async-agent"
                         :provider :zai
                         :model "glm-5")))
            (is (string= "subagent-1"
                         (clawmacs:subagent-handle-id handle)))
            (is (eq handle
                    (clawmacs:find-subagent
                     (clawmacs:subagent-handle-id handle))))
            (is (member handle (clawmacs:list-subagents)))
            (multiple-value-bind (result status returned-handle)
                (clawmacs:wait-subagent handle :timeout 2)
              (is (eq :succeeded status))
              (is (eq handle returned-handle))
              (is (clawmacs:subagent-done-p handle))
              (is (string= "async answer"
                           (clawmacs:prompt-run-result-final-text result)))
              (let ((snapshot (clawmacs:subagent-snapshot handle)))
                (is (string= "subagent-1" (getf snapshot :id)))
                (is (eq :succeeded (getf snapshot :status)))
                (is (getf snapshot :done-p))
                (is (eq result (getf snapshot :result)))))))))))

(test run-subagent-async-records-failures
  "Async provider failures are captured on the handle instead of escaping."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-subagent-registry-override ()
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (error "provider boom"))
          (clawmacs::init-default-keymap)
          (initialize-test-tools)
          (let ((handle (clawmacs:run-subagent-async
                         "Fail async work"
                         :provider :zai
                         :model "glm-5")))
            (multiple-value-bind (result status)
                (clawmacs:wait-subagent handle :timeout 2)
              (is (null result))
              (is (eq :failed status))
              (is (search "provider boom"
                          (clawmacs:subagent-error handle))))))))))

(test cancel-subagent-is-cooperative-and-stable
  "Cancelled handles stay cancelled even if the background provider returns."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-subagent-registry-override ()
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (sleep 0.1)
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (clawmacs::canonical-text-block
                                         "late answer"))))
          (clawmacs::init-default-keymap)
          (initialize-test-tools)
          (let ((handle (clawmacs:run-subagent-async
                         "Cancel async work"
                         :provider :zai
                         :model "glm-5")))
            (is (eq :running (clawmacs:subagent-status handle)))
            (clawmacs:cancel-subagent handle)
            (multiple-value-bind (result status)
                (clawmacs:wait-subagent handle :timeout 1)
              (is (null result))
              (is (eq :cancelled status)))
            (sleep 0.2)
            (is (eq :cancelled (clawmacs:subagent-status handle)))
            (is (null (clawmacs:subagent-result handle)))))))))

(test prompt-run-tool-verification-helpers
  "Parent agents can check tool usage without parsing raw events."
  (let* ((result (clawmacs::make-prompt-run-result
                  :tool-events
                  (list (clawmacs::make-prompt-tool-event
                         :name "doc_lookup")
                        (clawmacs::make-prompt-tool-event
                         :name "lisp_eval")))))
    (is (equal '("doc_lookup" "lisp_eval")
               (clawmacs:prompt-run-tool-names result)))
    (is (= 2 (clawmacs:prompt-run-tool-count result)))
    (is (= 1 (clawmacs:prompt-run-tool-count result "doc_lookup")))
    (is (clawmacs:prompt-run-used-tool-p result 'doc-lookup))
    (is-false (clawmacs:prompt-run-used-tool-p result "missing_tool"))))

(test execute-lisp-eval-captures-output-and-history
  "lisp_eval captures printed output, values, and returns Lisp data."
  (with-tool-table-restored
    (let ((clawmacs::*lisp-eval-history* nil)
          (clawmacs::*last-eval-result* nil)
          (clawmacs::*last-eval-condition* nil))
      (initialize-test-tools)
      (let* ((data (clawmacs:execute-tool
                    "lisp_eval"
                    '(:code "(progn (format t \"hello\") (values 4 5))")))
             (decoded (clawmacs::lisp-data-read data)))
        (is (search ":code" data :test #'char-equal))
        (is (search "4" (getf decoded :result)))
        (is (search "5" (getf decoded :result)))
        (is (string= "hello" (getf decoded :output)))
        (is (= 2 (getf decoded :values)))
        (is (equal '(4 5) clawmacs:*last-eval-result*))
        (is (null clawmacs:*last-eval-condition*))
        (is (search "hello" (clawmacs:eval-history-to-string)))))))

(test execute-lisp-eval-prints-values-defensively
  "Result printing failures do not turn successful lisp_eval calls into errors."
  (with-tool-table-restored
    (let ((clawmacs::*lisp-eval-history* nil)
          (clawmacs::*last-eval-result* nil)
          (clawmacs::*last-eval-condition* nil))
      (initialize-test-tools)
      (let* ((data (clawmacs:execute-tool
                    "lisp_eval"
                    '(:code "(clawmacs/tests::make-unprintable-eval-value)")))
             (decoded (clawmacs::lisp-data-read data)))
        (is (= 1 (getf decoded :values)))
        (is (search "unprintable value" (getf decoded :result)))
        (is (null (getf decoded :error)))
        (is (not (null clawmacs:*last-eval-result*)))
        (is (typep (first clawmacs:*last-eval-result*)
                   'unprintable-eval-value))
        (is (null clawmacs:*last-eval-condition*))
        (is (search "unprintable value"
                    (clawmacs:eval-history-to-string)))))))

(test execute-lisp-eval-records-errors
  "Failed lisp_eval executions expose the condition and still record history."
  (with-tool-table-restored
    (let ((clawmacs::*lisp-eval-history* nil)
          (clawmacs::*last-eval-result* nil)
          (clawmacs::*last-eval-condition* nil))
      (initialize-test-tools)
      (let* ((data (clawmacs:execute-tool
                    "lisp_eval"
                    '(:code "(error \"boom\")")))
             (decoded (clawmacs::lisp-data-read data)))
        (is (search ":error" data :test #'char-equal))
        (is (search "boom" (getf decoded :error)))
        (is (null clawmacs:*last-eval-result*))
        (is (not (null clawmacs:*last-eval-condition*)))
        (is (search "boom" (clawmacs:eval-history-to-string)))))))

(test run-single-prompt-error-carries-partial-tool-events
  "Prompt loop failures retain tool events for diagnostics."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (clawmacs::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback
                                                  model max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (make-completed-stream-state-response
                                  "tool_use"
                                  (list
                                   (clawmacs::canonical-tool-use-block
                                    "loop-call"
                                    "lisp_eval"
                                    '((:code . "(+ 1 1)"))))))
          (clawmacs::init-default-keymap)
          (initialize-test-tools)
          (handler-case
              (progn
                (clawmacs:run-single-prompt
                 "loop forever"
                 :provider :zai
                 :model "glm-5"
                 :max-tool-iterations 1)
                (fail "Expected prompt-run-error"))
            (clawmacs:prompt-run-error (condition)
              (let ((events (clawmacs:prompt-run-error-tool-events condition)))
                (is (search "Exceeded maximum tool iterations"
                            (clawmacs:prompt-run-error-message condition)))
                (is (= 1 (clawmacs:prompt-run-error-iterations condition)))
                (is (= 1 (length events)))
                (is (string= "lisp_eval"
                             (clawmacs:prompt-tool-event-name
                              (first events))))
                (is (search "2"
                            (clawmacs:prompt-tool-event-result-text
                             (first events))))))))))))

(test provider-token-anthropic-is-unsupported
  "Anthropic no longer has a provider-specific token path."
  (signals error
    (clawmacs::provider-token-path :anthropic))
  (signals error
    (clawmacs::read-provider-token :anthropic))
  (signals error
    (clawmacs::save-provider-token :anthropic "removed")))

(test provider-token-round-trip-openai-codex
  "OpenAI Codex tokens round-trip through provider-specific helpers."
  (let ((openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (nil openai-codex-path)
      (is (string= "openai-token"
                   (clawmacs::save-provider-token :openai-codex "openai-token")))
      (is (string= "openai-token"
                   (clawmacs::read-provider-token :openai-codex))))))

(test read-provider-token-trims-whitespace
  "Provider token reads trim surrounding whitespace."
  (let ((openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (nil openai-codex-path)
      (with-open-file (stream openai-codex-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string "  trimmed-token  " stream)
        (terpri stream))
      (is (string= "trimmed-token"
                   (clawmacs::read-provider-token :openai-codex))))))

;;; --------------------------------------------------------------------------
;;; Environment Variable Token Tests
;;; --------------------------------------------------------------------------

(defmacro with-env-var ((var value) &body body)
  "Temporarily set environment variable VAR to VALUE (or unset if VALUE is nil)."
  (let ((gvar (gensym "VAR-"))
        (gval (gensym "VAL-"))
        (gold (gensym "OLD-")))
    `(let* ((,gvar ,var)
            (,gval ,value)
            (,gold (uiop:getenv ,gvar)))
       (unwind-protect
            (progn
              (if ,gval
                  (setf (uiop:getenv ,gvar) ,gval)
                  (setf (uiop:getenv ,gvar) ""))
              ,@body)
         (if ,gold
             (setf (uiop:getenv ,gvar) ,gold)
             (setf (uiop:getenv ,gvar) ""))))))

(test ensure-prompt-workspace-project-registers-clawmacs-source-root
  "Prompt mode registers the mounted workspace as the clawmacs project."
  (let* ((base (project-test-directory))
         (workspace (merge-pathnames #P"workspace/" base))
         (custom (merge-pathnames #P"custom/" base))
         (definitions (merge-pathnames #P"defs/" base))
         (*project-registry* (make-hash-table :test #'equal))
         (*project-definitions-directory* definitions)
         (clawmacs::*project-definitions-loaded-p* nil))
    (ensure-directories-exist (merge-pathnames #P".keep" workspace))
    (ensure-directories-exist (merge-pathnames #P".keep" custom))
    (ensure-directories-exist (merge-pathnames #P".keep" definitions))
    (with-env-var ("CLAWMACS_PROMPT_PROJECT_ROOT" (namestring workspace))
      (clawmacs::ensure-prompt-workspace-project)
      (let ((project (find-project "clawmacs")))
        (is (not (null project)))
        (is (eq :builtin (project-source project)))
        (is (equal '(:clawmacs :clawmacs/tests)
                   (project-systems project)))
        (is (string= (namestring (truename workspace))
                     (namestring (project-root project)))))
      (define-project "clawmacs"
        :root custom
        :description "user-defined clawmacs project"
        :source :programmatic
        :replace t)
      (clawmacs::ensure-prompt-workspace-project)
      (let ((project (find-project "clawmacs")))
        (is (string= "user-defined clawmacs project"
                     (project-description project)))
        (is (string= (namestring (truename custom))
                     (namestring (project-root project))))))))

(test read-env-token-returns-value-when-set
  "read-env-token returns the token from a set environment variable."
  (with-env-var ("CLAWMACS_TEST_TOKEN" "test-env-token-123")
    (is (string= "test-env-token-123"
                 (clawmacs::read-env-token "CLAWMACS_TEST_TOKEN")))))

(test read-env-token-trims-whitespace
  "read-env-token trims leading and trailing whitespace."
  (with-env-var ("CLAWMACS_TEST_TOKEN" "  env-token-padded  ")
    (is (string= "env-token-padded"
                 (clawmacs::read-env-token "CLAWMACS_TEST_TOKEN")))))

(test read-env-token-returns-nil-for-empty
  "read-env-token returns nil for an empty environment variable."
  (with-env-var ("CLAWMACS_TEST_TOKEN" "")
    (is (null (clawmacs::read-env-token "CLAWMACS_TEST_TOKEN")))))

(test read-env-token-returns-nil-for-whitespace-only
  "read-env-token returns nil for a whitespace-only environment variable."
  (with-env-var ("CLAWMACS_TEST_TOKEN" "   ")
    (is (null (clawmacs::read-env-token "CLAWMACS_TEST_TOKEN")))))

(test read-env-token-returns-nil-for-unset
  "read-env-token returns nil for an unset environment variable."
  (is (null (clawmacs::read-env-token "CLAWMACS_DEFINITELY_NOT_SET_12345"))))

(test zai-env-var-takes-highest-priority
  "ZAI_CODING_MAX_API_KEY env var takes priority over the static token file."
  (let ((openai-codex-path (temp-test-token-path :openai-codex))
        (zai-path (temp-test-token-path :zai)))
    (with-provider-token-path-overrides (nil openai-codex-path zai-path)
      ;; Set up file-based token
      (clawmacs::save-provider-token :zai "file-zai-key")
      ;; Env var should win
      (with-env-var ("ZAI_CODING_MAX_API_KEY" "env-zai-key")
        (is (string= "env-zai-key"
                     (clawmacs::read-provider-token :zai)))))))

(test zai-env-var-falls-through-to-file
  "When ZAI_CODING_MAX_API_KEY is unset, falls through to static token file."
  (let ((openai-codex-path (temp-test-token-path :openai-codex))
        (zai-path (temp-test-token-path :zai)))
    (with-provider-token-path-overrides (nil openai-codex-path zai-path)
      (clawmacs::save-provider-token :zai "file-zai-key")
      (let ((clawmacs::*zai-env-var* "CLAWMACS_UNSET_ZAI_ENV_98765"))
        ;; With env var unset, should use file
        (is (string= "file-zai-key"
                     (clawmacs::read-provider-token :zai)))))))

(test env-var-does-not-affect-openai-codex
  "OpenAI Codex provider is not affected by Z.AI/OpenRouter env vars."
  (let ((openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (nil openai-codex-path)
      (clawmacs::save-provider-token :openai-codex "codex-file-token")
      (with-env-var ("ZAI_CODING_MAX_API_KEY" "env-zai-token")
        (with-env-var ("OPENROUTER_API_KEY" "env-openrouter-token")
          (is (string= "codex-file-token"
                       (clawmacs::read-provider-token :openai-codex))))))))

(test default-env-var-names-are-correct
  "Default environment variable names are as documented."
  (is (string= "ZAI_CODING_MAX_API_KEY" clawmacs::*zai-env-var*))
  (is (string= "OPENROUTER_API_KEY" clawmacs::*openrouter-env-var*)))

;;; --------------------------------------------------------------------------

(test resolve-buffer-provider-and-model-buffer-override-wins
  "Buffer overrides win over agent defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"zai\"}}")
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.3-codex")
    (set-buffer-think-level-override buf "high")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))
        (is (string= "high" think-level))))))

(test resolve-buffer-provider-and-model-agent-definition-wins-over-persisted-defaults
  "Programmatic agent definitions outrank persisted compatibility defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"zai\",\"model\":\"glm-5\"}}")
    (with-agent-defaults-path-override (path)
      (with-agent-definition-registry-override ()
        (clawmacs:register-agent-definition
         "spark"
         :provider :openai-codex
         :model "gpt-5.4"
         :think-level "high")
        (multiple-value-bind (provider model think-level)
            (clawmacs::resolve-buffer-provider-and-model buf)
          (is (eq :openai-codex provider))
          (is (string= "gpt-5.4" model))
          (is (string= "high" think-level)))))))

(test resolve-buffer-provider-and-model-agent-default-provider
  "Agent defaults are used when no buffer override is present."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\"}}")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))
        (is (null think-level))))))

(test resolve-buffer-provider-and-model-agent-default-model
  "Persisted agent default models are used when no buffer model override exists."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\",\"model\":\"gpt-5.3-codex\"}}")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))
        (is (null think-level))))))

(test resolve-buffer-provider-and-model-unknown-agent-falls-back
  "Unknown agents fall back to the current built-in provider/model defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "unknown-agent")))
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq clawmacs::*default-provider* provider))
        (is (string= (clawmacs::provider-fallback-model clawmacs::*default-provider*)
                     model))
        (is (null think-level))))))

(test resolve-buffer-provider-and-model-openai-codex-fallback-model
  "OpenAI Codex resolves to its built-in fallback model."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\"}}")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))
        (is (null think-level))))))

(test resolve-buffer-provider-and-model-unsupported-think-returns-nil
  "Unsupported think overrides do not resolve for the active model."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.1-codex-max")
    (set-buffer-think-level-override buf "xhigh")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.1-codex-max" model))
        (is (null think-level))))))

(test reconcile-buffer-think-level-override-resets-unsupported-model
  "Reconciliation clears a think level that no longer applies to the model."
  (let ((buf (make-buffer "test")))
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.1-codex-max")
    (set-buffer-think-level-override buf "xhigh")
    (multiple-value-bind (status think-level)
        (clawmacs::reconcile-buffer-think-level-override buf)
      (is (eq :reset status))
      (is (null think-level))
      (is (null (buffer-think-level-override buf))))))

(test resolve-buffer-provider-and-model-rejects-blank-persisted-model
  "Blank persisted default models are rejected when they become the resolved model."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\",\"model\":\"\"}}")
    (with-agent-defaults-path-override (path)
      (signals error
        (clawmacs::resolve-buffer-provider-and-model buf)))))

(test resolve-buffer-provider-and-model-rejects-blank-model
  "Blank buffer model overrides are rejected."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (setf (buffer-model-override buf) "")
    (with-agent-defaults-path-override (path)
      (signals error
        (clawmacs::resolve-buffer-provider-and-model buf)))))

(test agent-defaults-lazy-initialization-loads-file-and-built-ins
  "Lazy init loads file-backed defaults once and keeps built-in fallbacks."
  (let ((path (temp-agent-defaults-path)))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\"}}")
    (with-agent-defaults-path-override (path)
       (let ((initial-registry clawmacs::*agent-defaults-registry*))
         (is (null initial-registry))
         (is (eq :openai-codex (clawmacs::agent-default "spark")))
         (is (not (null clawmacs::*agent-defaults-registry*)))
         (is (eq clawmacs::*default-provider*
                 (clawmacs::agent-default "missing-agent")))))))

(test set-agent-default-persists-across-reload
  "set-agent-default persists provider and model across a registry reload."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (with-agent-defaults-path-override (path)
      (clawmacs::set-agent-default "spark" :openai-codex :model "gpt-5.3-codex")
      (setf clawmacs::*agent-defaults-registry* nil)
      (is (eq :openai-codex (clawmacs::agent-default "spark")))
       (multiple-value-bind (provider model think-level)
           (clawmacs::resolve-buffer-provider-and-model buf)
         (is (eq :openai-codex provider))
         (is (string= "gpt-5.3-codex" model))
         (is (null think-level))))))

(test clear-buffer-overrides-restores-agent-default-resolution
  "Clearing buffer overrides returns resolution to agent defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (with-agent-defaults-path-override (path)
      (set-agent-default "spark" :openai-codex :model "gpt-5.3-codex")
      (set-buffer-provider-override buf :zai)
      (set-buffer-model-override buf "glm-5")
      (set-buffer-think-level-override buf "high")
      (clear-buffer-routing-overrides buf)
      (multiple-value-bind (provider model think-level)
          (resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))
        (is (null think-level))
        (is (null (buffer-think-level-override buf)))))))

(test canonicalize-message-content-wraps-plain-text
  "Plain text content is normalized to one canonical text block."
  (is (equal '(((:type . "text")
                (:text . "hello")))
             (clawmacs::canonicalize-message-content "user" "hello"))))

(test canonicalize-message-content-accepts-assistant-tool-use
  "Assistant tool_use blocks are accepted as canonical content."
  (is (equal '(((:type . "tool_use")
                (:id . "toolu_123")
                (:name . "read_file")
                (:input . ((:path . "/tmp/example.txt")))))
             (clawmacs::canonicalize-message-content
              "assistant"
              '(((:type . "tool_use")
                 (:id . "toolu_123")
                 (:name . "read_file")
                 (:input . ((:path . "/tmp/example.txt")))))))))

(test canonicalize-message-content-accepts-user-tool-result
  "User tool_result blocks are accepted as canonical content."
  (is (equal '(((:type . "tool_result")
                (:tool--use--id . "toolu_123")
                (:content . "done")))
             (clawmacs::canonicalize-message-content
              "user"
              '(((:type . "tool_result")
                 (:tool-use-id . "toolu_123")
                 (:content . "done")))))))

(test canonical-tool-result-json-uses-tool-use-id-underscore-key
  "Canonical tool_result blocks encode tool_use_id (underscore), not camelCase."
  (let* ((block (first
                 (clawmacs::canonicalize-message-content
                  "user"
                  '(((:type . "tool_result")
                     (:tool-use-id . "toolu_123")
                     (:content . "done"))))))
         (json (clawmacs::api-json-encode block)))
    (is (search "\"tool_use_id\"" json))
    (is (not (search "\"toolUseId\"" json)))))

(test canonicalize-message-content-rejects-invalid-role-block-pairings
  "Invalid role/block pairings signal an error."
  (signals error
    (clawmacs::canonicalize-message-content
     "user"
     '(((:type . "tool_use")
        (:id . "toolu_123")
        (:name . "read_file")
        (:input . ((:path . "/tmp/example.txt")))))))
  (signals error
    (clawmacs::canonicalize-message-content
     "assistant"
     '(((:type . "tool_result")
         (:tool--use--id . "toolu_123")
         (:content . "done"))))))

(test canonicalize-message-content-rejects-invalid-role-for-plain-text
  "Plain string content rejects roles that cannot carry text blocks."
  (signals error
    (clawmacs::canonicalize-message-content "system" "hello")))

(test provider-request-rejects-anthropic-provider
  "Anthropic is no longer a dispatchable provider."
  (signals error
    (clawmacs::provider-request
     :anthropic
     '(((:role . "user") (:content . #())))
     :model "removed"))
  (signals error
    (clawmacs::provider-request-streaming
     :anthropic
     '(((:role . "user") (:content . #())))
     (lambda (state) (declare (ignore state)))
     :model "removed")))

(test provider-request-dispatches-openai-codex-adapter
  "OpenAI Codex requests use the Codex adapter and preserve model + reasoning."
  (let ((captured-model nil)
        (captured-reasoning nil))
    (with-function-override (clawmacs::openai-codex-request
                             (messages &key model max-tokens tools reasoning-effort system-prompt)
                             (declare (ignore messages max-tokens tools system-prompt))
                             (setf captured-model model
                                   captured-reasoning reasoning-effort)
                              '((:stop--reason . "stop")
                                (:content . #())))
      (is (equal '((:stop--reason . "stop")
                   (:content . #()))
                 (clawmacs::provider-request
                  :openai-codex
                  '(((:role . "user") (:content . #())))
                  :model "gpt-5.3-codex"
                  :reasoning-effort "high")))
      (is (string= "gpt-5.3-codex" captured-model))
      (is (string= "high" captured-reasoning)))))

(test provider-request-streaming-dispatches-by-provider
  "Streaming adapter dispatch follows the selected provider, model, and reasoning."
  (let ((openai-model nil)
        (openai-reasoning nil))
    (with-function-override (clawmacs::openai-codex-request-streaming
                              (messages callback &key model max-tokens tools reasoning-effort system-prompt)
                              (declare (ignore messages callback max-tokens tools system-prompt))
                              (setf openai-model model
                                    openai-reasoning reasoning-effort)
                              :openai-stream)
      (is (eq :openai-stream
              (clawmacs::provider-request-streaming
               :openai-codex
               '(((:role . "user") (:content . #())))
               (lambda (state) (declare (ignore state)))
               :model "codex-stream"
               :reasoning-effort "medium")))
      (is (string= "codex-stream" openai-model))
      (is (string= "medium" openai-reasoning)))))

(test start-streaming-response-uses-resolved-provider-and-model
  "Live streaming resolves provider/model/think first and passes them to the adapter."
  (let ((buf (make-buffer "routing-test" :agent-name "spark"))
        (captured-provider nil)
        (captured-model nil)
        (captured-reasoning nil)
        (captured-system-prompt nil))
    (with-function-override (clawmacs::resolve-buffer-provider-and-model (buffer)
                              (declare (ignore buffer))
                              (values :openai-codex "gpt-5.3-codex" "high"))
      (with-function-override (clawmacs::tool-definitions-for-api (&key buffer agent-name)
                                (declare (ignore buffer agent-name))
                                #())
        (with-function-override (clawmacs::build-conversation-messages (buffer)
                                  (declare (ignore buffer))
                                  '(((:role . "user") (:content . #()))))
          (with-function-override (clawmacs::build-agent-system-prompt (agent-name &key buffer)
                                    (declare (ignore buffer))
                                    (format nil "prompt for ~A" agent-name))
          (with-function-override (clawmacs::provider-request-streaming
                                    (provider messages callback &key model max-tokens tools reasoning-effort system-prompt)
                                    (declare (ignore messages callback max-tokens tools))
                                    (setf captured-provider provider
                                          captured-model model
                                          captured-reasoning reasoning-effort
                                          captured-system-prompt system-prompt)
                                    (clawmacs::make-stream-state))
            (clawmacs::start-streaming-response buf)
            (is (eq :openai-codex captured-provider))
            (is (string= "gpt-5.3-codex" captured-model))
            (is (string= "high" captured-reasoning))
            (is (string= "prompt for spark" captured-system-prompt))
            (let ((metadata (message-metadata (buffer-streaming-message buf))))
              (is (string= "spark"
                           (clawmacs::message-metadata-value
                            metadata :agent)))
              (is (eq :openai-codex
                      (clawmacs::message-metadata-value metadata :provider)))
              (is (string= "gpt-5.3-codex"
                           (clawmacs::message-metadata-value
                            metadata :model)))))))))))

(test start-streaming-response-surfaces-resolver-errors-in-buffer
  "Resolver failures are caught and rendered into the buffer as agent errors."
  (let ((buf (make-buffer "routing-error-test" :agent-name "spark")))
    (with-function-override (clawmacs::resolve-buffer-provider-and-model (buffer)
                              (declare (ignore buffer))
                              (error 'simple-error :format-control "resolver exploded"))
      (finishes (clawmacs::start-streaming-response buf))
      (is (eq :error (buffer-status buf)))
      (is (search "resolver exploded"
                  (message-text (buffer-first-message buf)))))))

(test update-streaming-response-does-not-double-count-mirrored-text-block
  "OpenAI-compatible streams mirror partial text into content blocks; render it once."
  (let* ((buf (make-buffer "stream-openai" :agent-name "agent"))
         (msg (buffer-insert-agent-message buf ""))
         (state (clawmacs::make-stream-state)))
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (setf (clawmacs::stream-state-text state) "hello"
            (clawmacs::stream-state-content-blocks state)
            (list (clawmacs::canonical-text-block "hello"))))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is-true (clawmacs::update-streaming-response buf))
    (is (string= "hello" (message-text msg)))
    (is (= 2 (buffer-message-count buf)))))

(test update-streaming-response-appends-accumulator-after-completed-blocks
  "Streaming state displays completed blocks plus the current text accumulator."
  (let* ((buf (make-buffer "stream-accumulator" :agent-name "agent"))
         (msg (buffer-insert-agent-message buf ""))
         (state (clawmacs::make-stream-state)))
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (setf (clawmacs::stream-state-text state) "world"
            (clawmacs::stream-state-content-blocks state)
            (list (clawmacs::canonical-text-block "Hello, "))))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is-true (clawmacs::update-streaming-response buf))
    (is (string= "Hello, world" (message-text msg)))
    (is (= 2 (buffer-message-count buf)))))

(test update-streaming-response-finalizes-single-placeholder-message
  "Completing a stream updates the existing placeholder instead of inserting another agent message."
  (let* ((buf (make-buffer "stream-final" :agent-name "agent"))
         (msg (buffer-insert-agent-message buf "partial"))
         (state (clawmacs::make-stream-state)))
    (clawmacs::put-message-metadata
     msg
     :agent "agent"
     :provider :zai
     :model "glm-5"
     :think-level nil)
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (setf (clawmacs::stream-state-text state) "final answer"
            (clawmacs::stream-state-content-blocks state)
            (list (clawmacs::canonical-text-block "final answer"))
            (clawmacs::stream-state-stop-reason state) "end_turn"
            (clawmacs::stream-state-usage state)
            '(:input-tokens 2006
              :output-tokens 300
              :total-tokens 2306
              :cached-input-tokens 1920
              :uncached-input-tokens 86
              :cache-hit-rate 0.9571286)
            (clawmacs::stream-state-done-p state) t))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is-false (clawmacs::update-streaming-response buf))
    (is (string= "final answer" (message-text msg)))
    (let ((metadata (message-metadata msg)))
      (is (eq :zai (clawmacs::message-metadata-value metadata :provider)))
      (is (string= "end_turn"
                   (clawmacs::message-metadata-value metadata :stop-reason)))
      (is (= 1 (clawmacs::message-metadata-value
                metadata :content-block-count)))
      (is (= 0 (clawmacs::message-metadata-value
                metadata :tool-call-count)))
      (is (= 1920 (clawmacs::message-metadata-value
                   metadata :cached-input-tokens)))
      (is (= 86 (clawmacs::message-metadata-value
                 metadata :uncached-input-tokens))))
    (is (= 2 (buffer-message-count buf)))
    (is (null (buffer-pending-stream buf)))
    (is (null (buffer-streaming-message buf)))
    (is (eq :idle (buffer-status buf)))))

(test update-streaming-response-records-completed-placeholder-once
  "Streaming placeholders are written to transcripts only after finalization."
  (let* ((*sessions-dir* (temp-session-test-directory "stream-final"))
         (session (load-or-create-session "stream-final"))
         (buf (make-buffer "stream-final"
                           :agent-name "agent"
                           :session session))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (clawmacs::make-stream-state)))
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (setf (clawmacs::stream-state-content-blocks state)
            (list (clawmacs::canonical-text-block "final answer"))
            (clawmacs::stream-state-stop-reason state) "end_turn"
            (clawmacs::stream-state-done-p state) t))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is (= 1 (length (session-current-events session))))
    (is-false (clawmacs::update-streaming-response buf))
    (let* ((events (session-current-events session))
           (message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            events))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "AGENT" (event-value event :sender)))
      (is (string= "final answer" (event-value event :text)))
      (is (vectorp (event-value event :raw-content))))))

(test update-streaming-response-records-streaming-error
  "Streaming errors are recorded as durable transcript messages."
  (let* ((*sessions-dir* (temp-session-test-directory "stream-error"))
         (session (load-or-create-session "stream-error"))
         (buf (make-buffer "stream-error"
                           :agent-name "agent"
                           :session session))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (clawmacs::make-stream-state)))
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (setf (clawmacs::stream-state-error-p state) "boom"))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is-false (clawmacs::update-streaming-response buf))
    (let* ((message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            (session-current-events session)))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "AGENT" (event-value event :sender)))
      (is (search "Streaming error"
                  (event-value event :text))))))

(test stop-streaming-response-records-partial-message
  "Stopping a stream preserves arrived text but does not feed the stop marker to providers."
  (let* ((*sessions-dir* (temp-session-test-directory "stream-stop"))
         (session (load-or-create-session "stream-stop"))
         (buf (make-buffer "stream-stop"
                           :agent-name "agent"
                           :session session))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (clawmacs::make-stream-state)))
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (setf (clawmacs::stream-state-text state) "partial answer"
            (clawmacs::stream-state-content-blocks state)
            (list (clawmacs::canonical-text-block "partial answer"))))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :thinking)
    (is-true (clawmacs::stop-streaming-response buf))
    (is (null (buffer-pending-stream buf)))
    (is (null (buffer-streaming-message buf)))
    (is (eq :idle (buffer-status buf)))
    (is (search "[Stopped by user]" (message-text msg)))
    (is (string= "partial answer"
                 (clawmacs::content-text-blocks (message-raw-content msg))))
    (is (not (search "[Stopped by user]"
                     (clawmacs::content-text-blocks
                      (message-raw-content msg)))))
    (is (string= "cancelled"
                 (clawmacs::message-metadata-value
                  (message-metadata msg)
                  :stop-reason)))
    (is-true (bt:with-lock-held ((clawmacs::stream-state-lock state))
               (clawmacs::stream-state-cancelled-p state)))
    (let* ((message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            (session-current-events session)))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "AGENT" (event-value event :sender)))
      (is (search "[Stopped by user]" (event-value event :text)))
      (is (vectorp (event-value event :raw-content))))))

(test stop-streaming-response-records-system-message-when-empty
  "Stopping before any assistant text creates a display-only system message."
  (let* ((*sessions-dir* (temp-session-test-directory "stream-stop-empty"))
         (session (load-or-create-session "stream-stop-empty"))
         (buf (make-buffer "stream-stop-empty"
                           :agent-name "agent"
                           :session session))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (clawmacs::make-stream-state)))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :thinking)
    (is-true (clawmacs::stop-streaming-response buf))
    (is (eq :system (message-sender msg)))
    (is (null (message-raw-content msg)))
    (is (string= "[Response stopped by user]" (message-text msg)))
    (is (null (clawmacs::build-conversation-messages buf)))
    (let* ((message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            (session-current-events session)))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "SYSTEM" (event-value event :sender)))
      (is (string= "[Response stopped by user]"
                   (event-value event :text))))))

(test handle-key-event-escape-stops-active-stream
  "Esc dispatches to the stop command while a stream is active."
  (let* ((buf (make-buffer "stream-stop-key" :agent-name "agent"))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (clawmacs::make-stream-state)))
    (clawmacs::init-default-keymap)
    (setf (buffer-keymap buf) *default-keymap*)
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (setf (clawmacs::stream-state-text state) "partial"))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :thinking)
    (is (eq :redraw
            (clawmacs::handle-key-event buf #\Esc)))
    (is (null (buffer-pending-stream buf)))
    (is (search "[Stopped by user]" (message-text msg)))))

(test insert-tool-results-message-records-raw-content
  "Tool result transcript events preserve canonical raw content."
  (let* ((*sessions-dir* (temp-session-test-directory "tool-result"))
         (session (load-or-create-session "tool-result"))
         (buf (make-buffer "tool-result"
                           :agent-name "agent"
                           :session session)))
    (clawmacs::insert-tool-results-message
     buf
     (list `((:result . "done")
             (:display . "[read_file] done")
             (:tool-id . "toolu_1"))))
    (let* ((message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            (session-current-events session)))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "TOOL-RESULT" (event-value event :sender)))
      (is (vectorp (event-value event :raw-content))))))

(test normalize-openai-token-usage-responses-shape
  "OpenAI Responses usage is normalized into prompt-cache telemetry."
  (let ((usage (clawmacs::normalize-openai-token-usage
                '((:input--tokens . 2006)
                  (:output--tokens . 300)
                  (:total--tokens . 2306)
                  (:input--tokens--details . ((:cached--tokens . 1920)))))))
    (is (= 2006 (getf usage :input-tokens)))
    (is (= 300 (getf usage :output-tokens)))
    (is (= 2306 (getf usage :total-tokens)))
    (is (= 1920 (getf usage :cached-input-tokens)))
    (is (= 86 (getf usage :uncached-input-tokens)))
    (is (< (abs (- (getf usage :cache-hit-rate)
                    (/ 1920.0 2006)))
           0.0001))
    (is (string= "tokens: input=2006 cached=1920 uncached=86 output=300 total=2306 cache-hit=95.7%"
                 (clawmacs::format-token-usage-summary usage)))))

(test normalize-openai-token-usage-chat-shape
  "Chat-style prompt token usage is normalized into the same telemetry shape."
  (let ((usage (clawmacs::normalize-openai-token-usage
                '((:prompt--tokens . 1000)
                  (:completion--tokens . 50)
                  (:total--tokens . 1050)
                  (:prompt--tokens--details . ((:cached--tokens . 768)))))))
    (is (= 1000 (getf usage :input-tokens)))
    (is (= 50 (getf usage :output-tokens)))
    (is (= 1050 (getf usage :total-tokens)))
    (is (= 768 (getf usage :cached-input-tokens)))
    (is (= 232 (getf usage :uncached-input-tokens)))
    (is (< (abs (- (getf usage :cache-hit-rate) 0.768))
           0.0001))))

(test openai-codex-request-normalizes-response-shape
  "OpenAI Codex non-streaming normalizes Responses output items."
  (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                            (declare (ignore refresh-if-needed))
                            '(:source :token-override
                              :mode :api-key
                              :token "openai-token"
                              :base-url "https://api.openai.com/v1"
                              :refreshable-p nil))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values
                               "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"hi from codex\"}]},{\"type\":\"function_call\",\"call_id\":\"call_1\",\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/codex.txt\\\"}\"}],\"usage\":{\"input_tokens\":2006,\"output_tokens\":300,\"total_tokens\":2306,\"input_tokens_details\":{\"cached_tokens\":1920}}}"
                               200))
      (let ((response (clawmacs::openai-codex-request '() :model "gpt-5.3-codex")))
        (is (string= "tool_use" (clawmacs::response-stop-reason response)))
        (is (equal '(((:type . "text")
                      (:text . "hi from codex"))
                     ((:type . "tool_use")
                      (:id . "call_1")
                      (:name . "read_file")
                      (:input . ((:path . "/tmp/codex.txt")))))
                   (clawmacs::response-content response)))
        (let ((usage (clawmacs::response-usage response)))
          (is (= 2006 (getf usage :input-tokens)))
          (is (= 1920 (getf usage :cached-input-tokens)))
          (is (= 86 (getf usage :uncached-input-tokens))))))))

(test openai-codex-request-normalizes-reasoning-summary
  "OpenAI Codex non-streaming preserves Responses reasoning summaries."
  (let* ((response '((:output . #(((:type . "reasoning")
                                  (:summary . #(((:type . "summary_text")
                                                 (:text . "provider summary")))))
                                 ((:type . "message")
                                  (:role . "assistant")
                                  (:content . #(((:type . "output_text")
                                                 (:text . "final")))))))))
         (canonical (clawmacs::responses-api-response->canonical-response
                     response))
         (content (clawmacs::response-content canonical)))
    (is (string= "end_turn" (clawmacs::response-stop-reason canonical)))
    (is (string= "final" (clawmacs::content-text-blocks content)))
    (is (equal '("provider summary")
               (clawmacs::content-reasoning-blocks content)))))

(test openai-codex-request-normalizes-reasoning-content
  "OpenAI Codex non-streaming preserves Responses reasoning content parts."
  (let* ((response '((:output . #(((:type . "reasoning")
                                  (:content . #(((:type . "reasoning_text")
                                                 (:text . "provider reasoning")))))
                                 ((:type . "message")
                                  (:role . "assistant")
                                  (:content . #(((:type . "output_text")
                                                 (:text . "final")))))))))
         (canonical (clawmacs::responses-api-response->canonical-response
                     response))
         (content (clawmacs::response-content canonical)))
    (is (string= "end_turn" (clawmacs::response-stop-reason canonical)))
    (is (string= "final" (clawmacs::content-text-blocks content)))
    (is (equal '("provider reasoning")
               (clawmacs::content-reasoning-blocks content)))))

(test openai-codex-request-uses-responses-api-and-chatgpt-headers
  "OpenAI Codex requests target /responses and use instructions + ChatGPT headers."
  (let* ((captured-request-body nil)
         (captured-url nil)
         (captured-headers nil)
         (messages (list (list (cons :role "user")
                               (cons :content
                                     (list (list (cons :type "text")
                                                 (cons :text "hello")))))))
         (auth '(:source :codex-chatgpt
                 :mode :chatgpt
                 :token "chatgpt-token"
                 :base-url "https://chatgpt.com/backend-api/codex"
                 :account-id "acct_123"
                 :refreshable-p t)))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-url (first args)
                                    captured-request-body (getf (rest args) :content)
                                    captured-headers (getf (rest args) :additional-headers))
                              (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                      200))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                auth)
        (with-function-override (clawmacs::build-system-prompt ()
                                  "boot prompt")
          (clawmacs::openai-codex-request messages :model "gpt-5.3-codex"))))
    (let* ((body (clawmacs::api-json-decode captured-request-body))
           (input-items (coerce (cdr (assoc :input body)) 'list))
           (message-item (first input-items))
           (content-item (first (coerce (cdr (assoc :content message-item)) 'list))))
      (is (string= "https://chatgpt.com/backend-api/codex/responses" captured-url))
      (is (string= "Bearer chatgpt-token"
                   (cdr (assoc "Authorization" captured-headers :test #'string=))))
      (is (string= "acct_123"
                   (cdr (assoc "ChatGPT-Account-ID" captured-headers :test #'string=))))
      (is (search "\"store\":false" captured-request-body))
      (is (search "\"stream\":false" captured-request-body))
      (is (not (search "max_output_tokens" captured-request-body)))
      (is (< (search "\"tools\"" captured-request-body)
             (search "\"input\"" captured-request-body)))
      (let ((reasoning (cdr (assoc :reasoning body))))
        (is (string= "detailed" (cdr (assoc :summary reasoning))))
        (is (null (assoc :effort reasoning))))
      (is (string= "boot prompt" (cdr (assoc :instructions body))))
      (is (not (assoc :messages body)))
      (is (string= "message" (cdr (assoc :type message-item))))
      (is (string= "user" (cdr (assoc :role message-item))))
      (is (string= "input_text" (cdr (assoc :type content-item))))
      (is (string= "hello" (cdr (assoc :text content-item)))))))

(test openai-codex-request-includes-reasoning-effort
  "OpenAI Codex requests include reasoning.effort when set."
  (let ((captured-request-body nil))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                      200))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (clawmacs::openai-codex-request '()
                                        :model "gpt-5.4"
                                        :reasoning-effort "xhigh")))
    (let* ((body (clawmacs::api-json-decode captured-request-body))
           (reasoning (cdr (assoc :reasoning body)))
           (effort (cdr (assoc :effort reasoning))))
      (is (string= "xhigh" effort))
      (is (string= "detailed" (cdr (assoc :summary reasoning)))))))

(test openai-codex-request-includes-prompt-cache-controls
  "OpenAI Codex requests include prompt cache routing controls when bound."
  (let ((captured-request-body nil))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body
                                    (getf (rest args) :content))
                              (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                      200))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((clawmacs::*openai-codex-prompt-cache-key*
                "clawmacs-agent-cache-probe")
              (clawmacs::*openai-codex-prompt-cache-retention* "24h"))
          (clawmacs::openai-codex-request '()
                                          :model "gpt-5.4"))))
    (let ((body (clawmacs::api-json-decode captured-request-body)))
      (is (string= "clawmacs-agent-cache-probe"
                   (cdr (assoc :prompt--cache--key body))))
      (is (string= "24h"
                   (cdr (assoc :prompt--cache--retention body)))))))

(test openai-codex-request-retries-on-401-after-refresh
  "OpenAI Codex retries once after a 401 when ChatGPT auth is refreshable."
  (let ((captured-authz nil)
        (calls 0)
        (refresh-called-p nil))
    (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                              (declare (ignore refresh-if-needed))
                              '(:source :codex-chatgpt
                                :mode :chatgpt
                                :token "expired-token"
                                :base-url "https://chatgpt.com/backend-api/codex"
                                :account-id "acct_123"
                                :refreshable-p t))
      (with-function-override (clawmacs::refresh-openai-codex-auth-descriptor ()
                                (setf refresh-called-p t)
                                '(:source :codex-chatgpt
                                  :mode :chatgpt
                                  :token "fresh-token"
                                  :base-url "https://chatgpt.com/backend-api/codex"
                                  :account-id "acct_123"
                                  :refreshable-p t))
        (with-function-override (drakma:http-request (url &rest args)
                                  (declare (ignore url))
                                  (push (cdr (assoc "Authorization"
                                                    (getf args :additional-headers)
                                                    :test #'string=))
                                        captured-authz)
                                  (incf calls)
                                  (if (= calls 1)
                                      (values "unauthorized" 401)
                                      (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                              200)))
          (let ((response (clawmacs::openai-codex-request '() :model "gpt-5.3-codex")))
            (is (string= "end_turn" (clawmacs::response-stop-reason response)))
            (is-true refresh-called-p)
            (is (= 2 calls))
            (is (equal '("Bearer expired-token" "Bearer fresh-token")
                       (nreverse captured-authz)))))))))

(test openai-codex-request-retries-transient-503-with-backoff
  "OpenAI Codex retries transient 503 responses before failing the request."
  (let ((calls 0)
        (sleeps nil))
    (let ((clawmacs::*provider-http-max-retries* 3)
          (clawmacs::*provider-http-initial-backoff-seconds* 0.5)
          (clawmacs::*provider-http-backoff-multiplier* 2.0)
          (clawmacs::*provider-http-max-backoff-seconds* 8.0)
          (clawmacs::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (drakma:http-request (&rest args)
                                  (declare (ignore args))
                                  (incf calls)
                                  (case calls
                                    ((1 2)
                                     (values "service unavailable" 503 nil))
                                    (otherwise
                                     (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                             200))))
          (let ((response (clawmacs::openai-codex-request '()
                                                          :model "gpt-5.3-codex")))
            (is (string= "end_turn"
                         (clawmacs::response-stop-reason response)))))))
    (is (= 3 calls))
    (is (equalp '(0.5 1.0) (nreverse sleeps)))))

(test openai-codex-request-honors-retry-after-header
  "Retry-After controls the backoff delay for transient provider responses."
  (let ((calls 0)
        (sleeps nil))
    (let ((clawmacs::*provider-http-max-retries* 2)
          (clawmacs::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (drakma:http-request (&rest args)
                                  (declare (ignore args))
                                  (incf calls)
                                  (if (= calls 1)
                                      (values "service unavailable"
                                              503
                                              '(("Retry-After" . "2")))
                                      (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                              200)))
          (let ((response (clawmacs::openai-codex-request '()
                                                          :model "gpt-5.3-codex")))
            (is (string= "end_turn"
                         (clawmacs::response-stop-reason response)))))))
    (is (= 2 calls))
    (is (equal '(2) (nreverse sleeps)))))

(test openai-codex-request-does-not-retry-client-errors
  "Non-transient HTTP errors are returned to the provider-specific handler."
  (let ((calls 0)
        (sleeps nil))
    (let ((clawmacs::*provider-http-max-retries* 3)
          (clawmacs::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (drakma:http-request (&rest args)
                                  (declare (ignore args))
                                  (incf calls)
                                  (values "bad request" 400 nil))
          (signals error
            (clawmacs::openai-codex-request '()
                                            :model "gpt-5.3-codex")))))
    (is (= 1 calls))
    (is (null sleeps))))

(test openai-codex-request-retries-connection-errors
  "Connection-level provider failures are retried before surfacing."
  (let ((calls 0)
        (sleeps nil))
    (let ((clawmacs::*provider-http-max-retries* 2)
          (clawmacs::*provider-http-initial-backoff-seconds* 0.25)
          (clawmacs::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (drakma:http-request (&rest args)
                                  (declare (ignore args))
                                  (incf calls)
                                  (if (= calls 1)
                                      (error "connection refused")
                                      (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                              200)))
          (let ((response (clawmacs::openai-codex-request '()
                                                          :model "gpt-5.3-codex")))
            (is (string= "end_turn"
                         (clawmacs::response-stop-reason response)))))))
    (is (= 2 calls))
    (is (equalp '(0.25) (nreverse sleeps)))))

(test openai-codex-streaming-normalizes-response-shape
  "OpenAI Codex streaming adapter accumulates Responses output deltas."
  (let ((captured-force-binary nil)
        (payloads '("data: {\"type\":\"response.output_text.delta\",\"delta\":\"hi from \"}"
                    ""
                    "data: {\"type\":\"response.output_text.delta\",\"delta\":\"codex\"}"
                    ""
                    "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-force-binary (getf (rest args) :force-binary))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((state (clawmacs::openai-codex-request-streaming '() (lambda (state) (declare (ignore state)))
                                                              :model "gpt-5.3-codex")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01))
          (is-true captured-force-binary)
          (is (string= "end_turn"
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-stop-reason state))))
          (is (equal '(((:type . "text")
                        (:text . "hi from codex")))
                     (bt:with-lock-held ((clawmacs::stream-state-lock state))
                       (reverse (clawmacs::stream-state-content-blocks state))))))))))

(test openai-codex-streaming-preserves-reasoning-summary
  "OpenAI Codex streaming adapter preserves reasoning summary events."
  (let ((state (clawmacs::make-stream-state)))
    (clawmacs::process-openai-codex-responses-sse-event
     "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"provider \"}"
     state)
    (clawmacs::process-openai-codex-responses-sse-event
     "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"summary\"}"
     state)
    (clawmacs::process-openai-codex-responses-sse-event
     "{\"type\":\"response.reasoning_summary_text.done\",\"text\":\"provider summary\"}"
     state)
    (clawmacs::process-openai-codex-responses-sse-event
     "{\"type\":\"response.output_text.delta\",\"delta\":\"final\"}"
     state)
    (clawmacs::process-openai-codex-responses-sse-event
     "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"
     state)
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (let ((content (reverse
                      (copy-list
                       (clawmacs::stream-state-content-blocks state)))))
        (is (string= "end_turn"
                     (clawmacs::stream-state-stop-reason state)))
        (is (string= "final"
                     (clawmacs::content-text-blocks content)))
        (is (equal '("provider summary")
                   (clawmacs::content-reasoning-blocks content)))))))

(test openai-codex-streaming-records-completed-usage
  "OpenAI Codex streaming adapter records usage from response.completed."
  (let ((state (clawmacs::make-stream-state)))
    (clawmacs::process-openai-codex-responses-sse-event
     "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\",\"usage\":{\"input_tokens\":2006,\"output_tokens\":300,\"total_tokens\":2306,\"input_tokens_details\":{\"cached_tokens\":1920}}}}"
     state)
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (is-true (clawmacs::stream-state-done-p state))
      (let ((usage (clawmacs::stream-state-usage state)))
        (is (= 2006 (getf usage :input-tokens)))
        (is (= 300 (getf usage :output-tokens)))
        (is (= 2306 (getf usage :total-tokens)))
        (is (= 1920 (getf usage :cached-input-tokens)))
        (is (= 86 (getf usage :uncached-input-tokens)))))))

(test openai-codex-streaming-uses-responses-instructions
  "OpenAI Codex streaming requests send instructions + input, not chat messages."
  (let* ((captured-request-body nil)
         (captured-external-format-in nil)
         (captured-force-binary nil)
         (messages (list (list (cons :role "user")
                               (cons :content
                                     (list (list (cons :type "text")
                                                 (cons :text "hello")))))))
         (payloads '("data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"
                     "")))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content)
                                    captured-external-format-in (getf (rest args) :external-format-in)
                                    captured-force-binary (getf (rest args) :force-binary))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (clawmacs::build-system-prompt ()
                                  "boot prompt")
          (let ((state (clawmacs::openai-codex-request-streaming
                        messages
                        (lambda (state) (declare (ignore state)))
                        :model "gpt-5.3-codex")))
            (loop repeat 100
                  until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                          (clawmacs::stream-state-done-p state))
                  do (sleep 0.01)))))
    (let* ((body (clawmacs::api-json-decode captured-request-body))
           (input-items (coerce (cdr (assoc :input body)) 'list))
           (message-item (first input-items))
           (content-item (first (coerce (cdr (assoc :content message-item)) 'list))))
      (is (eq :utf-8 captured-external-format-in))
      (is-true captured-force-binary)
      (is (search "\"store\":false" captured-request-body))
      (is (search "\"stream\":true" captured-request-body))
      (is (not (search "max_output_tokens" captured-request-body)))
      (let ((reasoning (cdr (assoc :reasoning body))))
        (is (string= "detailed" (cdr (assoc :summary reasoning))))
        (is (null (assoc :effort reasoning))))
      (is (string= "boot prompt" (cdr (assoc :instructions body))))
      (is (string= "message" (cdr (assoc :type message-item))))
      (is (string= "input_text" (cdr (assoc :type content-item))))
      (is (string= "hello" (cdr (assoc :text content-item)))))
    (setf captured-request-body nil)
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((state (clawmacs::openai-codex-request-streaming
                      '()
                      (lambda (state) (declare (ignore state)))
                      :model "gpt-5.4"
                      :reasoning-effort "high")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01)))))
    (let* ((body (clawmacs::api-json-decode captured-request-body))
           (reasoning (cdr (assoc :reasoning body))))
      (is (string= "high" (cdr (assoc :effort reasoning))))
      (is (string= "detailed" (cdr (assoc :summary reasoning)))))))

(test openai-codex-streaming-decodes-utf8-punctuation-from-octets
  "OpenAI Codex streaming decodes UTF-8 punctuation correctly from octet streams."
  (let* ((captured-force-binary nil)
         (expected "Test received — I’m here.")
         (payload (format nil
                          "data: {\"type\":\"response.output_text.delta\",\"delta\":\"~A\"}~%~%data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}~%~%"
                          expected))
         (octets (flexi-streams:string-to-octets payload :external-format :utf-8)))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-force-binary (getf (rest args) :force-binary))
                              (values (flexi-streams:make-in-memory-input-stream octets)
                                      200
                                      nil))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((state (clawmacs::openai-codex-request-streaming '() (lambda (state) (declare (ignore state)))
                                                              :model "gpt-5.3-codex")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01))
          (is-true captured-force-binary)
          (is (string= "end_turn"
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-stop-reason state))))
          (is (equal `(((:type . "text")
                        (:text . ,expected)))
                     (bt:with-lock-held ((clawmacs::stream-state-lock state))
                       (reverse (clawmacs::stream-state-content-blocks state))))))))))

(test openai-codex-streaming-supports-multiple-tool-calls
  "OpenAI Codex streaming keeps two Responses function calls separate and canonical."
  (let ((payloads '("data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call_1\",\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/one.txt\\\"}\"}}"
                    ""
                    "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call_2\",\"name\":\"write_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/two.txt\\\",\\\"content\\\":\\\"hello\\\"}\"}}"
                    ""
                    "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((state (clawmacs::openai-codex-request-streaming '() (lambda (state) (declare (ignore state)))
                                                              :model "gpt-5.3-codex")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01))
          (is (string= "tool_use"
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-stop-reason state))))
          (is (equal '(((:type . "tool_use")
                        (:id . "call_1")
                        (:name . "read_file")
                        (:input . ((:path . "/tmp/one.txt"))))
                       ((:type . "tool_use")
                        (:id . "call_2")
                        (:name . "write_file")
                        (:input . ((:path . "/tmp/two.txt")
                                   (:content . "hello")))))
                     (bt:with-lock-held ((clawmacs::stream-state-lock state))
                       (reverse (clawmacs::stream-state-content-blocks state))))))))))

;;; --------------------------------------------------------------------------
;;; OpenAI Codex OAuth Tests
;;; --------------------------------------------------------------------------

(test generate-code-verifier-length-and-characters
  "Code verifier is 43 alphanumeric characters."
  (let ((verifier (clawmacs::generate-code-verifier)))
    (is (= 43 (length verifier)))
    (is (every #'alphanumericp verifier))))

(test generate-code-verifier-uniqueness
  "Two generated verifiers are different."
  (let ((v1 (clawmacs::generate-code-verifier))
        (v2 (clawmacs::generate-code-verifier)))
    (is (not (string= v1 v2)))))

(test generate-oauth-state-length
  "OAuth state is a base64url token."
  (let ((state (clawmacs::generate-oauth-state)))
    (is (> (length state) 30))
    (is (not (find #\+ state)))
    (is (not (find #\/ state)))
    (is (not (find #\= state)))))

(test compute-code-challenge-is-base64url
  "Code challenge is base64url encoded (no +, /, or = characters)."
  (let ((challenge (clawmacs::compute-code-challenge "test-verifier-12345678901234567890123456")))
    (is (plusp (length challenge)))
    (is (not (find #\+ challenge)))
    (is (not (find #\/ challenge)))
    (is (not (find #\= challenge)))))

(test compute-code-challenge-deterministic
  "Same verifier produces the same challenge."
  (let* ((verifier "deterministic-test-verifier-1234567890abcdef")
         (c1 (clawmacs::compute-code-challenge verifier))
         (c2 (clawmacs::compute-code-challenge verifier)))
    (is (string= c1 c2))))

(test url-encode-param-preserves-safe-characters
  "URL encoding preserves unreserved characters (RFC 3986)."
  (is (string= "abc-_.~" (clawmacs::url-encode-param "abc-_.~")))
  (is (string= "ABCxyz0189" (clawmacs::url-encode-param "ABCxyz0189"))))

(test url-encode-param-encodes-special-characters
  "URL encoding percent-encodes spaces, slashes, and other special characters."
  (is (string= "hello%20world" (clawmacs::url-encode-param "hello world")))
  (is (string= "a%2Fb" (clawmacs::url-encode-param "a/b")))
  (is (string= "key%3Dvalue" (clawmacs::url-encode-param "key=value")))
  (is (string= "q%26a" (clawmacs::url-encode-param "q&a"))))

(test extract-oauth-callback-params-extracts-code-and-state
  "Callback URL parameters are correctly extracted."
  (multiple-value-bind (code state)
      (clawmacs::extract-oauth-callback-params
       "http://localhost:1455/auth/callback?code=abc123&state=xyz789")
    (is (string= "abc123" code))
    (is (string= "xyz789" state))))

(test extract-oauth-callback-params-code-only
  "Callback URL with only code (no state) still works."
  (multiple-value-bind (code state)
      (clawmacs::extract-oauth-callback-params
       "http://localhost:1455/auth/callback?code=onlycode")
    (is (string= "onlycode" code))
    (is (null state))))

(test extract-oauth-callback-params-rejects-missing-query
  "Callback URL without query parameters signals an error."
  (signals error
    (clawmacs::extract-oauth-callback-params
     "http://localhost:1455/auth/callback")))

(test extract-oauth-callback-params-rejects-missing-code
  "Callback URL without a code parameter signals an error."
  (signals error
    (clawmacs::extract-oauth-callback-params
     "http://localhost:1455/auth/callback?state=xyz789")))

(test openai-codex-oauth-start-returns-valid-url
  "oauth-start returns an authorization URL with all required PKCE parameters."
  (multiple-value-bind (url verifier state)
      (clawmacs::openai-codex-oauth-start)
    (is (search "https://auth.openai.com/oauth/authorize?" url))
    (is (search "client_id=app_EMoamEEZ73f0CkXaXp7hrann" url))
    (is (search "response_type=code" url))
    (is (search "redirect_uri=http%3A%2F%2Flocalhost%3A1455%2Fauth%2Fcallback" url))
    (is (search "scope=openid%20profile%20email%20offline_access%20api.connectors.read%20api.connectors.invoke" url))
    (is (search "code_challenge_method=S256" url))
    (is (search "code_challenge=" url))
    (is (search "id_token_add_organizations=true" url))
    (is (search "codex_cli_simplified_flow=true" url))
    (is (search "originator=codex_cli_rs" url))
    (is (search "api.connectors.read" url))
    (is (= 43 (length verifier)))
    (is (search (format nil "state=~A" state) url))))

(test save-and-read-openai-codex-oauth-tokens-round-trip
  "OpenAI Codex auth.json round-trips through the compatibility helpers."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (clawmacs::save-openai-codex-oauth-tokens
       "access-tok" "refresh-tok" nil
       :id-token "id-tok"
       :account-id "acct_123"
       :openai-api-key "sk-api"
       :auth-mode :chatgpt)
      (let ((creds (clawmacs::read-openai-codex-oauth-tokens)))
        (is (eq :chatgpt (getf creds :auth-mode)))
        (is (string= "access-tok" (getf creds :access-token)))
        (is (string= "refresh-tok" (getf creds :refresh-token)))
        (is (string= "acct_123" (getf creds :account-id)))
        (is (string= "sk-api" (getf creds :openai-api-key)))))))

(test read-openai-codex-oauth-tokens-returns-nil-when-missing
  "Reading from a nonexistent path returns nil."
  (with-codex-auth-path-override (#P"/tmp/nonexistent-clawmacs-oauth/auth.json")
    (is (null (clawmacs::read-openai-codex-oauth-tokens)))))

(test read-openai-codex-oauth-token-returns-valid-token
  "read-openai-codex-oauth-token returns the ChatGPT access token from auth.json."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "fresh-token"
                              :refresh-token "refresh-tok"))
      (with-function-override (clawmacs::openai-codex-chatgpt-auth-stale-p (auth-json)
                                (declare (ignore auth-json))
                                nil)
        (is (string= "fresh-token"
                     (clawmacs::read-openai-codex-oauth-token)))))))

(test read-openai-codex-oauth-token-refreshes-when-expired
  "read-openai-codex-oauth-token refreshes stale ChatGPT auth via auth.json."
  (let ((path (temp-codex-auth-path))
        (refresh-called-p nil))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "old-token"
                              :refresh-token "good-refresh"))
      (with-function-override (clawmacs::openai-codex-chatgpt-auth-stale-p (auth-json)
                                (declare (ignore auth-json))
                                t)
        (with-function-override (clawmacs::refresh-openai-codex-auth-json (&optional auth-json)
                                  (declare (ignore auth-json))
                                  (setf refresh-called-p t)
                                  (make-codex-chatgpt-auth-payload
                                   :access-token "refreshed-token"
                                   :refresh-token "good-refresh"))
          (is (string= "refreshed-token"
                       (clawmacs::read-openai-codex-oauth-token)))
          (is-true refresh-called-p))))))

(test read-provider-token-prefers-static-override-for-openai-codex
  "OpenAI Codex uses the clawmacs token file before shared auth.json."
  (let ((path (temp-codex-auth-path))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "oauth-token"))
      (with-provider-token-path-overrides (nil openai-codex-path)
      (clawmacs::save-provider-token :openai-codex "static-token")
        (is (string= "static-token"
                     (clawmacs::read-provider-token :openai-codex)))))))

(test read-provider-token-ignores-url-like-openai-codex-override
  "A URL-like OpenAI Codex override token is ignored in favor of shared auth.json."
  (let ((path (temp-codex-auth-path))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                              :account-id "acct_456"))
      (with-provider-token-path-overrides (nil openai-codex-path)
        (clawmacs::save-provider-token
         :openai-codex
         "http://localhost:1455/auth/callback?code=abc&state=xyz")
        (is (string= "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                     (clawmacs::read-provider-token :openai-codex)))))))

(test read-provider-token-falls-back-to-codex-auth-json-for-openai-codex
  "OpenAI Codex falls back to shared auth.json when no override token exists."
  (let ((path (temp-codex-auth-path))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "oauth-token"))
      (with-provider-token-path-overrides (nil openai-codex-path)
        (is (string= "oauth-token"
                     (clawmacs::read-provider-token :openai-codex)))))))

(test resolve-openai-codex-auth-api-key-mode-uses-openai-base-url
  "API-key auth.json resolves to the OpenAI base URL."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-api-key-auth-payload
                              :api-key "sk-api"))
      (let ((auth (clawmacs::resolve-openai-codex-auth)))
        (is (eq :api-key (getf auth :mode)))
        (is (string= "sk-api" (getf auth :token)))
        (is (string= "https://api.openai.com/v1" (getf auth :base-url)))))))

(test resolve-openai-codex-auth-chatgpt-mode-uses-chatgpt-base-url
  "ChatGPT auth.json resolves to the ChatGPT Codex backend and preserves account id."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                              :refresh-token "chatgpt-refresh"
                              :account-id "acct_456"))
      (with-function-override (clawmacs::openai-codex-chatgpt-auth-stale-p (auth-json)
                                (declare (ignore auth-json))
                                nil)
        (let ((auth (clawmacs::resolve-openai-codex-auth)))
          (is (eq :chatgpt (getf auth :mode)))
          (is (eq :codex-chatgpt (getf auth :source)))
          (is (string= "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                       (getf auth :token)))
          (is (string= "acct_456" (getf auth :account-id)))
          (is (string= "https://chatgpt.com/backend-api/codex"
                       (getf auth :base-url)))))))))

(test resolve-openai-codex-auth-chatgpt-missing-account-id-errors
  "ChatGPT auth requires an account id in the shared auth store."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :account-id nil))
      (signals error
        (clawmacs::resolve-openai-codex-auth)))))

(test provider-has-token-p-openai-codex-accepts-codex-auth-json
  "provider-has-token-p treats shared Codex auth.json as valid OpenAI Codex auth."
  (let ((path (temp-codex-auth-path))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-api-key-auth-payload
                              :api-key "sk-selector"))
      (with-provider-token-path-overrides (nil openai-codex-path)
        (is-true (clawmacs::provider-has-token-p :openai-codex))))))

(test exchange-openai-oauth-code-makes-correct-request
  "Token exchange sends the correct form parameters to the token endpoint."
  (let ((captured-content nil)
        (captured-url nil))
    (with-function-override (drakma:http-request (url &rest args)
                              (setf captured-url url
                                    captured-content (getf args :content))
                              (values "{\"id_token\":\"id-token\",\"access_token\":\"new-access\",\"refresh_token\":\"new-refresh\"}"
                                      200))
      (let ((tokens (clawmacs::exchange-openai-oauth-code "auth-code-123" "verifier-xyz")))
        (is (string= "https://auth.openai.com/oauth/token" captured-url))
        (is (search "grant_type=authorization_code" captured-content))
        (is (search "code=auth-code-123" captured-content))
        (is (search "code_verifier=verifier-xyz" captured-content))
        (is (string= "id-token" (getf tokens :id-token)))
        (is (string= "new-access" (getf tokens :access-token)))
        (is (string= "new-refresh" (getf tokens :refresh-token)))))))

(test openai-codex-oauth-finish-validates-state
  "oauth-finish rejects mismatched state parameters."
  (with-function-override (clawmacs::exchange-openai-oauth-code (code verifier &key redirect-uri)
                            (declare (ignore code verifier redirect-uri))
                            (list :id-token "id-token"
                                  :access-token "tok"
                                  :refresh-token "ref"
                                  :account-id "acct_123"))
    (let ((path (temp-codex-auth-path)))
      (with-codex-auth-path-override (path)
      (signals error
        (clawmacs::openai-codex-oauth-finish
         "http://localhost:1455/auth/callback?code=abc&state=wrong"
         "verifier"
         "expected-state"))))))

(test openai-codex-oauth-finish-succeeds-with-matching-state
  "oauth-finish completes and persists a Codex-compatible auth.json payload."
  (with-function-override (clawmacs::exchange-openai-oauth-code (code verifier &key redirect-uri)
                            (is (string= "auth-code" code))
                            (is (string= "my-verifier" verifier))
                            (is (string= (clawmacs::openai-oauth-redirect-uri) redirect-uri))
                            (list :id-token "id-token"
                                  :access-token "final-token"
                                  :refresh-token "final-refresh"
                                  :account-id "acct_789"))
    (with-function-override (clawmacs::obtain-openai-codex-api-key (id-token)
                              (is (string= "id-token" id-token))
                              "sk-exchanged")
      (let ((path (temp-codex-auth-path)))
        (with-codex-auth-path-override (path)
          (let ((result (clawmacs::openai-codex-oauth-finish
                         "http://localhost:1455/auth/callback?code=auth-code&state=good-state"
                         "my-verifier"
                         "good-state")))
            (is (string= "final-token" result))
            (let ((saved (clawmacs::read-openai-codex-oauth-tokens)))
              (is (eq :chatgpt (getf saved :auth-mode)))
              (is (string= "final-token" (getf saved :access-token)))
              (is (string= "final-refresh" (getf saved :refresh-token)))
              (is (string= "acct_789" (getf saved :account-id)))
              (is (string= "sk-exchanged" (getf saved :openai-api-key))))))))))

;;; --------------------------------------------------------------------------
;;; Z.AI (Zhipu AI) Provider Tests
;;; --------------------------------------------------------------------------

(test zai-provider-token-path
  "Z.AI provider token path is zai-api-key."
  (let ((path (clawmacs::provider-token-path :zai)))
    (is (search "zai-api-key" (namestring path)))))

(test zai-known-provider-p
  "Z.AI is recognized as a known provider."
  (is-true (clawmacs::known-provider-p :zai)))

(test zai-fallback-model-is-glm-5
  "Z.AI fallback model is glm-5."
  (is (string= "glm-5"
               (clawmacs::provider-fallback-model :zai))))

(test zai-normalize-provider-keyword
  "normalize-provider accepts :zai."
  (is (eq :zai (clawmacs::normalize-provider :zai))))

(test zai-normalize-provider-string
  "normalize-provider accepts \"zai\" string."
  (is (eq :zai (clawmacs::normalize-provider "zai"))))

(test zai-read-provider-token-from-file
  "read-provider-token reads Z.AI API key from static file."
  (let ((zai-path (temp-test-token-path :zai))
        (openai-codex-path (temp-test-token-path :openai-codex))
        (clawmacs::*zai-env-var* "CLAWMACS_UNSET_ZAI_ENV_98765"))
    (with-provider-token-path-overrides (nil openai-codex-path zai-path)
      (clawmacs::save-provider-token :zai "zai-test-key-abc123")
      (is (string= "zai-test-key-abc123"
                   (clawmacs::read-provider-token :zai))))))

(test zai-request-sends-correct-headers-and-body
  "Z.AI non-streaming sends correct Authorization and Accept-Language headers."
  (let ((captured-url nil)
        (captured-headers nil)
        (captured-body nil)
        (messages (list (list (cons :role "user")
                              (cons :content
                                    (list (list (cons :type "text")
                                                (cons :text "hello"))))))))
    (with-function-override (drakma:http-request (url &rest args)
                              (setf captured-url url
                                    captured-headers (getf args :additional-headers)
                                    captured-body (getf args :content))
                              (values "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"hi from glm\"}}]}"
                                      200))
      (with-function-override (clawmacs::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key-test")
        (clawmacs::zai-request messages :model "glm-5")))
    (is (string= "https://api.z.ai/api/coding/paas/v4/chat/completions" captured-url))
    (is (string= "Bearer zai-key-test"
                 (cdr (assoc "Authorization" captured-headers :test #'string=))))
    (is (string= "en-US,en"
                 (cdr (assoc "Accept-Language" captured-headers :test #'string=))))
    (let ((body (clawmacs::api-json-decode captured-body)))
      (is (string= "glm-5" (cdr (assoc :model body)))))))

(test zai-request-normalizes-response
  "Z.AI non-streaming normalizes the OpenAI-compatible response to canonical shape."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"你好世界\"}}]}"
                             200))
    (with-function-override (clawmacs::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let ((response (clawmacs::zai-request '() :model "glm-5")))
        (is (string= "end_turn" (clawmacs::response-stop-reason response)))
        (is (equal '(((:type . "text")
                      (:text . "你好世界")))
                    (clawmacs::response-content response)))))))

(test zai-request-retries-transient-503
  "Z.AI non-streaming requests use the shared transient HTTP retry path."
  (let ((calls 0)
        (sleeps nil))
    (let ((clawmacs::*provider-http-max-retries* 1)
          (clawmacs::*provider-http-initial-backoff-seconds* 0.5)
          (clawmacs::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (drakma:http-request (&rest args)
                                (declare (ignore args))
                                (incf calls)
                                (if (= calls 1)
                                    (values "service unavailable" 503 nil)
                                    (values
                                     "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"ok\"}}]}"
                                     200)))
        (with-function-override (clawmacs::read-provider-token (provider)
                                  (declare (ignore provider))
                                  "zai-key")
          (let ((response (clawmacs::zai-request '() :model "glm-5")))
            (is (string= "end_turn"
                         (clawmacs::response-stop-reason response)))))))
    (is (= 2 calls))
    (is (equalp '(0.5) (nreverse sleeps)))))

(test zai-request-with-tool-calls
  "Z.AI non-streaming handles tool_calls responses correctly."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"tool_calls\",\"message\":{\"content\":\"let me check\",\"tool_calls\":[{\"id\":\"call_z1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/test.txt\\\"}\"}}]}}]}"
                             200))
    (with-function-override (clawmacs::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let ((response (clawmacs::zai-request '() :model "glm-5")))
        (is (string= "tool_use" (clawmacs::response-stop-reason response)))
        (is (equal '(((:type . "text")
                      (:text . "let me check"))
                     ((:type . "tool_use")
                      (:id . "call_z1")
                      (:name . "read_file")
                      (:input . ((:path . "/tmp/test.txt")))))
                    (clawmacs::response-content response)))))))

(test zai-request-includes-system-prompt-message
  "Z.AI requests prepend the built system prompt as an OpenAI system message."
  (let ((captured-request-body nil)
        (messages (list (list (cons :role "user")
                              (cons :content
                                    (list (list (cons :type "text")
                                                (cons :text "hello"))))))))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"ok\"}}]}"
                                      200))
      (with-function-override (clawmacs::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (with-function-override (clawmacs::build-system-prompt ()
                                  "zai boot prompt")
          (clawmacs::zai-request messages :model "glm-5"))))
    (let* ((body (clawmacs::api-json-decode captured-request-body))
           (sent-messages (coerce (cdr (assoc :messages body)) 'list)))
      (is (string= "system" (cdr (assoc :role (first sent-messages)))))
      (is (string= "zai boot prompt" (cdr (assoc :content (first sent-messages)))))
      (is (string= "user" (cdr (assoc :role (second sent-messages))))))))

(test zai-request-uses-max-tokens-not-max-completion-tokens
  "Z.AI requests use max_tokens (not max_completion_tokens like OpenAI Codex)."
  (let ((captured-request-body nil))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"ok\"}}]}"
                                      200))
      (with-function-override (clawmacs::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (clawmacs::zai-request '() :model "glm-5" :max-tokens 4096)))
    (let ((body (clawmacs::api-json-decode captured-request-body)))
      (is (= 4096 (cdr (assoc :max--tokens body))))
      (is (null (assoc :max--completion--tokens body))))))

(test zai-streaming-normalizes-response-shape
  "Z.AI streaming adapter accumulates canonical content blocks."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"content\":\"你好\"}}]}"
                    ""
                    "data: {\"choices\":[{\"delta\":{\"content\":\"世界\"}}]}"
                    ""
                    "data: {\"choices\":[{\"finish_reason\":\"stop\"}]}"
                    ""
                    "data: [DONE]"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (let ((state (clawmacs::zai-request-streaming '() (lambda (state) (declare (ignore state)))
                                                      :model "glm-5")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01))
          (is (string= "end_turn"
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-stop-reason state))))
          (is (equal '(((:type . "text")
                        (:text . "你好世界")))
                     (bt:with-lock-held ((clawmacs::stream-state-lock state))
                       (reverse (clawmacs::stream-state-content-blocks state))))))))))

(test zai-streaming-includes-system-prompt
  "Z.AI streaming requests prepend the built system prompt."
  (let* ((captured-request-body nil)
         (messages (list (list (cons :role "user")
                               (cons :content
                                     (list (list (cons :type "text")
                                                 (cons :text "hello")))))))
         (payloads '("data: {\"choices\":[{\"finish_reason\":\"stop\"}]}"
                     ""
                     "data: [DONE]"
                     "")))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (with-function-override (clawmacs::build-system-prompt ()
                                  "zai system prompt")
          (let ((state (clawmacs::zai-request-streaming
                        messages
                        (lambda (state) (declare (ignore state)))
                        :model "glm-5")))
            (loop repeat 100
                  until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                          (clawmacs::stream-state-done-p state))
                  do (sleep 0.01))))))
    (let* ((body (clawmacs::api-json-decode captured-request-body))
           (sent-messages (coerce (cdr (assoc :messages body)) 'list)))
      (is (string= "system" (cdr (assoc :role (first sent-messages)))))
      (is (string= "zai system prompt" (cdr (assoc :content (first sent-messages))))))))

(test zai-streaming-with-tool-calls
  "Z.AI streaming supports tool calls via OpenAI-compatible protocol."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_z2\",\"type\":\"function\",\"function\":{\"name\":\"shell\",\"arguments\":\"{\\\"command\\\":\\\"ls\\\"}\"}}]}}]}"
                    ""
                    "data: {\"choices\":[{\"finish_reason\":\"tool_calls\"}]}"
                    ""
                    "data: [DONE]"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (let ((state (clawmacs::zai-request-streaming '() (lambda (state) (declare (ignore state)))
                                                      :model "glm-5")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01))
          (is (string= "tool_use"
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-stop-reason state))))
          (is (equal '(((:type . "tool_use")
                        (:id . "call_z2")
                        (:name . "shell")
                        (:input . ((:command . "ls")))))
                     (bt:with-lock-held ((clawmacs::stream-state-lock state))
                       (reverse (clawmacs::stream-state-content-blocks state))))))))))

(test zai-provider-dispatch-routes-correctly
  "provider-request dispatches :zai to zai-request."
  (let ((dispatched-provider nil))
    (with-function-override (clawmacs::zai-request (messages &key model max-tokens tools system-prompt)
                              (declare (ignore messages model max-tokens tools system-prompt))
                              (setf dispatched-provider :zai)
                              '((:stop--reason . "end_turn") (:content . #())))
      (clawmacs::provider-request :zai '() :model "glm-5")
      (is (eq :zai dispatched-provider)))))

(test zai-agent-defaults-round-trip
  "Agent defaults registry handles :zai as a provider."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (clawmacs::set-agent-default "zhipu" :zai :model "glm-4.7")
      (is (eq :zai (clawmacs::agent-default "zhipu")))
      (is (string= "glm-4.7"
                   (clawmacs::agent-default-model "zhipu" :zai))))))

;;; --------------------------------------------------------------------------
;;; Reasoning Content Handling (Z.AI GLM, DeepSeek R1, etc.)
;;; --------------------------------------------------------------------------

(test reasoning-content-non-streaming-content-preferred
  "Non-streaming: when both content and reasoning_content are present, content wins."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"Hello\",\"reasoning_content\":\"The user wants a greeting...\"}}]}"
                             200))
    (with-function-override (clawmacs::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let* ((response (clawmacs::zai-request '() :model "glm-5"))
             (content (clawmacs::response-content response)))
        (is (string= "Hello"
                     (cdr (assoc :text (first content)))))
        (is (equal '("The user wants a greeting...")
                   (clawmacs::content-reasoning-blocks content)))))))

(test reasoning-content-non-streaming-fallback
  "Non-streaming: when content is blank but reasoning_content is present, use reasoning."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"length\",\"message\":{\"content\":\"\",\"reasoning_content\":\"The user wants a greeting. Options: Hello, Hi...\"}}]}"
                             200))
    (with-function-override (clawmacs::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let ((response (clawmacs::zai-request '() :model "glm-5")))
        (is (string= "The user wants a greeting. Options: Hello, Hi..."
                     (cdr (assoc :text (first (clawmacs::response-content response))))))
        ;; finish_reason should be "max_tokens" (mapped from "length")
        (is (string= "max_tokens" (clawmacs::response-stop-reason response)))))))

(test reasoning-content-non-streaming-no-reasoning
  "Non-streaming: when only content is present (no reasoning), works normally."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"Hello world\"}}]}"
                             200))
    (with-function-override (clawmacs::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let ((response (clawmacs::zai-request '() :model "glm-5")))
        (is (string= "Hello world"
                     (cdr (assoc :text (first (clawmacs::response-content response))))))))))

(test reasoning-content-streaming-with-reasoning-then-content
  "Streaming: reasoning_content chunks are preserved separately from content."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"Thinking...\"}}]}"
                    ""
                    "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\" more thoughts\"}}]}"
                    ""
                    "data: {\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}"
                    ""
                    "data: {\"choices\":[{\"finish_reason\":\"stop\"}]}"
                    ""
                    "data: [DONE]"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (let ((state (clawmacs::zai-request-streaming '() (lambda (state) (declare (ignore state)))
                                                      :model "glm-5")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01))
          (let ((content-blocks
                  (bt:with-lock-held ((clawmacs::stream-state-lock state))
                    (is (string= "Hello" (clawmacs::stream-state-text state)))
                    (nreverse
                     (copy-list
                      (clawmacs::stream-state-content-blocks state))))))
            (is (equal '("Thinking... more thoughts")
                       (clawmacs::content-reasoning-blocks content-blocks)))
            (is (string= "Hello"
                         (clawmacs::content-text-blocks content-blocks)))
            (is (string= "Hello"
                         (clawmacs::stream-state-display-text state)))
            (is (string= (format nil "Hello~%;; reasoning~%Thinking... more thoughts")
                         (clawmacs::stream-state-display-text
                          state
                          :show-reasoning-p t)))))))))

(test reasoning-content-streaming-reasoning-only
  "Streaming: when only reasoning_content chunks arrive, it is captured as reasoning."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"Step 1: analyze...\"}}]}"
                    ""
                    "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\" Step 2: decide...\"}}]}"
                    ""
                    "data: {\"choices\":[{\"finish_reason\":\"length\",\"delta\":{\"content\":\"\"}}]}"
                    ""
                    "data: [DONE]"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (let ((state (clawmacs::zai-request-streaming '() (lambda (state) (declare (ignore state)))
                                                      :model "glm-5")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01))
          (let ((content-blocks
                  (bt:with-lock-held ((clawmacs::stream-state-lock state))
                    (is (string= "" (clawmacs::stream-state-text state)))
                    (is (string= "max_tokens"
                                 (clawmacs::stream-state-stop-reason state)))
                    (nreverse
                     (copy-list
                      (clawmacs::stream-state-content-blocks state))))))
            (is (equal '("Step 1: analyze... Step 2: decide...")
                       (clawmacs::content-reasoning-blocks content-blocks)))
            (is (string= ""
                         (clawmacs::stream-state-display-text state)))
            (is (string= (format nil ";; reasoning~%Step 1: analyze... Step 2: decide...")
                         (clawmacs::stream-state-display-text
                          state
                          :show-reasoning-p t)))))))))

;;; --------------------------------------------------------------------------
;;; Known Models Tests
;;; --------------------------------------------------------------------------

(test provider-known-models-openai-codex
  "Known OpenAI Codex models list is non-empty and contains the default."
  (let ((models (clawmacs::provider-known-models :openai-codex)))
    (is (listp models))
    (is (plusp (length models)))
    (is (member "gpt-5.3-codex" models :test #'string=))
    (is (member "gpt-5.4" models :test #'string=))
    (is (member "gpt-5.2-codex" models :test #'string=))
    (is (member "gpt-5.1-codex-max" models :test #'string=))
    (is (member "gpt-5.1-codex-mini" models :test #'string=))
    (is (member "gpt-5.2" models :test #'string=))
    (is (member clawmacs::*openai-codex-model* models :test #'string=))
    (is (= 6 (length models)))))

(test normalize-provider-openai-codex-storage-forms
  "normalize-provider accepts both kebab-case and JSON camelCase storage forms."
  (is (eq :openai-codex
          (clawmacs::normalize-provider "openai-codex")))
  (is (eq :openai-codex
          (clawmacs::normalize-provider "openaiCodex"))))

(test provider-model-supported-think-levels-openai-codex
  "OpenAI-Codex think levels are model-specific."
  (let ((gpt-54 (clawmacs::provider-model-supported-think-levels
                 :openai-codex "gpt-5.4"))
        (gpt-53-codex (clawmacs::provider-model-supported-think-levels
                       :openai-codex "gpt-5.3-codex"))
        (gpt-51-max (clawmacs::provider-model-supported-think-levels
                     :openai-codex "gpt-5.1-codex-max")))
    (is (equal '("none" "low" "medium" "high" "xhigh") gpt-54))
    (is (equal '("low" "medium" "high" "xhigh") gpt-53-codex))
    (is (equal '("none" "low" "medium" "high") gpt-51-max))
    (is (null (clawmacs::provider-model-supported-think-levels
               :zai "glm-5")))))

(test provider-known-models-zai
  "Known Z.AI models list is non-empty and contains the default."
  (let ((models (clawmacs::provider-known-models :zai)))
    (is (listp models))
    (is (plusp (length models)))
    (is (member "glm-5" models :test #'string=))
    ;; Should include turbo and older variants
    (is (member "glm-5-turbo" models :test #'string=))
    (is (member "glm-4.7" models :test #'string=))))

(test provider-known-models-unknown-returns-nil
  "Unknown provider returns nil for known models."
  (is (null (clawmacs::provider-known-models :unknown-provider))))

(test available-models-for-selector-marks-active
  "available-models-for-selector marks the current buffer's model as active."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      ;; Set agent default so resolution works
      (clawmacs::set-agent-default "coder" :zai :model "glm-5")
      (let ((buf (make-buffer "test" :agent-name "coder")))
        ;; Mock provider-has-token-p to only return t for :zai
        (with-function-override (clawmacs::provider-has-token-p (provider)
                                  (eq provider :zai))
          (let ((entries (clawmacs::available-models-for-selector buf)))
            ;; Should have entries for Z.AI only
            (is (plusp (length entries)))
            (is (every (lambda (e) (eq :zai (getf e :provider))) entries))
            ;; Exactly one entry should be active
            (let ((active-count (count-if (lambda (e) (getf e :active-p)) entries)))
              (is (= 1 active-count)))
            ;; The active entry should be the default model
            (let ((active (find-if (lambda (e) (getf e :active-p)) entries)))
              (is (string= "glm-5" (getf active :model))))))))))

(test available-models-for-selector-multi-provider
  "available-models-for-selector includes models from multiple providers."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (clawmacs::set-agent-default "coder" :zai :model "glm-5")
      (let ((buf (make-buffer "test" :agent-name "coder")))
        ;; Mock: both openai-codex and zai have tokens
        (with-function-override (clawmacs::provider-has-token-p (provider)
                                  (not (null (member provider '(:openai-codex :zai)))))
          (let ((entries (clawmacs::available-models-for-selector buf)))
            ;; Should have entries from both providers
            (is (plusp (length entries)))
            (let ((providers (remove-duplicates
                              (mapcar (lambda (e) (getf e :provider)) entries))))
              (is (member :openai-codex providers))
              (is (member :zai providers)))))))))

(test available-models-for-selector-no-tokens
  "available-models-for-selector returns nil when no provider has a token."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (let ((buf (make-buffer "test" :agent-name "agent")))
        ;; Mock: no tokens available
        (with-function-override (clawmacs::provider-has-token-p (provider)
                                  (declare (ignore provider))
                                  nil)
          (let ((entries (clawmacs::available-models-for-selector buf)))
            (is (null entries))))))))

;;; --------------------------------------------------------------------------
;;; OpenRouter Tests
;;; --------------------------------------------------------------------------

(test openrouter-provider-token-path
  "OpenRouter provider token path is provider-specific."
  (let ((home (user-homedir-pathname)))
    (is (equal (merge-pathnames #P".config/clawmacs/openrouter-api-key" home)
               (clawmacs::provider-token-path :openrouter)))))

(test openrouter-token-round-trip
  "OpenRouter API keys round-trip through provider-specific helpers."
  (let ((or-path (merge-pathnames
                  (format nil ".config/clawmacs/test-openrouter-~A" (gensym))
                  (user-homedir-pathname)))
        (clawmacs::*openrouter-env-var* "CLAWMACS_UNSET_OPENROUTER_ENV_98765"))
    (unwind-protect
         (let ((original (symbol-function 'clawmacs::provider-token-path)))
           (unwind-protect
                (progn
                  (setf (symbol-function 'clawmacs::provider-token-path)
                        (lambda (provider)
                          (if (eq provider :openrouter)
                              or-path
                              (funcall original provider))))
                  (is (string= "sk-or-test-key"
                               (clawmacs::save-provider-token :openrouter "sk-or-test-key")))
                  (is (string= "sk-or-test-key"
                               (clawmacs::read-provider-token :openrouter))))
             (setf (symbol-function 'clawmacs::provider-token-path) original)))
      (ignore-errors (delete-file or-path)))))

(test openrouter-env-var-token
  "read-provider-token prefers OPENROUTER_API_KEY environment variable."
  (with-env-var ("OPENROUTER_API_KEY" "sk-or-env-token")
    (is (string= "sk-or-env-token"
                 (clawmacs::read-env-token clawmacs::*openrouter-env-var*)))))

(test openrouter-provider-known
  "known-provider-p recognises :openrouter."
  (is (clawmacs::known-provider-p :openrouter)))

(test openrouter-normalize-provider
  "normalize-provider accepts :openrouter and the string form."
  (is (eq :openrouter (clawmacs::normalize-provider :openrouter)))
  (is (eq :openrouter (clawmacs::normalize-provider "openrouter")))
  (is (eq :openrouter (clawmacs::normalize-provider "OPENROUTER"))))

(test openrouter-fallback-model
  "provider-fallback-model returns a model string for :openrouter."
  (let ((m (clawmacs::provider-fallback-model :openrouter)))
    (is (and (stringp m) (plusp (length m))))))

(test openrouter-provider-known-models-static
  "provider-known-models returns the static fallback list for :openrouter
when no cached models are present."
  (let ((clawmacs::*openrouter-cached-models* nil))
    (let ((models (clawmacs::provider-known-models :openrouter)))
      (is (listp models))
      (is (plusp (length models)))
      ;; First static model is the default
      (is (string= "openai/gpt-5.3-codex" (first models)))
      (is-false (find-if (lambda (model)
                           (search "anthropic/" model))
                         models)))))

(test openrouter-provider-known-models-cached
  "provider-known-models returns the cached list when *openrouter-cached-models* is set."
  (let ((clawmacs::*openrouter-cached-models* '("custom/model-a" "custom/model-b")))
    (is (equal '("custom/model-a" "custom/model-b")
               (clawmacs::provider-known-models :openrouter)))))

(test fetch-openrouter-models-returns-cached
  "fetch-openrouter-models returns *openrouter-cached-models* without an HTTP call."
  (let ((clawmacs::*openrouter-cached-models* '("cached/model-1" "cached/model-2")))
    (is (equal '("cached/model-1" "cached/model-2")
               (clawmacs::fetch-openrouter-models)))))

(test fetch-openrouter-models-parses-api-response
  "fetch-openrouter-models populates *openrouter-cached-models* from parsed JSON."
  (let ((clawmacs::*openrouter-cached-models* nil))
    (with-function-override (clawmacs::read-provider-token
                             (provider)
                             (when (eq provider :openrouter) "sk-or-test"))
      (with-function-override (drakma:http-request
                               (url &rest args)
                               (declare (ignore url args))
                               (values "{\"data\":[{\"id\":\"openai/gpt-4o\"},{\"id\":\"google/gemini-2.5-pro\"}]}"
                                       200))
        (let ((models (clawmacs::fetch-openrouter-models)))
          (is (member "openai/gpt-4o" models :test #'string=))
          (is (member "google/gemini-2.5-pro" models :test #'string=))
          ;; Cache should be populated
          (is (equal models clawmacs::*openrouter-cached-models*)))))))

(test fetch-openrouter-models-falls-back-on-no-token
  "fetch-openrouter-models returns static fallback when no API key is configured."
  (let ((clawmacs::*openrouter-cached-models* nil))
    (with-function-override (clawmacs::read-provider-token
                             (provider)
                             (declare (ignore provider))
                             nil)
      (let ((models (clawmacs::fetch-openrouter-models)))
        (is (listp models))
        (is (plusp (length models)))))))

(test fetch-openrouter-models-falls-back-on-http-error
  "fetch-openrouter-models returns static fallback on HTTP errors."
  (let ((clawmacs::*openrouter-cached-models* nil))
    (with-function-override (clawmacs::read-provider-token
                             (provider)
                             (when (eq provider :openrouter) "sk-or-test"))
      (with-function-override (drakma:http-request
                               (url &rest args)
                               (declare (ignore url args))
                               (values "Unauthorized" 401))
        (let ((models (clawmacs::fetch-openrouter-models)))
          (is (listp models))
          (is (plusp (length models))))))))

(test provider-request-routes-openrouter
  "provider-request dispatches :openrouter to openrouter-request."
  (let ((routed-to nil))
    (with-function-override (clawmacs::openrouter-request
                             (messages &key model max-tokens tools system-prompt)
                             (declare (ignore messages max-tokens tools system-prompt))
                             (setf routed-to model)
                             `((:stop--reason . "end_turn")
                               (:content . ,(vector `((:type . "text")
                                                      (:text . "ok"))))))
      (clawmacs::provider-request :openrouter nil :model "openai/gpt-4o-mini")
      (is (string= "openai/gpt-4o-mini" routed-to)))))

(test provider-request-streaming-routes-openrouter
  "provider-request-streaming dispatches :openrouter to openrouter-request-streaming."
  (let ((routed-to nil))
    (with-function-override (clawmacs::openrouter-request-streaming
                             (messages callback &key model max-tokens tools system-prompt)
                             (declare (ignore messages callback max-tokens tools system-prompt))
                             (setf routed-to model)
                             (clawmacs::make-stream-state))
      (clawmacs::provider-request-streaming :openrouter nil nil
                                            :model "anthropic/claude-3-5-haiku")
      (is (string= "anthropic/claude-3-5-haiku" routed-to)))))
