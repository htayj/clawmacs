(in-package :clawmacs/tests)

(in-suite llm-suite)

(defun temp-test-token-path (provider)
  (let* ((base (make-pathname :directory (list :absolute "tmp"
                                               (format nil "clawmacs-llm-tests-~A"
                                                       (gensym)))))
         (filename (ecase provider
                     (:anthropic "claude-max-token")
                     (:openai-codex "openai-codex-token"))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames filename base)))

(defmacro with-provider-token-path-overrides ((anthropic-path openai-codex-path) &body body)
  `(let ((original-provider-token-path
           (symbol-function 'clawmacs::provider-token-path)))
     (unwind-protect
          (progn
            (setf (symbol-function 'clawmacs::provider-token-path)
                  (lambda (provider)
                    (case provider
                      (:anthropic ,anthropic-path)
                      (:openai-codex ,openai-codex-path)
                      (otherwise
                       (funcall original-provider-token-path provider)))))
            ,@body)
       (setf (symbol-function 'clawmacs::provider-token-path)
             original-provider-token-path))))

(defun temp-agent-defaults-path ()
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "clawmacs-agent-defaults-~A"
                                                      (gensym))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames "agent-defaults.json" base)))

(defmacro with-agent-defaults-path-override ((path) &body body)
  `(let ((clawmacs::*agent-defaults-path* ,path)
         (clawmacs::*agent-defaults-registry* nil))
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

(test provider-token-paths
  "Provider token paths are provider-specific."
  (let ((home (user-homedir-pathname)))
    (is (equal (merge-pathnames #P".config/clawmacs/claude-max-token" home)
               (clawmacs::provider-token-path :anthropic)))
    (is (equal (merge-pathnames #P".config/clawmacs/openai-codex-token" home)
               (clawmacs::provider-token-path :openai-codex)))))

(test provider-token-path-unknown-provider
  "Unknown providers signal a clear error."
  (signals error
    (clawmacs::provider-token-path :unknown-provider)))

(test provider-token-round-trip-anthropic
  "Anthropic tokens round-trip through provider-specific helpers."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      (is (string= "anthropic-token"
                   (clawmacs::save-provider-token :anthropic "anthropic-token")))
      (is (string= "anthropic-token"
                   (clawmacs::read-provider-token :anthropic))))))

(test provider-token-round-trip-openai-codex
  "OpenAI Codex tokens round-trip through provider-specific helpers."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      (is (string= "openai-token"
                   (clawmacs::save-provider-token :openai-codex "openai-token")))
      (is (string= "openai-token"
                   (clawmacs::read-provider-token :openai-codex))))))

(test read-provider-token-trims-whitespace
  "Provider token reads trim surrounding whitespace."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      (with-open-file (stream anthropic-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string "  trimmed-token  " stream)
        (terpri stream))
      (is (string= "trimmed-token"
                   (clawmacs::read-provider-token :anthropic))))))

(test read-claude-code-oauth-token-reads-credentials
  "read-claude-code-oauth-token extracts the access token from Claude Code credentials."
  (let ((creds-path (merge-pathnames
                     (format nil "clawmacs-creds-~A/credentials.json" (gensym))
                     #P"/tmp/")))
    (ensure-directories-exist creds-path)
    (with-open-file (s creds-path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-string "{\"claudeAiOauth\":{\"accessToken\":\"sk-ant-oat01-test-token\",\"refreshToken\":\"sk-ant-ort01-test\"}}" s))
    (let ((clawmacs::*claude-code-credentials-path* creds-path))
      (is (string= "sk-ant-oat01-test-token"
                   (clawmacs::read-claude-code-oauth-token))))))

(test read-claude-code-oauth-token-returns-nil-when-missing
  "read-claude-code-oauth-token returns nil when credentials file is absent."
  (let ((clawmacs::*claude-code-credentials-path*
          #P"/tmp/nonexistent-clawmacs-creds/credentials.json"))
    (is (null (clawmacs::read-claude-code-oauth-token)))))

(test read-provider-token-prefers-claude-code-for-anthropic
  "read-provider-token prefers Claude Code credentials for :ANTHROPIC."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex))
        (creds-path (merge-pathnames
                     (format nil "clawmacs-creds-~A/credentials.json" (gensym))
                     #P"/tmp/")))
    (ensure-directories-exist creds-path)
    ;; Write a token file with one value
    (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      (clawmacs::save-provider-token :anthropic "file-token")
      ;; Write Claude Code credentials with a different value
      (with-open-file (s creds-path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (write-string "{\"claudeAiOauth\":{\"accessToken\":\"claude-code-token\"}}" s))
      (let ((clawmacs::*claude-code-credentials-path* creds-path))
        ;; Should prefer Claude Code token
        (is (string= "claude-code-token"
                     (clawmacs::read-provider-token :anthropic)))
        ;; OpenAI Codex should still use its own file
        (clawmacs::save-provider-token :openai-codex "codex-token")
        (is (string= "codex-token"
                     (clawmacs::read-provider-token :openai-codex)))))))

(test read-provider-token-falls-back-to-file-when-no-claude-code
  "read-provider-token falls back to token file when Claude Code credentials are absent."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex))
        (clawmacs::*claude-code-credentials-path*
          #P"/tmp/nonexistent-clawmacs-creds/credentials.json"))
    (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      (clawmacs::save-provider-token :anthropic "fallback-token")
      (is (string= "fallback-token"
                   (clawmacs::read-provider-token :anthropic))))))

(test read-token-uses-anthropic-provider-path
  "read-token delegates to the Anthropic provider-specific file."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      (with-open-file (stream anthropic-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
         (write-string "anthropic-delegated-token" stream))
       (is (string= "anthropic-delegated-token"
                    (read-token))))))

(test resolve-buffer-provider-and-model-buffer-override-wins
  "Buffer overrides win over agent defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"anthropic\"}}")
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.3-codex")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))))))

(test resolve-buffer-provider-and-model-agent-default-provider
  "Agent defaults are used when no buffer override is present."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\"}}")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "codex-mini-latest" model))))))

(test resolve-buffer-provider-and-model-agent-default-model
  "Persisted agent default models are used when no buffer model override exists."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\",\"model\":\"gpt-5.3-codex\"}}")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))))))

(test resolve-buffer-provider-and-model-unknown-agent-falls-back
  "Unknown agents fall back to the Anthropic built-in default."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "unknown-agent")))
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :anthropic provider))
        (is (string= "claude-haiku-4-5-20251001" model))))))

(test resolve-buffer-provider-and-model-openai-codex-fallback-model
  "OpenAI Codex resolves to its built-in fallback model."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\"}}")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "codex-mini-latest" model))))))

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
         (is (eq :anthropic (clawmacs::agent-default "missing-agent")))))))

(test set-agent-default-persists-across-reload
  "set-agent-default persists provider and model across a registry reload."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (with-agent-defaults-path-override (path)
      (clawmacs::set-agent-default "spark" :openai-codex :model "gpt-5.3-codex")
      (setf clawmacs::*agent-defaults-registry* nil)
      (is (eq :openai-codex (clawmacs::agent-default "spark")))
       (multiple-value-bind (provider model)
           (clawmacs::resolve-buffer-provider-and-model buf)
         (is (eq :openai-codex provider))
         (is (string= "gpt-5.3-codex" model))))))

(test clear-buffer-overrides-restores-agent-default-resolution
  "Clearing buffer overrides returns resolution to agent defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (with-agent-defaults-path-override (path)
      (set-agent-default "spark" :openai-codex :model "gpt-5.3-codex")
      (set-buffer-provider-override buf :anthropic)
      (set-buffer-model-override buf "claude-override")
      (clear-buffer-provider/model-overrides buf)
      (multiple-value-bind (provider model)
          (resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))))))

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

(test provider-request-dispatches-anthropic-adapter
  "Anthropic provider requests use the Anthropic adapter and preserve the model."
  (let ((captured nil))
    (with-function-override (clawmacs::anthropic-request (messages &key model max-tokens tools)
                              (declare (ignore messages max-tokens tools))
                              (setf captured model)
                              '((:stop--reason . "end_turn")
                                (:content . #())))
      (is (equal '((:stop--reason . "end_turn")
                   (:content . #()))
                 (clawmacs::provider-request
                  :anthropic
                  '(((:role . "user") (:content . #())))
                  :model "claude-test")))
      (is (string= "claude-test" captured)))))

(test provider-request-dispatches-openai-codex-adapter
  "OpenAI Codex requests use the Codex adapter and preserve the model."
  (let ((captured nil))
    (with-function-override (clawmacs::openai-codex-request (messages &key model max-tokens tools)
                              (declare (ignore messages max-tokens tools))
                              (setf captured model)
                              '((:stop--reason . "stop")
                                (:content . #())))
      (is (equal '((:stop--reason . "stop")
                   (:content . #()))
                 (clawmacs::provider-request
                  :openai-codex
                  '(((:role . "user") (:content . #())))
                  :model "gpt-5.3-codex")))
      (is (string= "gpt-5.3-codex" captured)))))

(test provider-request-streaming-dispatches-by-provider
  "Streaming adapter dispatch follows the selected provider and model."
  (let ((anthropic-model nil)
        (openai-model nil))
    (with-function-override (clawmacs::anthropic-request-streaming (messages callback &key model max-tokens tools)
                              (declare (ignore messages callback max-tokens tools))
                              (setf anthropic-model model)
                              :anthropic-stream)
      (with-function-override (clawmacs::openai-codex-request-streaming (messages callback &key model max-tokens tools)
                                (declare (ignore messages callback max-tokens tools))
                                (setf openai-model model)
                                :openai-stream)
        (is (eq :anthropic-stream
                (clawmacs::provider-request-streaming
                 :anthropic
                 '(((:role . "user") (:content . #())))
                 (lambda (state) (declare (ignore state)))
                 :model "claude-stream")))
        (is (eq :openai-stream
                (clawmacs::provider-request-streaming
                 :openai-codex
                 '(((:role . "user") (:content . #())))
                 (lambda (state) (declare (ignore state)))
                 :model "codex-stream")))
        (is (string= "claude-stream" anthropic-model))
        (is (string= "codex-stream" openai-model))))))

(test start-streaming-response-uses-resolved-provider-and-model
  "Live streaming resolves provider/model first and passes both to the adapter."
  (let ((buf (make-buffer "routing-test" :agent-name "spark"))
        (captured-provider nil)
        (captured-model nil))
    (with-function-override (clawmacs::resolve-buffer-provider-and-model (buffer)
                              (declare (ignore buffer))
                              (values :openai-codex "gpt-5.3-codex"))
      (with-function-override (clawmacs::tool-definitions-for-api ()
                                #())
        (with-function-override (clawmacs::build-conversation-messages (buffer)
                                  (declare (ignore buffer))
                                  '(((:role . "user") (:content . #()))))
          (with-function-override (clawmacs::provider-request-streaming (provider messages callback &key model max-tokens tools)
                                    (declare (ignore messages callback max-tokens tools))
                                    (setf captured-provider provider
                                          captured-model model)
                                    (clawmacs::make-stream-state))
            (clawmacs::start-streaming-response buf)
            (is (eq :openai-codex captured-provider))
            (is (string= "gpt-5.3-codex" captured-model))))))))

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

(test anthropic-request-normalizes-response-shape
  "Anthropic adapter returns canonical stop reason and content blocks."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"stop_reason\":\"tool_use\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"},{\"type\":\"tool_use\",\"id\":\"toolu_1\",\"name\":\"read_file\",\"input\":{\"path\":\"/tmp/example.txt\"}}]}"
                             200))
    (with-function-override (clawmacs::read-token ()
                              "anthropic-token")
      (let ((response (clawmacs::anthropic-request '() :model "claude-test")))
        (is (string= "tool_use" (clawmacs::response-stop-reason response)))
        (is (equal '(((:type . "text")
                      (:text . "hi"))
                     ((:type . "tool_use")
                      (:id . "toolu_1")
                      (:name . "read_file")
                      (:input . ((:path . "/tmp/example.txt")))))
                   (clawmacs::response-content response)))))))

(test openai-codex-request-normalizes-response-shape
  "OpenAI Codex adapter returns canonical stop reason and content blocks."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"tool_calls\",\"message\":{\"content\":\"hi from codex\",\"tool_calls\":[{\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/codex.txt\\\"}\"}}]}}]}"
                             200))
    (with-function-override (clawmacs::read-provider-token (provider)
                              (declare (ignore provider))
                              "openai-token")
      (let ((response (clawmacs::openai-codex-request '() :model "gpt-5.3-codex")))
        (is (string= "tool_use" (clawmacs::response-stop-reason response)))
        (is (equal '(((:type . "text")
                      (:text . "hi from codex"))
                     ((:type . "tool_use")
                      (:id . "call_1")
                      (:name . "read_file")
                      (:input . ((:path . "/tmp/codex.txt")))))
                    (clawmacs::response-content response)))))))

(test openai-codex-request-includes-system-prompt-message
  "OpenAI Codex non-streaming requests prepend the built system prompt."
  (let* ((captured-request-body nil)
         (messages (list (list (cons :role "user")
                               (cons :content
                                     (list (list (cons :type "text")
                                                 (cons :text "hello")))))))
         (expected-system (list (cons :role "system")
                                (cons :content "boot prompt")))
         (expected-user (list (cons :role "user")
                              (cons :content "hello"))))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"ok\"}}]}"
                                      200))
      (with-function-override (clawmacs::read-provider-token (provider)
                                (declare (ignore provider))
                                "openai-token")
        (with-function-override (clawmacs::build-system-prompt ()
                                  "boot prompt")
          (clawmacs::openai-codex-request messages :model "gpt-5.3-codex"))))
    (let* ((body (clawmacs::api-json-decode captured-request-body))
           (sent-messages (coerce (cdr (assoc :messages body)) 'list)))
      (is (equal expected-system (first sent-messages)))
      (is (equal expected-user (second sent-messages))))))

(test anthropic-streaming-normalizes-response-shape
  "Anthropic streaming adapter accumulates canonical content blocks."
  (let ((payloads '("data: {\"type\":\"content_block_start\",\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
                    ""
                    "data: {\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"hi\"}}"
                    ""
                    "data: {\"type\":\"content_block_stop\"}"
                    ""
                    "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"}}"
                    ""
                    "data: {\"type\":\"message_stop\"}"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (clawmacs::read-token ()
                                "anthropic-token")
        (let ((state (clawmacs::anthropic-request-streaming '() (lambda (state) (declare (ignore state)))
                                                           :model "claude-stream")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01))
          (is (string= "end_turn"
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-stop-reason state))))
          (is (equal '(((:type . "text")
                        (:text . "hi")))
                     (bt:with-lock-held ((clawmacs::stream-state-lock state))
                       (reverse (clawmacs::stream-state-content-blocks state))))))))))

(test openai-codex-streaming-normalizes-response-shape
  "OpenAI Codex streaming adapter accumulates canonical content blocks."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"content\":\"hi from \"}}]}"
                    ""
                    "data: {\"choices\":[{\"delta\":{\"content\":\"codex\"}}]}"
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
                                "openai-token")
        (let ((state (clawmacs::openai-codex-request-streaming '() (lambda (state) (declare (ignore state)))
                                                              :model "gpt-5.3-codex")))
          (loop repeat 100
                until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                        (clawmacs::stream-state-done-p state))
                do (sleep 0.01))
          (is (string= "end_turn"
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-stop-reason state))))
          (is (equal '(((:type . "text")
                        (:text . "hi from codex")))
                     (bt:with-lock-held ((clawmacs::stream-state-lock state))
                       (reverse (clawmacs::stream-state-content-blocks state))))))))))

(test openai-codex-streaming-includes-system-prompt-message
  "OpenAI Codex streaming requests prepend the built system prompt."
  (let* ((captured-request-body nil)
         (messages (list (list (cons :role "user")
                               (cons :content
                                     (list (list (cons :type "text")
                                                 (cons :text "hello")))))))
         (expected-system (list (cons :role "system")
                                (cons :content "boot prompt")))
         (expected-user (list (cons :role "user")
                              (cons :content "hello")))
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
                                "openai-token")
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
           (sent-messages (coerce (cdr (assoc :messages body)) 'list)))
      (is (equal expected-system (first sent-messages)))
      (is (equal expected-user (second sent-messages)))))))

(test openai-codex-streaming-supports-multiple-tool-calls
  "OpenAI Codex streaming keeps two streamed tool calls separate and canonical."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/one.txt\\\"}\"}},{\"index\":1,\"id\":\"call_2\",\"type\":\"function\",\"function\":{\"name\":\"write_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/two.txt\\\",\\\"content\\\":\\\"hello\\\"}\"}}]}}]}"
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
                                "openai-token")
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
