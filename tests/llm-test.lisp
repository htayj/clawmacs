(in-package :clawmacs/tests)

(in-suite llm-suite)

(defun temp-test-token-path (provider)
  (let* ((base (make-pathname :directory (list :absolute "tmp"
                                               (format nil "clawmacs-llm-tests-~A"
                                                       (gensym)))))
         (filename (ecase provider
                     (:anthropic "claude-max-token")
                     (:openai-codex "openai-codex-token")
                     (:zai "zai-api-key"))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames filename base)))

(defmacro with-provider-token-path-overrides ((anthropic-path openai-codex-path &optional zai-path) &body body)
  `(let ((original-provider-token-path
           (symbol-function 'clawmacs::provider-token-path)))
     (unwind-protect
          (progn
            (setf (symbol-function 'clawmacs::provider-token-path)
                  (lambda (provider)
                    (case provider
                      (:anthropic ,anthropic-path)
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
                                                      (gensym))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames "agent-defaults.json" base)))

(defmacro with-agent-defaults-path-override ((path) &body body)
  `(let ((clawmacs::*agent-defaults-path* ,path)
         (clawmacs::*agent-defaults-registry* nil))
     ,@body))

(defun temp-codex-auth-path ()
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "clawmacs-codex-auth-~A"
                                                      (gensym))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames "auth.json" base)))

(defmacro with-codex-auth-path-override ((path) &body body)
  `(let ((clawmacs::*codex-auth-path* ,path)
         (clawmacs::*openai-codex-oauth-path* ,path))
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

(defmacro with-tool-table-restored (&body body)
  `(let ((snapshot (make-hash-table :test (hash-table-test clawmacs::*tool-table*))))
     (maphash (lambda (key value)
                (setf (gethash key snapshot) value))
              clawmacs::*tool-table*)
     (unwind-protect
          (progn
            ,@body)
       (clrhash clawmacs::*tool-table*)
       (maphash (lambda (key value)
                  (setf (gethash key clawmacs::*tool-table*) value))
                snapshot))))

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
    (is (equal (merge-pathnames #P".config/clawmacs/claude-max-token" home)
               (clawmacs::provider-token-path :anthropic)))
    (is (equal (merge-pathnames #P".config/clawmacs/openai-codex-token" home)
               (clawmacs::provider-token-path :openai-codex)))))

(test provider-token-path-unknown-provider
  "Unknown providers signal a clear error."
  (signals error
    (clawmacs::provider-token-path :unknown-provider)))

(test init-tools-registers-only-lisp-eval-by-default
  "init-tools removes dormant built-ins and exposes only lisp_eval by default."
  (with-tool-table-restored
    (clrhash clawmacs::*tool-table*)
    (clawmacs::init-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
           (tool-names (mapcar (lambda (tool) (cdr (assoc :name tool))) tools)))
      (is (equal '("lisp_eval") tool-names))
      (is (not (null (gethash "lisp_eval" clawmacs::*tool-table*))))
      (is (null (gethash "http_fetch" clawmacs::*tool-table*)))
      (is (null (gethash "file_read" clawmacs::*tool-table*)))
      (is (null (gethash "file_write" clawmacs::*tool-table*)))
      (is (null (gethash "file_edit" clawmacs::*tool-table*)))
      (is (null (gethash "shell_exec" clawmacs::*tool-table*)))
      (is-false (clawmacs::tool-requires-permission-p "lisp_eval")))))

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
    (clawmacs::init-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (clawmacs::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool) (cdr (assoc :name tool))) tools)
                             #'string<)))
      (is (equal '("custom_probe" "lisp_eval") tool-names))
      (is (not (null (gethash "custom_probe" clawmacs::*tool-table*))))
      (is (not (null (gethash "lisp_eval" clawmacs::*tool-table*)))))))

(test build-system-prompt-emphasizes-lisp-eval-workflow
  "The default system prompt teaches REPL-first search, docs, and calling guidance."
  (with-function-override (clawmacs::load-boot-files ()
                            nil)
    (let ((prompt (clawmacs::build-system-prompt)))
      (is (search "only built-in tool available by default is `lisp_eval`" prompt))
      (is (search "Do not merely describe searches, inspections, calls, or updates" prompt))
      (is (search "(apropos-list \"SUBSTRING\" :clawmacs)" prompt))
      (is (search "(multiple-value-list (find-symbol \"NAME\" :clawmacs))" prompt))
      (is (search "cl-community-spec" prompt))
      (is (search "(describe-common-lisp-symbol-to-string 'SYMBOL)" prompt))
      (is (search "(describe-system-to-string \"SYSTEM\")" prompt))
      (is (search "(search-system-docs \"SYSTEM\" \"QUERY\")" prompt))
      (is (search "(describe-function-to-string 'SYMBOL)" prompt))
      (is (search "(documentation 'SYMBOL 'function)" prompt))
      (is (search "Use `funcall` or `apply` when the callee or argument list is dynamic." prompt))
      (is (search "Prefer `(format nil ...)` over" prompt))
      (is-false (search "fetching URLs, reading/writing files, running shell commands" prompt))
      (is-false (search "http_fetch" prompt))
      (is-false (search "shell_exec" prompt))
      (is-false (search "file_read" prompt)))))

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

(test anthropic-env-var-takes-highest-priority
  "CLAUDE_CODE_OAUTH_TOKEN env var takes priority over Claude Code credentials and file."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex))
        (creds-path (merge-pathnames
                     (format nil "clawmacs-creds-env-~A/credentials.json" (gensym))
                     #P"/tmp/")))
    (ensure-directories-exist creds-path)
    (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      ;; Set up all three sources
      (clawmacs::save-provider-token :anthropic "file-token")
      (with-open-file (s creds-path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (write-string "{\"claudeAiOauth\":{\"accessToken\":\"claude-code-token\"}}" s))
      (let ((clawmacs::*claude-code-credentials-path* creds-path))
        ;; Env var should win over both
        (with-env-var ("CLAUDE_CODE_OAUTH_TOKEN" "env-var-token")
          (is (string= "env-var-token"
                       (clawmacs::read-provider-token :anthropic))))))))

(test anthropic-env-var-falls-through-when-unset
  "When CLAUDE_CODE_OAUTH_TOKEN is unset, falls through to Claude Code credentials."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex))
        (creds-path (merge-pathnames
                     (format nil "clawmacs-creds-env2-~A/credentials.json" (gensym))
                     #P"/tmp/")))
    (ensure-directories-exist creds-path)
    (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      (clawmacs::save-provider-token :anthropic "file-token")
      (with-open-file (s creds-path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (write-string "{\"claudeAiOauth\":{\"accessToken\":\"claude-code-token\"}}" s))
      (let ((clawmacs::*claude-code-credentials-path* creds-path)
            (clawmacs::*anthropic-env-var* "CLAWMACS_UNSET_ANTHROPIC_ENV_98765"))
        ;; With env var unset, should use Claude Code credentials
        (is (string= "claude-code-token"
                     (clawmacs::read-provider-token :anthropic)))))))

(test zai-env-var-takes-highest-priority
  "ZAI_CODING_MAX_API_KEY env var takes priority over the static token file."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex))
        (zai-path (temp-test-token-path :zai)))
    (with-provider-token-path-overrides (anthropic-path openai-codex-path zai-path)
      ;; Set up file-based token
      (clawmacs::save-provider-token :zai "file-zai-key")
      ;; Env var should win
      (with-env-var ("ZAI_CODING_MAX_API_KEY" "env-zai-key")
        (is (string= "env-zai-key"
                     (clawmacs::read-provider-token :zai)))))))

(test zai-env-var-falls-through-to-file
  "When ZAI_CODING_MAX_API_KEY is unset, falls through to static token file."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex))
        (zai-path (temp-test-token-path :zai)))
    (with-provider-token-path-overrides (anthropic-path openai-codex-path zai-path)
      (clawmacs::save-provider-token :zai "file-zai-key")
      (let ((clawmacs::*zai-env-var* "CLAWMACS_UNSET_ZAI_ENV_98765"))
        ;; With env var unset, should use file
        (is (string= "file-zai-key"
                     (clawmacs::read-provider-token :zai)))))))

(test env-var-does-not-affect-openai-codex
  "OpenAI Codex provider is not affected by Anthropic/Z.AI env vars."
  (let ((anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      (clawmacs::save-provider-token :openai-codex "codex-file-token")
      ;; Setting Anthropic env var shouldn't affect OpenAI Codex
      (with-env-var ("CLAUDE_CODE_OAUTH_TOKEN" "env-anthropic-token")
        (is (string= "codex-file-token"
                     (clawmacs::read-provider-token :openai-codex)))))))

(test default-env-var-names-are-correct
  "Default environment variable names are as documented."
  (is (string= "CLAUDE_CODE_OAUTH_TOKEN" clawmacs::*anthropic-env-var*))
  (is (string= "ZAI_CODING_MAX_API_KEY" clawmacs::*zai-env-var*)))

;;; --------------------------------------------------------------------------

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
    (set-buffer-think-level-override buf "high")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (clawmacs::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))
        (is (string= "high" think-level))))))

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
      (set-buffer-provider-override buf :anthropic)
      (set-buffer-model-override buf "claude-override")
      (set-buffer-think-level-override buf "high")
      (clear-buffer-provider/model-overrides buf)
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
  "OpenAI Codex requests use the Codex adapter and preserve model + reasoning."
  (let ((captured-model nil)
        (captured-reasoning nil))
    (with-function-override (clawmacs::openai-codex-request
                             (messages &key model max-tokens tools reasoning-effort)
                             (declare (ignore messages max-tokens tools))
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
  (let ((anthropic-model nil)
        (openai-model nil)
        (openai-reasoning nil))
    (with-function-override (clawmacs::anthropic-request-streaming (messages callback &key model max-tokens tools)
                              (declare (ignore messages callback max-tokens tools))
                              (setf anthropic-model model)
                              :anthropic-stream)
      (with-function-override (clawmacs::openai-codex-request-streaming
                                (messages callback &key model max-tokens tools reasoning-effort)
                                (declare (ignore messages callback max-tokens tools))
                                (setf openai-model model
                                      openai-reasoning reasoning-effort)
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
                 :model "codex-stream"
                 :reasoning-effort "medium")))
        (is (string= "claude-stream" anthropic-model))
        (is (string= "codex-stream" openai-model))
        (is (string= "medium" openai-reasoning))))))

(test start-streaming-response-uses-resolved-provider-and-model
  "Live streaming resolves provider/model/think first and passes them to the adapter."
  (let ((buf (make-buffer "routing-test" :agent-name "spark"))
        (captured-provider nil)
        (captured-model nil)
        (captured-reasoning nil))
    (with-function-override (clawmacs::resolve-buffer-provider-and-model (buffer)
                              (declare (ignore buffer))
                              (values :openai-codex "gpt-5.3-codex" "high"))
      (with-function-override (clawmacs::tool-definitions-for-api ()
                                #())
        (with-function-override (clawmacs::build-conversation-messages (buffer)
                                  (declare (ignore buffer))
                                  '(((:role . "user") (:content . #()))))
          (with-function-override (clawmacs::provider-request-streaming
                                    (provider messages callback &key model max-tokens tools reasoning-effort)
                                    (declare (ignore messages callback max-tokens tools))
                                    (setf captured-provider provider
                                          captured-model model
                                          captured-reasoning reasoning-effort)
                                    (clawmacs::make-stream-state))
            (clawmacs::start-streaming-response buf)
            (is (eq :openai-codex captured-provider))
            (is (string= "gpt-5.3-codex" captured-model))
            (is (string= "high" captured-reasoning))))))))

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
                               "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"hi from codex\"}]},{\"type\":\"function_call\",\"call_id\":\"call_1\",\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/codex.txt\\\"}\"}]}"
                               200))
      (let ((response (clawmacs::openai-codex-request '() :model "gpt-5.3-codex")))
        (is (string= "tool_use" (clawmacs::response-stop-reason response)))
        (is (equal '(((:type . "text")
                      (:text . "hi from codex"))
                     ((:type . "tool_use")
                      (:id . "call_1")
                      (:name . "read_file")
                      (:input . ((:path . "/tmp/codex.txt")))))
                   (clawmacs::response-content response)))))))

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
      (is (null (assoc :reasoning body)))
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
      (is (string= "xhigh" effort)))))

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
      (is (null (assoc :reasoning body)))
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
      (is (string= "high" (cdr (assoc :effort reasoning)))))))

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
        (anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "oauth-token"))
      (with-provider-token-path-overrides (anthropic-path openai-codex-path)
      (clawmacs::save-provider-token :openai-codex "static-token")
        (is (string= "static-token"
                     (clawmacs::read-provider-token :openai-codex)))))))

(test read-provider-token-ignores-url-like-openai-codex-override
  "A URL-like OpenAI Codex override token is ignored in favor of shared auth.json."
  (let ((path (temp-codex-auth-path))
        (anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                              :account-id "acct_456"))
      (with-provider-token-path-overrides (anthropic-path openai-codex-path)
        (clawmacs::save-provider-token
         :openai-codex
         "http://localhost:1455/auth/callback?code=abc&state=xyz")
        (is (string= "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                     (clawmacs::read-provider-token :openai-codex)))))))

(test read-provider-token-falls-back-to-codex-auth-json-for-openai-codex
  "OpenAI Codex falls back to shared auth.json when no override token exists."
  (let ((path (temp-codex-auth-path))
        (anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "oauth-token"))
      (with-provider-token-path-overrides (anthropic-path openai-codex-path)
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
        (anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-api-key-auth-payload
                              :api-key "sk-selector"))
      (with-provider-token-path-overrides (anthropic-path openai-codex-path)
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
        (anthropic-path (temp-test-token-path :anthropic))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (anthropic-path openai-codex-path zai-path)
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
    (with-function-override (clawmacs::zai-request (messages &key model max-tokens tools)
                              (declare (ignore messages model max-tokens tools))
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
      (let ((response (clawmacs::zai-request '() :model "glm-5")))
        (is (string= "Hello"
                     (cdr (assoc :text (first (clawmacs::response-content response))))))))))

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
  "Streaming: reasoning_content chunks accumulate, then content chunks append."
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
          ;; Should have accumulated both reasoning and content
          (is (string= "Thinking... more thoughtsHello"
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-text state)))))))))

(test reasoning-content-streaming-reasoning-only
  "Streaming: when only reasoning_content chunks arrive (no content), still works."
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
          ;; Should have accumulated the reasoning text
          (is (string= "Step 1: analyze... Step 2: decide..."
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-text state))))
          (is (string= "max_tokens"
                       (bt:with-lock-held ((clawmacs::stream-state-lock state))
                         (clawmacs::stream-state-stop-reason state)))))))))

;;; --------------------------------------------------------------------------
;;; Known Models Tests
;;; --------------------------------------------------------------------------

(test provider-known-models-anthropic
  "Known Anthropic models list is non-empty and contains the default."
  (let ((models (clawmacs::provider-known-models :anthropic)))
    (is (listp models))
    (is (plusp (length models)))
    ;; Default fallback model should be in the list
    (is (member "claude-haiku-4-5-20251001" models :test #'string=))))

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
               :anthropic "claude-haiku-4-5-20251001")))))

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
      (clawmacs::set-agent-default "claude" :anthropic :model "claude-haiku-4-5-20251001")
      (let ((buf (make-buffer "test" :agent-name "claude")))
        ;; Mock provider-has-token-p to only return t for :anthropic
        (with-function-override (clawmacs::provider-has-token-p (provider)
                                  (eq provider :anthropic))
          (let ((entries (clawmacs::available-models-for-selector buf)))
            ;; Should have entries for anthropic only
            (is (plusp (length entries)))
            (is (every (lambda (e) (eq :anthropic (getf e :provider))) entries))
            ;; Exactly one entry should be active
            (let ((active-count (count-if (lambda (e) (getf e :active-p)) entries)))
              (is (= 1 active-count)))
            ;; The active entry should be the default model
            (let ((active (find-if (lambda (e) (getf e :active-p)) entries)))
              (is (string= "claude-haiku-4-5-20251001" (getf active :model))))))))))

(test available-models-for-selector-multi-provider
  "available-models-for-selector includes models from multiple providers."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (clawmacs::set-agent-default "claude" :zai :model "glm-5")
      (let ((buf (make-buffer "test" :agent-name "claude")))
        ;; Mock: both anthropic and zai have tokens
        (with-function-override (clawmacs::provider-has-token-p (provider)
                                  (not (null (member provider '(:anthropic :zai)))))
          (let ((entries (clawmacs::available-models-for-selector buf)))
            ;; Should have entries from both providers
            (is (plusp (length entries)))
            (let ((providers (remove-duplicates
                              (mapcar (lambda (e) (getf e :provider)) entries))))
              (is (member :anthropic providers))
              (is (member :zai providers)))))))))

(test available-models-for-selector-no-tokens
  "available-models-for-selector returns nil when no provider has a token."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (let ((buf (make-buffer "test" :agent-name "claude")))
        ;; Mock: no tokens available
        (with-function-override (clawmacs::provider-has-token-p (provider)
                                  (declare (ignore provider))
                                  nil)
          (let ((entries (clawmacs::available-models-for-selector buf)))
            (is (null entries))))))))

;;; --------------------------------------------------------------------------
;;; Claude CLI Subprocess Tests
;;; --------------------------------------------------------------------------

(test claude-cli-model-p-recognizes-cli-models
  "claude-cli-model-p returns non-nil for models in *claude-cli-models*."
  (let ((clawmacs::*claude-cli-path* "claude"))
    (is (clawmacs::claude-cli-model-p "claude-sonnet-4-6"))
    (is (clawmacs::claude-cli-model-p "claude-opus-4-6"))
    (is (clawmacs::claude-cli-model-p "claude-sonnet-4-5"))
    (is (clawmacs::claude-cli-model-p "claude-opus-4-5-20251101"))))

(test claude-cli-model-p-rejects-rest-models
  "claude-cli-model-p returns nil for models NOT in *claude-cli-models*."
  (is (null (clawmacs::claude-cli-model-p "claude-haiku-4-5-20251001")))
  (is (null (clawmacs::claude-cli-model-p "claude-3-haiku-20240307")))
  (is (null (clawmacs::claude-cli-model-p "glm-5")))
  (is (null (clawmacs::claude-cli-model-p "codex-mini-latest"))))

(test claude-cli-model-p-disabled-when-path-nil
  "claude-cli-model-p returns nil when *claude-cli-path* is nil."
  (let ((clawmacs::*claude-cli-path* nil))
    (is (null (clawmacs::claude-cli-model-p "claude-sonnet-4-6")))))

(test claude-cli-build-prompt-single-message
  "claude-cli-build-prompt flattens a single user message."
  (let ((msgs (list `((:role . "user") (:content . "Hello world")))))
    (is (string= "[user]: Hello world"
                  (clawmacs::claude-cli-build-prompt msgs)))))

(test claude-cli-build-prompt-multi-turn
  "claude-cli-build-prompt concatenates multiple messages."
  (let ((msgs (list `((:role . "user") (:content . "Hi"))
                    `((:role . "assistant") (:content . "Hello"))
                    `((:role . "user") (:content . "How are you?")))))
    (let ((result (clawmacs::claude-cli-build-prompt msgs)))
      (is (search "[user]: Hi" result))
      (is (search "[assistant]: Hello" result))
      (is (search "[user]: How are you?" result)))))

(test claude-cli-build-prompt-vector-content
  "claude-cli-build-prompt extracts text from vector content blocks."
  (let ((msgs (list `((:role . "assistant")
                      (:content . ,(vector `((:type . "text") (:text . "Hi there"))))))))
    (is (search "Hi there" (clawmacs::claude-cli-build-prompt msgs)))))

(test provider-request-routes-cli-models
  "provider-request routes CLI models to claude-cli-request."
  (let ((routed-to nil)
        (clawmacs::*claude-cli-path* "claude"))
    (with-function-override (clawmacs::claude-cli-request
                             (messages &key model max-tokens tools)
                             (declare (ignore messages max-tokens tools))
                             (setf routed-to model)
                             `((:type . "message")
                               (:content . ,(vector `((:type . "text")
                                                      (:text . "ok"))))))
      (clawmacs::provider-request :anthropic nil :model "claude-sonnet-4-6")
      (is (string= "claude-sonnet-4-6" routed-to)))))

(test provider-request-routes-rest-models
  "provider-request routes REST-compatible models to anthropic-request."
  (let ((routed-to nil))
    (with-function-override (clawmacs::anthropic-request
                             (messages &key model max-tokens tools)
                             (declare (ignore messages max-tokens tools))
                             (setf routed-to model)
                             `((:type . "message")
                               (:content . ,(vector `((:type . "text")
                                                      (:text . "ok"))))))
      (clawmacs::provider-request :anthropic nil :model "claude-haiku-4-5-20251001")
      (is (string= "claude-haiku-4-5-20251001" routed-to)))))

(test provider-request-streaming-routes-cli-models
  "provider-request-streaming routes CLI models to claude-cli-request-streaming."
  (let ((routed-to nil)
        (clawmacs::*claude-cli-path* "claude"))
    (with-function-override (clawmacs::claude-cli-request-streaming
                             (messages callback &key model max-tokens tools)
                             (declare (ignore messages callback max-tokens tools))
                             (setf routed-to model)
                             (clawmacs::make-stream-state))
      (clawmacs::provider-request-streaming :anthropic nil nil
                                            :model "claude-opus-4-6")
      (is (string= "claude-opus-4-6" routed-to)))))

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
                  (user-homedir-pathname))))
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
      (is (string= "openai/gpt-4o-mini" (first models))))))

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
                             (messages &key model max-tokens tools)
                             (declare (ignore messages max-tokens tools))
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
                             (messages callback &key model max-tokens tools)
                             (declare (ignore messages callback max-tokens tools))
                             (setf routed-to model)
                             (clawmacs::make-stream-state))
      (clawmacs::provider-request-streaming :openrouter nil nil
                                            :model "anthropic/claude-3-5-haiku")
      (is (string= "anthropic/claude-3-5-haiku" routed-to)))))
