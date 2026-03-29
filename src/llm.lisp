(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Configuration
;;; --------------------------------------------------------------------------

(defvar *default-provider* :zai
  "The default LLM provider to use when no agent-specific override is set.
Must be a keyword matching a known provider (:anthropic, :openai-codex, :zai, :openrouter).")

(defvar *default-model* "glm-5"
  "The default model to use when no agent-specific or provider-fallback model
is configured. Should be a valid model name for *default-provider*.")

(defvar *anthropic-model* "claude-haiku-4-5-20251001"
  "The Anthropic model to use for chat completions.")

(defvar *anthropic-api-url* "https://api.anthropic.com/v1/messages"
  "The Anthropic Messages API endpoint.")

(defvar *anthropic-version* "2023-06-01"
  "The Anthropic API version header value.")

(defvar *anthropic-beta* "claude-code-20250219,oauth-2025-04-20,interleaved-thinking-2025-05-14"
  "Beta features header for Claude Code OAuth authentication and extended thinking.")

(defvar *openai-codex-model* "codex-mini-latest"
  "The OpenAI Codex model to use for chat completions.")

(defvar *openai-codex-api-url* "https://api.openai.com/v1/chat/completions"
  "The OpenAI Chat Completions API endpoint for Codex models.")

;;; Z.AI (Zhipu AI) Configuration
(defvar *zai-model* "glm-5"
  "The Z.AI model to use for chat completions.
GLM-5 is the flagship model with 200K context window.")

(defvar *zai-api-url* "https://api.z.ai/api/coding/paas/v4/chat/completions"
  "The Z.AI Chat Completions API endpoint for GLM Coding plan subscribers.
Uses the coding-specific endpoint for subscription-based access.")

;;; OpenRouter Configuration
(defvar *openrouter-model* "openai/gpt-4o-mini"
  "The default OpenRouter model to use for chat completions.
Model names follow the 'provider/model-name' format (e.g. 'openai/gpt-4o-mini',
'anthropic/claude-3-5-haiku', 'google/gemini-2.5-pro').")

(defvar *openrouter-api-url* "https://openrouter.ai/api/v1/chat/completions"
  "The OpenRouter Chat Completions API endpoint.
OpenRouter normalizes to the OpenAI-compatible chat completions schema.")

(defvar *openrouter-models-url* "https://openrouter.ai/api/v1/models"
  "The OpenRouter models listing endpoint.
Returns the full catalog of available models.")

(defvar *openrouter-env-var* "OPENROUTER_API_KEY"
  "Environment variable name for the OpenRouter API key.
When set, this takes highest priority over the static token file.")

(defvar *openrouter-cached-models* nil
  "Cached list of OpenRouter model ID strings fetched from the API.
Populated on first call to fetch-openrouter-models.  Set to nil to force refresh.")

;;; OpenAI Codex OAuth 2.0 Configuration
(defvar *openai-oauth-client-id* "app_EMoamEEZ73f0CkXaXp7hrann"
  "OpenAI Codex OAuth 2.0 public client identifier.")

(defvar *openai-oauth-auth-url* "https://auth.openai.com/oauth/authorize"
  "OpenAI OAuth 2.0 authorization endpoint.")

(defvar *openai-oauth-token-url* "https://auth.openai.com/oauth/token"
  "OpenAI OAuth 2.0 token exchange endpoint.")

(defvar *openai-oauth-redirect-uri* "http://localhost:1455/callback"
  "OAuth redirect URI. No server needed; user pastes the callback URL.")

(defvar *openai-oauth-scopes* "openid profile email offline_access"
  "OAuth scopes to request from OpenAI.")

(defvar *openai-codex-oauth-path*
  (merge-pathnames #P".config/clawmacs/openai-codex-oauth.json" (user-homedir-pathname))
  "Path to the persisted OpenAI Codex OAuth credentials (JSON).")

;;; Claude Code OAuth Identity
;;; The Anthropic REST API requires OAuth tokens (sk-ant-oat-*) to identify
;;; as Claude Code via a specific system prompt prefix sent as the first
;;; content block in array format.  Without this, Sonnet/Opus return 400.
(defvar *claude-code-system-prefix*
  "You are Claude Code, Anthropic's official CLI for Claude."
  "System prompt prefix required by the Anthropic API for Claude Max OAuth
tokens to access Sonnet/Opus models.  Must be the first text block in an
array-format system prompt.")

;;; Claude CLI Subprocess Configuration (for Claude Max subscription models)
;;; Uses the stream-json subprocess protocol.  With the correct beta headers
;;; and system prompt prefix, OAuth tokens now work for all models via REST.
(defvar *claude-cli-path* nil
  "Path to the Claude Code CLI binary. Set to nil (default) to route all
Anthropic models through the REST API with OAuth tokens. Set to \"claude\"
to re-enable CLI subprocess delegation for models in *claude-cli-models*.")

(defvar *claude-cli-models*
  '("claude-sonnet-4-6" "claude-opus-4-6"
    "claude-sonnet-4-5" "claude-sonnet-4-5-20250929"
    "claude-opus-4-5" "claude-opus-4-5-20251101"
    "claude-sonnet-4-20250514" "claude-opus-4-20250514")
  "Anthropic models that require the Claude CLI subprocess because the
REST API rejects OAuth tokens for them (returns 400 invalid_request_error).
These models are routed through the Claude Code CLI stream-json protocol.")

(defun claude-cli-model-p (model)
  "Return non-nil when MODEL must use the Claude CLI subprocess."
  (and *claude-cli-path*
       (member model *claude-cli-models* :test #'string=)))

(defvar *system-prompt-path*
  (merge-pathnames #P".config/clawmacs/system-prompt.txt" (user-homedir-pathname))
  "Path to an optional system prompt file.")

(defvar *agent-defaults-path*
  (merge-pathnames #P".config/clawmacs/agent-defaults.json" (user-homedir-pathname))
  "Path to the persisted agent defaults registry.")

(defvar *agent-defaults-registry* nil
  "Memoized agent defaults registry.")

(defvar *system-prompt*
  "You are a helpful assistant running inside clawmacs, a Lisp-native terminal chat interface.
You have access to tools for fetching URLs, reading/writing files, running shell commands,
and evaluating Common Lisp code. Be concise and direct in your responses.

## Introspection

You can discover and inspect all functions, variables, and types available in the clawmacs
system by using the lisp_eval tool. This is useful for understanding the editor's capabilities,
checking configuration, and modifying behavior at runtime.

### Discovering symbols

- `(list-functions)` — returns a sorted list of all exported function symbols.
- `(list-variables)` — returns a sorted list of all exported variable symbols.
- `(list-types)` — returns a sorted list of all exported type symbols (classes, structs, conditions).
- `(undocumented-functions)` — returns functions missing extended documentation.
- `(undocumented-variables)` — returns variables missing extended documentation.
- `(undocumented-types)` — returns types missing extended documentation.

### Inspecting symbols

- `(describe-function-to-string 'SYMBOL)` — returns a human-readable description of a function,
  including its type, arguments, keybindings, documentation, usage, return values, side effects,
  and related symbols.
- `(describe-variable-to-string 'SYMBOL)` — returns a human-readable description of a variable,
  including its kind (constant, special, variable), current value, type, and documentation.
- `(describe-type-to-string 'SYMBOL)` — returns a human-readable description of a type,
  including its kind (class, struct, condition), slots/fields, inheritance, and documentation.
- `(extended-doc 'SYMBOL :PROPERTY)` — returns a specific documentation property for a symbol.
  Properties: :category, :usage, :returns, :see-also, :side-effects.

### Getting variable values

- `(symbol-value 'VARIABLE)` or simply reference the variable name, e.g. `*default-model*`.
- Example: `(lisp_eval :code \"*default-model*\")` returns the current default model string.
- Example: `(lisp_eval :code \"*default-provider*\")` returns the current default provider keyword.

### Setting variable values

- `(setf VARIABLE VALUE)` — set a variable's value.
- Example: `(setf *default-model* \"claude-sonnet-4-6\")` changes the default model.
- Example: `(setf *default-provider* :anthropic)` changes the default provider.
- For special (dynamic) variables prefixed with `*earmuffs*`, use setf as shown above.
- For constants (defined with defconstant), values cannot be changed at runtime.

### Defining new functions and variables

- `(defun NAME (ARGS) \"docstring\" BODY)` — define a new function.
- `(defvar NAME VALUE \"docstring\")` — define a new special variable (only sets if unbound).
- `(defparameter NAME VALUE \"docstring\")` — define a new special variable (always sets).
- `(setf (gethash KEY *tool-table*) ...)` — register new tools.
- New definitions persist for the lifetime of the clawmacs process.

### Searching for symbols

- `(apropos \"SUBSTRING\")` — find all symbols containing a substring.
- `(apropos-list \"SUBSTRING\" :clawmacs)` — find symbols in the clawmacs package.
- Example: `(apropos-list \"buffer\" :clawmacs)` finds all buffer-related symbols.
- Example: `(apropos-list \"model\" :clawmacs)` finds all model-related symbols.

### Common configuration variables

- `*default-model*` — the fallback model name (string).
- `*default-provider*` — the fallback provider keyword (:anthropic, :zai, :openai, etc.).
- `*system-prompt*` — this system prompt (can be modified at runtime).
- `*sandbox-root*` — the root directory for file operations.
- `*prefix-handlers*` — alist of chat input prefix handlers (e.g., \"!\" for shell commands)."
  "The system prompt sent with every API request.
Built from boot MD files and/or *system-prompt-path* on startup.")

(defvar *boot-file-names*
  '("AGENTS.md" "SOUL.md" "USER.md" "IDENTITY.md" "TOOLS.md")
  "Boot markdown files to load, in order. Checked in the working directory
and ~/.config/clawmacs/. Compatible with OpenClaw workspace conventions.")

(defun load-boot-files ()
  "Load boot MD files from the working directory and ~/.config/clawmacs/.
Returns a concatenated string, or nil if no files found.
Files are loaded in the order specified by *boot-file-names*.
Project-local files take precedence over global ones."
  (let ((parts nil)
        (global-dir (merge-pathnames #P".config/clawmacs/" (user-homedir-pathname)))
        (local-dir (truename ".")))
    (dolist (name *boot-file-names*)
      (let ((local-path (merge-pathnames name local-dir))
            (global-path (merge-pathnames name global-dir)))
        ;; Project-local takes precedence
        (cond
          ((probe-file local-path)
           (push (uiop:read-file-string local-path) parts))
          ((probe-file global-path)
           (push (uiop:read-file-string global-path) parts)))))
    (when parts
      (format nil "~{~A~^~%~%---~%~%~}" (nreverse parts)))))

(defun build-system-prompt ()
  "Build the full system prompt from boot files + default/custom prompt.
Boot file content is prepended to the system prompt."
  (let ((boot-content (load-boot-files))
        (base-prompt *system-prompt*))
    (if boot-content
        (format nil "~A~%~%---~%~%~A" boot-content base-prompt)
        base-prompt)))

;;; --------------------------------------------------------------------------
;;; JSON Helpers (underscore-preserving round-trip)
;;; --------------------------------------------------------------------------

(defun json-name-to-lisp (name)
  "Convert a JSON key name to a Lisp identifier string, preserving underscores
as double-dashes so they round-trip correctly through cl-json encoding.
E.g., \"tool_use\" -> \"TOOL--USE\" -> (interned as :TOOL--USE) -> \"tool_use\"."
  (string-upcase
   (with-output-to-string (s)
     (loop :for c :across name
           :do (if (char= c #\_)
                   (write-string "--" s)
                   (write-char c s))))))

(defun api-json-decode (string)
  "Decode a JSON string using underscore-preserving key mapping."
  (let ((cl-json:*json-identifier-name-to-lisp* #'json-name-to-lisp))
    (cl-json:decode-json-from-string string)))

(defun api-json-encode (object)
  "Encode an object to JSON using cl-json's default encoding.
Keys with double-dashes encode as underscores (e.g., :TOOL--USE -> tool_use)."
  (cl-json:encode-json-to-string object))

(declaim (ftype (function (t) (or null list)) normalize-legacy-raw-content))

(defun canonical-text-block (text)
  "Return TEXT as a canonical text block." 
  `((:type . "text")
    (:text . ,(or text ""))))

(defun canonicalize-content-block (role block)
  "Normalize BLOCK for ROLE and enforce valid role/block pairings."
  (let ((block-type (cdr (assoc :type block))))
    (cond
      ((string= "text" (or block-type ""))
       (unless (member role '("user" "assistant") :test #'string=)
         (error "text blocks are only allowed on user or assistant messages"))
       (canonical-text-block (cdr (assoc :text block))))
      ((string= "tool_use" (or block-type ""))
       (unless (string= role "assistant")
         (error "tool_use blocks are only allowed on assistant messages"))
       `((:type . "tool_use")
         (:id . ,(cdr (assoc :id block)))
         (:name . ,(cdr (assoc :name block)))
         (:input . ,(cdr (assoc :input block)))))
      ((string= "tool_result" (or block-type ""))
       (unless (string= role "user")
         (error "tool_result blocks are only allowed on user messages"))
       (let ((tool-use-id (or (cdr (assoc :tool--use--id block))
                              (cdr (assoc :tool-use-id block)))))
         (unless tool-use-id
           (error "tool_result blocks require tool_use_id"))
        `((:type . "tool_result")
          (:tool--use--id . ,tool-use-id)
          (:content . ,(cdr (assoc :content block))))))
      (t
       (error "Unsupported content block type ~S" block-type)))))

(defun canonicalize-message-content (role content)
  "Normalize CONTENT into canonical content blocks for ROLE."
  (cond
    ((or (null content) (stringp content))
     (list (canonicalize-content-block role (canonical-text-block content))))
    ((vectorp content)
     (loop :for block :across content
           :collect (canonicalize-content-block role block)))
    ((listp content)
     (loop :for block :in content
           :collect (canonicalize-content-block role block)))
    (t
     (error "Unsupported message content ~S" content))))

(defun normalize-legacy-raw-content (raw-content)
  "Normalize persisted legacy session raw-content blocks to canonical blocks."
  (when raw-content
    (loop :for block :in (coerce raw-content 'list)
          :collect
          (let ((block-type (cdr (assoc :type block))))
            (cond
              ((string= "text" (or block-type ""))
               `((:type . "text")
                 (:text . ,(cdr (assoc :text block)))))
              ((string= "tool_use" (or block-type ""))
               `((:type . "tool_use")
                 (:id . ,(cdr (assoc :id block)))
                 (:name . ,(cdr (assoc :name block)))
                 (:input . ,(cdr (assoc :input block)))))
              ((string= "tool_result" (or block-type ""))
                `((:type . "tool_result")
                  (:tool--use--id . ,(or (cdr (assoc :tool--use--id block))
                                         (cdr (assoc :tool-use-id block))))
                  (:content . ,(cdr (assoc :content block)))))
              (t block))))))

;;; --------------------------------------------------------------------------
;;; Token Management
;;; --------------------------------------------------------------------------

(defun read-token ()
  "Read the Anthropic OAuth token from its provider-specific file."
  (read-provider-token :anthropic))

(defun save-token (token)
  "Save TOKEN to the Anthropic provider-specific token file."
  (save-provider-token :anthropic token))

(defun provider-token-path (provider)
  "Return the provider-specific token file path for PROVIDER."
  (merge-pathnames
   (case provider
     (:anthropic #P".config/clawmacs/claude-max-token")
     (:openai-codex #P".config/clawmacs/openai-codex-token")
     (:zai #P".config/clawmacs/zai-api-key")
     (:openrouter #P".config/clawmacs/openrouter-api-key")
     (otherwise
      (error "Unknown provider ~S. Supported providers: :ANTHROPIC, :OPENAI-CODEX, :ZAI, :OPENROUTER"
             provider)))
   (user-homedir-pathname)))

(defvar *claude-code-credentials-path*
  (merge-pathnames #P".claude/.credentials.json" (user-homedir-pathname))
  "Path to Claude Code's OAuth credentials file.")

(defvar *anthropic-env-var* "CLAUDE_CODE_OAUTH_TOKEN"
  "Environment variable name for the Anthropic Claude Max OAuth token.
When set, this takes highest priority over all other Anthropic token sources.")

(defvar *zai-env-var* "ZAI_CODING_MAX_API_KEY"
  "Environment variable name for the Z.AI Coding Max API key.
When set, this takes highest priority over the static token file.")

(defun read-env-token (env-var)
  "Read a token from the environment variable named ENV-VAR.
Returns the trimmed token string if the variable is set and non-empty, nil otherwise."
  (let ((value (uiop:getenv env-var)))
    (when (and value (stringp value))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
        (when (plusp (length trimmed))
          trimmed)))))

(defun read-claude-code-oauth-token ()
  "Read the Anthropic OAuth access token from Claude Code's credentials file.
Returns the access token string if the file exists and contains a valid
claudeAiOauth entry, otherwise nil."
  (when (probe-file *claude-code-credentials-path*)
    (handler-case
        (let* ((json-str (uiop:read-file-string *claude-code-credentials-path*))
               (creds (cl-json:decode-json-from-string json-str))
               (oauth (cdr (assoc :claude-ai-oauth creds)))
               (token (cdr (assoc :access-token oauth))))
          (when (and token (stringp token) (plusp (length token)))
            token))
      (error () nil))))

(defun read-provider-token (provider)
  "Read PROVIDER's token with the following priority per provider:

  :ANTHROPIC     1) CLAUDE_CODE_OAUTH_TOKEN env var
                 2) Claude Code credentials file (~/.claude/.credentials.json)
                 3) Static token file (~/.config/clawmacs/claude-max-token)

  :OPENAI-CODEX  1) OpenAI Codex OAuth credentials (auto-refreshing)
                 2) Static token file (~/.config/clawmacs/openai-codex-token)

  :ZAI           1) ZAI_CODING_MAX_API_KEY env var
                 2) Static token file (~/.config/clawmacs/zai-api-key)

  :OPENROUTER    1) OPENROUTER_API_KEY env var
                 2) Static token file (~/.config/clawmacs/openrouter-api-key)"
  (or ;; Environment variable sources (highest priority)
      (when (eq provider :anthropic)
        (read-env-token *anthropic-env-var*))
      (when (eq provider :zai)
        (read-env-token *zai-env-var*))
      (when (eq provider :openrouter)
        (read-env-token *openrouter-env-var*))
      ;; OAuth / credentials file sources
      (when (eq provider :anthropic)
        (read-claude-code-oauth-token))
      (when (eq provider :openai-codex)
        (read-openai-codex-oauth-token))
      ;; Static token file (lowest priority)
      (let ((token-path (provider-token-path provider)))
        (when (probe-file token-path)
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-string token-path))))))

(defun save-provider-token (provider token)
  "Save TOKEN to PROVIDER's provider-specific token file."
  (let ((token-path (provider-token-path provider)))
    (ensure-directories-exist token-path)
    (with-open-file (s token-path
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
      (write-string token s))
    (ignore-errors
      (uiop:run-program (list "chmod" "600" (namestring token-path))))
    token))

;;; --------------------------------------------------------------------------
;;; OpenAI Codex OAuth 2.0 (PKCE Flow)
;;; --------------------------------------------------------------------------

(defun generate-random-string (length)
  "Generate a random string of LENGTH alphanumeric characters.
Uses /dev/urandom for cryptographic randomness."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
        (num-chars 62))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (with-output-to-string (s)
        (loop :repeat length
              :for byte := (read-byte urandom)
              :do (write-char (char chars (mod byte num-chars)) s))))))

(defun generate-code-verifier ()
  "Generate a 43-character PKCE code verifier (RFC 7636)."
  (generate-random-string 43))

(defun generate-oauth-state ()
  "Generate a 32-character random state for CSRF protection."
  (generate-random-string 32))

(defun hex-string-to-bytes (hex-string)
  "Convert a HEX-STRING to a byte array."
  (let* ((len (/ (length hex-string) 2))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (loop :for i :from 0 :below (length hex-string) :by 2
          :for j :from 0
          :do (setf (aref bytes j)
                    (parse-integer hex-string :start i :end (+ i 2) :radix 16)))
    bytes))

(defun compute-code-challenge (code-verifier)
  "Compute S256 PKCE code challenge from CODE-VERIFIER (RFC 7636 Section 4.2).
Uses openssl for SHA-256 and base64, then converts to base64url encoding."
  (string-trim
   '(#\Newline #\Space #\Return)
   (with-output-to-string (out)
     (uiop:run-program
      (format nil "printf '%s' '~A' | openssl dgst -sha256 -binary | openssl base64 -A | tr '+/' '-_' | tr -d '='"
              code-verifier)
      :output out))))

(defun url-encode-param (string)
  "Percent-encode STRING for use as a URL query parameter value."
  (with-output-to-string (s)
    (loop :for char :across string
          :do (cond
                ((or (alphanumericp char)
                     (find char "-_.~"))
                 (write-char char s))
                ((< (char-code char) 128)
                 (format s "%~2,'0X" (char-code char)))
                (t
                 ;; Multi-byte: encode each UTF-8 byte
                 (loop :for byte :across (flexi-streams:string-to-octets
                                          (string char) :external-format :utf-8)
                       :do (format s "%~2,'0X" byte)))))))

(defun split-string-by-char (string delimiter)
  "Split STRING into substrings at each occurrence of DELIMITER character."
  (loop :for start := 0 :then (1+ end)
        :for end := (position delimiter string :start start)
        :collect (subseq string start (or end (length string)))
        :while end))

(defun parse-query-string (query)
  "Parse a URL QUERY string into an alist of (key . value) string pairs."
  (loop :for part :in (split-string-by-char query #\&)
        :for eq-pos := (position #\= part)
        :when eq-pos
          :collect (cons (subseq part 0 eq-pos)
                         (subseq part (1+ eq-pos)))))

(defun extract-oauth-callback-params (url)
  "Extract authorization code and state from an OAuth callback URL.
Returns (values code state). Signals an error if the code is missing."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) url))
         (query-start (position #\? trimmed)))
    (unless query-start
      (error "Invalid callback URL (no query parameters).~%Expected: ~A?code=...&state=..."
             *openai-oauth-redirect-uri*))
    (let* ((query (subseq trimmed (1+ query-start)))
           (params (parse-query-string query))
           (code (cdr (assoc "code" params :test #'string=)))
           (state (cdr (assoc "state" params :test #'string=))))
      (unless code
        (error "No authorization code in callback URL. Check that you copied the full URL."))
      (values code state))))

(defun openai-codex-oauth-start ()
  "Initiate an OpenAI Codex OAuth PKCE flow.
Generates PKCE pair and state, builds the authorization URL.
Returns (values auth-url code-verifier state)."
  (let* ((code-verifier (generate-code-verifier))
         (code-challenge (compute-code-challenge code-verifier))
         (state (generate-oauth-state))
         (auth-url (format nil "~A?client_id=~A&redirect_uri=~A&response_type=code&scope=~A&code_challenge=~A&code_challenge_method=S256&state=~A"
                           *openai-oauth-auth-url*
                           *openai-oauth-client-id*
                           (url-encode-param *openai-oauth-redirect-uri*)
                           (url-encode-param *openai-oauth-scopes*)
                           code-challenge
                           state)))
    (values auth-url code-verifier state)))

(defun exchange-openai-oauth-code (code code-verifier)
  "Exchange an OAuth authorization CODE for access/refresh tokens using CODE-VERIFIER.
Returns a plist (:access-token ... :refresh-token ... :expires-in ...)."
  (multiple-value-bind (body status-code)
      (drakma:http-request
       *openai-oauth-token-url*
       :method :post
       :content-type "application/x-www-form-urlencoded"
       :content (format nil "grant_type=authorization_code&client_id=~A&code=~A&redirect_uri=~A&code_verifier=~A"
                        (url-encode-param *openai-oauth-client-id*)
                        (url-encode-param code)
                        (url-encode-param *openai-oauth-redirect-uri*)
                        (url-encode-param code-verifier))
       :want-stream nil
       :force-binary nil)
    (let ((body-string (http-body-string body)))
      (unless (= status-code 200)
        (error "OAuth token exchange failed (~A): ~A" status-code body-string))
      (let ((response (api-json-decode body-string)))
        (list :access-token (cdr (assoc :access--token response))
              :refresh-token (cdr (assoc :refresh--token response))
              :expires-in (cdr (assoc :expires--in response)))))))

(defun save-openai-codex-oauth-tokens (access-token refresh-token expires-in)
  "Save OAuth tokens with computed expiry timestamp to the credential file.
Returns the access token."
  (let* ((expires-at (+ (get-universal-time) (or expires-in 3600)))
         (data `((:access--token . ,access-token)
                 (:refresh--token . ,refresh-token)
                 (:expires--at . ,expires-at))))
    (ensure-directories-exist *openai-codex-oauth-path*)
    (with-open-file (s *openai-codex-oauth-path*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-string (api-json-encode data) s))
    (ignore-errors
      (uiop:run-program (list "chmod" "600" (namestring *openai-codex-oauth-path*))))
    access-token))

(defun read-openai-codex-oauth-tokens ()
  "Read persisted OAuth credentials from the credential file.
Returns a plist (:access-token ... :refresh-token ... :expires-at ...)
or nil if the file does not exist or cannot be parsed."
  (when (probe-file *openai-codex-oauth-path*)
    (handler-case
        (let* ((json (uiop:read-file-string *openai-codex-oauth-path*))
               (data (api-json-decode json)))
          (list :access-token (cdr (assoc :access--token data))
                :refresh-token (cdr (assoc :refresh--token data))
                :expires-at (cdr (assoc :expires--at data))))
      (error () nil))))

(defun refresh-openai-codex-oauth-token (refresh-token)
  "Refresh the OpenAI Codex access token using REFRESH-TOKEN.
Saves the new credentials and returns the new access token, or nil on failure."
  (handler-case
      (multiple-value-bind (body status-code)
          (drakma:http-request
           *openai-oauth-token-url*
           :method :post
           :content-type "application/x-www-form-urlencoded"
           :content (format nil "grant_type=refresh_token&client_id=~A&refresh_token=~A"
                            (url-encode-param *openai-oauth-client-id*)
                            (url-encode-param refresh-token))
           :want-stream nil
           :force-binary nil)
        (let ((body-string (http-body-string body)))
          (when (= status-code 200)
            (let ((response (api-json-decode body-string)))
              (save-openai-codex-oauth-tokens
               (cdr (assoc :access--token response))
               (or (cdr (assoc :refresh--token response)) refresh-token)
               (cdr (assoc :expires--in response)))
              (cdr (assoc :access--token response))))))
    (error () nil)))

(defun read-openai-codex-oauth-token ()
  "Read a valid OpenAI Codex access token from the OAuth credential store.
Auto-refreshes if the token is expired or expiring within 5 minutes.
Returns the access token string or nil."
  (let ((creds (read-openai-codex-oauth-tokens)))
    (when creds
      (let ((access-token (getf creds :access-token))
            (refresh-token (getf creds :refresh-token))
            (expires-at (getf creds :expires-at)))
        (cond
          ;; Token is still valid (with 5 minute buffer)
          ((and access-token expires-at
                (> expires-at (+ (get-universal-time) 300)))
           access-token)
          ;; Token expired but we have a refresh token
          ((and refresh-token (stringp refresh-token) (plusp (length refresh-token)))
           (or (refresh-openai-codex-oauth-token refresh-token)
               access-token))
          ;; Return whatever we have
          (t access-token))))))

(defun openai-codex-oauth-finish (callback-url code-verifier expected-state)
  "Complete the OAuth flow by exchanging the authorization code for tokens.
CALLBACK-URL is the full redirect URL the user pasted.
CODE-VERIFIER is the PKCE verifier from the initial request.
EXPECTED-STATE is the state parameter for CSRF validation.
Returns the access token on success."
  (multiple-value-bind (code state)
      (extract-oauth-callback-params callback-url)
    (when (and expected-state state (not (string= state expected-state)))
      (error "OAuth state mismatch (possible CSRF). Expected ~A, got ~A"
             expected-state state))
    (let ((tokens (exchange-openai-oauth-code code code-verifier)))
      (save-openai-codex-oauth-tokens
       (getf tokens :access-token)
       (getf tokens :refresh-token)
       (getf tokens :expires-in))
      (getf tokens :access-token))))

(defparameter *provider-fallback-models*
  '((:anthropic . *anthropic-model*)
    (:openai-codex . *openai-codex-model*)
    (:zai . *zai-model*)
    (:openrouter . *openrouter-model*))
  "Alist mapping provider keywords to the variable holding their default model.
Each cdr is a symbol naming a special variable; provider-fallback-model
dereferences it at call time so that user customizations take effect.")

(defun known-provider-p (provider)
  "Return non-nil when PROVIDER is supported locally."
  (member provider '(:anthropic :openai-codex :zai :openrouter) :test #'eq))

(defun normalize-provider (provider)
  "Normalize PROVIDER to a supported keyword, or nil when absent."
  (cond
    ((null provider) nil)
    ((keywordp provider)
     (if (known-provider-p provider)
         provider
         (error "Unknown provider ~S. Supported providers: :ANTHROPIC, :OPENAI-CODEX, :ZAI, :OPENROUTER"
                provider)))
    ((stringp provider)
     (normalize-provider (intern (string-upcase provider) :keyword)))
    ((symbolp provider)
     (normalize-provider (symbol-name provider)))
    (t
     (error "Unknown provider ~S. Supported providers: :ANTHROPIC, :OPENAI-CODEX, :ZAI, :OPENROUTER"
            provider))))

(defun blank-string-p (value)
  "Return non-nil when VALUE is nil or all whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))

(defun provider-fallback-model (provider)
  "Return the fallback model for PROVIDER.
Looks up the provider-specific variable in *provider-fallback-models* and
returns its current value, so user customizations (e.g. via init.lisp) are
respected."
  (let ((entry (cdr (assoc provider *provider-fallback-models*))))
    (when entry (symbol-value entry))))

;;; --------------------------------------------------------------------------
;;; Known Models Per Provider
;;; --------------------------------------------------------------------------

(defparameter *provider-known-models*
  '((:anthropic
     "claude-haiku-4-5-20251001"
     "claude-sonnet-4-6"
     "claude-opus-4-6"
     "claude-3-5-haiku-20241022"
     "claude-3-haiku-20240307")
    (:openai-codex
     "codex-mini-latest"
     "o4-mini"
     "gpt-4.1-mini"
     "gpt-4.1-nano")
    (:zai
     "glm-5"
     "glm-5-turbo"
     "glm-4.7"
     "glm-4.6"
     "glm-4.5"
     "glm-4.5-air")
    (:openrouter
     "openai/gpt-4o-mini"
     "openai/gpt-4o"
     "anthropic/claude-3-5-haiku"
     "anthropic/claude-3-5-sonnet"
     "google/gemini-2.5-pro"
     "google/gemini-2.0-flash-001"
     "meta-llama/llama-4-maverick"
     "deepseek/deepseek-r1"))
  "Known model identifiers grouped by provider.
The first model in each list is the provider's default.
For :OPENROUTER, models are dynamically fetched by fetch-openrouter-models when
an API key is configured; this static list is used as a fallback.
These are used by the model selector overlay.")

(defun provider-known-models (provider)
  "Return the list of known model names for PROVIDER.
For :OPENROUTER, returns the dynamically-fetched model list when available,
falling back to the static *provider-known-models* entry."
  (if (and (eq provider :openrouter) *openrouter-cached-models*)
      *openrouter-cached-models*
      (cdr (assoc provider *provider-known-models*))))

(defun fetch-openrouter-models ()
  "Fetch the list of available models from the OpenRouter API.
Populates *openrouter-cached-models* and returns the model ID list.
Requires a valid OpenRouter API key to be configured.
Returns the cached list on subsequent calls; set *openrouter-cached-models* to
nil to force a refresh. Returns the static fallback list on any error."
  (when *openrouter-cached-models*
    (return-from fetch-openrouter-models *openrouter-cached-models*))
  (handler-case
      (let ((token (read-provider-token :openrouter)))
        (unless token
          (return-from fetch-openrouter-models
            (cdr (assoc :openrouter *provider-known-models*))))
        (multiple-value-bind (body status-code)
            (drakma:http-request
             *openrouter-models-url*
             :method :get
             :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token)))
             :want-stream nil
             :force-binary nil)
          (if (= status-code 200)
              (let* ((body-string (http-body-string body))
                     (response (api-json-decode body-string))
                     (models-data (cdr (assoc :data response)))
                     (ids (loop :for m :in (coerce models-data 'list)
                                :for id := (cdr (assoc :id m))
                                :when (and id (stringp id) (plusp (length id)))
                                  :collect id)))
                (when ids
                  (setf *openrouter-cached-models* ids))
                (or ids (cdr (assoc :openrouter *provider-known-models*))))
              (cdr (assoc :openrouter *provider-known-models*)))))
    (error ()
      (cdr (assoc :openrouter *provider-known-models*)))))

(defun provider-has-token-p (provider)
  "Return non-nil when PROVIDER has a usable API key or OAuth token configured."
  (handler-case
      (let ((token (read-provider-token provider)))
        (and token (stringp token) (plusp (length token))))
    (error () nil)))

(defun available-models-for-selector (buf)
  "Build the model selector entry list for BUF.
Returns a list of plists: ((:provider :anthropic :model \"name\" :active-p t/nil) ...)
Only includes providers that have a valid API key. The entry matching BUF's
currently resolved provider/model is marked :active-p t.
For :OPENROUTER, dynamically-fetched models are used when an API key is present."
  (multiple-value-bind (current-provider current-model)
      (handler-case (resolve-buffer-provider-and-model buf)
        (error () (values nil nil)))
    (let ((entries nil))
      (dolist (provider-models *provider-known-models*)
        (let* ((provider (car provider-models))
               (models (if (eq provider :openrouter)
                           (fetch-openrouter-models)
                           (cdr provider-models))))
          (when (provider-has-token-p provider)
            (dolist (model models)
              (push (list :provider provider
                          :model model
                          :active-p (and (eq provider current-provider)
                                         (string= model current-model)))
                    entries)))))
      (nreverse entries))))

(defun json-key-string (key)
  "Convert a decoded JSON key into a lowercase string."
  (string-downcase
   (etypecase key
     (string key)
     (symbol (symbol-name key)))))

(defun lookup-json-value (alist key)
  "Find KEY in decoded JSON ALIST, handling string and symbol keys."
  (let ((name (string-downcase key)))
    (loop :for (entry-key . value) :in alist
          :when (string= name (json-key-string entry-key))
            :do (return value))))

(defun make-agent-defaults-registry ()
  "Create an empty in-memory agent defaults registry."
  (list :agents (make-hash-table :test #'equal)))

(defun registry-agents (registry)
  "Return the agent table stored in REGISTRY."
  (getf registry :agents))

(defun agent-default-spec (agent-name)
  "Return the stored default spec for AGENT-NAME, or nil."
  (ensure-agent-defaults-loaded)
  (gethash (string-downcase agent-name)
           (registry-agents *agent-defaults-registry*)))

(defun load-agent-defaults ()
  "Load and memoize persisted agent defaults, overlaying built-in fallbacks."
  (let ((registry (make-agent-defaults-registry)))
    (when (probe-file *agent-defaults-path*)
      (let* ((json (uiop:read-file-string *agent-defaults-path*))
             (data (cl-json:decode-json-from-string json))
             (agents (registry-agents registry)))
        (dolist (entry data)
          (let* ((agent-name (json-key-string (car entry)))
                 (spec (cdr entry))
                 (provider (normalize-provider (lookup-json-value spec "provider")))
                 (model (lookup-json-value spec "model")))
            (setf (gethash agent-name agents)
                  (list :provider provider
                        :model (and (stringp model)
                                    model)))))))
    (setf *agent-defaults-registry* registry)))

(defun save-agent-defaults ()
  "Persist the current agent defaults registry to disk."
  (ensure-agent-defaults-loaded)
  (let ((payload nil))
    (maphash
     (lambda (agent-name spec)
       (let ((provider (getf spec :provider))
             (model (getf spec :model)))
         (push `(,agent-name . ((:provider . ,(and provider
                                                   (string-downcase (symbol-name provider))))
                                ,@(when model
                                    `((:model . ,model)))))
               payload)))
     (registry-agents *agent-defaults-registry*))
    (ensure-directories-exist *agent-defaults-path*)
    (with-open-file (stream *agent-defaults-path*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (api-json-encode (nreverse payload)) stream))
    *agent-defaults-path*))

(defun ensure-agent-defaults-loaded ()
  "Load the agent defaults registry on first use."
  (or *agent-defaults-registry*
      (load-agent-defaults)))

(defun agent-default (agent-name)
  "Return AGENT-NAME's default provider, or *default-provider*."
  (or (getf (agent-default-spec agent-name)
            :provider)
      *default-provider*))

(defun agent-default-model (agent-name provider)
  "Return AGENT-NAME's stored model when it matches PROVIDER."
  (let ((spec (agent-default-spec agent-name)))
    (when (eq provider (getf spec :provider))
      (getf spec :model))))

(defun set-agent-default (agent-name provider &key model)
  "Set AGENT-NAME's default provider and optional MODEL, then persist it."
  (ensure-agent-defaults-loaded)
  (let ((normalized-provider (normalize-provider provider)))
    (when (and model (blank-string-p model))
      (error "Resolved model must be a non-empty string"))
    (setf (gethash (string-downcase agent-name)
                   (registry-agents *agent-defaults-registry*))
          (list :provider normalized-provider
                :model model))
    (save-agent-defaults)
    normalized-provider))

(defun resolve-buffer-provider-and-model (buf)
  "Resolve BUF's effective provider and model using overrides and defaults.
Resolution order for provider: buffer override → agent default → *default-provider*.
Resolution order for model: buffer override → agent default → provider fallback → *default-model*."
  (ensure-agent-defaults-loaded)
  (let* ((provider (or (buffer-provider-override buf)
                       (agent-default (buffer-agent-name buf))
                       *default-provider*))
         (resolved-provider (normalize-provider provider))
         (model (or (buffer-model-override buf)
                    (agent-default-model (buffer-agent-name buf) resolved-provider)
                    (provider-fallback-model resolved-provider)
                    *default-model*)))
    (when (blank-string-p model)
      (error "Resolved model must be a non-empty string"))
    (values resolved-provider model)))

;;; --------------------------------------------------------------------------
;;; Conversation Building
;;; --------------------------------------------------------------------------

(defun build-conversation-messages (buf)
  "Build the Anthropic API messages array from the buffer's chat history.
Uses raw-content when available (for tool_use/tool_result messages),
falls back to plain text content.
System messages (sender :system) are excluded — they are display-only
and should not be sent to the API."
  (let ((messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :do (let ((sender (message-sender msg)))
                ;; Skip system messages — they are display-only (shell output, etc.)
                (unless (eq sender :system)
                  (let* ((role (cond
                                 ((eq sender :user) "user")
                                 ((eq sender :tool-result) "user")
                                 (t "assistant")))
                         (content (canonicalize-message-content
                                   role
                                   (or (message-raw-content msg)
                                       (message-text msg)))))
                    (push `((:role . ,role)
                            (:content . ,(coerce content 'vector)))
                          messages)))))
    (nreverse messages)))

(defun canonical-tool-use-block (id name input)
  "Return ID/NAME/INPUT as a canonical tool_use block."
  `((:type . "tool_use")
    (:id . ,id)
    (:name . ,name)
    (:input . ,input)))

(defun canonical-response (stop-reason content-blocks)
  "Return a provider-agnostic response payload."
  `((:stop--reason . ,stop-reason)
    (:content . ,(coerce content-blocks 'vector))))

(defun http-body-string (body)
  "Return BODY as a UTF-8 string."
  (if (stringp body)
      body
      (flexi-streams:octets-to-string body :external-format :utf-8)))

(defun openai-finish-reason->stop-reason (finish-reason)
  "Normalize OpenAI FINISH-REASON to clawmacs stop reasons."
  (cond
    ((null finish-reason) nil)
    ((string= finish-reason "tool_calls") "tool_use")
    ((string= finish-reason "stop") "end_turn")
    (t finish-reason)))

(defun openai-tool-call->canonical-block (tool-call)
  "Convert an OpenAI TOOL-CALL object into a canonical tool_use block."
  (let* ((function (cdr (assoc :function tool-call)))
         (arguments (cdr (assoc :arguments function)))
         (input (cond
                  ((blank-string-p arguments) nil)
                  ((stringp arguments) (api-json-decode arguments))
                  (t arguments))))
    (canonical-tool-use-block
     (cdr (assoc :id tool-call))
     (cdr (assoc :name function))
     input)))

(defun openai-choice->canonical-response (choice)
  "Normalize an OpenAI completion CHOICE to canonical response shape.
Handles reasoning models (Z.AI GLM, DeepSeek R1, etc.) that return
reasoning_content alongside content. When content is blank but
reasoning_content is present, falls back to reasoning_content."
  (let* ((message (cdr (assoc :message choice)))
         (content-blocks nil)
         (text (cdr (assoc :content message)))
         (reasoning (cdr (assoc :reasoning--content message)))
         (tool-calls (cdr (assoc :tool--calls message)))
         ;; Use content if non-blank, otherwise fall back to reasoning
         (effective-text (cond
                           ((not (blank-string-p text)) text)
                           ((not (blank-string-p reasoning)) reasoning)
                           (t nil))))
    (when effective-text
      (push (canonical-text-block effective-text) content-blocks))
    (dolist (tool-call (coerce (or tool-calls #()) 'list))
      (push (openai-tool-call->canonical-block tool-call) content-blocks))
    (canonical-response
     (openai-finish-reason->stop-reason (cdr (assoc :finish--reason choice)))
     (nreverse content-blocks))))

(defun message-role-content-blocks (message)
  "Extract a decoded provider message into ROLE and canonical content blocks."
  (values (cdr (assoc :role message))
          (coerce (cdr (assoc :content message)) 'list)))

(defun content-blocks->openai-tool-calls (tool-uses)
  "Convert canonical TOOL-USES into OpenAI tool call objects."
  (coerce
   (loop :for tool-use :in tool-uses
         :collect `((:id . ,(cdr (assoc :id tool-use)))
                    (:type . "function")
                    (:function . ((:name . ,(cdr (assoc :name tool-use)))
                                  (:arguments . ,(api-json-encode
                                                  (or (cdr (assoc :input tool-use))
                                                      '())))))))
   'vector))

(defun anthropic-tools->openai-tools (tools)
  "Translate Anthropic-style TOOLS to OpenAI tool definitions."
  (when (and tools (plusp (length tools)))
    (coerce
     (loop :for tool :across tools
           :collect `((:type . "function")
                      (:function . ((:name . ,(cdr (assoc :name tool)))
                                    (:description . ,(cdr (assoc :description tool)))
                                    (:parameters . ,(cdr (assoc :input--schema tool)))))))
     'vector)))

(defun conversation-messages->openai-messages (messages)
  "Translate canonical conversation MESSAGES into OpenAI chat messages."
  (loop :for message :in messages
        :append
        (multiple-value-bind (role content-blocks)
            (message-role-content-blocks message)
          (let ((text (content-text-blocks content-blocks))
                (tool-uses (content-tool-use-blocks content-blocks))
                (tool-results (remove-if-not (lambda (block)
                                               (string= "tool_result"
                                                        (content-block-type block)))
                                             content-blocks)))
            (cond
              ((string= role "assistant")
               (list `((:role . "assistant")
                       (:content . ,(unless (blank-string-p text) text))
                       ,@(when tool-uses
                           `((:tool--calls . ,(content-blocks->openai-tool-calls tool-uses)))))))
              ((and (string= role "user") tool-results)
               (append
                (when (not (blank-string-p text))
                  (list `((:role . "user")
                          (:content . ,text))))
                 (loop :for block :in tool-results
                       :collect `((:role . "tool")
                                 (:tool--call--id . ,(or (cdr (assoc :tool--use--id block))
                                                         (cdr (assoc :tool-use-id block))))
                                 (:content . ,(cdr (assoc :content block)))))))
              (t
               (list `((:role . ,role)
                       (:content . ,text)))))))))

(defun openai-messages-with-system-prompt (messages)
  "Translate MESSAGES and prepend the built system prompt when present."
  (let ((openai-messages (conversation-messages->openai-messages messages))
        (system-prompt (build-system-prompt)))
    (if system-prompt
        (cons `((:role . "system")
                (:content . ,system-prompt))
              openai-messages)
        openai-messages)))

(defun provider-request (provider messages &key model (max-tokens 8192) tools)
  "Dispatch a non-streaming request by resolved PROVIDER.
Anthropic models listed in *claude-cli-models* are routed through the
Claude Code CLI subprocess instead of the REST API."
  (ecase provider
    (:anthropic
     (if (claude-cli-model-p model)
         (claude-cli-request messages :model model :max-tokens max-tokens :tools tools)
         (anthropic-request messages :model model :max-tokens max-tokens :tools tools)))
    (:openai-codex
     (openai-codex-request messages :model model :max-tokens max-tokens :tools tools))
    (:zai
     (zai-request messages :model model :max-tokens max-tokens :tools tools))
    (:openrouter
     (openrouter-request messages :model model :max-tokens max-tokens :tools tools))))

(defun provider-request-streaming (provider messages callback &key model (max-tokens 8192) tools)
  "Dispatch a streaming request by resolved PROVIDER.
Anthropic models listed in *claude-cli-models* are routed through the
Claude Code CLI subprocess instead of the REST API."
  (ecase provider
    (:anthropic
     (if (claude-cli-model-p model)
         (claude-cli-request-streaming messages callback
                                       :model model
                                       :max-tokens max-tokens
                                       :tools tools)
         (anthropic-request-streaming messages callback
                                      :model model
                                      :max-tokens max-tokens
                                      :tools tools)))
    (:openai-codex
     (openai-codex-request-streaming messages callback
                                     :model model
                                     :max-tokens max-tokens
                                     :tools tools))
    (:zai
     (zai-request-streaming messages callback
                            :model model
                            :max-tokens max-tokens
                            :tools tools))
    (:openrouter
     (openrouter-request-streaming messages callback
                                   :model model
                                   :max-tokens max-tokens
                                   :tools tools))))

;;; --------------------------------------------------------------------------
;;; Claude CLI Subprocess Provider (stream-json protocol)
;;; --------------------------------------------------------------------------
;;; The Claude Max subscription (5x, 20x) restricts Sonnet/Opus models to the
;;; Claude Code CLI process.  The REST API returns 400 "Error" for these models
;;; even with valid OAuth tokens.
;;;
;;; We use the Claude Code CLI's stream-json subprocess protocol — the same
;;; protocol used by Goose, OpenClaw, and other agentic editors.  This spawns
;;; `claude --input-format stream-json --output-format stream-json` and
;;; communicates via NDJSON on stdin/stdout.
;;;
;;; Protocol overview:
;;;   1. Spawn: claude --input-format stream-json --output-format stream-json
;;;            --verbose --model <model> --system-prompt "..." --max-turns 1
;;;   2. Send:  {"type":"user","session_id":"...","message":{"role":"user",
;;;             "content":[{"type":"text","text":"..."}]}}  (newline-terminated)
;;;   3. Recv:  NDJSON events on stdout until {"type":"result",...}
;;;
;;; Each request spawns a fresh subprocess (auth/config load is fast).
;;; The --dangerously-skip-permissions flag prevents interactive permission
;;; prompts.  --max-turns 1 restricts to a single response turn.

(defvar *claude-cli-session-counter* 0
  "Monotonically increasing counter for generating unique session IDs.")

(defun claude-cli-next-session-id ()
  "Generate a unique session ID for a Claude CLI request."
  (format nil "clawmacs-~D-~D"
          (get-universal-time) (incf *claude-cli-session-counter*)))

(defun claude-cli-build-prompt (messages)
  "Flatten MESSAGES into a single prompt string for the Claude CLI.
Older messages are prepended as context so the model sees the full
conversation history.  System messages are excluded since the system
prompt is passed via --system-prompt."
  (with-output-to-string (s)
    (dolist (msg messages)
      (let ((role (cdr (assoc :role msg)))
            (raw  (cdr (assoc :content msg))))
        (when (and role raw (not (string= role "system")))
          (let ((text (cond
                        ;; Plain string content
                        ((stringp raw) raw)
                        ;; Vector of content blocks – concatenate text blocks
                        ((and (vectorp raw) (plusp (length raw)))
                         (with-output-to-string (ts)
                           (loop :for blk :across raw
                                 :when (string= "text"
                                                 (cdr (assoc :type blk)))
                                 :do (write-string (cdr (assoc :text blk)) ts))))
                        (t (format nil "~A" raw)))))
            (format s "~:[~;~%~%~][~A]: ~A" (plusp (file-position s))
                    role text)))))))

(defun claude-cli-build-ndjson-message (messages session-id)
  "Build an NDJSON message line for the Claude CLI stream-json protocol.
Flattens all conversation MESSAGES into a single user message with full
context, tagged with SESSION-ID."
  (let ((prompt-text (claude-cli-build-prompt messages)))
    (api-json-encode
     `((:type . "user")
       (:session--id . ,session-id)
       (:message . ((:role . "user")
                    (:content . ,(vector
                                  `((:type . "text")
                                    (:text . ,prompt-text))))))))))

(defun claude-cli-spawn-args (model)
  "Build the CLI argument list for spawning a Claude subprocess for MODEL.
Wraps with `env -u LD_LIBRARY_PATH` so the Nix-packaged CLI binary uses its
own RPATH for library resolution instead of picking up incompatible Guix
container libraries (e.g. OpenSSL version mismatch)."
  (let ((system-prompt (or (build-system-prompt)
                           "You are a helpful assistant.")))
    (list "env" "-u" "LD_LIBRARY_PATH"
          *claude-cli-path*
          "--input-format" "stream-json"
          "--output-format" "stream-json"
          "--verbose"
          "--model" model
          "--system-prompt" system-prompt
          "--dangerously-skip-permissions"
          "--max-turns" "1")))

(defun claude-cli-request (messages &key (model "claude-sonnet-4-6")
                                         (max-tokens 8192)
                                         tools)
  "Send a non-streaming request via the Claude Code CLI subprocess.
Uses the stream-json protocol: spawns `claude --input-format stream-json`,
writes an NDJSON message to stdin, and reads NDJSON events from stdout
until a result event arrives.  Returns a canonical Anthropic-format
response alist."
  (declare (ignore max-tokens tools))
  (let* ((session-id (claude-cli-next-session-id))
         (ndjson-msg (claude-cli-build-ndjson-message messages session-id))
         (args (claude-cli-spawn-args model))
         (proc (uiop:launch-program args
                                     :input :stream
                                     :output :stream
                                     :error-output nil))
         (stdin (uiop:process-info-input proc))
         (stdout (uiop:process-info-output proc)))
    ;; Send the NDJSON message and close stdin to signal completion
    (unwind-protect
         (progn
           (write-string ndjson-msg stdin)
           (write-char #\Newline stdin)
           (force-output stdin)
           (close stdin)
           (setf stdin nil)
           ;; Read NDJSON events from stdout until result or error
           (let ((accumulated-text "")
                 (usage nil))
             (loop :for line := (read-line stdout nil nil)
                   :while line
                   :when (and (plusp (length line))
                              (char= (char line 0) #\{))
                   :do (handler-case
                           (let* ((event (api-json-decode line))
                                  (event-type (cdr (assoc :type event))))
                             (cond
                               ;; Assistant message with content blocks
                               ;; Content is a list (cl-json decodes JSON
                               ;; arrays as lists).
                               ((string= event-type "assistant")
                                (let* ((message (cdr (assoc :message event)))
                                       (content (cdr (assoc :content message))))
                                  (when content
                                    (dolist (blk (coerce content 'list))
                                      (when (string= "text"
                                                      (or (cdr (assoc :type blk)) ""))
                                        (let ((text (cdr (assoc :text blk))))
                                          (when text
                                            (setf accumulated-text
                                                  (concatenate 'string
                                                               accumulated-text
                                                               text)))))))))
                               ;; Result event — final response
                               ((string= event-type "result")
                                (setf accumulated-text
                                      (or (cdr (assoc :result event))
                                          accumulated-text)
                                      usage (cdr (assoc :usage event)))
                                (return))
                               ;; Error event
                               ((string= event-type "error")
                                (error "Claude CLI error: ~A"
                                       (or (cdr (assoc :error event))
                                           "Unknown error")))))
                         (error (e)
                           ;; Skip malformed lines but propagate real errors
                           (when (search "Claude CLI error" (format nil "~A" e))
                             (error e)))))
             ;; Build canonical Anthropic response format
             `((:id . ,session-id)
               (:type . "message")
               (:role . "assistant")
               (:content . ,(vector (canonical-text-block (or accumulated-text ""))))
               (:model . ,model)
               (:stop--reason . "end_turn")
               (:usage . ,(or usage
                              `((:input--tokens . 0)
                                (:output--tokens . ,(length (or accumulated-text "")))))))))
      ;; Cleanup
      (when stdin (ignore-errors (close stdin)))
      (ignore-errors (close stdout))
      (ignore-errors (uiop:wait-process proc)))))

(defun claude-cli-request-streaming (messages callback
                                      &key (model "claude-sonnet-4-6")
                                           (max-tokens 8192)
                                           tools)
  "Send a streaming request via the Claude Code CLI subprocess.
Uses the stream-json protocol: spawns `claude --input-format stream-json`,
writes an NDJSON message to stdin, and parses NDJSON events from stdout
in a background thread.  Returns a stream-state that the event loop polls."
  (declare (ignore callback max-tokens tools))
  (let* ((session-id (claude-cli-next-session-id))
         (ndjson-msg (claude-cli-build-ndjson-message messages session-id))
         (args (claude-cli-spawn-args model))
         (state (make-stream-state)))
    (file-debug-log "cli-spawn" "session=~A model=~A args=(~{~A~^ ~})"
                    session-id model args)
    (file-debug-log "cli-stdin" "~A" ndjson-msg)
    ;; Spawn background thread to run the CLI and parse NDJSON output
    (bt:make-thread
     (lambda ()
       (handler-case
           (let* ((proc (uiop:launch-program args
                                              :input :stream
                                              :output :stream
                                              :error-output :stream))
                  (stdin (uiop:process-info-input proc))
                  (stdout (uiop:process-info-output proc))
                  (stderr (uiop:process-info-error-output proc))
                  (got-result nil))
             (file-debug-log "cli-spawn" "process launched, pid=~A"
                             (ignore-errors (uiop:process-info-pid proc)))
             (unwind-protect
                  (progn
                    ;; Send NDJSON message and close stdin
                    (write-string ndjson-msg stdin)
                    (write-char #\Newline stdin)
                    (force-output stdin)
                    (close stdin)
                    (setf stdin nil)
                    (file-debug-log "cli-stdin" "message sent, stdin closed")
                    ;; Read NDJSON events from stdout
                    (loop :for line := (read-line stdout nil nil)
                          :while line
                          :do (file-debug-log "cli-stdout" "~A" line)
                          :when (and (plusp (length line))
                                     (char= (char line 0) #\{))
                          :do (handler-case
                                  (let* ((event (api-json-decode line))
                                         (event-type (cdr (assoc :type event))))
                                    (file-debug-log "cli-event" "type=~A" event-type)
                                    (cond
                                      ;; Streaming events (wrapped in stream_event)
                                      ;; Delegate to process-stream-event which handles
                                      ;; all Anthropic event types (block start/delta/stop,
                                      ;; message_delta for stop_reason, etc.)
                                      ;; Pass the inner alist directly — no JSON roundtrip
                                      ;; (api-json-encode/decode mangles underscore keys).
                                      ;; Suppress message_stop — the CLI result event is
                                      ;; the authoritative completion signal.
                                      ((string= event-type "stream_event")
                                       (let* ((inner (cdr (assoc :event event)))
                                              (inner-type (when inner
                                                            (cdr (assoc :type inner)))))
                                         (file-debug-log "cli-stream" "inner-type=~A"
                                                         inner-type)
                                         (when (and inner
                                                    (not (and inner-type
                                                              (string= inner-type
                                                                       "message_stop"))))
                                           (process-stream-event inner state))))
                                      ;; Full assistant message (complete, not streaming)
                                      ;; The CLI sends these for each turn's response.
                                      ;; Only extract text blocks — tool_use blocks are
                                      ;; for the CLI's internal tools (Bash, ToolSearch,
                                      ;; Read, etc.) and must NOT propagate to clawmacs.
                                      ;; Content is a list (cl-json decodes JSON arrays
                                      ;; as lists, not vectors).
                                      ((string= event-type "assistant")
                                       (let* ((message (cdr (assoc :message event)))
                                              (content (cdr (assoc :content message)))
                                              (blocks (coerce content 'list)))
                                         (file-debug-log "cli-assistant"
                                                         "blocks=~D types=(~{~A~^ ~})"
                                                         (length blocks)
                                                         (mapcar (lambda (b)
                                                                   (cdr (assoc :type b)))
                                                                 blocks))
                                         (when blocks
                                           (bt:with-lock-held ((stream-state-lock state))
                                             (dolist (blk blocks)
                                               (when (string= "text"
                                                               (or (cdr (assoc :type blk)) ""))
                                                 (let ((text (cdr (assoc :text blk))))
                                                   (when text
                                                     (setf (stream-state-text state)
                                                           (concatenate 'string
                                                                        (stream-state-text state)
                                                                        text))
                                                     (push (canonical-text-block
                                                            (stream-state-text state))
                                                           (stream-state-content-blocks
                                                            state))))))))))
                                      ;; Result event — completion
                                      ;; Only overwrite streamed text when result
                                      ;; provides non-empty text (non-streaming fallback).
                                      ;; Always use "end_turn" as stop_reason — the CLI
                                      ;; handles its own tools internally, so "tool_use"
                                      ;; from the result refers to CLI-internal tools
                                      ;; (Bash, ToolSearch, etc.) that must NOT trigger
                                      ;; clawmacs tool execution.
                                      ((string= event-type "result")
                                       (setf got-result t)
                                       (let ((text (cdr (assoc :result event)))
                                             (result-stop (cdr (assoc :stop--reason event)))
                                             (subtype (cdr (assoc :subtype event))))
                                         (file-debug-log "cli-result"
                                                         "subtype=~A result-text-length=~A result-stop=~A blocks=~A accumulated-stop=~A"
                                                         subtype
                                                         (if text (length text) 0)
                                                         result-stop
                                                         (length (stream-state-content-blocks state))
                                                         (stream-state-stop-reason state))
                                         (bt:with-lock-held ((stream-state-lock state))
                                           (when (and text (plusp (length text)))
                                             (setf (stream-state-text state) text
                                                   (stream-state-content-blocks state)
                                                   (list (canonical-text-block text))))
                                           (when (null (stream-state-content-blocks state))
                                             (setf (stream-state-content-blocks state)
                                                   (list (canonical-text-block ""))))
                                           (setf (stream-state-stop-reason state) "end_turn"
                                                 (stream-state-done-p state) t)))
                                       (return))
                                      ;; Error event
                                      ((string= event-type "error")
                                       (setf got-result t)
                                       (let ((err (or (cdr (assoc :error event))
                                                      (cdr (assoc :message event))
                                                      "Unknown CLI error")))
                                         (file-debug-log "cli-error" "~A" err)
                                         (bt:with-lock-held ((stream-state-lock state))
                                           (setf (stream-state-error-p state) err
                                                 (stream-state-done-p state) t)))
                                       (return))))
                                (error (e)
                                  (file-debug-log "cli-parse-error" "~A on line: ~A"
                                                  e line)
                                  nil)))
                    ;; If CLI exited without result/error event, read stderr
                    (unless got-result
                      (let ((err-text
                              (handler-case
                                  (with-output-to-string (s)
                                    (loop :for ch := (read-char-no-hang stderr nil nil)
                                          :while ch
                                          :do (write-char ch s)))
                                (error () nil))))
                        (file-debug-log "cli-stderr" "~A"
                                        (or err-text "(empty)"))
                        (bt:with-lock-held ((stream-state-lock state))
                          (setf (stream-state-error-p state)
                                (if (and err-text (plusp (length err-text)))
                                    (format nil "Claude CLI exited without response: ~A"
                                            (string-trim '(#\Newline #\Return #\Space)
                                                         err-text))
                                    (format nil "Claude CLI exited without response (command: ~{~A~^ ~})"
                                            args)))))))
               ;; Cleanup process
               (when stdin (ignore-errors (close stdin)))
               (ignore-errors (close stdout))
               (ignore-errors (close stderr))
               (ignore-errors (uiop:wait-process proc))))
         (error (e)
           (file-debug-log "cli-fatal" "~A" e)
           (bt:with-lock-held ((stream-state-lock state))
             (setf (stream-state-error-p state) (format nil "~A" e)
                   (stream-state-done-p state) t))))
       ;; Ensure done flag is always set
       (file-debug-log "cli-done" "thread finishing, done-p=~A error-p=~A blocks=~A stop=~A"
                       (stream-state-done-p state)
                       (stream-state-error-p state)
                       (length (stream-state-content-blocks state))
                       (stream-state-stop-reason state))
       (bt:with-lock-held ((stream-state-lock state))
         (setf (stream-state-done-p state) t)))
     :name "clawmacs-claude-cli-reader")
    state))

;;; --------------------------------------------------------------------------
;;; API Call
;;; --------------------------------------------------------------------------

(defun anthropic-request (messages &key (model *anthropic-model*)
                                        (max-tokens 8192)
                                        tools)
  "Call the Anthropic Messages API. Returns the parsed response alist.
MESSAGES is a list of message alists. TOOLS is a vector of tool definitions
(or nil for no tools)."
  (let* ((token (or (read-token)
                    (error "No API token. Set CLAUDE_CODE_OAUTH_TOKEN env var, ~
                            run 'claude setup-token', or save to ~
                            ~~/.config/clawmacs/claude-max-token")))
         (request-body
           (let ((body `((:model . ,model)
                         (:max--tokens . ,max-tokens)
                         (:messages . ,(coerce messages 'vector)))))
             (when (and tools (plusp (length tools)))
               (push `(:tools . ,tools) body))
             ;; System prompt as array of content blocks — required for
             ;; Claude Max OAuth tokens (first block must be the CC prefix).
             (let* ((system-prompt (build-system-prompt))
                    (blocks (list `((:type . "text")
                                    (:text . ,*claude-code-system-prefix*)))))
               (when system-prompt
                 (setf blocks (append blocks
                                      (list `((:type . "text")
                                              (:text . ,system-prompt))))))
               (push `(:system . ,(coerce blocks 'vector)) body))
              (api-json-encode body))))
    (multiple-value-bind (body status-code)
        (drakma:http-request
         *anthropic-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers
         `(("Authorization" . ,(format nil "Bearer ~A" token))
           ("anthropic-version" . ,*anthropic-version*)
           ("anthropic-beta" . ,*anthropic-beta*))
         :content request-body
         :want-stream nil
         :force-binary nil)
       (let ((body-string (http-body-string body)))
         (unless (= status-code 200)
           (error "API error (~A): ~A" status-code body-string))
         (api-json-decode body-string)))))

(defun openai-codex-request (messages &key (model *openai-codex-model*)
                                           (max-tokens 8192)
                                           tools)
  "Call OpenAI Chat Completions and normalize the response shape."
  (let* ((token (or (read-provider-token :openai-codex)
                    (error 'simple-error
                           :format-control "No API token. Save to ~/.config/clawmacs/openai-codex-token")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--completion--tokens . ,max-tokens)
                         (:messages . ,(coerce (openai-messages-with-system-prompt messages)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(anthropic-tools->openai-tools tools)) body))
              (api-json-encode body))))
    (multiple-value-bind (body status-code)
        (drakma:http-request
         *openai-codex-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token)))
         :content request-body
         :want-stream nil
         :force-binary nil)
      (let ((body-string (http-body-string body)))
        (unless (= status-code 200)
          (error "API error (~A): ~A" status-code body-string))
        (let* ((response (api-json-decode body-string))
               (choices (cdr (assoc :choices response)))
               (choice (first (coerce choices 'list))))
          (unless choice
            (error "OpenAI response did not include a choice"))
          (openai-choice->canonical-response choice))))))

;;; --------------------------------------------------------------------------
;;; Streaming API Call
;;; --------------------------------------------------------------------------

(defstruct stream-state
  "Mutable state for an in-progress streaming response."
  (text ""             :type string)
  (content-blocks nil  :type list)
  (tool-input-json ""  :type string)   ; accumulates input_json_delta for tool_use
  (openai-tool-call-states (make-hash-table :test #'equal))
  (openai-tool-call-order nil :type list)
  (stop-reason nil)
  (done-p nil          :type boolean)
  (error-p nil)
  (lock (bt:make-lock "stream-state")))

(defun parse-sse-line (line)
  "Parse a single SSE line. Returns (values field value) or nil.
SSE format: 'field: value' or just 'data: {...}'."
  (let ((colon-pos (position #\: line)))
    (when (and colon-pos (plusp colon-pos))
      (let ((field (subseq line 0 colon-pos))
            (value (if (< (1+ colon-pos) (length line))
                       (string-left-trim " " (subseq line (1+ colon-pos)))
                       "")))
        (values field value)))))

(defun process-stream-event (event state)
  "Process a decoded SSE event alist and update STATE.
EVENT is an already-decoded alist (not a JSON string)."
  (let ((event-type (cdr (assoc :type event))))
    (cond
      ;; content_block_start: new content block beginning
      ((string= "content_block_start" (or event-type ""))
       (let ((block (cdr (assoc :content--block event))))
         (when block
           (bt:with-lock-held ((stream-state-lock state))
             (push block (stream-state-content-blocks state))
             ;; Reset accumulators for the new block
             (setf (stream-state-text state) ""
                   (stream-state-tool-input-json state) "")))))

      ;; content_block_delta: incremental text or tool input JSON
      ((string= "content_block_delta" (or event-type ""))
       (let* ((delta (cdr (assoc :delta event)))
              (delta-type (cdr (assoc :type delta))))
         (cond
           ((string= "text_delta" (or delta-type ""))
            (let ((text (cdr (assoc :text delta))))
              (when text
                (bt:with-lock-held ((stream-state-lock state))
                  (setf (stream-state-text state)
                        (concatenate 'string (stream-state-text state) text))))))
           ((string= "input_json_delta" (or delta-type ""))
            (let ((partial (cdr (assoc :partial--json delta))))
              (when partial
                (bt:with-lock-held ((stream-state-lock state))
                  (setf (stream-state-tool-input-json state)
                        (concatenate 'string
                                     (stream-state-tool-input-json state)
                                     partial)))))))))

      ;; content_block_stop: block finished
      ((string= "content_block_stop" (or event-type ""))
       (bt:with-lock-held ((stream-state-lock state))
         (let* ((blocks (stream-state-content-blocks state))
                (block (first blocks))
                (block-type (when block (cdr (assoc :type block)))))
           (cond
             ;; Finalize text block
             ((and block (string= "text" (or block-type "")))
               (setf (first blocks)
                     (canonical-text-block (stream-state-text state))))
              ;; Finalize tool_use block: parse accumulated JSON into input
              ((and block (string= "tool_use" (or block-type "")))
               (let ((json-str (stream-state-tool-input-json state)))
                 (setf (first blocks)
                       (canonical-tool-use-block
                        (cdr (assoc :id block))
                        (cdr (assoc :name block))
                        (when (plusp (length json-str))
                          (handler-case
                              (api-json-decode json-str)
                            (error () nil))))))))
            ;; Reset accumulators for next block
            (setf (stream-state-text state) ""
                  (stream-state-tool-input-json state) ""))))

      ;; message_delta: stop reason
      ((string= "message_delta" (or event-type ""))
       (let ((delta (cdr (assoc :delta event))))
         (when delta
           (let ((stop-reason (cdr (assoc :stop--reason delta))))
             (when stop-reason
               (bt:with-lock-held ((stream-state-lock state))
                 (setf (stream-state-stop-reason state) stop-reason)))))))

      ;; message_stop: streaming complete
      ((string= "message_stop" (or event-type ""))
       (bt:with-lock-held ((stream-state-lock state))
         (setf (stream-state-done-p state) t))))))

(defun process-sse-event (data state)
  "Process a single SSE data payload (JSON string) and update STATE."
  (handler-case
      (process-stream-event (api-json-decode data) state)
    (error (e)
      (declare (ignore e))
      ;; Silently skip malformed SSE events
      nil)))

(defun read-sse-stream (stream state)
  "Read SSE events from STREAM and update STATE. Runs in a background thread."
  (handler-case
      (loop :with data-buffer := nil
            :for line := (read-line stream nil nil)
            :while line
            :do (let ((trimmed (string-trim '(#\Return) line)))
                  (cond
                    ;; Empty line = end of event, process accumulated data
                    ((zerop (length trimmed))
                     (when data-buffer
                       (process-sse-event
                        (format nil "~{~A~}" (nreverse data-buffer))
                        state)
                       (setf data-buffer nil)))
                    ;; Data line
                    (t
                     (multiple-value-bind (field value) (parse-sse-line trimmed)
                       (when (and field (string= "data" field))
                         (push value data-buffer)))))))
    (error (e)
      (bt:with-lock-held ((stream-state-lock state))
        (setf (stream-state-error-p state) (format nil "~A" e)
              (stream-state-done-p state) t))))
  ;; Ensure done is set
  (bt:with-lock-held ((stream-state-lock state))
    (setf (stream-state-done-p state) t)))

(defun anthropic-request-streaming (messages callback
                                     &key (model *anthropic-model*)
                                          (max-tokens 8192)
                                          tools)
  "Call the Anthropic Messages API with streaming enabled.
CALLBACK is called with (stream-state) on each update from the background thread.
Returns the final stream-state when complete."
  (declare (ignore callback))
  (let* ((token (or (read-token)
                    (error "No API token. Set CLAUDE_CODE_OAUTH_TOKEN env var, ~
                            run 'claude setup-token', or save to ~
                            ~~/.config/clawmacs/claude-max-token")))
         (request-body
           (let ((body `((:model . ,model)
                         (:max--tokens . ,max-tokens)
                         (:stream . t)
                         (:messages . ,(coerce messages 'vector)))))
             (when (and tools (plusp (length tools)))
               (push `(:tools . ,tools) body))
              ;; System prompt as array of content blocks — required for
              ;; Claude Max OAuth tokens (first block must be the CC prefix).
              (let* ((system-prompt (build-system-prompt))
                     (blocks (list `((:type . "text")
                                     (:text . ,*claude-code-system-prefix*)))))
                (when system-prompt
                  (setf blocks (append blocks
                                       (list `((:type . "text")
                                               (:text . ,system-prompt))))))
                (push `(:system . ,(coerce blocks 'vector)) body))
              (api-json-encode body)))
         (state (make-stream-state)))
    (multiple-value-bind (body-stream status-code headers)
        (drakma:http-request
         *anthropic-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers
         `(("Authorization" . ,(format nil "Bearer ~A" token))
           ("anthropic-version" . ,*anthropic-version*)
           ("anthropic-beta" . ,*anthropic-beta*))
         :content request-body
         :want-stream t)
      (declare (ignore headers))
      (unless (= status-code 200)
        (let ((err (if (streamp body-stream)
                       (let ((s (make-string-output-stream)))
                         (loop :for c := (read-char body-stream nil nil)
                               :while c :do (write-char c s))
                         (get-output-stream-string s))
                       (format nil "~A" body-stream))))
          (error "API error (~A): ~A" status-code err)))
      ;; Spawn background thread to read SSE events
       (bt:make-thread
        (lambda ()
          (unwind-protect
               (read-sse-stream body-stream state)
            (close body-stream)))
       :name "clawmacs-sse-reader")
       state)))

(defun stream-state-first-block (state)
  "Return STATE's current leading content block."
  (first (stream-state-content-blocks state)))

(defun ensure-openai-stream-text-block (state)
  "Ensure STATE has a current canonical text block."
  (let ((block (stream-state-first-block state)))
    (unless (and block (string= "text" (cdr (assoc :type block))))
      (push (canonical-text-block "") (stream-state-content-blocks state)))))

(defun openai-stream-tool-call-key (tool-call)
  "Return a stable key for a streamed OpenAI TOOL-CALL delta."
  (or (cdr (assoc :index tool-call))
      (cdr (assoc :id tool-call))
      (error "OpenAI streaming tool call missing index/id: ~S" tool-call)))

(defun upsert-openai-stream-tool-call (state tool-call)
  "Merge TOOL-CALL delta into STATE's OpenAI tool-call assembly tables."
  (let* ((key (openai-stream-tool-call-key tool-call))
         (function (cdr (assoc :function tool-call)))
         (existing (or (gethash key (stream-state-openai-tool-call-states state))
                       (list :id nil :name nil :arguments ""))))
    (unless (gethash key (stream-state-openai-tool-call-states state))
      (setf (stream-state-openai-tool-call-order state)
            (append (stream-state-openai-tool-call-order state) (list key))))
    (let ((updated (list :id (or (cdr (assoc :id tool-call))
                                 (getf existing :id))
                         :name (or (cdr (assoc :name function))
                                   (getf existing :name))
                         :arguments (let ((arguments (cdr (assoc :arguments function))))
                                      (if arguments
                                          (concatenate 'string
                                                       (getf existing :arguments)
                                                       arguments)
                                          (getf existing :arguments))))))
      (setf (gethash key (stream-state-openai-tool-call-states state)) updated)
      updated)))

(defun finalize-openai-stream-tool-blocks (state)
  "Finalize all assembled OpenAI streaming tool calls into canonical blocks."
  (let ((non-tool-blocks (remove-if (lambda (block)
                                      (string= "tool_use" (cdr (assoc :type block))))
                                    (stream-state-content-blocks state)))
        (tool-blocks
          (loop :for key :in (stream-state-openai-tool-call-order state)
                :for call-state := (gethash key (stream-state-openai-tool-call-states state))
                :collect (canonical-tool-use-block
                          (getf call-state :id)
                          (getf call-state :name)
                          (let ((arguments (getf call-state :arguments)))
                            (when (plusp (length arguments))
                              (handler-case
                                  (api-json-decode arguments)
                                (error () nil))))))))
    (let ((display-blocks (append (nreverse (copy-list non-tool-blocks))
                                  tool-blocks)))
      (setf (stream-state-content-blocks state)
            (nreverse display-blocks)))))

(defun process-openai-sse-event (data state)
  "Process a single OpenAI SSE DATA payload into STATE."
  (cond
    ((string= data "[DONE]")
     (bt:with-lock-held ((stream-state-lock state))
       (finalize-openai-stream-tool-blocks state)
       (when (plusp (length (stream-state-text state)))
         (ensure-openai-stream-text-block state)
         (setf (first (stream-state-content-blocks state))
               (canonical-text-block (stream-state-text state))))
       (unless (stream-state-stop-reason state)
         (setf (stream-state-stop-reason state) "end_turn"))
       (setf (stream-state-done-p state) t)))
    (t
     (let* ((event (api-json-decode data))
            (choice (first (coerce (cdr (assoc :choices event)) 'list)))
            (delta (and choice (cdr (assoc :delta choice))))
            (text (and delta (cdr (assoc :content delta))))
            (reasoning (and delta (cdr (assoc :reasoning--content delta))))
            (tool-calls (and delta (cdr (assoc :tool--calls delta))))
            (finish-reason (and choice (cdr (assoc :finish--reason choice))))
            ;; Use whichever text field is present in this chunk.
            ;; Reasoning models (Z.AI GLM, DeepSeek R1) stream
            ;; reasoning_content first, then content.
            (effective-text (or text reasoning)))
        (bt:with-lock-held ((stream-state-lock state))
          (when effective-text
            (ensure-openai-stream-text-block state)
            (setf (stream-state-text state)
                  (concatenate 'string (stream-state-text state) effective-text)
                  (first (stream-state-content-blocks state))
                  (canonical-text-block (stream-state-text state))))
          (dolist (tool-call (coerce (or tool-calls #()) 'list))
            (upsert-openai-stream-tool-call state tool-call))
          (when finish-reason
            (when (or tool-calls
                      (stream-state-openai-tool-call-order state))
              (finalize-openai-stream-tool-blocks state))
            (setf (stream-state-stop-reason state)
                  (openai-finish-reason->stop-reason finish-reason))))))))

(defun read-openai-sse-stream (stream state)
  "Read OpenAI SSE events from STREAM into STATE."
  (handler-case
      (loop :with data-buffer := nil
            :for line := (read-line stream nil nil)
            :while line
            :do (let ((trimmed (string-trim '(#\Return) line)))
                  (cond
                    ((zerop (length trimmed))
                     (when data-buffer
                       (process-openai-sse-event
                        (format nil "~{~A~}" (nreverse data-buffer))
                        state)
                       (setf data-buffer nil)))
                    (t
                     (multiple-value-bind (field value) (parse-sse-line trimmed)
                       (when (and field (string= "data" field))
                         (push value data-buffer)))))))
    (error (e)
      (bt:with-lock-held ((stream-state-lock state))
        (setf (stream-state-error-p state) (format nil "~A" e)
              (stream-state-done-p state) t))))
  (bt:with-lock-held ((stream-state-lock state))
    (setf (stream-state-done-p state) t)))

(defun openai-codex-request-streaming (messages callback
                                       &key (model *openai-codex-model*)
                                            (max-tokens 8192)
                                            tools)
  "Call OpenAI Chat Completions with SSE streaming enabled."
  (declare (ignore callback))
  (let* ((token (or (read-provider-token :openai-codex)
                    (error 'simple-error
                           :format-control "No API token. Save to ~/.config/clawmacs/openai-codex-token")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--completion--tokens . ,max-tokens)
                          (:stream . t)
                         (:messages . ,(coerce (openai-messages-with-system-prompt messages)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(anthropic-tools->openai-tools tools)) body))
              (api-json-encode body)))
         (state (make-stream-state)))
    (multiple-value-bind (body-stream status-code headers)
        (drakma:http-request
         *openai-codex-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token)))
         :content request-body
         :want-stream t)
      (declare (ignore headers))
      (unless (= status-code 200)
        (let ((err (if (streamp body-stream)
                       (let ((s (make-string-output-stream)))
                         (loop :for c := (read-char body-stream nil nil)
                               :while c :do (write-char c s))
                         (get-output-stream-string s))
                       (format nil "~A" body-stream))))
          (error "API error (~A): ~A" status-code err)))
      (bt:make-thread
       (lambda ()
         (unwind-protect
              (read-openai-sse-stream body-stream state)
           (close body-stream)))
       :name "clawmacs-openai-sse-reader")
      state)))

;;; --------------------------------------------------------------------------
;;; OpenRouter API — OpenAI-compatible
;;; --------------------------------------------------------------------------
;;; OpenRouter proxies requests to 300+ models (OpenAI, Anthropic, Google,
;;; Meta, DeepSeek, etc.) through a single OpenAI-compatible endpoint.
;;; Authentication uses a Bearer API key obtained from openrouter.ai/keys.
;;; Model names follow the 'provider/model-name' format.

(defun openrouter-request (messages &key (model *openrouter-model*)
                                          (max-tokens 8192)
                                          tools)
  "Call the OpenRouter Chat Completions API and normalize the response shape.
Uses the OpenAI-compatible chat completions protocol."
  (let* ((token (or (read-provider-token :openrouter)
                    (error 'simple-error
                           :format-control "No OpenRouter API key. Set OPENROUTER_API_KEY env var or save to ~/.config/clawmacs/openrouter-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                         (:messages . ,(coerce (openai-messages-with-system-prompt messages)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(anthropic-tools->openai-tools tools)) body))
              (api-json-encode body))))
    (multiple-value-bind (body status-code)
        (drakma:http-request
         *openrouter-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                               ("HTTP-Referer" . "https://github.com/clawmacs/clawmacs")
                               ("X-Title" . "clawmacs"))
         :content request-body
         :want-stream nil
         :force-binary nil)
      (let ((body-string (http-body-string body)))
        (unless (= status-code 200)
          (error "OpenRouter API error (~A): ~A" status-code body-string))
        (let* ((response (api-json-decode body-string))
               (choices (cdr (assoc :choices response)))
               (choice (first (coerce choices 'list))))
          (unless choice
            (error "OpenRouter response did not include a choice"))
          (openai-choice->canonical-response choice))))))

(defun openrouter-request-streaming (messages callback
                                     &key (model *openrouter-model*)
                                          (max-tokens 8192)
                                          tools)
  "Call the OpenRouter Chat Completions API with SSE streaming enabled.
Uses the same OpenAI-compatible streaming protocol."
  (declare (ignore callback))
  (let* ((token (or (read-provider-token :openrouter)
                    (error 'simple-error
                           :format-control "No OpenRouter API key. Set OPENROUTER_API_KEY env var or save to ~/.config/clawmacs/openrouter-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                          (:stream . t)
                         (:messages . ,(coerce (openai-messages-with-system-prompt messages)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(anthropic-tools->openai-tools tools)) body))
              (api-json-encode body)))
         (state (make-stream-state)))
    (multiple-value-bind (body-stream status-code headers)
        (drakma:http-request
         *openrouter-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                               ("HTTP-Referer" . "https://github.com/clawmacs/clawmacs")
                               ("X-Title" . "clawmacs"))
         :content request-body
         :want-stream t)
      (declare (ignore headers))
      (unless (= status-code 200)
        (let ((err (if (streamp body-stream)
                       (let ((s (make-string-output-stream)))
                         (loop :for c := (read-char body-stream nil nil)
                               :while c :do (write-char c s))
                         (get-output-stream-string s))
                       (format nil "~A" body-stream))))
          (error "OpenRouter API error (~A): ~A" status-code err)))
      (bt:make-thread
       (lambda ()
         (unwind-protect
              (read-openai-sse-stream body-stream state)
           (close body-stream)))
       :name "clawmacs-openrouter-sse-reader")
      state)))

;;; --------------------------------------------------------------------------
;;; Z.AI (Zhipu AI) API — OpenAI-compatible
;;; --------------------------------------------------------------------------

(defun zai-request (messages &key (model *zai-model*)
                                   (max-tokens 8192)
                                   tools)
  "Call Z.AI Chat Completions API and normalize the response shape.
Uses the coding plan endpoint (api.z.ai/api/coding/paas/v4) which is
compatible with the GLM Coding Max-Monthly subscription.
The API follows the OpenAI Chat Completions format."
  (let* ((token (or (read-provider-token :zai)
                    (error 'simple-error
                           :format-control "No Z.AI API key. Set ZAI_CODING_MAX_API_KEY env var or save to ~/.config/clawmacs/zai-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                         (:messages . ,(coerce (openai-messages-with-system-prompt messages)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(anthropic-tools->openai-tools tools)) body))
              (api-json-encode body))))
    (multiple-value-bind (body status-code)
        (drakma:http-request
         *zai-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                               ("Accept-Language" . "en-US,en"))
         :content request-body
         :want-stream nil
         :force-binary nil)
      (let ((body-string (http-body-string body)))
        (unless (= status-code 200)
          (error "Z.AI API error (~A): ~A" status-code body-string))
        (let* ((response (api-json-decode body-string))
               (choices (cdr (assoc :choices response)))
               (choice (first (coerce choices 'list))))
          (unless choice
            (error "Z.AI response did not include a choice"))
          (openai-choice->canonical-response choice))))))

(defun zai-request-streaming (messages callback
                               &key (model *zai-model*)
                                    (max-tokens 8192)
                                    tools)
  "Call Z.AI Chat Completions API with SSE streaming enabled.
Uses the same OpenAI-compatible streaming protocol."
  (declare (ignore callback))
  (let* ((token (or (read-provider-token :zai)
                    (error 'simple-error
                           :format-control "No Z.AI API key. Set ZAI_CODING_MAX_API_KEY env var or save to ~/.config/clawmacs/zai-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                          (:stream . t)
                         (:messages . ,(coerce (openai-messages-with-system-prompt messages)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(anthropic-tools->openai-tools tools)) body))
              (api-json-encode body)))
         (state (make-stream-state)))
    (multiple-value-bind (body-stream status-code headers)
        (drakma:http-request
         *zai-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                               ("Accept-Language" . "en-US,en"))
         :content request-body
         :want-stream t)
      (declare (ignore headers))
      (unless (= status-code 200)
        (let ((err (if (streamp body-stream)
                       (let ((s (make-string-output-stream)))
                         (loop :for c := (read-char body-stream nil nil)
                               :while c :do (write-char c s))
                         (get-output-stream-string s))
                       (format nil "~A" body-stream))))
          (error "Z.AI API error (~A): ~A" status-code err)))
      (bt:make-thread
       (lambda ()
         (unwind-protect
              (read-openai-sse-stream body-stream state)
           (close body-stream)))
       :name "clawmacs-zai-sse-reader")
      state)))

;;; --------------------------------------------------------------------------
;;; Response Parsing Helpers
;;; --------------------------------------------------------------------------

(defun response-stop-reason (response)
  "Extract stop_reason from an API response."
  (cdr (assoc :stop--reason response)))

(defun response-content (response)
  "Extract content blocks from an API response as a list."
  (let ((content (cdr (assoc :content response))))
    (coerce content 'list)))

(defun content-block-type (block)
  "Return the type string of a content block."
  (cdr (assoc :type block)))

(defun content-text-blocks (content-blocks)
  "Extract and concatenate all text from text-type content blocks."
  (with-output-to-string (s)
    (let ((first t))
      (dolist (block content-blocks)
        (when (string= "text" (content-block-type block))
          (unless first (write-char #\Newline s))
          (write-string (cdr (assoc :text block)) s)
          (setf first nil))))))

(defun content-tool-use-blocks (content-blocks)
  "Extract tool_use blocks from content."
  (remove-if-not (lambda (b) (string= "tool_use" (content-block-type b)))
                 content-blocks))

(defun format-tool-call-display (tool-use-block)
  "Format a tool_use block for display in the chat."
  (let ((name (cdr (assoc :name tool-use-block)))
        (input (cdr (assoc :input tool-use-block))))
    (format nil "[tool: ~A ~A]" name
            (with-output-to-string (s)
              (loop :for (k . v) :in input
                    :for first := t :then nil
                    :do (unless first (write-string " " s))
                        (format s "~A=~S"
                                (string-downcase (symbol-name k)) v))))))

(defun format-tool-result-display (tool-name result-text)
  "Format a tool result for display in the chat."
  (let ((preview (if (> (length result-text) 200)
                     (concatenate 'string (subseq result-text 0 200) "...")
                     result-text)))
    (format nil "[~A result: ~A]" tool-name preview)))
