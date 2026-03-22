(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Configuration
;;; --------------------------------------------------------------------------

(defvar *anthropic-model* "claude-haiku-4-5-20251001"
  "The Anthropic model to use for chat completions.")

(defvar *anthropic-api-url* "https://api.anthropic.com/v1/messages"
  "The Anthropic Messages API endpoint.")

(defvar *anthropic-version* "2023-06-01"
  "The Anthropic API version header value.")

(defvar *anthropic-beta* "interleaved-thinking-2025-05-14,oauth-2025-04-20"
  "Beta features header for OAuth token authentication and extended thinking.")

(defvar *openai-codex-model* "codex-mini-latest"
  "The OpenAI Codex model to use for chat completions.")

(defvar *openai-codex-api-url* "https://api.openai.com/v1/chat/completions"
  "The OpenAI Chat Completions API endpoint for Codex models.")

(defvar *system-prompt-path*
  (merge-pathnames #P".config/clawmacs/system-prompt.txt" (user-homedir-pathname))
  "Path to an optional system prompt file.")

(defvar *agent-defaults-path*
  (merge-pathnames #P".config/clawmacs/agent-defaults.json" (user-homedir-pathname))
  "Path to the persisted agent defaults registry.")

(defvar *agent-defaults-registry* nil
  "Memoized agent defaults registry.")

(defvar *system-prompt*
  "You are a helpful assistant running inside clawmacs, a Lisp-native terminal chat interface. You have access to tools for fetching URLs, reading/writing files, running shell commands, and evaluating Common Lisp code. Be concise and direct in your responses."
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
     (otherwise
      (error "Unknown provider ~S. Supported providers: :ANTHROPIC, :OPENAI-CODEX"
             provider)))
   (user-homedir-pathname)))

(defvar *claude-code-credentials-path*
  (merge-pathnames #P".claude/.credentials.json" (user-homedir-pathname))
  "Path to Claude Code's OAuth credentials file.")

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
  "Read PROVIDER's token, preferring Claude Code's live OAuth credentials
for :ANTHROPIC, falling back to the provider-specific token file."
  (or (when (eq provider :anthropic)
        (read-claude-code-oauth-token))
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

(defparameter *provider-fallback-models*
  '((:anthropic . "claude-haiku-4-5-20251001")
    (:openai-codex . "codex-mini-latest"))
  "Built-in fallback model names by provider.")

(defun known-provider-p (provider)
  "Return non-nil when PROVIDER is supported locally."
  (member provider '(:anthropic :openai-codex) :test #'eq))

(defun normalize-provider (provider)
  "Normalize PROVIDER to a supported keyword, or nil when absent."
  (cond
    ((null provider) nil)
    ((keywordp provider)
     (if (known-provider-p provider)
         provider
         (error "Unknown provider ~S. Supported providers: :ANTHROPIC, :OPENAI-CODEX"
                provider)))
    ((stringp provider)
     (normalize-provider (intern (string-upcase provider) :keyword)))
    ((symbolp provider)
     (normalize-provider (symbol-name provider)))
    (t
     (error "Unknown provider ~S. Supported providers: :ANTHROPIC, :OPENAI-CODEX"
            provider))))

(defun blank-string-p (value)
  "Return non-nil when VALUE is nil or all whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))

(defun provider-fallback-model (provider)
  "Return the built-in fallback model for PROVIDER."
  (cdr (assoc provider *provider-fallback-models*)))

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
  "Return AGENT-NAME's default provider, or the built-in fallback."
  (or (getf (agent-default-spec agent-name)
            :provider)
      :anthropic))

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
  "Resolve BUF's effective provider and model using overrides and defaults."
  (ensure-agent-defaults-loaded)
  (let* ((provider (or (buffer-provider-override buf)
                       (agent-default (buffer-agent-name buf))
                       :anthropic))
         (resolved-provider (normalize-provider provider))
         (model (or (buffer-model-override buf)
                    (agent-default-model (buffer-agent-name buf) resolved-provider)
                    (provider-fallback-model resolved-provider))))
    (when (blank-string-p model)
      (error "Resolved model must be a non-empty string"))
    (values resolved-provider model)))

;;; --------------------------------------------------------------------------
;;; Conversation Building
;;; --------------------------------------------------------------------------

(defun build-conversation-messages (buf)
  "Build the Anthropic API messages array from the buffer's chat history.
Uses raw-content when available (for tool_use/tool_result messages),
falls back to plain text content."
  (let ((messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :do (let* ((sender (message-sender msg))
                     (role (cond
                             ((eq sender :user) "user")
                             ((eq sender :tool-result) "user")
                             (t "assistant")))
                     (content (canonicalize-message-content
                               role
                               (or (message-raw-content msg)
                                   (message-text msg)))))
                (push `((:role . ,role)
                        (:content . ,(coerce content 'vector)))
                      messages)))
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
  "Normalize an OpenAI completion CHOICE to canonical response shape."
  (let* ((message (cdr (assoc :message choice)))
         (content-blocks nil)
         (text (cdr (assoc :content message)))
         (tool-calls (cdr (assoc :tool--calls message))))
    (unless (blank-string-p text)
      (push (canonical-text-block text) content-blocks))
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
  "Dispatch a non-streaming request by resolved PROVIDER."
  (ecase provider
    (:anthropic
     (anthropic-request messages :model model :max-tokens max-tokens :tools tools))
    (:openai-codex
     (openai-codex-request messages :model model :max-tokens max-tokens :tools tools))))

(defun provider-request-streaming (provider messages callback &key model (max-tokens 8192) tools)
  "Dispatch a streaming request by resolved PROVIDER."
  (ecase provider
    (:anthropic
     (anthropic-request-streaming messages callback
                                  :model model
                                  :max-tokens max-tokens
                                  :tools tools))
    (:openai-codex
     (openai-codex-request-streaming messages callback
                                     :model model
                                     :max-tokens max-tokens
                                     :tools tools))))

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
                    (error "No API token. Run 'claude setup-token', save to ~
                            ~~/.config/clawmacs/claude-max-token")))
         (request-body
           (let ((body `((:model . ,model)
                         (:max--tokens . ,max-tokens)
                         (:messages . ,(coerce messages 'vector)))))
             (when (and tools (plusp (length tools)))
               (push `(:tools . ,tools) body))
             (let ((system-prompt (build-system-prompt)))
               (when system-prompt
                 (push `(:system . ,system-prompt) body)))
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

(defun process-sse-event (data state)
  "Process a single SSE data payload (JSON string) and update STATE."
  (handler-case
      (let* ((event (api-json-decode data))
             (event-type (cdr (assoc :type event))))
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
             (setf (stream-state-done-p state) t)))))
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
                    (error "No API token. Run 'claude setup-token', save to ~
                            ~~/.config/clawmacs/claude-max-token")))
         (request-body
           (let ((body `((:model . ,model)
                         (:max--tokens . ,max-tokens)
                         (:stream . t)
                         (:messages . ,(coerce messages 'vector)))))
             (when (and tools (plusp (length tools)))
               (push `(:tools . ,tools) body))
              (let ((system-prompt (build-system-prompt)))
                (when system-prompt
                  (push `(:system . ,system-prompt) body)))
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
            (tool-calls (and delta (cdr (assoc :tool--calls delta))))
            (finish-reason (and choice (cdr (assoc :finish--reason choice)))))
        (bt:with-lock-held ((stream-state-lock state))
          (when text
            (ensure-openai-stream-text-block state)
            (setf (stream-state-text state)
                  (concatenate 'string (stream-state-text state) text)
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
