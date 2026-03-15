(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Configuration
;;; --------------------------------------------------------------------------

(defvar *anthropic-model* "claude-sonnet-4-20250514"
  "The Anthropic model to use for chat completions.")

(defvar *anthropic-api-url* "https://api.anthropic.com/v1/messages"
  "The Anthropic Messages API endpoint.")

(defvar *anthropic-version* "2023-06-01"
  "The Anthropic API version header value.")

(defvar *anthropic-beta* "claude-code-20250219,oauth-2025-04-20"
  "Beta features header for OAuth token authentication.")

(defvar *token-path*
  (merge-pathnames #P".config/clawmacs/token" (user-homedir-pathname))
  "Path to the stored setup-token file.")

(defvar *system-prompt-path*
  (merge-pathnames #P".config/clawmacs/system-prompt.txt" (user-homedir-pathname))
  "Path to an optional system prompt file.")

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

;;; --------------------------------------------------------------------------
;;; Token Management
;;; --------------------------------------------------------------------------

(defun read-token ()
  "Read the Anthropic OAuth token. Checks in order:
1. Environment variable ANTHROPIC_OAUTH_TOKEN
2. Token file at ~/.config/clawmacs/token
Returns the token string or nil if not found."
  (or (uiop:getenv "ANTHROPIC_OAUTH_TOKEN")
      (when (probe-file *token-path*)
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (uiop:read-file-string *token-path*)))))

(defun save-token (token)
  "Save TOKEN to ~/.config/clawmacs/token."
  (ensure-directories-exist *token-path*)
  (with-open-file (s *token-path*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-string token s))
  (ignore-errors
    (uiop:run-program (list "chmod" "600" (namestring *token-path*))))
  token)

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
                     (content (if (message-raw-content msg)
                                  ;; Structured content (tool_use or tool_result)
                                  (coerce (message-raw-content msg) 'vector)
                                  ;; Plain text
                                  (message-text msg))))
                (push `((:role . ,role) (:content . ,content)) messages)))
    (nreverse messages)))

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
                            ~~/.config/clawmacs/token")))
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
      (let ((body-string (if (stringp body)
                             body
                             (flexi-streams:octets-to-string
                              body :external-format :utf-8))))
        (unless (= status-code 200)
          (error "API error (~A): ~A" status-code body-string))
        (api-json-decode body-string)))))

;;; --------------------------------------------------------------------------
;;; Streaming API Call
;;; --------------------------------------------------------------------------

(defstruct stream-state
  "Mutable state for an in-progress streaming response."
  (text ""             :type string)
  (content-blocks nil  :type list)
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
                 (push block (stream-state-content-blocks state))))))

          ;; content_block_delta: incremental text
          ((string= "content_block_delta" (or event-type ""))
           (let* ((delta (cdr (assoc :delta event)))
                  (delta-type (cdr (assoc :type delta))))
             (when (string= "text_delta" (or delta-type ""))
               (let ((text (cdr (assoc :text delta))))
                 (when text
                   (bt:with-lock-held ((stream-state-lock state))
                     (setf (stream-state-text state)
                           (concatenate 'string (stream-state-text state) text))))))))

          ;; content_block_stop: block finished
          ((string= "content_block_stop" (or event-type ""))
           ;; Update the last content block with accumulated text if it's a text block
           (bt:with-lock-held ((stream-state-lock state))
             (let ((blocks (stream-state-content-blocks state)))
               (when (and blocks
                          (string= "text" (or (cdr (assoc :type (first blocks))) "")))
                 (setf (first blocks)
                       (acons :text (stream-state-text state)
                              (remove :text (first blocks) :key #'car)))))))

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
  (let* ((token (or (read-token)
                    (error "No API token. Run 'claude setup-token', save to ~
                            ~~/.config/clawmacs/token")))
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
