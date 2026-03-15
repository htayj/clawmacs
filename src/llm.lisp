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
