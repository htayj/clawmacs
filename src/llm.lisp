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
  ;; Try to restrict permissions (best effort)
  (ignore-errors
    (uiop:run-program (list "chmod" "600" (namestring *token-path*))))
  token)

;;; --------------------------------------------------------------------------
;;; Conversation Building
;;; --------------------------------------------------------------------------

(defun build-conversation-messages (buf)
  "Build the Anthropic API messages array from the buffer's chat history.
Returns a list of alists with :role and :content keys."
  (let ((messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :do (let ((role (if (eq :user (message-sender msg))
                              "user"
                              "assistant"))
                    (text (message-text msg)))
                (push `((:role . ,role) (:content . ,text)) messages)))
    (nreverse messages)))

;;; --------------------------------------------------------------------------
;;; API Call
;;; --------------------------------------------------------------------------

(defun anthropic-chat (messages &key (model *anthropic-model*)
                                     (max-tokens 8192))
  "Call the Anthropic Messages API with MESSAGES.
Returns the assistant's response text.
MESSAGES is a list of alists with :role and :content keys."
  (let* ((token (or (read-token)
                    (error "No API token found. Run 'claude setup-token' then ~
                            save the token to ~~/.config/clawmacs/token ~
                            or set ANTHROPIC_OAUTH_TOKEN env var.")))
         (request-body
           (cl-json:encode-json-to-string
            `((:model . ,model)
              (:max--tokens . ,max-tokens)
              (:messages . ,(coerce messages 'vector)))))
         (response
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
                 (error "Anthropic API error (~A): ~A" status-code body-string))
               (cl-json:decode-json-from-string body-string)))))
    ;; Extract text from response
    ;; Format: {"content": [{"type": "text", "text": "..."}], ...}
    (let* ((content (cdr (assoc :content response)))
           (first-block (when content (elt content 0)))
           (text (when first-block (cdr (assoc :text first-block)))))
      (or text
          (error "No text in API response: ~A" response)))))
