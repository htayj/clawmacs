(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Self-insert support (must be defined before commands that reference it)
;;; --------------------------------------------------------------------------

(defvar *self-insert-char* nil
  "The character to insert for self-insert-command. Bound by the event loop.")

;;; --------------------------------------------------------------------------
;;; Agent
;;; --------------------------------------------------------------------------

(defun insert-agent-message-from-content (buf content-blocks agent-kw)
  "Insert an agent message from API content blocks.
Shows text parts and tool call summaries. Stores raw-content for round-trip."
  (let* ((text-parts (content-text-blocks content-blocks))
          (tool-uses (content-tool-use-blocks content-blocks))
          (canonical-content (canonicalize-message-content "assistant" content-blocks))
          (display (with-output-to-string (s)
                     (when (plusp (length text-parts))
                       (write-string text-parts s))
                     (dolist (tu tool-uses)
                       (when (plusp (length text-parts))
                         (write-char #\Newline s))
                       (write-string (format-tool-call-display tu) s))))
          (agent-msg (buffer-insert-agent-message buf (string-trim '(#\Space #\Tab #\Newline #\Return) display))))
    (setf (message-raw-content agent-msg) canonical-content)
    (setf (message-face-set agent-msg)
          (gethash agent-kw (buffer-face-registry buf)))
    agent-msg))

(defun begin-tool-approval (buf tool-use-blocks)
  "Start the tool approval process. Stashes user input and presents
the first tool needing permission, or auto-executes allowed tools.
Called from update-streaming-response when tool calls are detected."
  ;; Stash current input
  (setf (buffer-stashed-input buf) (message-text (buffer-input-message buf)))
  ;; Queue all tool calls for processing
  (setf (buffer-pending-tool-calls buf) (copy-list tool-use-blocks))
  (setf (buffer-tool-call-results buf) nil)
  ;; Process the first tool (or all auto-approved ones)
  (advance-tool-approval buf))

(defun advance-tool-approval (buf)
  "Process the next pending tool call. Auto-executes :agent-allowed tools,
shows approval prompt for :agent-with-permission tools.
When all tools are done, finalizes the results."
  (let ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword)))
    (loop :while (buffer-pending-tool-calls buf)
          :for tu := (first (buffer-pending-tool-calls buf))
          :for tool-name := (cdr (assoc :name tu))
          :for tool-input := (cdr (assoc :input tu))
          :for tool-id := (cdr (assoc :id tu))
          :do (cond
                ;; Tool needs permission: show approval prompt and return
                ((tool-requires-permission-p tool-name)
                 (let* ((raw-sexpr (format-tool-call-sexpr tool-name tool-input))
                        (expanded (format-tool-call-expanded tool-name tool-input))
                        (extra (tool-approval-extra-display tool-name tool-input)))
                   (setf (buffer-approval-pending buf)
                         `((:tool-name . ,tool-name)
                           (:tool-id . ,tool-id)
                           (:tool-input . ,tool-input)
                           (:display-raw . ,raw-sexpr)
                           (:display-expanded . ,expanded)
                           ,@(when extra `((:display-extra . ,extra)))
                           (:tool-use-block . ,tu)))
                   ;; Clear input area for the prompt
                   (set-message-text (buffer-input-message buf) "")
                   (setf (buffer-status buf) :approval)
                   (return)))  ; exit loop, wait for user
                ;; Tool is auto-approved: execute immediately
                (t
                 (let* ((*current-caller* agent-kw)
                        (result-text
                          (handler-case
                              (execute-tool tool-name tool-input)
                            (error (e)
                              (api-json-encode `((:error . ,(format nil "~A" e))))))))
                   (push `((:result . ,result-text)
                           (:display . ,(format-tool-result-display tool-name result-text))
                           (:tool-id . ,tool-id))
                         (buffer-tool-call-results buf)))
                 (pop (buffer-pending-tool-calls buf)))))
    ;; If no more pending tools, finalize
    (unless (buffer-pending-tool-calls buf)
      (finalize-tool-results buf))))

(defun handle-approval-response (buf response)
  "Handle user's approval decision for the current pending tool.
RESPONSE is :approve, :deny, or (:deny-with-message . \"reason\")."
  (let* ((approval (buffer-approval-pending buf))
         (tool-name (cdr (assoc :tool-name approval)))
         (tool-id (cdr (assoc :tool-id approval)))
         (tool-input (cdr (assoc :tool-input approval)))
         (agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword)))
    ;; Clear approval state
    (setf (buffer-approval-pending buf) nil)
    (cond
      ;; Approved: execute the tool
      ((eq response :approve)
       (let* ((*current-caller* agent-kw)
              (result-text
                (handler-case
                    (execute-tool tool-name tool-input)
                  (error (e)
                    (api-json-encode `((:error . ,(format nil "~A" e))))))))
         (push `((:result . ,result-text)
                 (:display . ,(format-tool-result-display tool-name result-text))
                 (:tool-id . ,tool-id))
               (buffer-tool-call-results buf))))
      ;; Denied with message
      ((and (consp response) (eq (car response) :deny-with-message))
       (let ((reason (cdr response)))
         (push `((:result . ,(api-json-encode
                              `((:denied . t)
                                (:reason . ,(or reason "User denied this tool call")))))
                 (:display . ,(format nil "[~A DENIED: ~A]" tool-name (or reason "denied")))
                 (:tool-id . ,tool-id))
               (buffer-tool-call-results buf))))
      ;; Denied (no message)
      (t
       (push `((:result . ,(api-json-encode `((:denied . t) (:reason . "User denied"))))
               (:display . ,(format nil "[~A DENIED]" tool-name))
               (:tool-id . ,tool-id))
             (buffer-tool-call-results buf))))
    ;; Move to next tool
    (pop (buffer-pending-tool-calls buf))
    ;; Restore stashed input
    (when (buffer-stashed-input buf)
      (set-message-text (buffer-input-message buf) (buffer-stashed-input buf))
      (setf (buffer-stashed-input buf) nil))
    ;; Continue with remaining tools
    (advance-tool-approval buf)))

(defun finalize-tool-results (buf)
  "Insert the accumulated tool results as a message and continue the conversation."
  (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (results (nreverse (buffer-tool-call-results buf)))
         (display-parts (mapcar (lambda (r) (cdr (assoc :display r))) results))
          (result-blocks (mapcar (lambda (r)
                                   `((:type . "tool_result")
                                     (:tool--use--id . ,(cdr (assoc :tool-id r)))
                                     (:content . ,(cdr (assoc :result r)))))
                                 results))
          (canonical-result-blocks (canonicalize-message-content "user" result-blocks))
          (display-text (format nil "~{~A~^~%~}" display-parts))
          (tr-msg (make-message :tool-result :read-only-p t))
          (input (buffer-input-message buf)))
    (set-message-text tr-msg display-text)
    (setf (message-raw-content tr-msg) canonical-result-blocks)
    (setf (message-timestamp tr-msg) (get-universal-time))
    (setf (message-face-set tr-msg)
          (gethash agent-kw (buffer-face-registry buf)))
    ;; Link into buffer before input
    (let ((before-input (message-prev input)))
      (setf (message-prev tr-msg) before-input
            (message-next tr-msg) input
            (message-prev input) tr-msg)
      (if before-input
          (setf (message-next before-input) tr-msg)
          (setf (buffer-first-message buf) tr-msg)))
    ;; Clear tool call state
    (setf (buffer-tool-call-results buf) nil
          (buffer-pending-tool-calls buf) nil)
    ;; Restore stashed input if still stashed
    (when (buffer-stashed-input buf)
      (set-message-text (buffer-input-message buf) (buffer-stashed-input buf))
      (setf (buffer-stashed-input buf) nil))
    ;; Continue: start next streaming request
    (start-streaming-response buf)))

(defun start-streaming-response (buf)
  "Start a streaming API call. Creates a placeholder agent message and
stores the stream state on the buffer. Non-blocking -- the event loop
polls for updates via update-streaming-response."
  (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (tools (let ((*current-caller* agent-kw))
                   (tool-definitions-for-api)))
         (messages (build-conversation-messages buf)))
    (handler-case
        (multiple-value-bind (provider model)
            (resolve-buffer-provider-and-model buf)
          ;; Debug: echo the outgoing request payload before sending
          (let ((req-json (api-json-encode
                           `((:messages . ,(coerce messages 'vector))
                             ,@(when (and tools (plusp (length tools)))
                                 `((:tools . ,tools)))))))
            (debug-log buf
              (format nil "[API REQUEST → ~(~A~)/~A  msg:~D  tools:~D]~%~A"
                      provider model
                      (length messages)
                      (if tools (length tools) 0)
                      req-json))
            (file-debug-log "api-request"
                            "provider=~(~A~) model=~A msgs=~D tools=~D payload=~A"
                            provider model
                            (length messages)
                            (if tools (length tools) 0)
                            req-json))
          (let* ((state (provider-request-streaming
                       provider
                       messages
                       (lambda (s) (declare (ignore s)))
                       :model model
                       :tools tools))
                 ;; Create placeholder message that will be updated as tokens arrive
                 (agent-msg (buffer-insert-agent-message buf "")))
          (setf (message-face-set agent-msg)
                (gethash agent-kw (buffer-face-registry buf)))
          (setf (buffer-pending-stream buf) state
                (buffer-streaming-message buf) agent-msg
                (buffer-status buf) :thinking)))
      (error (e)
        (setf (buffer-status buf) :error)
        (let ((err-msg (buffer-insert-agent-message
                         buf (format nil "[Error: ~A]" e))))
          (setf (message-face-set err-msg)
                (gethash agent-kw (buffer-face-registry buf))))))))

(defun update-streaming-response (buf)
  "Poll the active streaming response and update the display.
Returns T if still streaming, NIL if done."
  (let ((state (buffer-pending-stream buf))
        (msg (buffer-streaming-message buf)))
    (unless (and state msg)
      (return-from update-streaming-response nil))
    ;; Read state under lock
    (let ((in-progress-text (bt:with-lock-held ((stream-state-lock state))
                              (stream-state-text state)))
          (done (bt:with-lock-held ((stream-state-lock state))
                  (stream-state-done-p state)))
          (err (bt:with-lock-held ((stream-state-lock state))
                 (stream-state-error-p state))))
      ;; While streaming: update display with in-progress text
      ;; (stream-state-text accumulates the CURRENT block's text;
      ;; completed blocks have their text finalized in content-blocks)
      (unless done
        (let ((all-text (bt:with-lock-held ((stream-state-lock state))
                          ;; Collect text from completed blocks + current accumulator
                          (let ((completed (content-text-blocks
                                            (reverse (stream-state-content-blocks state)))))
                            (if (plusp (length in-progress-text))
                                (concatenate 'string completed in-progress-text)
                                completed)))))
          (when (plusp (length all-text))
            (set-message-text msg (string-trim '(#\Space #\Tab #\Newline #\Return) all-text)))))
      (cond
        ;; Error during streaming
        (err
         (set-message-text msg (format nil "[Streaming error: ~A]" err))
         (setf (buffer-pending-stream buf) nil
               (buffer-streaming-message buf) nil
               (buffer-status buf) :error)
         nil)
        ;; Streaming complete
        (done
         (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
                 (content-blocks
                   (bt:with-lock-held ((stream-state-lock state))
                     (nreverse (copy-list (stream-state-content-blocks state)))))
                 (canonical-content (canonicalize-message-content "assistant" content-blocks))
                 (stop-reason
                   (bt:with-lock-held ((stream-state-lock state))
                     (stream-state-stop-reason state)))
                 (tool-uses (content-tool-use-blocks canonical-content))
                 (final-text (content-text-blocks canonical-content)))
            ;; Build final display from content-blocks (not the accumulator)
            (let ((display (with-output-to-string (s)
                             (write-string final-text s)
                             (dolist (tu tool-uses)
                               (write-char #\Newline s)
                               (write-string (format-tool-call-display tu) s)))))
              (set-message-text msg (string-trim '(#\Space #\Tab #\Newline #\Return) display))
              (setf (message-raw-content msg) canonical-content))
            ;; Debug: echo the completed response
            (let ((resp-json (api-json-encode (coerce canonical-content 'vector))))
              (debug-log buf
                (format nil "[API RESPONSE  stop:~A  blocks:~D]~%~A"
                        (or stop-reason "nil")
                        (length content-blocks)
                        resp-json))
              (file-debug-log "api-response"
                              "stop=~A blocks=~D content=~A"
                              (or stop-reason "nil")
                              (length content-blocks)
                              resp-json))
            ;; Clear streaming state
            (setf (buffer-pending-stream buf) nil
                  (buffer-streaming-message buf) nil)
           ;; Handle tool calls
            (if (and (string= "tool_use" (or stop-reason ""))
                     tool-uses)
                (progn
                  (begin-tool-approval buf tool-uses)
                  t)
               (progn
                 (setf (buffer-status buf) :idle)
                 nil))))
        ;; Still streaming
        (t t)))))

(declaim (ftype (function (buffer) buffer) send-to-agent-with-context))
(defun send-to-agent-with-context (buf)
  "Start a streaming conversation with the LLM. Non-blocking --
the event loop polls for updates via update-streaming-response."
  (setf (buffer-status buf) :thinking)
  (start-streaming-response buf)
  buf)

;;; --------------------------------------------------------------------------
;;; Prefix Processing
;;; --------------------------------------------------------------------------

(defvar *prefix-handlers* nil
  "Alist mapping prefix strings to handler functions.
Each handler is called as (funcall handler buffer remaining-text) where
REMAINING-TEXT is the user's input with the prefix stripped.
Handlers should insert their results as system messages in the buffer.

Example entry: (\"!\" . shell-prefix-handler)")

(defun shell-prefix-handler (buf command-text)
  "Execute COMMAND-TEXT as a shell command and insert the output as a system message.
The command runs in the buffer's working directory."
  (let* ((trimmed (string-trim '(#\Space #\Tab) command-text))
         (working-dir (or (buffer-working-directory buf) "/workspace"))
         (output (handler-case
                     (multiple-value-bind (stdout stderr exit-code)
                         (uiop:run-program
                          (list "/bin/sh" "-c" trimmed)
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t)
                          :ignore-error-status t
                          :directory working-dir)
                       (let ((parts nil))
                         (when (and stderr (plusp (length stderr)))
                           (push stderr parts))
                         (when (and stdout (plusp (length stdout)))
                           (push stdout parts))
                         (let ((combined (format nil "~{~A~^~%~}" (nreverse parts))))
                           (if (zerop exit-code)
                               (format nil "$ ~A~%~A" trimmed combined)
                               (format nil "$ ~A  [exit ~D]~%~A"
                                       trimmed exit-code combined)))))
                   (error (e)
                     (format nil "$ ~A~%[Shell error: ~A]" trimmed e)))))
    (buffer-insert-system-message buf output)))

(defun find-prefix-handler (text)
  "Return (prefix . handler) if TEXT starts with a registered prefix, or NIL.
Longer prefixes are checked first to support prefix hierarchies."
  (let ((sorted (sort (copy-list *prefix-handlers*) #'>
                      :key (lambda (entry) (length (car entry))))))
    (dolist (entry sorted)
      (let ((prefix (car entry)))
        (when (and (<= (length prefix) (length text))
                   (string= prefix text :end2 (length prefix)))
          (return entry))))))

(defun process-prefix-command (buf input-text)
  "Check if INPUT-TEXT starts with a registered prefix.
If so, call the handler and return T. Otherwise return NIL."
  (let ((entry (find-prefix-handler input-text)))
    (when entry
      (let* ((prefix (car entry))
             (handler (cdr entry))
             (remaining (subseq input-text (length prefix))))
        (funcall handler buf remaining)
        t))))

;; Register the shell prefix handler
(setf *prefix-handlers*
      (acons "!" #'shell-prefix-handler
             (remove "!" *prefix-handlers* :key #'car :test #'string=)))

;;; --------------------------------------------------------------------------
;;; Commands
;;; --------------------------------------------------------------------------

(defcommand send-message (:permission :user-only :keys (#\Return))
  "Send the current input message to the agent."
  (buffer)
  (let ((input-text (message-text (buffer-input-message buffer))))
    (when (plusp (length (string-trim '(#\Space #\Tab #\Newline) input-text)))
      (buffer-finalize-input buffer)
      (setf (message-face-set (buffer-input-message buffer))
            (gethash :user (buffer-face-registry buffer)))
      ;; Check for prefix commands before sending to the LLM
      (unless (process-prefix-command buffer input-text)
        (send-to-agent-with-context buffer)))))

(defcommand insert-newline-command (:permission :user-only :keys (#\Linefeed))
  "Insert a newline in the input message."
  (buffer)
  (message-insert-newline (buffer-input-message buffer)))

(defcommand beginning-of-line-command (:permission :user-only)
  "Move point to the beginning of the current line."
  (buffer)
  (message-move-beginning-of-line (buffer-input-message buffer)))

(defcommand end-of-line-command (:permission :user-only)
  "Move point to the end of the current line."
  (buffer)
  (message-move-end-of-line (buffer-input-message buffer)))

(defcommand kill-line-command (:permission :user-only)
  "Kill from point to the end of the line."
  (buffer)
  (message-kill-line (buffer-input-message buffer)))

(defcommand yank-command (:permission :user-only)
  "Yank the top of the kill ring at point."
  (buffer)
  (message-yank (buffer-input-message buffer)))

(defcommand delete-char-backward-command (:permission :user-only)
  "Delete the character before point."
  (buffer)
  (message-delete-char-backward (buffer-input-message buffer)))

(defcommand delete-char-forward-command (:permission :user-only)
  "Delete the character after point."
  (buffer)
  (message-delete-char-forward (buffer-input-message buffer)))

(defcommand forward-char-command (:permission :user-only)
  "Move point one character forward."
  (buffer)
  (message-forward-char (buffer-input-message buffer)))

(defcommand backward-char-command (:permission :user-only)
  "Move point one character backward."
  (buffer)
  (message-backward-char (buffer-input-message buffer)))

(defcommand forward-word-command (:permission :user-only)
  "Move point forward to end of next word."
  (buffer)
  (message-forward-word (buffer-input-message buffer)))

(defcommand backward-word-command (:permission :user-only)
  "Move point backward to beginning of previous word."
  (buffer)
  (message-backward-word (buffer-input-message buffer)))

(defcommand kill-backward-line-command (:permission :user-only)
  "Kill from start of line to point."
  (buffer)
  (message-kill-backward-line (buffer-input-message buffer)))

(defcommand kill-word-command (:permission :user-only)
  "Kill from point to end of current word."
  (buffer)
  (message-kill-word (buffer-input-message buffer)))

(defcommand backward-kill-word-command (:permission :user-only)
  "Kill from beginning of current word to point."
  (buffer)
  (message-backward-kill-word (buffer-input-message buffer)))

(defcommand yank-pop-command (:permission :user-only)
  "Replace just-yanked text with next kill ring entry."
  (buffer)
  (message-yank-pop (buffer-input-message buffer)))

(defun message-insert-string (msg text)
  "Insert TEXT at point in MSG."
  (loop :for char :across text
        :do (if (char= char #\Newline)
                (message-insert-newline msg)
                (message-insert-char msg char)))
  msg)

(defcommand yank-previous-command-first-arg-command (:permission :user-only)
  "Insert the first argument of the previous user command."
  (buffer)
  (let ((arg (buffer-previous-command-first-argument buffer)))
    (when arg
      (message-insert-string (buffer-input-message buffer) arg))))

(defcommand yank-previous-command-last-arg-command (:permission :user-only)
  "Insert the last argument of the previous user command."
  (buffer)
  (let ((arg (buffer-previous-command-last-argument buffer)))
    (when arg
      (message-insert-string (buffer-input-message buffer) arg))))

(defcommand self-insert-command (:permission :user-only)
  "Insert a character at point. The character is passed via *self-insert-char*."
  (buffer)
  (when *self-insert-char*
    (message-insert-char (buffer-input-message buffer) *self-insert-char*)))

;;; --------------------------------------------------------------------------
;;; Scroll Commands
;;; --------------------------------------------------------------------------

(defvar *scroll-page-size* nil
  "Number of rows to scroll per page. Set by the event loop based on window height.")

(defcommand scroll-up-command (:permission :user-only)
  "Scroll history up (back) by one page."
  (buffer)
  (when *scroll-page-size*
    (incf (buffer-scroll-offset buffer) *scroll-page-size*)))

(defcommand scroll-down-command (:permission :user-only)
  "Scroll history down (forward) by one page."
  (buffer)
  (when *scroll-page-size*
    (decf (buffer-scroll-offset buffer) *scroll-page-size*)
    (when (minusp (buffer-scroll-offset buffer))
      (setf (buffer-scroll-offset buffer) 0))))

;;; --------------------------------------------------------------------------
;;; OpenAI Codex OAuth Command
;;; --------------------------------------------------------------------------

(defcommand openai-codex-oauth-command (:permission :user-only)
  "Start the OpenAI Codex OAuth login flow. Opens a browser URL and waits
for the user to paste the callback URL."
  (buffer)
  (handler-case
      (multiple-value-bind (auth-url code-verifier state)
          (openai-codex-oauth-start)
        (setf *openai-oauth-pending*
              `((:code-verifier . ,code-verifier) (:state . ,state)))
        ;; Display auth URL as a system message
        (let ((sys-msg (buffer-insert-agent-message
                        buffer
                        (format nil "[OpenAI Codex OAuth]~%~%Open this URL in your browser:~%~%  ~A~%~%After signing in, your browser will redirect to a localhost URL.~%Copy that full URL from the address bar and paste it here.~%(It starts with ~A?...)~%~%Press Enter to submit, C-g to cancel."
                                auth-url *openai-oauth-redirect-uri*))))
          (setf (message-sender sys-msg) :system))
        ;; Clear input area for pasting
        (set-message-text (buffer-input-message buffer) "")
        (setf (buffer-status buffer) :oauth))
    (error (e)
      (let ((sys-msg (buffer-insert-agent-message
                      buffer (format nil "[OAuth error: ~A]" e))))
        (setf (message-sender sys-msg) :system)))))

;;; --------------------------------------------------------------------------
;;; Buffer Management Commands
;;; --------------------------------------------------------------------------

(defcommand list-buffers-command (:permission :user-only)
  "Open the buffer selector to switch between agent sessions."
  (buffer)
  (declare (ignore buffer))
  (setf *buffer-selector-active* t
        *buffer-selector-index* 0
        *buffer-selector-scroll* 0))

;;; --------------------------------------------------------------------------
;;; Model Selection Commands
;;; --------------------------------------------------------------------------

(defcommand select-model-command (:permission :user-only)
  "Open the model selector to change the LLM model for this session.
Builds the available model list based on configured API keys."
  (buffer)
  (let ((entries (available-models-for-selector buffer)))
    (cond
      ((null entries)
       (let ((sys-msg (buffer-insert-agent-message
                       buffer "[No API keys configured. Cannot list models.]")))
         (setf (message-sender sys-msg) :system)))
      (t
       ;; Pre-select the currently active model (if found)
       (let ((active-idx (position-if (lambda (e) (getf e :active-p)) entries)))
         (setf *model-selector-entries* entries
               *model-selector-active* t
               *model-selector-index* (or active-idx 0)
               *model-selector-scroll* 0))))))

(defcommand minibuffer-select-model-command (:permission :user-only)
  "Open the minibuffer model selector with fuzzy search (helm/ivy/vertico style).
Activates the minibuffer with all available models as candidates, sorted by
recency then alphabetically. The user can type to fuzzy-filter and use C-n/C-p
to navigate."
  (buffer)
  (let ((entries (available-models-for-selector buffer)))
    (cond
      ((null entries)
       (let ((sys-msg (buffer-insert-agent-message
                       buffer "[No API keys configured. Cannot list models.]")))
         (setf (message-sender sys-msg) :system)))
      (t
       ;; Build display items with provider/model display string
       (let* ((items (mapcar (lambda (e)
                               (list :provider (getf e :provider)
                                     :model (getf e :model)
                                     :active-p (getf e :active-p)
                                     :display (format nil "~(~A~)/~A"
                                                      (getf e :provider)
                                                      (getf e :model))))
                             entries))
              ;; Sort: by recency (from history), then active, then alphabetical
              (sorted (sort-models-by-recency items)))
         (minibuffer-activate
          "Select Model" sorted
          (lambda (item)
            (let ((provider (getf item :provider))
                  (model (getf item :model)))
              (set-buffer-provider-override buffer provider)
              (set-buffer-model-override buffer model)
              ;; Record in history for recency sorting
              (setf *model-selection-history*
                    (cons (getf item :display)
                          (remove (getf item :display) *model-selection-history*
                                  :test #'string=)))
              ;; Show confirmation in chat
              (let ((sys-msg (buffer-insert-agent-message
                              buffer (format nil "[Model changed to ~(~A~)/~A]"
                                            provider model))))
                (setf (message-sender sys-msg) :system))))))))))

;;; --------------------------------------------------------------------------
;;; Buffer Management Commands (continued)
;;; --------------------------------------------------------------------------

(defcommand minibuffer-select-buffer-command (:permission :user-only)
  "Open the minibuffer buffer selector with fuzzy search (helm/ivy/vertico style).
Activates the minibuffer with all open buffers as candidates, sorted by
recency then alphabetically. The user can type to fuzzy-filter and use C-n/C-p
to navigate. Shows buffer name, agent, status, and message count."
  (buffer)
  (let* ((current (current-buffer))
         (items (mapcar (lambda (buf)
                          (let* ((name (buffer-name buf))
                                 (agent (buffer-agent-name buf))
                                 (status (string-downcase
                                          (symbol-name (buffer-status buf))))
                                 (msgs (max 0 (1- (buffer-message-count buf))))
                                 (current-p (eq buf current))
                                 (marker (if current-p "*" " "))
                                 (display (format nil "~A ~A  [~A] ~A  msgs:~D"
                                                  marker name agent status msgs)))
                            (list :buffer buf
                                  :name name
                                  :current-p current-p
                                  :display display)))
                        *buffer-ring*))
         ;; Sort: by recency (from history), then current buffer, then alphabetical
         (sorted (sort-buffers-by-recency items)))
    (minibuffer-activate
     "Switch Buffer" sorted
     (lambda (item)
       (let ((selected-buf (getf item :buffer))
             (name (getf item :name)))
         (when selected-buf
           (switch-to-buffer selected-buf)
           ;; Record in history for recency sorting
           (setf *buffer-selection-history*
                 (cons name
                       (remove name *buffer-selection-history*
                               :test #'string=)))))))))

(defcommand new-buffer-command (:permission :user-only)
  "Create a new chat buffer and switch to it."
  (buffer)
  (declare (ignore buffer))
  (let* ((name (next-buffer-name))
         (new-buf (make-buffer name
                               :agent-name *default-agent-name*
                               :working-directory (truename "."))))
    (init-face-registry new-buf)
    (setf (buffer-keymap new-buf) *default-keymap*)
    (add-buffer-to-ring new-buf)
    (switch-to-buffer new-buf)))

(defcommand next-buffer-command (:permission :user-only)
  "Switch to the next buffer in the ring."
  (buffer)
  (declare (ignore buffer))
  (when (cdr *buffer-ring*)
    ;; Rotate: move first to end
    (let ((current (pop *buffer-ring*)))
      (setf *buffer-ring* (append *buffer-ring* (list current))))))

(defcommand kill-buffer-command (:permission :user-only)
  "Kill the current buffer. Switches to the next buffer in the ring."
  (buffer)
  (declare (ignore buffer))
  (when (cdr *buffer-ring*)  ; Don't kill the last buffer
    (kill-buffer-from-ring (current-buffer))))

;;; --------------------------------------------------------------------------
;;; Session Commands
;;; --------------------------------------------------------------------------

(defcommand save-session-command (:permission :user-only)
  "Save the current buffer's conversation to a session file."
  (buffer)
  (let ((path (save-session buffer)))
    ;; Insert a system message confirming the save
    (let ((sys-msg (buffer-insert-agent-message
                    buffer (format nil "[Session saved to ~A]" path))))
      (setf (message-sender sys-msg) :system))))

;;; --------------------------------------------------------------------------
;;; Display Toggle Commands
;;; --------------------------------------------------------------------------

(defcommand toggle-tool-results-command (:permission :user-only)
  "Toggle visibility of tool-result messages in the chat."
  (buffer)
  (setf (buffer-show-tool-results-p buffer)
        (not (buffer-show-tool-results-p buffer))))

(defcommand toggle-debug-mode-command (:permission :user-only)
  "Toggle API debug mode on/off. When enabled, every outgoing API request
(provider, model, full messages/tools JSON) and every completed response
(stop-reason, content blocks) is echoed into the chat window as a debug
message, rendered in magenta so it stands out from normal system output.
Bound to C-c C-d."
  (buffer)
  (setf *debug-mode* (not *debug-mode*))
  (buffer-insert-system-message
   buffer
   (if *debug-mode*
       "[Debug mode ON — API calls will be shown in chat]"
       "[Debug mode OFF]")))

;;; --------------------------------------------------------------------------
;;; Debug Logging
;;; --------------------------------------------------------------------------

(defun debug-log (buf text)
  "Insert TEXT as a debug message in BUF when *debug-mode* is enabled.
Uses the global :debug face (bright magenta) so debug output is visually
distinct from normal system messages (cyan). Returns the message object,
or nil if debug mode is off."
  (when *debug-mode*
    (let* ((msg (buffer-insert-system-message buf text))
           (debug-face (or (global-face :debug)
                           (make-instance 'face :name :debug
                             :background (make-color-spec :cga 0)
                             :foreground (make-color-spec :cga 13)
                             :bold-p nil :underline-p nil :reverse-p nil)))
           (debug-fs (make-face-set
                      :debug
                      (list (make-instance 'face
                              :name :default
                              :parent debug-face
                              :background nil :foreground nil
                              :bold-p nil :underline-p nil :reverse-p nil)))))
      (setf (message-face-set msg) debug-fs)
      msg)))

(defun file-debug-log (category format-string &rest format-args)
  "Append a timestamped debug entry to *debug-log-file* when set.
CATEGORY is a short tag (e.g. \"cli-spawn\", \"ndjson\", \"stream-event\").
Thread-safe: opens, writes, and closes the file on each call."
  (when *debug-log-file*
    (ignore-errors
      (let ((line (format nil "[~A] [~A] ~?~%"
                          (format-timestamp (get-universal-time))
                          category
                          format-string format-args)))
        (with-open-file (f *debug-log-file*
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create
                           :external-format :utf-8)
          (write-string line f)
          (force-output f))))))

(defun format-timestamp (universal-time)
  "Format UNIVERSAL-TIME as ISO 8601 local time string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

;;; --------------------------------------------------------------------------
;;; Face Registry Setup
;;; --------------------------------------------------------------------------

(defun init-face-registry (buf)
  "Populate BUF's face registry with default face sets.
Includes user, agent, and system face sets."
  (let* ((registry (buffer-face-registry buf))
         (agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (user-fs (make-default-user-face-set))
         (agent-fs (make-default-agent-face-set agent-kw))
         (system-fs (make-default-system-face-set)))
    (setf (gethash :user registry) user-fs
          (gethash agent-kw registry) agent-fs
          (gethash :system registry) system-fs)
    (setf (message-face-set (buffer-input-message buf)) user-fs)
    buf))

;;; --------------------------------------------------------------------------
;;; Customize Face
;;; --------------------------------------------------------------------------

(defvar *customize-face-state* nil
  "When non-nil, a plist describing the face customization session:
  :face — the face object being customized
  :label — display label (e.g. \"user:default\")
  :field-index — 0-5 (which field is selected)
  :original-values — alist of original attribute values for revert
  :buffer — the customize buffer")

(defvar *customize-face-fields*
  '(:foreground :background :bold-p :underline-p :reverse-p :parent)
  "List of face attribute field keywords in display order for customize-face.")

(defun cga-color-name (value)
  "Return a human-readable name for a CGA color value (0-15)."
  (case value
    (0 "black") (1 "red") (2 "green") (3 "yellow")
    (4 "blue") (5 "magenta") (6 "cyan") (7 "white")
    (8 "bright-black") (9 "bright-red") (10 "bright-green") (11 "bright-yellow")
    (12 "bright-blue") (13 "bright-magenta") (14 "bright-cyan") (15 "bright-white")
    (t (format nil "~D" value))))

(defun format-color-spec-display (cs)
  "Format a color-spec for human-readable display."
  (if (null cs)
      "(inherit)"
      (ecase (color-spec-type cs)
        (:cga (format nil "~A (CGA ~D)" (cga-color-name (color-spec-value cs))
                       (color-spec-value cs)))
        (:256 (format nil "color-~D (256)" (color-spec-value cs)))
        (:hex (format nil "~A (hex)" (color-spec-value cs))))))

(defun format-boolean-display (val)
  "Format a boolean face attribute for display.
NIL means inherit from parent, T means yes."
  (if val "yes" "(inherit)"))

(defun format-face-parent-display (parent-face)
  "Format a face's parent for display."
  (if (null parent-face)
      "(none)"
      (format nil "~(~A~)" (face-name parent-face))))

(defun customize-face-field-value (face field)
  "Get the current value of FIELD on FACE."
  (ecase field
    (:foreground (face-foreground face))
    (:background (face-background face))
    (:bold-p (slot-value face 'bold-p))
    (:underline-p (slot-value face 'underline-p))
    (:reverse-p (slot-value face 'reverse-p))
    (:parent (face-parent face))))

(defun customize-face-set-field-value (face field value)
  "Set the value of FIELD on FACE to VALUE."
  (ecase field
    (:foreground (setf (face-foreground face) value))
    (:background (setf (face-background face) value))
    (:bold-p (setf (face-bold-p face) value))
    (:underline-p (setf (face-underline-p face) value))
    (:reverse-p (setf (face-reverse-p face) value))
    (:parent (setf (face-parent face) value))))

(defun customize-face-field-label (field)
  "Return the human-readable label for a face attribute field keyword."
  (ecase field
    (:foreground "Foreground")
    (:background "Background")
    (:bold-p "Bold")
    (:underline-p "Underline")
    (:reverse-p "Reverse")
    (:parent "Parent")))

(defun customize-face-field-display (face field)
  "Return the display string for FIELD's current value on FACE."
  (ecase field
    ((:foreground :background)
     (format-color-spec-display (customize-face-field-value face field)))
    ((:bold-p :underline-p :reverse-p)
     (format-boolean-display (customize-face-field-value face field)))
    (:parent
     (format-face-parent-display (customize-face-field-value face field)))))

(defun customize-face-snapshot (face)
  "Take a snapshot of FACE's current attribute values for revert.
Returns an alist of (field . value) pairs."
  (mapcar (lambda (field)
            (cons field (customize-face-field-value face field)))
          *customize-face-fields*))

(defun customize-face-restore-snapshot (face snapshot)
  "Restore FACE's attributes from SNAPSHOT (an alist from customize-face-snapshot)."
  (dolist (entry snapshot)
    (customize-face-set-field-value face (car entry) (cdr entry))))

(defun build-customize-face-content (face label field-index)
  "Build the text content for a customize-face buffer.
FACE is the face being customized, LABEL is its display name,
FIELD-INDEX is the currently selected field (0-5)."
  (with-output-to-string (s)
    (format s "Customize Face: ~A~%" label)
    (format s "~A~%~%"
            (make-string (min 50 (+ 16 (length label)))
                         :initial-element #\═))
    ;; Fields
    (loop :for field :in *customize-face-fields*
          :for idx :from 0
          :for selected-p := (= idx field-index)
          :for marker := (if selected-p "▸" " ")
          :for field-label := (customize-face-field-label field)
          :for value-str := (customize-face-field-display face field)
          :do (format s "~A ~A:~A~A~%"
                      marker
                      field-label
                      (make-string (max 1 (- 14 (length field-label)))
                                   :initial-element #\Space)
                      value-str))
    ;; Resolved preview
    (format s "~%Resolved attributes:~%")
    (handler-case
        (let ((resolved (resolve-face face)))
          (when resolved
            (format s "  FG: ~A  BG: ~A~%"
                    (format-color-spec-display (resolved-face-foreground resolved))
                    (format-color-spec-display (resolved-face-background resolved)))
            (format s "  Bold: ~:[no~;yes~]  Underline: ~:[no~;yes~]  Reverse: ~:[no~;yes~]~%"
                    (resolved-face-bold-p resolved)
                    (resolved-face-underline-p resolved)
                    (resolved-face-reverse-p resolved))))
      (error () (format s "  (cannot resolve — missing foreground or background)~%")))
    ;; Keybinding help
    (format s "~%~A~%" (make-string 40 :initial-element #\─))
    (format s "[RET] Edit  [SPC] Toggle  [C-n/C-p] Navigate~%")
    (format s "[C-c C-c] Apply  [C-c C-k] Cancel  [r] Revert")))

(defun rebuild-customize-face-display ()
  "Rebuild the customize buffer content from current *customize-face-state*.
Updates the form display message in-place."
  (when *customize-face-state*
    (let* ((face (getf *customize-face-state* :face))
           (label (getf *customize-face-state* :label))
           (field-index (getf *customize-face-state* :field-index))
           (buf (getf *customize-face-state* :buffer))
           (content (build-customize-face-content face label field-index)))
      ;; Find the first message (the form display) and update it
      (let ((msg (buffer-first-message buf)))
        (when (and msg (message-read-only-p msg))
          (set-message-text msg content))))))

(defun customize-face-next-field ()
  "Move to the next field in the customize form."
  (when *customize-face-state*
    (let ((idx (getf *customize-face-state* :field-index)))
      (when (< idx (1- (length *customize-face-fields*)))
        (setf (getf *customize-face-state* :field-index) (1+ idx))
        (rebuild-customize-face-display)))))

(defun customize-face-prev-field ()
  "Move to the previous field in the customize form."
  (when *customize-face-state*
    (let ((idx (getf *customize-face-state* :field-index)))
      (when (plusp idx)
        (setf (getf *customize-face-state* :field-index) (1- idx))
        (rebuild-customize-face-display)))))

(defun customize-face-toggle-field ()
  "Toggle a boolean field between yes (t) and inherit (nil).
Does nothing for non-boolean fields (foreground, background, parent)."
  (when *customize-face-state*
    (let* ((face (getf *customize-face-state* :face))
           (field-index (getf *customize-face-state* :field-index))
           (field (nth field-index *customize-face-fields*)))
      (when (member field '(:bold-p :underline-p :reverse-p))
        (let ((current (customize-face-field-value face field)))
          (customize-face-set-field-value face field (not current))
          (rebuild-customize-face-display))))))

(defun collect-all-faces ()
  "Collect all unique face objects from the global face registry and
all buffer face registries. Returns a sorted list of plists with
:face, :owner, :name, and :label keys."
  (let ((seen (make-hash-table :test #'eq))
        (result nil))
    ;; Global faces first
    (maphash (lambda (name face)
               (unless (gethash face seen)
                 (setf (gethash face seen) t)
                 (push (list :face face
                             :owner :global
                             :name name
                             :label (format nil "global:~(~A~)" name))
                       result)))
             *global-face-registry*)
    ;; Per-buffer faces
    (dolist (buf *buffer-ring*)
      (maphash (lambda (owner face-set)
                 (maphash (lambda (name face)
                            (unless (gethash face seen)
                              (setf (gethash face seen) t)
                              (push (list :face face
                                          :owner owner
                                          :name name
                                          :label (format nil "~(~A~):~(~A~)"
                                                         owner name))
                                    result)))
                          (face-set-faces face-set)))
               (buffer-face-registry buf)))
    (sort (nreverse result) #'string< :key (lambda (p) (getf p :label)))))

(defun make-color-selection-items ()
  "Build the list of items for color selection in the minibuffer.
Includes CGA colors 0-15 with names, plus an inherit option."
  (let ((items nil))
    (push (list :color-spec nil :display "(inherit / nil)") items)
    (loop :for i :from 0 :to 15
          :do (push (list :color-spec (make-color-spec :cga i)
                          :display (format nil "CGA ~2D: ~A" i (cga-color-name i)))
                    items))
    (nreverse items)))

(defun make-boolean-selection-items ()
  "Build the list of items for boolean field selection in the minibuffer."
  (list (list :value t :display "yes")
        (list :value nil :display "inherit (nil)")))

(defun make-parent-selection-items (current-face)
  "Build the list of parent face candidates for the minibuffer.
Excludes CURRENT-FACE to prevent inheritance cycles."
  (let ((items (list (list :face nil :display "(none)"))))
    (dolist (entry (collect-all-faces))
      (let ((face (getf entry :face)))
        (unless (eq face current-face)
          (push (list :face face
                      :display (getf entry :label))
                items))))
    (nreverse items)))

(defun customize-face-edit-field ()
  "Edit the currently selected field using the minibuffer.
Opens a field-appropriate minibuffer: color picker for foreground/background,
boolean selector for bold/underline/reverse, face selector for parent."
  (when *customize-face-state*
    (let* ((face (getf *customize-face-state* :face))
           (field-index (getf *customize-face-state* :field-index))
           (field (nth field-index *customize-face-fields*)))
      (ecase field
        ;; Color fields — pick from CGA palette
        ((:foreground :background)
         (let ((field-label (customize-face-field-label field)))
           (minibuffer-activate
            (format nil "Set ~A" field-label)
            (make-color-selection-items)
            (lambda (item)
              (customize-face-set-field-value face field (getf item :color-spec))
              (rebuild-customize-face-display)))))
        ;; Boolean fields — yes / inherit
        ((:bold-p :underline-p :reverse-p)
         (let ((field-label (customize-face-field-label field)))
           (minibuffer-activate
            (format nil "Set ~A" field-label)
            (make-boolean-selection-items)
            (lambda (item)
              (customize-face-set-field-value face field (getf item :value))
              (rebuild-customize-face-display)))))
        ;; Parent field — pick from available faces
        (:parent
         (minibuffer-activate
          "Set Parent"
          (make-parent-selection-items face)
          (lambda (item)
            (customize-face-set-field-value face :parent (getf item :face))
            (rebuild-customize-face-display))))))))

(defun customize-face-apply ()
  "Apply face customizations and close the customize buffer.
Changes are already applied to the face object (modified in-place),
so this just closes the buffer and confirms."
  (when *customize-face-state*
    (let ((buf (getf *customize-face-state* :buffer))
          (label (getf *customize-face-state* :label)))
      (setf *customize-face-state* nil)
      (kill-buffer-from-ring buf)
      ;; Show confirmation in the new current buffer
      (let ((sys-msg (buffer-insert-agent-message
                      (current-buffer)
                      (format nil "[Face ~A customized successfully]" label))))
        (setf (message-sender sys-msg) :system)))))

(defun customize-face-cancel ()
  "Cancel face customization, reverting all changes to original values.
Closes the customize buffer and switches to the previous buffer."
  (when *customize-face-state*
    (let ((face (getf *customize-face-state* :face))
          (snapshot (getf *customize-face-state* :original-values))
          (buf (getf *customize-face-state* :buffer)))
      (customize-face-restore-snapshot face snapshot)
      (setf *customize-face-state* nil)
      (kill-buffer-from-ring buf))))

(defun customize-face-revert-to-original ()
  "Revert all fields to their original values without closing the buffer."
  (when *customize-face-state*
    (let ((face (getf *customize-face-state* :face))
          (snapshot (getf *customize-face-state* :original-values)))
      (customize-face-restore-snapshot face snapshot)
      (rebuild-customize-face-display))))

(defun make-customize-face-buffer (face label)
  "Create a customize buffer for FACE with display LABEL.
Sets up the customize state and returns the new buffer."
  (let* ((buf-name (format nil "*customize:~A*" label))
         (existing (find-buffer-by-name buf-name)))
    ;; Kill any existing customize buffer for this face
    (when existing
      (kill-buffer-from-ring existing))
    (let* ((snapshot (customize-face-snapshot face))
           (buf (make-buffer buf-name :agent-name "customize")))
      (init-face-registry buf)
      (setf (buffer-keymap buf) *default-keymap*)
      (setf (buffer-major-mode buf) "customize")
      ;; Set up customize state
      (setf *customize-face-state*
            (list :face face
                  :label label
                  :field-index 0
                  :original-values snapshot
                  :buffer buf))
      ;; Build initial content
      (let ((content (build-customize-face-content face label 0)))
        (buffer-insert-agent-message buf content))
      (add-buffer-to-ring buf)
      buf)))

(defun handle-customize-key (key)
  "Handle a key event while in customize mode.
Supports field navigation (C-n/C-p), editing (Return), toggling (Space),
apply (C-c C-c), cancel (C-c C-k / C-g / q), revert (r), and passes
through C-x prefix commands for buffer management."
  (cond
    ;; C-c C-c: apply changes (C-c prefix then C-c = ETX = ASCII 3)
    ((equal key '(:ctrl-c #\Etx))
     (customize-face-apply))
    ;; C-c C-k: cancel (C-c prefix then C-k = VT = ASCII 11)
    ((equal key '(:ctrl-c #\Vt))
     (customize-face-cancel))
    ;; C-g: cancel
    ((and (characterp key) (char= key (code-char 7)))
     (customize-face-cancel))
    ;; q: cancel
    ((and (characterp key) (char= key #\q))
     (customize-face-cancel))
    ;; C-n or Down arrow: next field
    ((or (eq key :down)
         (and (characterp key) (char= key (code-char 14))))
     (customize-face-next-field))
    ;; C-p or Up arrow: previous field
    ((or (eq key :up)
         (and (characterp key) (char= key (code-char 16))))
     (customize-face-prev-field))
    ;; Return: edit selected field via minibuffer
    ((and (characterp key) (or (char= key #\Return) (char= key #\Newline)))
     (customize-face-edit-field))
    ;; Space: toggle boolean field
    ((and (characterp key) (char= key #\Space))
     (customize-face-toggle-field))
    ;; r: revert to original values
    ((and (characterp key) (char= key #\r))
     (customize-face-revert-to-original))
    ;; Pass through C-x prefix commands (buffer management, save, etc.)
    ((and (listp key) (eq (first key) :ctrl-x))
     (let ((command (keymap-lookup *default-keymap* key)))
       (when command (funcall command (current-buffer)))))
    ;; Scroll keys
    ((or (eq key :page-up) (eq key :page-down))
     (let ((command (keymap-lookup *default-keymap* key)))
       (when command (funcall command (current-buffer)))))
    ;; Everything else: ignore
    (t nil)))

(defcommand customize-face-command (:permission :user-only)
  "Open a face selector in the minibuffer, then customize the selected face.
Lists all faces from all buffer face registries. When a face is selected,
opens a customize buffer where face attributes can be edited interactively.
Bound to C-h F."
  (buffer)
  (declare (ignore buffer))
  (let ((faces (collect-all-faces)))
    (if (null faces)
        (let ((sys-msg (buffer-insert-agent-message
                        (current-buffer)
                        "[No faces found to customize]")))
          (setf (message-sender sys-msg) :system))
        (minibuffer-activate
         "Customize Face"
         (mapcar (lambda (entry)
                   (let* ((face (getf entry :face))
                          (label (getf entry :label))
                          (fg (format-color-spec-display (face-foreground face)))
                          (bg (format-color-spec-display (face-background face)))
                          (display (format nil "~A  fg:~A  bg:~A" label fg bg)))
                     (list :face face
                           :label label
                           :display display)))
                 faces)
         (lambda (item)
           (let* ((face (getf item :face))
                  (label (getf item :label))
                  (buf (make-customize-face-buffer face label)))
             (switch-to-buffer buf)))))))

;;; --------------------------------------------------------------------------
;;; Introspection: list-functions & describe-function
;;; --------------------------------------------------------------------------

(defun list-functions ()
  "Return a sorted list of function symbols exported from the clawmacs package.
Includes all exported symbols that have function bindings (functions, generic
functions, commands, macros)."
  (let ((functions nil))
    (do-external-symbols (sym :clawmacs)
      (when (fboundp sym)
        (push sym functions)))
    (sort functions #'string< :key #'symbol-name)))

(defun find-keybindings-for-command (command-sym &optional (keymap *default-keymap*))
  "Return a list of key specifications bound to COMMAND-SYM in KEYMAP.
Walks only the direct keymap bindings (not the parent chain)."
  (let ((bindings nil))
    (when keymap
      (maphash (lambda (key cmd)
                 (when (eq cmd command-sym)
                   (push key bindings)))
               (keymap-bindings keymap)))
    bindings))

(defun format-key-binding (key)
  "Format a key binding specification as a human-readable string.
Converts raw characters, keywords, and prefix lists to standard Emacs notation."
  (cond
    ((characterp key)
     (let ((code (char-code key)))
       (cond
         ((= code 13) "RET")
         ((= code 10) "C-j")
         ((= code 27) "ESC")
         ((= code 127) "DEL")
         ((< code 32) (format nil "C-~A" (code-char (+ code 96))))
         ((char= key #\Space) "SPC")
         (t (string key)))))
    ((keywordp key)
     (string-downcase (symbol-name key)))
    ((and (listp key) (eq (first key) :ctrl-x))
     (format nil "C-x ~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :ctrl-c))
     (format nil "C-c ~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :ctrl-h))
     (format nil "C-h ~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :alt))
     (format nil "M-~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :ctrl))
     (format nil "C-~A" (format-key-binding (second key))))
    (t (format nil "~S" key))))

(defun describe-function-to-string (fn-symbol)
  "Return a human-readable string describing FN-SYMBOL.
Includes: name, type, lambda list, docstring, keybindings, and permissions."
  (unless (and fn-symbol (fboundp fn-symbol))
    (return-from describe-function-to-string
      (format nil "~A is not a defined function." fn-symbol)))
  (with-output-to-string (s)
    ;; Header
    (format s "~A~%~A~%~%" fn-symbol
            (make-string (min 60 (length (symbol-name fn-symbol)))
                         :initial-element #\-))
    ;; Type
    (let* ((cmd-meta (gethash fn-symbol *command-table*))
           (fn-obj (fdefinition fn-symbol))
           (type-str (cond
                       ((macro-function fn-symbol) "Macro")
                       ((and cmd-meta (typep fn-obj 'generic-function)) "Command")
                       ((typep fn-obj 'generic-function) "Generic Function")
                       (t "Function"))))
      (format s "Type: ~A~%" type-str)
      ;; Lambda list
      (let ((lambda-list
              (handler-case
                  (cond
                    ((typep fn-obj 'generic-function)
                     #+sbcl (sb-mop:generic-function-lambda-list fn-obj)
                     #-sbcl nil)
                    (t
                     #+sbcl (sb-introspect:function-lambda-list fn-symbol)
                     #-sbcl nil))
                (error () nil))))
        (when lambda-list
          (format s "Arguments: (~{~A~^ ~})~%" lambda-list)))
      ;; Keybindings (from actual keymap scan)
      (let ((keybinds (find-keybindings-for-command fn-symbol)))
        (when keybinds
          (format s "Keybindings: ~{~A~^, ~}~%"
                  (mapcar #'format-key-binding keybinds))))
      ;; Permission (for commands)
      (when cmd-meta
        (format s "Permission: ~(~A~)~%"
                (command-metadata-permission cmd-meta)))
      ;; Docstring
      (let ((doc (or (documentation fn-symbol 'function) "")))
        (when (plusp (length doc))
          (format s "~%~A~%" doc)))
      ;; Extended documentation
      (let ((ext (extended-doc fn-symbol)))
        (when ext
          (let ((usage (getf ext :usage)))
            (when usage
              (format s "~%Usage:~%  ~A~%" usage)))
          (let ((returns (getf ext :returns)))
            (when returns
              (format s "~%Returns:~%  ~A~%" returns)))
          (let ((side-effects (getf ext :side-effects)))
            (when side-effects
              (format s "~%Side Effects:~%  ~A~%" side-effects)))
          (let ((see-also (getf ext :see-also)))
            (when see-also
              (format s "~%See Also: ~{~(~A~)~^, ~}~%" see-also)))
          (let ((category (getf ext :category)))
            (when category
              (format s "~%Category: ~A~%" category))))))))

(defun make-help-buffer (name content)
  "Create a help buffer with NAME containing CONTENT as read-only text.
The buffer is added to the buffer ring with major-mode \"help\".
Returns the new buffer."
  (let ((buf (make-buffer name :agent-name "help")))
    (init-face-registry buf)
    (setf (buffer-keymap buf) *default-keymap*)
    (setf (buffer-major-mode buf) "help")
    ;; Insert content as an agent message (from the \"help\" agent)
    (buffer-insert-agent-message buf content)
    (add-buffer-to-ring buf)
    buf))

(defcommand describe-function-command (:permission :user-only)
  "Open a minibuffer selector listing all functions.
On selection, displays detailed function description in a help buffer.
Bound to C-h f."
  (buffer)
  (declare (ignore buffer))
  (let* ((fn-list (list-functions))
         (items (mapcar (lambda (sym)
                          (let* ((name (string-downcase (symbol-name sym)))
                                 (cmd-meta (gethash sym *command-table*))
                                 (fn-obj (fdefinition sym))
                                 (type-str (cond
                                             ((macro-function sym) "macro")
                                             ((and cmd-meta
                                                   (typep fn-obj 'generic-function))
                                              "command")
                                             ((typep fn-obj 'generic-function)
                                              "generic")
                                             (t "function")))
                                 (keybinds (find-keybindings-for-command sym))
                                 (kb-str (if keybinds
                                             (format nil "  [~{~A~^, ~}]"
                                                     (mapcar #'format-key-binding keybinds))
                                             ""))
                                 (display (format nil "~A  (~A)~A" name type-str kb-str)))
                            (list :symbol sym
                                  :name name
                                  :display display)))
                        fn-list)))
    (minibuffer-activate
     "Describe Function" items
     (lambda (item)
       (let* ((sym (getf item :symbol))
              (desc (describe-function-to-string sym))
              (buf-name (format nil "*help:~A*"
                                (string-downcase (symbol-name sym))))
              ;; Reuse existing help buffer for this function if one exists
              (existing (find-buffer-by-name buf-name)))
         (if existing
             (switch-to-buffer existing)
             (let ((help-buf (make-help-buffer buf-name desc)))
               (switch-to-buffer help-buf))))))))

;;; --------------------------------------------------------------------------
;;; Introspection: list-variables & describe-variable
;;; --------------------------------------------------------------------------

(defun list-variables ()
  "Return a sorted list of variable symbols exported from the clawmacs package.
Includes all exported symbols that have global variable bindings (special
variables, constants, parameters)."
  (let ((variables nil))
    (do-external-symbols (sym :clawmacs)
      (when (boundp sym)
        (push sym variables)))
    (sort variables #'string< :key #'symbol-name)))

(defun variable-kind (sym)
  "Return a keyword describing the kind of variable SYM.
Returns :constant, :parameter, or :variable."
  (cond
    ((constantp sym) :constant)
    ;; Convention: *foo* with earmuffs is a special/dynamic variable.
    ;; defparameter defines a special variable with a default value — we call
    ;; those "parameter" to distinguish from plain defvar.  Since CL doesn't
    ;; store this distinction at runtime, we just rely on the earmuff naming.
    ((let ((name (symbol-name sym)))
       (and (> (length name) 2)
            (char= (char name 0) #\*)
            (char= (char name (1- (length name))) #\*)))
     :parameter)
    (t :variable)))

(defun truncate-value-string (value &optional (max-length 200))
  "Print VALUE to a string, truncating at MAX-LENGTH characters."
  (let* ((full (handler-case
                   (let ((*print-length* 20)
                         (*print-level* 3)
                         (*print-circle* t)
                         (*print-pretty* nil))
                     (prin1-to-string value))
                 (error (e)
                   (format nil "#<error printing value: ~A>" e))))
         (len (length full)))
    (if (> len max-length)
        (concatenate 'string (subseq full 0 max-length) "...")
        full)))

(defun describe-variable-to-string (var-symbol)
  "Return a human-readable string describing VAR-SYMBOL.
Includes: name, kind, type of current value, current value (truncated),
and docstring."
  (unless (and var-symbol (boundp var-symbol))
    (return-from describe-variable-to-string
      (format nil "~A is not a bound variable." var-symbol)))
  (with-output-to-string (s)
    ;; Header
    (format s "~A~%~A~%~%" var-symbol
            (make-string (min 60 (length (symbol-name var-symbol)))
                         :initial-element #\-))
    ;; Kind
    (let ((kind (variable-kind var-symbol)))
      (format s "Kind: ~A~%"
              (ecase kind
                (:constant  "Constant (defconstant)")
                (:parameter "Special Variable (defvar/defparameter)")
                (:variable  "Variable"))))
    ;; Value type
    (let ((val (symbol-value var-symbol)))
      (format s "Value Type: ~A~%" (type-of val))
      ;; Current value (truncated)
      (format s "Current Value: ~A~%" (truncate-value-string val)))
    ;; Docstring
    (let ((doc (or (documentation var-symbol 'variable) "")))
      (when (plusp (length doc))
        (format s "~%~A~%" doc)))
    ;; Extended documentation
    (let ((ext (extended-doc var-symbol)))
      (when ext
        (let ((side-effects (getf ext :side-effects)))
          (when side-effects
            (format s "~%Side Effects:~%  ~A~%" side-effects)))
        (let ((see-also (getf ext :see-also)))
          (when see-also
            (format s "~%See Also: ~{~(~A~)~^, ~}~%" see-also)))
        (let ((category (getf ext :category)))
          (when category
            (format s "~%Category: ~A~%" category)))))))

(defcommand describe-variable-command (:permission :user-only)
  "Open a minibuffer selector listing all exported variables.
On selection, displays detailed variable description in a help buffer.
Bound to C-h v."
  (buffer)
  (declare (ignore buffer))
  (let* ((var-list (list-variables))
         (items (mapcar (lambda (sym)
                          (let* ((name (string-downcase (symbol-name sym)))
                                 (kind (variable-kind sym))
                                 (kind-str (ecase kind
                                             (:constant "const")
                                             (:parameter "special")
                                             (:variable "var")))
                                 (val-preview
                                   (handler-case
                                       (let ((val (symbol-value sym)))
                                         (truncate-value-string val 40))
                                     (error () "#<unreadable>")))
                                 (display (format nil "~A  (~A)  = ~A"
                                                  name kind-str val-preview)))
                            (list :symbol sym
                                  :name name
                                  :display display)))
                        var-list)))
    (minibuffer-activate
     "Describe Variable" items
     (lambda (item)
       (let* ((sym (getf item :symbol))
              (desc (describe-variable-to-string sym))
              (buf-name (format nil "*help:~A*"
                                (string-downcase (symbol-name sym))))
              ;; Reuse existing help buffer for this variable if one exists
              (existing (find-buffer-by-name buf-name)))
         (if existing
             (switch-to-buffer existing)
             (let ((help-buf (make-help-buffer buf-name desc)))
               (switch-to-buffer help-buf))))))))

;;; --------------------------------------------------------------------------
;;; Introspection: list-types & describe-type
;;; --------------------------------------------------------------------------

(defun list-types ()
  "Return a sorted list of type-name symbols exported from the clawmacs package.
Includes CLOS classes, structures, and conditions — any exported symbol that
names a class (via find-class)."
  (let ((types nil))
    (do-external-symbols (sym :clawmacs)
      (when (find-class sym nil)
        (push sym types)))
    (sort types #'string< :key #'symbol-name)))

(defun type-kind (sym)
  "Return a keyword describing the kind of type SYM names.
Returns :condition, :structure, :standard-class, or :class."
  (let ((class (find-class sym nil)))
    (cond
      ((null class) :unknown)
      ((subtypep sym 'condition) :condition)
      ((typep class 'structure-class) :structure)
      ((typep class 'standard-class) :standard-class)
      (t :class))))

(defun type-kind-label (kind)
  "Return a human-readable label for a type-kind keyword."
  (ecase kind
    (:condition "Condition")
    (:structure "Structure (defstruct)")
    (:standard-class "Class (defclass)")
    (:class "Built-in Class")
    (:unknown "Unknown")))

(defun type-slot-info (class)
  "Return a list of plists describing each slot in CLASS.
Each plist has :name, :type, :initform, :initargs, :readers, :writers,
:allocation, and :documentation."
  #+sbcl
  (handler-case
      (progn
        ;; Ensure the class is finalized so slots are available
        (unless (sb-mop:class-finalized-p class)
          (sb-mop:finalize-inheritance class))
        (mapcar
         (lambda (slot)
           (list :name (sb-mop:slot-definition-name slot)
                 :type (let ((ty (sb-mop:slot-definition-type slot)))
                         (if (eq ty t) nil ty))
                 :initform (if (sb-mop:slot-definition-initfunction slot)
                               (sb-mop:slot-definition-initform slot)
                               :no-initform)
                 :initargs (sb-mop:slot-definition-initargs slot)
                 :readers (when (typep slot 'sb-mop:direct-slot-definition)
                            (sb-mop:slot-definition-readers slot))
                 :writers (when (typep slot 'sb-mop:direct-slot-definition)
                            (sb-mop:slot-definition-writers slot))
                 :allocation (sb-mop:slot-definition-allocation slot)
                 :documentation (documentation slot t)))
         (sb-mop:class-direct-slots class)))
  (error () nil))
  #-sbcl nil)

(defun type-struct-slot-info (sym)
  "Return a list of plists describing each slot in structure type SYM.
Uses sb-kernel:dd-slots to get defstruct slot details."
  #+sbcl
  (handler-case
      (let* ((layout (sb-kernel:find-layout sym))
             (dd (when layout (sb-kernel:layout-info layout))))
        (when dd
          (mapcar
           (lambda (dsd)
             (list :name (sb-kernel:dsd-name dsd)
                   :type (let ((ty (sb-kernel:dsd-type dsd)))
                           (if (eq ty t) nil ty))
                   :read-only (sb-kernel:dsd-read-only dsd)
                   :accessor (let ((acc-name (sb-kernel:dsd-accessor-name dsd)))
                               (when (fboundp acc-name) acc-name))))
           (sb-kernel:dd-slots dd))))
    (error () nil))
  #-sbcl nil)

(defun describe-type-to-string (type-symbol)
  "Return a human-readable string describing the type named by TYPE-SYMBOL.
Includes: name, kind, superclasses, slots/fields with their types, initforms,
accessors, and documentation. Also shows class-level and extended documentation."
  (let ((class (find-class type-symbol nil)))
    (unless class
      (return-from describe-type-to-string
        (format nil "~A does not name a type." type-symbol))))
  (with-output-to-string (s)
    ;; Header
    (format s "~A~%~A~%~%" type-symbol
            (make-string (min 60 (length (symbol-name type-symbol)))
                         :initial-element #\-))
    (let* ((class (find-class type-symbol))
           (kind (type-kind type-symbol)))
      ;; Kind
      (format s "Kind: ~A~%" (type-kind-label kind))
      ;; Superclasses
      #+sbcl
      (handler-case
          (let* ((supers (sb-mop:class-direct-superclasses class))
                 (super-names (mapcar #'class-name supers)))
            (when (and super-names
                       (not (equal super-names '(structure-object)))
                       (not (equal super-names '(standard-object))))
              (format s "Superclasses: ~{~A~^, ~}~%" super-names)))
        (error () nil))
      ;; Class documentation
      (let ((doc (documentation class t)))
        (when (and doc (plusp (length doc)))
          (format s "~%~A~%" doc)))
      ;; Slots / Fields
      (cond
        ;; Structure type — use defstruct slot introspection
        ((eq kind :structure)
         (let ((slots (type-struct-slot-info type-symbol)))
           (when slots
             (format s "~%Fields:~%")
             (dolist (slot slots)
               (let ((name (getf slot :name))
                     (type (getf slot :type))
                     (read-only (getf slot :read-only))
                     (accessor (getf slot :accessor)))
                 (format s "  ~A" name)
                 (when type (format s " : ~A" type))
                 (when read-only (format s "  [read-only]"))
                 (when accessor (format s "  (accessor: ~A)" accessor))
                 (format s "~%"))))))
        ;; CLOS class or condition — use MOP
        ((member kind '(:standard-class :condition))
         (let ((slots (type-slot-info class)))
           (when slots
             (format s "~%Slots:~%")
             (dolist (slot slots)
               (let ((name (getf slot :name))
                     (type (getf slot :type))
                     (initform (getf slot :initform))
                     (initargs (getf slot :initargs))
                     (readers (getf slot :readers))
                     (writers (getf slot :writers))
                     (doc (getf slot :documentation)))
                 (format s "  ~A" name)
                 (when type (format s " : ~A" type))
                 (format s "~%")
                 (when initargs
                   (format s "    Initargs: ~{~S~^, ~}~%" initargs))
                 (unless (eq initform :no-initform)
                   (format s "    Default: ~S~%" initform))
                 (when readers
                   (format s "    Readers: ~{~A~^, ~}~%" readers))
                 (when writers
                   (format s "    Writers: ~{~A~^, ~}~%" writers))
                 (when (and doc (plusp (length doc)))
                   (format s "    ~A~%" doc))))))))
      ;; Extended documentation
      (let ((ext (extended-doc type-symbol)))
        (when ext
          (let ((usage (getf ext :usage)))
            (when usage
              (format s "~%Usage:~%  ~A~%" usage)))
          (let ((returns (getf ext :returns)))
            (when returns
              (format s "~%Returns:~%  ~A~%" returns)))
          (let ((side-effects (getf ext :side-effects)))
            (when side-effects
              (format s "~%Side Effects:~%  ~A~%" side-effects)))
          (let ((see-also (getf ext :see-also)))
            (when see-also
              (format s "~%See Also: ~{~(~A~)~^, ~}~%" see-also)))
          (let ((category (getf ext :category)))
            (when category
              (format s "~%Category: ~A~%" category))))))))

(defun undocumented-types ()
  "Return a list of exported type symbols that lack a defdoc entry.
Useful for finding types that still need extended documentation."
  (let ((missing nil))
    (dolist (sym (list-types))
      (unless (gethash sym *extended-docs*)
        (push sym missing)))
    (nreverse missing)))

(defcommand describe-type-command (:permission :user-only)
  "Open a minibuffer selector listing all defined types.
On selection, displays detailed type description in a help buffer.
Bound to C-h T."
  (buffer)
  (declare (ignore buffer))
  (let* ((type-list (list-types))
         (items (mapcar (lambda (sym)
                          (let* ((name (string-downcase (symbol-name sym)))
                                 (kind (type-kind sym))
                                 (kind-str (ecase kind
                                             (:condition "condition")
                                             (:structure "struct")
                                             (:standard-class "class")
                                             (:class "built-in")
                                             (:unknown "unknown")))
                                 (class (find-class sym nil))
                                 (doc-preview
                                   (let ((doc (when class (documentation class t))))
                                     (if (and doc (plusp (length doc)))
                                         (let ((first-line
                                                 (subseq doc 0
                                                         (or (position #\Newline doc)
                                                             (min 50 (length doc))))))
                                           (if (> (length first-line) 50)
                                               (concatenate 'string (subseq first-line 0 47) "...")
                                               first-line))
                                         "")))
                                 (display (if (plusp (length doc-preview))
                                              (format nil "~A  (~A)  ~A" name kind-str doc-preview)
                                              (format nil "~A  (~A)" name kind-str))))
                            (list :symbol sym
                                  :name name
                                  :display display)))
                        type-list)))
    (minibuffer-activate
     "Describe Type" items
     (lambda (item)
       (let* ((sym (getf item :symbol))
              (desc (describe-type-to-string sym))
              (buf-name (format nil "*help:~A*"
                                (string-downcase (symbol-name sym))))
              ;; Reuse existing help buffer for this type if one exists
              (existing (find-buffer-by-name buf-name)))
         (if existing
             (switch-to-buffer existing)
             (let ((help-buf (make-help-buffer buf-name desc)))
               (switch-to-buffer help-buf))))))))

;;; --------------------------------------------------------------------------
;;; Describe Bindings (C-h b)
;;; --------------------------------------------------------------------------

(defun categorize-command (command-sym)
  "Return a category string for COMMAND-SYM based on its name.
Used to group keybindings in the describe-bindings listing."
  (let ((name (string-downcase (symbol-name command-sym))))
    (cond
      ((or (search "scroll" name) (search "forward-char" name)
           (search "backward-char" name) (search "forward-word" name)
           (search "backward-word" name) (search "beginning-of-line" name)
           (search "end-of-line" name))
       "Movement")
      ((or (search "kill" name) (search "delete" name)
           (search "yank" name) (search "insert-newline" name)
           (search "self-insert" name))
       "Editing")
      ((or (search "buffer" name) (search "save-session" name))
       "Buffers & Sessions")
      ((or (search "model" name) (search "select-model" name))
       "Model Selection")
      ((or (search "describe" name) (search "help" name)
           (search "customize" name))
       "Help & Introspection")
      ((or (search "debug" name) (search "toggle" name)
           (search "oauth" name))
       "Toggles & Misc")
      ((search "send-message" name)
       "Chat")
      (t "Other"))))

(defun describe-bindings-to-string (&optional (keymap *default-keymap*))
  "Return a formatted string listing all keybindings in KEYMAP, grouped by category.
Each binding shows the key notation and the command name."
  (let ((entries nil))
    ;; Collect all bindings as (key-string command-sym category) triples
    (maphash (lambda (key cmd)
               (let ((key-str (format-key-binding key)))
                 (push (list key-str cmd (categorize-command cmd)) entries)))
             (keymap-bindings keymap))
    ;; Group by category
    (let ((groups (make-hash-table :test #'equal)))
      (dolist (entry entries)
        (destructuring-bind (key-str cmd category) entry
          (declare (ignore cmd))
          (push (list key-str (third entry) (second entry)) (gethash category groups nil))))
      ;; Deduplicate: multiple keys may map to the same command.
      ;; For each category, collect unique (command → list-of-keys) then format.
      (with-output-to-string (s)
        (format s "Key Bindings~%")
        (format s "============~%~%")
        (format s "Keymap: ~A~%~%" (keymap-name keymap))
        ;; Define a stable category order
        (let ((category-order '("Chat" "Movement" "Editing" "Buffers & Sessions"
                                "Model Selection" "Help & Introspection"
                                "Toggles & Misc" "Other")))
          (dolist (category category-order)
            (let ((cat-entries (gethash category groups)))
              (when cat-entries
                (format s "~A~%" category)
                (format s "~A~%" (make-string (length category) :initial-element #\-))
                ;; Group by command symbol within the category
                (let ((cmd-keys (make-hash-table :test #'eq)))
                  (dolist (entry cat-entries)
                    (let ((key-str (first entry))
                          (cmd (third entry)))
                      (push key-str (gethash cmd cmd-keys nil))))
                  ;; Sort commands alphabetically and format
                  (let ((cmd-list nil))
                    (maphash (lambda (cmd keys)
                               (push (cons cmd (sort (copy-list keys) #'string<)) cmd-list))
                             cmd-keys)
                    (setf cmd-list (sort cmd-list #'string<
                                         :key (lambda (c)
                                                (string-downcase (symbol-name (car c))))))
                    (dolist (item cmd-list)
                      (let* ((cmd (car item))
                             (keys (cdr item))
                             (cmd-name (string-downcase (symbol-name cmd)))
                             (keys-str (format nil "~{~A~^, ~}" keys)))
                        (format s "  ~20A  ~A~%" keys-str cmd-name)))))
                (format s "~%")))))))))

(defcommand describe-bindings-command (:permission :user-only)
  "Open a help buffer listing all keybindings in the default keymap.
Bound to C-h b."
  (buffer)
  (declare (ignore buffer))
  (let* ((buf-name "*help:keybindings*")
         (existing (find-buffer-by-name buf-name)))
    (if existing
        (switch-to-buffer existing)
        (let ((help-buf (make-help-buffer buf-name
                                          (describe-bindings-to-string))))
          (switch-to-buffer help-buf)))))

;;; --------------------------------------------------------------------------
;;; Event Loop
;;; --------------------------------------------------------------------------

(defvar *debug-mode* nil
  "When non-nil, all API requests and responses are echoed into the chat
window as debug messages. Toggle interactively with C-c C-d.")

(defvar *debug-log-file* nil
  "When non-nil, a pathname to a file where detailed debug log entries are
appended. Set via the --debug-log <path> command-line flag. Unlike
*debug-mode* (which shows condensed info in the chat buffer), this logs
raw NDJSON lines, stream state transitions, CLI spawn args, stderr
output, and other low-level details useful for post-mortem debugging.")

(defvar *meta-pending* nil
  "When non-nil, the next key event is combined with Meta (ESC prefix).")

(defvar *cx-pending* nil
  "When non-nil, the next key event is combined with C-x prefix.")

(defvar *cc-pending* nil
  "When non-nil, the next key event is combined with C-c prefix.
C-c is reserved for buffer-mode-specific commands (e.g. C-c t).
Quit is C-x C-c (global command, uses C-x prefix).")

(defvar *ch-pending* nil
  "When non-nil, the next key event is combined with C-h prefix.
C-h is the help prefix (e.g. C-h b = describe bindings).")

(defvar *openai-oauth-pending* nil
  "When non-nil, an alist storing the active OAuth flow state:
(:code-verifier . string) (:state . string).")

(defvar *buffer-selector-active* nil
  "When non-nil, the buffer selector overlay is displayed.")

(defvar *buffer-selector-index* 0
  "The currently highlighted index in the buffer selector.")

(defvar *buffer-selector-scroll* 0
  "Scroll offset for the buffer selector (first visible entry index).")

(defvar *model-selector-active* nil
  "When non-nil, the model selector overlay is displayed.")

(defvar *model-selector-index* 0
  "The currently highlighted index in the model selector.")

(defvar *model-selector-scroll* 0
  "Scroll offset for the model selector (first visible entry index).")

(defvar *model-selector-entries* nil
  "List of model entries for the selector, each a plist
(:provider :keyword :model \"string\" :active-p bool).")

;;; --------------------------------------------------------------------------
;;; Minibuffer State
;;; --------------------------------------------------------------------------

(defvar *minibuffer-active* nil
  "When non-nil, the minibuffer is active and the cursor is in it.")

(defvar *minibuffer-prompt* ""
  "The prompt string displayed in the minibuffer (e.g. \"Select Model\").")

(defvar *minibuffer-input* ""
  "The current input text in the minibuffer.")

(defvar *minibuffer-point* 0
  "Cursor position within *minibuffer-input*.")

(defvar *minibuffer-items* nil
  "Complete list of candidate items for the minibuffer completion.
Each item is a plist with at least a :display key.")

(defvar *minibuffer-filtered-items* nil
  "Candidates after fuzzy-filtering by *minibuffer-input*.
Subset of *minibuffer-items*.")

(defvar *minibuffer-selected-index* 0
  "Index of the currently selected candidate in *minibuffer-filtered-items*.")

(defvar *minibuffer-callback* nil
  "Function called with the selected item plist when the user confirms.
Set to nil when inactive.")

(defvar *minibuffer-max-height* 12
  "Maximum number of rows the minibuffer can expand to (including prompt).")

(defvar *minibuffer-scroll-offset* 0
  "Index of the first visible candidate in the minibuffer.
When the selected item moves beyond the visible window, this offset
is adjusted so that the selection is always visible.")

(defvar *minibuffer-match-positions* nil
  "List of match-position lists parallel to *minibuffer-filtered-items*.
Each element is a sorted list of character indices (into the corresponding
item's display string) that were matched by the current query.
NIL entries mean no query is active or no positions were recorded.")

(defvar *model-selection-history* nil
  "List of recently selected model display strings (most recent first).
Used for recency sorting in the minibuffer model selector.")

(defvar *buffer-selection-history* nil
  "List of recently selected buffer names (most recent first).
Used for recency sorting in the minibuffer buffer selector.")


(defvar *deny-message-mode* nil
  "When non-nil, the input area is being used to type a denial message.")

;;; --------------------------------------------------------------------------
;;; Minibuffer Functions
;;; --------------------------------------------------------------------------

(defun split-query-tokens (query)
  "Split QUERY by spaces and return a list of non-empty token strings.
Used for orderless-style matching where each space-separated word must
independently match the candidate via subsequence search."
  (let ((result nil) (start nil))
    (dotimes (i (1+ (length query)))
      (let ((ch (when (< i (length query)) (char query i))))
        (if (or (null ch) (char= ch #\Space))
            (when start
              (push (subseq query start i) result)
              (setf start nil))
            (unless start
              (setf start i)))))
    (nreverse result)))

(defun fuzzy-token-match-p (token candidate)
  "Return T if all characters in TOKEN appear in CANDIDATE in order (case-insensitive).
Both TOKEN and CANDIDATE should already be downcased."
  (loop :with ci := 0
        :for qchar :across token
        :do (let ((pos (position qchar candidate :start ci)))
              (if pos
                  (setf ci (1+ pos))
                  (return nil)))
        :finally (return t)))

(defun fuzzy-match-p (query candidate)
  "Return T if QUERY matches CANDIDATE (case-insensitive).
Supports space-separated tokens (orderless-style): the query is split on
spaces and every token must independently match CANDIDATE via subsequence
search.  An empty query (or all-spaces query) matches everything."
  (let ((tokens (split-query-tokens (string-downcase query)))
        (c (string-downcase candidate)))
    (every (lambda (tok) (fuzzy-token-match-p tok c)) tokens)))

(defun fuzzy-token-positions (token candidate)
  "Return a list of character positions in CANDIDATE matched by TOKEN using
greedy left-to-right subsequence matching.  TOKEN and CANDIDATE must already
be downcased.  Returns NIL if TOKEN does not match CANDIDATE."
  (let ((positions nil))
    (loop :with ci := 0
          :for qchar :across token
          :do (let ((pos (position qchar candidate :start ci)))
                (if pos
                    (progn (push pos positions) (setf ci (1+ pos)))
                    (return-from fuzzy-token-positions nil))))
    (nreverse positions)))

(defun fuzzy-match-positions (query candidate)
  "Return a sorted list of character positions in CANDIDATE matched by QUERY.
Handles space-separated tokens (orderless-style): combines matched positions
from every token.  Returns NIL if any token fails to match or query is empty."
  (when (zerop (length (string-trim '(#\Space) query)))
    (return-from fuzzy-match-positions nil))
  (let ((tokens (split-query-tokens (string-downcase query)))
        (c (string-downcase candidate))
        (all-positions nil))
    (dolist (tok tokens)
      (let ((positions (fuzzy-token-positions tok c)))
        (unless positions
          (return-from fuzzy-match-positions nil))
        (setf all-positions (append all-positions positions))))
    (sort (remove-duplicates all-positions) #'<)))

(defun fuzzy-token-score (token candidate)
  "Return a relevance score for TOKEN matching CANDIDATE (both already downcased).
Returns NIL when TOKEN does not match.  Higher scores mean better matches.
Scoring bonuses:
  +100 exact match of the full token
  +50  candidate starts with token (prefix)
  +30  token appears as a contiguous substring
  +20  match starts at position 0 (decays by 1 per position)
  +5   each consecutive matched-character pair
  +8   each match that starts at a word boundary (after - _ / . or start)"
  (let ((positions (fuzzy-token-positions token candidate)))
    (unless positions (return-from fuzzy-token-score nil))
    (let ((score 1)
          (tlen (length token))
          (clen (length candidate)))
      ;; Exact match
      (when (string= token candidate) (incf score 100))
      ;; Prefix match (candidate starts with token)
      (when (and (<= tlen clen) (string= token (subseq candidate 0 tlen)))
        (incf score 50))
      ;; Contiguous substring present
      (when (search token candidate) (incf score 30))
      ;; Position bonus: earlier first-match = better (max +20 at position 0)
      (incf score (max 0 (- 20 (first positions))))
      ;; Consecutive character run bonus
      (loop :for i :from 1 :below (length positions)
            :when (= (nth i positions) (1+ (nth (1- i) positions)))
            :do (incf score 5))
      ;; Word-boundary start bonus (each matched char that starts at a boundary)
      (dolist (pos positions)
        (when (or (zerop pos)
                  (and (plusp pos)
                       (member (char candidate (1- pos))
                               '(#\- #\_ #\/ #\. #\Space))))
          (incf score 8)))
      score)))

(defun fuzzy-score (query candidate)
  "Return a relevance score for QUERY matching CANDIDATE (case-insensitive).
Handles space-separated tokens (orderless-style): scores each token
independently and sums them.  Returns 0 for an empty/blank query, or NIL
if any token fails to match."
  (when (zerop (length (string-trim '(#\Space) query)))
    (return-from fuzzy-score 0))
  (let ((tokens (split-query-tokens (string-downcase query)))
        (c (string-downcase candidate)))
    (unless tokens (return-from fuzzy-score 0))
    (let ((total 0))
      (dolist (tok tokens total)
        (let ((s (fuzzy-token-score tok c)))
          (unless s (return-from fuzzy-score nil))
          (incf total s))))))

(defun minibuffer-item-display (item)
  "Get the display string for a minibuffer candidate item.
If ITEM is a string, returns it directly. Otherwise returns the :display plist value."
  (if (stringp item)
      item
      (or (getf item :display) "")))

(defun minibuffer-activate (prompt items callback)
  "Activate the minibuffer with PROMPT text, a list of candidate ITEMS,
and a CALLBACK function to call with the selected item on confirmation."
  (setf *minibuffer-active* t
        *minibuffer-prompt* prompt
        *minibuffer-input* ""
        *minibuffer-point* 0
        *minibuffer-items* items
        *minibuffer-filtered-items* (copy-list items)
        *minibuffer-match-positions* (make-list (length items) :initial-element nil)
        *minibuffer-selected-index* 0
        *minibuffer-scroll-offset* 0
        *minibuffer-callback* callback))

(defun minibuffer-deactivate ()
  "Deactivate the minibuffer, clearing all state."
  (setf *minibuffer-active* nil
        *minibuffer-prompt* ""
        *minibuffer-input* ""
        *minibuffer-point* 0
        *minibuffer-items* nil
        *minibuffer-filtered-items* nil
        *minibuffer-match-positions* nil
        *minibuffer-selected-index* 0
        *minibuffer-scroll-offset* 0
        *minibuffer-callback* nil))

(defun minibuffer-update-filter ()
  "Re-filter *minibuffer-items* based on *minibuffer-input*.
When a non-empty query is present the matching candidates are scored and
sorted by relevance (highest score first) so the best match floats to the
top automatically.  Matched character positions are precomputed and stored in
*minibuffer-match-positions* for the renderer to use for highlighting.
Clamps *minibuffer-selected-index* to the new filtered list length and
resets the scroll offset."
  (let ((query *minibuffer-input*))
    (if (zerop (length query))
        ;; No query — show all items in their original order, no highlights.
        (setf *minibuffer-filtered-items* (copy-list *minibuffer-items*)
              *minibuffer-match-positions* (make-list (length *minibuffer-items*)
                                                      :initial-element nil))
        ;; Query present — filter, score, sort, record positions.
        (let* ((matched (remove-if-not
                         (lambda (item)
                           (fuzzy-match-p query (minibuffer-item-display item)))
                         *minibuffer-items*))
               ;; Pair each item with its relevance score.
               (scored (mapcar (lambda (item)
                                 (cons (or (fuzzy-score query
                                                        (minibuffer-item-display item))
                                           0)
                                       item))
                               matched))
               ;; Sort descending by score (stable-sort preserves recency order
               ;; for items that score equally).
               (sorted (stable-sort scored #'> :key #'car))
               (sorted-items (mapcar #'cdr sorted)))
          (setf *minibuffer-filtered-items* sorted-items
                *minibuffer-match-positions*
                (mapcar (lambda (item)
                          (fuzzy-match-positions query (minibuffer-item-display item)))
                        sorted-items)))))
  ;; Clamp selected index to valid range.
  (setf *minibuffer-selected-index*
        (max 0 (min *minibuffer-selected-index*
                    (1- (max 1 (length *minibuffer-filtered-items*))))))
  ;; Reset scroll and ensure the selection is visible.
  (setf *minibuffer-scroll-offset* 0)
  (minibuffer-ensure-visible))

(defun minibuffer-insert-char (char)
  "Insert CHAR at the current point in the minibuffer input and re-filter."
  (setf *minibuffer-input*
        (concatenate 'string
                     (subseq *minibuffer-input* 0 *minibuffer-point*)
                     (string char)
                     (subseq *minibuffer-input* *minibuffer-point*)))
  (incf *minibuffer-point*)
  (minibuffer-update-filter))

(defun minibuffer-delete-backward ()
  "Delete the character before point in the minibuffer input and re-filter."
  (when (plusp *minibuffer-point*)
    (setf *minibuffer-input*
          (concatenate 'string
                       (subseq *minibuffer-input* 0 (1- *minibuffer-point*))
                       (subseq *minibuffer-input* *minibuffer-point*)))
    (decf *minibuffer-point*)
    (minibuffer-update-filter)))

(defun minibuffer-visible-item-count ()
  "Return the number of candidate rows visible in the minibuffer.
This is the total minibuffer height minus 1 (for the prompt line)."
  (1- (min *minibuffer-max-height*
           (1+ (length *minibuffer-filtered-items*)))))

(defun minibuffer-ensure-visible ()
  "Adjust *minibuffer-scroll-offset* so that *minibuffer-selected-index*
is within the visible window of candidates."
  (let ((visible (minibuffer-visible-item-count)))
    (when (plusp visible)
      ;; If selection is above the visible window, scroll up
      (when (< *minibuffer-selected-index* *minibuffer-scroll-offset*)
        (setf *minibuffer-scroll-offset* *minibuffer-selected-index*))
      ;; If selection is below the visible window, scroll down
      (when (>= *minibuffer-selected-index*
                (+ *minibuffer-scroll-offset* visible))
        (setf *minibuffer-scroll-offset*
              (1+ (- *minibuffer-selected-index* visible)))))))

(defun minibuffer-next-item ()
  "Move the selection to the next candidate in the filtered list."
  (when (< *minibuffer-selected-index*
           (1- (length *minibuffer-filtered-items*)))
    (incf *minibuffer-selected-index*)
    (minibuffer-ensure-visible)))

(defun minibuffer-prev-item ()
  "Move the selection to the previous candidate in the filtered list."
  (when (plusp *minibuffer-selected-index*)
    (decf *minibuffer-selected-index*)
    (minibuffer-ensure-visible)))

(defun minibuffer-confirm ()
  "Confirm the current selection, invoke the callback, and deactivate."
  (let ((item (when (plusp (length *minibuffer-filtered-items*))
                (nth *minibuffer-selected-index* *minibuffer-filtered-items*)))
        (cb *minibuffer-callback*))
    (minibuffer-deactivate)
    (when (and item cb)
      (funcall cb item))))

(defun minibuffer-cancel ()
  "Cancel the minibuffer without invoking the callback."
  (minibuffer-deactivate))

(defun sort-models-by-recency (items)
  "Sort model ITEMS by recency (from *model-selection-history*) then alphabetically.
Items that were selected more recently appear first. Items not in the history
are sorted with the currently active model first, then alphabetically."
  (stable-sort (copy-list items)
               (lambda (a b)
                 (let* ((a-disp (getf a :display))
                        (b-disp (getf b :display))
                        (history *model-selection-history*)
                        (a-pos (position a-disp history :test #'string=))
                        (b-pos (position b-disp history :test #'string=)))
                   (cond
                     ;; Both in history: lower position (more recent) first
                     ((and a-pos b-pos) (< a-pos b-pos))
                     ;; Only a in history: a first
                     (a-pos t)
                     ;; Only b in history: b first
                     (b-pos nil)
                     ;; Neither: active model first, then alphabetical
                     ((and (getf a :active-p) (not (getf b :active-p))) t)
                     ((and (getf b :active-p) (not (getf a :active-p))) nil)
                     (t (string< a-disp b-disp)))))))

(defun sort-buffers-by-recency (items)
  "Sort buffer ITEMS by recency (from *buffer-selection-history*) then alphabetically.
Items that were selected more recently appear first. Items not in the history
are sorted with the current buffer first, then alphabetically."
  (stable-sort (copy-list items)
               (lambda (a b)
                 (let* ((a-disp (getf a :display))
                        (b-disp (getf b :display))
                        (history *buffer-selection-history*)
                        (a-pos (position a-disp history :test #'string=))
                        (b-pos (position b-disp history :test #'string=)))
                   (cond
                     ;; Both in history: lower position (more recent) first
                     ((and a-pos b-pos) (< a-pos b-pos))
                     ;; Only a in history: a first
                     (a-pos t)
                     ;; Only b in history: b first
                     (b-pos nil)
                     ;; Neither: current buffer first, then alphabetical
                     ((and (getf a :current-p) (not (getf b :current-p))) t)
                     ((and (getf b :current-p) (not (getf a :current-p))) nil)
                     (t (string< a-disp b-disp)))))))

(defun handle-minibuffer-key (key)
  "Handle a key event while the minibuffer is active.
Supports: C-g (cancel), Return (confirm), C-n/Down and C-p/Up (navigate),
Backspace (delete), C-a/C-e (move), C-u (kill all), and self-insert."
  (let ((base-key (if (and (listp key) (= (length key) 2)
                           (member (first key) '(:alt :ctrl-x :ctrl-c)))
                      (second key)
                      key)))
    (cond
      ;; C-g: cancel
      ((and (characterp base-key) (char= base-key (code-char 7)))
       (minibuffer-cancel))
      ;; Return/Newline: confirm selection
      ((and (characterp base-key) (or (char= base-key #\Return)
                                       (char= base-key #\Newline)))
       (minibuffer-confirm))
      ;; C-n or Down arrow: next item
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (minibuffer-next-item))
      ;; C-p or Up arrow: previous item
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (minibuffer-prev-item))
      ;; Backspace: delete character before point
      ((or (eq base-key :backspace)
           (and (characterp base-key) (or (char= base-key #\Backspace)
                                           (char= base-key #\Rubout))))
       (minibuffer-delete-backward))
      ;; C-a: beginning of input
      ((and (characterp base-key) (char= base-key #\Soh))
       (setf *minibuffer-point* 0))
      ;; C-e: end of input
      ((and (characterp base-key) (char= base-key #\Enq))
       (setf *minibuffer-point* (length *minibuffer-input*)))
      ;; C-u: kill all input
      ((and (characterp base-key) (char= base-key (code-char 21)))
       (setf *minibuffer-input* ""
             *minibuffer-point* 0)
       (minibuffer-update-filter))
      ;; Self-insert: printable characters
      ((and (characterp base-key) (graphic-char-p base-key))
       (minibuffer-insert-char base-key))
      ;; Everything else: ignore
      (t nil))))


(defun handle-buffer-selector-key (key)
  "Handle a key event while the buffer selector is active.
Strips any meta/ctrl-x prefix so the selector has simple key bindings."
  (let ((base-key (if (and (listp key) (= (length key) 2)
                           (member (first key) '(:alt :ctrl-x :ctrl-c)))
                      (second key)
                      key))
        (num-buffers (length *buffer-ring*)))
    (cond
      ;; C-g: cancel selector
      ((and (characterp base-key) (char= base-key (code-char 7)))
       (setf *buffer-selector-active* nil))
      ;; q: cancel selector
      ((and (characterp base-key) (char= base-key #\q))
       (setf *buffer-selector-active* nil))
      ;; Enter: select highlighted buffer and close
      ((and (characterp base-key) (or (char= base-key #\Return)
                                       (char= base-key #\Newline)))
       (let ((selected (nth *buffer-selector-index* *buffer-ring*)))
         (when selected
           (switch-to-buffer selected)))
       (setf *buffer-selector-active* nil))
      ;; C-p or Up arrow: move highlight up
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (when (plusp *buffer-selector-index*)
         (decf *buffer-selector-index*)))
      ;; C-n or Down arrow: move highlight down
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (when (< *buffer-selector-index* (1- num-buffers))
         (incf *buffer-selector-index*)))
      ;; n: create new session buffer and switch to it
      ((and (characterp base-key) (char= base-key #\n))
       (let* ((name (next-buffer-name))
              (new-buf (make-buffer name
                                    :agent-name *default-agent-name*
                                    :working-directory (truename "."))))
         (init-face-registry new-buf)
         (setf (buffer-keymap new-buf) *default-keymap*)
         (add-buffer-to-ring new-buf)
         (switch-to-buffer new-buf))
       (setf *buffer-selector-active* nil))
      ;; k: kill highlighted buffer (unless it is the last one)
      ((and (characterp base-key) (char= base-key #\k))
       (when (> num-buffers 1)
         (let ((target (nth *buffer-selector-index* *buffer-ring*)))
           (when target
             (kill-buffer-from-ring target)
             (setf *buffer-selector-index*
                   (min *buffer-selector-index*
                        (max 0 (1- (length *buffer-ring*)))))))))
      ;; Everything else: ignore
      (t nil))))

(defun handle-model-selector-key (key buf)
  "Handle a key event while the model selector is active.
Strips any meta/ctrl-x/ctrl-c prefix so the selector has simple key bindings.
On Enter, sets the buffer's provider and model overrides to the selected entry."
  (let ((base-key (if (and (listp key) (= (length key) 2)
                           (member (first key) '(:alt :ctrl-x :ctrl-c)))
                      (second key)
                      key))
        (num-entries (length *model-selector-entries*)))
    (cond
      ;; C-g: cancel selector
      ((and (characterp base-key) (char= base-key (code-char 7)))
       (setf *model-selector-active* nil))
      ;; q: cancel selector
      ((and (characterp base-key) (char= base-key #\q))
       (setf *model-selector-active* nil))
      ;; Enter: select highlighted model and apply to current buffer
      ((and (characterp base-key) (or (char= base-key #\Return)
                                       (char= base-key #\Newline)))
       (let ((selected (nth *model-selector-index* *model-selector-entries*)))
         (when selected
           (let ((provider (getf selected :provider))
                 (model (getf selected :model)))
             (set-buffer-provider-override buf provider)
             (set-buffer-model-override buf model)
             ;; Show confirmation in chat
             (let ((sys-msg (buffer-insert-agent-message
                             buf (format nil "[Model changed to ~(~A~)/~A]"
                                        provider model))))
               (setf (message-sender sys-msg) :system)))))
       (setf *model-selector-active* nil))
      ;; C-p or Up arrow: move highlight up
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (when (plusp *model-selector-index*)
         (decf *model-selector-index*)))
      ;; C-n or Down arrow: move highlight down
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (when (< *model-selector-index* (1- num-entries))
         (incf *model-selector-index*)))
      ;; Everything else: ignore
      (t nil))))

(defun handle-key-event (buf key)
  "Dispatch a normalized key through the buffer's keymap.
Returns :QUIT if the application should exit, or nil otherwise.
Handles approval mode, deny-message mode, and normal dispatch.
KEY is already normalized by the backend before calling this."
  (let ((*current-caller* :user))
    (when (null key)
      (return-from handle-key-event nil))
    (cond
      ;; C-x C-c always quits (Emacs standard quit chord)
      ((equal key (list :ctrl-x #\Etx))
       :quit)

      ;; === MINIBUFFER MODE ===
      ;; When the minibuffer is active, it captures all input
      (*minibuffer-active*
       (handle-minibuffer-key key)
       nil)

      ;; === BUFFER SELECTOR MODE ===
      ;; Navigation and selection within the buffer list overlay
      (*buffer-selector-active*
       (handle-buffer-selector-key key)
       nil)

      ;; === MODEL SELECTOR MODE ===
      ;; Navigation and selection within the model list overlay
      (*model-selector-active*
       (handle-model-selector-key key buf)
       nil)

      ;; === CUSTOMIZE MODE ===
      ;; When the current buffer is a customize buffer, dispatch to
      ;; the customize key handler for field navigation and editing.
      ;; Clean up stale state if the customize buffer was killed.
      ((and *customize-face-state*
            (let ((cbuf (getf *customize-face-state* :buffer)))
              (if (member cbuf *buffer-ring*)
                  (eq buf cbuf)
                  (progn (setf *customize-face-state* nil) nil))))
       (handle-customize-key key)
       nil)

      ;; === OPENAI OAUTH MODE ===
      ;; User is pasting the callback URL; Enter/Return submits, C-g cancels
      (*openai-oauth-pending*
       (cond
         ;; C-g: cancel OAuth flow
         ((and (characterp key) (char= key (code-char 7)))
          (setf *openai-oauth-pending* nil
                (buffer-status buf) :idle)
          (let ((sys-msg (buffer-insert-agent-message buf "[OAuth cancelled]")))
            (setf (message-sender sys-msg) :system)))
         ;; Return or C-j: submit callback URL
         ((and (characterp key) (or (char= key #\Return)
                                     (char= key #\Newline)))
          (let ((callback-url (message-text (buffer-input-message buf))))
            (handler-case
                (let* ((code-verifier (cdr (assoc :code-verifier *openai-oauth-pending*)))
                       (expected-state (cdr (assoc :state *openai-oauth-pending*))))
                  (openai-codex-oauth-finish callback-url code-verifier expected-state)
                  (let ((sys-msg (buffer-insert-agent-message
                                  buf "[OpenAI Codex OAuth: Login successful! Token saved.]")))
                    (setf (message-sender sys-msg) :system)))
              (error (e)
                (let ((sys-msg (buffer-insert-agent-message
                                buf (format nil "[OAuth error: ~A]" e))))
                  (setf (message-sender sys-msg) :system)))))
          (setf *openai-oauth-pending* nil
                (buffer-status buf) :idle)
          (set-message-text (buffer-input-message buf) ""))
         ;; Normal editing (but not send-message)
         ((keymap-lookup (buffer-keymap buf) key)
          (let ((command (keymap-lookup (buffer-keymap buf) key)))
            (unless (eq command 'send-message)
              (funcall command buf))))
         ;; Self-insert for pasting URL characters
         ((and (characterp key) (graphic-char-p key))
          (let ((*self-insert-char* key))
            (self-insert-command buf))))
       nil)

      ;; === DENY MESSAGE MODE ===
      ;; User is typing a denial reason; Enter submits, normal editing works
      (*deny-message-mode*
       (cond
         ((and (characterp key) (char= key #\Newline))
          ;; Submit denial message
          (let ((reason (message-text (buffer-input-message buf))))
            (setf *deny-message-mode* nil)
            (handle-approval-response buf (cons :deny-with-message reason))))
         ;; Normal editing in the input area
         ((keymap-lookup (buffer-keymap buf) key)
          (let ((command (keymap-lookup (buffer-keymap buf) key)))
            ;; Only allow editing commands, not send-message
            (unless (eq command 'send-message)
              (funcall command buf))))
         ((and (characterp key) (graphic-char-p key))
          (let ((*self-insert-char* key))
            (self-insert-command buf))))
       nil)

      ;; === APPROVAL MODE ===
      ;; Waiting for a/d/m keypress
      ((buffer-approval-pending buf)
       (when (characterp key)
         (case key
           (#\a (handle-approval-response buf :approve))
           (#\d (handle-approval-response buf :deny))
           (#\m
            ;; Switch to deny-message mode: clear input for typing reason
            (set-message-text (buffer-input-message buf) "")
            (setf *deny-message-mode* t))))
       nil)

      ;; === NORMAL MODE ===
      ;; Keymap lookup
      ((keymap-lookup (buffer-keymap buf) key)
       (let ((command (keymap-lookup (buffer-keymap buf) key)))
         (funcall command buf)
         (when (and (characterp key)
                    (not (member command '(scroll-up-command scroll-down-command))))
           (setf (buffer-scroll-offset buf) 0))
         nil))
      ;; Self-insert
      ((and (characterp key) (graphic-char-p key))
       (let ((*self-insert-char* key))
         (self-insert-command buf))
       (setf (buffer-scroll-offset buf) 0)
       nil)
      (t nil))))

(defvar *user-init-directory*
  (merge-pathnames #P".clawmacs.d/" (user-homedir-pathname))
  "Directory for user Lisp configuration files.")

(defvar *user-init-file*
  (merge-pathnames "init.lisp" *user-init-directory*)
  "Path to the user init file, loaded at startup if it exists.")

(defvar *inhibit-user-init* nil
  "When non-nil, skip loading the user init file at startup.")

(defun load-user-init-file ()
  "Load ~/.clawmacs.d/init.lisp if it exists. Errors are caught and reported."
  (when *inhibit-user-init*
    (return-from load-user-init-file nil))
  (let ((init-path (probe-file *user-init-file*)))
    (when init-path
      (handler-case
          (let ((*package* (find-package :clawmacs)))
            (load init-path :verbose nil :print nil))
        (error (e)
          (format *error-output*
                  "~&;; Warning: error loading ~A:~%;; ~A~%"
                  init-path e)
          (file-debug-log "init" "error loading ~A: ~A" init-path e)
          nil)))))

(defun parse-clawmacs-args ()
  "Parse command-line arguments and environment variables.
Recognized flags:
  --debug-log <path>   Enable file-based debug logging to <path>.
  --no-init            Skip loading the user init file.
Environment variables:
  CLAWMACS_DEBUG_LOG   Same as --debug-log (CLI flag takes precedence)."
  ;; CLI args (everything after SBCL's -- separator)
  (let ((args (uiop:command-line-arguments)))
    (loop :while args
          :for arg := (pop args)
          :do (cond
                ((string= arg "--debug-log")
                 (let ((path (pop args)))
                   (when path
                     (setf *debug-log-file* (pathname path)))))
                ((string= arg "--no-init")
                 (setf *inhibit-user-init* t)))))
  ;; Environment variable fallback
  (unless *debug-log-file*
    (let ((env (uiop:getenv "CLAWMACS_DEBUG_LOG")))
      (when (and env (plusp (length env)))
        (setf *debug-log-file* (pathname env)))))
  ;; Log startup marker
  (when *debug-log-file*
    (file-debug-log "startup" "debug log enabled, writing to ~A" *debug-log-file*)))

(defun clawmacs-main (&key (session-name "clawmacs:session-01")
                           (agent-name *default-agent-name*))
  "Entry point for clawmacs. Initializes state and delegates to the UI backend."
  (parse-clawmacs-args)
  (init-default-keymap)
  (init-tools)
  (init-global-faces)
  ;; Load custom system prompt from file if it exists
  (when (probe-file *system-prompt-path*)
    (setf *system-prompt*
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-string *system-prompt-path*))))
  ;; User init runs BEFORE backend — so (setf *ui-backend* ...) works
  (load-user-init-file)
  ;; Default to croatoan terminal backend
  (unless *ui-backend*
    (setf *ui-backend* (make-instance 'croatoan-backend)))
  ;; Create initial buffer and initialize global state
  (let ((buf (make-buffer session-name
                          :agent-name agent-name
                          :working-directory (truename "."))))
    (init-face-registry buf)
    (setf (buffer-keymap buf) *default-keymap*)
    ;; Initialize buffer ring, selector state, and OAuth state
    (setf *buffer-ring* nil *buffer-counter* 0)
    (setf *buffer-selector-active* nil
          *buffer-selector-index* 0
          *buffer-selector-scroll* 0)
    (setf *model-selector-active* nil
          *model-selector-index* 0
          *model-selector-scroll* 0
          *model-selector-entries* nil)
    ;; Initialize minibuffer state
    (setf *minibuffer-active* nil
          *minibuffer-prompt* ""
          *minibuffer-input* ""
          *minibuffer-point* 0
          *minibuffer-items* nil
          *minibuffer-filtered-items* nil
          *minibuffer-selected-index* 0
          *minibuffer-scroll-offset* 0
          *minibuffer-callback* nil)
    (setf *openai-oauth-pending* nil)
    (setf *meta-pending* nil *cx-pending* nil *cc-pending* nil *ch-pending* nil)
    (add-buffer-to-ring buf)
    ;; Set sandbox root to the working directory
    (setf *sandbox-root* (truename "."))
    ;; Delegate to the backend
    (backend-run *ui-backend* buf)))
