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
          (agent-msg (buffer-insert-agent-message buf display)))
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
            (set-message-text msg all-text))))
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
              (set-message-text msg display)
              (setf (message-raw-content msg) canonical-content))
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
                               :agent-name "claude"
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

;;; --------------------------------------------------------------------------
;;; Face Registry Setup
;;; --------------------------------------------------------------------------

(defun init-face-registry (buf)
  "Populate BUF's face registry with default face sets."
  (let* ((registry (buffer-face-registry buf))
         (agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (user-fs (make-default-user-face-set))
         (agent-fs (make-default-agent-face-set agent-kw)))
    (setf (gethash :user registry) user-fs
          (gethash agent-kw registry) agent-fs)
    (setf (message-face-set (buffer-input-message buf)) user-fs)
    buf))

;;; --------------------------------------------------------------------------
;;; Event Loop
;;; --------------------------------------------------------------------------

(defvar *meta-pending* nil
  "When non-nil, the next key event is combined with Meta (ESC prefix).")

(defvar *cx-pending* nil
  "When non-nil, the next key event is combined with C-x prefix.")

(defvar *cc-pending* nil
  "When non-nil, the next key event is combined with C-c prefix.
C-c is reserved for buffer-mode-specific commands (e.g. C-c t).
Quit is C-x C-c (global command, uses C-x prefix).")

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

(defvar *model-selection-history* nil
  "List of recently selected model display strings (most recent first).
Used for recency sorting in the minibuffer model selector.")

(defvar *buffer-selection-history* nil
  "List of recently selected buffer names (most recent first).
Used for recency sorting in the minibuffer buffer selector.")

(defun normalize-key (event)
  "Extract and normalize a key from a croatoan EVENT.
Returns a character, a keyword (for special keys), a list (:alt <key>)
for Meta combinations, a list (:ctrl-x <key>) for C-x prefix (global
commands), or a list (:ctrl-c <key>) for C-c prefix (mode-specific commands)."
  (let* ((raw-key (if (typep event 'croatoan:event)
                      (croatoan:event-key event)
                      event))
         (ctrl-p (and (typep raw-key 'croatoan:key)
                      (croatoan:key-ctrl raw-key)))
         (alt-p (and (typep raw-key 'croatoan:key)
                     (croatoan:key-alt raw-key)))
         (key (if (typep raw-key 'croatoan:key)
                   (croatoan:key-name raw-key)
                   raw-key)))
    (cond
      ((and ctrl-p (eql key :backspace))
       (list :ctrl :backspace))
      ((and alt-p (eql key :backspace))
       (list :alt :backspace))
      ;; ── Pending prefix resolution (must come BEFORE raw prefix detection) ──
      ;; When a prefix key was already pressed, the NEXT keystroke completes the
      ;; chord.  We must check these first, otherwise a raw C-c/C-x/ESC that
      ;; arrives as the second key would start a *new* prefix instead of
      ;; completing the chord (e.g. C-x C-c would never produce (:ctrl-x #\Etx)).
      (*meta-pending*
       (setf *meta-pending* nil)
       (list :alt key))
      (*cx-pending*
       (setf *cx-pending* nil)
       (list :ctrl-x key))
      (*cc-pending*
       (setf *cc-pending* nil)
       (list :ctrl-c key))
      ;; ── Raw prefix detection (first key of a chord) ──
      ;; ESC received: set meta-pending, return nil (consume the ESC)
      ((and (characterp key) (char= key #\Esc))
       (setf *meta-pending* t)
       nil)
      ;; C-x received (ASCII 24): set cx-pending, return nil
      ((and (characterp key) (char= key (code-char 24)))
       (setf *cx-pending* t)
       nil)
      ;; C-c received (ASCII 3 = ETX): set cc-pending, return nil.
      ;; C-c is the prefix for buffer-mode-specific commands.
      ((and (characterp key) (char= key #\Etx))
       (setf *cc-pending* t)
       nil)
      ;; Normal key
      (t key))))

(defvar *deny-message-mode* nil
  "When non-nil, the input area is being used to type a denial message.")

;;; --------------------------------------------------------------------------
;;; Minibuffer Functions
;;; --------------------------------------------------------------------------

(defun fuzzy-match-p (query candidate)
  "Return T if all characters in QUERY appear in CANDIDATE in order (case-insensitive).
Used for narrowing the minibuffer candidate list as the user types."
  (let ((q (string-downcase query))
        (c (string-downcase candidate)))
    (loop :with ci := 0
          :for qchar :across q
          :do (let ((pos (position qchar c :start ci)))
                (if pos
                    (setf ci (1+ pos))
                    (return nil)))
          :finally (return t))))

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
        *minibuffer-selected-index* 0
        *minibuffer-callback* callback
        *minibuffer-max-height* 12))

(defun minibuffer-deactivate ()
  "Deactivate the minibuffer, clearing all state."
  (setf *minibuffer-active* nil
        *minibuffer-prompt* ""
        *minibuffer-input* ""
        *minibuffer-point* 0
        *minibuffer-items* nil
        *minibuffer-filtered-items* nil
        *minibuffer-selected-index* 0
        *minibuffer-callback* nil))

(defun minibuffer-update-filter ()
  "Re-filter *minibuffer-items* based on *minibuffer-input*.
Clamps *minibuffer-selected-index* to the new filtered list length."
  (setf *minibuffer-filtered-items*
        (if (zerop (length *minibuffer-input*))
            (copy-list *minibuffer-items*)
            (remove-if-not (lambda (item)
                             (fuzzy-match-p *minibuffer-input*
                                            (minibuffer-item-display item)))
                           *minibuffer-items*)))
  ;; Clamp selected index
  (setf *minibuffer-selected-index*
        (max 0 (min *minibuffer-selected-index*
                    (1- (max 1 (length *minibuffer-filtered-items*)))))))

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

(defun minibuffer-next-item ()
  "Move the selection to the next candidate in the filtered list."
  (when (< *minibuffer-selected-index*
           (1- (length *minibuffer-filtered-items*)))
    (incf *minibuffer-selected-index*)))

(defun minibuffer-prev-item ()
  "Move the selection to the previous candidate in the filtered list."
  (when (plusp *minibuffer-selected-index*)
    (decf *minibuffer-selected-index*)))

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

(defun update-window-layout (scr main-win modeline-win minibuffer-win)
  "Resize and reposition all windows based on screen dimensions and minibuffer state.
Layout from top to bottom: main-win, modeline-win (1 row), minibuffer-win."
  (let* ((h (croatoan:height scr))
         (w (croatoan:width scr))
         (mb-h (minibuffer-current-height))
         (main-h (max 1 (- h 1 mb-h))))
    (croatoan:resize main-win main-h w)
    (croatoan:resize modeline-win 1 w)
    (croatoan:move-window modeline-win main-h 0)
    (croatoan:resize minibuffer-win mb-h w)
    (croatoan:move-window minibuffer-win (1+ main-h) 0)
    ;; Update scroll page size based on available history area
    (setf *scroll-page-size* (max 1 (- main-h 3)))))

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
                                    :agent-name "claude"
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

(defun handle-key-event (buf event)
  "Dispatch a key event through the buffer's keymap.
Returns :QUIT if the application should exit, or nil otherwise.
Handles approval mode, deny-message mode, ESC prefix, and normal dispatch."
  (let ((key (normalize-key event))
        (*current-caller* :user))
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

(defun clawmacs-main (&key (session-name "clawmacs:session-01")
                           (agent-name "claude"))
  "Entry point for clawmacs. Initializes the TUI and runs the event loop."
  (init-default-keymap)
  (init-tools)
  ;; Load custom system prompt from file if it exists
  (when (probe-file *system-prompt-path*)
    (setf *system-prompt*
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-string *system-prompt-path*))))
  ;; Boot files are loaded dynamically by build-system-prompt on each API call
  ;; :process-control-chars nil puts the terminal into raw mode (ncurses:raw)
  ;; instead of cbreak mode, so C-c is delivered as a keystroke (ASCII 3)
  ;; rather than generating SIGINT.  This is required for C-c to work as
  ;; a prefix key (e.g. C-c t).  Quit is C-x C-c.
  (croatoan:with-screen (scr :input-echoing nil
                             :input-blocking t
                             :cursor-visible t
                             :enable-colors t
                             :process-control-chars nil)
    (let* ((screen-height (croatoan:height scr))
           (screen-width (croatoan:width scr))
           ;; Three-window layout: main (chat), modeline (1 row), minibuffer (bottom)
           (main-win (make-instance 'croatoan:window
                       :height (- screen-height 2)
                       :width screen-width
                       :position '(0 0)))
           (modeline-win (make-instance 'croatoan:window
                           :height 1
                           :width screen-width
                           :position (list (- screen-height 2) 0)))
           (minibuffer-win (make-instance 'croatoan:window
                             :height 1
                             :width screen-width
                             :position (list (1- screen-height) 0)))
            (buf (make-buffer session-name
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
            *minibuffer-callback* nil
            *minibuffer-max-height* 12)
      (setf *openai-oauth-pending* nil)
      (setf *meta-pending* nil *cx-pending* nil *cc-pending* nil)
      (add-buffer-to-ring buf)
      ;; Set sandbox root to the working directory
      (setf *sandbox-root* (truename "."))
      ;; Set scroll page size based on available history area
      (setf *scroll-page-size* (max 1 (- (- screen-height 2) 3)))
      ;; Flush stdscr's pending clear before our first render
      (croatoan:refresh scr)
      ;; Local render helper: updates window layout (for dynamic minibuffer height)
      ;; then renders all three windows. Centralizes the render dispatch.
      (labels ((do-render (buf)
                 (update-window-layout scr main-win modeline-win minibuffer-win)
                 (cond
                   (*buffer-selector-active*
                    (render-buffer-selector main-win modeline-win))
                   (*model-selector-active*
                    (render-model-selector main-win modeline-win))
                   (t
                    (render-buffer buf main-win modeline-win)))
                 (render-minibuffer minibuffer-win)))
        ;; Initial render
        (do-render (current-buffer))
        ;; Event loop: current-buffer may change between iterations.
        ;; Short timeout when streaming is active for polling updates.
        (loop :named main-loop
            :for buf := (current-buffer)
            :for streaming := (buffer-pending-stream buf)
            :do (progn
                  (setf (croatoan:input-blocking scr)
                        (if streaming 100 t))
                  (let ((event (croatoan:get-wide-event scr)))
                    (cond
                        ;; No event (timeout) -- poll streaming and re-render
                        ((null event)
                         (when streaming
                           (update-streaming-response buf)
                           (do-render buf)))
                        ;; Window resize event -- re-layout and re-render
                        ((and (typep event 'croatoan:event)
                              (typep (croatoan:event-key event) 'croatoan:key)
                              (eq :resize (croatoan:key-name
                                           (croatoan:event-key event))))
                         (do-render (current-buffer)))
                        ;; Key event -- dispatch then re-render
                        (t
                         (let ((result (handle-key-event buf event)))
                           (when (eq result :quit)
                             (return-from main-loop)))
                         (let ((cur (current-buffer)))
                           (when (buffer-pending-stream cur)
                             (update-streaming-response cur))
                           (do-render cur)))))))))))
