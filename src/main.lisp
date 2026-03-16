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
                                     (:tool-use-id . ,(cdr (assoc :tool-id r)))
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
      (send-to-agent-with-context buffer))))

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
;;; Buffer Management Commands
;;; --------------------------------------------------------------------------

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

(defun normalize-key (event)
  "Extract and normalize a key from a croatoan EVENT.
Returns a character, a keyword (for special keys), a list (:alt <key>)
for Meta combinations, or a list (:ctrl-x <key>) for C-x prefix."
  (let* ((raw-key (if (typep event 'croatoan:event)
                      (croatoan:event-key event)
                      event))
         (key (if (typep raw-key 'croatoan:key)
                  (croatoan:key-name raw-key)
                  raw-key)))
    (cond
      ;; ESC received: set meta-pending, return nil (consume the ESC)
      ((and (characterp key) (char= key #\Esc))
       (setf *meta-pending* t)
       nil)
      ;; C-x received (ASCII 24): set cx-pending, return nil
      ((and (characterp key) (char= key (code-char 24)))
       (setf *cx-pending* t)
       nil)
      ;; Meta prefix is active: combine with this key
      (*meta-pending*
       (setf *meta-pending* nil)
       (list :alt key))
      ;; C-x prefix is active: combine with this key
      (*cx-pending*
       (setf *cx-pending* nil)
       (list :ctrl-x key))
      ;; Normal key
      (t key))))

(defvar *deny-message-mode* nil
  "When non-nil, the input area is being used to type a denial message.")

(defun handle-key-event (buf event)
  "Dispatch a key event through the buffer's keymap.
Returns :QUIT if the application should exit, or nil otherwise.
Handles approval mode, deny-message mode, ESC prefix, and normal dispatch."
  (let ((key (normalize-key event))
        (*current-caller* :user))
    (when (null key)
      (return-from handle-key-event nil))
    (cond
      ;; C-c always quits
      ((and (characterp key) (char= key #\Etx))
       :quit)

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
                           (agent-name "echo-agent"))
  "Entry point for clawmacs. Initializes the TUI and runs the event loop."
  (init-default-keymap)
  (init-tools)
  ;; Load custom system prompt from file if it exists
  (when (probe-file *system-prompt-path*)
    (setf *system-prompt*
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-string *system-prompt-path*))))
  ;; Boot files are loaded dynamically by build-system-prompt on each API call
  (croatoan:with-screen (scr :input-echoing nil
                             :input-blocking t
                             :cursor-visible t
                             :enable-colors t)
    (let* ((screen-height (croatoan:height scr))
           (screen-width (croatoan:width scr))
           (main-win (make-instance 'croatoan:window
                       :height (1- screen-height)
                       :width screen-width
                       :position '(0 0)))
           (modeline-win (make-instance 'croatoan:window
                           :height 1
                           :width screen-width
                           :position (list (1- screen-height) 0)))
            (buf (make-buffer session-name
                              :agent-name agent-name
                              :working-directory (truename "."))))
      (init-face-registry buf)
      (setf (buffer-keymap buf) *default-keymap*)
      ;; Initialize buffer ring
      (setf *buffer-ring* nil *buffer-counter* 0)
      (add-buffer-to-ring buf)
      ;; Set sandbox root to the working directory
      (setf *sandbox-root* (truename "."))
      ;; Set scroll page size based on available history area
      (setf *scroll-page-size* (max 1 (- (1- screen-height) 3)))
      ;; Flush stdscr's pending clear before our first render
      (croatoan:refresh scr)
      ;; Initial render
      (render-buffer (current-buffer) main-win modeline-win)
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
                      ((null event)
                       (when streaming
                         (update-streaming-response buf)
                         (render-buffer buf main-win modeline-win)))
                      ((and (typep event 'croatoan:event)
                            (typep (croatoan:event-key event) 'croatoan:key)
                            (eq :resize (croatoan:key-name
                                         (croatoan:event-key event))))
                       (let ((new-height (croatoan:height scr))
                             (new-width (croatoan:width scr)))
                         (croatoan:resize main-win (1- new-height) new-width)
                         (croatoan:resize modeline-win 1 new-width)
                         (croatoan:move-window modeline-win (1- new-height) 0)
                         (render-buffer (current-buffer) main-win modeline-win)))
                      (t
                       (let ((result (handle-key-event buf event)))
                         (when (eq result :quit)
                           (return-from main-loop)))
                       (let ((cur (current-buffer)))
                         (when (buffer-pending-stream cur)
                           (update-streaming-response cur))
                         (render-buffer cur main-win modeline-win))))))))))
