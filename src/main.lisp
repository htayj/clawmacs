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
         (display (with-output-to-string (s)
                    (when (plusp (length text-parts))
                      (write-string text-parts s))
                    (dolist (tu tool-uses)
                      (when (plusp (length text-parts))
                        (write-char #\Newline s))
                      (write-string (format-tool-call-display tu) s))))
         (agent-msg (buffer-insert-agent-message buf display)))
    (setf (message-raw-content agent-msg) content-blocks)
    (setf (message-face-set agent-msg)
          (gethash agent-kw (buffer-face-registry buf)))
    agent-msg))

(defun execute-tool-calls (buf tool-use-blocks agent-kw)
  "Execute tool calls and insert a single tool-result message into the buffer.
The message displays tool results for the user and carries raw-content
for the API (tool_result blocks). No separate display messages are created
to avoid breaking the assistant→tool_result message sequence."
  (let ((result-blocks nil)
        (display-parts nil)
        (*current-caller* agent-kw))
    ;; Execute each tool and collect results
    (dolist (tu tool-use-blocks)
      (let* ((tool-id (cdr (assoc :id tu)))
             (tool-name (cdr (assoc :name tu)))
             (tool-input (cdr (assoc :input tu)))
             (result-text
               (handler-case
                   (execute-tool tool-name tool-input)
                 (error (e)
                   (api-json-encode `((:error . ,(format nil "~A" e))))))))
        (push (format-tool-result-display tool-name result-text) display-parts)
        (push `((:type . "tool_result")
                (:tool--use--id . ,tool-id)
                (:content . ,result-text))
              result-blocks)))
    ;; Insert ONE message: displays results + carries raw-content for API
    (let* ((display-text (format nil "~{~A~^~%~}" (nreverse display-parts)))
           (raw (nreverse result-blocks))
           (tr-msg (make-message :tool-result :read-only-p t))
           (input (buffer-input-message buf)))
      (set-message-text tr-msg display-text)
      (setf (message-raw-content tr-msg) raw)
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
      raw)))

(declaim (ftype (function (buffer) buffer) send-to-agent-with-context))
(defun send-to-agent-with-context (buf)
  "Send the conversation to the LLM with tool support.
Loops: call API, execute tool calls, send results, repeat until end_turn."
  (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (tools (let ((*current-caller* agent-kw))
                  (tool-definitions-for-api)))
         (max-iterations 10))
    (handler-case
        (progn
          (setf (buffer-status buf) :thinking)
          (loop :for iteration :from 0 :below max-iterations
                :do (let* ((messages (build-conversation-messages buf))
                           (response (anthropic-request messages :tools tools))
                           (content (response-content response))
                           (stop-reason (response-stop-reason response))
                           (tool-uses (content-tool-use-blocks content)))
                      ;; Insert assistant message (text + tool call display)
                      (insert-agent-message-from-content buf content agent-kw)
                      (if (and (string= "tool_use" (or stop-reason ""))
                               tool-uses)
                          ;; Execute tools and loop
                          (execute-tool-calls buf tool-uses agent-kw)
                          ;; Done
                          (progn
                            (setf (buffer-status buf) :idle)
                            (return)))))
          (setf (buffer-status buf) :idle))
      (error (e)
        (setf (buffer-status buf) :error)
        (let ((err-msg (buffer-insert-agent-message
                        buf (format nil "[Error: ~A]" e))))
          (setf (message-face-set err-msg)
                (gethash agent-kw (buffer-face-registry buf)))))))
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

(defun handle-key-event (buf event)
  "Dispatch a key event through the buffer's keymap.
Returns :QUIT if the application should exit, or nil otherwise.
Handles ESC prefix for Meta keys: ESC followed by a key becomes (:alt <key>)."
  (let ((key (normalize-key event))
        (*current-caller* :user))
    ;; nil key means ESC was consumed, waiting for next key
    (when (null key)
      (return-from handle-key-event nil))
    (cond
      ;; C-c to quit
      ((and (characterp key) (char= key #\Etx))
       :quit)
      ;; Keymap lookup (works for characters, keywords, and (:alt ...) lists)
      ((keymap-lookup (buffer-keymap buf) key)
       (let ((command (keymap-lookup (buffer-keymap buf) key)))
         (funcall command buf)
         ;; Reset scroll to bottom when user types (not when scrolling)
         (when (and (characterp key)
                    (not (member command '(scroll-up-command scroll-down-command))))
           (setf (buffer-scroll-offset buf) 0))
         nil))
      ;; Self-insert for printable characters
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
      ;; Set scroll page size based on available history area
      (setf *scroll-page-size* (max 1 (- (1- screen-height) 3)))
      ;; Flush stdscr's pending clear before our first render so that
      ;; event-case's auto-refresh of scr doesn't wipe our windows.
      (croatoan:refresh scr)
      ;; Initial render
      (render-buffer buf main-win modeline-win)
      ;; Read events from scr (stdscr). event-case auto-refreshes the
      ;; event source before each getch; since scr was pre-flushed and
      ;; never written to, the auto-refresh is a no-op.
      (croatoan:event-case (scr event)
        (:resize
         (let ((new-height (croatoan:height scr))
               (new-width (croatoan:width scr)))
           (croatoan:resize main-win (1- new-height) new-width)
           (croatoan:resize modeline-win 1 new-width)
           (croatoan:move-window modeline-win (1- new-height) 0)
           (render-buffer buf main-win modeline-win)))
        (otherwise
         (let ((result (handle-key-event buf event)))
           (when (eq result :quit)
             (return-from croatoan:event-case))
           (render-buffer buf main-win modeline-win)))))))
