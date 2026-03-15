(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Self-insert support (must be defined before commands that reference it)
;;; --------------------------------------------------------------------------

(defvar *self-insert-char* nil
  "The character to insert for self-insert-command. Bound by the event loop.")

;;; --------------------------------------------------------------------------
;;; Stub Agent
;;; --------------------------------------------------------------------------

(declaim (ftype (function (buffer) buffer) send-to-agent-with-context))
(defun send-to-agent-with-context (buf)
  "Stub agent: echoes the last user message back as an agent message.
Assigns the agent's face-set from the buffer's face registry."
  (let* ((input (buffer-input-message buf))
         (user-msg (message-prev input))
         (user-text (if user-msg (message-text user-msg) ""))
         (agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (agent-msg (buffer-insert-agent-message buf (format nil "Echo: ~A" user-text))))
    (setf (message-face-set agent-msg)
          (gethash agent-kw (buffer-face-registry buf))))
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

(defun normalize-key (event)
  "Extract and normalize a key from a croatoan EVENT.
Returns a character, a keyword (for special keys), or a list (:alt <key>)
for Meta combinations."
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
      ;; Meta prefix is active: combine with this key
      (*meta-pending*
       (setf *meta-pending* nil)
       (list :alt key))
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
