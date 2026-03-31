(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Keymap
;;; --------------------------------------------------------------------------

(defclass keymap ()
  ((name     :initarg :name
             :reader keymap-name
             :type keyword
             :documentation "Keyword name identifying this keymap (e.g. :default).")
   (bindings :initarg :bindings
             :reader keymap-bindings
             :type hash-table
             :documentation "Hash table mapping key specs to command symbols.")
   (parent   :initarg :parent
             :reader keymap-parent
             :initform nil
             :type (or null keymap)
             :documentation "Parent keymap for fallback lookups, or nil."))
  (:documentation "A key-to-command mapping with optional parent chain for fallback."))

(declaim (ftype (function (keyword &key (:parent (or null keymap))) keymap) make-keymap))
(defun make-keymap (name &key parent)
  "Create a new empty keymap with NAME and optional PARENT for fallback."
  (make-instance 'keymap
    :name name
    :bindings (make-hash-table :test #'equal)
    :parent parent))

(declaim (ftype (function (keymap t symbol) keymap) keymap-bind))
(defun keymap-bind (keymap key command)
  "Bind KEY to COMMAND in KEYMAP."
  (setf (gethash key (keymap-bindings keymap)) command)
  keymap)

(declaim (ftype (function (keymap t) (or null symbol)) keymap-lookup))
(defun keymap-lookup (keymap key)
  "Look up KEY in KEYMAP, walking the parent chain if not found."
  (or (gethash key (keymap-bindings keymap))
      (when (keymap-parent keymap)
        (keymap-lookup (keymap-parent keymap) key))))

;;; --------------------------------------------------------------------------
;;; Default Keymap
;;; --------------------------------------------------------------------------

(defvar *default-keymap* nil
  "The default keymap for chat buffers. Initialized by init-default-keymap.")

(defun init-default-keymap ()
  "Build and install the default keymap with standard chat buffer bindings."
  (let ((km (make-keymap :default)))
    ;; Send message: both #\Return (CR, ASCII 13) and #\Newline (LF, ASCII 10).
    ;; Terminals with nl() mode translate CR→LF, so Enter arrives as #\Newline.
    ;; Bind both to catch either terminal behavior.
    (keymap-bind km #\Return 'send-message)
    (keymap-bind km #\Newline 'send-message)
    ;; Insert newline: C-o (open-line, ASCII 15) since C-j (#\Newline)
    ;; is indistinguishable from Enter in most terminals.
    (keymap-bind km (code-char 15) 'insert-newline-command) ; C-o = ASCII 15
    ;; Cursor movement
    (keymap-bind km #\Soh 'beginning-of-line-command)       ; C-a = ASCII 1
    (keymap-bind km #\Enq 'end-of-line-command)             ; C-e = ASCII 5
    (keymap-bind km (code-char 6) 'forward-char-command)    ; C-f = ASCII 6
    (keymap-bind km (code-char 2) 'backward-char-command)   ; C-b = ASCII 2
    (keymap-bind km '(:alt #\f) 'forward-word-command)      ; M-f
    (keymap-bind km '(:alt #\b) 'backward-word-command)     ; M-b
    ;; Kill/cut
    (keymap-bind km #\Vt 'kill-line-command)                ; C-k = ASCII 11
    (keymap-bind km (code-char 21) 'kill-backward-line-command) ; C-u = ASCII 21
    (keymap-bind km '(:alt #\d) 'kill-word-command)         ; M-d
    (keymap-bind km (code-char 23) 'backward-kill-word-command) ; C-w = ASCII 23
    (keymap-bind km '(:alt #\Backspace) 'backward-kill-word-command)
    (keymap-bind km '(:alt #\Rubout) 'backward-kill-word-command)
    (keymap-bind km '(:alt :backspace) 'backward-kill-word-command)
    (keymap-bind km '(:ctrl #\Backspace) 'backward-kill-word-command)
    (keymap-bind km '(:ctrl #\Rubout) 'backward-kill-word-command)
    (keymap-bind km '(:ctrl :backspace) 'backward-kill-word-command)
    ;; Yank/paste
    (keymap-bind km #\Em 'yank-command)                     ; C-y = ASCII 25
    (keymap-bind km '(:alt #\y) 'yank-pop-command)          ; M-y
    (keymap-bind km '(:alt #\Em) 'yank-previous-command-first-arg-command) ; M-C-y
    (keymap-bind km '(:alt #\.) 'yank-previous-command-last-arg-command)    ; M-.
    (keymap-bind km '(:alt #\_) 'yank-previous-command-last-arg-command)    ; M-_
    ;; Delete
    ;; Note: #\Backspace (ASCII 8 = C-h) is now consumed as the help prefix key.
    ;; Backspace still works via #\Rubout (ASCII 127) and :backspace (ncurses keyword).
    (keymap-bind km (code-char 4) 'delete-char-forward-command) ; C-d = ASCII 4
    (keymap-bind km #\Rubout 'delete-char-backward-command)
    (keymap-bind km :backspace 'delete-char-backward-command)
    (keymap-bind km (code-char 4) 'delete-char-forward-command) ; C-d = ASCII 4
    ;; Home / End — cursor to beginning / end of line
    (keymap-bind km :home 'beginning-of-line-command)
    (keymap-bind km :end 'end-of-line-command)
    ;; Scroll: Page Up / Page Down
    ;; Croatoan delivers special keys as KEY structs; we extract :name
    ;; in handle-key-event, so bind by the keyword name.
    (keymap-bind km :page-up 'scroll-up-command)
    (keymap-bind km :page-down 'scroll-down-command)
    ;; Emacs-style scroll: M-v (scroll up/back), C-v (scroll down/forward)
    ;; M-v arrives as ESC then v, normalized to (:alt #\v) by handle-key-event.
    (keymap-bind km '(:alt #\v) 'scroll-up-command)
    (keymap-bind km (code-char 22) 'scroll-down-command)  ; C-v = ASCII 22
    ;; ----- C-c prefix: buffer-mode-specific commands -----
    ;; C-c is reserved for commands that act on or within the current
    ;; buffer's major mode (e.g. chat-specific toggles, mode actions).
    ;; Note: C-x C-c quits the application (handled in handle-key-event).
    (keymap-bind km '(:ctrl-c #\t) 'toggle-tool-results-command) ; C-c t = toggle tool results
    ;; C-c C-m = minibuffer model selector (helm/ivy/vertico style).
    ;; C-m = ASCII 13 = #\Return.
    ;; Some terminals send #\Newline (LF, ASCII 10) for Enter, so bind both.
    (keymap-bind km '(:ctrl-c #\Return) 'minibuffer-select-model-command)  ; C-c C-m
    (keymap-bind km '(:ctrl-c #\Newline) 'minibuffer-select-model-command) ; C-c C-m (LF variant)
    ;; C-c M (capital M) = old overlay model selector
    (keymap-bind km '(:ctrl-c #\M) 'select-model-command)
    ;; C-c g = spawn read-only McCLIM popup window
    (keymap-bind km '(:ctrl-c #\g) 'popup-gui-command)
    ;; C-c C-d = toggle API debug mode (echo requests/responses in chat)
    ;; C-d = ASCII 4 = #\Eot (end of transmission)
    (keymap-bind km (list :ctrl-c (code-char 4)) 'toggle-debug-mode-command)
    ;; ----- C-h prefix: help & introspection commands -----
    ;; C-h is the help prefix key (Emacs standard).
    (keymap-bind km '(:ctrl-h #\b) 'describe-bindings-command)  ; C-h b = describe keybindings
    (keymap-bind km '(:ctrl-h #\f) 'describe-function-command)  ; C-h f = describe function
    (keymap-bind km '(:ctrl-h #\v) 'describe-variable-command)  ; C-h v = describe variable
    (keymap-bind km '(:ctrl-h #\T) 'describe-type-command)      ; C-h T = describe type
    (keymap-bind km '(:ctrl-h #\F) 'customize-face-command)     ; C-h F = customize face
    ;; ----- C-x prefix: global / cross-buffer commands -----
    ;; C-x is reserved for global commands that operate across buffers
    ;; or affect the application as a whole (buffer management, I/O, etc.).
    (keymap-bind km '(:ctrl-x #\n) 'new-buffer-command)       ; C-x n = new buffer
    (keymap-bind km '(:ctrl-x #\k) 'kill-buffer-command)      ; C-x k = kill buffer
    (keymap-bind km (list :ctrl-x (code-char 19)) 'save-session-command) ; C-x C-s
    ;; C-x b = old overlay buffer selector (table view)
    (keymap-bind km '(:ctrl-x #\b) 'list-buffers-command)
    ;; C-x C-b = minibuffer buffer selector (helm/ivy/vertico style)
    (keymap-bind km (list :ctrl-x (code-char 2)) 'minibuffer-select-buffer-command)
    (setf *default-keymap* km)))
