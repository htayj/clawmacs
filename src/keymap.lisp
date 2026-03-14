(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Keymap
;;; --------------------------------------------------------------------------

(defclass keymap ()
  ((name     :initarg :name
             :reader keymap-name
             :type keyword)
   (bindings :initarg :bindings
             :reader keymap-bindings
             :type hash-table)
   (parent   :initarg :parent
             :reader keymap-parent
             :initform nil
             :type (or null keymap)))
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
    (keymap-bind km #\Soh 'beginning-of-line-command)       ; C-a = ASCII 1
    (keymap-bind km #\Enq 'end-of-line-command)             ; C-e = ASCII 5
    (keymap-bind km #\Vt 'kill-line-command)                ; C-k = ASCII 11
    (keymap-bind km #\Em 'yank-command)                     ; C-y = ASCII 25
    (keymap-bind km #\Backspace 'delete-char-backward-command)
    (keymap-bind km #\Rubout 'delete-char-backward-command)
    (setf *default-keymap* km)))
