(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Buffer
;;; --------------------------------------------------------------------------

(defclass buffer ()
  ((name              :initarg :name
                      :accessor buffer-name
                      :type string)
   (first-message     :initarg :first-message
                      :accessor buffer-first-message
                      :type message)
   (last-message      :initarg :last-message
                      :accessor buffer-last-message
                      :type message)
   (agent-name        :initarg :agent-name
                      :accessor buffer-agent-name
                      :initform "agent"
                      :type string)
   (working-directory :initarg :working-directory
                      :accessor buffer-working-directory
                      :initform (truename ".")
                      :type pathname)
   (token-count       :initarg :token-count
                      :accessor buffer-token-count
                      :initform 0
                      :type integer)
   (context-limit     :initarg :context-limit
                      :accessor buffer-context-limit
                      :initform 200000
                      :type integer)
   (status            :initarg :status
                      :accessor buffer-status
                      :initform :idle
                      :type keyword)
   (face-registry     :initarg :face-registry
                      :accessor buffer-face-registry
                      :type hash-table)
   (keymap            :initarg :keymap
                      :accessor buffer-keymap
                      :initform nil)
   (scroll-offset    :initarg :scroll-offset
                      :accessor buffer-scroll-offset
                      :initform 0
                      :type integer
                      :documentation "Number of visual rows scrolled up from the bottom.
0 means auto-scroll (latest messages visible).")
   (show-tool-results-p :initarg :show-tool-results-p
                         :accessor buffer-show-tool-results-p
                         :initform t
                         :type boolean
                         :documentation "When nil, tool-result messages are hidden from display.")
   (pending-stream      :initarg :pending-stream
                         :accessor buffer-pending-stream
                         :initform nil
                         :documentation "When non-nil, holds a stream-state for an in-progress streaming response.")
   (streaming-message   :initarg :streaming-message
                         :accessor buffer-streaming-message
                         :initform nil
                         :type (or null message)
                         :documentation "The message being updated by streaming. Updated in-place as tokens arrive."))
  (:documentation
   "A chat buffer containing a doubly-linked list of messages.
The last message is always the input message (read-only-p = nil).
Invariant: last-message and input-message always refer to the same object."))

(defun buffer-input-message (buf)
  "Return the input message (alias for last-message).
Enforces the invariant that it is not read-only."
  (let ((msg (buffer-last-message buf)))
    (assert (not (message-read-only-p msg)) ()
            "Invariant violated: input message is read-only")
    msg))

(declaim (ftype (function (string &key (:agent-name string)
                                       (:working-directory pathname)
                                       (:context-limit integer))
                          buffer)
                make-buffer))
(defun make-buffer (name &key (agent-name "agent")
                              (working-directory (truename "."))
                              (context-limit 200000))
  "Create a new buffer with a single empty input message."
  (let* ((input-msg (make-message :user))
         (registry (make-hash-table :test #'eq))
         (buf (make-instance 'buffer
                :name name
                :first-message input-msg
                :last-message input-msg
                :agent-name agent-name
                :working-directory working-directory
                :context-limit context-limit
                :face-registry registry)))
    buf))

(declaim (ftype (function (buffer) fixnum) buffer-message-count))
(defun buffer-message-count (buf)
  "Count the number of messages in BUF."
  (loop :for current := (buffer-first-message buf) :then (message-next current)
        :while current
        :count t))

;;; --------------------------------------------------------------------------
;;; Buffer Operations
;;; --------------------------------------------------------------------------

(declaim (ftype (function (buffer) buffer) buffer-finalize-input))
(defun buffer-finalize-input (buf)
  "Finalize the current input message: make it read-only, timestamp it,
and create a new empty input message at the tail."
  (let ((input (buffer-input-message buf)))
    (setf (message-read-only-p input) t
          (message-timestamp input) (get-universal-time)
          (message-sender input) :user)
    (let ((new-input (make-message :user)))
      (setf (message-prev new-input) input
            (message-next input) new-input
            (buffer-last-message buf) new-input)))
  buf)

(defun set-message-text (msg text)
  "Replace MSG's lines with lines split from TEXT on newlines."
  (let* ((parts (loop :for start := 0 :then (1+ pos)
                      :for pos := (position #\Newline text :start start)
                      :collect (subseq text start (or pos (length text)))
                      :while pos))
         (lines (mapcar #'make-line (or parts (list "")))))
    ;; Link lines into a DLL
    (loop :for (a b) :on lines
          :when b
          :do (setf (line-next a) b
                    (line-prev b) a))
    (setf (message-first-line msg) (first lines)
          (message-last-line msg) (car (last lines))
          (message-point-line msg) (first lines)
          (message-point-offset msg) 0))
  msg)

(declaim (ftype (function (buffer string) message) buffer-insert-agent-message))
(defun buffer-insert-agent-message (buf text)
  "Create a read-only agent message with TEXT and insert it before the input message.
TEXT may contain newlines, which are split into separate line objects."
  (let* ((agent-keyword (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (agent-msg (make-message agent-keyword :read-only-p t))
         (input (buffer-input-message buf)))
    (set-message-text agent-msg text)
    (setf (message-timestamp agent-msg) (get-universal-time))
    (let ((before-input (message-prev input)))
      (setf (message-prev agent-msg) before-input
            (message-next agent-msg) input
            (message-prev input) agent-msg)
      (if before-input
          (setf (message-next before-input) agent-msg)
          (setf (buffer-first-message buf) agent-msg)))
    agent-msg))
