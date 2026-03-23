(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Buffer
;;; --------------------------------------------------------------------------

(defclass buffer ()
  ((name              :initarg :name
                      :accessor buffer-name
                      :type string
                      :documentation "Display name for this buffer (e.g. \"session-01\").")
   (first-message     :initarg :first-message
                      :accessor buffer-first-message
                      :type message
                      :documentation "First message in the buffer's doubly-linked list.")
   (last-message      :initarg :last-message
                      :accessor buffer-last-message
                      :type message
                      :documentation "Last message in the buffer (always the editable input message).")
   (agent-name        :initarg :agent-name
                      :accessor buffer-agent-name
                      :initform "agent"
                      :type string
                      :documentation "Name of the AI agent for this buffer (e.g. \"claude\").")
   (working-directory :initarg :working-directory
                      :accessor buffer-working-directory
                      :initform (truename ".")
                      :type pathname
                      :documentation "Working directory for shell commands and file operations.")
   (token-count       :initarg :token-count
                      :accessor buffer-token-count
                      :initform 0
                      :type integer
                      :documentation "Running count of tokens used in this buffer's conversation.")
   (context-limit     :initarg :context-limit
                      :accessor buffer-context-limit
                      :initform 200000
                      :type integer
                      :documentation "Maximum token context window size for the model.")
   (status            :initarg :status
                       :accessor buffer-status
                       :initform :idle
                       :type keyword
                       :documentation "Current buffer state: :idle, :thinking, :streaming, :error, :approval, or :oauth.")
   (provider-override :initarg :provider-override
                      :accessor buffer-provider-override
                      :initform nil
                      :type (or null keyword)
                      :documentation "When non-nil, overrides the agent's default provider (e.g. :anthropic).")
   (model-override    :initarg :model-override
                      :accessor buffer-model-override
                      :initform nil
                      :type (or null string)
                      :documentation "When non-nil, overrides the agent's default model name.")
    (face-registry     :initarg :face-registry
                       :accessor buffer-face-registry
                       :type hash-table
                       :documentation "Hash table mapping sender keywords to face-set objects for rendering.")
   (keymap            :initarg :keymap
                      :accessor buffer-keymap
                      :initform nil
                      :documentation "Keymap for this buffer. Falls back to *default-keymap*.")
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
                         :documentation "The message being updated by streaming. Updated in-place as tokens arrive.")
   ;; Permission approval state
   (approval-pending    :initarg :approval-pending
                         :accessor buffer-approval-pending
                         :initform nil
                         :documentation "When non-nil, an alist describing the tool call awaiting approval:
(:tool-name :tool-id :tool-input :display-raw :display-expanded :tool-use-block)")
   (approval-result     :initarg :approval-result
                         :accessor buffer-approval-result
                         :initform nil
                         :documentation "Set by the approval UI: :approve, :deny, or (:deny-with-message . \"reason\")")
   (stashed-input       :initarg :stashed-input
                         :accessor buffer-stashed-input
                         :initform nil
                         :type (or null string)
                         :documentation "User's input text stashed during approval prompt.")
   (pending-tool-calls  :initarg :pending-tool-calls
                         :accessor buffer-pending-tool-calls
                         :initform nil
                         :documentation "List of tool_use blocks awaiting sequential approval.")
   (tool-call-results   :initarg :tool-call-results
                         :accessor buffer-tool-call-results
                         :initform nil
                         :documentation "Accumulated results from approved/denied tool calls.")
   (major-mode          :initarg :major-mode
                         :accessor buffer-major-mode
                         :initform "chat"
                         :type string
                         :documentation "The major mode name for modeline display (e.g. \"chat\", \"help\")."))
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

(declaim (ftype (function (buffer keyword) buffer) set-buffer-provider-override))
(defun set-buffer-provider-override (buf provider)
  "Set BUF's provider override to PROVIDER and return BUF."
  (setf (buffer-provider-override buf) provider)
  buf)

(declaim (ftype (function (buffer string) buffer) set-buffer-model-override))
(defun set-buffer-model-override (buf model)
  "Set BUF's model override to MODEL and return BUF."
  (setf (buffer-model-override buf) model)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-provider-override))
(defun clear-buffer-provider-override (buf)
  "Clear BUF's provider override and return BUF."
  (setf (buffer-provider-override buf) nil)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-model-override))
(defun clear-buffer-model-override (buf)
  "Clear BUF's model override and return BUF."
  (setf (buffer-model-override buf) nil)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-provider/model-overrides))
(defun clear-buffer-provider/model-overrides (buf)
  "Clear BUF's provider and model overrides and return BUF."
  (clear-buffer-provider-override buf)
  (clear-buffer-model-override buf)
  buf)

;;; --------------------------------------------------------------------------
;;; Buffer Ring
;;; --------------------------------------------------------------------------

(defvar *buffer-ring* nil
  "List of all buffers. The first element is the current buffer.")

(defvar *buffer-counter* 0
  "Counter for generating unique buffer names.")

(defun current-buffer ()
  "Return the current buffer (first in the ring)."
  (first *buffer-ring*))

(defun add-buffer-to-ring (buf)
  "Add BUF to the front of the buffer ring."
  (push buf *buffer-ring*)
  buf)

(defun switch-to-buffer (buf)
  "Make BUF the current buffer by moving it to the front of the ring."
  (setf *buffer-ring* (cons buf (remove buf *buffer-ring*)))
  buf)

(defun kill-buffer-from-ring (buf)
  "Remove BUF from the buffer ring. Returns the new current buffer or nil."
  (setf *buffer-ring* (remove buf *buffer-ring*))
  (first *buffer-ring*))

(defun find-buffer-by-name (name)
  "Find a buffer in the ring by name. Returns the buffer or nil."
  (find name *buffer-ring* :key #'buffer-name :test #'string=))

(defun next-buffer-name ()
  "Generate the next unique buffer name."
  (incf *buffer-counter*)
  (format nil "session-~A" *buffer-counter*))

(defun buffer-names ()
  "Return a list of all buffer names in the ring."
  (mapcar #'buffer-name *buffer-ring*))

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

(defun whitespace-char-p (char)
  "Return T when CHAR is treated as command-word whitespace."
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Newline)
      (char= char #\Return)))

(defun split-command-words (text)
  "Split TEXT into whitespace-delimited words."
  (let ((words nil)
        (len (length text))
        (start 0))
    (loop
      (let ((word-start (position-if-not #'whitespace-char-p text :start start)))
        (unless word-start
          (return (nreverse words)))
        (let ((word-end (or (position-if #'whitespace-char-p text :start word-start)
                            len)))
          (push (subseq text word-start word-end) words)
          (setf start word-end))))))

(defun buffer-previous-user-command-text (buf)
  "Return the latest finalized user message text in BUF, or nil."
  (loop :for msg := (message-prev (buffer-input-message buf)) :then (message-prev msg)
        :while msg
        :for text := (message-text msg)
        :when (and (message-read-only-p msg)
                   (eq (message-sender msg) :user)
                   (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
          :return text
        :finally (return nil)))

(defun buffer-previous-command-first-argument (buf)
  "Return previous command's first argument (second word), or nil."
  (let* ((command (buffer-previous-user-command-text buf))
         (words (and command (split-command-words command))))
    (second words)))

(defun buffer-previous-command-last-argument (buf)
  "Return previous command's last argument, or nil when unavailable."
  (let* ((command (buffer-previous-user-command-text buf))
         (words (and command (split-command-words command))))
    (car (last words))))

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

(declaim (ftype (function (buffer string) message) buffer-insert-system-message))
(defun buffer-insert-system-message (buf text)
  "Create a read-only system message with TEXT and insert it before the input message.
System messages are display-only — they are excluded from API conversation history."
  (let* ((sys-msg (make-message :system :read-only-p t))
         (input (buffer-input-message buf)))
    (set-message-text sys-msg text)
    (setf (message-timestamp sys-msg) (get-universal-time))
    (let ((before-input (message-prev input)))
      (setf (message-prev sys-msg) before-input
            (message-next sys-msg) input
            (message-prev input) sys-msg)
      (if before-input
          (setf (message-next before-input) sys-msg)
          (setf (buffer-first-message buf) sys-msg)))
    sys-msg))

;;; --------------------------------------------------------------------------
;;; Session Persistence
;;; --------------------------------------------------------------------------

(defvar *sessions-dir*
  (merge-pathnames #P".config/clawmacs/sessions/" (user-homedir-pathname))
  "Directory for saved session files.")

(defun session-path (session-name)
  "Return the file path for a session by name."
  (merge-pathnames (format nil "~A.json" session-name) *sessions-dir*))

(defun serialize-message (msg)
  "Serialize a message to an alist for JSON encoding."
  `((:sender . ,(symbol-name (message-sender msg)))
    (:text . ,(message-text msg))
    (:timestamp . ,(message-timestamp msg))
    (:read-only-p . ,(message-read-only-p msg))
    ,@(when (message-raw-content msg)
        `((:raw-content . ,(coerce (message-raw-content msg) 'vector))))))

(defun serialize-buffer (buf)
  "Serialize a buffer's conversation to JSON-ready alist."
  (let ((messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :when (message-read-only-p msg)
          :do (push (serialize-message msg) messages))
    `((:name . ,(buffer-name buf))
      (:agent-name . ,(buffer-agent-name buf))
      (:provider-override . ,(buffer-provider-override buf))
      (:model-override . ,(buffer-model-override buf))
      (:messages . ,(coerce (nreverse messages) 'vector)))))

(defun save-session (buf)
  "Save the buffer's conversation to a session file."
  (let ((path (session-path (buffer-name buf))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (cl-json:encode-json-to-string (serialize-buffer buf)) s))
    path))

(defun load-session (session-name &key (agent-name "claude"))
  "Load a saved session into a new buffer. Returns the buffer or nil."
  (let ((path (session-path session-name)))
    (unless (probe-file path)
      (return-from load-session nil))
    (let ((cl-json:*json-array-type* 'vector))
      (let* ((json-str (uiop:read-file-string path))
             (data (cl-json:decode-json-from-string json-str))
             (name (or (cdr (assoc :name data)) session-name))
             (agent (or (cdr (assoc :agent-name data)) agent-name))
             (provider-override (cdr (assoc :provider-override data)))
             (model-override (cdr (assoc :model-override data)))
             (messages (cdr (assoc :messages data)))
             (buf (make-buffer name :agent-name agent
                                     :working-directory (truename "."))))
        (setf (buffer-provider-override buf)
              (and provider-override
                   (intern (string-upcase provider-override) :keyword))
              (buffer-model-override buf)
              model-override)
        ;; Replay messages into the buffer
        (loop :for msg-data :across messages
              :for sender-str := (cdr (assoc :sender msg-data))
              :for text := (cdr (assoc :text msg-data))
              :for raw-content := (cdr (assoc :raw-content msg-data))
              :for sender-kw := (intern sender-str :keyword)
              :do (let ((msg (make-message sender-kw :read-only-p t)))
                    (set-message-text msg (or text ""))
                     (setf (message-timestamp msg)
                           (cdr (assoc :timestamp msg-data)))
                     (when raw-content
                       (setf (message-raw-content msg)
                            (normalize-legacy-raw-content raw-content)))
                     ;; Insert before input
                     (let ((input (buffer-input-message buf))
                           (before (message-prev (buffer-input-message buf))))
                      (setf (message-prev msg) before
                            (message-next msg) input
                            (message-prev input) msg)
                      (if before
                          (setf (message-next before) msg)
                          (setf (buffer-first-message buf) msg)))))
        buf))))

(defun list-saved-sessions ()
  "Return a list of saved session names."
  (when (probe-file *sessions-dir*)
    (let ((files (directory (merge-pathnames "*.json" *sessions-dir*))))
      (mapcar (lambda (f) (pathname-name f)) files))))
