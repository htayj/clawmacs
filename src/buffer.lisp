(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Buffer
;;; --------------------------------------------------------------------------

(defvar *default-context-limit* 200000
  "Default token context window size for new buffers.")

(defvar *default-agent-name* "agent"
  "Default agent name for new buffers. Affects modeline display and agent-defaults resolution.")

(defvar *default-show-tool-results* t
  "When nil, new buffers hide tool-result messages by default.")

(defvar *scratch-buffer-name* "*scratch*"
  "Name used for the process-local scratch buffer.")

(defvar *scratch-buffer-initial-text* ""
  "Initial text inserted into the scratch buffer when it is created.")

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
                      :documentation "Name of the AI agent for this buffer (e.g. \"agent\").")
   (kind              :initarg :kind
                      :accessor buffer-kind
                      :initform :chat
                      :type keyword
                      :documentation "Buffer kind. Built-ins include :chat and :scratch.")
   (working-directory :initarg :working-directory
                      :accessor buffer-working-directory
                      :initform (truename ".")
                      :type pathname
                      :documentation "Working directory for shell commands and file operations.")
   (project-name      :initarg :project-name
                      :accessor buffer-project-name
                      :initform nil
                      :type (or null string)
                      :documentation "Project name associated with this buffer, when any.")
   (resource-path     :initarg :resource-path
                      :accessor buffer-resource-path
                      :initform nil
                      :type (or null string)
                      :documentation "Project-relative resource path associated with this buffer, when any.")
   (original-text     :initarg :original-text
                      :accessor buffer-original-text
                      :initform ""
                      :type string
                      :documentation "Last saved text for project-backed editable buffers.")
   (dirty-p           :initarg :dirty-p
                      :accessor buffer-dirty-p
                      :initform nil
                      :type boolean
                      :documentation "True when a project-backed editable buffer has unsaved changes.")
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
                      :documentation "When non-nil, overrides the agent's default provider (e.g. :zai).")
   (model-override    :initarg :model-override
                      :accessor buffer-model-override
                      :initform nil
                      :type (or null string)
                      :documentation "When non-nil, overrides the agent's default model name.")
   (think-level-override :initarg :think-level-override
                         :accessor buffer-think-level-override
                         :initform nil
                         :type (or null string)
                         :documentation "When non-nil, overrides the model's default reasoning effort.")
   (enabled-packages :initarg :enabled-packages
                     :accessor buffer-enabled-packages
                     :initform nil
                     :type list
                     :documentation "Package names explicitly enabled for this buffer.")
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
                         :initform *default-show-tool-results*
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
                                       (:kind keyword)
                                       (:working-directory pathname)
                                       (:project-name (or null string))
                                       (:resource-path (or null string))
                                       (:original-text string)
                                       (:dirty-p boolean)
                                       (:context-limit integer)
                                       (:enabled-packages list))
                          buffer)
                make-buffer))
(defun make-buffer (name &key (agent-name *default-agent-name*)
                              (kind :chat)
                              (working-directory (truename "."))
                              project-name
                              resource-path
                              (original-text "")
                              (dirty-p nil)
                              (context-limit *default-context-limit*)
                              (enabled-packages nil))
  "Create a new buffer with a single empty input message."
  (let* ((input-msg (make-message :user))
         (registry (make-hash-table :test #'eq))
         (buf (make-instance 'buffer
                :name name
                :first-message input-msg
                :last-message input-msg
                :agent-name agent-name
                :kind kind
                :working-directory working-directory
                :project-name project-name
                :resource-path resource-path
                :original-text original-text
                :dirty-p dirty-p
                :context-limit context-limit
                :enabled-packages (copy-list enabled-packages)
                :face-registry registry)))
    buf))

(declaim (ftype (function (buffer) boolean) scratch-buffer-p))
(defun scratch-buffer-p (buf)
  "Return true when BUF is the process-local scratch buffer."
  (eq (buffer-kind buf) :scratch))

(declaim (ftype (function (buffer) boolean) file-buffer-p))
(defun file-buffer-p (buf)
  "Return true when BUF is a project-backed editable file buffer."
  (eq (buffer-kind buf) :file))

(declaim (ftype (function (buffer) boolean) document-buffer-p))
(defun document-buffer-p (buf)
  "Return true when BUF is an editable document buffer rather than a chat buffer."
  (or (scratch-buffer-p buf)
      (file-buffer-p buf)))

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

(defun normalize-think-level-override (value)
  "Normalize VALUE for storage as a think-level override."
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (string value))))
      (when (plusp (length trimmed))
        (string-downcase trimmed)))))

(declaim (ftype (function (buffer string) buffer) set-buffer-think-level-override))
(defun set-buffer-think-level-override (buf think-level)
  "Set BUF's think-level override to THINK-LEVEL and return BUF."
  (setf (buffer-think-level-override buf)
        (normalize-think-level-override think-level))
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

(declaim (ftype (function (buffer) buffer) clear-buffer-think-level-override))
(defun clear-buffer-think-level-override (buf)
  "Clear BUF's think-level override and return BUF."
  (setf (buffer-think-level-override buf) nil)
  buf)

(declaim (ftype (function (buffer) buffer) clear-buffer-routing-overrides))
(defun clear-buffer-routing-overrides (buf)
  "Clear BUF's provider, model, and think-level overrides and return BUF."
  (clear-buffer-provider-override buf)
  (clear-buffer-model-override buf)
  (clear-buffer-think-level-override buf)
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
  (unless (and buf (scratch-buffer-p buf))
    (setf *buffer-ring* (remove buf *buffer-ring*)))
  (first *buffer-ring*))

(defun find-buffer-by-name (name)
  "Find a buffer in the ring by name. Returns the buffer or nil."
  (find name *buffer-ring* :key #'buffer-name :test #'string=))

(defun scratch-buffer ()
  "Return the loaded scratch buffer, or nil when it has not been created yet."
  (find-if #'scratch-buffer-p *buffer-ring*))

(defun next-buffer-name ()
  "Generate the next unique buffer name."
  (incf *buffer-counter*)
  (format nil "session-~A" *buffer-counter*))

(defun buffer-names ()
  "Return a list of all buffer names in the ring."
  (mapcar #'buffer-name *buffer-ring*))

(defun scratch-buffer-text (&optional (buf (scratch-buffer)))
  "Return BUF's editable scratch text, or nil when no scratch buffer is loaded."
  (when buf
    (unless (scratch-buffer-p buf)
      (error "Not a scratch buffer: ~A" (buffer-name buf)))
    (message-text (buffer-input-message buf))))

(defun (setf scratch-buffer-text) (text &optional (buf (scratch-buffer)))
  "Replace BUF's editable scratch text with TEXT."
  (unless buf
    (error "No scratch buffer is loaded"))
  (unless (scratch-buffer-p buf)
    (error "Not a scratch buffer: ~A" (buffer-name buf)))
  (set-message-text (buffer-input-message buf) text))

(defun file-buffer-text (&optional (buf (current-buffer)))
  "Return BUF's editable file text, or nil when BUF is nil."
  (when buf
    (unless (file-buffer-p buf)
      (error "Not a file buffer: ~A" (buffer-name buf)))
    (message-text (buffer-input-message buf))))

(defun (setf file-buffer-text) (text &optional (buf (current-buffer)))
  "Replace BUF's editable file text and update its dirty state."
  (unless buf
    (error "No file buffer is current"))
  (unless (file-buffer-p buf)
    (error "Not a file buffer: ~A" (buffer-name buf)))
  (set-message-text (buffer-input-message buf) text)
  (setf (buffer-dirty-p buf)
        (not (string= text (buffer-original-text buf))))
  text)

(declaim (ftype (function (&optional (or null buffer)) boolean)
                file-buffer-dirty-p))
(defun file-buffer-dirty-p (&optional (buf (current-buffer)))
  "Return true when BUF is a project-backed file buffer with unsaved changes."
  (when buf
    (unless (file-buffer-p buf)
      (error "Not a file buffer: ~A" (buffer-name buf)))
    (buffer-dirty-p buf)))

(defun mark-buffer-dirty (buf)
  "Mark BUF dirty when it is a project-backed file buffer."
  (when (file-buffer-p buf)
    (setf (buffer-dirty-p buf) t))
  buf)

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

(declaim (ftype (function (buffer string) message) buffer-insert-context-message))
(defun buffer-insert-context-message (buf text)
  "Create a read-only context message with TEXT before the input message.
Context messages are sent to providers as user-context messages."
  (let* ((context-msg (make-message :context :read-only-p t))
         (input (buffer-input-message buf)))
    (set-message-text context-msg text)
    (setf (message-timestamp context-msg) (get-universal-time))
    (let ((before-input (message-prev input)))
      (setf (message-prev context-msg) before-input
            (message-next context-msg) input
            (message-prev input) context-msg)
      (if before-input
          (setf (message-next before-input) context-msg)
          (setf (buffer-first-message buf) context-msg)))
    context-msg))

(declaim (ftype (function (buffer string) message) buffer-insert-system-message))
(defun buffer-insert-system-message (buf text)
  "Create a read-only system message with TEXT and insert it before the input message.
System messages are display-only — they are excluded from API conversation history.
Assigns the :system face set from the buffer's face registry if available."
  (let* ((sys-msg (make-message :system :read-only-p t))
         (input (buffer-input-message buf)))
    (set-message-text sys-msg text)
    (setf (message-timestamp sys-msg) (get-universal-time))
    ;; Assign system face set if registered
    (let ((sys-fs (gethash :system (buffer-face-registry buf))))
      (when sys-fs
        (setf (message-face-set sys-msg) sys-fs)))
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
      (:think-level-override . ,(buffer-think-level-override buf))
      (:enabled-packages . ,(coerce (copy-list (buffer-enabled-packages buf))
                                    'vector))
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

(defun load-session (session-name &key (agent-name *default-agent-name*))
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
             (think-level-override (cdr (assoc :think-level-override data)))
             (enabled-packages (cdr (assoc :enabled-packages data)))
             (messages (cdr (assoc :messages data)))
             (buf (make-buffer name :agent-name agent
                                     :working-directory (truename "."))))
        (setf (buffer-provider-override buf)
              (and provider-override
                   (ignore-errors
                     (normalize-provider provider-override)))
              (buffer-model-override buf)
              model-override
              (buffer-think-level-override buf)
              (normalize-think-level-override think-level-override)
              (buffer-enabled-packages buf)
              (loop :for package :in (coerce (or enabled-packages #()) 'list)
                    :when (stringp package)
                      :collect package))
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
        (ignore-errors
          (reconcile-buffer-think-level-override buf))
        buf))))

(defun list-saved-sessions ()
  "Return a list of saved session names."
  (when (probe-file *sessions-dir*)
    (let ((files (directory (merge-pathnames "*.json" *sessions-dir*))))
      (mapcar (lambda (f) (pathname-name f)) files))))
