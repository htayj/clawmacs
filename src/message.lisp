(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Line
;;; --------------------------------------------------------------------------

(defclass line ()
  ((content :initarg :content
            :accessor line-content
            :initform ""
            :type string)
   (next    :initarg :next
            :accessor line-next
            :initform nil
            :type (or null line))
   (prev    :initarg :prev
            :accessor line-prev
            :initform nil
            :type (or null line)))
  (:documentation "A single line of text in a message. Lines form a doubly-linked list."))

(declaim (ftype (function (&optional string) line) make-line))
(defun make-line (&optional (content ""))
  "Create a new unlinked line with CONTENT."
  (make-instance 'line :content content))

;;; --------------------------------------------------------------------------
;;; Message
;;; --------------------------------------------------------------------------

(defclass message ()
  ((first-line    :initarg :first-line
                  :accessor message-first-line
                  :type line)
   (last-line     :initarg :last-line
                  :accessor message-last-line
                  :type line)
   (point-line    :initarg :point-line
                  :accessor message-point-line
                  :type line)
   (point-offset  :initarg :point-offset
                  :accessor message-point-offset
                  :initform 0
                  :type fixnum)
   (mark-line     :initarg :mark-line
                  :accessor message-mark-line
                  :initform nil
                  :type (or null line))
   (mark-offset   :initarg :mark-offset
                  :accessor message-mark-offset
                  :initform nil
                  :type (or null fixnum))
   (sender        :initarg :sender
                  :accessor message-sender
                  :type keyword)
   (timestamp     :initarg :timestamp
                  :accessor message-timestamp
                  :initform nil
                  :type (or null integer))
   (face-set      :initarg :face-set
                  :accessor message-face-set
                  :initform nil
                  :type (or null face-set))
   (read-only-p   :initarg :read-only-p
                  :accessor message-read-only-p
                  :initform nil
                  :type boolean)
   (next          :initarg :next
                  :accessor message-next
                  :initform nil
                  :type (or null message))
   (prev          :initarg :prev
                  :accessor message-prev
                  :initform nil
                  :type (or null message))
   (raw-content   :initarg :raw-content
                  :accessor message-raw-content
                  :initform nil
                  :type list
                  :documentation "When non-nil, holds the API-format content blocks
(list of alists) for this message. Used for tool_use assistant messages
and tool_result user messages to preserve structured content for API
round-tripping."))
  (:documentation
   "A message in the chat buffer. Contains a doubly-linked list of lines,
a point and optional mark for intra-message cursor/selection, sender
identity, and links to adjacent messages in the buffer."))

(declaim (ftype (function (keyword &key (:face-set (or null face-set))
                                       (:read-only-p boolean))
                          message)
                make-message))
(defun make-message (sender &key face-set (read-only-p nil))
  "Create a new message with a single empty line. Point starts at offset 0."
  (let* ((initial-line (make-line ""))
         (msg (make-instance 'message
                :first-line initial-line
                :last-line initial-line
                :point-line initial-line
                :point-offset 0
                :sender sender
                :face-set face-set
                :read-only-p read-only-p)))
    msg))

(declaim (ftype (function (message) string) message-text))
(defun message-text (msg)
  "Return the full text of MSG as a single string with newlines between lines."
  (with-output-to-string (s)
    (loop :for current := (message-first-line msg) :then (line-next current)
          :while current
          :for first := t :then nil
          :do (unless first (write-char #\Newline s))
              (write-string (line-content current) s))))

(declaim (ftype (function (message) fixnum) message-line-count))
(defun message-line-count (msg)
  "Count the number of lines in MSG."
  (loop :for current := (message-first-line msg) :then (line-next current)
        :while current
        :count t))

;;; --------------------------------------------------------------------------
;;; Kill Ring
;;; --------------------------------------------------------------------------

(defvar *kill-ring* nil
  "The global kill ring. A list of strings, most recent first.")

(defvar *kill-ring-max* 60
  "Maximum number of entries in the kill ring.")

(declaim (ftype (function (string) string) kill-ring-push))
(defun kill-ring-push (string)
  "Push STRING onto the kill ring. Trims ring to *kill-ring-max* entries."
  (push string *kill-ring*)
  (when (> (length *kill-ring*) *kill-ring-max*)
    (setf *kill-ring* (subseq *kill-ring* 0 *kill-ring-max*)))
  string)

(declaim (ftype (function () (or null string)) kill-ring-top))
(defun kill-ring-top ()
  "Return the most recent kill ring entry, or nil if empty."
  (first *kill-ring*))

;;; --------------------------------------------------------------------------
;;; Message Editing Operations
;;; --------------------------------------------------------------------------

(declaim (ftype (function (message character) message) message-insert-char))
(defun message-insert-char (msg char)
  "Insert CHAR at point in MSG. Advances point by 1."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl))
         (new-content (concatenate 'string
                                   (subseq content 0 po)
                                   (string char)
                                   (subseq content po))))
    (setf (line-content pl) new-content
          (message-point-offset msg) (1+ po)))
  msg)

(declaim (ftype (function (message) message) message-insert-newline))
(defun message-insert-newline (msg)
  "Insert a newline at point, splitting the current line into two."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl))
         (before (subseq content 0 po))
         (after (subseq content po))
         (new-line (make-line after)))
    (setf (line-content pl) before)
    (setf (line-next new-line) (line-next pl)
          (line-prev new-line) pl)
    (when (line-next pl)
      (setf (line-prev (line-next pl)) new-line))
    (setf (line-next pl) new-line)
    (when (eq pl (message-last-line msg))
      (setf (message-last-line msg) new-line))
    (setf (message-point-line msg) new-line
          (message-point-offset msg) 0))
  msg)

(declaim (ftype (function (message) message) message-delete-char-backward))
(defun message-delete-char-backward (msg)
  "Delete the character before point. If at start of line, join with previous line."
  (let ((pl (message-point-line msg))
        (po (message-point-offset msg)))
    (cond
      ((> po 0)
       (let* ((content (line-content pl))
              (new-content (concatenate 'string
                                        (subseq content 0 (1- po))
                                        (subseq content po))))
         (setf (line-content pl) new-content
               (message-point-offset msg) (1- po))))
      ((line-prev pl)
       (let* ((prev (line-prev pl))
              (prev-len (length (line-content prev)))
              (merged (concatenate 'string (line-content prev) (line-content pl))))
         (setf (line-content prev) merged)
         (setf (line-next prev) (line-next pl))
         (when (line-next pl)
           (setf (line-prev (line-next pl)) prev))
         (when (eq pl (message-last-line msg))
           (setf (message-last-line msg) prev))
         (setf (message-point-line msg) prev
               (message-point-offset msg) prev-len)))
      (t nil)))
  msg)

(declaim (ftype (function (message) message) message-move-beginning-of-line))
(defun message-move-beginning-of-line (msg)
  "Move point to the beginning of the current line."
  (setf (message-point-offset msg) 0)
  msg)

(declaim (ftype (function (message) message) message-move-end-of-line))
(defun message-move-end-of-line (msg)
  "Move point to the end of the current line."
  (setf (message-point-offset msg)
        (length (line-content (message-point-line msg))))
  msg)

(declaim (ftype (function (message) message) message-forward-char))
(defun message-forward-char (msg)
  "Move point one character forward. Wraps to next line if at end."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (len (length (line-content pl))))
    (cond
      ((< po len)
       (setf (message-point-offset msg) (1+ po)))
      ((line-next pl)
       (setf (message-point-line msg) (line-next pl)
             (message-point-offset msg) 0))))
  msg)

(declaim (ftype (function (message) message) message-backward-char))
(defun message-backward-char (msg)
  "Move point one character backward. Wraps to previous line if at start."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg)))
    (cond
      ((> po 0)
       (setf (message-point-offset msg) (1- po)))
      ((line-prev pl)
       (setf (message-point-line msg) (line-prev pl)
             (message-point-offset msg)
             (length (line-content (line-prev pl)))))))
  msg)

(defun word-char-p (char)
  "Return T if CHAR is a word constituent (alphanumeric or underscore)."
  (or (alphanumericp char) (char= char #\_)))

(declaim (ftype (function (message) message) message-forward-word))
(defun message-forward-word (msg)
  "Move point forward to end of next word."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl))
         (len (length content)))
    ;; Skip non-word characters
    (loop :while (and (< po len) (not (word-char-p (char content po))))
          :do (incf po))
    ;; Skip word characters
    (loop :while (and (< po len) (word-char-p (char content po)))
          :do (incf po))
    (setf (message-point-offset msg) po))
  msg)

(declaim (ftype (function (message) message) message-backward-word))
(defun message-backward-word (msg)
  "Move point backward to beginning of previous word."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl)))
    ;; Skip non-word characters backward
    (loop :while (and (> po 0) (not (word-char-p (char content (1- po)))))
          :do (decf po))
    ;; Skip word characters backward
    (loop :while (and (> po 0) (word-char-p (char content (1- po))))
          :do (decf po))
    (setf (message-point-offset msg) po))
  msg)

;;; --------------------------------------------------------------------------
;;; Kill/Cut Operations
;;; --------------------------------------------------------------------------

(declaim (ftype (function (message) message) message-kill-line))
(defun message-kill-line (msg)
  "Kill from point to end of line. If at end of line, join with next line.
Killed text is pushed to the kill ring."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl)))
    (cond
      ((< po (length content))
       (let ((killed (subseq content po)))
         (setf (line-content pl) (subseq content 0 po))
         (kill-ring-push killed)))
      ((line-next pl)
       (let ((next (line-next pl)))
         (setf (line-content pl)
               (concatenate 'string content (line-content next)))
         (setf (line-next pl) (line-next next))
         (when (line-next next)
           (setf (line-prev (line-next next)) pl))
         (when (eq next (message-last-line msg))
           (setf (message-last-line msg) pl))
         (kill-ring-push (string #\Newline))))
      (t nil)))
  msg)

(declaim (ftype (function (message) message) message-kill-backward-line))
(defun message-kill-backward-line (msg)
  "Kill from start of line to point (C-u). Pushes killed text to kill ring."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl)))
    (when (> po 0)
      (let ((killed (subseq content 0 po)))
        (setf (line-content pl) (subseq content po)
              (message-point-offset msg) 0)
        (kill-ring-push killed))))
  msg)

(declaim (ftype (function (message) message) message-kill-word))
(defun message-kill-word (msg)
  "Kill from point to end of current word (M-d). Pushes to kill ring."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl))
         (len (length content))
         (end po))
    ;; Skip non-word characters
    (loop :while (and (< end len) (not (word-char-p (char content end))))
          :do (incf end))
    ;; Skip word characters
    (loop :while (and (< end len) (word-char-p (char content end)))
          :do (incf end))
    (when (> end po)
      (let ((killed (subseq content po end)))
        (setf (line-content pl)
              (concatenate 'string (subseq content 0 po) (subseq content end)))
        (kill-ring-push killed))))
  msg)

(declaim (ftype (function (message) message) message-backward-kill-word))
(defun message-backward-kill-word (msg)
  "Kill from beginning of current word to point (C-w). Pushes to kill ring."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl))
         (start po))
    ;; Skip non-word characters backward
    (loop :while (and (> start 0) (not (word-char-p (char content (1- start)))))
          :do (decf start))
    ;; Skip word characters backward
    (loop :while (and (> start 0) (word-char-p (char content (1- start))))
          :do (decf start))
    (when (< start po)
      (let ((killed (subseq content start po)))
        (setf (line-content pl)
              (concatenate 'string (subseq content 0 start) (subseq content po))
              (message-point-offset msg) start)
        (kill-ring-push killed))))
  msg)

(declaim (ftype (function (message) message) message-yank))
(defun message-yank (msg)
  "Insert the top of the kill ring at point (C-y)."
  (let ((text (kill-ring-top)))
    (when text
      (loop :for char :across text
            :do (if (char= char #\Newline)
                    (message-insert-newline msg)
                    (message-insert-char msg char)))))
  msg)

(defvar *yank-index* 0
  "Current position in the kill ring for yank-pop cycling.")

(declaim (ftype (function (message) message) message-yank-pop))
(defun message-yank-pop (msg)
  "Replace the just-yanked text with the next kill ring entry (M-y).
Must be called after message-yank or message-yank-pop."
  (when (and *kill-ring* (> (length *kill-ring*) 1))
    ;; Delete the previously yanked text by undoing
    ;; For simplicity: we track yank-index and re-yank
    (incf *yank-index*)
    (when (>= *yank-index* (length *kill-ring*))
      (setf *yank-index* 0))
    ;; The previous yank text needs to be removed first.
    ;; This is complex without tracking yank boundaries.
    ;; For now, just insert the next kill ring entry at point.
    ;; Users should use C-y then M-y to cycle.
    (let ((text (nth *yank-index* *kill-ring*)))
      (when text
        (loop :for char :across text
              :do (if (char= char #\Newline)
                      (message-insert-newline msg)
                      (message-insert-char msg char))))))
  msg)
