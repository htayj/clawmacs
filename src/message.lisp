(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Line
;;; --------------------------------------------------------------------------

(defclass line ()
  ((content :initarg :content
            :accessor line-content
            :initform ""
            :type string
            :documentation "The text content of this line.")
   (next    :initarg :next
            :accessor line-next
            :initform nil
            :type (or null line)
            :documentation "Next line in the doubly-linked list, or nil if last.")
   (prev    :initarg :prev
            :accessor line-prev
            :initform nil
            :type (or null line)
            :documentation "Previous line in the doubly-linked list, or nil if first."))
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
                  :type line
                  :documentation "First line in this message's doubly-linked list of lines.")
   (last-line     :initarg :last-line
                  :accessor message-last-line
                  :type line
                  :documentation "Last line in this message's doubly-linked list of lines.")
   (point-line    :initarg :point-line
                  :accessor message-point-line
                  :type line
                  :documentation "The line containing the editing cursor (point).")
   (point-offset  :initarg :point-offset
                  :accessor message-point-offset
                  :initform 0
                  :type fixnum
                  :documentation "Character offset of the cursor within point-line.")
   (mark-line     :initarg :mark-line
                  :accessor message-mark-line
                  :initform nil
                  :type (or null line)
                  :documentation "The line containing the mark for selection, or nil.")
   (mark-offset   :initarg :mark-offset
                  :accessor message-mark-offset
                  :initform nil
                  :type (or null fixnum)
                  :documentation "Character offset of the mark within mark-line, or nil.")
   (sender        :initarg :sender
                  :accessor message-sender
                  :type keyword
                  :documentation "Keyword identifying who sent this message (e.g. :user, :agent, :system).")
   (timestamp     :initarg :timestamp
                  :accessor message-timestamp
                  :initform nil
                  :type (or null integer)
                  :documentation "Universal time when this message was finalized, or nil.")
   (read-only-p   :initarg :read-only-p
                  :accessor message-read-only-p
                  :initform nil
                  :type boolean
                  :documentation "When non-nil, this message cannot be edited (agent/system messages).")
   (next          :initarg :next
                  :accessor message-next
                  :initform nil
                  :type (or null message)
                  :documentation "Next message in the buffer's doubly-linked list, or nil if last.")
   (prev          :initarg :prev
                  :accessor message-prev
                  :initform nil
                  :type (or null message)
                  :documentation "Previous message in the buffer's doubly-linked list, or nil if first.")
   (raw-content   :initarg :raw-content
                  :accessor message-raw-content
                  :initform nil
                  :type list
                  :documentation "When non-nil, holds the API-format content blocks
(list of alists) for this message. Used for tool_use assistant messages
and tool_result user messages to preserve structured content for API
round-tripping.")
   (metadata      :initarg :metadata
                  :accessor message-metadata
                  :initform nil
                  :type list
                  :documentation "Display-only metadata alist for a message.
This is not sent to providers.")
   (entry-id      :initarg :entry-id
                  :accessor message-entry-id
                  :initform nil
                  :type (or null string)
                  :documentation "Durable session tree entry id for this message.")
   (parent-entry-id :initarg :parent-entry-id
                    :accessor message-parent-entry-id
                    :initform nil
                    :type (or null string)
                    :documentation "Durable parent entry id for this message."))
  (:documentation
   "A message in the chat buffer. Contains a doubly-linked list of lines,
a point and optional mark for intra-message cursor/selection, sender
identity, and links to adjacent messages in the buffer."))

(declaim (ftype (function (keyword &key (:read-only-p boolean))
                          message)
                make-message))
(defun make-message (sender &key (read-only-p nil))
  "Create a new message with a single empty line. Point starts at offset 0."
  (let* ((initial-line (make-line ""))
         (msg (make-instance 'message
                :first-line initial-line
                :last-line initial-line
                :point-line initial-line
                :point-offset 0
                :sender sender
                :read-only-p read-only-p)))
    msg))

(declaim (ftype (function (list keyword) t) message-metadata-value)
         (ftype (function (message &rest t) message) put-message-metadata))
(defun message-metadata-value (metadata key)
  "Return KEY's value from a message metadata alist."
  (declare (type list metadata)
           (type keyword key))
  (cdr (assoc key metadata :test #'eq)))

(defun put-message-metadata (msg &rest pairs)
  "Set metadata PAIRS on MSG and return MSG."
  (declare (type message msg)
           (type list pairs))
  (let ((metadata (copy-list (message-metadata msg))))
    (loop :for (key value) :on pairs :by #'cddr
          :do (setf metadata
                    (acons key value
                           (remove key metadata :key #'car :test #'eq))))
    (setf (message-metadata msg) metadata)
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

(declaim (ftype (function (message string) message) set-message-text))
(defun set-message-text (msg text)
  "Replace MSG's lines with lines split from TEXT on newlines."
  (declare (type message msg)
           (type string text))
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
          (message-point-offset msg) 0
          (message-mark-line msg) nil
          (message-mark-offset msg) nil))
  msg)

(declaim (ftype (function (message) fixnum) message-point-absolute-offset))
(defun message-point-absolute-offset (msg)
  "Return MSG point as a character offset in MESSAGE-TEXT coordinates."
  (let ((offset 0)
        (target-line (message-point-line msg)))
    (loop :for line := (message-first-line msg) :then (line-next line)
          :while line
          :do (if (eq line target-line)
                  (return (+ offset
                             (max 0
                                  (min (message-point-offset msg)
                                       (length (line-content line))))))
                  (incf offset (1+ (length (line-content line)))))
          :finally (return offset))))

(declaim (ftype (function (message integer) message)
                set-message-point-from-absolute-offset))
(defun set-message-point-from-absolute-offset (msg offset)
  "Move MSG point to absolute text OFFSET and return MSG."
  (let ((remaining (max 0 offset)))
    (loop :for line := (message-first-line msg) :then (line-next line)
          :while line
          :for len := (length (line-content line))
          :do (cond
                ((or (<= remaining len) (null (line-next line)))
                 (setf (message-point-line msg) line
                       (message-point-offset msg)
                       (max 0 (min remaining len)))
                 (return msg))
                (t
                 (decf remaining (1+ len))))
          :finally (return msg))))

(declaim (ftype (function (message) boolean) message-mark-active-p))
(defun message-mark-active-p (msg)
  "Return true when MSG has an active mark."
  (and (message-mark-line msg)
       (integerp (message-mark-offset msg))
       t))

(declaim (ftype (function (message) (or null fixnum))
                message-mark-absolute-offset))
(defun message-mark-absolute-offset (msg)
  "Return MSG mark as an absolute text offset, or NIL when no mark is set."
  (when (message-mark-active-p msg)
    (let ((offset 0)
          (target-line (message-mark-line msg)))
      (loop :for line := (message-first-line msg) :then (line-next line)
            :while line
            :do (if (eq line target-line)
                    (return (+ offset
                               (max 0
                                    (min (or (message-mark-offset msg) 0)
                                         (length (line-content line))))))
                    (incf offset (1+ (length (line-content line)))))
            :finally (return nil)))))

(declaim (ftype (function (message) message) message-set-mark-at-point))
(defun message-set-mark-at-point (msg)
  "Set MSG mark to the current point."
  (setf (message-mark-line msg) (message-point-line msg)
        (message-mark-offset msg) (message-point-offset msg))
  msg)

(declaim (ftype (function (message) message) message-clear-mark))
(defun message-clear-mark (msg)
  "Clear MSG mark."
  (setf (message-mark-line msg) nil
        (message-mark-offset msg) nil)
  msg)

(declaim (ftype (function (message) (values integer integer))
                message-region-bounds))
(defun message-region-bounds (msg)
  "Return MSG region bounds as START and END absolute offsets.
Signals an error when no mark is active."
  (declare (type message msg))
  (let ((mark (message-mark-absolute-offset msg)))
    (unless mark
      (error "No active region."))
    (let ((point (message-point-absolute-offset msg)))
      (if (<= mark point)
          (values mark point)
          (values point mark)))))

(declaim (ftype (function (message) string) message-region-text))
(defun message-region-text (msg)
  "Return the text inside MSG's active region."
  (multiple-value-bind (start end)
      (message-region-bounds msg)
    (subseq (message-text msg) start end)))

(declaim (ftype (function (message) message) message-delete-region))
(defun message-delete-region (msg)
  "Delete MSG's active region and leave point at the region start."
  (multiple-value-bind (start end)
      (message-region-bounds msg)
    (let ((text (message-text msg)))
      (set-message-text
       msg
       (concatenate 'string (subseq text 0 start) (subseq text end)))
      (set-message-point-from-absolute-offset msg start)
      (message-clear-mark msg)))
  msg)

(declaim (ftype (function (string) string) kill-ring-push))
(declaim (ftype (function (message) message) message-kill-region))
(defun message-kill-region (msg)
  "Kill MSG's active region into the kill ring."
  (kill-ring-push (message-region-text msg))
  (message-delete-region msg))

(declaim (ftype (function (message) message) message-copy-region))
(defun message-copy-region (msg)
  "Copy MSG's active region into the kill ring without deleting it."
  (kill-ring-push (message-region-text msg))
  msg)

(declaim (ftype (function (message) message) message-exchange-point-and-mark))
(defun message-exchange-point-and-mark (msg)
  "Swap MSG point and mark."
  (let ((mark (message-mark-absolute-offset msg)))
    (unless mark
      (error "No active region."))
    (let ((point (message-point-absolute-offset msg)))
      (set-message-point-from-absolute-offset msg mark)
      (let ((mark-line (message-point-line msg))
            (mark-offset (message-point-offset msg)))
        (set-message-point-from-absolute-offset msg point)
        (setf (message-mark-line msg) (message-point-line msg)
              (message-mark-offset msg) (message-point-offset msg)
              (message-point-line msg) mark-line
              (message-point-offset msg) mark-offset))))
  msg)

(declaim (ftype (function (message) message) message-mark-whole-buffer))
(defun message-mark-whole-buffer (msg)
  "Mark the whole MSG text, leaving point at the beginning."
  (let ((end (length (message-text msg))))
    (set-message-point-from-absolute-offset msg end)
    (message-set-mark-at-point msg)
    (set-message-point-from-absolute-offset msg 0))
  msg)

(declaim (ftype (function (message) fixnum) message-current-line-number))
(defun message-current-line-number (msg)
  "Return MSG point line number, one-based."
  (loop :for line := (message-first-line msg) :then (line-next line)
        :for number :from 1
        :while line
        :when (eq line (message-point-line msg))
          :return number
        :finally (return 1)))

(declaim (ftype (function (message) fixnum) message-current-column-number))
(defun message-current-column-number (msg)
  "Return MSG point column number, zero-based."
  (max 0 (message-point-offset msg)))

;;; --------------------------------------------------------------------------
;;; Kill Ring
;;; --------------------------------------------------------------------------

(declaim (type list *kill-ring*)
         (type (integer 1 *) *kill-ring-max*))
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
  (when (message-mark-active-p msg)
    (message-delete-region msg))
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
  (when (message-mark-active-p msg)
    (message-delete-region msg))
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
  (when (message-mark-active-p msg)
    (return-from message-delete-char-backward (message-delete-region msg)))
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

(declaim (ftype (function (message) message) message-delete-char-forward))
(defun message-delete-char-forward (msg)
  "Delete the character after point. If at end of line, join with next line."
  (when (message-mark-active-p msg)
    (return-from message-delete-char-forward (message-delete-region msg)))
  (let ((pl (message-point-line msg))
        (po (message-point-offset msg)))
    (cond
      ((< po (length (line-content pl)))
       (let* ((content (line-content pl))
              (new-content (concatenate 'string
                                        (subseq content 0 po)
                                        (subseq content (1+ po)))))
         (setf (line-content pl) new-content)))
      ((line-next pl)
       (let ((next (line-next pl)))
         (setf (line-content pl)
               (concatenate 'string (line-content pl) (line-content next)))
         (setf (line-next pl) (line-next next))
         (when (line-next next)
           (setf (line-prev (line-next next)) pl))
         (when (eq next (message-last-line msg))
           (setf (message-last-line msg) pl))))
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

(declaim (ftype (function (character) boolean) word-char-p))
(defun word-char-p (char)
  "Return T if CHAR is a word constituent (alphanumeric or underscore)."
  (declare (type character char))
  (not (null (or (alphanumericp char) (char= char #\_)))))

(declaim (ftype (function (message) message) message-forward-word))
(defun message-forward-word (msg)
  "Move point forward to end of next word."
  (let* ((text (message-text msg))
         (pos (message-point-absolute-offset msg))
         (len (length text)))
    ;; Skip non-word characters
    (loop :while (and (< pos len) (not (word-char-p (char text pos))))
          :do (incf pos))
    ;; Skip word characters
    (loop :while (and (< pos len) (word-char-p (char text pos)))
          :do (incf pos))
    (set-message-point-from-absolute-offset msg pos))
  msg)

(declaim (ftype (function (message) message) message-backward-word))
(defun message-backward-word (msg)
  "Move point backward to beginning of previous word."
  (let* ((text (message-text msg))
         (pos (message-point-absolute-offset msg)))
    ;; Skip non-word characters backward
    (loop :while (and (> pos 0) (not (word-char-p (char text (1- pos)))))
          :do (decf pos))
    ;; Skip word characters backward
    (loop :while (and (> pos 0) (word-char-p (char text (1- pos))))
          :do (decf pos))
    (set-message-point-from-absolute-offset msg pos))
  msg)

(declaim (ftype (function (message) message) message-forward-line))
(defun message-forward-line (msg)
  "Move point to the next line, preserving the current column when possible."
  (let* ((line (message-point-line msg))
         (column (message-point-offset msg))
         (next (and line (line-next line))))
    (when next
      (setf (message-point-line msg) next
            (message-point-offset msg)
            (min column (length (line-content next))))))
  msg)

(declaim (ftype (function (message) message) message-backward-line))
(defun message-backward-line (msg)
  "Move point to the previous line, preserving the current column when possible."
  (let* ((line (message-point-line msg))
         (column (message-point-offset msg))
         (prev (and line (line-prev line))))
    (when prev
      (setf (message-point-line msg) prev
            (message-point-offset msg)
            (min column (length (line-content prev))))))
  msg)

(declaim (ftype (function (message) message) message-beginning-of-buffer))
(defun message-beginning-of-buffer (msg)
  "Move point to the beginning of MSG."
  (setf (message-point-line msg) (message-first-line msg)
        (message-point-offset msg) 0)
  msg)

(declaim (ftype (function (message) message) message-end-of-buffer))
(defun message-end-of-buffer (msg)
  "Move point to the end of MSG."
  (setf (message-point-line msg) (message-last-line msg)
        (message-point-offset msg)
        (length (line-content (message-last-line msg))))
  msg)

(declaim (ftype (function (message string) (or null fixnum))
                message-search-forward))
(defun message-search-forward (msg query)
  "Search forward for QUERY from point and move point to the match start."
  (let* ((text (message-text msg))
         (start (message-point-absolute-offset msg))
         (pos (and (plusp (length query))
                   (search query text :start2 start))))
    (when pos
      (set-message-point-from-absolute-offset msg pos))
    pos))

(declaim (ftype (function (message string) (or null fixnum))
                message-search-backward))
(defun message-search-backward (msg query)
  "Search backward for QUERY before point and move point to the match start."
  (let* ((text (message-text msg))
         (end (message-point-absolute-offset msg))
         (pos (and (plusp (length query))
                   (search query text :end2 end :from-end t))))
    (when pos
      (set-message-point-from-absolute-offset msg pos))
    pos))

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
  (let* ((text (message-text msg))
         (start (message-point-absolute-offset msg))
         (len (length text))
         (end start))
    ;; Skip non-word characters
    (loop :while (and (< end len) (not (word-char-p (char text end))))
          :do (incf end))
    ;; Skip word characters
    (loop :while (and (< end len) (word-char-p (char text end)))
          :do (incf end))
    (when (> end start)
      (kill-ring-push (subseq text start end))
      (set-message-text
       msg
       (concatenate 'string (subseq text 0 start) (subseq text end)))
      (set-message-point-from-absolute-offset msg start)))
  msg)

(declaim (ftype (function (message) message) message-backward-kill-word))
(defun message-backward-kill-word (msg)
  "Kill from beginning of current word to point (C-w). Pushes to kill ring."
  (let* ((text (message-text msg))
         (end (message-point-absolute-offset msg))
         (start end))
    ;; Skip non-word characters backward
    (loop :while (and (> start 0) (not (word-char-p (char text (1- start)))))
          :do (decf start))
    ;; Skip word characters backward
    (loop :while (and (> start 0) (word-char-p (char text (1- start))))
          :do (decf start))
    (when (< start end)
      (kill-ring-push (subseq text start end))
      (set-message-text
       msg
       (concatenate 'string (subseq text 0 start) (subseq text end)))
      (set-message-point-from-absolute-offset msg start)))
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

(declaim (type fixnum *yank-index*))
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
