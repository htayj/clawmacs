(in-package :clawmacs/tests)
(in-suite message-suite)

(test line-creation
  "Lines can be created with content."
  (let ((l (make-line "hello")))
    (is (string= "hello" (line-content l)))
    (is (null (line-next l)))
    (is (null (line-prev l)))))

(test line-linking
  "Lines can be linked into a doubly-linked list."
  (let* ((l1 (make-line "first"))
         (l2 (make-line "second"))
         (l3 (make-line "third")))
    (setf (line-next l1) l2
          (line-prev l2) l1
          (line-next l2) l3
          (line-prev l3) l2)
    (is (eq l2 (line-next l1)))
    (is (eq l1 (line-prev l2)))
    (is (eq l3 (line-next l2)))
    (is (eq l2 (line-prev l3)))
    (is (null (line-prev l1)))
    (is (null (line-next l3)))))

(test message-creation
  "Messages are created with a single empty line, point at (first-line, 0)."
  (let ((m (make-message :user)))
    (is (eq :user (message-sender m)))
    (is (not (null (message-first-line m))))
    (is (eq (message-first-line m) (message-last-line m)))
    (is (string= "" (line-content (message-first-line m))))
    (is (eq (message-first-line m) (message-point-line m)))
    (is (= 0 (message-point-offset m)))
    (is (null (message-mark-line m)))
    (is (not (message-read-only-p m)))
    (is (null (message-next m)))
    (is (null (message-prev m)))))

(test message-text-extraction
  "message-text returns the full text content of a message."
  (let* ((l1 (make-line "hello"))
         (l2 (make-line "world"))
         (m (make-message :user)))
    (setf (slot-value m 'clawmacs::first-line) l1
          (slot-value m 'clawmacs::last-line) l2
          (line-next l1) l2
          (line-prev l2) l1)
    (is (string= (format nil "hello~%world") (message-text m)))))

(test message-line-count
  "message-line-count returns the number of lines."
  (let* ((m (make-message :user)))
    (is (= 1 (message-line-count m)))
    (let ((l2 (make-line "second")))
      (setf (line-next (message-first-line m)) l2
            (line-prev l2) (message-first-line m)
            (slot-value m 'clawmacs::last-line) l2))
    (is (= 2 (message-line-count m)))))

(test message-insert-char
  "Inserting a character at point advances point."
  (let ((m (make-message :user)))
    (message-insert-char m #\h)
    (message-insert-char m #\i)
    (is (string= "hi" (line-content (message-first-line m))))
    (is (= 2 (message-point-offset m)))))

(test message-insert-newline
  "Inserting a newline splits the current line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (setf (message-point-offset m) 1)
    (message-insert-newline m)
    (is (= 2 (message-line-count m)))
    (is (string= "a" (line-content (message-first-line m))))
    (is (string= "b" (line-content (message-last-line m))))
    (is (eq (message-last-line m) (message-point-line m)))
    (is (= 0 (message-point-offset m)))))

(test message-delete-char-backward
  "Deleting backward removes char before point."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (message-insert-char m #\c)
    (message-delete-char-backward m)
    (is (string= "ab" (line-content (message-first-line m))))
    (is (= 2 (message-point-offset m)))))

(test message-delete-char-backward-joins-lines
  "Deleting backward at start of line joins with previous line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-newline m)
    (message-insert-char m #\b)
    (setf (message-point-offset m) 0)
    (message-delete-char-backward m)
    (is (= 1 (message-line-count m)))
    (is (string= "ab" (line-content (message-first-line m))))
    (is (= 1 (message-point-offset m)))))

(test message-move-beginning-of-line
  "C-a moves point to beginning of current line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (message-move-beginning-of-line m)
    (is (= 0 (message-point-offset m)))))

(test message-move-end-of-line
  "C-e moves point to end of current line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (message-move-beginning-of-line m)
    (message-move-end-of-line m)
    (is (= 2 (message-point-offset m)))))

(test message-kill-line
  "C-k kills from point to end of line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (message-insert-char m #\c)
    (setf (message-point-offset m) 1)
    (message-kill-line m)
    (is (string= "a" (line-content (message-point-line m))))
    (is (= 1 (message-point-offset m)))
    (is (string= "bc" (kill-ring-top)))))

(test message-kill-line-at-end-joins
  "C-k at end of line joins with next line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-newline m)
    (message-insert-char m #\b)
    (setf (message-point-line m) (message-first-line m)
          (message-point-offset m) 1)
    (message-kill-line m)
    (is (= 1 (message-line-count m)))
    (is (string= "ab" (line-content (message-first-line m))))))

(test message-yank
  "C-y inserts the top of the kill ring at point."
  (let ((m (make-message :user)))
    (kill-ring-push "hello")
    (message-yank m)
    (is (string= "hello" (line-content (message-first-line m))))
    (is (= 5 (message-point-offset m)))))
