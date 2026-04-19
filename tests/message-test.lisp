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

(test message-delete-char-forward
  "Deleting forward removes the next character and preserves point."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (message-insert-char m #\c)
    (setf (message-point-offset m) 1)
    (message-delete-char-forward m)
    (is (string= "ac" (line-content (message-first-line m))))
    (is (= 1 (message-point-offset m)))))

(test message-delete-char-forward-joins-lines
  "Deleting forward at end of line removes the line break and preserves point."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-newline m)
    (message-insert-char m #\b)
    (setf (message-point-line m) (message-first-line m)
          (message-point-offset m) 1)
    (message-delete-char-forward m)
    (is (= 1 (message-line-count m)))
    (is (string= "ab" (line-content (message-first-line m))))
    (is (eq (message-first-line m) (message-last-line m)))
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

(test message-point-mark-and-region-operations
  "Point and mark support Emacs-style region copy, kill, and exchange."
  (let ((m (make-message :user)))
    (clawmacs::set-message-text m "alpha beta gamma")
    (clawmacs::set-message-point-from-absolute-offset m 6)
    (message-set-mark-at-point m)
    (clawmacs::set-message-point-from-absolute-offset m 10)
    (is (string= "beta" (message-region-text m)))
    (message-copy-region m)
    (is (string= "beta" (kill-ring-top)))
    (message-exchange-point-and-mark m)
    (is (= 6 (clawmacs::message-point-absolute-offset m)))
    (is (= 10 (message-mark-absolute-offset m)))
    (message-kill-region m)
    (is (string= "alpha  gamma" (message-text m)))
    (is-false (message-mark-active-p m))
    (is (= 6 (clawmacs::message-point-absolute-offset m)))))

(test message-mark-whole-buffer-and-replacement-insert
  "Self insertion replaces an active region."
  (let ((m (make-message :user)))
    (clawmacs::set-message-text m "old text")
    (message-mark-whole-buffer m)
    (message-insert-char m #\x)
    (is (string= "x" (message-text m)))
    (is-false (message-mark-active-p m))
    (is (= 1 (clawmacs::message-point-absolute-offset m)))))

(test message-line-motion-and-search-cross-lines
  "Editor movement and search operate across multi-line message text."
  (let ((m (make-message :user)))
    (clawmacs::set-message-text m "one
two words
three")
    (message-forward-line m)
    (is (= 2 (message-current-line-number m)))
    (is (= 0 (message-current-column-number m)))
    (setf (message-point-offset m) 4)
    (message-forward-line m)
    (is (= 3 (message-current-line-number m)))
    (is (= 4 (message-current-column-number m)))
    (is (= 4 (message-search-backward m "two")))
    (is (= 14 (message-search-forward m "three")))
    (message-backward-word m)
    (is (= 8 (clawmacs::message-point-absolute-offset m)))))
