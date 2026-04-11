(in-package :clawmacs/tests)
(in-suite sexed-suite)

(defun sexed-test-directory ()
  "Return a fresh temporary directory for sexed tests."
  (let ((dir (merge-pathnames
              (format nil "clawmacs-sexed-tests-~36R/"
                      (random (expt 36 8)))
              #P"/tmp/")))
    (ensure-directories-exist dir)
    dir))

(defun write-sexed-test-file (path text)
  "Write TEXT to PATH for sexed file adapter tests."
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text stream)))

(test sexed-balanced-p-ignores-strings-comments-and-character-literals
  "Structural scanning ignores parens inside strings, comments, and char literals."
  (let ((text (format nil
                      "(list \"(\" #\\) ; ignored )~% #| ignored ) #| nested ( |# |# :ok)")))
    (is-true (sexed-balanced-p text))
    (is (null (sexed-diagnostics text)))))

(test sexed-diagnostics-report-unbalanced-source
  "Unbalanced source returns diagnostics instead of pretending the edit is safe."
  (let ((missing-close (sexed-diagnostics "(foo (bar)"))
        (unexpected-close (sexed-diagnostics "(foo))")))
    (is-false (sexed-balanced-p "(foo (bar)"))
    (is (eq :missing-close-paren (getf (first missing-close) :kind)))
    (is (eq :unexpected-close-paren (getf (first unexpected-close) :kind)))))

(test sexed-find-forms-selects-by-head-name-depth-and-nth
  "Agents can address forms by structural selectors instead of cursor positions."
  (let ((text (format nil "(defun foo (x) (bar x))~%(defun bar () nil)")))
    (let ((forms (sexed-find-forms text :head "defun" :name "foo")))
      (is (= 1 (length forms)))
      (is (string= "defun" (getf (first forms) :head)))
      (is (string= "foo" (getf (first forms) :name))))
    (is (string= "(bar x)"
                 (sexed-form-text text '(:head "bar" :depth 1))))
    (is (string= "(defun bar () nil)"
                 (sexed-form-text text '(:head "defun" :nth 1))))
    (signals error
      (sexed-form-text text '(:head "defun")))))

(test sexed-outline-is-agent-readable
  "Outlines include ids, depths, heads, names, spans, and previews."
  (let ((outline (sexed-outline-to-string "(defun foo () (list 1 2))"
                                          :max-depth 1)))
    (is (search "[0] d0 list defun foo" outline))
    (is (search "[4] d1 list list" outline))))

(test sexed-pure-edits-preserve-balance
  "Pure edit functions return modified text and reject unbalanced replacements."
  (let ((text "(defun foo () (+ 1 2))"))
    (is (string= "(defun foo () (list 1 2))"
                 (sexed-replace-form text '(:head "+") "(list 1 2)")))
    (is (string= "(defun foo () (progn (+ 1 2)))"
                 (sexed-wrap-form text '(:head "+") "(progn " ")")))
    (is (string= "(defun foo () )"
                 (sexed-delete-form text '(:head "+"))))
    (signals error
      (sexed-replace-form text '(:head "+") "(list 1 2"))))

(test sexed-insert-edits-add-separating-whitespace
  "Insert helpers add whitespace when adjacent forms would otherwise touch."
  (is (string= "(foo (bar) (quux) (baz))"
               (sexed-insert-after-form
                "(foo (bar) (baz))"
                '(:head "bar")
                "(quux)")))
  (is (string= "(foo (quux) (bar))"
               (sexed-insert-before-form
                "(foo(bar))"
                '(:head "bar")
                "(quux)")))
  (is (string= "(foo (bar) (quux) (baz))"
               (sexed-insert-form-after
                "(foo (bar) (baz))"
                '(:head "bar")
                "(quux)"))))

(test sexed-structural-list-edits
  "Splice, raise, slurp, and barf operate on selected s-expression spans."
  (is (string= "(foo bar baz quux)"
               (sexed-splice-form "(foo (bar baz) quux)" '(:head "bar"))))
  (is (string= "(bar baz)"
               (sexed-raise-form "(foo (bar baz) quux)"
                                 '(:head "foo")
                                 '(:head "bar"))))
  (is (string= "(foo (bar baz quux))"
               (sexed-slurp-forward "(foo (bar baz) quux)" '(:head "bar"))))
  (is (string= "(foo (bar baz) quux)"
               (sexed-barf-forward "(foo (bar baz quux))" '(:head "bar")))))

(test sexed-file-adapters-validate-before-writing
  "File adapters edit sandboxed files and leave files unchanged on invalid edits."
  (let* ((dir (sexed-test-directory))
         (file (merge-pathnames "sample.lisp" dir))
         (*sandbox-root* (truename dir))
         (initial "(defun foo () (+ 1 2))"))
    (write-sexed-test-file file initial)
    (let ((result (sexed-replace-file-form "sample.lisp"
                                           '(:head "+")
                                           "(list 1 2)")))
      (is (eq :ok (getf result :status)))
      (is (string= "(defun foo () (list 1 2))"
                   (uiop:read-file-string file))))
    (signals error
      (sexed-replace-file-form "sample.lisp"
                               '(:head "list")
                               "(vector 1 2"))
    (is (string= "(defun foo () (list 1 2))"
                 (uiop:read-file-string file)))))

(test sexed-message-adapters-edit-editable-messages
  "Message adapters update editable message text and reject read-only messages."
  (let ((message (make-message :user))
        (read-only (make-message :agent :read-only-p t)))
    (clawmacs::set-message-text message "(note (list 1 2))")
    (sexed-replace-message-form message '(:head "list") "(vector 1 2)")
    (is (string= "(note (vector 1 2))" (message-text message)))
    (sexed-insert-message-form-after message
                                     '(:head "vector")
                                     "(status ok)")
    (is (string= "(note (vector 1 2) (status ok))" (message-text message)))
    (signals error
      (sexed-replace-message-form read-only '(:id 0) "(ok)"))))

(test sexed-can-edit-scratch-buffer-text
  "Scratch buffer text can be edited structurally through the message adapter."
  (let ((*buffer-ring* nil)
        (*scratch-buffer-initial-text* "(scratch (todo one))"))
    (clawmacs::init-default-keymap)
    (clawmacs::init-global-faces)
    (let ((scratch (ensure-scratch-buffer)))
      (sexed-replace-message-form (buffer-input-message scratch)
                                  '(:head "todo")
                                  "(done one)")
      (is (string= "(scratch (done one))"
                   (scratch-buffer-text scratch))))))

(test sexed-scratch-adapters-hide-message-plumbing
  "Scratch adapters let agents edit scratch text without touching message internals."
  (let ((*buffer-ring* nil)
        (*scratch-buffer-initial-text* ""))
    (clawmacs::init-default-keymap)
    (clawmacs::init-global-faces)
    (ensure-scratch-buffer)
    (setf (scratch-buffer-text)
          "(workspace (todo alpha) (todo beta) (notes (keep old)))")
    (is (search "todo beta"
                (sexed-scratch-outline-to-string :head "todo")))
    (let ((replace-result
            (sexed-replace-scratch-form '(:head "todo" :nth 1)
                                        "(done beta)")))
      (is (eq :ok (getf replace-result :status)))
      (is (search "(done beta)" (getf replace-result :final-text))))
    (let ((insert-result
            (sexed-insert-after-scratch-form
             '(:head "done")
             "(note \"sexed edited scratch\")")))
      (is (eq :ok (getf insert-result :status)))
      (is (string= "(workspace (todo alpha) (done beta) (note \"sexed edited scratch\") (notes (keep old)))"
                   (scratch-buffer-text)))
      (is-true (getf insert-result :balanced)))))
