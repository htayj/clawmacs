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

(defun sexed-test-top-level-calls (count)
  "Return COUNT simple top-level forms."
  (with-output-to-string (out)
    (dotimes (index count)
      (format out "(item-~D)~%" index))))

(defun sexed-test-wide-form (count)
  "Return one top-level form with COUNT direct structural children."
  (with-output-to-string (out)
    (format out "(progn")
    (dotimes (index count)
      (format out "~%  (item-~D)" index))
    (format out ")~%")))

(defmacro with-sexed-tool-registry-restored (&body body)
  "Run BODY with tool registries restored afterwards."
  `(let ((tool-snapshot
           (make-hash-table :test (hash-table-test clawmacs::*tool-table*)))
         (agent-tool-snapshot
           (make-hash-table
            :test (hash-table-test clawmacs::*agent-tool-metadata-table*)))
         (agent-tool-name-snapshot
           (make-hash-table
            :test (hash-table-test clawmacs::*agent-tool-name-table*))))
     (maphash (lambda (key value)
                (setf (gethash key tool-snapshot) value))
              clawmacs::*tool-table*)
     (maphash (lambda (key value)
                (setf (gethash key agent-tool-snapshot) value))
              clawmacs::*agent-tool-metadata-table*)
     (maphash (lambda (key value)
                (setf (gethash key agent-tool-name-snapshot) value))
              clawmacs::*agent-tool-name-table*)
     (unwind-protect
          (progn ,@body)
       (clrhash clawmacs::*tool-table*)
       (maphash (lambda (key value)
                  (setf (gethash key clawmacs::*tool-table*) value))
                tool-snapshot)
       (clrhash clawmacs::*agent-tool-metadata-table*)
       (maphash (lambda (key value)
                  (setf (gethash key clawmacs::*agent-tool-metadata-table*)
                        value))
                agent-tool-snapshot)
       (clrhash clawmacs::*agent-tool-name-table*)
       (maphash (lambda (key value)
                  (setf (gethash key clawmacs::*agent-tool-name-table*) value))
                agent-tool-name-snapshot))))

(test sexed-balanced-p-ignores-strings-comments-and-character-literals
  "Structural scanning ignores parens inside strings, comments, and char literals."
  (let ((text (format nil
                      "(list \"(\" #\\) ; ignored )~% #| ignored ) #| nested ( |# |# :ok)")))
    (is-true (sexed-balanced-p text))
    (is-true (balanced-parentheses-p text))
    (is (null (sexed-diagnostics text)))))

(test sexed-diagnostics-report-unbalanced-source
  "Unbalanced source returns diagnostics instead of pretending the edit is safe."
  (let ((missing-close (sexed-diagnostics "(foo (bar)"))
        (unexpected-close (sexed-diagnostics "(foo))")))
    (is-false (sexed-balanced-p "(foo (bar)"))
    (is-false (balanced-parentheses-p "(foo (bar)"))
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
      (sexed-form-text text '(:id 1)))
    (is (string= "defun"
                 (sexed-form-text text '(:id 1 :include-atoms t))))
    (signals error
      (sexed-form-text text '(:head "defun")))))

(test sexed-outline-is-agent-readable
  "Outlines include ids, depths, heads, names, spans, and previews."
  (let ((outline (sexed-outline-to-string "(defun foo () (list 1 2))"
                                          :max-depth 1)))
    (is (search "[0] d0 list defun foo" outline))
    (is (search "[4] d1 list list" outline))))

(test sexed-outline-filter-miss-includes-top-level-fallback
  "Filtered misses still give agents enough structure to choose a selector."
  (let ((outline (sexed-outline-to-string "(defun foo () (list 1 2))"
                                          :head "missing")))
    (is (search "No forms matched for the supplied filters." outline))
    (is (search "Unfiltered top-level forms:" outline))
    (is (search "[0] d0 list defun foo" outline))))

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
                "(quux)")))
  (is (string= (format nil "(defun a () 1)~%~%(defun b () 2)~%~%(defun c () 3)")
               (sexed-insert-after-form
                (format nil "(defun a () 1)~%~%(defun c () 3)")
                '(:head "defun" :name "a")
                (format nil "~%(defun b () 2)"))))
  (is (string= (format nil "(defun a () 1)~%~%(defun b () 2)~%~%(defun c () 3)")
               (sexed-insert-after-form
                (format nil "(defun a () 1)~%~%(defun c () 3)")
                '(:head "defun" :name "a")
                "(defun b () 2)"))))

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
  "File adapters edit working-directory-relative files and preserve invalid edits."
  (let* ((dir (sexed-test-directory))
         (file (merge-pathnames "sample.lisp" dir))
         (*tool-working-directory* (truename dir))
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

(test sexed-project-adapters-edit-project-resources
  "Project-aware adapters edit persistent Lisp resources through the project layer."
  (let* ((dir (sexed-test-directory))
         (*project-registry* (make-hash-table :test #'equal))
         (file (merge-pathnames "source.lisp" dir))
         (initial "(defun foo () (+ 1 2))"))
    (write-sexed-test-file file initial)
    (define-project "sexed" :root dir)
    (is (search "defun foo"
                (sexed-project-outline-to-string "sexed"
                                                "source.lisp"
                                                :head "defun")))
    (let ((result (sexed-replace-project-form "sexed"
                                              "source.lisp"
                                              '(:head "+")
                                              "(list 1 2)")))
      (is (eq :ok (getf result :status)))
      (is-true (getf result :balanced)))
    (is (string= "(defun foo () (list 1 2))"
                 (project-read-file "sexed" "source.lisp")))
    (signals error
      (sexed-replace-project-form "sexed"
                                  "source.lisp"
                                  '(:head "list")
                                  "(vector 1 2"))
    (is (string= "(defun foo () (list 1 2))"
                 (project-read-file "sexed" "source.lisp")))))

(test sexed-read-tools-return-exact-text-for-ordinary-files
  "Safety pagination does not change normal small-file reads."
  (let* ((dir (sexed-test-directory))
         (file (merge-pathnames "small.lisp" dir))
         (*tool-working-directory* (truename dir))
         (text "(defun small () :ok)"))
    (write-sexed-test-file file text)
    (let ((clawmacs::*sexed-read-form-count-limit* 3))
      (is (string= text
                   (clawmacs::sexed-tool-file-read
                    '(:path "small.lisp")))))))

(test sexed-read-tools-page-structurally-large-files
  "Large files return structural safety pages with continuation hints."
  (let* ((dir (sexed-test-directory))
         (file (merge-pathnames "large.lisp" dir))
         (*tool-working-directory* (truename dir))
         (text (sexed-test-top-level-calls 5)))
    (write-sexed-test-file file text)
    (let ((clawmacs::*sexed-read-form-count-limit* 3))
      (let ((first-page
              (clawmacs::sexed-tool-file-read
               '(:path "large.lisp"))))
        (is (search "Sexed structural read page" first-page))
        (is (search "Mode: safety fallback" first-page))
        (is (search "Total structural forms: 5" first-page))
        (is (search "Continue: call sexed_file_read with offset 4" first-page))
        (is (search "item-0" first-page))
        (is (search "item-2" first-page))
        (is-false (search "item-4" first-page))
        (is-false (string= text first-page)))
      (let ((next-page
              (clawmacs::sexed-tool-file-read
               '(:path "large.lisp" :offset 4 :limit 2))))
        (is (search "Page: offset 4, limit 2" next-page))
        (is (search "item-3" next-page))
        (is (search "item-4" next-page))
        (is-false (search "item-0" next-page)))
      (is (string= text
                   (clawmacs::sexed-tool-file-read
                    '(:path "large.lisp" :full t)))))))

(test sexed-read-tools-page-inside-selected-forms
  "Selector and level options let agents drill into giant structural forms."
  (let* ((dir (sexed-test-directory))
         (file (merge-pathnames "wide.lisp" dir))
         (*tool-working-directory* (truename dir))
         (text (sexed-test-wide-form 5)))
    (write-sexed-test-file file text)
    (let ((clawmacs::*sexed-read-form-count-limit* 3))
      (let ((top-page
              (clawmacs::sexed-tool-file-read
               '(:path "wide.lisp"))))
        (is (search "Sexed structural read page" top-page))
        (is (search "progn" top-page))
        (is (search "Drill down: call sexed_file_read with selector" top-page)))
      (let ((child-page
              (clawmacs::sexed-tool-file-read
               '(:path "wide.lisp"
                 :selector ((:id . 0))
                 :level 1
                 :limit 2))))
        (is (search "Scope: selector id 0" child-page))
        (is (search "Page: offset 1, limit 2, level 1" child-page))
        (is (search "item-0" child-page))
        (is (search "item-1" child-page))
        (is-false (search "item-3" child-page))
        (is (search "and the same selector" child-page)))
      (is (string= "(progn
  (item-0)
  (item-1)
  (item-2)
  (item-3)
  (item-4))"
                   (clawmacs::sexed-tool-file-read
                    '(:path "wide.lisp"
                      :selector ((:id . 0))
                      :full t)))))))

(test sexed-project-read-tools-share-safety-pagination
  "Project reads use the same structural safety pagination as file reads."
  (let* ((dir (sexed-test-directory))
         (*project-registry* (make-hash-table :test #'equal))
         (file (merge-pathnames "large.lisp" dir)))
    (write-sexed-test-file file (sexed-test-top-level-calls 5))
    (define-project "sexed-page" :root dir)
    (let ((clawmacs::*sexed-read-form-count-limit* 3))
      (let ((page
              (clawmacs::sexed-tool-project-read
               '(:project "sexed-page"
                 :path "large.lisp"))))
        (is (search "Sexed structural read page" page))
        (is (search "Source: sexed-page/large.lisp" page))
        (is (search "Continue: call sexed_project_read with offset 4" page))))))

(test sexed-package-prompt-points-to-tools-not-lisp-eval
  "The package prompt advertises provider tools instead of function-call examples."
  (with-sexed-tool-registry-restored
    (with-package-state-override ((default-package-test-channels))
      (clawmacs::init-tools)
      (set-package-enablement-scope "sexed" :global)
      (load-active-packages)
      (let ((prompt (render-package-prompt-sections
                     (list-package-prompt-sections))))
        (is (search "sexed_project_outline" prompt))
        (is (search "sexed_project_edit" prompt))
        (is-false (search "lisp_eval" prompt :test #'char-equal))
        (is-false (search "raw Lisp" prompt :test #'char-equal))
        (is-false (search "evaluate" prompt :test #'char-equal))
        (is-false (search "change_set" prompt :test #'char-equal))
        (is-false (search "init" prompt :test #'char-equal))
        (is-false (search "scratch" prompt :test #'char-equal))
        (is-false (search "(sexed-" prompt :test #'char=))))))

(test sexed-package-tools-edit-project-files
  "Enabled sexed package tools expose disk-backed structural read and edit adapters."
  (with-sexed-tool-registry-restored
    (with-package-state-override ((default-package-test-channels))
      (let* ((dir (sexed-test-directory))
             (*project-registry* (make-hash-table :test #'equal))
             (*tool-working-directory* (truename dir))
             (file (merge-pathnames "source.lisp" dir)))
        (write-sexed-test-file file "(defun foo () (+ 1 2))")
        (define-project "sexed-tool" :root dir)
        (clawmacs::init-tools)
        (set-package-enablement-scope "sexed" :global)
        (load-active-packages)
        (let* ((*current-caller* :user)
               (tools (coerce (tool-definitions-for-api) 'list))
               (tool-names (mapcar (lambda (tool)
                                     (cdr (assoc :name tool)))
                                   tools)))
          (labels ((tool-properties (name)
                     (let* ((tool (find name tools
                                        :key (lambda (entry)
                                               (cdr (assoc :name entry)))
                                        :test #'string=))
                            (schema (cdr (assoc :input--schema tool))))
                       (cdr (assoc :properties schema)))))
            (dolist (arg '("offset" "limit" "selector" "level"
                           "include-atoms" "full"))
              (is (assoc arg (tool-properties "sexed_file_read")
                         :test #'string=))
              (is (assoc arg (tool-properties "sexed_project_read")
                         :test #'string=))))
          (is (member "sexed_project_outline" tool-names :test #'string=))
          (is (member "sexed_project_edit" tool-names :test #'string=))
          (is (member "sexed_project_edits" tool-names :test #'string=))
          (is (member "sexed_project_read" tool-names :test #'string=))
          (is (member "sexed_project_write" tool-names :test #'string=))
          (is (member "sexed_file_read" tool-names :test #'string=))
          (is (member "sexed_file_write" tool-names :test #'string=))
          (is (member "sexed_file_edits" tool-names :test #'string=))
          (is (member "sexed_text_edits" tool-names :test #'string=))
          (is-false (member "sexed_stage_project_edit" tool-names
                            :test #'string=))
          (is-false (member "sexed_change_set_begin" tool-names
                            :test #'string=))
          (is-false (member "sexed_init_edit" tool-names :test #'string=))
          (is-false (member "sexed_scratch_edit" tool-names :test #'string=))
          (is (member "sexed_text_diagnostics" tool-names :test #'string=))
          (let ((diagnostics
                  (clawmacs::lisp-data-read
                   (execute-tool "sexed_text_diagnostics"
                                 '(:text "(defun broken ()")))))
            (is-false (getf diagnostics :balanced))
            (is (eq :missing-close-paren
                    (getf (first (getf diagnostics :diagnostics)) :kind))))
          (is (string= "(defun foo () (vector 1 2))"
                       (execute-tool
                        "sexed_text_edit"
                        '(:text "(defun foo () (+ 1 2))"
                          :operation "replace"
                          :selector (("head" . "+"))
                          :new_text "(vector 1 2)"))))
          (is (string= "(defun foo () (list 1 2))"
                       (execute-tool
                        "sexed_text_edit"
                        '(:text "(defun foo () (+ 1 2))"
                          :operation "replace"
                          :selector (("head" . "+"))
                          :new--text "(list 1 2)"))))
          (is (string= "(defun foo () (array 1 2))"
                       (execute-tool
                        "sexed_text_edit"
                        '(:text "(defun foo () (+ 1 2))"
                          :operation "replace"
                          :selector (("head" . "+"))
                          :replacement "(array 1 2)"))))
          (is (string= "(defun foo () (quote 1 2))"
                       (execute-tool
                        "sexed_text_edit"
                        '(:text "(defun foo () (+ 1 2))"
                          :operation "replace"
                          :selector (("head" . "+"))
                          :newtext "(quote 1 2)"))))
          (is (search "defun foo"
                      (execute-tool
                       "sexed_text_outline"
                       '(:text "(defun foo () (+ 1 2))"
                         :head ""
                         :max-depth 0))))
          (let ((batch-result
                  (clawmacs::lisp-data-read
                   (execute-tool
                    "sexed_text_edits"
                    '(:text "(defun foo () (+ 1 2))"
                      :edits (((:operation . "replace")
                               (:selector . ((:head . "+")))
                               (:newtext . "(list 1 2)"))))))))
            (is (= 1 (getf batch-result :edits-applied)))
            (is (string= "(defun foo () (list 1 2))"
                         (getf batch-result :text))))
          (is (search "defun foo"
                      (execute-tool
                       "sexed_project_outline"
                       '(:project "sexed-tool"
                         :path "source.lisp"
                        :head "defun"))))
          (is (string= "(defun foo () (+ 1 2))"
                       (execute-tool
                        "sexed_project_read"
                        '(:project "sexed-tool"
                          :path "source.lisp"))))
          (let ((result
                  (clawmacs::lisp-data-read
                   (execute-tool
                    "sexed_project_edit"
                    '(:project "sexed-tool"
                      :path "source.lisp"
                      :operation "replace"
                      :selector (("head" . "+"))
                      :new-text "(list 1 2)")))))
            (is (eq :ok (getf result :status)))
            (is-true (getf result :balanced)))
          (is (string= "(defun foo () (list 1 2))"
                       (project-read-file "sexed-tool" "source.lisp")))
          (let ((write-result
                  (clawmacs::lisp-data-read
                   (execute-tool
                    "sexed_project_write"
                    '(:project "sexed-tool"
                      :path "generated.lisp"
                      :content "(defun generated () :ok)")))))
            (is (eq :ok (getf write-result :status)))
            (is-true (getf write-result :balanced)))
          (is (string= "(defun generated () :ok)"
                       (project-read-file "sexed-tool" "generated.lisp")))
          (let ((project-batch-result
                  (clawmacs::lisp-data-read
                   (execute-tool
                    "sexed_project_edits"
                    '(:project "sexed-tool"
                      :path "generated.lisp"
                      :edits (((:operation . "replace")
                               (:selector . ((:head . "defun")
                                             (:name . "generated")))
                               (:newtext . "(defun generated () :batch)"))))))))
            (is (eq :ok (getf project-batch-result :status)))
            (is (= 1 (getf project-batch-result :edits-applied)))
            (is-true (getf project-batch-result :balanced)))
          (is (string= "(defun generated () :batch)"
                       (project-read-file "sexed-tool" "generated.lisp")))
          (let ((file-write-result
                  (clawmacs::lisp-data-read
                   (execute-tool
                    "sexed_file_write"
                    '(:path "local.lisp"
                      :content "(defun local () :ok)")))))
            (is (eq :ok (getf file-write-result :status)))
            (is-true (getf file-write-result :balanced)))
          (let ((file-batch-result
                  (clawmacs::lisp-data-read
                   (execute-tool
                    "sexed_file_edits"
                    '(:path "local.lisp"
                      :edits (((:operation . "replace")
                               (:selector . ((:head . "defun")
                                             (:name . "local")))
                               (:newtext . "(defun local () :batch)"))))))))
            (is (eq :ok (getf file-batch-result :status)))
            (is (= 1 (getf file-batch-result :edits-applied)))
            (is-true (getf file-batch-result :balanced)))
          (is (string= "(defun local () :batch)"
                       (execute-tool "sexed_file_read"
                                     '(:path "local.lisp")))))))))
