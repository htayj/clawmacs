(in-package :clawmacs/tests)
(in-suite projects-suite)

(defun project-test-directory ()
  "Return a fresh temporary directory for project tests."
  (let ((dir (merge-pathnames
              (format nil "clawmacs-project-tests-~D-~D-~36R/"
                      (get-universal-time)
                      (get-internal-real-time)
                      (random (expt 36 8)))
              #P"/tmp/")))
    (ensure-directories-exist (merge-pathnames #P".keep" dir))
    (truename dir)))

(defun write-project-test-file (path text)
  "Write TEXT to PATH for project tests."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text stream)))

(defmacro with-project-test-state ((root-var definitions-var) &body body)
  `(let* ((base (project-test-directory))
          (,root-var (merge-pathnames #P"root/" base))
          (,definitions-var (merge-pathnames #P"defs/" base))
          (*project-registry* (make-hash-table :test #'equal))
          (*project-definitions-directory* ,definitions-var)
          (clawmacs::*project-definitions-loaded-p* nil)
          (clawmacs::*buffer-ring* nil)
          (clawmacs::*buffer-counter* 0))
     (ensure-directories-exist (merge-pathnames #P".keep" ,root-var))
     (ensure-directories-exist (merge-pathnames #P".keep" ,definitions-var))
     ,@body))

(test define-project-registers-programmatic-project
  "DEFINE-PROJECT registers a path-backed project without writing a manifest."
  (with-project-test-state (root definitions)
    (let ((project (define-project "Alpha"
                     :root root
                     :description "A test project")))
      (is (eq project (find-project "alpha")))
      (is (string= "Alpha" (project-name project)))
      (is (string= "A test project" (project-description project)))
      (is (eq :programmatic (project-source project)))
      (is (string= (namestring root) (namestring (project-root project))))
      (is (null (directory (merge-pathnames "*.project" definitions)))))))

(test create-project-persists-inert-manifest-and-loads-it
  "CREATE-PROJECT writes a data manifest that LOAD-PROJECT-DEFINITIONS autoloads."
  (with-project-test-state (root definitions)
    (let ((project (create-project "Persisted"
                     :root root
                     :description "from manifest")))
      (is (probe-file (clawmacs::project-manifest-path "Persisted"
                                                       definitions)))
      (is (eq :manifest (project-source project))))
    (clrhash *project-registry*)
    (load-project-definitions)
    (let ((loaded (find-project "persisted")))
      (is (not (null loaded)))
      (is (eq :manifest (project-source loaded)))
      (is (string= "from manifest" (project-description loaded)))
      (is (not (null (find-project "config")))))))

(test load-project-definitions-preserves-init-defined-projects
  "Programmatic definitions win over same-named manifest projects."
  (with-project-test-state (root definitions)
    (let ((other-root (merge-pathnames #P"other/" root)))
      (ensure-directories-exist (merge-pathnames #P".keep" other-root))
      (define-project "same" :root root :description "from init")
      (write-project-test-file
       (clawmacs::project-manifest-path "same" definitions)
       (format nil "(:name \"same\" :root ~S :description \"from manifest\")"
               (namestring other-root)))
      (load-project-definitions)
      (let ((project (find-project "same")))
        (is (string= "from init" (project-description project)))
        (is (string= (namestring root) (namestring (project-root project))))))))

(test config-project-uses-user-init-directory
  "The user init directory is exposed as the config project."
  (with-project-test-state (root definitions)
    (let ((clawmacs::*user-init-directory* root))
      (load-project-definitions)
      (let ((config (find-project "config")))
        (is (not (null config)))
        (is (eq :builtin (project-source config)))
        (is (string= (namestring root)
                     (namestring (project-root config))))))))

(test project-paths-cannot-escape-root
  "Project resource paths are relative and reject absolute or parent escapes."
  (with-project-test-state (root definitions)
    (define-project "safe" :root root)
    (signals error
      (project-read-file "safe" "../outside.lisp"))
    (signals error
      (project-read-file "safe" "/tmp/outside.lisp"))
    (signals error
      (project-create-file "safe" "dir/" :content ""))))

(test project-file-list-read-create-save-and-search
  "Project APIs cover normal persistent file workflows."
  (with-project-test-state (root definitions)
    (define-project "files" :root root)
    (project-create-file "files" "src/sample.lisp"
                         :content "(defun alpha () :old)")
    (project-create-file "files" "README.md"
                         :content "alpha docs")
    (is (equal '("README.md" "src/sample.lisp")
               (project-list-files "files")))
    (is (search ":old" (project-read-file "files" "src/sample.lisp")))
    (let ((hits (project-search "files" "alpha")))
      (is (= 2 (length hits)))
      (is (member "src/sample.lisp" hits
                  :key (lambda (hit) (getf hit :path))
                  :test #'string=)))
    (let ((summary (project-save-file "files"
                                      "src/sample.lisp"
                                      "(defun alpha () :new)")))
      (is (eq :ok (getf summary :status)))
      (is (search ":new" (project-read-file "files" "src/sample.lisp"))))))

(test project-traversal-ignores-generated-noise
  "Project listing and search skip backup, debug, and binary artifacts."
  (with-project-test-state (root definitions)
    (define-project "quiet" :root root)
    (project-save-file "quiet" "src/live.lisp" "(defun target () :ok)")
    (project-save-file "quiet" "src/live.lisp~" "target backup")
    (project-save-file "quiet" "debug.log" "target log")
    (project-save-file "quiet" "debug-prompt.log" "target prompt log")
    (project-save-file "quiet" "src/cache.fasl" "target binary")
    (is (equal '("src/live.lisp")
               (project-list-files "quiet")))
    (let ((hits (project-search "quiet" "target")))
      (is (= 1 (length hits)))
      (is (string= "src/live.lisp" (getf (first hits) :path))))))

(test project-traversal-hides-bulk-trees-unless-requested
  "Default project traversal keeps bulky reference trees out of agent context."
  (with-project-test-state (root definitions)
    (define-project "bulk" :root root)
    (project-save-file "bulk" "src/live.lisp" "(defun target () :source)")
    (project-save-file "bulk" "vendor/spec.lisp" "(defun target () :vendor)")
    (project-save-file "bulk" "reference/example.lisp" "(defun target () :reference)")
    (project-save-file "bulk" ".cache/generated.lisp" "(defun target () :cache)")
    (is (equal '("src/live.lisp")
               (project-list-files "bulk")))
    (is (equal '("reference/example.lisp"
                 "src/live.lisp"
                 "vendor/spec.lisp")
               (project-list-files "bulk" :include-bulk t)))
    (is (member ".cache/generated.lisp"
                (project-list-files "bulk"
                                    :include-bulk t
                                    :include-ignored t)
                :test #'string=))
    (let ((hits (project-search "bulk" "target")))
      (is (= 1 (length hits)))
      (is (string= "src/live.lisp" (getf (first hits) :path))))
    (let ((hits (project-search "bulk" "target" :include-bulk t)))
      (is (= 3 (length hits)))
      (is (member "vendor/spec.lisp" hits
                  :key (lambda (hit) (getf hit :path))
                  :test #'string=)))))

(test project-traversal-respects-root-gitignore
  "Default project traversal skips basic root .gitignore matches."
  (with-project-test-state (root definitions)
    (define-project "gitignored" :root root)
    (project-save-file "gitignored" ".gitignore"
                       (format nil "draft.md~%session-*.md~%ignored-dir/~%docs/generated.md~%"))
    (project-save-file "gitignored" "src/live.lisp" "(defun target () :source)")
    (project-save-file "gitignored" "draft.md" "target draft")
    (project-save-file "gitignored" "session-one.md" "target session")
    (project-save-file "gitignored" "ignored-dir/notes.txt" "target ignored")
    (project-save-file "gitignored" "docs/generated.md" "target generated")
    (is (equal '(".gitignore" "src/live.lisp")
               (project-list-files "gitignored")))
    (let ((hits (project-search "gitignored" "target")))
      (is (= 1 (length hits)))
      (is (string= "src/live.lisp" (getf (first hits) :path))))
    (let ((files (project-list-files "gitignored" :include-ignored t)))
      (is (member "draft.md" files :test #'string=))
      (is (member "session-one.md" files :test #'string=))
      (is (member "ignored-dir/notes.txt" files :test #'string=))
      (is (member "docs/generated.md" files :test #'string=)))))

(test project-traversal-default-limits-are-customizable
  "Traversal limits are special variables that init.lisp can override."
  (with-project-test-state (root definitions)
    (define-project "limits" :root root)
    (project-save-file "limits" "a.lisp" "(defun alpha-a () :a)")
    (project-save-file "limits" "b.lisp" "(defun alpha-b () :b)")
    (let ((*project-list-file-limit* 1)
          (*project-search-result-limit* 1)
          (*project-outline-file-limit* 1))
      (is (= 1 (length (project-list-files "limits"))))
      (let ((summary (project-search-to-string "limits" "alpha")))
        (is (search "limited to 1 matches" summary)))
      (let ((outline (project-outline-to-string "limits")))
        (flet ((occurrences (needle haystack)
                 (loop :with count := 0
                       :for start := (search needle haystack)
                         :then (search needle haystack :start2 (1+ start))
                       :while start
                       :do (incf count)
                       :finally (return count))))
          (is (= 1 (occurrences ";;; " outline))))))))

(test project-read-file-lines-returns-bounded-numbered-slices
  "PROJECT-READ-FILE-LINES reads targeted source slices for agents."
  (with-project-test-state (root definitions)
    (define-project "lines" :root root)
    (project-save-file "lines" "notes.txt"
                       (format nil "one~%two~%three~%four~%five~%six"))
    (let ((slice (project-read-file-lines "lines" "notes.txt"
                                          :line 4
                                          :context 1
                                          :max-lines 3)))
      (is (search "notes.txt: lines 3-5 of 6" slice))
      (is (search "3: three" slice))
      (is (search "4: four" slice))
      (is (search "5: five" slice))
      (is (not (search "2: two" slice)))
      (is (not (search "6: six" slice))))
    (let ((slice (project-read-file-lines "lines" "notes.txt" 2 4)))
      (is (search "notes.txt: lines 2-4 of 6" slice))
      (is (search "2: two" slice))
      (is (search "4: four" slice))
      (is (not (search "5: five" slice))))))

(test project-replace-text-replaces-exact-text
  "PROJECT-REPLACE-TEXT gives agents a small exact text edit primitive."
  (with-project-test-state (root definitions)
    (define-project "replace" :root root)
    (project-save-file "replace" "notes.txt" "alpha beta beta")
    (let ((summary (project-replace-text "replace" "notes.txt"
                                         "beta"
                                         "gamma")))
      (is (eq :ok (getf summary :status)))
      (is (= 1 (getf summary :replacements)))
      (is (string= "alpha gamma beta"
                   (project-read-file "replace" "notes.txt"))))
    (let ((summary (project-replace-text "replace" "notes.txt"
                                         "beta"
                                         "delta"
                                         :count nil)))
      (is (= 1 (getf summary :replacements)))
      (is (string= "alpha gamma delta"
                   (project-read-file "replace" "notes.txt"))))
    (signals error
      (project-replace-text "replace" "notes.txt" "missing" "value"))))

(test project-replace-text-between-replaces-marker-bounded-spans
  "PROJECT-REPLACE-TEXT-BETWEEN lets agents avoid fragile substring surgery."
  (with-project-test-state (root definitions)
    (define-project "replace-between" :root root)
    (project-save-file "replace-between"
                       "notes.txt"
                       (format nil "alpha~%START remove me~%END~%omega"))
    (let ((summary (project-replace-text-between "replace-between"
                                                 "notes.txt"
                                                 "START"
                                                 "END"
                                                 "")))
      (is (eq :ok (getf summary :status)))
      (is (plusp (getf summary :bytes-replaced)))
      (is (string= (format nil "alpha~%END~%omega")
                   (project-read-file "replace-between" "notes.txt"))))
    (project-replace-text-between "replace-between"
                                  "notes.txt"
                                  "alpha"
                                  "omega"
                                  "middle"
                                  :include-start-marker nil
                                  :include-end-marker nil)
    (is (string= "alphamiddleomega"
                 (project-read-file "replace-between" "notes.txt")))
    (signals error
      (project-replace-text-between "replace-between"
                                    "notes.txt"
                                    "missing"
                                    "omega"
                                    ""))))

(test stage-project-replace-text-composes-with-change-sets
  "STAGE-PROJECT-REPLACE-TEXT edits staged text without touching the file."
  (with-project-test-state (root definitions)
    (define-project "stage-replace" :root root)
    (project-save-file "stage-replace" "notes.txt" "alpha beta")
    (let ((change-set (begin-change-set :name "replace-text")))
      (let ((summary (stage-project-replace-text "stage-replace"
                                                 "notes.txt"
                                                 "beta"
                                                 "gamma"
                                                 :change-set change-set)))
        (is (eq :staged (getf summary :status)))
        (is (= 1 (getf summary :replacements))))
      (is (string= "alpha beta"
                   (project-read-file "stage-replace" "notes.txt")))
      (is (string= "alpha gamma"
                   (change-set-project-file-text "stage-replace"
                                                 "notes.txt"
                                                 change-set))))))

(test project-open-file-creates-editable-buffer-and-save-buffer-persists
  "PROJECT-OPEN-FILE creates a file buffer whose text can be saved back."
  (with-project-test-state (root definitions)
    (define-project "edit" :root root)
    (project-create-file "edit" "notes.lisp" :content "(note old)")
    (let ((buffer (project-open-file "edit" "notes.lisp")))
      (is (file-buffer-p buffer))
      (is (string= "edit" (buffer-project-name buffer)))
      (is (string= "notes.lisp" (buffer-resource-path buffer)))
      (is (string= "(note old)" (file-buffer-text buffer)))
      (setf (file-buffer-text buffer) "(note new)")
      (is (buffer-dirty-p buffer))
      (let ((summary (project-save-buffer buffer)))
        (is (eq :ok (getf summary :status))))
      (is-false (buffer-dirty-p buffer))
      (is (string= "(note new)" (project-read-file "edit" "notes.lisp"))))))

(test direct-project-writes-refresh-open-file-buffer
  "Direct project writes keep already-open file buffers coherent."
  (with-project-test-state (root definitions)
    (define-project "sync" :root root)
    (project-create-file "sync" "notes.lisp" :content "(note old)")
    (let ((buffer (project-open-file "sync" "notes.lisp")))
      (setf (file-buffer-text buffer) "(note unsaved)")
      (is (file-buffer-dirty-p buffer))
      (project-create-file "sync" "notes.lisp"
                           :content "(note fresh)"
                           :if-exists :supersede)
      (is (eq buffer (project-open-file "sync" "notes.lisp")))
      (is (string= "(note fresh)" (file-buffer-text buffer)))
      (is (string= "(note fresh)" (buffer-original-text buffer)))
      (is-false (file-buffer-dirty-p buffer))
      (setf (file-buffer-text buffer)
            (concatenate 'string (file-buffer-text buffer)
                         (string #\Newline)
                         ";; one append"))
      (project-save-buffer buffer)
      (is-false (file-buffer-dirty-p buffer))
      (is (string= "(note fresh)
;; one append"
                   (project-read-file "sync" "notes.lisp"))))))

(test save-session-command-saves-project-file-buffers
  "C-x C-s behavior saves project file buffers instead of sessions."
  (with-project-test-state (root definitions)
    (clawmacs::init-default-keymap)
    (define-project "save" :root root)
    (project-create-file "save" "draft.txt" :content "old")
    (let ((buffer (project-open-file "save" "draft.txt")))
      (setf (file-buffer-text buffer) "new")
      (clawmacs::save-session-command buffer)
      (is (string= "new" (project-read-file "save" "draft.txt")))
      (is-false (buffer-dirty-p buffer))
      (let ((notice (message-prev (buffer-input-message buffer))))
        (is (not (null notice)))
        (is (search "Saved save:draft.txt" (message-text notice)))))))

(test define-project-records-checks-systems-and-reload-function
  "Project definitions can expose validation and reload metadata."
  (with-project-test-state (root definitions)
    (let ((project (define-project "meta"
                     :root root
                     :systems '("clawmacs")
                     :check-functions
                     (list (lambda (project)
                             (list :checked (project-name project))))
                     :reload-function
                     (lambda (project)
                       (list :reloaded (project-name project))))))
      (is (equal '("clawmacs") (project-systems project)))
      (is (= 1 (length (project-check-functions project))))
      (is (functionp (project-reload-function project)))
      (let ((checks (run-project-checks "meta")))
        (is (eq :passed (getf (first checks) :status)))
        (is (equal '(:checked "meta") (getf (first checks) :result))))
      (let ((reloads (reload-project-system "meta")))
        (is (eq :ok (getf (first reloads) :status)))
        (is (equal '(:reloaded "meta") (getf (first reloads) :result)))))
    (is (not (null definitions)))))

(test change-set-stages-applies-and-reverts-project-file
  "Change sets stage project writes without touching files until apply."
  (with-project-test-state (root definitions)
    (define-project "tx" :root root)
    (project-create-file "tx" "src/sample.lisp"
                         :content "(defun sample () :old)")
    (let ((change-set (begin-change-set :name "sample-edit")))
      (stage-project-file "tx" "src/sample.lisp" "(defun sample () :new)"
                          :change-set change-set)
      (is (search "+(defun sample () :new)"
                  (change-set-diff-to-string change-set)))
      (is (string= "(defun sample () :old)"
                   (project-read-file "tx" "src/sample.lisp")))
      (is (string= "(defun sample () :new)"
                   (change-set-project-file-text "tx" "src/sample.lisp"
                                                 change-set)))
      (apply-change-set change-set)
      (is (eq :applied (change-set-status change-set)))
      (is (string= "(defun sample () :new)"
                   (project-read-file "tx" "src/sample.lisp")))
      (revert-change-set change-set)
      (is (eq :reverted (change-set-status change-set)))
      (is (string= "(defun sample () :old)"
                   (project-read-file "tx" "src/sample.lisp"))))
    (is (not (null definitions)))))

(test change-set-delete-and-rename-restore-original-state
  "Delete and rename entries can be applied and reverted."
  (with-project-test-state (root definitions)
    (define-project "moves" :root root)
    (project-create-file "moves" "old.lisp" :content "(old)")
    (project-create-file "moves" "delete.lisp" :content "(delete)")
    (let ((change-set (begin-change-set :name "moves")))
      (stage-project-rename "moves" "old.lisp" "new.lisp"
                            :change-set change-set)
      (stage-project-delete "moves" "delete.lisp"
                            :change-set change-set)
      (apply-change-set change-set)
      (is (string= "(old)" (project-read-file "moves" "new.lisp")))
      (signals error
        (project-read-file "moves" "old.lisp"))
      (signals error
        (project-read-file "moves" "delete.lisp"))
      (revert-change-set change-set)
      (is (string= "(old)" (project-read-file "moves" "old.lisp")))
      (is (string= "(delete)" (project-read-file "moves" "delete.lisp")))
      (signals error
        (project-read-file "moves" "new.lisp")))
    (is (not (null definitions)))))

(test project-code-intelligence-finds-definitions-and-packages
  "Project intelligence helpers summarize Lisp files without direct file tools."
  (with-project-test-state (root definitions)
    (define-project "intel" :root root)
    (project-create-file
     "intel"
     "src/example.lisp"
     :content "(defpackage :example
  (:use :cl))

(in-package :example)

(defun alpha ()
  :ok)

(defmacro with-alpha (&body body)
  `(progn ,@body))")
    (is (search "defun alpha"
                (project-outline-to-string "intel"
                                           :path "src/example.lisp"
                                           :max-depth 0)))
    (is (search "src/example.lisp:defun alpha"
                (project-find-definitions-to-string "intel"
                                                    :name "alpha")))
    (is (search "defpackage"
                (project-package-map-to-string "intel")))
    (is (search "(defun alpha ()"
                (project-describe-definition-to-string "intel" "alpha")))
    (is (search "with-alpha"
                (project-find-references-to-string "intel" "with-alpha")))
    (is (not (null definitions)))))
