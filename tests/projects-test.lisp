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
