(in-package :clawmacs/tests)

(in-suite package-manager-suite)

(defvar *package-entrypoint-load-count* 0
  "Tracks how many times the test package entrypoint was loaded.")

(defvar *package-init-continued* nil
  "Tracks whether load-user-init-file continued after a package warning.")

(defun temp-package-test-directory (label)
  (make-pathname :directory (list :absolute "tmp"
                                  (format nil "clawmacs-package-tests-~A-~A"
                                          label
                                          (gensym)))))

(defmacro with-packages-directory-override ((path) &body body)
  `(let ((clawmacs::*packages-directory* (uiop:ensure-directory-pathname ,path))
         (clawmacs::*loaded-packages* (make-hash-table :test #'equal)))
     ,@body))

(defun write-test-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(defun run-git-command (directory &rest args)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (append (list "git") args)
                        :directory directory
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (unless (zerop exit-code)
      (error "git ~{~A~^ ~} failed in ~A:~%~A~@[~%~A~]"
             args
             (namestring directory)
             stdout
             stderr))
    stdout))

(defun initialize-test-git-repo (repo-root)
  (run-git-command repo-root "init")
  (run-git-command repo-root "config" "user.name" "Clawmacs Test")
  (run-git-command repo-root "config" "user.email" "clawmacs-tests@example.com"))

(defun commit-test-git-repo (repo-root)
  (run-git-command repo-root "add" ".")
  (run-git-command repo-root "commit" "-m" "Add test package"))

(defun make-package-repo (&key label manifest entrypoint-content)
  (let ((repo-root (uiop:ensure-directory-pathname
                    (temp-package-test-directory (or label "repo")))))
    (ensure-directories-exist (merge-pathnames #P".keep" repo-root))
    (initialize-test-git-repo repo-root)
    (when manifest
      (write-test-file (merge-pathnames "manifest.lisp" repo-root) manifest))
    (when entrypoint-content
      (write-test-file (merge-pathnames "test-package.lisp" repo-root) entrypoint-content))
    (commit-test-git-repo repo-root)
    repo-root))

(test clawmacs-use-package-clones-and-loads-local-git-repo
  "A local git repo is cloned, read via manifest.lisp, and loaded."
  (let* ((*package-entrypoint-load-count* 0)
         (source-repo
           (make-package-repo
            :label "happy"
            :manifest "(:name \"test-package\" :entrypoint \"test-package.lisp\")"
            :entrypoint-content "(incf clawmacs/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "install-root")))
    (with-packages-directory-override (packages-root)
      (is (clawmacs:clawmacs-use-package :src-type :git
                                         :repo (namestring source-repo)))
      (is (= 1 *package-entrypoint-load-count*))
      (is (probe-file
           (clawmacs::package-install-directory :git (namestring source-repo)))))))

(test clawmacs-use-package-loads-once-per-session
  "Repeated calls do not reload an already-loaded package."
  (let* ((*package-entrypoint-load-count* 0)
         (source-repo
           (make-package-repo
            :label "idempotent"
            :manifest "(:name test-package :entrypoint \"test-package.lisp\")"
            :entrypoint-content "(incf clawmacs/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "install-root")))
    (with-packages-directory-override (packages-root)
      (is (clawmacs:clawmacs-use-package :src-type :git
                                         :repo (namestring source-repo)))
      (is (clawmacs:clawmacs-use-package :src-type :git
                                         :repo (namestring source-repo)))
      (is (= 1 *package-entrypoint-load-count*)))))

(test clawmacs-use-package-honors-packages-directory-override
  "The install root follows *packages-directory*."
  (let* ((*package-entrypoint-load-count* 0)
         (source-repo
           (make-package-repo
            :label "override"
            :manifest "(:name \"override-package\" :entrypoint \"test-package.lisp\")"
            :entrypoint-content "(incf clawmacs/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "custom-root")))
    (with-packages-directory-override (packages-root)
      (is (clawmacs:clawmacs-use-package :src-type :git
                                         :repo (namestring source-repo)))
      (let ((install-dir
              (clawmacs::package-install-directory :git (namestring source-repo))))
        (is (not (null (probe-file install-dir))))
        (is (equal (namestring (uiop:ensure-directory-pathname packages-root))
                   (subseq (namestring install-dir)
                           0
                           (length (namestring (uiop:ensure-directory-pathname packages-root))))))))))

(test clawmacs-use-package-rejects-unsupported-source-types
  "Unsupported source types return NIL without signaling."
  (let ((packages-root (temp-package-test-directory "source-type")))
    (with-packages-directory-override (packages-root)
      (is (null (clawmacs:clawmacs-use-package :src-type :github
                                               :repo "owner/repo"))))))

(test clawmacs-use-package-fails-when-manifest-is-missing
  "Packages without manifest.lisp return NIL."
  (let* ((source-repo
           (make-package-repo
            :label "missing-manifest"
            :entrypoint-content "(incf clawmacs/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "missing-manifest-root")))
    (with-packages-directory-override (packages-root)
      (is (null (clawmacs:clawmacs-use-package :src-type :git
                                               :repo (namestring source-repo)))))))

(test clawmacs-use-package-fails-when-manifest-is-malformed
  "Packages with a non-plist manifest return NIL."
  (let* ((source-repo
           (make-package-repo
            :label "bad-manifest"
            :manifest "(this is not a plist)"
            :entrypoint-content "(incf clawmacs/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "bad-manifest-root")))
    (with-packages-directory-override (packages-root)
      (is (null (clawmacs:clawmacs-use-package :src-type :git
                                               :repo (namestring source-repo)))))))

(test clawmacs-use-package-fails-when-entrypoint-is-missing
  "Manifest entrypoints must exist inside the repo."
  (let* ((source-repo
           (make-package-repo
            :label "missing-entrypoint"
            :manifest "(:name \"missing-entry\" :entrypoint \"missing.lisp\")"))
         (packages-root (temp-package-test-directory "missing-entrypoint-root")))
    (with-packages-directory-override (packages-root)
      (is (null (clawmacs:clawmacs-use-package :src-type :git
                                               :repo (namestring source-repo)))))))

(test load-user-init-file-continues-after-package-warning
  "Package loader failures do not abort later init forms."
  (let* ((packages-root (temp-package-test-directory "init-root"))
         (init-root (uiop:ensure-directory-pathname
                     (temp-package-test-directory "init-file")))
         (init-path (merge-pathnames "init.lisp" init-root)))
    (ensure-directories-exist (merge-pathnames #P".keep" init-root))
    (write-test-file init-path
                     (format nil "(setf clawmacs/tests::*package-init-continued* nil)~%
(clawmacs-use-package :src-type :git :repo ~S)~%
(setf clawmacs/tests::*package-init-continued* t)~%"
                             "/tmp/does-not-exist-clawmacs-package"))
    (let ((clawmacs::*user-init-file* init-path)
          (clawmacs::*packages-directory* packages-root)
          (clawmacs::*inhibit-user-init* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (*package-init-continued* nil))
      (clawmacs:load-user-init-file)
      (is (not (null *package-init-continued*))))))
