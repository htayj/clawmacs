(in-package :rplaca/tests)

(in-suite git-package-suite)

(defun git-package-test-directory (label)
  "Return a fresh temporary directory for git package tests."
  (let ((dir (merge-pathnames
              (format nil "rplaca-git-package-tests-~A-~36R-~36R/"
                      label
                      (get-universal-time)
                      (get-internal-real-time))
              #P"/tmp/")))
    (ensure-directories-exist (merge-pathnames #P".keep" dir))
    dir))

(defmacro with-git-package-state (&body body)
  "Run BODY with isolated package, project, and tool registries."
  `(let* ((root (git-package-test-directory "config"))
          (rplaca::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (rplaca::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (rplaca::*tool-table* (make-hash-table :test #'equal))
          (rplaca::*project-registry* (make-hash-table :test #'equal))
          (rplaca::*project-definitions-loaded-p* nil)
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels* (default-package-test-channels))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil))
     ,@body))

(defun make-git-package-repo (&optional (label "repo"))
  "Create a local git repo with one initial commit."
  (let ((repo (git-package-test-directory label)))
    (initialize-test-git-repo repo)
    (write-test-file (merge-pathnames "README.md" repo)
                     "hello from git package tests
")
    (run-git-command repo "add" "README.md")
    (run-git-command repo "commit" "-m" "Initial commit")
    repo))

(defun load-test-git-package ()
  "Enable and load the bundled git package."
  (set-package-enablement-scope "git" :global)
  (load-active-packages))

(defun git-package-tool-result (tool-name args)
  "Execute TOOL-NAME with ARGS and read its Lisp data result."
  (nth-value 0
    (rplaca::lisp-data-read
     (rplaca:execute-tool tool-name args))))

(test git-package-registers-agent-tools-and-prompt
  "Enabling git exposes package-scoped provider tools and prompt guidance."
  (with-git-package-state
    (load-test-git-package)
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<))
           (prompt (render-package-prompt-sections)))
      (dolist (name '("git_add" "git_branch" "git_commit" "git_diff"
                      "git_log" "git_push" "git_remote" "git_show"
                      "git_status"))
        (is (member name tool-names :test #'string=)))
      (is (search "Git workflow with git" prompt))
      (is (search "git_status" prompt))
      (is (search "git_diff" prompt))
      (is (search "Avoid `lisp_eval`" prompt))
      (is (search "force-push" prompt)))))

(test git-package-read-only-tools-return-structured-results
  "Read-only git tools operate on a RPLACA project and return Lisp data."
  (with-git-package-state
    (let ((repo (make-git-package-repo "readonly")))
      (define-project "git-readonly" :root repo)
      (load-test-git-package)
      (write-test-file (merge-pathnames "README.md" repo)
                       "hello from git package tests
new line
")
      (run-git-command repo "remote" "add" "origin" "/tmp/rplaca-git-package-origin.git")
      (let ((status (git-package-tool-result
                     "git_status"
                     '(:project "git-readonly" :branch t))))
        (is-true (getf status :ok))
        (is (search "README.md" (getf status :stdout)))
        (is (search "## " (getf status :stdout))))
      (let ((log (git-package-tool-result
                  "git_log"
                  '(:project "git-readonly" :limit 1))))
        (is-true (getf log :ok))
        (is (search "Initial commit" (getf log :stdout))))
      (let ((diff (git-package-tool-result
                   "git_diff"
                   '(:project "git-readonly" :path "README.md"))))
        (is-true (getf diff :ok))
        (is (search "+new line" (getf diff :stdout))))
      (let ((show (git-package-tool-result
                   "git_show"
                   '(:project "git-readonly" :revision "HEAD" :stat t))))
        (is-true (getf show :ok))
        (is (search "Initial commit" (getf show :stdout))))
      (let ((branch (git-package-tool-result
                     "git_branch"
                     '(:project "git-readonly"))))
        (is-true (getf branch :ok))
        (is (search "*" (getf branch :stdout))))
      (let ((remote (git-package-tool-result
                     "git_remote"
                     '(:project "git-readonly" :verbose t))))
        (is-true (getf remote :ok))
        (is (search "origin" (getf remote :stdout)))))))

(test git-package-mutating-tools-stage-commit-and-push
  "Git tools directly support add, commit, and local bare-remote push."
  (with-git-package-state
    (let* ((repo (make-git-package-repo "mutating"))
           (remote (git-package-test-directory "remote")))
      (run-git-command remote "init" "--bare")
      (define-project "git-mutating" :root repo)
      (load-test-git-package)
      (run-git-command repo "remote" "add" "origin" (namestring remote))
      (write-test-file (merge-pathnames "new.txt" repo)
                       "new file
")
      (let ((add (git-package-tool-result
                  "git_add"
                  '(:project "git-mutating" :paths #("new.txt")))))
        (is-true (getf add :ok)))
      (let ((status (git-package-tool-result
                     "git_status"
                     '(:project "git-mutating"))))
        (is-true (getf status :ok))
        (is (search "A  new.txt" (getf status :stdout))))
      (let ((commit (git-package-tool-result
                     "git_commit"
                     '(:project "git-mutating" :message "Add new file"))))
        (is-true (getf commit :ok))
        (is (search "Add new file" (getf commit :stdout))))
      (let* ((branch (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (run-git-command repo
                                                   "rev-parse"
                                                   "--abbrev-ref"
                                                   "HEAD")))
             (push (git-package-tool-result
                    "git_push"
                    `(:project "git-mutating"
                      :remote "origin"
                      :branch ,branch))))
        (is-true (getf push :ok))
        (is (search "Add new file"
                    (run-git-command remote "log" "--oneline" branch)))))))

(test git-package-rejects-unsafe-inputs-and-returns-git-failures
  "Tool validation rejects unsafe args while git command failures are data."
  (with-git-package-state
    (let ((repo (make-git-package-repo "safety")))
      (define-project "git-safety" :root repo)
      (load-test-git-package)
      (signals error
        (rplaca:execute-tool
         "git_add"
         '(:project "git-safety" :paths #("../outside"))))
      (signals error
        (rplaca:execute-tool
         "git_diff"
         '(:project "git-safety" :path "/tmp/outside")))
      (signals error
        (rplaca:execute-tool
         "git_diff"
         '(:project "git-safety" :path "*.lisp")))
      (signals error
        (rplaca:execute-tool
         "git_show"
         '(:project "git-safety" :revision "--help")))
      (signals error
        (rplaca:execute-tool
         "git_push"
         '(:project "git-safety" :branch "main")))
      (let ((missing (git-package-tool-result
                      "git_log"
                      '(:project "git-safety"
                        :revision "missing-ref"
                        :limit 1))))
        (is-false (getf missing :ok))
        (is (< 0 (getf missing :exit-code)))
        (is (search "missing-ref" (getf missing :stderr)))))))
