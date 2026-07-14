(in-package :clawmacs)

(defvar *git-tool-default-max-chars* 12000
  "Default maximum characters returned from git stdout or stderr.")

(defun git-tool-blank-string-p (value)
  "Return true when VALUE is NIL or only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun git-tool-string (value field-name &key allow-nil)
  "Normalize VALUE as a string argument named FIELD-NAME."
  (cond
    ((null value)
     (if allow-nil
         nil
         (error "~A is required." field-name)))
    ((stringp value)
     value)
    (t
     (error "~A must be a string, got ~S." field-name value))))

(defun git-tool-positive-integer (value field-name default)
  "Return VALUE as a positive integer, or DEFAULT when VALUE is NIL."
  (cond
    ((null value) default)
    ((and (integerp value) (plusp value)) value)
    (t
     (error "~A must be a positive integer, got ~S." field-name value))))

(defun git-tool-boolean-arg (args key default)
  "Return boolean argument KEY from ARGS, defaulting to DEFAULT when omitted."
  (multiple-value-bind (value supplied-p)
      (tool-argument-value args key)
    (if supplied-p
        (not (null value))
        default)))

(defun git-tool-whitespace-char-p (char)
  "Return true when CHAR is ASCII whitespace."
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun git-tool-safe-token (value field-name &key default)
  "Normalize VALUE as a git ref/remote token that cannot be parsed as an option."
  (let ((token (if value
                   (git-tool-string value field-name)
                   default)))
    (when (git-tool-blank-string-p token)
      (return-from git-tool-safe-token nil))
    (when (alexandria:starts-with-subseq "-" token)
      (error "~A must not start with '-'." field-name))
    (when (find-if #'git-tool-whitespace-char-p token)
      (error "~A must not contain whitespace: ~S." field-name token))
    token))

(defun git-tool-normalize-path (path)
  "Return PATH as a safe repo-relative namestring."
  (let ((value (git-tool-string path "path")))
    (when (git-tool-blank-string-p value)
      (error "Git path must not be blank."))
    (when (alexandria:starts-with-subseq "-" value)
      (error "Git path must not start with '-': ~A" value))
    (project-resource-name value :allow-directory t)))

(defun git-tool-normalize-paths (paths)
  "Return PATHS as a non-empty list of safe repo-relative namestrings."
  (let ((values
          (cond
            ((null paths) nil)
            ((stringp paths) (list paths))
            ((vectorp paths) (coerce paths 'list))
            ((listp paths) paths)
            (t
             (error "paths must be an array of strings, got ~S." paths)))))
    (unless values
      (error "paths must contain at least one path."))
    (mapcar #'git-tool-normalize-path values)))

(defun git-tool-existing-directory (path context)
  "Return PATH as an existing directory pathname, or signal using CONTEXT."
  (let ((directory (uiop:ensure-directory-pathname path)))
    (or (uiop:directory-exists-p directory)
        (error "~A does not name an existing directory: ~A"
               context
               directory))))

(defun git-tool-repository-from-project (project-designator)
  "Return the root directory for PROJECT-DESIGNATOR."
  (project-root (ensure-project project-designator)))

(defun git-tool-repository-from-path (repository)
  "Resolve REPOSITORY and return it as an existing directory."
  (git-tool-existing-directory
   (lispi:resolve-tool-path repository)
   "Git repository"))

(defun git-tool-buffer-working-directory ()
  "Return the current tool buffer working directory, when it exists."
  (when (and (boundp '*current-tool-buffer*)
             *current-tool-buffer*
             (buffer-working-directory *current-tool-buffer*))
    (uiop:directory-exists-p
     (uiop:ensure-directory-pathname
      (buffer-working-directory *current-tool-buffer*)))))

(defun git-tool-working-directory-root ()
  "Return the effective tool working directory."
  (lispi:tool-working-directory-pathname))

(defun git-tool-resolve-repository (args)
  "Resolve ARGS to the directory where git should run."
  (let ((project (tool-arg args :project "project"))
        (repository (tool-arg args :repository "repository")))
    (cond
      ((not (git-tool-blank-string-p project))
       (git-tool-repository-from-project project))
      ((not (git-tool-blank-string-p repository))
       (git-tool-repository-from-path repository))
      ((git-tool-buffer-working-directory))
      (t
       (git-tool-existing-directory (git-tool-working-directory-root)
                                    "Git repository")))))

(defun git-tool-truncate (text max-chars)
  "Return TEXT truncated to MAX-CHARS, plus truncation metadata."
  (let* ((value (or text ""))
         (length (length value)))
    (if (> length max-chars)
        (values (subseq value 0 max-chars) t length)
        (values value nil length))))

(defun git-tool-command-label (argv)
  "Return a display string for git ARGV."
  (format nil "git ~{~A~^ ~}" argv))

(defun git-tool-run-raw (repository argv)
  "Run git with ARGV in REPOSITORY, returning stdout, stderr, and exit code."
  (uiop:run-program (cons "git" argv)
                    :directory repository
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun git-tool-run (repository argv &key max-chars)
  "Run git with ARGV and return a Lisp data result string."
  (let ((limit (git-tool-positive-integer max-chars
                                          "max-chars"
                                          *git-tool-default-max-chars*)))
    (multiple-value-bind (stdout stderr exit-code)
        (git-tool-run-raw repository argv)
      (multiple-value-bind (out out-truncated-p out-length)
          (git-tool-truncate stdout limit)
        (multiple-value-bind (err err-truncated-p err-length)
            (git-tool-truncate stderr limit)
          (lisp-data-string
           (list :ok (zerop exit-code)
                 :repository (namestring repository)
                 :command (git-tool-command-label argv)
                 :exit-code exit-code
                 :stdout out
                 :stderr err
                 :stdout-length out-length
                 :stderr-length err-length
                 :stdout-truncated out-truncated-p
                 :stderr-truncated err-truncated-p)))))))

(defun git-tool-status (args)
  "Return git status for a repository."
  (let* ((repository (git-tool-resolve-repository args))
         (short (git-tool-boolean-arg args :short t))
         (branch (git-tool-boolean-arg args :branch nil))
         (argv (append (list "status")
                       (when short (list "--short"))
                       (when branch (list "--branch")))))
    (git-tool-run repository argv)))

(defun git-tool-log (args)
  "Return git log output for a repository."
  (let* ((repository (git-tool-resolve-repository args))
         (limit (git-tool-positive-integer
                 (tool-arg args :limit "limit")
                 "limit"
                 20))
         (max-chars (git-tool-positive-integer
                     (tool-arg args :max-chars "max-chars")
                     "max-chars"
                     *git-tool-default-max-chars*))
         (revision (git-tool-safe-token
                    (tool-arg args :revision "revision")
                    "revision"))
         (path (tool-arg args :path "path"))
         (argv (append (list "log" "--oneline" "-n" (write-to-string limit))
                       (when revision (list revision))
                       (when path
                         (list "--" (git-tool-normalize-path path))))))
    (git-tool-run repository argv :max-chars max-chars)))

(defun git-tool-diff (args)
  "Return git diff output for a repository."
  (let* ((repository (git-tool-resolve-repository args))
         (max-chars (git-tool-positive-integer
                     (tool-arg args :max-chars "max-chars")
                     "max-chars"
                     *git-tool-default-max-chars*))
         (staged (git-tool-boolean-arg args :staged nil))
         (stat (git-tool-boolean-arg args :stat nil))
         (path (tool-arg args :path "path"))
         (argv (append (list "diff")
                       (when staged (list "--staged"))
                       (when stat (list "--stat"))
                       (when path
                         (list "--" (git-tool-normalize-path path))))))
    (git-tool-run repository argv :max-chars max-chars)))

(defun git-tool-show (args)
  "Return git show output for a repository."
  (let* ((repository (git-tool-resolve-repository args))
         (max-chars (git-tool-positive-integer
                     (tool-arg args :max-chars "max-chars")
                     "max-chars"
                     *git-tool-default-max-chars*))
         (revision (git-tool-safe-token
                    (tool-arg args :revision "revision")
                    "revision"
                    :default "HEAD"))
         (stat (git-tool-boolean-arg args :stat nil))
         (argv (append (list "show")
                       (when stat (list "--stat"))
                       (list revision))))
    (git-tool-run repository argv :max-chars max-chars)))

(defun git-tool-branch (args)
  "Return git branch output for a repository."
  (let* ((repository (git-tool-resolve-repository args))
         (all (git-tool-boolean-arg args :all nil))
         (argv (append (list "branch")
                       (when all (list "--all")))))
    (git-tool-run repository argv)))

(defun git-tool-remote (args)
  "Return git remote output for a repository."
  (let* ((repository (git-tool-resolve-repository args))
         (verbose (git-tool-boolean-arg args :verbose nil))
         (argv (append (list "remote")
                       (when verbose (list "--verbose")))))
    (git-tool-run repository argv)))

(defun git-tool-add (args)
  "Stage repo-relative paths with git add."
  (let* ((repository (git-tool-resolve-repository args))
         (paths (git-tool-normalize-paths
                 (tool-arg args :paths "paths")))
         (argv (append (list "add" "--") paths)))
    (git-tool-run repository argv)))

(defun git-tool-commit (args)
  "Create a git commit with the supplied message."
  (let* ((repository (git-tool-resolve-repository args))
         (message (git-tool-string (tool-arg args :message "message")
                                   "message")))
    (when (git-tool-blank-string-p message)
      (error "message must not be blank."))
    (git-tool-run repository (list "commit" "-m" message))))

(defun git-tool-push (args)
  "Push the current git repository to a remote."
  (let* ((repository (git-tool-resolve-repository args))
         (remote (git-tool-safe-token
                  (tool-arg args :remote "remote")
                  "remote"))
         (branch (git-tool-safe-token
                  (tool-arg args :branch "branch")
                  "branch"))
         (argv (append (list "push")
                       (cond
                         ((and remote branch) (list remote branch))
                         (remote (list remote))
                         (branch
                          (error "branch requires remote for git_push."))
                         (t nil)))))
    (git-tool-run repository argv)))

(register-package-prompt-section
 "git"
 "## Git workflow with git

- Use the `git_*` provider tools for git repository inspection and ordinary
  git workflow actions.
- Prefer `git_status` before changing git state. Use `git_diff` before
  `git_add` or `git_commit` so staged and unstaged changes are explicit.
- Use `git_log`, `git_show`, `git_branch`, and `git_remote` for repository
  history, commit, branch, and remote inspection.
- Use `git_add` only for the repo-relative paths you intend to stage.
- Use `git_commit` only after checking staged diff output. Keep commit
  messages concise and specific.
- Use `git_push` for normal pushes only. There are no force-push, reset,
  checkout, restore, clean, rebase, or stash tools in this package.
- Prefer `project` when a Clawmacs project is known. Otherwise use a
  `repository` path or omit both to use the current buffer or tool working
  directory. Absolute and parent-relative repository paths are allowed.
- Avoid `lisp_eval` or shell-style workarounds for git operations when a
  `git_*` tool fits the task."
 :title "Git workflow with git"
 :package "git")

(deftool git-tool-status
  :name "git_status"
  :description "Return git status for a repository. Defaults to compact short output."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. Takes precedence over repository.")
         (repository :type "string" :required nil
                     :description "Optional repository directory path.")
         (short :type "boolean" :required nil
                :description "Use compact --short output. Defaults to true.")
         (branch :type "boolean" :required nil
                 :description "Include branch headers with --branch.")))

(deftool git-tool-log
  :name "git_log"
  :description "Return compact git log output for a repository."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. Takes precedence over repository.")
         (repository :type "string" :required nil
                     :description "Optional repository directory path.")
         (revision :type "string" :required nil
                   :description "Optional revision or ref to start from. Must not start with '-'.")
         (path :type "string" :required nil
               :description "Optional safe repo-relative path to filter.")
         (limit :type "integer" :required nil
                :description "Maximum commits to return. Defaults to 20.")
         (max-chars :type "integer" :required nil
                    :description "Maximum stdout/stderr characters returned. Defaults to 12000.")))

(deftool git-tool-diff
  :name "git_diff"
  :description "Return git diff output for unstaged or staged changes."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. Takes precedence over repository.")
         (repository :type "string" :required nil
                     :description "Optional repository directory path.")
         (path :type "string" :required nil
               :description "Optional safe repo-relative path to filter.")
         (staged :type "boolean" :required nil
                 :description "When true, diff staged changes with --staged.")
         (stat :type "boolean" :required nil
               :description "When true, return --stat output.")
         (max-chars :type "integer" :required nil
                    :description "Maximum stdout/stderr characters returned. Defaults to 12000.")))

(deftool git-tool-show
  :name "git_show"
  :description "Return git show output for a revision. Defaults to HEAD."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. Takes precedence over repository.")
         (repository :type "string" :required nil
                     :description "Optional repository directory path.")
         (revision :type "string" :required nil
                   :description "Optional revision or ref. Defaults to HEAD and must not start with '-'.")
         (stat :type "boolean" :required nil
               :description "When true, include --stat output.")
         (max-chars :type "integer" :required nil
                    :description "Maximum stdout/stderr characters returned. Defaults to 12000.")))

(deftool git-tool-branch
  :name "git_branch"
  :description "List git branches for a repository."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. Takes precedence over repository.")
         (repository :type "string" :required nil
                     :description "Optional repository directory path.")
         (all :type "boolean" :required nil
              :description "When true, include remote branches with --all.")))

(deftool git-tool-remote
  :name "git_remote"
  :description "List git remotes for a repository."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. Takes precedence over repository.")
         (repository :type "string" :required nil
                     :description "Optional repository directory path.")
         (verbose :type "boolean" :required nil
                  :description "When true, include remote URLs with --verbose.")))

(deftool git-tool-add
  :name "git_add"
  :description "Stage specific safe repo-relative paths with git add."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. Takes precedence over repository.")
         (repository :type "string" :required nil
                     :description "Optional repository directory path.")
         (paths :type "array" :items ((:type . "string"))
                :description "Non-empty array of safe repo-relative paths to stage.")))

(deftool git-tool-commit
  :name "git_commit"
  :description "Create a git commit with the supplied message."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. Takes precedence over repository.")
         (repository :type "string" :required nil
                     :description "Optional repository directory path.")
         (message :type "string"
                  :description "Commit message. Must not be blank.")))

(deftool git-tool-push
  :name "git_push"
  :description "Push the current repository to a remote. Supports normal pushes only."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. Takes precedence over repository.")
         (repository :type "string" :required nil
                     :description "Optional repository directory path.")
         (remote :type "string" :required nil
                 :description "Optional remote name, such as origin. Must not start with '-'.")
         (branch :type "string" :required nil
                 :description "Optional branch to push. Requires remote when supplied.")))
