(in-package :clawmacs/tests)

(in-suite package-manager-suite)

(defvar *package-entrypoint-load-count* 0
  "Tracks how many times the test package entrypoint was loaded.")

(defvar *package-buffer-rendered* nil
  "Tracks custom package buffer presentation dispatch in tests.")

(defvar *package-init-continued* nil
  "Tracks whether load-user-init-file continued after a package warning.")

(defvar *startup-hook-ran* nil
  "Tracks whether the startup hook fired during clawmacs-main tests.")

(defvar *initial-buffer-hook-ran* nil
  "Tracks whether the initial-buffer hook fired during clawmacs-main tests.")

(defvar *initial-buffer-hook-binding* nil
  "Captures a key binding observed from the initial buffer hook.")

(defmacro with-mcclim-runner-override (&body body)
  `(let ((original-runner (symbol-function 'clawmacs:run-clawmacs-mcclim)))
     (unwind-protect
          (progn
            (setf (symbol-function 'clawmacs:run-clawmacs-mcclim)
                  (lambda (buffer &rest _keys)
                    (declare (ignore _keys))
                    buffer))
            ,@body)
       (setf (symbol-function 'clawmacs:run-clawmacs-mcclim)
             original-runner))))

(defun temp-package-test-directory (label)
  (make-pathname :directory (list :absolute "tmp"
                                  (format nil "clawmacs-package-tests-~A-~36R-~36R-~A"
                                          label
                                          (get-universal-time)
                                          (get-internal-real-time)
                                          (gensym)))))

(defmacro with-packages-directory-override ((path) &body body)
  `(let ((clawmacs::*packages-directory* (uiop:ensure-directory-pathname ,path))
         (clawmacs::*package-configuration-path*
          (merge-pathnames "packages.json" (uiop:ensure-directory-pathname ,path)))
         (clawmacs::*package-configuration* nil)
         (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
         (clawmacs::*package-channels* nil)
         (clawmacs::*available-packages* nil)
         (clawmacs::*package-registry-loaded-p* nil)
         (clawmacs::*package-prompt-sections* nil)
         (clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry)))
     ,@body))

(defmacro with-project-test-state ((root-var definitions-var) &body body)
  `(let* ((base (merge-pathnames
                 (format nil "clawmacs-project-tests-~D-~D-~36R/"
                         (get-universal-time)
                         (get-internal-real-time)
                         (random (expt 36 8)))
                 #P"/tmp/"))
          (,root-var (merge-pathnames #P"root/" base))
          (,definitions-var (merge-pathnames #P"defs/" base))
          (clawmacs::*project-registry* (make-hash-table :test #'equal))
          (clawmacs::*project-definitions-directory* ,definitions-var)
          (clawmacs::*project-definitions-loaded-p* nil)
          (clawmacs::*buffer-ring* nil)
          (clawmacs::*buffer-counter* 0))
     (ensure-directories-exist (merge-pathnames #P".keep" ,root-var))
     (ensure-directories-exist (merge-pathnames #P".keep" ,definitions-var))
     ,@body))

(defun default-package-test-channels ()
  (list (clawmacs:make-package-channel
         :name "default"
         :root clawmacs:*default-package-channel-directory*
         :description "Bundled Clawmacs packages"
         :source :builtin)))

(defmacro with-package-state-override ((channels-form &key enabled-builtin-packages)
                                       &body body)
  (declare (ignore enabled-builtin-packages))
  `(let* ((package-test-root (temp-package-test-directory "config"))
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json"
                            (uiop:ensure-directory-pathname package-test-root)))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels* ,channels-form)
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
          (clawmacs::*buffer-type-registry*
           (clawmacs::make-buffer-type-registry))
          (clawmacs::*enabled-builtin-packages* nil))
     ,@body))

(defmacro with-packrat-resource-state (() &body body)
  `(let ((clawmacs::*command-table* (make-hash-table :test #'eq))
         (clawmacs::*extended-docs* (make-hash-table :test #'eq))
         (clawmacs::*agent-tool-metadata-table* (make-hash-table :test #'eq))
         (clawmacs::*agent-tool-name-table* (make-hash-table :test #'equal))
         (clawmacs::*hook-metadata-table* (make-hash-table :test #'eq))
         (clawmacs::*advice-table* (make-hash-table :test #'eq))
         (clawmacs::*slash-command-table* (make-hash-table :test #'equal))
         (clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry))
         (clawmacs::*package-prompt-sections* nil)
         (clawmacs::*current-package-resource-types* nil))
     ,@body))

(defun write-test-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(test count-occurrences-counts-non-overlapping-substrings
  "COUNT-OCCURRENCES gives agents a small string-counting helper."
  (is (= 2 (count-occurrences "needle" "needle haystack needle")))
  (is (= 2 (count-occurrences "aa" "aaaa")))
  (is (= 0 (count-occurrences "missing" "haystack")))
  (signals error
    (count-occurrences "" "haystack")))

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

(defun make-package-source-directory (&key label manifest entrypoint-content)
  (let ((root (uiop:ensure-directory-pathname
               (temp-package-test-directory (or label "source")))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (when manifest
      (write-test-file (merge-pathnames "manifest.lisp" root) manifest))
    (when entrypoint-content
      (write-test-file (merge-pathnames "test-package.lisp" root)
                       entrypoint-content))
    root))

(defun packrat-resource-entrypoint-content (version)
  (format nil "(in-package :clawmacs)

(defvar *packrat-resource-version* ~S)

(defun packrat-resource-command (buffer)
  (declare (ignore buffer))
  *packrat-resource-version*)

(defcommand packrat-resource-command)

(defun packrat-resource-tool (args)
  (declare (ignore args))
  (lisp-data-string (list :ok t :version *packrat-resource-version*)))

(deftool packrat-resource-tool
  :name \"packrat_resource_tool\"
  :description \"Packrat resource tool.\"
  :permission :agent-allowed
  :call-style :raw-args
  :args ((value :type \"string\" :description \"value\")))

(defdoc packrat-resource-command
  :category \"packrat\"
  :usage \"(packrat-resource-command)\"
  :returns \"string\"
  :see-also ())

(defhook *packrat-resource-hook* (buffer)
  \"Packrat resource hook.\")

(defun packrat-resource-target (value)
  value)

(defadvice packrat-resource-target packrat-resource-advice :before (value)
  (declare (ignore value))
  nil)

(define-buffer-type :packrat-resource
  :description \"Packrat resource buffer\"
  :major-mode \"packrat-resource\")
" version))

(defun packrat-resource-package-content (version)
  (concatenate 'string
               (packrat-resource-entrypoint-content version)
               "

(register-package-prompt-section
 \"packrat-resource\"
 \"## Packrat resource prompt
PACKRAT RESOURCE PROMPT\"
 :package \"resource-package\")
"))

(defun make-packrat-resource-package-source (&key label version)
  (let* ((root (make-package-source-directory
                :label label
                :manifest
                "(:name \"resource-package\"
 :description \"Resource package\"
 :entrypoint \"test-package.lisp\"
 :prompt-template-directory \"prompts/\")"
                :entrypoint-content
                (packrat-resource-package-content version))))
    (write-test-file (merge-pathnames "prompts/review.md" root)
                     "---
description: Resource prompt template
---
Resource prompt body.")
    root))

(defun make-package-channel-root (&key label package-name manifest entrypoint-content)
  (let* ((channel-root (uiop:ensure-directory-pathname
                        (temp-package-test-directory (or label "channel"))))
         (package-dir (merge-pathnames
                       (uiop:ensure-directory-pathname
                        (or package-name "test-package"))
                       channel-root)))
    (ensure-directories-exist (merge-pathnames #P".keep" package-dir))
    (write-test-file
     (merge-pathnames "manifest.lisp" channel-root)
     (format nil "(:name ~S :description ~S :packages (~S))"
             (or label "test-channel")
             "Test package channel"
             (or package-name "test-package")))
    (write-test-file
     (merge-pathnames "manifest.lisp" package-dir)
     manifest)
    (write-test-file
     (merge-pathnames "entry.lisp" package-dir)
     entrypoint-content)
    channel-root))

(test add-hook-and-remove-hook-manage-hook-lists
  "Hook helpers support stable registration and removal from init-facing hook vars."
  (let ((clawmacs::*startup-hook* nil))
    (clawmacs:add-hook 'clawmacs::*startup-hook* 'identity)
    (clawmacs:add-hook 'clawmacs::*startup-hook* 'identity)
    (clawmacs:add-hook 'clawmacs::*startup-hook* 'car :append t)
    (is (equal '(identity car) clawmacs::*startup-hook*))
    (clawmacs:remove-hook 'clawmacs::*startup-hook* 'identity)
    (is (equal '(car) clawmacs::*startup-hook*))))

(test clawmacs-use-package-clones-and-installs-local-git-repo
  "A local git repo is cloned and read via manifest.lisp without loading."
  (let* ((*package-entrypoint-load-count* 0)
         (source-repo
           (make-package-repo
            :label "happy"
            :manifest "(:name \"test-package\" :entrypoint \"test-package.lisp\")"
            :entrypoint-content "(incf clawmacs/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "install-root")))
    (with-packages-directory-override (packages-root)
      (let ((definition (clawmacs:clawmacs-use-package
                         :src-type :git
                         :repo (namestring source-repo))))
        (is (not (null definition)))
        (is (string= "test-package"
                     (clawmacs:package-definition-name definition))))
      (is (= 0 *package-entrypoint-load-count*))
      (is (not (null (clawmacs:find-installed-package "test-package"))))
      (is (probe-file
           (clawmacs::package-install-directory :git (namestring source-repo)))))))

(test clawmacs-use-package-is-install-only-and-idempotent
  "Repeated install calls do not load the package entrypoint."
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
      (is (= 0 *package-entrypoint-load-count*)))))

(test load-active-packages-loads-enabled-installed-package-once
  "Installed packages load only after enablement and only once per session."
  (let* ((*package-entrypoint-load-count* 0)
         (source-repo
           (make-package-repo
            :label "active-load"
            :manifest "(:name \"active-package\" :entrypoint \"test-package.lisp\")"
            :entrypoint-content "(incf clawmacs/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "active-install-root")))
    (with-packages-directory-override (packages-root)
      (is (clawmacs:clawmacs-use-package :src-type :git
                                         :repo (namestring source-repo)))
      (is (null (clawmacs:load-active-packages)))
      (is (= 0 *package-entrypoint-load-count*))
      (is (eq :global
              (clawmacs:set-package-enablement-scope
               "active-package" :global)))
      (is (= 1 (length (clawmacs:load-active-packages))))
      (is (= 1 *package-entrypoint-load-count*))
      (is (= 1 (length (clawmacs:load-active-packages))))
      (is (= 1 *package-entrypoint-load-count*)))))

(test default-package-channel-discovers-bundled-packages
  "The bundled default channel advertises the built-in packages."
  (with-package-state-override ((default-package-test-channels))
    (let* ((definitions (clawmacs:reload-package-channels))
           (names (sort (mapcar #'clawmacs:package-definition-name definitions)
                        #'string<))
           (git (find "git" definitions
                      :key #'clawmacs:package-definition-name
                      :test #'string=))
           (lispi (find "lispi" definitions
                        :key #'clawmacs:package-definition-name
                        :test #'string=))
           (netcons (find "netcons" definitions
                          :key #'clawmacs:package-definition-name
                          :test #'string=))
           (organa (find "organa" definitions
                         :key #'clawmacs:package-definition-name
                         :test #'string=))
           (packrat (find "packrat" definitions
                          :key #'clawmacs:package-definition-name
                          :test #'string=))
           (pipelines (find "pipelines" definitions
                            :key #'clawmacs:package-definition-name
                            :test #'string=))
           (prove (find "prove" definitions
                        :key #'clawmacs:package-definition-name
                        :test #'string=))
           (quaestor (find "quaestor" definitions
                           :key #'clawmacs:package-definition-name
                           :test #'string=))
           (modelaria (find "modelaria" definitions
                            :key #'clawmacs:package-definition-name
                            :test #'string=))
           (artifactum (find "artifactum" definitions
                             :key #'clawmacs:package-definition-name
                             :test #'string=))
           (speculum (find "speculum" definitions
                           :key #'clawmacs:package-definition-name
                           :test #'string=))
           (sexed (find "sexed" definitions
                        :key #'clawmacs:package-definition-name
                        :test #'string=))
           (slop (find "slop" definitions
                       :key #'clawmacs:package-definition-name
                       :test #'string=))
           (subagent (find "subagent" definitions
                           :key #'clawmacs:package-definition-name
                           :test #'string=))
           (templata (find "templata" definitions
                           :key #'clawmacs:package-definition-name
                           :test #'string=)))
      (is (equal '("artifactum" "git" "lispi" "mcp-bridge" "modelaria"
                   "netcons" "organa" "packrat" "pipelines" "prove"
                   "quaestor" "sexed" "slop" "speculum" "subagent"
                   "templata")
                 names))
      (is (not (null git)))
      (is (eq :builtin (clawmacs:package-definition-source-tier git)))
      (is (clawmacs:package-definition-autoload git))
      (is (probe-file (clawmacs:package-definition-entrypoint git)))
      (is (not (null lispi)))
      (is (eq :builtin (clawmacs:package-definition-source-tier lispi)))
      (is (probe-file (clawmacs:package-definition-entrypoint lispi)))
      (is (not (null netcons)))
      (is (eq :builtin (clawmacs:package-definition-source-tier netcons)))
      (is (clawmacs:package-definition-autoload netcons))
      (is (probe-file (clawmacs:package-definition-entrypoint netcons)))
      (is (not (null organa)))
      (is (eq :builtin (clawmacs:package-definition-source-tier organa)))
      (is (clawmacs:package-definition-autoload organa))
      (is (probe-file (clawmacs:package-definition-entrypoint organa)))
      (is (not (null packrat)))
      (is (eq :builtin (clawmacs:package-definition-source-tier packrat)))
      (is (not (clawmacs:package-definition-autoload packrat)))
      (is (probe-file (clawmacs:package-definition-entrypoint packrat)))
      (is (not (null pipelines)))
      (is (eq :builtin (clawmacs:package-definition-source-tier pipelines)))
      (is (clawmacs:package-definition-autoload pipelines))
      (is (probe-file (clawmacs:package-definition-entrypoint pipelines)))
      (is (not (null prove)))
      (is (eq :builtin (clawmacs:package-definition-source-tier prove)))
      (is (clawmacs:package-definition-autoload prove))
      (is (probe-file (clawmacs:package-definition-entrypoint prove)))
      (is (not (null quaestor)))
      (is (eq :builtin (clawmacs:package-definition-source-tier quaestor)))
      (is (clawmacs:package-definition-autoload quaestor))
      (is (probe-file (clawmacs:package-definition-entrypoint quaestor)))
      (is (not (null modelaria)))
      (is (eq :builtin (clawmacs:package-definition-source-tier modelaria)))
      (is (clawmacs:package-definition-autoload modelaria))
      (is (probe-file (clawmacs:package-definition-entrypoint modelaria)))
      (is (not (null artifactum)))
      (is (eq :builtin (clawmacs:package-definition-source-tier artifactum)))
      (is (clawmacs:package-definition-autoload artifactum))
      (is (probe-file (clawmacs:package-definition-entrypoint artifactum)))
      (is (not (null speculum)))
      (is (eq :builtin (clawmacs:package-definition-source-tier speculum)))
      (is (clawmacs:package-definition-autoload speculum))
      (is (probe-file (clawmacs:package-definition-entrypoint speculum)))
      (is (not (null sexed)))
      (is (eq :builtin (clawmacs:package-definition-source-tier sexed)))
      (is (clawmacs:package-definition-autoload sexed))
      (is (probe-file (clawmacs:package-definition-entrypoint sexed)))
      (is (not (null slop)))
      (is (eq :builtin (clawmacs:package-definition-source-tier slop)))
      (is (clawmacs:package-definition-autoload slop))
      (is (probe-file (clawmacs:package-definition-entrypoint slop)))
      (is (not (null subagent)))
      (is (eq :builtin (clawmacs:package-definition-source-tier subagent)))
      (is (clawmacs:package-definition-autoload subagent))
      (is (probe-file (clawmacs:package-definition-entrypoint subagent)))
      (is (not (null templata)))
      (is (eq :builtin (clawmacs:package-definition-source-tier templata)))
      (is (clawmacs:package-definition-autoload templata))
      (is (probe-file (clawmacs:package-definition-entrypoint templata)))
      (is (not (null (find "packrat" definitions
                           :key #'clawmacs:package-definition-name
                           :test #'string=))))
      (is (eq :builtin
              (clawmacs:package-definition-source-tier
               (find "packrat" definitions
                     :key #'clawmacs:package-definition-name
                     :test #'string=)))))))

(test load-autoload-packages-skips-disabled-builtin-sexed
  "Bundled sexed stays discoverable but does not autoload by default."
  (with-package-state-override ((default-package-test-channels))
    (let ((loaded (clawmacs:load-autoload-packages)))
      (is (null loaded))
      (is (not (null (clawmacs:find-available-package "sexed"))))
      (is (not (null (clawmacs:find-available-package "slop"))))
      (is (not (null (clawmacs:find-available-package "netcons"))))
      (is (not (null (clawmacs:find-available-package "organa"))))
      (is (not (null (clawmacs:find-available-package "modelaria"))))
      (is (not (null (clawmacs:find-available-package "artifactum"))))
      (is (not (null (clawmacs:find-available-package "pipelines"))))
      (is (not (null (clawmacs:find-available-package "prove"))))
      (is (not (null (clawmacs:find-available-package "quaestor"))))
      (is (not (null (clawmacs:find-available-package "speculum"))))
      (is (not (null (clawmacs:find-available-package "subagent"))))
      (is (not (null (clawmacs:find-available-package "templata"))))
      (is (not (null (clawmacs:find-available-package "packrat"))))
      (is (null (clawmacs:render-package-prompt-sections))))))

(test load-autoload-packages-registers-enabled-pipelines-surface
  "The bundled pipelines package owns its prompt, commands, and docs."
  (with-package-state-override ((default-package-test-channels))
    (clawmacs:set-package-enablement-scope "pipelines" :global)
    (let ((loaded (clawmacs:load-autoload-packages)))
      (is (= 1 (length loaded)))
      (is (string= "pipelines"
                   (clawmacs:package-definition-name (first loaded))))
      (let ((prompt-section (clawmacs:render-package-prompt-sections)))
        (is (search "Deterministic pipelines" prompt-section))
        (is (search "define-pipeline" prompt-section))
        (is (search "defpipeline" prompt-section))
        (is (search "self-modify" prompt-section))
        (is (search "packages and skills" prompt-section)))
      (is (member 'clawmacs:set-buffer-pipeline
                  (clawmacs:list-available-commands)
                  :test #'eq))
      (is (member 'clawmacs:clear-buffer-pipeline
                  (clawmacs:list-available-commands)
                  :test #'eq))
      (is (string= "pipelines"
                   (clawmacs:command-metadata-package
                    (gethash 'clawmacs:set-buffer-pipeline
                             clawmacs::*command-table*))))
      (is (string= "pipelines"
                   (getf (clawmacs:extended-doc 'clawmacs:define-pipeline)
                         :package))))))

(test load-autoload-packages-registers-enabled-sexed-prompt-section
  "Explicitly enabled bundled packages still register their prompt contributions."
  (with-package-state-override ((default-package-test-channels))
    (clawmacs:set-package-enablement-scope "sexed" :global)
    (let ((loaded (clawmacs:load-autoload-packages)))
      (is (= 1 (length loaded)))
      (let ((prompt-section (clawmacs:render-package-prompt-sections)))
        (is (search "Structural editing with sexed" prompt-section))
        (is (search "sexed_project_outline" prompt-section))
        (is (search "sexed_project_write" prompt-section))
        (is-false (search "lisp_eval" prompt-section :test #'char-equal))
        (is-false (search "change_set" prompt-section :test #'char-equal))
        (is-false (search "(sexed-" prompt-section :test #'char=))))))

(test package-enablement-scope-resolves-buffer-agent-global-default
  "Package enablement inherits global, agent, and buffer scopes without explicit disables."
  (with-package-state-override ((default-package-test-channels))
    (let ((buf (make-buffer "pkg-scope" :agent-name "coder")))
      (is (eq :default
              (clawmacs:package-enablement-scope "sexed" :buffer buf)))
      (is (equal nil (clawmacs:active-package-names :buffer buf)))
      (clawmacs:set-package-enablement-scope "sexed" :global :buffer buf)
      (is (eq :global
              (clawmacs:package-enablement-scope "sexed" :buffer buf)))
      (is (member "sexed" (clawmacs:active-package-names :buffer buf)
                  :test #'string=))
      (clawmacs:set-package-enablement-scope "sexed" :agent :buffer buf)
      (is (eq :agent
              (clawmacs:package-enablement-scope "sexed" :buffer buf)))
      (is (not (clawmacs::package-enabled-globally-p "sexed")))
      (clawmacs:set-package-enablement-scope "sexed" :buffer :buffer buf)
      (is (eq :buffer
              (clawmacs:package-enablement-scope "sexed" :buffer buf)))
      (is (not (clawmacs::package-enabled-for-agent-p "sexed" "coder")))
      (clawmacs:set-package-enablement-scope "sexed" :default :buffer buf)
      (is (eq :default
              (clawmacs:package-enablement-scope "sexed" :buffer buf)))
      (is (equal nil (clawmacs:active-package-names :buffer buf))))))

(test package-enablement-configuration-persists-global-and-agent
  "Global and agent package enablement round-trip through packages.json."
  (with-package-state-override ((default-package-test-channels))
    (let ((path clawmacs::*package-configuration-path*))
      (clawmacs:set-package-enablement-scope "sexed" :global)
      (clawmacs:set-package-enablement-scope "lispi" :agent
                                            :agent-name "coder")
      (setf clawmacs::*package-configuration* nil)
      (is (probe-file path))
      (is (eq :global
              (clawmacs:package-enablement-scope "sexed")))
      (is (eq :agent
              (clawmacs:package-enablement-scope "lispi"
                                                 :agent-name "coder")))
      (is (equal '("lispi" "sexed")
                 (sort (copy-list
                        (clawmacs:active-package-names
                         :agent-name "coder"))
                       #'string<))))))

(test cycle-package-enablement-scope-uses-simple-cycle
  "Package scope cycling avoids explicit disable chains."
  (with-package-state-override ((default-package-test-channels))
    (let ((buf (make-buffer "pkg-cycle" :agent-name "coder")))
      (is (eq :buffer
              (clawmacs:cycle-package-enablement-scope "sexed"
                                                       :buffer buf)))
      (is (eq :agent
              (clawmacs:cycle-package-enablement-scope "sexed"
                                                       :buffer buf)))
      (is (eq :global
              (clawmacs:cycle-package-enablement-scope "sexed"
                                                       :buffer buf)))
      (is (eq :default
              (clawmacs:cycle-package-enablement-scope "sexed"
                                                       :buffer buf))))))

(test package-enablement-refreshes-system-prompt-header
  "Changing package enablement refreshes the synthetic system-prompt header."
  (with-package-state-override ((default-package-test-channels))
    (let ((original (symbol-function 'clawmacs:build-agent-system-prompt)))
      (unwind-protect
           (progn
             (setf (symbol-function 'clawmacs:build-agent-system-prompt)
                   (lambda (agent-name &key buffer)
                     (declare (ignore agent-name))
                     (format nil "SEXED SCOPE: ~A"
                             (clawmacs:package-enablement-scope "sexed"
                                                                 :buffer buffer))))
             (let ((buf (clawmacs:make-chat-buffer "package-prompt-header"
                                                   :session-persistence-mode
                                                   :ephemeral)))
               (is (search "SEXED SCOPE: DEFAULT"
                           (message-text (buffer-first-message buf))))
               (clawmacs:set-package-enablement-scope "sexed" :global :buffer buf)
               (is (search "SEXED SCOPE: GLOBAL"
                           (message-text (buffer-first-message buf))))))
        (setf (symbol-function 'clawmacs:build-agent-system-prompt)
              original)))))

(test package-channel-loads-package-and-manifest-prompt
  "A local channel can advertise and load a package with a prompt section."
  (let* ((*package-entrypoint-load-count* 0)
         (channel-root
           (make-package-channel-root
            :label "custom-channel"
            :package-name "custom-package"
            :manifest "(:name \"custom-package\"
 :description \"Custom package\"
 :entrypoint \"entry.lisp\"
 :autoload t
 :system-prompt-section \"## Custom package prompt

CUSTOM PACKAGE PROMPT\")"
            :entrypoint-content
            "(incf clawmacs/tests::*package-entrypoint-load-count*)")))
    (with-package-state-override (nil)
      (clawmacs:register-package-channel "custom" channel-root
                                         :description "Custom channel")
      (let ((definition (clawmacs:find-available-package "custom-package")))
        (is (not (null definition)))
        (is (eq :channel (clawmacs:package-definition-source-tier definition)))
        (is (clawmacs:load-clawmacs-package "custom-package"))
        (is (= 1 *package-entrypoint-load-count*))
        (is (null (clawmacs:render-package-prompt-sections)))
        (clawmacs:set-package-enablement-scope "custom-package" :global)
        (is (search "CUSTOM PACKAGE PROMPT"
                    (clawmacs:render-package-prompt-sections)))
        (is (equal '("Custom package prompt")
                   (mapcar #'clawmacs:package-prompt-section-title
                           (clawmacs:package-owned-prompt-sections
                            "custom-package"))))))))

(test package-channel-loads-package-buffer-type-presentation
  "Package entrypoints can register buffer types with McCLIM presentation hooks."
  (let* ((*package-buffer-rendered* nil)
         (channel-root
           (make-package-channel-root
            :label "buffer-channel"
            :package-name "dashboard-package"
            :manifest "(:name \"dashboard-package\"
 :description \"Dashboard package\"
 :entrypoint \"entry.lisp\")"
            :entrypoint-content
            "(defun package-dashboard-presenter (pane buffer rows cols char-w char-h)
  (declare (ignore pane rows cols char-w char-h))
  (setf clawmacs/tests::*package-buffer-rendered*
        (list :rendered (buffer-name buffer))))

(define-buffer-type :package-dashboard
  :description \"Package dashboard buffer\"
  :major-mode \"dashboard\"
  :presentation-function 'package-dashboard-presenter)")))
    (with-package-state-override (nil)
      (clawmacs:register-package-channel "custom" channel-root
                                         :description "Custom channel")
      (is (clawmacs:load-clawmacs-package "dashboard-package"))
      (let ((type (clawmacs:find-buffer-type :package-dashboard)))
        (is (not (null type)))
        (is (string= "dashboard-package"
                     (clawmacs:buffer-type-package type)))
        (is (string= "dashboard"
                     (clawmacs:buffer-type-major-mode type))))
      (is (= 1 (length (clawmacs:package-owned-buffer-types
                        "dashboard-package"))))
      (let ((help (clawmacs:describe-installed-package-to-string
                   (clawmacs:find-installed-package "dashboard-package")
                   nil)))
        (is (search "Buffer Types:" help))
        (is (search "package-dashboard" help :test #'char-equal)))
      (let ((buf (make-buffer "dashboard" :kind :package-dashboard)))
        (clawmacs::mcclim-render-buffer nil buf 10 80 8 16)
        (is (equal '(:rendered "dashboard") *package-buffer-rendered*))))))

(test package-dashboard-display-entries-include-installed-packages
  "The package dashboard exposes installed packages as presented entries."
  (with-package-state-override ((default-package-test-channels))
    (let* ((origin (make-buffer "chat" :agent-name "coder"))
           (dashboard (make-buffer "*Packages*" :kind :package-dashboard
                                   :agent-name "packages")))
      (setf (clawmacs::package-dashboard-origin-buffer dashboard) origin)
      (let* ((entries (clawmacs::package-dashboard-display-entries dashboard))
             (sexed (find-if (lambda (entry)
                               (and (eq 'clawmacs::package-dashboard-entry-ref
                                        (getf entry :presentation-type))
                                    (search " sexed :: " (getf entry :text))))
                             entries)))
        (is (not (null sexed)))
        (is (search "[default] [ok] sexed" (getf sexed :text)))
        (is (eq origin (getf (getf sexed :object) :origin-buffer)))))))

(test package-dashboard-toggle-entry-cycles-origin-buffer-scope
  "Selecting a package dashboard entry cycles scope in the originating buffer context."
  (with-package-state-override ((default-package-test-channels))
    (let* ((origin (make-buffer "chat" :agent-name "coder"))
           (dashboard (make-buffer "*Packages*" :kind :package-dashboard
                                   :agent-name "packages"))
           (entry (find "sexed"
                        (clawmacs::package-doctor-report :buffer origin)
                        :key (lambda (item) (getf item :name))
                        :test #'string=)))
      (setf (clawmacs::package-dashboard-origin-buffer dashboard) origin)
      (is (eq :buffer
              (clawmacs::package-dashboard-toggle-entry dashboard entry
                                                        :origin-buffer origin)))
      (is (eq :buffer
              (clawmacs:package-enablement-scope "sexed" :buffer origin)))
      (is (member "sexed" (buffer-enabled-packages origin) :test #'string=)))))

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

(test clawmacs-use-package-installs-path-packages-and-records-metadata
  "Local-path installs copy the package, persist a record, and report scope."
  (let* ((source (make-packrat-resource-package-source
                  :label "packrat-local"
                  :version "v1"))
         (packages-root (temp-package-test-directory "packrat-local-root")))
    (with-packages-directory-override (packages-root)
      (let ((definition (clawmacs:clawmacs-use-package
                         :src-type :path
                         :repo (namestring source)
                         :resource-types '(:tool :command :doc :buffer-type))))
        (is (not (null definition)))
        (is (string= "resource-package"
                     (clawmacs:package-definition-name definition)))
        (let ((record (clawmacs:package-install-record-for-definition definition)))
          (is (eq :path (clawmacs:package-install-record-source-type record)))
          (is (eq :global (clawmacs:package-install-record-scope record)))
          (is (equal '(:tool :command :doc :buffer-type)
                     (clawmacs:package-install-record-resource-types record)))
          (is (string= (namestring source)
                       (clawmacs:package-install-record-source record)))
          (is (probe-file
               (clawmacs::package-install-metadata-path
                (clawmacs:package-definition-root definition)))))
        (is (search "resource-package"
                    (clawmacs:install-package-status-string definition)))
        (is (search "resources: tool, command, doc, buffer-type"
                    (clawmacs:package-resource-policy-string definition))))))

(test clawmacs-use-package-updates-and-removes-installed-packages
  "Installed package records can be refreshed from source and removed again."
  (let* ((source (make-packrat-resource-package-source
                  :label "packrat-cycle"
                  :version "v1"))
         (packages-root (temp-package-test-directory "packrat-cycle-root")))
    (with-packages-directory-override (packages-root)
      (let ((definition (clawmacs:clawmacs-use-package
                         :src-type :path
                         :repo (namestring source))))
        (is (not (null definition)))
        (let ((installed-file
                (merge-pathnames "test-package.lisp"
                                 (clawmacs:package-definition-root definition))))
          (is (search "v1" (uiop:read-file-string installed-file)))
          (write-test-file (merge-pathnames "test-package.lisp" source)
                           (packrat-resource-package-content "v2"))
          (is (not (null (clawmacs:update-installed-package
                          "resource-package"))))
          (is (search "v2" (uiop:read-file-string installed-file)))
          (uiop:delete-directory-tree source :validate t :if-does-not-exist :ignore)
          (let* ((report (clawmacs:package-doctor-report))
                 (entry (find "resource-package" report
                              :key (lambda (item) (getf item :name))
                              :test #'string=)))
            (is (not (null entry)))
            (is (eq :missing-source (getf entry :status))))
          (is (search "missing-source" (clawmacs:package-doctor-to-string)))
          (is (not (null (clawmacs:remove-installed-package "resource-package"))))
          (is (null (clawmacs:find-installed-package "resource-package"))))))))

(test clawmacs-use-package-respects-package-resource-filtering
  "Package resource allowlists control which package-owned artifacts register."
  (let* ((source (make-packrat-resource-package-source
                  :label "packrat-filter"
                  :version "v1"))
         (packages-root (temp-package-test-directory "packrat-filter-root")))
    (with-packages-directory-override (packages-root)
      (with-packrat-resource-state ()
        (let ((clawmacs::*prompt-template-user-directory*
                (uiop:ensure-directory-pathname
                 (temp-package-test-directory "packrat-filter-global"))))
          (clawmacs:clawmacs-use-package
           :src-type :path
           :repo (namestring source)
           :resource-types '(:tool :command :doc :buffer-type))
          (clawmacs:set-package-enablement-scope "resource-package" :global)
          (is (not (null (clawmacs:load-active-packages))))
          (is (not (null (gethash 'clawmacs::packrat-resource-command
                                  clawmacs::*command-table*))))
          (is (not (null (clawmacs:find-agent-tool-metadata
                          'clawmacs::packrat-resource-tool))))
          (is (not (null (gethash 'clawmacs::packrat-resource-command
                                  clawmacs::*extended-docs*))))
          (is (not (null (clawmacs:find-buffer-type :packrat-resource))))
          (is (null (clawmacs:find-hook-metadata
                     'clawmacs::*packrat-resource-hook*)))
          (is (null (clawmacs:list-advices 'clawmacs::packrat-resource-target)))
          (is (null (clawmacs:render-package-prompt-sections)))
          (is (null (clawmacs:discover-prompt-templates
                     :buffer (make-buffer "packrat-filter")))))))))

(test project-declared-packages-auto-install-and-project-local-install-wins
  "Project manifests can auto-install declared packages and prefer project-local installs."
  (let* ((global-source (make-packrat-resource-package-source
                         :label "packrat-global"
                         :version "global"))
         (project-source (make-packrat-resource-package-source
                          :label "packrat-project"
                          :version "project"))
         (project-root (temp-package-test-directory "packrat-project-root"))
         (project-packages
           `((:name "resource-package"
              :src-type :path
              :path ,(namestring project-source)
              :scope :project
              :resource-types (:tool :command)))))
    (with-project-test-state (root definitions)
      (with-packages-directory-override
          ((temp-package-test-directory "packrat-precedence-packages"))
        (clawmacs:create-project "packrat-project"
                                 :root project-root
                                 :packages project-packages)
        (clawmacs:clawmacs-use-package
         :src-type :path
         :repo (namestring global-source))
        (clawmacs:load-project-definitions)
        (let* ((project (clawmacs:find-project "packrat-project"))
               (global-definition (clawmacs:find-installed-package
                                   "resource-package"))
               (project-definition
                 (clawmacs:find-installed-package "resource-package"
                                                  :project project))
               (buffer (make-buffer "packrat-project"
                                    :working-directory project-root)))
          (is (not (null project)))
          (is (eq :third-party
                  (clawmacs:package-definition-source-tier global-definition)))
          (is (search "global"
                      (uiop:read-file-string
                       (merge-pathnames "test-package.lisp"
                                        (clawmacs:package-definition-root
                                         global-definition)))))
          (is (not (null project-definition)))
          (is (eq :project (clawmacs:package-definition-source-tier
                            project-definition)))
          (is (search "project"
                      (uiop:read-file-string
                       (merge-pathnames "test-package.lisp"
                                        (clawmacs:package-definition-root
                                         project-definition)))))
          (is (eq :project (clawmacs:package-enablement-scope
                            "resource-package"
                            :buffer buffer
                            :project project)))
          (is (eq project-definition
                  (clawmacs:find-installed-package "resource-package"
                                                   :buffer buffer
                                                   :project project))))))))

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
      (is (not (null *package-init-continued*)))))))

(test load-user-init-file-can-register-package-channels
  "init.lisp can register additional local package channels."
  (let* ((*package-entrypoint-load-count* 0)
         (channel-root
           (make-package-channel-root
            :label "init-channel"
            :package-name "init-package"
            :manifest "(:name \"init-package\"
 :description \"Init package\"
 :entrypoint \"entry.lisp\"
 :autoload t)"
            :entrypoint-content
            "(incf clawmacs/tests::*package-entrypoint-load-count*)"))
         (init-root (uiop:ensure-directory-pathname
                     (temp-package-test-directory "package-channel-init")))
         (init-path (merge-pathnames "init.lisp" init-root)))
    (ensure-directories-exist (merge-pathnames #P".keep" init-root))
    (write-test-file
     init-path
     (format nil "(register-package-channel \"init\" #P~S :description \"Init channel\")"
             (namestring channel-root)))
    (let ((clawmacs::*user-init-file* init-path)
          (clawmacs::*inhibit-user-init* nil))
      (with-package-state-override (nil)
        (clawmacs:load-user-init-file)
        (is (not (null (find "init"
                             (clawmacs:list-package-channels)
                             :key #'clawmacs:package-channel-name
                             :test #'string=))))
        (clawmacs:set-package-enablement-scope "init-package" :global)
        (is (clawmacs:load-autoload-packages))
        (is (= 1 *package-entrypoint-load-count*))))))

(test load-user-init-file-can-register-agent-definitions
  "User init forms can register programmatic agent definitions."
  (let* ((init-root (uiop:ensure-directory-pathname
                     (temp-package-test-directory "agent-init")))
         (init-path (merge-pathnames "init.lisp" init-root)))
    (ensure-directories-exist (merge-pathnames #P".keep" init-root))
    (write-test-file
     init-path
     "(register-agent-definition \"writer\"
   :provider :openai-codex
   :model \"gpt-5.4\"
   :think-level \"high\"
   :core-prompt \"writer core\"
   :personality-prompt \"writer personality\")")
    (let ((clawmacs::*user-init-file* init-path)
          (clawmacs::*inhibit-user-init* nil)
          (clawmacs::*agent-definition-registry* (make-hash-table :test #'equal)))
      (clawmacs:load-user-init-file)
      (let ((definition (clawmacs:find-agent-definition "writer")))
        (is (not (null definition)))
        (is (eq :openai-codex (clawmacs:agent-definition-provider definition)))
        (is (string= "gpt-5.4" (clawmacs:agent-definition-model definition)))
        (is (string= "high" (clawmacs:agent-definition-think-level definition)))
        (is (string= "writer core" (clawmacs:agent-definition-core-prompt definition)))
        (is (string= "writer personality" (clawmacs:agent-definition-personality-prompt definition)))))))

(test clawmacs-main-allows-init-based-prompt-and-hook-customization
  "init.lisp can reload the personality prompt, mutate seeded defaults, and hook the initial buffer."
  (let* ((init-root (uiop:ensure-directory-pathname
                     (temp-package-test-directory "main-init")))
         (init-path (merge-pathnames "init.lisp" init-root))
         (prompt-path (merge-pathnames "custom-system-prompt.txt" init-root))
         (missing-path (merge-pathnames "missing-system-prompt.txt" init-root)))
    (ensure-directories-exist (merge-pathnames #P".keep" init-root))
    (write-test-file prompt-path
                     (format nil "  Custom personality prompt from init file.~%"))
    (write-test-file
     init-path
     (format nil
             "(setf *personality-prompt-path* #P~S)~%
(load-personality-prompt-file)~%
(keymap-bind *default-keymap* '(:ctrl-c #\\z) 'toggle-debug-mode-command)~%
(add-hook '*startup-hook*
          (lambda ()
            (setf clawmacs/tests::*startup-hook-ran*
                  (eq (keymap-lookup *default-keymap* '(:ctrl-c #\\z))
                      'toggle-debug-mode-command))))~%
(add-hook '*initial-buffer-hook*
          (lambda (buffer)
            (setf clawmacs/tests::*initial-buffer-hook-ran* t)
            (setf clawmacs/tests::*initial-buffer-hook-binding*
                  (keymap-lookup (buffer-keymap buffer) '(:ctrl-c #\\z)))
            (buffer-insert-system-message buffer \"init buffer hook ran\")))~%"
             (namestring prompt-path)))
    (let ((clawmacs::*user-init-file* init-path)
          (clawmacs::*inhibit-user-init* nil)
          (clawmacs::*personality-prompt-path* missing-path)
          (clawmacs::*default-personality-prompt* "Default personality prompt")
          (clawmacs::*startup-hook* nil)
          (clawmacs::*initial-buffer-hook* nil)
          (clawmacs::*default-keymap* nil)
          (clawmacs::*debug-log-file* nil)
          (clawmacs::*package-channels* (default-package-test-channels))
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
          (clawmacs::*enabled-builtin-packages* nil)
          (*startup-hook-ran* nil)
          (*initial-buffer-hook-ran* nil)
          (*initial-buffer-hook-binding* nil))
      (let ((buf (with-mcclim-runner-override
                   (clawmacs:clawmacs-main :session-name "init-customization"))))
        (is (string= "Custom personality prompt from init file."
                     clawmacs:*default-personality-prompt*))
        (is (not (null *startup-hook-ran*)))
        (is (not (null *initial-buffer-hook-ran*)))
        (is (eq 'toggle-debug-mode-command *initial-buffer-hook-binding*))
        (is (eq 'toggle-debug-mode-command
                (clawmacs:keymap-lookup (clawmacs:buffer-keymap buf)
                                        '(:ctrl-c #\z))))
        (is (= 2 (clawmacs:buffer-message-count buf)))))))
