(in-package :clawmacs/tests)

(in-suite package-manager-suite)

(defvar *package-entrypoint-load-count* 0
  "Tracks how many times the test package entrypoint was loaded.")

(defvar *package-init-continued* nil
  "Tracks whether load-user-init-file continued after a package warning.")

(defvar *startup-hook-ran* nil
  "Tracks whether the startup hook fired during clawmacs-main tests.")

(defvar *initial-buffer-hook-ran* nil
  "Tracks whether the initial-buffer hook fired during clawmacs-main tests.")

(defvar *initial-buffer-hook-binding* nil
  "Captures a key binding observed from the initial buffer hook.")

(defclass test-ui-backend (clawmacs:ui-backend)
  ())

(defmethod clawmacs:backend-run ((backend test-ui-backend) initial-buffer)
  (declare (ignore backend))
  initial-buffer)

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
         (clawmacs::*package-prompt-sections* nil))
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
          (clawmacs::*enabled-builtin-packages* nil))
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

(test default-package-channel-discovers-sexed
  "The bundled default channel advertises the built-in packages."
  (with-package-state-override ((default-package-test-channels))
    (let* ((definitions (clawmacs:reload-package-channels))
           (names (sort (mapcar #'clawmacs:package-definition-name definitions)
                        #'string<))
           (lispi (find "lispi" definitions
                        :key #'clawmacs:package-definition-name
                        :test #'string=))
           (sexed (find "sexed" definitions
                        :key #'clawmacs:package-definition-name
                        :test #'string=)))
      (is (equal '("lispi" "sexed") names))
      (is (not (null lispi)))
      (is (eq :builtin (clawmacs:package-definition-source-tier lispi)))
      (is (probe-file (clawmacs:package-definition-entrypoint lispi)))
      (is (not (null sexed)))
      (is (eq :builtin (clawmacs:package-definition-source-tier sexed)))
      (is (clawmacs:package-definition-autoload sexed))
      (is (probe-file (clawmacs:package-definition-entrypoint sexed))))))

(test load-autoload-packages-skips-disabled-builtin-sexed
  "Bundled sexed stays discoverable but does not autoload by default."
  (with-package-state-override ((default-package-test-channels))
    (let ((loaded (clawmacs:load-autoload-packages)))
      (is (null loaded))
      (is (not (null (clawmacs:find-available-package "sexed"))))
      (is (null (clawmacs:render-package-prompt-sections))))))

(test load-autoload-packages-registers-enabled-sexed-prompt-section
  "Explicitly enabled bundled packages still register their prompt contributions."
  (with-package-state-override ((default-package-test-channels))
    (clawmacs:set-package-enablement-scope "sexed" :global)
    (let ((loaded (clawmacs:load-autoload-packages)))
      (is (= 1 (length loaded)))
      (let ((prompt-section (clawmacs:render-package-prompt-sections)))
        (is (search "Structural editing with sexed" prompt-section))
        (is (search "sexed_project_outline" prompt-section))
        (is-false (search "lisp_eval" prompt-section :test #'char-equal))
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
 :system-prompt-section \"CUSTOM PACKAGE PROMPT\")"
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
                    (clawmacs:render-package-prompt-sections)))))))

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
          (clawmacs::*ui-backend* (make-instance 'test-ui-backend))
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
      (let ((buf (clawmacs:clawmacs-main :session-name "init-customization")))
        (is (string= "Custom personality prompt from init file."
                     clawmacs:*default-personality-prompt*))
        (is (not (null *startup-hook-ran*)))
        (is (not (null *initial-buffer-hook-ran*)))
        (is (eq 'toggle-debug-mode-command *initial-buffer-hook-binding*))
        (is (eq 'toggle-debug-mode-command
                (clawmacs:keymap-lookup (clawmacs:buffer-keymap buf)
                                        '(:ctrl-c #\z))))
        (is (= 2 (clawmacs:buffer-message-count buf)))))))
