(in-package :rplaca/tests)

(in-suite migration-integration-suite)

(defvar *legacy-executable-ran* nil)

(defmacro with-migration-integration-root ((root) &body body)
  `(with-legacy-path-test-root (,root)
     (rplaca::reset-legacy-path-warnings)
     ,@body))

(defun migration-path (root relative)
  (merge-pathnames relative (uiop:ensure-directory-pathname root)))

(test inert-stores-fallback-read-only-and-canonical-writes-win
  "Credentials, appearance, model metadata, and defaults are inert data."
  (with-migration-integration-root (root)
    (let* ((canonical-dir (migration-path root #P"rplaca/"))
           (legacy-dir (migration-path root #P"clawmacs/"))
           (canonical-token (migration-path canonical-dir #P"zai-api-key"))
           (legacy-token (migration-path legacy-dir #P"zai-api-key"))
           (canonical-appearance
             (migration-path canonical-dir #P"appearance.sexp"))
           (legacy-appearance
             (migration-path legacy-dir #P"appearance.sexp"))
           (canonical-modelaria
             (migration-path canonical-dir #P"modelaria.json"))
           (legacy-modelaria
             (migration-path legacy-dir #P"modelaria.json"))
           (canonical-personality
             (migration-path canonical-dir #P"system-prompt.txt"))
           (legacy-personality
             (migration-path legacy-dir #P"system-prompt.txt"))
           (canonical-defaults
             (migration-path canonical-dir #P"agent-defaults.json"))
           (legacy-defaults
             (migration-path legacy-dir #P"agent-defaults.json")))
      (write-legacy-path-test-file legacy-token "legacy-token")
      (write-legacy-path-test-file
       legacy-appearance
       "(:clawmacs-appearance :version 1 :theme :dark :strict-contrast nil :overrides ())")
      (write-legacy-path-test-file
       legacy-modelaria
       "{\"active_role\":\"review\",\"role_set\":[\"review\"],\"service_tier\":\"priority\"}")
      (write-legacy-path-test-file legacy-personality "legacy personality")
      (write-legacy-path-test-file
       legacy-defaults
       "{\"planner\":{\"provider\":\"zai\",\"model\":\"legacy-model\"}}")
      (let ((rplaca::*provider-token-directory* canonical-dir)
            (rplaca::*legacy-provider-token-directory* legacy-dir)
            (rplaca::+default-personality-prompt-path+ canonical-personality)
            (rplaca::+legacy-personality-prompt-path+ legacy-personality)
            (rplaca::*personality-prompt-path* canonical-personality)
            (rplaca::+default-agent-defaults-path+ canonical-defaults)
            (rplaca::+legacy-agent-defaults-path+ legacy-defaults)
            (rplaca::*agent-defaults-path* canonical-defaults)
            (rplaca::*agent-defaults-registry* nil)
            (rplaca::+default-appearance-config-path+ canonical-appearance)
            (rplaca::+legacy-appearance-config-path+ legacy-appearance)
            (rplaca::*appearance-config-path* canonical-appearance)
            (rplaca::+default-modelaria-global-config-path+ canonical-modelaria)
            (rplaca::+legacy-modelaria-global-config-path+ legacy-modelaria)
            (rplaca::*modelaria-global-config-path* canonical-modelaria))
        (is (string= "legacy-token"
                     (rplaca::read-provider-file-token :zai)))
        (is (string= "legacy personality"
                     (rplaca::load-personality-prompt-file)))
        (is (string= "legacy-model"
                     (getf (rplaca::agent-default-spec "planner") :model)))
        (multiple-value-bind (profile status)
            (rplaca::read-appearance-profile-file)
          (is (eq :valid status))
          (is (eq :dark
                  (rplaca:appearance-profile-selected-theme profile))))
        ;; Modelaria JSON contains only role/model metadata. It cannot launch
        ;; commands or load code, so it is deliberately an inert fallback.
        (is (string= "review"
                     (getf (rplaca::modelaria-global-config) :active-role)))
        (rplaca::save-provider-token :zai "canonical-token")
        (is (string= "canonical-token"
                     (uiop:read-file-string canonical-token)))
        (is (string= "canonical-token"
                     (rplaca::read-provider-file-token :zai)))
        (is (string= "legacy-token"
                     (uiop:read-file-string legacy-token)))
        (write-legacy-path-test-file
         canonical-appearance
         "(:rplaca-appearance :version 1 :theme :classic :strict-contrast nil :overrides ())")
        (multiple-value-bind (profile status)
            (rplaca::read-appearance-profile-file)
          (is (eq :valid status))
          (is (eq :classic
                  (rplaca:appearance-profile-selected-theme profile))))))))

(test behavioral-stores-refuse-legacy-automatic-loading
  "Init, package, skill, MCP, and project registries can execute behavior."
  (with-migration-integration-root (root)
    (let* ((canonical-dir (migration-path root #P"rplaca/"))
           (legacy-dir (migration-path root #P"clawmacs/"))
           (canonical-init (migration-path canonical-dir #P"init.lisp"))
           (legacy-init (migration-path legacy-dir #P"init.lisp"))
           (canonical-packages (migration-path canonical-dir #P"packages.json"))
           (legacy-packages (migration-path legacy-dir #P"packages.json"))
           (canonical-skills (migration-path canonical-dir #P"skills.json"))
           (legacy-skills (migration-path legacy-dir #P"skills.json"))
           (canonical-mcp (migration-path canonical-dir #P"mcp-servers.json"))
           (legacy-mcp (migration-path legacy-dir #P"mcp-servers.json"))
           (canonical-projects (migration-path root #P"rplaca-projects/"))
           (legacy-projects (migration-path root #P"clawmacs-projects/"))
           (project-root (migration-path root #P"legacy-project-root/")))
      (write-legacy-path-test-file
       legacy-init
       "(setf rplaca/tests::*legacy-executable-ran* t)")
      (write-legacy-path-test-file
       legacy-packages
       "{\"global\":[\"modelaria\"],\"agents\":{}}")
      (write-legacy-path-test-file
       legacy-skills
       "{\"disabled\":[\"/tmp/legacy/SKILL.md\"]}")
      (write-legacy-path-test-file
       legacy-mcp
       "{\"servers\":[{\"name\":\"legacy\",\"transport\":\"stdio\",\"command\":\"false\"}]}")
      (write-legacy-path-test-file
       (migration-path legacy-projects #P"legacy.project")
       (format nil "(:name \"legacy-only\" :root ~S)"
               (namestring project-root)))
      (let ((rplaca::+default-user-init-file+ canonical-init)
            (rplaca::+legacy-user-init-file+ legacy-init)
            (rplaca::*user-init-file* canonical-init)
            (rplaca::*inhibit-user-init* nil)
            (rplaca::+default-package-configuration-path+ canonical-packages)
            (rplaca::+legacy-package-configuration-path+ legacy-packages)
            (rplaca::*package-configuration-path* canonical-packages)
            (rplaca::*package-configuration* nil)
            (rplaca::+default-skill-configuration-path+ canonical-skills)
            (rplaca::+legacy-skill-configuration-path+ legacy-skills)
            (rplaca::*skill-configuration-path* canonical-skills)
            (rplaca::*skill-disabled-paths* nil)
            (rplaca::+default-mcp-server-configuration-path+ canonical-mcp)
            (rplaca::+legacy-mcp-server-configuration-path+ legacy-mcp)
            (rplaca::*mcp-server-configuration-path* canonical-mcp)
            (rplaca::*mcp-server-registry* nil)
            (rplaca::+default-project-definitions-directory+ canonical-projects)
            (rplaca::+legacy-project-definitions-directory+ legacy-projects)
            (rplaca::*project-definitions-directory* canonical-projects)
            (rplaca::*project-registry* (make-hash-table :test #'equal))
            (rplaca::*project-definitions-loaded-p* nil)
            (rplaca::*user-init-directory* canonical-dir)
            (*legacy-executable-ran* nil))
        (rplaca::load-user-init-file)
        (is-false *legacy-executable-ran*)
        (is-false
         (gethash "modelaria"
                  (rplaca::package-configuration-global-table
                   (rplaca::load-package-configuration))))
        (is (= 0 (hash-table-count
                  (rplaca::load-skill-disabled-paths))))
        (is (= 0 (hash-table-count
                  (rplaca::load-mcp-server-configurations))))
        (rplaca:load-project-definitions)
        (is-false (rplaca:find-project "legacy-only"))
        (is-false (probe-file canonical-init))
        (is-false (probe-file canonical-packages))
        (is-false (probe-file canonical-skills))
        (is-false (probe-file canonical-mcp))))))

(test sessions-discover-then-materialize-whole-tree-before-mutation
  "Discovery is read-only; mutation copies one complete tree and preserves IDs."
  (with-migration-integration-root (root)
    (let* ((canonical (migration-path root #P"rplaca/sessions/"))
           (legacy (migration-path root #P"clawmacs/sessions/"))
           (rplaca::+default-sessions-dir+ canonical)
           (rplaca::+legacy-sessions-dir+ legacy)
           (rplaca::*sessions-dir* canonical)
           (legacy-session
             (rplaca::load-or-create-session "historic"
                                             :root legacy
                                             :source-root legacy))
           (legacy-id (rplaca::session-id legacy-session))
           (legacy-transcript
             (rplaca::session-current-transcript-path legacy-session)))
      #+sbcl
      (sb-posix:chmod (namestring legacy-transcript) #o600)
      (is (equal '("historic") (rplaca:list-saved-sessions)))
      (is-false (probe-file canonical))
      (let ((record (first (rplaca::list-saved-session-records))))
        (is (equal legacy (getf record :source-root)))
        (is (string= legacy-id (getf record :session-id))))
      (let ((loaded
              (rplaca:load-session
               (namestring (rplaca::session-manifest-path legacy-session)))))
        (is (not (null loaded)))
        (is (equal legacy
                   (rplaca::session-source-root
                    (rplaca:buffer-session loaded)))))
      (is (probe-file canonical))
      (is (probe-file
           (rplaca::session-current-transcript-path
            (rplaca::load-or-create-session
             "historic" :source-root legacy))))
      (let ((migrated
              (rplaca::load-or-create-session
               "historic" :source-root legacy)))
        (is (string= legacy-id (rplaca::session-id migrated)))
        (is (equal legacy (rplaca::session-source-root migrated)))
        (is (uiop:subpathp (rplaca::session-current-transcript-path migrated)
                           canonical))
        #+sbcl
        (is (= #o600
               (logand #o777
                       (sb-posix:stat-mode
                        (sb-posix:stat
                         (namestring
                          (rplaca::session-current-transcript-path
                           migrated)))))))))))

(test session-first-use-migration-is-safe-across-processes
  "Two first-use processes publish one complete canonical session tree."
  (with-migration-integration-root (root)
    (let* ((canonical (migration-path root #P"rplaca/sessions/"))
           (legacy (migration-path root #P"clawmacs/sessions/"))
           (canonical-nested (migration-path canonical #P"locked/nested/"))
           (legacy-nested (migration-path legacy #P"locked/nested/"))
           (canonical-restricted
             (migration-path canonical-nested #P"restricted.json"))
           (legacy-restricted
             (migration-path legacy-nested #P"restricted.json"))
           (barrier (migration-path root #P"barrier/"))
           (repo (asdf:system-source-directory :rplaca))
           (entry (merge-pathnames
                   #P"tests/session-migration-subprocess.lisp" repo))
           (output-one (migration-path root #P"process-one.log"))
           (output-two (migration-path root #P"process-two.log"))
           (output-late (migration-path root #P"process-late.log")))
      (dotimes (index 32)
        (write-legacy-path-test-file
         (migration-path
          legacy
          (make-pathname :name (format nil "legacy-~2,'0D" index)
                         :type "json"))
         (make-string (+ 1024 index) :initial-element #\x)))
      (write-legacy-path-test-file
       legacy-restricted
       "restricted")
      (ensure-directories-exist (migration-path barrier #P".keep"))
      #+sbcl
      (progn
        (sb-posix:chmod (namestring legacy-restricted) #o400)
        (sb-posix:chmod (namestring legacy-nested) #o555)
        (sb-posix:chmod (namestring legacy) #o555))
      (unwind-protect
           (progn
             (let* ((command
                      (list
                       "env"
                       (format nil "RPLACA_QUICKLISP_SETUP=~A"
                               (or (uiop:getenv "RPLACA_QUICKLISP_SETUP")
                                   (error
                                    "RPLACA_QUICKLISP_SETUP is required")))
                       (format nil "RPLACA_TEST_REPO_ROOT=~A"
                               (namestring repo))
                       (format nil "RPLACA_TEST_CANONICAL_SESSIONS=~A"
                               (namestring canonical))
                       (format nil "RPLACA_TEST_LEGACY_SESSIONS=~A"
                               (namestring legacy))
                       (format nil "RPLACA_TEST_SESSION_BARRIER=~A"
                               (namestring barrier))
                       "sbcl" "--noinform" "--disable-debugger"
                       "--script" (namestring entry)))
                    (one
                      (uiop:launch-program command
                                           :output output-one
                                           :error-output :output))
                    (two
                      (uiop:launch-program command
                                           :output output-two
                                           :error-output :output)))
               (is (zerop (uiop:wait-process one)))
               (is (zerop (uiop:wait-process two)))
               (let ((late
                       (uiop:launch-program command
                                            :output output-late
                                            :error-output :output)))
                 (is (zerop (uiop:wait-process late)))))
             (is (rplaca::completed-session-migration-p canonical legacy))
             (is (= 32
                    (length
                     (remove-if
                      (lambda (path)
                        (string=
                         (file-namestring path)
                         (file-namestring
                          rplaca::+session-migration-completion-marker+)))
                      (uiop:directory-files canonical)))))
             (is (string= "restricted"
                          (uiop:read-file-string canonical-restricted)))
             #+sbcl
             (progn
               (is (= #o555 (rplaca::session-path-mode canonical)))
               (is (= #o555
                      (rplaca::session-path-mode canonical-nested)))
               (is (= #o400
                      (rplaca::session-path-mode canonical-restricted))))
             (is-false
              (find-if
               (lambda (directory)
                 (search ".sessions-migration-" (namestring directory)))
               (uiop:subdirectories
                (uiop:pathname-parent-directory-pathname canonical)))))
        #+sbcl
        (dolist (directory
                 (list canonical-nested canonical legacy-nested legacy))
          (when (probe-file directory)
            (sb-posix:chmod (namestring directory) #o700)))))))

(test pending-canonical-session-migration-is-recovered-before-use
  "A legacy published pending tree is replaced, never returned as canonical."
  (with-migration-integration-root (root)
    (let* ((canonical (migration-path root #P"rplaca/sessions/"))
           (legacy (migration-path root #P"clawmacs/sessions/"))
           (marker
             (migration-path
              canonical rplaca::+session-migration-completion-marker+))
           (expected (migration-path legacy #P"expected.json"))
           (stale (migration-path canonical #P"stale.json"))
           (rplaca::+default-sessions-dir+ canonical)
           (rplaca::+legacy-sessions-dir+ legacy)
           (rplaca::*sessions-dir* canonical))
      (write-legacy-path-test-file expected "expected")
      (write-legacy-path-test-file stale "must-not-survive")
      (write-legacy-path-test-file marker (format nil "pending~%"))
      #+sbcl
      (sb-posix:chmod (namestring canonical) #o555)
      (unwind-protect
           (progn
             (is (equal legacy (rplaca::selected-sessions-read-root)))
             (is (equal canonical
                        (rplaca::materialize-legacy-sessions-before-mutation)))
             (is-false (probe-file stale))
             (is (string= "expected"
                          (uiop:read-file-string
                           (migration-path canonical #P"expected.json"))))
             (is (rplaca::completed-session-migration-p canonical legacy)))
        #+sbcl
        (when (probe-file canonical)
          (sb-posix:chmod (namestring canonical) #o700))))))

(test crashed-session-publisher-leaves-no-usable-canonical-and-recovers
  "A crash before rename leaves only recoverable staging, never canonical."
  (with-migration-integration-root (root)
    (let* ((canonical (migration-path root #P"rplaca/sessions/"))
           (legacy (migration-path root #P"clawmacs/sessions/"))
           (canonical-nested (migration-path canonical #P"locked/"))
           (legacy-nested (migration-path legacy #P"locked/"))
           (legacy-file (migration-path legacy-nested #P"payload.json"))
           (canonical-file (migration-path canonical-nested #P"payload.json"))
           (barrier (migration-path root #P"barrier/"))
           (repo (asdf:system-source-directory :rplaca))
           (entry (merge-pathnames
                   #P"tests/session-migration-subprocess.lisp" repo))
           (crash-output (migration-path root #P"publisher-crash.log"))
           (recovery-output (migration-path root #P"publisher-recovery.log")))
      (write-legacy-path-test-file legacy-file "crash-safe")
      (ensure-directories-exist (migration-path barrier #P".keep"))
      #+sbcl
      (progn
        (sb-posix:chmod (namestring legacy-file) #o400)
        (sb-posix:chmod (namestring legacy-nested) #o555)
        (sb-posix:chmod (namestring legacy) #o555))
      (unwind-protect
           (let* ((base-command
                    (list
                     "env"
                     (format nil "RPLACA_QUICKLISP_SETUP=~A"
                             (or (uiop:getenv "RPLACA_QUICKLISP_SETUP")
                                 (error
                                  "RPLACA_QUICKLISP_SETUP is required")))
                     (format nil "RPLACA_TEST_REPO_ROOT=~A"
                             (namestring repo))
                     (format nil "RPLACA_TEST_CANONICAL_SESSIONS=~A"
                             (namestring canonical))
                     (format nil "RPLACA_TEST_LEGACY_SESSIONS=~A"
                             (namestring legacy))
                     (format nil "RPLACA_TEST_SESSION_BARRIER=~A"
                             (namestring barrier))
                     "RPLACA_TEST_SESSION_BARRIER_COUNT=1"))
                  (script-command
                    (list "sbcl" "--noinform" "--disable-debugger"
                          "--script" (namestring entry)))
                  (crash
                    (uiop:launch-program
                     (append base-command
                             (list
                              "RPLACA_TEST_SESSION_CRASH_BEFORE_PUBLISH=1")
                             script-command)
                     :output crash-output
                     :error-output :output)))
             (is (= 77 (uiop:wait-process crash)))
             (is-false (probe-file canonical))
             (is (probe-file
                  (rplaca::session-migration-lock-path canonical)))
             (is (find-if
                  (lambda (directory)
                    (search ".sessions-migration-" (namestring directory)))
                  (uiop:subdirectories
                   (uiop:pathname-parent-directory-pathname canonical))))
             (let ((recovery
                     (uiop:launch-program
                      (append base-command script-command)
                      :output recovery-output
                      :error-output :output)))
               (is (zerop (uiop:wait-process recovery))))
             ;; The crashed process left its lock file behind, but the kernel
             ;; released the advisory lock.  Recovery acquired that same file,
             ;; removed the stale staging tree, and published normally.
             (is (probe-file
                  (rplaca::session-migration-lock-path canonical)))
             (is (string= "crash-safe"
                          (uiop:read-file-string canonical-file)))
             #+sbcl
             (progn
               (is (= #o555 (rplaca::session-path-mode canonical)))
               (is (= #o555
                      (rplaca::session-path-mode canonical-nested)))
               (is (= #o400
                      (rplaca::session-path-mode canonical-file))))
             (is-false
              (find-if
               (lambda (directory)
                 (search ".sessions-migration-" (namestring directory)))
               (uiop:subdirectories
                (uiop:pathname-parent-directory-pathname canonical)))))
        #+sbcl
        (dolist (directory
                 (list canonical-nested canonical legacy-nested legacy))
          (when (probe-file directory)
            (sb-posix:chmod (namestring directory) #o700)))))))

(test prompts-and-model-metadata-use-one-project-local-root
  "Project prompt and model metadata fallback without merging roots."
  (with-migration-integration-root (root)
    (let* ((legacy-prompts (migration-path root #P".clawmacs/prompts/"))
           (canonical-prompts (migration-path root #P".rplaca/prompts/"))
           (legacy-model
             (migration-path root #P".clawmacs-modelaria.json"))
           (buffer (rplaca::make-buffer "migration-project"
                                       :kind :scratch
                                       :working-directory root)))
      (write-legacy-path-test-file
       (migration-path legacy-prompts #P"old.md") "legacy")
      (write-legacy-path-test-file
       legacy-model
       "{\"active_role\":\"cheap\",\"role_set\":[\"cheap\"]}")
      (is (equal legacy-prompts
                 (rplaca::project-prompt-template-directory buffer)))
      (is (string= "cheap"
                   (getf (rplaca::modelaria-project-config buffer)
                         :active-role)))
      (write-legacy-path-test-file
       (migration-path canonical-prompts #P"new.md") "canonical")
      (is (equal canonical-prompts
                 (rplaca::project-prompt-template-directory buffer))))))

(test global-boot-fallback-uses-relevant-file-surface
  "An empty canonical config directory does not mask legacy boot files."
  (with-migration-integration-root (root)
    (let* ((canonical (migration-path root #P"rplaca/"))
           (legacy (migration-path root #P"clawmacs/"))
           (project (migration-path root #P"project/"))
           (rplaca::*global-boot-directory* canonical)
           (rplaca::*legacy-global-boot-directory* legacy))
      (ensure-directories-exist (migration-path canonical #P".keep"))
      (ensure-directories-exist (migration-path project #P".keep"))
      (write-legacy-path-test-file
       (migration-path legacy #P"AGENTS.md") "legacy-global")
      (is (search "legacy-global"
                  (rplaca::load-boot-files :directory project)))
      (write-legacy-path-test-file
       (migration-path canonical #P"SOUL.md") "canonical-global")
      (let ((instructions
              (rplaca::load-boot-files :directory project)))
        (is (search "canonical-global" instructions))
        (is-false (search "legacy-global" instructions))))))

(test old-crash-reports-are-archival-only
  "Crash output is canonical; the legacy path is location-only archival data."
  (let ((canonical (rplaca::crash-report-directory))
        (legacy (rplaca::archived-legacy-crash-report-directory)))
    (is-false (equal canonical legacy))
    (is (search "/rplaca/crash-reports/" (namestring canonical)))
    (is (search "/clawmacs/crash-reports/" (namestring legacy)))))
