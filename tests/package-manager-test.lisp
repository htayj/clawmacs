(in-package :rplaca/tests)

(in-suite package-manager-suite)

(defvar *package-entrypoint-load-count* 0
  "Tracks how many times the test package entrypoint was loaded.")

(defvar *package-buffer-rendered* nil
  "Tracks custom package buffer presentation dispatch in tests.")

(defvar *package-init-continued* nil
  "Tracks whether load-user-init-file continued after a package warning.")

(defvar *startup-hook-ran* nil
  "Tracks whether the startup hook fired during rplaca-main tests.")

(defvar *initial-buffer-hook-ran* nil
  "Tracks whether the initial-buffer hook fired during rplaca-main tests.")

(defvar *initial-buffer-hook-binding* nil
  "Captures a key binding observed from the initial buffer hook.")

(defstruct concurrent-package-load-state
  lock
  entered
  release
  (count 0)
  (active 0)
  (max-active 0)
  (failures-left 0))

(defvar *concurrent-package-load-state* nil)

(defun concurrent-package-entrypoint (&optional label)
  "Barrier-controlled package entrypoint body for lifecycle race tests."
  (declare (ignore label))
  (let ((state *concurrent-package-load-state*))
    (unless state
      (error "Concurrent package load state is not installed."))
    (bt:with-lock-held ((concurrent-package-load-state-lock state))
      (incf (concurrent-package-load-state-count state))
      (incf (concurrent-package-load-state-active state))
      (setf (concurrent-package-load-state-max-active state)
            (max (concurrent-package-load-state-max-active state)
                 (concurrent-package-load-state-active state))))
    (unwind-protect
         (progn
           (bt:signal-semaphore (concurrent-package-load-state-entered state))
           (unless (bt:wait-on-semaphore
                    (concurrent-package-load-state-release state)
                    :timeout 5.0)
             (error "Timed out waiting for package lifecycle test release."))
           (let ((fail-p nil))
             (bt:with-lock-held ((concurrent-package-load-state-lock state))
               (when (plusp
                      (concurrent-package-load-state-failures-left state))
                 (decf (concurrent-package-load-state-failures-left state))
                 (setf fail-p t)))
             (when fail-p
               (error "Intentional package lifecycle test failure."))))
      (bt:with-lock-held ((concurrent-package-load-state-lock state))
        (decf (concurrent-package-load-state-active state))))))

(defun temp-package-test-directory (label)
  (make-pathname :directory (list :absolute "tmp"
                                  (format nil "rplaca-package-tests-~A-~36R-~36R-~A"
                                          label
                                          (get-universal-time)
                                          (get-internal-real-time)
                                          (gensym)))))

(defmacro with-packages-directory-override ((path) &body body)
  `(let ((rplaca::*packages-directory* (uiop:ensure-directory-pathname ,path))
         (rplaca::*package-configuration-path*
          (merge-pathnames "packages.json" (uiop:ensure-directory-pathname ,path)))
         (rplaca::*package-configuration* nil)
         (rplaca::*loaded-packages* (make-hash-table :test #'equal))
         (rplaca::*package-channels* nil)
         (rplaca::*available-packages* nil)
         (rplaca::*package-registry-loaded-p* nil)
         (rplaca::*package-prompt-sections* nil)
         (rplaca::*buffer-type-registry*
          (rplaca::make-buffer-type-registry)))
     ,@body))

(defmacro with-project-test-state ((root-var definitions-var) &body body)
  `(let* ((base (merge-pathnames
                 (format nil "rplaca-project-tests-~D-~D-~36R/"
                         (get-universal-time)
                         (get-internal-real-time)
                         (random (expt 36 8)))
                 #P"/tmp/"))
          (,root-var (merge-pathnames #P"root/" base))
          (,definitions-var (merge-pathnames #P"defs/" base))
          (rplaca::*project-registry* (make-hash-table :test #'equal))
          (rplaca::*project-definitions-directory* ,definitions-var)
          (rplaca::*project-definitions-loaded-p* nil)
          (rplaca::*buffer-ring* nil)
          (rplaca::*buffer-counter* 0))
     (ensure-directories-exist (merge-pathnames #P".keep" ,root-var))
     (ensure-directories-exist (merge-pathnames #P".keep" ,definitions-var))
     ,@body))

(defun default-package-test-channels ()
  (list (rplaca:make-package-channel
         :name "default"
         :root rplaca:*default-package-channel-directory*
         :description "Bundled RPLACA packages"
         :source :builtin)))

(defmacro with-package-state-override ((channels-form &key enabled-builtin-packages)
                                       &body body)
  (declare (ignore enabled-builtin-packages))
  `(let* ((package-test-root (temp-package-test-directory "config"))
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json"
                            (uiop:ensure-directory-pathname package-test-root)))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels* ,channels-form)
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
          (rplaca::*buffer-type-registry*
           (rplaca::make-buffer-type-registry))
          (rplaca::*buffer-input-presentation-providers* nil)
          (rplaca::*enabled-builtin-packages* nil)
          (rplaca::*advice-table* (make-hash-table :test #'eq)))
     ,@body))

(defmacro with-packrat-resource-state (() &body body)
  `(let ((rplaca::*command-table* (make-hash-table :test #'eq))
         (rplaca::*extended-docs* (make-hash-table :test #'eq))
         (rplaca::*agent-tool-metadata-table* (make-hash-table :test #'eq))
         (rplaca::*agent-tool-name-table* (make-hash-table :test #'equal))
         (rplaca::*hook-metadata-table* (make-hash-table :test #'eq))
         (rplaca::*advice-table* (make-hash-table :test #'eq))
         (rplaca::*slash-command-table* (make-hash-table :test #'equal))
         (rplaca::*buffer-type-registry*
          (rplaca::make-buffer-type-registry))
         (rplaca::*buffer-input-presentation-providers* nil)
         (rplaca::*package-prompt-sections* nil)
         (rplaca::*current-package-resource-types* nil))
     ,@body))

(defun write-test-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(defun make-concurrent-package-definition (label)
  "Create a temporary package whose entrypoint waits on the lifecycle barrier."
  (let* ((root (uiop:ensure-directory-pathname
                (temp-package-test-directory label)))
         (entrypoint (merge-pathnames "entrypoint.lisp" root)))
    (write-test-file
     entrypoint
     (format nil
             "(rplaca/tests::concurrent-package-entrypoint ~S)~%"
             label))
    (rplaca:make-package-definition
     :name label
     :description "Concurrent lifecycle test package."
     :root root
     :entrypoint entrypoint
     :dependencies nil
     :autoload nil)))

(defmacro with-concurrent-package-load-state (() &body body)
  "Install fresh process-visible synchronization state for lifecycle tests."
  `(let ((old-state *concurrent-package-load-state*)
         (state
           (make-concurrent-package-load-state
            :lock (bt:make-lock "concurrent package load state")
            :entered (bt:make-semaphore
                      :name "concurrent package load entered")
            :release (bt:make-semaphore
                      :name "concurrent package load release"))))
     (unwind-protect
          (progn
            (setf *concurrent-package-load-state* state)
            ,@body)
       (setf *concurrent-package-load-state* old-state))))

(defun make-concurrent-package-loader-thread
    (definition loaded-table start ready results-cell result-lock name)
  "Create one synchronized direct package entrypoint loader."
  (bt:make-thread
   (lambda ()
     (bt:signal-semaphore ready)
     (unless (bt:wait-on-semaphore start :timeout 5.0)
       (error "Timed out waiting for package loader start."))
     (let ((rplaca::*loaded-packages* loaded-table))
       (let ((result
               (rplaca::load-package-definition-entrypoint definition)))
         (bt:with-lock-held (result-lock)
           (push result (car results-cell))))))
   :name name))

(defmacro with-process-package-configuration ((path-var) &body body)
  "Install and restore actual process globals so worker threads share COW state."
  `(let* ((old-path rplaca::*package-configuration-path*)
          (old-write-function rplaca::*package-configuration-write-function*)
          (old-configuration
            (bt:with-lock-held (rplaca::*package-configuration-lock*)
              rplaca::*package-configuration*))
          (root (uiop:ensure-directory-pathname
                 (temp-package-test-directory "concurrent-config")))
          (,path-var (merge-pathnames "packages.json" root)))
     (unwind-protect
          (progn
            (setf rplaca::*package-configuration-path* ,path-var
                  rplaca::*package-configuration-write-function* nil)
            (bt:with-lock-held (rplaca::*package-configuration-lock*)
              (setf rplaca::*package-configuration* nil))
            ,@body)
       (bt:with-lock-held (rplaca::*package-configuration-lock*)
         (setf rplaca::*package-configuration* old-configuration))
       (setf rplaca::*package-configuration-path* old-path
             rplaca::*package-configuration-write-function*
             old-write-function)
       (when (probe-file root)
         (uiop:delete-directory-tree root
                                     :validate t
                                     :if-does-not-exist :ignore)))))

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
  (run-git-command repo-root "config" "user.name" "RPLACA Test")
  (run-git-command repo-root "config" "user.email" "rplaca-tests@example.com"))

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
  (format nil "(in-package :rplaca)

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
  (let ((rplaca::*startup-hook* nil))
    (rplaca:add-hook 'rplaca::*startup-hook* 'identity)
    (rplaca:add-hook 'rplaca::*startup-hook* 'identity)
    (rplaca:add-hook 'rplaca::*startup-hook* 'car :append t)
    (is (equal '(identity car) rplaca::*startup-hook*))
    (rplaca:remove-hook 'rplaca::*startup-hook* 'identity)
    (is (equal '(car) rplaca::*startup-hook*))))

(test rplaca-use-package-clones-and-installs-local-git-repo
  "A local git repo is cloned and read via manifest.lisp without loading."
  (let* ((*package-entrypoint-load-count* 0)
         (source-repo
           (make-package-repo
            :label "happy"
            :manifest "(:name \"test-package\" :entrypoint \"test-package.lisp\")"
            :entrypoint-content "(incf rplaca/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "install-root")))
    (with-packages-directory-override (packages-root)
      (let ((definition (rplaca:rplaca-use-package
                         :src-type :git
                         :repo (namestring source-repo))))
        (is (not (null definition)))
        (is (string= "test-package"
                     (rplaca:package-definition-name definition))))
      (is (= 0 *package-entrypoint-load-count*))
      (is (not (null (rplaca:find-installed-package "test-package"))))
      (is (probe-file
           (rplaca::package-install-directory :git (namestring source-repo)))))))

(test rplaca-use-package-is-install-only-and-idempotent
  "Repeated install calls do not load the package entrypoint."
  (let* ((*package-entrypoint-load-count* 0)
         (source-repo
           (make-package-repo
            :label "idempotent"
            :manifest "(:name test-package :entrypoint \"test-package.lisp\")"
            :entrypoint-content "(incf rplaca/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "install-root")))
    (with-packages-directory-override (packages-root)
      (is (rplaca:rplaca-use-package :src-type :git
                                         :repo (namestring source-repo)))
      (is (rplaca:rplaca-use-package :src-type :git
                                         :repo (namestring source-repo)))
      (is (= 0 *package-entrypoint-load-count*)))))

(test load-active-packages-loads-enabled-installed-package-once
  "Installed packages load only after enablement and only once per session."
  (let* ((*package-entrypoint-load-count* 0)
         (source-repo
           (make-package-repo
            :label "active-load"
            :manifest "(:name \"active-package\" :entrypoint \"test-package.lisp\")"
            :entrypoint-content "(incf rplaca/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "active-install-root")))
    (with-packages-directory-override (packages-root)
      (is (rplaca:rplaca-use-package :src-type :git
                                         :repo (namestring source-repo)))
      (is (null (rplaca:load-active-packages)))
      (is (= 0 *package-entrypoint-load-count*))
      (is (eq :global
              (rplaca:set-package-enablement-scope
               "active-package" :global)))
      (is (= 1 (length (rplaca:load-active-packages))))
      (is (= 1 *package-entrypoint-load-count*))
      (is (= 1 (length (rplaca:load-active-packages))))
      (is (= 1 *package-entrypoint-load-count*)))))

(test concurrent-cold-load-is-exact-once-and-wakes-three-contenders
  "Three simultaneous callers execute one package entrypoint exactly once."
  (with-concurrent-package-load-state ()
    (let* ((definition (make-concurrent-package-definition "same-package"))
           (loaded-table (make-hash-table :test #'equal))
           (start (bt:make-semaphore :name "same package start"))
           (ready (bt:make-semaphore :name "same package ready"))
           (result-lock (bt:make-lock "same package results"))
           (results-cell (list nil))
           (threads
             (loop :for index :below 3
                   :collect
                   (make-concurrent-package-loader-thread
                    definition loaded-table start ready results-cell result-lock
                    (format nil "same-package-loader-~D" index)))))
      (unwind-protect
           (progn
             (dotimes (_ 3)
               (declare (ignore _))
               (is-true (bt:wait-on-semaphore ready :timeout 2.0)))
             (bt:signal-semaphore start :count 3)
             (is-true
              (bt:wait-on-semaphore
               (concurrent-package-load-state-entered
                *concurrent-package-load-state*)
               :timeout 2.0))
             (is-false
              (bt:wait-on-semaphore
               (concurrent-package-load-state-entered
                *concurrent-package-load-state*)
               :timeout 0.05))
             (bt:signal-semaphore
              (concurrent-package-load-state-release
               *concurrent-package-load-state*))
             (dolist (thread threads)
               (bt:join-thread thread))
             (is (= 1 (concurrent-package-load-state-count
                       *concurrent-package-load-state*)))
             (is (= 1 (concurrent-package-load-state-max-active
                       *concurrent-package-load-state*)))
             (is (= 3 (length (car results-cell))))
             (is (every (lambda (result) (eq result definition))
                        (car results-cell))))
        (bt:signal-semaphore
         (concurrent-package-load-state-release
          *concurrent-package-load-state*)
         :count 3)
        (dolist (thread threads)
          (when (bt:thread-alive-p thread)
            (bt:join-thread thread)))))))

(test concurrent-different-package-loads-are-serialized
  "Different entrypoints never execute concurrently in the shared Lisp image."
  (with-concurrent-package-load-state ()
    (let* ((first (make-concurrent-package-definition "different-first"))
           (second (make-concurrent-package-definition "different-second"))
           (loaded-table (make-hash-table :test #'equal))
           (start (bt:make-semaphore :name "different package start"))
           (ready (bt:make-semaphore :name "different package ready"))
           (result-lock (bt:make-lock "different package results"))
           (results-cell (list nil))
           (threads
             (list
              (make-concurrent-package-loader-thread
               first loaded-table start ready results-cell result-lock
               "different-package-loader-first")
              (make-concurrent-package-loader-thread
               second loaded-table start ready results-cell result-lock
               "different-package-loader-second"))))
      (unwind-protect
           (progn
             (dotimes (_ 2)
               (declare (ignore _))
               (is-true (bt:wait-on-semaphore ready :timeout 2.0)))
             (bt:signal-semaphore start :count 2)
             (is-true
              (bt:wait-on-semaphore
               (concurrent-package-load-state-entered
                *concurrent-package-load-state*)
               :timeout 2.0))
             (is-false
              (bt:wait-on-semaphore
               (concurrent-package-load-state-entered
                *concurrent-package-load-state*)
               :timeout 0.05))
             (bt:signal-semaphore
              (concurrent-package-load-state-release
               *concurrent-package-load-state*))
             (is-true
              (bt:wait-on-semaphore
               (concurrent-package-load-state-entered
                *concurrent-package-load-state*)
               :timeout 2.0))
             (bt:signal-semaphore
              (concurrent-package-load-state-release
               *concurrent-package-load-state*))
             (dolist (thread threads)
               (bt:join-thread thread))
             (is (= 2 (concurrent-package-load-state-count
                       *concurrent-package-load-state*)))
             (is (= 1 (concurrent-package-load-state-max-active
                       *concurrent-package-load-state*)))
             (is (= 2 (hash-table-count loaded-table))))
        (bt:signal-semaphore
         (concurrent-package-load-state-release
          *concurrent-package-load-state*)
         :count 2)
        (dolist (thread threads)
          (when (bt:thread-alive-p thread)
            (bt:join-thread thread)))))))

(test failed-cold-load-releases-owner-for-one-serialized-retry
  "An entrypoint error clears ownership and lets one waiter retry safely."
  (with-concurrent-package-load-state ()
    (setf (concurrent-package-load-state-failures-left
           *concurrent-package-load-state*)
          1)
    (let* ((definition (make-concurrent-package-definition "failure-release"))
           (loaded-table (make-hash-table :test #'equal))
           (start (bt:make-semaphore :name "failure release start"))
           (ready (bt:make-semaphore :name "failure release ready"))
           (result-lock (bt:make-lock "failure release results"))
           (results-cell (list nil))
           (threads
             (loop :for index :below 2
                   :collect
                   (make-concurrent-package-loader-thread
                    definition loaded-table start ready results-cell result-lock
                    (format nil "failure-release-loader-~D" index)))))
      (unwind-protect
           (progn
             (dotimes (_ 2)
               (declare (ignore _))
               (is-true (bt:wait-on-semaphore ready :timeout 2.0)))
             (bt:signal-semaphore start :count 2)
             (is-true
              (bt:wait-on-semaphore
               (concurrent-package-load-state-entered
                *concurrent-package-load-state*)
               :timeout 2.0))
             (bt:signal-semaphore
              (concurrent-package-load-state-release
               *concurrent-package-load-state*))
             (is-true
              (bt:wait-on-semaphore
               (concurrent-package-load-state-entered
                *concurrent-package-load-state*)
               :timeout 2.0))
             (bt:signal-semaphore
              (concurrent-package-load-state-release
               *concurrent-package-load-state*))
             (dolist (thread threads)
               (bt:join-thread thread))
             (is (= 2 (concurrent-package-load-state-count
                       *concurrent-package-load-state*)))
             (is (= 1 (concurrent-package-load-state-max-active
                       *concurrent-package-load-state*)))
             (is (= 1 (count nil (car results-cell))))
             (is (= 1 (count definition (car results-cell) :test #'eq)))
             (is (= 1 (hash-table-count loaded-table)))
             (is-false (rplaca::package-lifecycle-operation-active-p)))
        (bt:signal-semaphore
         (concurrent-package-load-state-release
          *concurrent-package-load-state*)
         :count 2)
        (dolist (thread threads)
          (when (bt:thread-alive-p thread)
            (bt:join-thread thread)))))))

(test busy-package-remove-refuses-before-deleting-files
  "Destructive package maintenance signals before changing an install tree."
  (let* ((source (make-packrat-resource-package-source
                  :label "busy-remove"
                  :version "v1"))
         (packages-root (temp-package-test-directory "busy-remove-root")))
    (unwind-protect
         (with-packages-directory-override (packages-root)
           (let* ((definition
                    (rplaca:rplaca-use-package
                     :src-type :path
                     :repo (namestring source)))
                  (root (rplaca:package-definition-root definition))
                  (buffer (make-buffer "busy-package-remove")))
             (let ((rplaca::*buffer-ring* (list buffer)))
               (setf (buffer-status buffer) :oauth)
               (signals rplaca::package-runtime-maintenance-refused
                 (rplaca:remove-installed-package
                  definition :buffer buffer))
               (is (probe-file root))
               (setf (buffer-status buffer) :idle)
               (is (rplaca:remove-installed-package
                    definition :buffer buffer))
               (is-false (probe-file root)))))
      (when (probe-file source)
        (uiop:delete-directory-tree source
                                    :validate t
                                    :if-does-not-exist :ignore)))))

(test busy-package-reload-preserves-committed-registrations
  "Reload refusal happens before reset removes the committed package surface."
  (let* ((*package-entrypoint-load-count* 0)
         (definition (make-concurrent-package-definition "busy-reload"))
         (loaded-table (make-hash-table :test #'equal))
         (section
           (rplaca::make-package-prompt-section
            :name "busy-reload"
            :package "busy-reload"
            :body "committed"))
         (buffer (make-buffer "busy-package-reload")))
    ;; Replace the barrier entrypoint with a nonblocking committed load.
    (write-test-file
     (rplaca:package-definition-entrypoint definition)
     "(incf rplaca/tests::*package-entrypoint-load-count*)")
    (let ((rplaca::*loaded-packages* loaded-table)
          (rplaca::*package-prompt-sections* (list section))
          (rplaca::*buffer-ring* (list buffer)))
      (is (rplaca::load-package-definition-entrypoint definition))
      (is (= 1 *package-entrypoint-load-count*))
      (setf (buffer-status buffer) :oauth)
      (signals rplaca::package-runtime-maintenance-refused
        (rplaca::reload-rplaca-package definition))
      (is (= 1 *package-entrypoint-load-count*))
      (is (eq section (first rplaca::*package-prompt-sections*)))
      (is (gethash
           (rplaca::package-install-key
            (rplaca:package-definition-root definition))
           loaded-table)))))

(test default-package-channel-discovers-bundled-packages
  "The bundled default channel advertises the built-in packages."
  (with-package-state-override ((default-package-test-channels))
    (let* ((definitions (rplaca:reload-package-channels))
           (names (sort (mapcar #'rplaca:package-definition-name definitions)
                        #'string<))
           (git (find "git" definitions
                      :key #'rplaca:package-definition-name
                      :test #'string=))
           (lispi (find "lispi" definitions
                        :key #'rplaca:package-definition-name
                        :test #'string=))
           (netcons (find "netcons" definitions
                          :key #'rplaca:package-definition-name
                          :test #'string=))
           (organa (find "organa" definitions
                         :key #'rplaca:package-definition-name
                         :test #'string=))
           (packrat (find "packrat" definitions
                          :key #'rplaca:package-definition-name
                          :test #'string=))
           (pipelines (find "pipelines" definitions
                            :key #'rplaca:package-definition-name
                            :test #'string=))
           (prove (find "prove" definitions
                        :key #'rplaca:package-definition-name
                        :test #'string=))
           (quaestor (find "quaestor" definitions
                           :key #'rplaca:package-definition-name
                           :test #'string=))
           (modelaria (find "modelaria" definitions
                           :key #'rplaca:package-definition-name
                           :test #'string=))
           (media (find "media" definitions
                        :key #'rplaca:package-definition-name
                        :test #'string=))
           (artifactum (find "artifactum" definitions
                             :key #'rplaca:package-definition-name
                             :test #'string=))
           (speculum (find "speculum" definitions
                           :key #'rplaca:package-definition-name
                           :test #'string=))
           (sexed (find "sexed" definitions
                        :key #'rplaca:package-definition-name
                        :test #'string=))
           (slop (find "slop" definitions
                       :key #'rplaca:package-definition-name
                       :test #'string=))
           (subagent (find "subagent" definitions
                           :key #'rplaca:package-definition-name
                           :test #'string=))
           (templata (find "templata" definitions
                           :key #'rplaca:package-definition-name
                           :test #'string=)))
      (is (equal '("artifactum" "codex-image" "git" "lispi" "mcp-bridge" "media" "modelaria"
                   "netcons" "organa" "packrat" "pipelines" "prove"
                   "quaestor" "sexed" "slop" "speculum" "subagent"
                   "templata")
                 names))
      (is (not (null git)))
      (is (eq :builtin (rplaca:package-definition-source-tier git)))
      (is (rplaca:package-definition-autoload git))
      (is (probe-file (rplaca:package-definition-entrypoint git)))
      (is (not (null lispi)))
      (is (eq :builtin (rplaca:package-definition-source-tier lispi)))
      (is (probe-file (rplaca:package-definition-entrypoint lispi)))
      (is (not (null netcons)))
      (is (eq :builtin (rplaca:package-definition-source-tier netcons)))
      (is (rplaca:package-definition-autoload netcons))
      (is (probe-file (rplaca:package-definition-entrypoint netcons)))
      (is (not (null organa)))
      (is (eq :builtin (rplaca:package-definition-source-tier organa)))
      (is (rplaca:package-definition-autoload organa))
      (is (probe-file (rplaca:package-definition-entrypoint organa)))
      (is (not (null packrat)))
      (is (eq :builtin (rplaca:package-definition-source-tier packrat)))
      (is (not (rplaca:package-definition-autoload packrat)))
      (is (probe-file (rplaca:package-definition-entrypoint packrat)))
      (is (not (null pipelines)))
      (is (eq :builtin (rplaca:package-definition-source-tier pipelines)))
      (is (rplaca:package-definition-autoload pipelines))
      (is (probe-file (rplaca:package-definition-entrypoint pipelines)))
      (is (not (null prove)))
      (is (eq :builtin (rplaca:package-definition-source-tier prove)))
      (is (rplaca:package-definition-autoload prove))
      (is (probe-file (rplaca:package-definition-entrypoint prove)))
      (is (not (null quaestor)))
      (is (eq :builtin (rplaca:package-definition-source-tier quaestor)))
      (is (rplaca:package-definition-autoload quaestor))
      (is (probe-file (rplaca:package-definition-entrypoint quaestor)))
      (is (not (null modelaria)))
      (is (eq :builtin (rplaca:package-definition-source-tier modelaria)))
      (is (rplaca:package-definition-autoload modelaria))
      (is (probe-file (rplaca:package-definition-entrypoint modelaria)))
      (is (not (null media)))
      (is (eq :builtin (rplaca:package-definition-source-tier media)))
      (is (rplaca:package-definition-autoload media))
      (is (probe-file (rplaca:package-definition-entrypoint media)))
      (is (not (null artifactum)))
      (is (eq :builtin (rplaca:package-definition-source-tier artifactum)))
      (is (rplaca:package-definition-autoload artifactum))
      (is (probe-file (rplaca:package-definition-entrypoint artifactum)))
      (is (not (null speculum)))
      (is (eq :builtin (rplaca:package-definition-source-tier speculum)))
      (is (rplaca:package-definition-autoload speculum))
      (is (probe-file (rplaca:package-definition-entrypoint speculum)))
      (is (not (null sexed)))
      (is (eq :builtin (rplaca:package-definition-source-tier sexed)))
      (is (rplaca:package-definition-autoload sexed))
      (is (probe-file (rplaca:package-definition-entrypoint sexed)))
      (is (not (null slop)))
      (is (eq :builtin (rplaca:package-definition-source-tier slop)))
      (is (rplaca:package-definition-autoload slop))
      (is (probe-file (rplaca:package-definition-entrypoint slop)))
      (is (not (null subagent)))
      (is (eq :builtin (rplaca:package-definition-source-tier subagent)))
      (is (rplaca:package-definition-autoload subagent))
      (is (probe-file (rplaca:package-definition-entrypoint subagent)))
      (is (not (null templata)))
      (is (eq :builtin (rplaca:package-definition-source-tier templata)))
      (is (rplaca:package-definition-autoload templata))
      (is (probe-file (rplaca:package-definition-entrypoint templata)))
      (is (not (null (find "packrat" definitions
                           :key #'rplaca:package-definition-name
                           :test #'string=))))
      (is (eq :builtin
              (rplaca:package-definition-source-tier
               (find "packrat" definitions
                     :key #'rplaca:package-definition-name
                     :test #'string=)))))))

(test load-autoload-packages-skips-disabled-builtin-sexed
  "Bundled sexed stays discoverable but does not autoload by default."
  (with-package-state-override ((default-package-test-channels))
    (let ((loaded (rplaca:load-autoload-packages)))
      (is (null loaded))
      (is (not (null (rplaca:find-available-package "sexed"))))
      (is (not (null (rplaca:find-available-package "slop"))))
      (is (not (null (rplaca:find-available-package "netcons"))))
      (is (not (null (rplaca:find-available-package "organa"))))
      (is (not (null (rplaca:find-available-package "modelaria"))))
      (is (not (null (rplaca:find-available-package "artifactum"))))
      (is (not (null (rplaca:find-available-package "codex-image"))))
      (is (not (null (rplaca:find-available-package "pipelines"))))
      (is (not (null (rplaca:find-available-package "prove"))))
      (is (not (null (rplaca:find-available-package "quaestor"))))
      (is (not (null (rplaca:find-available-package "speculum"))))
      (is (not (null (rplaca:find-available-package "subagent"))))
      (is (not (null (rplaca:find-available-package "templata"))))
      (is (not (null (rplaca:find-available-package "packrat"))))
      (is (null (rplaca:render-package-prompt-sections))))))

(test load-autoload-packages-registers-enabled-pipelines-surface
  "The bundled pipelines package owns its prompt, commands, and docs."
  (with-package-state-override ((default-package-test-channels))
    (rplaca:set-package-enablement-scope "pipelines" :global)
    (let ((loaded (rplaca:load-autoload-packages)))
      (is (= 1 (length loaded)))
      (is (string= "pipelines"
                   (rplaca:package-definition-name (first loaded))))
      (let ((prompt-section (rplaca:render-package-prompt-sections)))
        (is (search "Deterministic pipelines" prompt-section))
        (is (search "define-pipeline" prompt-section))
        (is (search "defpipeline" prompt-section))
        (is (search "self-modify" prompt-section))
        (is (search "packages and skills" prompt-section)))
      (is (member 'rplaca:set-buffer-pipeline
                  (rplaca:list-available-commands)
                  :test #'eq))
      (is (member 'rplaca:clear-buffer-pipeline
                  (rplaca:list-available-commands)
                  :test #'eq))
      (is (string= "pipelines"
                   (rplaca:command-metadata-package
                    (gethash 'rplaca:set-buffer-pipeline
                             rplaca::*command-table*))))
      (is (string= "pipelines"
                   (getf (rplaca:extended-doc 'rplaca:define-pipeline)
                         :package))))))

(test load-autoload-packages-registers-enabled-sexed-prompt-section
  "Explicitly enabled bundled packages still register their prompt contributions."
  (with-package-state-override ((default-package-test-channels))
    (rplaca:set-package-enablement-scope "sexed" :global)
    (let ((loaded (rplaca:load-autoload-packages)))
      (is (= 1 (length loaded)))
      (let ((prompt-section (rplaca:render-package-prompt-sections)))
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
              (rplaca:package-enablement-scope "sexed" :buffer buf)))
      (is (equal nil (rplaca:active-package-names :buffer buf)))
      (rplaca:set-package-enablement-scope "sexed" :global :buffer buf)
      (is (eq :global
              (rplaca:package-enablement-scope "sexed" :buffer buf)))
      (is (member "sexed" (rplaca:active-package-names :buffer buf)
                  :test #'string=))
      (rplaca:set-package-enablement-scope "sexed" :agent :buffer buf)
      (is (eq :agent
              (rplaca:package-enablement-scope "sexed" :buffer buf)))
      (is (not (rplaca::package-enabled-globally-p "sexed")))
      (rplaca:set-package-enablement-scope "sexed" :buffer :buffer buf)
      (is (eq :buffer
              (rplaca:package-enablement-scope "sexed" :buffer buf)))
      (is (not (rplaca::package-enabled-for-agent-p "sexed" "coder")))
      (rplaca:set-package-enablement-scope "sexed" :default :buffer buf)
      (is (eq :default
              (rplaca:package-enablement-scope "sexed" :buffer buf)))
      (is (equal nil (rplaca:active-package-names :buffer buf))))))

(test package-enablement-configuration-persists-global-and-agent
  "Global and agent package enablement round-trip through packages.json."
  (with-package-state-override ((default-package-test-channels))
    (let ((path rplaca::*package-configuration-path*))
      (rplaca:set-package-enablement-scope "sexed" :global)
      (rplaca:set-package-enablement-scope "lispi" :agent
                                            :agent-name "coder")
      (setf rplaca::*package-configuration* nil)
      (is (probe-file path))
      (is (eq :global
              (rplaca:package-enablement-scope "sexed")))
      (is (eq :agent
              (rplaca:package-enablement-scope "lispi"
                                                 :agent-name "coder")))
      (is (equal '("lispi" "sexed")
                 (sort (copy-list
                        (rplaca:active-package-names
                        :agent-name "coder"))
                       #'string<))))))

(test concurrent-package-enablement-cow-commits-do-not-lose-updates
  "Concurrent writers serialize snapshots, commits, and atomic file replacement."
  (with-process-package-configuration (path)
    (let ((ready (bt:make-semaphore :name "package config writers ready"))
          (start (bt:make-semaphore :name "package config writers start"))
          (errors-cell (list nil))
          (errors-lock (bt:make-lock "package config writer errors"))
          (names (loop :for index :below 8
                       :collect (format nil "race-package-~D" index))))
      (let ((threads
              (mapcar
               (lambda (name)
                 (bt:make-thread
                  (lambda ()
                    (bt:signal-semaphore ready)
                    (unless (bt:wait-on-semaphore start :timeout 5.0)
                      (error "Timed out waiting for package config start."))
                    (handler-case
                        (rplaca:set-package-enablement-scope name :global)
                      (error (condition)
                        (bt:with-lock-held (errors-lock)
                          (push condition (car errors-cell))))))
                  :name (format nil "package-config-writer-~A" name)))
               names)))
        (unwind-protect
             (progn
               (dotimes (_ (length names))
                 (declare (ignore _))
                 (is-true (bt:wait-on-semaphore ready :timeout 2.0)))
               (bt:signal-semaphore start :count (length names))
               (dolist (thread threads)
                 (bt:join-thread thread))
               (is (null (car errors-cell)))
               (let ((active (rplaca:active-package-names)))
                 (dolist (name names)
                   (is (member name active :test #'string=))))
               (is (probe-file path))
               (is (listp
                    (cl-json:decode-json-from-string
                     (uiop:read-file-string path)))))
          (bt:signal-semaphore start :count (length names))
          (dolist (thread threads)
            (when (bt:thread-alive-p thread)
              (bt:join-thread thread))))))))

(test failed-package-configuration-write-does-not-publish-memory
  "A failed atomic write leaves the prior in-memory generation unchanged."
  (with-process-package-configuration (path)
    (rplaca:set-package-enablement-scope "stable-package" :global)
    (let ((before (uiop:read-file-string path)))
      (let ((rplaca::*package-configuration-write-function*
              (lambda (json target)
                (declare (ignore json target))
                (error "Injected package configuration write failure."))))
        (signals error
          (rplaca:set-package-enablement-scope "uncommitted-package"
                                                :global)))
      (is (rplaca::package-enabled-globally-p "stable-package"))
      (is-false
       (rplaca::package-enabled-globally-p "uncommitted-package"))
      (is (string= before (uiop:read-file-string path))))))

(test cycle-package-enablement-scope-uses-simple-cycle
  "Package scope cycling avoids explicit disable chains."
  (with-package-state-override ((default-package-test-channels))
    (let ((buf (make-buffer "pkg-cycle" :agent-name "coder")))
      (is (eq :buffer
              (rplaca:cycle-package-enablement-scope "sexed"
                                                       :buffer buf)))
      (is (eq :agent
              (rplaca:cycle-package-enablement-scope "sexed"
                                                       :buffer buf)))
      (is (eq :global
              (rplaca:cycle-package-enablement-scope "sexed"
                                                       :buffer buf)))
      (is (eq :default
              (rplaca:cycle-package-enablement-scope "sexed"
                                                       :buffer buf))))))

(test package-enablement-refreshes-system-prompt-header
  "Changing package enablement refreshes the synthetic system-prompt header."
  (with-package-state-override ((default-package-test-channels))
    (let ((original (symbol-function 'rplaca:build-agent-system-prompt)))
      (unwind-protect
           (progn
             (setf (symbol-function 'rplaca:build-agent-system-prompt)
                   (lambda (agent-name &key buffer)
                     (declare (ignore agent-name))
                     (format nil "SEXED SCOPE: ~A"
                             (rplaca:package-enablement-scope "sexed"
                                                                 :buffer buffer))))
             (let ((buf (rplaca:make-chat-buffer "package-prompt-header"
                                                   :session-persistence-mode
                                                   :ephemeral)))
               (is (search "SEXED SCOPE: DEFAULT"
                           (message-text (buffer-first-message buf))))
               (rplaca:set-package-enablement-scope "sexed" :global :buffer buf)
               (is (search "SEXED SCOPE: GLOBAL"
                           (message-text (buffer-first-message buf))))))
        (setf (symbol-function 'rplaca:build-agent-system-prompt)
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
            "(incf rplaca/tests::*package-entrypoint-load-count*)")))
    (with-package-state-override (nil)
      (rplaca:register-package-channel "custom" channel-root
                                         :description "Custom channel")
      (let ((definition (rplaca:find-available-package "custom-package")))
        (is (not (null definition)))
        (is (eq :channel (rplaca:package-definition-source-tier definition)))
        (is (rplaca:load-rplaca-package "custom-package"))
        (is (= 1 *package-entrypoint-load-count*))
        (is (null (rplaca:render-package-prompt-sections)))
        (rplaca:set-package-enablement-scope "custom-package" :global)
        (is (search "CUSTOM PACKAGE PROMPT"
                    (rplaca:render-package-prompt-sections)))
        (is (equal '("Custom package prompt")
                   (mapcar #'rplaca:package-prompt-section-title
                           (rplaca:package-owned-prompt-sections
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
            "(defun package-dashboard-presenter (buffer columns)
  (list (list :text (format nil \"rendered ~A at ~D\"
                            (buffer-name buffer)
                            columns))))

(define-buffer-type :package-dashboard
  :description \"Package dashboard buffer\"
  :major-mode \"dashboard\"
  :presentation-function 'package-dashboard-presenter)")))
    (with-package-state-override (nil)
      (rplaca:register-package-channel "custom" channel-root
                                         :description "Custom channel")
      (is (rplaca:load-rplaca-package "dashboard-package"))
      (let ((type (rplaca:find-buffer-type :package-dashboard)))
        (is (not (null type)))
        (is (string= "dashboard-package"
                     (rplaca:buffer-type-package type)))
        (is (string= "dashboard"
                     (rplaca:buffer-type-major-mode type)))
        (is (eq 'rplaca::package-dashboard-presenter
                (rplaca:buffer-type-presentation-function type))))
      (is (= 1 (length (rplaca:package-owned-buffer-types
                        "dashboard-package"))))
      (let ((help (rplaca:describe-installed-package-to-string
                   (rplaca:find-installed-package "dashboard-package")
                   nil)))
        (is (search "Buffer Types:" help))
        (is (search "package-dashboard" help :test #'char-equal))))))

(test package-dashboard-display-entries-include-installed-packages
  "The package dashboard exposes installed packages as presented entries."
  (with-package-state-override ((default-package-test-channels))
    (let* ((origin (make-buffer "chat" :agent-name "coder"))
           (dashboard (make-buffer "*Packages*" :kind :package-dashboard
                                   :agent-name "packages")))
      (setf (rplaca::package-dashboard-origin-buffer dashboard) origin)
      (let* ((entries (rplaca::package-dashboard-display-entries dashboard))
             (sexed (find-if (lambda (entry)
                               (and (eq 'rplaca::package-dashboard-entry-ref
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
                        (rplaca::package-doctor-report :buffer origin)
                        :key (lambda (item) (getf item :name))
                        :test #'string=)))
      (setf (rplaca::package-dashboard-origin-buffer dashboard) origin)
      (is (eq :buffer
              (rplaca::package-dashboard-toggle-entry dashboard entry
                                                        :origin-buffer origin)))
      (is (eq :buffer
              (rplaca:package-enablement-scope "sexed" :buffer origin)))
      (is (member "sexed" (buffer-enabled-packages origin) :test #'string=)))))

(test rplaca-use-package-honors-packages-directory-override
  "The install root follows *packages-directory*."
  (let* ((*package-entrypoint-load-count* 0)
         (source-repo
           (make-package-repo
            :label "override"
            :manifest "(:name \"override-package\" :entrypoint \"test-package.lisp\")"
            :entrypoint-content "(incf rplaca/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "custom-root")))
    (with-packages-directory-override (packages-root)
      (is (rplaca:rplaca-use-package :src-type :git
                                         :repo (namestring source-repo)))
      (let ((install-dir
              (rplaca::package-install-directory :git (namestring source-repo))))
        (is (not (null (probe-file install-dir))))
        (is (equal (namestring (uiop:ensure-directory-pathname packages-root))
                   (subseq (namestring install-dir)
                           0
                           (length (namestring (uiop:ensure-directory-pathname packages-root))))))))))

(test rplaca-use-package-rejects-unsupported-source-types
  "Unsupported source types return NIL without signaling."
  (let ((packages-root (temp-package-test-directory "source-type")))
    (with-packages-directory-override (packages-root)
      (is (null (rplaca:rplaca-use-package :src-type :github
                                               :repo "owner/repo"))))))

(test rplaca-use-package-fails-when-manifest-is-missing
  "Packages without manifest.lisp return NIL."
  (let* ((source-repo
           (make-package-repo
            :label "missing-manifest"
            :entrypoint-content "(incf rplaca/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "missing-manifest-root")))
    (with-packages-directory-override (packages-root)
      (is (null (rplaca:rplaca-use-package :src-type :git
                                               :repo (namestring source-repo)))))))

(test rplaca-use-package-fails-when-manifest-is-malformed
  "Packages with a non-plist manifest return NIL."
  (let* ((source-repo
           (make-package-repo
            :label "bad-manifest"
            :manifest "(this is not a plist)"
            :entrypoint-content "(incf rplaca/tests::*package-entrypoint-load-count*)"))
         (packages-root (temp-package-test-directory "bad-manifest-root")))
    (with-packages-directory-override (packages-root)
      (is (null (rplaca:rplaca-use-package :src-type :git
                                               :repo (namestring source-repo)))))))

(test rplaca-use-package-fails-when-entrypoint-is-missing
  "Manifest entrypoints must exist inside the repo."
  (let* ((source-repo
           (make-package-repo
            :label "missing-entrypoint"
            :manifest "(:name \"missing-entry\" :entrypoint \"missing.lisp\")"))
         (packages-root (temp-package-test-directory "missing-entrypoint-root")))
    (with-packages-directory-override (packages-root)
      (is (null (rplaca:rplaca-use-package :src-type :git
                                               :repo (namestring source-repo)))))))

(test rplaca-use-package-installs-path-packages-and-records-metadata
  "Local-path installs copy the package, persist a record, and report scope."
  (let* ((source (make-packrat-resource-package-source
                  :label "packrat-local"
                  :version "v1"))
         (packages-root (temp-package-test-directory "packrat-local-root")))
    (with-packages-directory-override (packages-root)
      (let ((definition (rplaca:rplaca-use-package
                         :src-type :path
                         :repo (namestring source)
                         :resource-types '(:tool :command :doc :buffer-type))))
        (is (not (null definition)))
        (is (string= "resource-package"
                     (rplaca:package-definition-name definition)))
        (let ((record (rplaca:package-install-record-for-definition definition)))
          (is (eq :path (rplaca:package-install-record-source-type record)))
          (is (eq :global (rplaca:package-install-record-scope record)))
          (is (equal '(:tool :command :doc :buffer-type)
                     (rplaca:package-install-record-resource-types record)))
          (is (string= (namestring source)
                       (rplaca:package-install-record-source record)))
          (is (probe-file
               (rplaca::package-install-metadata-path
                (rplaca:package-definition-root definition)))))
        (is (search "resource-package"
                    (rplaca:install-package-status-string definition)))
        (is (search "resources: tool, command, doc, buffer-type"
                    (rplaca:package-resource-policy-string definition))))))

(test rplaca-use-package-updates-and-removes-installed-packages
  "Installed package records can be refreshed from source and removed again."
  (let* ((source (make-packrat-resource-package-source
                  :label "packrat-cycle"
                  :version "v1"))
         (packages-root (temp-package-test-directory "packrat-cycle-root")))
    (with-packages-directory-override (packages-root)
      (let ((definition (rplaca:rplaca-use-package
                         :src-type :path
                         :repo (namestring source))))
        (is (not (null definition)))
        (let ((installed-file
                (merge-pathnames "test-package.lisp"
                                 (rplaca:package-definition-root definition))))
          (is (search "v1" (uiop:read-file-string installed-file)))
          (write-test-file (merge-pathnames "test-package.lisp" source)
                           (packrat-resource-package-content "v2"))
          (is (not (null (rplaca:update-installed-package
                          "resource-package"))))
          (is (search "v2" (uiop:read-file-string installed-file)))
          (uiop:delete-directory-tree source :validate t :if-does-not-exist :ignore)
          (let* ((report (rplaca:package-doctor-report))
                 (entry (find "resource-package" report
                              :key (lambda (item) (getf item :name))
                              :test #'string=)))
            (is (not (null entry)))
            (is (eq :missing-source (getf entry :status))))
          (is (search "missing-source" (rplaca:package-doctor-to-string)))
          (is (not (null (rplaca:remove-installed-package "resource-package"))))
          (is (null (rplaca:find-installed-package "resource-package"))))))))

(test rplaca-use-package-respects-package-resource-filtering
  "Package resource allowlists control which package-owned artifacts register."
  (let* ((source (make-packrat-resource-package-source
                  :label "packrat-filter"
                  :version "v1"))
         (packages-root (temp-package-test-directory "packrat-filter-root")))
    (with-packages-directory-override (packages-root)
      (with-packrat-resource-state ()
        (let ((rplaca::*prompt-template-user-directory*
                (uiop:ensure-directory-pathname
                 (temp-package-test-directory "packrat-filter-global"))))
          (rplaca:rplaca-use-package
           :src-type :path
           :repo (namestring source)
           :resource-types '(:tool :command :doc :buffer-type))
          (rplaca:set-package-enablement-scope "resource-package" :global)
          (is (not (null (rplaca:load-active-packages))))
          (is (not (null (gethash 'rplaca::packrat-resource-command
                                  rplaca::*command-table*))))
          (is (not (null (rplaca:find-agent-tool-metadata
                          'rplaca::packrat-resource-tool))))
          (is (not (null (gethash 'rplaca::packrat-resource-command
                                  rplaca::*extended-docs*))))
          (is (not (null (rplaca:find-buffer-type :packrat-resource))))
          (is (null (rplaca:find-hook-metadata
                     'rplaca::*packrat-resource-hook*)))
          (is (null (rplaca:list-advices 'rplaca::packrat-resource-target)))
          (is (null (rplaca:render-package-prompt-sections)))
          (is (null (rplaca:discover-prompt-templates
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
        (rplaca:create-project "packrat-project"
                                 :root project-root
                                 :packages project-packages)
        (rplaca:rplaca-use-package
         :src-type :path
         :repo (namestring global-source))
        (rplaca:load-project-definitions)
        (let* ((project (rplaca:find-project "packrat-project"))
               (global-definition (rplaca:find-installed-package
                                   "resource-package"))
               (project-definition
                 (rplaca:find-installed-package "resource-package"
                                                  :project project))
               (buffer (make-buffer "packrat-project"
                                    :working-directory project-root)))
          (is (not (null project)))
          (is (eq :third-party
                  (rplaca:package-definition-source-tier global-definition)))
          (is (search "global"
                      (uiop:read-file-string
                       (merge-pathnames "test-package.lisp"
                                        (rplaca:package-definition-root
                                         global-definition)))))
          (is (not (null project-definition)))
          (is (eq :project (rplaca:package-definition-source-tier
                            project-definition)))
          (is (search "project"
                      (uiop:read-file-string
                       (merge-pathnames "test-package.lisp"
                                        (rplaca:package-definition-root
                                         project-definition)))))
          (is (eq :project (rplaca:package-enablement-scope
                            "resource-package"
                            :buffer buffer
                            :project project)))
          (is (eq project-definition
                  (rplaca:find-installed-package "resource-package"
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
                     (format nil "(setf rplaca/tests::*package-init-continued* nil)~%
(rplaca-use-package :src-type :git :repo ~S)~%
(setf rplaca/tests::*package-init-continued* t)~%"
                             "/tmp/does-not-exist-rplaca-package"))
    (let ((rplaca::*user-init-file* init-path)
          (rplaca::*packages-directory* packages-root)
          (rplaca::*inhibit-user-init* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (*package-init-continued* nil))
      (rplaca:load-user-init-file)
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
            "(incf rplaca/tests::*package-entrypoint-load-count*)"))
         (init-root (uiop:ensure-directory-pathname
                     (temp-package-test-directory "package-channel-init")))
         (init-path (merge-pathnames "init.lisp" init-root)))
    (ensure-directories-exist (merge-pathnames #P".keep" init-root))
    (write-test-file
     init-path
     (format nil "(register-package-channel \"init\" #P~S :description \"Init channel\")"
             (namestring channel-root)))
    (let ((rplaca::*user-init-file* init-path)
          (rplaca::*inhibit-user-init* nil))
      (with-package-state-override (nil)
        (rplaca:load-user-init-file)
        (is (not (null (find "init"
                             (rplaca:list-package-channels)
                             :key #'rplaca:package-channel-name
                             :test #'string=))))
        (rplaca:set-package-enablement-scope "init-package" :global)
        (is (rplaca:load-autoload-packages))
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
    (let ((rplaca::*user-init-file* init-path)
          (rplaca::*inhibit-user-init* nil)
          (rplaca::*agent-definition-registry* (make-hash-table :test #'equal)))
      (rplaca:load-user-init-file)
      (let ((definition (rplaca:find-agent-definition "writer")))
        (is (not (null definition)))
        (is (eq :openai-codex (rplaca:agent-definition-provider definition)))
        (is (string= "gpt-5.4" (rplaca:agent-definition-model definition)))
        (is (string= "high" (rplaca:agent-definition-think-level definition)))
        (is (string= "writer core" (rplaca:agent-definition-core-prompt definition)))
        (is (string= "writer personality" (rplaca:agent-definition-personality-prompt definition)))))))

(test rplaca-main-allows-init-based-prompt-and-hook-customization
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
            (setf rplaca/tests::*startup-hook-ran*
                  (eq (keymap-lookup *default-keymap* '(:ctrl-c #\\z))
                      'toggle-debug-mode-command))))~%
(add-hook '*initial-buffer-hook*
          (lambda (buffer)
            (setf rplaca/tests::*initial-buffer-hook-ran* t)
            (setf rplaca/tests::*initial-buffer-hook-binding*
                  (keymap-lookup (buffer-keymap buffer) '(:ctrl-c #\\z)))
            (buffer-insert-system-message buffer \"init buffer hook ran\")))~%"
             (namestring prompt-path)))
    (let ((rplaca::*user-init-file* init-path)
          (rplaca::*inhibit-user-init* nil)
          (rplaca::*personality-prompt-path* missing-path)
          (rplaca::*default-personality-prompt* "Default personality prompt")
          (rplaca::*startup-hook* nil)
          (rplaca::*initial-buffer-hook* nil)
          (rplaca::*default-keymap* nil)
          (rplaca::*debug-log-file* nil)
          (rplaca::*package-channels* (default-package-test-channels))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
          (rplaca::*enabled-builtin-packages* nil)
          (*startup-hook-ran* nil)
          (*initial-buffer-hook-ran* nil)
          (*initial-buffer-hook-binding* nil))
      (let ((buf (rplaca:rplaca-main :session-name "init-customization"
                                          :run-frame nil)))
        (is (string= "Custom personality prompt from init file."
                     rplaca:*default-personality-prompt*))
        (is (not (null *startup-hook-ran*)))
        (is (not (null *initial-buffer-hook-ran*)))
        (is (eq 'toggle-debug-mode-command *initial-buffer-hook-binding*))
        (is (eq 'toggle-debug-mode-command
                (rplaca:keymap-lookup (rplaca:buffer-keymap buf)
                                        '(:ctrl-c #\z))))
        (is (= 2 (rplaca:buffer-message-count buf)))))))
