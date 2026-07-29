(in-package :rplaca/tests)
(in-suite projects-suite)

(defun project-test-directory ()
  "Return a fresh temporary directory for project tests."
  (let ((dir (merge-pathnames
              (format nil "rplaca-project-tests-~D-~D-~36R/"
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
          (rplaca::*project-definitions-loaded-p* nil)
          (rplaca::*buffer-ring* nil)
          (rplaca::*buffer-counter* 0))
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
      (is (probe-file (rplaca::project-manifest-path "Persisted"
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
       (rplaca::project-manifest-path "same" definitions)
       (format nil "(:name \"same\" :root ~S :description \"from manifest\")"
               (namestring other-root)))
      (load-project-definitions)
      (let ((project (find-project "same")))
        (is (string= "from init" (project-description project)))
        (is (string= (namestring root) (namestring (project-root project))))))))

(test config-project-uses-user-init-directory
  "The user init directory is exposed as the config project."
  (with-project-test-state (root definitions)
    (let ((rplaca::*user-init-directory* root))
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

(test project-file-buffer-uses-editor-keymap-and-emacs-editing
  "Project file buffers use the file keymap and can be edited with normal keys."
  (with-project-test-state (root definitions)
    (rplaca::init-default-keymap)
    (define-project "keys" :root root)
    (project-create-file "keys" "notes.txt" :content "alpha")
    (let ((buffer (project-open-file "keys" "notes.txt")))
      (is (eq rplaca::*file-keymap* (buffer-keymap buffer)))
      (rplaca::handle-key-event buffer '(:meta #\>))
      (rplaca::handle-key-event buffer #\Return)
      (dolist (char '(#\b #\e #\t #\a))
        (rplaca::handle-key-event buffer char))
      (is (string= "alpha
beta" (file-buffer-text buffer)))
      (is (buffer-dirty-p buffer))
      (rplaca::save-session-command buffer)
      (is (string= "alpha
beta" (project-read-file "keys" "notes.txt"))))))

(test project-file-buffer-region-editing
  "File buffers support mark, region kill, and write-file-as."
  (with-project-test-state (root definitions)
    (rplaca::init-default-keymap)
    (define-project "region" :root root)
    (project-create-file "region" "notes.txt" :content "abcdef")
    (let ((buffer (project-open-file "region" "notes.txt"))
          (msg nil))
      (setf msg (buffer-input-message buffer))
      (rplaca::set-message-point-from-absolute-offset msg 1)
      (rplaca::handle-key-event buffer (code-char 0))
      (rplaca::set-message-point-from-absolute-offset msg 4)
      (rplaca::handle-key-event buffer (code-char 23))
      (is (string= "aef" (file-buffer-text buffer)))
      (is (string= "bcd" (kill-ring-top)))
      (rplaca::write-project-file-as-command buffer "renamed.txt")
      (is (string= "renamed.txt" (buffer-resource-path buffer)))
      (is (string= "aef" (project-read-file "region" "renamed.txt"))))))

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
    (rplaca::init-default-keymap)
    (define-project "save" :root root)
    (project-create-file "save" "draft.txt" :content "old")
    (let ((buffer (project-open-file "save" "draft.txt")))
      (setf (file-buffer-text buffer) "new")
      (rplaca::save-session-command buffer)
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
                     :systems '("rplaca")
                     :check-functions
                     (list (lambda (project)
                             (list :checked (project-name project))))
                     :reload-function
                     (lambda (project)
                       (list :reloaded (project-name project))))))
      (is (equal '("rplaca") (project-systems project)))
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

(test change-set-transactions-serialize-and-publish-entry-flags-at-commit
  "Independent applies serialize file I/O and publish detached flags atomically."
  (with-project-test-state (root definitions)
    (is (pathnamep definitions))
    (define-project "serialized" :root root)
    (project-create-file "serialized" "first.txt" :content "old-first")
    (project-create-file "serialized" "second.txt" :content "old-second")
    (let* ((project-table *project-registry*)
           (first-change-set (begin-change-set :name "first-transaction"))
           (second-change-set (begin-change-set :name "second-transaction"))
           (first-entered
             (bt:make-semaphore :name "first change-set entry applied"))
           (second-entered
             (bt:make-semaphore :name "second change-set entry applied"))
           (release-first
             (bt:make-semaphore :name "release first change-set entry"))
           (counter-lock (bt:make-lock "change-set apply counter"))
           (active 0)
           (maximum-active 0)
           (first-error nil)
           (second-error nil)
           (first-thread nil)
           (second-thread nil))
      (stage-project-file "serialized" "first.txt" "new-first"
                          :change-set first-change-set)
      (stage-project-file "serialized" "second.txt" "new-second"
                          :change-set second-change-set)
      (labels ((apply-with-pause (entry)
                 (bt:with-lock-held (counter-lock)
                   (incf active)
                   (setf maximum-active (max maximum-active active)))
                 (unwind-protect
                      (progn
                        ;; The production helper mutates only this detached copy.
                        (rplaca::apply-change-set-entry entry)
                        (cond
                          ((string= "new-first"
                                    (rplaca::change-set-entry-new-text entry))
                           (bt:signal-semaphore first-entered)
                           (unless (bt:wait-on-semaphore release-first
                                                         :timeout 5.0)
                             (error "Timed out releasing first apply.")))
                          (t
                           (bt:signal-semaphore second-entered)))
                        entry)
                   (bt:with-lock-held (counter-lock)
                     (decf active)))))
        (unwind-protect
             (progn
               (setf first-thread
                     (bt:make-thread
                      (lambda ()
                        (let ((*project-registry* project-table)
                              (rplaca::*buffer-ring* nil)
                              (rplaca::*change-set-entry-apply-function*
                                #'apply-with-pause))
                          (handler-case
                              (apply-change-set first-change-set)
                            (error (condition)
                              (setf first-error condition)))))
                      :name "first serialized change set"))
               (is-true (bt:wait-on-semaphore first-entered :timeout 2.0))
               ;; The detached entry has completed its write and set its flag,
               ;; but registry readers still see the last committed generation.
               (let* ((snapshot
                        (cdr (assoc (change-set-id first-change-set)
                                    (rplaca::change-set-registry-snapshot)
                                    :test #'string=)))
                      (entry (first (change-set-entries snapshot))))
                 (is (eq :applying (change-set-status snapshot)))
                 (is-false (rplaca::change-set-entry-applied-p entry)))
               (setf second-thread
                     (bt:make-thread
                      (lambda ()
                        (let ((*project-registry* project-table)
                              (rplaca::*buffer-ring* nil)
                              (rplaca::*change-set-entry-apply-function*
                                #'apply-with-pause))
                          (handler-case
                              (apply-change-set second-change-set)
                            (error (condition)
                              (setf second-error condition)))))
                      :name "second serialized change set"))
               (is-false
                (bt:wait-on-semaphore second-entered :timeout 0.25))
               (bt:signal-semaphore release-first)
               (is-true
                (bt:wait-on-semaphore second-entered :timeout 2.0))
               (bt:join-thread first-thread)
               (setf first-thread nil)
               (bt:join-thread second-thread)
               (setf second-thread nil)
               (is (null first-error))
               (is (null second-error))
               (is (= 1 maximum-active))
               (is (eq :applied (change-set-status first-change-set)))
               (is (eq :applied (change-set-status second-change-set)))
               (is-true
                (rplaca::change-set-entry-applied-p
                 (first (change-set-entries first-change-set))))
               (is (string= "new-first"
                            (project-read-file "serialized" "first.txt")))
               (is (string= "new-second"
                            (project-read-file "serialized" "second.txt"))))
          (bt:signal-semaphore release-first)
          (when (and first-thread (bt:thread-alive-p first-thread))
            (bt:join-thread first-thread))
          (when (and second-thread (bt:thread-alive-p second-thread))
            (bt:join-thread second-thread)))))))

(test project-manifest-writes-serialize-and-atomically-replace
  "Manifest writers never overlap, and failed temp writes preserve the target."
  (with-project-test-state (root definitions)
    (let* ((first-project (create-project "atomic" :root root
                                          :description "original"))
           (second-project (rplaca::copy-project first-project))
           (first-entered
             (bt:make-semaphore :name "first manifest writer entered"))
           (second-entered
             (bt:make-semaphore :name "second manifest writer entered"))
           (release-first
             (bt:make-semaphore :name "release first manifest writer"))
           (counter-lock (bt:make-lock "manifest writer counter"))
           (active 0)
           (maximum-active 0)
           (first-error nil)
           (second-error nil)
           (first-thread nil)
           (second-thread nil))
      (setf (project-description first-project) "first"
            (project-description second-project) "second")
      (labels ((write-with-pause (path manifest)
                 (bt:with-lock-held (counter-lock)
                   (incf active)
                   (setf maximum-active (max maximum-active active)))
                 (unwind-protect
                      (progn
                        (if (string= "first" (getf manifest :description))
                            (progn
                              (bt:signal-semaphore first-entered)
                              (unless (bt:wait-on-semaphore release-first
                                                            :timeout 5.0)
                                (error "Timed out releasing manifest writer.")))
                            (bt:signal-semaphore second-entered))
                        (rplaca::write-project-manifest-generation
                         path manifest))
                   (bt:with-lock-held (counter-lock)
                     (decf active)))))
        (unwind-protect
             (progn
               (setf first-thread
                     (bt:make-thread
                      (lambda ()
                        (let ((rplaca::*project-manifest-write-function*
                                #'write-with-pause))
                          (handler-case
                              (rplaca::write-project-manifest
                               first-project definitions)
                            (error (condition)
                              (setf first-error condition)))))
                      :name "first serialized manifest"))
               (is-true (bt:wait-on-semaphore first-entered :timeout 2.0))
               (setf second-thread
                     (bt:make-thread
                      (lambda ()
                        (let ((rplaca::*project-manifest-write-function*
                                #'write-with-pause))
                          (handler-case
                              (rplaca::write-project-manifest
                               second-project definitions)
                            (error (condition)
                              (setf second-error condition)))))
                      :name "second serialized manifest"))
               (is-false
                (bt:wait-on-semaphore second-entered :timeout 0.25))
               (bt:signal-semaphore release-first)
               (is-true
                (bt:wait-on-semaphore second-entered :timeout 2.0))
               (bt:join-thread first-thread)
               (setf first-thread nil)
               (bt:join-thread second-thread)
               (setf second-thread nil)
               (is (null first-error))
               (is (null second-error))
               (is (= 1 maximum-active))
               (is (string= "second"
                            (getf (rplaca::read-project-manifest
                                   (rplaca::project-manifest-path
                                    "atomic" definitions))
                                  :description))))
          (bt:signal-semaphore release-first)
          (when (and first-thread (bt:thread-alive-p first-thread))
            (bt:join-thread first-thread))
          (when (and second-thread (bt:thread-alive-p second-thread))
            (bt:join-thread second-thread))))
      ;; A failed temporary generation cannot truncate the committed target.
      (let ((rplaca::*project-manifest-write-function*
              (lambda (path manifest)
                (declare (ignore manifest))
                (with-open-file (stream path
                                        :direction :output
                                        :if-exists :error
                                        :if-does-not-exist :create)
                  (write-string "(:name \"atomic\"" stream))
                (error "Injected manifest write failure."))))
        (setf (project-description first-project) "must-not-publish")
        (signals error
          (rplaca::write-project-manifest first-project definitions)))
      (is (string= "second"
                   (getf (rplaca::read-project-manifest
                          (rplaca::project-manifest-path
                           "atomic" definitions))
                         :description)))
      (is (null (directory
                 (merge-pathnames
                  (make-pathname :name :wild :type "tmp")
                  definitions)))))))

(test change-set-entry-failure-after-mutation-is-compensated
  "A failing apply or revert entry restores the authoritative file snapshot."
  (with-project-test-state (root definitions)
    (is (pathnamep definitions))
    (define-project "compensate" :root root)
    (project-create-file "compensate" "state.txt" :content "old")
    (let ((failed-apply (begin-change-set :name "failed-apply")))
      (stage-project-file "compensate" "state.txt" "new"
                          :change-set failed-apply)
      (let ((rplaca::*change-set-entry-apply-function*
              (lambda (entry)
                (rplaca::apply-change-set-entry entry)
                (error "Injected failure after apply mutation."))))
        (signals error (apply-change-set failed-apply)))
      (is (eq :failed (change-set-status failed-apply)))
      (is-false
       (rplaca::change-set-entry-applied-p
        (first (change-set-entries failed-apply))))
      (is (string= "old" (project-read-file "compensate" "state.txt"))))
    (let ((failed-revert (begin-change-set :name "failed-revert")))
      (stage-project-file "compensate" "state.txt" "new"
                          :change-set failed-revert)
      (apply-change-set failed-revert)
      (let ((rplaca::*change-set-entry-revert-function*
              (lambda (entry)
                (rplaca::revert-change-set-entry entry)
                (error "Injected failure after revert mutation."))))
        (signals error (revert-change-set failed-revert)))
      (is (eq :revert-failed (change-set-status failed-revert)))
      (is-true
       (rplaca::change-set-entry-applied-p
        (first (change-set-entries failed-revert))))
      (is (string= "new"
                   (project-read-file "compensate" "state.txt"))))))

(test background-project-write-defers-buffer-sync-to-frame-effect
  "Worker file writes publish an immutable effect; frame replay mutates UI state."
  (with-project-test-state (root definitions)
    (is (pathnamep definitions))
    (define-project "effect" :root root)
    (project-create-file "effect" "notes.txt" :content "old")
    (let* ((buffer (project-open-file "effect" "notes.txt"))
           (project-table *project-registry*)
           (effects nil)
           (worker-thread nil)
           (worker-error nil)
           (worker
             (bt:make-thread
              (lambda ()
                (let ((*project-registry* project-table)
                      (rplaca::*tool-effect-recorder*
                        (lambda (kind effect)
                          (push (cons kind effect) effects))))
                  (setf worker-thread (bt:current-thread))
                  (handler-case
                      (project-save-file "effect" "notes.txt" "new")
                    (error (condition)
                      (setf worker-error condition)))))
              :name "background project writer")))
      (bt:join-thread worker)
      (is (null worker-error))
      (is (not (eq worker-thread (bt:current-thread))))
      (is (string= "new" (project-read-file "effect" "notes.txt")))
      (is (string= "old" (file-buffer-text buffer)))
      (is (= 1 (length effects)))
      (is (eq :project-buffer (caar effects)))
      (rplaca::apply-interactive-tool-effects buffer (nreverse effects))
      (is (string= "new" (file-buffer-text buffer)))
      (is-false (buffer-dirty-p buffer)))))

(test direct-project-write-failure-preserves-target-and-cleans-temp
  "A pre-rename write failure leaves the prior file and no temp generation."
  (with-project-test-state (root definitions)
    (is (pathnamep definitions))
    (define-project "atomic-write" :root root)
    (project-create-file "atomic-write" "state.txt" :content "stable")
    ;; WRITE-STRING rejects this value only after the same-directory temporary
    ;; file has been opened, deterministically exercising unwind cleanup.
    (signals error
      (project-save-file "atomic-write" "state.txt" 42))
    (is (string= "stable"
                 (project-read-file "atomic-write" "state.txt")))
    (is (null (directory
               (merge-pathnames
                (make-pathname :name :wild :type "tmp")
                root))))))

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
