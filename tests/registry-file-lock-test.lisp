(in-package :clawmacs/tests)

(defmacro with-isolated-process-tool-registry (&body body)
  "Run BODY with isolated tables treated as the process-global registry."
  `(let* ((clawmacs::*tool-registry-lock*
            (bt:make-lock "test process tool registry"))
          (clawmacs::*tool-table* (make-hash-table :test #'equal))
          (clawmacs::*process-tool-table* clawmacs::*tool-table*)
          (clawmacs::*agent-tool-metadata-table*
            (make-hash-table :test #'eq))
          (clawmacs::*process-agent-tool-metadata-table*
            clawmacs::*agent-tool-metadata-table*)
          (clawmacs::*agent-tool-name-table*
            (make-hash-table :test #'equal))
          (clawmacs::*process-agent-tool-name-table*
            clawmacs::*agent-tool-name-table*)
          (clawmacs::*temporary-tool-table* nil))
     ,@body))

(in-suite llm-suite)

(test tool-registry-snapshot-survives-concurrent-refresh
  "Prompt traversal uses a snapshot and never holds the lock in callbacks."
  (with-isolated-process-tool-registry
    (let* ((table clawmacs::*tool-table*)
           (lock clawmacs::*tool-registry-lock*)
           (callback-entered
             (bt:make-semaphore :name "tool snapshot callback entered"))
           (release-callback
             (bt:make-semaphore :name "release tool snapshot callback"))
           (refresh-complete
             (bt:make-semaphore :name "tool refresh complete"))
           (mapped nil)
           (map-error nil)
           (refresh-error nil)
           (mapper nil)
           (refresher nil))
      (flet ((install (name)
               (clawmacs::register-tool
                name name '((:type . "object"))
                (lambda (args) (declare (ignore args)) name))))
        (install "snapshot_alpha")
        (install "snapshot_beta")
        (unwind-protect
             (progn
               (setf mapper
                     (bt:make-thread
                      (lambda ()
                        (let ((clawmacs::*tool-table* table)
                              (clawmacs::*process-tool-table* table)
                              (clawmacs::*tool-registry-lock* lock)
                              (clawmacs::*temporary-tool-table* nil)
                              (first-p t))
                          (handler-case
                              (clawmacs::map-effective-tool-definitions
                               (lambda (name definition)
                                 (declare (ignore definition))
                                 (push name mapped)
                                 (when first-p
                                   (setf first-p nil)
                                   (bt:signal-semaphore callback-entered)
                                   (unless (bt:wait-on-semaphore
                                            release-callback :timeout 5.0)
                                     (error "Timed out releasing tool snapshot callback")))))
                            (error (condition)
                              (setf map-error condition)))))
                      :name "tool-registry-snapshot-mapper"))
               (is-true
                (bt:wait-on-semaphore callback-entered :timeout 2.0))
               (setf refresher
                     (bt:make-thread
                      (lambda ()
                        (let ((clawmacs::*tool-table* table)
                              (clawmacs::*process-tool-table* table)
                              (clawmacs::*tool-registry-lock* lock))
                          (handler-case
                              (progn
                                (clawmacs::remove-registered-tools
                                 '("snapshot_alpha" "snapshot_beta"))
                                (install "snapshot_gamma"))
                            (error (condition)
                              (setf refresh-error condition)))
                          (bt:signal-semaphore refresh-complete)))
                      :name "tool-registry-concurrent-refresh"))
               ;; This completes while the mapper callback is deliberately
               ;; blocked, proving no extension callback owns the registry lock.
               (is-true
                (bt:wait-on-semaphore refresh-complete :timeout 2.0))
               (bt:signal-semaphore release-callback)
               (bt:join-thread mapper)
               (setf mapper nil)
               (bt:join-thread refresher)
               (setf refresher nil)
               (is (null map-error))
               (is (null refresh-error))
               (is (equal '("snapshot_alpha" "snapshot_beta")
                          (sort mapped #'string<)))
               (is (equal '("snapshot_gamma")
                          (sort (mapcar #'car
                                        (clawmacs::registered-tool-definitions-snapshot))
                                #'string<))))
          (bt:signal-semaphore release-callback)
          (when (and mapper (bt:thread-alive-p mapper))
            (bt:join-thread mapper))
          (when (and refresher (bt:thread-alive-p refresher))
            (bt:join-thread refresher)))))))

(test dynamically-bound-tool-table-does-not-contend-on-process-lock
  "A private dynamic tool table is not serialized by the global registry lock."
  (with-isolated-process-tool-registry
    (let* ((process-table clawmacs::*tool-table*)
           (lock clawmacs::*tool-registry-lock*)
           (private-table (make-hash-table :test #'equal))
           (completed (bt:make-semaphore :name "private tool table complete"))
           (worker nil)
           (worker-error nil))
      (bt:acquire-lock lock)
      (unwind-protect
           (progn
             (setf worker
                   (bt:make-thread
                    (lambda ()
                      (let ((clawmacs::*tool-table* private-table)
                            (clawmacs::*process-tool-table* process-table)
                            (clawmacs::*tool-registry-lock* lock))
                        (handler-case
                            (clawmacs::register-tool
                             "private_tool" "private" nil
                             (lambda (args) (declare (ignore args)) "ok"))
                          (error (condition)
                            (setf worker-error condition)))
                        (bt:signal-semaphore completed)))
                    :name "private-dynamic-tool-table"))
             (is-true (bt:wait-on-semaphore completed :timeout 2.0))
             (is (null worker-error))
             (is-true (gethash "private_tool" private-table))
             (is-false (gethash "private_tool" process-table)))
        (bt:release-lock lock)
        (when worker
          (bt:join-thread worker))))))

(defun exercise-registry-snapshot-against-refresh
    (snapshot-function refresh-function)
  "Traverse a stable snapshot while REFRESH-FUNCTION replaces its registry.

Return the keys observed by the traversal.  The deterministic semaphore
interleaving also proves the writer does not wait for snapshot consumers."
  (let ((reader-entered
          (bt:make-semaphore :name "registry snapshot reader entered"))
        (release-reader
          (bt:make-semaphore :name "release registry snapshot reader"))
        (refresh-complete
          (bt:make-semaphore :name "registry snapshot refresh complete"))
        (observed nil)
        (reader-error nil)
        (refresh-error nil)
        (reader nil)
        (refresher nil))
    (unwind-protect
         (progn
           (setf reader
                 (bt:make-thread
                  (lambda ()
                    (handler-case
                        (let ((first-p t))
                          (dolist (entry (funcall snapshot-function))
                            (push (car entry) observed)
                            (when first-p
                              (setf first-p nil)
                              (bt:signal-semaphore reader-entered)
                              (unless (bt:wait-on-semaphore
                                       release-reader :timeout 5.0)
                                (error "Timed out releasing registry reader")))))
                      (error (condition)
                        (setf reader-error condition))))
                  :name "registry-snapshot-reader"))
           (is-true (bt:wait-on-semaphore reader-entered :timeout 2.0))
           (setf refresher
                 (bt:make-thread
                  (lambda ()
                    (handler-case
                        (funcall refresh-function)
                      (error (condition)
                        (setf refresh-error condition)))
                    (bt:signal-semaphore refresh-complete))
                  :name "registry-concurrent-refresh"))
           ;; The writer must finish while the reader is deliberately paused
           ;; inside traversal of its detached snapshot.
           (is-true (bt:wait-on-semaphore refresh-complete :timeout 2.0))
           (bt:signal-semaphore release-reader)
           (bt:join-thread reader)
           (setf reader nil)
           (bt:join-thread refresher)
           (setf refresher nil)
           (is (null reader-error))
           (is (null refresh-error))
           observed)
      (bt:signal-semaphore release-reader)
      (when (and reader (bt:thread-alive-p reader))
        (bt:join-thread reader))
      (when (and refresher (bt:thread-alive-p refresher))
        (bt:join-thread refresher)))))

(defun registry-lock-command-alpha (buffer)
  (declare (ignore buffer))
  :alpha)

(defun registry-lock-command-beta (buffer)
  (declare (ignore buffer))
  :beta)

(defun registry-lock-command-gamma (buffer)
  (declare (ignore buffer))
  :gamma)

(defvar *registry-lock-hook-alpha* nil)
(defvar *registry-lock-hook-beta* nil)
(defvar *registry-lock-hook-gamma* nil)

(defun registry-lock-advice-target-alpha (value) value)
(defun registry-lock-advice-target-beta (value) value)
(defun registry-lock-advice-target-gamma (value) value)
(defun registry-lock-before-advice (value)
  (declare (ignore value))
  nil)

(in-suite commands-suite)

(test command-registry-snapshot-survives-concurrent-package-refresh
  (let ((table (make-hash-table :test #'eq))
        (lock (bt:make-lock "test command registry")))
    (labels ((within-registry (function)
               (let ((clawmacs::*command-table* table)
                     (clawmacs::*process-command-table* table)
                     (clawmacs::*command-registry-lock* lock))
                 (funcall function)))
             (register (symbol package)
               (within-registry
                (lambda ()
                  (let ((clawmacs::*current-clawmacs-package* package))
                    (clawmacs::register-command-metadata symbol)))))
             (snapshot ()
               (within-registry #'clawmacs::command-registry-snapshot))
             (refresh ()
               (within-registry
                (lambda ()
                  (clawmacs::remove-command-metadata-for-package "registry-old")
                  (let ((clawmacs::*current-clawmacs-package* "registry-new"))
                    (clawmacs::register-command-metadata
                     'registry-lock-command-gamma))))))
      (register 'registry-lock-command-alpha "registry-old")
      (register 'registry-lock-command-beta "registry-old")
      (let ((observed
              (exercise-registry-snapshot-against-refresh
               #'snapshot #'refresh)))
        (is (equal '(registry-lock-command-alpha
                     registry-lock-command-beta)
                   (sort observed #'string< :key #'symbol-name)))
        (is (equal '(registry-lock-command-gamma)
                   (mapcar #'car (snapshot))))))))

(test extended-doc-registry-snapshot-survives-concurrent-package-refresh
  (let ((table (make-hash-table :test #'eq))
        (lock (bt:make-lock "test extended doc registry")))
    (labels ((within-registry (function)
               (let ((clawmacs::*extended-docs* table)
                     (clawmacs::*process-extended-docs* table)
                     (clawmacs::*extended-doc-registry-lock* lock))
                 (funcall function)))
             (register (symbol package)
               (within-registry
                (lambda ()
                  (clawmacs::register-extended-doc
                   symbol (list :category "test" :package package)))))
             (snapshot ()
               (within-registry #'clawmacs::extended-doc-registry-snapshot))
             (refresh ()
               (within-registry
                (lambda ()
                  (clawmacs::remove-extended-docs-for-package "registry-old")
                  (clawmacs::register-extended-doc
                   'registry-lock-command-gamma
                   '(:category "test" :package "registry-new"))))))
      (register 'registry-lock-command-alpha "registry-old")
      (register 'registry-lock-command-beta "registry-old")
      (let ((observed
              (exercise-registry-snapshot-against-refresh
               #'snapshot #'refresh)))
        (is (equal '(registry-lock-command-alpha
                     registry-lock-command-beta)
                   (sort observed #'string< :key #'symbol-name)))
        (is (equal '(registry-lock-command-gamma)
                   (mapcar #'car (snapshot))))))))

(test slash-command-registry-snapshot-survives-concurrent-package-refresh
  (let ((table (make-hash-table :test #'equal))
        (lock (bt:make-lock "test slash command registry")))
    (labels ((within-registry (function)
               (let ((clawmacs::*slash-command-table* table)
                     (clawmacs::*process-slash-command-table* table)
                     (clawmacs::*slash-command-registry-lock* lock))
                 (funcall function)))
             (register (name package)
               (within-registry
                (lambda ()
                  (clawmacs:register-slash-command
                   name (lambda (&rest args) (declare (ignore args)) nil)
                   :package package))))
             (snapshot ()
               (within-registry #'clawmacs::slash-command-registry-snapshot))
             (refresh ()
               (within-registry
                (lambda ()
                  (clawmacs::remove-slash-commands-for-package "registry-old")
                  (clawmacs:register-slash-command
                   "registry-gamma"
                   (lambda (&rest args) (declare (ignore args)) nil)
                   :package "registry-new")))))
      (register "registry-alpha" "registry-old")
      (register "registry-beta" "registry-old")
      (let ((observed
              (exercise-registry-snapshot-against-refresh
               #'snapshot #'refresh)))
        (is (equal '("registry-alpha" "registry-beta")
                   (sort observed #'string<)))
        (is (equal '("registry-gamma")
                   (mapcar #'car (snapshot))))))))

(in-suite buffer-suite)

(test buffer-type-registry-snapshot-survives-concurrent-package-refresh
  (let ((table (make-hash-table :test #'eq))
        (lock (bt:make-lock "test buffer type registry")))
    (labels ((within-registry (function)
               (let ((clawmacs::*buffer-type-registry* table)
                     (clawmacs::*process-buffer-type-registry* table)
                     (clawmacs::*buffer-type-registry-lock* lock)
                     (clawmacs::*buffer-input-presentation-providers* nil))
                 (funcall function)))
             (register (name package)
               (within-registry
                (lambda ()
                  (clawmacs:register-buffer-type name :package package))))
             (snapshot ()
               (within-registry #'clawmacs::buffer-type-registry-snapshot))
             (refresh ()
               (within-registry
                (lambda ()
                  (clawmacs::remove-buffer-types-for-package "registry-old")
                  (clawmacs:register-buffer-type
                   :registry-gamma :package "registry-new")))))
      (register :registry-alpha "registry-old")
      (register :registry-beta "registry-old")
      (let ((observed
              (exercise-registry-snapshot-against-refresh
               #'snapshot #'refresh)))
        (is (equal '(:registry-alpha :registry-beta)
                   (sort observed #'string< :key #'symbol-name)))
        (is (equal '(:registry-gamma)
                   (mapcar #'car (snapshot))))))))

(in-suite commands-suite)

(test hook-metadata-snapshot-survives-concurrent-package-refresh
  (let ((table (make-hash-table :test #'eq))
        (lock (bt:make-lock "test hook registry")))
    (labels ((within-registry (function)
               (let ((clawmacs::*hook-metadata-table* table)
                     (clawmacs::*process-hook-metadata-table* table)
                     (clawmacs::*hook-registry-lock* lock)
                     (clawmacs::*package-hook-registrations* nil))
                 (funcall function)))
             (register (symbol package)
               (within-registry
                (lambda ()
                  (let ((clawmacs::*current-clawmacs-package* package))
                    (clawmacs::register-hook-metadata symbol '(value) "test")))))
             (snapshot ()
               (within-registry #'clawmacs::hook-metadata-registry-snapshot))
             (refresh ()
               (within-registry
                (lambda ()
                  (clawmacs::remove-package-hook-registrations "registry-old")
                  (let ((clawmacs::*current-clawmacs-package* "registry-new"))
                    (clawmacs::register-hook-metadata
                     '*registry-lock-hook-gamma* '(value) "test"))))))
      (register '*registry-lock-hook-alpha* "registry-old")
      (register '*registry-lock-hook-beta* "registry-old")
      (let ((observed
              (exercise-registry-snapshot-against-refresh
               #'snapshot #'refresh)))
        (is (equal '(*registry-lock-hook-alpha* *registry-lock-hook-beta*)
                   (sort observed #'string< :key #'symbol-name)))
        (is (equal '(*registry-lock-hook-gamma*)
                   (mapcar #'car (snapshot))))))))

(test advice-registry-snapshot-survives-concurrent-package-refresh
  (let ((table (make-hash-table :test #'eq))
        (lock (bt:make-lock "test advice registry")))
    (labels ((within-registry (function)
               (let ((clawmacs::*advice-table* table)
                     (clawmacs::*process-advice-table* table)
                     (clawmacs::*advice-registry-lock* lock))
                 (funcall function)))
             (register (symbol package)
               (within-registry
                (lambda ()
                  (let ((clawmacs::*current-clawmacs-package* package))
                    (clawmacs:add-advice
                     symbol :before 'registry-lock-before-advice
                     :name package)))))
             (snapshot ()
               (within-registry #'clawmacs::advice-registry-snapshot))
             (refresh ()
               (within-registry
                (lambda ()
                  (clawmacs::remove-package-advices "registry-old")
                  (let ((clawmacs::*current-clawmacs-package* "registry-new"))
                    (clawmacs:add-advice
                     'registry-lock-advice-target-gamma
                     :before 'registry-lock-before-advice
                     :name "registry-new"))))))
      (unwind-protect
           (progn
             (register 'registry-lock-advice-target-alpha "registry-old")
             (register 'registry-lock-advice-target-beta "registry-old")
             (let ((observed
                     (exercise-registry-snapshot-against-refresh
                      #'snapshot #'refresh)))
               (is (equal '(registry-lock-advice-target-alpha
                            registry-lock-advice-target-beta)
                          (sort observed #'string< :key #'symbol-name)))
               (is (equal '(registry-lock-advice-target-gamma)
                          (mapcar #'car (snapshot))))))
        (within-registry
         (lambda ()
           (dolist (symbol '(registry-lock-advice-target-alpha
                             registry-lock-advice-target-beta
                             registry-lock-advice-target-gamma))
             (clawmacs:clear-advices symbol))))))))

(in-suite llm-suite)

(test agent-definition-snapshot-survives-concurrent-package-refresh
  (let ((table (make-hash-table :test #'equal))
        (lock (bt:make-lock "test agent definition registry")))
    (labels ((within-registry (function)
               (let ((clawmacs::*agent-definition-registry* table)
                     (clawmacs::*process-agent-definition-registry* table)
                     (clawmacs::*agent-definition-registry-lock* lock))
                 (funcall function)))
             (register (name package)
               (within-registry
                (lambda ()
                  (let ((clawmacs::*current-clawmacs-package* package))
                    (clawmacs:register-agent-definition name)))))
             (snapshot ()
               (within-registry #'clawmacs::agent-definition-registry-snapshot))
             (refresh ()
               (within-registry
                (lambda ()
                  (clawmacs::remove-agent-definitions-for-package "registry-old")
                  (let ((clawmacs::*current-clawmacs-package* "registry-new"))
                    (clawmacs:register-agent-definition "registry-gamma"))))))
      (register "registry-alpha" "registry-old")
      (register "registry-beta" "registry-old")
      (let ((observed
              (exercise-registry-snapshot-against-refresh
               #'snapshot #'refresh)))
        (is (equal '("registry-alpha" "registry-beta")
                   (sort observed #'string<)))
        (is (equal '("registry-gamma")
                   (mapcar #'car (snapshot))))))))

(test pipeline-registry-snapshots-survive-concurrent-package-refresh
  (let ((definitions (make-hash-table :test #'equal))
        (profiles (make-hash-table :test #'equal))
        (lock (bt:make-lock "test pipeline registries")))
    (labels ((within-registry (function)
               (let ((clawmacs::*pipeline-definition-registry* definitions)
                     (clawmacs::*process-pipeline-definition-registry*
                       definitions)
                     (clawmacs::*pipeline-test-profile-registry* profiles)
                     (clawmacs::*process-pipeline-test-profile-registry* profiles)
                     (clawmacs::*pipeline-registry-lock* lock))
                 (funcall function)))
             (register (name package)
               (within-registry
                (lambda ()
                  (let ((clawmacs::*current-clawmacs-package* package))
                    (clawmacs:register-pipeline-definition
                     name :stages '((:name "only" :prompt "test")))
                    (clawmacs:register-pipeline-test-profile
                     name :command '("true"))))))
             (snapshot ()
               (within-registry
                (lambda ()
                  (append
                   (mapcar (lambda (entry)
                             (cons (format nil "definition/~A" (car entry))
                                   (cdr entry)))
                           (clawmacs::pipeline-registry-snapshot definitions))
                   (mapcar (lambda (entry)
                             (cons (format nil "profile/~A" (car entry))
                                   (cdr entry)))
                           (clawmacs::pipeline-registry-snapshot profiles))))))
             (refresh ()
               (within-registry
                (lambda ()
                  (clawmacs::remove-pipeline-registrations-for-package
                   "registry-old")
                  (let ((clawmacs::*current-clawmacs-package* "registry-new"))
                    (clawmacs:register-pipeline-definition
                     "registry-gamma"
                     :stages '((:name "only" :prompt "test")))
                    (clawmacs:register-pipeline-test-profile
                     "registry-gamma" :command '("true")))))))
      (register "registry-alpha" "registry-old")
      (register "registry-beta" "registry-old")
      (let ((observed
              (exercise-registry-snapshot-against-refresh
               #'snapshot #'refresh)))
        (is (equal '("definition/registry-alpha"
                     "definition/registry-beta"
                     "profile/registry-alpha"
                     "profile/registry-beta")
                   (sort observed #'string<)))
        (is (equal '("definition/registry-gamma"
                     "profile/registry-gamma")
                   (sort (mapcar #'car (snapshot)) #'string<)))))))

(in-suite artifactum-package-suite)

(defun artifactum-lock-test-record (id name)
  "Return a complete deterministic Artifactum record for lock tests."
  (list :id id
        :kind "artifact"
        :name name
        :mime-type "text/plain"
        :path (format nil "/tmp/~A" name)
        :size 1
        :preview name
        :extracted-text name
        :author "test"
        :created-at 1
        :updated-at 1))

(test artifactum-index-transaction-serializes-detached-and-live-buffers
  "Concurrent upserts sharing a session path retain both valid JSON records."
  (with-artifactum-test-state
    (let* ((live (make-artifactum-test-buffer "concurrent-index"))
           (detached (clawmacs::make-tool-execution-buffer-snapshot live))
           (live-session (buffer-session live))
           (detached-session (buffer-session detached))
           (lock (bt:make-lock "test artifactum process index"))
           (ready (bt:make-semaphore :name "artifactum writers ready"))
           (start (bt:make-semaphore :name "start artifactum writers"))
           (completed (bt:make-semaphore :name "artifactum writer complete"))
           (first-error nil)
           (second-error nil)
           (first-thread nil)
           (second-thread nil)
           (lock-held-p nil))
      (labels ((writer (session record error-setter)
                 (bt:signal-semaphore ready)
                 (unless (bt:wait-on-semaphore start :timeout 5.0)
                   (error "Timed out starting Artifactum writer"))
                 (let ((clawmacs::*artifactum-index-lock* lock))
                   (handler-case
                       (clawmacs::artifactum-upsert-record session record)
                     (error (condition)
                       (funcall error-setter condition))))
                 (bt:signal-semaphore completed)))
        (bt:acquire-lock lock)
        (setf lock-held-p t)
        (unwind-protect
             (progn
               (setf first-thread
                     (bt:make-thread
                      (lambda ()
                        (writer live-session
                                (artifactum-lock-test-record
                                 "art-live" "live.txt")
                                (lambda (condition)
                                  (setf first-error condition))))
                      :name "artifactum-live-index-writer")
                     second-thread
                     (bt:make-thread
                      (lambda ()
                        (writer detached-session
                                (artifactum-lock-test-record
                                 "art-detached" "detached.txt")
                                (lambda (condition)
                                  (setf second-error condition))))
                      :name "artifactum-detached-index-writer"))
               (is-true (bt:wait-on-semaphore ready :timeout 2.0))
               (is-true (bt:wait-on-semaphore ready :timeout 2.0))
               (bt:signal-semaphore start)
               (bt:signal-semaphore start)
               ;; Both operations have begun but the held transaction lock
               ;; prevents either read-modify-supersede sequence from escaping.
               (is-false (bt:wait-on-semaphore completed :timeout 0.05))
               (bt:release-lock lock)
               (setf lock-held-p nil)
               (bt:join-thread first-thread)
               (setf first-thread nil)
               (bt:join-thread second-thread)
               (setf second-thread nil)
               (is (null first-error))
               (is (null second-error))
               (is (equal (namestring (session-directory live-session))
                          (namestring (session-directory detached-session))))
               (is (not (eq (clawmacs::session-lock live-session)
                            (clawmacs::session-lock detached-session))))
               (let* ((records (clawmacs::artifactum-session-records live))
                      (ids (sort (mapcar (lambda (record) (getf record :id))
                                         records)
                                 #'string<))
                      (index-path
                        (clawmacs::artifactum-session-index-path live-session))
                      (decoded
                        (let ((cl-json:*json-array-type* 'vector))
                          (cl-json:decode-json-from-string
                           (uiop:read-file-string index-path)))))
                 (is (equal '("art-detached" "art-live") ids))
                 (is (= 2 (length decoded)))
                 (is (= 2 (length
                           (clawmacs::artifactum-session-records detached))))))
          (when lock-held-p
            (bt:release-lock lock))
          (bt:signal-semaphore start)
          (bt:signal-semaphore start)
          (when first-thread
            (bt:join-thread first-thread))
          (when second-thread
            (bt:join-thread second-thread)))))))
