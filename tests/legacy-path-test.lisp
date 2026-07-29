(in-package :clawmacs/tests)

(in-suite legacy-path-suite)

(defun make-legacy-path-test-root ()
  (let ((root (merge-pathnames
               (format nil "clawmacs-legacy-path-~36R-~36R/"
                       (get-universal-time)
                       (random most-positive-fixnum))
               (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    root))

(defmacro with-legacy-path-test-root ((root) &body body)
  `(let ((,root (make-legacy-path-test-root)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,root
                                                   :validate t
                                                   :if-does-not-exist :ignore)))))

(defun write-legacy-path-test-file (path text)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text stream))
  path)

(test canonical-path-wins-without-merging
  (with-legacy-path-test-root (root)
    (let* ((canonical (merge-pathnames #P"rplaca/config.json" root))
           (legacy (merge-pathnames #P"clawmacs/config.json" root))
           (warnings nil))
      (write-legacy-path-test-file canonical "canonical")
      (write-legacy-path-test-file legacy "legacy")
      (let ((clawmacs::*legacy-path-warning-function*
              (lambda (control &rest arguments)
                (push (apply #'format nil control arguments) warnings))))
        (clawmacs::reset-legacy-path-warnings)
        (let ((selection
                (clawmacs::select-migration-path
                 canonical legacy :label "test configuration")))
          (is (equal canonical
                     (clawmacs::legacy-path-selection-read-path selection)))
          (is (equal canonical
                     (clawmacs::legacy-path-selection-write-path selection)))
          (is (eq :conflict
                  (clawmacs::legacy-path-selection-source selection)))
          (is (= 1 (length warnings)))
          (is (search "not merging" (first warnings))))))))

(test legacy-inert-data-is-read-only-fallback
  (with-legacy-path-test-root (root)
    (let* ((canonical (merge-pathnames #P"rplaca/config.json" root))
           (legacy (merge-pathnames #P"clawmacs/config.json" root))
           (warnings nil))
      (write-legacy-path-test-file legacy "legacy")
      (let ((clawmacs::*legacy-path-warning-function*
              (lambda (control &rest arguments)
                (push (apply #'format nil control arguments) warnings))))
        (clawmacs::reset-legacy-path-warnings)
        (let ((selection
                (clawmacs::select-migration-path
                 canonical legacy :label "test configuration")))
          (is (equal legacy
                     (clawmacs::legacy-path-selection-read-path selection)))
          (is (equal canonical
                     (clawmacs::legacy-path-selection-write-path selection)))
          (is (eq :legacy
                  (clawmacs::legacy-path-selection-source selection)))
          (is (= 1 (length warnings)))
          (is (search "read-only fallback" (first warnings)))
          (is-false (probe-file canonical)))))))

(test legacy-executable-configuration-is-never-loaded
  (with-legacy-path-test-root (root)
    (let* ((canonical (merge-pathnames #P"rplaca/init.lisp" root))
           (legacy (merge-pathnames #P"clawmacs/init.lisp" root))
           (warnings nil))
      (write-legacy-path-test-file legacy "(error \"must not load\")")
      (let ((clawmacs::*legacy-path-warning-function*
              (lambda (control &rest arguments)
                (push (apply #'format nil control arguments) warnings))))
        (clawmacs::reset-legacy-path-warnings)
        (let ((selection
                (clawmacs::select-migration-path
                 canonical legacy
                 :label "user init"
                 :executable-p t)))
          (is (null
               (clawmacs::legacy-path-selection-read-path selection)))
          (is (equal canonical
                     (clawmacs::legacy-path-selection-write-path selection)))
          (is (eq :legacy-executable
                  (clawmacs::legacy-path-selection-source selection)))
          (is (= 1 (length warnings)))
          (is (search "will not be loaded automatically" (first warnings))))))))

(test absent-path-selects-canonical-for-read-and-write
  (with-legacy-path-test-root (root)
    (let* ((canonical (merge-pathnames #P"rplaca/config.json" root))
           (legacy (merge-pathnames #P"clawmacs/config.json" root))
           (selection
             (clawmacs::select-migration-path canonical legacy)))
      (is (equal canonical
                 (clawmacs::legacy-path-selection-read-path selection)))
      (is (equal canonical
                 (clawmacs::legacy-path-selection-write-path selection)))
      (is (eq :absent (clawmacs::legacy-path-selection-source selection))))))

(test migration-warning-is-emitted-once-per-path
  (with-legacy-path-test-root (root)
    (let* ((canonical (merge-pathnames #P"rplaca/config.json" root))
           (legacy (merge-pathnames #P"clawmacs/config.json" root))
           (warnings nil))
      (write-legacy-path-test-file legacy "legacy")
      (let ((clawmacs::*legacy-path-warning-function*
              (lambda (control &rest arguments)
                (push (apply #'format nil control arguments) warnings))))
        (clawmacs::reset-legacy-path-warnings)
        (dotimes (index 3)
          (declare (ignore index))
          (clawmacs::migration-read-path canonical legacy :label "test"))
        (is (= 1 (length warnings)))))))

(test canonical-probe-errors-fail-closed
  (with-legacy-path-test-root (root)
    (let ((legacy (merge-pathnames #P"clawmacs/config.json" root)))
      (write-legacy-path-test-file legacy "legacy")
      (signals error
        (clawmacs::select-migration-path
         (merge-pathnames (make-pathname :name :wild :type :wild) root)
         legacy)))))

(test canonical-created-during-probe-wins
  (with-legacy-path-test-root (root)
    (let* ((canonical (merge-pathnames #P"rplaca/config.json" root))
           (legacy (merge-pathnames #P"clawmacs/config.json" root))
           (canonical-probes 0)
           (real-probe #'probe-file))
      (write-legacy-path-test-file legacy "legacy")
      (let ((clawmacs::*migration-path-probe-function*
              (lambda (path)
                (when (equal path canonical)
                  (incf canonical-probes))
                (when (and (equal path canonical)
                           (= canonical-probes 2))
                  (write-legacy-path-test-file canonical "canonical"))
                (funcall real-probe path))))
        (let ((selection
                (clawmacs::select-migration-path canonical legacy)))
          (is (= 2 canonical-probes))
          (is (equal canonical
                     (clawmacs::legacy-path-selection-read-path selection)))
          (is (eq :conflict
                  (clawmacs::legacy-path-selection-source selection))))))))

(test canonical-executable-path-still-wins
  (with-legacy-path-test-root (root)
    (let ((canonical (merge-pathnames #P"rplaca/init.lisp" root))
          (legacy (merge-pathnames #P"clawmacs/init.lisp" root)))
      (write-legacy-path-test-file canonical "(values)")
      (write-legacy-path-test-file legacy "(error \"legacy\")")
      (let ((selection
              (clawmacs::select-migration-path
               canonical legacy :executable-p t)))
        (is (equal canonical
                   (clawmacs::legacy-path-selection-read-path selection)))
        (is (eq :conflict
                (clawmacs::legacy-path-selection-source selection)))))))

(test migration-read-roots-never-merges
  (with-legacy-path-test-root (root)
    (let ((canonical (merge-pathnames #P"rplaca/" root))
          (legacy (merge-pathnames #P"clawmacs/" root)))
      (ensure-directories-exist (merge-pathnames #P".keep" canonical))
      (ensure-directories-exist (merge-pathnames #P".keep" legacy))
      (let ((roots (clawmacs::migration-read-roots canonical legacy)))
        (is (= 1 (length roots)))
        (is (equal canonical (first roots))))
      (is (equal canonical
                 (clawmacs::migration-write-path canonical))))))

(test concurrent-warning-is-emitted-once
  (with-legacy-path-test-root (root)
    (let* ((canonical (merge-pathnames #P"rplaca/config.json" root))
           (legacy (merge-pathnames #P"clawmacs/config.json" root))
           (warning-lock (bt:make-lock "legacy warning test"))
           (warning-count 0))
      (write-legacy-path-test-file legacy "legacy")
      (let ((warning-function
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (bt:with-lock-held (warning-lock)
                  (incf warning-count)))))
        (clawmacs::reset-legacy-path-warnings)
        (let ((threads
                (loop :repeat 8
                      :collect
                      (bt:make-thread
                       (lambda ()
                         (let ((clawmacs::*legacy-path-warning-function*
                                 warning-function))
                           (clawmacs::select-migration-path
                            canonical legacy)))))))
          (dolist (thread threads)
            (bt:join-thread thread)))
        (is (= 1 warning-count))))))

(test warning-callback-may-resolve-path-reentrantly
  (with-legacy-path-test-root (root)
    (let* ((canonical (merge-pathnames #P"rplaca/config.json" root))
           (legacy (merge-pathnames #P"clawmacs/config.json" root))
           (nested-canonical
             (merge-pathnames #P"rplaca/nested.json" root))
           (nested-legacy
             (merge-pathnames #P"clawmacs/nested.json" root))
           (nested-selection nil))
      (write-legacy-path-test-file legacy "legacy")
      (let ((clawmacs::*legacy-path-warning-function*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (setf nested-selection
                      (clawmacs::select-migration-path
                       nested-canonical nested-legacy)))))
        (clawmacs::reset-legacy-path-warnings)
        (is (eq :legacy
                (clawmacs::legacy-path-selection-source
                 (clawmacs::select-migration-path canonical legacy))))
        (is (eq :absent
                (clawmacs::legacy-path-selection-source nested-selection)))))))
