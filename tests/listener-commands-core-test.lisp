(in-package :rplaca/tests)

(in-suite listener-commands-core-suite)

(test listener-context-defaults-from-lisp-eval-package
  (let ((rplaca::*lisp-eval-default-package* "cl-user"))
    (let ((context (rplaca::make-listener-context)))
      (is (string= "CL-USER" (rplaca::listener-context-package-name context)))
      (is (null (rplaca::listener-context-directory-stack context)))
      (is (eq :eval (rplaca::listener-context-input-mode context))))))

(test listener-context-directory-transitions-are-immutable
  (let* ((first-directory (uiop:ensure-directory-pathname #P"/tmp/first/"))
         (second-directory (uiop:ensure-directory-pathname #P"/tmp/second/"))
         (initial (rplaca::make-listener-context))
         (first (rplaca::listener-context-push-directory
                 initial first-directory))
         (second (rplaca::listener-context-push-directory
                  first second-directory)))
    (is (null (rplaca::listener-context-directory-stack initial)))
    (is (equal (list first-directory)
               (rplaca::listener-context-directory-stack first)))
    (is (equal (list second-directory first-directory)
               (rplaca::listener-context-directory-stack second)))
    (multiple-value-bind (popped-context popped-directory)
        (rplaca::listener-context-pop-directory second)
      (is (equal second-directory popped-directory))
      (is (equal (list first-directory)
                 (rplaca::listener-context-directory-stack popped-context)))
      (is (equal (list second-directory first-directory)
                 (rplaca::listener-context-directory-stack second))))))

(test listener-context-package-transition-validates-without-global-mutation
  (let* ((initial (rplaca::make-listener-context :package-name "CL-USER"))
         (before *package*)
         (updated (rplaca::listener-context-set-package
                   initial "rplaca/tests")))
    (is (string= "CL-USER" (rplaca::listener-context-package-name initial)))
    (is (string= "RPLACA/TESTS"
                 (rplaca::listener-context-package-name updated)))
    (is (eq before *package*))
    (signals rplaca::unknown-listener-package
      (rplaca::listener-context-set-package initial "NO-SUCH-PACKAGE"))))

(test listener-context-input-mode-transition-validates
  (let* ((initial (rplaca::make-listener-context))
         (say (rplaca::listener-context-set-input-mode initial :say)))
    (is (eq :eval (rplaca::listener-context-input-mode initial)))
    (is (eq :say (rplaca::listener-context-input-mode say)))
    (signals error
      (rplaca::listener-context-set-input-mode initial :unknown))))

(test listener-context-empty-directory-pop-signals
  (signals rplaca::empty-listener-directory-stack
    (rplaca::listener-context-pop-directory
     (rplaca::make-listener-context))))

(test listener-shell-helper-is-bounded-and-preserves-exit-status
  (let* ((directory (truename "."))
         (success
           (rplaca::run-listener-shell-command
            "printf 'out'; printf 'err' >&2"
            directory :timeout 5))
         (nonzero
           (rplaca::run-listener-shell-command
            "printf 'bad' >&2; exit 7"
            directory :timeout 5))
         (bounded
           (rplaca::run-listener-shell-command
            "printf '123456789'"
            directory :timeout 5 :output-limit 5)))
    (is (= 0 (getf success :exit-code)))
    (is (string= "out" (getf success :stdout)))
    (is (string= "err" (getf success :stderr)))
    (is (string= (namestring (uiop:ensure-directory-pathname directory))
                 (getf success :directory)))
    (is (= 7 (getf nonzero :exit-code)))
    (is (string= "bad" (getf nonzero :stderr)))
    (is (= 5 (length (getf bounded :stdout))))
    (is-true (getf bounded :stdout-truncated-p))
    (signals error
      (rplaca::run-listener-shell-command "" directory :timeout 5))
    (signals error
      (rplaca::run-listener-shell-command nil directory :timeout 5))))

(defun make-listener-utility-test-root ()
  (let ((root (merge-pathnames
               (format nil "rplaca-listener-utility-~36R-~36R/"
                       (get-universal-time)
                       (random most-positive-fixnum))
               (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    root))

(defmacro with-listener-utility-test-root ((root) &body body)
  `(let ((,root (make-listener-utility-test-root)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (uiop:delete-directory-tree ,root
                                     :validate t
                                     :if-does-not-exist :ignore)))))

(defun write-listener-utility-test-file (pathname contents)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream))
  pathname)

(test listener-pushd-popd-and-dirs-are-immutable
  (with-listener-utility-test-root (root)
    (let* ((first (uiop:pathname-directory-pathname
                   (ensure-directories-exist
                    (merge-pathnames #P"first/.keep" root))))
           (second (uiop:pathname-directory-pathname
                    (ensure-directories-exist
                     (merge-pathnames #P"second/.keep" root))))
           (initial (rplaca::make-listener-context)))
      (multiple-value-bind (pushed current)
          (rplaca::listener-command-pushd initial root first)
        (is (equal (uiop:ensure-directory-pathname first) current))
        (is (null (rplaca::listener-command-dirs initial)))
        (is (equal (list (uiop:ensure-directory-pathname root))
                   (rplaca::listener-command-dirs pushed)))
        (multiple-value-bind (pushed-again next)
            (rplaca::listener-command-pushd pushed current second)
          (is (equal (uiop:ensure-directory-pathname second) next))
          (is (equal (list (uiop:ensure-directory-pathname current)
                           (uiop:ensure-directory-pathname root))
                     (rplaca::listener-command-dirs pushed-again)))
          (multiple-value-bind (popped restored)
              (rplaca::listener-command-popd pushed-again)
            (is (equal (uiop:ensure-directory-pathname current) restored))
            (is (equal (rplaca::listener-command-dirs pushed)
                       (rplaca::listener-command-dirs popped)))))))))

(test listener-pushd-validates-target-directory
  (with-listener-utility-test-root (root)
    (signals error
      (rplaca::listener-command-pushd
       (rplaca::make-listener-context)
       root
       (merge-pathnames #P"missing/" root)))))

(test listener-apropos-results-use-context-package
  (let* ((context (rplaca::make-listener-context :package-name "RPLACA/TESTS"))
         (name (format nil "TODO12-APROPOS-~36R" (random most-positive-fixnum)))
         (symbol (intern name :rplaca/tests)))
    (unwind-protect
         (is (member symbol (rplaca::listener-context-apropos context name)))
      (unintern symbol :rplaca/tests))))

(test listener-describe-and-inspect-use-explicit-context
  (let ((context (rplaca::make-listener-context :package-name "CL-USER")))
    (is (search "CAR" (rplaca::listener-context-describe context 'car)))
    (is (= 42 (rplaca::listener-context-inspect context '(+ 40 2))))))

(test listener-load-file-loads-an-existing-file
  (with-listener-utility-test-root (root)
    (let* ((context (rplaca::make-listener-context :package-name "RPLACA/TESTS"))
           (source (merge-pathnames #P"load-me.lisp" root)))
      (write-listener-utility-test-file
       source (format nil "(in-package :rplaca/tests)~%~
                           (defparameter *todo12-loaded* :loaded)~%"))
      (unwind-protect
           (progn
             (is (equal (truename source)
                        (rplaca::listener-context-load-file context source)))
             (is (eq :loaded (symbol-value
                              (find-symbol "*TODO12-LOADED*" :rplaca/tests)))))
        (let ((symbol (find-symbol "*TODO12-LOADED*" :rplaca/tests)))
          (when (and symbol (boundp symbol))
            (makunbound symbol)))))))

(test listener-compile-file-compiles-an-existing-file
  (with-listener-utility-test-root (root)
    (let* ((context (rplaca::make-listener-context :package-name "CL-USER"))
           (source (merge-pathnames #P"compile-me.lisp" root)))
      (write-listener-utility-test-file
       source (format nil "(in-package :cl-user)~%~
                           (defun todo12-compiled-function () 12)~%"))
      (multiple-value-bind (output warnings-p failure-p)
          (rplaca::listener-context-compile-file context source)
        (declare (ignore warnings-p))
        (is (pathnamep output))
        (is (probe-file output))
        (is-false failure-p)))))

(test listener-in-package-validates-and-returns-new-context
  (let* ((initial (rplaca::make-listener-context :package-name "CL-USER"))
         (updated (rplaca::listener-context-in-package
                   initial (find-package :keyword))))
    (is (string= "CL-USER" (rplaca::listener-context-package-name initial)))
    (is (string= "KEYWORD" (rplaca::listener-context-package-name updated)))
    (signals rplaca::unknown-listener-package
      (rplaca::listener-context-in-package initial "NO-SUCH-PACKAGE"))))

(test listener-room-report-produces-output
  (let ((report (rplaca::listener-context-room
                 (rplaca::make-listener-context))))
    (is (stringp report))
    (is (plusp (length report)))))

(test listener-help-commands-is-generated-from-supplied-command-names
  (let ((text (rplaca::listener-context-help-commands
               (rplaca::make-listener-context)
               '("Room" "Apropos" "Push Directory"))))
    (is (search "Available listener commands" text))
    (is (search ",Apropos" text))
    (is (search ",Push Directory" text))
    (is (search ",Room" text))
    (is (< (search ",Apropos" text) (search ",Room" text)))
    (is (null (search ",Load File" text)))))
