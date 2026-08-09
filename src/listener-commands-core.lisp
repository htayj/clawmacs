(in-package :rplaca)

(defconstant +listener-shell-output-limit+ 20000)

(define-condition unknown-listener-package (error)
  ((name
    :initarg :name
    :reader unknown-listener-package-name))
  (:report
   (lambda (condition stream)
     (format stream "No package named ~A."
             (unknown-listener-package-name condition)))))

(define-condition empty-listener-directory-stack (error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (write-string "Directory stack is empty." stream))))

(defstruct (listener-context
            (:constructor %make-listener-context
                (&key package-name directory-stack input-mode)))
  (package-name "CL-USER" :type string)
  (directory-stack nil :type list)
  (input-mode :eval :type (member :eval :say)))

(defun listener-context-default-package-name ()
  (let ((name (and (boundp '*lisp-eval-default-package*)
                   *lisp-eval-default-package*)))
    (if (and (stringp name)
             (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         name))))
        (string-upcase name)
        "CL-USER")))

(defun resolve-listener-context-package-name (name)
  (let* ((text (and name (ignore-errors (string name))))
         (trimmed (and text
                       (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    text)))
         (normalized (and trimmed (string-upcase trimmed)))
         (package (and normalized
                       (plusp (length normalized))
                       (find-package normalized))))
    (unless package
      (error 'unknown-listener-package :name name))
    normalized))

(defun make-listener-context
    (&key (package-name (listener-context-default-package-name))
          directory-stack
          (input-mode :eval))
  (require-listener-input-mode input-mode)
  (%make-listener-context
   :package-name (resolve-listener-context-package-name package-name)
   :directory-stack directory-stack
   :input-mode input-mode))

(defun listener-context-with
    (context &key
               (package-name (listener-context-package-name context))
               (directory-stack (listener-context-directory-stack context))
               (input-mode (listener-context-input-mode context)))
  (%make-listener-context
   :package-name package-name
   :directory-stack directory-stack
   :input-mode input-mode))

(defun listener-context-push-directory (context directory)
  (listener-context-with
   context
   :directory-stack
   (cons (uiop:ensure-directory-pathname directory)
         (listener-context-directory-stack context))))

(defun listener-context-pop-directory (context)
  (let ((stack (listener-context-directory-stack context)))
    (unless stack
      (error 'empty-listener-directory-stack))
    (values (listener-context-with context :directory-stack (rest stack))
            (first stack))))

(defun listener-context-set-package (context name)
  (listener-context-with
   context :package-name (resolve-listener-context-package-name name)))

(defun listener-context-set-input-mode (context mode)
  (require-listener-input-mode mode)
  (listener-context-with context :input-mode mode))

(defun run-listener-shell-command
    (command directory
     &key (timeout *interactive-subprocess-default-timeout*)
          (output-limit +listener-shell-output-limit+))
  (when (blank-string-p command)
    (error "Shell command is required."))
  (run-interactive-subprocess
   command
   :directory (uiop:ensure-directory-pathname directory)
   :timeout timeout
   :output-limit output-limit))

(defun listener-context-package (context)
  (check-type context listener-context)
  (or (find-package (listener-context-package-name context))
      (error 'unknown-listener-package
             :name (listener-context-package-name context))))

(defun listener-existing-directory (directory)
  (let* ((candidate (uiop:ensure-directory-pathname directory))
         (existing (uiop:directory-exists-p candidate)))
    (unless existing
      (error "Directory does not exist: ~A" (namestring candidate)))
    (uiop:ensure-directory-pathname existing)))

(defun listener-context-current-directory (context directory)
  (check-type context listener-context)
  (uiop:ensure-directory-pathname directory))

(defun listener-command-pushd (context current-directory new-directory)
  (let ((current (listener-context-current-directory context current-directory))
        (target (listener-existing-directory new-directory)))
    (values (listener-context-push-directory context current)
            target)))

(defun listener-command-popd (context)
  (check-type context listener-context)
  (listener-context-pop-directory context))

(defun listener-command-dirs (context)
  (check-type context listener-context)
  (copy-list (listener-context-directory-stack context)))

(defun listener-context-apropos (context text)
  (when (blank-string-p text)
    (error "Apropos text is required."))
  (sort (copy-list (apropos-list text (listener-context-package context)))
        (lambda (left right)
          (let ((left-name (symbol-name left))
                (right-name (symbol-name right)))
            (if (string= left-name right-name)
                (string< (package-name (symbol-package left))
                         (package-name (symbol-package right)))
                (string< left-name right-name))))))

(defun listener-context-describe (context object)
  (let ((*package* (listener-context-package context)))
    (with-output-to-string (stream)
      (describe object stream))))

(defun listener-context-inspect (context form)
  (let ((*package* (listener-context-package context)))
    (eval form)))

(defun listener-existing-file (pathname)
  (or (probe-file pathname)
      (error "File does not exist: ~A" (namestring pathname))))

(defun listener-context-load-file (context pathname)
  (let ((file (listener-existing-file pathname))
        (*package* (listener-context-package context)))
    (load file)
    file))

(defun listener-context-compile-file (context pathname)
  (let ((file (listener-existing-file pathname))
        (*package* (listener-context-package context)))
    (compile-file file)))

(defun listener-context-in-package (context package)
  (listener-context-set-package
   context
   (if (packagep package) (package-name package) package)))

(defun listener-context-room (context)
  (let ((*package* (listener-context-package context)))
    (with-output-to-string (stream)
      (let ((*standard-output* stream))
        (room)))))

(defun listener-context-help-commands (context command-names)
  (check-type context listener-context)
  (let ((names (sort (remove-duplicates (copy-list command-names)
                                        :test #'string=)
                     #'string-lessp)))
    (with-output-to-string (stream)
      (write-line "Available listener commands" stream)
      (terpri stream)
      (dolist (name names)
        (format stream ",~A~%" name)))))
