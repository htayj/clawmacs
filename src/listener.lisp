(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Listener Buffers
;;; --------------------------------------------------------------------------

(defvar *listener-buffer-name* "*listener*"
  "Default name for the in-buffer Common Lisp listener.")

(defvar *listener-buffer-states* (make-hash-table :test #'eq)
  "Per-buffer listener state, with durable fields saved in sessions.")

(defvar *listener-max-output-chars* 20000
  "Maximum characters retained from one listener evaluation or command.")

(defstruct (listener-state
            (:constructor make-listener-state
                (&key (package-name "CL-USER")
                      directory-stack
                      last-values
                      command-history)))
  "State for a Clawmacs McCLIM-style listener buffer."
  (package-name "CL-USER" :type string)
  (directory-stack nil :type list)
  (last-values nil :type list)
  (command-history nil :type list))

(defun listener-default-package-name ()
  "Return the package name used by new listener buffers."
  (let ((name (and (boundp '*lisp-eval-default-package*)
                   *lisp-eval-default-package*)))
    (if (and (stringp name)
             (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         name))))
        (string-upcase name)
        "CL-USER")))

(defun listener-buffer-p (buf)
  "Return true when BUF is a listener buffer."
  (and buf (eq (buffer-kind buf) :listener)))

(defun listener-buffer-state (buf)
  "Return BUF's listener state, creating it when necessary."
  (unless (listener-buffer-p buf)
    (error "Not a listener buffer: ~A" (and buf (buffer-name buf))))
  (or (gethash buf *listener-buffer-states*)
      (setf (gethash buf *listener-buffer-states*)
            (make-listener-state
             :package-name (listener-default-package-name)))))

(defun listener-current-package (buf)
  "Return the active package object for BUF."
  (or (find-package
       (string-upcase
        (listener-state-package-name (listener-buffer-state buf))))
      (find-package :cl-user)))

(defun listener-set-package (buf name)
  "Set BUF's active package to NAME and return the package object."
  (let* ((package-name
           (string-upcase
            (string-trim '(#\Space #\Tab #\Newline #\Return)
                         (string name))))
         (package (find-package package-name)))
    (unless package
      (error "No package named ~A." package-name))
    (setf (listener-state-package-name (listener-buffer-state buf))
          (package-name package))
    package))

(defun listener-current-directory (buf)
  "Return BUF's active directory as a directory pathname."
  (uiop:ensure-directory-pathname (buffer-working-directory buf)))

(defun listener-prompt-text (buf)
  "Return the package-sensitive prompt for BUF."
  (format nil "~A> " (package-name (listener-current-package buf))))

(defun listener-memory-summary ()
  "Return a compact implementation-specific memory summary."
  #+sbcl
  (format nil "~D MB consed"
          (round (sb-ext:get-bytes-consed) 1048576))
  #-sbcl
  "memory n/a")

(defun listener-wholine-text (buf)
  "Return a McCLIM Listener-inspired status line for BUF."
  (let* ((state (listener-buffer-state buf))
         (user (or (uiop:getenv "USER") "user"))
         (host (machine-instance))
         (directory (namestring (listener-current-directory buf))))
    (format nil "~A@~A  pkg:~A  dir:~A  stack:~D  ~A"
            user
            host
            (package-name (listener-current-package buf))
            directory
            (length (listener-state-directory-stack state))
            (listener-memory-summary))))

(defun listener-truncate-text (text)
  "Bound TEXT to *LISTENER-MAX-OUTPUT-CHARS* characters."
  (let ((string (or text "")))
    (if (> (length string) *listener-max-output-chars*)
        (concatenate 'string
                     (subseq string 0 *listener-max-output-chars*)
                     (format nil "~%[truncated at ~D characters]"
                             *listener-max-output-chars*))
        string)))

(defun listener-safe-value-string (value)
  "Return a bounded printed representation of VALUE."
  (handler-case
      (let ((*print-length* 100)
            (*print-level* 8)
            (*print-circle* t)
            (*print-pretty* nil)
            (*print-readably* nil)
            (*print-escape* t))
        (prin1-to-string value))
    (error ()
      "#<unprintable value>")))

(defun listener-condition-string (condition)
  "Return a safe display string for CONDITION."
  (handler-case
      (let ((*print-length* 50)
            (*print-level* 4)
            (*print-circle* t)
            (*print-pretty* nil)
            (*print-readably* nil))
        (format nil "~A" condition))
    (error ()
      "#<unprintable condition>")))

(defun listener-normalize-package-name (value)
  "Return VALUE as a valid listener package name, defaulting safely."
  (let* ((candidate (and value (string-upcase (string value))))
         (package (and candidate (find-package candidate))))
    (package-name (or package (find-package (listener-default-package-name))))))

(defun listener-serialize-last-values (values)
  "Return VALUES as a vector of printed representations."
  (coerce (mapcar #'listener-safe-value-string values) 'vector))

(defun listener-restore-last-values (items)
  "Return ITEMS restored from printed listener values when readable."
  (loop :for item :in (coerce (or items #()) 'list)
        :collect (if (stringp item)
                     (handler-case
                         (let ((*read-eval* nil)
                               (*package* (find-package :cl-user)))
                           (read-from-string item))
                       (error ()
                         item))
                     item)))

(defun listener-serialize-buffer-state (buf)
  "Return BUF's listener-specific persistence state."
  (let ((state (listener-buffer-state buf)))
    `((:package-name . ,(listener-state-package-name state))
      (:directory-stack
       . ,(coerce (mapcar #'namestring
                          (listener-state-directory-stack state))
                  'vector))
      (:last-values
       . ,(listener-serialize-last-values
           (listener-state-last-values state)))
      (:command-history
       . ,(coerce (copy-list (listener-state-command-history state))
                  'vector)))))

(defun listener-restore-buffer-state (buf persisted-state)
  "Restore PERSISTED-STATE into BUF's listener state."
  (let ((state (listener-buffer-state buf)))
    (setf (listener-state-package-name state)
          (listener-normalize-package-name
           (cdr (assoc :package-name persisted-state)))
          (listener-state-directory-stack state)
          (loop :for item :in (coerce (or (cdr (assoc :directory-stack
                                                      persisted-state))
                                          #())
                                      'list)
                :when item
                  :collect (uiop:ensure-directory-pathname
                            (if (pathnamep item)
                                item
                                (pathname item))))
          (listener-state-last-values state)
          (listener-restore-last-values
           (cdr (assoc :last-values persisted-state)))
          (listener-state-command-history state)
          (loop :for item :in (coerce (or (cdr (assoc :command-history
                                                      persisted-state))
                                          #())
                                      'list)
                :when (stringp item)
                  :collect item)))
  buf)

(defun listener-read-forms (text package)
  "Read every Lisp form from TEXT using PACKAGE."
  (let ((eof (gensym "EOF"))
        (forms nil))
    (with-input-from-string (stream text)
      (let ((*package* package))
        (loop :for form := (read stream nil eof)
              :until (eq form eof)
              :do (push form forms))))
    (nreverse forms)))

(defun listener-format-evaluation-result (stdout stderr values)
  "Return listener text for captured output and VALUES."
  (listener-truncate-text
   (with-output-to-string (stream)
     (let ((out (get-output-stream-string stdout))
           (err (get-output-stream-string stderr)))
       (unless (blank-string-p out)
         (write-string out stream)
         (unless (char= (char out (1- (length out))) #\Newline)
           (terpri stream)))
       (unless (blank-string-p err)
         (write-string ";; error-output" stream)
         (terpri stream)
         (write-string err stream)
         (unless (char= (char err (1- (length err))) #\Newline)
           (terpri stream)))
       (if values
           (dolist (value values)
             (format stream "=> ~A~%" (listener-safe-value-string value)))
           (write-string "=> ; no values" stream))))))

(defun listener-format-error-result (condition stdout stderr)
  "Return listener text for CONDITION and captured output."
  (listener-truncate-text
   (with-output-to-string (stream)
     (let ((out (get-output-stream-string stdout))
           (err (get-output-stream-string stderr)))
       (unless (blank-string-p out)
         (write-string out stream)
         (unless (char= (char out (1- (length out))) #\Newline)
           (terpri stream)))
       (unless (blank-string-p err)
         (write-string ";; error-output" stream)
         (terpri stream)
         (write-string err stream)
         (unless (char= (char err (1- (length err))) #\Newline)
           (terpri stream)))
       (format stream "Error: ~A" (listener-condition-string condition))))))

(defun listener-evaluate-text (buf text)
  "Evaluate TEXT as one or more Lisp forms in BUF's listener package."
  (let* ((state (listener-buffer-state buf))
         (package (listener-current-package buf))
         (stdout (make-string-output-stream))
         (stderr (make-string-output-stream))
         (values nil)
         (result-package-name (package-name package)))
    (handler-case
        (let ((forms (listener-read-forms text package)))
          (unless forms
            (return-from listener-evaluate-text "No form to evaluate."))
          (let ((*package* package)
                (*standard-output* stdout)
                (*trace-output* stderr)
                (*error-output* stderr)
                (*default-pathname-defaults*
                  (listener-current-directory buf)))
            (dolist (form forms)
              (setf values (multiple-value-list (eval form))))
            (setf result-package-name (package-name *package*)))
          (setf (listener-state-package-name state) result-package-name
                (listener-state-last-values state) values
                *last-eval-result* values
                *last-eval-condition* nil)
          (listener-format-evaluation-result stdout stderr values))
      (error (condition)
        (setf (listener-state-last-values state) nil
              *last-eval-result* nil
              *last-eval-condition* condition)
        (listener-format-error-result condition stdout stderr)))))

(defun listener-command-help-text ()
  "Return help for listener comma commands."
  (format nil
          "~{~A~%~}"
          '("McCLIM Listener commands"
            ""
            "Type a Lisp form and press RET to evaluate it."
            "Prefix a line with comma to run a listener command."
            "Prefix a line with #! to run a shell command in the buffer directory."
            ""
            ",Help Commands           show this command list"
            ",Package PACKAGE         change the read/eval package"
            ",Show Directory [DIR]    list files and subdirectories"
            ",Pwd                     show the current directory"
            ",Cd DIR                  change the current directory"
            ",Push Directory DIR      push current directory and cd to DIR"
            ",Pop Directory           restore the previous pushed directory"
            ",Display Directory Stack show the directory stack"
            ",Run COMMAND             run a shell command synchronously"
            ",Background Run COMMAND  start a shell command asynchronously"
            ",Apropos STRING          search symbols visible to the listener"
            ",Describe OBJECT         describe a Lisp object read in the listener package"
            ",Inspect FORM            inspect the first value of FORM with Clouseau when available"
            ",Load File PATH          load a Lisp file"
            ",Compile File PATH       compile a Lisp file"
            ",Compile And Load PATH   compile then load a Lisp file"
            ",Room                    show implementation memory information"
            ",Clear Output History    clear this listener buffer"
            ""
            "This buffer models the McCLIM Listener inside Clawmacs buffers.")))

(defparameter *listener-command-phrases*
  '(("help commands" . :help)
    ("help" . :help)
    ("clear output history" . :clear)
    ("clear" . :clear)
    ("show directory" . :show-directory)
    ("show-directory" . :show-directory)
    ("display directory stack" . :dirs)
    ("directories" . :dirs)
    ("dirs" . :dirs)
    ("push directory" . :pushd)
    ("pushd" . :pushd)
    ("pop directory" . :popd)
    ("popd" . :popd)
    ("run" . :run)
    ("background run" . :background-run)
    ("background-run" . :background-run)
    ("apropos" . :apropos)
    ("describe" . :describe)
    ("inspect" . :inspect)
    ("load file" . :load-file)
    ("load-file" . :load-file)
    ("compile and load" . :compile-and-load)
    ("compile-and-load" . :compile-and-load)
    ("compile file" . :compile-file)
    ("compile-file" . :compile-file)
    ("package" . :package)
    ("in-package" . :package)
    ("eval" . :eval)
    ("room" . :room)
    ("pwd" . :pwd)
    ("cd" . :cd)
    ("exit" . :exit)))

(defun listener-command-prefix-p (text phrase)
  "Return true when TEXT starts with listener command PHRASE."
  (let ((phrase-length (length phrase))
        (text-length (length text)))
    (and (>= text-length phrase-length)
         (string-equal phrase text :end2 phrase-length)
         (or (= text-length phrase-length)
             (whitespace-char-p (char text phrase-length))))))

(defun listener-parse-command (text)
  "Return command keyword and argument string parsed from comma TEXT."
  (let* ((command-text
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (if (and (plusp (length text))
                                 (char= (char text 0) #\,))
                            (subseq text 1)
                            text)))
         (phrases (sort (copy-list *listener-command-phrases*) #'>
                        :key (lambda (entry) (length (car entry))))))
    (dolist (entry phrases
             (error "Unknown listener command: ~A.  Try ,Help Commands."
                    command-text))
      (let ((phrase (car entry)))
        (when (listener-command-prefix-p command-text phrase)
          (return
            (values (cdr entry)
                    (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                      (subseq command-text
                                              (length phrase)))
                    phrase)))))))

(defun listener-merge-pathname (buf text &optional default)
  "Resolve TEXT against BUF's current directory."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (or text "")))
         (raw (if (plusp (length trimmed))
                  trimmed
                  (or default "."))))
    (merge-pathnames raw (listener-current-directory buf))))

(defun listener-resolve-directory (buf text &optional default)
  "Resolve TEXT to an existing directory pathname."
  (let* ((candidate (uiop:ensure-directory-pathname
                     (listener-merge-pathname buf text default)))
         (directory (uiop:directory-exists-p candidate)))
    (unless directory
      (error "Directory does not exist: ~A" (namestring candidate)))
    (uiop:ensure-directory-pathname directory)))

(defun listener-resolve-file (buf text)
  "Resolve TEXT to an existing file pathname."
  (let* ((candidate (listener-merge-pathname buf text nil))
         (file (probe-file candidate)))
    (unless file
      (error "File does not exist: ~A" (namestring candidate)))
    file))

(defun listener-path-display-name (pathname)
  "Return a compact name for PATHNAME."
  (or (and (plusp (length (file-namestring pathname)))
           (file-namestring pathname))
      (let ((parts (pathname-directory pathname)))
        (when (and (consp parts) (cdr parts))
          (format nil "~A/" (car (last parts)))))
      (namestring pathname)))

(defun listener-show-directory (buf args)
  "Return a directory listing for ARGS or BUF's current directory."
  (let* ((directory (listener-resolve-directory buf args "."))
         (subdirs (mapcar #'listener-path-display-name
                          (uiop:subdirectories directory)))
         (files (mapcar #'listener-path-display-name
                        (uiop:directory-files directory))))
    (setf subdirs (sort subdirs #'string<)
          files (sort files #'string<))
    (with-output-to-string (stream)
      (format stream "Directory: ~A~%" (namestring directory))
      (dolist (dir subdirs)
        (format stream "[dir]  ~A~%" dir))
      (dolist (file files)
        (format stream "       ~A~%" file)))))

(defun listener-directory-stack-text (buf)
  "Return BUF's directory stack as display text."
  (let ((stack (listener-state-directory-stack (listener-buffer-state buf))))
    (if stack
        (with-output-to-string (stream)
          (format stream "Directory stack:~%")
          (loop :for directory :in stack
                :for index :from 0
                :do (format stream "~D: ~A~%" index (namestring directory))))
        "Directory stack is empty.")))

(defun listener-run-shell-command (buf command &key background-p)
  "Run COMMAND in BUF's current directory."
  (when (blank-string-p command)
    (error "Shell command is required."))
  (if background-p
      (let ((process
              (uiop:launch-program (list "/bin/sh" "-c" command)
                                   :directory (listener-current-directory buf)
                                   :output :interactive
                                   :error-output :interactive)))
        (format nil "Started background process: ~A" process))
      (multiple-value-bind (stdout stderr code)
          (uiop:run-program (list "/bin/sh" "-c" command)
                            :directory (listener-current-directory buf)
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (listener-truncate-text
         (with-output-to-string (stream)
           (unless (blank-string-p stdout)
             (write-string stdout stream)
             (unless (char= (char stdout (1- (length stdout))) #\Newline)
               (terpri stream)))
           (unless (blank-string-p stderr)
             (write-string ";; stderr" stream)
             (terpri stream)
             (write-string stderr stream)
             (unless (char= (char stderr (1- (length stderr))) #\Newline)
               (terpri stream)))
           (format stream "Exit status: ~A" code))))))

(defun listener-read-object (buf text)
  "Read one object from TEXT in BUF's listener package."
  (when (blank-string-p text)
    (error "A Lisp object or form is required."))
  (let ((*package* (listener-current-package buf)))
    (read-from-string text)))

(defun listener-describe-object (buf text)
  "Return DESCRIBE output for TEXT."
  (with-output-to-string (stream)
    (describe (listener-read-object buf text) stream)))

(defun listener-apropos (buf text)
  "Return APROPOS output for TEXT."
  (declare (ignore buf))
  (when (blank-string-p text)
    (error "Apropos text is required."))
  (with-output-to-string (stream)
    (let ((*standard-output* stream))
      (apropos text))))

(defun listener-eval-first-value (buf text)
  "Evaluate TEXT and return its first value."
  (let* ((package (listener-current-package buf))
         (forms (listener-read-forms text package))
         (values nil))
    (unless forms
      (error "A Lisp form is required."))
    (let ((*package* package)
          (*default-pathname-defaults* (listener-current-directory buf)))
      (dolist (form forms)
        (setf values (multiple-value-list (eval form)))))
    (first values)))

(defun listener-inspect-form (buf text)
  "Inspect the first value of TEXT using Clouseau when available."
  (let ((value (listener-eval-first-value buf text)))
    (cond
      ((fboundp 'mcclim-debug-inspect-target)
       (funcall (symbol-function 'mcclim-debug-inspect-target)
                value
                :label "Listener Inspect")
       (format nil "Inspecting: ~A" (listener-safe-value-string value)))
      (t
       (format nil "Inspector unavailable. Value: ~A"
               (listener-safe-value-string value))))))

(defun listener-room ()
  "Return implementation ROOM output."
  (with-output-to-string (stream)
    (let ((*standard-output* stream))
      (room))))

(defun listener-load-file (buf args &key compile-p load-compiled-p)
  "Load, compile, or compile-and-load ARGS."
  (let ((file (listener-resolve-file buf args)))
    (cond
      ((and compile-p load-compiled-p)
       (let ((fasl (compile-file file)))
         (when fasl
           (load fasl))
         (format nil "Compiled and loaded: ~A" (namestring file))))
      (compile-p
       (let ((fasl (compile-file file)))
         (format nil "Compiled: ~A~@[ -> ~A~]"
                 (namestring file)
                 (and fasl (namestring fasl)))))
      (t
       (load file)
       (format nil "Loaded: ~A" (namestring file))))))

(defun listener-run-command (buf text)
  "Run listener comma command TEXT for BUF."
  (multiple-value-bind (command args)
      (listener-parse-command text)
    (ecase command
      (:help
       (listener-command-help-text))
      (:clear
       (buffer-clear-history-before-input buf)
       "Listener history cleared.")
      (:show-directory
       (listener-show-directory buf args))
      (:dirs
       (listener-directory-stack-text buf))
      (:pushd
       (let* ((state (listener-buffer-state buf))
              (old-directory (listener-current-directory buf))
              (new-directory (listener-resolve-directory buf args nil)))
         (push old-directory (listener-state-directory-stack state))
         (setf (buffer-working-directory buf) new-directory)
         (format nil "Directory: ~A" (namestring new-directory))))
      (:popd
       (let* ((state (listener-buffer-state buf))
              (stack (listener-state-directory-stack state)))
         (unless stack
           (error "Directory stack is empty."))
         (setf (buffer-working-directory buf) (first stack)
               (listener-state-directory-stack state) (rest stack))
         (format nil "Directory: ~A"
                 (namestring (listener-current-directory buf)))))
      (:run
       (listener-run-shell-command buf args))
      (:background-run
       (listener-run-shell-command buf args :background-p t))
      (:apropos
       (listener-apropos buf args))
      (:describe
       (listener-describe-object buf args))
      (:inspect
       (listener-inspect-form buf args))
      (:load-file
       (listener-load-file buf args))
      (:compile-file
       (listener-load-file buf args :compile-p t))
      (:compile-and-load
       (listener-load-file buf args :compile-p t :load-compiled-p t))
      (:package
       (if (blank-string-p args)
           (format nil "Package: ~A"
                   (package-name (listener-current-package buf)))
           (format nil "Package set to ~A"
                   (package-name (listener-set-package buf args)))))
      (:eval
       (listener-evaluate-text buf args))
      (:room
       (listener-room))
      (:pwd
       (format nil "Directory: ~A" (namestring (listener-current-directory buf))))
      (:cd
       (let ((directory (listener-resolve-directory buf args nil)))
         (setf (buffer-working-directory buf) directory)
         (format nil "Directory: ~A" (namestring directory))))
      (:exit
       "Use C-x k to kill this listener buffer."))))

(defun listener-shell-macro-p (text)
  "Return true when TEXT starts with the Listener #! shell macro."
  (and (>= (length text) 2)
       (char= (char text 0) #\#)
       (char= (char text 1) #\!)))

(defun listener-handle-input (buf text)
  "Evaluate or command-dispatch listener TEXT."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (cond
      ((blank-string-p trimmed)
       nil)
      ((listener-shell-macro-p trimmed)
       (listener-run-shell-command
        buf
        (string-left-trim '(#\Space #\Tab)
                          (subseq trimmed 2))))
      ((char= (char trimmed 0) #\,)
       (listener-run-command buf trimmed))
      (t
       (listener-evaluate-text buf text)))))

(defun submit-listener-input (buf)
  "Finalize and run the current listener input in BUF."
  (unless (listener-buffer-p buf)
    (error "Not a listener buffer: ~A" (and buf (buffer-name buf))))
  (let ((input-text (message-text (buffer-input-message buf))))
    (unless (blank-string-p input-text)
      (let ((prompt (listener-prompt-text buf)))
        (declare (ignorable prompt))
        (put-message-metadata (buffer-input-message buf)
                              :listener-prompt prompt))
      (push input-text
            (listener-state-command-history (listener-buffer-state buf)))
      (buffer-finalize-input buf)
      (let ((user-message (message-prev (buffer-input-message buf))))
        (when user-message
          (setf (message-face-set user-message)
                (gethash :user (buffer-face-registry buf)))))
      (handler-case
          (let ((output (listener-handle-input buf input-text)))
            (unless (blank-string-p output)
              (buffer-insert-read-only-message buf :listener output
                                               :record-p nil)))
        (error (condition)
          (buffer-insert-system-message
           buf
           (format nil "Listener error: ~A"
                   (listener-condition-string condition))
           :record-p nil)))
      (setf (buffer-scroll-offset buf) 0)
      (notify-buffer-display-change buf :listener)
      :redraw)))

(defun listener-welcome-text (buf)
  "Return the initial listener help text for BUF."
  (format nil
          "~A~%~A~%~A"
          "McCLIM-style Common Lisp Listener"
          "Type Lisp forms to evaluate. Type ,Help Commands for commands."
          (listener-wholine-text buf)))

(defun make-listener-buffer
    (&key (name *listener-buffer-name*)
          (package-name (listener-default-package-name))
          (working-directory (truename "."))
          (add-to-ring-p nil))
  "Create a Clawmacs listener buffer."
  (let ((buf (make-buffer name
                          :agent-name "listener"
                          :kind :listener
                          :working-directory
                          (uiop:ensure-directory-pathname working-directory)
                          :major-mode "listener")))
    (initialize-buffer-display-defaults buf)
    (setf (gethash buf *listener-buffer-states*)
          (make-listener-state
           :package-name
           (package-name
            (or (find-package (string-upcase package-name))
                (error "No package named ~A." package-name)))))
    (buffer-insert-read-only-message buf :listener (listener-welcome-text buf)
                                     :record-p nil)
    (when add-to-ring-p
      (add-buffer-to-ring buf))
    buf))

(defun ensure-listener-buffer
    (&key (name *listener-buffer-name*)
          (package-name (listener-default-package-name))
          (working-directory (truename ".")))
  "Return an existing listener buffer or create one."
  (or (find-if #'listener-buffer-p *buffer-ring*)
      (make-listener-buffer :name name
                            :package-name package-name
                            :working-directory working-directory
                            :add-to-ring-p t)))

(register-buffer-type
 :listener
 :description "Interactive Common Lisp listener buffer."
 :major-mode "listener"
 :serialize-state-function 'listener-serialize-buffer-state
 :restore-state-function 'listener-restore-buffer-state)
