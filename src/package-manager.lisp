(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Package Loader
;;; --------------------------------------------------------------------------

(defvar *packages-directory*
  (merge-pathnames #P".clawmacs.d/packages/" (user-homedir-pathname))
  "Directory where user-installed Clawmacs packages are cloned.")

(defvar *loaded-packages* (make-hash-table :test #'equal)
  "Session-local registry of package install directories already loaded.")

(defun emit-package-warning (format-string &rest format-args)
  "Report a non-fatal package warning and return NIL."
  (let ((message (apply #'format nil format-string format-args)))
    (format *error-output* "~&;; Warning: ~A~%" message)
    (file-debug-log "package" "~A" message)
    nil))

(defun normalize-package-source-type (source-type)
  "Normalize SOURCE-TYPE to a keyword, defaulting NIL to :GIT."
  (cond
    ((null source-type) :git)
    ((keywordp source-type) source-type)
    ((symbolp source-type)
     (intern (string-upcase (symbol-name source-type)) :keyword))
    ((stringp source-type)
     (intern (string-upcase source-type) :keyword))
    (t nil)))

(defun normalize-package-repo (repo)
  "Normalize REPO to a string, or NIL when unsupported."
  (cond
    ((stringp repo) repo)
    ((pathnamep repo) (namestring repo))
    (t nil)))

(defun string-suffix-p (suffix string)
  "Return non-nil when STRING ends with SUFFIX."
  (let ((suffix-length (length suffix))
        (string-length (length string)))
    (and (<= suffix-length string-length)
         (string= suffix string
                  :start1 0
                  :end1 suffix-length
                  :start2 (- string-length suffix-length)
                  :end2 string-length))))

(defun repo-display-name (repo)
  "Return a human-readable package name derived from REPO."
  (let* ((trimmed (string-right-trim "/" repo))
         (slash-pos (position #\/ trimmed :from-end t))
         (colon-pos (position #\: trimmed :from-end t))
         (separator (cond
                      ((and slash-pos colon-pos) (max slash-pos colon-pos))
                      (slash-pos slash-pos)
                      (colon-pos colon-pos)
                      (t nil)))
         (component (if separator
                        (subseq trimmed (1+ separator))
                        trimmed)))
    (if (string-suffix-p ".git" component)
        (subseq component 0 (- (length component) 4))
        component)))

(defun filesystem-safe-component (string)
  "Convert STRING into a filesystem-safe lowercase directory component."
  (let ((sanitized
          (with-output-to-string (out)
            (let ((wrote-dash-p nil))
              (loop :for char :across string
                    :for safe-char := (if (alphanumericp char)
                                          (char-downcase char)
                                          #\-)
                    :do (cond
                          ((char= safe-char #\-)
                           (unless wrote-dash-p
                             (write-char safe-char out)
                             (setf wrote-dash-p t)))
                          (t
                           (write-char safe-char out)
                           (setf wrote-dash-p nil))))))))
    (let ((trimmed (string-trim "-" sanitized)))
      (if (plusp (length trimmed))
          trimmed
          "package"))))

(defun package-source-hash (source)
  "Return a deterministic hexadecimal hash for SOURCE."
  (let ((hash #xCBF29CE484222325)
        (prime #x100000001B3))
    (loop :for char :across source
          :do (setf hash (logxor hash (char-code char))
                    hash (ldb (byte 64 0) (* hash prime))))
    (format nil "~16,'0X" hash)))

(defun package-install-directory (source-type repo)
  "Return the directory pathname where SOURCE-TYPE/REPO should be installed."
  (let* ((source-label (string-downcase (symbol-name source-type)))
         (repo-label (filesystem-safe-component (repo-display-name repo)))
         (digest (string-downcase (package-source-hash repo)))
         (dir-name (format nil "~A-~A-~A/" source-label repo-label digest)))
    (uiop:ensure-directory-pathname
     (merge-pathnames dir-name *packages-directory*))))

(defun package-install-key (install-dir)
  "Return the hash-table key used for INSTALL-DIR."
  (namestring (uiop:ensure-directory-pathname install-dir)))

(defun package-output-summary (stdout stderr exit-code)
  "Format subprocess output for diagnostics."
  (let ((parts nil))
    (when (and stdout (plusp (length stdout)))
      (push stdout parts))
    (when (and stderr (plusp (length stderr)))
      (push stderr parts))
    (if parts
        (format nil "exit code ~D~%~{~A~^~%~}" exit-code (nreverse parts))
        (format nil "exit code ~D" exit-code))))

(defun ensure-packages-directory-exists ()
  "Create *PACKAGES-DIRECTORY* if needed."
  (ensure-directories-exist (merge-pathnames #P".keep" *packages-directory*)))

(defun ensure-package-installed (repo install-dir)
  "Clone REPO into INSTALL-DIR when missing. Returns non-nil on success."
  (ensure-packages-directory-exists)
  (if (probe-file install-dir)
      t
      (handler-case
          (multiple-value-bind (stdout stderr exit-code)
              (uiop:run-program (list "git" "clone" repo (namestring install-dir))
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (if (zerop exit-code)
                (progn
                  (file-debug-log "package"
                                  "cloned ~A into ~A"
                                  repo
                                  (namestring install-dir))
                  t)
                (emit-package-warning "Failed to clone package from ~A into ~A: ~A"
                                      repo
                                      (namestring install-dir)
                                      (package-output-summary stdout stderr exit-code))))
        (error (e)
          (emit-package-warning "Failed to start git clone for ~A: ~A" repo e)))))

(defun package-manifest-plist-p (manifest)
  "Return non-nil when MANIFEST is a proper property list."
  (and (listp manifest)
       (handler-case
           (evenp (length manifest))
         (type-error ()
           nil))))

(defun manifest-package-name (value)
  "Normalize a manifest :NAME value to a lowercase string."
  (cond
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (and (plusp (length trimmed))
            (string-downcase trimmed))))
    ((symbolp value)
     (string-downcase (symbol-name value)))
    (t nil)))

(defun manifest-entrypoint-pathname (value)
  "Normalize a manifest :ENTRYPOINT value to a relative pathname."
  (let ((pathname
          (cond
            ((stringp value) (pathname value))
            ((pathnamep value) value)
            (t nil))))
    (when pathname
      (let ((directory (pathname-directory pathname)))
        (when (and (not (uiop:absolute-pathname-p pathname))
                   (not (member :absolute directory))
                   (not (member :up directory))
                   (not (member :back directory)))
          pathname)))))

(defun read-package-manifest (package-root)
  "Read and validate PACKAGE-ROOT/manifest.lisp.
Returns a normalized plist or NIL on failure."
  (let ((manifest-path (merge-pathnames "manifest.lisp" package-root)))
    (unless (probe-file manifest-path)
      (return-from read-package-manifest
        (emit-package-warning "Package at ~A is missing manifest.lisp"
                              (namestring package-root))))
    (handler-case
        (let* ((*read-eval* nil)
               (manifest
                 (with-open-file (stream manifest-path :direction :input)
                   (read stream nil :eof))))
          (when (eq manifest :eof)
            (return-from read-package-manifest
              (emit-package-warning "Package manifest ~A is empty"
                                    (namestring manifest-path))))
          (unless (package-manifest-plist-p manifest)
            (return-from read-package-manifest
              (emit-package-warning "Package manifest ~A must contain a property list"
                                    (namestring manifest-path))))
          (let* ((missing (list nil))
                 (raw-name (getf manifest :name missing))
                 (raw-entrypoint (getf manifest :entrypoint missing))
                 (name (and (not (eq raw-name missing))
                            (manifest-package-name raw-name)))
                 (entrypoint (and (not (eq raw-entrypoint missing))
                                  (manifest-entrypoint-pathname raw-entrypoint))))
            (unless name
              (return-from read-package-manifest
                (emit-package-warning "Package manifest ~A must define :name as a symbol or string"
                                      (namestring manifest-path))))
            (unless entrypoint
              (return-from read-package-manifest
                (emit-package-warning "Package manifest ~A must define :entrypoint as a relative pathname"
                                      (namestring manifest-path))))
            (let ((resolved-entrypoint (merge-pathnames entrypoint package-root)))
              (unless (probe-file resolved-entrypoint)
                (return-from read-package-manifest
                  (emit-package-warning "Package ~A entrypoint ~A does not exist"
                                        name
                                        (namestring resolved-entrypoint))))
              (list :name name
                    :entrypoint resolved-entrypoint))))
      (error (e)
        (emit-package-warning "Failed to read package manifest ~A: ~A"
                              (namestring manifest-path)
                              e)))))

(defun load-package-entrypoint (manifest package-root install-dir)
  "Load MANIFEST's entrypoint from PACKAGE-ROOT unless INSTALL-DIR is loaded."
  (let* ((install-key (package-install-key install-dir))
         (package-name (getf manifest :name))
         (entrypoint (getf manifest :entrypoint)))
    (when (gethash install-key *loaded-packages*)
      (return-from load-package-entrypoint t))
    (handler-case
        (let ((*default-pathname-defaults* package-root)
              (*package* (find-package :clawmacs)))
          (load entrypoint :verbose nil :print nil)
          (setf (gethash install-key *loaded-packages*) package-name)
          (file-debug-log "package"
                          "loaded package ~A from ~A"
                          package-name
                          (namestring entrypoint))
          t)
      (error (e)
        (emit-package-warning "Failed to load package ~A from ~A: ~A"
                              package-name
                              (namestring entrypoint)
                              e)))))

(defun clawmacs-use-package (&key (src-type :git) repo &allow-other-keys)
  "Install and load a Clawmacs package from a git repository."
  (let ((normalized-source-type (normalize-package-source-type src-type))
        (normalized-repo (normalize-package-repo repo)))
    (cond
      ((null normalized-repo)
       (emit-package-warning "clawmacs-use-package requires :repo to be a string or pathname"))
      ((null normalized-source-type)
       (emit-package-warning "Unsupported package source type ~S" src-type))
      ((not (eq normalized-source-type :git))
       (emit-package-warning "Unsupported package source type ~S. Only :git is supported in v1."
                             src-type))
      (t
       (let* ((install-dir (package-install-directory normalized-source-type normalized-repo))
              (package-root (or (probe-file install-dir) install-dir))
              (install-key (package-install-key install-dir)))
         (or (gethash install-key *loaded-packages*)
             (when (ensure-package-installed normalized-repo install-dir)
               (let ((manifest (read-package-manifest package-root)))
                 (and manifest
                      (load-package-entrypoint manifest package-root install-dir))))))))))
