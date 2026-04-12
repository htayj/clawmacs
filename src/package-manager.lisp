(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Package Loader
;;; --------------------------------------------------------------------------

(defstruct package-channel
  "A local channel root that advertises Clawmacs packages."
  name
  root
  description
  source)

(defstruct package-definition
  "A package advertised by a channel or loaded directly."
  name
  description
  root
  entrypoint
  channel
  source-tier
  autoload
  dependencies
  system-prompt-section)

(defstruct package-prompt-section
  "A prompt section contributed by a loaded Clawmacs package."
  name
  title
  package
  body)

(defun clawmacs-system-source-directory ()
  "Return the source directory for the clawmacs ASDF system."
  (or (ignore-errors (asdf:system-source-directory :clawmacs))
      (truename ".")))

(defvar *default-package-channel-directory*
  (merge-pathnames #P"packages/channels/default/"
                   (clawmacs-system-source-directory))
  "Directory containing the bundled default package channel.")

(defvar *package-channels*
  (list (make-package-channel
         :name "default"
         :root *default-package-channel-directory*
         :description "Bundled Clawmacs packages"
         :source :builtin))
  "Registered package channels. init.lisp may set or extend this list.")

(defvar *available-packages* nil
  "Cached package definitions discovered from *PACKAGE-CHANNELS*.")

(defvar *package-registry-loaded-p* nil
  "Non-nil when *AVAILABLE-PACKAGES* reflects *PACKAGE-CHANNELS*.")

(defvar *enabled-builtin-packages* nil
  "Builtin package names that autoload from the default channel.
Set to T to autoload every builtin package, or NIL to autoload none.")

(defvar *packages-directory*
  (merge-pathnames #P".clawmacs.d/packages/" (user-homedir-pathname))
  "Directory where user-installed Clawmacs packages are cloned.")

(defvar *loaded-packages* (make-hash-table :test #'equal)
  "Session-local registry of package install directories already loaded.")

(defvar *package-prompt-sections* nil
  "Prompt sections registered by loaded packages.")

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

(defun count-occurrences (needle haystack &key (start 0) end (test #'char=))
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (let* ((needle (string needle))
         (haystack (string haystack))
         (needle-length (length needle))
         (limit (or end (length haystack))))
    (unless (plusp needle-length)
      (error "NEEDLE must not be empty."))
    (loop :with position := start
          :for match := (search needle haystack
                                :start2 position
                                :end2 limit
                                :test test)
          :while match
          :count t
          :do (setf position (+ match needle-length)))))

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

(defun manifest-string (value)
  "Normalize optional manifest string VALUE."
  (cond
    ((null value) nil)
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (and (plusp (length trimmed)) trimmed)))
    ((symbolp value)
     (string-downcase (symbol-name value)))
    (t nil)))

(defun manifest-package-name-list (value manifest-path field-name)
  "Normalize VALUE as a list of package names."
  (cond
    ((null value) nil)
    ((listp value)
     (loop :for item :in value
           :for name := (manifest-package-name item)
           :if name
             :collect name
           :else
             :do (emit-package-warning
                  "Ignoring invalid ~A entry ~S in ~A"
                  field-name item (namestring manifest-path))))
    (t
     (emit-package-warning "Package manifest ~A field ~A must be a list"
                           (namestring manifest-path)
                           field-name)
     nil)))

(defun package-directory-pathname (path)
  "Return PATH as a directory pathname."
  (uiop:ensure-directory-pathname (pathname path)))

(defun clear-package-registry ()
  "Clear cached channel package discovery results."
  (setf *available-packages* nil
        *package-registry-loaded-p* nil))

(defun register-package-channel (name root &key description (source :user))
  "Register ROOT as a package channel named NAME."
  (let ((normalized-name (manifest-package-name name)))
    (unless normalized-name
      (error "Package channel name must be a non-empty string or symbol."))
    (let ((channel (make-package-channel
                    :name normalized-name
                    :root (package-directory-pathname root)
                    :description (or (manifest-string description) "")
                    :source source)))
      (setf *package-channels*
            (cons channel
                  (remove normalized-name *package-channels*
                          :key #'package-channel-name
                          :test #'string=)))
      (clear-package-registry)
      channel)))

(defun list-package-channels ()
  "Return registered package channels."
  (copy-list *package-channels*))

(defun register-package-prompt-section (name body &key title package)
  "Register BODY as a system-prompt section contributed by a package."
  (let ((normalized-name (manifest-package-name name))
        (normalized-package (and package (manifest-package-name package)))
        (text (manifest-string body)))
    (unless normalized-name
      (error "Package prompt section name must be a non-empty string or symbol."))
    (unless text
      (error "Package prompt section body must be a non-empty string."))
    (let ((section (make-package-prompt-section
                    :name normalized-name
                    :title (manifest-string title)
                    :package normalized-package
                    :body text)))
      (setf *package-prompt-sections*
            (cons section
                  (remove normalized-name *package-prompt-sections*
                          :key #'package-prompt-section-name
                          :test #'string=)))
      section)))

(defun list-package-prompt-sections ()
  "Return package prompt sections in registration order."
  (reverse *package-prompt-sections*))

(defun render-package-prompt-sections
    (&optional (sections (list-package-prompt-sections)))
  "Render loaded package prompt sections for the system prompt."
  (when sections
    (with-output-to-string (stream)
      (format stream "<package_instructions>~%")
      (dolist (section sections)
        (when (package-prompt-section-title section)
          (format stream "<!-- ~A -->~%" (package-prompt-section-title section)))
        (format stream "~A~%~%" (package-prompt-section-body section)))
      (format stream "</package_instructions>"))))

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

(defun manifest-directory-pathname (value)
  "Normalize a relative manifest directory pathname."
  (let ((pathname (manifest-entrypoint-pathname value)))
    (and pathname
         (uiop:ensure-directory-pathname pathname))))

(defun read-manifest-form (manifest-path context)
  "Read a manifest plist from MANIFEST-PATH, returning NIL on warning."
  (unless (probe-file manifest-path)
    (return-from read-manifest-form
      (emit-package-warning "~A manifest ~A is missing"
                            context
                            (namestring manifest-path))))
  (handler-case
      (let* ((*read-eval* nil)
             (manifest
               (with-open-file (stream manifest-path :direction :input)
                 (read stream nil :eof))))
        (cond
          ((eq manifest :eof)
           (emit-package-warning "~A manifest ~A is empty"
                                 context
                                 (namestring manifest-path)))
          ((not (package-manifest-plist-p manifest))
           (emit-package-warning "~A manifest ~A must contain a property list"
                                 context
                                 (namestring manifest-path)))
          (t manifest)))
    (error (e)
      (emit-package-warning "Failed to read ~A manifest ~A: ~A"
                            context
                            (namestring manifest-path)
                            e))))

(defun read-package-manifest (package-root &key channel (source-tier :third-party))
  "Read and validate PACKAGE-ROOT/manifest.lisp.
Returns a normalized plist or NIL on failure."
  (let* ((root (package-directory-pathname package-root))
         (manifest-path (merge-pathnames "manifest.lisp" root))
         (manifest (read-manifest-form manifest-path "Package")))
    (when manifest
      (let* ((missing (list nil))
             (raw-name (getf manifest :name missing))
             (raw-entrypoint (getf manifest :entrypoint missing))
             (name (and (not (eq raw-name missing))
                        (manifest-package-name raw-name)))
             (entrypoint (and (not (eq raw-entrypoint missing))
                              (manifest-entrypoint-pathname raw-entrypoint)))
             (description (manifest-string (getf manifest :description)))
             (dependencies (manifest-package-name-list
                            (getf manifest :dependencies)
                            manifest-path
                            :dependencies))
             (system-prompt-section
               (manifest-string (getf manifest :system-prompt-section))))
        (unless name
          (return-from read-package-manifest
            (emit-package-warning
             "Package manifest ~A must define :name as a symbol or string"
             (namestring manifest-path))))
        (unless entrypoint
          (return-from read-package-manifest
            (emit-package-warning
             "Package manifest ~A must define :entrypoint as a relative pathname"
             (namestring manifest-path))))
        (let ((resolved-entrypoint (merge-pathnames entrypoint root)))
          (unless (probe-file resolved-entrypoint)
            (return-from read-package-manifest
              (emit-package-warning "Package ~A entrypoint ~A does not exist"
                                    name
                                    (namestring resolved-entrypoint))))
          (list :name name
                :description (or description "")
                :root root
                :entrypoint resolved-entrypoint
                :channel channel
                :source-tier source-tier
                :autoload (not (null (getf manifest :autoload)))
                :dependencies dependencies
                :system-prompt-section system-prompt-section))))))

(defun package-definition-from-manifest (manifest)
  "Build a PACKAGE-DEFINITION from a normalized manifest plist."
  (make-package-definition
   :name (getf manifest :name)
   :description (getf manifest :description)
   :root (getf manifest :root)
   :entrypoint (getf manifest :entrypoint)
   :channel (getf manifest :channel)
   :source-tier (getf manifest :source-tier)
   :autoload (getf manifest :autoload)
   :dependencies (copy-list (getf manifest :dependencies))
   :system-prompt-section (getf manifest :system-prompt-section)))

(defun register-package-manifest-prompt-section (definition)
  "Register DEFINITION's manifest-level prompt section when present."
  (when (package-definition-system-prompt-section definition)
    (register-package-prompt-section
     (package-definition-name definition)
     (package-definition-system-prompt-section definition)
     :package (package-definition-name definition)
     :title (format nil "Package ~A" (package-definition-name definition)))))

(defun load-package-definition-entrypoint (definition)
  "Load DEFINITION's entrypoint unless its root is already loaded."
  (let* ((install-key (package-install-key (package-definition-root definition)))
         (package-name (package-definition-name definition))
         (entrypoint (package-definition-entrypoint definition)))
    (when (gethash install-key *loaded-packages*)
      (return-from load-package-definition-entrypoint definition))
    (handler-case
        (let ((*default-pathname-defaults* (package-definition-root definition))
              (*package* (find-package :clawmacs)))
          (load entrypoint :verbose nil :print nil)
          (register-package-manifest-prompt-section definition)
          (setf (gethash install-key *loaded-packages*) package-name)
          (file-debug-log "package"
                          "loaded package ~A from ~A"
                          package-name
                          (namestring entrypoint))
          definition)
      (error (e)
        (emit-package-warning "Failed to load package ~A from ~A: ~A"
                              package-name
                              (namestring entrypoint)
                              e)))))

(defun load-package-entrypoint (manifest package-root install-dir)
  "Load MANIFEST's entrypoint from PACKAGE-ROOT unless INSTALL-DIR is loaded."
  (declare (ignore package-root install-dir))
  (load-package-definition-entrypoint
   (package-definition-from-manifest manifest)))

(defun read-package-channel-manifest (channel)
  "Read CHANNEL's manifest plist, returning NIL on warning."
  (let* ((root (package-channel-root channel))
         (manifest-path (merge-pathnames "manifest.lisp" root))
         (manifest (read-manifest-form manifest-path "Package channel")))
    (when manifest
      (let* ((manifest-name (manifest-package-name (getf manifest :name)))
             (description (manifest-string (getf manifest :description)))
             (packages (getf manifest :packages)))
        (unless (listp packages)
          (return-from read-package-channel-manifest
            (emit-package-warning
             "Package channel manifest ~A must define :packages as a list"
             (namestring manifest-path))))
        (list :name (or manifest-name (package-channel-name channel))
              :description (or description (package-channel-description channel) "")
              :packages packages)))))

(defun channel-package-entry-pathname (entry channel)
  "Return ENTRY's package directory relative to CHANNEL, or NIL on warning."
  (let ((value (if (package-manifest-plist-p entry)
                   (or (getf entry :path)
                       (getf entry :name))
                   entry)))
    (cond
      ((or (stringp value) (symbolp value) (pathnamep value))
       (let ((pathname (manifest-directory-pathname value)))
         (or pathname
             (emit-package-warning
              "Ignoring invalid package channel entry ~S in channel ~A"
              entry
              (package-channel-name channel)))))
      (t
       (emit-package-warning "Ignoring invalid package channel entry ~S in channel ~A"
                             entry
                             (package-channel-name channel))))))

(defun package-channel-source-tier (channel)
  "Return the source tier assigned to packages from CHANNEL."
  (if (eq (package-channel-source channel) :builtin)
      :builtin
      :channel))

(defun discover-channel-packages (channel)
  "Return package definitions advertised by CHANNEL."
  (let ((manifest (read-package-channel-manifest channel))
        (definitions nil))
    (when manifest
      (dolist (entry (getf manifest :packages))
        (let ((relative-root (channel-package-entry-pathname entry channel)))
          (when relative-root
            (let* ((package-root (merge-pathnames relative-root
                                                  (package-channel-root channel)))
                   (package-manifest
                     (read-package-manifest
                      package-root
                      :channel channel
                      :source-tier (package-channel-source-tier channel))))
              (when package-manifest
                (push (package-definition-from-manifest package-manifest)
                      definitions)))))))
    (nreverse definitions)))

(defun reload-package-channels ()
  "Rescan registered package channels and return discovered package definitions."
  (let ((definitions nil))
    (dolist (channel *package-channels*)
      (setf definitions
            (nconc definitions (discover-channel-packages channel))))
    (setf *available-packages* definitions
          *package-registry-loaded-p* t)
    (copy-list *available-packages*)))

(defun ensure-package-registry-loaded ()
  "Return discovered package definitions, reloading channels when needed."
  (if *package-registry-loaded-p*
      *available-packages*
      (reload-package-channels)))

(defun list-available-packages ()
  "Return package definitions discovered from registered channels."
  (copy-list (ensure-package-registry-loaded)))

(defun find-available-package (name)
  "Find a package definition by NAME in registered channels."
  (let ((normalized-name (manifest-package-name name)))
    (and normalized-name
         (find normalized-name (ensure-package-registry-loaded)
               :key #'package-definition-name
               :test #'string=))))

(defun builtin-package-autoload-enabled-p (definition)
  "Return non-nil when builtin DEFINITION may autoload."
  (let ((enabled *enabled-builtin-packages*))
    (cond
      ((eq enabled t) t)
      ((null enabled) nil)
      ((listp enabled)
       (member (package-definition-name definition)
               enabled
               :test #'string=))
      (t nil))))

(defun package-autoload-enabled-p (definition)
  "Return non-nil when DEFINITION should autoload."
  (and (package-definition-autoload definition)
       (or (not (eq (package-definition-source-tier definition) :builtin))
           (builtin-package-autoload-enabled-p definition))))

(defun load-clawmacs-package (package &optional seen)
  "Load PACKAGE by name or definition, including dependencies.
Returns the loaded package definition on success, or NIL on warning/failure."
  (let* ((definition (typecase package
                       (package-definition package)
                       (t (find-available-package package))))
         (name (and definition (package-definition-name definition))))
    (unless definition
      (return-from load-clawmacs-package
        (emit-package-warning "Unknown Clawmacs package ~S" package)))
    (let ((seen-table (or seen (make-hash-table :test #'equal))))
      (when (gethash name seen-table)
        (return-from load-clawmacs-package definition))
      (setf (gethash name seen-table) t)
      (dolist (dependency (package-definition-dependencies definition))
        (unless (load-clawmacs-package dependency seen-table)
          (return-from load-clawmacs-package
            (emit-package-warning
             "Package ~A dependency ~A failed to load"
             name
             dependency))))
      (load-package-definition-entrypoint definition))))

(defun load-autoload-packages ()
  "Load all autoload-enabled packages from registered channels."
  (let ((loaded nil))
    (dolist (definition (ensure-package-registry-loaded))
      (when (package-autoload-enabled-p definition)
        (let ((result (load-clawmacs-package definition)))
          (when result
            (push result loaded)))))
    (nreverse loaded)))

(defun clawmacs-use-package (&key (src-type :git) repo &allow-other-keys)
  "Install and load a Clawmacs package from a git repository.
Returns the loaded package definition on success, or NIL on warning/failure."
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
               (let ((manifest (read-package-manifest
                                package-root
                                :source-tier :third-party)))
                 (and manifest
                      (load-package-entrypoint manifest package-root install-dir))))))))))
