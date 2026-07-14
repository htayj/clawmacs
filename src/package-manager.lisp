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
  system-prompt-section
  prompt-template-directory
  slash-commands)

(defstruct package-slash-command-spec
  "A slash command resource declared by a package manifest."
  name
  description
  argument-hint
  handler)

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
  "Legacy compatibility variable for old builtin package autoload init files.
Package enablement now lives in *PACKAGE-CONFIGURATION-PATH*.")

(defvar *package-configuration-path*
  (merge-pathnames #P".clawmacs.d/packages.json" (user-homedir-pathname))
  "Path to persisted package enablement configuration.")

(defvar *package-configuration* nil
  "Memoized package enablement configuration.")

(defvar *package-configuration-lock*
  (bt:make-lock "clawmacs package configuration")
  "Lock guarding the published package enablement configuration.")

(defvar *package-configuration-save-lock*
  (bt:make-lock "clawmacs package configuration save")
  "Lock serializing package configuration commits with their disk writes.")

(defvar *package-configuration-write-function* nil
  "Test override called with JSON and target path instead of the atomic writer.")

(defvar *packages-directory*
  (merge-pathnames #P".clawmacs.d/packages/" (user-homedir-pathname))
  "Directory where user-installed Clawmacs packages are cloned.")

(defvar *loaded-packages* (make-hash-table :test #'equal)
  "Session-local registry of package install directories already loaded.")

(defvar *package-lifecycle-lock*
  (bt:make-lock "clawmacs package lifecycle")
  "Short-held lock guarding the exact process-wide package lifecycle owner.")

(defvar *package-lifecycle-condition*
  (bt:make-condition-variable :name "clawmacs package lifecycle")
  "Condition variable used by package lifecycle contenders.")

(defvar *package-lifecycle-owner* nil
  "Exact token owning package load/reset/reload publication, or NIL.")

(defvar *package-lifecycle-operation* nil
  "Human-readable operation associated with *PACKAGE-LIFECYCLE-OWNER*.")

(defvar *package-lifecycle-token* nil
  "Dynamically bound lifecycle token used for dependency and maintenance recursion.")

(defvar *package-runtime-maintenance-admitted-p* nil
  "True only while an outer runtime maintenance owner has closed admission.")

(define-condition package-runtime-maintenance-refused (error)
  ((operation :initarg :operation
              :reader package-runtime-maintenance-refused-operation)
   (blocker :initarg :blocker
            :reader package-runtime-maintenance-refused-blocker))
  (:report
   (lambda (condition stream)
     (format stream "Cannot ~A while Clawmacs runtime activity is ~A."
             (package-runtime-maintenance-refused-operation condition)
             (package-runtime-maintenance-refused-blocker condition)))))

(defvar *package-prompt-sections* nil
  "Prompt sections registered by loaded packages.")

(defvar *current-package-resource-types* nil
  "Dynamic list of allowed resource types while loading one package entrypoint.
When NIL, all package-owned resource types are allowed.")

(defvar *package-install-record-file-name* "packrat.json"
  "Sidecar metadata file stored alongside each installed package.")

(defvar *supported-package-source-types* '(:git :path :local :npm)
  "Supported package source types for installed packages.")

(defun package-lifecycle-operation-snapshot ()
  "Return the active package lifecycle operation, or NIL.

The returned string is immutable process state.  This function is intentionally
small because safe reload calls it while holding the outer runtime-admission
lock; package lifecycle code must never acquire that outer lock while holding
*PACKAGE-LIFECYCLE-LOCK*."
  (bt:with-lock-held (*package-lifecycle-lock*)
    *package-lifecycle-operation*))

(defun package-lifecycle-operation-active-p ()
  "Return true while a package load/reset/reload owner is published."
  (not (null (package-lifecycle-operation-snapshot))))

(defun try-claim-package-lifecycle (token operation)
  "Try to publish TOKEN as the exact package lifecycle owner.
Return values CLAIMED-P and the observed competing owner token."
  (bt:with-lock-held (*package-lifecycle-lock*)
    (cond
      ((eq token *package-lifecycle-owner*)
       (values t nil))
      ((null *package-lifecycle-owner*)
       (setf *package-lifecycle-owner* token
             *package-lifecycle-operation* operation)
       (values t nil))
      (t
       (values nil *package-lifecycle-owner*)))))

(defun wait-for-package-lifecycle-owner (owner)
  "Wait outside runtime admission until observed package lifecycle OWNER leaves."
  (bt:with-lock-held (*package-lifecycle-lock*)
    (loop :while (eq owner *package-lifecycle-owner*)
          :do (bt:condition-wait *package-lifecycle-condition*
                                 *package-lifecycle-lock*
                                 ;; BT v1 has no portable broadcast.  The
                                 ;; timeout makes its single-notify fallback
                                 ;; safe for more than one contender.
                                 :timeout 0.1))))

(defun release-package-lifecycle (token)
  "Release exact package lifecycle TOKEN and wake every contender."
  (bt:with-lock-held (*package-lifecycle-lock*)
    (when (eq token *package-lifecycle-owner*)
      (setf *package-lifecycle-owner* nil
            *package-lifecycle-operation* nil)
      #+sbcl
      (sb-thread:condition-broadcast *package-lifecycle-condition*)
      #-sbcl
      (bt:condition-notify *package-lifecycle-condition*)
      t)))

(defun call-with-package-cold-load-admission (function operation)
  "Try FUNCTION under outer runtime admission, when that layer is loaded."
  (if (and (not *package-runtime-maintenance-admitted-p*)
           (fboundp 'call-with-runtime-admission))
      (funcall 'call-with-runtime-admission function :operation operation)
      (funcall function)))

(defun call-with-package-lifecycle (function &key (operation "load packages"))
  "Call FUNCTION as the exact process-wide package lifecycle owner.

The lifecycle mutex is never held while FUNCTION, LOAD, a package entrypoint,
or an extension callback runs.  Dependency recursion reuses the dynamically
bound token.  A competing thread waits only after outer runtime admission has
been released, then retries the atomic admission/claim sequence."
  (if *package-lifecycle-token*
      (funcall function)
      (let ((token (list :package-lifecycle (gensym "OWNER-"))))
        (loop
          (multiple-value-bind (claimed-p observed-owner)
              (call-with-package-cold-load-admission
               (lambda ()
                 (try-claim-package-lifecycle token operation))
               operation)
            (when claimed-p
              (return
                (let ((*package-lifecycle-token* token))
                  (unwind-protect
                       (funcall function)
                    (release-package-lifecycle token)))))
            (wait-for-package-lifecycle-owner observed-owner))))))

(defun package-maintenance-runtime-functions-available-p ()
  "Return true when safe reload's runtime maintenance primitives are loaded."
  (and (fboundp 'make-safe-reload-request)
       (fboundp 'safe-reload-try-claim-request)
       (fboundp 'finish-safe-reload-request)))

(defun claim-package-runtime-maintenance (operation buffer)
  "Close runtime admission for destructive package OPERATION or signal."
  (unless (package-maintenance-runtime-functions-available-p)
    (error 'package-runtime-maintenance-refused
           :operation operation
           :blocker :runtime-admission-unavailable))
  (let ((request
          (funcall 'make-safe-reload-request
                   :token (list :package-maintenance (gensym "OWNER-"))
                   :mode :package-maintenance
                   :buffer buffer
                   :started-at (get-internal-real-time)
                   :notify-p nil)))
    (multiple-value-bind (claimed-p blocker)
        (funcall 'safe-reload-try-claim-request request)
      (unless claimed-p
        (error 'package-runtime-maintenance-refused
               :operation operation
               :blocker blocker))
      request)))

(defun call-with-package-runtime-maintenance
    (function &key (operation "mutate package runtime") buffer)
  "Run destructive package FUNCTION only with proven process quiescence.

The existing safe-reload admission owner blocks new provider, tool, OAuth,
interop, subagent, and cold-package starts for the whole mutation.  Nested
reset/reload calls and safe reload's own registration refresh reuse the exact
outer admission rather than attempting to claim it recursively."
  (cond
    (*package-runtime-maintenance-admitted-p*
     (call-with-package-lifecycle function :operation operation))
    (t
     (let ((request (claim-package-runtime-maintenance operation buffer)))
       (unwind-protect
            (let ((*package-runtime-maintenance-admitted-p* t))
              (call-with-package-lifecycle function :operation operation))
         (funcall 'finish-safe-reload-request request))))))

(defun emit-package-warning (format-string &rest format-args)
  "Report a non-fatal package warning and return NIL."
  (let ((message (apply #'format nil format-string format-args)))
    (format *error-output* "~&;; Warning: ~A~%" message)
    (file-debug-log "package" "~A" message)
    nil))

(defun normalize-package-source-type (source-type)
  "Normalize SOURCE-TYPE to a keyword, defaulting NIL to :GIT."
  (let ((normalized
          (cond
            ((null source-type) :git)
            ((keywordp source-type) source-type)
            ((symbolp source-type)
             (intern (string-upcase (symbol-name source-type)) :keyword))
            ((stringp source-type)
             (intern (string-upcase source-type) :keyword))
            (t nil))))
    (and normalized
         (member normalized *supported-package-source-types* :test #'eq)
         normalized)))

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

(defun project-packages-directory (project-designator)
  "Return the directory used for project-local package installs."
  (let ((project (ensure-project project-designator)))
    (merge-pathnames #P".clawmacs.d/packages/"
                     (project-root project))))

(defun package-install-root (&key (scope :global) project)
  "Return the directory that should hold a package install for SCOPE."
  (ecase scope
    (:global *packages-directory*)
    (:project (or (and project (project-packages-directory project))
                  (error "Project-local package installs require :PROJECT.")))))

(defun package-install-directory (source-type repo &key (scope :global) project)
  "Return the directory pathname where SOURCE-TYPE/REPO should be installed."
  (let* ((source-label (string-downcase (symbol-name source-type)))
         (repo-label (filesystem-safe-component (repo-display-name repo)))
         (digest (string-downcase (package-source-hash repo)))
         (dir-name (format nil "~A-~A-~A/" source-label repo-label digest))
         (root (package-install-root :scope scope :project project)))
    (uiop:ensure-directory-pathname
     (merge-pathnames dir-name root))))

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

(defun package-resource-type-name (value)
  "Normalize VALUE to a lower-case package resource type keyword."
  (cond
    ((null value) nil)
    ((keywordp value) value)
    ((symbolp value)
     (intern (string-upcase (symbol-name value)) :keyword))
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (and (plusp (length trimmed))
            (intern (string-upcase (package-identifier-string trimmed))
                    :keyword))))
    (t nil)))

(defun normalize-package-resource-type-list (types)
  "Return TYPES as a list of normalized resource-type keywords."
  (remove-duplicates
   (loop :for type :in (coerce (or types #()) 'list)
         :for normalized := (package-resource-type-name type)
         :when normalized
           :collect normalized)
   :test #'eq))

(defun package-resource-type-allowed-p (type)
  "Return true when TYPE is allowed for the package currently being loaded."
  (let ((policy *current-package-resource-types*))
    (or (null policy)
        (member (package-resource-type-name type) policy :test #'eq))))

(defun package-install-metadata-path (install-dir)
  "Return the sidecar metadata path for INSTALL-DIR."
  (merge-pathnames *package-install-record-file-name*
                   (uiop:ensure-directory-pathname install-dir)))

(defun package-install-record-plist (definition &key source-type source ref
                                                scope project resource-types)
  "Return a persisted metadata plist for an installed package."
  (labels ((source-string (value)
             (cond
               ((null value) nil)
               ((stringp value) value)
               ((pathnamep value) (namestring value))
               (t (princ-to-string value)))))
    (list :name (package-definition-name definition)
          :description (package-definition-description definition)
          :source-type (package-resource-type-name source-type)
          :source (source-string source)
          :ref (manifest-string ref)
          :scope (package-resource-type-name scope)
          :project (manifest-string project)
          :resource-types
          (coerce (normalize-package-resource-type-list resource-types)
                  'vector)
          :installed-at (get-universal-time)
          :updated-at (get-universal-time))))

(defun package-install-record-source-type (record)
  "Return RECORD's normalized source type."
  (package-resource-type-name (getf record :source-type)))

(defun package-install-record-resource-types (record)
  "Return RECORD's normalized resource type allowlist."
  (normalize-package-resource-type-list (getf record :resource-types)))

(defun package-install-record-scope (record)
  "Return RECORD's normalized install scope."
  (package-resource-type-name (getf record :scope)))

(defun package-install-record-project (record)
  "Return RECORD's normalized project name."
  (manifest-string (getf record :project)))

(defun package-install-record-source (record)
  "Return RECORD's source string."
  (manifest-string (getf record :source)))

(defun package-install-record-ref (record)
  "Return RECORD's source ref string."
  (manifest-string (getf record :ref)))

(defun read-package-install-record (install-dir)
  "Read the install metadata for INSTALL-DIR, returning a plist or NIL."
  (let ((path (package-install-metadata-path install-dir)))
    (when (probe-file path)
      (handler-case
          (let ((cl-json:*json-array-type* 'vector))
            (let ((data (cl-json:decode-json-from-string
                         (uiop:read-file-string path))))
              (list :name (manifest-package-name (package-lookup-json-value data "name"))
                    :description (manifest-string
                                  (package-lookup-json-value data "description"))
                    :source-type (package-resource-type-name
                                  (package-lookup-json-value data "source_type"))
                    :source (manifest-string (package-lookup-json-value data "source"))
                    :ref (manifest-string (package-lookup-json-value data "ref"))
                    :scope (package-resource-type-name
                            (package-lookup-json-value data "scope"))
                    :project (manifest-string (package-lookup-json-value data "project"))
                    :resource-types
                    (package-install-record-resource-types
                     (list :resource-types
                           (package-lookup-json-value data "resource_types")))
                    :installed-at (package-lookup-json-value data "installed_at")
                    :updated-at (package-lookup-json-value data "updated_at"))))
        (error (e)
          (emit-package-warning "Failed to read package install record ~A: ~A"
                                (namestring path)
                                e)
          nil)))))

(defun write-package-install-record (install-dir record)
  "Persist RECORD alongside INSTALL-DIR."
  (let ((path (package-install-metadata-path install-dir)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string
       (cl-json:encode-json-to-string
        `((:name . ,(getf record :name))
          (:description . ,(getf record :description))
          (:source-type . ,(string-downcase
                            (symbol-name (or (getf record :source-type) :git))))
          (:source . ,(or (getf record :source) ""))
          (:ref . ,(or (getf record :ref) ""))
          (:scope . ,(string-downcase
                      (symbol-name (or (getf record :scope) :global))))
          (:project . ,(or (getf record :project) ""))
          (:resource-types . ,(coerce (package-install-record-resource-types record)
                                      'vector))
          (:installed-at . ,(or (getf record :installed-at)
                                (get-universal-time)))
          (:updated-at . ,(get-universal-time))))
       stream))
    path))

(defun ensure-package-installed (repo install-dir &key (src-type :git) ref)
  "Install REPO into INSTALL-DIR when missing. Returns non-nil on success."
  (let ((install-root
          (uiop:pathname-parent-directory-pathname
           (uiop:ensure-directory-pathname install-dir))))
    (ensure-directories-exist (merge-pathnames #P".keep" install-root)))
  (if (probe-file (merge-pathnames #P"manifest.lisp"
                                   (uiop:ensure-directory-pathname install-dir)))
      t
      (handler-case
          (let ((source-type (package-resource-type-name src-type)))
            (cond
              ((member source-type '(:path :local :npm) :test #'eq)
               (let* ((source-path (uiop:ensure-directory-pathname (pathname repo)))
                      (target (uiop:ensure-directory-pathname install-dir)))
                 (ensure-directories-exist target)
                 (multiple-value-bind (stdout stderr exit-code)
                     (uiop:run-program
                      (list "cp" "-R" (concatenate 'string (namestring source-path) "/.")
                            (namestring target))
                      :output :string
                      :error-output :string
                      :ignore-error-status t)
                   (declare (ignore stdout stderr))
                   (if (zerop exit-code)
                       t
                       (emit-package-warning
                        "Failed to copy package from ~A into ~A (exit ~D)"
                        source-path
                        (namestring target)
                        exit-code)))))
              (t
               (multiple-value-bind (stdout stderr exit-code)
                   (uiop:run-program (list "git" "clone" repo (namestring install-dir))
                                     :output :string
                                     :error-output :string
                                     :ignore-error-status t)
                 (when (and (zerop exit-code) ref)
                   (multiple-value-bind (checkout-stdout checkout-stderr checkout-exit-code)
                       (uiop:run-program (list "git" "-C" (namestring install-dir)
                                               "checkout" ref)
                                         :output :string
                                         :error-output :string
                                         :ignore-error-status t)
                     (setf stdout (concatenate 'string stdout checkout-stdout)
                           stderr (concatenate 'string stderr checkout-stderr)
                           exit-code (max exit-code checkout-exit-code))))
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
                                           (package-output-summary stdout stderr exit-code)))))))
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

(defun manifest-function-name (value)
  "Normalize a manifest function reference to a lowercase string."
  (cond
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (and (plusp (length trimmed))
            (string-downcase trimmed))))
    ((symbolp value)
     (string-downcase (symbol-name value)))
    (t nil)))

(defun manifest-package-slash-command-spec (value manifest-path)
  "Normalize one slash-command VALUE from MANIFEST-PATH."
  (unless (package-manifest-plist-p value)
    (emit-package-warning
     "Ignoring invalid :slash-command entry ~S in ~A"
     value
     (namestring manifest-path))
    (return-from manifest-package-slash-command-spec nil))
  (let ((name (manifest-package-name (getf value :name)))
        (description (or (manifest-string (getf value :description)) ""))
        (argument-hint (manifest-string (getf value :argument-hint)))
        (handler (manifest-function-name (getf value :handler))))
    (unless name
      (emit-package-warning
       "Ignoring :slash-command entry without a valid :name in ~A"
       (namestring manifest-path))
      (return-from manifest-package-slash-command-spec nil))
    (unless handler
      (emit-package-warning
       "Ignoring :slash-command ~A without a valid :handler in ~A"
       name
       (namestring manifest-path))
      (return-from manifest-package-slash-command-spec nil))
    (make-package-slash-command-spec
     :name name
     :description description
     :argument-hint argument-hint
     :handler handler)))

(defun manifest-package-slash-command-list (value manifest-path)
  "Normalize VALUE as a list of package slash-command specs."
  (cond
    ((null value) nil)
    ((listp value)
     (loop :for item :in value
           :for spec := (manifest-package-slash-command-spec item manifest-path)
           :when spec
             :collect spec))
    (t
     (emit-package-warning "Package manifest ~A field :slash-commands must be a list"
                           (namestring manifest-path))
     nil)))

(defun normalize-package-name-list (value)
  "Normalize VALUE as a list/vector of package names, dropping invalid entries."
  (loop :for item :in (coerce (or value #()) 'list)
        :for name := (manifest-package-name item)
        :when name
          :collect name))

(defun normalize-package-agent-name (agent-name)
  "Normalize AGENT-NAME for package configuration keys."
  (let ((trimmed (manifest-string agent-name)))
    (and trimmed (string-downcase trimmed))))

(defun make-package-enable-table (&optional names)
  "Return an equal-test hash table containing normalized package NAMES."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (name (normalize-package-name-list names))
      (setf (gethash name table) t))
    table))

(defun make-package-configuration ()
  "Create an empty package enablement configuration."
  (list :global (make-package-enable-table)
        :agents (make-hash-table :test #'equal)))

(defun package-identifier-string (value)
  "Return VALUE as a lower-case hyphenated identifier string."
  (let ((raw
          (cond
            ((keywordp value) (symbol-name value))
            ((symbolp value) (symbol-name value))
            ((stringp value) value)
            ((pathnamep value) (namestring value))
            (t (princ-to-string value)))))
    (with-output-to-string (out)
      (loop :with last-emitted-separator-p := t
            :for index :from 0 :below (length raw)
            :for char := (char raw index)
            :for previous := (and (> index 0) (char raw (1- index)))
            :do (cond
                  ((find char " _-" :test #'char=)
                   (setf last-emitted-separator-p t))
                  ((and (upper-case-p char)
                        previous
                        (or (lower-case-p previous)
                            (digit-char-p previous)))
                   (unless last-emitted-separator-p
                     (write-char #\- out))
                   (write-char (char-downcase char) out)
                   (setf last-emitted-separator-p nil))
                  (t
                   (write-char (char-downcase char) out)
                   (setf last-emitted-separator-p nil)))))))

(defun package-json-normalize-key-string (key)
  "Return KEY normalized for package JSON lookup."
  (package-identifier-string key))

(defun package-lookup-json-value (alist key)
  "Look up KEY in decoded JSON ALIST using string-insensitive key matching."
  (let ((target (package-json-normalize-key-string key)))
    (cdr (find target alist
               :key (lambda (entry)
                      (package-json-normalize-key-string (car entry)))
               :test #'string=))))

(defun package-configuration-global-table (configuration)
  "Return CONFIGURATION's global package enablement table."
  (getf configuration :global))

(defun package-configuration-agents-table (configuration)
  "Return CONFIGURATION's agent package enablement table."
  (getf configuration :agents))

(defun package-table-names (table)
  "Return sorted enabled package names from TABLE."
  (let ((names nil))
    (when table
      (maphash (lambda (name enabled-p)
                 (when enabled-p
                   (push name names)))
               table))
    (sort names #'string<)))

(defun copy-package-enable-table (table)
  "Return an independent copy of package enablement TABLE."
  (let ((copy (make-hash-table :test #'equal)))
    (when table
      (maphash (lambda (name enabled-p)
                 (setf (gethash name copy) enabled-p))
               table))
    copy))

(defun copy-package-configuration (configuration)
  "Return a deep copy of package CONFIGURATION's nested hash tables."
  (let ((copy (make-package-configuration)))
    (setf (getf copy :global)
          (copy-package-enable-table
           (package-configuration-global-table configuration)))
    (maphash
     (lambda (agent-name table)
       (setf (gethash agent-name
                      (package-configuration-agents-table copy))
             (copy-package-enable-table table)))
     (package-configuration-agents-table configuration))
    copy))

(defun load-package-configuration ()
  "Load and memoize persisted package enablement configuration."
  (let ((configuration (make-package-configuration)))
    (when (probe-file *package-configuration-path*)
      (handler-case
          (let* ((json (uiop:read-file-string *package-configuration-path*))
                 (data (cl-json:decode-json-from-string json))
                 (global (package-lookup-json-value data "global"))
                 (agents (package-lookup-json-value data "agents")))
            (setf (getf configuration :global)
                  (make-package-enable-table global))
            (when (listp agents)
              (dolist (entry agents)
                (let ((agent-name (normalize-package-agent-name (car entry))))
                  (when agent-name
                    (setf (gethash agent-name
                                   (package-configuration-agents-table
                                    configuration))
                          (make-package-enable-table (cdr entry))))))))
        (error (e)
          (emit-package-warning "Failed to load package configuration ~A: ~A"
                                (namestring *package-configuration-path*)
                                e))))
    ;; File I/O and JSON decoding occur before the short publication lock.
    ;; If another loader or writer won meanwhile, preserve its newer object.
    (bt:with-lock-held (*package-configuration-lock*)
      (or *package-configuration*
          (setf *package-configuration* configuration)))))

(defun ensure-package-configuration-loaded ()
  "Return the package enablement configuration, loading it if needed."
  (or (bt:with-lock-held (*package-configuration-lock*)
        *package-configuration*)
      (load-package-configuration)))

(defun package-configuration-snapshot ()
  "Return an immutable-for-the-caller package configuration snapshot."
  (ensure-package-configuration-loaded)
  (bt:with-lock-held (*package-configuration-lock*)
    (copy-package-configuration *package-configuration*)))

(defun package-configuration-json (configuration)
  "Encode private CONFIGURATION as persisted package JSON."
  (let ((global (package-table-names
                 (package-configuration-global-table configuration)))
        (agents nil))
    (maphash (lambda (agent-name table)
               (let ((names (package-table-names table)))
                 (when names
                   (push `(,agent-name . ,(coerce names 'vector))
                         agents))))
             (package-configuration-agents-table configuration))
    (cl-json:encode-json-to-string
     `((:global . ,(coerce global 'vector))
       (:agents . ,(sort agents #'string< :key #'car))))))

(defun write-package-configuration-json (json)
  "Atomically replace the package configuration file with JSON."
  (when *package-configuration-write-function*
    (return-from write-package-configuration-json
      (funcall *package-configuration-write-function*
               json *package-configuration-path*)))
  (ensure-directories-exist *package-configuration-path*)
  (let* ((target *package-configuration-path*)
         (temporary
           (make-pathname
            :name (format nil ".~A-~D-~D"
                          (or (pathname-name target) "packages")
                          (get-universal-time)
                          (get-internal-real-time))
            :type "tmp"
            :defaults target)))
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string json stream))
           (uiop:rename-file-overwriting-target temporary target))
      (when (probe-file temporary)
        (ignore-errors (delete-file temporary)))))
  *package-configuration-path*)

(defun save-package-configuration ()
  "Persist package enablement configuration to disk."
  (ensure-package-configuration-loaded)
  ;; SAVE-LOCK is outermost for writers.  A scope commit and its rename cannot
  ;; be overtaken by an older snapshot writing after a newer one.
  (bt:with-lock-held (*package-configuration-save-lock*)
    (write-package-configuration-json
     (package-configuration-json (package-configuration-snapshot)))))

(defun package-agent-enable-table (agent-name &key create)
  "Return a private snapshot of AGENT-NAME's package enablement table.
CREATE is retained for compatibility and returns an empty private table when
the agent has no published table; callers must use the COW update helpers to
publish changes."
  (let ((agent-key (normalize-package-agent-name agent-name)))
    (when agent-key
      (let* ((configuration (package-configuration-snapshot))
             (agents (package-configuration-agents-table configuration))
             (table (gethash agent-key agents)))
        (or table
            (when create
              (make-package-enable-table)))))))

(defun package-enablement-names-snapshot (agent-name)
  "Return values global and AGENT-NAME enabled package-name lists."
  (let* ((configuration (package-configuration-snapshot))
         (agent-key (normalize-package-agent-name agent-name))
         (agent-table (and agent-key
                           (gethash agent-key
                                    (package-configuration-agents-table
                                     configuration)))))
    (values
     (package-table-names
      (package-configuration-global-table configuration))
     (package-table-names agent-table))))

(defun package-enablement-flags-snapshot (package-name agent-name)
  "Return values global-enabled-p and agent-enabled-p from one snapshot."
  (let* ((configuration (package-configuration-snapshot))
         (agent-key (normalize-package-agent-name agent-name))
         (agent-table (and agent-key
                           (gethash agent-key
                                    (package-configuration-agents-table
                                     configuration)))))
    (values
     (package-name-enabled-in-table-p
      package-name (package-configuration-global-table configuration))
     (package-name-enabled-in-table-p package-name agent-table))))

(defun package-name-enabled-in-table-p (name table)
  "Return true when normalized package NAME is enabled in TABLE."
  (and table (gethash name table)))

(defun set-package-name-enabled-in-table (name table enabled-p)
  "Set normalized package NAME enablement in TABLE."
  (if enabled-p
      (setf (gethash name table) t)
      (remhash name table))
  enabled-p)

(defun buffer-package-name-enabled-p (buffer package-name)
  "Return true when BUFFER explicitly enables PACKAGE-NAME."
  (and buffer
       (member package-name (buffer-enabled-packages buffer) :test #'string=)))

(defun set-buffer-package-name-enabled (buffer package-name enabled-p)
  "Set BUFFER's explicit PACKAGE-NAME enablement."
  (unless buffer
    (error "Buffer package enablement requires a buffer."))
  (setf (buffer-enabled-packages buffer)
        (remove package-name (buffer-enabled-packages buffer) :test #'string=))
  (when enabled-p
    (push package-name (buffer-enabled-packages buffer)))
  enabled-p)

(defun package-enabled-globally-p (package-name)
  "Return true when PACKAGE-NAME is globally enabled."
  (multiple-value-bind (global-enabled-p _agent-enabled-p)
      (package-enablement-flags-snapshot package-name nil)
    (declare (ignore _agent-enabled-p))
    global-enabled-p))

(defun package-enabled-for-agent-p (package-name agent-name)
  "Return true when PACKAGE-NAME is enabled for AGENT-NAME."
  (multiple-value-bind (_global-enabled-p agent-enabled-p)
      (package-enablement-flags-snapshot package-name agent-name)
    (declare (ignore _global-enabled-p))
    agent-enabled-p))

(defun package-enablement-scope (package &key buffer agent-name project)
  "Return PACKAGE's effective enablement scope for BUFFER/AGENT-NAME/PROJECT."
  (let* ((name (manifest-package-name package))
         (agent (or agent-name
                    (and buffer (buffer-agent-name buffer))))
         (project-name (or project
                           (and buffer (buffer-project-name buffer))))
         (project-definition (and project-name
                                  (find-installed-package name
                                                          :project project-name))))
    (multiple-value-bind (global-enabled-p agent-enabled-p)
        (package-enablement-flags-snapshot name agent)
      (cond
        ((null name) :default)
        ((and project-definition
              (eq :project (package-definition-source-tier project-definition)))
         :project)
        ((buffer-package-name-enabled-p buffer name) :buffer)
        (agent-enabled-p :agent)
        (global-enabled-p :global)
        (t :default)))))

(defun active-package-names (&key buffer agent-name)
  "Return package names active for BUFFER/AGENT-NAME."
  (let ((table (make-hash-table :test #'equal))
        (agent (or agent-name
                   (and buffer (buffer-agent-name buffer)))))
    (multiple-value-bind (global-names agent-names)
        (package-enablement-names-snapshot agent)
      (dolist (name global-names)
        (setf (gethash name table) t))
      (dolist (name agent-names)
        (setf (gethash name table) t)))
    (when buffer
      (dolist (name (buffer-enabled-packages buffer))
        (let ((normalized (manifest-package-name name)))
          (when normalized
            (setf (gethash normalized table) t)))))
    (let ((project (and buffer (buffer-project-name buffer))))
      (when project
        (dolist (definition (list-installed-packages :buffer buffer))
          (when (eq :project (package-definition-source-tier definition))
            (setf (gethash (package-definition-name definition) table) t)))))
    (package-table-names table)))

(defun update-package-configuration-scope (name agent scope)
  "Publish and persist one serialized COW package enablement update."
  (ensure-package-configuration-loaded)
  (bt:with-lock-held (*package-configuration-save-lock*)
    (let ((configuration nil))
      (bt:with-lock-held (*package-configuration-lock*)
        (setf configuration
              (copy-package-configuration *package-configuration*))
        (let* ((agents (package-configuration-agents-table configuration))
               (agent-key (normalize-package-agent-name agent))
               (agent-table
                 (and agent-key
                      (or (gethash agent-key agents)
                          (setf (gethash agent-key agents)
                                (make-package-enable-table)))))
               (global-table
                 (package-configuration-global-table configuration)))
          (when agent-table
            (set-package-name-enabled-in-table name agent-table nil))
          (set-package-name-enabled-in-table name global-table nil)
          (ecase scope
            ((:default :buffer) nil)
            (:agent
             (set-package-name-enabled-in-table name agent-table t))
            (:global
             (set-package-name-enabled-in-table name global-table t))))
        nil)
      ;; Persist the private copy before publishing it.  A failed write leaves
      ;; both the previous in-memory generation and the prior file intact.
      (write-package-configuration-json
       (package-configuration-json configuration))
      (bt:with-lock-held (*package-configuration-lock*)
        (setf *package-configuration* configuration)))))

(defun package-active-p (package &key buffer agent-name)
  "Return true when PACKAGE is active for BUFFER/AGENT-NAME."
  (let ((name (manifest-package-name package)))
    (and name
         (member name (active-package-names :buffer buffer
                                            :agent-name agent-name)
                 :test #'string=))))

(defun set-package-enablement-scope (package scope &key buffer agent-name)
  "Set PACKAGE to SCOPE for BUFFER/AGENT-NAME.
SCOPE is one of :DEFAULT, :BUFFER, :AGENT, or :GLOBAL. Setting one scope
removes the package from the other scopes in the same context."
  (let* ((name (or (manifest-package-name package)
                   (error "Package name must be a non-empty string or symbol.")))
         (agent (or agent-name
                    (and buffer (buffer-agent-name buffer))
                    *default-agent-name*))
         (previous-scope (and buffer
                              (package-enablement-scope
                               name
                               :buffer buffer
                               :agent-name agent)))
         (definition (and buffer (find-installed-package name)))
         (had-context-p (and buffer
                             (buffer-has-conversation-context-p buffer))))
    (unless (member scope '(:default :buffer :agent :global) :test #'eq)
      (error "Unsupported package enablement scope: ~S" scope))
    (when buffer
      (set-buffer-package-name-enabled buffer name nil))
    (ecase scope
      (:default nil)
      (:buffer
       (set-buffer-package-name-enabled buffer name t))
      ((:agent :global) nil))
    (update-package-configuration-scope name agent scope)
    (when buffer
      (cond
        ((eq scope :default)
         (remove-package-context-messages buffer name))
        ((eq previous-scope :default)
         (maybe-insert-enabled-package-context buffer definition previous-scope scope
                                               had-context-p))))
    (when buffer
      (sync-buffer-system-prompt-display buffer))
    (maybe-run-hook-with-args
     '*package-enablement-changed-hook*
     name
     scope
     buffer
     agent)
    scope))

(defun cycle-package-enablement-scope (package &key buffer agent-name)
  "Cycle PACKAGE through default, buffer, agent, global, and back."
  (let* ((current (package-enablement-scope package
                                           :buffer buffer
                                           :agent-name agent-name))
         (next (ecase current
                 (:default (if buffer :buffer :agent))
                 (:buffer :agent)
                 (:agent :global)
                 (:global :default))))
    (set-package-enablement-scope package next
                                  :buffer buffer
                                  :agent-name agent-name)))

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
  (when (and *current-clawmacs-package*
             (not (package-resource-type-allowed-p :prompt-section)))
    (return-from register-package-prompt-section nil))
  (let ((normalized-name (manifest-package-name name))
        (normalized-package (or (and package (manifest-package-name package))
                                *current-clawmacs-package*))
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

(defun package-prompt-section-active-p (section &key buffer agent-name)
  "Return true when SECTION should render for BUFFER/AGENT-NAME."
  (let ((package (package-prompt-section-package section)))
    (and package
         (package-active-p package :buffer buffer :agent-name agent-name))))

(defun render-package-prompt-sections
    (&optional (sections (list-package-prompt-sections))
     &key buffer agent-name)
  "Render active package prompt sections for the system prompt."
  (let ((active-sections
          (remove-if-not (lambda (section)
                           (package-prompt-section-active-p
                            section
                            :buffer buffer
                            :agent-name agent-name))
                         sections)))
    (when active-sections
      (with-output-to-string (stream)
        (format stream "<package_instructions>~%")
        (dolist (section active-sections)
          (when (package-prompt-section-title section)
            (format stream "<!-- ~A -->~%" (package-prompt-section-title section)))
          (format stream "~A~%~%" (package-prompt-section-body section)))
        (format stream "</package_instructions>")))))

(defun package-display-description (definition)
  "Return DEFINITION's single-line selector/help description."
  (let ((description (package-definition-description definition)))
    (if (and description (plusp (length description)))
        description
        "No description.")))

(defun package-scope-label (scope)
  "Return the selector label for package enablement SCOPE."
  (ecase scope
    (:default "default")
    (:buffer "buffer")
    (:agent "agent")
    (:global "global")))

(defun package-scope-message (scope)
  "Return a short user-facing description for package SCOPE."
  (ecase scope
    (:default "default")
    (:buffer "enabled for this buffer")
    (:agent "enabled for this agent")
    (:global "enabled globally")))

(defun package-owned-command-metadata (package-name)
  "Return command metadata registered by PACKAGE-NAME."
  (sort
   (loop :for (_name . metadata) :in (command-registry-snapshot)
         :when (string= package-name
                        (or (command-metadata-package metadata) ""))
           :collect metadata)
   #'string<
   :key (lambda (metadata)
          (symbol-name (command-metadata-name metadata)))))

(defun package-owned-tool-metadata (package-name)
  "Return tool metadata registered by PACKAGE-NAME."
  (remove-if-not (lambda (metadata)
                   (string= package-name
                            (or (agent-tool-metadata-package metadata) "")))
                 (list-agent-tool-metadata)))

(defun package-owned-slash-commands (package-name)
  "Return slash commands registered by PACKAGE-NAME."
  (sort
   (loop :for (_name . command) :in (slash-command-registry-snapshot)
         :when (string= package-name
                        (or (slash-command-package command) ""))
           :collect command)
   #'string< :key #'slash-command-name))

(defun package-owned-prompt-templates (package &key buffer project)
  "Return prompt templates contributed by PACKAGE's prompt directory."
  (let* ((definition (typecase package
                       (package-definition package)
                       (t (find-installed-package package
                                                 :buffer buffer
                                                 :project project))))
         (record (and definition
                      (package-install-record-for-definition definition))))
    (when (and definition
               (or (null record)
                   (member :prompt-template
                           (package-install-record-resource-types record)
                           :test #'eq)))
      (let ((directory (package-prompt-template-directory definition)))
        (when (and directory (probe-file directory))
          (discover-prompt-templates-in-directory
           directory
           :scope :package
           :package (package-definition-name definition)))))))

(defun package-owned-prompt-sections (package-name)
  "Return prompt sections registered by PACKAGE-NAME."
  (remove-if-not (lambda (section)
                   (string= package-name
                            (or (package-prompt-section-package section) "")))
                 (list-package-prompt-sections)))

(defun package-owned-buffer-types (package-name)
  "Return buffer types registered by PACKAGE-NAME."
  (let ((name (normalize-buffer-type-package-name package-name)))
    (and name
         (remove-if-not (lambda (type)
                          (string= name
                                   (or (buffer-type-package type) "")))
                        (list-buffer-types)))))

(defun package-context-message-marker (package-name)
  "Return the stable marker used for PACKAGE-NAME context messages."
  (format nil "<package_context package=~S>" package-name))

(defun package-context-message-p (msg package-name)
  "Return true when MSG is the injected context message for PACKAGE-NAME."
  (and (eq (message-sender msg) :context)
       (let* ((metadata (message-metadata msg))
              (metadata-package-name
                (and metadata (message-metadata-value metadata :package-name)))
              (marker (package-context-message-marker package-name)))
         (or (and metadata-package-name
                  (string= package-name metadata-package-name))
             (search marker (message-text msg))))))

(defun buffer-has-conversation-context-p (buffer)
  "Return true when BUFFER already has provider-visible context."
  (loop :for msg := (buffer-first-message buffer) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buffer))))
        :thereis (not (eq (message-sender msg) :system))))

(defun buffer-package-context-message-present-p (buffer package-name)
  "Return true when BUFFER already contains PACKAGE-NAME's context marker."
  (loop :for msg := (buffer-first-message buffer) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buffer))))
        :thereis (package-context-message-p msg package-name)))

(defun remove-package-context-messages (buffer package-name)
  "Remove injected context messages for PACKAGE-NAME from BUFFER."
  (when buffer
    (buffer-remove-messages-if
     buffer
     (lambda (msg)
       (package-context-message-p msg package-name))))
  buffer)

(defun package-owned-tool-definitions-for-context (package-name)
  "Return provider-style tool definitions owned by PACKAGE-NAME."
  (mapcar (lambda (metadata)
            `((:name . ,(agent-tool-metadata-name metadata))
              (:description . ,(agent-tool-metadata-description metadata))
              (:input--schema . ,(agent-tool-metadata-input-schema metadata))))
          (package-owned-tool-metadata package-name)))

(defun package-system-prompt-context-text (definition buffer)
  "Return package prompt content that should be appended to existing context."
  (when (load-clawmacs-package definition)
    (let* ((name (package-definition-name definition))
           (prompt-section
             (render-package-prompt-sections
              (package-owned-prompt-sections name)
              :buffer buffer))
           (tools-section
             (render-agent-tools-section
              (package-owned-tool-definitions-for-context name)))
           (parts (remove nil (list prompt-section tools-section))))
      (when parts
        (with-output-to-string (s)
          (format s "~A~%" (package-context-message-marker name))
          (format s "The ~A package was enabled after earlier conversation context. Apply this package context to subsequent work.~%~%"
                  name)
          (format s "~{~A~^~%~%~}" parts)
          (format s "~%</package_context>"))))))

(defun maybe-insert-enabled-package-context
    (buffer definition previous-scope new-scope had-context-p)
  "Append newly enabled package prompt content to BUFFER context when needed."
  (let ((name (and definition (package-definition-name definition))))
    (when (and name
               had-context-p
               (eq previous-scope :default)
               (not (eq new-scope :default))
               (not (buffer-package-context-message-present-p buffer name)))
      (let ((text (package-system-prompt-context-text definition buffer)))
        (when text
          (buffer-insert-context-message
           buffer text
           :metadata `((:package-context . t)
                       (:package-name . ,name))))))))

(defun package-owned-extended-docs (package-name)
  "Return extended documentation entries registered by PACKAGE-NAME."
  (sort
   (remove-if-not
    (lambda (entry)
      (string= package-name (or (getf (cdr entry) :package) "")))
    (extended-doc-registry-snapshot))
   #'string< :key (lambda (entry)
                    (symbol-name (car entry)))))

(defun describe-installed-package-to-string (definition buffer)
  "Return the help text for installed package DEFINITION."
  (load-clawmacs-package definition)
  (let* ((name (package-definition-name definition))
         (scope (package-enablement-scope name :buffer buffer)))
    (with-output-to-string (s)
      (format s "Package: ~A~%" name)
      (format s "Enabled: [~A]~%" (package-scope-label scope))
      (format s "Source: ~(~A~)~%" (package-definition-source-tier definition))
      (format s "Root: ~A~%~%" (namestring (package-definition-root definition)))
      (format s "~A~%~%" (package-display-description definition))
      (let ((sections (package-owned-prompt-sections name)))
        (when sections
          (format s "Prompt Sections:~%")
          (dolist (section sections)
            (format s "  - ~A~%"
                    (or (package-prompt-section-title section)
                        (package-prompt-section-name section))))
          (format s "~%")))
      (let ((tools (package-owned-tool-metadata name)))
        (when tools
          (format s "Tools:~%")
          (dolist (tool tools)
            (format s "  - ~A: ~A~%"
                    (agent-tool-metadata-name tool)
                    (agent-tool-metadata-description tool)))
          (format s "~%")))
      (let ((commands (package-owned-command-metadata name)))
        (when commands
          (format s "Commands:~%")
          (dolist (command commands)
            (format s "  - ~(~A~): ~A~%"
                    (command-metadata-name command)
                    (command-metadata-docstring command)))
          (format s "~%")))
      (let ((slash-commands (package-owned-slash-commands name)))
        (when slash-commands
          (format s "Slash Commands:~%")
          (dolist (command slash-commands)
            (format s "  - /~A~@[ ~A~]: ~A~%"
                    (slash-command-name command)
                    (and (slash-command-argument-hint command)
                         (plusp (length (slash-command-argument-hint command)))
                         (slash-command-argument-hint command))
                    (or (slash-command-description command) "")))
          (format s "~%")))
      (let ((templates (package-owned-prompt-templates definition :buffer buffer)))
        (when templates
          (format s "Prompt Templates:~%")
          (dolist (template templates)
            (format s "  - /~A~@[ ~A~]: ~A~%"
                    (prompt-template-name template)
                    (let ((hint (template-argument-summary template)))
                      (and (plusp (length hint)) hint))
                    (or (prompt-template-description template) "")))
          (format s "~%")))
      (let ((buffer-types (package-owned-buffer-types name)))
        (when buffer-types
          (format s "Buffer Types:~%")
          (dolist (type buffer-types)
            (format s "  - ~(~A~): ~A~%"
                    (buffer-type-name type)
                    (or (buffer-type-description type) "")))
          (format s "~%")))
      (let ((docs (package-owned-extended-docs name)))
        (when docs
          (format s "Documentation:~%")
          (dolist (entry docs)
            (let ((doc (cdr entry)))
              (format s "  - ~(~A~)~@[: ~A~]~%"
                      (car entry)
                      (getf doc :category))))
          (format s "~%"))))))

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
               (manifest-string (getf manifest :system-prompt-section)))
             (prompt-template-directory
               (manifest-directory-pathname
                (getf manifest :prompt-template-directory)))
             (slash-commands
               (manifest-package-slash-command-list
                (getf manifest :slash-commands)
                manifest-path)))
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
                :system-prompt-section system-prompt-section
                :prompt-template-directory
                (and prompt-template-directory
                     (merge-pathnames prompt-template-directory root))
                :slash-commands slash-commands))))))

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
   :system-prompt-section (getf manifest :system-prompt-section)
   :prompt-template-directory (getf manifest :prompt-template-directory)
   :slash-commands (copy-list (getf manifest :slash-commands))))

(defun register-package-manifest-prompt-section (definition)
  "Register DEFINITION's manifest-level prompt section when present."
  (labels ((inferred-title (body)
             (loop :for line :in (split-lines body)
                   :for trimmed := (string-trim '(#\Space #\Tab #\Return #\Newline)
                                                line)
                   :for heading := (string-left-trim "#" trimmed)
                   :when (and (plusp (length trimmed))
                              (< (length heading) (length trimmed))
                              (char= (char heading 0) #\Space))
                     :return (string-trim '(#\Space #\Tab) heading))))
    (when (package-definition-system-prompt-section definition)
      (register-package-prompt-section
       (package-definition-name definition)
       (package-definition-system-prompt-section definition)
       :package (package-definition-name definition)
       :title (or (inferred-title
                   (package-definition-system-prompt-section definition))
                  (format nil "Package ~A" (package-definition-name definition)))))))

(defun resolve-package-manifest-handler (definition handler-name)
  "Return the function named by HANDLER-NAME for DEFINITION, or NIL on warning."
  (let* ((symbol-name (string-upcase handler-name))
         (symbol (find-symbol symbol-name :clawmacs)))
    (cond
      ((null symbol)
       (emit-package-warning
        "Package ~A slash command handler ~A is not interned in the CLAWMACS package"
        (package-definition-name definition)
        handler-name)
       nil)
      ((not (fboundp symbol))
       (emit-package-warning
        "Package ~A slash command handler ~A is not fbound after load"
        (package-definition-name definition)
        handler-name)
       nil)
      (t
       symbol))))

(defun register-package-manifest-slash-commands (definition)
  "Register DEFINITION's manifest-declared slash commands."
  (dolist (spec (package-definition-slash-commands definition))
    (let ((handler (resolve-package-manifest-handler
                    definition
                    (package-slash-command-spec-handler spec))))
      (when handler
        (register-slash-command
         (package-slash-command-spec-name spec)
         handler
         :description (package-slash-command-spec-description spec)
         :argument-hint (package-slash-command-spec-argument-hint spec)
         :package (package-definition-name definition))))))

(defun %load-package-definition-entrypoint (definition)
  "Load DEFINITION's entrypoint unless its root is already loaded."
  (let* ((install-key (package-install-key (package-definition-root definition)))
         (package-name (package-definition-name definition))
         (entrypoint (package-definition-entrypoint definition))
         (install-record (read-package-install-record
                          (package-definition-root definition)))
         (*current-package-resource-types*
           (and install-record
                (package-install-record-resource-types install-record))))
    (when (gethash install-key *loaded-packages*)
      (return-from %load-package-definition-entrypoint definition))
    (handler-case
        (let ((*default-pathname-defaults* (package-definition-root definition))
              (*package* (find-package :clawmacs))
              (*current-clawmacs-package* package-name))
          (load entrypoint :verbose nil :print nil)
          (register-package-manifest-prompt-section definition)
          (register-package-manifest-slash-commands definition)
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

(defun load-package-definition-entrypoint (definition)
  "Load DEFINITION's entrypoint exactly once across concurrent callers."
  (call-with-package-lifecycle
   (lambda () (%load-package-definition-entrypoint definition))
   :operation (format nil "load package ~A"
                      (package-definition-name definition))))

(defun load-package-entrypoint (manifest package-root install-dir)
  "Load MANIFEST's entrypoint from PACKAGE-ROOT unless INSTALL-DIR is loaded."
  (declare (ignore package-root install-dir))
  (load-package-definition-entrypoint
   (package-definition-from-manifest manifest)))

(defun %reset-package-runtime-state (package)
  "Remove package-owned runtime registrations for PACKAGE."
  (let* ((definition (typecase package
                       (package-definition package)
                       (t (find-installed-package package))))
         (name (and definition (package-definition-name definition))))
    (unless definition
      (return-from %reset-package-runtime-state nil))
    (setf *package-prompt-sections*
          (remove name
                  *package-prompt-sections*
                  :key #'package-prompt-section-package
                  :test #'string=))
    (dolist (metadata (package-owned-tool-metadata name))
      (unregister-agent-tool-metadata (agent-tool-metadata-symbol metadata)))
    (when (fboundp 'remove-registered-tool-definitions-for-package)
      (funcall (symbol-function 'remove-registered-tool-definitions-for-package)
               name))
    (when (fboundp 'remove-agent-definitions-for-package)
      (funcall 'remove-agent-definitions-for-package name))
    (when (fboundp 'remove-pipeline-registrations-for-package)
      (funcall 'remove-pipeline-registrations-for-package name))
    (when (fboundp 'remove-package-hook-registrations)
      (funcall 'remove-package-hook-registrations name))
    (when (fboundp 'remove-package-advices)
      (funcall 'remove-package-advices name))
    (remove-command-metadata-for-package name)
    (remove-slash-commands-for-package name)
    (remove-buffer-types-for-package name)
    (remove-buffer-input-presentation-providers-for-package name)
    (remove-extended-docs-for-package name)
    (remhash (package-install-key (package-definition-root definition))
             *loaded-packages*)
    definition))

(defun reset-package-runtime-state (package)
  "Remove PACKAGE registrations only after process quiescence is proven."
  (call-with-package-runtime-maintenance
   (lambda () (%reset-package-runtime-state package))
   :operation (format nil "reset package ~A" package)))

(defun %reload-clawmacs-package (package)
  "Reload PACKAGE by removing package-owned runtime state, then loading it."
  (let ((definition (reset-package-runtime-state package)))
    (when definition
      (load-clawmacs-package definition))))

(defun reload-clawmacs-package (package)
  "Reload PACKAGE only after process quiescence is proven."
  (call-with-package-runtime-maintenance
   (lambda () (%reload-clawmacs-package package))
   :operation (format nil "reload package ~A" package)))

(defun %reload-active-packages (&key buffer agent-name)
  "Reload packages active for BUFFER/AGENT-NAME and return loaded definitions."
  (let ((loaded nil))
    (dolist (name (active-package-names :buffer buffer :agent-name agent-name))
      (let ((definition (find-installed-package name :buffer buffer)))
        (cond
          ((null definition)
           (emit-package-warning "Enabled Clawmacs package ~A is not installed"
                                 name))
          (t
           (let ((result (reload-clawmacs-package definition)))
             (when result
               (when (fboundp 'register-package-agent-tool-provider-definitions)
                 (funcall
                  (symbol-function
                   'register-package-agent-tool-provider-definitions)
                  (package-definition-name result)))
               (push result loaded)))))))
    (nreverse loaded)))

(defun reload-active-packages (&key buffer agent-name)
  "Reload active packages only after process quiescence is proven."
  (call-with-package-runtime-maintenance
   (lambda ()
     (%reload-active-packages :buffer buffer :agent-name agent-name))
   :operation "reload active packages"
   :buffer buffer))

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

(defun installed-package-manifest-roots (&key project)
  "Return package roots installed under the configured install roots."
  (let ((roots nil))
    (when (probe-file *packages-directory*)
      (push *packages-directory* roots))
    (when project
      (let ((project-root (project-packages-directory project)))
        (when (probe-file project-root)
          (push project-root roots))))
    (nreverse roots)))

(defun scan-installed-package-definitions (&key project)
  "Return package definitions discovered from installed package roots."
  (let ((definitions nil))
    (dolist (root (installed-package-manifest-roots :project project))
      (let ((source-tier (if (and project
                                  (equal (namestring (uiop:ensure-directory-pathname root))
                                         (namestring (project-packages-directory project))))
                             :project
                             :third-party)))
        (dolist (manifest-path (directory
                                (merge-pathnames #P"*/manifest.lisp"
                                                 (uiop:ensure-directory-pathname root))))
          (let ((manifest (read-package-manifest
                           (uiop:pathname-directory-pathname manifest-path)
                           :source-tier source-tier)))
            (when manifest
              (push (package-definition-from-manifest manifest) definitions))))))
    (nreverse definitions)))

(defun unique-package-definitions (definitions)
  "Return DEFINITIONS de-duplicated by package name, keeping first wins."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (definition definitions)
      (let ((name (package-definition-name definition)))
        (unless (gethash name seen)
          (setf (gethash name seen) t)
          (push definition result))))
    (nreverse result)))

(defun package-project-designator (buffer project)
  "Return the project designator implied by BUFFER or PROJECT."
  (or project
      (and buffer (buffer-project-name buffer))))

(defun list-installed-packages (&key buffer project)
  "Return package definitions present on disk and available for enablement."
  (let* ((project-designator (package-project-designator buffer project))
         (project-definitions
           (and project-designator
                (scan-installed-package-definitions :project project-designator)))
         (global-definitions
           (scan-installed-package-definitions)))
    (unique-package-definitions
     (append project-definitions
             global-definitions
             (list-available-packages)))))

(defun find-installed-package (name &key buffer project)
  "Find an installed package definition by NAME."
  (let ((normalized-name (manifest-package-name name)))
    (and normalized-name
         (find normalized-name
               (list-installed-packages :buffer buffer :project project)
               :key #'package-definition-name
               :test #'string=))))

(defun %load-clawmacs-package (package &key seen buffer project)
  "Load PACKAGE by name or definition, including dependencies.
Returns the loaded package definition on success, or NIL on warning/failure."
  (let* ((definition (typecase package
                       (package-definition package)
                       (t (find-installed-package package
                                                 :buffer buffer
                                                 :project project))))
         (name (and definition (package-definition-name definition))))
    (unless definition
      (return-from %load-clawmacs-package
        (emit-package-warning "Unknown Clawmacs package ~S" package)))
    (let ((seen-table (or seen (make-hash-table :test #'equal))))
      (when (gethash name seen-table)
        (return-from %load-clawmacs-package definition))
      (setf (gethash name seen-table) t)
      (dolist (dependency (package-definition-dependencies definition))
        (unless (load-clawmacs-package dependency
                                       :seen seen-table
                                       :buffer buffer
                                       :project project)
          (return-from %load-clawmacs-package
            (emit-package-warning
             "Package ~A dependency ~A failed to load"
             name
             dependency))))
      (load-package-definition-entrypoint definition))))

(defun load-clawmacs-package (package &key seen buffer project)
  "Load PACKAGE and dependencies under the process-wide cold-load owner."
  (call-with-package-lifecycle
   (lambda ()
     (%load-clawmacs-package package
                             :seen seen
                             :buffer buffer
                             :project project))
   :operation (format nil "load package ~A" package)))

(defun %load-active-packages (&key buffer agent-name)
  "Load packages active for BUFFER/AGENT-NAME and return loaded definitions."
  (let ((loaded nil))
    (dolist (name (active-package-names :buffer buffer :agent-name agent-name))
      (let ((definition (find-installed-package name :buffer buffer)))
        (cond
          ((null definition)
           (emit-package-warning "Enabled Clawmacs package ~A is not installed"
                                 name))
          (t
           (let ((result (load-clawmacs-package definition
                                                :buffer buffer)))
             (when result
               (when (fboundp 'register-package-agent-tool-provider-definitions)
                 (funcall
                  (symbol-function
                   'register-package-agent-tool-provider-definitions)
                  (package-definition-name result)))
               (push result loaded)))))))
    (nreverse loaded)))

(defun load-active-packages (&key buffer agent-name)
  "Load the complete active package set under one process-wide owner."
  (call-with-package-lifecycle
   (lambda ()
     (%load-active-packages :buffer buffer :agent-name agent-name))
   :operation "load active packages"))

(defun load-autoload-packages ()
  "Compatibility wrapper that loads globally enabled packages."
  (load-active-packages))

(defun clawmacs-use-package (&key (src-type :git) repo ref scope project
                               resource-types &allow-other-keys)
  "Install a Clawmacs package from a git repository without enabling it.
Returns the installed package definition on success, or NIL on warning/failure."
  (let ((normalized-source-type (normalize-package-source-type src-type))
        (normalized-repo (normalize-package-repo repo))
        (normalized-scope (or scope :global))
        (normalized-project (and project (ensure-project project)))
        (normalized-resource-types
          (normalize-package-resource-type-list resource-types)))
    (cond
      ((null normalized-repo)
        (emit-package-warning "clawmacs-use-package requires :repo to be a string or pathname"))
      ((null normalized-source-type)
        (emit-package-warning "Unsupported package source type ~S" src-type))
      (t
       (let* ((install-dir (package-install-directory normalized-source-type
                                                      normalized-repo
                                                      :scope normalized-scope
                                                      :project normalized-project))
              (package-root (or (probe-file install-dir) install-dir)))
         (when (ensure-package-installed normalized-repo
                                         install-dir
                                         :src-type normalized-source-type
                                         :ref ref)
           (let ((manifest (read-package-manifest
                            package-root
                            :source-tier (if (eq normalized-scope :project)
                                             :project
                                             :third-party))))
             (when manifest
               (write-package-install-record
                install-dir
                (package-install-record-plist
                 (package-definition-from-manifest manifest)
                 :source-type normalized-source-type
                 :source normalized-repo
                 :ref ref
                 :scope normalized-scope
                 :project (and normalized-project
                               (project-name normalized-project))
                 :resource-types normalized-resource-types)))
             (and manifest
                  (package-definition-from-manifest manifest)))))))))

(defun package-install-record-for-definition (definition)
  "Return persisted install metadata for DEFINITION, when present."
  (and definition
       (read-package-install-record (package-definition-root definition))))

(defun package-install-record-scope-label (record)
  "Return a short human-readable install scope label for RECORD."
  (case (package-install-record-scope record)
    (:project "project")
    (:global "global")
    (otherwise "legacy")))

(defun package-install-record-resource-summary (record)
  "Return a display string describing RECORD's resource policy."
  (let ((types (package-install-record-resource-types record)))
    (if types
        (format nil "resources: ~{~(~A~)~^, ~}" types)
        "resources: all")))

(defun package-install-status-entry (definition &key buffer project)
  "Return a status plist for an installed package DEFINITION."
  (let* ((record (package-install-record-for-definition definition))
         (name (package-definition-name definition))
         (scope (package-enablement-scope name :buffer buffer)))
    (list :name name
          :enabled-scope scope
          :install-scope (and record (package-install-record-scope record))
          :source-type (and record (package-install-record-source-type record))
          :source (and record (package-install-record-source record))
          :ref (and record (package-install-record-ref record))
          :project (or (and record (package-install-record-project record))
                       (and project (project-display-name project)))
          :resource-types (and record (package-install-record-resource-types record))
          :description (package-definition-description definition)
          :root (namestring (package-definition-root definition))
          :manifest-present-p (probe-file
                               (merge-pathnames "manifest.lisp"
                                                (package-definition-root definition)))
          :record-present-p (probe-file
                            (package-install-metadata-path
                             (package-definition-root definition))))))

(defun package-doctor-report (&key buffer project)
  "Return a list of installed-package health records."
  (loop :for definition :in (list-installed-packages :buffer buffer :project project)
        :collect
        (let* ((root (package-definition-root definition))
               (manifest (ignore-errors
                           (read-package-manifest root
                                                  :source-tier
                                                  (package-definition-source-tier
                                                   definition))))
               (record (package-install-record-for-definition definition))
               (source-type (and record (package-install-record-source-type record)))
               (source (and record (package-install-record-source record)))
               (status (cond
                         ((null manifest) :broken-manifest)
                         ((and (member source-type '(:path :local :npm) :test #'eq)
                               (or (null source) (not (probe-file source))))
                          :missing-source)
                         (t :ok))))
          (list :name (package-definition-name definition)
                :status status
                :scope (package-install-record-scope record)
                :install-scope (package-install-record-scope record)
                :enabled-scope (package-enablement-scope
                                (package-definition-name definition)
                                :buffer buffer)
                :description (package-definition-description definition)
                :source-type source-type
                :source source
                :ref (and record (package-install-record-ref record))
                :resource-types (and record (package-install-record-resource-types record))
                :root (namestring root)
                :manifest-present-p (not (null manifest))
                :record-present-p (not (null record))))))

(defun package-status-to-string (&key buffer project)
  "Return a human-readable package status report."
  (with-output-to-string (out)
    (format out "Installed packages:~%")
    (dolist (entry (package-doctor-report :buffer buffer :project project))
      (format out "- [~(~A~)] [~(~A~)] ~A~%"
              (or (getf entry :install-scope) :legacy)
              (getf entry :enabled-scope)
              (getf entry :name))
      (when (getf entry :description)
        (format out "  ~A~%" (getf entry :description)))
      (when (getf entry :source-type)
        (format out "  source: ~(~A~)~@[ ~A~]~%"
                (getf entry :source-type)
                (getf entry :source)))
      (when (getf entry :resource-types)
        (format out "  resources: ~{~(~A~)~^, ~}~%"
                (getf entry :resource-types)))
      (format out "  root: ~A~%" (getf entry :root))
      (format out "  status: ~(~A~)~%" (getf entry :status))
      (when (getf entry :ref)
        (format out "  ref: ~A~%" (getf entry :ref)))
      (terpri out))))

(defun package-doctor-to-string (&key buffer project)
  "Return a human-readable package doctor report."
  (with-output-to-string (out)
    (format out "Package doctor report:~%")
    (dolist (entry (package-doctor-report :buffer buffer :project project))
      (format out "- ~(~A~): ~(~A~)~%" (getf entry :name) (getf entry :status))
      (when (not (eq (getf entry :status) :ok))
        (format out "  root: ~A~%" (getf entry :root))
        (when (getf entry :source)
          (format out "  source: ~A~%" (getf entry :source)))
        (when (getf entry :resource-types)
          (format out "  resources: ~{~(~A~)~^, ~}~%"
                  (getf entry :resource-types)))))
    (terpri out)))

(defun package-resource-policy-string (definition)
  "Return a short resource policy string for DEFINITION."
  (let ((record (package-install-record-for-definition definition)))
    (if record
        (package-install-record-resource-summary record)
        "resources: all")))

(defvar *package-dashboard-origin-buffer-table* (make-hash-table :test #'eq)
  "Map package dashboard buffers to the originating interaction buffer.")

(defun package-dashboard-origin-buffer (buffer)
  "Return the originating interaction buffer for package dashboard BUFFER."
  (gethash buffer *package-dashboard-origin-buffer-table*))

(defun (setf package-dashboard-origin-buffer) (origin buffer)
  "Record ORIGIN as the interaction buffer for package dashboard BUFFER."
  (setf (gethash buffer *package-dashboard-origin-buffer-table*) origin))

(defun package-dashboard-buffer-p (buffer)
  "Return true when BUFFER is a package dashboard buffer."
  (and buffer (eq (buffer-kind buffer) :package-dashboard)))

(defun package-dashboard-entry-face (scope status)
  "Return a display face for package entry SCOPE and STATUS."
  (cond
    ((not (eq status :ok)) :system)
    ((eq scope :default) :selector-entry)
    (t :selector-selected)))

(defun package-dashboard-entry-line (entry)
  "Return one summary line for dashboard ENTRY."
  (format nil "[~A] [~A] ~A :: ~A"
          (package-scope-label (getf entry :enabled-scope))
          (string-downcase (symbol-name (or (getf entry :status) :unknown)))
          (getf entry :name)
          (or (getf entry :description) "")))

(defun package-dashboard-detail-line (entry)
  "Return a secondary detail line for dashboard ENTRY."
  (let* ((source-type (getf entry :source-type))
         (source (getf entry :source))
         (resource-types (getf entry :resource-types))
         (project (getf entry :project))
         (parts
           (remove nil
                   (list
                    (and source-type
                         (format nil "source: ~(~A~)~@[ ~A~]"
                                 source-type source))
                    (and resource-types
                         (format nil "resources: ~{~(~A~)~^, ~}"
                                 resource-types))
                    (and project
                         (format nil "project: ~A" project))))))
    (if parts
        (format nil "  ~{~A~^ | ~}" parts)
        "  source: bundled")))

(defun package-dashboard-display-entries (dashboard-buffer &optional columns)
  "Return styled generic presentation entries for DASHBOARD-BUFFER."
  (declare (ignore columns))
  (let* ((origin (package-dashboard-origin-buffer dashboard-buffer))
         (items (package-doctor-report :buffer origin))
         (target-name (if origin
                          (buffer-name origin)
                          "<default>"))
         (target-agent (if origin
                           (buffer-agent-name origin)
                           *default-agent-name*))
         (entries
           (list
            (list :text (format nil "Packages for ~A" target-name)
                  :face :selector-title)
            (list :text (format nil "Target agent: ~A" target-agent)
                  :face :selector-header)
            (list :text "Select toggles scope. Describe shows full package help."
                  :face :selector-footer)
            (list :text "" :face :default-text))))
    (if items
        (dolist (entry (sort (copy-list items) #'string< :key (lambda (item)
                                                                (getf item :name)))
                 (nreverse entries))
          (push (list :text (package-dashboard-detail-line entry)
                      :face :selector-footer)
                entries)
          (push (list :text (package-dashboard-entry-line entry)
                      :face (package-dashboard-entry-face
                             (getf entry :enabled-scope)
                             (getf entry :status))
                      :object (list :dashboard-buffer dashboard-buffer
                                    :origin-buffer origin
                                    :entry entry)
                      :presentation-type 'package-dashboard-entry-ref)
                entries))
        (nconc entries
               (list (list :text "[No installed packages available.]"
                           :face :system))))))

(defun package-dashboard-refresh (dashboard-buffer)
  "Request redisplay of DASHBOARD-BUFFER."
  (when dashboard-buffer
    (notify-buffer-display-change dashboard-buffer :package-dashboard))
  dashboard-buffer)

(defun package-dashboard-toggle-entry (dashboard-buffer entry &key origin-buffer)
  "Cycle the package scope for ENTRY from DASHBOARD-BUFFER."
  (let* ((origin (or origin-buffer
                     (package-dashboard-origin-buffer dashboard-buffer)))
         (name (getf entry :name))
         (scope (cycle-package-enablement-scope name :buffer origin)))
    (load-active-packages :buffer origin)
    (when origin
      (buffer-insert-system-message
       origin
       (format nil "[Package ~A ~A]"
               name
               (package-scope-message scope))))
    (package-dashboard-refresh dashboard-buffer)
    scope))

(defun package-dashboard-describe-entry (entry &key buffer)
  "Open a help buffer describing package dashboard ENTRY."
  (let* ((definition (or (find-installed-package (getf entry :name) :buffer buffer)
                         (find-installed-package (getf entry :name))))
         (name (and definition (package-definition-name definition))))
    (when definition
      (let* ((content (describe-installed-package-to-string definition buffer))
             (buf-name (format nil "*help:package:~A*" name))
             (existing (find-buffer-by-name buf-name)))
        (if existing
            (progn
              (set-message-text (message-prev (buffer-input-message existing))
                                content)
              (switch-to-buffer existing))
            (switch-to-buffer (make-help-buffer buf-name content)))))))

(defun open-package-dashboard (&key buffer)
  "Open or refresh the package dashboard for BUFFER."
  (reload-package-channels)
  (let* ((origin (or buffer (current-buffer)))
         (existing (find-buffer-by-name "*Packages*")))
    (if (and existing (package-dashboard-buffer-p existing))
        (progn
          (setf (package-dashboard-origin-buffer existing) origin
                (buffer-working-directory existing)
                (if origin
                    (buffer-working-directory origin)
                    (truename ".")))
          (package-dashboard-refresh existing)
          (switch-to-buffer existing))
        (let ((dashboard (make-buffer "*Packages*"
                                      :agent-name "packages"
                                      :kind :package-dashboard
                                      :working-directory
                                      (if origin
                                          (buffer-working-directory origin)
                                          (truename ".")))))
          (initialize-buffer-display-defaults dashboard)
          (setf (buffer-major-mode dashboard) "package-dashboard"
                (package-dashboard-origin-buffer dashboard) origin)
          (add-buffer-to-ring dashboard)
          (switch-to-buffer dashboard)))))

(register-buffer-type
 :package-dashboard
 :description "Installed package browser."
 :major-mode "package-dashboard"
 :presentation-function 'package-dashboard-display-entries)

(defun %remove-installed-package (package &key buffer project)
  "Delete PACKAGE's installed files and return its definition."
  (let* ((definition (typecase package
                       (package-definition package)
                       (t (find-installed-package package
                                                 :buffer buffer
                                                 :project project))))
         (root (and definition (package-definition-root definition))))
    (when definition
      (reset-package-runtime-state definition)
      (ignore-errors
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
      (clear-package-registry)
      definition)))

(defun remove-installed-package (package &key buffer project)
  "Remove PACKAGE only after runtime quiescence, before deleting any files."
  (call-with-package-runtime-maintenance
   (lambda ()
     (%remove-installed-package package :buffer buffer :project project))
   :operation (format nil "remove package ~A" package)
   :buffer buffer))

(defun refresh-package-source-directory (definition record)
  "Refresh DEFINITION's source directory using RECORD."
  (let* ((root (package-definition-root definition))
         (source-type (package-install-record-source-type record))
         (source (package-install-record-source record))
         (ref (package-install-record-ref record)))
    (cond
      ((member source-type '(:path :local :npm) :test #'eq)
       (when (probe-file root)
         (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))
       (ensure-package-installed source root :src-type source-type :ref ref))
      ((eq source-type :git)
       (multiple-value-bind (stdout stderr exit-code)
           (uiop:run-program (list "git" "-C" (namestring root)
                                   "pull" "--ff-only")
                             :output :string
                             :error-output :string
                             :ignore-error-status t)
         (if (zerop exit-code)
             t
             (emit-package-warning
              "Failed to update package ~A from ~A: ~A"
              (package-definition-name definition)
              source
              (package-output-summary stdout stderr exit-code)))))
      (t
       (emit-package-warning "Package ~A cannot be updated from source type ~S"
                             (package-definition-name definition)
                             source-type)))))

(defun %update-installed-package (package &key buffer project)
  "Refresh PACKAGE from its recorded source and return the updated definition."
  (let* ((definition (typecase package
                       (package-definition package)
                       (t (find-installed-package package
                                                 :buffer buffer
                                                 :project project))))
         (record (and definition
                      (package-install-record-for-definition definition))))
    (when (and definition record)
      (when (refresh-package-source-directory definition record)
        (reset-package-runtime-state definition)
        (write-package-install-record
         (package-definition-root definition)
         (progn
           (setf (getf record :updated-at) (get-universal-time))
           record))
        (clear-package-registry)
        (reload-clawmacs-package definition)))))

(defun update-installed-package (package &key buffer project)
  "Update PACKAGE only after quiescence, before refreshing its source tree."
  (call-with-package-runtime-maintenance
   (lambda ()
     (%update-installed-package package :buffer buffer :project project))
   :operation (format nil "update package ~A" package)
   :buffer buffer))

(defun %set-installed-package-resource-types (package resource-types
                                                     &key buffer project)
  "Persist RESOURCE-TYPES as PACKAGE's allowlist and return the new record."
  (let* ((definition (typecase package
                       (package-definition package)
                       (t (find-installed-package package
                                                 :buffer buffer
                                                 :project project))))
         (root (and definition (package-definition-root definition)))
         (record (and definition
                      (or (package-install-record-for-definition definition)
                          (package-install-record-plist definition))))
         (normalized (normalize-package-resource-type-list resource-types)))
    (when (and definition root)
      (setf (getf record :resource-types) (coerce normalized 'vector)
            (getf record :updated-at) (get-universal-time))
      (write-package-install-record root record)
      (clear-package-registry)
      (reload-clawmacs-package definition)
      normalized)))

(defun set-installed-package-resource-types (package resource-types
                                                     &key buffer project)
  "Set PACKAGE's resource policy only after process quiescence is proven."
  (call-with-package-runtime-maintenance
   (lambda ()
     (%set-installed-package-resource-types package resource-types
                                            :buffer buffer
                                            :project project))
   :operation (format nil "configure package ~A" package)
   :buffer buffer))

(defun install-package-status-string (package &key buffer project)
  "Return a short status string for PACKAGE."
  (let ((definition (typecase package
                      (package-definition package)
                      (t (find-installed-package package
                                                :buffer buffer
                                                :project project)))))
    (if definition
        (with-output-to-string (out)
          (format out "[~(~A~)] ~A - ~A~%"
                  (package-install-record-scope
                   (or (package-install-record-for-definition definition)
                       (list :scope :global)))
                  (package-definition-name definition)
                  (package-display-description definition))
          (format out "  root: ~A~%" (namestring (package-definition-root definition)))
          (format out "  ~A~%" (package-resource-policy-string definition)))
        "Unknown package.")))
