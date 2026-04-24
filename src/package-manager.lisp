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

(defun package-json-key-string (key)
  "Return KEY as a lowercase JSON field name string."
  (cond
    ((keywordp key) (string-downcase (symbol-name key)))
    ((symbolp key) (string-downcase (symbol-name key)))
    ((stringp key) key)
    (t (string key))))

(defun package-lookup-json-value (alist key)
  "Look up KEY in decoded JSON ALIST using string-insensitive key matching."
  (let ((target (string-downcase key)))
    (cdr (find target alist
               :key (lambda (entry)
                      (string-downcase (package-json-key-string (car entry))))
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
    (setf *package-configuration* configuration)))

(defun ensure-package-configuration-loaded ()
  "Return the package enablement configuration, loading it if needed."
  (or *package-configuration*
      (load-package-configuration)))

(defun save-package-configuration ()
  "Persist package enablement configuration to disk."
  (let* ((configuration (ensure-package-configuration-loaded))
         (global (package-table-names
                  (package-configuration-global-table configuration)))
         (agents nil))
    (maphash (lambda (agent-name table)
               (let ((names (package-table-names table)))
                 (when names
                   (push `(,agent-name . ,(coerce names 'vector))
                         agents))))
             (package-configuration-agents-table configuration))
    (ensure-directories-exist *package-configuration-path*)
    (with-open-file (stream *package-configuration-path*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string
       (cl-json:encode-json-to-string
        `((:global . ,(coerce global 'vector))
          (:agents . ,(sort agents #'string< :key #'car))))
       stream))
    *package-configuration-path*))

(defun package-agent-enable-table (agent-name &key create)
  "Return AGENT-NAME's package enablement table."
  (let ((agent-key (normalize-package-agent-name agent-name)))
    (when agent-key
      (let* ((configuration (ensure-package-configuration-loaded))
             (agents (package-configuration-agents-table configuration))
             (table (gethash agent-key agents)))
        (or table
            (when create
              (setf (gethash agent-key agents)
                    (make-package-enable-table))))))))

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
  (package-name-enabled-in-table-p
   package-name
   (package-configuration-global-table
    (ensure-package-configuration-loaded))))

(defun package-enabled-for-agent-p (package-name agent-name)
  "Return true when PACKAGE-NAME is enabled for AGENT-NAME."
  (package-name-enabled-in-table-p
   package-name
   (package-agent-enable-table agent-name)))

(defun package-enablement-scope (package &key buffer agent-name)
  "Return PACKAGE's effective enablement scope for BUFFER/AGENT-NAME."
  (let* ((name (manifest-package-name package))
         (agent (or agent-name
                    (and buffer (buffer-agent-name buffer)))))
    (cond
      ((null name) :default)
      ((buffer-package-name-enabled-p buffer name) :buffer)
      ((package-enabled-for-agent-p name agent) :agent)
      ((package-enabled-globally-p name) :global)
      (t :default))))

(defun active-package-names (&key buffer agent-name)
  "Return package names active for BUFFER/AGENT-NAME."
  (let ((table (make-hash-table :test #'equal))
        (agent (or agent-name
                   (and buffer (buffer-agent-name buffer)))))
    (dolist (name (package-table-names
                   (package-configuration-global-table
                    (ensure-package-configuration-loaded))))
      (setf (gethash name table) t))
    (when agent
      (dolist (name (package-table-names
                     (package-agent-enable-table agent)))
        (setf (gethash name table) t)))
    (when buffer
      (dolist (name (buffer-enabled-packages buffer))
        (let ((normalized (manifest-package-name name)))
          (when normalized
            (setf (gethash normalized table) t)))))
    (package-table-names table)))

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
         (configuration (ensure-package-configuration-loaded)))
    (unless (member scope '(:default :buffer :agent :global) :test #'eq)
      (error "Unsupported package enablement scope: ~S" scope))
    (when buffer
      (set-buffer-package-name-enabled buffer name nil))
    (when agent
      (set-package-name-enabled-in-table
       name (package-agent-enable-table agent :create t) nil))
    (set-package-name-enabled-in-table
     name (package-configuration-global-table configuration) nil)
    (ecase scope
      (:default nil)
      (:buffer
       (set-buffer-package-name-enabled buffer name t))
      (:agent
       (set-package-name-enabled-in-table
        name (package-agent-enable-table agent :create t) t))
      (:global
       (set-package-name-enabled-in-table
        name (package-configuration-global-table configuration) t)))
    (save-package-configuration)
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
  (let ((entries nil))
    (maphash (lambda (_name metadata)
               (declare (ignore _name))
               (when (string= package-name
                              (or (command-metadata-package metadata) ""))
                 (push metadata entries)))
             *command-table*)
    (sort entries #'string<
          :key (lambda (metadata)
                 (symbol-name (command-metadata-name metadata))))))

(defun package-owned-tool-metadata (package-name)
  "Return tool metadata registered by PACKAGE-NAME."
  (remove-if-not (lambda (metadata)
                   (string= package-name
                            (or (agent-tool-metadata-package metadata) "")))
                 (list-agent-tool-metadata)))

(defun package-owned-slash-commands (package-name)
  "Return slash commands registered by PACKAGE-NAME."
  (let ((entries nil))
    (maphash (lambda (_name command)
               (declare (ignore _name))
               (when (string= package-name
                              (or (slash-command-package command) ""))
                 (push command entries)))
             *slash-command-table*)
    (sort entries #'string< :key #'slash-command-name)))

(defun package-owned-prompt-templates (package-name)
  "Return prompt templates contributed by PACKAGE-NAME's prompt directory."
  (let ((definition (find-installed-package package-name)))
    (when definition
      (let ((directory (package-prompt-template-directory definition)))
        (when (and directory (probe-file directory))
          (discover-prompt-templates-in-directory
           directory
           :scope :package
           :package package-name))))))

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

(defun buffer-has-conversation-context-p (buffer)
  "Return true when BUFFER already has provider-visible context."
  (loop :for msg := (buffer-first-message buffer) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buffer))))
        :thereis (not (eq (message-sender msg) :system))))

(defun buffer-package-context-message-present-p (buffer package-name)
  "Return true when BUFFER already contains PACKAGE-NAME's context marker."
  (let ((marker (package-context-message-marker package-name)))
    (loop :for msg := (buffer-first-message buffer) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buffer))))
          :thereis (and (eq (message-sender msg) :context)
                        (search marker (message-text msg))))))

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
          (buffer-insert-context-message buffer text))))))

(defun package-owned-extended-docs (package-name)
  "Return extended documentation entries registered by PACKAGE-NAME."
  (let ((entries nil))
    (maphash (lambda (symbol doc)
               (when (string= package-name (or (getf doc :package) ""))
                 (push (cons symbol doc) entries)))
             *extended-docs*)
    (sort entries #'string< :key (lambda (entry)
                                   (symbol-name (car entry))))))

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
            (format s "  - ~A (~(~A~)): ~A~%"
                    (agent-tool-metadata-name tool)
                    (agent-tool-metadata-permission tool)
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
      (let ((templates (package-owned-prompt-templates name)))
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

(defun load-package-definition-entrypoint (definition)
  "Load DEFINITION's entrypoint unless its root is already loaded."
  (let* ((install-key (package-install-key (package-definition-root definition)))
         (package-name (package-definition-name definition))
         (entrypoint (package-definition-entrypoint definition)))
    (when (gethash install-key *loaded-packages*)
      (return-from load-package-definition-entrypoint definition))
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

(defun load-package-entrypoint (manifest package-root install-dir)
  "Load MANIFEST's entrypoint from PACKAGE-ROOT unless INSTALL-DIR is loaded."
  (declare (ignore package-root install-dir))
  (load-package-definition-entrypoint
   (package-definition-from-manifest manifest)))

(defun reset-package-runtime-state (package)
  "Remove package-owned runtime registrations for PACKAGE."
  (let* ((definition (typecase package
                       (package-definition package)
                       (t (find-installed-package package))))
         (name (and definition (package-definition-name definition))))
    (unless definition
      (return-from reset-package-runtime-state nil))
    (setf *package-prompt-sections*
          (remove name
                  *package-prompt-sections*
                  :key #'package-prompt-section-package
                  :test #'string=))
    (dolist (metadata (package-owned-tool-metadata name))
      (unregister-agent-tool-metadata (agent-tool-metadata-symbol metadata)))
    (dolist (metadata (package-owned-command-metadata name))
      (remhash (command-metadata-name metadata) *command-table*))
    (dolist (command (package-owned-slash-commands name))
      (remhash (slash-command-name command) *slash-command-table*))
    (dolist (type (package-owned-buffer-types name))
      (remhash (buffer-type-name type) *buffer-type-registry*))
    (dolist (entry (package-owned-extended-docs name))
      (remhash (car entry) *extended-docs*))
    (remhash (package-install-key (package-definition-root definition))
             *loaded-packages*)
    definition))

(defun reload-clawmacs-package (package)
  "Reload PACKAGE by removing package-owned runtime state, then loading it."
  (let ((definition (reset-package-runtime-state package)))
    (when definition
      (load-clawmacs-package definition))))

(defun reload-active-packages (&key buffer agent-name)
  "Reload packages active for BUFFER/AGENT-NAME and return loaded definitions."
  (let ((loaded nil))
    (dolist (name (active-package-names :buffer buffer :agent-name agent-name))
      (let ((definition (find-installed-package name)))
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

(defun installed-package-manifest-roots ()
  "Return package roots installed under *PACKAGES-DIRECTORY*."
  (when (probe-file *packages-directory*)
    (loop :for manifest :in (directory
                             (merge-pathnames #P"*/manifest.lisp"
                                              *packages-directory*))
          :collect (uiop:pathname-directory-pathname manifest))))

(defun scan-installed-package-definitions ()
  "Return package definitions discovered from *PACKAGES-DIRECTORY*."
  (let ((definitions nil))
    (dolist (root (installed-package-manifest-roots))
      (let ((manifest (read-package-manifest root :source-tier :third-party)))
        (when manifest
          (push (package-definition-from-manifest manifest) definitions))))
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

(defun list-installed-packages ()
  "Return package definitions present on disk and available for enablement."
  (unique-package-definitions
   (append (scan-installed-package-definitions)
           (list-available-packages))))

(defun find-installed-package (name)
  "Find an installed package definition by NAME."
  (let ((normalized-name (manifest-package-name name)))
    (and normalized-name
         (find normalized-name (list-installed-packages)
               :key #'package-definition-name
               :test #'string=))))

(defun load-clawmacs-package (package &optional seen)
  "Load PACKAGE by name or definition, including dependencies.
Returns the loaded package definition on success, or NIL on warning/failure."
  (let* ((definition (typecase package
                       (package-definition package)
                       (t (find-installed-package package))))
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

(defun load-active-packages (&key buffer agent-name)
  "Load packages active for BUFFER/AGENT-NAME and return loaded definitions."
  (let ((loaded nil))
    (dolist (name (active-package-names :buffer buffer :agent-name agent-name))
      (let ((definition (find-installed-package name)))
        (cond
          ((null definition)
           (emit-package-warning "Enabled Clawmacs package ~A is not installed"
                                 name))
          (t
           (let ((result (load-clawmacs-package definition)))
             (when result
               (when (fboundp 'register-package-agent-tool-provider-definitions)
                 (funcall
                  (symbol-function
                   'register-package-agent-tool-provider-definitions)
                  (package-definition-name result)))
               (push result loaded)))))))
    (nreverse loaded)))

(defun load-autoload-packages ()
  "Compatibility wrapper that loads globally enabled packages."
  (load-active-packages))

(defun clawmacs-use-package (&key (src-type :git) repo &allow-other-keys)
  "Install a Clawmacs package from a git repository without enabling it.
Returns the installed package definition on success, or NIL on warning/failure."
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
              (package-root (or (probe-file install-dir) install-dir)))
         (when (ensure-package-installed normalized-repo install-dir)
           (let ((manifest (read-package-manifest
                            package-root
                            :source-tier :third-party)))
             (and manifest
                  (package-definition-from-manifest manifest)))))))))
