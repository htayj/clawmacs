(in-package :rplaca)

(defun packrat-blank-string-p (value)
  "Return true when VALUE is NIL or only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun packrat-infer-source-type (source)
  "Infer a package source type from SOURCE when none is provided."
  (if (and (stringp source) (probe-file source))
      :path
      :git))

(defun packrat-normalize-source-type (source-type source)
  "Return SOURCE-TYPE or infer one from SOURCE."
  (or (normalize-package-source-type source-type)
      (packrat-infer-source-type source)))

(defun packrat-parse-resource-types (value)
  "Normalize VALUE into a list of resource type keywords."
  (cond
    ((null value) nil)
    ((stringp value)
     (normalize-package-resource-type-list
      (remove-if #'packrat-blank-string-p
                 (uiop:split-string value :separator '(#\, #\Space #\Tab)))))
    ((vectorp value)
     (normalize-package-resource-type-list (coerce value 'list)))
    ((listp value)
     (normalize-package-resource-type-list value))
    (t nil)))

(defun packrat-parse-scope (scope)
  "Normalize SCOPE to a supported install scope keyword."
  (let ((value (manifest-string scope)))
    (cond
      ((null value) :global)
      ((string= (string-downcase value) "project") :project)
      ((string= (string-downcase value) "global") :global)
      (t :global))))

(defun packrat-source-spec->install-args (source &key src-type ref scope project
                                          resource-types)
  "Return keyword arguments for RPLACA-USE-PACKAGE from a source string."
  (let ((normalized-source (manifest-string source))
        (normalized-type (packrat-normalize-source-type src-type source))
        (normalized-scope (packrat-parse-scope scope))
        (normalized-resource-types (packrat-parse-resource-types resource-types)))
    (list :src-type normalized-type
          :repo normalized-source
          :ref (manifest-string ref)
          :scope normalized-scope
          :project project
          :resource-types normalized-resource-types)))

(defun packrat-install-package (source &key src-type ref scope project
                                 resource-types)
  "Install SOURCE and return the installed package definition."
  (apply #'rplaca-use-package
         (packrat-source-spec->install-args
          source
          :src-type src-type
          :ref ref
          :scope scope
          :project project
          :resource-types resource-types)))

(defun packrat-installed-package-definition (package &optional buffer)
  "Return the installed definition for PACKAGE in BUFFER's scope."
  (find-installed-package package :buffer buffer))

(defun packrat-package-summary-lines (&optional buffer project)
  "Return summary lines for the installed packages visible to BUFFER or PROJECT."
  (loop :for definition :in (list-installed-packages :buffer buffer :project project)
        :collect
        (let* ((record (package-install-record-for-definition definition))
               (enabled (package-enablement-scope
                         (package-definition-name definition)
                         :buffer buffer
                         :project project))
               (install-scope (or (and record (package-install-record-scope record))
                                  :legacy)))
          (format nil "[~(~A~)] [~(~A~)] ~A - ~A"
                  install-scope
                  enabled
                  (package-definition-name definition)
                  (package-display-description definition)))))

(defun packrat-status-text (&optional buffer)
  "Return the visible package status text for BUFFER."
  (with-output-to-string (out)
    (format out "~A" (package-status-to-string :buffer buffer))))

(defun packrat-doctor-text (&optional buffer)
  "Return the visible package doctor text for BUFFER."
  (with-output-to-string (out)
    (format out "~A" (package-doctor-to-string :buffer buffer))))

(defun packrat-list-packages-command (buffer)
  "Show installed package status in a help buffer."
  (declare (ignore buffer))
  (switch-to-buffer
   (make-help-buffer "*help:packrat*"
                     (packrat-status-text buffer))))
(defcommand packrat-list-packages-command)

(defun packrat-doctor-command (buffer)
  "Show package doctor output in a help buffer."
  (declare (ignore buffer))
  (switch-to-buffer
   (make-help-buffer "*help:packrat-doctor*"
                     (packrat-doctor-text buffer))))
(defcommand packrat-doctor-command)

(defun packrat-install-package-command (buffer source)
  "Install a package from SOURCE using a repo URL or local path."
  (let ((definition (packrat-install-package
                     source
                     :project (buffer-project-name buffer))))
    (if definition
        (buffer-insert-system-message
         buffer
         (format nil "[Installed ~A]"
                 (package-definition-name definition)))
        (buffer-insert-system-message buffer "[Package install failed.]"))))
(defcommand packrat-install-package-command
  :prompts ((source :prompt "Package source (git URL or local path)")))

(defun packrat-remove-package-command (buffer package)
  "Remove PACKAGE from the current project or global install roots."
  (let ((definition (remove-installed-package package
                                             :buffer buffer
                                             :project (buffer-project-name buffer))))
    (if definition
        (buffer-insert-system-message
         buffer
         (format nil "[Removed ~A]" (package-definition-name definition)))
        (buffer-insert-system-message buffer "[Package remove failed.]"))))
(defcommand packrat-remove-package-command
  :prompts ((package :prompt "Package name")))

(defun packrat-update-package-command (buffer package)
  "Refresh PACKAGE from its recorded source."
  (let ((definition (update-installed-package package
                                             :buffer buffer
                                             :project (buffer-project-name buffer))))
    (if definition
        (buffer-insert-system-message
         buffer
         (format nil "[Updated ~A]" (package-definition-name definition)))
        (buffer-insert-system-message buffer "[Package update failed.]"))))
(defcommand packrat-update-package-command
  :prompts ((package :prompt "Package name")))

(defun packrat-config-package-command (buffer package resource-types)
  "Update PACKAGE's allowed resource types."
  (let ((types (packrat-parse-resource-types resource-types)))
    (if (set-installed-package-resource-types
         package types
         :buffer buffer
         :project (buffer-project-name buffer))
        (buffer-insert-system-message
         buffer
         (format nil "[Configured ~A resources: ~{~(~A~)~^, ~}]"
                 package types))
        (buffer-insert-system-message buffer "[Package config failed.]"))))
(defcommand packrat-config-package-command
  :prompts ((package :prompt "Package name")
            (resource-types :prompt "Allowed resource types (comma-separated)")))

(defun packrat-package-status-command (buffer)
  "Show package status in a help buffer."
  (switch-to-buffer
   (make-help-buffer "*help:packrat-status*"
                     (packrat-status-text buffer))))
(defcommand packrat-package-status-command)

(defun packrat-package-doctor-command (buffer)
  "Show package doctor output in a help buffer."
  (switch-to-buffer
   (make-help-buffer "*help:packrat-doctor*"
                     (packrat-doctor-text buffer))))
(defcommand packrat-package-doctor-command)

(defun packrat-tool-install (args)
  "Install a package from source using tool arguments."
  (let* ((source (tool-arg args :repo "repo"))
         (src-type (tool-arg args :src-type "src_type"))
         (ref (tool-arg args :ref "ref"))
         (scope (tool-arg args :scope "scope"))
         (project (tool-arg args :project "project"))
         (resource-types (tool-arg args :resource-types "resource_types"))
         (definition (packrat-install-package
                      source
                      :src-type src-type
                      :ref ref
                      :scope scope
                      :project project
                      :resource-types resource-types)))
    (lisp-data-string
     (list :ok (not (null definition))
           :package (and definition (package-definition-name definition))
           :root (and definition
                      (namestring (package-definition-root definition)))
           :scope (and definition
                       (package-install-record-scope
                        (package-install-record-for-definition definition)))
           :resource-types (and definition
                                (package-install-record-resource-types
                                 (package-install-record-for-definition
                                  definition)))))))

(defun packrat-tool-remove (args)
  "Remove a package from disk using tool arguments."
  (let* ((package (tool-arg args :package "package"))
         (project (tool-arg args :project "project"))
         (definition (remove-installed-package package :project project)))
    (lisp-data-string
     (list :ok (not (null definition))
           :package (and definition (package-definition-name definition))
           :root (and definition
                      (namestring (package-definition-root definition)))))))

(defun packrat-tool-update (args)
  "Refresh a package from its source using tool arguments."
  (let* ((package (tool-arg args :package "package"))
         (project (tool-arg args :project "project"))
         (definition (update-installed-package package :project project)))
    (lisp-data-string
     (list :ok (not (null definition))
           :package (and definition (package-definition-name definition))
           :root (and definition
                      (namestring (package-definition-root definition)))))))

(defun packrat-tool-list (args)
  "Return a list of installed packages visible to the current project."
  (let* ((project (tool-arg args :project "project"))
         (items (packrat-package-summary-lines nil project)))
    (lisp-data-string
     (list :ok t
           :packages (coerce items 'vector)))))

(defun packrat-tool-config (args)
  "Update a package's resource filter via tool arguments."
  (let* ((package (tool-arg args :package "package"))
         (project (tool-arg args :project "project"))
         (resource-types (tool-arg args :resource-types "resource_types"))
         (types (packrat-parse-resource-types resource-types))
         (result (set-installed-package-resource-types
                  package types
                  :project project)))
    (lisp-data-string
     (list :ok (not (null result))
           :package package
           :resource-types (coerce types 'vector)))))

(defun packrat-tool-status (args)
  "Return package status data for the current project."
  (let ((project (tool-arg args :project "project")))
    (lisp-data-string
     (list :ok t
           :status (package-doctor-report :project project)))))

(defun packrat-tool-doctor (args)
  "Return package doctor data for the current project."
  (let ((project (tool-arg args :project "project")))
    (lisp-data-string
     (list :ok t
           :doctor (package-doctor-report :project project)))))

(deftool packrat-tool-install
  :name "packrat_install"
  :description "Install a RPLACA package from a repo URL or local path."
  :call-style :raw-args
  :execution :frame
  :args ((repo :type "string"
               :description "Git repository URL or local path to install.")
         (src-type :type "string" :required nil
                   :description "Optional source type: git, path, local, or npm.")
         (ref :type "string" :required nil
              :description "Optional git ref to check out after clone.")
         (scope :type "string" :required nil
                :description "Optional install scope: global or project.")
         (project :type "string" :required nil
                  :description "Optional project name for project-local installs.")
         (resource-types :type "array" :required nil
                         :items ((:type . "string"))
                         :description "Optional allowed resource types.")))

(deftool packrat-tool-remove
  :name "packrat_remove"
  :description "Remove an installed RPLACA package."
  :call-style :raw-args
  :execution :frame
  :args ((package :type "string"
                  :description "Installed package name.")
         (project :type "string" :required nil
                  :description "Optional project name for project-local packages.")))

(deftool packrat-tool-update
  :name "packrat_update"
  :description "Update an installed RPLACA package from its recorded source."
  :call-style :raw-args
  :execution :frame
  :args ((package :type "string"
                  :description "Installed package name.")
         (project :type "string" :required nil
                  :description "Optional project name for project-local packages.")))

(deftool packrat-tool-list
  :name "packrat_list"
  :description "List installed packages with install and enablement scopes."
  :call-style :raw-args
  :execution :frame
  :args ((project :type "string" :required nil
                  :description "Optional project name to include project-local packages.")))

(deftool packrat-tool-config
  :name "packrat_config"
  :description "Set an installed package's allowed resource types."
  :call-style :raw-args
  :execution :frame
  :args ((package :type "string"
                  :description "Installed package name.")
         (resource-types :type "array" :items ((:type . "string"))
                         :description "Allowed resource types.")
         (project :type "string" :required nil
                  :description "Optional project name for project-local packages.")))

(deftool packrat-tool-status
  :name "packrat_status"
  :description "Return a package status report."
  :call-style :raw-args
  :execution :frame
  :args ((project :type "string" :required nil
                  :description "Optional project name to include project-local packages.")))

(deftool packrat-tool-doctor
  :name "packrat_doctor"
  :description "Return a package doctor report."
  :call-style :raw-args
  :execution :frame
  :args ((project :type "string" :required nil
                  :description "Optional project name to include project-local packages.")))
