(in-package :rplaca/tests)

(in-suite organa-package-suite)

(defun organa-package-test-directory (label)
  "Return a fresh temporary directory for Organa package tests."
  (let ((dir (merge-pathnames
              (format nil "rplaca-organa-package-tests-~A-~36R-~36R/"
                      label
                      (get-universal-time)
                      (get-internal-real-time))
              #P"/tmp/")))
    (ensure-directories-exist (merge-pathnames #P".keep" dir))
    dir))

(defmacro with-organa-package-state (&body body)
  "Run BODY with isolated package, project, tool, and buffer registries."
  `(let* ((root (organa-package-test-directory "config"))
          (rplaca::*tool-working-directory* root)
          (rplaca::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (rplaca::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (rplaca::*tool-table* (make-hash-table :test #'equal))
          (rplaca::*project-registry* (make-hash-table :test #'equal))
          (rplaca::*project-definitions-loaded-p* nil)
          (rplaca::*buffer-ring* nil)
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels* (default-package-test-channels))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
          (rplaca::*buffer-type-registry*
           (rplaca::make-buffer-type-registry)))
     ,@body))

(defun organa-test-file (root &optional (name "tasks.org"))
  "Return a pathname under ROOT for an org test file."
  (merge-pathnames name root))

(defun write-organa-test-file (root contents &optional (name "tasks.org"))
  "Write CONTENTS to NAME under ROOT and return the pathname."
  (let ((path (organa-test-file root name)))
    (write-test-file path contents)
    path))

(defun load-test-organa-package ()
  "Enable and load the bundled Organa package."
  (set-package-enablement-scope "organa" :global)
  (load-active-packages))

(defun organa-package-tool-result (tool-name args)
  "Execute TOOL-NAME with ARGS and read its Lisp data result."
  (nth-value 0
    (rplaca::lisp-data-read
     (rplaca:execute-tool tool-name args))))

(defun organa-plist-vector (plist key)
  "Return KEY from PLIST as a list."
  (coerce (getf plist key) 'list))

(defun organa-summary-titles (summary)
  "Return TODO titles from an Organa summary plist."
  (mapcar (lambda (todo) (getf todo :title))
          (organa-plist-vector summary :todos)))

(defmacro with-organa-function-override ((name lambda-list &body implementation)
                                         &body body)
  `(let ((original-function (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name)
                  (lambda ,lambda-list
                    ,@implementation))
            ,@body)
       (setf (symbol-function ',name) original-function))))

(defun make-organa-listener-frame (buffer)
  "Return a listener frame whose conversation buffer is BUFFER."
  (clim:make-application-frame
   'rplaca::rplaca-listener
   :conversation-buffer buffer
   :listener-context (rplaca::make-listener-context)))

(test organa-package-registers-buffer-type-tools-and-prompt
  "Enabling Organa registers its buffer type, commands, tools, and prompt text."
  (with-organa-package-state
    (load-test-organa-package)
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<))
           (prompt (render-package-prompt-sections))
           (buffer-type (find-buffer-type :organa)))
      (is (not (null buffer-type)))
      (is (string= "organa"
                   (rplaca::buffer-type-major-mode buffer-type)))
      (is (eq 'rplaca::organa-display-entries
              (rplaca:buffer-type-presentation-function buffer-type)))
      (dolist (name '("organa_todo_add"
                      "organa_todo_link_dependency"
                      "organa_todo_move"
                      "organa_todo_overview"
                      "organa_todo_set_status"))
        (is (member name tool-names :test #'string=)))
      (is (search "Project TODO management with organa" prompt))
      (is (search "organa_todo_overview" prompt))
      (is (search "ORGANA_DEPENDS" prompt))
      (is (search "instead of raw `lisp_eval`" prompt)))))

(test organa-tools-manage-org-todos-on-disk
  "Organa tools parse, add, reorder, and persist dependency links in org files."
  (with-organa-package-state
    (let ((path (write-organa-test-file
                 root
                 "#+TITLE: Project

* TODO Design parser
* NEXT Implement package
:PROPERTIES:
:ID: implement-package
:END:
* TODO Test package
")))
      (declare (ignore path))
      (load-test-organa-package)
      (let ((overview (organa-package-tool-result
                       "organa_todo_overview"
                       '(:path "tasks.org"))))
        (is (equal '("Design parser" "Implement package" "Test package")
                   (organa-summary-titles overview)))
        (is (= 3 (length (organa-plist-vector overview :todos)))))
      (let ((added (organa-package-tool-result
                    "organa_todo_add"
                    '(:path "tasks.org"
                      :title "Ship package"
                      :status "NEXT"))))
        (is-true (getf added :ok))
        (is (search "* NEXT Ship package"
                    (uiop:read-file-string
                     (organa-test-file root))))
        (is (search ":ID: organa-ship-package"
                    (uiop:read-file-string
                     (organa-test-file root)))))
      (let ((linked (organa-package-tool-result
                     "organa_todo_link_dependency"
                     '(:path "tasks.org"
                       :todo "Test package"
                       :depends-on "Implement package"))))
        (is-true (getf linked :ok))
        (let ((text (uiop:read-file-string (organa-test-file root))))
          (is (search ":ORGANA_DEPENDS: implement-package" text))
          (is (search ":ID: organa-test-package" text))))
      (let ((updated (organa-package-tool-result
                      "organa_todo_set_status"
                      '(:path "tasks.org"
                        :todo "Implement package"
                        :status "DONE"))))
        (is-true (getf updated :ok))
        (is (search "* DONE Implement package"
                    (uiop:read-file-string
                     (organa-test-file root)))))
      (let ((moved (organa-package-tool-result
                    "organa_todo_move"
                    '(:path "tasks.org"
                      :todo "Ship package"
                      :after "Design parser"))))
        (is-true (getf moved :ok))
        (let* ((text (uiop:read-file-string (organa-test-file root)))
               (design-pos (search "* TODO Design parser" text))
               (ship-pos (search "* NEXT Ship package" text))
               (implement-pos (search "* DONE Implement package" text)))
          (is (and design-pos ship-pos implement-pos
                   (< design-pos ship-pos implement-pos))))))))

(test organa-open-command-creates-custom-buffer
  "The user command opens a file-backed Organa buffer with the custom kind."
  (with-organa-package-state
    (write-organa-test-file
     root
     "#+TITLE: Project

* TODO First task
")
    (load-test-organa-package)
    (let ((buffer (rplaca::organa-open-todo-file "tasks.org")))
      (is (eq :organa (buffer-kind buffer)))
      (is (string= "organa" (buffer-major-mode buffer)))
      (is (search "tasks.org" (buffer-resource-path buffer)))
      (is (eq buffer (current-buffer)))
      (is (eq :dashboard (rplaca::organa-view-for-buffer buffer)))
      (rplaca::organa-cycle-view-command buffer)
      (is (eq :kanban (rplaca::organa-view-for-buffer buffer)))
      (rplaca::organa-cycle-view-command buffer)
      (is (eq :dependency (rplaca::organa-view-for-buffer buffer)))
      (is (not (null (find-buffer-type :organa)))))))

(test organa-listener-commands-and-translators-are-registered
  "Organa presentation actions belong to the listener command table."
  (with-organa-package-state
    (load-test-organa-package)
    (let ((table (clim:find-command-table 'rplaca::rplaca-listener)))
      (dolist (command '(rplaca::com-organa-cycle-todo-status
                         rplaca::com-organa-focus-dependency
                         rplaca::com-organa-describe-todo))
        (is-true (clim:command-present-in-command-table-p command table)))
      (dolist (translator '(rplaca::select-organa-todo
                            rplaca::describe-organa-todo
                            rplaca::select-organa-dependency))
        (is-true (clim:find-presentation-translator translator table
                                                    :errorp nil))))))

(test organa-listener-presentation-actions-use-conversation-and-details
  "Organa listener commands mutate the conversation buffer and show descriptions in details."
  (with-organa-package-state
    (write-organa-test-file
     root
     "#+TITLE: Project

* TODO First task
  Explain the first task.
")
    (load-test-organa-package)
    (let* ((buffer (rplaca::organa-open-todo-file "tasks.org"))
           (frame (make-organa-listener-frame buffer))
           (todo (first (rplaca::organa-model-todos
                         (rplaca::organa-location-model
                          (rplaca::organa-read-buffer-location buffer)))))
           (layout-selection nil))
      (let ((clim:*application-frame* frame))
        (rplaca::com-organa-cycle-todo-status todo))
      (is (search "* NEXT First task"
                  (uiop:read-file-string (organa-test-file root))))
      (with-organa-function-override
          (rplaca::listener-set-details-layout (actual-frame)
            (setf layout-selection
                  (rplaca::rplaca-listener-selected-detail actual-frame)))
        (let ((clim:*application-frame* frame))
          (rplaca::com-organa-describe-todo todo)))
      (is (equal layout-selection
                 (rplaca::rplaca-listener-selected-detail frame)))
      (is (eq :text (cdr layout-selection)))
      (is (search "Status: TODO" (car layout-selection))))))

(test organa-todo-presentations-can-cycle-status
  "Selecting an Organa TODO advances it to the next workflow status."
  (with-organa-package-state
    (write-organa-test-file
     root
     "#+TITLE: Project

* TODO First task
")
    (load-test-organa-package)
    (let* ((buffer (rplaca::organa-open-todo-file "tasks.org"))
           (model (rplaca::organa-location-model
                   (rplaca::organa-read-buffer-location buffer)))
           (todo (first (rplaca::organa-model-todos model))))
      (is (string= "TODO" (rplaca::organa-todo-status todo)))
      (is (clim:presentation-typep todo 'rplaca::organa-todo-ref))
      (let ((entry (find-if
                    (lambda (row)
                      (and (eq 'rplaca::organa-todo-ref
                               (getf row :presentation-type))
                           (search "First task" (getf row :text))))
                    (rplaca::organa-display-entries buffer 100))))
        (is (not (null entry)))
        (is (clim:presentation-typep (getf entry :object)
                                     'rplaca::organa-todo-ref)))
      (is (string= "NEXT"
                   (rplaca::organa-cycle-todo-status buffer todo)))
      (is (search "* NEXT First task"
                  (uiop:read-file-string (organa-test-file root)))))))

(test organa-dependency-rows-follow-blockers
  "Dependency rows expose dependency references that focus the target TODO."
  (with-organa-package-state
    (write-organa-test-file
     root
     "#+TITLE: Project

* NEXT Implement package
:PROPERTIES:
:ID: implement-package
:END:
* TODO Test package
:PROPERTIES:
:ID: test-package
:ORGANA_DEPENDS: implement-package
:END:
")
    (load-test-organa-package)
    (let* ((buffer (rplaca::organa-open-todo-file "tasks.org"))
           (location (rplaca::organa-read-buffer-location buffer))
           (rows (rplaca::organa-dependency-rows
                  (rplaca::organa-location-model location)))
           (row (first rows)))
      (is (eq 'rplaca::organa-dependency-ref
              (getf row :presentation-type)))
      (is (string= "implement-package" (getf row :object)))
      (is (clim:presentation-typep (getf row :object)
                                   'rplaca::organa-dependency-ref))
      (is (string= "implement-package"
                   (rplaca::organa-focus-todo-by-id
                    buffer
                    (getf row :object))))
      (is (eq :outline (rplaca::organa-view-for-buffer buffer))))))
