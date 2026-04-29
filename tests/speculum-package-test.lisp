(in-package :clawmacs/tests)

(in-suite speculum-package-suite)

(defmacro with-speculum-package-state (&body body)
  "Run BODY with isolated package, project, and tool registries."
  `(let* ((root (temp-package-test-directory "speculum-config"))
          (clawmacs::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (clawmacs::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (clawmacs::*tool-table* (make-hash-table :test #'equal))
          (clawmacs::*project-registry* (make-hash-table :test #'equal))
          (clawmacs::*project-definitions-loaded-p* nil)
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels* (default-package-test-channels))
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
          (clawmacs::*enabled-builtin-packages* nil)
          (clawmacs::*clawmacs-frame* nil)
          (clawmacs::*mcclim-live-frames* nil))
     ,@body))

(defun load-test-speculum-package ()
  "Enable and load the bundled speculum package."
  (set-package-enablement-scope "speculum" :global)
  (load-active-packages))

(defun speculum-package-tool-result (tool-name args)
  "Execute TOOL-NAME with ARGS and read its Lisp data result."
  (nth-value 0
    (clawmacs::lisp-data-read
     (clawmacs:execute-tool tool-name args))))

(test speculum-package-registers-agent-tools-and-prompt
  "Enabling speculum exposes package-scoped provider tools and prompt guidance."
  (with-speculum-package-state
    (load-test-speculum-package)
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<))
           (prompt (render-package-prompt-sections)))
      (dolist (name '("speculum_inspect"
                      "speculum_screenshot"
                      "speculum_window_state"))
        (is (member name tool-names :test #'string=))
        (is-false (clawmacs::tool-requires-permission-p name)))
      (is (search "McCLIM self-visibility with speculum" prompt))
      (is (search "speculum_screenshot" prompt))
      (is (search "fixed allowlist" prompt))
      (is (search "Prefer these tools over `lisp_eval`" prompt)))))

(test inactive-speculum-tools-are-hidden-from-provider-discovery
  "Speculum tools remain package-scoped until the package is enabled."
  (with-speculum-package-state
    (clawmacs:load-clawmacs-package "speculum")
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (mapcar (lambda (tool)
                                 (cdr (assoc :name tool)))
                               tools)))
      (is-false (member "speculum_screenshot" tool-names :test #'string=))
      (is-false (member "speculum_window_state" tool-names :test #'string=)))))

(test speculum-window-state-reports-no-frame-gracefully
  "Prompt-only or non-GUI sessions get structured unavailable state."
  (with-speculum-package-state
    (load-test-speculum-package)
    (let ((result (speculum-package-tool-result
                   "speculum_window_state"
                   '(:scope "all"))))
      (is-false (getf result :ok))
      (is-false (getf result :available))
      (is (search "No live Clawmacs McCLIM frame" (getf result :reason))))))

(test speculum-screenshot-reports-no-frame-gracefully
  "Screenshot capture does not crash without a live McCLIM frame."
  (with-speculum-package-state
    (load-test-speculum-package)
    (let ((result (speculum-package-tool-result
                   "speculum_screenshot"
                   '(:path "screenshots/speculum/unit-no-frame.png"))))
      (is-false (getf result :ok))
      (is-false (getf result :available))
      (is (search "No live Clawmacs McCLIM frame" (getf result :reason))))))

(test speculum-inspect-uses-allowlist
  "speculum_inspect exposes known names and reports unknown names safely."
  (with-speculum-package-state
    (load-test-speculum-package)
    (let* ((result (speculum-package-tool-result
                    "speculum_inspect"
                    '(:names #("live_frame_count" "not-a-real-variable"))))
           (results (getf result :results))
           (live-count (aref results 0))
           (unknown (aref results 1)))
      (is (getf result :ok))
      (is (getf live-count :available))
      (is (= 0 (getf live-count :value)))
      (is-false (getf unknown :available))
      (is (search "Unknown speculum inspection name"
                  (getf unknown :reason)))
      (is (find "render-snapshot"
                (coerce (getf result :allowlist) 'list)
                :test #'string=)))))

(test speculum-pane-summary-includes-service-panes
  "Standard pane summaries include compose and pointer documentation panes."
  (let* ((summaries (clawmacs::speculum-panes-summary nil))
         (names (map 'list (lambda (entry) (getf entry :name)) summaries)))
    (is (member "compose-pane" names :test #'string=))
    (is (member "pointer-doc-pane" names :test #'string=))))
