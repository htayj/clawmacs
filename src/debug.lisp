(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Debug Logging
;;; --------------------------------------------------------------------------

(defvar *debug-mode* nil
  "When non-nil, all API requests and responses are echoed into the chat
window as debug messages. Toggle interactively with C-c C-d.")

(defvar *debug-log-file* nil
  "When non-nil, a pathname to a file where detailed debug log entries are
appended. Set via the --debug-log <path> command-line flag. Unlike
*debug-mode* (which shows condensed info in the chat buffer), this logs
raw NDJSON lines, stream state transitions, CLI spawn args, stderr
output, and other low-level details useful for post-mortem debugging.")

(defun debug-log (buf text)
  "Insert TEXT as a debug message in BUF when *debug-mode* is enabled.
Uses the global :debug face (bright magenta) so debug output is visually
distinct from normal system messages (cyan). Returns the message object,
or nil if debug mode is off."
  (when *debug-mode*
    (let* ((msg (buffer-insert-system-message buf text))
           (debug-face (or (global-face :debug)
                           (make-instance 'drawing-style :name :debug
                             :background-ink (make-cga-ink 15)
                             :ink (make-cga-ink 5)
                             :bold-p nil :underline-p nil :reverse-p nil)))
           (debug-fs (make-face-set
                      :debug
                      (list (make-instance 'drawing-style
                              :name :default
                              :parent debug-face
                              :background-ink nil :ink nil
                              :bold-p nil :underline-p nil :reverse-p nil)))))
      (setf (message-face-set msg) debug-fs)
      msg)))

(defun file-debug-log (category format-string &rest format-args)
  "Append a timestamped debug entry to *debug-log-file* when set.
CATEGORY is a short tag (e.g. \"cli-spawn\", \"ndjson\", \"stream-event\").
Thread-safe: opens, writes, and closes the file on each call."
  (when *debug-log-file*
    (ignore-errors
      (let ((line (format nil "[~A] [~A] ~?~%"
                          (format-timestamp (get-universal-time))
                          category
                          format-string format-args)))
        (with-open-file (f *debug-log-file*
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create
                           :external-format :utf-8)
          (write-string line f)
          (force-output f))))))

(defun format-timestamp (universal-time)
  "Format UNIVERSAL-TIME as ISO 8601 local time string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

;;; --------------------------------------------------------------------------
;;; McCLIM Debugging
;;; --------------------------------------------------------------------------

(defvar *mcclim-debugger-enabled* nil
  "When non-nil, the McCLIM debugger has been installed as *debugger-hook*.")

(defvar *mcclim-debugger-previous-hook* nil
  "The debugger hook that was active before installing the McCLIM debugger.")

(defvar *mcclim-listener-debugger-enabled* t
  "When non-nil, CLIM Listener processes are launched with debugger support.")

(defvar *mcclim-debug-inspector-frames* nil
  "Recent Clouseau inspector frames opened by Clawmacs debug commands.")

(defvar *clouseau-extensions-installed-p* nil
  "When non-nil, Clawmacs-specific Clouseau inspection methods are installed.")

(defvar *clouseau-extension-classes* nil
  "Class and structure names for which Clawmacs installed Clouseau methods.")

(defvar *clouseau-inspector-counter* 0
  "Process-local counter used to label Clouseau inspectors opened by Clawmacs.")

(defparameter *mcclim-debug-systems*
  '((:debugger "clim-debugger" "CLIM-DEBUGGER" "INSTALL-DEBUGGER")
    (:inspector "clouseau" "CLOUSEAU" "INSPECT")
    (:listener "clim-listener" "CLIM-LISTENER" "RUN-LISTENER"))
  "McCLIM debug-related systems loaded lazily by Clawmacs.")

(declaim (special *clawmacs-frame*))

(defun mcclim-debug-system-spec (name)
  "Return the debug system spec named NAME."
  (or (assoc name *mcclim-debug-systems* :test #'eq)
      (error "Unknown McCLIM debug system ~S." name)))

(defun mcclim-debug-system-name (name)
  "Return the ASDF system name for debug integration NAME."
  (second (mcclim-debug-system-spec name)))

(defun mcclim-debug-package-name (name)
  "Return the package name for debug integration NAME."
  (third (mcclim-debug-system-spec name)))

(defun mcclim-debug-entry-name (name)
  "Return the primary entrypoint name for debug integration NAME."
  (fourth (mcclim-debug-system-spec name)))

(defun mcclim-debug-system-available-p (system-name)
  "Return true when ASDF can locate SYSTEM-NAME."
  (handler-case
      (not (null (asdf:find-system system-name nil)))
    (error () nil)))

(defun mcclim-debug-find-symbol (package-name symbol-name)
  "Find SYMBOL-NAME in PACKAGE-NAME, returning nil when unavailable."
  (let ((package (find-package package-name)))
    (when package
      (multiple-value-bind (symbol status)
          (find-symbol symbol-name package)
        (when status symbol)))))

(defun mcclim-debug-function (package-name symbol-name)
  "Return the function named SYMBOL-NAME in PACKAGE-NAME, or nil."
  (let ((symbol (mcclim-debug-find-symbol package-name symbol-name)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun mcclim-debug-call (package-name symbol-name &rest args)
  "Call SYMBOL-NAME from PACKAGE-NAME with ARGS."
  (let ((function (mcclim-debug-function package-name symbol-name)))
    (unless function
      (error "~A:~A is not available." package-name symbol-name))
    (apply function args)))

(defun mcclim-debug-load-system (name)
  "Load the McCLIM debug integration NAME.
Returns two values: success-p and a human-readable message."
  (let ((system-name (mcclim-debug-system-name name)))
    (handler-case
        (progn
          (asdf:load-system system-name)
          (values t (format nil "Loaded ~A." system-name)))
      (error (condition)
        (values nil
                (format nil "Could not load ~A: ~A"
                        system-name condition))))))

(defun mcclim-debug-ensure-system (name)
  "Ensure debug integration NAME is loaded, or signal an error."
  (multiple-value-bind (ok message)
      (mcclim-debug-load-system name)
    (unless ok
      (error "~A" message))
    message))

(defun mcclim-debug-system-status (name)
  "Return a plist describing debug integration NAME."
  (let* ((system (mcclim-debug-system-name name))
         (package-name (mcclim-debug-package-name name))
         (entry-name (mcclim-debug-entry-name name)))
    (list :name name
          :system system
          :package package-name
          :available-p (mcclim-debug-system-available-p system)
          :loaded-p (not (null (find-package package-name)))
          :entrypoint-p (not (null (mcclim-debug-function package-name
                                                          entry-name))))))

(defun mcclim-debug-current-frame ()
  "Return the primary or first live Clawmacs McCLIM frame, if any."
  (or (and (boundp '*clawmacs-frame*)
           *clawmacs-frame*
           (not (ignore-errors (frame-quit-flag *clawmacs-frame*)))
           *clawmacs-frame*)
      (and (fboundp 'mcclim-live-frames)
           (first (mcclim-live-frames)))))

(defun mcclim-debug-live-frame-count ()
  "Return the number of live McCLIM frames known to Clawmacs."
  (if (fboundp 'mcclim-live-frames)
      (length (mcclim-live-frames))
      (if (mcclim-debug-current-frame) 1 0)))

(defun mcclim-debug-class-name (object)
  "Return a printable class name for OBJECT."
  (handler-case
      (let ((name (class-name (class-of object))))
        (if name
            (string-downcase (symbol-name name))
            (format nil "~S" (class-of object))))
    (error () "unknown")))

(defun mcclim-debug-object-summary (object)
  "Return a compact description of OBJECT for debug reports."
  (cond
    ((null object) "nil")
    ((typep object 'buffer)
     (format nil "~A buffer \"~A\""
             (string-downcase (symbol-name (buffer-kind object)))
             (buffer-name object)))
    (t
     (format nil "~A ~A"
             (mcclim-debug-class-name object)
             (mcclim-debug-safe-string object)))))

(defun mcclim-debug-safe-string (object &optional (limit 160))
  "Return a bounded printed representation of OBJECT."
  (handler-case
      (let ((*print-circle* t)
            (*print-length* 8)
            (*print-level* 4)
            (*print-lines* 4)
            (*print-pretty* nil)
            (*print-readably* nil))
        (let ((text (prin1-to-string object)))
          (if (> (length text) limit)
              (concatenate 'string (subseq text 0 limit) "...")
              text)))
    (error (condition)
      (format nil "#<unprintable ~A: ~A>"
              (mcclim-debug-class-name object) condition))))

(defun clouseau-bound-value (symbol &optional default)
  "Return SYMBOL's value when it is bound, otherwise DEFAULT."
  (if (boundp symbol)
      (symbol-value symbol)
      default))

(defun clouseau-call-if-bound (symbol object)
  "Call SYMBOL as a one-argument reader for OBJECT when possible."
  (if (and (symbolp symbol) (fboundp symbol))
      (funcall symbol object)
      nil))

(defun clouseau-buffer-message-count (buffer)
  "Return the number of messages in BUFFER, including the input message."
  (loop :for message := (buffer-first-message buffer)
          :then (message-next message)
        :while message
        :count message))

(defun clouseau-buffer-input-text (buffer)
  "Return BUFFER's editable input text."
  (message-text (buffer-input-message buffer)))

(defun clouseau-buffer-pending-stream-present-p (buffer)
  "Return true when BUFFER has an active pending stream state."
  (not (null (buffer-pending-stream buffer))))

(defun clouseau-buffer-streaming-message-present-p (buffer)
  "Return true when BUFFER is currently updating a streaming message."
  (not (null (buffer-streaming-message buffer))))

(defun clouseau-session-active-path-length (session)
  "Return the length of SESSION's active branch path."
  (length (session-active-path-ids session)))

(defun clouseau-session-tree-root-count (session)
  "Return the number of tree roots in SESSION."
  (length (session-tree-roots session)))

(defun clouseau-session-current-branch-events (session)
  "Return SESSION's active branch events."
  (session-active-branch-message-events session))

(defun clouseau-session-tree-node-child-count (node)
  "Return NODE's child count."
  (length (session-tree-node-children node)))

(defun clouseau-window-buffer-summary (window)
  "Return a concise summary of WINDOW's buffer."
  (mcclim-debug-object-summary (clawmacs-window-buffer window)))

(defun clouseau-package-channels ()
  "Return registered package channels, when the package manager is loaded."
  (clouseau-bound-value '*package-channels* nil))

(defun clouseau-available-packages ()
  "Return discovered package definitions, when the package manager is loaded."
  (when (fboundp 'list-available-packages)
    (list-available-packages)))

(defun clouseau-agent-tools ()
  "Return registered agent tool metadata, when the tool system is loaded."
  (when (fboundp 'list-agent-tool-metadata)
    (list-agent-tool-metadata)))

(defun clouseau-pipeline-definitions ()
  "Return registered pipeline definitions, when the pipeline system is loaded."
  (when (fboundp 'list-pipeline-definitions)
    (list-pipeline-definitions)))

(defun clouseau-frame-visible-buffer (frame)
  "Return FRAME's visible buffer when available."
  (when (fboundp 'frame-visible-buffer)
    (frame-visible-buffer frame)))

(defun clouseau-frame-selected-window (frame)
  "Return FRAME's selected logical window when available."
  (when (fboundp 'frame-selected-window)
    (frame-selected-window frame)))

(defun clouseau-frame-main-pane (frame)
  "Return FRAME's main pane when available."
  (ignore-errors (clim:find-pane-named frame 'main-pane)))

(defun clouseau-frame-input-pane (frame)
  "Return FRAME's input pane when available."
  (ignore-errors (clim:find-pane-named frame 'input-pane)))

(defun clouseau-application-state ()
  "Return a plist of live Clawmacs state useful as a Clouseau root."
  (let ((frame (mcclim-debug-current-frame)))
    (list :frame frame
          :visible-buffer (and frame (clouseau-frame-visible-buffer frame))
          :selected-window (and frame (clouseau-frame-selected-window frame))
          :current-buffer (ignore-errors (current-buffer))
          :buffer-ring (clouseau-bound-value '*buffer-ring* nil)
          :package-channels (clouseau-package-channels)
          :available-packages (ignore-errors (clouseau-available-packages))
          :agent-tools (ignore-errors (clouseau-agent-tools))
          :pipelines (ignore-errors (clouseau-pipeline-definitions))
          :debug-status (mcclim-debug-feature-status))))

(defun clouseau-format-reader-row (stream object reader label)
  "Write a Clouseau reader place row for READER on OBJECT."
  (when (and (symbolp reader) (fboundp reader))
    (let ((reader-place (mcclim-debug-find-symbol "CLOUSEAU" "READER-PLACE")))
      (when reader-place
        (mcclim-debug-call "CLOUSEAU" "FORMAT-PLACE-ROW"
                           stream object reader-place reader
                           :label label)))))

(defun clouseau-format-reader-table (stream object title rows)
  "Write TITLE and Clouseau place ROWS for OBJECT."
  (fresh-line stream)
  (when title
    (write-string title stream)
    (fresh-line stream))
  (clim:formatting-table (stream)
    (dolist (row rows)
      (destructuring-bind (label reader) row
        (clouseau-format-reader-row stream object reader label))))
  (fresh-line stream))

(defun clouseau-buffer-header (object stream)
  "Write a Clouseau header for a Clawmacs buffer."
  (format stream "Clawmacs buffer ~S [~(~A~), ~(~A~)]"
          (buffer-name object)
          (buffer-kind object)
          (buffer-status object)))

(defun clouseau-buffer-body (object stream)
  "Write Clouseau summary rows for a Clawmacs buffer."
  (clouseau-format-reader-table
   stream object "Clawmacs buffer summary"
   '(("Name" buffer-name)
     ("Kind" buffer-kind)
     ("Status" buffer-status)
     ("Agent" buffer-agent-name)
     ("Working directory" buffer-working-directory)
     ("Project" buffer-project-name)
     ("Resource" buffer-resource-path)
     ("Dirty" buffer-dirty-p)
     ("Pipeline" buffer-pipeline-name)
     ("Session" buffer-session)
     ("Messages" clouseau-buffer-message-count)
     ("Input text" clouseau-buffer-input-text)
     ("Enabled packages" buffer-enabled-packages)
     ("Show tool results" buffer-show-tool-results-p)
     ("Show reasoning" buffer-show-reasoning-p)
     ("Show metadata" buffer-show-metadata-p)
     ("Pending stream" clouseau-buffer-pending-stream-present-p)
     ("Streaming message" clouseau-buffer-streaming-message-present-p))))

(defun clouseau-message-header (object stream)
  "Write a Clouseau header for a Clawmacs message."
  (format stream "Clawmacs ~(~A~) message [~D line~:P]"
          (message-sender object)
          (message-line-count object)))

(defun clouseau-message-body (object stream)
  "Write Clouseau summary rows for a Clawmacs message."
  (clouseau-format-reader-table
   stream object "Clawmacs message summary"
   '(("Sender" message-sender)
     ("Read-only" message-read-only-p)
     ("Timestamp" message-timestamp)
     ("Entry id" message-entry-id)
     ("Parent entry id" message-parent-entry-id)
     ("Line count" message-line-count)
     ("Point line" message-current-line-number)
     ("Point column" message-current-column-number)
     ("Text" message-text)
     ("Metadata" message-metadata)
     ("Raw content" message-raw-content)
     ("Previous message" message-prev)
     ("Next message" message-next))))

(defun clouseau-session-header (object stream)
  "Write a Clouseau header for a saved session."
  (format stream "Clawmacs session ~S [~A]"
          (session-name object)
          (session-id object)))

(defun clouseau-session-body (object stream)
  "Write Clouseau summary rows for a saved session."
  (clouseau-format-reader-table
   stream object "Clawmacs session summary"
   '(("Name" session-name)
     ("Id" session-id)
     ("Directory" session-directory)
     ("Manifest" session-manifest-path)
     ("Transcript directory" session-transcript-directory)
     ("Current transcript" session-current-transcript-path)
     ("Transcript index" session-current-transcript-index)
     ("Current leaf id" session-current-leaf-id)
     ("Parent session" session-parent-session)
     ("Active path length" clouseau-session-active-path-length)
     ("Tree root count" clouseau-session-tree-root-count)
     ("Current branch events" clouseau-session-current-branch-events))))

(defun clouseau-session-tree-node-header (object stream)
  "Write a Clouseau header for a session tree node."
  (format stream "Clawmacs session tree node ~A"
          (or (ignore-errors
                (session-event-id (session-tree-node-entry object)))
              "(unlabeled)")))

(defun clouseau-session-tree-node-body (object stream)
  "Write Clouseau summary rows for a session tree node."
  (clouseau-format-reader-table
   stream object "Session tree node summary"
   '(("Entry" session-tree-node-entry)
     ("Label" session-tree-node-label)
     ("Children" session-tree-node-children)
     ("Child count" clouseau-session-tree-node-child-count))))

(defun clouseau-window-header (object stream)
  "Write a Clouseau header for a logical Clawmacs window."
  (format stream "Clawmacs logical window ~D"
          (clawmacs-window-id object)))

(defun clouseau-window-body (object stream)
  "Write Clouseau summary rows for a logical Clawmacs window."
  (clouseau-format-reader-table
   stream object "Logical window summary"
   '(("Id" clawmacs-window-id)
     ("Buffer" clawmacs-window-buffer)
     ("Buffer summary" clouseau-window-buffer-summary))))

(defun clouseau-package-channel-header (object stream)
  "Write a Clouseau header for a package channel."
  (format stream "Clawmacs package channel ~S"
          (clouseau-call-if-bound 'package-channel-name object)))

(defun clouseau-package-channel-body (object stream)
  "Write Clouseau summary rows for a package channel."
  (clouseau-format-reader-table
   stream object "Package channel summary"
   '(("Name" package-channel-name)
     ("Root" package-channel-root)
     ("Description" package-channel-description)
     ("Source" package-channel-source))))

(defun clouseau-package-definition-header (object stream)
  "Write a Clouseau header for a package definition."
  (format stream "Clawmacs package ~S"
          (clouseau-call-if-bound 'package-definition-name object)))

(defun clouseau-package-definition-body (object stream)
  "Write Clouseau summary rows for a package definition."
  (clouseau-format-reader-table
   stream object "Package definition summary"
   '(("Name" package-definition-name)
     ("Description" package-definition-description)
     ("Root" package-definition-root)
     ("Entrypoint" package-definition-entrypoint)
     ("Channel" package-definition-channel)
     ("Source tier" package-definition-source-tier)
     ("Autoload" package-definition-autoload)
     ("Dependencies" package-definition-dependencies)
     ("System prompt section" package-definition-system-prompt-section))))

(defun clouseau-agent-tool-header (object stream)
  "Write a Clouseau header for tool metadata."
  (format stream "Clawmacs tool ~S"
          (clouseau-call-if-bound 'agent-tool-metadata-name object)))

(defun clouseau-agent-tool-body (object stream)
  "Write Clouseau summary rows for tool metadata."
  (clouseau-format-reader-table
   stream object "Agent tool summary"
   '(("Lisp symbol" agent-tool-metadata-symbol)
     ("Provider name" agent-tool-metadata-name)
     ("Description" agent-tool-metadata-description)
     ("Arguments" agent-tool-metadata-args)
     ("Input schema" agent-tool-metadata-input-schema)
     ("Permission" agent-tool-metadata-permission)
     ("Call style" agent-tool-metadata-call-style)
     ("Command" agent-tool-metadata-command-p)
     ("Lambda list" agent-tool-metadata-lambda-list)
     ("Package" agent-tool-metadata-package))))

(defun clouseau-pipeline-definition-header (object stream)
  "Write a Clouseau header for a pipeline definition."
  (format stream "Clawmacs pipeline ~S"
          (clouseau-call-if-bound 'pipeline-definition-name object)))

(defun clouseau-pipeline-definition-body (object stream)
  "Write Clouseau summary rows for a pipeline definition."
  (clouseau-format-reader-table
   stream object "Pipeline definition summary"
   '(("Name" pipeline-definition-name)
     ("Description" pipeline-definition-description)
     ("Entry stage" pipeline-definition-entry-stage)
     ("Stages" pipeline-definition-stages)
     ("Max steps" pipeline-definition-max-steps)
     ("Max tool iterations" pipeline-definition-max-tool-iterations)
     ("Auto approve tools" pipeline-definition-auto-approve-tools-p))))

(defun clouseau-pipeline-stage-header (object stream)
  "Write a Clouseau header for a pipeline stage."
  (format stream "Clawmacs pipeline stage ~S"
          (clouseau-call-if-bound 'pipeline-stage-name object)))

(defun clouseau-pipeline-stage-body (object stream)
  "Write Clouseau summary rows for a pipeline stage."
  (clouseau-format-reader-table
   stream object "Pipeline stage summary"
   '(("Name" pipeline-stage-name)
     ("Agent" pipeline-stage-agent-name)
     ("Prompt" pipeline-stage-prompt)
     ("Next" pipeline-stage-next)
     ("Provider" pipeline-stage-provider)
     ("Model" pipeline-stage-model)
     ("Think level" pipeline-stage-think-level)
     ("Tools" pipeline-stage-tool-names)
     ("Packages" pipeline-stage-package-names)
     ("Max tool iterations" pipeline-stage-max-tool-iterations)
     ("Auto approve tools" pipeline-stage-auto-approve-tools-p))))

(defun clouseau-pipeline-run-result-header (object stream)
  "Write a Clouseau header for a pipeline run result."
  (format stream "Clawmacs pipeline result ~S [~(~A~)]"
          (clouseau-call-if-bound 'pipeline-run-result-pipeline-name object)
          (clouseau-call-if-bound 'pipeline-run-result-status object)))

(defun clouseau-pipeline-run-result-body (object stream)
  "Write Clouseau summary rows for a pipeline run result."
  (clouseau-format-reader-table
   stream object "Pipeline run result summary"
   '(("Pipeline" pipeline-run-result-pipeline-name)
     ("Status" pipeline-run-result-status)
     ("Original prompt" pipeline-run-result-original-prompt)
     ("Stage results" pipeline-run-result-stage-results)
     ("Final stage" pipeline-run-result-final-stage-result)
     ("Final text" pipeline-run-result-final-text)
     ("Error" pipeline-run-result-error))))

(defun clouseau-frame-header (object stream)
  "Write a Clouseau header for the Clawmacs McCLIM frame."
  (format stream "Clawmacs McCLIM frame [~A render sequence]"
          (clouseau-call-if-bound 'frame-render-sequence object)))

(defun clouseau-frame-body (object stream)
  "Write Clouseau summary rows for the Clawmacs McCLIM frame."
  (clouseau-format-reader-table
   stream object "McCLIM frame summary"
   '(("Display buffer" frame-display-buffer)
     ("Visible buffer" clouseau-frame-visible-buffer)
     ("Window tree" frame-window-tree)
     ("Selected window id" frame-selected-window-id)
     ("Selected window" clouseau-frame-selected-window)
     ("Main pane" clouseau-frame-main-pane)
     ("Input pane" clouseau-frame-input-pane)
     ("Render sequence" frame-render-sequence)
     ("Last render snapshot" frame-last-render-snapshot)
     ("Character width" frame-char-width)
     ("Character height" frame-char-height)
     ("Quit flag" frame-quit-flag)
     ("Syncing Drei" frame-syncing-drei-p)
     ("Last Drei buffer" frame-last-drei-buffer)
     ("UI state" frame-ui-state))))

(defun clouseau-define-inspection-method (class-name header body)
  "Install Clouseau inspection methods for CLASS-NAME."
  (let ((inspect-object-using-state
          (mcclim-debug-find-symbol "CLOUSEAU" "INSPECT-OBJECT-USING-STATE"))
        (inspected-instance
          (mcclim-debug-find-symbol "CLOUSEAU" "INSPECTED-INSTANCE")))
    (when (and inspect-object-using-state
               inspected-instance
               (find-class class-name nil)
               (fboundp header)
               (fboundp body))
      (handler-case
          (progn
            (eval `(defmethod ,inspect-object-using-state
                       ((object ,class-name)
                        (state ,inspected-instance)
                        (style (eql :expanded-header))
                        (stream t))
                     (declare (ignore state style))
                     (,header object stream)))
            (eval `(defmethod ,inspect-object-using-state :before
                       ((object ,class-name)
                        (state ,inspected-instance)
                        (style (eql :expanded-body))
                        (stream t))
                     (declare (ignore state style))
                     (,body object stream)))
            (pushnew class-name *clouseau-extension-classes* :test #'eq)
            t)
        (error (condition)
          (file-debug-log "clouseau"
                          "Could not install inspection methods for ~A: ~A"
                          class-name condition)
          nil)))))

(defun clouseau-install-inspection-methods ()
  "Install Clawmacs-specific Clouseau inspection methods."
  (setf *clouseau-extension-classes* nil)
  (dolist (spec '((buffer clouseau-buffer-header clouseau-buffer-body)
                  (message clouseau-message-header clouseau-message-body)
                  (session clouseau-session-header clouseau-session-body)
                  (session-tree-node clouseau-session-tree-node-header
                   clouseau-session-tree-node-body)
                  (clawmacs-window clouseau-window-header clouseau-window-body)
                  (package-channel clouseau-package-channel-header
                   clouseau-package-channel-body)
                  (package-definition clouseau-package-definition-header
                   clouseau-package-definition-body)
                  (agent-tool-metadata clouseau-agent-tool-header
                   clouseau-agent-tool-body)
                  (pipeline-definition clouseau-pipeline-definition-header
                   clouseau-pipeline-definition-body)
                  (pipeline-stage clouseau-pipeline-stage-header
                   clouseau-pipeline-stage-body)
                  (pipeline-run-result clouseau-pipeline-run-result-header
                   clouseau-pipeline-run-result-body)
                  (clawmacs-gui clouseau-frame-header clouseau-frame-body)))
    (destructuring-bind (class-name header body) spec
      (clouseau-define-inspection-method class-name header body)))
  (setf *clouseau-extensions-installed-p* t)
  *clouseau-extension-classes*)

(defun ensure-clouseau-support (&key force)
  "Load Clouseau and install Clawmacs-specific inspection support."
  (mcclim-debug-ensure-system :inspector)
  (when (or force (not *clouseau-extensions-installed-p*))
    (clouseau-install-inspection-methods))
  (format nil "Clouseau support installed for ~D Clawmacs type~:P."
          (length *clouseau-extension-classes*)))

(defun clouseau-inspector-entries ()
  "Return Clouseau inspectors opened by Clawmacs, newest first."
  (copy-list *mcclim-debug-inspector-frames*))

(defun clouseau-find-inspector-entry (id)
  "Return the remembered Clouseau inspector entry with numeric ID."
  (find id *mcclim-debug-inspector-frames* :key (lambda (entry)
                                                  (getf entry :id))
        :test #'eql))

(defun clouseau-inspectors-to-string ()
  "Return a user-facing list of Clouseau inspectors opened by Clawmacs."
  (with-output-to-string (out)
    (format out "Clouseau Inspectors~%~%")
    (if *mcclim-debug-inspector-frames*
        (dolist (entry (reverse *mcclim-debug-inspector-frames*))
          (format out "#~D  ~A~%" (getf entry :id) (getf entry :label))
          (format out "    root: ~A~%"
                  (mcclim-debug-object-summary (getf entry :object)))
          (format out "    frame: ~A~%"
                  (mcclim-debug-safe-string (getf entry :frame)))
          (format out "    opened: ~A~%~%"
                  (format-timestamp (getf entry :timestamp))))
        (format out "No inspectors have been opened through Clawmacs.~%"))
    (format out "~%Commands:~%")
    (format out "- M-x clouseau-list-inspectors-command: show this page.~%")
    (format out "- M-x clouseau-refresh-inspectors-command: refresh remembered roots.~%")
    (format out "- M-x clouseau-set-inspector-root-command: replace a remembered inspector root with the result of a Lisp form.~%")))

(defun clouseau-update-inspector-root (id object &key label)
  "Set remembered inspector ID's root object to OBJECT and refresh Clouseau."
  (ensure-clouseau-support)
  (let ((entry (clouseau-find-inspector-entry id)))
    (unless entry
      (error "No Clouseau inspector with id ~D is remembered." id))
    (setf (getf entry :object) object)
    (when label
      (setf (getf entry :label) label))
    (mcclim-debug-refresh-inspector-entry entry)
    (format nil "Updated Clouseau inspector #~D to ~A."
            id (or label (mcclim-debug-object-summary object)))))

(defun clouseau-status-to-string ()
  "Return a user-facing Clouseau status and usage report."
  (let ((status (mcclim-debug-system-status :inspector)))
    (with-output-to-string (out)
      (format out "Clouseau Support~%~%")
      (format out "Clouseau is McCLIM's object inspector. Clawmacs uses it for live frame, pane, buffer, session, command, tool, package, and pipeline objects.~%~%")
      (format out "Protocol: clouseau:inspect-object-using-state methods add Clawmacs summaries with clouseau:format-place-row. Start with M-x clouseau-inspect-application-state-command or M-x clouseau-list-inspectors-command.~%~%")
      (format out "Status:~%")
      (format out "- system: ~A~%" (getf status :system))
      (format out "- available: ~A~%" (getf status :available-p))
      (format out "- loaded: ~A~%" (getf status :loaded-p))
      (format out "- inspect entrypoint: ~A~%" (getf status :entrypoint-p))
      (format out "- Clawmacs inspection methods installed: ~A~%"
              *clouseau-extensions-installed-p*)
      (format out "- Clawmacs inspection classes: ~{~(~A~)~^, ~}~%"
              (reverse *clouseau-extension-classes*))
      (format out "- remembered inspectors: ~D~%~%"
              (length *mcclim-debug-inspector-frames*))
      (format out "Clouseau interaction notes from the McCLIM manual:~%")
      (format out "- Left-click object nodes to expand or collapse them.~%")
      (format out "- Middle-click an object to make it the inspector root.~%")
      (format out "- Right-click objects and places for the inspector command menu.~%")
      (format out "- Place rows can copy, set, swap, remove, or increment values when the place supports that operation.~%")
      (format out "- Inspector eval binds CL:* to the selected object or place value and CL:** to the root object.~%")
      (format out "- Clawmacs extensions use clouseau:inspect-object-using-state and clouseau:format-place-row so summaries are normal inspector places, not ad hoc text.~%~%")
      (format out "Commands:~%")
      (format out "- M-x clouseau-status-command: show this page.~%")
      (format out "- M-x clouseau-install-extensions-command: load Clouseau and install Clawmacs inspection methods.~%")
      (format out "- M-x clouseau-list-inspectors-command: list Clouseau windows opened by Clawmacs.~%")
      (format out "- M-x clouseau-inspect-application-state-command: inspect a live state plist.~%")
      (format out "- M-x clouseau-inspect-buffer-ring-command: inspect all buffers.~%")
      (format out "- M-x clouseau-inspect-current-session-command: inspect the current session sidecar.~%")
      (format out "- M-x clouseau-inspect-input-message-command: inspect the editable input message.~%")
      (format out "- M-x clouseau-inspect-package-registry-command: inspect package channels and discovered packages.~%")
      (format out "- M-x clouseau-inspect-tool-registry-command: inspect registered agent tools.~%")
      (format out "- M-x clouseau-inspect-pipeline-registry-command: inspect deterministic pipeline definitions.~%")
      (format out "- M-x clouseau-set-inspector-root-command: refresh one inspector around a new Lisp object.~%"))))

(defun mcclim-debug-feature-status ()
  "Return a plist describing the current McCLIM debug integration state."
  (list :debugger (mcclim-debug-system-status :debugger)
        :inspector (mcclim-debug-system-status :inspector)
        :listener (mcclim-debug-system-status :listener)
        :debugger-enabled-p *mcclim-debugger-enabled*
        :listener-debugger-enabled-p *mcclim-listener-debugger-enabled*
        :clouseau-extensions-installed-p *clouseau-extensions-installed-p*
        :clouseau-extension-classes (copy-list *clouseau-extension-classes*)
        :debugger-hook *debugger-hook*
        :live-frame-count (mcclim-debug-live-frame-count)
        :inspector-count (length *mcclim-debug-inspector-frames*)))

(defun mcclim-debug-status-to-string ()
  "Return a user-facing McCLIM debugging status report."
  (with-output-to-string (out)
    (format out "McCLIM Debugging~%~%")
    (format out "Manual integrations:~%")
    (format out "- clim-debugger: condition debugger with frames, restarts, locals, and frame eval.~%")
    (format out "- Clouseau: object inspector for frames, panes, buffers, functions, slots, and hash tables.~%")
    (format out "- CLIM Listener: CLIM-aware Lisp listener with command output destinations and debugger integration.~%~%")
    (format out "Status:~%")
    (dolist (name '(:debugger :inspector :listener))
      (let* ((status (mcclim-debug-system-status name))
             (system (getf status :system)))
        (format out "- ~A: available=~A loaded=~A entrypoint=~A~%"
                system
                (getf status :available-p)
                (getf status :loaded-p)
                (getf status :entrypoint-p))))
    (format out "- debugger hook installed: ~A~%"
            *mcclim-debugger-enabled*)
    (format out "- listener debugger enabled: ~A~%"
            *mcclim-listener-debugger-enabled*)
    (format out "- Clouseau inspection methods installed: ~A~%"
            *clouseau-extensions-installed-p*)
    (format out "- Clouseau inspection classes: ~{~(~A~)~^, ~}~%"
            (reverse *clouseau-extension-classes*))
    (format out "- live Clawmacs frames: ~D~%"
            (mcclim-debug-live-frame-count))
    (format out "- remembered Clouseau inspectors: ~D~%~%"
            (length *mcclim-debug-inspector-frames*))
    (format out "Commands:~%")
    (format out "- M-x mcclim-debug-status-command: show this page.~%")
    (format out "- M-x mcclim-debug-snapshot-command: inspect frame, pane, window, and buffer state as text.~%")
    (format out "- M-x mcclim-install-debugger-command: install clim-debugger as *debugger-hook*.~%")
    (format out "- M-x mcclim-disable-debugger-command: restore the previous debugger hook.~%")
    (format out "- M-x mcclim-launch-listener-command: open a CLIM Listener process.~%")
    (format out "- M-x mcclim-inspect-current-frame-command: inspect the current application frame in Clouseau.~%")
    (format out "- M-x mcclim-inspect-visible-buffer-command: inspect the visible buffer in Clouseau.~%")
    (format out "- M-x mcclim-inspect-window-tree-command: inspect the logical window tree in Clouseau.~%")
    (format out "- M-x mcclim-inspect-lisp-form-command: evaluate a Lisp form and inspect the result in Clouseau.~%")
    (format out "- M-x mcclim-refresh-inspectors-command: refresh Clouseau roots opened by Clawmacs.~%")
    (format out "- M-x clouseau-status-command: show Clouseau-specific usage and extension status.~%")
    (format out "- M-x clouseau-inspect-application-state-command: inspect a live Clawmacs state plist.~%")))

(defun mcclim-install-debugger ()
  "Install McCLIM's condition debugger as *debugger-hook*."
  (mcclim-debug-ensure-system :debugger)
  (unless *mcclim-debugger-enabled*
    (setf *mcclim-debugger-previous-hook* *debugger-hook*))
  (mcclim-debug-call (mcclim-debug-package-name :debugger)
                     "INSTALL-DEBUGGER")
  (setf *mcclim-debugger-enabled* t)
  "McCLIM debugger installed.")

(defun mcclim-disable-debugger ()
  "Restore the debugger hook that was active before mcclim-install-debugger."
  (if *mcclim-debugger-enabled*
      (progn
        (setf *debugger-hook* *mcclim-debugger-previous-hook*
              *mcclim-debugger-previous-hook* nil
              *mcclim-debugger-enabled* nil)
        "Previous debugger hook restored.")
      "McCLIM debugger was not installed by Clawmacs."))

(defun mcclim-launch-listener ()
  "Launch the McCLIM CLIM Listener in a new process."
  (mcclim-debug-ensure-system :listener)
  (mcclim-debug-call (mcclim-debug-package-name :listener)
                     "RUN-LISTENER"
                     :new-process t
                     :debugger *mcclim-listener-debugger-enabled*)
  (format nil "CLIM Listener launched with debugger=~A."
          *mcclim-listener-debugger-enabled*))

(defun mcclim-debug-open-inspector (object &key (label "object"))
  "Open OBJECT in a new Clouseau inspector process."
  (ensure-clouseau-support)
  (multiple-value-bind (root frame)
      (mcclim-debug-call (mcclim-debug-package-name :inspector)
                         "INSPECT"
                         object
                         :new-process t
                         :handle-errors t)
    (let ((id (incf *clouseau-inspector-counter*)))
      (push (list :id id
                  :label label
                :object root
                :frame frame
                :timestamp (get-universal-time))
            *mcclim-debug-inspector-frames*)
      (format nil "Opened Clouseau inspector #~D for ~A." id label))))

(defun mcclim-debug-refresh-inspector-entry (entry)
  "Refresh one Clouseau inspector ENTRY, returning true on success."
  (let* ((frame (getf entry :frame))
         (object (getf entry :object))
         (root-object (mcclim-debug-find-symbol
                       (mcclim-debug-package-name :inspector)
                       "ROOT-OBJECT"))
         (setter (and root-object
                      (let ((name `(setf ,root-object)))
                        (and (fboundp name) (fdefinition name))))))
    (when (and frame setter)
      (funcall setter object frame :run-hook-p t)
      t)))

(defun mcclim-refresh-inspectors ()
  "Refresh Clouseau inspector roots opened through Clawmacs."
  (ensure-clouseau-support)
  (let ((count 0))
    (dolist (entry *mcclim-debug-inspector-frames*)
      (when (ignore-errors (mcclim-debug-refresh-inspector-entry entry))
        (incf count)))
    (format nil "Refreshed ~D Clouseau inspector~:P." count)))

(defun mcclim-debug-target-object (target &optional buffer)
  "Return the object named by TARGET and a human-readable label."
  (let ((frame (mcclim-debug-current-frame)))
    (case target
      (:frame
       (values (or frame (error "No live Clawmacs McCLIM frame is available."))
               "current McCLIM frame"))
      (:current-buffer
       (values (or buffer (current-buffer)
                   (error "No current buffer is available."))
               "current buffer"))
      (:visible-buffer
       (values (if (and frame (fboundp 'frame-visible-buffer))
                   (frame-visible-buffer frame)
                   (or buffer (current-buffer)
                       (error "No visible buffer is available.")))
               "visible buffer"))
      (:window-tree
       (values (and frame
                    (fboundp 'mcclim-ensure-window-tree)
                    (mcclim-ensure-window-tree frame))
               "logical window tree"))
      (:selected-window
       (values (and frame
                    (fboundp 'frame-selected-window)
                    (frame-selected-window frame))
               "selected logical window"))
      (:main-pane
       (values (and frame (clim:find-pane-named frame 'main-pane))
               "main pane"))
      (:input-pane
       (values (and frame
                    (fboundp 'frame-drei-input-pane)
                    (frame-drei-input-pane frame))
               "Drei input pane"))
      (:minibuffer-pane
       (values (and frame (clim:find-pane-named frame 'minibuffer-pane))
               "minibuffer pane"))
      (:render-snapshot
       (values (and frame
                    (fboundp 'frame-last-render-snapshot)
                    (frame-last-render-snapshot frame))
               "last render snapshot"))
      (:buffer-ring
       (values (clouseau-bound-value '*buffer-ring* nil)
               "buffer ring"))
      (:current-session
       (let ((target-buffer (or buffer (ignore-errors (current-buffer)))))
         (values (and target-buffer (buffer-session target-buffer))
                 "current buffer session")))
      (:input-message
       (let ((target-buffer (or buffer (ignore-errors (current-buffer)))))
         (values (and target-buffer (buffer-input-message target-buffer))
                 "current input message")))
      (:package-registry
       (values (list :channels (clouseau-package-channels)
                     :available-packages (ignore-errors
                                           (clouseau-available-packages)))
               "package registry"))
      (:tool-registry
       (values (clouseau-agent-tools)
               "agent tool registry"))
      (:pipeline-registry
       (values (clouseau-pipeline-definitions)
               "pipeline registry"))
      (:application-state
       (values (clouseau-application-state)
               "Clawmacs application state"))
      (:inspectors
       (values (clouseau-inspector-entries)
               "remembered Clouseau inspectors"))
      (:debug-status
       (values (mcclim-debug-feature-status)
               "McCLIM debug status"))
      (otherwise
       (error "Unknown McCLIM debug target ~S." target)))))

(defun mcclim-debug-inspect-target (target &optional buffer)
  "Open TARGET in Clouseau."
  (multiple-value-bind (object label)
      (mcclim-debug-target-object target buffer)
    (if object
        (mcclim-debug-open-inspector object :label label)
        (format nil "No object is available for ~A." label))))

(defun mcclim-debug-read-eval-form (form)
  "Read and evaluate FORM in the Clawmacs package, returning its first value."
  (let ((*package* (find-package :clawmacs)))
    (eval (read-from-string form))))

(defun mcclim-debug-inspect-lisp-form (form)
  "Evaluate FORM and open its first value in Clouseau."
  (let ((object (mcclim-debug-read-eval-form form)))
    (mcclim-debug-open-inspector
     object
     :label (format nil "result of ~A" form))))

(defun mcclim-debug-region-summary (sheet)
  "Return a compact summary of SHEET's region."
  (handler-case
      (let ((region (clim:sheet-region sheet)))
        (format nil "~Dx~D"
                (floor (clim:bounding-rectangle-width region))
                (floor (clim:bounding-rectangle-height region))))
    (error (condition)
      (format nil "unavailable (~A)" condition))))

(defun mcclim-debug-pane-size-summary (pane)
  "Return a compact summary of PANE's allocated size."
  (handler-case
      (multiple-value-bind (width height)
          (if (fboundp 'pane-pixel-size)
              (pane-pixel-size pane)
              (let ((region (clim:sheet-region pane)))
                (values (floor (clim:bounding-rectangle-width region))
                        (floor (clim:bounding-rectangle-height region)))))
        (format nil "~Dx~D" width height))
    (error (condition)
      (format nil "unavailable (~A)" condition))))

(defun mcclim-debug-stream-cursor-summary (pane)
  "Return PANE's stream cursor summary, if available."
  (handler-case
      (multiple-value-bind (x y)
          (clim:stream-cursor-position pane)
        (format nil "~D,~D" x y))
    (error () "n/a")))

(defun mcclim-debug-pane-report (frame pane-name out)
  "Write a pane report for PANE-NAME in FRAME to OUT."
  (let ((pane (and frame (clim:find-pane-named frame pane-name))))
    (format out "- ~A: " pane-name)
    (if pane
        (format out "~A size=~A region=~A cursor=~A~%"
                (mcclim-debug-class-name pane)
                (mcclim-debug-pane-size-summary pane)
                (mcclim-debug-region-summary pane)
                (mcclim-debug-stream-cursor-summary pane))
        (format out "missing~%"))))

(defun mcclim-debug-window-report (tree out)
  "Write a logical window report for TREE to OUT."
  (if tree
      (dolist (window (clawmacs-window-tree-windows tree))
        (format out "- window ~D: ~A~%"
                (clawmacs-window-id window)
                (mcclim-debug-object-summary
                 (clawmacs-window-buffer window))))
      (format out "- no logical window tree~%")))

(defun mcclim-debug-snapshot-to-string (&optional buffer)
  "Return a textual snapshot of McCLIM frame and rendering state."
  (let ((frame (mcclim-debug-current-frame)))
    (with-output-to-string (out)
      (format out "McCLIM Runtime Snapshot~%~%")
      (format out "Frame: ~A~%"
              (if frame
                  (mcclim-debug-object-summary frame)
                  "no live Clawmacs frame"))
      (when frame
        (format out "Render sequence: ~A~%"
                (if (fboundp 'frame-render-sequence)
                    (ignore-errors (frame-render-sequence frame))
                    "n/a"))
        (format out "Character cell: ~Ax~A~%"
                (if (fboundp 'frame-char-width)
                    (ignore-errors (frame-char-width frame))
                    "n/a")
                (if (fboundp 'frame-char-height)
                    (ignore-errors (frame-char-height frame))
                    "n/a"))
        (format out "Selected logical window id: ~A~%"
                (if (fboundp 'frame-selected-window-id)
                    (ignore-errors (frame-selected-window-id frame))
                    "n/a"))
        (format out "Visible buffer: ~A~%"
                (mcclim-debug-object-summary
                 (ignore-errors
                   (if (fboundp 'frame-visible-buffer)
                       (frame-visible-buffer frame)
                       buffer)))))
      (format out "~%Panes:~%")
      (dolist (pane-name '(main-pane input-pane minibuffer-pane
                           modeline-pane who-line-pane))
        (mcclim-debug-pane-report frame pane-name out))
      (format out "~%Logical windows:~%")
      (mcclim-debug-window-report
       (ignore-errors
         (and frame
              (fboundp 'mcclim-ensure-window-tree)
              (mcclim-ensure-window-tree frame)))
       out)
      (format out "~%Current buffer: ~A~%"
              (mcclim-debug-object-summary
               (or buffer (current-buffer))))
      (format out "Buffer ring length: ~D~%" (length *buffer-ring*))
      (format out "Debugger hook installed: ~A~%"
              *mcclim-debugger-enabled*)
      (format out "Inspector frames remembered: ~D~%"
              (length *mcclim-debug-inspector-frames*)))))
