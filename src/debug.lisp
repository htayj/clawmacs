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
     (format nil "~A ~S" (mcclim-debug-class-name object) object))))

(defun mcclim-debug-feature-status ()
  "Return a plist describing the current McCLIM debug integration state."
  (list :debugger (mcclim-debug-system-status :debugger)
        :inspector (mcclim-debug-system-status :inspector)
        :listener (mcclim-debug-system-status :listener)
        :debugger-enabled-p *mcclim-debugger-enabled*
        :listener-debugger-enabled-p *mcclim-listener-debugger-enabled*
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
    (format out "- M-x mcclim-refresh-inspectors-command: refresh Clouseau roots opened by Clawmacs.~%")))

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
  (mcclim-debug-ensure-system :inspector)
  (multiple-value-bind (root frame)
      (mcclim-debug-call (mcclim-debug-package-name :inspector)
                         "INSPECT"
                         object
                         :new-process t
                         :handle-errors t)
    (push (list :label label
                :object root
                :frame frame
                :timestamp (get-universal-time))
          *mcclim-debug-inspector-frames*)
    (format nil "Opened Clouseau inspector for ~A." label)))

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
  (mcclim-debug-ensure-system :inspector)
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
