(defpackage :clawmacs/mcclim-e2e
  (:use :cl :clawmacs)
  (:export #:snapshot
           #:write-snapshot
           #:start-control-thread))

(in-package :clawmacs/mcclim-e2e)

(defvar *control-thread* nil)
(defvar *control-thread-lock* (bt:make-lock "mcclim-e2e-control-thread"))
(defvar *control-result-sequence* 0)
(defvar *last-control-result*
  '((:sequence . 0)
    (:ok . t)
    (:value . "")
    (:error . ""))
  "Last deterministic e2e control command result.")

(defun control-dir ()
  (or (uiop:getenv "CLAWMACS_MCCLIM_E2E_CONTROL_DIR")
      (error "CLAWMACS_MCCLIM_E2E_CONTROL_DIR is not set.")))

(defun control-pathname (directory)
  (uiop:ensure-directory-pathname directory))

(defun control-command-pathname (directory)
  (merge-pathnames "command.sexp" (control-pathname directory)))

(defun read-control-command (&optional (directory (control-dir)))
  "Read and remove one pending e2e control command, if present."
  (let ((path (control-command-pathname directory)))
    (when (probe-file path)
      (unwind-protect
           (with-open-file (stream path :direction :input)
             (read stream nil nil))
        (ignore-errors
          (delete-file path))))))

(defun truncate-string (string max-chars)
  (if (and string (> (length string) max-chars))
      (concatenate 'string (subseq string 0 max-chars) "...")
      (or string "")))

(defun message-entry (message)
  `((:sender . ,(string-downcase (symbol-name (message-sender message))))
    (:text . ,(truncate-string (message-text message) 1000))
    (:read-only . ,(message-read-only-p message))
    (:timestamp . ,(message-timestamp message))))

(defun buffer-message-tail (buffer &key (limit 80))
  (let ((items nil))
    (loop :for message := (buffer-first-message buffer) :then (message-next message)
          :while (and message (not (eq message (buffer-input-message buffer))))
          :do (push (message-entry message) items))
    (coerce (last (nreverse items) limit) 'vector)))

(defun modeline-string (buffer)
  (handler-case
      (clawmacs::format-modeline buffer 120 :major-mode (buffer-major-mode buffer))
    (error (condition)
      (format nil "[modeline unavailable: ~A]" condition))))

(defun who-line-vector (buffer)
  (handler-case
      (multiple-value-bind (row1 row2)
          (clawmacs::format-who-line buffer 120)
        (vector row1 row2))
    (error (condition)
      (vector (format nil "[who-line unavailable: ~A]" condition) ""))))

(defun current-mcclim-frame ()
  "Return the primary McCLIM frame when the GUI is running."
  (and (boundp 'clawmacs::*clawmacs-frame*)
       clawmacs::*clawmacs-frame*))

(defun safe-control-result-string (value)
  "Return a bounded readable string for a control command VALUE."
  (let ((*print-length* 80)
        (*print-level* 10)
        (*print-circle* t)
        (*print-pretty* nil))
    (truncate-string (prin1-to-string value) 8000)))

(defun record-control-result (ok value error-text)
  "Record a JSON-ready result for the latest deterministic control command."
  (setf *last-control-result*
        `((:sequence . ,(incf *control-result-sequence*))
          (:ok . ,ok)
          (:value . ,(or value ""))
          (:error . ,(or error-text "")))))

(defun apply-control-eval-command (form)
  "Evaluate FORM for deterministic e2e assertions and record its result."
  (handler-case
      (record-control-result t (safe-control-result-string (eval form)) "")
    (serious-condition (condition)
      (record-control-result nil "" (safe-control-result-string condition)))))

(defun apply-control-window-command (command frame)
  "Apply symbolic window COMMAND to FRAME."
  (clawmacs::with-mcclim-frame-ui-state (frame)
    (ecase command
      (:split-below
       (clawmacs::mcclim-split-selected-window frame :vertical))
      (:split-right
       (clawmacs::mcclim-split-selected-window frame :horizontal))
      (:other-window
       (clawmacs::mcclim-select-other-window frame))
      (:delete-window
       (clawmacs::mcclim-delete-selected-window frame))
      (:delete-other-windows
       (clawmacs::mcclim-delete-other-windows frame)))
    (clawmacs::mcclim-redisplay-frame frame :force-p t))
  (record-control-result t (safe-control-result-string command) ""))

(defun apply-control-command (command)
  "Apply COMMAND to the McCLIM frame for deterministic e2e control."
  (let ((frame (current-mcclim-frame)))
    (cond
      ((and (consp command) (eq (first command) :eval))
       (if frame
           (clawmacs::with-mcclim-frame-ui-state (frame)
             (apply-control-eval-command (second command)))
           (apply-control-eval-command (second command)))
       t)
      ((and frame (symbolp command))
       (apply-control-window-command command frame)
       t)
      ((null frame)
       (record-control-result nil "" "No McCLIM frame is running.")
       t)
      (t
       (record-control-result
        nil "" (format nil "Unknown e2e control command: ~S" command))
       t))))

(defmacro with-current-mcclim-ui-state (&body body)
  "Bind frame-local McCLIM UI state while observing the running application."
  (let ((frame-var (gensym "FRAME-")))
    `(let ((,frame-var (current-mcclim-frame)))
       (if (and ,frame-var (typep ,frame-var 'clawmacs::clawmacs-gui))
           (clawmacs::with-mcclim-frame-ui-state (,frame-var)
             ,@body)
           (progn ,@body)))))

(defun selected-minibuffer-display ()
  (let ((items clawmacs::*minibuffer-filtered-items*)
        (index clawmacs::*minibuffer-selected-index*))
    (when (and items (<= 0 index) (< index (length items)))
      (minibuffer-item-display (nth index items)))))

(defun minibuffer-candidates ()
  (coerce (mapcar #'minibuffer-item-display
                  clawmacs::*minibuffer-filtered-items*)
          'vector))

(defun minibuffer-state ()
  `((:active . ,clawmacs::*minibuffer-active*)
    (:mode . ,(string-downcase (symbol-name clawmacs::*minibuffer-mode*)))
    (:prompt . ,clawmacs::*minibuffer-prompt*)
    (:input . ,clawmacs::*minibuffer-input*)
    (:point . ,clawmacs::*minibuffer-point*)
    (:selected-index . ,clawmacs::*minibuffer-selected-index*)
    (:selected . ,(or (selected-minibuffer-display) ""))
    (:candidates . ,(minibuffer-candidates))
    (:filtered-count . ,(length clawmacs::*minibuffer-filtered-items*))))

(defun selector-state ()
  `((:buffer-selector-active . ,clawmacs::*buffer-selector-active*)
    (:buffer-selector-index . ,clawmacs::*buffer-selector-index*)
    (:session-tree-selector-active
     . ,clawmacs::*session-tree-selector-active*)
    (:session-tree-selector-index
     . ,clawmacs::*session-tree-selector-index*)
    (:session-tree-selector-filter
     . ,(string-downcase
         (symbol-name clawmacs::*session-tree-selector-filter-mode*)))
    (:session-tree-selector-search
     . ,clawmacs::*session-tree-selector-search*)
    (:session-tree-selector-selected
     . ,(let ((item (clawmacs::session-tree-selector-current-item)))
          (or (and item (getf item :id)) "")))
    (:session-tree-selector-rows
     . ,(coerce
         (mapcar (lambda (item)
                   (clawmacs::format-session-tree-selector-line "" item 120))
                 clawmacs::*session-tree-selector-filtered-items*)
         'vector))
    (:model-selector-active . ,clawmacs::*model-selector-active*)
    (:model-selector-index . ,clawmacs::*model-selector-index*)
    (:think-selector-active . ,clawmacs::*think-selector-active*)
    (:think-selector-index . ,clawmacs::*think-selector-index*)))

(defun buffer-ring-state ()
  (let ((current (current-buffer)))
    (coerce
     (loop :for buffer :in *buffer-ring*
           :collect `((:name . ,(buffer-name buffer))
                      (:agent . ,(buffer-agent-name buffer))
                      (:status . ,(string-downcase
                                    (symbol-name (buffer-status buffer))))
                      (:message-count . ,(max 0 (1- (buffer-message-count buffer))))
                      (:current . ,(eq buffer current))))
     'vector)))

(defun skill-completion-selected-display ()
  (let ((items clawmacs::*skill-completion-filtered-items*)
        (index clawmacs::*skill-completion-selected-index*))
    (when (and items (<= 0 index) (< index (length items)))
      (minibuffer-item-display (nth index items)))))

(defun skill-completion-candidates ()
  (coerce (mapcar #'minibuffer-item-display
                  clawmacs::*skill-completion-filtered-items*)
          'vector))

(defun skill-completion-state ()
  `((:active . ,clawmacs::*skill-completion-active*)
    (:query . ,clawmacs::*skill-completion-query*)
    (:token . ,(or clawmacs::*skill-completion-token-text* ""))
    (:selected-index . ,clawmacs::*skill-completion-selected-index*)
    (:selected . ,(or (skill-completion-selected-display) ""))
    (:candidates . ,(skill-completion-candidates))
    (:filtered-count . ,(length clawmacs::*skill-completion-filtered-items*))))

(defun slash-completion-selected-display ()
  (let ((items clawmacs::*slash-completion-filtered-items*)
        (index clawmacs::*slash-completion-selected-index*))
    (when (and items (<= 0 index) (< index (length items)))
      (getf (nth index items) :display))))

(defun slash-completion-candidates ()
  (coerce (mapcar (lambda (item)
                    (or (getf item :display) ""))
                  clawmacs::*slash-completion-filtered-items*)
          'vector))

(defun slash-completion-state ()
  `((:active . ,clawmacs::*slash-completion-active*)
    (:query . ,clawmacs::*slash-completion-query*)
    (:token . ,(or clawmacs::*slash-completion-token-text* ""))
    (:selected-index . ,clawmacs::*slash-completion-selected-index*)
    (:selected . ,(or (slash-completion-selected-display) ""))
    (:candidates . ,(slash-completion-candidates))
    (:filtered-count . ,(length clawmacs::*slash-completion-filtered-items*))))

(defun approval-state (buffer)
  (when (buffer-approval-pending buffer)
    `((:pending . t)
      (:tool-name . ,(or (getf (buffer-approval-pending buffer) :tool-name)
                         ""))
      (:display . ,(truncate-string
                    (or (getf (buffer-approval-pending buffer) :display-expanded)
                        (getf (buffer-approval-pending buffer) :display-raw)
                        "")
                    2000)))))

(defun buffer-state (buffer)
  `((:name . ,(buffer-name buffer))
    (:agent . ,(buffer-agent-name buffer))
    (:kind . ,(string-downcase (symbol-name (buffer-kind buffer))))
    (:status . ,(string-downcase (symbol-name (buffer-status buffer))))
    (:message-count . ,(max 0 (1- (buffer-message-count buffer))))
    (:input . ,(message-text (buffer-input-message buffer)))
    (:input-point . ,(clawmacs::mcclim-message-point-absolute-offset
                      (buffer-input-message buffer)))
    (:scroll-offset . ,(buffer-scroll-offset buffer))
    (:show-tool-results . ,(buffer-show-tool-results-p buffer))
    (:show-reasoning . ,(buffer-show-reasoning-p buffer))
    (:show-metadata . ,(buffer-show-metadata-p buffer))
    (:modeline . ,(modeline-string buffer))
    (:who-line . ,(who-line-vector buffer))
    (:approval . ,(approval-state buffer))
    (:messages . ,(buffer-message-tail buffer))))

(defun render-state ()
  "Return the latest render snapshot recorded by the McCLIM display path."
  (let ((frame (and (boundp 'clawmacs::*clawmacs-frame*)
                    clawmacs::*clawmacs-frame*)))
    (or (and frame
             (clawmacs::frame-last-render-snapshot frame))
        '((:ready . nil)))))

(defun pointer-documentation-state ()
  "Return semantic hover-documentation state for the running McCLIM frame."
  (let ((frame (current-mcclim-frame)))
    (if (and frame (typep frame 'clawmacs::clawmacs-gui))
        (let ((text (or (ignore-errors
                          (clawmacs::frame-pointer-documentation-text frame))
                        "")))
          `((:active . ,(plusp (length text)))
            (:count . ,(if (plusp (length text)) 1 0))
            (:text . ,text)))
        '((:active . nil)
          (:count . 0)
          (:text . "")))))

(defun pane-viewport-region (pane)
  "Return PANE's viewport region, falling back to its sheet region."
  (handler-case
      (clim:window-viewport pane)
    (error ()
      (clim:sheet-region pane))))

(defun pane-geometry (frame pane-name)
  "Return pixel/grid geometry for PANE-NAME in FRAME, or NIL."
  (let ((pane (and frame (clim:find-pane-named frame pane-name))))
    (when pane
      (let* ((region (pane-viewport-region pane))
             (top-sheet (clim:frame-top-level-sheet frame))
             (delta (ignore-errors
                      (clim:sheet-delta-transformation pane top-sheet)))
             (char-w (max 1 (clawmacs::frame-char-width frame)))
             (char-h (max 1 (clawmacs::frame-char-height frame))))
        (clim:with-bounding-rectangle* (x1 y1 x2 y2) region
          (multiple-value-bind (tx1 ty1)
              (if delta
                  (clim:transform-position delta x1 y1)
                  (values x1 y1))
            (multiple-value-bind (tx2 ty2)
                (if delta
                    (clim:transform-position delta x2 y2)
                    (values x2 y2))
              (let* ((left (round (min tx1 tx2)))
                     (top (round (min ty1 ty2)))
                     (width (max 1 (round (abs (- tx2 tx1)))))
                     (height (max 1 (round (abs (- ty2 ty1))))))
                `((:x . ,left)
                  (:y . ,top)
                  (:pixel-width . ,width)
                  (:pixel-height . ,height)
                  (:cols . ,(max 1 (floor width char-w)))
                  (:rows . ,(max 1 (floor height char-h))))))))))))

(defun pane-state ()
  "Return geometry for the panes used by the McCLIM e2e harness."
  (let ((frame (current-mcclim-frame)))
    (if (and frame (typep frame 'clawmacs::clawmacs-gui))
        `((:main . ,(or (pane-geometry frame 'clawmacs::main-pane)
                        '((:x . 0)
                          (:y . 0)
                          (:pixel-width . 1)
                          (:pixel-height . 1)
                          (:cols . 1)
                          (:rows . 1))))
          (:compose . ,(or (pane-geometry frame 'clawmacs::compose-pane)
                           '((:x . 0)
                             (:y . 0)
                             (:pixel-width . 1)
                             (:pixel-height . 1)
                             (:cols . 1)
                             (:rows . 1))))
          (:input . ,(or (pane-geometry frame 'clawmacs::input-pane)
                         '((:x . 0)
                           (:y . 0)
                           (:pixel-width . 1)
                           (:pixel-height . 1)
                           (:cols . 1)
                           (:rows . 1)))))
        '((:main . ((:x . 0)
                    (:y . 0)
                    (:pixel-width . 1)
                    (:pixel-height . 1)
                    (:cols . 1)
                    (:rows . 1)))
          (:compose . ((:x . 0)
                       (:y . 0)
                       (:pixel-width . 1)
                       (:pixel-height . 1)
                       (:cols . 1)
                       (:rows . 1)))
          (:input . ((:x . 0)
                     (:y . 0)
                     (:pixel-width . 1)
                     (:pixel-height . 1)
                     (:cols . 1)
                     (:rows . 1)))))))

(defun window-state ()
  "Return the running McCLIM frame's logical window state."
  (let ((frame (current-mcclim-frame)))
    (if (and frame (typep frame 'clawmacs::clawmacs-gui))
        (let* ((tree (clawmacs::mcclim-ensure-window-tree frame))
               (selected-id (clawmacs::frame-selected-window-id frame))
               (windows (clawmacs:clawmacs-window-tree-windows tree)))
          `((:selected-id . ,(or selected-id -1))
            (:count . ,(length windows))
            (:windows
             . ,(coerce
                 (mapcar (lambda (window)
                           (let ((buffer
                                   (clawmacs:clawmacs-window-buffer window)))
                             `((:id . ,(clawmacs:clawmacs-window-id window))
                               (:buffer-name . ,(if buffer
                                                    (buffer-name buffer)
                                                    ""))
                               (:selected
                                . ,(eql selected-id
                                        (clawmacs:clawmacs-window-id
                                         window))))))
                         windows)
                 'vector))))
        '((:selected-id . -1)
          (:count . 0)
          (:windows . #())))))

(defun snapshot ()
  "Return a JSON-ready semantic snapshot of the running McCLIM session."
  (handler-case
      (with-current-mcclim-ui-state
        (let ((buffer (current-buffer)))
          `((:ready . ,(not (null buffer)))
            (:timestamp . ,(get-universal-time))
            (:buffer . ,(when buffer (buffer-state buffer)))
            (:buffers . ,(buffer-ring-state))
            (:windows . ,(window-state))
            (:panes . ,(pane-state))
            (:render . ,(render-state))
            (:pointer-documentation . ,(pointer-documentation-state))
            (:control-result . ,*last-control-result*)
            (:minibuffer . ,(minibuffer-state))
            (:slash-completion . ,(slash-completion-state))
            (:skill-completion . ,(skill-completion-state))
            (:selectors . ,(selector-state)))))
    (error (condition)
      `((:ready . nil)
        (:timestamp . ,(get-universal-time))
        (:error . ,(format nil "~A" condition))))))

(defun write-snapshot (&optional (directory (control-dir)))
  "Write the latest semantic snapshot into DIRECTORY/latest.json."
  (let* ((root (control-pathname directory))
         (target (merge-pathnames "latest.json" root))
         (temporary (merge-pathnames "latest.json.tmp" root))
         (json (cl-json:encode-json-to-string (snapshot))))
    (ensure-directories-exist target)
    (with-open-file (stream temporary
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string json stream)
      (terpri stream))
    (uiop:rename-file-overwriting-target temporary target)
    target))

(defun control-loop (directory interval)
  (loop
    (let ((command (read-control-command directory)))
      (when command
        (apply-control-command command)))
    (ignore-errors
      (write-snapshot directory))
    (sleep interval)))

(defun start-control-thread (&key (interval 0.2))
  "Start the background control writer used by the McCLIM e2e harness."
  (bt:with-lock-held (*control-thread-lock*)
    (unless (and *control-thread*
                 (bt:thread-alive-p *control-thread*))
      (let ((directory (control-dir)))
        (ensure-directories-exist
         (merge-pathnames "latest.json" (control-pathname directory)))
        (setf *control-thread*
              (bt:make-thread
               (lambda () (control-loop directory interval))
               :name "mcclim-e2e-control")))))
  t)
