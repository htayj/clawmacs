(defpackage :clawmacs/mcclim-e2e
  (:use :cl :clawmacs)
  (:export #:snapshot
           #:write-snapshot
           #:start-control-thread))

(in-package :clawmacs/mcclim-e2e)

(defvar *control-thread* nil)
(defvar *control-thread-lock* (bt:make-lock "mcclim-e2e-control-thread"))

(defun control-dir ()
  (or (uiop:getenv "CLAWMACS_MCCLIM_E2E_CONTROL_DIR")
      (error "CLAWMACS_MCCLIM_E2E_CONTROL_DIR is not set.")))

(defun control-pathname (directory)
  (uiop:ensure-directory-pathname directory))

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
    (:scroll-offset . ,(buffer-scroll-offset buffer))
    (:show-tool-results . ,(buffer-show-tool-results-p buffer))
    (:show-reasoning . ,(buffer-show-reasoning-p buffer))
    (:show-metadata . ,(buffer-show-metadata-p buffer))
    (:modeline . ,(modeline-string buffer))
    (:who-line . ,(who-line-vector buffer))
    (:approval . ,(approval-state buffer))
    (:messages . ,(buffer-message-tail buffer))))

(defun snapshot ()
  "Return a JSON-ready semantic snapshot of the running McCLIM session."
  (handler-case
      (let ((buffer (current-buffer)))
        `((:ready . ,(not (null buffer)))
          (:timestamp . ,(get-universal-time))
          (:buffer . ,(when buffer (buffer-state buffer)))
          (:buffers . ,(buffer-ring-state))
          (:minibuffer . ,(minibuffer-state))
          (:skill-completion . ,(skill-completion-state))
          (:selectors . ,(selector-state))))
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
