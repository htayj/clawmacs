(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Prefix Processing
;;; --------------------------------------------------------------------------

(defvar *prefix-handlers* nil
  "Alist mapping prefix strings to handler functions.
Each handler is called as (funcall handler buffer remaining-text) where
REMAINING-TEXT is the user's input with the prefix stripped.
Handlers should insert their results as system messages in the buffer.

Example entry: (\"!\" . shell-prefix-handler)")

(defun interactive-shell-result-text (command result)
  "Return a bounded visible transcript message for shell RESULT."
  (let ((parts nil))
    (when (plusp (length (or (getf result :stderr) "")))
      (push (getf result :stderr) parts))
    (when (plusp (length (or (getf result :stdout) "")))
      (push (getf result :stdout) parts))
    (when (getf result :stderr-truncated-p)
      (push "[stderr truncated]" parts))
    (when (getf result :stdout-truncated-p)
      (push "[stdout truncated]" parts))
    (let ((combined (and parts
                         (format nil "~{~A~^~%~}" (nreverse parts)))))
      (cond
        ((getf result :cancelled-p)
         (format nil "$ ~A  [cancelled]~@[~%~A~]" command combined))
        ((getf result :timed-out-p)
         (format nil "$ ~A  [timed out]~@[~%~A~]" command combined))
        ((zerop (or (getf result :exit-code) 1))
         (format nil "$ ~A~@[~%~A~]" command combined))
        (t
         (format nil "$ ~A  [exit ~A]~@[~%~A~]"
                 command (or (getf result :exit-code) "unknown") combined))))))

(defun shell-prefix-handler (buf command-text)
  "Start COMMAND-TEXT off the CLIM frame and return its managed operation."
  (let* ((trimmed (string-trim '(#\Space #\Tab) command-text))
         (full-input (concatenate 'string "!" command-text))
         (working-dir (or (buffer-working-directory buf) (truename "."))))
    (when (blank-string-p trimmed)
      (buffer-insert-system-message buf "[Shell command is empty.]")
      (return-from shell-prefix-handler nil))
    (multiple-value-bind (operation refusal)
        (start-interactive-buffer-operation
         buf
         :shell
         (lambda (snapshot operation)
           (declare (ignore snapshot operation))
           (run-interactive-subprocess trimmed :directory working-dir))
         (lambda (buffer operation result error-text)
           (declare (ignore operation))
           (cond
             (error-text
              (setf (buffer-status buffer) :error)
              (buffer-insert-system-message
               buffer
               (format nil "$ ~A~%[Shell error: ~A]" trimmed error-text)))
             (t
              (setf (buffer-status buffer) :idle)
              (buffer-insert-system-message
               buffer (interactive-shell-result-text trimmed result))))
           (run-hook-with-args '*after-send-message-hook*
                               buffer full-input result))
         :cancel-function
         (lambda (buffer operation)
           (declare (ignore operation))
           (buffer-insert-system-message
            buffer
            (interactive-shell-result-text
             trimmed (list :cancelled-p t)))
           (run-hook-with-args '*after-send-message-hook*
                               buffer full-input :cancelled))
         :payload (list :command trimmed :input full-input)
         :status :shell-running)
      (when refusal
        (setf (buffer-status buf) :idle)
        (buffer-insert-system-message
         buf (format nil "[Shell command refused: ~A]" refusal)))
      operation)))

(defun find-prefix-handler (text)
  "Return (prefix . handler) if TEXT starts with a registered prefix, or NIL.
Longer prefixes are checked first to support prefix hierarchies."
  (let ((sorted (sort (copy-list *prefix-handlers*) #'>
                      :key (lambda (entry) (length (car entry))))))
    (dolist (entry sorted)
      (let ((prefix (car entry)))
        (when (and (<= (length prefix) (length text))
                   (string= prefix text :end2 (length prefix)))
          (return entry))))))

(defun process-prefix-command (buf input-text)
  "Check if INPUT-TEXT starts with a registered prefix.
Return values HANDLED-P and the exact handler result."
  (let ((entry (find-prefix-handler input-text)))
    (when entry
      (let* ((prefix (car entry))
             (handler (cdr entry))
             (remaining (subseq input-text (length prefix))))
        (values t (funcall handler buf remaining))))))

(setf *prefix-handlers*
      (acons "!" #'shell-prefix-handler
             (remove "!" *prefix-handlers* :key #'car :test #'string=)))
