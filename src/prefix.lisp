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

(defun shell-prefix-handler (buf command-text)
  "Execute COMMAND-TEXT as a shell command and insert the output as a system message.
The command runs in the buffer's working directory."
  (let* ((trimmed (string-trim '(#\Space #\Tab) command-text))
         (working-dir (or (buffer-working-directory buf) "/workspace"))
         (output (handler-case
                     (multiple-value-bind (stdout stderr exit-code)
                         (uiop:run-program
                          (list "/bin/sh" "-c" trimmed)
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t)
                          :ignore-error-status t
                          :directory working-dir)
                       (let ((parts nil))
                         (when (and stderr (plusp (length stderr)))
                           (push stderr parts))
                         (when (and stdout (plusp (length stdout)))
                           (push stdout parts))
                         (let ((combined (format nil "~{~A~^~%~}" (nreverse parts))))
                           (if (zerop exit-code)
                               (format nil "$ ~A~%~A" trimmed combined)
                               (format nil "$ ~A  [exit ~D]~%~A"
                                       trimmed exit-code combined)))))
                   (error (e)
                     (format nil "$ ~A~%[Shell error: ~A]" trimmed e)))))
    (buffer-insert-system-message buf output)))

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
If so, call the handler and return T. Otherwise return NIL."
  (let ((entry (find-prefix-handler input-text)))
    (when entry
      (let* ((prefix (car entry))
             (handler (cdr entry))
             (remaining (subseq input-text (length prefix))))
        (funcall handler buf remaining)
        t))))

(setf *prefix-handlers*
      (acons "!" #'shell-prefix-handler
             (remove "!" *prefix-handlers* :key #'car :test #'string=)))
