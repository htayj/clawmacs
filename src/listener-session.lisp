(in-package :rplaca)

(defvar *listener-contexts-by-buffer* (make-hash-table :test #'eq))

(defun listener-context-for-buffer (buffer)
  "Return BUFFER's immutable listener context, creating a default one."
  (or (gethash buffer *listener-contexts-by-buffer*)
      (setf (gethash buffer *listener-contexts-by-buffer*)
            (make-listener-context))))

(defun legacy-listener-package-name (state)
  (let ((value (and (listp state) (cdr (assoc :package-name state)))))
    (if (and (stringp value) (find-package value))
        (package-name (find-package value))
        (listener-context-default-package-name))))

(defun legacy-listener-directory-stack (state)
  (let ((value (and (listp state) (cdr (assoc :directory-stack state)))))
    (if (or (listp value) (vectorp value))
        (loop :for item :in (coerce value 'list)
              :for directory := (and (or (stringp item) (pathnamep item))
                                     (ignore-errors
                                       (uiop:ensure-directory-pathname item)))
              :when directory
                :collect directory)
        nil)))

(defun restore-legacy-listener-context (buffer state)
  "Translate retired listener STATE without retaining REPL-only fields."
  (setf (gethash buffer *listener-contexts-by-buffer*)
        (make-listener-context
         :package-name (legacy-listener-package-name state)
         :directory-stack (legacy-listener-directory-stack state)))
  buffer)

(defun mark-legacy-listener-history-display-only (buffer)
  (loop :for message := (buffer-first-message buffer) :then (message-next message)
        :while (and message (not (eq message (buffer-input-message buffer))))
        :do (put-message-metadata message
                                  :ephemeral-display t
                                  :legacy-listener-history t)
            (setf (message-sender message) :system))
  buffer)

(defun migrate-retired-listener-buffer (buffer)
  "Convert a restored retired listener buffer into a chat conversation."
  (when (eq :listener (buffer-kind buffer))
    (listener-context-for-buffer buffer)
    (mark-legacy-listener-history-display-only buffer)
    (setf (buffer-kind buffer) :chat
          (buffer-major-mode buffer) "chat")
    (when (boundp '*listener-buffer-states*)
      (remhash buffer *listener-buffer-states*)))
  buffer)

(defun migrate-retired-listener-session-after-load (buffer session-name)
  (declare (ignore session-name))
  (migrate-retired-listener-buffer buffer))

(defun listener-session-label-for-buffer (buffer)
  (let ((session (and buffer (buffer-session buffer))))
    (cond
      (session (session-display-name-or-name session))
      (buffer (buffer-name buffer))
      (t "rplaca"))))

(defun listener-saved-session-records ()
  (copy-list (or (list-saved-session-records) nil)))

(clim:define-presentation-type saved-listener-session ()
  :inherit-from t)

(clim:define-presentation-method clim:presentation-typep
    (object (type saved-listener-session))
  (and (listp object)
       (stringp (getf object :session-name))))

(clim:define-presentation-method clim:present
    (object (type saved-listener-session) stream
            (view clim:textual-view) &key)
  (declare (ignore view))
  (write-string (session-selector-display-text object) stream))

(clim:define-presentation-method clim:accept
    ((type saved-listener-session) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (clim:completing-from-suggestions (stream)
    (dolist (record (listener-saved-session-records))
      (clim:suggest (session-selector-display-text record) record))))

(defun display-listener-session-list (records stream)
  "Render saved session RECORDS as resumable presentations."
  (if records
      (dolist (record records)
        (clim:with-output-as-presentation
            (stream record 'saved-listener-session)
          (clim:present record 'saved-listener-session :stream stream))
        (terpri stream))
      (write-line "No saved sessions available." stream))
  records)

(defun listener-session-name-available-p (name)
  (and (null (find-buffer-by-name name))
       (not (member name (list-saved-sessions) :test #'string=))))

(defun next-listener-session-name ()
  (loop :for name := (next-buffer-name)
        :when (listener-session-name-available-p name)
          :return name))

(defun listener-activate-session-buffer (frame buffer)
  "Install BUFFER and its context as FRAME's active listener session."
  (unless (member buffer *buffer-ring* :test #'eq)
    (initialize-buffer-display-defaults buffer)
    (add-buffer-to-ring buffer))
  (switch-to-buffer buffer)
  (setf (rplaca-listener-conversation-buffer frame) buffer
        (rplaca-listener-context frame) (listener-context-for-buffer buffer)
        (rplaca-listener-pending-session-name frame)
        (and (buffer-session buffer) (session-name (buffer-session buffer)))
        (rplaca-listener-session-label frame)
        (listener-session-label-for-buffer buffer))
  buffer)

(defun listener-create-session (frame)
  (let* ((source (rplaca-listener-conversation-buffer frame))
         (name (next-listener-session-name))
         (buffer (make-buffer
                  name
                  :agent-name (buffer-agent-name source)
                  :working-directory (buffer-working-directory source)
                  :session-persistence-mode :persistent)))
    (ensure-buffer-session buffer)
    (listener-activate-session-buffer frame buffer)))

(defun listener-resume-session (frame record)
  (let* ((name (if (stringp record) record (getf record :session-name)))
         (existing (find-open-session-buffer name))
         (buffer (or existing (load-session name))))
    (when buffer
      (listener-activate-session-buffer frame buffer))))

(register-buffer-type
 :listener
 :restore-state-function 'restore-legacy-listener-context)

(add-hook '*after-session-load-hook*
          #'migrate-retired-listener-session-after-load
          :append t)
