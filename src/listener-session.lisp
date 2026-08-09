(in-package :rplaca)

(defvar *listener-contexts-by-buffer* (make-hash-table :test #'eq))

(defvar *listener-buffer-states* (make-hash-table :test #'eq))

(defstruct (listener-state
            (:constructor make-listener-state
                (&key (package-name "CL-USER")
                      directory-stack
                      last-values
                      command-history)))
  (package-name "CL-USER" :type string)
  (directory-stack nil :type list)
  (last-values nil :type list)
  (command-history nil :type list))

(defun listener-default-package-name ()
  (let ((name (and (boundp '*lisp-eval-default-package*)
                   *lisp-eval-default-package*)))
    (if (and (stringp name)
             (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         name))))
        (string-upcase name)
        "CL-USER")))

(defun listener-buffer-p (buffer)
  (and buffer (eq (buffer-kind buffer) :listener)))

(defun listener-buffer-state (buffer)
  (unless (listener-buffer-p buffer)
    (error "Not a retired listener buffer: ~A"
           (and buffer (buffer-name buffer))))
  (or (gethash buffer *listener-buffer-states*)
      (setf (gethash buffer *listener-buffer-states*)
            (make-listener-state
             :package-name (listener-default-package-name)))))

(defun listener-safe-value-string (value)
  (handler-case
      (let ((*print-length* 100)
            (*print-level* 8)
            (*print-circle* t)
            (*print-pretty* nil)
            (*print-readably* nil)
            (*print-escape* t))
        (prin1-to-string value))
    (error () "#<unprintable value>")))

(defun listener-normalize-package-name (value)
  (let* ((candidate (and value (string-upcase (string value))))
         (package (and candidate (find-package candidate))))
    (package-name (or package (find-package (listener-default-package-name))))))

(defun listener-serialize-last-values (values)
  (coerce (mapcar #'listener-safe-value-string values) 'vector))

(defun listener-restore-last-values (items)
  (loop :for item :in (coerce (or items #()) 'list)
        :collect (if (stringp item)
                     (handler-case
                         (let ((*read-eval* nil)
                               (*package* (find-package :cl-user)))
                           (read-from-string item))
                       (error () item))
                     item)))

(defun listener-serialize-buffer-state (buffer)
  "Serialize the retired listener state for snapshot compatibility."
  (let ((state (listener-buffer-state buffer)))
    `((:package-name . ,(listener-state-package-name state))
      (:directory-stack
       . ,(coerce (mapcar #'namestring
                          (listener-state-directory-stack state))
                  'vector))
      (:last-values
       . ,(listener-serialize-last-values
           (listener-state-last-values state)))
      (:command-history
       . ,(coerce (copy-list (listener-state-command-history state))
                  'vector)))))

(defun listener-restore-buffer-state (buffer persisted-state)
  "Restore the retired listener state long enough to migrate its snapshot."
  (let ((state (listener-buffer-state buffer)))
    (setf (listener-state-package-name state)
          (listener-normalize-package-name
           (cdr (assoc :package-name persisted-state)))
          (listener-state-directory-stack state)
          (loop :for item :in (coerce (or (cdr (assoc :directory-stack
                                                      persisted-state))
                                          #())
                                      'list)
                :when item
                  :collect (uiop:ensure-directory-pathname
                            (if (pathnamep item) item (pathname item))))
          (listener-state-last-values state)
          (listener-restore-last-values
           (cdr (assoc :last-values persisted-state)))
          (listener-state-command-history state)
          (loop :for item :in (coerce (or (cdr (assoc :command-history
                                                      persisted-state))
                                          #())
                                      'list)
                :when (stringp item)
                  :collect item)))
  buffer)

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

(defun restore-retired-listener-buffer-state (buffer state)
  (ignore-errors (listener-restore-buffer-state buffer state))
  (restore-legacy-listener-context buffer state))

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
    (remhash buffer *listener-buffer-states*))
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

(defstruct (session-history-item
             (:constructor make-session-history-item (kind text source)))
  kind
  (text "" :type string)
  source)

(clim:define-presentation-type session-history-item ()
  :inherit-from t)

(clim:define-presentation-method clim:present
    (object (type session-history-item) stream
            (view clim:textual-view) &key)
  (declare (ignore view))
  (write-string (session-history-item-text object) stream))

(defun listener-history-metadata-plist (metadata)
  (loop :for (key . value) :in metadata
        :append (list key value)))

(defun listener-history-list-value (value)
  (if (vectorp value) (coerce value 'list) (copy-list value)))

(defun listener-history-assistant-status (metadata)
  (let ((status (or (message-metadata-value metadata :status)
                    (message-metadata-value metadata :stop-reason))))
    (cond
      ((and status (search "cancel" (string-downcase (princ-to-string status))))
       :cancelled)
      ((and status (search "error" (string-downcase (princ-to-string status))))
       :error)
      (t :complete))))

(defun listener-history-assistant-sender-p (sender)
  (not (member sender
               '(:user :tool-result :compaction-summary :branch-summary
                 :context :system :listener)
               :test #'eq)))

(defun listener-history-event-raw-content (event)
  (let ((raw-content (session-alist-value event :raw-content)))
    (and raw-content (normalize-legacy-raw-content raw-content))))

(defun listener-history-event-assistant-turn (event)
  (let* ((raw-content (listener-history-event-raw-content event))
         (metadata (copy-tree (or (session-alist-value event :metadata) nil)))
         (raw-text (and raw-content (content-text-blocks raw-content)))
         (text (if (and raw-text (not (blank-string-p raw-text)))
                   raw-text
                   (or (session-alist-value event :text) ""))))
    (make-assistant-turn
     :primary-text text
     :tool-uses (mapcar #'listener-tool-use-plist
                        (content-tool-use-blocks raw-content))
     :reasoning (content-reasoning-blocks raw-content)
     :metadata (listener-history-metadata-plist metadata)
     :artifact-refs
     (listener-history-list-value
      (or (message-metadata-value metadata :artifact-refs) nil))
     :media-refs
     (listener-history-list-value
      (or (message-metadata-value metadata :media-refs) nil))
     :inspect-payload raw-content
     :status (listener-history-assistant-status metadata))))

(defun listener-history-simple-text (kind source text)
  (cond
    ((eq kind :compaction)
     (format nil "[Compaction~@[: ~A~]]~@[ ~A~]"
             (session-alist-value source :reason)
             (session-alist-value source :summary)))
    ((eq kind :branch)
     (format nil "[Branch] ~A" (or (session-alist-value source :summary) "")))
    ((eq kind :user) (format nil "User: ~A" text))
    ((eq kind :tool) (format nil "Tool: ~A" text))
    ((eq kind :listener-output) (format nil "Listener: ~A" text))
    ((eq kind :error) (format nil "[Error] ~A" text))
    ((eq kind :cancelled) (format nil "[Cancelled] ~A" text))
    (t text)))

(defun emit-listener-history-item (frame kind text source)
  (let* ((stream (clim:frame-standard-output frame))
         (display-text (listener-history-simple-text kind source text))
         (item (make-session-history-item kind display-text source)))
    (clim:with-output-as-presentation
        (stream item 'session-history-item :single-box t)
      (write-string display-text stream))
    (terpri stream)
    item))

(defun listener-history-message-kind (sender metadata)
  (cond
    ((message-metadata-value metadata :legacy-listener-history)
     :listener-output)
    ((eq sender :user) :user)
    ((eq sender :tool-result) :tool)
    ((and (message-metadata-value metadata :status)
          (search "cancel"
                  (string-downcase
                   (princ-to-string
                    (message-metadata-value metadata :status)))))
     :cancelled)
    ((and (message-metadata-value metadata :status)
          (search "error"
                  (string-downcase
                   (princ-to-string
                    (message-metadata-value metadata :status)))))
     :error)
    (t :message)))

(defun emit-listener-history-message-event (frame event)
  (let* ((sender-name (or (session-alist-value event :sender) "SYSTEM"))
         (sender (intern (string-upcase sender-name) :keyword))
         (metadata (copy-tree (or (session-alist-value event :metadata) nil)))
         (text (or (session-alist-value event :text) "")))
    (if (listener-history-assistant-sender-p sender)
        (let ((turn (listener-history-event-assistant-turn event)))
          (setf (rplaca-listener-pending-assistant-turn frame) turn)
          (emit-listener-assistant-turn frame turn))
        (emit-listener-history-item
         frame (listener-history-message-kind sender metadata) text event))))

(defun emit-listener-history-branch-event (frame event)
  (let ((kind (session-event-kind event)))
    (cond
      ((string= kind "message")
       (emit-listener-history-message-event frame event))
      ((string= kind "compaction")
       (emit-listener-history-item frame :compaction "" event))
      ((string= kind "branch-summary")
       (emit-listener-history-item frame :branch "" event)))))

(defun listener-legacy-history-events (buffer)
  (loop :for message := (buffer-first-message buffer) :then (message-next message)
        :while (and message (not (eq message (buffer-input-message buffer))))
        :for metadata := (message-metadata message)
        :when (and metadata
                   (message-metadata-value metadata :legacy-listener-history))
          :collect `((:event . "message")
                     (:sender . "LISTENER")
                     (:text . ,(message-text message))
                     (:timestamp . ,(message-timestamp message))
                     (:read-only-p . t)
                     (:metadata . ,metadata))))

(defun listener-session-replay-key (buffer)
  (let ((session (buffer-session buffer)))
    (list (and session (session-id session))
          (and session (session-name session))
          (and session (session-effective-leaf-id session)))))

(defun render-session-history-inline (frame &optional buffer)
  "Emit BUFFER's restored active branch once into FRAME's interactor."
  (let* ((buffer (or buffer (rplaca-listener-conversation-buffer frame)))
         (key (listener-session-replay-key buffer))
         (rendered (rplaca-listener-replayed-session-branches frame)))
    (unless (gethash key rendered)
      (setf (rplaca-listener-selected-detail frame) nil)
      (let* ((session (buffer-session buffer))
             (branch-events (and session (session-branch-events session)))
             (events (or branch-events (listener-legacy-history-events buffer))))
        (dolist (event events)
          (emit-listener-history-branch-event frame event)))
      (setf (gethash key rendered) t))
    buffer))

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
      (listener-activate-session-buffer frame buffer)
      (render-session-history-inline frame buffer))))

(add-hook '*after-session-load-hook*
          #'migrate-retired-listener-session-after-load
          :append t)
