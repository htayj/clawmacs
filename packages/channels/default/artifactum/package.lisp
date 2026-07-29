(in-package :rplaca)

(defun artifactum-current-buffer ()
  "Return the current tool buffer, or the current UI buffer."
  (or *current-tool-buffer*
      (current-buffer)))

(defun artifactum-record-metadata (record &key attachment-p)
  "Return message metadata for RECORD."
  `((:artifact-id . ,(getf record :id))
    (:artifact-kind . ,(getf record :kind))
    (:artifact-name . ,(getf record :name))
    (:artifact-path . ,(getf record :path))
    (:mime-type . ,(getf record :mime-type))
    (:attachment-p . ,attachment-p)))

(defun artifactum-session-records-sorted (buffer)
  "Return BUFFER artifact records newest first."
  (sort (copy-list (artifactum-session-records buffer))
        #'>
        :key (lambda (record)
               (getf record :updated-at 0))))

(defun artifactum-record-items (buffer)
  "Return minibuffer/help items for BUFFER's artifact store."
  (mapcar (lambda (record)
            (list :record record
                  :display (artifactum-record-summary-line record)
                  :match-text (format nil "~A ~A ~A ~A"
                                      (getf record :id)
                                      (getf record :kind)
                                      (getf record :name)
                                      (getf record :mime-type))))
          (artifactum-session-records-sorted buffer)))

(defun artifactum-find-open-buffer (artifact-id)
  "Return an open artifact buffer for ARTIFACT-ID, or NIL."
  (find-if (lambda (buffer)
             (and (eq (buffer-kind buffer) :artifact)
                  (let ((msg (message-prev (buffer-input-message buffer))))
                    (and msg
                         (string= artifact-id
                                  (or (message-metadata-value
                                       (message-metadata msg)
                                       :artifact-id)
                                      ""))))))
           *buffer-ring*))

(defun artifactum-artifact-buffer-name (record)
  "Return the standard buffer name for RECORD."
  (format nil "*artifact:~A*" (getf record :name)))

(defun artifactum-open-record-buffer (record)
  "Show RECORD in a dedicated artifact buffer."
  (let* ((artifact-id (getf record :id))
         (existing (artifactum-find-open-buffer artifact-id)))
    (if existing
        (switch-to-buffer existing)
        (let ((buffer (make-buffer (artifactum-artifact-buffer-name record)
                                   :agent-name "artifactum"
                                   :kind :artifact
                                   :working-directory
                                    (uiop:pathname-directory-pathname
                                    (pathname (getf record :path)))
                                   :session-persistence-mode :ephemeral)))
          (initialize-buffer-display-defaults buffer)
          (setf (buffer-major-mode buffer) "artifact")
          (artifactum-artifact-buffer-open-record buffer record)
          (add-buffer-to-ring buffer)
          (switch-to-buffer buffer)))))

(defun artifactum-note-record (buffer record &key context-p)
  "Insert RECORD into BUFFER as a context or system message."
  (let ((text (artifactum-reference-text record :context-p context-p))
        (metadata (artifactum-record-metadata record :attachment-p context-p)))
    (if context-p
        (buffer-insert-context-message buffer text :metadata metadata)
        (buffer-insert-system-message buffer text :metadata metadata))))

(defun artifactum-list-artifacts-command (buffer)
  "Show BUFFER's artifact records in a help buffer."
  (let ((records (artifactum-session-records-sorted buffer)))
    (if records
        (switch-to-buffer
         (make-help-buffer
          "*help:artifactum*"
          (format nil "Artifacts~%=========~%~%~{~A~%~}"
                  (mapcar #'artifactum-record-summary-line records))))
        (buffer-insert-system-message buffer "[No artifacts in the current session.]"))))
(defcommand artifactum-list-artifacts-command)

(defun artifactum-open-artifact-command (buffer)
  "Open a session artifact in a dedicated artifact buffer."
  (let ((items (artifactum-record-items buffer)))
    (if items
        (progn
          (minibuffer-activate
           "Artifact"
           items
           (lambda (item)
             (artifactum-open-record-buffer (getf item :record))))
          (preselect-minibuffer-active-item items))
        (buffer-insert-system-message buffer "[No artifacts in the current session.]"))))
(defcommand artifactum-open-artifact-command)

(defun artifactum-attach-file-command (buffer path)
  "Copy PATH into BUFFER's session artifact store and insert a context message."
  (let ((record (artifactum-create-from-file buffer path :kind "attachment"
                                             :author "user")))
    (artifactum-note-record buffer record :context-p t)
    record))
(defcommand artifactum-attach-file-command
  :prompts ((path :prompt "Attachment file path"))
  :docstring "Attach a local file to the current session as durable context.")

(defun artifactum-tool-string-field (args names)
  "Return the first non-blank field from ARGS for NAMES."
  (loop :for name :in names
        :for value := (cdr (assoc name args :test #'equal))
        :for normalized := (artifactum-normalize-string value)
        :when normalized
          :return normalized))

(defun artifactum-tool-buffer ()
  "Return the current buffer for artifactum tools."
  (or (artifactum-current-buffer)
      (error "Artifactum tools require a current buffer.")))

(defun artifactum-list-tool (_args)
  "Return the current buffer's durable artifact records."
  (declare (ignore _args))
  (lisp-data-string
   (coerce (mapcar (lambda (record)
                     (artifactum-read-record-data
                      (artifactum-tool-buffer)
                      (getf record :id)
                      :include-content-p nil))
                   (artifactum-session-records-sorted
                    (artifactum-tool-buffer)))
           'vector)))

(deftool artifactum-list-tool
  :name "artifactum_list"
  :description "List durable session attachments and artifacts for the current buffer."
  :call-style :raw-args
  :args ())

(defun artifactum-read-tool (args)
  "Read one durable artifact by id."
  (let ((artifact-id (artifactum-tool-string-field args '(:id "id"))))
    (unless artifact-id
      (error "artifactum_read requires :id."))
    (lisp-data-string
     (artifactum-read-record-data (artifactum-tool-buffer) artifact-id))))

(deftool artifactum-read-tool
  :name "artifactum_read"
  :description "Read one durable artifact by id, including text content when available."
  :call-style :raw-args
  :args ((id :type "string"
             :description "Artifact id returned by artifactum_list or attachment context.")))

(defun artifactum-create-tool (args)
  "Create one durable artifact in the current buffer's session."
  (let* ((name (artifactum-tool-string-field args '(:name "name")))
         (content (artifactum-tool-string-field args '(:content "content")))
         (source-path (artifactum-tool-string-field args '(:source-path "source_path")))
         (mime-type (artifactum-tool-string-field args '(:mime-type "mime_type")))
         (buffer (artifactum-tool-buffer))
         (record (cond
                   (source-path
                    (artifactum-create-from-file buffer source-path
                                                 :name name
                                                 :mime-type mime-type
                                                 :kind "artifact"
                                                 :author "agent"))
                   (content
                    (artifactum-create-from-content buffer name content
                                                    :mime-type mime-type
                                                    :kind "artifact"
                                                    :author "agent"))
                   (t
                    (error "artifactum_create requires :content or :source_path.")))))
    (artifactum-note-record buffer record)
    (lisp-data-string (artifactum-read-record-data buffer (getf record :id)))))

(deftool artifactum-create-tool
  :name "artifactum_create"
  :description "Create one durable artifact for the current session from text content or a local file path."
  :call-style :raw-args
  :args ((name :type "string" :required nil
               :description "Artifact file name, such as report.md or chart.json.")
         (content :type "string" :required nil
                  :description "Text content to write into the artifact.")
         (mime_type :type "string" :required nil
                    :description "Optional explicit MIME type.")
         (source_path :type "string" :required nil
                      :description "Local file path to copy into artifact storage.")))

(defun artifactum-update-tool (args)
  "Update one durable artifact."
  (let* ((artifact-id (artifactum-tool-string-field args '(:id "id")))
         (name (artifactum-tool-string-field args '(:name "name")))
         (content (artifactum-tool-string-field args '(:content "content")))
         (source-path (artifactum-tool-string-field args '(:source-path "source_path")))
         (mime-type (artifactum-tool-string-field args '(:mime-type "mime_type")))
         (buffer (artifactum-tool-buffer))
         (record (artifactum-update-record buffer artifact-id
                                           :name name
                                           :content content
                                           :source-path source-path
                                           :mime-type mime-type)))
    (artifactum-note-record buffer record)
    (lisp-data-string (artifactum-read-record-data buffer artifact-id))))

(deftool artifactum-update-tool
  :name "artifactum_update"
  :description "Update one durable artifact in the current session."
  :call-style :raw-args
  :args ((id :type "string"
             :description "Artifact id to update.")
         (name :type "string" :required nil
               :description "Optional new artifact file name.")
         (content :type "string" :required nil
                  :description "Replacement text content.")
         (mime_type :type "string" :required nil
                    :description "Optional replacement MIME type.")
         (source_path :type "string" :required nil
                      :description "Replacement local file path to copy.")))

(define-buffer-type :artifact
  :description "Read-only buffer showing one durable session artifact."
  :major-mode "artifact"
  :serialize-state-function 'artifactum-buffer-state-serializer
  :restore-state-function 'artifactum-buffer-state-restorer)

(register-package-prompt-section
 "artifactum"
 "## Attachments and artifacts with artifactum

- User-attached files appear in context messages with an `artifact-id`.
- Use `artifactum_list` to see durable files in the current session.
- Use `artifactum_read` to inspect a full artifact or attachment when the
  preview in the transcript is not enough.
- Use `artifactum_create` or `artifactum_update` instead of pasting large HTML,
  SVG, Markdown, JSON, or other generated outputs into the chat."
 :title "Attachments and artifacts with artifactum"
 :package "artifactum")
