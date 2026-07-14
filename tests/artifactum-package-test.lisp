(in-package :clawmacs/tests)

(in-suite artifactum-package-suite)

(defmacro with-artifactum-test-state (&body body)
  `(with-package-state-override ((default-package-test-channels))
     (let* ((*sessions-dir* (temp-session-test-directory "artifactum"))
            (clawmacs::*tool-working-directory* *sessions-dir*)
            (clawmacs::*buffer-ring* nil)
            (clawmacs::*buffer-counter* 0)
            (clawmacs::*default-keymap* nil)
            (clawmacs::*scratch-keymap* nil)
            (clawmacs::*file-keymap* nil)
            (clawmacs::*command-table* (make-hash-table :test #'eq))
            (clawmacs::*extended-docs* (make-hash-table :test #'eq))
            (clawmacs::*agent-tool-metadata-table*
             (make-hash-table :test #'eq))
            (clawmacs::*agent-tool-name-table*
             (make-hash-table :test #'equal))
            (clawmacs::*slash-command-table* (make-hash-table :test #'equal))
            (clawmacs::*buffer-type-registry*
             (clawmacs::make-buffer-type-registry)))
       (set-package-enablement-scope "artifactum" :global)
       (load-active-packages)
       (clawmacs::init-default-keymap)
       ,@body)))

(defun make-artifactum-test-buffer (label)
  "Return a persistent chat buffer for artifactum tests."
  (let ((root (merge-pathnames
               (format nil "~A/" label)
               clawmacs::*tool-working-directory*)))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (let ((buffer (clawmacs::make-chat-buffer label :working-directory root)))
      (setf (clawmacs::buffer-keymap buffer) clawmacs::*default-keymap*)
      (add-buffer-to-ring buffer)
      (switch-to-buffer buffer)
      buffer)))

(defun artifactum-finalized-messages (buffer)
  "Return BUFFER's finalized messages in chronological order."
  (loop :for msg := (buffer-first-message buffer) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buffer))))
        :collect msg))

(defun artifactum-package-tool-result (tool-name args)
  "Execute TOOL-NAME with ARGS and decode its Lisp data result."
  (nth-value 0
    (clawmacs::lisp-data-read
     (clawmacs:execute-tool tool-name args))))

(test artifactum-package-registers-tools-buffer-type-and-commands
  "Artifactum loads its tools, prompt section, commands, and buffer type."
  (with-artifactum-test-state
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (mapcar (lambda (tool) (cdr (assoc :name tool))) tools))
           (prompt (render-package-prompt-sections))
           (commands (list-available-commands))
           (buffer-type (find-buffer-type :artifact)))
      (is (member "artifactum_list" tool-names :test #'string=))
      (is (member "artifactum_read" tool-names :test #'string=))
      (is (member "artifactum_create" tool-names :test #'string=))
      (is (member "artifactum_update" tool-names :test #'string=))
      (is (search "Attachments and artifacts with artifactum" prompt))
      (is (member 'clawmacs::artifactum-attach-file-command commands :test #'eq))
      (is (member 'clawmacs::artifactum-open-artifact-command commands :test #'eq))
      (is (member 'clawmacs::artifactum-list-artifacts-command commands :test #'eq))
      (is (not (null buffer-type)))
      (is (string= "artifactum" (buffer-type-package buffer-type))))))

(test artifactum-attach-command-copies-files-and-opens-ephemeral-artifact-buffers
  "User attachment ingestion copies files into the session store and opens artifact viewers."
  (with-artifactum-test-state
    (let* ((buffer (make-artifactum-test-buffer "attach"))
           (source (merge-pathnames "notes.md" (buffer-working-directory buffer))))
      (with-open-file (stream source
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string "# Notes\n\nArtifactum preview text." stream))
      (let* ((record (clawmacs::artifactum-attach-file-command
                      buffer
                      (namestring source)))
             (records (clawmacs::artifactum-session-records buffer))
             (message (car (last (artifactum-finalized-messages buffer)))))
        (is (= 1 (length records)))
        (is (string= "attachment" (getf record :kind)))
        (is (search "Artifactum preview text" (or (getf record :preview) "")))
        (is (eq :context (message-sender message)))
        (is (string= (getf record :id)
                     (clawmacs::message-metadata-value
                      (message-metadata message)
                      :artifact-id)))
        (is (probe-file (pathname (getf record :path))))
        (clawmacs::artifactum-open-record-buffer record)
        (let ((artifact-buffer (current-buffer))
              (artifact-message (car (artifactum-finalized-messages
                                      (current-buffer)))))
          (is (eq :artifact (buffer-kind artifact-buffer)))
          (is (eq :ephemeral
                  (buffer-session-persistence-mode artifact-buffer)))
          (is (search "Content:" (message-text artifact-message)))
          (is (search "Artifactum preview text"
                      (message-text artifact-message))))))))

(test artifactum-tools-manage-durable-records-and-export-visible-references
  "Artifactum tools create, update, list, and read records while exporting visible references."
  (with-artifactum-test-state
    (let* ((buffer (make-artifactum-test-buffer "tools"))
           (export-path (merge-pathnames "artifactum-export.html" *sessions-dir*))
           (*current-tool-buffer* buffer)
           (*current-caller* :agent))
      (let* ((created (artifactum-package-tool-result
                       "artifactum_create"
                       '((:name . "report.json")
                         (:content . "{\"ok\":true}")
                         (:mime_type . "application/json"))))
             (record (first (clawmacs::artifactum-session-records buffer)))
             (artifact-id (getf record :id)))
        (is (string= "report.json" (getf created :name)))
        (is (string= artifact-id (getf created :id)))
        (is (eq :system
                (message-sender
                 (car (last (artifactum-finalized-messages buffer))))))
        (let* ((updated (artifactum-package-tool-result
                         "artifactum_update"
                         `((:id . ,artifact-id)
                           (:content . "{\"ok\":false}"))))
               (listed (artifactum-package-tool-result "artifactum_list" nil))
               (read-back (artifactum-package-tool-result
                           "artifactum_read"
                           `((:id . ,artifact-id)))))
          (is (string= "{\"ok\":false}" (getf updated :content)))
          (is (string= artifact-id (getf updated :id)))
          (is (= 1 (length (coerce listed 'list))))
          (is (string= artifact-id (getf (first (coerce listed 'list)) :id)))
          (is (string= "{\"ok\":false}" (getf read-back :content)))
          (let ((html (progn
                        (clawmacs::export-buffer-session-html
                         buffer
                         :path export-path)
                        (uiop:read-file-string export-path))))
            (is (search artifact-id html))
            (is (search (getf record :path) html))
            (is (search "report.json" html))))))))
