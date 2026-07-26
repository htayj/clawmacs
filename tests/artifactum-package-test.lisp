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

(defun read-artifactum-octets (path)
  "Return PATH's contents as an octet vector."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(test artifactum-json-value-accepts-normalized-keys-and-skips-invalid-entries
  "JSON lookup returns the first matching string or symbol key."
  (let ((object '((42 . "ignored")
                  ("provider-name" . "OpenAI")
                  (:provider_name . "later match"))))
    (is (string= "OpenAI"
                 (clawmacs::artifactum-json-value object :provider_name)))
    (is (null (clawmacs::artifactum-json-value object "missing")))))

(test artifactum-normalize-string-preserves-absent-values
  "Absent JSON strings remain absent instead of becoming the symbol name NIL."
  (is (null (clawmacs::artifactum-normalize-string nil)))
  (is (null (clawmacs::artifactum-normalize-string " \t\n")))
  (is (string= "artifact" (clawmacs::artifactum-normalize-string :artifact))))

(test artifactum-normalizes-source-path-and-mime-type-json-keys
  "Normalized JSON keys retain source paths and MIME types in legacy records."
  (let ((record (clawmacs::normalize-artifactum-record
                 '((:id . "legacy-media")
                   (:source_path . "/tmp/legacy-media.png")
                   (:mime_type . "image/png")))))
    (is (string= "/tmp/legacy-media.png" (getf record :path)))
    (is (string= "image/png" (getf record :mime-type)))))

(test artifactum-normalizes-record-without-updated-timestamp
  "Legacy records use their creation timestamp when updated_at is absent."
  (let ((record (clawmacs::normalize-artifactum-record
                 '((:id . "legacy-created")
                   (:path . "/tmp/legacy-created.txt")
                   (:created_at . 12345)))))
    (is (= 12345 (getf record :created-at)))
    (is (= 12345 (getf record :updated-at)))))

(test artifactum-normalizes-record-without-timestamps
  "Records without timestamps receive one consistent fallback timestamp."
  (let* ((record (clawmacs::normalize-artifactum-record
                  '((:id . "legacy-undated")
                    (:path . "/tmp/legacy-undated.txt"))))
         (created-at (getf record :created-at)))
    (is (integerp created-at))
    (is (= created-at (getf record :updated-at)))))

(test artifactum-normalization-preserves-supplied-timestamps
  "Complete records retain distinct supplied creation and update timestamps."
  (let ((record (clawmacs::normalize-artifactum-record
                 '((:id . "complete")
                   (:path . "/tmp/complete.txt")
                   (:created_at . 12345)
                   (:updated_at . 23456)))))
    (is (= 12345 (getf record :created-at)))
    (is (= 23456 (getf record :updated-at)))))

(test artifactum-session-records-read-legacy-index-without-updated-timestamp
  "The durable index reader accepts legacy records missing updated_at."
  (with-artifactum-test-state
    (let* ((buffer (make-artifactum-test-buffer "legacy-index"))
           (session (clawmacs::artifactum-session-for-buffer buffer))
           (index-path (clawmacs::artifactum-session-index-path session)))
      (ensure-directories-exist index-path)
      (with-open-file (stream index-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
        (write-string
         "[{\"id\":\"legacy-index\",\"path\":\"/tmp/legacy-index.txt\",\"created_at\":34567}]"
         stream))
      (let ((records (clawmacs::artifactum-session-records buffer)))
        (is (= 1 (length records)))
        (is (string= "legacy-index" (getf (first records) :id)))
        (is (= 34567 (getf (first records) :created-at)))
        (is (= 34567 (getf (first records) :updated-at)))))))

(test artifactum-create-from-octets-preserves-bytes-and-normalizes-name
  "Generated binary media is byte-exact and never retains path components."
  (with-artifactum-test-state
    (let* ((buffer (make-artifactum-test-buffer "octets"))
           (octets (make-array 6
                               :element-type '(unsigned-byte 8)
                               :initial-contents '(0 1 127 128 254 255)))
           (record (clawmacs:artifactum-create-from-octets
                    buffer
                    "../nested/ Generated Image.PNG "
                    octets
                    :mime-type "image/png")))
      (is (string= "Generated Image.PNG" (getf record :name)))
      (is (string= "png" (pathname-type (pathname (getf record :path)))))
      (is (not (search "nested" (file-namestring (pathname (getf record :path)))
                       :test #'char-equal)))
      (is (= (length octets) (getf record :size)))
      (is (equalp octets (read-artifactum-octets (getf record :path)))))))

(test artifactum-octet-records-round-trip-normalized-metadata-and-provenance
  "Media metadata and provenance round-trip through the durable JSON index."
  (with-artifactum-test-state
    (let* ((buffer (make-artifactum-test-buffer "metadata"))
           (record (clawmacs:artifactum-create-from-octets
                    buffer "image.webp" #(82 73 70 70)
                    :mime-type "image/webp"
                    :metadata '((:Provider-Name . "OpenAI")
                                (:Settings . ((:Quality . "high"))))
                    :provenance '((:Tool-Name . "image_generate")
                                  (:Request-Id . "req-123"))))
           (read-back (first (clawmacs::artifactum-session-records buffer)))
           (index (uiop:read-file-string
                   (clawmacs::artifactum-session-index-path
                    (clawmacs::artifactum-session-for-buffer buffer)))))
      (is (string= "OpenAI"
                   (clawmacs::artifactum-json-value
                    (getf record :metadata) "provider_name")))
      (is (string= "high"
                   (clawmacs::artifactum-json-value
                    (clawmacs::artifactum-json-value
                     (getf record :metadata) "settings")
                    "quality")))
      (is (string= "req-123"
                   (clawmacs::artifactum-json-value
                    (getf read-back :provenance) "request_id")))
      (is (search "\"provider_name\"" index))
      (is (search "\"request_id\"" index)))))

(test artifactum-record-normalization-keeps-legacy-and-skips-malformed-attributes
  "Malformed optional fields do not make legacy durable records unreadable."
  (let ((legacy (clawmacs::normalize-artifactum-record
                 '((:id . "legacy-media")
                   (:path . "/tmp/legacy-media.png")
                   (:created_at . 12345)
                   (:metadata . "not-an-object")
                   (:provenance . ((:source . #\x)))))))
    (is (null (clawmacs::normalize-artifactum-record 42)))
    (is (string= "legacy-media" (getf legacy :id)))
    (is (= 12345 (getf legacy :updated-at)))
    (is (null (getf legacy :metadata)))
    (is (null (getf legacy :provenance)))))

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
      (is (fboundp 'artifactum-create-from-octets))
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
