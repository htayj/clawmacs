(in-package :rplaca)

(defvar *artifactum-directory-name* #P"artifacts/"
  "Directory name under a session root for durable artifact files.")

(defvar *artifactum-index-file-name* "index.json"
  "Artifact metadata index file stored under the artifact root.")

(defvar *artifactum-index-lock*
  (bt:make-lock "rplaca artifactum index")
  "Process-wide lock serializing Artifactum index and store transactions.

Detached tool buffers copy session objects and therefore do not share the
session lock of their live buffer.  This lock intentionally lives above those
session objects so buffers that name the same session directory cannot race a
read-modify-supersede update of artifacts/index.json.")

(declaim (type integer *artifactum-preview-character-limit*))
(defvar *artifactum-preview-character-limit* 2000
  "Maximum preview text length stored in artifact metadata and context messages.")

(defparameter +artifactum-supported-attachment-mime-types+
  '("image/png"
    "image/jpeg"
    "image/gif"
    "image/webp"
    "image/svg+xml"
    "application/pdf"
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    "application/vnd.openxmlformats-officedocument.presentationml.presentation"
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    "text/plain"
    "text/markdown"
    "text/csv"
    "application/json")
  "MIME types accepted by artifactum attachment ingestion.")

(declaim (ftype (function (t) boolean) artifactum-blank-string-p
                artifactum-image-mime-type-p
                artifactum-textual-mime-type-p
                artifactum-supported-attachment-mime-type-p)
         (ftype (function (t) (or null string)) artifactum-normalize-string
                artifactum-normalize-artifact-name
                artifactum-file-extension
                artifactum-strip-xml-markup
                artifactum-run-program-string
                artifactum-preview-text)
         (ftype (function (t) string) artifactum-path-string
                artifactum-guess-mime-type
                artifactum-sanitize-file-name
                artifactum-collapse-whitespace)
         (ftype (function ((or string symbol)) string) artifactum-json-key)
         (ftype (function (list (or string symbol)) t) artifactum-json-value)
         (ftype (function (string string string) string) artifactum-string-replace-all)
         (ftype (function (t string) (or null string)) artifactum-zip-entry-output)
         (ftype (function (t) list) artifactum-zip-entry-list
                normalize-artifactum-record
                artifactum-record-json
                artifactum-read-index-unlocked
                artifactum-read-index
                artifactum-session-records)
         (ftype (function (t list) (or null string)) artifactum-office-xml-text)
         (ftype (function (t) integer) artifactum-file-size)
         (ftype (function (t) (or null list)) artifactum-normalize-record-attributes)
         (ftype (function (t t) (or null list)) artifactum-find-record)
         (ftype (function (t string) string) artifactum-generate-id-unlocked
                artifactum-generate-id)
         (ftype (function (t string string) pathname) artifactum-store-path)
         (ftype (function (t list) list) artifactum-upsert-record-unlocked
                artifactum-upsert-record)
         (ftype (function (t list) t) artifactum-write-index-unlocked
                artifactum-write-index))

(defun artifactum-blank-string-p (value)
  "Return true when VALUE is NIL or ASCII whitespace only."
  (or (null value)
      (let ((string (typecase value
                      (string value)
                      (symbol (symbol-name value))
                      (character (string value))
                      (t nil))))
        (or (null string)
            (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        string)))))))

(defun artifactum-normalize-string (value)
  "Return VALUE as a trimmed string, or NIL when blank."
  (let ((string (typecase value
                  (null nil)
                  (string value)
                  (symbol (symbol-name value))
                  (character (string value))
                  (t nil))))
    (unless (artifactum-blank-string-p string)
      (string-trim '(#\Space #\Tab #\Newline #\Return) string))))

(defun artifactum-path-string (path)
  "Return PATH as a stable namestring."
  (namestring (pathname path)))

(defun artifactum-json-key (key)
  "Return KEY as a lowercase JSON-style key."
  (let ((raw (string-downcase
              (etypecase key
                (string key)
                (symbol (symbol-name key))))))
    (with-output-to-string (out)
      (let ((last-separator-p nil))
        (loop :for ch :across raw
              :do (if (or (char= ch #\-) (char= ch #\_))
                      (unless last-separator-p
                        (write-char #\_ out)
                        (setf last-separator-p t))
                      (progn
                        (write-char ch out)
                        (setf last-separator-p nil))))))))

(defun artifactum-json-value (alist key)
  "Return KEY's value from decoded JSON ALIST."
  (let ((name (artifactum-json-key key)))
    (when (listp alist)
      (loop :for entry :in alist
            :when (and (consp entry)
                       (let ((entry-key (car entry)))
                         (and (typep entry-key '(or string symbol))
                              (string= name (artifactum-json-key entry-key)))))
              :return (cdr entry)))))

(defun artifactum-session-root (session)
  "Return SESSION's artifact root pathname."
  (merge-pathnames *artifactum-directory-name* (session-directory session)))

(defun artifactum-session-index-path (session)
  "Return SESSION's artifact index pathname."
  (merge-pathnames *artifactum-index-file-name*
                   (artifactum-session-root session)))

(defun artifactum-session-for-buffer (buffer)
  "Return BUFFER's durable session, or signal an error."
  (or (ensure-buffer-session buffer)
      (error "Artifact storage requires a persistent chat session.")))

(defun artifactum-file-extension (path-or-name)
  "Return PATH-OR-NAME's lowercase filename extension without the dot."
  (let ((type (pathname-type (pathname path-or-name))))
    (and type
         (string-downcase type))))

(defun artifactum-guess-mime-type (path-or-name)
  "Return a best-effort MIME type for PATH-OR-NAME."
  (let ((extension (artifactum-file-extension path-or-name)))
    (cond
      ((null extension) "application/octet-stream")
      ((string= extension "png") "image/png")
      ((or (string= extension "jpg") (string= extension "jpeg")) "image/jpeg")
      ((string= extension "gif") "image/gif")
      ((string= extension "webp") "image/webp")
      ((string= extension "svg") "image/svg+xml")
      ((string= extension "pdf") "application/pdf")
      ((string= extension "docx")
       "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
      ((string= extension "pptx")
       "application/vnd.openxmlformats-officedocument.presentationml.presentation")
      ((string= extension "xlsx")
       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
      ((string= extension "md") "text/markdown")
      ((string= extension "csv") "text/csv")
      ((string= extension "json") "application/json")
      ((or (string= extension "txt")
           (string= extension "lisp")
           (string= extension "cl")
           (string= extension "org")
           (string= extension "html")
           (string= extension "htm")
           (string= extension "xml")
           (string= extension "svg"))
       "text/plain")
      (t "application/octet-stream"))))

(defun artifactum-image-mime-type-p (mime-type)
  "Return true when MIME-TYPE is an image artifact."
  (not (null (and mime-type
                  (or (alexandria:starts-with-subseq "image/" mime-type)
                      (string= mime-type "image/svg+xml"))))))

(defun artifactum-textual-mime-type-p (mime-type)
  "Return true when MIME-TYPE should be read as plain text."
  (not (null (and mime-type
                  (or (alexandria:starts-with-subseq "text/" mime-type)
                      (member mime-type
                              '("application/json"
                                "image/svg+xml")
                              :test #'string=))))))

(defun artifactum-supported-attachment-mime-type-p (mime-type)
  "Return true when MIME-TYPE is supported by artifactum attachments."
  (not (null (member mime-type +artifactum-supported-attachment-mime-types+
                     :test #'string=))))

(defun artifactum-normalize-artifact-name (name)
  "Return NAME as a display-safe basename without directory components."
  (let* ((raw (artifactum-normalize-string name))
         (slash-normalized (and raw (substitute #\/ #\\ raw)))
         (separator (and slash-normalized
                         (position #\/ slash-normalized :from-end t)))
         (basename (artifactum-normalize-string
                    (and slash-normalized
                         (if separator
                             (subseq slash-normalized (1+ separator))
                             slash-normalized)))))
    (if (or (artifactum-blank-string-p basename)
            (member basename '("." "..") :test #'string=))
        "artifact"
        basename)))

(defun artifactum-sanitize-file-name (name)
  "Return NAME as a filesystem-safe basename while preserving its extension."
  (let* ((basename (artifactum-normalize-artifact-name name))
         (pathname (pathname basename))
         (type (pathname-type pathname))
         (stem (or (pathname-name pathname) "artifact"))
         (safe-stem (session-safe-component stem))
         (safe-type (and type
                         (with-output-to-string (stream)
                           (loop :for character :across type
                                 :when (alphanumericp character)
                                   :do (write-char (char-downcase character)
                                                   stream))))))
    (if (artifactum-blank-string-p safe-type)
        safe-stem
        (format nil "~A.~A" safe-stem safe-type))))

(defun artifactum-run-program-string (argv)
  "Run ARGV and return stdout as a string, or NIL on failure."
  (handler-case
      (multiple-value-bind (stdout stderr exit-code)
          (uiop:run-program argv
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (declare (ignore stderr))
        (when (zerop exit-code)
          stdout))
    (error ()
      nil)))

(defun artifactum-string-replace-all (string needle replacement)
  "Return STRING with every NEEDLE replaced by REPLACEMENT."
  (if (artifactum-blank-string-p needle)
      (or string "")
      (with-output-to-string (out)
        (loop :with haystack := (or string "")
              :with start := 0
              :for pos := (search needle haystack :start2 start)
              :do (write-string haystack out :start start :end (or pos (length haystack)))
                  (when pos
                    (write-string replacement out)
                    (setf start (+ pos (length needle))))
              :while pos))))

(defun artifactum-collapse-whitespace (text)
  "Return TEXT with ASCII whitespace runs collapsed to one space."
  (with-output-to-string (out)
    (let ((pending-space-p nil))
      (loop :for ch :across (or text "")
            :do (if (find ch '(#\Space #\Tab #\Newline #\Return))
                    (setf pending-space-p t)
                    (progn
                      (when pending-space-p
                        (write-char #\Space out)
                        (setf pending-space-p nil))
                      (write-char ch out)))))))

(defun artifactum-strip-xml-markup (text)
  "Drop XML tags from TEXT and decode a few common entities."
  (let ((raw
          (with-output-to-string (out)
            (let ((in-tag-p nil))
              (loop :for ch :across (or text "")
                    :do (cond
                          ((char= ch #\<)
                           (setf in-tag-p t))
                          ((char= ch #\>)
                           (setf in-tag-p nil)
                           (write-char #\Space out))
                          ((not in-tag-p)
                           (write-char ch out))))))))
    (let ((decoded raw))
      (setf decoded (artifactum-string-replace-all decoded "&amp;" "&"))
      (setf decoded (artifactum-string-replace-all decoded "&lt;" "<"))
      (setf decoded (artifactum-string-replace-all decoded "&gt;" ">"))
      (setf decoded (artifactum-string-replace-all decoded "&quot;" "\""))
      (setf decoded (artifactum-string-replace-all decoded "&apos;" "'"))
      (artifactum-normalize-string
       (artifactum-collapse-whitespace decoded)))))

(defun artifactum-zip-entry-output (path entry)
  "Return PATH zip ENTRY content as a string, or NIL on failure."
  (artifactum-run-program-string
   (list "unzip" "-p" (artifactum-path-string path) entry)))

(defun artifactum-zip-entry-list (path)
  "Return PATH zip entry names as a list of strings."
  (let ((output (artifactum-run-program-string
                 (list "unzip" "-Z1" (artifactum-path-string path)))))
    (and output
         (remove-if #'artifactum-blank-string-p
                    (split-string-by-newline output)))))

(defun artifactum-office-xml-text (path prefixes)
  "Return concatenated stripped XML text from PATH zip entries matching PREFIXES."
  (let ((entries (artifactum-zip-entry-list path))
        (chunks nil))
    (when entries
      (dolist (entry entries)
        (when (some (lambda (prefix)
                      (alexandria:starts-with-subseq prefix entry))
                    prefixes)
          (let ((xml (artifactum-zip-entry-output path entry)))
            (when xml
              (push (artifactum-strip-xml-markup xml) chunks)))))
      (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (format nil "~{~A~^~%~}" (nreverse chunks)))))
        (unless (zerop (length text))
          text)))))

(defun artifactum-extract-text-from-pdf (path)
  "Return extracted text from PDF PATH when pdftotext is available."
  (let ((output (artifactum-run-program-string
                 (list "pdftotext" "-q"
                       (artifactum-path-string path)
                       "-"))))
    (and output
         (artifactum-normalize-string output))))

(defun artifactum-extract-text-from-path (path mime-type)
  "Return best-effort extracted text for PATH and MIME-TYPE."
  (cond
    ((artifactum-textual-mime-type-p mime-type)
     (artifactum-normalize-string
      (ignore-errors (uiop:read-file-string path))))
    ((string= mime-type "application/pdf")
     (artifactum-extract-text-from-pdf path))
    ((string= mime-type
              "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
     (artifactum-office-xml-text path '("word/")))
    ((string= mime-type
              "application/vnd.openxmlformats-officedocument.presentationml.presentation")
     (artifactum-office-xml-text path '("ppt/slides/")))
    ((string= mime-type
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
     (artifactum-office-xml-text path '("xl/sharedStrings" "xl/worksheets/")))
    (t
     nil)))

(defun artifactum-preview-text (text)
  "Return a safe preview excerpt for TEXT."
  (let ((normalized (artifactum-normalize-string text)))
    (when normalized
      (if (> (length normalized) *artifactum-preview-character-limit*)
          (format nil "~A..." (subseq normalized 0 *artifactum-preview-character-limit*))
          normalized))))

(defun artifactum-file-size (path)
  "Return PATH's byte size."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun artifactum-json-object-entry-value (entry)
  "Return ENTRY's value from Artifactum's dotted-alist representation."
  (cdr entry))

(defun artifactum-json-object-p (value)
  "Return true when VALUE is an alist with string or symbol keys."
  (and (listp value)
       (every (lambda (entry)
                (and (consp entry)
                     (typep (car entry) '(or string symbol))))
              value)))

(defun artifactum-normalize-json-data (value)
  "Return VALUE as JSON-compatible data and a flag indicating success."
  (typecase value
    (null (values nil t))
    ((eql t) (values t t))
    (string (values value t))
    (integer (values value t))
    (float (values value t))
    (symbol (values (string-downcase (symbol-name value)) t))
    (vector
     (let ((normalized nil))
       (loop :for element :across value
             :do (multiple-value-bind (normalized-element valid-p)
                     (artifactum-normalize-json-data element)
                   (unless valid-p
                     (return-from artifactum-normalize-json-data
                       (values nil nil)))
                   (push normalized-element normalized)))
       (values (coerce (nreverse normalized) 'vector) t)))
    (list
     (if (artifactum-json-object-p value)
         (values (artifactum-normalize-record-attributes value) t)
         (let ((normalized nil))
           (dolist (element value)
             (multiple-value-bind (normalized-element valid-p)
                 (artifactum-normalize-json-data element)
               (unless valid-p
                 (return-from artifactum-normalize-json-data
                   (values nil nil)))
               (push normalized-element normalized)))
           (values (nreverse normalized) t))))
    (t (values nil nil))))

(defun artifactum-normalize-record-attributes (attributes)
  "Return ATTRIBUTES as a canonical JSON-object alist, or NIL when invalid.

Metadata and provenance are deliberately data-only: keys are normalized to
underscored JSON names, nested objects receive the same treatment, and values
must be JSON-compatible.  Missing or malformed fields on older index records
therefore remain readable as NIL rather than invalidating the whole record."
  (when (artifactum-json-object-p attributes)
    (let ((normalized nil))
      (dolist (entry attributes)
        (multiple-value-bind (value valid-p)
            (artifactum-normalize-json-data
             (artifactum-json-object-entry-value entry))
          (unless valid-p
            (return-from artifactum-normalize-record-attributes nil))
          (push (cons (artifactum-json-key (car entry)) value) normalized)))
      (sort (remove-duplicates normalized
                               :key #'car
                               :test #'string=
                               :from-end t)
            #'string<
            :key #'car))))

(defun artifactum-normalize-nonnegative-integer (value default)
  "Return VALUE when it is a nonnegative integer, otherwise DEFAULT."
  (if (and (integerp value) (not (minusp value)))
      value
      default))

(defun normalize-artifactum-record (record)
  "Normalize RECORD into a plist."
  (when (listp record)
    (let* ((id (artifactum-normalize-string
             (artifactum-json-value record "id")))
        (kind (or (artifactum-normalize-string
                   (artifactum-json-value record "kind"))
                  "artifact"))
        (name (or (artifactum-normalize-string
                   (artifactum-json-value record "name"))
                  "artifact"))
        (mime-type (or (artifactum-normalize-string
                        (artifactum-json-value record "mime_type"))
                       "application/octet-stream"))
        (path (artifactum-normalize-string
               (or (artifactum-json-value record "path")
                   (artifactum-json-value record "source_path"))))
        (size (artifactum-normalize-nonnegative-integer
               (artifactum-json-value record "size") 0))
        (preview (artifactum-normalize-string
                  (artifactum-json-value record "preview")))
        (extracted-text (artifactum-normalize-string
                         (artifactum-json-value record "extracted_text")))
        (author (or (artifactum-normalize-string
                     (artifactum-json-value record "author"))
                    "agent"))
        (metadata (artifactum-normalize-record-attributes
                   (artifactum-json-value record "metadata")))
        (provenance (artifactum-normalize-record-attributes
                     (artifactum-json-value record "provenance")))
        (created-at (artifactum-normalize-nonnegative-integer
                     (artifactum-json-value record "created_at")
                     (get-universal-time)))
        (updated-at (artifactum-normalize-nonnegative-integer
                     (artifactum-json-value record "updated_at")
                     created-at)))
      (when (and id path)
        (list :id id
              :kind kind
              :name name
              :mime-type mime-type
              :path path
              :size size
              :preview preview
              :extracted-text extracted-text
              :author author
              :metadata metadata
              :provenance provenance
              :created-at created-at
              :updated-at updated-at)))))

(defun artifactum-record-json (record)
  "Return RECORD as a JSON alist."
  (append
   `((:id . ,(getf record :id))
     (:kind . ,(getf record :kind))
     (:name . ,(getf record :name))
     (:mime_type . ,(getf record :mime-type))
     (:path . ,(getf record :path))
     (:size . ,(getf record :size))
     (:preview . ,(getf record :preview))
     (:extracted_text . ,(getf record :extracted-text))
     (:author . ,(getf record :author)))
   (when (getf record :metadata)
     `((:metadata . ,(getf record :metadata))))
   (when (getf record :provenance)
     `((:provenance . ,(getf record :provenance))))
   `((:created_at . ,(getf record :created-at))
     (:updated_at . ,(getf record :updated-at)))))

(defun artifactum-read-index-unlocked (session)
  "Return SESSION's artifact records while the caller owns the index lock."
  (let ((path (artifactum-session-index-path session)))
    (if (probe-file path)
        (let ((cl-json:*json-array-type* 'vector))
          (remove nil
                  (map 'list #'normalize-artifactum-record
                       (or (ignore-errors
                             (cl-json:decode-json-from-string
                              (uiop:read-file-string path)))
                           #()))))
        nil)))

(defun artifactum-read-index (session)
  "Return SESSION's artifact records under the process-wide index lock."
  (bt:with-lock-held (*artifactum-index-lock*)
    (artifactum-read-index-unlocked session)))

(defun artifactum-write-index-unlocked (session records)
  "Persist SESSION RECORDS while the caller owns the index lock."
  (let ((path (artifactum-session-index-path session)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string
       (cl-json:encode-json-to-string
        (coerce (mapcar #'artifactum-record-json records) 'vector))
       stream))
    path))

(defun artifactum-write-index (session records)
  "Persist SESSION RECORDS under the process-wide index lock."
  (bt:with-lock-held (*artifactum-index-lock*)
    (artifactum-write-index-unlocked session records)))

(defun artifactum-session-records (buffer)
  "Return BUFFER's durable artifact records."
  (artifactum-read-index (artifactum-session-for-buffer buffer)))

(defun artifactum-find-record (buffer artifact-id)
  "Return BUFFER artifact ARTIFACT-ID, or NIL."
  (let ((session (artifactum-session-for-buffer buffer)))
    (bt:with-lock-held (*artifactum-index-lock*)
      (find (artifactum-normalize-string artifact-id)
            (artifactum-read-index-unlocked session)
            :key (lambda (record) (getf record :id))
            :test #'string=))))

(defun artifactum-generate-id-unlocked (session name)
  "Return a unique artifact id while the caller owns the index lock."
  (let* ((records (artifactum-read-index-unlocked session))
         (existing-ids (mapcar (lambda (record) (getf record :id)) records))
         (base (artifactum-sanitize-file-name name))
         (stamp (get-universal-time)))
    (loop :for suffix :from 0
          :for candidate := (if (zerop suffix)
                                (format nil "art-~D-~A" stamp base)
                                (format nil "art-~D-~A-~D" stamp base suffix))
          :unless (member candidate existing-ids :test #'string=)
            :return candidate)))

(defun artifactum-generate-id (session name)
  "Return a unique artifact id for SESSION derived from NAME."
  (bt:with-lock-held (*artifactum-index-lock*)
    (artifactum-generate-id-unlocked session name)))

(defun artifactum-store-path (session artifact-id name)
  "Return the on-disk storage path for ARTIFACT-ID and NAME."
  (let* ((basename (artifactum-sanitize-file-name name))
         (type (pathname-type (pathname basename)))
         (stem (pathname-name (pathname basename)))
         (filename (if type
                       (format nil "~A-~A.~A" artifact-id stem type)
                       (format nil "~A-~A" artifact-id stem))))
    (merge-pathnames filename (artifactum-session-root session))))

(defun artifactum-upsert-record-unlocked (session record)
  "Upsert RECORD while the caller owns the complete index transaction."
  (let* ((records (artifactum-read-index-unlocked session))
         (id (getf record :id))
         (updated (cons record
                        (remove id records
                                :key (lambda (existing)
                                       (getf existing :id))
                                :test #'string=))))
    (artifactum-write-index-unlocked
     session
     (sort updated #'string<
           :key (lambda (existing)
                  (getf existing :id))))
    record))

(defun artifactum-upsert-record (session record)
  "Atomically insert or replace RECORD in SESSION's artifact index."
  (bt:with-lock-held (*artifactum-index-lock*)
    (artifactum-upsert-record-unlocked session record)))

(defun artifactum-create-from-file (buffer source-path
                                     &key name mime-type
                                       (kind "attachment")
                                       (author "user"))
  "Copy SOURCE-PATH into BUFFER's session artifact store and return its record."
  (let* ((session (artifactum-session-for-buffer buffer))
         (source (lispi:resolve-tool-path source-path))
         (_exists (or (probe-file source)
                      (error "Artifact source path does not exist: ~A" source-path)))
         (resolved-name (or (artifactum-normalize-string name)
                            (file-namestring source)))
         (resolved-mime (or (artifactum-normalize-string mime-type)
                            (artifactum-guess-mime-type resolved-name)))
         (_supported (or (not (string= kind "attachment"))
                         (artifactum-supported-attachment-mime-type-p resolved-mime)
                         (error "Unsupported attachment type: ~A" resolved-mime))))
    (declare (ignore _exists _supported))
    (bt:with-lock-held (*artifactum-index-lock*)
      (let* ((id (artifactum-generate-id-unlocked session resolved-name))
             (target (artifactum-store-path session id resolved-name)))
        (ensure-directories-exist target)
        (uiop:copy-file source target)
        (let* ((extracted-text
                 (artifactum-extract-text-from-path target resolved-mime))
               (record (list :id id
                             :kind kind
                             :name resolved-name
                             :mime-type resolved-mime
                             :path (artifactum-path-string target)
                             :size (artifactum-file-size target)
                             :preview (artifactum-preview-text extracted-text)
                             :extracted-text extracted-text
                             :author author
                             :created-at (get-universal-time)
                             :updated-at (get-universal-time))))
          (artifactum-upsert-record-unlocked session record))))))

(defun artifactum-create-from-content (buffer name content
                                        &key mime-type
                                          (kind "artifact")
                                          (author "agent"))
  "Write CONTENT into BUFFER's session artifact store and return its record."
  (let* ((session (artifactum-session-for-buffer buffer))
         (resolved-name (or (artifactum-normalize-string name)
                            (error "Artifact content creation requires a file name.")))
         (resolved-mime (or (artifactum-normalize-string mime-type)
                            (artifactum-guess-mime-type resolved-name)))
         (text (or content "")))
    (bt:with-lock-held (*artifactum-index-lock*)
      (let* ((id (artifactum-generate-id-unlocked session resolved-name))
             (target (artifactum-store-path session id resolved-name)))
        (ensure-directories-exist target)
        (with-open-file (stream target
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-string text stream))
        (let* ((extracted-text
                 (if (artifactum-textual-mime-type-p resolved-mime)
                     text
                     (artifactum-extract-text-from-path target resolved-mime)))
               (record (list :id id
                             :kind kind
                             :name resolved-name
                             :mime-type resolved-mime
                             :path (artifactum-path-string target)
                             :size (artifactum-file-size target)
                             :preview (artifactum-preview-text extracted-text)
                             :extracted-text extracted-text
                             :author author
                             :created-at (get-universal-time)
                             :updated-at (get-universal-time))))
          (artifactum-upsert-record-unlocked session record))))))

(defun artifactum-create-from-octets (buffer name octets
                                      &key mime-type
                                        (kind "generated-media")
                                        (author "agent")
                                        metadata provenance)
  "Persist OCTETS as a durable BUFFER artifact and return its record.

NAME is reduced to a basename before it is recorded and used to derive the
storage path.  METADATA and PROVENANCE are optional JSON objects; their keys
are normalized to lowercase underscore names before persistence."
  (unless (typep octets '(vector (unsigned-byte 8)))
    (error "Artifact octets must be a vector of (unsigned-byte 8)."))
  (let* ((session (artifactum-session-for-buffer buffer))
         (resolved-name (artifactum-normalize-artifact-name name))
         (resolved-mime (or (artifactum-normalize-string mime-type)
                            (artifactum-guess-mime-type resolved-name)))
         (normalized-metadata (artifactum-normalize-record-attributes metadata))
         (normalized-provenance (artifactum-normalize-record-attributes provenance)))
    (when (and metadata (null normalized-metadata))
      (error "Artifact metadata must be a JSON object."))
    (when (and provenance (null normalized-provenance))
      (error "Artifact provenance must be a JSON object."))
    (bt:with-lock-held (*artifactum-index-lock*)
      (let* ((id (artifactum-generate-id-unlocked session resolved-name))
             (target (artifactum-store-path session id resolved-name)))
        (ensure-directories-exist target)
        (with-open-file (stream target
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-sequence octets stream))
        (let* ((extracted-text
                 (artifactum-extract-text-from-path target resolved-mime))
               (record (list :id id
                             :kind kind
                             :name resolved-name
                             :mime-type resolved-mime
                             :path (artifactum-path-string target)
                             :size (length octets)
                             :preview (artifactum-preview-text extracted-text)
                             :extracted-text extracted-text
                             :author author
                             :metadata normalized-metadata
                             :provenance normalized-provenance
                             :created-at (get-universal-time)
                             :updated-at (get-universal-time))))
          (artifactum-upsert-record-unlocked session record))))))

(defun artifactum-update-record (buffer artifact-id
                                  &key name content source-path mime-type)
  "Update BUFFER artifact ARTIFACT-ID from CONTENT or SOURCE-PATH."
  (let* ((session (artifactum-session-for-buffer buffer))
         (resolved-source (and source-path
                               (lispi:resolve-tool-path source-path))))
    (bt:with-lock-held (*artifactum-index-lock*)
      (let* ((record
               (or (find (artifactum-normalize-string artifact-id)
                         (artifactum-read-index-unlocked session)
                         :key (lambda (entry) (getf entry :id))
                         :test #'string=)
                   (error "Unknown artifact id: ~A" artifact-id)))
             (resolved-name (or (artifactum-normalize-string name)
                                (getf record :name)))
             (resolved-mime (or (artifactum-normalize-string mime-type)
                                (artifactum-guess-mime-type resolved-name)
                                (getf record :mime-type)))
             (target (artifactum-store-path
                      session (getf record :id) resolved-name)))
        (ensure-directories-exist target)
        (cond
          (resolved-source
           (uiop:copy-file resolved-source target))
          (t
           (with-open-file (stream target
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (write-string (or content "") stream))))
        (let* ((extracted-text
                 (artifactum-extract-text-from-path target resolved-mime))
               (updated (copy-list record)))
          (setf (getf updated :name) resolved-name
                (getf updated :mime-type) resolved-mime
                (getf updated :path) (artifactum-path-string target)
                (getf updated :size) (artifactum-file-size target)
                (getf updated :preview) (artifactum-preview-text extracted-text)
                (getf updated :extracted-text) extracted-text
                (getf updated :updated-at) (get-universal-time))
          (artifactum-upsert-record-unlocked session updated))))))

(defun artifactum-read-record-data (buffer artifact-id &key (include-content-p t))
  "Return BUFFER artifact ARTIFACT-ID as a plist suitable for tools."
  (let ((session (artifactum-session-for-buffer buffer)))
    (bt:with-lock-held (*artifactum-index-lock*)
      (let* ((record
               (or (find (artifactum-normalize-string artifact-id)
                         (artifactum-read-index-unlocked session)
                         :key (lambda (entry) (getf entry :id))
                         :test #'string=)
                   (error "Unknown artifact id: ~A" artifact-id)))
             (path (pathname (getf record :path)))
             (mime-type (getf record :mime-type))
             (content (and include-content-p
                           (artifactum-textual-mime-type-p mime-type)
                           (ignore-errors (uiop:read-file-string path)))))
        (list :id (getf record :id)
              :kind (getf record :kind)
              :name (getf record :name)
              :mime-type mime-type
              :path (getf record :path)
              :size (getf record :size)
              :preview (getf record :preview)
              :extracted-text (getf record :extracted-text)
              :metadata (getf record :metadata)
              :provenance (getf record :provenance)
              :content content
              :created-at (getf record :created-at)
              :updated-at (getf record :updated-at))))))

(defun artifactum-record-summary-line (record)
  "Return one selector/help line for RECORD."
  (format nil "[~A] ~A - ~A (~A bytes)"
          (getf record :kind)
          (getf record :name)
          (getf record :mime-type)
          (getf record :size)))

(defun artifactum-reference-text (record &key context-p)
  "Return a user-visible reference block for RECORD."
  (with-output-to-string (stream)
    (format stream "~A: ~A~%"
            (if context-p "Attached file" "Artifact")
            (getf record :name))
    (format stream "artifact-id: ~A~%" (getf record :id))
    (format stream "kind: ~A~%" (getf record :kind))
    (format stream "mime-type: ~A~%" (getf record :mime-type))
    (format stream "path: ~A~%" (getf record :path))
    (format stream "size: ~D bytes~%" (getf record :size))
    (cond
      ((artifactum-image-mime-type-p (getf record :mime-type))
       (format stream "~%![~A](~A)~%"
               (getf record :name)
               (getf record :path)))
      ((getf record :preview)
       (format stream "~%Preview:~%~A~%" (getf record :preview)))
      (t
       (format stream "~%Preview unavailable.~%")))))

(defun artifactum-buffer-state-serializer (buffer)
  "Return BUFFER's artifact-buffer persistence state."
  (let ((msg (message-prev (buffer-input-message buffer))))
    (and msg
         (let ((metadata (message-metadata msg)))
           (when metadata
             (let ((artifact-id (message-metadata-value metadata :artifact-id)))
               (and artifact-id
                    (list :artifact-id artifact-id))))))))

(defun artifactum-artifact-buffer-open-record (buffer record)
  "Populate artifact BUFFER from RECORD."
  (buffer-remove-messages-if buffer (constantly t))
  (let* ((metadata `((:artifact-id . ,(getf record :id))
                     (:artifact-kind . ,(getf record :kind))
                     (:artifact-name . ,(getf record :name))
                     (:artifact-path . ,(getf record :path))
                     (:mime-type . ,(getf record :mime-type))))
         (full-text
           (or (and (artifactum-textual-mime-type-p (getf record :mime-type))
                    (ignore-errors
                      (uiop:read-file-string (pathname (getf record :path)))))
               (getf record :extracted-text)))
         (body (if (or (null full-text)
                       (artifactum-image-mime-type-p
                        (getf record :mime-type)))
                   (artifactum-reference-text record)
                   (format nil "~A~%~%Content:~%~A"
                           (artifactum-reference-text record)
                           full-text))))
    (buffer-insert-agent-message
     buffer
     body
     :metadata metadata))
  (setf (buffer-scroll-offset buffer) most-positive-fixnum)
  buffer)

(defun artifactum-buffer-state-restorer (buffer state)
  "Restore artifact BUFFER from persistence STATE."
  (let* ((artifact-id (getf state :artifact-id))
         (record (and artifact-id
                      (artifactum-find-record buffer artifact-id))))
    (when record
      (artifactum-artifact-buffer-open-record buffer record)))
  buffer)
