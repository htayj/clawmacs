(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Sessions and transcripts
;;; --------------------------------------------------------------------------

(defvar *sessions-dir*
  (merge-pathnames #P".config/clawmacs/sessions/" (user-homedir-pathname))
  "Directory for saved session snapshots and transcript sidecars.")

(defstruct (session
            (:constructor %make-session
                (name id directory manifest-path transcript-directory
                 current-transcript-index current-transcript-path created-at
                 &key updated-at)))
  "Persistent chat session metadata and current transcript segment."
  (name "" :type string)
  (id "" :type string)
  (directory #P"" :type pathname)
  (manifest-path #P"" :type pathname)
  (transcript-directory #P"" :type pathname)
  (current-transcript-index 1 :type integer)
  (current-transcript-path #P"" :type pathname)
  (created-at 0 :type integer)
  (updated-at 0 :type integer)
  (lock (bt:make-lock "session-transcript")))

(defun session-name-hash (name)
  "Return a deterministic 64-bit hexadecimal hash for NAME."
  (let ((hash #xCBF29CE484222325)
        (prime #x100000001B3))
    (loop :for char :across name
          :do (setf hash (logxor hash (char-code char))
                    hash (ldb (byte 64 0) (* hash prime))))
    (format nil "~16,'0X" hash)))

(defun session-safe-component (name)
  "Return a filesystem-safe component for session NAME."
  (let ((sanitized
          (with-output-to-string (out)
            (let ((wrote-dash-p nil))
              (loop :for char :across name
                    :for safe-char := (if (alphanumericp char)
                                          (char-downcase char)
                                          #\-)
                    :do (cond
                          ((char= safe-char #\-)
                           (unless wrote-dash-p
                             (write-char safe-char out)
                             (setf wrote-dash-p t)))
                          (t
                           (write-char safe-char out)
                           (setf wrote-dash-p nil))))))))
    (format nil "~A-~A"
            (let ((trimmed (string-trim "-" sanitized)))
              (if (plusp (length trimmed))
                  trimmed
                  "session"))
            (string-downcase (session-name-hash name)))))

(defun session-sidecar-directory (name &key (root *sessions-dir*))
  "Return the sidecar directory for session NAME under ROOT."
  (merge-pathnames
   (make-pathname :directory (list :relative (session-safe-component name)))
   (uiop:ensure-directory-pathname root)))

(defun session-transcript-path-for-index (transcript-directory index)
  "Return the transcript segment path for INDEX."
  (merge-pathnames (format nil "~6,'0D.jsonl" index)
                   (uiop:ensure-directory-pathname transcript-directory)))

(defun session-path-string (path)
  "Return PATH as a namestring, preferring a true absolute path when possible."
  (namestring (or (ignore-errors (truename path))
                  path)))

(defun session-reason-string (reason)
  "Return a stable JSON string for transcript rotation REASON."
  (cond
    ((keywordp reason) (string-downcase (symbol-name reason)))
    ((symbolp reason) (string-downcase (symbol-name reason)))
    (t (princ-to-string reason))))

(defun session-json-vector (value)
  "Return VALUE in a JSON array-friendly shape."
  (cond
    ((null value) nil)
    ((vectorp value) (copy-seq value))
    ((listp value) (coerce (copy-tree value) 'vector))
    (t value)))

(defun session-manifest-alist (session)
  "Return SESSION as a JSON-ready manifest alist."
  `((:name . ,(session-name session))
    (:id . ,(session-id session))
    (:created-at . ,(session-created-at session))
    (:updated-at . ,(session-updated-at session))
    (:directory . ,(session-path-string (session-directory session)))
    (:transcript-directory
     . ,(session-path-string (session-transcript-directory session)))
    (:current-transcript-index
     . ,(session-current-transcript-index session))
    (:current-transcript-path
     . ,(session-path-string (session-current-transcript-path session)))))

(defun write-session-manifest (session)
  "Write SESSION metadata to its sidecar manifest."
  (ensure-directories-exist (session-manifest-path session))
  (with-open-file (stream (session-manifest-path session)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string (cl-json:encode-json-to-string
                   (session-manifest-alist session))
                  stream))
  session)

(defun read-session-manifest (path)
  "Read a session manifest from PATH, returning NIL on absence or failure."
  (when (probe-file path)
    (ignore-errors
      (let ((cl-json:*json-array-type* 'vector))
        (cl-json:decode-json-from-string (uiop:read-file-string path))))))

(defun ensure-session-directories (session)
  "Ensure SESSION sidecar and transcript directories exist."
  (ensure-directories-exist (merge-pathnames #P".keep"
                                             (session-directory session)))
  (ensure-directories-exist (merge-pathnames #P".keep"
                                             (session-transcript-directory
                                              session)))
  session)

(defun session-start-event (session)
  "Return the first transcript event for a new session segment."
  `((:event . "session-start")
    (:timestamp . ,(get-universal-time))
    (:session-name . ,(session-name session))
    (:session-id . ,(session-id session))
    (:transcript-index
     . ,(session-current-transcript-index session))))

(defun previous-transcript-event (session previous-path previous-index reason)
  "Return the first event for a transcript segment created by compaction."
  `((:event . "previous-transcript")
    (:timestamp . ,(get-universal-time))
    (:session-name . ,(session-name session))
    (:session-id . ,(session-id session))
    (:transcript-index
     . ,(session-current-transcript-index session))
    (:previous-transcript-path . ,(session-path-string previous-path))
    (:previous-transcript-index . ,previous-index)
    (:reason . ,(session-reason-string reason))))

(defun %append-session-event-unlocked (session event)
  "Append EVENT to SESSION's current transcript.
Caller must hold SESSION's lock."
  (ensure-directories-exist (session-current-transcript-path session))
  (with-open-file (stream (session-current-transcript-path session)
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string (cl-json:encode-json-to-string event) stream)
    (terpri stream)
    (force-output stream))
  (setf (session-updated-at session) (get-universal-time))
  session)

(defun append-session-event (session event)
  "Append EVENT to SESSION's current transcript and update its manifest."
  (when session
    (bt:with-lock-held ((session-lock session))
      (%append-session-event-unlocked session event)
      (write-session-manifest session)))
  session)

(defun record-session-message (session message)
  "Append MESSAGE as a durable transcript event for SESSION."
  (when session
    (let ((event `((:event . "message")
                   (:sender . ,(symbol-name (message-sender message)))
                   (:text . ,(message-text message))
                   (:timestamp . ,(or (message-timestamp message)
                                      (get-universal-time)))
                   (:read-only-p . ,(message-read-only-p message))
                   ,@(when (message-raw-content message)
                       `((:raw-content
                          . ,(session-json-vector
                              (message-raw-content message)))))
                   ,@(when (message-metadata message)
                       `((:metadata . ,(message-metadata message)))))))
      (append-session-event session event)))
  message)

(defun load-or-create-session (name &key (root *sessions-dir*))
  "Load or initialize persistent session metadata for NAME."
  (let* ((id (session-safe-component name))
         (directory (session-sidecar-directory name :root root))
         (manifest-path (merge-pathnames "session.json" directory))
         (transcript-directory (merge-pathnames #P"transcripts/" directory))
         (manifest (read-session-manifest manifest-path))
         (now (get-universal-time))
         (created-at (or (cdr (assoc :created-at manifest)) now))
         (updated-at (or (cdr (assoc :updated-at manifest)) created-at))
         (index (or (cdr (assoc :current-transcript-index manifest)) 1))
         (manifest-path-string (cdr (assoc :current-transcript-path manifest)))
         (current-path (if manifest-path-string
                           (pathname manifest-path-string)
                           (session-transcript-path-for-index
                            transcript-directory
                            index)))
         (session (%make-session name
                                 id
                                 directory
                                 manifest-path
                                 transcript-directory
                                 index
                                 current-path
                                 created-at
                                 :updated-at updated-at)))
    (ensure-session-directories session)
    (unless (probe-file (session-current-transcript-path session))
      (bt:with-lock-held ((session-lock session))
        (%append-session-event-unlocked session (session-start-event session))))
    (write-session-manifest session)
    session))

(defun rotate-session-transcript (session &key (reason :auto))
  "Advance SESSION to a new transcript segment.
The first event in the new segment points at the previous transcript path."
  (when session
    (bt:with-lock-held ((session-lock session))
      (let ((previous-path (session-current-transcript-path session))
            (previous-index (session-current-transcript-index session)))
        (loop
          :do (incf (session-current-transcript-index session))
              (setf (session-current-transcript-path session)
                    (session-transcript-path-for-index
                     (session-transcript-directory session)
                     (session-current-transcript-index session)))
          :until (not (probe-file (session-current-transcript-path session))))
        (%append-session-event-unlocked
         session
         (previous-transcript-event session previous-path previous-index reason))
        (write-session-manifest session)
        (values (session-current-transcript-path session)
                previous-path)))))
