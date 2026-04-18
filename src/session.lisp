(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Sessions and transcripts
;;; --------------------------------------------------------------------------

(defvar *sessions-dir*
  (merge-pathnames #P".config/clawmacs/sessions/" (user-homedir-pathname))
  "Directory for saved session snapshots and transcript sidecars.")

(defvar *session-format-version* 2
  "Current durable session sidecar format version.")

(defstruct (session
            (:constructor %make-session
                (name id directory manifest-path transcript-directory
                 current-transcript-index current-transcript-path created-at
                 &key updated-at current-leaf-id parent-session)))
  "Persistent chat session metadata and current transcript segment."
  (name "" :type string)
  (id "" :type string)
  (directory #P"" :type pathname)
  (manifest-path #P"" :type pathname)
  (transcript-directory #P"" :type pathname)
  (current-transcript-index 1 :type integer)
  (current-transcript-path #P"" :type pathname)
  (current-leaf-id nil :type (or null string))
  (parent-session nil :type (or null string))
  (created-at 0 :type integer)
  (updated-at 0 :type integer)
  (lock (bt:make-lock "session-transcript")))

(defstruct (session-tree-node
            (:constructor make-session-tree-node
                (entry &key label children)))
  "In-memory node for a durable session entry tree."
  entry
  (label nil :type (or null string))
  (children nil :type list))

(defun session-alist-value (alist key &optional default)
  "Return KEY's value in ALIST, or DEFAULT when absent."
  (let ((cell (assoc key alist)))
    (if cell
        (cdr cell)
        default)))

(defun session-alist-put (alist key value)
  "Return ALIST with KEY set to VALUE."
  (acons key value (remove key alist :key #'car :test #'eq)))

(defun session-event-kind (event)
  "Return EVENT's durable kind string."
  (or (session-alist-value event :event)
      (session-alist-value event :type)
      ""))

(defun session-event-id (event)
  "Return EVENT's tree entry id, if present."
  (session-alist-value event :id))

(defun session-event-parent-id (event)
  "Return EVENT's tree parent id, if present."
  (session-alist-value event :parent-id))

(defun session-event-timestamp (event)
  "Return EVENT's timestamp, defaulting to 0 for sorting."
  (or (session-alist-value event :timestamp) 0))

(defun session-tree-event-kind-p (kind)
  "Return true when KIND participates in the session entry tree."
  (member kind
          '("message" "compaction" "branch-summary" "label"
            "model-change" "think-level-change" "session-info")
          :test #'string=))

(defun session-tree-event-p (event)
  "Return true when EVENT participates in the session entry tree."
  (session-tree-event-kind-p (session-event-kind event)))

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
  `((:version . ,*session-format-version*)
    (:name . ,(session-name session))
    (:id . ,(session-id session))
    (:created-at . ,(session-created-at session))
    (:updated-at . ,(session-updated-at session))
    (:current-leaf-id . ,(session-current-leaf-id session))
    ,@(when (session-parent-session session)
        `((:parent-session . ,(session-parent-session session))))
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

(defun read-session-transcript-events (path)
  "Read transcript JSONL events from PATH, returning decoded alists."
  (let ((events nil))
    (when (probe-file path)
      (with-open-file (stream path :direction :input :external-format :utf-8)
        (loop :for line := (read-line stream nil nil)
              :while line
              :unless (zerop (length line))
                :do (let ((cl-json:*json-array-type* 'vector))
                      (push (cl-json:decode-json-from-string line) events)))))
    (nreverse events)))

(defun session-transcript-paths (session)
  "Return SESSION's transcript segment paths in chronological order."
  (sort (copy-list
         (or (directory (merge-pathnames "*.jsonl"
                                         (session-transcript-directory session)))
             nil))
        #'string<
        :key #'namestring))

(defun session-transcript-events (session)
  "Return all durable events in SESSION in transcript order."
  (loop :for path :in (session-transcript-paths session)
        :append (read-session-transcript-events path)))

(defun session-existing-entry-id-table (session)
  "Return a hash table of tree entry ids already used in SESSION."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (event (session-transcript-events session))
      (let ((id (session-event-id event)))
        (when id
          (setf (gethash id ids) t))))
    ids))

(defun generate-session-entry-id (session)
  "Return a short tree entry id that does not collide in SESSION."
  (let ((ids (session-existing-entry-id-table session)))
    (loop :for id := (string-downcase
                      (format nil "~8,'0X" (random #x100000000)))
          :unless (gethash id ids)
            :return id)))

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

(defun %append-session-tree-event-unlocked
    (session event &key parent-id (advance-leaf-p t))
  "Append EVENT as a tree entry.
Caller must hold SESSION's lock. Returns the written event and its id."
  (let* ((id (or (session-event-id event)
                 (generate-session-entry-id session)))
         (parent (if (assoc :parent-id event)
                     (session-event-parent-id event)
                     parent-id))
         (timestamp (or (session-alist-value event :timestamp)
                        (get-universal-time)))
         (event (session-alist-put
                 (session-alist-put
                  (session-alist-put event :id id)
                  :parent-id parent)
                 :timestamp timestamp)))
    (%append-session-event-unlocked session event)
    (when advance-leaf-p
      (setf (session-current-leaf-id session) id))
    (values event id)))

(defun append-session-event (session event)
  "Append EVENT to SESSION's current transcript and update its manifest."
  (when session
    (bt:with-lock-held ((session-lock session))
      (%append-session-event-unlocked session event)
      (write-session-manifest session)))
  session)

(defun append-session-tree-event
    (session event &key (parent-id nil parent-id-supplied-p)
                   (advance-leaf-p t))
  "Append EVENT as a durable tree entry and update SESSION's manifest.
Returns the written event and entry id."
  (when session
    (bt:with-lock-held ((session-lock session))
      (multiple-value-prog1
          (%append-session-tree-event-unlocked
           session event
           :parent-id (if parent-id-supplied-p
                          parent-id
                          (session-current-leaf-id session))
           :advance-leaf-p advance-leaf-p)
        (write-session-manifest session)))))

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
      (multiple-value-bind (written id)
          (append-session-tree-event session event)
        (when (fboundp 'message-entry-id)
          (setf (message-entry-id message) id))
        (when (and (fboundp 'message-parent-entry-id)
                   (assoc :parent-id written))
          (setf (message-parent-entry-id message)
                (session-event-parent-id written))))))
  message)

(defun record-session-compaction (session &key reason summary tokens-before)
  "Append a compaction marker to SESSION and return its entry id."
  (when session
    (multiple-value-bind (event id)
        (append-session-tree-event
         session
         `((:event . "compaction")
           (:reason . ,(session-reason-string reason))
           ,@(when summary `((:summary . ,summary)))
           ,@(when tokens-before `((:tokens-before . ,tokens-before)))))
      (declare (ignore event))
      id)))

(defun record-session-branch-summary
    (session parent-id summary &key details from-hook)
  "Append a branch summary under PARENT-ID and return its entry id."
  (when session
    (multiple-value-bind (event id)
        (append-session-tree-event
         session
         `((:event . "branch-summary")
           (:from-id . ,(or parent-id "root"))
           (:summary . ,summary)
           ,@(when details `((:details . ,details)))
           ,@(when from-hook `((:from-hook . ,from-hook))))
         :parent-id parent-id)
      (declare (ignore event))
      id)))

(defun record-session-label-change (session target-id label)
  "Set or clear TARGET-ID's label in SESSION without changing the active leaf."
  (when session
    (multiple-value-bind (event id)
        (append-session-tree-event
         session
         `((:event . "label")
           (:target-id . ,target-id)
           (:label . ,(and label
                           (plusp (length (string-trim
                                            '(#\Space #\Tab #\Newline #\Return)
                                            label)))
                           label)))
         :advance-leaf-p nil)
      (declare (ignore event))
      id)))

(defun record-session-model-change (session provider model &key think-level)
  "Append a model-selection event to SESSION and return its entry id."
  (when session
    (multiple-value-bind (event id)
        (append-session-tree-event
         session
         `((:event . "model-change")
           (:provider . ,(string-downcase (symbol-name provider)))
           (:model . ,model)
           ,@(when think-level `((:think-level . ,think-level)))))
      (declare (ignore event))
      id)))

(defun record-session-think-level-change (session think-level)
  "Append a think-level-selection event to SESSION and return its entry id."
  (when session
    (multiple-value-bind (event id)
        (append-session-tree-event
         session
         `((:event . "think-level-change")
           (:think-level . ,think-level)))
      (declare (ignore event))
      id)))

(defun session-normalized-tree-events (session)
  "Return SESSION's tree entries, synthesizing ids for legacy linear events."
  (let ((events nil)
        (previous-id nil)
        (index 0))
    (dolist (event (session-transcript-events session))
      (when (session-tree-event-p event)
        (incf index)
        (let* ((id (or (session-event-id event)
                       (format nil "legacy-~6,'0D" index)))
               (parent-id (if (assoc :parent-id event)
                              (session-event-parent-id event)
                              previous-id))
               (timestamp (or (session-alist-value event :timestamp) 0))
               (normalized
                 (session-alist-put
                  (session-alist-put
                   (session-alist-put event :id id)
                   :parent-id parent-id)
                  :timestamp timestamp)))
          (push normalized events)
          (setf previous-id id))))
    (nreverse events)))

(defun session-entry-table (session)
  "Return a hash table mapping entry ids to normalized tree events."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (event (session-normalized-tree-events session))
      (setf (gethash (session-event-id event) table) event))
    table))

(defun session-label-table (session)
  "Return a hash table mapping entry ids to current labels."
  (let ((labels (make-hash-table :test #'equal)))
    (dolist (event (session-normalized-tree-events session))
      (when (string= "label" (session-event-kind event))
        (let ((target-id (session-alist-value event :target-id))
              (label (session-alist-value event :label)))
          (when target-id
            (if (and label
                     (plusp (length (string-trim
                                      '(#\Space #\Tab #\Newline #\Return)
                                      label))))
                (setf (gethash target-id labels) label)
                (remhash target-id labels))))))
    labels))

(defun session-last-tree-entry-id (session)
  "Return the last normalized tree entry id in SESSION, if any."
  (let ((last nil))
    (dolist (event (session-normalized-tree-events session) last)
      (unless (string= "label" (session-event-kind event))
        (setf last (session-event-id event))))))

(defun session-effective-leaf-id (session)
  "Return SESSION's current leaf id."
  (session-current-leaf-id session))

(defun session-find-entry (session entry-id)
  "Return normalized tree entry ENTRY-ID in SESSION."
  (and entry-id
       (gethash entry-id (session-entry-table session))))

(defun session-entry-ancestor-p (session ancestor-id descendant-id)
  "Return true when ANCESTOR-ID is on DESCENDANT-ID's parent chain."
  (let ((entries (session-entry-table session))
        (current-id descendant-id))
    (loop :while current-id
          :do (when (string= current-id ancestor-id)
                (return t))
              (let ((entry (gethash current-id entries)))
                (setf current-id (and entry
                                      (session-event-parent-id entry))))
          :finally (return nil))))

(defun session-branch-events (session &optional leaf-id)
  "Return normalized tree events from root to LEAF-ID."
  (let* ((entries (session-entry-table session))
         (current-id (or leaf-id (session-effective-leaf-id session)))
         (path nil))
    (loop :while current-id
          :for entry := (gethash current-id entries)
          :while entry
          :do (push entry path)
              (setf current-id (session-event-parent-id entry)))
    path))

(defun session-active-path-ids (session &optional leaf-id)
  "Return entry ids on SESSION's active path."
  (mapcar #'session-event-id
          (session-branch-events session leaf-id)))

(defun session-context-events-for-leaf (session &optional leaf-id)
  "Return branch events that should be replayed into context.
Events before the latest compaction marker on the branch are skipped."
  (let* ((path (session-branch-events session leaf-id))
         (last-compaction
           (position-if (lambda (event)
                          (string= "compaction" (session-event-kind event)))
                        path
                        :from-end t)))
    (if last-compaction
        (subseq path (1+ last-compaction))
        path)))

(defun session-message-event-to-serialized-message (event)
  "Convert a replayable SESSION tree EVENT to buffer serialized message data."
  (let ((kind (session-event-kind event)))
    (cond
      ((string= kind "message")
       `((:entry-id . ,(session-event-id event))
         (:parent-entry-id . ,(session-event-parent-id event))
         (:sender . ,(or (session-alist-value event :sender) "SYSTEM"))
         (:text . ,(or (session-alist-value event :text) ""))
         (:timestamp . ,(session-alist-value event :timestamp))
         (:read-only-p . ,(session-alist-value event :read-only-p))
         ,@(when (session-alist-value event :raw-content)
             `((:raw-content . ,(session-alist-value event :raw-content))))
         ,@(when (session-alist-value event :metadata)
             `((:metadata . ,(session-alist-value event :metadata))))))
      ((string= kind "branch-summary")
       `((:entry-id . ,(session-event-id event))
         (:parent-entry-id . ,(session-event-parent-id event))
         (:sender . "BRANCH-SUMMARY")
         (:text . ,(or (session-alist-value event :summary) ""))
         (:timestamp . ,(session-alist-value event :timestamp))
         (:read-only-p . t)
         (:metadata . ((:kind . "branch-summary")
                       (:from-id . ,(session-alist-value event :from-id))))))
      (t nil))))

(defun session-active-branch-message-events (session &optional leaf-id)
  "Return serialized messages for SESSION's active branch."
  (remove nil
          (mapcar #'session-message-event-to-serialized-message
                  (session-context-events-for-leaf session leaf-id))))

(defun session-branch-state (session &optional leaf-id)
  "Return provider, model, and think-level state along a branch."
  (let ((provider nil)
        (model nil)
        (think-level nil))
    (dolist (event (session-branch-events session leaf-id))
      (let ((kind (session-event-kind event)))
        (cond
          ((string= kind "model-change")
           (let ((provider-name (session-alist-value event :provider)))
             (setf provider (and provider-name
                                 (intern (string-upcase provider-name)
                                         :keyword))
                   model (session-alist-value event :model)
                   think-level (session-alist-value event :think-level))))
          ((string= kind "think-level-change")
           (setf think-level (session-alist-value event :think-level))))))
    (values provider model think-level)))

(defun session-tree-roots (session)
  "Return SESSION's entry tree roots as SESSION-TREE-NODE objects."
  (let* ((events (session-normalized-tree-events session))
         (labels (session-label-table session))
         (nodes (make-hash-table :test #'equal))
         (roots nil))
    (dolist (event events)
      (let ((id (session-event-id event)))
        (setf (gethash id nodes)
              (make-session-tree-node
               event
               :label (gethash id labels)))))
    (dolist (event events)
      (let* ((id (session-event-id event))
             (parent-id (session-event-parent-id event))
             (node (gethash id nodes))
             (parent (and parent-id
                          (not (string= parent-id id))
                          (gethash parent-id nodes))))
        (if parent
            (push node (session-tree-node-children parent))
            (push node roots))))
    (labels ((sort-node (node)
               (setf (session-tree-node-children node)
                     (sort (nreverse (session-tree-node-children node))
                           #'<
                           :key (lambda (child)
                                  (session-event-timestamp
                                   (session-tree-node-entry child)))))
               (dolist (child (session-tree-node-children node))
                 (sort-node child))))
      (setf roots
            (sort (nreverse roots)
                  #'<
                  :key (lambda (node)
                         (session-event-timestamp
                          (session-tree-node-entry node)))))
      (dolist (root roots)
        (sort-node root)))
    roots))

(defun set-session-current-leaf (session leaf-id)
  "Set SESSION's active leaf to LEAF-ID and persist the manifest."
  (when session
    (when (and leaf-id (not (session-find-entry session leaf-id)))
      (error "Session entry not found: ~A" leaf-id))
    (setf (session-current-leaf-id session) leaf-id)
    (write-session-manifest session))
  session)

(defun session-navigation-leaf-for-entry (session entry-id)
  "Return the leaf to use when navigating to ENTRY-ID.
Selecting a user message returns its parent so the message can be edited."
  (let ((entry (session-find-entry session entry-id)))
    (unless entry
      (error "Session entry not found: ~A" entry-id))
    (if (and (string= "message" (session-event-kind entry))
             (string= "USER" (or (session-alist-value entry :sender) "")))
        (session-event-parent-id entry)
        entry-id)))

(defun session-entry-user-message-text (session entry-id)
  "Return ENTRY-ID's user message text, or NIL."
  (let ((entry (session-find-entry session entry-id)))
    (when (and entry
               (string= "message" (session-event-kind entry))
               (string= "USER" (or (session-alist-value entry :sender) "")))
      (or (session-alist-value entry :text) ""))))

(defun unique-session-branch-name (base-name)
  "Return a session name derived from BASE-NAME that is not already saved."
  (let ((candidate (format nil "~A branch" base-name)))
    (if (not (member candidate (or (ignore-errors (list-saved-sessions)) nil)
                    :test #'string=))
        candidate
        (loop :for suffix :from 2
              :for name := (format nil "~A branch ~D" base-name suffix)
              :unless (member name (or (ignore-errors (list-saved-sessions)) nil)
                              :test #'string=)
                :return name))))

(defun create-branched-session (session leaf-id &key name)
  "Create a new session containing SESSION's path through LEAF-ID."
  (let* ((path (session-branch-events session leaf-id))
         (new-name (or name (unique-session-branch-name (session-name session))))
         (parent (session-path-string (session-directory session)))
         (new-session (load-or-create-session new-name
                                              :parent-session parent))
         (labels (session-label-table session))
         (path-ids (mapcar #'session-event-id path)))
    (bt:with-lock-held ((session-lock new-session))
      (dolist (event path)
        (%append-session-event-unlocked new-session event))
      (setf (session-current-leaf-id new-session) leaf-id)
      (write-session-manifest new-session))
    (dolist (entry-id path-ids)
      (let ((label (gethash entry-id labels)))
        (when label
          (record-session-label-change new-session entry-id label))))
    (write-session-manifest new-session)
    new-session))

(defun load-or-create-session
    (name &key (root *sessions-dir*) parent-session)
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
         (current-leaf-cell (assoc :current-leaf-id manifest))
         (current-leaf-id (cdr current-leaf-cell))
         (parent-session (or (cdr (assoc :parent-session manifest))
                             parent-session))
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
                                 :updated-at updated-at
                                 :current-leaf-id current-leaf-id
                                 :parent-session parent-session)))
    (ensure-session-directories session)
    (unless (probe-file (session-current-transcript-path session))
      (bt:with-lock-held ((session-lock session))
        (%append-session-event-unlocked session (session-start-event session))))
    (unless current-leaf-cell
      (setf (session-current-leaf-id session)
            (session-last-tree-entry-id session)))
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
