(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Structured Output
;;; --------------------------------------------------------------------------

(define-condition structured-output-validation-error (error)
  ((schema :initarg :schema
           :reader structured-output-validation-error-schema)
   (text :initarg :text
         :reader structured-output-validation-error-text)
   (reason :initarg :reason
           :reader structured-output-validation-error-reason))
  (:report (lambda (condition stream)
             (format stream "Structured output did not satisfy the schema: ~A"
                     (structured-output-validation-error-reason condition)))))

(defun interop-json-ready (value)
  "Recursively normalize VALUE into a CL-JSON-friendly JSON object shape."
  (cond
    ((stringp value)
     value)
    ((vectorp value)
     (map 'vector #'interop-json-ready value))
    ((and (listp value)
          (every #'consp value))
     (mapcar (lambda (entry)
               (cons (car entry)
                     (interop-json-ready (cdr entry))))
             value))
    ((tool-plist-p value)
     (loop :for (key item) :on value :by #'cddr
           :collect (cons key (interop-json-ready item))))
    ((listp value)
     (coerce (mapcar #'interop-json-ready value) 'vector))
    (t
     value)))

(defun interop-schema-value (schema key &optional default)
  "Return KEY from JSON-like SCHEMA alists, or DEFAULT."
  (if (and (listp schema)
           (every #'consp schema))
      (loop :for (schema-key . value) :in schema
            :when (tool-key= schema-key key)
              :return value
            :finally (return default))
      default))

(defun interop-json-object-p (value)
  "Return true when VALUE looks like a decoded JSON object alist."
  (and (listp value)
       (every #'consp value)))

(defun interop-json-array-list (value)
  "Return VALUE as a list when it is a decoded JSON array."
  (cond
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t nil)))

(defun interop-output-schema-present-p (schema)
  "Return true when SCHEMA names a structured-output contract."
  (and schema
       (not (and (stringp schema)
                 (blank-string-p schema)))))

(defun normalize-output-schema (schema)
  "Normalize SCHEMA into a decoded JSON-schema object, or NIL."
  (cond
    ((null schema) nil)
    ((and (stringp schema) (blank-string-p schema))
     nil)
    ((stringp schema)
     (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) schema))
            (path (ignore-errors (probe-file trimmed))))
       (cond
         ((and path (not (uiop:directory-pathname-p path)))
          (api-json-decode (uiop:read-file-string path)))
         (t
          (api-json-decode trimmed)))))
    ((or (listp schema) (vectorp schema))
     schema)
    (t
     (error "Unsupported output schema value: ~S" schema))))

(defun interop-schema-enum-member-p (value enum-values)
  "Return true when VALUE is a member of ENUM-VALUES."
  (let ((values (cond
                  ((vectorp enum-values) (coerce enum-values 'list))
                  ((listp enum-values) enum-values)
                  (t (list enum-values)))))
    (member value values :test #'equal)))

(defun interop-schema-object-key-present-p (value key)
  "Return true when VALUE contains KEY."
  (and (interop-json-object-p value)
       (loop :for (object-key . object-value) :in value
             :thereis (tool-key= object-key key))))

(defun interop-value-type-matches-p (type value)
  "Return true when VALUE satisfies JSON schema TYPE."
  (let ((name (string-downcase (tool-key-name type))))
    (cond
      ((string= name "object")
       (interop-json-object-p value))
      ((string= name "array")
       (not (null (interop-json-array-list value))))
      ((string= name "string")
       (stringp value))
      ((string= name "number")
       (numberp value))
      ((string= name "integer")
       (integerp value))
      ((string= name "boolean")
       (or (eq value t) (null value)))
      ((string= name "null")
       (null value))
      (t
       t))))

(defun interop-validate-json-schema-value (schema value &optional (path "$"))
  "Validate VALUE against a JSON-like SCHEMA or signal an error."
  (when schema
    (let ((type (interop-schema-value schema :type))
          (enum-values (interop-schema-value schema :enum))
          (required (interop-schema-value schema :required))
          (properties (interop-schema-value schema :properties))
          (items (interop-schema-value schema :items))
          (additional-properties
            (if (member :additional-properties schema :key #'car :test #'tool-key=)
                (interop-schema-value schema :additional-properties)
                t)))
      (when (and type
                 (not (interop-value-type-matches-p type value)))
        (error "Expected ~A at ~A, got ~S" type path value))
      (when (and enum-values
                 (not (interop-schema-enum-member-p value enum-values)))
        (error "Value at ~A is not in enum ~S: ~S"
               path enum-values value))
      (when (interop-json-object-p value)
        (let ((required-keys (interop-json-array-list required)))
          (dolist (key required-keys)
            (unless (interop-schema-object-key-present-p value key)
              (error "Missing required property ~A at ~A"
                     (tool-key-name key) path))))
        (when properties
          (dolist (entry value)
            (let* ((key (car entry))
                   (child-value (cdr entry))
                   (child-schema (interop-schema-value properties key)))
              (cond
                (child-schema
                 (interop-validate-json-schema-value
                  child-schema
                  child-value
                  (format nil "~A.~A" path (tool-key-name key))))
                ((null additional-properties)
                 (error "Unexpected property ~A at ~A"
                        (tool-key-name key) path))
                ((interop-json-object-p additional-properties)
                 (interop-validate-json-schema-value
                  additional-properties
                  child-value
                  (format nil "~A.~A" path (tool-key-name key)))))))))
      (let ((array-items (interop-json-array-list value)))
        (when (and array-items items)
          (loop :for item :in array-items
                :for index :from 0
                :do (interop-validate-json-schema-value
                     items item (format nil "~A[~D]" path index)))))))
  value)

(defun parse-and-validate-structured-output (text schema)
  "Decode TEXT as JSON and validate it against SCHEMA."
  (let ((normalized-schema (normalize-output-schema schema)))
    (when normalized-schema
      (handler-case
          (let ((decoded (api-json-decode text)))
            (interop-validate-json-schema-value normalized-schema decoded)
            decoded)
        (error (condition)
          (error 'structured-output-validation-error
                 :schema normalized-schema
                 :text text
                 :reason (format nil "~A" condition)))))))

(defun apply-output-schema-to-prompt-run-result (result output-schema)
  "Attach validated structured output to RESULT when OUTPUT-SCHEMA is present."
  (when (interop-output-schema-present-p output-schema)
    (setf (prompt-run-result-structured-output result)
          (parse-and-validate-structured-output
           (prompt-run-result-final-text result)
           output-schema)))
  result)

(defun apply-output-schema-to-pipeline-stage-result (stage-result output-schema)
  "Attach structured output to STAGE-RESULT when OUTPUT-SCHEMA is present."
  (when (and stage-result
             (interop-output-schema-present-p output-schema))
    (setf (pipeline-stage-result-parsed-output stage-result)
          (parse-and-validate-structured-output
           (or (pipeline-stage-result-final-text stage-result) "")
           output-schema)))
  stage-result)

;;; --------------------------------------------------------------------------
;;; Threads
;;; --------------------------------------------------------------------------

(defstruct interop-thread
  "Programmatic thread backed by a Clawmacs buffer/session."
  id
  buffer
  (ephemeral-p nil :type boolean)
  created-at
  updated-at
  current-turn-id
  last-result
  execution-owner
  registry-sequence
  (retained-resources-released-p nil :type boolean)
  (lock (bt:make-lock "interop-thread")))

(defstruct interop-client
  "Minimal in-process Lisp client for the Clawmacs interop surface.")

(defstruct interop-turn
  "Programmatic async turn state for the Clawmacs interop surface."
  id
  thread-id
  status
  input
  output-schema
  created-at
  updated-at
  started-at
  finished-at
  result
  error
  current-stream-state
  (interrupt-requested-p nil :type boolean)
  event-callback
  runner-thread
  (runner-installed-p nil :type boolean)
  (runner-finished-p nil :type boolean)
  terminal-sequence
  (retained-resources-released-p nil :type boolean)
  (lock (bt:make-lock "interop-turn")))

(defparameter *interop-protocol-version* 1
  "Current in-process/stdi interop protocol version.")

(defvar *interop-thread-table* (make-hash-table :test #'equal)
  "Live interop threads keyed by thread id.")

(defvar *interop-turn-table* (make-hash-table :test #'equal)
  "Live interop turns keyed by turn id.")

(defparameter *interop-terminal-turn-history-limit* 128
  "Maximum number of settled interop turns retained for read requests.")

(defparameter *interop-idle-thread-history-limit* 32
  "Maximum number of idle, unreferenced interop threads retained in memory.")

(defvar *interop-registry-sequence-counter* 0
  "Monotonic sequence for interop registry recency and terminal ordering.")

(defvar *interop-runtime-operations* (make-hash-table :test #'eq)
  "Exact construction and synchronous-run cleanup reservations.")

(defvar *interop-registry-lock* (bt:make-lock "interop-registry")
  "Lock protecting the live interop thread and turn registries.")

(defvar *interop-thread-start-lock* (bt:make-lock "interop-thread-start")
  "Single-flight lock for persistent session load, buffer creation, and publish.")

(defun active-interop-runtime-operation-count ()
  "Return interop construction/run tails still executing Clawmacs code."
  (bt:with-lock-held (*interop-registry-lock*)
    (hash-table-count *interop-runtime-operations*)))

(defun publish-interop-runtime-operation-under-admission (token operation)
  "Publish TOKEN while outer admission is held and inner locks are free."
  (bt:with-lock-held (*interop-registry-lock*)
    (setf (gethash token *interop-runtime-operations*) operation))
  token)

(defun settle-interop-runtime-operation (token)
  "Remove TOKEN only after all operation cleanup has completed."
  (when (bt:with-lock-held (*interop-registry-lock*)
          (gethash token *interop-runtime-operations*))
    (call-with-runtime-settlement-admission
     (lambda ()
       (bt:with-lock-held (*interop-registry-lock*)
         (remhash token *interop-runtime-operations*)))
     :operation "interop runtime cleanup"))
  token)

(defun interop-turn-terminal-status-p (status)
  "Return true when STATUS is terminal for an interop turn."
  (member status '(:succeeded :failed :interrupted) :test #'eq))

(defun settled-interop-turn-p (turn)
  "Return true when TURN is terminal and its managed runner has settled."
  (and (interop-turn-runner-installed-p turn)
       (interop-turn-runner-finished-p turn)
       (interop-turn-terminal-status-p (interop-turn-status turn))))

(defun note-interop-thread-use-locked (thread)
  "Record THREAD recency with the registry lock already held."
  (bt:with-lock-held ((interop-thread-lock thread))
    (setf (interop-thread-registry-sequence thread)
          (incf *interop-registry-sequence-counter*))))

(defun note-interop-turn-terminal-sequence-locked (turn)
  "Record TURN settlement order with the registry lock already held."
  (bt:with-lock-held ((interop-turn-lock turn))
    (when (and (settled-interop-turn-p turn)
               (null (interop-turn-terminal-sequence turn)))
      (setf (interop-turn-terminal-sequence turn)
            (incf *interop-registry-sequence-counter*)))))

(defun release-evicted-interop-turn-resources (turn)
  "Release heavy references from an evicted, settled TURN."
  (bt:with-lock-held ((interop-turn-lock turn))
    (when (settled-interop-turn-p turn)
      (setf (interop-turn-input turn) nil
            (interop-turn-output-schema turn) nil
            (interop-turn-result turn) nil
            (interop-turn-error turn) nil
            (interop-turn-current-stream-state turn) nil
            (interop-turn-event-callback turn) nil
            (interop-turn-runner-thread turn) nil
            (interop-turn-retained-resources-released-p turn) t)))
  turn)

(defun release-evicted-interop-thread-resources (thread)
  "Dispose the buffer and release result metadata held by evicted THREAD."
  (let ((buffer nil))
    (bt:with-lock-held ((interop-thread-lock thread))
      (unless (interop-thread-execution-owner thread)
        (setf buffer (interop-thread-buffer thread)
              (interop-thread-buffer thread) nil
              (interop-thread-last-result thread) nil
              (interop-thread-retained-resources-released-p thread) t)))
    ;; DISPOSE-BUFFER may cancel streams or tools.  It deliberately runs after
    ;; every interop lock has been released.
    (when buffer
      (handler-case
          (dispose-buffer buffer)
        (error (condition)
          (ignore-errors
            (cancel-buffer-runtime-operations buffer))
          (ignore-errors
            (file-debug-event "interop-evicted-thread-dispose-failed"
                              :thread-id (interop-thread-id thread)
                              :condition (format nil "~A" condition)))))))
  thread)

(defun prune-interop-registries (&key protected-thread)
  "Bound settled turn history and idle thread retention.

PROTECTED-THREAD is a newly returned object that must remain usable by its
caller.  Active turns, all threads referenced by retained turns, and threads
with an execution reservation are never candidates.  Removal happens under
the registry lock; resource release and buffer cancellation happen afterward."
  (let ((evicted-turns nil)
        (evicted-threads nil)
        (turn-limit (max 0 *interop-terminal-turn-history-limit*))
        (thread-limit (max 0 *interop-idle-thread-history-limit*)))
    (bt:with-lock-held (*interop-registry-lock*)
      (let ((terminal-turns nil))
        (maphash
         (lambda (_id turn)
           (declare (ignore _id))
           (note-interop-turn-terminal-sequence-locked turn)
           (bt:with-lock-held ((interop-turn-lock turn))
             (when (settled-interop-turn-p turn)
               (push turn terminal-turns))))
         *interop-turn-table*)
        (setf terminal-turns
              (sort terminal-turns #'>
                    :key (lambda (turn)
                           (or (interop-turn-terminal-sequence turn) 0))))
        (dolist (turn (nthcdr turn-limit terminal-turns))
          (when (eq turn
                    (gethash (interop-turn-id turn) *interop-turn-table*))
            (remhash (interop-turn-id turn) *interop-turn-table*)
            (push turn evicted-turns))))
      (let ((referenced-thread-ids
              (loop :for turn :being :the hash-values :of *interop-turn-table*
                    :collect (interop-turn-thread-id turn)))
            (idle-threads nil))
        (maphash
         (lambda (_id thread)
           (declare (ignore _id))
           (bt:with-lock-held ((interop-thread-lock thread))
             (when (and (not (eq thread protected-thread))
                        (null (interop-thread-execution-owner thread))
                        (not (member (interop-thread-id thread)
                                     referenced-thread-ids
                                     :test #'equal)))
               (push thread idle-threads))))
         *interop-thread-table*)
        (setf idle-threads
              (sort idle-threads #'>
                    :key (lambda (thread)
                           (or (interop-thread-registry-sequence thread) 0))))
        (dolist (thread (nthcdr thread-limit idle-threads))
          (when (eq thread
                    (gethash (interop-thread-id thread)
                             *interop-thread-table*))
            (remhash (interop-thread-id thread) *interop-thread-table*)
            (push thread evicted-threads)))))
    (dolist (turn evicted-turns)
      (release-evicted-interop-turn-resources turn))
    (dolist (thread evicted-threads)
      (release-evicted-interop-thread-resources thread))
    (values (length evicted-turns) (length evicted-threads))))

(defun clawmacs-system-version ()
  "Return the loaded Clawmacs system version string."
  (or (ignore-errors
        (asdf:component-version (asdf:find-system :clawmacs)))
      "0.1.0"))

(defun interop-generate-id (prefix)
  "Return a best-effort unique id with PREFIX."
  (format nil "~A-~36R-~36R"
          prefix
          (get-universal-time)
          (get-internal-real-time)))

(defun interop-thread-buffer-session-id (buffer)
  "Return BUFFER's persistent session id, or NIL."
  (let ((session (buffer-session buffer)))
    (and session (session-id session))))

(defun interop-thread-buffer-session-name (buffer)
  "Return BUFFER's session name, or BUFFER-NAME for ephemeral threads."
  (let ((session (buffer-session buffer)))
    (if session
        (session-name session)
        (buffer-name buffer))))

(defun interop-thread-summary (thread)
  "Return THREAD as a JSON-ready plist and its retained buffer.

Both values are NIL when bounded-history eviction won a concurrent race."
  (let (buffer ephemeral-p created-at updated-at current-turn-id)
    (bt:with-lock-held ((interop-thread-lock thread))
      (setf buffer (interop-thread-buffer thread)
            ephemeral-p (interop-thread-ephemeral-p thread)
            created-at (interop-thread-created-at thread)
            updated-at (interop-thread-updated-at thread)
            current-turn-id (interop-thread-current-turn-id thread)))
    (when buffer
      (let ((session (buffer-session buffer)))
        (values
         (list :id (interop-thread-id thread)
               :session-name (interop-thread-buffer-session-name buffer)
               :session-id (and session (session-id session))
               :display-name (and session
                                  (session-display-name-or-name session))
               :ephemeral-p ephemeral-p
               :working-directory
               (session-path-string (buffer-working-directory buffer))
               :agent-name (buffer-agent-name buffer)
               :provider (buffer-provider-override buffer)
               :model (buffer-model-override buffer)
               :created-at created-at
               :updated-at updated-at
               :current-turn-id current-turn-id)
         buffer)))))

(defun register-interop-thread (thread)
  "Register THREAD in the live interop registry."
  (call-with-runtime-admission
   (lambda ()
     (bt:with-lock-held (*interop-registry-lock*)
       (setf (gethash (interop-thread-id thread) *interop-thread-table*) thread)
       (note-interop-thread-use-locked thread)))
   :operation "interop thread publication")
  thread)

(defun register-interop-thread-if-absent (thread)
  "Publish THREAD unless its id already has a live object.
Return the canonical live object and true when THREAD won publication."
  (bt:with-lock-held (*interop-registry-lock*)
    (multiple-value-bind (existing present-p)
        (gethash (interop-thread-id thread) *interop-thread-table*)
      (if present-p
          (progn
            (note-interop-thread-use-locked existing)
            (values existing nil))
          (progn
            (setf (gethash (interop-thread-id thread) *interop-thread-table*)
                  thread)
            (note-interop-thread-use-locked thread)
            (values thread t))))))

(defun interop-thread-registry-snapshot ()
  "Return a stable list of the threads currently in the live registry."
  (bt:with-lock-held (*interop-registry-lock*)
    (loop :for thread :being :the hash-values :of *interop-thread-table*
          :collect thread)))

(defun make-interop-thread-from-buffer
    (buffer &key id ephemeral-p (register-p t))
  "Create an interop thread for BUFFER and optionally register it."
  (let* ((session-id (interop-thread-buffer-session-id buffer))
         (thread-id (or id session-id (interop-generate-id "thr")))
         (now (get-universal-time))
         (thread (make-interop-thread
                  :id thread-id
                  :buffer buffer
                  :ephemeral-p (if session-id nil ephemeral-p)
                  :created-at now
                  :updated-at now)))
    (if register-p
        (register-interop-thread thread)
        thread)))

(defun dispose-unregistered-interop-thread (thread)
  "Release runtime resources retained by unpublished THREAD."
  (let ((buffer (interop-thread-buffer thread)))
    (handler-case
        (dispose-buffer buffer)
      (error (condition)
        ;; A failed loser cleanup must not replace the canonical live object for
        ;; callers.  Retry cancellation because DISPOSE-BUFFER may have failed
        ;; after marking the buffer disposed.
        (ignore-errors
          (cancel-buffer-runtime-operations buffer))
        (ignore-errors
          (file-debug-event "interop-resume-loser-dispose-failed"
                            :condition (format nil "~A" condition))))))
  thread)

(defun resolve-interop-working-directory (cwd)
  "Normalize CWD for interop entrypoints."
  (normalize-session-working-directory (or cwd (truename "."))))

(defun start-interop-thread (&key session-name
                                  (agent-name *default-agent-name*)
                                  cwd
                                  ephemeral
                                  provider
                                  model
                                  think-level
                                  model-role
                                  service-tier
                                  package-names)
  "Create a programmatic thread and return its canonical live object.

Persistent starts are single-flight across session load, buffer construction,
and registry publication.  This prevents even constructor/autosave hooks from
writing one session through two candidate buffers."
  (let ((token (cons :interop-start (gensym "START-")))
        (result nil))
    (unwind-protect
         (setf
          result
          (multiple-value-bind (live loser)
              (call-with-runtime-admission
               (lambda ()
                 (publish-interop-runtime-operation-under-admission
                  token :thread-start)
                 (bt:with-lock-held (*interop-thread-start-lock*)
           (let* ((working-directory (resolve-interop-working-directory cwd))
                  (resolved-name (or session-name
                                     (interop-generate-id
                                      "clawmacs-thread")))
                  (persistent-p (not ephemeral))
                  (session (and persistent-p
                                (load-or-create-session
                                 resolved-name
                                 :working-directory working-directory)))
                  (session-id (and session (session-id session)))
                  (existing (and session-id
                                 (find-live-interop-thread session-id))))
             (if existing
                 (values existing nil)
                 (let* ((*buffer-system-prompt-display-enabled* nil)
                        (buffer
                          (make-chat-buffer
                           resolved-name
                           :agent-name agent-name
                           :working-directory working-directory
                           :session session
                           :session-persistence-mode
                           (if persistent-p :persistent :ephemeral))))
                   (maybe-apply-prompt-routing-overrides
                    buffer provider model think-level
                    :model-role model-role
                    :service-tier service-tier)
                   (when package-names
                     (setf (buffer-enabled-packages buffer)
                           (normalize-package-name-list package-names)))
                   (let ((candidate
                           (make-interop-thread-from-buffer
                            buffer :ephemeral-p ephemeral :register-p nil)))
                     (multiple-value-bind (canonical published-p)
                         (register-interop-thread-if-absent candidate)
                           (values canonical
                                   (unless published-p candidate)))))))))
               :operation "interop thread construction")
            ;; The exact operation token remains visible while cancellation
            ;; and pruning run outside the construction and registry locks.
            (when loser
              (dispose-unregistered-interop-thread loser))
            (prune-interop-registries :protected-thread live)
            live))
      (settle-interop-runtime-operation token))
    result))

(defun find-live-interop-thread (thread-id)
  "Return the live interop thread named THREAD-ID, or NIL."
  (and thread-id
       (bt:with-lock-held (*interop-registry-lock*)
         (let ((thread (gethash thread-id *interop-thread-table*)))
           (when thread
             (note-interop-thread-use-locked thread))
           thread))))

(defun resume-interop-thread-under-admission
    (thread-id agent-name)
  "Resolve THREAD-ID while runtime admission is already held.
Return the canonical thread and any unpublished loser requiring disposal."
  (let ((existing (find-live-interop-thread thread-id)))
    (if existing
        (values existing nil)
        (bt:with-lock-held (*interop-thread-start-lock*)
          ;; Recheck after acquiring the same construction lock used by START.
          (let ((canonical (find-live-interop-thread thread-id)))
            (if canonical
                (values canonical nil)
                (let* ((*buffer-system-prompt-display-enabled* nil)
                       (buffer
                         (load-session thread-id :agent-name agent-name)))
                  (if (null buffer)
                      (values nil nil)
                      (let ((candidate
                              (make-interop-thread-from-buffer
                               buffer :register-p nil)))
                        (multiple-value-bind (published published-p)
                            (register-interop-thread-if-absent candidate)
                          (values
                           published
                           (and (not published-p)
                                (not (eq buffer
                                         (interop-thread-buffer published)))
                                candidate))))))))))))

(defun resume-interop-thread (thread-id &key (agent-name *default-agent-name*))
  "Resume THREAD-ID from the live table or saved sessions."
  (let ((token (cons :interop-resume (gensym "RESUME-")))
        (result nil))
    (unwind-protect
         (setf
          result
          (multiple-value-bind (live loser)
              (call-with-runtime-admission
               (lambda ()
                 (publish-interop-runtime-operation-under-admission
                  token :thread-resume)
                 (resume-interop-thread-under-admission thread-id agent-name))
               :operation "interop thread resume")
            (when loser
              (dispose-unregistered-interop-thread loser))
            (when live
              (prune-interop-registries :protected-thread live))
            live))
      (settle-interop-runtime-operation token))
    result))

(defun fork-interop-thread (thread-id &key name)
  "Fork THREAD-ID into a new persistent thread."
  (let ((losers nil)
        (forked nil)
        (token (cons :interop-fork (gensym "FORK-OP-"))))
    (unwind-protect
         (progn
           (unwind-protect
                (setf
                 forked
                 (call-with-runtime-admission
                  (lambda ()
                    (publish-interop-runtime-operation-under-admission
                     token :thread-fork)
                    (multiple-value-bind (thread resume-loser)
                        (resume-interop-thread-under-admission
                         thread-id *default-agent-name*)
                      (when resume-loser
                        (push resume-loser losers))
                      (unless thread
                        (error "Unknown thread: ~A" thread-id))
                      (let ((owner (cons :interop-fork (gensym "FORK-"))))
                        (reserve-interop-thread-execution-under-admission
                         thread owner)
                        (unwind-protect
                             (let* ((buffer (interop-thread-buffer thread))
                                    (session
                                      (or (buffer-session buffer)
                                          (error
                                           "Thread ~A is ephemeral and cannot be forked yet."
                                           thread-id)))
                                    (leaf-id
                                      (or (session-current-leaf-id session)
                                          (session-last-tree-entry-id session)))
                                    (new-session
                                      (create-branched-session
                                       session leaf-id :name name))
                                    (*buffer-system-prompt-display-enabled* nil)
                                    (new-buffer
                                      (make-chat-buffer
                                       (session-name new-session)
                                       :agent-name (buffer-agent-name buffer)
                                       :working-directory
                                       (session-working-directory new-session)
                                       :session new-session
                                       :session-persistence-mode :persistent)))
                               (replace-buffer-history-with-serialized-messages
                                new-buffer
                                (session-active-branch-message-events
                                 new-session leaf-id)
                                :autosave-p nil)
                               (let ((candidate
                                       (make-interop-thread-from-buffer
                                        new-buffer :register-p nil)))
                                 (multiple-value-bind (canonical published-p)
                                     (register-interop-thread-if-absent
                                      candidate)
                                   (unless published-p
                                     (push candidate losers))
                                   canonical)))
                          (release-interop-thread-execution thread owner)))))
                  :operation "interop thread fork"))
             ;; Loser disposal runs without construction/registry/object locks,
             ;; while TOKEN still makes this cleanup visible to safe reload.
             (dolist (loser losers)
               (dispose-unregistered-interop-thread loser)))
           (prune-interop-registries :protected-thread forked)
           forked)
      (settle-interop-runtime-operation token))))

(defun list-interop-threads (&key cwd)
  "Return saved and live thread summaries, optionally filtered by CWD."
  (let* ((target-directory
           (and cwd
                (session-path-string
                 (resolve-interop-working-directory cwd))))
         (live-threads (interop-thread-registry-snapshot))
         (live nil)
         (records (or (list-saved-session-records) nil)))
    (dolist (thread live-threads)
      (let ((summary (interop-thread-summary thread)))
        (when (and summary
                   (or (null target-directory)
                       (string= target-directory
                                (getf summary :working-directory))))
          (push summary live))))
    (dolist (record records)
      (let* ((session-id (or (getf record :session-id)
                             (getf record :session-name)))
             (live-thread (and session-id
                               (find session-id live-threads
                                     :key #'interop-thread-id
                                     :test #'string=))))
        (unless live-thread
          (let ((summary
                  (list :id session-id
                        :session-name (getf record :session-name)
                        :session-id (getf record :session-id)
                        :display-name (getf record :display-name)
                        :ephemeral-p nil
                        :working-directory
                        (and (getf record :working-directory)
                             (session-path-string
                              (getf record :working-directory)))
                        :created-at (getf record :created-at)
                        :updated-at (getf record :updated-at)
                        :current-turn-id nil)))
            (when (or (null target-directory)
                      (string= target-directory
                               (getf summary :working-directory)))
              (push summary live))))))
    (sort live #'>
          :key (lambda (summary)
                 (or (getf summary :updated-at)
                     (getf summary :created-at)
                     0)))))

(defun buffer-message-item (message)
  "Return MESSAGE as a JSON-ready thread item plist."
  (list :role (string-downcase (symbol-name (message-sender message)))
        :text (message-text message)
        :timestamp (message-timestamp message)
        :metadata (message-metadata message)
        :entry-id (message-entry-id message)
        :parent-entry-id (message-parent-entry-id message)))

(defun interop-thread-items (buffer)
  "Return finalized BUFFER messages as a vector of thread items."
  (coerce
   (loop :for message := (buffer-first-message buffer)
           :then (message-next message)
         :while (and message
                     (not (eq message (buffer-input-message buffer))))
         :collect (buffer-message-item message))
   'vector))

(defun read-interop-thread (thread-id &key include-turns)
  "Return THREAD-ID as a structured read response."
  (let ((thread (or (resume-interop-thread thread-id)
                    (error "Unknown thread: ~A" thread-id))))
    (multiple-value-bind (summary buffer)
        (interop-thread-summary thread)
      (unless summary
        (error "Thread ~A was evicted while being read." thread-id))
      (append summary
              (when include-turns
                (list :items (interop-thread-items buffer)))))))

(defun interop-turn-status-string (status)
  "Return STATUS as the stable string used by the interop surface."
  (string-downcase (symbol-name status)))

(defun interop-turn-completed-p (turn)
  "Return true when TURN is no longer running."
  (interop-turn-terminal-status-p (interop-turn-status turn)))

(defun interop-turn-summary (turn)
  "Return TURN as a JSON-ready plist."
  (let (id thread-id status input created-at updated-at started-at finished-at
        interrupt-requested-p error result)
    (bt:with-lock-held ((interop-turn-lock turn))
      (setf id (interop-turn-id turn)
            thread-id (interop-turn-thread-id turn)
            status (interop-turn-status turn)
            input (interop-turn-input turn)
            created-at (interop-turn-created-at turn)
            updated-at (interop-turn-updated-at turn)
            started-at (interop-turn-started-at turn)
            finished-at (interop-turn-finished-at turn)
            interrupt-requested-p (interop-turn-interrupt-requested-p turn)
            error (interop-turn-error turn)
            result (interop-turn-result turn)))
    (append
     (list :id id
           :thread-id thread-id
           :status (interop-turn-status-string status)
           :input input
           :created-at created-at
           :updated-at updated-at
           :started-at started-at
           :finished-at finished-at
           :interrupt-requested-p interrupt-requested-p)
     (when error
       (list :error error))
     (when result
       (interop-run-result-data result thread-id id)))))

(defun register-interop-turn (turn)
  "Register TURN in the live interop turn registry."
  (call-with-runtime-admission
   (lambda ()
     (bt:with-lock-held (*interop-registry-lock*)
       (setf (gethash (interop-turn-id turn) *interop-turn-table*) turn)
       (note-interop-turn-terminal-sequence-locked turn)))
   :operation "interop turn publication")
  turn)

(defun interop-turn-registry-snapshot ()
  "Return a stable list of the turns currently in the live registry."
  (bt:with-lock-held (*interop-registry-lock*)
    (loop :for turn :being :the hash-values :of *interop-turn-table*
          :collect turn)))

(defun find-interop-turn (turn-id)
  "Return the live interop turn named TURN-ID, or NIL."
  (and turn-id
       (bt:with-lock-held (*interop-registry-lock*)
         (gethash turn-id *interop-turn-table*))))

(defun interop-turn-active-p (turn)
  "Return true when TURN has not reached a terminal status."
  (bt:with-lock-held ((interop-turn-lock turn))
    (not (interop-turn-completed-p turn))))

(defun reserve-interop-thread-execution-under-admission (thread owner)
  "Reserve THREAD for OWNER while runtime admission is already held."
  ;; Registry-before-thread is the pruning lock order.  The membership check
  ;; prevents a caller that fetched an old object just before eviction from
  ;; reserving and mutating its disposed buffer.
  (bt:with-lock-held (*interop-registry-lock*)
    (bt:with-lock-held ((interop-thread-lock thread))
      (unless (eq thread
                  (gethash (interop-thread-id thread) *interop-thread-table*))
        (error "Thread ~A is no longer live." (interop-thread-id thread)))
      (when (interop-thread-execution-owner thread)
        (error "Thread ~A already has an active turn."
               (interop-thread-id thread)))
      (setf (interop-thread-execution-owner thread) owner
            (interop-thread-registry-sequence thread)
            (incf *interop-registry-sequence-counter*))))
  thread)

(defun reserve-interop-thread-execution (thread owner)
  "Reserve THREAD's mutable buffer for OWNER or signal when it is busy."
  (call-with-runtime-admission
   (lambda ()
     (reserve-interop-thread-execution-under-admission thread owner))
   :operation "an interop thread execution"))

(defun release-interop-thread-execution (thread owner)
  "Release THREAD exactly when OWNER still holds its execution reservation."
  (bt:with-lock-held ((interop-thread-lock thread))
    (when (equal owner (interop-thread-execution-owner thread))
      (setf (interop-thread-execution-owner thread) nil)
      t)))

(defun interop-thread-execution-reserved-p (thread)
  "Return true when THREAD's mutable buffer is owned by a live execution."
  (bt:with-lock-held ((interop-thread-lock thread))
    (not (null (interop-thread-execution-owner thread)))))

(defun thread-has-active-interop-turn-p (thread-id)
  "Return true when THREAD-ID already has a live active turn."
  (let ((thread (find-live-interop-thread thread-id)))
    (and thread
         (interop-thread-execution-reserved-p thread))))

(defun ensure-thread-has-no-active-interop-turn (thread-id)
  "Signal an error when THREAD-ID already has an active turn."
  (when (thread-has-active-interop-turn-p thread-id)
    (error "Thread ~A already has an active turn." thread-id)))

(defun register-interop-turn-for-idle-thread (thread turn)
  "Reserve THREAD and register TURN as its sole execution.
The per-thread reservation covers synchronous and asynchronous runs alike."
  (let ((owner (interop-turn-id turn)))
    (reserve-interop-thread-execution thread owner)
    (handler-case
        (progn
          (bt:with-lock-held (*interop-registry-lock*)
            (when (gethash owner *interop-turn-table*)
              (error "Turn id collision: ~A" owner))
            (setf (gethash owner *interop-turn-table*) turn))
          turn)
      (error (condition)
        (release-interop-thread-execution thread owner)
        (error condition)))))

(defun call-interop-event-callback (callback event)
  "Send EVENT to CALLBACK when CALLBACK is non-nil."
  (when callback
    (funcall callback event)))

(defun call-interop-turn-event (turn event)
  "Send EVENT through TURN's bounded callback proxy when available."
  (call-interop-event-callback (interop-turn-event-callback turn) event))

(defun call-interop-turn-event-safely (turn event)
  "Send EVENT without allowing an external callback failure to escape.

This boundary is for terminal notifications from an async runner's own error
path.  The original failure has already settled TURN; a second failure from a
client callback must not unwind the managed runner or obscure cleanup."
  (handler-case
      (call-interop-turn-event turn event)
    (error (condition)
      (ignore-errors
        (file-debug-event "interop-event-callback-error"
                          :turn-id (interop-turn-id turn)
                          :event (getf event :event)
                          :condition (format nil "~A" condition)))
      nil)))

(defun interop-wrap-prompt-event (event thread-id turn-id)
  "Attach THREAD-ID and TURN-ID to EVENT."
  (append (list :event (getf event :event)
                :thread-id thread-id
                :turn-id turn-id)
          (loop :for (key value) :on event :by #'cddr
                :unless (eq key :event)
                  :append (list key value))))

(defun interop-run-result-data (result thread-id turn-id &optional buffer)
  "Return RESULT as a JSON-ready plist for interop responses."
  (list :thread-id thread-id
        :turn-id turn-id
        :final-response (prompt-run-result-final-text result)
        :structured-output (prompt-run-result-structured-output result)
        :usage (prompt-run-result-usage result)
        :stop-reason (prompt-run-result-stop-reason result)
        :agent-name (prompt-run-result-agent-name result)
        :provider (prompt-run-result-provider result)
        :model (prompt-run-result-model result)
        :think-level (prompt-run-result-think-level result)
        :service-tier (prompt-run-result-service-tier result)
        :items (interop-thread-items
                (or buffer
                    (interop-thread-buffer
                     (or (find-live-interop-thread thread-id)
                         (resume-interop-thread thread-id)))))))

(defun update-interop-turn-stream-state (turn state)
  "Record STATE as TURN's active provider stream."
  (let ((cancel-now-p nil))
    (bt:with-lock-held ((interop-turn-lock turn))
      (setf (interop-turn-current-stream-state turn) state
            (interop-turn-updated-at turn) (get-universal-time))
      (setf cancel-now-p (interop-turn-interrupt-requested-p turn)))
    (when cancel-now-p
      (cancel-stream-state state :stop-reason "cancelled")))
  state)

(defun finalize-interop-turn (turn status &key result error)
  "Finalize TURN with STATUS and optional RESULT or ERROR."
  (bt:with-lock-held ((interop-turn-lock turn))
    (setf (interop-turn-status turn) status
          (interop-turn-result turn) result
          (interop-turn-error turn) error
          (interop-turn-current-stream-state turn) nil
          (interop-turn-updated-at turn) (get-universal-time)
          (interop-turn-finished-at turn) (get-universal-time)))
  turn)

(defun finish-interop-turn-runner (turn)
  "Publish TURN settlement and prune history under settlement admission."
  (call-with-runtime-settlement-admission
   (lambda ()
     (bt:with-lock-held (*interop-registry-lock*)
       (bt:with-lock-held ((interop-turn-lock turn))
         (setf (interop-turn-runner-finished-p turn) t
               (interop-turn-current-stream-state turn) nil
               ;; Client callbacks can retain transports and application state;
               ;; terminal status reads do not need them.
               (interop-turn-event-callback turn) nil)
         (when (and (interop-turn-terminal-status-p
                     (interop-turn-status turn))
                    (null (interop-turn-terminal-sequence turn)))
           (setf (interop-turn-terminal-sequence turn)
                 (incf *interop-registry-sequence-counter*)))))
     ;; TURN is now eligible.  Evicted resource cleanup happens outside the
     ;; inner registry/object locks, but before outer admission reopens.
     (prune-interop-registries))
   :operation "interop turn settlement")
  turn)

(defun read-interop-turn (turn-id)
  "Return TURN-ID as a structured read response."
  (let ((turn (or (find-interop-turn turn-id)
                  (error "Unknown turn: ~A" turn-id))))
    (interop-turn-summary turn)))

(defun interrupt-interop-turn (turn-id)
  "Request cancellation of TURN-ID and return its updated summary."
  (let ((turn (or (find-interop-turn turn-id)
                  (error "Unknown turn: ~A" turn-id)))
        (should-notify-p nil)
        (thread-id nil)
        (stream-state nil))
    (bt:with-lock-held ((interop-turn-lock turn))
      (setf thread-id (interop-turn-thread-id turn))
      (unless (interop-turn-completed-p turn)
        (unless (interop-turn-interrupt-requested-p turn)
          (setf should-notify-p t))
        (setf (interop-turn-interrupt-requested-p turn) t
              (interop-turn-updated-at turn) (get-universal-time)
              stream-state (interop-turn-current-stream-state turn))))
    ;; Cancellation may close streams, invoke callbacks, or block briefly.  It
    ;; must never run while holding the turn lock because those callbacks may
    ;; read the same turn.
    (when stream-state
      (cancel-stream-state stream-state :stop-reason "cancelled"))
    (when should-notify-p
      (call-interop-turn-event
       turn
       (list :event "turn.interrupted"
             :thread-id thread-id
             :turn-id (interop-turn-id turn))))
    (interop-turn-summary turn)))

(defun append-interop-thread-input (buffer prompt)
  "Append PROMPT as the next finalized user turn in BUFFER."
  (set-message-text (buffer-input-message buffer) prompt)
  (buffer-finalize-input buffer)
  buffer)

(defun note-interop-thread-turn-started (thread turn-id)
  "Publish TURN-ID as THREAD's current execution metadata."
  (bt:with-lock-held ((interop-thread-lock thread))
    (setf (interop-thread-current-turn-id thread) turn-id
          (interop-thread-updated-at thread) (get-universal-time)))
  thread)

(defun note-interop-thread-turn-completed (thread result)
  "Publish RESULT as THREAD's latest completed execution metadata."
  (bt:with-lock-held ((interop-thread-lock thread))
    (setf (interop-thread-last-result thread) result
          (interop-thread-updated-at thread) (get-universal-time)))
  thread)

(defun make-cancelled-interop-run-result (buffer prompt)
  "Return the terminal prompt result for a cooperatively interrupted turn."
  (let ((session (buffer-session buffer)))
    (make-prompt-run-result
     :prompt prompt
     :final-text ""
     :tool-events nil
     :reasoning-blocks nil
     :agent-name (buffer-agent-name buffer)
     :provider (buffer-provider-override buffer)
     :model (buffer-model-override buffer)
     :think-level (buffer-think-level-override buffer)
     :service-tier (buffer-service-tier-override buffer)
     :iterations 0
     :stop-reason "cancelled"
     :usage nil
     :session-name (and session (session-name session))
     :session-id (and session (session-id session)))))

(defun run-interop-thread* (thread prompt
                            &key provider model think-level
                              model-role service-tier
                              package-names
                              (max-tool-iterations *prompt-max-tool-iterations*)
                              output-schema
                              event-callback
                              event-callback-dispatched-p
                              turn-id
                              stream-state-callback
                              cancel-requested-p)
  "Run PROMPT on reserved THREAD and return RESULT and its turn id."
  (when (blank-string-p prompt)
    (error "Thread input must be non-empty"))
  (let* ((event-callback
           (if event-callback-dispatched-p
               event-callback
               (make-bounded-runtime-callback
                event-callback
                :label (format nil "interop thread ~A"
                               (interop-thread-id thread)))))
         (thread-id (interop-thread-id thread))
         (buffer (interop-thread-buffer thread))
         (effective-turn-id (or turn-id
                                (interop-generate-id "turn"))))
    (note-interop-thread-turn-started thread effective-turn-id)
    (call-interop-event-callback
     event-callback
     (list :event "turn.started"
           :thread-id thread-id
           :turn-id effective-turn-id
           :input prompt))
    (append-interop-thread-input buffer prompt)
    (maybe-apply-prompt-routing-overrides buffer provider model think-level
                                          :model-role model-role
                                          :service-tier service-tier)
    (when package-names
      (setf (buffer-enabled-packages buffer)
            (normalize-package-name-list package-names)))
    (let* ((wrapped-callback
             (and event-callback
                  (lambda (event)
                    (call-interop-event-callback
                     event-callback
                     (interop-wrap-prompt-event
                      event thread-id effective-turn-id)))))
           (result
             (handler-case
                 (run-prompt-with-buffer
                  buffer prompt nil
                  max-tool-iterations
                  nil nil
                  :event-callback wrapped-callback
                  :output-schema output-schema
                  :stream-state-callback stream-state-callback
                  :cancel-requested-p cancel-requested-p)
               (prompt-run-cancelled ()
                 (make-cancelled-interop-run-result buffer prompt)))))
      (note-interop-thread-turn-completed thread result)
      (call-interop-event-callback
       event-callback
       (append (list :event "turn.completed"
                     :thread-id thread-id
                     :turn-id effective-turn-id)
               (interop-run-result-data
                result thread-id effective-turn-id buffer)))
      (values result effective-turn-id))))

(defun run-interop-thread (thread-id prompt
                           &key provider model think-level
                             model-role service-tier
                             package-names
                             (max-tool-iterations *prompt-max-tool-iterations*)
                             output-schema
                             event-callback
                             turn-id
                             stream-state-callback)
  "Run PROMPT on THREAD-ID and return a prompt result."
  (let* ((thread (or (resume-interop-thread thread-id)
                     (error "Unknown thread: ~A" thread-id)))
         (effective-turn-id (or turn-id (interop-generate-id "turn")))
         (token (cons :synchronous-interop (gensym "RUN-"))))
    (unwind-protect
         (progn
           (call-with-runtime-admission
            (lambda ()
              (publish-interop-runtime-operation-under-admission
               token :synchronous-run)
              (reserve-interop-thread-execution-under-admission
               thread effective-turn-id))
            :operation "a synchronous interop run")
           (unwind-protect
                (run-interop-thread*
                 thread
                 prompt
                 :provider provider
                 :model model
                 :think-level think-level
                 :model-role model-role
                 :service-tier service-tier
                 :package-names package-names
                 :max-tool-iterations max-tool-iterations
                 :output-schema output-schema
                 :event-callback event-callback
                 :turn-id effective-turn-id
                 :stream-state-callback stream-state-callback)
             (unwind-protect
                  (release-interop-thread-execution thread effective-turn-id)
               (prune-interop-registries :protected-thread thread))))
      (settle-interop-runtime-operation token))))

(defun make-interop-turn-runner-thread (function name)
  "Start FUNCTION as the managed runner named NAME."
  (bt:make-thread function :name name))

(defun start-interop-turn (thread-id prompt
                           &key provider model think-level
                             model-role service-tier
                             package-names
                             (max-tool-iterations *prompt-max-tool-iterations*)
                             output-schema
                             event-callback)
  "Start an async turn for THREAD-ID and return its live turn object."
  (when (blank-string-p prompt)
    (error "Turn input must be non-empty"))
  (let* ((thread (or (resume-interop-thread thread-id)
                     (error "Unknown thread: ~A" thread-id)))
         (canonical-thread-id (interop-thread-id thread))
         (turn-id (interop-generate-id "turn"))
         (now (get-universal-time))
         (event-callback
           (make-bounded-runtime-callback
            event-callback
            :label (format nil "interop turn ~A" turn-id)))
         (turn (make-interop-turn
                :id turn-id
                :thread-id canonical-thread-id
                :status :queued
                :input prompt
                :output-schema output-schema
                :created-at now
                :updated-at now
                :event-callback event-callback))
         (runner-start-gate
           (bt:make-semaphore :name "interop-turn-runner-start")))
    (register-interop-turn-for-idle-thread thread turn)
    (handler-case
        (let ((runner
                (make-interop-turn-runner-thread
                 (lambda ()
                   ;; Publish the runner object before application work can
                   ;; settle, so terminal pruning cannot race its installation.
                   (bt:wait-on-semaphore runner-start-gate)
                   (unwind-protect
                        (progn
                          (bt:with-lock-held ((interop-turn-lock turn))
                            (setf (interop-turn-status turn) :running
                                  (interop-turn-started-at turn)
                                  (get-universal-time)
                                  (interop-turn-updated-at turn)
                                  (get-universal-time)))
                          (handler-case
                              (let ((result
                                      (run-interop-thread*
                                       thread
                                       prompt
                                       :provider provider
                                       :model model
                                       :think-level think-level
                                       :model-role model-role
                                       :service-tier service-tier
                                       :package-names package-names
                                       :max-tool-iterations max-tool-iterations
                                       :output-schema output-schema
                                       :event-callback event-callback
                                       :event-callback-dispatched-p t
                                       :turn-id turn-id
                                       :stream-state-callback
                                       (lambda (state)
                                         (update-interop-turn-stream-state
                                          turn state))
                                       :cancel-requested-p
                                       (lambda ()
                                         (bt:with-lock-held
                                             ((interop-turn-lock turn))
                                           (interop-turn-interrupt-requested-p
                                            turn))))))
                                (if (string=
                                     "cancelled"
                                     (or (prompt-run-result-stop-reason result)
                                         ""))
                                    (finalize-interop-turn
                                     turn :interrupted :result result)
                                    (finalize-interop-turn
                                     turn :succeeded :result result)))
                            (error (condition)
                              (finalize-interop-turn
                               turn :failed :error (format nil "~A" condition))
                              (call-interop-turn-event-safely
                               turn
                               (list :event "turn.failed"
                                     :thread-id canonical-thread-id
                                     :turn-id turn-id
                                     :error (format nil "~A" condition))))))
                     (unwind-protect
                          (release-interop-thread-execution thread turn-id)
                       (finish-interop-turn-runner turn))))
                 (format nil "clawmacs-interop-turn-~A" turn-id))))
          (bt:with-lock-held ((interop-turn-lock turn))
            (unless (interop-turn-retained-resources-released-p turn)
              (setf (interop-turn-runner-thread turn) runner
                    (interop-turn-runner-installed-p turn) t)))
          (bt:signal-semaphore runner-start-gate))
      (error (condition)
        ;; The turn is already visible, so publish a terminal failure instead
        ;; of leaving a permanently queued registry entry.
        (bt:with-lock-held ((interop-turn-lock turn))
          (setf (interop-turn-runner-installed-p turn) t))
        (finalize-interop-turn turn :failed
                               :error (format nil "~A" condition))
        (release-interop-thread-execution thread turn-id)
        (ignore-errors
          (call-interop-turn-event
           turn
           (list :event "turn.failed"
                 :thread-id canonical-thread-id
                 :turn-id turn-id
                 :error (format nil "~A" condition))))
        (finish-interop-turn-runner turn)
        (error condition)))
    turn))

;;; --------------------------------------------------------------------------
;;; Request Handling / Local Client
;;; --------------------------------------------------------------------------

(defun interop-request-param (params key &optional default)
  "Return KEY from JSON-like PARAMS, or DEFAULT."
  (let ((alist (ignore-errors (tool-args-alist params))))
    (if alist
        (loop :for (param-key . value) :in alist
              :when (tool-key= param-key key)
                :return value
              :finally (return default))
        default)))

(defun interop-response-server-info ()
  "Return server metadata for initialize responses."
  (list :protocol-version *interop-protocol-version*
        :server-info (list :name "clawmacs-app-server"
                           :version (clawmacs-system-version))))

(defun handle-interop-request (request &key event-callback)
  "Handle one interop REQUEST alist and return a result plist."
  (let* ((method (or (interop-request-param request :method)
                     (error "Interop request is missing :method")))
         (params (or (interop-request-param request :params) nil)))
    (cond
      ((string= method "initialize")
       (interop-response-server-info))
      ((string= method "thread.start")
       (interop-thread-summary
        (start-interop-thread
         :session-name (interop-request-param params :session-name)
         :agent-name (or (interop-request-param params :agent-name)
                         *default-agent-name*)
         :cwd (interop-request-param params :cwd)
         :ephemeral (not (null (interop-request-param params :ephemeral)))
         :provider (interop-request-param params :model-provider)
         :model (interop-request-param params :model)
         :think-level (or (interop-request-param params :effort)
                          (interop-request-param params :think-level))
         :model-role (interop-request-param params :model-role)
         :service-tier (interop-request-param params :service-tier)
         :package-names (interop-request-param params :packages))))
      ((string= method "thread.list")
       (list :threads
             (coerce (list-interop-threads
                      :cwd (interop-request-param params :cwd))
                     'vector)))
      ((string= method "thread.resume")
       (interop-thread-summary
        (resume-interop-thread
         (or (interop-request-param params :thread-id)
             (interop-request-param params :id))
         :agent-name (or (interop-request-param params :agent-name)
                         *default-agent-name*))))
      ((string= method "thread.fork")
       (interop-thread-summary
        (fork-interop-thread
         (or (interop-request-param params :thread-id)
             (interop-request-param params :id))
         :name (interop-request-param params :name))))
      ((string= method "thread.read")
       (read-interop-thread
        (or (interop-request-param params :thread-id)
            (interop-request-param params :id))
        :include-turns (not (null
                             (interop-request-param params :include-turns)))))
      ((string= method "turn.start")
       (interop-turn-summary
        (start-interop-turn
         (or (interop-request-param params :thread-id)
             (interop-request-param params :id))
         (let ((input (or (interop-request-param params :input)
                          (interop-request-param params :prompt)
                          (interop-request-param params :text)
                          "")))
           (if (stringp input)
               input
               (princ-to-string input)))
         :provider (interop-request-param params :model-provider)
         :model (interop-request-param params :model)
         :think-level (or (interop-request-param params :effort)
                          (interop-request-param params :think-level))
         :model-role (interop-request-param params :model-role)
         :service-tier (interop-request-param params :service-tier)
         :package-names (interop-request-param params :packages)
         :max-tool-iterations
         (or (interop-request-param params :max-tool-iterations)
             *prompt-max-tool-iterations*)
         :output-schema (interop-request-param params :output-schema)
         :event-callback event-callback)))
      ((string= method "turn.read")
       (read-interop-turn
        (or (interop-request-param params :turn-id)
            (interop-request-param params :id))))
      ((string= method "turn.interrupt")
       (interrupt-interop-turn
        (or (interop-request-param params :turn-id)
            (interop-request-param params :id))))
      ((string= method "thread.run")
       (let* ((thread-id
                (or (interop-request-param params :thread-id)
                    (interop-request-param params :id)))
              (input (or (interop-request-param params :input)
                         (interop-request-param params :prompt)
                         (interop-request-param params :text)
                         "")))
         (multiple-value-bind (result turn-id)
             (run-interop-thread
              thread-id
              (if (stringp input)
                  input
                  (princ-to-string input))
              :provider (interop-request-param params :model-provider)
              :model (interop-request-param params :model)
              :think-level (or (interop-request-param params :effort)
                               (interop-request-param params :think-level))
              :model-role (interop-request-param params :model-role)
              :service-tier (interop-request-param params :service-tier)
              :package-names (interop-request-param params :packages)
              :max-tool-iterations
              (or (interop-request-param params :max-tool-iterations)
                  *prompt-max-tool-iterations*)
              :output-schema (interop-request-param params :output-schema)
              :event-callback event-callback)
           (interop-run-result-data result thread-id turn-id))))
      (t
       (error "Unknown interop method: ~A" method)))))

(defun make-interop-local-client ()
  "Return a minimal in-process Lisp interop client."
  (make-interop-client))

(defun interop-client-call (_client method &optional params &key event-callback)
  "Call METHOD with PARAMS through the in-process interop dispatcher."
  (declare (ignore _client))
  (handle-interop-request
   (list :method method :params params)
   :event-callback event-callback))

;;; --------------------------------------------------------------------------
;;; Stdio App Server
;;; --------------------------------------------------------------------------

(defun interop-write-jsonl-message (message &optional (stream *standard-output*))
  "Write MESSAGE as one JSONL line to STREAM."
  (write-string (api-json-encode (interop-json-ready message)) stream)
  (terpri stream)
  (finish-output stream))

(defun interop-request-id (request)
  "Return REQUEST's id field, or NIL."
  (interop-request-param request :id))

(defun interop-success-response (request-id result)
  "Return a JSON-ready success response."
  `((:id . ,request-id)
    (:result . ,result)))

(defun interop-error-response (request-id condition)
  "Return a JSON-ready error response."
  `((:id . ,request-id)
    (:error . ((:message . ,(format nil "~A" condition))))))

(defun clawmacs-app-server-main ()
  "Run the Clawmacs stdio app-server until standard input reaches EOF."
  (parse-clawmacs-args)
  (initialize-clawmacs-runtime)
  (reset-interaction-state)
  (ensure-prompt-workspace-project)
  (let ((write-lock (bt:make-lock "interop-jsonl-output")))
    (labels ((safe-write (message)
               (bt:with-lock-held (write-lock)
                 (interop-write-jsonl-message message))))
      (loop :for line := (read-line *standard-input* nil nil)
            :while line
            :for trimmed := (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         line)
            :unless (blank-string-p trimmed)
              :do (let* ((request (api-json-decode trimmed))
                         (request-id (interop-request-id request))
                         (event-callback
                           (lambda (event)
                             (safe-write
                              `((:event . ,(or (interop-request-param event :event)
                                               "event"))
                                (:request-id . ,request-id)
                                (:payload . ,event))))))
                    (handler-case
                        (safe-write
                         (interop-success-response
                          request-id
                          (handle-interop-request
                           request
                           :event-callback event-callback)))
                      (error (condition)
                        (safe-write
                         (interop-error-response request-id condition))))))))
  (uiop:quit 0))
