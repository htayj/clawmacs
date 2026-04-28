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
  last-result)

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
  (lock (bt:make-lock "interop-turn")))

(defparameter *interop-protocol-version* 1
  "Current in-process/stdi interop protocol version.")

(defvar *interop-thread-table* (make-hash-table :test #'equal)
  "Live interop threads keyed by thread id.")

(defvar *interop-turn-table* (make-hash-table :test #'equal)
  "Live interop turns keyed by turn id.")

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
  "Return THREAD as a JSON-ready plist."
  (let* ((buffer (interop-thread-buffer thread))
         (session (buffer-session buffer)))
    (list :id (interop-thread-id thread)
          :session-name (interop-thread-buffer-session-name buffer)
          :session-id (and session (session-id session))
          :display-name (and session (session-display-name-or-name session))
          :ephemeral-p (interop-thread-ephemeral-p thread)
          :working-directory
          (session-path-string (buffer-working-directory buffer))
          :agent-name (buffer-agent-name buffer)
          :provider (buffer-provider-override buffer)
          :model (buffer-model-override buffer)
          :created-at (interop-thread-created-at thread)
          :updated-at (interop-thread-updated-at thread)
          :current-turn-id (interop-thread-current-turn-id thread))))

(defun register-interop-thread (thread)
  "Register THREAD in the live interop registry."
  (setf (gethash (interop-thread-id thread) *interop-thread-table*) thread)
  thread)

(defun make-interop-thread-from-buffer (buffer &key id ephemeral-p)
  "Create and register a live interop thread for BUFFER."
  (let* ((session-id (interop-thread-buffer-session-id buffer))
         (thread-id (or id session-id (interop-generate-id "thr")))
         (now (get-universal-time))
         (thread (make-interop-thread
                  :id thread-id
                  :buffer buffer
                  :ephemeral-p (if session-id nil ephemeral-p)
                  :created-at now
                  :updated-at now)))
    (register-interop-thread thread)))

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
  "Create a new programmatic thread and return its live thread object."
  (let* ((working-directory (resolve-interop-working-directory cwd))
         (resolved-name (or session-name
                            (interop-generate-id "clawmacs-thread")))
         (persistent-p (not ephemeral))
         (session (and persistent-p
                       (load-or-create-session
                        resolved-name
                        :working-directory working-directory)))
         (*buffer-system-prompt-display-enabled* nil)
         (buffer (make-chat-buffer
                  resolved-name
                  :agent-name agent-name
                  :working-directory working-directory
                  :session session
                  :session-persistence-mode
                  (if persistent-p :persistent :ephemeral))))
    (maybe-apply-prompt-routing-overrides buffer provider model think-level
                                          :model-role model-role
                                          :service-tier service-tier)
    (when package-names
      (setf (buffer-enabled-packages buffer)
            (normalize-package-name-list package-names)))
    (make-interop-thread-from-buffer buffer :ephemeral-p ephemeral)))

(defun find-live-interop-thread (thread-id)
  "Return the live interop thread named THREAD-ID, or NIL."
  (and thread-id
       (gethash thread-id *interop-thread-table*)))

(defun resume-interop-thread (thread-id &key (agent-name *default-agent-name*))
  "Resume THREAD-ID from the live table or saved sessions."
  (or (find-live-interop-thread thread-id)
      (let* ((*buffer-system-prompt-display-enabled* nil)
             (buffer (load-session thread-id :agent-name agent-name)))
        (and buffer
             (make-interop-thread-from-buffer buffer)))))

(defun fork-interop-thread (thread-id &key name)
  "Fork THREAD-ID into a new persistent thread."
  (let* ((thread (or (resume-interop-thread thread-id)
                     (error "Unknown thread: ~A" thread-id)))
         (buffer (interop-thread-buffer thread))
         (session (or (buffer-session buffer)
                      (error "Thread ~A is ephemeral and cannot be forked yet."
                             thread-id)))
         (leaf-id (or (session-current-leaf-id session)
                      (session-last-tree-entry-id session)))
         (new-session (create-branched-session session leaf-id :name name))
         (*buffer-system-prompt-display-enabled* nil)
         (new-buffer (make-chat-buffer
                      (session-name new-session)
                      :agent-name (buffer-agent-name buffer)
                      :working-directory (session-working-directory new-session)
                      :session new-session
                      :session-persistence-mode :persistent)))
    (replace-buffer-history-with-serialized-messages
     new-buffer
     (session-active-branch-message-events new-session leaf-id)
     :autosave-p nil)
    (make-interop-thread-from-buffer new-buffer)))

(defun list-interop-threads (&key cwd)
  "Return saved and live thread summaries, optionally filtered by CWD."
  (let* ((target-directory
           (and cwd
                (session-path-string
                 (resolve-interop-working-directory cwd))))
         (live nil)
         (records (or (list-saved-session-records) nil)))
    (maphash (lambda (_id thread)
               (declare (ignore _id))
               (let ((summary (interop-thread-summary thread)))
                 (when (or (null target-directory)
                           (string= target-directory
                                    (getf summary :working-directory)))
                   (push summary live))))
             *interop-thread-table*)
    (dolist (record records)
      (let* ((session-id (or (getf record :session-id)
                             (getf record :session-name)))
             (live-thread (and session-id
                               (find-live-interop-thread session-id))))
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
  (let* ((thread (or (resume-interop-thread thread-id)
                     (error "Unknown thread: ~A" thread-id)))
         (summary (interop-thread-summary thread)))
    (append summary
            (when include-turns
              (list :items
                    (interop-thread-items
                     (interop-thread-buffer thread)))))))

(defun interop-turn-status-string (status)
  "Return STATUS as the stable string used by the interop surface."
  (string-downcase (symbol-name status)))

(defun interop-turn-completed-p (turn)
  "Return true when TURN is no longer running."
  (member (interop-turn-status turn)
          '(:succeeded :failed :interrupted)
          :test #'eq))

(defun interop-turn-summary (turn)
  "Return TURN as a JSON-ready plist."
  (bt:with-lock-held ((interop-turn-lock turn))
    (append
     (list :id (interop-turn-id turn)
           :thread-id (interop-turn-thread-id turn)
           :status (interop-turn-status-string (interop-turn-status turn))
           :input (interop-turn-input turn)
           :created-at (interop-turn-created-at turn)
           :updated-at (interop-turn-updated-at turn)
           :started-at (interop-turn-started-at turn)
           :finished-at (interop-turn-finished-at turn)
           :interrupt-requested-p (interop-turn-interrupt-requested-p turn))
     (when (interop-turn-error turn)
       (list :error (interop-turn-error turn)))
     (when (interop-turn-result turn)
       (interop-run-result-data
        (interop-turn-result turn)
        (interop-turn-thread-id turn)
        (interop-turn-id turn))))))

(defun register-interop-turn (turn)
  "Register TURN in the live interop turn registry."
  (setf (gethash (interop-turn-id turn) *interop-turn-table*) turn)
  turn)

(defun find-interop-turn (turn-id)
  "Return the live interop turn named TURN-ID, or NIL."
  (and turn-id
       (gethash turn-id *interop-turn-table*)))

(defun thread-has-active-interop-turn-p (thread-id)
  "Return true when THREAD-ID already has a live active turn."
  (loop :for turn :being :the hash-values :of *interop-turn-table*
        :thereis
        (and (string= thread-id (interop-turn-thread-id turn))
             (not (interop-turn-completed-p turn)))))

(defun ensure-thread-has-no-active-interop-turn (thread-id)
  "Signal an error when THREAD-ID already has an active turn."
  (when (thread-has-active-interop-turn-p thread-id)
    (error "Thread ~A already has an active turn." thread-id)))

(defun call-interop-event-callback (callback event)
  "Send EVENT to CALLBACK when CALLBACK is non-nil."
  (when callback
    (funcall callback event)))

(defun call-interop-turn-event (turn event)
  "Send EVENT through TURN's event callback when available."
  (call-interop-event-callback (interop-turn-event-callback turn) event))

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
        (thread-id nil))
    (bt:with-lock-held ((interop-turn-lock turn))
      (setf thread-id (interop-turn-thread-id turn))
      (unless (interop-turn-completed-p turn)
        (unless (interop-turn-interrupt-requested-p turn)
          (setf should-notify-p t))
        (setf (interop-turn-interrupt-requested-p turn) t
              (interop-turn-updated-at turn) (get-universal-time))
        (when (interop-turn-current-stream-state turn)
          (cancel-stream-state (interop-turn-current-stream-state turn)
                               :stop-reason "cancelled"))))
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

(defun run-interop-thread* (thread prompt
                            &key provider model think-level
                              model-role service-tier
                              package-names
                              (max-tool-iterations *prompt-max-tool-iterations*)
                              auto-approve-tools-p
                              output-schema
                              event-callback
                              turn-id
                              stream-state-callback)
  "Run PROMPT on live THREAD and return a prompt result."
  (when (blank-string-p prompt)
    (error "Thread input must be non-empty"))
  (let* ((thread-id (interop-thread-id thread))
         (buffer (interop-thread-buffer thread))
         (effective-turn-id (or turn-id
                                (interop-generate-id "turn"))))
    (setf (interop-thread-current-turn-id thread) effective-turn-id
          (interop-thread-updated-at thread) (get-universal-time))
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
             (run-prompt-with-buffer
              buffer prompt nil
              max-tool-iterations auto-approve-tools-p
              nil nil
              :event-callback wrapped-callback
              :output-schema output-schema
              :stream-state-callback stream-state-callback)))
      (setf (interop-thread-last-result thread) result
            (interop-thread-updated-at thread) (get-universal-time))
      (call-interop-event-callback
       event-callback
       (append (list :event "turn.completed"
                     :thread-id thread-id
                     :turn-id effective-turn-id)
               (interop-run-result-data
                result thread-id effective-turn-id buffer)))
      result)))

(defun run-interop-thread (thread-id prompt
                           &key provider model think-level
                             model-role service-tier
                             package-names
                             (max-tool-iterations *prompt-max-tool-iterations*)
                             auto-approve-tools-p
                             output-schema
                             event-callback
                             turn-id
                             stream-state-callback)
  "Run PROMPT on THREAD-ID and return a prompt result."
  (run-interop-thread*
   (or (resume-interop-thread thread-id)
       (error "Unknown thread: ~A" thread-id))
   prompt
   :provider provider
   :model model
   :think-level think-level
   :model-role model-role
   :service-tier service-tier
   :package-names package-names
   :max-tool-iterations max-tool-iterations
   :auto-approve-tools-p auto-approve-tools-p
   :output-schema output-schema
   :event-callback event-callback
   :turn-id turn-id
   :stream-state-callback stream-state-callback))

(defun start-interop-turn (thread-id prompt
                           &key provider model think-level
                             model-role service-tier
                             package-names
                             (max-tool-iterations *prompt-max-tool-iterations*)
                             auto-approve-tools-p
                             output-schema
                             event-callback)
  "Start an async turn for THREAD-ID and return its live turn object."
  (when (blank-string-p prompt)
    (error "Turn input must be non-empty"))
  (let* ((thread (or (resume-interop-thread thread-id)
                     (error "Unknown thread: ~A" thread-id)))
         (turn-id (interop-generate-id "turn"))
         (now (get-universal-time))
         (turn (make-interop-turn
                :id turn-id
                :thread-id thread-id
                :status :queued
                :input prompt
                :output-schema output-schema
                :created-at now
                :updated-at now
                :event-callback event-callback)))
    (ensure-thread-has-no-active-interop-turn thread-id)
    (register-interop-turn turn)
    (setf (interop-turn-runner-thread turn)
          (bt:make-thread
           (lambda ()
             (bt:with-lock-held ((interop-turn-lock turn))
               (setf (interop-turn-status turn) :running
                     (interop-turn-started-at turn) (get-universal-time)
                     (interop-turn-updated-at turn) (get-universal-time)))
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
                          :auto-approve-tools-p auto-approve-tools-p
                          :output-schema output-schema
                          :event-callback event-callback
                          :turn-id turn-id
                          :stream-state-callback
                          (lambda (state)
                            (update-interop-turn-stream-state turn state)))))
                   (if (string= "cancelled"
                                (or (prompt-run-result-stop-reason result) ""))
                       (finalize-interop-turn turn :interrupted :result result)
                       (finalize-interop-turn turn :succeeded :result result)))
               (error (condition)
                 (finalize-interop-turn turn :failed
                                        :error (format nil "~A" condition))
                 (call-interop-turn-event
                  turn
                  (list :event "turn.failed"
                        :thread-id thread-id
                        :turn-id turn-id
                        :error (format nil "~A" condition))))))
           :name (format nil "clawmacs-interop-turn-~A" turn-id)))
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
         :auto-approve-tools-p
         (not (null (interop-request-param params :auto-approve-tools-p)))
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
         (interop-run-result-data
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
           :auto-approve-tools-p
           (not (null (interop-request-param params :auto-approve-tools-p)))
           :output-schema (interop-request-param params :output-schema)
           :event-callback event-callback)
          thread-id
          (interop-thread-current-turn-id
           (or (find-live-interop-thread thread-id)
               (resume-interop-thread thread-id))))))
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
  (setf *sandbox-root* (truename "."))
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
