(in-package :rplaca/tests)

(in-suite listener-session-suite)

(defun listener-session-temp-directory (label)
  (merge-pathnames
   (format nil "rplaca-listener-session-~A-~A/" label (gensym))
   #P"/tmp/"))

(defmacro with-listener-session-directory ((directory label) &body body)
  `(let* ((,directory (listener-session-temp-directory ,label))
          (rplaca::*sessions-dir* ,directory))
     (unwind-protect
          (progn ,@body)
       (when (probe-file ,directory)
         (uiop:delete-directory-tree
          ,directory :validate t :if-does-not-exist :ignore)))))

(defun write-listener-session-snapshot (root name buffer-state)
  (let ((path (rplaca::session-path name :root root)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string
       (cl-json:encode-json-to-string
        `((:name . ,name)
          (:agent-name . "legacy-listener")
          (:kind . "LISTENER")
          (:major-mode . "listener")
          (:working-directory . "/tmp/")
          (:buffer-state . ,buffer-state)
          (:messages
           . #(((:sender . "USER")
                (:text . "(+ 1 2)")
                (:timestamp . 10)
                (:read-only-p . t))
               ((:sender . "LISTENER")
                (:text . "3")
                (:timestamp . 11)
                (:read-only-p . t))))))
       stream))
    path))

(defun listener-session-history (buffer)
  (loop :for message := (buffer-first-message buffer) :then (message-next message)
        :while (and message (not (eq message (buffer-input-message buffer))))
        :unless (rplaca::buffer-system-prompt-display-message-p message)
          :collect message))

(defun make-listener-session-frame (buffer)
  (clim:make-application-frame
   'rplaca::rplaca-listener
   :conversation-buffer buffer
   :listener-context (rplaca::make-listener-context)))

(defmacro with-listener-session-function ((name lambda-list &body replacement)
                                          &body body)
  (let ((original (gensym "ORIGINAL")))
    `(let ((,original (symbol-function ',name)))
       (unwind-protect
            (progn
              (setf (symbol-function ',name)
                    (lambda ,lambda-list ,@replacement))
              ,@body)
         (setf (symbol-function ',name) ,original)))))

(defun append-listener-session-event (session event &key parent-id)
  (multiple-value-bind (written id)
      (if parent-id
          (rplaca::append-session-tree-event session event :parent-id parent-id)
          (rplaca::append-session-tree-event session event))
    (declare (ignore written))
    id))

(defun listener-session-message-event (sender text timestamp &key raw-content metadata)
  `((:event . "message")
    (:sender . ,sender)
    (:text . ,text)
    (:timestamp . ,timestamp)
    (:read-only-p . t)
    ,@(when raw-content `((:raw-content . ,(coerce raw-content 'vector))))
    ,@(when metadata `((:metadata . ,metadata)))))

(defun make-replay-session (name)
  (let* ((session (load-or-create-session name :display-name name))
         (user-id
           (append-listener-session-event
            session (listener-session-message-event "USER" "user one" 10)))
         (assistant-id
           (append-listener-session-event
            session
            (listener-session-message-event
             "ASSISTANT" "assistant one" 20
             :raw-content
             '(((:type . "text") (:text . "assistant one"))
               ((:type . "tool_use") (:id . "tool-1")
                (:name . "read") (:input . ((:path . "README.md"))))
               ((:type . "reasoning") (:text . "reason one")))
             :metadata '((:model . "test-model")))
            :parent-id user-id))
         (tool-id
           (append-listener-session-event
            session
            (listener-session-message-event
             "TOOL-RESULT" "tool summary" 30
             :raw-content
             '(((:type . "tool_result") (:tool--use--id . "tool-1")
                (:content . "tool summary"))))
            :parent-id assistant-id))
         (compaction-id
           (append-listener-session-event
            session
            '((:event . "compaction") (:reason . "manual")
              (:summary . "compacted earlier turns") (:timestamp . 40))
            :parent-id tool-id))
         (branch-id
           (append-listener-session-event
            session
            '((:event . "branch-summary") (:from-id . "root")
              (:summary . "branch marker") (:timestamp . 50))
            :parent-id compaction-id))
         (error-id
           (append-listener-session-event
            session
            (listener-session-message-event
             "ASSISTANT" "error marker" 60 :metadata '((:status . "error")))
            :parent-id branch-id)))
    (append-listener-session-event
     session
     (listener-session-message-event
      "ASSISTANT" "cancel marker" 70
      :metadata '((:stop-reason . "cancelled")))
     :parent-id error-id)
    session))

(defun ordered-substrings-p (strings text)
  (loop :with start := 0
        :for string :in strings
        :for position := (search string text :start2 start)
        :always position
        :do (setf start (+ position (length string)))))

(test first-listener-say-creates-manifest-and-jsonl
  (with-listener-session-directory (root "first-say")
    (let* ((buffer (make-buffer "first-listener-say"
                                :agent-name "test-agent"
                                :session-persistence-mode :persistent))
           (rplaca::*before-send-message-hook* nil)
           (rplaca::*after-send-message-hook* nil)
           (rplaca::*after-message-insert-hook* nil))
      (with-listener-say-functions
          ((rplaca::start-interactive-compaction
            (lambda (live-buffer &key reason continuation &allow-other-keys)
              (declare (ignore live-buffer reason continuation))
              (values nil nil 0 0)))
           (rplaca::send-to-agent-with-context
            (lambda (live-buffer)
              (declare (ignore live-buffer))
              :provider-started)))
        (multiple-value-bind (result accepted-p)
            (rplaca::send-prose-message buffer "hello")
          (is (eq :provider-started result))
          (is-true accepted-p)))
      (let ((session (buffer-session buffer)))
        (is (probe-file (rplaca::session-manifest-path session)))
        (is (probe-file (rplaca::session-current-transcript-path session)))
        (is (> (with-open-file (stream (rplaca::session-current-transcript-path session))
                 (file-length stream))
               0))))))

(test retired-listener-snapshot-migrates-to-display-only-chat
  (with-listener-session-directory (root "legacy")
    (let* ((name "retired-listener")
           (path (write-listener-session-snapshot
                  root name
                  '((:package-name . "KEYWORD")
                    (:directory-stack . #("/tmp/legacy-a/" "/tmp/legacy-b/"))
                    (:last-values . #("3"))
                    (:command-history . #("(+ 1 2)")))))
           (before (uiop:read-file-string path))
           (buffer (load-session name))
           (history (listener-session-history buffer))
           (context (rplaca::listener-context-for-buffer buffer)))
      (is (eq :chat (buffer-kind buffer)))
      (is (string= "chat" (buffer-major-mode buffer)))
      (is (= 2 (length history)))
      (is (every #'rplaca::buffer-ephemeral-display-message-p history))
      (let ((provider-texts
              (mapcar (lambda (message) (cdr (assoc :content message)))
                      (build-conversation-messages buffer))))
        (is-false (member "(+ 1 2)" provider-texts :test #'string=))
        (is-false (member "3" provider-texts :test #'string=)))
      (is (string= "KEYWORD"
                   (rplaca::listener-context-package-name context)))
      (is (equal (list #P"/tmp/legacy-a/" #P"/tmp/legacy-b/")
                 (rplaca::listener-context-directory-stack context)))
      (is (string= before (uiop:read-file-string path))))))

(test malformed-retired-listener-state-falls-back-safely
  (with-listener-session-directory (root "malformed")
    (let* ((name "malformed-retired-listener")
           (path (write-listener-session-snapshot
                  root name
                  '((:package-name . 42)
                    (:directory-stack . "not-a-directory-list"))))
           (before (uiop:read-file-string path))
           (buffer (load-session name))
           (context (rplaca::listener-context-for-buffer buffer)))
      (is (eq :chat (buffer-kind buffer)))
      (is (string= (rplaca::listener-context-default-package-name)
                   (rplaca::listener-context-package-name context)))
      (is (null (rplaca::listener-context-directory-stack context)))
      (is (every #'rplaca::buffer-ephemeral-display-message-p
                 (listener-session-history buffer)))
      (is (string= before (uiop:read-file-string path))))))

(test new-session-command-creates-and-attaches-fresh-buffer
  (with-listener-session-directory (root "new-command")
    (let* ((rplaca::*buffer-ring* nil)
           (rplaca::*buffer-counter* 0)
           (source (make-buffer "source" :agent-name "test-agent"))
           (frame (make-listener-session-frame source)))
      (add-buffer-to-ring source)
      (let ((clim:*application-frame* frame))
        (let ((created (rplaca::com-new-session)))
          (is (not (eq source created)))
          (is (eq created (rplaca::rplaca-listener-conversation-buffer frame)))
          (is (eq :chat (buffer-kind created)))
          (is (buffer-session created))
          (is (eq created (current-buffer)))
          (is (probe-file (rplaca::session-manifest-path
                           (buffer-session created)))))))))

(test list-sessions-renders-resumable-presentations
  (with-listener-session-directory (root "list-command")
    (let* ((session (load-or-create-session "listed-session"
                                            :display-name "Listed Session"))
           (buffer (make-buffer "listed-session" :session session)))
      (save-session buffer)
      (let* ((records (rplaca::listener-saved-session-records))
             (record (find "listed-session" records
                           :key (lambda (item) (getf item :session-name))
                           :test #'string=))
             (text (with-output-to-string (stream)
                     (rplaca::display-listener-session-list records stream))))
        (is (not (null record)))
        (is (clim:presentation-typep
             record 'rplaca::saved-listener-session))
        (is (search "Listed Session" text))
        (is-true
         (clim:command-present-in-command-table-p
          'rplaca::com-resume-session
          (clim:find-command-table 'rplaca::rplaca-listener)))))))

(test resume-session-command-loads-buffer-and-context-into-frame
  (with-listener-session-directory (root "resume-command")
    (let* ((rplaca::*buffer-ring* nil)
           (source (make-buffer "source" :agent-name "test-agent"))
           (session (load-or-create-session "resume-target"
                                            :display-name "Resume Target"))
           (saved (make-buffer "resume-target"
                               :agent-name "test-agent"
                               :session session))
           (frame (make-listener-session-frame source)))
      (rplaca::set-message-text (buffer-input-message saved) "remember me")
      (buffer-finalize-input saved)
      (save-session saved)
      (add-buffer-to-ring source)
      (let* ((record (find "resume-target"
                           (rplaca::listener-saved-session-records)
                           :key (lambda (item) (getf item :session-name))
                           :test #'string=))
             (clim:*application-frame* frame)
             (loaded (rplaca::com-resume-session record)))
        (is (eq loaded (rplaca::rplaca-listener-conversation-buffer frame)))
        (is (string= "remember me"
                     (message-text (first (listener-session-history loaded)))))
        (is (string= "Resume Target"
                     (rplaca::rplaca-listener-session-label frame)))
        (is (eq (rplaca::listener-context-for-buffer loaded)
                (rplaca::rplaca-listener-context frame)))))))

(test resume-session-replays-active-branch-history-once-in-chronological-order
  (with-listener-session-directory (root "replay-order")
    (let* ((rplaca::*buffer-ring* nil)
           (session (make-replay-session "replay-order"))
           (source (make-buffer "source" :agent-name "test-agent"))
           (frame (make-listener-session-frame source))
           (output (make-string-output-stream))
           (assistant-turns nil))
      (declare (ignore session))
      (add-buffer-to-ring source)
      (with-listener-session-function
          (clim:frame-standard-output (requested-frame)
            (declare (ignore requested-frame))
            output)
        (with-listener-session-function
            (rplaca::emit-listener-assistant-turn (requested-frame turn)
              (declare (ignore requested-frame))
              (push turn assistant-turns)
              (format output "assistant: ~A~%"
                      (rplaca::assistant-turn-primary-text turn))
              turn)
          (let ((clim:*application-frame* frame))
            (rplaca::com-resume-session "replay-order")
            (rplaca::com-resume-session "replay-order"))))
      (let ((text (get-output-stream-string output)))
        (is (ordered-substrings-p
             '("user one" "assistant one" "tool summary"
               "compacted earlier turns" "branch marker"
               "error marker" "cancel marker")
             text))
        (is (equal '(:complete :error :cancelled)
                   (mapcar #'rplaca::assistant-turn-status
                           (reverse assistant-turns))))
        (is (= 1 (rplaca::count-occurrences "user one" text)))
        (is (= 1 (rplaca::count-occurrences "cancel marker" text)))))))

(test restored-assistant-turn-retains-clickable-facet-presentations
  (with-listener-session-directory (root "replay-facets")
    (let* ((rplaca::*buffer-ring* nil)
           (session (make-replay-session "replay-facets"))
           (source (make-buffer "source" :agent-name "test-agent"))
           (frame (make-listener-session-frame source))
           (captured-turn nil))
      (declare (ignore session))
      (add-buffer-to-ring source)
      (with-listener-session-function
          (clim:frame-standard-output (requested-frame)
            (declare (ignore requested-frame))
            (make-string-output-stream))
        (with-listener-session-function
            (rplaca::emit-listener-assistant-turn (requested-frame turn)
              (declare (ignore requested-frame))
              (when (rplaca::assistant-turn-tool-uses turn)
                (setf captured-turn turn))
              turn)
          (let ((clim:*application-frame* frame))
            (rplaca::com-resume-session "replay-facets"))))
      (is (clim:presentation-typep captured-turn 'rplaca::assistant-turn))
      (is (equal '(:tools :reasoning :metadata :inspect)
                 (rplaca::listener-turn-nonempty-facet-kinds captured-turn)))
      (dolist (kind (rplaca::listener-turn-nonempty-facet-kinds captured-turn))
        (is (clim:presentation-typep
             (rplaca::make-turn-facet :turn captured-turn :kind kind)
             'rplaca::turn-facet))))))

(test resume-session-replays-only-the-selected-branch
  (with-listener-session-directory (root "replay-branch")
    (let* ((rplaca::*buffer-ring* nil)
           (session (load-or-create-session "replay-branch"))
           (root-id
             (append-listener-session-event
              session (listener-session-message-event "USER" "shared root" 10)))
           (first-leaf
             (append-listener-session-event
              session
              (listener-session-message-event "ASSISTANT" "wrong branch" 20)
              :parent-id root-id))
           (source (make-buffer "source" :agent-name "test-agent"))
           (frame (make-listener-session-frame source))
           (output (make-string-output-stream)))
      (declare (ignore first-leaf))
      (set-session-current-leaf session root-id)
      (append-listener-session-event
       session
       (listener-session-message-event "ASSISTANT" "selected branch" 30)
       :parent-id root-id)
      (add-buffer-to-ring source)
      (with-listener-session-function
          (clim:frame-standard-output (requested-frame)
            (declare (ignore requested-frame))
            output)
        (let ((clim:*application-frame* frame))
          (rplaca::com-resume-session "replay-branch")))
      (let ((text (get-output-stream-string output)))
        (is (search "shared root" text))
        (is (search "selected branch" text))
        (is-false (search "wrong branch" text))))))

(test migrated-listener-output-is-visible-inline-but-absent-from-provider-context
  (with-listener-session-directory (root "replay-legacy")
    (let* ((rplaca::*buffer-ring* nil)
           (name "replay-legacy")
           (path (write-listener-session-snapshot
                  root name
                  '((:package-name . "CL-USER")
                    (:last-values . #("3"))
                    (:command-history . #("(+ 1 2)")))))
           (source (make-buffer "source" :agent-name "test-agent"))
           (frame (make-listener-session-frame source))
           (output (make-string-output-stream)))
      (declare (ignore path))
      (add-buffer-to-ring source)
      (with-listener-session-function
          (clim:frame-standard-output (requested-frame)
            (declare (ignore requested-frame))
            output)
        (let ((clim:*application-frame* frame))
          (let* ((loaded (rplaca::com-resume-session name))
                 (provider-texts
                   (mapcar (lambda (message) (cdr (assoc :content message)))
                           (build-conversation-messages loaded))))
            (is-false (member "(+ 1 2)" provider-texts :test #'string=))
            (is-false (member "3" provider-texts :test #'string=)))))
      (let ((text (get-output-stream-string output)))
        (is (ordered-substrings-p '("(+ 1 2)" "3") text))))))

(test listener-wholine-shows-attached-session-display-name
  (with-listener-session-directory (root "wholine")
    (let* ((session (load-or-create-session "wholine-name"
                                            :display-name "Wholine Label"))
           (buffer (make-buffer "wholine-name" :session session))
           (frame (make-listener-session-frame buffer))
           (text (with-output-to-string (stream)
                   (rplaca::display-listener-wholine frame stream))))
      (is (search "Wholine Label" text)))))
