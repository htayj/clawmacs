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

(test listener-wholine-shows-attached-session-display-name
  (with-listener-session-directory (root "wholine")
    (let* ((session (load-or-create-session "wholine-name"
                                            :display-name "Wholine Label"))
           (buffer (make-buffer "wholine-name" :session session))
           (frame (make-listener-session-frame buffer))
           (text (with-output-to-string (stream)
                   (rplaca::display-listener-wholine frame stream))))
      (is (search "Wholine Label" text)))))
