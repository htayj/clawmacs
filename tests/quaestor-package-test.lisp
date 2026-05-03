(in-package :clawmacs/tests)

(in-suite quaestor-package-suite)

(defvar *quaestor-test-resume-record* nil
  "Captured callback payload from quaestor Lisp API tests.")

(defun quaestor-test-resume-handler (buffer request payload resume-state)
  "Record the latest quaestor resume callback for tests."
  (setf *quaestor-test-resume-record*
        (list :buffer (buffer-name buffer)
              :request request
              :payload payload
              :resume-state resume-state)))

(defmacro with-quaestor-function-override ((name lambda-list &body implementation)
                                           &body body)
  `(let ((original-function (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name)
                  (lambda ,lambda-list
                    ,@implementation))
            ,@body)
       (setf (symbol-function ',name) original-function))))

(defmacro with-quaestor-package-state (&body body)
  "Run BODY with isolated package, tool, command, keymap, and advice state."
  `(let* ((root (temp-package-test-directory "quaestor-config"))
          (approval-path (temp-approval-policy-path))
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels* (default-package-test-channels))
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
          (clawmacs::*enabled-builtin-packages* nil)
          (clawmacs::*tool-table* (make-hash-table :test #'equal))
          (clawmacs::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (clawmacs::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (clawmacs::*command-table* (make-hash-table :test #'eq))
          (clawmacs::*extended-docs* (make-hash-table :test #'eq))
          (clawmacs::*slash-command-table* (make-hash-table :test #'equal))
          (clawmacs::*buffer-type-registry*
           (clawmacs::make-buffer-type-registry))
          (clawmacs::*buffer-ring* nil)
          (clawmacs::*buffer-counter* 0)
          (clawmacs::*default-keymap* nil)
          (clawmacs::*scratch-keymap* nil)
          (clawmacs::*file-keymap* nil)
          (clawmacs::*after-buffer-display-change-hook* nil)
          (clawmacs::*approval-policy-path* approval-path)
          (clawmacs::*approval-policy-registry* nil)
          (clawmacs::*approval-policy-project-registry-cache*
           (make-hash-table :test #'equal))
          (clawmacs::*advice-table* (make-hash-table :test #'eq))
          (*quaestor-test-resume-record* nil))
     (unwind-protect
          (progn
            ,@body)
       (ignore-errors
         (clawmacs:remove-advice 'clawmacs::send-message
                                 'clawmacs::quaestor-send-message))
       (ignore-errors
         (clawmacs:remove-advice 'clawmacs::handle-key-event
                                 'clawmacs::quaestor-handle-key-event))
       (ignore-errors
         (clawmacs:remove-advice 'clawmacs::advance-tool-approval
                                 'clawmacs::quaestor-advance-tool-approval))
       (ignore-errors
         (clawmacs:remove-advice 'clawmacs::execute-prompt-tool-call
                                 'clawmacs::quaestor-prompt-tool-call))
       (ignore-errors
         (clawmacs:remove-advice 'clawmacs::mcclim-render-buffer
                                 'clawmacs::quaestor-mcclim-render-buffer))
       (ignore-errors
         (clawmacs:remove-advice 'clawmacs::init-default-keymap
                                 'clawmacs::quaestor-init-default-keymap)))))

(defun load-test-quaestor-package ()
  "Enable and load the bundled quaestor package."
  (set-package-enablement-scope "quaestor" :global)
  (load-active-packages)
  (clawmacs::init-default-keymap))

(defun quaestor-test-request-args ()
  "Return a normalized request_user_input question payload."
  '(:questions #((:header "Scope"
                  :id "scope"
                  :question "Pick a scope."
                  :options #((:label "Alpha" :description "Smaller change.")
                             (:label "Beta" :description "Broader change."))
                  :freeform t))))

(defun quaestor-last-finalized-message (buffer)
  "Return the latest finalized message before BUFFER's input editor."
  (message-prev (buffer-input-message buffer)))

(defun make-quaestor-test-buffer (name)
  "Return a chat buffer rooted in a fresh temp directory."
  (let ((root (temp-package-test-directory
               (format nil "quaestor-buffer-~A" name))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (clawmacs::make-chat-buffer name :working-directory root)))

(test quaestor-package-registers-tool-prompt-commands-and-bindings
  "Enabling quaestor exposes its tool, prompt, commands, and queue bindings."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (mapcar (lambda (tool)
                                 (cdr (assoc :name tool)))
                               tools))
           (prompt (render-package-prompt-sections))
           (commands (list-available-commands)))
      (is (member "request_user_input" tool-names :test #'string=))
      (is-false (clawmacs::tool-requires-permission-p "request_user_input"))
      (is (search "Structured user questions with quaestor" prompt))
      (is (search "request_user_input" prompt))
      (is (member 'clawmacs::quaestor-show-queued-messages-command
                  commands
                  :test #'eq))
      (is (member 'clawmacs::quaestor-recall-last-queued-message-command
                  commands
                  :test #'eq))
      (is (member 'clawmacs::quaestor-queue-steering-message-command
                  commands
                  :test #'eq))
      (is (member 'clawmacs::quaestor-cancel-and-restore-command
                  commands
                  :test #'eq))
      (is (eq 'clawmacs::quaestor-show-queued-messages-command
              (clawmacs::keymap-lookup clawmacs::*default-keymap*
                                      '(:ctrl-c #\q))))
      (is (eq 'clawmacs::quaestor-recall-last-queued-message-command
              (clawmacs::keymap-lookup clawmacs::*default-keymap*
                                      '(:ctrl-c #\Q))))
      (is (eq 'clawmacs::quaestor-queue-steering-message-command
              (clawmacs::keymap-lookup clawmacs::*default-keymap*
                                      '(:ctrl-c #\j))))
      (is (eq 'clawmacs::quaestor-cancel-and-restore-command
              (clawmacs::keymap-lookup clawmacs::*default-keymap*
                                      '(:ctrl-c #\J)))))))

(test quaestor-tool-request-suspends-interactive-run-and-records-answer
  "Interactive request_user_input suspends tool execution, routes keys, and records the answer."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let ((buf (make-quaestor-test-buffer "quaestor-tool"))
          (start-stream-called nil))
      (with-quaestor-function-override (clawmacs::start-streaming-response (buffer)
                                        (declare (ignore buffer))
                                        (setf start-stream-called t)
                                        :stubbed)
        (let ((tool-use (clawmacs::canonical-tool-use-block
                         "call-1"
                         "request_user_input"
                         (quaestor-test-request-args))))
          (clawmacs::begin-tool-approval buf (list tool-use))
          (is (eq :question (buffer-status buf)))
          (is (not (null (clawmacs::buffer-user-input-pending buf))))
          (is (= 1 (length (buffer-pending-tool-calls buf))))
          (is (search "[request_user_input]"
                      (message-text (quaestor-last-finalized-message buf))))
          (clawmacs::handle-key-event buf :down)
          (clawmacs::handle-key-event buf #\Space)
          (clawmacs::handle-key-event buf #\Tab)
          (clawmacs::handle-key-event buf #\o)
          (clawmacs::handle-key-event buf #\k)
          (clawmacs::handle-key-event buf #\Return)
          (is (null (clawmacs::buffer-user-input-pending buf)))
          (is (null (buffer-pending-tool-calls buf)))
          (is (null (buffer-tool-call-results buf)))
          (is-true start-stream-called)
          (let* ((tool-result (quaestor-last-finalized-message buf))
                 (answer (message-prev tool-result)))
            (is (search "request_user_input answered"
                        (message-text tool-result)))
            (is (search "Scope: Beta; ok"
                        (message-text answer)))
            (is (string= "request-user-input-answer"
                         (cdr (assoc :kind
                                     (message-metadata answer)
                                     :test #'eq))))))))))

(test quaestor-request-user-input-is-unavailable-in-prompt-mode
  "Prompt mode receives a structured unavailable result instead of suspending."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let* ((buf (make-quaestor-test-buffer "quaestor-prompt"))
           (tool-use (clawmacs::canonical-tool-use-block
                      "call-1"
                      "request_user_input"
                      (quaestor-test-request-args)))
           (events nil))
      (multiple-value-bind (result event)
          (clawmacs::execute-prompt-tool-call
           buf tool-use :agent t
           :event-callback (lambda (payload)
                             (push payload events)))
        (let ((events (nreverse events)))
        (is (search "unavailable in prompt mode"
                    (cdr (assoc :display result))
                    :test #'char-equal))
        (is (search "unavailable in non-interactive prompt mode"
                    (cdr (assoc :result result))
                    :test #'char-equal))
        (is (clawmacs::prompt-tool-event-denied-p event))
        (is (= 2 (length events)))
        (is (string= "tool.call"
                     (getf (first events) :event)))
        (is (string= "call-1"
                     (getf (first events) :id)))
        (is (string= "request_user_input"
                     (getf (first events) :name)))
        (is (equal (quaestor-test-request-args)
                   (getf (first events) :input)))
        (is (string= "tool.result"
                     (getf (second events) :event)))
        (is-true (getf (second events) :denied-p)))))))

(test quaestor-lisp-api-resumes-with-structured-payload
  "The Lisp API can suspend for user input and resume with normalized answers."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let ((buf (make-quaestor-test-buffer "quaestor-lisp")))
      (clawmacs::quaestor-request-user-input
       buf
       '((:header "Path"
          :id "path"
          :question "Describe the path forward."))
       :resume-function 'quaestor-test-resume-handler
       :resume-state '(:ticket 7))
      (is (eq :question (buffer-status buf)))
      (clawmacs::handle-key-event buf #\d)
      (clawmacs::handle-key-event buf #\o)
      (clawmacs::handle-key-event buf #\n)
      (clawmacs::handle-key-event buf #\e)
      (clawmacs::handle-key-event buf #\Return)
      (is (eq :idle (buffer-status buf)))
      (is (not (null *quaestor-test-resume-record*)))
      (is (string= "quaestor-lisp"
                   (getf *quaestor-test-resume-record* :buffer)))
      (is (equal '(:ticket 7)
                 (getf *quaestor-test-resume-record* :resume-state)))
      (is (equalp '(:answers (("path" :answers #("done"))))
                  (getf *quaestor-test-resume-record* :payload))))))

(test quaestor-queues-follow-up-messages-and-can-recall-them
  "Busy interactive buffers queue follow-up text instead of dropping it."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let ((buf (make-quaestor-test-buffer "quaestor-queue")))
      (setf (buffer-pending-tool-calls buf)
            (list (clawmacs::canonical-tool-use-block
                   "call-1"
                   "request_user_input"
                   (quaestor-test-request-args))))
      (set-message-text (buffer-input-message buf) "follow up")
      (clawmacs::send-message buf)
      (is (= 1 (length (clawmacs::buffer-queued-follow-up-messages buf))))
      (is (string= "" (message-text (buffer-input-message buf))))
      (is (search "Queued follow-up message"
                  (message-text (quaestor-last-finalized-message buf))))
      (clawmacs::quaestor-recall-last-queued-message-command buf)
      (is (null (clawmacs::buffer-queued-follow-up-messages buf)))
      (is (string= "follow up"
                   (message-text (buffer-input-message buf)))))))

(test quaestor-steering-and-cancel-restore-round-trip-queued-drafts
  "Steering and follow-up drafts can be restored into the composer together."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let ((buf (make-quaestor-test-buffer "quaestor-steering")))
      (setf (buffer-status buf) :streaming)
      (set-message-text (buffer-input-message buf) "steer now")
      (clawmacs::quaestor-queue-steering-message-command buf)
      (clawmacs::queue-buffer-message buf :follow-up "follow later")
      (set-message-text (buffer-input-message buf) "draft text")
      (clawmacs::quaestor-cancel-and-restore-command buf)
      (is (null (clawmacs::buffer-queued-steering-messages buf)))
      (is (null (clawmacs::buffer-queued-follow-up-messages buf)))
      (let ((text (message-text (buffer-input-message buf))))
        (is (search "steer now" text))
        (is (search "follow later" text))
        (is (search "draft text" text))))))
