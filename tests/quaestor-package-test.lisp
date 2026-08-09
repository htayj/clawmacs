(in-package :rplaca/tests)

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
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels* (default-package-test-channels))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
          (rplaca::*enabled-builtin-packages* nil)
          (rplaca::*tool-table* (make-hash-table :test #'equal))
          (rplaca::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (rplaca::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (rplaca::*command-table* (make-hash-table :test #'eq))
          (rplaca::*extended-docs* (make-hash-table :test #'eq))
          (rplaca::*slash-command-table* (make-hash-table :test #'equal))
          (rplaca::*buffer-type-registry*
           (rplaca::make-buffer-type-registry))
          (rplaca::*buffer-input-presentation-providers* nil)
          (rplaca::*buffer-ring* nil)
          (rplaca::*buffer-counter* 0)
          (rplaca::*default-keymap* nil)
          (rplaca::*scratch-keymap* nil)
          (rplaca::*file-keymap* nil)
          (rplaca::*after-buffer-display-change-hook* nil)
          (rplaca::*advice-table* (make-hash-table :test #'eq))
          (*quaestor-test-resume-record* nil))
     (unwind-protect
          (progn
            ,@body)
       (ignore-errors
         (rplaca:remove-advice 'rplaca::send-message
                                 'rplaca::quaestor-send-message))
       (ignore-errors
         (rplaca:remove-advice 'rplaca::handle-key-event
                                 'rplaca::quaestor-handle-key-event))
       (ignore-errors
         (rplaca:remove-advice 'rplaca::advance-tool-calls
                                 'rplaca::quaestor-advance-tool-calls))
       (ignore-errors
         (rplaca:remove-advice 'rplaca::execute-prompt-tool-call
                                 'rplaca::quaestor-prompt-tool-call))
       (ignore-errors
         (rplaca:remove-advice 'rplaca::init-default-keymap
                                 'rplaca::quaestor-init-default-keymap)))))

(defun load-test-quaestor-package ()
  "Enable and load the bundled quaestor package."
  (set-package-enablement-scope "quaestor" :global)
  (load-active-packages)
  (rplaca::init-default-keymap))

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
    (rplaca::make-chat-buffer name :working-directory root)))

(defun make-quaestor-listener-frame (buffer)
  "Return a listener frame whose conversation buffer is BUFFER."
  (clim:make-application-frame
   'rplaca::rplaca-listener
   :conversation-buffer buffer
   :listener-context (rplaca::make-listener-context)))

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
      (let ((buf (make-quaestor-test-buffer "quaestor-provider-check")))
        (is (member 'rplaca::quaestor-input-presentation-entries
                    (rplaca::buffer-input-presentation-functions buf)
                    :test #'eq)))
      (is (search "Structured user questions with quaestor" prompt))
      (is (search "request_user_input" prompt))
      (is (member 'rplaca::quaestor-show-queued-messages-command
                  commands
                  :test #'eq))
      (is (member 'rplaca::quaestor-recall-last-queued-message-command
                  commands
                  :test #'eq))
      (is (member 'rplaca::quaestor-queue-steering-message-command
                  commands
                  :test #'eq))
      (is (member 'rplaca::quaestor-cancel-and-restore-command
                  commands
                  :test #'eq))
      (is (eq 'rplaca::quaestor-show-queued-messages-command
              (rplaca::keymap-lookup rplaca::*default-keymap*
                                      '(:ctrl-c #\q))))
      (is (eq 'rplaca::quaestor-recall-last-queued-message-command
              (rplaca::keymap-lookup rplaca::*default-keymap*
                                      '(:ctrl-c #\Q))))
      (is (eq 'rplaca::quaestor-queue-steering-message-command
              (rplaca::keymap-lookup rplaca::*default-keymap*
                                      '(:ctrl-c #\j))))
      (is (eq 'rplaca::quaestor-cancel-and-restore-command
              (rplaca::keymap-lookup rplaca::*default-keymap*
                                      '(:ctrl-c #\J))))
      (let ((buf (make-quaestor-test-buffer "quaestor-provider-reset")))
        (is (member 'rplaca::quaestor-input-presentation-entries
                    (rplaca::buffer-input-presentation-functions buf)
                    :test #'eq))
        (rplaca::reset-package-runtime-state "quaestor")
        (is-false (member 'rplaca::quaestor-input-presentation-entries
                          (rplaca::buffer-input-presentation-functions buf)
                          :test #'eq))))))

(test quaestor-input-presentations-describe-active-request
  "The package-owned input panel exposes options and submit affordances."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let ((buf (make-quaestor-test-buffer "quaestor-panel")))
      (rplaca::quaestor-request-user-input
       buf
       '((:header "Scope"
          :id "scope"
          :question "Pick a scope."
          :options ((:label "Alpha" :description "Smaller change.")
                    (:label "Beta" :description "Broader change."))
          :freeform t)))
      (is (member 'rplaca::quaestor-input-presentation-entries
                  (rplaca::buffer-input-presentation-functions buf)
                  :test #'eq))
      (let* ((entries (rplaca::quaestor-input-presentation-entries buf 100))
             (beta (find-if (lambda (entry)
                              (search "Beta" (getf entry :text)))
                            entries))
             (submit (find 'rplaca::quaestor-submit-ref entries
                           :key (lambda (entry)
                                  (getf entry :presentation-type)))))
        (is (search "Quaestor request 1/1: Scope"
                    (getf (second entries) :text)))
        (is (not (null beta)))
        (is (eq 'rplaca::quaestor-option-ref
                (getf beta :presentation-type)))
        (is (clim:presentation-typep (getf beta :object)
                                     'rplaca::quaestor-option-ref))
        (is (not (null submit)))
        (is (clim:presentation-typep (getf submit :object)
                                     'rplaca::quaestor-submit-ref))
        (rplaca::quaestor-activate-option buf (getf beta :object))
        (let ((updated (rplaca::quaestor-input-presentation-entries buf 100)))
          (is (find-if (lambda (entry)
                         (and (search "[x] Beta" (getf entry :text))
                              (eq :selector-selected (getf entry :face))))
                        updated)))))))

(test quaestor-listener-commands-and-translators-are-registered
  "Quaestor options, freeform answers, and submission use the listener command table."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let ((table (clim:find-command-table 'rplaca::rplaca-listener)))
      (dolist (command '(rplaca::com-quaestor-select-option
                         rplaca::com-quaestor-answer-request
                         rplaca::com-quaestor-submit-request))
        (is-true (clim:command-present-in-command-table-p command table)))
      (dolist (translator '(rplaca::select-quaestor-option
                            rplaca::select-quaestor-submit))
        (is-true (clim:find-presentation-translator translator table
                                                    :errorp nil))))))

(test quaestor-pending-question-emits-inline-listener-presentations
  "A pending request writes its question and clickable choices to the listener interactor."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let* ((buffer (make-quaestor-test-buffer "quaestor-listener-inline"))
           (frame (make-quaestor-listener-frame buffer))
           (output (make-string-output-stream)))
      (rplaca::quaestor-request-user-input
       buffer
       '((:header "Scope"
          :id "scope"
          :question "Pick a scope."
          :options ((:label "Alpha" :description "Smaller change.")
                    (:label "Beta" :description "Broader change."))
          :freeform t)))
      (with-quaestor-function-override (clim:frame-standard-output (actual-frame)
                                         (declare (ignore actual-frame))
                                         output)
        (rplaca::emit-quaestor-pending-request frame))
      (let ((text (get-output-stream-string output)))
        (is (search "Quaestor request 1/1: Scope" text))
        (is (search "Pick a scope." text))
        (is (search "Beta" text))
        (is (search "[Submit request]" text))))))

(test quaestor-tool-request-suspends-interactive-run-and-records-answer
  "Interactive request_user_input suspends tool execution, routes keys, and records the answer."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let ((buf (make-quaestor-test-buffer "quaestor-tool"))
          (start-stream-called nil))
      (with-quaestor-function-override (rplaca::start-streaming-response (buffer)
                                        (declare (ignore buffer))
                                        (setf start-stream-called t)
                                        :stubbed)
        (let ((tool-use (rplaca::canonical-tool-use-block
                         "call-1"
                         "request_user_input"
                         (quaestor-test-request-args))))
          (rplaca::begin-tool-calls buf (list tool-use))
          (is (eq :question (buffer-status buf)))
          (is (not (null (rplaca::buffer-user-input-pending buf))))
          (is (= 1 (length (buffer-pending-tool-calls buf))))
          (is (search "[request_user_input]"
                      (message-text (quaestor-last-finalized-message buf))))
          (rplaca::handle-key-event buf :down)
          (rplaca::handle-key-event buf #\Space)
          (rplaca::handle-key-event buf #\Tab)
          (rplaca::handle-key-event buf #\o)
          (rplaca::handle-key-event buf #\k)
          (rplaca::handle-key-event buf #\Return)
          (is (null (rplaca::buffer-user-input-pending buf)))
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
           (tool-use (rplaca::canonical-tool-use-block
                      "call-1"
                      "request_user_input"
                      (quaestor-test-request-args)))
           (events nil))
      (multiple-value-bind (result event)
          (rplaca::execute-prompt-tool-call
           buf tool-use :agent
           :event-callback (lambda (payload)
                             (push payload events)))
        (let ((events (nreverse events)))
        (is (search "unavailable in prompt mode"
                    (cdr (assoc :display result))
                    :test #'char-equal))
        (is (search "unavailable in non-interactive prompt mode"
                    (cdr (assoc :result result))
                    :test #'char-equal))
        (is (rplaca::prompt-tool-event-denied-p event))
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

(test quaestor-interactor-answer-advances-multi-question-requests
  "Listener interactor answers advance active requests instead of prematurely completing them."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let* ((buf (make-quaestor-test-buffer "quaestor-multi-question"))
           (frame (make-quaestor-listener-frame buf)))
      (rplaca::quaestor-request-user-input
       buf
       '((:header "First"
          :id "first"
          :question "First answer?")
         (:header "Second"
          :id "second"
           :question "Second answer?"))
       :resume-function 'quaestor-test-resume-handler)
      (with-quaestor-function-override (clim:frame-standard-output (actual-frame)
                                         (declare (ignore actual-frame))
                                         (make-broadcast-stream))
        (let ((clim:*application-frame* frame))
          (rplaca::com-quaestor-answer-request "one")))
      (is (rplaca::buffer-user-input-pending buf))
      (is (= 1 (rplaca::quaestor-request-current-index
                 (rplaca::buffer-user-input-pending buf))))
      (is (string= "" (message-text (buffer-input-message buf))))
      (with-quaestor-function-override (clim:frame-standard-output (actual-frame)
                                         (declare (ignore actual-frame))
                                         (make-broadcast-stream))
        (let ((clim:*application-frame* frame))
          (rplaca::com-quaestor-answer-request "two")))
      (is-false (rplaca::buffer-user-input-pending buf))
      (is (equalp '(:answers (("first" :answers #("one"))
                              ("second" :answers #("two"))))
                  (getf *quaestor-test-resume-record* :payload))))))

(test quaestor-lisp-api-resumes-with-structured-payload
  "The Lisp API can suspend for user input and resume with normalized answers."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let ((buf (make-quaestor-test-buffer "quaestor-lisp")))
      (rplaca::quaestor-request-user-input
       buf
       '((:header "Path"
          :id "path"
          :question "Describe the path forward."))
       :resume-function 'quaestor-test-resume-handler
       :resume-state '(:ticket 7))
      (is (eq :question (buffer-status buf)))
      (rplaca::handle-key-event buf #\d)
      (rplaca::handle-key-event buf #\o)
      (rplaca::handle-key-event buf #\n)
      (rplaca::handle-key-event buf #\e)
      (rplaca::handle-key-event buf #\Return)
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
            (list (rplaca::canonical-tool-use-block
                   "call-1"
                   "request_user_input"
                   (quaestor-test-request-args))))
      (set-message-text (buffer-input-message buf) "follow up")
      (rplaca::send-message buf)
      (is (= 1 (length (rplaca::buffer-queued-follow-up-messages buf))))
      (is (string= "" (message-text (buffer-input-message buf))))
      (is (search "Queued follow-up message"
                  (message-text (quaestor-last-finalized-message buf))))
      (rplaca::quaestor-recall-last-queued-message-command buf)
      (is (null (rplaca::buffer-queued-follow-up-messages buf)))
      (is (string= "follow up"
                   (message-text (buffer-input-message buf)))))))

(test quaestor-steering-and-cancel-restore-round-trip-queued-drafts
  "Steering and follow-up drafts can be restored into the composer together."
  (with-quaestor-package-state
    (load-test-quaestor-package)
    (let ((buf (make-quaestor-test-buffer "quaestor-steering")))
      (setf (buffer-status buf) :streaming)
      (set-message-text (buffer-input-message buf) "steer now")
      (rplaca::quaestor-queue-steering-message-command buf)
      (rplaca::queue-buffer-message buf :follow-up "follow later")
      (set-message-text (buffer-input-message buf) "draft text")
      (rplaca::quaestor-cancel-and-restore-command buf)
      (is (null (rplaca::buffer-queued-steering-messages buf)))
      (is (null (rplaca::buffer-queued-follow-up-messages buf)))
      (let ((text (message-text (buffer-input-message buf))))
        (is (search "steer now" text))
        (is (search "follow later" text))
        (is (search "draft text" text))))))
