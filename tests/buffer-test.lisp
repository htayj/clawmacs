(in-package :rplaca/tests)
(in-suite buffer-suite)

(defvar *mx-test-command-log* nil
  "Records command invocations during buffer tests.")

(defun temp-session-test-directory (label)
  "Return a fresh temporary directory for session transcript tests."
  (make-pathname :directory
                 (list :absolute "tmp"
                       (format nil "rplaca-session-tests-~A-~A-~A"
                               label
                               (get-universal-time)
                               (gensym)))))

(defun read-jsonl-events (path)
  "Read JSONL transcript events from PATH."
  (let ((events nil))
    (when (probe-file path)
      (with-open-file (stream path :direction :input :external-format :utf-8)
        (loop :for line := (read-line stream nil nil)
              :while line
              :unless (zerop (length line))
                :do (let ((cl-json:*json-array-type* 'vector))
                      (push (cl-json:decode-json-from-string line) events)))))
    (nreverse events)))

(defun event-value (event key)
  "Return KEY's value from decoded transcript EVENT."
  (cdr (assoc key event)))

(defun session-current-events (session)
  "Return decoded events from SESSION's current transcript."
  (read-jsonl-events (session-current-transcript-path session)))

(defun make-recorded-test-message (sender text)
  "Return a read-only test message."
  (let ((msg (make-message sender :read-only-p t)))
    (rplaca::set-message-text msg text)
    (setf (message-timestamp msg) (get-universal-time))
    msg))

(defun first-substantive-buffer-message (buffer)
  "Return BUFFER's first non-synthetic finalized message, or NIL."
  (loop :for msg := (buffer-first-message buffer) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buffer))))
        :unless (rplaca::buffer-system-prompt-display-message-p msg)
          :return msg))

(defun mx-test-noarg-command (buffer)
  "Test command used to verify M-x invocation without arguments."
  (declare (ignore buffer))
  (setf *mx-test-command-log* '(:noarg)))
(rplaca:defcommand mx-test-noarg-command)

(defun mx-test-arg-command (buffer count label)
  "Test command used to verify M-x argument prompting."
  (declare (ignore buffer))
  (setf *mx-test-command-log* (list :args count label)))
(rplaca:defcommand mx-test-arg-command
  :prompts ((count :prompt "Count" :reader parse-integer)
            (label :prompt "Label")))

(test scroll-down-command-clamps-near-bottom
  "One page down from within one page of bottom returns to offset zero."
  (let ((buf (make-buffer "scroll-clamp")))
    (let ((rplaca::*scroll-page-size* 16))
      (setf (buffer-scroll-offset buf) 17)
      (rplaca::scroll-down-command buf)
      (is (= 0 (buffer-scroll-offset buf))))))

(test scroll-down-command-subtracts-page-when-far-from-bottom
  "Page down keeps a positive offset when more than one page from bottom."
  (let ((buf (make-buffer "scroll-page")))
    (let ((rplaca::*scroll-page-size* 16))
      (setf (buffer-scroll-offset buf) 40)
      (rplaca::scroll-down-command buf)
      (is (= 24 (buffer-scroll-offset buf))))))

(defmacro with-interactive-command-test-buffer ((buffer-var) &body body)
  `(let ((*buffer-ring* nil)
         (rplaca::*buffer-counter* 0)
         (rplaca::*chat-interaction-state*
           (rplaca::make-chat-interaction-state))
         (*buffer-selector-active* nil)
         (*model-selector-active* nil)
         (*think-selector-active* nil)
         (*openai-oauth-pending* nil)
         (*mx-test-command-log* nil))
     (rplaca::init-default-keymap)
     (let ((,buffer-var (make-buffer "test-session")))
       (setf (buffer-keymap ,buffer-var) *default-keymap*)
       (add-buffer-to-ring ,buffer-var)
       ,@body)))

(defun select-minibuffer-command (command)
  "Move the minibuffer selection to COMMAND and confirm it."
  (let ((index (position-if (lambda (item)
                              (eq command (getf item :command)))
                            *minibuffer-filtered-items*)))
    (is (not (null index)))
    (setf *minibuffer-selected-index* index)
    (minibuffer-confirm)))

(defun latest-buffer-message (buf)
  "Return the message immediately before BUF's input message."
  (message-prev (buffer-input-message buf)))

(defun buffer-test-history-messages (buf)
  "Return read-only history messages before BUF's input message."
  (loop :for msg := (buffer-first-message buf) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buf))))
        :collect msg))

(test buffer-creation
  "A new buffer has one message (the input message)."
  (let ((buf (make-buffer "test-session"
                          :agent-name "echo-agent"
                          :working-directory #P"/tmp/")))
    (is (string= "test-session" (buffer-name buf)))
    (is (string= "echo-agent" (buffer-agent-name buf)))
    (is (eq :chat (buffer-kind buf)))
    (is (not (null (buffer-input-message buf))))
    (is (eq (buffer-first-message buf) (buffer-input-message buf)))
    (is (eq (buffer-last-message buf) (buffer-input-message buf)))
    (is (not (message-read-only-p (buffer-input-message buf))))
    (is (= 1 (buffer-message-count buf)))
    (is (eq :idle (buffer-status buf)))))

(test make-buffer-supports-ephemeral-session-persistence-mode
  "Explicit ephemeral buffers never attach or persist durable session state."
  (let* ((*sessions-dir* (temp-session-test-directory "ephemeral-buffer"))
         (buf (make-buffer "ephemeral-session"
                           :agent-name "echo"
                           :session-persistence-mode :ephemeral)))
    (is (buffer-ephemeral-p buf))
    (is-false (buffer-persistent-session-p buf))
    (is (null (buffer-session buf)))
    (is (null (rplaca::ensure-buffer-session buf)))
    (rplaca::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (save-session buf)
    (is-false (probe-file (rplaca::session-path "ephemeral-session")))
    (is-false (probe-file (rplaca::session-sidecar-manifest-path
                           "ephemeral-session")))))

(test package-buffer-type-registration-controls-buffer-defaults
  "Registered buffer types provide package-extensible kind metadata."
  (let ((rplaca::*buffer-type-registry*
          (rplaca::make-buffer-type-registry)))
    (let ((type (register-buffer-type
                 :ledger
                 :description "Ledger view"
                 :major-mode "ledger"
                 :document-p t
                 :presentation-function 'identity
                 :input-presentation-function 'identity
                 :package "accounts")))
      (is (eq :ledger (buffer-type-name type)))
      (is (string= "accounts" (buffer-type-package type)))
      (let ((buf (make-buffer "ledger" :kind :ledger)))
        (is (eq :ledger (buffer-kind buf)))
        (is (string= "ledger" (buffer-major-mode buf)))
        (is (document-buffer-p buf))
        (is (eq 'identity (buffer-presentation-function buf)))
        (is (eq 'identity (buffer-input-presentation-function buf)))))))

(test package-buffer-type-defaults-major-mode-from-kind
  "Custom buffer types without an explicit major-mode use their kind label."
  (let ((rplaca::*buffer-type-registry*
          (rplaca::make-buffer-type-registry)))
    (register-buffer-type :dashboard)
    (let ((buf (make-buffer "dashboard" :kind :dashboard)))
      (is (string= "dashboard" (buffer-major-mode buf))))))

(test built-in-special-buffer-types-are-registered
  "Help, info, and font editor buffers are first-class non-document kinds."
  (let ((rplaca::*buffer-type-registry*
          (rplaca::make-buffer-type-registry)))
    (let ((help (find-buffer-type :help))
          (info (find-buffer-type :info))
          (font-editor (find-buffer-type :font-editor)))
      (is (not (null help)))
      (is (not (null info)))
      (is (not (null font-editor)))
      (is (string= "help" (buffer-type-major-mode help)))
      (is (string= "info" (buffer-type-major-mode info)))
      (is (string= "font-editor" (buffer-type-major-mode font-editor)))
      (is (not (buffer-type-document-p help)))
      (is (not (buffer-type-document-p info)))
      (is (not (buffer-type-document-p font-editor))))))

(test make-help-buffer-stores-read-only-help-content
  "Help buffers expose their text through help-buffer-text."
  (let ((*buffer-ring* nil))
    (let ((buf (make-help-buffer "*help:test*" "Help title~%==========")))
      (is (help-buffer-p buf))
      (is (string= "help" (buffer-major-mode buf)))
      (is (search "Help title" (help-buffer-text buf)))
      (is (= 1 (length (buffer-test-history-messages buf)))))))

(test toggle-reasoning-output-command-flips-buffer-flag
  "The reasoning output toggle controls per-buffer reasoning display."
  (let ((buf (make-buffer "reasoning-toggle")))
    (is (not (buffer-show-reasoning-p buf)))
    (rplaca::toggle-reasoning-output-command buf)
    (is (buffer-show-reasoning-p buf))
    (rplaca::toggle-reasoning-output-command buf)
    (is (not (buffer-show-reasoning-p buf)))))

(test toggle-metadata-output-command-flips-buffer-flag
  "The metadata output toggle controls per-buffer metadata display."
  (let ((buf (make-buffer "metadata-toggle")))
    (is (not (buffer-show-metadata-p buf)))
    (rplaca::toggle-metadata-output-command buf)
    (is (buffer-show-metadata-p buf))
    (rplaca::toggle-metadata-output-command buf)
    (is (not (buffer-show-metadata-p buf)))))

(test make-chat-buffer-shows-system-prompt-header-without-counting-it
  "Chat buffers show the full system prompt as a synthetic display header."
  (let ((original (symbol-function 'rplaca:build-agent-system-prompt)))
    (unwind-protect
         (progn
           (setf (symbol-function 'rplaca:build-agent-system-prompt)
                 (lambda (agent-name &key buffer)
                   (declare (ignore buffer))
                   (format nil "PROMPT FOR ~A" agent-name)))
           (let ((buf (make-chat-buffer "prompt-header"
                                        :agent-name "writer"
                                        :session-persistence-mode :ephemeral)))
             (let ((first (buffer-first-message buf)))
               (is (rplaca::buffer-system-prompt-display-message-p first))
               (is (search "PROMPT FOR writer" (message-text first)))
               (is (eq (buffer-input-message buf) (message-next first)))
               (is (= 1 (buffer-message-count buf))))))
      (setf (symbol-function 'rplaca:build-agent-system-prompt) original))))

(test serialize-buffer-omits-system-prompt-header
  "Synthetic system-prompt headers do not become durable session messages."
  (let ((original (symbol-function 'rplaca:build-agent-system-prompt)))
    (unwind-protect
         (progn
           (setf (symbol-function 'rplaca:build-agent-system-prompt)
                 (lambda (agent-name &key buffer)
                   (declare (ignore buffer))
                   (format nil "PROMPT FOR ~A" agent-name)))
           (let ((buf (make-chat-buffer "serialize-header"
                                        :agent-name "writer"
                                        :session-persistence-mode :ephemeral)))
             (let ((data (rplaca::serialize-buffer buf)))
               (is (null (coerce (cdr (assoc :messages data)) 'list))))
             (rplaca::set-message-text (buffer-input-message buf) "hello")
             (buffer-finalize-input buf)
             (let* ((data (rplaca::serialize-buffer buf))
                    (messages (coerce (cdr (assoc :messages data)) 'list)))
               (is (= 1 (length messages)))
               (is (string= "USER" (cdr (assoc :sender (first messages)))))
               (is (string= "hello" (cdr (assoc :text (first messages))))))))
      (setf (symbol-function 'rplaca:build-agent-system-prompt) original))))

(test explicit-session-records-transcript-events
  "Buffers with an attached session append durable JSONL message events."
  (let* ((*sessions-dir* (temp-session-test-directory "explicit"))
         (session (load-or-create-session "Real Session"))
         (buf (make-buffer "Real Session"
                           :agent-name "agent"
                           :session session)))
    (is (probe-file (session-manifest-path session)))
    (is (probe-file (session-current-transcript-path session)))
    (let ((initial-events (session-current-events session)))
      (is (= 1 (length initial-events)))
      (is (string= "session-start"
                   (event-value (first initial-events) :event))))
    (rplaca::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (buffer-insert-system-message buf "display-only")
    (rplaca::buffer-insert-context-message buf "late context")
    (buffer-insert-agent-message buf "answer")
    (let* ((events (session-current-events session))
           (message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            events)))
      (is (= 4 (length message-events)))
      (is (equal '("USER" "SYSTEM" "CONTEXT" "AGENT")
                 (mapcar (lambda (event)
                           (event-value event :sender))
                         message-events)))
      (is (string= "hello" (event-value (first message-events) :text)))
      (is (string= "answer" (event-value (fourth message-events) :text))))))

(test buffers-without-session-do-not-write-transcripts
  "Plain unit-test buffers stay transcript-free unless a session is attached."
  (let* ((*sessions-dir* (temp-session-test-directory "quiet"))
         (buf (make-buffer "plain-buffer")))
    (rplaca::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (buffer-insert-agent-message buf "answer")
    (is (null (buffer-session buf)))
    (is (not (probe-file *sessions-dir*)))))

(test persistent-session-autosaves-snapshot-on-message
  "Buffers with attached sessions refresh loadable snapshots automatically."
  (let* ((session-name "autosaved-session")
         (*sessions-dir* (temp-session-test-directory "autosave"))
         (session (load-or-create-session session-name))
         (buf (make-buffer session-name
                           :agent-name "echo"
                           :session session))
         (path (rplaca::session-path session-name)))
    (is (not (probe-file path)))
    (rplaca::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (is (probe-file path))
    (is (member session-name (list-saved-sessions) :test #'string=))
    (let ((loaded (load-session session-name)))
      (is (not (null loaded)))
      (is (string= session-name (buffer-name loaded)))
      (is (string= "echo" (buffer-agent-name loaded)))
      (let ((msg (first-substantive-buffer-message loaded)))
        (is (eq :user (message-sender msg)))
        (is (string= "hello" (message-text msg)))))))

(test load-session-shows-system-prompt-header
  "Loaded session buffers prepend the current full system prompt as a header."
  (let* ((session-name "session-with-prompt-header")
         (*sessions-dir* (temp-session-test-directory "load-prompt-header"))
         (original (symbol-function 'rplaca:build-agent-system-prompt)))
    (unwind-protect
         (progn
           (setf (symbol-function 'rplaca:build-agent-system-prompt)
                 (lambda (agent-name &key buffer)
                   (declare (ignore buffer))
                   (format nil "LOADED PROMPT FOR ~A" agent-name)))
           (let* ((session (load-or-create-session session-name))
                  (buf (make-buffer session-name
                                    :agent-name "reader"
                                    :session session)))
             (rplaca::set-message-text (buffer-input-message buf) "hello")
             (buffer-finalize-input buf)
             (save-session buf))
           (let ((loaded (load-session session-name :agent-name "reader")))
             (is (not (null loaded)))
             (let ((first (buffer-first-message loaded)))
               (is (rplaca::buffer-system-prompt-display-message-p first))
               (is (search "LOADED PROMPT FOR reader" (message-text first)))
               (let ((next (message-next first)))
                 (is (not (null next)))
                 (is (eq :user (message-sender next)))
                 (is (string= "hello" (message-text next)))))))
      (setf (symbol-function 'rplaca:build-agent-system-prompt) original))))

(test sidecar-only-session-is-listed-and-loadable
  "Transcript sidecars are visible to the load-session command before a snapshot exists."
  (let* ((session-name "sidecar-only-session")
         (*sessions-dir* (temp-session-test-directory "sidecar"))
         (session (load-or-create-session session-name))
         (path (rplaca::session-path session-name))
         (msg (make-message :user :read-only-p t)))
    (rplaca::set-message-text msg "from transcript")
    (setf (message-timestamp msg) 42)
    (record-session-message session msg)
    (is (not (probe-file path)))
    (is (member session-name (list-saved-sessions) :test #'string=))
    (let ((loaded (load-session session-name)))
      (is (not (null loaded)))
      (is (string= session-name (buffer-name loaded)))
      (is (probe-file path))
      (let ((loaded-msg (first-substantive-buffer-message loaded)))
        (is (eq :user (message-sender loaded-msg)))
        (is (string= "from transcript" (message-text loaded-msg)))))))

(test sidecar-only-session-round-trips-working-directory
  "Sidecar-backed session loads preserve the original working directory."
  (let* ((session-name "sidecar-working-directory")
         (working-directory #P"/tmp/rplaca-sidecar-session/")
         (*sessions-dir* (temp-session-test-directory "sidecar-working-directory"))
         (session (load-or-create-session session-name
                                          :working-directory working-directory))
         (msg (make-message :user :read-only-p t)))
    (rplaca::set-message-text msg "from transcript")
    (setf (message-timestamp msg) 42)
    (record-session-message session msg)
    (let ((loaded (load-session session-name)))
      (is (not (null loaded)))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                 (buffer-working-directory loaded)))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                 (rplaca::session-working-directory
                  (buffer-session loaded)))))))

(test session-transcripts-are-tree-shaped
  "Recorded transcript messages get ids, parent ids, and advance the leaf."
  (let* ((*sessions-dir* (temp-session-test-directory "tree-shape"))
         (session (load-or-create-session "Tree Shape"))
         (buf (make-buffer "Tree Shape"
                           :agent-name "agent"
                           :session session)))
    (rplaca::set-message-text (buffer-input-message buf) "root")
    (buffer-finalize-input buf)
    (buffer-insert-agent-message buf "answer")
    (let* ((events (session-current-events session))
           (messages (remove-if-not
                      (lambda (event)
                        (string= "message" (event-value event :event)))
                      events))
           (first-id (event-value (first messages) :id))
           (second-id (event-value (second messages) :id)))
      (is (= 2 (length messages)))
      (is (stringp first-id))
      (is (stringp second-id))
      (is (null (event-value (first messages) :parent-id)))
      (is (string= first-id
                   (event-value (second messages) :parent-id)))
      (is (string= second-id (session-current-leaf-id session)))
      (is (string= first-id
                   (message-entry-id (buffer-first-message buf)))))))

(test session-load-replays-active-branch-only
  "Loading a branched sidecar follows the manifest leaf path."
  (let* ((*sessions-dir* (temp-session-test-directory "branch-load"))
         (session-name "branch-load-session")
         (session (load-or-create-session session-name))
         (root (make-recorded-test-message :user "root"))
         (old-answer (make-recorded-test-message :agent "old answer"))
         (new-answer (make-recorded-test-message :agent "new answer")))
    (record-session-message session root)
    (let ((root-id (message-entry-id root)))
      (record-session-message session old-answer)
      (set-session-current-leaf session root-id)
      (record-session-message session new-answer)
      (let* ((loaded (load-session session-name))
             (texts (loop :for msg := (and loaded (buffer-first-message loaded))
                            :then (message-next msg)
                          :while (and msg
                                      (not (eq msg
                                               (buffer-input-message loaded))))
                          :collect (message-text msg))))
        (is (not (null loaded)))
        (let ((first (first-substantive-buffer-message loaded)))
          (is (string= "root" (message-text first)))
          (is (string= "new answer"
                       (message-text (message-next first)))))
        (is (not (member "old answer" texts :test #'string=)))))))

(test session-labels-feed-tree-selector
  "Label changes are resolved onto selector entries and filterable."
  (let* ((*sessions-dir* (temp-session-test-directory "tree-label"))
         (session (load-or-create-session "Tree Labels"))
         (buf (make-buffer "Tree Labels"
                           :agent-name "agent"
                           :session session))
         (msg (make-recorded-test-message :user "bookmark me")))
    (record-session-message session msg)
    (record-session-label-change session (message-entry-id msg) "mark")
    (session-tree-selector-activate buf (lambda (item) (declare (ignore item))))
    (let ((item (find (message-entry-id msg)
                      rplaca::*session-tree-selector-filtered-items*
                      :key (lambda (candidate) (getf candidate :id))
                      :test #'string=)))
      (is (not (null item)))
      (is (string= "mark" (getf item :label))))
    (rplaca::session-tree-selector-set-filter :labeled-only)
    (is (= 1 (length rplaca::*session-tree-selector-filtered-items*)))
    (session-tree-selector-deactivate)))

(test ensure-scratch-buffer-creates-loaded-scratch-without-stealing-current
  "The scratch buffer is loaded into the ring but does not become current."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*scratch-buffer-name* "*scratch*")
        (*scratch-buffer-initial-text* "notes"))
    (rplaca::init-default-keymap)
    (let ((chat (make-buffer "chat")))
      (setf (buffer-keymap chat) *default-keymap*)
      (add-buffer-to-ring chat)
      (let ((scratch (ensure-scratch-buffer)))
        (is (eq chat (current-buffer)))
        (is (eq scratch (scratch-buffer)))
        (is (scratch-buffer-p scratch))
        (is (eq :scratch (buffer-kind scratch)))
        (is (string= "*scratch*" (buffer-name scratch)))
        (is (string= "scratch" (buffer-major-mode scratch)))
        (is (string= "notes" (scratch-buffer-text scratch)))
        (is (eq rplaca::*scratch-keymap* (buffer-keymap scratch)))
        (is (= 2 (length *buffer-ring*)))
        (is (eq scratch (ensure-scratch-buffer)))
        (is (= 2 (length *buffer-ring*)))))))

(test scratch-buffer-text-is-programmatically-editable
  "The scratch text accessor reads and replaces the editable document."
  (let ((*buffer-ring* nil)
        (*scratch-buffer-initial-text* ""))
    (rplaca::init-default-keymap)
    (let ((scratch (ensure-scratch-buffer)))
      (setf (scratch-buffer-text scratch) "alpha")
      (is (string= "alpha" (scratch-buffer-text scratch)))
      (setf (scratch-buffer-text) "beta")
      (is (string= "beta" (scratch-buffer-text scratch))))))

(test buffer-finalize-input
  "Finalizing input makes it read-only and creates a new input message."
  (let ((buf (make-buffer "test")))
    (message-insert-char (buffer-input-message buf) #\h)
    (message-insert-char (buffer-input-message buf) #\i)
    (buffer-finalize-input buf)
    (let ((first-msg (buffer-first-message buf)))
      (is (message-read-only-p first-msg))
      (is (eq :user (message-sender first-msg)))
      (is (not (null (message-timestamp first-msg))))
      (is (string= "hi" (message-text first-msg))))
    (is (= 2 (buffer-message-count buf)))
    (is (not (message-read-only-p (buffer-input-message buf))))
    (is (string= "" (message-text (buffer-input-message buf))))
    (is (eq (buffer-last-message buf) (buffer-input-message buf)))))

(test buffer-insert-agent-message
  "Agent messages are inserted before the input message."
  (let ((buf (make-buffer "test" :agent-name "echo")))
    (message-insert-char (buffer-input-message buf) #\h)
    (message-insert-char (buffer-input-message buf) #\i)
    (buffer-finalize-input buf)
    (buffer-insert-agent-message buf "Echo: hi")
    (is (= 3 (buffer-message-count buf)))
    (let ((agent-msg (message-next (buffer-first-message buf))))
     (is (string= "Echo: hi" (message-text agent-msg)))
     (is (message-read-only-p agent-msg))
     (is (eq :echo (message-sender agent-msg))))
    (is (not (message-read-only-p (buffer-input-message buf))))))

(test buffer-provider-model-and-think-overrides
  "Buffer overrides can be set and read back."
  (let ((buf (make-buffer "test")))
    (is (null (buffer-provider-override buf)))
    (is (null (buffer-model-override buf)))
    (is (null (buffer-think-level-override buf)))
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.4")
    (set-buffer-think-level-override buf "HIGH")
    (is (eq :openai-codex (buffer-provider-override buf)))
    (is (string= "gpt-5.4" (buffer-model-override buf)))
    (is (string= "high" (buffer-think-level-override buf)))))

(test buffer-override-helpers-only-mutate-target-buffer
  "Per-buffer override helpers update only the provided buffer."
  (let ((first-buffer (make-buffer "first"))
        (second-buffer (make-buffer "second")))
    (set-buffer-provider-override first-buffer :openai-codex)
    (set-buffer-model-override first-buffer "gpt-5.3-codex")
    (set-buffer-think-level-override first-buffer "high")
    (is (eq :openai-codex (buffer-provider-override first-buffer)))
    (is (string= "gpt-5.3-codex" (buffer-model-override first-buffer)))
    (is (string= "high" (buffer-think-level-override first-buffer)))
    (is (null (buffer-provider-override second-buffer)))
    (is (null (buffer-model-override second-buffer)))
    (is (null (buffer-think-level-override second-buffer)))))

(test serialize-buffer-includes-overrides
  "Serialized sessions include provider/model/think overrides."
  (let ((buf (make-buffer "test" :agent-name "echo")))
    (setf (buffer-provider-override buf) :zai
          (buffer-model-override buf) "glm-5"
          (buffer-pipeline-name buf) "plan-build"
          (buffer-enabled-packages buf) '("sexed"))
    (set-buffer-think-level-override buf "medium")
    (let ((data (rplaca::serialize-buffer buf)))
      (is (eq :zai (cdr (assoc :provider-override data))))
      (is (string= "glm-5" (cdr (assoc :model-override data))))
      (is (string= "medium" (cdr (assoc :think-level-override data))))
      (is (string= "plan-build" (cdr (assoc :pipeline-name data))))
      (is (equal '("sexed")
                 (coerce (cdr (assoc :enabled-packages data)) 'list))))))

(test load-session-missing-overrides-default-to-nil
  "Sessions without override fields load nil overrides."
  (let* ((session-name "missing-overrides")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "rplaca-buffer-tests")))
         (path (rplaca::session-path session-name)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string
       (cl-json:encode-json-to-string
        '((:name . "missing-overrides")
          (:agent-name . "echo")
          (:messages . #((( :sender . "USER")
                          (:text . "hello")
                          (:timestamp . 0)
                          (:read-only-p . t))))))
       stream))
    (let ((cl-json:*json-array-type* 'list))
      (let ((buf (load-session session-name)))
       (is (not (null buf)))
       (is (null (buffer-provider-override buf)))
       (is (null (buffer-model-override buf)))
       (is (null (buffer-think-level-override buf)))
       (is (null (buffer-enabled-packages buf)))))))

(test read-session-manifest-returns-structured-error-on-malformed-json
  "Malformed sidecar manifests return a structured parse error object."
  (let* ((session-name "malformed-manifest")
         (*sessions-dir* (temp-session-test-directory "malformed-manifest"))
         (manifest-path (rplaca::session-sidecar-manifest-path session-name)))
    (ensure-directories-exist manifest-path)
    (with-open-file (stream manifest-path :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
      (write-string "{not valid json" stream))
    (let ((result (rplaca::read-session-manifest manifest-path)))
      (is (typep result 'rplaca::session-manifest-parse-error))
      (is (search "malformed-manifest"
                  (rplaca::session-manifest-parse-error-path result)))
      (is (search "Failed to parse session manifest"
                  (format nil "~A" result))))
    (signals rplaca::session-manifest-parse-error
      (rplaca::load-session-sidecar session-name))))

(test list-saved-sessions-skips-malformed-sidecars-with-warning
  "Malformed sidecars warn and do not hide valid saved sessions."
  (let* ((good-session "good-sidecar-session")
         (bad-session "bad-sidecar-session")
         (*sessions-dir* (temp-session-test-directory "malformed-sidecars"))
         (good-session-object (load-or-create-session good-session))
         (bad-manifest-path (rplaca::session-sidecar-manifest-path bad-session))
         (good-manifest-path (rplaca::session-sidecar-manifest-path good-session))
         (warnings nil))
    (declare (ignore good-session-object))
    (ensure-directories-exist bad-manifest-path)
    (with-open-file (stream bad-manifest-path :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
      (write-string "{not valid json" stream))
    (is (probe-file good-manifest-path))
    (handler-bind ((warning
                     (lambda (condition)
                       (push (princ-to-string condition) warnings)
                       (muffle-warning condition))))
      (let ((names (list-saved-sessions)))
        (is (member good-session names :test #'string=))
        (is (not (member bad-session names :test #'string=)))))
    (is (= 1 (length warnings)))
    (is (search "Failed to parse session manifest" (first warnings)))))

(test load-session-does-not-duplicate-snapshot-into-transcript
  "Loading a snapshot attaches a session but does not replay old messages into JSONL."
  (let* ((session-name "legacy-transcript-replay")
         (*sessions-dir* (temp-session-test-directory "load"))
         (path (rplaca::session-path session-name)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string
       (cl-json:encode-json-to-string
        '((:name . "legacy-transcript-replay")
          (:agent-name . "echo")
          (:messages . #(((:sender . "USER")
                          (:text . "old message")
                          (:timestamp . 10)
                          (:read-only-p . t))))))
       stream))
    (let ((buf (load-session session-name)))
      (is (not (null buf)))
      (is (not (null (buffer-session buf))))
      (let ((events (session-current-events (buffer-session buf))))
        (is (= 1 (length events)))
        (is (string= "session-start"
                     (event-value (first events) :event))))
      (buffer-insert-system-message buf "new display event")
      (let* ((events (session-current-events (buffer-session buf)))
             (message-events
               (remove-if-not (lambda (event)
                                (string= "message"
                                         (event-value event :event)))
                              events)))
        (is (= 1 (length message-events)))
        (is (string= "new display event"
                     (event-value (first message-events) :text)))))))

(test save-and-load-session-round-trips-overrides
  "Saved sessions preserve override values and types when reloaded."
  (let* ((session-name "override-round-trip")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "rplaca-buffer-tests")))
         (buf (make-buffer session-name :agent-name "echo")))
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.4"
          (buffer-pipeline-name buf) "plan-build"
          (buffer-enabled-packages buf) '("sexed" "lispi"))
    (set-buffer-think-level-override buf "high")
    (message-insert-char (buffer-input-message buf) #\h)
    (message-insert-char (buffer-input-message buf) #\i)
    (buffer-finalize-input buf)
    (save-session buf)
    (let* ((cl-json:*json-array-type* 'list)
           (loaded (load-session session-name)))
      (is (eq :openai-codex (buffer-provider-override loaded)))
      (is (typep (buffer-provider-override loaded) 'keyword))
      (is (string= "gpt-5.4" (buffer-model-override loaded)))
      (is (typep (buffer-model-override loaded) 'string))
      (is (string= "high" (buffer-think-level-override loaded)))
      (is (typep (buffer-think-level-override loaded) 'string))
      (is (string= "plan-build" (buffer-pipeline-name loaded)))
      (is (equal '("sexed" "lispi")
                 (buffer-enabled-packages loaded))))))

(test save-and-load-session-round-trips-working-directory
  "Saved sessions preserve the buffer working directory and session root."
  (let* ((session-name "working-directory-round-trip")
         (working-directory #P"/tmp/rplaca-working-directory-session/")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "rplaca-buffer-tests")))
         (session (load-or-create-session session-name
                                          :working-directory working-directory))
         (buf (make-buffer session-name
                           :agent-name "echo"
                           :session session
                           :working-directory working-directory)))
    (rplaca::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (save-session buf)
    (let ((loaded (load-session session-name)))
      (is (not (null loaded)))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                 (buffer-working-directory loaded)))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                 (rplaca::session-working-directory
                  (buffer-session loaded)))))))

(test save-and-load-session-round-trips-display-name
  "Saved sessions preserve a display name separate from the buffer name."
  (let* ((session-name "display-name-round-trip")
         (*sessions-dir* (temp-session-test-directory "display-name"))
         (session (load-or-create-session session-name
                                          :display-name "Focus Session"))
         (buf (make-buffer session-name
                           :agent-name "echo"
                           :session session)))
    (rplaca::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (save-session buf)
    (let ((loaded (load-session session-name)))
      (is (string= "Focus Session"
                   (rplaca::session-display-name
                    (buffer-session loaded)))))
    (let ((records (rplaca::list-saved-session-records)))
      (let ((record (find session-name records
                          :key (lambda (entry)
                                 (getf entry :session-name))
                          :test #'string=)))
        (is (not (null record)))
        (is (string= "Focus Session" (getf record :display-name)))
        (is (string= (rplaca::session-id session)
                     (getf record :session-id)))))))

(test load-session-accepts-unique-session-id-prefix
  "Saved sessions can be loaded by a unique session id prefix."
  (let* ((session-name "load-by-id-prefix")
         (*sessions-dir* (temp-session-test-directory "id-prefix"))
         (session (load-or-create-session session-name))
         (buf (make-buffer session-name
                           :agent-name "echo"
                           :session session))
         (prefix (subseq (rplaca::session-id session) 0 8)))
    (rplaca::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (save-session buf)
    (let ((loaded (load-session prefix)))
      (is (not (null loaded)))
      (is (string= session-name (buffer-name loaded))))))

(test load-session-accepts-explicit-manifest-path
  "Saved sessions can be loaded directly from a sidecar manifest path."
  (let* ((session-name "load-by-path")
         (*sessions-dir* (temp-session-test-directory "manifest-path"))
         (session (load-or-create-session session-name))
         (buf (make-buffer session-name
                           :agent-name "echo"
                           :session session))
         (manifest-path
           (rplaca::session-sidecar-manifest-path session-name)))
    (rplaca::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (save-session buf)
    (let ((loaded (load-session (namestring manifest-path))))
      (is (not (null loaded)))
      (is (string= session-name (buffer-name loaded))))))

(test retired-listener-buffer-state-migrates-through-session-load
  "Retired listener snapshots preserve only frame-relevant context as chat."
  (let* ((session-name "listener-state-round-trip")
         (working-directory #P"/tmp/rplaca-listener-session/")
         (stack-entry #P"/tmp/rplaca-listener-stack/")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "rplaca-buffer-tests")))
         (rplaca::*buffer-type-registry* (rplaca::make-buffer-type-registry))
         (rplaca::*listener-buffer-states* (make-hash-table :test #'eq))
         (session (load-or-create-session session-name
                                          :working-directory working-directory))
         (buf (make-buffer session-name
                           :kind :listener
                           :major-mode "listener"
                           :working-directory working-directory)))
    (register-buffer-type
     :listener
     :serialize-state-function 'rplaca::listener-serialize-buffer-state
     :restore-state-function 'rplaca::listener-restore-buffer-state)
    (setf (buffer-session buf) session)
    (setf (rplaca::listener-state-package-name
           (rplaca::listener-buffer-state buf))
          "KEYWORD"
          (rplaca::listener-state-directory-stack
           (rplaca::listener-buffer-state buf))
          (list stack-entry)
          (rplaca::listener-state-last-values
           (rplaca::listener-buffer-state buf))
          '(42 "done")
          (rplaca::listener-state-command-history
           (rplaca::listener-buffer-state buf))
          '(",pwd" "(+ 1 2)"))
    (save-session buf)
    (let* ((snapshot-path (rplaca::session-path session-name))
           (snapshot-before (uiop:read-file-string snapshot-path))
           (loaded (load-session session-name))
           (context (rplaca::listener-context-for-buffer loaded)))
      (is (eq :chat (buffer-kind loaded)))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                  (buffer-working-directory loaded)))
      (is (string= "KEYWORD"
                   (rplaca::listener-context-package-name context)))
      (is (equal (list (uiop:ensure-directory-pathname stack-entry))
                  (rplaca::listener-context-directory-stack context)))
      (is (every #'rplaca::buffer-ephemeral-display-message-p
                 (buffer-test-history-messages loaded)))
      (is (string= snapshot-before
                   (uiop:read-file-string snapshot-path))))))

(test clear-buffer-overrides-clears-think-level
  "Clearing buffer overrides also clears think-level state."
  (let ((buf (make-buffer "test")))
    (set-buffer-provider-override buf :openai-codex)
    (set-buffer-model-override buf "gpt-5.4")
    (set-buffer-think-level-override buf "high")
    (clear-buffer-routing-overrides buf)
    (is (null (buffer-provider-override buf)))
    (is (null (buffer-model-override buf)))
    (is (null (buffer-think-level-override buf)))))

(test load-session-normalizes-legacy-raw-content
  "Legacy saved session raw-content is normalized to canonical blocks on load."
  (let* ((session-name "legacy-raw-content")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "rplaca-buffer-tests")))
         (path (rplaca::session-path session-name)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string
       "{\"name\":\"legacy-raw-content\",\"agent-name\":\"echo\",\"messages\":[{\"sender\":\"ECHO\",\"text\":\"Searching\",\"timestamp\":1,\"read-only-p\":true,\"raw-content\":[{\"type\":\"text\",\"text\":\"Searching\"},{\"type\":\"tool_use\",\"id\":\"toolu_123\",\"name\":\"read_file\",\"input\":{\"path\":\"/tmp/example.txt\"}}]},{\"sender\":\"TOOL-RESULT\",\"text\":\"done\",\"timestamp\":2,\"read-only-p\":true,\"raw-content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_123\",\"content\":\"done\"}]}]}"
       stream))
    (let ((buf (load-session session-name)))
      (is (not (null buf)))
      (let* ((assistant-msg (first-substantive-buffer-message buf))
             (tool-result-msg (message-next assistant-msg)))
        (is (equal '(((:type . "text")
                      (:text . "Searching"))
                     ((:type . "tool_use")
                      (:id . "toolu_123")
                      (:name . "read_file")
                      (:input . ((:path . "/tmp/example.txt")))))
                   (message-raw-content assistant-msg)))
        (is (equal '(((:type . "tool_result")
                      (:tool--use--id . "toolu_123")
                      (:content . "done")))
                   (message-raw-content tool-result-msg)))))))

(test previous-command-argument-extraction
  "Extract first and last argument from previous user command."
  (let ((buf (make-buffer "test")))
    (rplaca::set-message-text (buffer-input-message buf) "git commit -m msg")
    (buffer-finalize-input buf)
    (is (string= "commit" (rplaca::buffer-previous-command-first-argument buf)))
    (is (string= "msg" (rplaca::buffer-previous-command-last-argument buf)))))

(test previous-command-argument-extraction-no-arguments
  "Return nil when previous command has no arguments."
  (let ((buf (make-buffer "test")))
    (rplaca::set-message-text (buffer-input-message buf) "ls")
    (buffer-finalize-input buf)
    (is (null (rplaca::buffer-previous-command-first-argument buf)))
    (is (string= "ls" (rplaca::buffer-previous-command-last-argument buf)))))

(test handle-key-event-control-l-requests-redraw-in-minibuffer-mode
  "Ctrl+l requests a redraw even when modal UI state like the minibuffer is active."
  (let ((rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state))
        (*buffer-selector-active* nil)
        (*model-selector-active* nil)
        (*think-selector-active* nil)
        (*openai-oauth-pending* nil))
    (setf *minibuffer-active* t)
    (let ((buf (make-buffer "test")))
      (is (eq :redraw
              (rplaca::handle-key-event buf (code-char 12)))))))

(test minibuffer-completion-navigation-wraps-with-tab-and-backtab
  "Completion navigation accepts Emacs-style keys and wraps at both ends."
  (let ((rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state))
        (*minibuffer-max-height* 3))
    (minibuffer-activate
     "Pick"
     (list (list :display "alpha")
           (list :display "bravo")
           (list :display "charlie"))
     (lambda (item) (declare (ignore item))))
    (is (= 0 *minibuffer-selected-index*))
    (handle-minibuffer-key #\Tab)
    (is (= 1 *minibuffer-selected-index*))
    (handle-minibuffer-key :down)
    (is (= 2 *minibuffer-selected-index*))
    (handle-minibuffer-key (code-char 14))
    (is (= 0 *minibuffer-selected-index*))
    (handle-minibuffer-key '(:ctrl #\n))
    (is (= 1 *minibuffer-selected-index*))
    (handle-minibuffer-key '(:control #\n))
    (is (= 2 *minibuffer-selected-index*))
    (handle-minibuffer-key #\Tab)
    (is (= 0 *minibuffer-selected-index*))
    (is (= 0 *minibuffer-scroll-offset*))
    (handle-minibuffer-key :backtab)
    (is (= 2 *minibuffer-selected-index*))
    (handle-minibuffer-key '(:control #\p))
    (is (= 1 *minibuffer-selected-index*))
    (handle-minibuffer-key '(:meta #\Tab))
    (is (= 0 *minibuffer-selected-index*))
    (handle-minibuffer-key '(:meta :tab))
    (is (= 2 *minibuffer-selected-index*))
    (handle-minibuffer-key :up)
    (is (= 1 *minibuffer-selected-index*))))

(test minibuffer-completion-return-with-no-match-keeps-query
  "Return on an empty completion result keeps the minibuffer active."
  (let ((submitted nil)
        (rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state)))
    (minibuffer-activate
     "Pick"
     (list (list :display "alpha"))
     (lambda (item) (setf submitted item)))
    (handle-minibuffer-key #\z)
    (is (null *minibuffer-filtered-items*))
    (handle-minibuffer-key #\Return)
    (is-true *minibuffer-active*)
    (is (string= "z" *minibuffer-input*))
    (is (null submitted))))

(test minibuffer-completion-mode-supports-emacs-motion-keys
  "Completion minibuffers support compose-like C-b/C-f/M-b/M-f editing motions."
  (let ((rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state)))
    (minibuffer-activate
     "Edit"
     (list (list :display "foo bar baz"))
     (lambda (item) (declare (ignore item)))
     :initial-input "foo bar")
    (is (= 7 *minibuffer-point*))
    (handle-minibuffer-key '(:meta #\b))
    (is (= 4 *minibuffer-point*))
    (handle-minibuffer-key (code-char 2))
    (is (= 3 *minibuffer-point*))
    (handle-minibuffer-key '(:control #\f))
    (is (= 4 *minibuffer-point*))
    (handle-minibuffer-key '(:meta #\f))
    (is (= 7 *minibuffer-point*))
    (is (string= "foo bar" *minibuffer-input*))))

(test minibuffer-completion-mode-supports-emacs-kill-and-yank-keys
  "Completion minibuffers support C-d/M-d/M-Backspace/C-k/C-y editing."
  (let ((rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state))
        (rplaca::*kill-ring* nil))
    (minibuffer-activate
     "Edit"
     (list (list :display "foo bar baz"))
     (lambda (item) (declare (ignore item)))
     :initial-input "foo bar baz")
    (setf *minibuffer-point* 4)
    (handle-minibuffer-key (code-char 4))
    (is (string= "foo ar baz" *minibuffer-input*))
    (setf *minibuffer-input* "foo bar baz"
          *minibuffer-point* 4)
    (minibuffer-update-filter)
    (handle-minibuffer-key '(:meta #\d))
    (is (string= "foo  baz" *minibuffer-input*))
    (is (= 4 *minibuffer-point*))
    (is (string= "bar" (first rplaca::*kill-ring*)))
    (handle-minibuffer-key '(:control #\y))
    (is (string= "foo bar baz" *minibuffer-input*))
    (setf *minibuffer-point* 8)
    (handle-minibuffer-key '(:meta :backspace))
    (is (string= "foo baz" *minibuffer-input*))
    (is (= 4 *minibuffer-point*))
    (handle-minibuffer-key (code-char 11))
    (is (string= "foo " *minibuffer-input*))
    (setf *minibuffer-input* "foo bar"
          *minibuffer-point* 7)
    (minibuffer-update-filter)
    (handle-minibuffer-key (code-char 23))
    (is (string= "foo " *minibuffer-input*))
    (is (= 4 *minibuffer-point*))))

(test minibuffer-prompt-mode-preserves-editing-keys
  "Prompt mode still edits raw input instead of cycling completion candidates."
  (let ((submitted nil)
        (rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state)))
    (minibuffer-prompt "Raw" (lambda (input) (setf submitted input)))
    (handle-minibuffer-key #\a)
    (handle-minibuffer-key #\b)
    (handle-minibuffer-key :down)
    (handle-minibuffer-key :backtab)
    (is (string= "ab" *minibuffer-input*))
    (handle-minibuffer-key #\Backspace)
    (is (string= "a" *minibuffer-input*))
    (handle-minibuffer-key #\Return)
    (is (string= "a" submitted))))

;;; --------------------------------------------------------------------------
;;; M-x Tests
;;; --------------------------------------------------------------------------

(test default-keymap-binds-m-x-to-execute-extended-command
  "The default keymap exposes M-x as execute-extended-command."
  (rplaca::init-default-keymap)
  (is (eq 'execute-extended-command
          (keymap-lookup *default-keymap* '(:meta #\x))))
  (is (null (keymap-lookup *default-keymap* '(:alt #\x)))))

(test execute-extended-command-opens-the-command-picker
  "M-x opens the minibuffer with commands."
  (with-interactive-command-test-buffer (buf)
    (rplaca::handle-key-event buf '(:meta #\x))
    (is (eq t *minibuffer-active*))
    (is (eq :completion *minibuffer-mode*))
    (is (string= "M-x" *minibuffer-prompt*))
    (is (position-if (lambda (item)
                       (eq 'mx-test-noarg-command (getf item :command)))
                     *minibuffer-filtered-items*))
    (is (position-if (lambda (item)
                       (eq 'mx-test-arg-command (getf item :command)))
                     *minibuffer-filtered-items*))))

(test execute-extended-command-runs-noarg-commands-immediately
  "Selecting a no-arg command from M-x invokes it without another prompt."
  (with-interactive-command-test-buffer (buf)
    (rplaca::handle-key-event buf '(:meta #\x))
    (select-minibuffer-command 'mx-test-noarg-command)
    (is (null *minibuffer-active*))
    (is (equal '(:noarg) *mx-test-command-log*))))

(test execute-extended-command-fuzzy-matches-and-runs-selection
  "M-x filters command names by fuzzy abbreviation and Return runs the selection."
  (with-interactive-command-test-buffer (buf)
    (rplaca::handle-key-event buf '(:meta #\x))
    (dolist (char (coerce "mxno" 'list))
      (rplaca::handle-key-event buf char))
    (is (string= "mxno" *minibuffer-input*))
    (is (eq 'mx-test-noarg-command
            (getf (first *minibuffer-filtered-items*) :command)))
    (handle-minibuffer-key #\Return)
    (is (null *minibuffer-active*))
    (is (equal '(:noarg) *mx-test-command-log*))))

(test execute-extended-command-prompts-for-each-argument
  "Selecting a parameterized command prompts for each command argument."
  (with-interactive-command-test-buffer (buf)
    (rplaca::handle-key-event buf '(:meta #\x))
    (select-minibuffer-command 'mx-test-arg-command)
    (is (eq t *minibuffer-active*))
    (is (eq :prompt *minibuffer-mode*))
    (is (string= "Count" *minibuffer-prompt*))
    (setf *minibuffer-input* "7"
          *minibuffer-point* 1)
    (minibuffer-confirm)
    (is (eq t *minibuffer-active*))
    (is (eq :prompt *minibuffer-mode*))
    (is (string= "Label" *minibuffer-prompt*))
    (setf *minibuffer-input* "demo"
          *minibuffer-point* 4)
    (minibuffer-confirm)
    (is (null *minibuffer-active*))
    (is (equal '(:args 7 "demo") *mx-test-command-log*))))

(test execute-extended-command-reprompts-on-invalid-argument
  "Reader errors keep the command pending and preserve the user's input."
  (with-interactive-command-test-buffer (buf)
    (rplaca::handle-key-event buf '(:meta #\x))
    (select-minibuffer-command 'mx-test-arg-command)
    (setf *minibuffer-input* "oops"
          *minibuffer-point* 4)
    (minibuffer-confirm)
    (is (eq t *minibuffer-active*))
    (is (eq :prompt *minibuffer-mode*))
    (is (string= "Count" *minibuffer-prompt*))
    (is (string= "oops" *minibuffer-input*))
    (is (null *mx-test-command-log*))
    (let ((msg (latest-buffer-message buf)))
      (is (not (null msg)))
      (is (eq :system (message-sender msg)))
      (is (search "Invalid count" (message-text msg))))))

(test execute-extended-command-cancels-parameter-prompts-with-c-g
  "Cancelling a prompted command abandons the invocation cleanly."
  (with-interactive-command-test-buffer (buf)
    (rplaca::handle-key-event buf '(:meta #\x))
    (select-minibuffer-command 'mx-test-arg-command)
    (handle-minibuffer-key (code-char 7))
    (is (null *minibuffer-active*))
    (is (null *mx-test-command-log*))))

(test key-dispatch-prompts-for-parameterized-commands
  "Key-dispatched commands use the same prompt path as M-x."
  (with-interactive-command-test-buffer (buf)
    (let ((km (make-keymap :test :parent *default-keymap*)))
      (keymap-bind km #\? 'mx-test-arg-command)
      (setf (buffer-keymap buf) km)
      (rplaca::handle-key-event buf #\?)
      (is (eq t *minibuffer-active*))
      (is (eq :prompt *minibuffer-mode*))
      (is (string= "Count" *minibuffer-prompt*)))))

;;; --------------------------------------------------------------------------
;;; Buffer Selector Tests
;;; --------------------------------------------------------------------------

(test buffer-recency-sort-compares-history-with-buffer-names
  "Decorated selector rows still honor the plain names stored in history."
  (let ((*buffer-selection-history* '("bravo" "alpha")))
    (let* ((alpha (list :name "alpha"
                        :display "  alpha [idle] msgs:0"))
           (bravo (list :name "bravo"
                        :display "  bravo [idle] msgs:0"))
           (charlie (list :name "charlie"
                          :current-p t
                          :display "* charlie [idle] msgs:0"))
           (sorted (sort-buffers-by-recency
                    (list charlie alpha bravo))))
      (is (equal '("bravo" "alpha" "charlie")
                 (mapcar (lambda (item) (getf item :name)) sorted))))))

(test buffer-recency-sort-preserves-ring-order-without-history
  "The previous ring buffer remains the default when no selector history exists."
  (let ((*buffer-selection-history* nil))
    (let* ((current (list :name "current" :current-p t
                          :display "* current"))
           (previous (list :name "zulu" :display "  zulu"))
           (older (list :name "alpha" :display "  alpha"))
           (sorted (sort-buffers-by-recency
                    (list current previous older))))
      (is (equal '("current" "zulu" "alpha")
                 (mapcar (lambda (item) (getf item :name)) sorted))))))

(test minibuffer-query-selects-the-highest-scoring-match
  "Typing after an explicit preselection resets Return to fuzzy result zero."
  (let ((rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state)))
    (minibuffer-activate
     "Pick"
     (list (list :display "switch-e2e-0")
           (list :display "switch-e2e-1")
           (list :display "unrelated"))
     #'identity)
    (setf *minibuffer-selected-index* 1)
    (dolist (character (coerce "switch-e2e-0" 'list))
      (rplaca::minibuffer-insert-char character))
    (is (= 0 *minibuffer-selected-index*))
    (is (string= "switch-e2e-0"
                 (getf (first *minibuffer-filtered-items*) :display)))))

(test buffer-selector-fuzzy-matches-semantic-names
  "An exact buffer name outranks prefix neighbors despite decorated rows."
  (let ((*buffer-selection-history* nil)
        (rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state)))
    (let* ((current (make-buffer "selector-origin"
                                 :session-persistence-mode :ephemeral))
           (exact (make-buffer "switch-e2e-1"
                               :session-persistence-mode :ephemeral))
           (neighbor-10 (make-buffer "switch-e2e-10"
                                     :session-persistence-mode :ephemeral))
           (neighbor-11 (make-buffer "switch-e2e-11"
                                     :session-persistence-mode :ephemeral))
           (*buffer-ring* (list current neighbor-11 neighbor-10 exact)))
      (rplaca::minibuffer-select-buffer-command current)
      (is (every (lambda (item)
                   (string= (getf item :name)
                            (getf item :match-text)))
                 *minibuffer-items*))
      (dolist (character (coerce "switch-e2e-1" 'list))
        (rplaca::minibuffer-insert-char character))
      (is (string= "switch-e2e-1"
                   (getf (first *minibuffer-filtered-items*) :name))))))

(test visible-buffer-selector-return-defaults-to-another-buffer
  "Unfiltered Return selects a non-current buffer instead of doing nothing."
  (let ((*buffer-ring* nil)
        (*buffer-selection-history* nil)
        (rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state)))
    (let ((current (make-buffer "selector-current"
                                :session-persistence-mode :ephemeral))
          (other (make-buffer "selector-other"
                              :session-persistence-mode :ephemeral)))
      (add-buffer-to-ring other)
      (add-buffer-to-ring current)
      (rplaca::minibuffer-select-buffer-command current)
      (is-true *minibuffer-active*)
      (is (eq other
              (getf (nth *minibuffer-selected-index*
                         *minibuffer-filtered-items*)
                    :buffer)))
      (dolist (character (coerce "selector-current" 'list))
        (rplaca::minibuffer-insert-char character))
      (is (eq current
              (getf (first *minibuffer-filtered-items*) :buffer)))
      (dotimes (ignored (length "selector-current"))
        (declare (ignore ignored))
        (rplaca::minibuffer-delete-backward))
      (is (eq other
              (getf (nth *minibuffer-selected-index*
                         *minibuffer-filtered-items*)
                    :buffer)))
      (minibuffer-confirm)
      (is (eq other (current-buffer)))
      (is (string= "selector-other"
                   (first *buffer-selection-history*))))))

(test list-buffers-command-uses-visible-minibuffer
  "The legacy command name delegates to the visible minibuffer selector."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state))
        (*buffer-selector-active* nil)
        (*buffer-selector-index* 99)
        (rplaca::*buffer-selector-scroll* 99))
    (rplaca::init-default-keymap)
    (let ((buf (make-buffer "test")))
      (setf (buffer-keymap buf) *default-keymap*)
      (add-buffer-to-ring buf)
      (list-buffers-command buf)
      (is-false *buffer-selector-active*)
      (is-true *minibuffer-active*)
      (is (string= "Switch Buffer" *minibuffer-prompt*))
      (is (= 1 (length *minibuffer-filtered-items*))))))

(test buffer-selector-navigate-down-and-up
  "Navigating the buffer selector moves the index."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 0)
        (rplaca::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "session-1"))
          (buf2 (make-buffer "session-2"))
          (buf3 (make-buffer "session-3")))
      (add-buffer-to-ring buf3)
      (add-buffer-to-ring buf2)
      (add-buffer-to-ring buf1)
      ;; Navigate down (C-n = ASCII 14)
      (rplaca::handle-buffer-selector-key (code-char 14))
      (is (= 1 *buffer-selector-index*))
      (rplaca::handle-buffer-selector-key (code-char 14))
      (is (= 2 *buffer-selector-index*))
      ;; Can't go past end
      (rplaca::handle-buffer-selector-key (code-char 14))
      (is (= 2 *buffer-selector-index*))
      ;; Navigate up (C-p = ASCII 16)
      (rplaca::handle-buffer-selector-key (code-char 16))
      (is (= 1 *buffer-selector-index*)))))

(test buffer-selector-enter-selects-buffer
  "Pressing Enter in the buffer selector switches to the highlighted buffer."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 1)
        (rplaca::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "session-1"))
          (buf2 (make-buffer "session-2")))
      (add-buffer-to-ring buf2)
      (add-buffer-to-ring buf1)
      ;; Select index 1 (session-2) and press Enter
      (rplaca::handle-buffer-selector-key #\Newline)
      (is (eq nil *buffer-selector-active*))
      (is (eq buf2 (current-buffer))))))

(test buffer-selector-cancel-with-c-g
  "C-g closes the buffer selector without switching."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 1)
        (rplaca::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "session-1"))
          (buf2 (make-buffer "session-2")))
      (add-buffer-to-ring buf2)
      (add-buffer-to-ring buf1)
      ;; C-g = ASCII 7
      (rplaca::handle-buffer-selector-key (code-char 7))
      (is (eq nil *buffer-selector-active*))
      ;; Current buffer unchanged (buf1 is still first)
      (is (eq buf1 (current-buffer))))))

(test buffer-selector-new-buffer
  "Pressing n creates a new buffer and closes the selector."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 0)
        (rplaca::*buffer-selector-scroll* 0))
    (rplaca::init-default-keymap)
    (let ((buf1 (make-buffer "session-1")))
      (setf (buffer-keymap buf1) *default-keymap*)
      (add-buffer-to-ring buf1)
      (rplaca::handle-buffer-selector-key #\n)
      (is (eq nil *buffer-selector-active*))
      (is (= 2 (length *buffer-ring*)))
      (is (string= "session-1" (buffer-name buf1)))
      ;; The new buffer is now current
      (is (not (eq buf1 (current-buffer)))))))

(test buffer-selector-kill-buffer
  "Pressing k kills the highlighted buffer."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 1)
        (rplaca::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "session-1"))
          (buf2 (make-buffer "session-2"))
          (buf3 (make-buffer "session-3")))
      (add-buffer-to-ring buf3)
      (add-buffer-to-ring buf2)
      (add-buffer-to-ring buf1)
      ;; Kill index 1 (session-2)
      (rplaca::handle-buffer-selector-key #\k)
      (is (= 2 (length *buffer-ring*)))
      (is (null (find-buffer-by-name "session-2")))
      ;; Index clamped
      (is (<= *buffer-selector-index* (1- (length *buffer-ring*))))))

  ;; Cannot kill the last buffer
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 0)
        (rplaca::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "only-buffer")))
      (add-buffer-to-ring buf1)
      (rplaca::handle-buffer-selector-key #\k)
      (is (= 1 (length *buffer-ring*)))
      (is (eq buf1 (current-buffer))))))

(test scratch-buffer-cannot-be-killed
  "Scratch remains loaded when kill commands target it."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*scratch-buffer-initial-text* ""))
    (rplaca::init-default-keymap)
    (let ((chat (make-buffer "chat")))
      (setf (buffer-keymap chat) *default-keymap*)
      (add-buffer-to-ring chat)
      (let ((scratch (ensure-scratch-buffer)))
        (is (= 2 (length *buffer-ring*)))
        (kill-buffer-from-ring scratch)
        (is (= 2 (length *buffer-ring*)))
        (is (eq scratch (scratch-buffer)))
        (switch-to-buffer scratch)
        (rplaca::kill-buffer-command scratch)
        (is (= 2 (length *buffer-ring*)))
        (is (eq scratch (scratch-buffer)))
        (kill-buffer-from-ring chat)
        (is (= 1 (length *buffer-ring*)))
        (is (eq scratch (current-buffer)))))))

(test buffer-selector-does-not-kill-scratch-buffer
  "The buffer selector kill key refuses to remove scratch."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 1)
        (rplaca::*buffer-selector-scroll* 0)
        (*scratch-buffer-initial-text* ""))
    (rplaca::init-default-keymap)
    (let ((chat (make-buffer "chat")))
      (setf (buffer-keymap chat) *default-keymap*)
      (add-buffer-to-ring chat)
      (let ((scratch (ensure-scratch-buffer)))
        (is (equal (list chat scratch) *buffer-ring*))
        (rplaca::handle-buffer-selector-key #\k)
        (is (= 2 (length *buffer-ring*)))
        (is (eq scratch (scratch-buffer)))))))

(test scratch-return-inserts-newline-without-sending
  "RET edits scratch text instead of finalizing and sending a chat turn."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state))
        (*buffer-selector-active* nil)
        (*model-selector-active* nil)
        (*think-selector-active* nil)
        (*openai-oauth-pending* nil)
        (*scratch-buffer-initial-text* ""))
    (rplaca::init-default-keymap)
    (let ((scratch (ensure-scratch-buffer)))
      (rplaca::handle-key-event scratch #\a)
      (rplaca::handle-key-event scratch #\Return)
      (rplaca::handle-key-event scratch #\b)
      (is (= 1 (buffer-message-count scratch)))
      (is (string= (format nil "a~%b") (scratch-buffer-text scratch))))))

(test save-session-command-skips-scratch-buffer
  "Saving scratch does not write a session file."
  (let* ((*buffer-ring* nil)
         (rplaca::*buffer-counter* 0)
         (*scratch-buffer-name* "scratch-test")
         (*scratch-buffer-initial-text* "draft")
         (*sessions-dir*
           (make-pathname
            :directory (list :absolute "tmp"
                             (format nil "rplaca-scratch-test-~A"
                                     (string-downcase
                                      (symbol-name (gensym "DIR")))))))
         (path (rplaca::session-path *scratch-buffer-name*)))
    (rplaca::init-default-keymap)
    (let ((scratch (ensure-scratch-buffer)))
      (rplaca::save-session-command scratch)
      (is (not (probe-file path)))
      (is (string= "draft" (scratch-buffer-text scratch)))
      (let ((notice (message-prev (buffer-input-message scratch))))
        (is (not (null notice)))
        (is (eq :system (message-sender notice)))
        (is (search "not saved" (message-text notice)))))))

(test load-session-command-opens-minibuffer-selector
  "Loading sessions opens the minibuffer with saved-session candidates."
  (with-interactive-command-test-buffer (buf)
    (let* ((session-name-a "saved-session-a")
           (session-name-b "saved-session-b")
           (*sessions-dir* (temp-session-test-directory "selector")))
      (save-session (make-buffer session-name-b :agent-name "echo"))
      (save-session (make-buffer session-name-a :agent-name "echo"))
      (rplaca::load-session-command buf)
      (is (eq t *minibuffer-active*))
      (is (eq :completion *minibuffer-mode*))
      (is (string= "Load Session" *minibuffer-prompt*))
      (is (equal (list session-name-a session-name-b)
                 (mapcar (lambda (item)
                           (getf item :session-name))
                         *minibuffer-filtered-items*))))))

(test load-session-command-shows-display-names-and-searches-session-ids
  "The load-session selector exposes display names and id/path match text."
  (with-interactive-command-test-buffer (buf)
    (let* ((session-name "saved-session-display")
           (*sessions-dir* (temp-session-test-directory "selector-display"))
           (session (load-or-create-session session-name
                                            :display-name "Focused Work"))
           (saved (make-buffer session-name :agent-name "echo" :session session)))
      (save-session saved)
      (rplaca::load-session-command buf)
      (let ((item (find session-name *minibuffer-filtered-items*
                        :key (lambda (entry)
                               (getf entry :session-name))
                        :test #'string=)))
        (is (not (null item)))
        (is (search "Focused Work" (getf item :display)))
        (is (search (rplaca::session-id session)
                    (getf item :match-text)))
        (is (search (namestring (rplaca::session-path session-name))
                    (getf item :match-text)))))))

(test load-session-command-preselects-most-recent-session-for-working-directory
  "The load-session selector preselects the newest session for the buffer cwd."
  (with-interactive-command-test-buffer (buf)
    (let* ((working-directory #P"/tmp/rplaca-load-session-selector/")
           (*sessions-dir* (temp-session-test-directory "selector-recent"))
           (older-session (load-or-create-session "selector-older"
                                                  :working-directory
                                                  working-directory))
           (newer-session (load-or-create-session "selector-newer"
                                                  :working-directory
                                                  working-directory))
           (other-session (load-or-create-session "selector-other"
                                                  :working-directory
                                                  #P"/tmp/rplaca-selector-other/"))
           (older-buffer (make-buffer "selector-older"
                                      :agent-name "echo"
                                      :working-directory working-directory
                                      :session older-session))
           (newer-buffer (make-buffer "selector-newer"
                                      :agent-name "echo"
                                      :working-directory working-directory
                                      :session newer-session))
           (other-buffer (make-buffer "selector-other"
                                      :agent-name "echo"
                                      :working-directory #P"/tmp/rplaca-selector-other/"
                                      :session other-session)))
      (setf (rplaca::session-updated-at older-session) 10
            (rplaca::session-updated-at newer-session) 20
            (rplaca::session-updated-at other-session) 30
            (buffer-working-directory buf) working-directory)
      (save-session older-buffer)
      (save-session newer-buffer)
      (save-session other-buffer)
      (rplaca::load-session-command buf)
      (let ((selected (nth *minibuffer-selected-index*
                           *minibuffer-filtered-items*)))
        (is (string= "selector-newer"
                     (getf selected :session-name)))))))

(test load-session-command-loads-selected-session-into-a-new-buffer
  "Selecting a saved session loads it into a new current buffer."
  (with-interactive-command-test-buffer (buf)
    (let* ((session-name "saved-session-load")
           (*sessions-dir* (temp-session-test-directory "load-command"))
           (saved (make-buffer session-name :agent-name "echo")))
      (rplaca::set-message-text (buffer-input-message saved) "hello")
      (buffer-finalize-input saved)
      (save-session saved)
      (rplaca::load-session-command buf)
      (let ((index (position session-name *minibuffer-filtered-items*
                             :key (lambda (item)
                                    (getf item :session-name))
                             :test #'string=)))
        (is (not (null index)))
        (setf *minibuffer-selected-index* index)
        (minibuffer-confirm))
      (let ((loaded (current-buffer)))
        (is (not (eq buf loaded)))
        (is (string= session-name (buffer-name loaded)))
        (is (string= "echo" (buffer-agent-name loaded)))
        (is (eq *default-keymap* (buffer-keymap loaded)))
        (is (not (null (buffer-session loaded))))
        (is (member session-name *buffer-selection-history* :test #'string=))
        (let ((msg (first-substantive-buffer-message loaded)))
          (is (not (eq msg (buffer-input-message loaded))))
          (is (eq :user (message-sender msg)))
          (is (string= "hello" (message-text msg))))))))

(test load-session-command-loads-fresh-buffer-even-when-session-is-open
  "Explicit load-session opens a distinct loaded buffer for an already-open session."
  (with-interactive-command-test-buffer (buf)
    (let* ((session-name "saved-session-open")
           (*sessions-dir* (temp-session-test-directory "load-command-open"))
           (saved (make-buffer session-name :agent-name "echo")))
      (rplaca::set-message-text (buffer-input-message saved) "hello")
      (buffer-finalize-input saved)
      (save-session saved)
      (let ((existing (load-session session-name)))
        (is (not (null existing)))
        (add-buffer-to-ring existing)
        (rplaca::load-session-command buf)
        (let ((index (position session-name *minibuffer-filtered-items*
                               :key (lambda (item)
                                      (getf item :session-name))
                               :test #'string=)))
          (is (not (null index)))
          (setf *minibuffer-selected-index* index)
          (minibuffer-confirm))
        (let ((loaded (current-buffer)))
          (is (not (eq existing loaded)))
          (is (string= "saved-session-open<2>" (buffer-name loaded)))
          (is (string= session-name
                       (rplaca::session-name (buffer-session loaded)))))))))

(test new-buffer-command-creates-loadable-session
  "New interactive chat buffers are saved immediately for later loading."
  (with-interactive-command-test-buffer (buf)
    (let ((*sessions-dir* (temp-session-test-directory "new-command")))
      (rplaca::new-buffer-command buf)
      (let* ((created (current-buffer))
             (session-name (buffer-name created)))
        (is (not (eq buf created)))
        (is (not (null (buffer-session created))))
        (is (probe-file (rplaca::session-path session-name)))
        (is (member session-name (list-saved-sessions) :test #'string=))
        (let ((loaded (load-session session-name)))
          (is (not (null loaded)))
          (is (string= session-name (buffer-name loaded))))))))

(test continue-session-command-loads-most-recent-session-for-working-directory
  "Continuing a session picks the most recent saved session for the buffer cwd."
  (with-interactive-command-test-buffer (buf)
    (let* ((working-directory #P"/tmp/rplaca-continue-session/")
           (*sessions-dir* (temp-session-test-directory "continue-session"))
           (older-session (load-or-create-session "older-session"
                                                  :working-directory
                                                  working-directory))
           (newer-session (load-or-create-session "newer-session"
                                                  :working-directory
                                                  working-directory))
           (other-session (load-or-create-session "other-session"
                                                  :working-directory
                                                  #P"/tmp/rplaca-other-session/"))
           (older-buffer (make-buffer "older-session"
                                      :agent-name "echo"
                                      :working-directory working-directory
                                      :session older-session))
           (newer-buffer (make-buffer "newer-session"
                                      :agent-name "echo"
                                      :working-directory working-directory
                                      :session newer-session))
           (other-buffer (make-buffer "other-session"
                                      :agent-name "echo"
                                      :working-directory #P"/tmp/rplaca-other-session/"
                                      :session other-session)))
      (setf (rplaca::session-created-at older-session) 10
            (rplaca::session-updated-at older-session) 10
            (rplaca::session-created-at newer-session) 20
            (rplaca::session-updated-at newer-session) 20
            (rplaca::session-created-at other-session) 30
            (rplaca::session-updated-at other-session) 30
            (buffer-working-directory buf) working-directory)
      (save-session older-buffer)
      (save-session newer-buffer)
      (save-session other-buffer)
      (rplaca::continue-session-command buf)
      (let ((loaded (current-buffer)))
        (is (not (eq buf loaded)))
        (is (string= "newer-session"
                     (rplaca::session-name
                      (buffer-session loaded))))))))

(test session-info-command-opens-help-buffer-with-display-name
  "Session info opens a help buffer with display name, routing, and usage."
  (with-interactive-command-test-buffer (buf)
    (let ((*sessions-dir* (temp-session-test-directory "session-info")))
      (setf (buffer-session buf)
            (load-or-create-session "info-session"
                                    :display-name "Info Session"))
      (rplaca::set-buffer-provider-override buf :openai-codex)
      (rplaca::set-buffer-model-override buf "gpt-5.4")
      (rplaca::set-buffer-think-level-override buf "high")
      (let ((agent-message (buffer-insert-agent-message buf "Done"
                                                        :record-p nil
                                                        :run-hook-p nil)))
        (rplaca::put-message-metadata
         agent-message
         :provider :openai-codex
         :model "gpt-5.4"
         :think-level "high"
         :input-tokens 120
         :cached-input-tokens 96
         :uncached-input-tokens 24
         :output-tokens 30
         :total-tokens 150
         :cache-hit-rate 0.8))
      (rplaca::session-info-command buf)
      (let* ((help (current-buffer))
             (text (help-buffer-text help)))
        (is (help-buffer-p help))
        (is (search "Session: Info Session" text))
        (is (search "Session name: info-session" text))
        (is (search "Provider/model: openai-codex/gpt-5.4" text))
        (is (search "Thinking: high" text))
        (is (search "tokens: input=120 cached=96 uncached=24 output=30 total=150 cache-hit=80.0%"
                    text))))))

(test fork-session-from-entry-id-branches-directly-from-message
  "Message entry ids are enough to fork a new session buffer."
  (with-interactive-command-test-buffer (buf)
    (let ((*sessions-dir* (temp-session-test-directory "fork-from-entry")))
      (rplaca::ensure-buffer-session buf)
      (set-message-text (buffer-input-message buf) "Draft feature plan")
      (buffer-finalize-input buf)
      (let* ((message (message-prev (buffer-input-message buf)))
             (entry-id (message-entry-id message)))
        (is (stringp entry-id))
        (is (not (string= "" entry-id)))
        (rplaca::fork-session-from-entry-id buf entry-id)
        (let ((forked (current-buffer)))
          (is (not (eq buf forked)))
          (is (search "branch" (buffer-name forked) :test #'char-equal))
          (is (string= "Draft feature plan"
                       (message-text (buffer-input-message forked))))
          (is (search "[Forked from "
                      (message-text
                       (message-prev (buffer-input-message forked))))))))))

;;; --------------------------------------------------------------------------
;;; Package Selector Tests
;;; --------------------------------------------------------------------------

(test minibuffer-package-selector-activates
  "The package enable command lists installed packages with scope and description."
  (with-interactive-command-test-buffer (buf)
    (with-package-state-override ((default-package-test-channels))
      (rplaca::minibuffer-toggle-package-command buf)
      (is (eq t *minibuffer-active*))
      (is (string= "Enable Package" *minibuffer-prompt*))
      (let ((item (find "sexed" *minibuffer-filtered-items*
                        :key (lambda (entry)
                               (getf entry :package-name))
                        :test #'string=)))
        (is (not (null item)))
        (is (search "[default] sexed - " (getf item :display))))
      (let ((item (find "lispi" *minibuffer-filtered-items*
                        :key (lambda (entry)
                               (getf entry :package-name))
                        :test #'string=)))
        (is (not (null item)))
        (is (search "[default] lispi - " (getf item :display)))))))

(test minibuffer-package-selector-cycles-and-refreshes
  "Confirming a package cycles scope and reopens the package selector."
  (with-interactive-command-test-buffer (buf)
    (with-package-state-override ((default-package-test-channels))
      (rplaca::minibuffer-toggle-package-command buf)
      (let ((index (position "sexed" *minibuffer-filtered-items*
                             :key (lambda (entry)
                                    (getf entry :package-name))
                             :test #'string=)))
        (is (not (null index)))
        (setf *minibuffer-selected-index* index)
        (minibuffer-confirm))
      (is (eq t *minibuffer-active*))
      (is (eq :buffer
              (rplaca:package-enablement-scope "sexed" :buffer buf)))
      (is (equal '("sexed") (buffer-enabled-packages buf)))
      (is (null (find :context
                      (buffer-test-history-messages buf)
                      :key #'message-sender)))
      (let ((item (find "sexed" *minibuffer-filtered-items*
                        :key (lambda (entry)
                               (getf entry :package-name))
                        :test #'string=)))
        (is (search "[buffer] sexed - " (getf item :display)))))))

(test minibuffer-package-selector-appends-context-when-enabling-in-context
  "Enabling a package in an existing conversation appends package prompt context."
  (with-interactive-command-test-buffer (buf)
    (with-package-state-override ((default-package-test-channels))
      (rplaca::set-message-text (buffer-input-message buf)
                                  "existing conversation context")
      (buffer-finalize-input buf)
      (rplaca::minibuffer-toggle-package-command buf)
      (let ((index (position "sexed" *minibuffer-filtered-items*
                             :key (lambda (entry)
                                    (getf entry :package-name))
                             :test #'string=)))
        (is (not (null index)))
        (setf *minibuffer-selected-index* index)
        (minibuffer-confirm))
      (let* ((history (buffer-test-history-messages buf))
             (context (find :context history :key #'message-sender))
             (provider-messages (build-conversation-messages buf)))
        (is (not (null context)))
        (is (search "<package_context package=\"sexed\">"
                    (message-text context)))
        (is (search "Structural editing with sexed"
                    (message-text context)))
        (is (= 2 (length provider-messages)))
        (let* ((last-message (car (last provider-messages)))
               (content (cdr (assoc :content last-message)))
               (text (cdr (assoc :text (aref content 0)))))
          (is (string= "user" (cdr (assoc :role last-message))))
          (is (search "<package_context package=\"sexed\">" text)))))))

(test package-disable-removes-injected-context-from-buffer-and-provider-messages
  "Disabling an enabled package retracts its injected context cleanly."
  (with-interactive-command-test-buffer (buf)
    (with-package-state-override ((default-package-test-channels))
      (rplaca::set-message-text (buffer-input-message buf)
                                  "existing conversation context")
      (buffer-finalize-input buf)
      (rplaca:set-package-enablement-scope "sexed" :global :buffer buf)
      (let ((context (find :context
                           (buffer-test-history-messages buf)
                           :key #'message-sender)))
        (is (not (null context)))
        (is (search "<package_context package=\"sexed\">"
                    (message-text context))))
      (rplaca:set-package-enablement-scope "sexed" :default :buffer buf)
      (is (null (find :context
                      (buffer-test-history-messages buf)
                      :key #'message-sender)))
      (let ((provider-messages (build-conversation-messages buf)))
        (is (= 1 (length provider-messages)))
        (is (string= "user" (cdr (assoc :role (first provider-messages)))))
        (let* ((content (cdr (assoc :content (first provider-messages))))
               (text (cdr (assoc :text (aref content 0)))))
          (is (search "existing conversation context" text)))))))

(test describe-installed-package-command-opens-help-buffer
  "The describe package command loads package metadata and opens a help buffer."
  (with-interactive-command-test-buffer (buf)
    (with-package-state-override ((default-package-test-channels))
      (rplaca::describe-installed-package-command buf)
      (let ((index (position "sexed" *minibuffer-filtered-items*
                             :key (lambda (entry)
                                    (getf entry :package-name))
                             :test #'string=)))
        (is (not (null index)))
        (setf *minibuffer-selected-index* index)
        (minibuffer-confirm))
      (let ((help (current-buffer)))
        (is (string= "*help:package:sexed*" (buffer-name help)))
        (let ((text (message-text (message-prev (buffer-input-message help)))))
          (is (search "Package: sexed" text))
          (is (search "Prompt Sections:" text))
          (is (search "Structural editing with sexed" text)))))))

(test package-dashboard-command-opens-package-dashboard-buffer
  "The package dashboard opens a dedicated special buffer."
  (with-interactive-command-test-buffer (buf)
    (with-package-state-override ((default-package-test-channels))
      (rplaca::package-dashboard-command buf)
      (let ((dashboard (current-buffer)))
        (is (eq :package-dashboard (buffer-kind dashboard)))
        (is (string= "*Packages*" (buffer-name dashboard)))
        (is (eq buf (rplaca::package-dashboard-origin-buffer dashboard)))
        (is (string= "package-dashboard" (buffer-major-mode dashboard)))))))

;;; --------------------------------------------------------------------------
;;; Agent Selector Tests
;;; --------------------------------------------------------------------------

(test minibuffer-agent-selector-activates
  "C-c A opens the minibuffer agent selector with the active agent preselected."
  (with-interactive-command-test-buffer (buf)
    (let ((rplaca::*agent-definition-registry* (make-hash-table :test #'equal))
          (rplaca::*agent-defaults-registry* (rplaca::make-agent-defaults-registry)))
      (setf (buffer-agent-name buf) "writer")
      (register-agent-definition "writer" :provider :zai :model "glm-5")
      (register-agent-definition "pair" :provider :openai-codex :model "gpt-5.4")
      (rplaca::handle-key-event buf '(:ctrl-c #\A))
      (is (eq t *minibuffer-active*))
      (is (string= "Select Agent" *minibuffer-prompt*))
      (is (string= "writer" (getf (first *minibuffer-filtered-items*) :agent-name)))
      (is (getf (first *minibuffer-filtered-items*) :active-p))
      (is (= 0 *minibuffer-selected-index*)))))

(test minibuffer-agent-selector-switches-buffer-and-clears-overrides
  "Selecting an agent updates the buffer, clears overrides, and ensures a face set."
  (with-interactive-command-test-buffer (buf)
    (let ((rplaca::*agent-definition-registry* (make-hash-table :test #'equal))
          (rplaca::*agent-defaults-registry* (rplaca::make-agent-defaults-registry)))
      (register-agent-definition "writer" :provider :zai :model "glm-5")
      (register-agent-definition "pair"
                                 :provider :openai-codex
                                 :model "gpt-5.4"
                                 :think-level "high")
      (setf (buffer-agent-name buf) "writer"
            (buffer-provider-override buf) :zai
            (buffer-model-override buf) "glm-5")
      (set-buffer-think-level-override buf "medium")
      (rplaca::handle-key-event buf '(:ctrl-c #\A))
      (let ((index (position-if (lambda (item)
                                  (string= "pair" (getf item :agent-name)))
                                *minibuffer-filtered-items*)))
        (is (not (null index)))
        (setf *minibuffer-selected-index* index)
        (minibuffer-confirm))
      (is (null *minibuffer-active*))
      (is (string= "pair" (buffer-agent-name buf)))
      (is (null (buffer-provider-override buf)))
      (is (null (buffer-model-override buf)))
      (is (null (buffer-think-level-override buf)))
      (let ((msg (latest-buffer-message buf)))
        (is (not (null msg)))
        (is (eq :system (message-sender msg)))
        (is (search "Agent changed to pair" (message-text msg)))
        (is (search "openai-codex/gpt-5.4" (message-text msg)))
        (is (search "think high" (message-text msg)))))))

(test minibuffer-agent-selector-cancel-leaves-buffer-unchanged
  "Cancelling the agent selector leaves the buffer state untouched."
  (with-interactive-command-test-buffer (buf)
    (let ((rplaca::*agent-definition-registry* (make-hash-table :test #'equal))
          (rplaca::*agent-defaults-registry* (rplaca::make-agent-defaults-registry)))
      (register-agent-definition "writer" :provider :zai :model "glm-5")
      (register-agent-definition "pair" :provider :openai-codex :model "gpt-5.4")
      (setf (buffer-agent-name buf) "writer"
            (buffer-provider-override buf) :zai
            (buffer-model-override buf) "glm-5")
      (set-buffer-think-level-override buf "medium")
      (let ((before-count (buffer-message-count buf)))
        (rplaca::handle-key-event buf '(:ctrl-c #\A))
        (handle-minibuffer-key (code-char 7))
        (is (null *minibuffer-active*))
        (is (string= "writer" (buffer-agent-name buf)))
        (is (eq :zai (buffer-provider-override buf)))
        (is (string= "glm-5" (buffer-model-override buf)))
        (is (string= "medium" (buffer-think-level-override buf)))
        (is (= before-count (buffer-message-count buf)))))))

;;; --------------------------------------------------------------------------
;;; Model Selector Tests
;;; --------------------------------------------------------------------------

(test model-selector-activates
  "Legacy model selector data remains testable without owning key capture."
  (let ((*buffer-ring* nil)
        (rplaca::*buffer-counter* 0)
        (*model-selector-active* nil)
        (*model-selector-index* 0)
        (rplaca::*model-selector-scroll* 0)
        (*model-selector-entries* nil))
    (let ((buf (make-buffer "test-session" :agent-name "coder")))
      (add-buffer-to-ring buf)
      ;; Mock: set entries directly (since we can't call read-provider-token in tests)
      (setf *model-selector-entries*
            (list (list :provider :zai :model "glm-5" :active-p t)
                  (list :provider :zai :model "glm-5-turbo" :active-p nil)
                  (list :provider :openai-codex :model "gpt-5.4" :active-p nil)))
      (setf *model-selector-active* t
            *model-selector-index* 0)
      (is (eq t *model-selector-active*))
      (is (= 3 (length *model-selector-entries*))))))

(test model-selector-navigate-down-and-up
  "C-n and C-p navigate the model selector."
  (let ((*model-selector-active* t)
        (*model-selector-index* 0)
        (rplaca::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :zai :model "model-a" :active-p t)
                (list :provider :zai :model "model-b" :active-p nil)
                (list :provider :openai-codex :model "model-c" :active-p nil))))
    (let ((buf (make-buffer "test")))
      ;; C-n = move down
      (rplaca::handle-model-selector-key (code-char 14) buf)
      (is (= 1 *model-selector-index*))
      ;; C-n again
      (rplaca::handle-model-selector-key (code-char 14) buf)
      (is (= 2 *model-selector-index*))
      ;; C-n at bottom = no change
      (rplaca::handle-model-selector-key (code-char 14) buf)
      (is (= 2 *model-selector-index*))
      ;; C-p = move up
      (rplaca::handle-model-selector-key (code-char 16) buf)
      (is (= 1 *model-selector-index*)))))

(test model-selector-enter-selects-model
  "Enter in model selector sets buffer overrides and closes."
  (let ((*model-selector-active* t)
        (*model-selector-index* 1)
        (rplaca::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :openai-codex :model "gpt-5.4" :active-p t)
                (list :provider :zai :model "glm-5" :active-p nil)))
        (*buffer-ring* nil)
        (rplaca::*buffer-counter* 0))
    (let ((buf (make-buffer "test")))
      (add-buffer-to-ring buf)
      ;; Select index 1 (zai/glm-5)
      (rplaca::handle-model-selector-key #\Return buf)
      (is (null *model-selector-active*))
      (is (eq :zai (buffer-provider-override buf)))
      (is (string= "glm-5" (buffer-model-override buf)))
      (is (null (buffer-think-level-override buf))))))

(test model-selector-enter-keeps-supported-think-level
  "Switching models keeps the current think level when the new model supports it."
  (let ((*model-selector-active* t)
        (*model-selector-index* 1)
        (rplaca::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :openai-codex :model "gpt-5.4" :active-p t)
                (list :provider :openai-codex :model "gpt-5.2" :active-p nil))))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "high")
      (rplaca::handle-model-selector-key #\Return buf)
      (is (string= "gpt-5.2" (buffer-model-override buf)))
      (is (string= "high" (buffer-think-level-override buf))))))

(test model-selector-enter-resets-unsupported-think-level
  "Switching models clears the current think level when unsupported by the new model."
  (let ((*model-selector-active* t)
        (*model-selector-index* 1)
        (rplaca::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :openai-codex :model "gpt-5.4" :active-p t)
                (list :provider :openai-codex :model "gpt-5.1-codex-max" :active-p nil))))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "xhigh")
      (rplaca::handle-model-selector-key #\Return buf)
      (is (string= "gpt-5.1-codex-max" (buffer-model-override buf)))
      (is (null (buffer-think-level-override buf))))))

(test model-selector-cancel-with-c-g
  "C-g in model selector closes without changing the model."
  (let ((*model-selector-active* t)
        (*model-selector-index* 1)
        (rplaca::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :openai-codex :model "gpt-5.4" :active-p t)
                (list :provider :zai :model "glm-5" :active-p nil))))
    (let ((buf (make-buffer "test")))
      ;; C-g = cancel
      (rplaca::handle-model-selector-key (code-char 7) buf)
      (is (null *model-selector-active*))
      ;; No overrides set
      (is (null (buffer-provider-override buf)))
      (is (null (buffer-model-override buf))))))

(test model-selector-active-model-pre-selected
  "When opening the model selector, the active model index is pre-selected."
  (let ((*model-selector-entries*
          (list (list :provider :openai-codex :model "gpt-5.4" :active-p nil)
                (list :provider :openai-codex :model "gpt-5.2" :active-p nil)
                (list :provider :zai :model "glm-5" :active-p t))))
    ;; Simulate what select-model-command does
    (let ((active-idx (position-if (lambda (e) (getf e :active-p))
                                   *model-selector-entries*)))
      (is (= 2 active-idx)))))

;;; --------------------------------------------------------------------------
;;; Think Selector Tests
;;; --------------------------------------------------------------------------

(test think-selector-activates
  "The legacy think command delegates to the visible minibuffer selector."
  (let ((rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state))
        (*think-selector-active* nil)
        (*think-selector-index* 99)
        (rplaca::*think-selector-scroll* 99)
        (*think-selector-entries* nil))
    (let ((buf (make-buffer "test-session" :agent-name "spark")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (select-think-level-command buf)
      (is-false *think-selector-active*)
      (is-true *minibuffer-active*)
      (is (= 0 *minibuffer-selected-index*))
      (is (= 6 (length *minibuffer-filtered-items*)))
      (is (string= "default"
                   (getf (first *minibuffer-filtered-items*) :display))))))

(test think-selector-active-level-pre-selected
  "The visible think selector preselects the active level."
  (let ((rplaca::*chat-interaction-state*
          (rplaca::make-chat-interaction-state))
        (*think-selector-active* nil)
        (*think-selector-index* 0)
        (rplaca::*think-selector-scroll* 0)
        (*think-selector-entries* nil))
    (let ((buf (make-buffer "test-session" :agent-name "spark")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "high")
      (select-think-level-command buf)
      (is (= 4 *minibuffer-selected-index*)))))

(test think-selector-enter-selects-level
  "Enter in think selector sets the buffer think level and closes."
  (let ((*think-selector-active* t)
        (*think-selector-index* 3)
        (rplaca::*think-selector-scroll* 0))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.3-codex")
      (setf *think-selector-entries* (rplaca::available-think-levels-for-selector buf))
      ;; Index 3 = high for gpt-5.3-codex: default, low, medium, high, xhigh
      (rplaca::handle-think-selector-key #\Return buf)
      (is (null *think-selector-active*))
      (is (string= "high" (buffer-think-level-override buf))))))

(test think-selector-default-clears-level
  "Selecting the default think entry clears the buffer think override."
  (let ((*think-selector-active* t)
        (*think-selector-index* 0)
        (rplaca::*think-selector-scroll* 0))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "high")
      (setf *think-selector-entries* (rplaca::available-think-levels-for-selector buf))
      (rplaca::handle-think-selector-key #\Return buf)
      (is (null *think-selector-active*))
      (is (null (buffer-think-level-override buf))))))

(test think-selector-cancel-with-c-g
  "C-g in think selector closes without changing the think level."
  (let ((*think-selector-active* t)
        (*think-selector-index* 1)
        (rplaca::*think-selector-scroll* 0))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "medium")
      (setf *think-selector-entries* (rplaca::available-think-levels-for-selector buf))
      (rplaca::handle-think-selector-key (code-char 7) buf)
      (is (null *think-selector-active*))
      (is (string= "medium" (buffer-think-level-override buf))))))

(test export-buffer-session-html-renders-images-reasoning-and-metadata
  "HTML export writes the current branch with optional reasoning, metadata, and images."
  (let* ((*sessions-dir* (temp-session-test-directory "export-html"))
         (image-path (merge-pathnames "diagram.png" *sessions-dir*))
         (export-path (merge-pathnames "branch.html" *sessions-dir*))
         (session (load-or-create-session "export-html"
                                          :display-name "Export HTML"))
         (buf (make-buffer "export-html"
                           :agent-name "echo"
                           :session session)))
    (ensure-directories-exist image-path)
    (with-open-file (stream image-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-byte 0 stream))
    (set-message-text (buffer-input-message buf)
                      (format nil "Need this~%![Diagram](~A)"
                              (namestring image-path)))
    (buffer-finalize-input buf)
    (buffer-insert-agent-message
     buf
     "Done."
     :raw-content '(((:type . "text") (:text . "Done."))
                    ((:type . "reasoning") (:text . "Private reasoning.")))
     :metadata '((:provider . :openai-codex)
                 (:model . "gpt-5.4")
                 (:think-level . "high")))
    (let* ((result (rplaca::export-buffer-session-html
                    buf
                    :path export-path
                    :show-reasoning-p t
                    :show-metadata-p t))
           (html (uiop:read-file-string export-path)))
      (is (equal (namestring export-path)
                 (namestring (getf result :path))))
      (is (search "Export HTML" html))
      (is (search "Need this" html))
      (is (search "Private reasoning." html))
      (is (search "provider/model: openai-codex/gpt-5.4" html))
      (is (search "think: high" html))
      (is (search "<img" html))
      (is (search (namestring image-path) html)))))

(test message-metadata-help-string-includes-core-message-fields
  "The McCLIM metadata help text reports stable message identity and metadata."
  (let ((msg (make-message :agent :read-only-p t)))
    (set-message-text msg "Hello")
    (setf (message-timestamp msg) 42
          (message-entry-id msg) "entry-1"
          (message-parent-entry-id msg) "parent-1"
          (message-raw-content msg) '(((:type . "text") (:text . "Hello")))
          (message-metadata msg) '((:provider . :test)
                                   (:model . "model-1")))
    (let ((help (rplaca::message-metadata-help-string msg)))
      (is (search "Sender: AGENT" help))
      (is (search "Entry id: entry-1" help))
      (is (search "Parent entry id: parent-1" help))
      (is (search "Raw content blocks: 1" help))
      (is (search ":PROVIDER" help)))))

(test export-buffer-session-html-hides-reasoning-and-metadata-when-disabled
  "HTML export omits sidecar reasoning and metadata when requested."
  (let* ((*sessions-dir* (temp-session-test-directory "export-html-hidden"))
         (export-path (merge-pathnames "branch.html" *sessions-dir*))
         (session (load-or-create-session "export-html-hidden"))
         (buf (make-buffer "export-html-hidden"
                           :agent-name "echo"
                           :session session)))
    (set-message-text (buffer-input-message buf) "Question")
    (buffer-finalize-input buf)
    (buffer-insert-agent-message
     buf
     "Answer."
     :raw-content '(((:type . "text") (:text . "Answer."))
                    ((:type . "reasoning") (:text . "Hidden reasoning.")))
     :metadata '((:provider . :openai-codex)
                 (:model . "gpt-5.4")
                 (:think-level . "high")))
    (let ((html (progn
                  (rplaca::export-buffer-session-html
                   buf
                   :path export-path
                   :show-reasoning-p nil
                   :show-metadata-p nil)
                  (uiop:read-file-string export-path))))
      (is (search "Question" html))
      (is (search "Answer." html))
      (is-false (search "Hidden reasoning." html))
      (is-false (search "provider/model" html))
      (is-false (search "think: high" html)))))

(test session-export-share-handlers-support-registered-hook-and-local-copy-flows
  "Session export can dispatch through registered handlers, hook handlers, and the built-in local copy flow."
  (let* ((*sessions-dir* (temp-session-test-directory "export-share"))
         (export-path (merge-pathnames "branch.html" *sessions-dir*))
         (session (load-or-create-session "export-share"))
         (buf (make-buffer "export-share"
                           :agent-name "echo"
                           :session session))
         (rplaca::*session-share-handler-table*
           (make-hash-table :test #'equal))
         (rplaca::*session-share-hook* nil)
         (registered-call nil)
         (hook-call nil))
    (set-message-text (buffer-input-message buf) "Share me")
    (buffer-finalize-input buf)
    (let ((export-info (rplaca::export-buffer-session-html
                        buf
                        :path export-path)))
      (rplaca::register-session-share-handler
       "capture"
       (lambda (buffer info)
         (setf registered-call (list buffer info))
         "share://capture"))
      (let ((captured (rplaca::share-session-export
                       buf export-info :handler "capture")))
        (is (equal "capture" (getf captured :handler)))
        (is (equal "share://capture" (getf captured :result)))
        (is (eq buf (first registered-call))))
      (setf rplaca::*session-share-hook*
            (list (lambda (buffer info)
                    (setf hook-call (list buffer info))
                    "share://hook")))
      (let ((hooked (rplaca::share-session-export
                     buf export-info :handler "hook")))
        (is (equal "hook" (getf hooked :handler)))
        (is (equal "share://hook" (getf hooked :result)))
        (is (eq buf (first hook-call))))
      (let* ((shared (rplaca::share-session-export
                      buf export-info :handler "local-copy"))
             (shared-path (getf shared :result)))
        (is (equal "local-copy" (getf shared :handler)))
        (is (pathnamep shared-path))
        (is (probe-file shared-path))
        (is (search "/shares/" (namestring shared-path)))))))

(test ephemeral-chat-buffers-do-not-attach-or-autosave-sessions
  "Ephemeral chat buffers stay off disk until explicitly persisted elsewhere."
  (let* ((*sessions-dir* (temp-session-test-directory "ephemeral-chat"))
         (session-name "ephemeral-chat")
         (buf (make-chat-buffer session-name
                                :session-persistence-mode :ephemeral)))
    (is (rplaca::buffer-ephemeral-p buf))
    (is (null (buffer-session buf)))
    (set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (buffer-insert-agent-message buf "world")
    (is (null (rplaca::ensure-buffer-session buf)))
    (is-false (probe-file (rplaca::session-path session-name)))
    (is-false (probe-file
               (rplaca::session-sidecar-manifest-path session-name)))))
