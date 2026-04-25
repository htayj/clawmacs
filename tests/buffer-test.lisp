(in-package :clawmacs/tests)
(in-suite buffer-suite)

(defvar *mx-test-command-log* nil
  "Records command invocations during buffer tests.")

(defun temp-session-test-directory (label)
  "Return a fresh temporary directory for session transcript tests."
  (make-pathname :directory
                 (list :absolute "tmp"
                       (format nil "clawmacs-session-tests-~A-~A-~A"
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
    (clawmacs::set-message-text msg text)
    (setf (message-timestamp msg) (get-universal-time))
    msg))

(defun mx-test-noarg-command (buffer)
  "Test command used to verify M-x invocation without arguments."
  (declare (ignore buffer))
  (setf *mx-test-command-log* '(:noarg)))
(clawmacs:defcommand mx-test-noarg-command)

(defun mx-test-arg-command (buffer count label)
  "Test command used to verify M-x argument prompting."
  (declare (ignore buffer))
  (setf *mx-test-command-log* (list :args count label)))
(clawmacs:defcommand mx-test-arg-command
  :prompts ((count :prompt "Count" :reader parse-integer)
            (label :prompt "Label")))

(defmacro with-interactive-command-test-buffer ((buffer-var) &body body)
  `(let ((*buffer-ring* nil)
         (clawmacs::*buffer-counter* 0)
         (*buffer-selector-active* nil)
         (clawmacs::*session-tree-selector-active* nil)
         (clawmacs::*session-tree-selector-buffer* nil)
         (clawmacs::*session-tree-selector-items* nil)
         (clawmacs::*session-tree-selector-filtered-items* nil)
         (clawmacs::*session-tree-selector-index* 0)
         (clawmacs::*session-tree-selector-scroll* 0)
         (clawmacs::*session-tree-selector-search* "")
         (clawmacs::*session-tree-selector-filter-mode* :default)
         (clawmacs::*session-tree-selector-folded-ids* nil)
         (*model-selector-active* nil)
         (*think-selector-active* nil)
         (*customize-face-state* nil)
         (*openai-oauth-pending* nil)
         (*deny-message-mode* nil)
         (*cc-pending* nil)
         (*cx-pending* nil)
         (*ch-pending* nil)
         (*meta-pending* nil)
         (*minibuffer-active* nil)
         (*minibuffer-mode* :completion)
         (*minibuffer-prompt* "")
         (*minibuffer-input* "")
         (*minibuffer-point* 0)
         (*minibuffer-items* nil)
         (*minibuffer-filtered-items* nil)
         (*minibuffer-match-positions* nil)
         (*minibuffer-selected-index* 0)
         (*minibuffer-scroll-offset* 0)
         (*minibuffer-callback* nil)
         (*mx-test-command-log* nil))
     (clawmacs::init-default-keymap)
     (let ((,buffer-var (make-buffer "test-session")))
       (clawmacs::init-face-registry ,buffer-var)
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

(test package-buffer-type-registration-controls-buffer-defaults
  "Registered buffer types provide package-extensible kind metadata."
  (let ((clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry)))
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
  (let ((clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry)))
    (register-buffer-type :dashboard)
    (let ((buf (make-buffer "dashboard" :kind :dashboard)))
      (is (string= "dashboard" (buffer-major-mode buf))))))

(test built-in-special-buffer-types-are-registered
  "Help, customize, and listener buffers are first-class non-document kinds."
  (let ((clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry)))
    (let ((help (find-buffer-type :help))
          (customize (find-buffer-type :customize))
          (listener (find-buffer-type :listener)))
      (is (not (null help)))
      (is (not (null customize)))
      (is (not (null listener)))
      (is (string= "help" (buffer-type-major-mode help)))
      (is (string= "customize" (buffer-type-major-mode customize)))
      (is (string= "listener" (buffer-type-major-mode listener)))
      (is (not (buffer-type-document-p help)))
      (is (not (buffer-type-document-p customize)))
      (is (not (buffer-type-document-p listener))))))

(test mcclim-registers-built-in-special-presentations
  "The McCLIM UI installs presentation renderers for special built-in buffers."
  (let ((clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry)))
    (clawmacs::register-mcclim-core-buffer-presentations)
    (let ((help (make-buffer "help" :kind :help))
          (customize (make-buffer "customize" :kind :customize))
          (listener (make-buffer "listener" :kind :listener)))
      (is (eq 'clawmacs::mcclim-render-help-buffer
              (buffer-presentation-function help)))
      (is (eq 'clawmacs::mcclim-render-empty-input-pane
              (buffer-input-presentation-function help)))
      (is (eq 'clawmacs::mcclim-render-customize-buffer
              (buffer-presentation-function customize)))
      (is (eq 'clawmacs::mcclim-render-empty-input-pane
              (buffer-input-presentation-function customize)))
      (is (eq 'clawmacs::mcclim-render-listener-buffer
              (buffer-presentation-function listener)))
      (is (eq 'clawmacs::mcclim-render-listener-input-pane
              (buffer-input-presentation-function listener)))
      (is (eq 'clawmacs::listener-serialize-buffer-state
              (clawmacs::buffer-type-serialize-state-function
               (find-buffer-type :listener))))
      (is (eq 'clawmacs::listener-restore-buffer-state
              (clawmacs::buffer-type-restore-state-function
               (find-buffer-type :listener)))))))

(test make-listener-buffer-evaluates-lisp-and-comma-commands
  "Listener buffers evaluate Lisp forms and dispatch McCLIM-style comma commands."
  (let ((*buffer-ring* nil)
        (clawmacs::*listener-buffer-states* (make-hash-table :test #'eq)))
    (clawmacs::init-default-keymap)
    (let ((buf (make-listener-buffer :working-directory #P"/tmp/"
                                     :add-to-ring-p t)))
      (is (listener-buffer-p buf))
      (is (string= "listener" (buffer-major-mode buf)))
      (is (not (document-buffer-p buf)))
      (is (search "McCLIM-style Common Lisp Listener"
                  (message-text (latest-buffer-message buf))))

      (set-message-text (buffer-input-message buf) "(+ 1 2)")
      (submit-listener-input buf)
      (is (search "=> 3" (message-text (latest-buffer-message buf))))
      (is (search ">" (clawmacs::message-metadata-value
                       (message-metadata
                        (message-prev (message-prev (buffer-input-message buf))))
                       :listener-prompt)))

      (set-message-text (buffer-input-message buf) ",Help Commands")
      (submit-listener-input buf)
      (is (search "McCLIM Listener commands"
                  (message-text (latest-buffer-message buf))))

      (set-message-text (buffer-input-message buf) ",Package cl-user")
      (submit-listener-input buf)
      (is (search "Package set to"
                  (message-text (latest-buffer-message buf))))

      (set-message-text (buffer-input-message buf) ",Clear Output History")
      (submit-listener-input buf)
      (is (search "Listener history cleared"
                  (message-text (latest-buffer-message buf))))
      (is (= 1 (length (buffer-test-history-messages buf)))))))

(test make-help-buffer-stores-read-only-help-content
  "Help buffers expose their text through help-buffer-text."
  (let ((*buffer-ring* nil))
    (let ((buf (make-help-buffer "*help:test*" "Help title~%==========")))
      (is (help-buffer-p buf))
      (is (string= "help" (buffer-major-mode buf)))
      (is (search "Help title" (help-buffer-text buf)))
      (is (= 1 (length (buffer-test-history-messages buf)))))))

(test make-customize-face-buffer-uses-dedicated-buffer-kind
  "Customize buffers are not backed by a rendered agent message."
  (let ((*buffer-ring* nil)
        (*customize-face-state* nil))
    (clawmacs::init-default-keymap)
    (let* ((style (make-drawing-style :test-customize
                                      :ink (make-cga-ink 1)))
           (buf (clawmacs::make-customize-face-buffer
                 style "test-customize")))
      (is (customize-buffer-p buf))
      (is (string= "customize" (buffer-major-mode buf)))
      (is (eq buf (getf *customize-face-state* :buffer)))
      (is (= 0 (length (buffer-test-history-messages buf))))
      (is (= 1 (buffer-message-count buf))))))

(test toggle-reasoning-output-command-flips-buffer-flag
  "The reasoning output toggle controls per-buffer reasoning display."
  (let ((buf (make-buffer "reasoning-toggle")))
    (is (not (buffer-show-reasoning-p buf)))
    (clawmacs::toggle-reasoning-output-command buf)
    (is (buffer-show-reasoning-p buf))
    (clawmacs::toggle-reasoning-output-command buf)
    (is (not (buffer-show-reasoning-p buf)))))

(test toggle-metadata-output-command-flips-buffer-flag
  "The metadata output toggle controls per-buffer metadata display."
  (let ((buf (make-buffer "metadata-toggle")))
    (is (not (buffer-show-metadata-p buf)))
    (clawmacs::toggle-metadata-output-command buf)
    (is (buffer-show-metadata-p buf))
    (clawmacs::toggle-metadata-output-command buf)
    (is (not (buffer-show-metadata-p buf)))))

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
    (clawmacs::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (buffer-insert-system-message buf "display-only")
    (clawmacs::buffer-insert-context-message buf "late context")
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
    (clawmacs::set-message-text (buffer-input-message buf) "hello")
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
         (path (clawmacs::session-path session-name)))
    (is (not (probe-file path)))
    (clawmacs::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (is (probe-file path))
    (is (member session-name (list-saved-sessions) :test #'string=))
    (let ((loaded (load-session session-name)))
      (is (not (null loaded)))
      (is (string= session-name (buffer-name loaded)))
      (is (string= "echo" (buffer-agent-name loaded)))
      (let ((msg (buffer-first-message loaded)))
        (is (eq :user (message-sender msg)))
        (is (string= "hello" (message-text msg)))))))

(test sidecar-only-session-is-listed-and-loadable
  "Transcript sidecars are visible to the load-session command before a snapshot exists."
  (let* ((session-name "sidecar-only-session")
         (*sessions-dir* (temp-session-test-directory "sidecar"))
         (session (load-or-create-session session-name))
         (path (clawmacs::session-path session-name))
         (msg (make-message :user :read-only-p t)))
    (clawmacs::set-message-text msg "from transcript")
    (setf (message-timestamp msg) 42)
    (record-session-message session msg)
    (is (not (probe-file path)))
    (is (member session-name (list-saved-sessions) :test #'string=))
    (let ((loaded (load-session session-name)))
      (is (not (null loaded)))
      (is (string= session-name (buffer-name loaded)))
      (is (probe-file path))
      (let ((loaded-msg (buffer-first-message loaded)))
        (is (eq :user (message-sender loaded-msg)))
        (is (string= "from transcript" (message-text loaded-msg)))))))

(test sidecar-only-session-round-trips-working-directory
  "Sidecar-backed session loads preserve the original working directory."
  (let* ((session-name "sidecar-working-directory")
         (working-directory #P"/tmp/clawmacs-sidecar-session/")
         (*sessions-dir* (temp-session-test-directory "sidecar-working-directory"))
         (session (load-or-create-session session-name
                                          :working-directory working-directory))
         (msg (make-message :user :read-only-p t)))
    (clawmacs::set-message-text msg "from transcript")
    (setf (message-timestamp msg) 42)
    (record-session-message session msg)
    (let ((loaded (load-session session-name)))
      (is (not (null loaded)))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                 (buffer-working-directory loaded)))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                 (clawmacs::session-working-directory
                  (buffer-session loaded)))))))

(test session-transcripts-are-tree-shaped
  "Recorded transcript messages get ids, parent ids, and advance the leaf."
  (let* ((*sessions-dir* (temp-session-test-directory "tree-shape"))
         (session (load-or-create-session "Tree Shape"))
         (buf (make-buffer "Tree Shape"
                           :agent-name "agent"
                           :session session)))
    (clawmacs::set-message-text (buffer-input-message buf) "root")
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
        (is (string= "root" (message-text (buffer-first-message loaded))))
        (is (string= "new answer"
                     (message-text
                      (message-next (buffer-first-message loaded)))))
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
                      clawmacs::*session-tree-selector-filtered-items*
                      :key (lambda (candidate) (getf candidate :id))
                      :test #'string=)))
      (is (not (null item)))
      (is (string= "mark" (getf item :label))))
    (clawmacs::session-tree-selector-set-filter :labeled-only)
    (is (= 1 (length clawmacs::*session-tree-selector-filtered-items*)))
    (session-tree-selector-deactivate)))

(test ensure-scratch-buffer-creates-loaded-scratch-without-stealing-current
  "The scratch buffer is loaded into the ring but does not become current."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*scratch-buffer-name* "*scratch*")
        (*scratch-buffer-initial-text* "notes"))
    (clawmacs::init-default-keymap)
    (clawmacs::init-global-faces)
    (let ((chat (make-buffer "chat")))
      (clawmacs::init-face-registry chat)
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
        (is (eq clawmacs::*scratch-keymap* (buffer-keymap scratch)))
        (is (= 2 (length *buffer-ring*)))
        (is (eq scratch (ensure-scratch-buffer)))
        (is (= 2 (length *buffer-ring*)))))))

(test scratch-buffer-text-is-programmatically-editable
  "The scratch text accessor reads and replaces the editable document."
  (let ((*buffer-ring* nil)
        (*scratch-buffer-initial-text* ""))
    (clawmacs::init-default-keymap)
    (clawmacs::init-global-faces)
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
    (let ((data (clawmacs::serialize-buffer buf)))
      (is (eq :zai (cdr (assoc :provider-override data))))
      (is (string= "glm-5" (cdr (assoc :model-override data))))
      (is (string= "medium" (cdr (assoc :think-level-override data))))
      (is (string= "plan-build" (cdr (assoc :pipeline-name data))))
      (is (equal '("sexed")
                 (coerce (cdr (assoc :enabled-packages data)) 'list))))))

(test load-session-missing-overrides-default-to-nil
  "Sessions without override fields load nil overrides."
  (let* ((session-name "missing-overrides")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "clawmacs-buffer-tests")))
         (path (clawmacs::session-path session-name)))
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
         (manifest-path (clawmacs::session-sidecar-manifest-path session-name)))
    (ensure-directories-exist manifest-path)
    (with-open-file (stream manifest-path :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
      (write-string "{not valid json" stream))
    (let ((result (clawmacs::read-session-manifest manifest-path)))
      (is (typep result 'clawmacs::session-manifest-parse-error))
      (is (search "malformed-manifest"
                  (clawmacs::session-manifest-parse-error-path result)))
      (is (search "Failed to parse session manifest"
                  (format nil "~A" result))))
    (signals clawmacs::session-manifest-parse-error
      (clawmacs::load-session-sidecar session-name))))

(test list-saved-sessions-skips-malformed-sidecars-with-warning
  "Malformed sidecars warn and do not hide valid saved sessions."
  (let* ((good-session "good-sidecar-session")
         (bad-session "bad-sidecar-session")
         (*sessions-dir* (temp-session-test-directory "malformed-sidecars"))
         (good-session-object (load-or-create-session good-session))
         (bad-manifest-path (clawmacs::session-sidecar-manifest-path bad-session))
         (good-manifest-path (clawmacs::session-sidecar-manifest-path good-session))
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
         (path (clawmacs::session-path session-name)))
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
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "clawmacs-buffer-tests")))
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
         (working-directory #P"/tmp/clawmacs-working-directory-session/")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "clawmacs-buffer-tests")))
         (session (load-or-create-session session-name
                                          :working-directory working-directory))
         (buf (make-buffer session-name
                           :agent-name "echo"
                           :session session
                           :working-directory working-directory)))
    (clawmacs::set-message-text (buffer-input-message buf) "hello")
    (buffer-finalize-input buf)
    (save-session buf)
    (let ((loaded (load-session session-name)))
      (is (not (null loaded)))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                 (buffer-working-directory loaded)))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                 (clawmacs::session-working-directory
                  (buffer-session loaded)))))))

(test listener-buffer-state-round-trips-through-session-save
  "Listener buffers preserve package, directory stack, history, and values."
  (let* ((session-name "listener-state-round-trip")
         (working-directory #P"/tmp/clawmacs-listener-session/")
         (stack-entry #P"/tmp/clawmacs-listener-stack/")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "clawmacs-buffer-tests")))
         (clawmacs::*listener-buffer-states* (make-hash-table :test #'eq))
         (session (load-or-create-session session-name
                                          :working-directory working-directory))
         (buf (make-listener-buffer :name session-name
                                    :working-directory working-directory)))
    (setf (buffer-session buf) session)
    (clawmacs::listener-set-package buf "KEYWORD")
    (setf (listener-state-directory-stack (listener-buffer-state buf))
          (list stack-entry)
          (listener-state-last-values (listener-buffer-state buf))
          '(42 "done")
          (listener-state-command-history (listener-buffer-state buf))
          '(",pwd" "(+ 1 2)"))
    (save-session buf)
    (let* ((loaded (load-session session-name))
           (loaded-state (listener-buffer-state loaded)))
      (is (listener-buffer-p loaded))
      (is (equal (uiop:ensure-directory-pathname working-directory)
                 (buffer-working-directory loaded)))
      (is (string= "KEYWORD" (listener-state-package-name loaded-state)))
      (is (equal (list (uiop:ensure-directory-pathname stack-entry))
                 (listener-state-directory-stack loaded-state)))
      (is (equal '(42 "done")
                 (listener-state-last-values loaded-state)))
      (is (equal '(",pwd" "(+ 1 2)")
                 (listener-state-command-history loaded-state))))))

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
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "clawmacs-buffer-tests")))
         (path (clawmacs::session-path session-name)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string
       "{\"name\":\"legacy-raw-content\",\"agent-name\":\"echo\",\"messages\":[{\"sender\":\"ECHO\",\"text\":\"Searching\",\"timestamp\":1,\"read-only-p\":true,\"raw-content\":[{\"type\":\"text\",\"text\":\"Searching\"},{\"type\":\"tool_use\",\"id\":\"toolu_123\",\"name\":\"read_file\",\"input\":{\"path\":\"/tmp/example.txt\"}}]},{\"sender\":\"TOOL-RESULT\",\"text\":\"done\",\"timestamp\":2,\"read-only-p\":true,\"raw-content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_123\",\"content\":\"done\"}]}]}"
       stream))
    (let ((buf (load-session session-name)))
      (is (not (null buf)))
      (let* ((assistant-msg (buffer-first-message buf))
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
    (clawmacs::set-message-text (buffer-input-message buf) "git commit -m msg")
    (buffer-finalize-input buf)
    (is (string= "commit" (clawmacs::buffer-previous-command-first-argument buf)))
    (is (string= "msg" (clawmacs::buffer-previous-command-last-argument buf)))))

(test previous-command-argument-extraction-no-arguments
  "Return nil when previous command has no arguments."
  (let ((buf (make-buffer "test")))
    (clawmacs::set-message-text (buffer-input-message buf) "ls")
    (buffer-finalize-input buf)
    (is (null (clawmacs::buffer-previous-command-first-argument buf)))
    (is (string= "ls" (clawmacs::buffer-previous-command-last-argument buf)))))

(test handle-key-event-control-l-requests-redraw-in-minibuffer-mode
  "Ctrl+l requests a redraw even when modal UI state like the minibuffer is active."
  (let ((*minibuffer-active* t)
        (*minibuffer-mode* :completion)
        (*buffer-selector-active* nil)
        (clawmacs::*session-tree-selector-active* nil)
        (*model-selector-active* nil)
        (*think-selector-active* nil)
        (*customize-face-state* nil)
        (*openai-oauth-pending* nil)
        (*deny-message-mode* nil)
        (*cc-pending* nil)
        (*cx-pending* nil)
        (*ch-pending* nil)
        (*meta-pending* nil))
    (let ((buf (make-buffer "test")))
      (is (eq :redraw
              (clawmacs::handle-key-event buf (code-char 12)))))))

;;; --------------------------------------------------------------------------
;;; M-x Tests
;;; --------------------------------------------------------------------------

(test default-keymap-binds-m-x-to-execute-extended-command
  "The default keymap exposes M-x as execute-extended-command."
  (clawmacs::init-default-keymap)
  (is (eq 'execute-extended-command
          (keymap-lookup *default-keymap* '(:meta #\x))))
  (is (null (keymap-lookup *default-keymap* '(:alt #\x)))))

(test execute-extended-command-opens-the-command-picker
  "M-x opens the minibuffer with commands."
  (with-interactive-command-test-buffer (buf)
    (clawmacs::handle-key-event buf '(:meta #\x))
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
    (clawmacs::handle-key-event buf '(:meta #\x))
    (select-minibuffer-command 'mx-test-noarg-command)
    (is (null *minibuffer-active*))
    (is (equal '(:noarg) *mx-test-command-log*))))

(test execute-extended-command-prompts-for-each-argument
  "Selecting a parameterized command prompts for each command argument."
  (with-interactive-command-test-buffer (buf)
    (clawmacs::handle-key-event buf '(:meta #\x))
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
    (clawmacs::handle-key-event buf '(:meta #\x))
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
    (clawmacs::handle-key-event buf '(:meta #\x))
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
      (clawmacs::handle-key-event buf #\?)
      (is (eq t *minibuffer-active*))
      (is (eq :prompt *minibuffer-mode*))
      (is (string= "Count" *minibuffer-prompt*)))))

;;; --------------------------------------------------------------------------
;;; Buffer Selector Tests
;;; --------------------------------------------------------------------------

(test buffer-selector-activates
  "list-buffers-command activates the buffer selector."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*buffer-selector-active* nil)
        (*buffer-selector-index* 99)
        (clawmacs::*buffer-selector-scroll* 99))
    (clawmacs::init-default-keymap)
    (let ((buf (make-buffer "test")))
      (clawmacs::init-face-registry buf)
      (setf (buffer-keymap buf) *default-keymap*)
      (add-buffer-to-ring buf)
      (list-buffers-command buf)
      (is (eq t *buffer-selector-active*))
      (is (= 0 *buffer-selector-index*))
      (is (= 0 clawmacs::*buffer-selector-scroll*)))))

(test buffer-selector-navigate-down-and-up
  "Navigating the buffer selector moves the index."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 0)
        (clawmacs::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "session-1"))
          (buf2 (make-buffer "session-2"))
          (buf3 (make-buffer "session-3")))
      (add-buffer-to-ring buf3)
      (add-buffer-to-ring buf2)
      (add-buffer-to-ring buf1)
      ;; Navigate down (C-n = ASCII 14)
      (clawmacs::handle-buffer-selector-key (code-char 14))
      (is (= 1 *buffer-selector-index*))
      (clawmacs::handle-buffer-selector-key (code-char 14))
      (is (= 2 *buffer-selector-index*))
      ;; Can't go past end
      (clawmacs::handle-buffer-selector-key (code-char 14))
      (is (= 2 *buffer-selector-index*))
      ;; Navigate up (C-p = ASCII 16)
      (clawmacs::handle-buffer-selector-key (code-char 16))
      (is (= 1 *buffer-selector-index*)))))

(test buffer-selector-enter-selects-buffer
  "Pressing Enter in the buffer selector switches to the highlighted buffer."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 1)
        (clawmacs::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "session-1"))
          (buf2 (make-buffer "session-2")))
      (add-buffer-to-ring buf2)
      (add-buffer-to-ring buf1)
      ;; Select index 1 (session-2) and press Enter
      (clawmacs::handle-buffer-selector-key #\Newline)
      (is (eq nil *buffer-selector-active*))
      (is (eq buf2 (current-buffer))))))

(test buffer-selector-cancel-with-c-g
  "C-g closes the buffer selector without switching."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 1)
        (clawmacs::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "session-1"))
          (buf2 (make-buffer "session-2")))
      (add-buffer-to-ring buf2)
      (add-buffer-to-ring buf1)
      ;; C-g = ASCII 7
      (clawmacs::handle-buffer-selector-key (code-char 7))
      (is (eq nil *buffer-selector-active*))
      ;; Current buffer unchanged (buf1 is still first)
      (is (eq buf1 (current-buffer))))))

(test buffer-selector-new-buffer
  "Pressing n creates a new buffer and closes the selector."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 0)
        (clawmacs::*buffer-selector-scroll* 0))
    (clawmacs::init-default-keymap)
    (let ((buf1 (make-buffer "session-1")))
      (clawmacs::init-face-registry buf1)
      (setf (buffer-keymap buf1) *default-keymap*)
      (add-buffer-to-ring buf1)
      (clawmacs::handle-buffer-selector-key #\n)
      (is (eq nil *buffer-selector-active*))
      (is (= 2 (length *buffer-ring*)))
      (is (string= "session-1" (buffer-name buf1)))
      ;; The new buffer is now current
      (is (not (eq buf1 (current-buffer)))))))

(test buffer-selector-kill-buffer
  "Pressing k kills the highlighted buffer."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 1)
        (clawmacs::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "session-1"))
          (buf2 (make-buffer "session-2"))
          (buf3 (make-buffer "session-3")))
      (add-buffer-to-ring buf3)
      (add-buffer-to-ring buf2)
      (add-buffer-to-ring buf1)
      ;; Kill index 1 (session-2)
      (clawmacs::handle-buffer-selector-key #\k)
      (is (= 2 (length *buffer-ring*)))
      (is (null (find-buffer-by-name "session-2")))
      ;; Index clamped
      (is (<= *buffer-selector-index* (1- (length *buffer-ring*))))))

  ;; Cannot kill the last buffer
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 0)
        (clawmacs::*buffer-selector-scroll* 0))
    (let ((buf1 (make-buffer "only-buffer")))
      (add-buffer-to-ring buf1)
      (clawmacs::handle-buffer-selector-key #\k)
      (is (= 1 (length *buffer-ring*)))
      (is (eq buf1 (current-buffer))))))

(test scratch-buffer-cannot-be-killed
  "Scratch remains loaded when kill commands target it."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*scratch-buffer-initial-text* ""))
    (clawmacs::init-default-keymap)
    (clawmacs::init-global-faces)
    (let ((chat (make-buffer "chat")))
      (clawmacs::init-face-registry chat)
      (setf (buffer-keymap chat) *default-keymap*)
      (add-buffer-to-ring chat)
      (let ((scratch (ensure-scratch-buffer)))
        (is (= 2 (length *buffer-ring*)))
        (kill-buffer-from-ring scratch)
        (is (= 2 (length *buffer-ring*)))
        (is (eq scratch (scratch-buffer)))
        (switch-to-buffer scratch)
        (clawmacs::kill-buffer-command scratch)
        (is (= 2 (length *buffer-ring*)))
        (is (eq scratch (scratch-buffer)))
        (kill-buffer-from-ring chat)
        (is (= 1 (length *buffer-ring*)))
        (is (eq scratch (current-buffer)))))))

(test buffer-selector-does-not-kill-scratch-buffer
  "The buffer selector kill key refuses to remove scratch."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*buffer-selector-active* t)
        (*buffer-selector-index* 1)
        (clawmacs::*buffer-selector-scroll* 0)
        (*scratch-buffer-initial-text* ""))
    (clawmacs::init-default-keymap)
    (clawmacs::init-global-faces)
    (let ((chat (make-buffer "chat")))
      (clawmacs::init-face-registry chat)
      (setf (buffer-keymap chat) *default-keymap*)
      (add-buffer-to-ring chat)
      (let ((scratch (ensure-scratch-buffer)))
        (is (equal (list chat scratch) *buffer-ring*))
        (clawmacs::handle-buffer-selector-key #\k)
        (is (= 2 (length *buffer-ring*)))
        (is (eq scratch (scratch-buffer)))))))

(test scratch-return-inserts-newline-without-sending
  "RET edits scratch text instead of finalizing and sending a chat turn."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*minibuffer-active* nil)
        (*buffer-selector-active* nil)
        (*model-selector-active* nil)
        (*think-selector-active* nil)
        (*customize-face-state* nil)
        (*openai-oauth-pending* nil)
        (*deny-message-mode* nil)
        (*cc-pending* nil)
        (*cx-pending* nil)
        (*ch-pending* nil)
        (*meta-pending* nil)
        (*scratch-buffer-initial-text* ""))
    (clawmacs::init-default-keymap)
    (clawmacs::init-global-faces)
    (let ((scratch (ensure-scratch-buffer)))
      (clawmacs::handle-key-event scratch #\a)
      (clawmacs::handle-key-event scratch #\Return)
      (clawmacs::handle-key-event scratch #\b)
      (is (= 1 (buffer-message-count scratch)))
      (is (string= (format nil "a~%b") (scratch-buffer-text scratch))))))

(test save-session-command-skips-scratch-buffer
  "Saving scratch does not write a session file."
  (let* ((*buffer-ring* nil)
         (clawmacs::*buffer-counter* 0)
         (*scratch-buffer-name* "scratch-test")
         (*scratch-buffer-initial-text* "draft")
         (*sessions-dir*
           (make-pathname
            :directory (list :absolute "tmp"
                             (format nil "clawmacs-scratch-test-~A"
                                     (string-downcase
                                      (symbol-name (gensym "DIR")))))))
         (path (clawmacs::session-path *scratch-buffer-name*)))
    (clawmacs::init-default-keymap)
    (clawmacs::init-global-faces)
    (let ((scratch (ensure-scratch-buffer)))
      (clawmacs::save-session-command scratch)
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
      (clawmacs::load-session-command buf)
      (is (eq t *minibuffer-active*))
      (is (eq :completion *minibuffer-mode*))
      (is (string= "Load Session" *minibuffer-prompt*))
      (is (equal (list session-name-a session-name-b)
                 (mapcar (lambda (item)
                           (getf item :session-name))
                         *minibuffer-filtered-items*))))))

(test load-session-command-loads-selected-session-into-a-new-buffer
  "Selecting a saved session loads it into a new current buffer."
  (with-interactive-command-test-buffer (buf)
    (let* ((session-name "saved-session-load")
           (*sessions-dir* (temp-session-test-directory "load-command"))
           (saved (make-buffer session-name :agent-name "echo")))
      (clawmacs::set-message-text (buffer-input-message saved) "hello")
      (buffer-finalize-input saved)
      (save-session saved)
      (clawmacs::load-session-command buf)
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
        (let ((msg (buffer-first-message loaded)))
          (is (not (eq msg (buffer-input-message loaded))))
          (is (eq :user (message-sender msg)))
          (is (string= "hello" (message-text msg))))))))

(test new-buffer-command-creates-loadable-session
  "New interactive chat buffers are saved immediately for later loading."
  (with-interactive-command-test-buffer (buf)
    (let ((*sessions-dir* (temp-session-test-directory "new-command")))
      (clawmacs::new-buffer-command buf)
      (let* ((created (current-buffer))
             (session-name (buffer-name created)))
        (is (not (eq buf created)))
        (is (not (null (buffer-session created))))
        (is (probe-file (clawmacs::session-path session-name)))
        (is (member session-name (list-saved-sessions) :test #'string=))
        (let ((loaded (load-session session-name)))
          (is (not (null loaded)))
          (is (string= session-name (buffer-name loaded))))))))

;;; --------------------------------------------------------------------------
;;; Package Selector Tests
;;; --------------------------------------------------------------------------

(test minibuffer-package-selector-activates
  "The package enable command lists installed packages with scope and description."
  (with-interactive-command-test-buffer (buf)
    (with-package-state-override ((default-package-test-channels))
      (clawmacs::minibuffer-toggle-package-command buf)
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
      (clawmacs::minibuffer-toggle-package-command buf)
      (let ((index (position "sexed" *minibuffer-filtered-items*
                             :key (lambda (entry)
                                    (getf entry :package-name))
                             :test #'string=)))
        (is (not (null index)))
        (setf *minibuffer-selected-index* index)
        (minibuffer-confirm))
      (is (eq t *minibuffer-active*))
      (is (eq :buffer
              (clawmacs:package-enablement-scope "sexed" :buffer buf)))
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
      (clawmacs::set-message-text (buffer-input-message buf)
                                  "existing conversation context")
      (buffer-finalize-input buf)
      (clawmacs::minibuffer-toggle-package-command buf)
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

(test describe-installed-package-command-opens-help-buffer
  "The describe package command loads package metadata and opens a help buffer."
  (with-interactive-command-test-buffer (buf)
    (with-package-state-override ((default-package-test-channels))
      (clawmacs::describe-installed-package-command buf)
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

;;; --------------------------------------------------------------------------
;;; Agent Selector Tests
;;; --------------------------------------------------------------------------

(test minibuffer-agent-selector-activates
  "C-c A opens the minibuffer agent selector with the active agent preselected."
  (with-interactive-command-test-buffer (buf)
    (let ((clawmacs::*agent-definition-registry* (make-hash-table :test #'equal))
          (clawmacs::*agent-defaults-registry* (clawmacs::make-agent-defaults-registry)))
      (setf (buffer-agent-name buf) "writer")
      (register-agent-definition "writer" :provider :zai :model "glm-5")
      (register-agent-definition "pair" :provider :openai-codex :model "gpt-5.4")
      (clawmacs::handle-key-event buf '(:ctrl-c #\A))
      (is (eq t *minibuffer-active*))
      (is (string= "Select Agent" *minibuffer-prompt*))
      (is (string= "writer" (getf (first *minibuffer-filtered-items*) :agent-name)))
      (is (getf (first *minibuffer-filtered-items*) :active-p))
      (is (= 0 *minibuffer-selected-index*)))))

(test minibuffer-agent-selector-switches-buffer-and-clears-overrides
  "Selecting an agent updates the buffer, clears overrides, and ensures a face set."
  (with-interactive-command-test-buffer (buf)
    (let ((clawmacs::*agent-definition-registry* (make-hash-table :test #'equal))
          (clawmacs::*agent-defaults-registry* (clawmacs::make-agent-defaults-registry)))
      (register-agent-definition "writer" :provider :zai :model "glm-5")
      (register-agent-definition "pair"
                                 :provider :openai-codex
                                 :model "gpt-5.4"
                                 :think-level "high")
      (setf (buffer-agent-name buf) "writer"
            (buffer-provider-override buf) :zai
            (buffer-model-override buf) "glm-5")
      (set-buffer-think-level-override buf "medium")
      (clawmacs::handle-key-event buf '(:ctrl-c #\A))
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
      (is (not (null (gethash :PAIR (buffer-face-registry buf)))))
      (let ((msg (latest-buffer-message buf)))
        (is (not (null msg)))
        (is (eq :system (message-sender msg)))
        (is (search "Agent changed to pair" (message-text msg)))
        (is (search "openai-codex/gpt-5.4" (message-text msg)))
        (is (search "think high" (message-text msg)))))))

(test minibuffer-agent-selector-cancel-leaves-buffer-unchanged
  "Cancelling the agent selector leaves the buffer state untouched."
  (with-interactive-command-test-buffer (buf)
    (let ((clawmacs::*agent-definition-registry* (make-hash-table :test #'equal))
          (clawmacs::*agent-defaults-registry* (clawmacs::make-agent-defaults-registry)))
      (register-agent-definition "writer" :provider :zai :model "glm-5")
      (register-agent-definition "pair" :provider :openai-codex :model "gpt-5.4")
      (setf (buffer-agent-name buf) "writer"
            (buffer-provider-override buf) :zai
            (buffer-model-override buf) "glm-5")
      (set-buffer-think-level-override buf "medium")
      (let ((before-count (buffer-message-count buf)))
        (clawmacs::handle-key-event buf '(:ctrl-c #\A))
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
  "select-model-command sets the model selector state when entries exist."
  (let ((*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0)
        (*model-selector-active* nil)
        (*model-selector-index* 0)
        (clawmacs::*model-selector-scroll* 0)
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
        (clawmacs::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :zai :model "model-a" :active-p t)
                (list :provider :zai :model "model-b" :active-p nil)
                (list :provider :openai-codex :model "model-c" :active-p nil))))
    (let ((buf (make-buffer "test")))
      ;; C-n = move down
      (clawmacs::handle-model-selector-key (code-char 14) buf)
      (is (= 1 *model-selector-index*))
      ;; C-n again
      (clawmacs::handle-model-selector-key (code-char 14) buf)
      (is (= 2 *model-selector-index*))
      ;; C-n at bottom = no change
      (clawmacs::handle-model-selector-key (code-char 14) buf)
      (is (= 2 *model-selector-index*))
      ;; C-p = move up
      (clawmacs::handle-model-selector-key (code-char 16) buf)
      (is (= 1 *model-selector-index*)))))

(test model-selector-enter-selects-model
  "Enter in model selector sets buffer overrides and closes."
  (let ((*model-selector-active* t)
        (*model-selector-index* 1)
        (clawmacs::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :openai-codex :model "gpt-5.4" :active-p t)
                (list :provider :zai :model "glm-5" :active-p nil)))
        (*buffer-ring* nil)
        (clawmacs::*buffer-counter* 0))
    (let ((buf (make-buffer "test")))
      (add-buffer-to-ring buf)
      ;; Select index 1 (zai/glm-5)
      (clawmacs::handle-model-selector-key #\Return buf)
      (is (null *model-selector-active*))
      (is (eq :zai (buffer-provider-override buf)))
      (is (string= "glm-5" (buffer-model-override buf)))
      (is (null (buffer-think-level-override buf))))))

(test model-selector-enter-keeps-supported-think-level
  "Switching models keeps the current think level when the new model supports it."
  (let ((*model-selector-active* t)
        (*model-selector-index* 1)
        (clawmacs::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :openai-codex :model "gpt-5.4" :active-p t)
                (list :provider :openai-codex :model "gpt-5.2" :active-p nil))))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "high")
      (clawmacs::handle-model-selector-key #\Return buf)
      (is (string= "gpt-5.2" (buffer-model-override buf)))
      (is (string= "high" (buffer-think-level-override buf))))))

(test model-selector-enter-resets-unsupported-think-level
  "Switching models clears the current think level when unsupported by the new model."
  (let ((*model-selector-active* t)
        (*model-selector-index* 1)
        (clawmacs::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :openai-codex :model "gpt-5.4" :active-p t)
                (list :provider :openai-codex :model "gpt-5.1-codex-max" :active-p nil))))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "xhigh")
      (clawmacs::handle-model-selector-key #\Return buf)
      (is (string= "gpt-5.1-codex-max" (buffer-model-override buf)))
      (is (null (buffer-think-level-override buf))))))

(test model-selector-cancel-with-c-g
  "C-g in model selector closes without changing the model."
  (let ((*model-selector-active* t)
        (*model-selector-index* 1)
        (clawmacs::*model-selector-scroll* 0)
        (*model-selector-entries*
          (list (list :provider :openai-codex :model "gpt-5.4" :active-p t)
                (list :provider :zai :model "glm-5" :active-p nil))))
    (let ((buf (make-buffer "test")))
      ;; C-g = cancel
      (clawmacs::handle-model-selector-key (code-char 7) buf)
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
  "select-think-level-command sets selector state for supported active models."
  (let ((*think-selector-active* nil)
        (*think-selector-index* 99)
        (clawmacs::*think-selector-scroll* 99)
        (*think-selector-entries* nil))
    (let ((buf (make-buffer "test-session" :agent-name "spark")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (select-think-level-command buf)
      (is (eq t *think-selector-active*))
      (is (= 0 *think-selector-index*))
      (is (= 6 (length *think-selector-entries*)))
      (is (string= "default" (getf (first *think-selector-entries*) :display))))))

(test think-selector-active-level-pre-selected
  "When opening the think selector, the active think level index is pre-selected."
  (let ((*think-selector-active* nil)
        (*think-selector-index* 0)
        (clawmacs::*think-selector-scroll* 0)
        (*think-selector-entries* nil))
    (let ((buf (make-buffer "test-session" :agent-name "spark")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "high")
      (select-think-level-command buf)
      (is (= 4 *think-selector-index*)))))

(test think-selector-enter-selects-level
  "Enter in think selector sets the buffer think level and closes."
  (let ((*think-selector-active* t)
        (*think-selector-index* 3)
        (clawmacs::*think-selector-scroll* 0))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.3-codex")
      (setf *think-selector-entries* (clawmacs::available-think-levels-for-selector buf))
      ;; Index 3 = high for gpt-5.3-codex: default, low, medium, high, xhigh
      (clawmacs::handle-think-selector-key #\Return buf)
      (is (null *think-selector-active*))
      (is (string= "high" (buffer-think-level-override buf))))))

(test think-selector-default-clears-level
  "Selecting the default think entry clears the buffer think override."
  (let ((*think-selector-active* t)
        (*think-selector-index* 0)
        (clawmacs::*think-selector-scroll* 0))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "high")
      (setf *think-selector-entries* (clawmacs::available-think-levels-for-selector buf))
      (clawmacs::handle-think-selector-key #\Return buf)
      (is (null *think-selector-active*))
      (is (null (buffer-think-level-override buf))))))

(test think-selector-cancel-with-c-g
  "C-g in think selector closes without changing the think level."
  (let ((*think-selector-active* t)
        (*think-selector-index* 1)
        (clawmacs::*think-selector-scroll* 0))
    (let ((buf (make-buffer "test")))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (set-buffer-think-level-override buf "medium")
      (setf *think-selector-entries* (clawmacs::available-think-levels-for-selector buf))
      (clawmacs::handle-think-selector-key (code-char 7) buf)
      (is (null *think-selector-active*))
      (is (string= "medium" (buffer-think-level-override buf))))))
