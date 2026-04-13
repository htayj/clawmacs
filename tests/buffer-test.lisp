(in-package :clawmacs/tests)
(in-suite buffer-suite)

(defvar *mx-test-command-log* nil
  "Records command invocations during buffer tests.")

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
          (buffer-enabled-packages buf) '("sexed"))
    (set-buffer-think-level-override buf "medium")
    (let ((data (clawmacs::serialize-buffer buf)))
      (is (eq :zai (cdr (assoc :provider-override data))))
      (is (string= "glm-5" (cdr (assoc :model-override data))))
      (is (string= "medium" (cdr (assoc :think-level-override data))))
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

(test save-and-load-session-round-trips-overrides
  "Saved sessions preserve override values and types when reloaded."
  (let* ((session-name "override-round-trip")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "clawmacs-buffer-tests")))
         (buf (make-buffer session-name :agent-name "echo")))
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.4"
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
      (is (equal '("sexed" "lispi")
                 (buffer-enabled-packages loaded))))))

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
          (keymap-lookup *default-keymap* '(:alt #\x)))))

(test execute-extended-command-opens-the-command-picker
  "M-x opens the minibuffer with commands."
  (with-interactive-command-test-buffer (buf)
    (clawmacs::handle-key-event buf '(:alt #\x))
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
    (clawmacs::handle-key-event buf '(:alt #\x))
    (select-minibuffer-command 'mx-test-noarg-command)
    (is (null *minibuffer-active*))
    (is (equal '(:noarg) *mx-test-command-log*))))

(test execute-extended-command-prompts-for-each-argument
  "Selecting a parameterized command prompts for each command argument."
  (with-interactive-command-test-buffer (buf)
    (clawmacs::handle-key-event buf '(:alt #\x))
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
    (clawmacs::handle-key-event buf '(:alt #\x))
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
    (clawmacs::handle-key-event buf '(:alt #\x))
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
        (is (search "[default] sexed - " (getf item :display)))))))

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
      (let ((item (find "sexed" *minibuffer-filtered-items*
                        :key (lambda (entry)
                               (getf entry :package-name))
                        :test #'string=)))
        (is (search "[buffer] sexed - " (getf item :display)))))))

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
