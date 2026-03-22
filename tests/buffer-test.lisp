(in-package :clawmacs/tests)
(in-suite buffer-suite)

(test buffer-creation
  "A new buffer has one message (the input message)."
  (let ((buf (make-buffer "test-session"
                          :agent-name "echo-agent"
                          :working-directory #P"/tmp/")))
    (is (string= "test-session" (buffer-name buf)))
    (is (string= "echo-agent" (buffer-agent-name buf)))
    (is (not (null (buffer-input-message buf))))
    (is (eq (buffer-first-message buf) (buffer-input-message buf)))
    (is (eq (buffer-last-message buf) (buffer-input-message buf)))
    (is (not (message-read-only-p (buffer-input-message buf))))
    (is (= 1 (buffer-message-count buf)))
    (is (eq :idle (buffer-status buf)))))

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

(test buffer-provider-and-model-overrides
  "Buffer overrides can be set and read back."
  (let ((buf (make-buffer "test")))
    (is (null (buffer-provider-override buf)))
    (is (null (buffer-model-override buf)))
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.4")
    (is (eq :openai-codex (buffer-provider-override buf)))
    (is (string= "gpt-5.4" (buffer-model-override buf)))))

(test buffer-override-helpers-only-mutate-target-buffer
  "Per-buffer override helpers update only the provided buffer."
  (let ((first-buffer (make-buffer "first"))
        (second-buffer (make-buffer "second")))
    (set-buffer-provider-override first-buffer :openai-codex)
    (set-buffer-model-override first-buffer "gpt-5.3-codex")
    (is (eq :openai-codex (buffer-provider-override first-buffer)))
    (is (string= "gpt-5.3-codex" (buffer-model-override first-buffer)))
    (is (null (buffer-provider-override second-buffer)))
    (is (null (buffer-model-override second-buffer)))))

(test serialize-buffer-includes-overrides
  "Serialized sessions include provider/model overrides."
  (let ((buf (make-buffer "test" :agent-name "echo")))
    (setf (buffer-provider-override buf) :anthropic
          (buffer-model-override buf) "claude-3.7")
    (let ((data (clawmacs::serialize-buffer buf)))
      (is (eq :anthropic (cdr (assoc :provider-override data))))
      (is (string= "claude-3.7" (cdr (assoc :model-override data)))))))

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
        (is (null (buffer-model-override buf)))))))

(test save-and-load-session-round-trips-overrides
  "Saved sessions preserve override values and types when reloaded."
  (let* ((session-name "override-round-trip")
         (*sessions-dir* (make-pathname :directory (list :absolute "tmp" "clawmacs-buffer-tests")))
         (buf (make-buffer session-name :agent-name "echo")))
    (setf (buffer-provider-override buf) :anthropic
          (buffer-model-override buf) "claude-3.7")
    (message-insert-char (buffer-input-message buf) #\h)
    (message-insert-char (buffer-input-message buf) #\i)
    (buffer-finalize-input buf)
    (save-session buf)
    (let* ((cl-json:*json-array-type* 'list)
           (loaded (load-session session-name)))
      (is (eq :anthropic (buffer-provider-override loaded)))
      (is (typep (buffer-provider-override loaded) 'keyword))
      (is (string= "claude-3.7" (buffer-model-override loaded)))
      (is (typep (buffer-model-override loaded) 'string)))))

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
