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
                      (:tool-use-id . "toolu_123")
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
