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
