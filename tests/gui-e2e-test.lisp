(in-package :clawmacs/tests)

(in-suite gui-e2e-suite)

(defun temp-gui-e2e-path (name)
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "clawmacs-gui-e2e-tests-~36R-~36R-~A"
                                                      (get-universal-time)
                                                      (get-internal-real-time)
                                                      (gensym))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames name base)))

(defun read-gui-e2e-event-json (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (loop :for line := (read-line stream nil nil)
          :while line
          :for marker := (search "[e2e-event]" line)
          :when marker
            :do (return (clawmacs::api-json-decode
                         (string-trim '(#\Space #\Tab)
                                      (subseq line (+ marker (length "[e2e-event]")))))))))

(defun wait-for-gui-e2e-stream (state &key (timeout-seconds 2))
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop :until (bt:with-lock-held ((clawmacs::stream-state-lock state))
                   (clawmacs::stream-state-done-p state))
          :while (< (get-internal-real-time) deadline)
          :do (sleep 0.02))
    (bt:with-lock-held ((clawmacs::stream-state-lock state))
      (clawmacs::stream-state-done-p state))))

(test e2e-debug-event-writes-structured-json
  (let ((path (temp-gui-e2e-path "debug.log")))
    (let ((clawmacs::*debug-log-file* path)
          (clawmacs::*e2e-events-enabled-override* t)
          (clawmacs::*debug-event-sequence* 0))
      (clawmacs::file-debug-event "unit-test" :alpha "beta" :count 2))
    (let ((event (read-gui-e2e-event-json path)))
      (is (string= "unit-test" (cdr (assoc :event event))))
      (is (= 1 (cdr (assoc :sequence event))))
      (is (string= "beta" (cdr (assoc :alpha event))))
      (is (= 2 (cdr (assoc :count event)))))))

(test e2e-provider-streams-deterministic-hello-sentinel
  (let ((clawmacs::*e2e-provider-enabled-override* t)
        (callback-count 0))
    (is-true (clawmacs::known-provider-p :e2e))
    (is-true (clawmacs::provider-has-token-p :e2e))
    (let ((state (clawmacs::provider-request-streaming
                  :e2e
                  '(((:role . "user") (:content . "hello")))
                  (lambda (stream-state)
                    (declare (ignore stream-state))
                    (incf callback-count))
                  :model "e2e-model"
                  :system-prompt "")))
      (is-true (wait-for-gui-e2e-stream state))
      (is (< 0 callback-count))
      (bt:with-lock-held ((clawmacs::stream-state-lock state))
        (is (string= "end_turn" (clawmacs::stream-state-stop-reason state)))
        (is (search "CLAWMACS_E2E_HELLO_SENTINEL"
                    (clawmacs::content-text-blocks
                     (reverse (copy-list (clawmacs::stream-state-content-blocks state))))))))))

(test e2e-frame-snapshot-includes-semantic-chat-state
  (let ((clawmacs::*e2e-provider-enabled-override* t))
    (let* ((buf (make-buffer "e2e-buffer"
                             :agent-name "agent"
                             :session-persistence-mode :ephemeral))
           (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                               :buffer buf)))
      (set-buffer-provider-override buf :e2e)
      (set-buffer-model-override buf "e2e-model")
      (buffer-insert-read-only-message buf :user "hello" :record-p nil)
      (buffer-insert-agent-message
       buf "CLAWMACS_E2E_HELLO_SENTINEL: deterministic hello"
       :record-p nil)
      (set-message-text (buffer-input-message buf) "draft")
      (let* ((snapshot (clawmacs::chat-frame-e2e-snapshot frame))
             (screen-text (getf snapshot :screen-text)))
        (is (string= "e2e-buffer" (getf snapshot :buffer-name)))
        (is (string= "agent" (getf snapshot :agent)))
        (is (string= "e2e-model" (getf snapshot :model)))
        (is (search "user>" screen-text))
        (is (search "hello" screen-text))
        (is (search "agent>" screen-text))
        (is (search "CLAWMACS_E2E_HELLO_SENTINEL" screen-text))
        (is (string= "draft" (getf snapshot :compose-text)))
        (is (= 5 (getf snapshot :compose-length)))
        (is (string= (clawmacs::file-checkpoint-content-hash "draft")
                     (getf snapshot :compose-fingerprint)))
        (is-false (search "user> draft" screen-text))))))
