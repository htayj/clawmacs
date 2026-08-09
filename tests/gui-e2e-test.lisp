(in-package :rplaca/tests)

(in-suite gui-e2e-suite)

(defun temp-gui-e2e-path (name)
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "rplaca-gui-e2e-tests-~36R-~36R-~A"
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
            :do (return (rplaca::api-json-decode
                         (string-trim '(#\Space #\Tab)
                                      (subseq line (+ marker (length "[e2e-event]")))))))))

(defun wait-for-gui-e2e-stream (state &key (timeout-seconds 2))
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop :until (bt:with-lock-held ((rplaca::stream-state-lock state))
                   (rplaca::stream-state-done-p state))
          :while (< (get-internal-real-time) deadline)
          :do (sleep 0.02))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (rplaca::stream-state-done-p state))))

(test e2e-debug-event-writes-structured-json
  (let ((path (temp-gui-e2e-path "debug.log")))
    (let ((rplaca::*debug-log-file* path)
          (rplaca::*e2e-events-enabled-override* t)
          (rplaca::*debug-event-sequence* 0))
      (rplaca::file-debug-event "unit-test" :alpha "beta" :count 2))
    (let ((event (read-gui-e2e-event-json path)))
      (is (string= "unit-test" (cdr (assoc :event event))))
      (is (= 1 (cdr (assoc :sequence event))))
      (is (string= "beta" (cdr (assoc :alpha event))))
      (is (= 2 (cdr (assoc :count event)))))))

(test e2e-provider-streams-deterministic-hello-sentinel
  (let ((rplaca::*e2e-provider-enabled-override* t)
        (callback-count 0))
    (is-true (rplaca::known-provider-p :e2e))
    (is-true (rplaca::provider-has-token-p :e2e))
    (let ((state (rplaca::provider-request-streaming
                  :e2e
                  '(((:role . "user") (:content . "hello")))
                  (lambda (stream-state)
                    (declare (ignore stream-state))
                    (incf callback-count))
                  :model "e2e-model"
                  :system-prompt "")))
      (is-true (wait-for-gui-e2e-stream state))
      (is (< 0 callback-count))
      (bt:with-lock-held ((rplaca::stream-state-lock state))
        (is (string= "end_turn" (rplaca::stream-state-stop-reason state)))
        (is (search "RPLACA_E2E_HELLO_SENTINEL"
                    (rplaca::content-text-blocks
                     (reverse (copy-list (rplaca::stream-state-content-blocks state))))))))))
