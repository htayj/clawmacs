(in-package :clawmacs/tests)
(in-suite safe-reload-suite)

(defun safe-reload-test-result (status summary &key stage preflight live source-root)
  (clawmacs::make-safe-reload-result
   :status status
   :stage stage
   :summary summary
   :preflight preflight
   :live live
   :source-root source-root))

(defmacro with-safe-reload-test-runners ((preflight live) &body body)
  `(let ((old-preflight clawmacs::*safe-reload-preflight-function*)
         (old-live clawmacs::*safe-reload-live-function*))
     (unwind-protect
          (progn
            (setf clawmacs::*safe-reload-preflight-function* ,preflight
                  clawmacs::*safe-reload-live-function* ,live)
            ,@body)
       (setf clawmacs::*safe-reload-preflight-function* old-preflight
             clawmacs::*safe-reload-live-function* old-live))))

(defun safe-reload-buffer-texts (buffer)
  (loop :for message := (buffer-first-message buffer)
        :then (message-next message)
        :while message
        :collect (message-text message)))

(defun safe-reload-temp-path (name)
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "clawmacs-safe-reload-tests-~36R-~36R-~A"
                                                      (get-universal-time)
                                                      (get-internal-real-time)
                                                      (gensym))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames name base)))

(defun read-safe-reload-debug-event (path event-name)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (loop :for line := (read-line stream nil nil)
          :while line
          :for marker := (search "[e2e-event]" line)
          :when marker
            :do (let ((event (clawmacs::api-json-decode
                              (string-trim '(#\Space #\Tab)
                                           (subseq line (+ marker
                                                           (length "[e2e-event]")))))))
                  (when (string= event-name (cdr (assoc :event event)))
                    (return event))))))

(test safe-reload-api-symbols-are-exported
  "The core safe reload entry points are public Clawmacs API symbols."
  (dolist (name '("CLAWMACS-SAFE-RELOAD"
                  "CLAWMACS-SAFE-RELOAD-PREFLIGHT"
                  "CLAWMACS-RELOAD-RESULT-OK-P"
                  "CLAWMACS-RELOAD-RESULT-SUMMARY"
                  "SAFE-RELOAD-CLAWMACS-COMMAND"))
    (multiple-value-bind (symbol status)
        (find-symbol name :clawmacs)
      (is (eq :external status))
      (is (fboundp symbol)))))

(test safe-reload-preflight-failure-skips-live-reload-and-notifies
  "A failed isolated preflight returns to the caller without mutating via live reload."
  (let* ((buffer (make-buffer "reload-preflight-failure"
                              :session-persistence-mode :ephemeral))
         (live-called-p nil))
    (buffer-insert-read-only-message buffer :user "still visible" :record-p nil)
    (set-message-text (buffer-input-message buffer) "draft")
    (with-safe-reload-test-runners
        ((lambda (&key timeout source-root)
           (declare (ignore timeout source-root))
           (safe-reload-test-result :preflight-failed "Preflight failed."))
         (lambda (&key buffer source-root)
           (declare (ignore buffer source-root))
           (setf live-called-p t)
           (safe-reload-test-result :ok "Live should not run.")))
      (let ((result (clawmacs:clawmacs-safe-reload :buffer buffer)))
        (is (eq :preflight-failed
                (clawmacs::safe-reload-result-status result)))
        (is-false (clawmacs:clawmacs-reload-result-ok-p result))
        (is-false live-called-p)
        (is (string= "Preflight failed."
                     (clawmacs:clawmacs-reload-result-summary result)))
        (is (string= "draft" (message-text (buffer-input-message buffer))))
        (let ((texts (safe-reload-buffer-texts buffer)))
          (is (member "still visible" texts :test #'string=))
          (is (find-if (lambda (text)
                         (search "Clawmacs safe reload failed" text))
                       texts)))))))

(test safe-reload-success-runs-live-reload-and-preserves-buffer-state
  "After an OK preflight, live reload runs and reports a visible success message."
  (let* ((buffer (make-buffer "reload-success"
                              :session-persistence-mode :ephemeral))
         (live-called-p nil)
         (preflight (safe-reload-test-result :ok "Preflight ok.")))
    (buffer-insert-read-only-message buffer :user "conversation remains" :record-p nil)
    (set-message-text (buffer-input-message buffer) "compose survives")
    (with-safe-reload-test-runners
        ((lambda (&key timeout source-root)
           (declare (ignore timeout source-root))
           preflight)
         (lambda (&key buffer source-root)
           (declare (ignore buffer source-root))
           (setf live-called-p t)
           (safe-reload-test-result :ok "Live reload ok.")))
      (let ((result (clawmacs:clawmacs-safe-reload :buffer buffer)))
        (is-true (clawmacs:clawmacs-reload-result-ok-p result))
        (is-true live-called-p)
        (is (eq preflight (clawmacs::safe-reload-result-preflight result)))
        (is (string= "compose survives"
                     (message-text (buffer-input-message buffer))))
        (let ((texts (safe-reload-buffer-texts buffer)))
          (is (member "conversation remains" texts :test #'string=))
          (is (find-if (lambda (text)
                         (search "Clawmacs safe reload succeeded" text))
                       texts))
          (is (find-if (lambda (text)
                         (search "Preflight and live reload completed" text))
                       texts))
          (is-false (find-if (lambda (text)
                               (search "succeeded: Clawmacs safe reload succeeded" text))
                             texts)))))))

(test safe-reload-command-inserts-start-notification-before-preflight
  "Interactive safe reload commands surface progress before blocking preflight."
  (let* ((buffer (make-buffer "reload-started"
                              :session-persistence-mode :ephemeral))
         (start-present-before-preflight-p nil))
    (with-safe-reload-test-runners
        ((lambda (&key timeout source-root)
           (declare (ignore timeout source-root))
           (setf start-present-before-preflight-p
                 (not (null
                       (find-if (lambda (text)
                                  (search "Clawmacs safe reload started" text))
                                (safe-reload-buffer-texts buffer)))))
           (safe-reload-test-result :ok "Preflight ok."))
         (lambda (&key buffer source-root)
           (declare (ignore buffer source-root))
           (safe-reload-test-result :ok "Live reload ok.")))
      (let ((result (clawmacs:safe-reload-clawmacs-command buffer)))
        (is-true (clawmacs:clawmacs-reload-result-ok-p result))
        (is-true start-present-before-preflight-p)
        (let ((texts (safe-reload-buffer-texts buffer)))
          (is (find-if (lambda (text)
                         (search "Clawmacs safe reload started" text))
                       texts))
          (is (find-if (lambda (text)
                         (search "Clawmacs safe reload succeeded" text))
                       texts)))))))

(test safe-reload-visible-failure-summary-is-concise
  "Visible transcript notifications do not include full compiler/process logs."
  (let* ((long-summary (format nil "Preflight reload failed: ~A~%~A"
                               (make-string 400 :initial-element #\x)
                               (make-string 400 :initial-element #\y)))
         (result (safe-reload-test-result :preflight-failed long-summary))
         (notification (clawmacs::safe-reload-notification-text result)))
    (is (< (length notification) 360))
    (is (search "see tool result/debug log for details" notification))
    (is-false (search (make-string 300 :initial-element #\y) notification))))

(test safe-reload-source-root-reaches-preflight-and-live-reload
  "A caller-supplied source root is checked and loaded consistently."
  (let ((source-root (safe-reload-temp-path "source-root/"))
        (preflight-source-root nil)
        (live-source-root nil))
    (with-safe-reload-test-runners
        ((lambda (&key timeout source-root)
           (declare (ignore timeout))
           (setf preflight-source-root source-root)
           (safe-reload-test-result :ok "Preflight ok."
                                    :source-root source-root))
         (lambda (&key buffer source-root)
           (declare (ignore buffer))
           (setf live-source-root source-root)
           (safe-reload-test-result :ok "Live ok."
                                    :source-root source-root)))
      (let ((result (clawmacs:clawmacs-safe-reload :source-root source-root
                                                   :notify-p nil)))
        (is-true (clawmacs:clawmacs-reload-result-ok-p result))
        (is (equal source-root preflight-source-root))
        (is (equal source-root live-source-root))
        (is (equal source-root
                   (clawmacs::safe-reload-result-source-root result)))))))

(test safe-reload-live-failure-is-caught-and-reported
  "Live reload errors become :LIVE-FAILED results instead of escaping."
  (let ((buffer (make-buffer "reload-live-failure"
                             :session-persistence-mode :ephemeral)))
    (with-safe-reload-test-runners
        ((lambda (&key timeout source-root)
           (declare (ignore timeout source-root))
           (safe-reload-test-result :ok "Preflight ok."))
         (lambda (&key buffer source-root)
           (declare (ignore buffer source-root))
           (error "boom during live reload")))
      (let ((result (clawmacs:clawmacs-safe-reload :buffer buffer)))
        (is (eq :live-failed (clawmacs::safe-reload-result-status result)))
        (is-false (clawmacs:clawmacs-reload-result-ok-p result))
        (is (search "boom during live reload"
                    (clawmacs:clawmacs-reload-result-summary result)))
        (is (find-if (lambda (text)
                       (search "Clawmacs safe reload failed" text))
                     (safe-reload-buffer-texts buffer)))))))

(test safe-reload-overlap-returns-busy-without-running-preflight
  "The reload lock is nonblocking for overlapping requests."
  (let ((preflight-called-p nil))
    (is-true (bt:acquire-lock clawmacs::*safe-reload-lock* nil))
    (unwind-protect
         (with-safe-reload-test-runners
             ((lambda (&key timeout source-root)
                (declare (ignore timeout source-root))
                (setf preflight-called-p t)
                (safe-reload-test-result :ok "Preflight ok."))
              (lambda (&key buffer source-root)
                (declare (ignore buffer source-root))
                (safe-reload-test-result :ok "Live ok.")))
           (let ((result (clawmacs:clawmacs-safe-reload :notify-p nil)))
             (is (eq :busy (clawmacs::safe-reload-result-status result)))
             (is-false preflight-called-p)))
      (bt:release-lock clawmacs::*safe-reload-lock*))))

(test safe-reload-provider-tool-is-agent-allowed
  "clawmacs_reload is exposed as an agent-allowed provider tool."
  (let ((clawmacs::*tool-table* (make-hash-table :test #'equal)))
    (clawmacs:init-tools)
    (let ((definition (gethash "clawmacs_reload" clawmacs::*tool-table*)))
      (is (not (null definition)))
      (is (eq :agent-allowed
              (clawmacs:tool-definition-permission definition))))))

(test safe-reload-emits-debug-result-event
  "Safe reload completion is visible to the semantic debug event stream."
  (let ((path (safe-reload-temp-path "debug.log")))
    (with-safe-reload-test-runners
        ((lambda (&key timeout source-root)
           (declare (ignore timeout source-root))
           (safe-reload-test-result :ok "Preflight ok."))
         (lambda (&key buffer source-root)
           (declare (ignore buffer source-root))
           (safe-reload-test-result :ok "Live ok.")))
      (let ((clawmacs::*debug-log-file* path)
            (clawmacs::*e2e-events-enabled-override* t)
            (clawmacs::*debug-event-sequence* 0))
        (clawmacs:clawmacs-safe-reload :notify-p nil)))
    (let ((event (read-safe-reload-debug-event path "safe-reload-result")))
      (is (not (null event)))
      (is (string= "ok" (cdr (assoc :status event))))
      (is (search "Preflight and live reload completed"
                  (cdr (assoc :summary event)))))))

(test safe-reload-stress-overlapping-requests
  "A burst of concurrent reload requests yields one reload and busy results for the overlap."
  (let ((results nil)
        (results-lock (bt:make-lock "safe-reload-test-results"))
        (start-p nil)
        (preflight-entered-p nil)
        (release-preflight-p nil))
    (with-safe-reload-test-runners
        ((lambda (&key timeout source-root)
           (declare (ignore timeout source-root))
           (setf preflight-entered-p t)
           (loop :until release-preflight-p
                 :do (sleep 0.01))
           (safe-reload-test-result :ok "Preflight ok."))
         (lambda (&key buffer source-root)
           (declare (ignore buffer source-root))
           (safe-reload-test-result :ok "Live ok.")))
      (let ((threads
              (loop :repeat 12
                    :collect (bt:make-thread
                              (lambda ()
                                (loop :until start-p
                                      :do (sleep 0.001))
                                (let ((result (clawmacs:clawmacs-safe-reload
                                               :notify-p nil)))
                                  (bt:with-lock-held (results-lock)
                                    (push (clawmacs::safe-reload-result-status result)
                                          results))))))))
        (setf start-p t)
        (loop :until preflight-entered-p
              :do (sleep 0.01))
        (sleep 0.05)
        (setf release-preflight-p t)
        (dolist (thread threads)
          (bt:join-thread thread))
        (is (= 12 (length results)))
        (is (= 1 (count :ok results)))
        (is (= 11 (count :busy results)))))))
