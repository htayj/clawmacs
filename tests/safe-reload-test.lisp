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

(defmacro with-safe-reload-test-async-runtime
    ((completion-dispatch &optional worker-constructor) &body body)
  "Run BODY with test-owned worker construction and completion dispatch."
  `(let ((old-dispatch
           clawmacs::*safe-reload-completion-dispatch-function*)
         (old-constructor clawmacs::*safe-reload-worker-constructor*))
     (unwind-protect
          (progn
            (setf clawmacs::*safe-reload-completion-dispatch-function*
                  ,completion-dispatch
                  clawmacs::*safe-reload-worker-constructor*
                  ,worker-constructor)
            ,@body)
       (setf clawmacs::*safe-reload-completion-dispatch-function* old-dispatch
             clawmacs::*safe-reload-worker-constructor* old-constructor))))

(defmacro with-safe-reload-quiescent-process ((&rest buffers) &body body)
  "Run BODY with deterministic empty process-wide runtime registries."
  `(let ((clawmacs::*buffer-ring* (list ,@buffers))
         (clawmacs::*openai-oauth-pending* nil)
         (clawmacs::*interop-thread-table* (make-hash-table :test #'equal))
         (clawmacs::*interop-turn-table* (make-hash-table :test #'equal))
         (clawmacs::*interop-runtime-operations* (make-hash-table :test #'eq))
         (clawmacs::*subagent-handles* (make-hash-table :test #'equal))
         (clawmacs::*synchronous-subagent-runs* (make-hash-table :test #'eq))
         (clawmacs::*message-help-runtime-reservations*
           (make-hash-table :test #'eq)))
     ,@body))

(defun safe-reload-buffer-texts (buffer)
  (loop :for message := (buffer-first-message buffer)
        :then (message-next message)
        :while message
        :collect (message-text message)))

(defun safe-reload-test-wait-until (predicate &key (timeout 2.0))
  "Wait boundedly until PREDICATE is true."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop
      (when (funcall predicate)
        (return t))
      (when (>= (get-internal-real-time) deadline)
        (return nil))
      (sleep 0.005))))

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
                  "CALL-WITH-RUNTIME-ADMISSION"
                  "CALL-WITH-RUNTIME-SETTLEMENT-ADMISSION"
                  "SAFE-RELOAD-CLAWMACS-COMMAND"))
    (multiple-value-bind (symbol status)
        (find-symbol name :clawmacs)
      (is (eq :external status))
      (is (fboundp symbol)))))

(test safe-reload-admission-condition-is-exported
  "Runtime start paths can portably handle the closed-admission condition."
  (multiple-value-bind (symbol status)
      (find-symbol "RUNTIME-ADMISSION-CLOSED" :clawmacs)
    (is (eq :external status))
    (is (subtypep symbol 'error))))

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
  "Interactive safe reload surfaces progress before its managed preflight."
  (let* ((buffer (make-buffer "reload-started"
                              :session-persistence-mode :ephemeral))
         (start-present-before-preflight-p nil)
         (completion nil)
         (completion-lock (bt:make-lock "safe-reload-start-completion")))
    (with-safe-reload-quiescent-process (buffer)
      (with-safe-reload-test-async-runtime
          ((lambda (request)
             (bt:with-lock-held (completion-lock)
               (setf completion request))))
        (with-safe-reload-test-runners
            ((lambda (&key timeout source-root)
               (declare (ignore timeout source-root))
               (setf start-present-before-preflight-p
                     (not (null
                           (find-if
                            (lambda (text)
                              (search "Clawmacs safe reload started" text))
                            (safe-reload-buffer-texts buffer)))))
               (safe-reload-test-result :ok "Preflight ok."))
             (lambda (&key buffer source-root)
               (declare (ignore buffer source-root))
               (safe-reload-test-result :ok "Live reload ok.")))
          (let ((started (clawmacs:safe-reload-clawmacs-command buffer)))
            (is (eq :started
                    (clawmacs::safe-reload-result-status started)))
            (is-true
             (safe-reload-test-wait-until
              (lambda () start-present-before-preflight-p)))
            (is-true start-present-before-preflight-p)
            (is-true
             (safe-reload-test-wait-until
              (lambda ()
                (bt:with-lock-held (completion-lock)
                  (not (null completion))))))
            (let ((result
                    (clawmacs::apply-safe-reload-preflight-completion
                     (bt:with-lock-held (completion-lock) completion))))
              (is-true (clawmacs:clawmacs-reload-result-ok-p result)))
            (let ((texts (safe-reload-buffer-texts buffer)))
              (is (find-if (lambda (text)
                             (search "Clawmacs safe reload started" text))
                           texts))
              (is (find-if (lambda (text)
                             (search "Clawmacs safe reload succeeded" text))
                           texts)))))))))

(test safe-reload-command-refuses-active-process-runtime-before-preflight
  "An active runtime owner anywhere in the process prevents interactive reload."
  (let* ((command-buffer
           (make-buffer "reload-refused-command"
                        :session-persistence-mode :ephemeral))
         (active-buffer
           (make-buffer "reload-refused-active"
                        :session-persistence-mode :ephemeral))
         (preflight-called-p nil)
         (live-called-p nil))
    (with-safe-reload-quiescent-process (command-buffer active-buffer)
      (with-safe-reload-test-async-runtime
          ((lambda (request)
             (declare (ignore request))
             (error "Refused reload must not dispatch completion.")))
        (bt:with-lock-held ((clawmacs::buffer-runtime-lock active-buffer))
          (setf (clawmacs::buffer-runtime-start-generation active-buffer) 1
                (clawmacs::buffer-runtime-start-owner active-buffer)
                (bt:current-thread)))
        (unwind-protect
             (with-safe-reload-test-runners
                 ((lambda (&key timeout source-root)
                    (declare (ignore timeout source-root))
                    (setf preflight-called-p t)
                    (safe-reload-test-result :ok "Preflight should not run."))
                  (lambda (&key buffer source-root)
                    (declare (ignore buffer source-root))
                    (setf live-called-p t)
                    (safe-reload-test-result :ok "Live should not run.")))
               (let ((result
                       (clawmacs:safe-reload-clawmacs-command command-buffer)))
                 (is (eq :refused
                         (clawmacs::safe-reload-result-status result)))
                 (is (eq :quiescence
                         (clawmacs::safe-reload-result-stage result)))
                 (is-false preflight-called-p)
                 (is-false live-called-p)
                 (let* ((texts (safe-reload-buffer-texts command-buffer))
                        (refusal
                          (find-if
                           (lambda (text)
                             (search "Clawmacs safe reload refused" text))
                           texts)))
                   (is (not (null refusal)))
                   (is (< (length refusal) 180))
                   (is-false
                    (find-if (lambda (text)
                               (search "Clawmacs safe reload started" text))
                             texts)))))
          (bt:with-lock-held ((clawmacs::buffer-runtime-lock active-buffer))
            (setf (clawmacs::buffer-runtime-start-generation active-buffer) nil
                  (clawmacs::buffer-runtime-start-owner active-buffer) nil)))))))

(test safe-reload-atomic-claim-reports-buffer-and-interop-blockers
  "Atomic claim preserves the precise quiescence blocker instead of :BUSY."
  (let ((command-buffer
          (make-buffer "reload-claim-command"
                       :session-persistence-mode :ephemeral))
        (active-buffer
          (make-buffer "reload-claim-buffer-runtime"
                       :session-persistence-mode :ephemeral)))
    (with-safe-reload-quiescent-process (command-buffer active-buffer)
      (bt:with-lock-held ((clawmacs::buffer-runtime-lock active-buffer))
        (setf (clawmacs::buffer-runtime-start-generation active-buffer) 1
              (clawmacs::buffer-runtime-start-owner active-buffer)
              (bt:current-thread)))
      (unwind-protect
           (multiple-value-bind (claimed-p blocker)
               (clawmacs::safe-reload-try-claim-request
                (clawmacs::make-safe-reload-request
                 :token (gensym "BUFFER-BLOCKER-")
                 :buffer command-buffer))
             (is-false claimed-p)
             (is (eq :buffer-runtime blocker)))
        (bt:with-lock-held ((clawmacs::buffer-runtime-lock active-buffer))
          (setf (clawmacs::buffer-runtime-start-generation active-buffer) nil
                (clawmacs::buffer-runtime-start-owner active-buffer) nil)))
      (let ((thread
              (clawmacs::make-interop-thread
               :id "reload-claim-interop"
               :buffer active-buffer
               :execution-owner "active-turn")))
        (clawmacs::register-interop-thread thread)
        (multiple-value-bind (claimed-p blocker)
            (clawmacs::safe-reload-try-claim-request
             (clawmacs::make-safe-reload-request
              :token (gensym "INTEROP-BLOCKER-")
              :buffer command-buffer))
          (is-false claimed-p)
          (is (eq :interop blocker)))
        (clawmacs::release-interop-thread-execution thread "active-turn")))))

(test safe-reload-command-runs-when-process-runtime-is-quiescent
  "A quiescent process still performs preflight and live reload synchronously."
  (let ((buffer (make-buffer "reload-quiescent"
                             :session-persistence-mode :ephemeral))
        (preflight-called-p nil)
        (live-called-p nil)
        (completion nil)
        (completion-lock (bt:make-lock "safe-reload-quiescent-completion")))
    (with-safe-reload-quiescent-process (buffer)
      (with-safe-reload-test-async-runtime
          ((lambda (request)
             (bt:with-lock-held (completion-lock)
               (setf completion request))))
        (with-safe-reload-test-runners
            ((lambda (&key timeout source-root)
               (declare (ignore timeout source-root))
               (setf preflight-called-p t)
               (safe-reload-test-result :ok "Preflight ok."))
             (lambda (&key buffer source-root)
               (declare (ignore buffer source-root))
               (setf live-called-p t)
               (safe-reload-test-result :ok "Live ok.")))
          (let ((started (clawmacs:safe-reload-clawmacs-command buffer)))
            (is (eq :started
                    (clawmacs::safe-reload-result-status started)))
            (is-true
             (safe-reload-test-wait-until
              (lambda ()
                (bt:with-lock-held (completion-lock)
                  (not (null completion))))))
            (let ((result
                    (clawmacs::apply-safe-reload-preflight-completion
                     (bt:with-lock-held (completion-lock) completion))))
              (is-true (clawmacs:clawmacs-reload-result-ok-p result)))
            (is-true preflight-called-p)
            (is-true live-called-p)
            (let ((texts (safe-reload-buffer-texts buffer)))
              (is (find-if (lambda (text)
                             (search "Clawmacs safe reload started" text))
                           texts))
              (is (find-if (lambda (text)
                             (search "Clawmacs safe reload succeeded" text))
                           texts)))))))))

(test safe-reload-blocked-preflight-returns-and-applies-on-frame-owner
  "Blocked preflight leaves the command process responsive until marshalled."
  (let* ((buffer (make-buffer "reload-blocked-preflight"
                              :session-persistence-mode :ephemeral))
         (gate-lock (bt:make-lock "safe-reload-blocked-gate"))
         (gate-condition
           (bt:make-condition-variable :name "safe-reload-blocked-gate"))
         (preflight-entered-p nil)
         (release-preflight-p nil)
         (completion nil)
         (completion-lock (bt:make-lock "safe-reload-blocked-completion"))
         (live-called-p nil)
         (live-thread nil)
         (frame-owner (bt:current-thread)))
    (with-safe-reload-quiescent-process (buffer)
      (with-safe-reload-test-async-runtime
          ((lambda (request)
             (bt:with-lock-held (completion-lock)
               (setf completion request))))
        (with-safe-reload-test-runners
            ((lambda (&key timeout source-root)
               (declare (ignore timeout source-root))
               (bt:with-lock-held (gate-lock)
                 (setf preflight-entered-p t)
                 (bt:condition-notify gate-condition)
                 (loop :until release-preflight-p
                       :do (bt:condition-wait gate-condition gate-lock)))
               (safe-reload-test-result :ok "Preflight unblocked."))
             (lambda (&key buffer source-root)
               (declare (ignore buffer source-root))
               (setf live-called-p t
                     live-thread (bt:current-thread))
               (safe-reload-test-result :ok "Live applied.")))
          (let ((started (clawmacs:safe-reload-clawmacs-command buffer)))
            (is (eq :started
                    (clawmacs::safe-reload-result-status started)))
            (is-true
             (safe-reload-test-wait-until
              (lambda ()
                (bt:with-lock-held (gate-lock) preflight-entered-p))))
            ;; This ordinary input command runs while the worker remains
            ;; blocked, proving the initiating frame/command owner returned.
            (let ((clawmacs::*self-insert-char* #\r))
              (clawmacs::self-insert-command buffer))
            (is (string= "r" (message-text (buffer-input-message buffer))))
            (is-false live-called-p)
            (is-true clawmacs::*safe-reload-running-p*)
            (bt:with-lock-held (gate-lock)
              (setf release-preflight-p t)
              (bt:condition-notify gate-condition))
            (is-true
             (safe-reload-test-wait-until
              (lambda ()
                (bt:with-lock-held (completion-lock)
                  (not (null completion))))))
            ;; Worker completion alone cannot run live reload.  Only this
            ;; frame-owned application step may do so.
            (is-false live-called-p)
            (let ((result
                    (clawmacs::apply-safe-reload-preflight-completion
                     (bt:with-lock-held (completion-lock) completion))))
              (is-true (clawmacs:clawmacs-reload-result-ok-p result)))
            (is-true live-called-p)
            (is (eq frame-owner live-thread))
            (is-false clawmacs::*safe-reload-running-p*)
            (is (string= "r"
                         (message-text (buffer-input-message buffer))))))))))

(test safe-reload-admission-remains-closed-through-live-application
  "No runtime start can publish after claim or during live redefinition."
  (let ((buffer (make-buffer "reload-admission-barrier"
                             :session-persistence-mode :ephemeral))
        (completion nil)
        (completion-lock (bt:make-lock "reload-admission-completion"))
        (published-during-preflight-p nil)
        (published-during-live-p nil)
        (preflight-refusal nil)
        (live-refusal nil)
        (published-after-p nil))
    (with-safe-reload-quiescent-process (buffer)
      (with-safe-reload-test-async-runtime
          ((lambda (request)
             (bt:with-lock-held (completion-lock)
               (setf completion request))))
        (with-safe-reload-test-runners
            ((lambda (&key timeout source-root)
               (declare (ignore timeout source-root))
               (safe-reload-test-result :ok "Immediate preflight."))
             (lambda (&key buffer source-root)
               (declare (ignore buffer source-root))
               (handler-case
                   (clawmacs:call-with-runtime-admission
                    (lambda () (setf published-during-live-p t))
                    :operation "test live-overlap start")
                 (clawmacs:runtime-admission-closed (condition)
                   (setf live-refusal condition)))
               (safe-reload-test-result :ok "Live applied.")))
          (let ((started (clawmacs:safe-reload-clawmacs-command buffer)))
            (is (eq :started
                    (clawmacs::safe-reload-result-status started)))
            (handler-case
                (clawmacs:call-with-runtime-admission
                 (lambda () (setf published-during-preflight-p t))
                 :operation "test preflight-overlap start")
              (clawmacs:runtime-admission-closed (condition)
                (setf preflight-refusal condition)))
            (is-false published-during-preflight-p)
            (is (typep preflight-refusal
                       'clawmacs:runtime-admission-closed))
            (is-true
             (safe-reload-test-wait-until
              (lambda ()
                (bt:with-lock-held (completion-lock)
                  (not (null completion))))))
            (let ((result
                    (clawmacs::apply-safe-reload-preflight-completion
                     (bt:with-lock-held (completion-lock) completion))))
              (is-true (clawmacs:clawmacs-reload-result-ok-p result)))
            (is-false published-during-live-p)
            (is (typep live-refusal 'clawmacs:runtime-admission-closed))
            (clawmacs:call-with-runtime-admission
             (lambda () (setf published-after-p t))
             :operation "test post-reload start")
            (is-true published-after-p)))))))

(test safe-reload-immediate-worker-is-recorded-and-joined-before-live
  "An immediate preflight cannot outrun worker handoff or overlap live code."
  (let ((buffer (make-buffer "reload-immediate-worker"
                             :session-persistence-mode :ephemeral))
        (completion nil)
        (completion-lock (bt:make-lock "reload-immediate-completion"))
        (dispatch-worker nil)
        (handoff-valid-p nil)
        (worker-alive-during-live-p :unknown))
    (with-safe-reload-quiescent-process (buffer)
      (with-safe-reload-test-async-runtime
          ((lambda (request)
             (setf dispatch-worker (bt:current-thread)
                   handoff-valid-p
                   (eq dispatch-worker
                       (clawmacs::safe-reload-request-worker request)))
             (bt:with-lock-held (completion-lock)
               (setf completion request))))
        (with-safe-reload-test-runners
            ((lambda (&key timeout source-root)
               (declare (ignore timeout source-root))
               (safe-reload-test-result :ok "Immediate preflight."))
             (lambda (&key buffer source-root)
               (declare (ignore buffer source-root))
               (setf worker-alive-during-live-p
                     (bt:thread-alive-p dispatch-worker))
               (safe-reload-test-result :ok "Live after join.")))
          (is (eq :started
                  (clawmacs::safe-reload-result-status
                   (clawmacs:safe-reload-clawmacs-command buffer))))
          (is-true
           (safe-reload-test-wait-until
            (lambda ()
              (bt:with-lock-held (completion-lock)
                (not (null completion))))))
          (is-true handoff-valid-p)
          (is-true
           (clawmacs:clawmacs-reload-result-ok-p
            (clawmacs::apply-safe-reload-preflight-completion
             (bt:with-lock-held (completion-lock) completion))))
          (is-false worker-alive-during-live-p))))))

(test safe-reload-worker-constructor-failure-is-contained-and-visible
  "Interactive thread-construction failure settles ownership without preflight."
  (let ((buffer (make-buffer "reload-worker-start-failure"
                             :session-persistence-mode :ephemeral))
        (preflight-called-p nil))
    (with-safe-reload-quiescent-process (buffer)
      (with-safe-reload-test-async-runtime
          ((lambda (request)
             (declare (ignore request))
             (error "Completion must not dispatch."))
           (lambda (function name)
             (declare (ignore function name))
             (error "synthetic safe reload thread failure")))
        (with-safe-reload-test-runners
            ((lambda (&key timeout source-root)
               (declare (ignore timeout source-root))
               (setf preflight-called-p t)
               (safe-reload-test-result :ok "Preflight should not run."))
             (lambda (&key buffer source-root)
               (declare (ignore buffer source-root))
               (safe-reload-test-result :ok "Live should not run.")))
          (let ((result (clawmacs:safe-reload-clawmacs-command buffer)))
            (is (eq :preflight-failed
                    (clawmacs::safe-reload-result-status result)))
            (is (eq :worker-start
                    (clawmacs::safe-reload-result-stage result)))
            (is-false preflight-called-p)
            (is-false clawmacs::*safe-reload-running-p*)
            (is (find-if
                 (lambda (text)
                   (and (search "Clawmacs safe reload failed" text)
                        (search "synthetic safe reload thread failure" text)))
                 (safe-reload-buffer-texts buffer)))))))))

(test safe-reload-nil-worker-constructor-is-contained-and-releases-admission
  "A constructor returning NIL cannot strand exact reload ownership."
  (let ((buffer (make-buffer "reload-nil-worker"
                             :session-persistence-mode :ephemeral)))
    (with-safe-reload-quiescent-process (buffer)
      (with-safe-reload-test-async-runtime
          ((lambda (request)
             (declare (ignore request))
             (error "Completion must not dispatch."))
           (lambda (function name)
             (declare (ignore function name))
             nil))
        (with-safe-reload-test-runners
            ((lambda (&key timeout source-root)
               (declare (ignore timeout source-root))
               (error "Preflight must not run."))
             (lambda (&key buffer source-root)
               (declare (ignore buffer source-root))
               (error "Live must not run.")))
          (let ((result (clawmacs:safe-reload-clawmacs-command buffer)))
            (is (eq :preflight-failed
                    (clawmacs::safe-reload-result-status result)))
            (is (eq :worker-start
                    (clawmacs::safe-reload-result-stage result)))
            (is (search "returned NIL"
                        (clawmacs:clawmacs-reload-result-summary result)))
            (is-false clawmacs::*safe-reload-running-p*)
            (is (null clawmacs::*safe-reload-active-request*))))))))

(test safe-reload-quiescence-covers-global-runtime-registries
  "OAuth, interop, and subagent registries all participate in quiescence."
  (let ((buffer (make-buffer "reload-registry-coverage"
                             :session-persistence-mode :ephemeral)))
    (with-safe-reload-quiescent-process (buffer)
      (is (null (clawmacs::safe-reload-process-runtime-activity buffer)))
      (let ((flow (clawmacs::make-openai-oauth-flow :buffer buffer)))
        (is-true (clawmacs::publish-openai-oauth-pending-flow flow))
        (is (eq :oauth
                (clawmacs::safe-reload-process-runtime-activity buffer)))
        (is (eq flow (clawmacs::take-openai-oauth-pending-flow))))
      (let ((reservation (clawmacs::reserve-message-help-runtime)))
        (unwind-protect
             (is (eq :help-frame
                     (clawmacs::safe-reload-process-runtime-activity buffer)))
          (clawmacs::release-message-help-runtime reservation)))
      (let ((thread (clawmacs::make-interop-thread
                     :id "reload-active-thread"
                     :buffer buffer
                     :execution-owner "reload-active-turn")))
        (clawmacs::register-interop-thread thread)
        (is (eq :interop
                (clawmacs::safe-reload-process-runtime-activity buffer)))
        (is-true
         (clawmacs::release-interop-thread-execution
          thread "reload-active-turn")))
      (let ((turn (clawmacs::make-interop-turn
                   :id "reload-active-turn"
                   :thread-id "reload-active-thread"
                   :status :running
                   :runner-installed-p t)))
        (clawmacs::register-interop-turn turn)
        (is (eq :interop
                (clawmacs::safe-reload-process-runtime-activity buffer)))
        (clawmacs::finalize-interop-turn turn :succeeded)
        ;; Terminal provider state precedes runner unwind; reload must still
        ;; refuse until that exact runner publishes final settlement.
        (is (eq :interop
                (clawmacs::safe-reload-process-runtime-activity buffer)))
        (clawmacs::finish-interop-turn-runner turn)
        (is (null (clawmacs::safe-reload-process-runtime-activity buffer))))
      (let ((handle (clawmacs::make-subagent-handle
                     :id "reload-active-subagent"
                     :status :running
                     :worker-finished-p nil)))
        (clawmacs::register-subagent-handle handle)
        (is (eq :subagent
                (clawmacs::safe-reload-process-runtime-activity buffer))))
      (clrhash clawmacs::*subagent-handles*)
      (clawmacs::call-with-synchronous-subagent-run-reservation
       (lambda ()
         (is (= 1 (clawmacs:active-synchronous-subagent-run-count)))
         (is (eq :subagent
                 (clawmacs::safe-reload-process-runtime-activity buffer)))))
      (is (= 0 (clawmacs:active-synchronous-subagent-run-count))))))

(test safe-reload-refuses-terminal-oauth-until-worker-cleanup-settles
  "OAuth result publication cannot hide a callback worker still unwinding."
  (let* ((buffer (make-buffer "reload-oauth-cleanup-barrier"
                              :session-persistence-mode :ephemeral))
         (flow (clawmacs::make-openai-oauth-flow :buffer buffer))
         (gate-lock (bt:make-lock "reload-oauth-cleanup-gate"))
         (gate-condition
           (bt:make-condition-variable :name "reload-oauth-cleanup-gate"))
         (cleanup-entered-p nil)
         (release-cleanup-p nil)
         (settled-wake
           (bt:make-semaphore :name "reload-oauth-settled-wake"))
         (applied
           (bt:make-semaphore :name "reload-oauth-settled-applied"))
         (worker nil)
         (pump nil)
         (automatic-result :not-run)
         (preflight-called-p nil)
         (clawmacs::*runtime-settlement-notify-function*
           (lambda (changed-buffer reason)
             (declare (ignore changed-buffer))
             (when (eq reason :oauth-settled)
               (bt:signal-semaphore settled-wake)))))
    ;; Keep the pending-flow cell process-global in this cross-thread test;
    ;; dynamically bound specials are intentionally not shared by BT workers.
    (let ((clawmacs::*buffer-ring* (list buffer))
          (clawmacs::*interop-thread-table* (make-hash-table :test #'equal))
          (clawmacs::*interop-turn-table* (make-hash-table :test #'equal))
          (clawmacs::*interop-runtime-operations* (make-hash-table :test #'eq))
          (clawmacs::*subagent-handles* (make-hash-table :test #'equal))
          (clawmacs::*synchronous-subagent-runs* (make-hash-table :test #'eq))
          (clawmacs::*message-help-runtime-reservations*
            (make-hash-table :test #'eq)))
      (bt:with-lock-held (clawmacs::*openai-oauth-pending-lock*)
        (setf clawmacs::*openai-oauth-pending* nil))
      (unwind-protect
           (progn
             (setf worker
                   (bt:make-thread
                    (lambda ()
                      (clawmacs::openai-oauth-flow-set-result
                       flow :success t :token "synthetic-token")
                      (bt:with-lock-held (gate-lock)
                        (setf cleanup-entered-p t)
                        (bt:condition-notify gate-condition)
                        (loop :until release-cleanup-p
                              :do (bt:condition-wait gate-condition
                                                     gate-lock))))
                    :name "safe-reload-oauth-cleanup-barrier"))
             (bt:with-lock-held ((clawmacs::openai-oauth-flow-lock flow))
               (setf (clawmacs::openai-oauth-flow-thread flow) worker))
             (is-true (clawmacs::publish-openai-oauth-pending-flow flow))
             (is-true
              (safe-reload-test-wait-until
               (lambda ()
                 (bt:with-lock-held (gate-lock) cleanup-entered-p))))
             (with-safe-reload-test-runners
                 ((lambda (&key timeout source-root)
                    (declare (ignore timeout source-root))
                    (setf preflight-called-p t)
                    (safe-reload-test-result :ok "Must not run."))
                  (lambda (&key buffer source-root)
                    (declare (ignore buffer source-root))
                    (safe-reload-test-result :ok "Must not run.")))
               (let ((result (clawmacs:clawmacs-safe-reload
                              :buffer buffer :notify-p nil)))
                 (is (eq :refused
                         (clawmacs::safe-reload-result-status result)))
                 (is-false preflight-called-p)))
             (setf pump
                   (bt:make-thread
                    (lambda ()
                      (when (bt:wait-on-semaphore settled-wake :timeout 5)
                        (setf automatic-result
                              (clawmacs::update-openai-oauth-login buffer)))
                      (bt:signal-semaphore applied))
                    :name "simulated-clim-oauth-event-pump"))
             ;; The first frame update only installs the managed joiner.
             (is-true (clawmacs::update-openai-oauth-login buffer))
             (is (eq flow (clawmacs::openai-oauth-pending-flow)))
             (is-true (bt:thread-alive-p worker))
             (bt:with-lock-held (gate-lock)
               (setf release-cleanup-p t)
               (bt:condition-notify gate-condition))
             ;; The joiner supplies the only retry wake; the test does not
             ;; manually poll UPDATE a second time.
             (is-true (bt:wait-on-semaphore applied :timeout 3))
             (bt:join-thread pump)
             (setf pump nil)
             (is (null automatic-result))
             (is (null (clawmacs::openai-oauth-pending-flow)))
             (is (null (clawmacs::openai-oauth-flow-thread flow)))
             (is (null
                  (clawmacs::openai-oauth-flow-settlement-thread flow))))
        (bt:with-lock-held (gate-lock)
          (setf release-cleanup-p t)
          (bt:condition-notify gate-condition))
        (bt:signal-semaphore settled-wake)
        (when pump
          (bt:join-thread pump))
        (when worker
          (bt:join-thread worker))
        (clawmacs::claim-openai-oauth-pending-flow flow)
        (bt:with-lock-held (clawmacs::*openai-oauth-pending-lock*)
          (setf clawmacs::*openai-oauth-pending* nil))))))

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

(test safe-reload-is-a-user-command-not-a-provider-tool
  "Safe reload remains an explicit command and is absent from provider tools."
  (let ((clawmacs::*tool-table* (make-hash-table :test #'equal)))
    (clawmacs:init-tools)
    (is (null (gethash "clawmacs_reload" clawmacs::*tool-table*)))
    (is (fboundp 'clawmacs:safe-reload-clawmacs-command))))

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

(test safe-reload-finish-wakes-three-runtime-settlement-waiters
  "One exact request completion releases every already-owned cleanup waiter."
  (let* ((request (clawmacs::make-safe-reload-request
                   :token (list :settlement-test (gensym "REQUEST-"))
                   :mode :test
                   :started-at (get-internal-real-time)
                   :notify-p nil))
         (ready (bt:make-semaphore :name "settlement waiters ready"))
         (completed (bt:make-semaphore :name "settlement waiters completed"))
         (threads nil))
    (bt:with-lock-held (clawmacs::*safe-reload-lock*)
      (setf clawmacs::*safe-reload-active-request* request
            clawmacs::*safe-reload-running-p* t))
    (unwind-protect
         (progn
           (setf threads
                 (loop :for index :below 3
                       :collect
                       (bt:make-thread
                        (lambda ()
                          (bt:signal-semaphore ready)
                          (clawmacs:call-with-runtime-settlement-admission
                           (lambda ()
                             (bt:signal-semaphore completed))
                           :operation "three-waiter settlement test"))
                        :name (format nil "settlement-waiter-~D" index))))
           (dotimes (_ 3)
             (declare (ignore _))
             (is-true (bt:wait-on-semaphore ready :timeout 2.0)))
           (is-false (bt:wait-on-semaphore completed :timeout 0.05))
           (is-true (clawmacs::finish-safe-reload-request request))
           (dotimes (_ 3)
             (declare (ignore _))
             (is-true (bt:wait-on-semaphore completed :timeout 2.0)))
           (dolist (thread threads)
             (bt:join-thread thread)))
      (clawmacs::finish-safe-reload-request request)
      (dolist (thread threads)
        (when (bt:thread-alive-p thread)
          (bt:join-thread thread))))))
