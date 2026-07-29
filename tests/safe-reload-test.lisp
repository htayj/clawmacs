(in-package :rplaca/tests)
(in-suite safe-reload-suite)

(defun safe-reload-test-result (status summary &key stage preflight live source-root)
  (rplaca::make-safe-reload-result
   :status status
   :stage stage
   :summary summary
   :preflight preflight
   :live live
   :source-root source-root))

(defmacro with-safe-reload-test-runners ((preflight live) &body body)
  `(let ((old-preflight rplaca::*safe-reload-preflight-function*)
         (old-live rplaca::*safe-reload-live-function*))
     (unwind-protect
          (progn
            (setf rplaca::*safe-reload-preflight-function* ,preflight
                  rplaca::*safe-reload-live-function* ,live)
            ,@body)
       (setf rplaca::*safe-reload-preflight-function* old-preflight
             rplaca::*safe-reload-live-function* old-live))))

(defmacro with-safe-reload-function-override
    ((name lambda-list &body implementation) &body body)
  "Temporarily replace NAME during a serial Safe Reload unit test."
  (let ((original (gensym "ORIGINAL")))
    `(let ((,original (symbol-function ',name)))
       (unwind-protect
            (progn
              (setf (symbol-function ',name)
                    (lambda ,lambda-list ,@implementation))
              ,@body)
         (setf (symbol-function ',name) ,original)))))

(defmacro with-safe-reload-test-async-runtime
    ((completion-dispatch &optional worker-constructor) &body body)
  "Run BODY with test-owned worker construction and completion dispatch."
  `(let ((old-dispatch
           rplaca::*safe-reload-completion-dispatch-function*)
         (old-constructor rplaca::*safe-reload-worker-constructor*))
     (unwind-protect
          (progn
            (setf rplaca::*safe-reload-completion-dispatch-function*
                  ,completion-dispatch
                  rplaca::*safe-reload-worker-constructor*
                  ,worker-constructor)
            ,@body)
       (setf rplaca::*safe-reload-completion-dispatch-function* old-dispatch
             rplaca::*safe-reload-worker-constructor* old-constructor))))

(defmacro with-safe-reload-quiescent-process ((&rest buffers) &body body)
  "Run BODY with deterministic empty process-wide runtime registries."
  `(let ((rplaca::*buffer-ring* (list ,@buffers))
         (rplaca::*openai-oauth-pending* nil)
         (rplaca::*interop-thread-table* (make-hash-table :test #'equal))
         (rplaca::*interop-turn-table* (make-hash-table :test #'equal))
         (rplaca::*interop-runtime-operations* (make-hash-table :test #'eq))
         (rplaca::*subagent-handles* (make-hash-table :test #'equal))
         (rplaca::*synchronous-subagent-runs* (make-hash-table :test #'eq))
         (rplaca::*message-help-runtime-reservations*
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
                                              (format nil "rplaca-safe-reload-tests-~36R-~36R-~A"
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
            :do (let ((event (rplaca::api-json-decode
                              (string-trim '(#\Space #\Tab)
                                           (subseq line (+ marker
                                                           (length "[e2e-event]")))))))
                  (when (string= event-name (cdr (assoc :event event)))
                    (return event))))))

(test safe-reload-api-symbols-are-exported
  "The core safe reload entry points are public RPLACA API symbols."
  (dolist (name '("RPLACA-SAFE-RELOAD"
                  "RPLACA-SAFE-RELOAD-PREFLIGHT"
                  "RPLACA-RELOAD-RESULT-OK-P"
                  "RPLACA-RELOAD-RESULT-SUMMARY"
                  "CALL-WITH-RUNTIME-ADMISSION"
                  "CALL-WITH-RUNTIME-SETTLEMENT-ADMISSION"
                  "SAFE-RELOAD-RPLACA-COMMAND"))
    (multiple-value-bind (symbol status)
        (find-symbol name :rplaca)
      (is (eq :external status))
      (is (fboundp symbol)))))

(test safe-reload-admission-condition-is-exported
  "Runtime start paths can portably handle the closed-admission condition."
  (multiple-value-bind (symbol status)
      (find-symbol "RUNTIME-ADMISSION-CLOSED" :rplaca)
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
      (let ((result (rplaca:rplaca-safe-reload :buffer buffer)))
        (is (eq :preflight-failed
                (rplaca::safe-reload-result-status result)))
        (is-false (rplaca:rplaca-reload-result-ok-p result))
        (is-false live-called-p)
        (is (string= "Preflight failed."
                     (rplaca:rplaca-reload-result-summary result)))
        (is (string= "draft" (message-text (buffer-input-message buffer))))
        (let ((texts (safe-reload-buffer-texts buffer)))
          (is (member "still visible" texts :test #'string=))
          (is (find-if (lambda (text)
                         (search "RPLACA safe reload failed" text))
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
      (let ((result (rplaca:rplaca-safe-reload :buffer buffer)))
        (is-true (rplaca:rplaca-reload-result-ok-p result))
        (is-true live-called-p)
        (is (eq preflight (rplaca::safe-reload-result-preflight result)))
        (is (string= "compose survives"
                     (message-text (buffer-input-message buffer))))
        (let ((texts (safe-reload-buffer-texts buffer)))
          (is (member "conversation remains" texts :test #'string=))
          (is (find-if (lambda (text)
                         (search "RPLACA safe reload succeeded" text))
                       texts))
          (is (find-if (lambda (text)
                         (search "Preflight and live reload completed" text))
                       texts))
          (is-false (find-if (lambda (text)
                               (search "succeeded: RPLACA safe reload succeeded" text))
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
                              (search "RPLACA safe reload started" text))
                            (safe-reload-buffer-texts buffer)))))
               (safe-reload-test-result :ok "Preflight ok."))
             (lambda (&key buffer source-root)
               (declare (ignore buffer source-root))
               (safe-reload-test-result :ok "Live reload ok.")))
          (let ((started (rplaca:safe-reload-rplaca-command buffer)))
            (is (eq :started
                    (rplaca::safe-reload-result-status started)))
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
                    (rplaca::apply-safe-reload-preflight-completion
                     (bt:with-lock-held (completion-lock) completion))))
              (is-true (rplaca:rplaca-reload-result-ok-p result)))
            (let ((texts (safe-reload-buffer-texts buffer)))
              (is (find-if (lambda (text)
                             (search "RPLACA safe reload started" text))
                           texts))
              (is (find-if (lambda (text)
                             (search "RPLACA safe reload succeeded" text))
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
        (bt:with-lock-held ((rplaca::buffer-runtime-lock active-buffer))
          (setf (rplaca::buffer-runtime-start-generation active-buffer) 1
                (rplaca::buffer-runtime-start-owner active-buffer)
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
                       (rplaca:safe-reload-rplaca-command command-buffer)))
                 (is (eq :refused
                         (rplaca::safe-reload-result-status result)))
                 (is (eq :quiescence
                         (rplaca::safe-reload-result-stage result)))
                 (is-false preflight-called-p)
                 (is-false live-called-p)
                 (let* ((texts (safe-reload-buffer-texts command-buffer))
                        (refusal
                          (find-if
                           (lambda (text)
                             (search "RPLACA safe reload refused" text))
                           texts)))
                   (is (not (null refusal)))
                   (is (< (length refusal) 180))
                   (is-false
                    (find-if (lambda (text)
                               (search "RPLACA safe reload started" text))
                             texts)))))
          (bt:with-lock-held ((rplaca::buffer-runtime-lock active-buffer))
            (setf (rplaca::buffer-runtime-start-generation active-buffer) nil
                  (rplaca::buffer-runtime-start-owner active-buffer) nil)))))))

(test safe-reload-atomic-claim-reports-buffer-and-interop-blockers
  "Atomic claim preserves the precise quiescence blocker instead of :BUSY."
  (let ((command-buffer
          (make-buffer "reload-claim-command"
                       :session-persistence-mode :ephemeral))
        (active-buffer
          (make-buffer "reload-claim-buffer-runtime"
                       :session-persistence-mode :ephemeral)))
    (with-safe-reload-quiescent-process (command-buffer active-buffer)
      (bt:with-lock-held ((rplaca::buffer-runtime-lock active-buffer))
        (setf (rplaca::buffer-runtime-start-generation active-buffer) 1
              (rplaca::buffer-runtime-start-owner active-buffer)
              (bt:current-thread)))
      (unwind-protect
           (multiple-value-bind (claimed-p blocker)
               (rplaca::safe-reload-try-claim-request
                (rplaca::make-safe-reload-request
                 :token (gensym "BUFFER-BLOCKER-")
                 :buffer command-buffer))
             (is-false claimed-p)
             (is (eq :buffer-runtime blocker)))
        (bt:with-lock-held ((rplaca::buffer-runtime-lock active-buffer))
          (setf (rplaca::buffer-runtime-start-generation active-buffer) nil
                (rplaca::buffer-runtime-start-owner active-buffer) nil)))
      (let ((thread
              (rplaca::make-interop-thread
               :id "reload-claim-interop"
               :buffer active-buffer
               :execution-owner "active-turn")))
        (rplaca::register-interop-thread thread)
        (multiple-value-bind (claimed-p blocker)
            (rplaca::safe-reload-try-claim-request
             (rplaca::make-safe-reload-request
              :token (gensym "INTEROP-BLOCKER-")
              :buffer command-buffer))
          (is-false claimed-p)
          (is (eq :interop blocker)))
        (rplaca::release-interop-thread-execution thread "active-turn")))))

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
          (let ((started (rplaca:safe-reload-rplaca-command buffer)))
            (is (eq :started
                    (rplaca::safe-reload-result-status started)))
            (is-true
             (safe-reload-test-wait-until
              (lambda ()
                (bt:with-lock-held (completion-lock)
                  (not (null completion))))))
            (let ((result
                    (rplaca::apply-safe-reload-preflight-completion
                     (bt:with-lock-held (completion-lock) completion))))
              (is-true (rplaca:rplaca-reload-result-ok-p result)))
            (is-true preflight-called-p)
            (is-true live-called-p)
            (let ((texts (safe-reload-buffer-texts buffer)))
              (is (find-if (lambda (text)
                             (search "RPLACA safe reload started" text))
                           texts))
              (is (find-if (lambda (text)
                             (search "RPLACA safe reload succeeded" text))
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
          (let ((started (rplaca:safe-reload-rplaca-command buffer)))
            (is (eq :started
                    (rplaca::safe-reload-result-status started)))
            (is-true
             (safe-reload-test-wait-until
              (lambda ()
                (bt:with-lock-held (gate-lock) preflight-entered-p))))
            ;; This ordinary input command runs while the worker remains
            ;; blocked, proving the initiating frame/command owner returned.
            (let ((rplaca::*self-insert-char* #\r))
              (rplaca::self-insert-command buffer))
            (is (string= "r" (message-text (buffer-input-message buffer))))
            (is-false live-called-p)
            (is-true rplaca::*safe-reload-running-p*)
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
                    (rplaca::apply-safe-reload-preflight-completion
                     (bt:with-lock-held (completion-lock) completion))))
              (is-true (rplaca:rplaca-reload-result-ok-p result)))
            (is-true live-called-p)
            (is (eq frame-owner live-thread))
            (is-false rplaca::*safe-reload-running-p*)
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
                   (rplaca:call-with-runtime-admission
                    (lambda () (setf published-during-live-p t))
                    :operation "test live-overlap start")
                 (rplaca:runtime-admission-closed (condition)
                   (setf live-refusal condition)))
               (safe-reload-test-result :ok "Live applied.")))
          (let ((started (rplaca:safe-reload-rplaca-command buffer)))
            (is (eq :started
                    (rplaca::safe-reload-result-status started)))
            (handler-case
                (rplaca:call-with-runtime-admission
                 (lambda () (setf published-during-preflight-p t))
                 :operation "test preflight-overlap start")
              (rplaca:runtime-admission-closed (condition)
                (setf preflight-refusal condition)))
            (is-false published-during-preflight-p)
            (is (typep preflight-refusal
                       'rplaca:runtime-admission-closed))
            (is-true
             (safe-reload-test-wait-until
              (lambda ()
                (bt:with-lock-held (completion-lock)
                  (not (null completion))))))
            (let ((result
                    (rplaca::apply-safe-reload-preflight-completion
                     (bt:with-lock-held (completion-lock) completion))))
              (is-true (rplaca:rplaca-reload-result-ok-p result)))
            (is-false published-during-live-p)
            (is (typep live-refusal 'rplaca:runtime-admission-closed))
            (rplaca:call-with-runtime-admission
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
                       (rplaca::safe-reload-request-worker request)))
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
                  (rplaca::safe-reload-result-status
                   (rplaca:safe-reload-rplaca-command buffer))))
          (is-true
           (safe-reload-test-wait-until
            (lambda ()
              (bt:with-lock-held (completion-lock)
                (not (null completion))))))
          (is-true handoff-valid-p)
          (is-true
           (rplaca:rplaca-reload-result-ok-p
            (rplaca::apply-safe-reload-preflight-completion
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
          (let ((result (rplaca:safe-reload-rplaca-command buffer)))
            (is (eq :preflight-failed
                    (rplaca::safe-reload-result-status result)))
            (is (eq :worker-start
                    (rplaca::safe-reload-result-stage result)))
            (is-false preflight-called-p)
            (is-false rplaca::*safe-reload-running-p*)
            (is (find-if
                 (lambda (text)
                   (and (search "RPLACA safe reload failed" text)
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
          (let ((result (rplaca:safe-reload-rplaca-command buffer)))
            (is (eq :preflight-failed
                    (rplaca::safe-reload-result-status result)))
            (is (eq :worker-start
                    (rplaca::safe-reload-result-stage result)))
            (is (search "returned NIL"
                        (rplaca:rplaca-reload-result-summary result)))
            (is-false rplaca::*safe-reload-running-p*)
            (is (null rplaca::*safe-reload-active-request*))))))))

(test safe-reload-quiescence-covers-global-runtime-registries
  "OAuth, interop, and subagent registries all participate in quiescence."
  (let ((buffer (make-buffer "reload-registry-coverage"
                             :session-persistence-mode :ephemeral)))
    (with-safe-reload-quiescent-process (buffer)
      (is (null (rplaca::safe-reload-process-runtime-activity buffer)))
      (let ((flow (rplaca::make-openai-oauth-flow :buffer buffer)))
        (is-true (rplaca::publish-openai-oauth-pending-flow flow))
        (is (eq :oauth
                (rplaca::safe-reload-process-runtime-activity buffer)))
        (is (eq flow (rplaca::take-openai-oauth-pending-flow))))
      (let ((reservation (rplaca::reserve-message-help-runtime)))
        (unwind-protect
             (is (eq :help-frame
                     (rplaca::safe-reload-process-runtime-activity buffer)))
          (rplaca::release-message-help-runtime reservation)))
      (let ((thread (rplaca::make-interop-thread
                     :id "reload-active-thread"
                     :buffer buffer
                     :execution-owner "reload-active-turn")))
        (rplaca::register-interop-thread thread)
        (is (eq :interop
                (rplaca::safe-reload-process-runtime-activity buffer)))
        (is-true
         (rplaca::release-interop-thread-execution
          thread "reload-active-turn")))
      (let ((turn (rplaca::make-interop-turn
                   :id "reload-active-turn"
                   :thread-id "reload-active-thread"
                   :status :running
                   :runner-installed-p t)))
        (rplaca::register-interop-turn turn)
        (is (eq :interop
                (rplaca::safe-reload-process-runtime-activity buffer)))
        (rplaca::finalize-interop-turn turn :succeeded)
        ;; Terminal provider state precedes runner unwind; reload must still
        ;; refuse until that exact runner publishes final settlement.
        (is (eq :interop
                (rplaca::safe-reload-process-runtime-activity buffer)))
        (rplaca::finish-interop-turn-runner turn)
        (is (null (rplaca::safe-reload-process-runtime-activity buffer))))
      (let ((handle (rplaca::make-subagent-handle
                     :id "reload-active-subagent"
                     :status :running
                     :worker-finished-p nil)))
        (rplaca::register-subagent-handle handle)
        (is (eq :subagent
                (rplaca::safe-reload-process-runtime-activity buffer))))
      (clrhash rplaca::*subagent-handles*)
      (rplaca::call-with-synchronous-subagent-run-reservation
       (lambda ()
         (is (= 1 (rplaca:active-synchronous-subagent-run-count)))
         (is (eq :subagent
                 (rplaca::safe-reload-process-runtime-activity buffer)))))
      (is (= 0 (rplaca:active-synchronous-subagent-run-count))))))

(test safe-reload-refuses-terminal-oauth-until-worker-cleanup-settles
  "OAuth result publication cannot hide a callback worker still unwinding."
  (let* ((buffer (make-buffer "reload-oauth-cleanup-barrier"
                              :session-persistence-mode :ephemeral))
         (flow (rplaca::make-openai-oauth-flow :buffer buffer))
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
         (rplaca::*runtime-settlement-notify-function*
           (lambda (changed-buffer reason)
             (declare (ignore changed-buffer))
             (when (eq reason :oauth-settled)
               (bt:signal-semaphore settled-wake)))))
    ;; Keep the pending-flow cell process-global in this cross-thread test;
    ;; dynamically bound specials are intentionally not shared by BT workers.
    (let ((rplaca::*buffer-ring* (list buffer))
          (rplaca::*interop-thread-table* (make-hash-table :test #'equal))
          (rplaca::*interop-turn-table* (make-hash-table :test #'equal))
          (rplaca::*interop-runtime-operations* (make-hash-table :test #'eq))
          (rplaca::*subagent-handles* (make-hash-table :test #'equal))
          (rplaca::*synchronous-subagent-runs* (make-hash-table :test #'eq))
          (rplaca::*message-help-runtime-reservations*
            (make-hash-table :test #'eq)))
      (bt:with-lock-held (rplaca::*openai-oauth-pending-lock*)
        (setf rplaca::*openai-oauth-pending* nil))
      (unwind-protect
           (progn
             (setf worker
                   (bt:make-thread
                    (lambda ()
                      (rplaca::openai-oauth-flow-set-result
                       flow :success t :token "synthetic-token")
                      (bt:with-lock-held (gate-lock)
                        (setf cleanup-entered-p t)
                        (bt:condition-notify gate-condition)
                        (loop :until release-cleanup-p
                              :do (bt:condition-wait gate-condition
                                                     gate-lock))))
                    :name "safe-reload-oauth-cleanup-barrier"))
             (bt:with-lock-held ((rplaca::openai-oauth-flow-lock flow))
               (setf (rplaca::openai-oauth-flow-thread flow) worker))
             (is-true (rplaca::publish-openai-oauth-pending-flow flow))
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
               (let ((result (rplaca:rplaca-safe-reload
                              :buffer buffer :notify-p nil)))
                 (is (eq :refused
                         (rplaca::safe-reload-result-status result)))
                 (is-false preflight-called-p)))
             (setf pump
                   (bt:make-thread
                    (lambda ()
                      (when (bt:wait-on-semaphore settled-wake :timeout 5)
                        (setf automatic-result
                              (rplaca::update-openai-oauth-login buffer)))
                      (bt:signal-semaphore applied))
                    :name "simulated-clim-oauth-event-pump"))
             ;; The first frame update only installs the managed joiner.
             (is-true (rplaca::update-openai-oauth-login buffer))
             (is (eq flow (rplaca::openai-oauth-pending-flow)))
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
             (is (null (rplaca::openai-oauth-pending-flow)))
             (is (null (rplaca::openai-oauth-flow-thread flow)))
             (is (null
                  (rplaca::openai-oauth-flow-settlement-thread flow))))
        (bt:with-lock-held (gate-lock)
          (setf release-cleanup-p t)
          (bt:condition-notify gate-condition))
        (bt:signal-semaphore settled-wake)
        (when pump
          (bt:join-thread pump))
        (when worker
          (bt:join-thread worker))
        (rplaca::claim-openai-oauth-pending-flow flow)
        (bt:with-lock-held (rplaca::*openai-oauth-pending-lock*)
          (setf rplaca::*openai-oauth-pending* nil))))))

(test safe-reload-visible-failure-summary-is-concise
  "Visible transcript notifications do not include full compiler/process logs."
  (let* ((long-summary (format nil "Preflight reload failed: ~A~%~A"
                               (make-string 400 :initial-element #\x)
                               (make-string 400 :initial-element #\y)))
         (result (safe-reload-test-result :preflight-failed long-summary))
         (notification (rplaca::safe-reload-notification-text result)))
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
      (let ((result (rplaca:rplaca-safe-reload :source-root source-root
                                                   :notify-p nil)))
        (is-true (rplaca:rplaca-reload-result-ok-p result))
        (is (equal source-root preflight-source-root))
        (is (equal source-root live-source-root))
        (is (equal source-root
                   (rplaca::safe-reload-result-source-root result)))))))

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
      (let ((result (rplaca:rplaca-safe-reload :buffer buffer)))
        (is (eq :live-failed (rplaca::safe-reload-result-status result)))
        (is-false (rplaca:rplaca-reload-result-ok-p result))
        (is (search "boom during live reload"
                    (rplaca:rplaca-reload-result-summary result)))
        (is (find-if (lambda (text)
                       (search "RPLACA safe reload failed" text))
                     (safe-reload-buffer-texts buffer)))))))

(test safe-reload-overlap-returns-busy-without-running-preflight
  "The reload lock is nonblocking for overlapping requests."
  (let ((preflight-called-p nil))
    (is-true (bt:acquire-lock rplaca::*safe-reload-lock* nil))
    (unwind-protect
         (with-safe-reload-test-runners
             ((lambda (&key timeout source-root)
                (declare (ignore timeout source-root))
                (setf preflight-called-p t)
                (safe-reload-test-result :ok "Preflight ok."))
              (lambda (&key buffer source-root)
                (declare (ignore buffer source-root))
                (safe-reload-test-result :ok "Live ok.")))
           (let ((result (rplaca:rplaca-safe-reload :notify-p nil)))
             (is (eq :busy (rplaca::safe-reload-result-status result)))
             (is-false preflight-called-p)))
      (bt:release-lock rplaca::*safe-reload-lock*))))

(test safe-reload-is-a-user-command-not-a-provider-tool
  "Safe reload remains an explicit command and is absent from provider tools."
  (let ((rplaca::*tool-table* (make-hash-table :test #'equal)))
    (rplaca:init-tools)
    (is (null (gethash "rplaca_reload" rplaca::*tool-table*)))
    (is (fboundp 'rplaca:safe-reload-rplaca-command))))

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
      (let ((rplaca::*debug-log-file* path)
            (rplaca::*e2e-events-enabled-override* t)
            (rplaca::*debug-event-sequence* 0))
        (rplaca:rplaca-safe-reload :notify-p nil)))
    (let ((event (read-safe-reload-debug-event path "safe-reload-result")))
      (is (not (null event)))
      (is (string= "ok" (cdr (assoc :status event))))
      (is (search "Preflight and live reload completed"
                  (cdr (assoc :summary event)))))))

(test reload-active-packages-is-one-outer-rollback-transaction
  "Failure of package B restores package A and swaps complete registries."
  (let* ((a (make-package-definition
             :name "org.example.a" :root #P"/tmp/" :description "a"))
         (b (make-package-definition
             :name "org.example.b" :root #P"/tmp/" :description "b"))
         (original-table (make-hash-table :test #'equal))
         (rplaca::*tool-table* original-table)
         (appearance-state :before)
         (batch-events nil))
    (setf (gethash "before" original-table) :present)
    (let ((rplaca::*package-appearance-batch-begin-function*
            (lambda ()
              (push :begin batch-events)
              appearance-state))
          (rplaca::*package-appearance-batch-restore-function*
            (lambda (snapshot)
              (push :restore batch-events)
              (setf appearance-state snapshot)))
          (rplaca::*package-appearance-batch-end-function*
            (lambda (snapshot)
              (declare (ignore snapshot))
              (push :end batch-events))))
      (with-safe-reload-function-override
          (rplaca::active-package-names (&key buffer agent-name)
            (declare (ignore buffer agent-name))
            '("org.example.a" "org.example.b"))
        (with-safe-reload-function-override
            (rplaca:find-installed-package (name &key buffer)
              (declare (ignore buffer))
              (if (string= name "org.example.a") a b))
          (with-safe-reload-function-override
              (rplaca::reload-rplaca-package (definition)
                (if (string= (package-definition-name definition)
                             "org.example.a")
                    (progn
                      (setf (gethash "from-a" rplaca::*tool-table*)
                            :partial)
                      definition)
                    (error "package B failed")))
            (signals error
              (rplaca::%reload-active-packages))))))
    (is (eq :before appearance-state))
    (is (equal '(:begin :end) (nreverse batch-events)))
    (is (eq :present (gethash "before" rplaca::*tool-table*)))
    (is (null (gethash "from-a" rplaca::*tool-table*)))
    ;; Registry identity is intentionally not part of the package API.  Whole
    ;; table replacement prevents concurrent readers observing partial rebuild.
    (is-false (eq original-table rplaca::*tool-table*))))

(defun stage-safe-reload-package-appearance
    (definition &optional role-local-name)
  "Stage one complete reload declaration batch for DEFINITION."
  (let* ((owner (package-definition-name definition))
         (rplaca::*current-rplaca-package* owner)
         (rplaca::*current-package-resource-types* '(:appearance))
         (rplaca::*package-appearance-entrypoint-reload-p* t)
         (rplaca::*package-appearance-entrypoint-staging*
           (rplaca::begin-package-appearance-entrypoint-staging definition)))
    (when role-local-name
      (register-package-appearance-role
       (make-appearance-role-definition
        :id (list :package owner role-local-name)
        :kind :content)))
    (rplaca::commit-package-appearance-entrypoint-staging
     rplaca::*package-appearance-entrypoint-staging*)))

(test reload-active-packages-discards-deferred-appearance-before-late-failure
  "Package A cannot publish appearance state before package B succeeds."
  (let* ((a (make-package-definition
             :name "org.example.a" :root #P"/tmp/" :description "a"))
         (b (make-package-definition
             :name "org.example.b" :root #P"/tmp/" :description "b"))
         (rplaca::*package-appearance-declarations*
           (make-hash-table :test #'equal))
         (rplaca::*package-appearance-catalog*
           (make-classic-appearance-catalog))
         (before rplaca::*package-appearance-catalog*)
         (plans 0)
         (releases 0)
         (finalizations 0)
         (checkpoints 0)
         (restores 0)
         (rplaca::*appearance-package-frame-transition-planner*
           (lambda (frame catalog)
             (declare (ignore frame catalog))
             (incf plans)
             (list :status :ready)))
         (rplaca::*appearance-package-frame-transition-publisher*
           (lambda (reservation)
             (declare (ignore reservation))
             (incf releases)
             t))
         (rplaca::*appearance-package-frame-transition-finalizer*
           (lambda (token reservations commit rollback)
             (declare (ignore token reservations rollback))
             (incf finalizations)
             (funcall commit)
             t))
         (rplaca::*appearance-package-batch-checkpoint-function*
           (lambda (reservations)
             (declare (ignore reservations))
             (incf checkpoints)))
         (rplaca::*appearance-package-live-frame-provider*
           (lambda () (list :frame)))
         (rplaca::*package-appearance-batch-begin-function*
           (constantly nil))
         (rplaca::*package-appearance-batch-restore-function*
           (lambda (snapshot)
             (declare (ignore snapshot))
             (incf restores)))
         (rplaca::*package-appearance-batch-end-function*
           (lambda (snapshot) (declare (ignore snapshot)))))
    (with-safe-reload-function-override
        (rplaca::active-package-names (&key buffer agent-name)
          (declare (ignore buffer agent-name))
          '("org.example.a" "org.example.b"))
      (with-safe-reload-function-override
          (rplaca:find-installed-package (name &key buffer)
            (declare (ignore buffer))
            (if (string= name "org.example.a") a b))
        (with-safe-reload-function-override
            (rplaca::register-package-agent-tool-provider-definitions
                (owner)
              (declare (ignore owner))
              t)
          (with-safe-reload-function-override
              (rplaca::reload-rplaca-package (definition)
                (if (string= (package-definition-name definition)
                             "org.example.a")
                    (progn
                      (stage-safe-reload-package-appearance
                       definition "staged")
                      definition)
                    (error "package B failed")))
            (signals error
              (rplaca::%reload-active-packages))))))
    (is (eq before rplaca::*package-appearance-catalog*))
    (is (= 0
           (hash-table-count
            rplaca::*package-appearance-declarations*)))
    (is (= 0 plans))
    (is (= 0 releases))
    (is (= 0 finalizations))
    (is (= 0 checkpoints))
    (is (= 0 restores))))

(test reload-active-packages-publishes-one-combined-appearance-transition
  "All owners, including an empty removal, publish in one generation/frame transaction."
  (let* ((a (make-package-definition
             :name "org.example.a" :root #P"/tmp/" :description "a"))
         (b (make-package-definition
             :name "org.example.b" :root #P"/tmp/" :description "b"))
         (rplaca::*package-appearance-declarations*
           (make-hash-table :test #'equal))
         (rplaca::*package-appearance-catalog*
           (make-classic-appearance-catalog))
         (rplaca::*appearance-package-live-frame-provider*
           (constantly nil)))
    ;; Establish B's old declaration outside the deferred outer transaction.
    (stage-safe-reload-package-appearance b "removed")
    (let* ((before-generation
            (appearance-catalog-generation
             rplaca::*package-appearance-catalog*))
          (plans 0)
          (reservations 0)
          (releases 0)
          (finalizations 0)
          (checkpoints 0)
          (rplaca::*appearance-package-live-frame-provider*
            (lambda () (list :frame)))
          (rplaca::*appearance-package-frame-transition-planner*
            (lambda (frame catalog)
              (declare (ignore frame catalog))
              (incf plans)
              (list :status :ready)))
          (rplaca::*appearance-package-frame-transition-reserver*
            (lambda (frame plan catalog token)
              (incf reservations)
              (list frame plan catalog token)))
          (rplaca::*appearance-package-frame-transition-publisher*
            (lambda (reservation)
              (declare (ignore reservation))
              (incf releases)
              t))
          (rplaca::*appearance-package-frame-transition-finalizer*
            (lambda (token frame-reservations commit rollback)
              (declare (ignore frame-reservations rollback))
              (incf finalizations)
              (funcall commit)
              (setf
               (rplaca::appearance-package-transition-token-state token)
               :committed)
              t))
          (rplaca::*appearance-package-batch-checkpoint-function*
            (lambda (frame-reservations)
              (declare (ignore frame-reservations))
              (incf checkpoints)))
          (rplaca::*package-appearance-batch-begin-function*
            (constantly nil))
          (rplaca::*package-appearance-batch-restore-function*
            (lambda (snapshot) (declare (ignore snapshot))))
          (rplaca::*package-appearance-batch-end-function*
            (lambda (snapshot) (declare (ignore snapshot)))))
      (with-safe-reload-function-override
          (rplaca::active-package-names (&key buffer agent-name)
            (declare (ignore buffer agent-name))
            '("org.example.a" "org.example.b"))
        (with-safe-reload-function-override
            (rplaca:find-installed-package (name &key buffer)
              (declare (ignore buffer))
              (if (string= name "org.example.a") a b))
          (with-safe-reload-function-override
              (rplaca::register-package-agent-tool-provider-definitions
                  (owner)
                (declare (ignore owner))
                t)
            (with-safe-reload-function-override
                (rplaca::reload-rplaca-package (definition)
                  (stage-safe-reload-package-appearance
                   definition
                   (and (string= (package-definition-name definition)
                                 "org.example.a")
                        "replacement"))
                  definition)
              (is (= 2
                     (length
                      (rplaca::%reload-active-packages))))))))
      (is (= (1+ before-generation)
             (appearance-catalog-generation
              rplaca::*package-appearance-catalog*)))
      (is (find-appearance-role-definition
           rplaca::*package-appearance-catalog*
           '(:package "org.example.a" "replacement")))
      (is (null
           (find-appearance-role-definition
            rplaca::*package-appearance-catalog*
            '(:package "org.example.b" "removed"))))
      (is (gethash "org.example.a"
                   rplaca::*package-appearance-declarations*))
      (is (null
           (gethash "org.example.b"
                    rplaca::*package-appearance-declarations*)))
      (is (= 1 plans))
      (is (= 1 reservations))
      (is (= 1 releases))
      (is (= 1 finalizations))
      (is (= 1 checkpoints)))))

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
                                (let ((result (rplaca:rplaca-safe-reload
                                               :notify-p nil)))
                                  (bt:with-lock-held (results-lock)
                                    (push (rplaca::safe-reload-result-status result)
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
  (let* ((request (rplaca::make-safe-reload-request
                   :token (list :settlement-test (gensym "REQUEST-"))
                   :mode :test
                   :started-at (get-internal-real-time)
                   :notify-p nil))
         (ready (bt:make-semaphore :name "settlement waiters ready"))
         (completed (bt:make-semaphore :name "settlement waiters completed"))
         (threads nil))
    (bt:with-lock-held (rplaca::*safe-reload-lock*)
      (setf rplaca::*safe-reload-active-request* request
            rplaca::*safe-reload-running-p* t))
    (unwind-protect
         (progn
           (setf threads
                 (loop :for index :below 3
                       :collect
                       (bt:make-thread
                        (lambda ()
                          (bt:signal-semaphore ready)
                          (rplaca:call-with-runtime-settlement-admission
                           (lambda ()
                             (bt:signal-semaphore completed))
                           :operation "three-waiter settlement test"))
                        :name (format nil "settlement-waiter-~D" index))))
           (dotimes (_ 3)
             (declare (ignore _))
             (is-true (bt:wait-on-semaphore ready :timeout 2.0)))
           (is-false (bt:wait-on-semaphore completed :timeout 0.05))
           (is-true (rplaca::finish-safe-reload-request request))
           (dotimes (_ 3)
             (declare (ignore _))
             (is-true (bt:wait-on-semaphore completed :timeout 2.0)))
           (dolist (thread threads)
             (bt:join-thread thread)))
      (rplaca::finish-safe-reload-request request)
      (dolist (thread threads)
        (when (bt:thread-alive-p thread)
          (bt:join-thread thread))))))
