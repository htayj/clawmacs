(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Subagents
;;; --------------------------------------------------------------------------

(defvar *default-subagent-name* "subagent"
  "Default transient agent name used by RUN-SUBAGENT.")

(defvar *subagent-handle-counter* 0
  "Monotonic process-local counter used to generate subagent handle ids.")

(defvar *subagent-handles* (make-hash-table :test #'equal)
  "Process-local registry mapping subagent ids to SUBAGENT-HANDLE objects.")

(defparameter *subagent-terminal-history-limit* 64
  "Maximum number of settled subagent handles retained for status queries.")

(defvar *subagent-terminal-sequence-counter* 0
  "Monotonic sequence used to retain the most recently settled subagents.")

(defvar *synchronous-subagent-runs* (make-hash-table :test #'eq)
  "Exact reservations for direct synchronous RUN-SUBAGENT calls.")

(defvar *subagent-registry-lock* (bt:make-lock "subagent-registry")
  "Lock protecting subagent id generation and registry updates.")

(define-condition prompt-run-cancelled (condition)
  ()
  (:documentation "Internal control condition for cooperative prompt cancellation."))

(defstruct subagent-handle
  "Handle for a background subagent run."
  id
  prompt
  agent-name
  (status :running :type keyword)
  result
  error
  started-at
  finished-at
  thread
  (cancel-requested-p nil :type boolean)
  current-stream-state
  (worker-finished-p nil :type boolean)
  terminal-sequence
  (retained-resources-released-p nil :type boolean)
  (lock (bt:make-lock "subagent-handle")))

(defun next-subagent-id ()
  "Return a fresh process-local subagent id."
  (bt:with-lock-held (*subagent-registry-lock*)
    (format nil "subagent-~D" (incf *subagent-handle-counter*))))

(defun active-synchronous-subagent-run-count ()
  "Return the number of direct subagent runs visible to safe reload."
  (bt:with-lock-held (*subagent-registry-lock*)
    (hash-table-count *synchronous-subagent-runs*)))

(defun settle-synchronous-subagent-run (token)
  "Remove TOKEN only after its direct run has stopped executing project code."
  (when (bt:with-lock-held (*subagent-registry-lock*)
          (gethash token *synchronous-subagent-runs*))
    (call-with-runtime-settlement-admission
     (lambda ()
       (bt:with-lock-held (*subagent-registry-lock*)
         (remhash token *synchronous-subagent-runs*)))
     :operation "synchronous subagent settlement"))
  token)

(defun call-with-synchronous-subagent-run-reservation (function)
  "Call FUNCTION with one exact process-visible synchronous run reservation."
  (let ((token (cons :synchronous-subagent (gensym "RUN-"))))
    (unwind-protect
         (progn
           (call-with-runtime-admission
            (lambda ()
              (bt:with-lock-held (*subagent-registry-lock*)
                (setf (gethash token *synchronous-subagent-runs*) t)))
           :operation "a synchronous subagent run")
           (funcall function))
      (settle-synchronous-subagent-run token))))

(defun settled-subagent-handle-p (handle)
  "Return true when HANDLE is terminal and its worker has exited."
  (and (subagent-handle-worker-finished-p handle)
       (member (subagent-handle-status handle)
               '(:succeeded :failed :cancelled)
               :test #'eq)))

(defun note-subagent-terminal-sequence-locked (handle)
  "Assign HANDLE's terminal sequence with the registry lock already held."
  (bt:with-lock-held ((subagent-handle-lock handle))
    (when (and (settled-subagent-handle-p handle)
               (null (subagent-handle-terminal-sequence handle)))
      (setf (subagent-handle-terminal-sequence handle)
            (incf *subagent-terminal-sequence-counter*)))))

(defun release-subagent-retained-resources (handle)
  "Release heavy resources from an evicted, settled HANDLE."
  (bt:with-lock-held ((subagent-handle-lock handle))
    (when (settled-subagent-handle-p handle)
      (setf (subagent-handle-prompt handle) nil
            (subagent-handle-result handle) nil
            (subagent-handle-error handle) nil
            (subagent-handle-thread handle) nil
            (subagent-handle-current-stream-state handle) nil
            (subagent-handle-retained-resources-released-p handle) t)))
  handle)

(defun prune-subagent-terminal-history ()
  "Bound settled handle history and release resources held by evictions.

Running and cancelling handles are never candidates.  Registry removal is
atomic under the registry lock; resource release happens afterward so it does
not lengthen the registry critical section."
  (let ((evicted nil)
        (limit (max 0 *subagent-terminal-history-limit*)))
    (bt:with-lock-held (*subagent-registry-lock*)
      (let ((terminal-handles nil))
        (maphash
         (lambda (_id handle)
           (declare (ignore _id))
           (note-subagent-terminal-sequence-locked handle)
           (bt:with-lock-held ((subagent-handle-lock handle))
             (when (settled-subagent-handle-p handle)
               (push handle terminal-handles))))
         *subagent-handles*)
        (setf terminal-handles
              (sort terminal-handles #'>
                    :key (lambda (handle)
                           (or (subagent-handle-terminal-sequence handle) 0))))
        (dolist (handle (nthcdr limit terminal-handles))
          (when (eq handle
                    (gethash (subagent-handle-id handle) *subagent-handles*))
            (remhash (subagent-handle-id handle) *subagent-handles*)
            (push handle evicted)))))
    ;; No cancellation, stream close, or other external work runs under the
    ;; registry lock.
    (dolist (handle evicted)
      (release-subagent-retained-resources handle))
    (length evicted)))

(defun register-subagent-handle (handle)
  "Store HANDLE in the process-local subagent registry."
  (bt:with-lock-held (*subagent-registry-lock*)
    (setf (gethash (subagent-handle-id handle) *subagent-handles*) handle)
    (note-subagent-terminal-sequence-locked handle))
  (prune-subagent-terminal-history)
  handle)

(defun find-subagent (handle-or-id)
  "Return the subagent handle identified by HANDLE-OR-ID, or NIL."
  (cond
    ((subagent-handle-p handle-or-id) handle-or-id)
    (t
     (bt:with-lock-held (*subagent-registry-lock*)
       (gethash (princ-to-string handle-or-id) *subagent-handles*)))))

(defun list-subagents ()
  "Return process-local subagent handles sorted by start time."
  (let ((handles nil))
    (bt:with-lock-held (*subagent-registry-lock*)
      (maphash (lambda (id handle)
                 (declare (ignore id))
                 (push handle handles))
               *subagent-handles*))
    (sort handles #'< :key (lambda (handle)
                             (or (subagent-handle-started-at handle) 0)))))

(defun subagent-status (handle)
  "Return HANDLE's current status keyword."
  (let ((handle (or (find-subagent handle)
                    (error "Unknown subagent handle: ~S" handle))))
    (bt:with-lock-held ((subagent-handle-lock handle))
      (subagent-handle-status handle))))

(defun subagent-done-p (handle)
  "Return true when HANDLE has reached a terminal status and its worker exited."
  (let ((handle (or (find-subagent handle)
                    (error "Unknown subagent handle: ~S" handle))))
    (bt:with-lock-held ((subagent-handle-lock handle))
      (and (subagent-handle-worker-finished-p handle)
           (not (null (member (subagent-handle-status handle)
                              '(:succeeded :failed :cancelled))))))))

(defun subagent-result (handle)
  "Return HANDLE's prompt result when available."
  (let ((handle (or (find-subagent handle)
                    (error "Unknown subagent handle: ~S" handle))))
    (bt:with-lock-held ((subagent-handle-lock handle))
      (subagent-handle-result handle))))

(defun subagent-error (handle)
  "Return HANDLE's error text when the subagent failed."
  (let ((handle (or (find-subagent handle)
                    (error "Unknown subagent handle: ~S" handle))))
    (bt:with-lock-held ((subagent-handle-lock handle))
      (subagent-handle-error handle))))

(defun subagent-snapshot (handle)
  "Return an immutable plist snapshot of HANDLE's current state."
  (let ((handle (or (find-subagent handle)
                    (error "Unknown subagent handle: ~S" handle))))
    (bt:with-lock-held ((subagent-handle-lock handle))
      (list :id (subagent-handle-id handle)
            :prompt (subagent-handle-prompt handle)
            :agent-name (subagent-handle-agent-name handle)
            :status (subagent-handle-status handle)
            :done-p (and (subagent-handle-worker-finished-p handle)
                         (not (null (member (subagent-handle-status handle)
                                            '(:succeeded :failed :cancelled)))))
            :result (subagent-handle-result handle)
            :error (subagent-handle-error handle)
            :started-at (subagent-handle-started-at handle)
            :finished-at (subagent-handle-finished-at handle)
            :worker-finished-p (subagent-handle-worker-finished-p handle)
            :cancel-requested-p
            (subagent-handle-cancel-requested-p handle)))))

(defun complete-subagent-handle (handle status &key result error)
  "Record terminal STATUS on HANDLE unless cancellation already won."
  (bt:with-lock-held ((subagent-handle-lock handle))
    (unless (subagent-handle-cancel-requested-p handle)
      (setf (subagent-handle-status handle) status
            (subagent-handle-result handle) result
            (subagent-handle-error handle) error
            (subagent-handle-finished-at handle) (get-universal-time))))
  handle)

(defun update-subagent-stream-state (handle state)
  "Record STATE as HANDLE's active provider stream and honor early cancel."
  (let ((cancel-now-p nil))
    (bt:with-lock-held ((subagent-handle-lock handle))
      (setf (subagent-handle-current-stream-state handle) state
            cancel-now-p (subagent-handle-cancel-requested-p handle)))
    (when (and cancel-now-p state)
      (cancel-stream-state state :stop-reason "cancelled")))
  state)

(defun finish-subagent-worker-under-admission (handle)
  "Settle HANDLE while runtime settlement admission is already held."
  (bt:with-lock-held ((subagent-handle-lock handle))
    (setf (subagent-handle-current-stream-state handle) nil
          (subagent-handle-worker-finished-p handle) t)
    (when (subagent-handle-cancel-requested-p handle)
      (setf (subagent-handle-status handle) :cancelled
            (subagent-handle-result handle) nil
            (subagent-handle-error handle) nil
            (subagent-handle-finished-at handle) (get-universal-time)))
    (unless (subagent-handle-finished-at handle)
      (setf (subagent-handle-finished-at handle) (get-universal-time))))
  (bt:with-lock-held (*subagent-registry-lock*)
    (when (eq handle
              (gethash (subagent-handle-id handle) *subagent-handles*))
      (note-subagent-terminal-sequence-locked handle)))
  (prune-subagent-terminal-history)
  handle)

(defun finish-subagent-worker (handle)
  "Publish HANDLE's final worker settlement exactly once."
  (call-with-runtime-settlement-admission
   (lambda ()
     (finish-subagent-worker-under-admission handle))
   :operation "subagent worker settlement"))

(defun make-subagent-worker-thread (function name)
  "Create the background thread for one subagent run.

This small boundary keeps thread-creation failure deterministic in lifecycle
tests without replacing Bordeaux Threads process-wide."
  (bt:make-thread function :name name))

(defun cancel-subagent (handle)
  "Cooperatively cancel HANDLE.
The active provider stream is closed when available.  The handle remains
:CANCELLING until its worker has stopped, then settles as :CANCELLED."
  (let ((handle (or (find-subagent handle)
                    (error "Unknown subagent handle: ~S" handle)))
        (stream-state nil))
    (bt:with-lock-held ((subagent-handle-lock handle))
      (unless (member (subagent-handle-status handle)
                      '(:succeeded :failed :cancelled))
        (setf (subagent-handle-cancel-requested-p handle) t
              (subagent-handle-status handle) :cancelling
              stream-state (subagent-handle-current-stream-state handle))))
    (when stream-state
      (cancel-stream-state stream-state :stop-reason "cancelled"))
    handle))

(defun wait-subagent (handle &key timeout (poll-interval 0.05))
  "Wait for HANDLE to finish.
Returns values RESULT, STATUS, and HANDLE.  When TIMEOUT seconds elapse before
completion, returns NIL, :TIMEOUT, and HANDLE."
  (let* ((handle (or (find-subagent handle)
                     (error "Unknown subagent handle: ~S" handle)))
         (deadline (when timeout
                     (+ (get-internal-real-time)
                        (round (* timeout internal-time-units-per-second)))))
         (sleep-interval (max 0.001 poll-interval)))
    (loop
      (let ((snapshot (subagent-snapshot handle)))
        (when (getf snapshot :done-p)
          (return (values (getf snapshot :result)
                          (getf snapshot :status)
                          handle))))
      (when (and deadline
                 (>= (get-internal-real-time) deadline))
        (return (values nil :timeout handle)))
      (sleep sleep-interval))))

(defun normalize-run-custom-tools (custom-tools)
  "Normalize CUSTOM-TOOLS for a prompt run."
  (when custom-tools
    (normalize-subagent-tools custom-tools)))

(defun temporary-tool-table-from-definitions (definitions)
  "Build a temporary tool table from already normalized DEFINITIONS."
  (when definitions
    (let ((table (make-hash-table :test #'equal)))
      (dolist (definition definitions table)
        (setf (gethash (tool-definition-name definition) table)
              definition)))))

(defun resolve-prompt-tool-names (agent-name custom-tool-definitions
                                  tool-names tool-names-supplied-p)
  "Resolve the dynamic tool allowlist for a prompt or subagent run."
  (cond
    (tool-names-supplied-p
     (normalize-tool-name-list tool-names))
    (custom-tool-definitions
     (mapcar #'tool-definition-name custom-tool-definitions))
    (t
     (agent-definition-tool-names-for-name agent-name))))

(defun run-subagent (prompt &key (agent-name *default-subagent-name*)
                                  provider model think-level
                                  model-role service-tier
                                  core-prompt personality-prompt
                                  (working-directory
                                   (default-prompt-working-directory))
                                  (tool-names nil tool-names-supplied-p)
                                  custom-tools
                                  (max-tool-iterations *prompt-max-tool-iterations*)
                                  stream-state-callback
                                  cancel-requested-p)
  "Run PROMPT through a synchronous subagent and return a PROMPT-RUN-RESULT.
AGENT-NAME may name a registered agent. Explicit routing, prompt, and tool
arguments override the registered definition for this run only."
  (call-with-synchronous-subagent-run-reservation
   (lambda ()
     (let* ((effective-agent-name (or agent-name *default-subagent-name*))
            (name-key (normalize-agent-name-key effective-agent-name))
            (prompt-override nil)
            (run-args (list :agent-name effective-agent-name
                            :provider provider
                            :model model
                            :think-level think-level
                            :model-role model-role
                            :service-tier service-tier
                            :working-directory working-directory
                            :max-tool-iterations max-tool-iterations
                            :custom-tools custom-tools
                            :stream-state-callback stream-state-callback
                            :cancel-requested-p cancel-requested-p)))
       (when core-prompt
         (setf (getf prompt-override :core-prompt) core-prompt))
       (when personality-prompt
         (setf (getf prompt-override :personality-prompt)
               personality-prompt))
       (when tool-names-supplied-p
         (setf run-args (append run-args (list :tool-names tool-names))))
       (let ((*agent-prompt-overrides*
               (if prompt-override
                   (acons name-key prompt-override *agent-prompt-overrides*)
                   *agent-prompt-overrides*)))
         (apply #'run-single-prompt prompt run-args))))))

(defun run-subagent-async (prompt &key (agent-name *default-subagent-name*)
                                        provider model think-level
                                        model-role service-tier
                                        core-prompt personality-prompt
                                        (working-directory
                                         (default-prompt-working-directory))
                                        (tool-names nil tool-names-supplied-p)
                                        custom-tools
                                        (max-tool-iterations
                                         *prompt-max-tool-iterations*))
  "Run PROMPT in a background subagent and return a SUBAGENT-HANDLE."
  (when (blank-string-p prompt)
    (error "Prompt must be non-empty"))
  (let ((effective-agent-name (or agent-name *default-subagent-name*))
        (handle nil))
    ;; Publish package-contributed agent/tools before the async handle or its
    ;; worker becomes visible.  RUN-SINGLE-PROMPT keeps its worker-side load as
    ;; an idempotent warm-path check.
    (load-active-packages :agent-name effective-agent-name)
    (call-with-runtime-admission
     (lambda ()
       (setf handle
             (make-subagent-handle
              :id (next-subagent-id)
              :prompt prompt
              :agent-name effective-agent-name
              :status :running
              :started-at (get-universal-time)))
       (let ((run-args
               (list :agent-name effective-agent-name
                     :provider provider
                     :model model
                     :think-level think-level
                     :model-role model-role
                     :service-tier service-tier
                     :core-prompt core-prompt
                     :personality-prompt personality-prompt
                     :working-directory working-directory
                     :custom-tools custom-tools
                     :max-tool-iterations max-tool-iterations
                     :stream-state-callback
                     (lambda (state)
                       (update-subagent-stream-state handle state))
                     :cancel-requested-p
                     (lambda ()
                       (bt:with-lock-held ((subagent-handle-lock handle))
                         (subagent-handle-cancel-requested-p handle))))))
         (when tool-names-supplied-p
           (setf run-args (append run-args (list :tool-names tool-names))))
         (register-subagent-handle handle)
         (handler-case
             (let ((thread
                     (make-subagent-worker-thread
                      (lambda ()
                        (unwind-protect
                             (handler-case
                                 (let ((result
                                         (apply #'run-subagent prompt run-args)))
                                   (complete-subagent-handle
                                    handle :succeeded :result result))
                               (prompt-run-cancelled () nil)
                               (error (condition)
                                 (complete-subagent-handle
                                  handle
                                  :failed
                                  :error (format nil "~A" condition))))
                          (finish-subagent-worker handle)))
                      (format nil "clawmacs-~A"
                              (subagent-handle-id handle)))))
               ;; A very short worker can settle before MAKE-THREAD returns.
               ;; Do not reinstall its thread after history eviction.
               (bt:with-lock-held ((subagent-handle-lock handle))
                 (unless
                     (subagent-handle-retained-resources-released-p handle)
                   (setf (subagent-handle-thread handle) thread))))
           (error (condition)
             ;; The handle was made visible before thread creation.  Settle it
             ;; so status/list operations never observe :RUNNING forever.
             (complete-subagent-handle handle :failed
                                       :error (format nil "~A" condition))
             (finish-subagent-worker-under-admission handle)
             (error condition)))))
     :operation "an asynchronous subagent run")
    handle))
