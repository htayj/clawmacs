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

(defvar *subagent-registry-lock* (bt:make-lock "subagent-registry")
  "Lock protecting subagent id generation and registry updates.")

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
  (lock (bt:make-lock "subagent-handle")))

(defun next-subagent-id ()
  "Return a fresh process-local subagent id."
  (bt:with-lock-held (*subagent-registry-lock*)
    (format nil "subagent-~D" (incf *subagent-handle-counter*))))

(defun register-subagent-handle (handle)
  "Store HANDLE in the process-local subagent registry."
  (bt:with-lock-held (*subagent-registry-lock*)
    (setf (gethash (subagent-handle-id handle) *subagent-handles*) handle))
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
  "Return true when HANDLE has reached a terminal status."
  (not (null (member (subagent-status handle)
                     '(:succeeded :failed :cancelled)))))

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
            :done-p (not (null (member (subagent-handle-status handle)
                                       '(:succeeded :failed :cancelled))))
            :result (subagent-handle-result handle)
            :error (subagent-handle-error handle)
            :started-at (subagent-handle-started-at handle)
            :finished-at (subagent-handle-finished-at handle)
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

(defun cancel-subagent (handle)
  "Cooperatively cancel HANDLE.
The provider request may continue in the background, but late completion will
not overwrite the public cancelled status or result."
  (let ((handle (or (find-subagent handle)
                    (error "Unknown subagent handle: ~S" handle))))
    (bt:with-lock-held ((subagent-handle-lock handle))
      (unless (member (subagent-handle-status handle)
                      '(:succeeded :failed :cancelled))
        (setf (subagent-handle-cancel-requested-p handle) t
              (subagent-handle-status handle) :cancelled
              (subagent-handle-finished-at handle) (get-universal-time))))
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
      (let ((status (subagent-status handle)))
        (when (member status '(:succeeded :failed :cancelled))
          (return (values (subagent-result handle) status handle))))
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
                                  (tool-names nil tool-names-supplied-p)
                                  custom-tools
                                  (max-tool-iterations *prompt-max-tool-iterations*)
                                  auto-approve-tools-p)
  "Run PROMPT through a synchronous subagent and return a PROMPT-RUN-RESULT.
AGENT-NAME may name a registered agent. Explicit routing, prompt, and tool
arguments override the registered definition for this run only."
  (let* ((effective-agent-name (or agent-name *default-subagent-name*))
         (name-key (normalize-agent-name-key effective-agent-name))
         (prompt-override nil)
         (run-args (list :agent-name effective-agent-name
                         :provider provider
                         :model model
                         :think-level think-level
                         :model-role model-role
                         :service-tier service-tier
                         :max-tool-iterations max-tool-iterations
                         :auto-approve-tools-p auto-approve-tools-p
                         :custom-tools custom-tools)))
    (when core-prompt
      (setf (getf prompt-override :core-prompt) core-prompt))
    (when personality-prompt
      (setf (getf prompt-override :personality-prompt) personality-prompt))
    (when tool-names-supplied-p
      (setf run-args (append run-args (list :tool-names tool-names))))
    (let ((*agent-prompt-overrides*
            (if prompt-override
                (acons name-key prompt-override *agent-prompt-overrides*)
                *agent-prompt-overrides*)))
      (apply #'run-single-prompt prompt run-args))))

(defun run-subagent-async (prompt &key (agent-name *default-subagent-name*)
                                        provider model think-level
                                        model-role service-tier
                                        core-prompt personality-prompt
                                        (tool-names nil tool-names-supplied-p)
                                        custom-tools
                                        (max-tool-iterations
                                         *prompt-max-tool-iterations*)
                                        auto-approve-tools-p)
  "Run PROMPT in a background subagent and return a SUBAGENT-HANDLE."
  (when (blank-string-p prompt)
    (error "Prompt must be non-empty"))
  (let* ((effective-agent-name (or agent-name *default-subagent-name*))
         (handle (make-subagent-handle
                  :id (next-subagent-id)
                  :prompt prompt
                  :agent-name effective-agent-name
                  :status :running
                  :started-at (get-universal-time)))
         (run-args (list :agent-name effective-agent-name
                         :provider provider
                         :model model
                         :think-level think-level
                         :model-role model-role
                         :service-tier service-tier
                         :core-prompt core-prompt
                         :personality-prompt personality-prompt
                         :custom-tools custom-tools
                         :max-tool-iterations max-tool-iterations
                         :auto-approve-tools-p auto-approve-tools-p)))
    (when tool-names-supplied-p
      (setf run-args (append run-args (list :tool-names tool-names))))
    (register-subagent-handle handle)
    (setf (subagent-handle-thread handle)
          (bt:make-thread
           (lambda ()
             (handler-case
                 (let ((result (apply #'run-subagent prompt run-args)))
                   (complete-subagent-handle handle :succeeded
                                             :result result))
               (error (condition)
                 (complete-subagent-handle
                  handle
                  :failed
                  :error (format nil "~A" condition)))))
           :name (format nil "clawmacs-~A"
                         (subagent-handle-id handle))))
    handle))
