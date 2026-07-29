(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Narrow pre-alpha path migration boundary
;;; --------------------------------------------------------------------------

(defstruct (legacy-path-selection
            (:constructor make-legacy-path-selection
                (read-path write-path source)))
  "One non-merging canonical/legacy path decision.

READ-PATH is NIL for executable legacy configuration, which must be migrated
manually. WRITE-PATH is always canonical. SOURCE is one of :CANONICAL,
:LEGACY, :ABSENT, :CONFLICT, or :LEGACY-EXECUTABLE."
  (read-path nil :type (or null pathname) :read-only t)
  (write-path #P"" :type pathname :read-only t)
  (source :absent :type keyword :read-only t))

(defvar *legacy-path-warning-lock*
  (bt:make-lock "clawmacs legacy path warnings"))

(defvar *legacy-path-selection-lock*
  (bt:make-lock "clawmacs legacy path selection"))

(defvar *legacy-path-warning-keys* (make-hash-table :test #'equal)
  "Process-local set of migration warnings already emitted.")

(defvar *legacy-path-warning-function*
  (lambda (control &rest arguments)
    (apply #'warn control arguments))
  "Function receiving a format control and arguments for migration warnings.")

(defvar *migration-path-probe-function* #'probe-file
  "Filesystem probe used by migration selection; dynamically replaced by tests.")

(defun reset-legacy-path-warnings ()
  "Forget process-local migration warning suppression.

This is public only as a deterministic test boundary."
  (bt:with-lock-held (*legacy-path-warning-lock*)
    (clrhash *legacy-path-warning-keys*))
  nil)

(defun emit-legacy-path-warning-once (key control &rest arguments)
  "Emit one migration warning identified by KEY."
  (let ((emit-p
          (bt:with-lock-held (*legacy-path-warning-lock*)
            (unless (gethash key *legacy-path-warning-keys*)
              (setf (gethash key *legacy-path-warning-keys*) t)
              t))))
    (when emit-p
      (apply *legacy-path-warning-function* control arguments)))
  nil)

(defun migration-path-present-p (path)
  "Return true when PATH names an existing filesystem object.

Probe errors deliberately propagate: an unreadable or malformed canonical path
must never be treated as permission to fall back to legacy state."
  (not (null (funcall *migration-path-probe-function* path))))

(defun select-migration-path
    (canonical legacy &key (label "configuration") executable-p)
  "Select one read path without merging CANONICAL and LEGACY.

CANONICAL always wins. If only LEGACY exists, inert data may be read from it
with an explicit warning, but all writes still target CANONICAL. Executable
legacy configuration is only detected and warned about; it is never selected
for automatic loading. When both paths exist, LEGACY is ignored rather than
merged."
  (let ((canonical-path (pathname canonical))
        (legacy-path (pathname legacy)))
    ;; This lock gives every in-process resolver one linearization boundary.
    ;; The final canonical probe below is the decision point for a legacy
    ;; fallback and catches canonical creation during the initial probes.
    (multiple-value-bind (selection warning)
        (bt:with-lock-held (*legacy-path-selection-lock*)
          (let* ((canonical-p (migration-path-present-p canonical-path))
                 (legacy-p (migration-path-present-p legacy-path))
                 (canonical-final-p
                   (or canonical-p
                       (and legacy-p
                            (migration-path-present-p canonical-path)))))
            (cond
              (canonical-final-p
               (values
                (make-legacy-path-selection canonical-path canonical-path
                                            (if legacy-p
                                                :conflict
                                                :canonical))
                (and legacy-p
                     (list
                      (list :conflict
                            (namestring canonical-path)
                            (namestring legacy-path))
                      "Both canonical ~A path ~A and legacy path ~A exist; using only the canonical path and not merging them."
                      label canonical-path legacy-path))))
              ((and legacy-p executable-p)
               (values
                (make-legacy-path-selection nil canonical-path
                                            :legacy-executable)
                (list
                 (list :legacy-executable (namestring legacy-path))
                 "Legacy executable ~A exists at ~A; it will not be loaded automatically. Migrate and review it for the RPLACA namespace before enabling it."
                 label legacy-path)))
              (legacy-p
               (values
                (make-legacy-path-selection legacy-path canonical-path :legacy)
                (list
                 (list :legacy-fallback (namestring legacy-path))
                 "Using legacy ~A at ~A as a read-only fallback; all writes target ~A."
                 label legacy-path canonical-path)))
              (t
               (values
                (make-legacy-path-selection canonical-path canonical-path
                                            :absent)
                nil)))))
      ;; Warning handlers and test adapters are arbitrary code. Never invoke
      ;; them while holding the non-recursive selection lock.
      (when warning
        (apply #'emit-legacy-path-warning-once warning))
      selection)))

(defun migration-read-path
    (canonical legacy &key (label "configuration") executable-p)
  "Return the sole selected read path for CANONICAL and LEGACY."
  (legacy-path-selection-read-path
   (select-migration-path canonical legacy
                          :label label
                          :executable-p executable-p)))

(defun migration-write-path (canonical)
  "Return CANONICAL as the unconditional write target."
  (pathname canonical))

(defun migration-read-roots
    (canonical legacy &key (label "directory") executable-p)
  "Return zero or one selected read root; canonical and legacy never merge."
  (let ((path (migration-read-path canonical legacy
                                   :label label
                                   :executable-p executable-p)))
    (and path (list path))))
