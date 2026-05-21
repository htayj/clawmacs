(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Safe in-place Clawmacs reload
;;; --------------------------------------------------------------------------

(defparameter *safe-reload-preflight-timeout* 60
  "Default timeout, in seconds, for the isolated safe reload preflight worker.")

(defparameter *safe-reload-worker-marker* "CLAWMACS-SAFE-RELOAD-RESULT "
  "Marker prefix used to locate the isolated reload worker's Lisp data result.")

(defparameter *safe-reload-visible-summary-limit* 240
  "Maximum characters of result summary shown in visible reload notifications.")

(defvar *safe-reload-lock* (bt:make-lock "clawmacs safe reload")
  "Process-local nonblocking lock guarding in-place Clawmacs reloads.")

(defvar *safe-reload-running-p* nil
  "True while the interactive safe reload command is visibly in progress.")

(defvar *safe-reload-preflight-function* nil
  "Function used by CLAWMACS-SAFE-RELOAD-PREFLIGHT.
The value is dynamically replaceable by tests.  NIL means use the built-in
isolated SBCL worker preflight implementation.")

(defvar *safe-reload-live-function* nil
  "Function used for the live in-place reload after preflight succeeds.
The value is dynamically replaceable by tests and is called with :BUFFER and
:SOURCE-ROOT.  NIL means use ASDF:LOAD-SYSTEM on the current image without
reinitializing Clawmacs runtime state.")

(defstruct safe-reload-result
  "Result object returned by Clawmacs safe reload entry points."
  status
  stage
  summary
  preflight
  live
  stdout
  stderr
  exit-code
  condition-type
  condition-message
  duration-seconds
  source-root)

(defun clawmacs-reload-result-ok-p (result)
  "Return true when RESULT represents a completed safe reload."
  (and (typep result 'safe-reload-result)
       (eq :ok (safe-reload-result-status result))))

(defun clawmacs-reload-result-summary (result)
  "Return the human-readable summary string for a safe reload RESULT."
  (if (typep result 'safe-reload-result)
      (or (safe-reload-result-summary result) "")
      (format nil "~A" result)))

(defun safe-reload-now-seconds ()
  "Return an internal real-time timestamp in fractional seconds."
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defun safe-reload-elapsed-seconds (started-at)
  "Return seconds elapsed since STARTED-AT."
  (- (safe-reload-now-seconds) started-at))

(defun safe-reload-normalize-timeout (timeout)
  "Return TIMEOUT as a positive integer number of seconds."
  (let ((value (cond
                 ((null timeout) *safe-reload-preflight-timeout*)
                 ((integerp timeout) timeout)
                 ((stringp timeout) (parse-integer timeout))
                 (t (error "Reload timeout must be an integer, got ~S" timeout)))))
    (max 1 value)))

(defun safe-reload-source-root (&optional source-root)
  "Return the Clawmacs source root used by worker and live reloads."
  (uiop:ensure-directory-pathname
   (truename
    (or source-root
        (asdf:system-source-directory :clawmacs)
        "."))))

(defun safe-reload-setup-candidates ()
  "Return candidate Lisp dependency setup files for reload workers."
  (remove nil
          (list (uiop:getenv "CLAWMACS_QUICKLISP_SETUP")
                (uiop:getenv "CLAWMACS_ULTRALISP_SETUP")
                (namestring (merge-pathnames #P"quicklisp/setup.lisp"
                                             (user-homedir-pathname))))))

(defun safe-reload-worker-setup-path ()
  "Return the first readable setup file for reload workers, or NIL."
  (dolist (candidate (safe-reload-setup-candidates))
    (unless (blank-string-p candidate)
      (let ((path (probe-file candidate)))
        (when path
          (return (namestring (truename path))))))))

(defun safe-reload-asd-path (source-root)
  "Return SOURCE-ROOT's clawmacs.asd path when present."
  (let ((path (merge-pathnames #P"clawmacs.asd"
                               (uiop:ensure-directory-pathname source-root))))
    (and (probe-file path) path)))

(defun safe-reload-load-asd (source-root)
  "Reload SOURCE-ROOT's clawmacs.asd when present."
  (let ((asd-path (safe-reload-asd-path source-root)))
    (when asd-path
      (asdf:load-asd asd-path))))

(defun safe-reload-worker-form (source-root marker)
  "Return the Lisp form evaluated by the isolated reload preflight worker."
  (format nil
          "(let ((*print-case* :downcase)~%      (*print-circle* t)~%      (*print-escape* t)~%      (*print-pretty* t)~%      (*print-readably* nil)~%      (exit-code 0))~%  (handler-case~%      (let* ((source-root (truename ~S))~%             (asd-path (merge-pathnames #P\"clawmacs.asd\" source-root)))~%        (pushnew source-root asdf:*central-registry* :test #'equal)~%        (when (probe-file asd-path)~%          (asdf:load-asd asd-path))~%        (let ((ql-package (find-package :ql)))~%          (when ql-package~%            (multiple-value-bind (quickload present-p)~%                (find-symbol \"QUICKLOAD\" ql-package)~%              (when (and present-p (fboundp quickload))~%                (funcall quickload :clawmacs :silent t)))))~%        (asdf:load-system :clawmacs :force t)~%        (format t \"~~&~A~~S~~%\"~%                (list :status :ok~%                      :summary \"Preflight reload succeeded.\")))~%    (error (condition)~%      (setf exit-code 1)~%      (format t \"~~&~A~~S~~%\"~%              (list :status :preflight-failed~%                    :summary (format nil \"Preflight reload failed: ~~A\" condition)~%                    :condition-type (format nil \"~~S\" (type-of condition))~%                    :condition-message (format nil \"~~A\" condition)))))~%  (uiop:quit exit-code))"
          (namestring source-root)
          marker
          marker))

(defun safe-reload-worker-command (source-root timeout)
  "Return the argv list for the isolated safe reload preflight worker."
  (let ((argv (list "timeout" (format nil "~D" timeout)
                    "sbcl" "--noinform" "--disable-debugger"
                    "--eval" "(require :asdf)"))
        (setup-path (safe-reload-worker-setup-path)))
    (when setup-path
      (setf argv (append argv (list "--load" setup-path))))
    (append argv
            (list "--eval" (safe-reload-worker-form
                             source-root
                             *safe-reload-worker-marker*)
                  "--eval" "(uiop:quit)"))))

(defun safe-reload-worker-payload (stdout)
  "Return the worker payload embedded in STDOUT, or NIL when absent."
  (let* ((marker *safe-reload-worker-marker*)
         (position (and stdout (search marker stdout :from-end t))))
    (when position
      (handler-case
          (lisp-data-read (subseq stdout (+ position (length marker))))
        (error () nil)))))

(defun safe-reload-payload-value (payload key)
  "Return KEY from worker PAYLOAD when it is a plist."
  (and (listp payload) (getf payload key)))

(defun safe-reload-worker-summary (payload stdout stderr exit-code)
  "Return a concise preflight summary for PAYLOAD and process outputs."
  (or (safe-reload-payload-value payload :summary)
      (safe-reload-payload-value payload :condition-message)
      (and (not (blank-string-p stderr))
           (format nil "Preflight reload failed: ~A"
                   (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (bounded-tool-execution-string stderr))))
      (and (not (blank-string-p stdout))
           (format nil "Preflight reload failed: ~A"
                   (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (bounded-tool-execution-string stdout))))
      (format nil "Preflight reload worker failed with exit code ~A."
              exit-code)))

(defun safe-reload-worker-result (payload stdout stderr exit-code duration source-root)
  "Convert worker process output to a SAFE-RELOAD-RESULT."
  (let* ((ok-p (and (eql exit-code 0)
                    (eq :ok (safe-reload-payload-value payload :status))))
         (status (if ok-p :ok :preflight-failed)))
    (make-safe-reload-result
     :status status
     :stage :preflight
     :summary (if ok-p
                  (or (safe-reload-payload-value payload :summary)
                      "Preflight reload succeeded.")
                  (safe-reload-worker-summary payload stdout stderr exit-code))
     :stdout (bounded-tool-execution-string stdout)
     :stderr (bounded-tool-execution-string stderr)
     :exit-code exit-code
     :condition-type (safe-reload-payload-value payload :condition-type)
     :condition-message (safe-reload-payload-value payload :condition-message)
     :duration-seconds duration
     :source-root source-root)))

(defun safe-reload-condition-result (status stage condition started-at
                                     &key preflight source-root)
  "Return a SAFE-RELOAD-RESULT describing CONDITION."
  (make-safe-reload-result
   :status status
   :stage stage
   :summary (format nil "~:(~A~) failed: ~A" stage condition)
   :preflight preflight
   :condition-type (condition-type-name condition)
   :condition-message (format nil "~A" condition)
   :duration-seconds (safe-reload-elapsed-seconds started-at)
   :source-root source-root))

(defun %safe-reload-run-preflight-worker (&key timeout source-root)
  "Run the isolated worker that proves Clawmacs can be loaded from SOURCE-ROOT."
  (let* ((started-at (safe-reload-now-seconds))
         (root (safe-reload-source-root source-root))
         (seconds (safe-reload-normalize-timeout timeout))
         (argv (safe-reload-worker-command root seconds)))
    (handler-case
        (multiple-value-bind (stdout stderr exit-code)
            (uiop:run-program argv
                              :directory root
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (safe-reload-worker-result
           (safe-reload-worker-payload stdout)
           stdout
           stderr
           exit-code
           (safe-reload-elapsed-seconds started-at)
           root))
      (error (condition)
        (safe-reload-condition-result :preflight-failed
                                      :preflight
                                      condition
                                      started-at
                                      :source-root root)))))

(defun safe-reload-refresh-runtime-registrations (buffer)
  "Refresh non-destructive runtime registrations after a live source reload."
  ;; Do not call INITIALIZE-CLAWMACS-RUNTIME, RESET-INTERACTION-STATE, or
  ;; CLAWMACS-MAIN here.  These targeted refreshes make reloaded commands,
  ;; tools, keybindings, and package entrypoints visible while preserving the
  ;; user's buffers, sessions, frame, and compose draft.
  (when (fboundp 'init-tools)
    (funcall 'init-tools))
  (when (fboundp 'install-chat-frame-keybindings)
    (funcall 'install-chat-frame-keybindings))
  (when (fboundp 'reload-package-channels)
    (funcall 'reload-package-channels))
  (when (fboundp 'reload-active-packages)
    (funcall 'reload-active-packages :buffer buffer)))

(defun %safe-reload-live-reload (&key buffer source-root)
  "Reload Clawmacs in the current image without resetting application state."
  (let ((started-at (safe-reload-now-seconds))
        (stdout (make-string-output-stream))
        (stderr (make-string-output-stream))
        (root (safe-reload-source-root source-root)))
    (handler-case
        (let ((*standard-output* stdout)
              (*trace-output* stderr)
              (*error-output* stderr))
          (safe-reload-load-asd root)
          (asdf:load-system :clawmacs :force t)
          (safe-reload-refresh-runtime-registrations buffer)
          (make-safe-reload-result
           :status :ok
           :stage :live
           :summary "Live reload succeeded."
           :stdout (bounded-tool-execution-string
                    (get-output-stream-string stdout))
           :stderr (bounded-tool-execution-string
                    (get-output-stream-string stderr))
           :duration-seconds (safe-reload-elapsed-seconds started-at)
           :source-root root))
      (error (condition)
        (let ((result (safe-reload-condition-result :live-failed
                                                    :live
                                                    condition
                                                    started-at
                                                    :source-root root)))
          (setf (safe-reload-result-stdout result)
                (bounded-tool-execution-string
                 (get-output-stream-string stdout))
                (safe-reload-result-stderr result)
                (bounded-tool-execution-string
                 (get-output-stream-string stderr)))
          result)))))

(defun safe-reload-preflight-function ()
  "Return the effective preflight function."
  (or *safe-reload-preflight-function*
      #'%safe-reload-run-preflight-worker))

(defun safe-reload-live-function ()
  "Return the effective test override live reload function, if any."
  *safe-reload-live-function*)

(defun call-safe-reload-live-function (buffer source-root)
  "Run the effective live reload function for BUFFER and SOURCE-ROOT."
  (if *safe-reload-live-function*
      (funcall *safe-reload-live-function*
               :buffer buffer
               :source-root source-root)
      (%safe-reload-live-reload :buffer buffer :source-root source-root)))

(defun clawmacs-safe-reload-preflight (&key timeout source-root)
  "Preflight a Clawmacs reload in an isolated worker and return a result.
The current Lisp image is not mutated by this function."
  (let ((started-at (safe-reload-now-seconds)))
    (handler-case
        (funcall (safe-reload-preflight-function)
                 :timeout (safe-reload-normalize-timeout timeout)
                 :source-root source-root)
      (error (condition)
        (safe-reload-condition-result :preflight-failed
                                      :preflight
                                      condition
                                      started-at
                                      :source-root source-root)))))

(defun safe-reload-result-plist (result)
  "Return RESULT as Lisp data suitable for tools and debug summaries."
  (when (typep result 'safe-reload-result)
    (append
     (list :status (safe-reload-result-status result)
           :ok (clawmacs-reload-result-ok-p result)
           :stage (safe-reload-result-stage result)
           :summary (safe-reload-result-summary result))
     (when (safe-reload-result-duration-seconds result)
       (list :duration-seconds (safe-reload-result-duration-seconds result)))
     (when (safe-reload-result-exit-code result)
       (list :exit-code (safe-reload-result-exit-code result)))
     (when (safe-reload-result-condition-type result)
       (list :condition-type (safe-reload-result-condition-type result)))
     (when (safe-reload-result-condition-message result)
       (list :condition-message (safe-reload-result-condition-message result)))
     (when (safe-reload-result-source-root result)
       (list :source-root (namestring (safe-reload-result-source-root result))))
     (when (and (safe-reload-result-stdout result)
                (not (blank-string-p (safe-reload-result-stdout result))))
       (list :stdout (safe-reload-result-stdout result)))
     (when (and (safe-reload-result-stderr result)
                (not (blank-string-p (safe-reload-result-stderr result))))
       (list :stderr (safe-reload-result-stderr result)))
     (when (safe-reload-result-preflight result)
       (list :preflight
             (safe-reload-result-plist
              (safe-reload-result-preflight result))))
     (when (safe-reload-result-live result)
       (list :live
             (safe-reload-result-plist
              (safe-reload-result-live result)))))))

(defun emit-safe-reload-result-event (result)
  "Emit the structured safe-reload-result debug event for RESULT."
  (ignore-errors
    (apply #'file-debug-event
           "safe-reload-result"
           (safe-reload-result-plist result))))

(defun safe-reload-start-notification-text ()
  "Return the visible notification shown before reload preflight starts."
  "[Clawmacs safe reload started: preflighting updated source...]")

(defun notify-safe-reload-started (buffer)
  "Insert a visible start notification for BUFFER."
  (when buffer
    (buffer-insert-system-message buffer (safe-reload-start-notification-text))))

(defun safe-reload-current-chat-frame ()
  "Return the currently active chat frame, when running under McCLIM."
  (let ((class (find-class 'clawmacs-chat-frame nil))
        (frame clim:*application-frame*))
    (and class (typep frame class) frame)))

(defun redisplay-safe-reload-status-now ()
  "Synchronously redisplay the current chat frame after a reload status change."
  (let ((frame (safe-reload-current-chat-frame)))
    (when (and frame (fboundp 'handle-chat-frame-redisplay))
      (ignore-errors
        (funcall 'handle-chat-frame-redisplay frame)))))

(defun safe-reload-one-line-summary (summary)
  "Return SUMMARY collapsed to one display line."
  (with-output-to-string (out)
    (let ((space-p nil))
      (loop :for char :across (or summary "")
            :do (cond
                  ((find char '(#\Space #\Tab #\Newline #\Return) :test #'char=)
                   (unless space-p
                     (write-char #\Space out)
                     (setf space-p t)))
                  (t
                   (write-char char out)
                   (setf space-p nil)))))))

(defun safe-reload-visible-summary (result)
  "Return RESULT summary text suitable for the transcript surface."
  (let* ((summary (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (safe-reload-one-line-summary
                                (clawmacs-reload-result-summary result))))
         (limit *safe-reload-visible-summary-limit*))
    (cond
      ((<= (length summary) limit)
       summary)
      (t
       (format nil "~A... see tool result/debug log for details."
               (subseq summary 0 limit))))))

(defun safe-reload-notification-text (result)
  "Return the visible buffer notification for RESULT."
  (let ((summary (safe-reload-visible-summary result)))
    (case (and (typep result 'safe-reload-result)
               (safe-reload-result-status result))
      (:ok
       (format nil "[Clawmacs safe reload succeeded: ~A]" summary))
      (:busy
       (format nil "[Clawmacs safe reload busy: ~A]" summary))
      (:preflight-failed
       (format nil "[Clawmacs safe reload failed during preflight: ~A]" summary))
      (:live-failed
       (format nil "[Clawmacs safe reload failed during live reload: ~A]" summary))
      (t
       (format nil "[Clawmacs safe reload failed: ~A]" summary)))))

(defun maybe-notify-safe-reload-buffer (buffer result notify-p)
  "Insert RESULT as a visible system message when requested."
  (when (and notify-p buffer)
    (buffer-insert-system-message buffer (safe-reload-notification-text result)))
  result)

(defun finalize-safe-reload-result (result buffer notify-p)
  "Emit debug and visible notifications for RESULT, then return it."
  (emit-safe-reload-result-event result)
  (maybe-notify-safe-reload-buffer buffer result notify-p))

(defun safe-reload-busy-result ()
  "Return a result for an overlapping reload request."
  (make-safe-reload-result
   :status :busy
   :stage :lock
   :summary "Another Clawmacs safe reload is already running."))

(defun safe-reload-complete-result (preflight live started-at)
  "Return the final success result combining PREFLIGHT and LIVE phases."
  (make-safe-reload-result
   :status :ok
   :stage :complete
   :summary "Preflight and live reload completed."
   :preflight preflight
   :live live
   :duration-seconds (safe-reload-elapsed-seconds started-at)
   :source-root (or (safe-reload-result-source-root live)
                    (safe-reload-result-source-root preflight))))

(defun safe-reload-live-failed-result (live preflight)
  "Attach PREFLIGHT to LIVE failure result and return it."
  (when (and (typep live 'safe-reload-result)
             (null (safe-reload-result-preflight live)))
    (setf (safe-reload-result-preflight live) preflight))
  live)

(defun run-safe-reload-under-lock (buffer timeout source-root notify-p started-at)
  "Run preflight and, if safe, the live reload while the reload lock is held."
  (let ((preflight (clawmacs-safe-reload-preflight
                    :timeout timeout
                    :source-root source-root)))
    (if (clawmacs-reload-result-ok-p preflight)
        (let ((live (handler-case
                        (call-safe-reload-live-function buffer source-root)
                      (error (condition)
                        (safe-reload-condition-result :live-failed
                                                      :live
                                                      condition
                                                      started-at
                                                      :preflight preflight
                                                      :source-root source-root)))))
          (finalize-safe-reload-result
           (if (clawmacs-reload-result-ok-p live)
               (safe-reload-complete-result preflight live started-at)
               (safe-reload-live-failed-result live preflight))
           buffer
           notify-p))
        (finalize-safe-reload-result preflight buffer notify-p))))

(defun safe-reload-try-acquire-lock ()
  "Try to acquire the reload lock without blocking; return NIL on overlap."
  (handler-case
      (bt:acquire-lock *safe-reload-lock* nil)
    (error () nil)))

(defun clawmacs-safe-reload (&key buffer timeout source-root (notify-p t))
  "Safely reload updated Clawmacs source in-place.
An isolated worker first proves that Clawmacs can load from SOURCE-ROOT.  The
live image is reloaded only when that preflight returns :OK.  All preflight,
busy, and live-reload failures are returned as SAFE-RELOAD-RESULT objects rather
than signaled to the caller."
  (let ((started-at (safe-reload-now-seconds)))
    (if (safe-reload-try-acquire-lock)
        (unwind-protect
             (run-safe-reload-under-lock buffer timeout source-root notify-p started-at)
          (bt:release-lock *safe-reload-lock*))
        (finalize-safe-reload-result (safe-reload-busy-result)
                                     buffer
                                     notify-p))))

(defun safe-reload-clawmacs-command (buffer)
  "Safely reload updated Clawmacs source from an interactive command."
  (let ((*safe-reload-running-p* t))
    (notify-safe-reload-started buffer)
    (redisplay-safe-reload-status-now)
    (clawmacs-safe-reload :buffer buffer)))
(defcommand safe-reload-clawmacs-command)

(defun execute-clawmacs-reload (args)
  "Provider tool entry point for requesting a safe Clawmacs reload."
  (let* ((timeout (or (tool-arg args :timeout "timeout")
                      *safe-reload-preflight-timeout*))
         (buffer (or *current-tool-buffer*
                     (ignore-errors (current-buffer))))
         (result (clawmacs-safe-reload :buffer buffer
                                       :timeout timeout)))
    (lisp-data-string (safe-reload-result-plist result))))

(deftool execute-clawmacs-reload
  :name "clawmacs_reload"
  :description "Safely reload updated Clawmacs source in place. Runs an isolated preflight worker first; only on preflight success does the live image reload. Returns a Lisp data status payload and inserts a visible system notification in the active buffer."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((timeout :type "integer"
                  :required nil
                  :description "Optional isolated preflight timeout in seconds. Default: 60.")))
