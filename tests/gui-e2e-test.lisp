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

(defparameter *listener-gui-e2e-provider-mode* :success)
(defparameter *listener-gui-e2e-provider-delay-seconds* 0.18)
(defvar *listener-gui-e2e-controller-phase* nil)
(defvar *listener-gui-e2e-controller-observations* nil)
(defvar *listener-gui-e2e-prompt-count* 0)
(defvar *listener-gui-e2e-beep-count* 0)
(defvar *listener-gui-e2e-original-prompt-function* nil)
(defvar *listener-gui-e2e-original-beep-function* nil)

(defun listener-gui-e2e-dispatch-cases ()
  (list (list :name :eval :input "(+ 20 22)" :transport :grafted-editor)
        (list :name :package :input ",Package KEYWORD"
              :transport :programmatic-frame-command)
        (list :name :enter-say :input "!" :transport :grafted-editor)
        (list :name :exit-say :input "," :transport :grafted-editor)
        (list :name :interpolation :input "answer ,(+ 20 22)"
              :transport :programmatic-frame-command)
        (list :name :shell :input "#!printf RPLACA_E2E_SHELL"
              :transport :grafted-editor)
        (list :name :no-op :input "   " :transport :grafted-editor)
        (list :name :leading-space :input " !escaped"
              :transport :grafted-editor)))

(defun listener-gui-e2e-artifact-path (name)
  (merge-pathnames name
                   (uiop:ensure-directory-pathname
                    (or (uiop:getenv "RPLACA_GUI_E2E_ARTIFACT_DIR")
                        "/tmp/"))))

(defun listener-gui-e2e-write-evidence (name lines)
  (let ((path (listener-gui-e2e-artifact-path name)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
      (dolist (line lines)
        (write-line line stream)))
    path))

(defun listener-gui-e2e-event (name &rest fields)
  (apply #'rplaca::file-debug-event name fields))

(defun listener-gui-e2e-provider-double
    (mode messages callback &rest args &key network-attempt-function
                                      &allow-other-keys)
  (declare (ignore args network-attempt-function))
  (ecase mode
    (:failure
     (listener-gui-e2e-event "listener-provider-seam"
                             :mode "failure" :network-attempts 0)
     (let ((state (rplaca::make-stream-state :callback callback)))
       (rplaca::start-stream-state-reader-worker
        state callback "rplaca-listener-e2e-failing-provider"
        (lambda (worker-state)
          (sleep 0.08)
          (when (rplaca::transition-stream-state-to-terminal
                 worker-state :error "deterministic provider seam failure")
            (rplaca::maybe-call-streaming-callback
             callback worker-state :terminal t))))
       state))
    (:success
     (let* ((text (if (search "hello" (string-downcase
                                        (rplaca::e2e-message-text messages)))
                      "RPLACA_E2E_HELLO_SENTINEL: listener slow result."
                      "RPLACA_E2E_SENTINEL: listener slow result."))
            (state (rplaca::make-stream-state :callback callback)))
       (listener-gui-e2e-event "listener-provider-seam"
                               :mode "success" :network-attempts 0)
       (rplaca::start-stream-state-reader-worker
        state callback "rplaca-listener-e2e-provider"
        (lambda (worker-state)
          (dolist (chunk (rplaca::e2e-response-chunks text))
            (sleep *listener-gui-e2e-provider-delay-seconds*)
            (unless (rplaca::call-with-active-stream-state
                     worker-state
                     (lambda (locked-state)
                       (setf (rplaca::stream-state-text locked-state)
                             (concatenate
                              'string
                              (rplaca::stream-state-text locked-state)
                              chunk))
                       (rplaca::set-stream-state-text-block
                        locked-state
                        (rplaca::stream-state-text locked-state))))
              (return))
            (rplaca::maybe-call-streaming-callback callback worker-state))
          (when (rplaca::transition-stream-state-to-terminal
                 worker-state :stop-reason "end_turn"
                 :update
                 (lambda (locked-state)
                   (rplaca::set-stream-state-text-block locked-state text)
                   (setf (rplaca::stream-state-usage locked-state)
                         (rplaca::e2e-token-usage text))))
            (listener-gui-e2e-event
             "listener-provider-complete" :network-attempts 0)
            (rplaca::maybe-call-streaming-callback
             callback worker-state :terminal t))))
       state))))

(defun listener-gui-e2e-provider-request-streaming
    (provider messages callback &rest args &key &allow-other-keys)
  (if (eq provider :e2e)
      (apply #'listener-gui-e2e-provider-double
             *listener-gui-e2e-provider-mode* messages callback args)
      (error "Listener E2E attempted a non-fixture provider: ~S" provider)))

(defun listener-gui-e2e-prompt (stream frame)
  (incf *listener-gui-e2e-prompt-count*)
  (funcall *listener-gui-e2e-original-prompt-function* stream frame))

(defun listener-gui-e2e-beep (&optional medium)
  (incf *listener-gui-e2e-beep-count*)
  (funcall *listener-gui-e2e-original-beep-function* medium))

(defun listener-gui-e2e-accept-token (frame input &key mode)
  (when mode
    (setf (rplaca::rplaca-listener-context frame)
          (rplaca::listener-context-set-input-mode
           (rplaca::rplaca-listener-context frame) mode)))
  (let ((clim:*application-frame* frame))
    (clim:accept-from-string 'rplaca::command-form-or-prose input)))

(defun listener-gui-e2e-token-kind (frame input &key mode)
  (rplaca::listener-input-token-kind
   (listener-gui-e2e-accept-token frame input :mode mode)))

(defun listener-gui-e2e-require (condition format-control &rest args)
  (unless condition
    (error (apply #'format nil format-control args))))

(defun listener-gui-e2e-wholine (frame)
  (with-output-to-string (stream)
    (rplaca::display-listener-wholine frame stream)))

(defun listener-gui-e2e-layout-transition (function)
  (handler-case (funcall function)
    (clim-internals::frame-layout-changed () nil)))

(defun listener-gui-e2e-snapshot (frame scenario &rest fields)
  (let* ((context (rplaca::rplaca-listener-context frame))
         (buffer (rplaca::rplaca-listener-conversation-buffer frame))
         (turn (rplaca::rplaca-listener-pending-assistant-turn frame)))
    (apply #'listener-gui-e2e-event
           "ui-snapshot"
           :scenario scenario
           :frame "rplaca-listener"
           :layout (string-downcase
                    (symbol-name (clim:frame-current-layout frame)))
           :package (rplaca::listener-context-package-name context)
           :directory (namestring (rplaca::buffer-working-directory buffer))
           :prompt-count *listener-gui-e2e-prompt-count*
           :wholine (listener-gui-e2e-wholine frame)
           :turn-status (and turn
                             (string-downcase
                              (symbol-name
                               (rplaca::assistant-turn-status turn))))
           :turn-text (and turn (rplaca::assistant-turn-primary-text turn))
           fields)))

(defun listener-gui-e2e-run-dispatch (frame)
  (listener-gui-e2e-require
   (eq :form (listener-gui-e2e-token-kind frame "(+ 20 22)" :mode :eval))
   "eval dispatch failed")
  (let ((clim:*application-frame* frame))
    (rplaca::com-eval '(+ 20 22) "(+ 20 22)")
    (rplaca::com-package (find-package :keyword)))
  (listener-gui-e2e-require
   (string= "KEYWORD"
            (rplaca::listener-context-package-name
             (rplaca::rplaca-listener-context frame)))
   "package command failed")
  (listener-gui-e2e-require
   (eq :enter-say (listener-gui-e2e-token-kind frame "!" :mode :eval))
   "enter-say dispatch failed")
  (let ((clim:*application-frame* frame))
    (rplaca::com-set-input-mode :say))
  (listener-gui-e2e-require
   (eq :exit-say (listener-gui-e2e-token-kind frame "," :mode :say))
   "exit-say dispatch failed")
  (let ((clim:*application-frame* frame))
    (rplaca::com-set-input-mode :eval))
  (listener-gui-e2e-require
   (string= "answer 42"
            (rplaca:expand-prose-interpolations
             "answer ,(+ 20 22)" (find-package :cl-user) #'eval))
   "interpolation dispatch failed")
  (let ((clim:*application-frame* frame))
    (rplaca::com-run-shell "printf RPLACA_E2E_SHELL")
    (rplaca::com-no-op))
  (listener-gui-e2e-require
   (eq :form (listener-gui-e2e-token-kind frame " !escaped" :mode :eval))
   "leading-space escape failed")
  (listener-gui-e2e-snapshot frame "dispatch-table"
                             :cases 8 :transport "programmatic-frame-command"))

(defun listener-gui-e2e-run-editor (frame)
  (multiple-value-bind (package type end)
      (clim:accept-from-string 'rplaca::rplaca-package "CL-USE")
    (declare (ignore type))
    (listener-gui-e2e-require
     (and (eq package (find-package :cl-user)) (> end 0))
     "package completion failed"))
  (let ((exact "  (+ 1 2) ; exact source"))
    (multiple-value-bind (token)
        (listener-gui-e2e-accept-token frame exact :mode :eval)
      (listener-gui-e2e-require
       (string= exact (rplaca::listener-input-token-source token))
       "input source was not exact")))
  (listener-gui-e2e-require
   (eq :error
       (listener-gui-e2e-token-kind frame (format nil "!line one~%line two")
                                    :mode :eval))
   "multiline prose was accepted")
  (listener-gui-e2e-require
   (eq :form
       (listener-gui-e2e-token-kind frame (format nil "(+ 20~% 22)")
                                    :mode :eval))
   "multiline Lisp was rejected")
  (listener-gui-e2e-snapshot
   frame "grafted-input-editor" :completion t :exact-source t
   :multiline-prose "rejected" :multiline-lisp "accepted"
   :grafted (not (null (rplaca::listener-grafted-top-level-sheet frame)))
   :transport "programmatic-frame-command"
   :xtest-limitation
   "WM-less Xvfb does not reliably dispatch XTEST into accept-based interactor input"))

(defun listener-gui-e2e-queue-key (frame key-name &key character control)
  (let ((interactor (clim:frame-standard-output frame)))
    (clim:queue-event
     (rplaca::listener-grafted-top-level-sheet frame)
     (make-instance 'clim:key-press-event
                    :sheet interactor :x 0 :y 0
                    :key-name key-name :key-character character
                    :modifier-state
                    (if control
                        (clim:make-modifier-state :control)
                        0)))))

(defun listener-gui-e2e-run-slow-turn (frame)
  (let* ((buffer (rplaca::rplaca-listener-conversation-buffer frame))
         (prompt-count *listener-gui-e2e-prompt-count*)
         (*listener-gui-e2e-provider-mode* :success))
    (rplaca::set-buffer-provider-override buffer :e2e)
    (rplaca::set-buffer-model-override buffer "e2e-model")
    (setf *listener-gui-e2e-controller-phase* :slow)
    (let ((clim:*application-frame* frame))
      (rplaca::com-say "hello slow provider"))
    (setf *listener-gui-e2e-controller-phase* nil)
    (listener-gui-e2e-require
     (= prompt-count *listener-gui-e2e-prompt-count*)
     "next prompt appeared during the active turn")
    (listener-gui-e2e-require
     (and (null (rplaca::rplaca-listener-active-await-request frame))
          (search "RPLACA_E2E_HELLO_SENTINEL"
                  (rplaca::assistant-turn-primary-text
                   (rplaca::rplaca-listener-pending-assistant-turn frame))))
     "slow provider did not settle to one inline result")
    (listener-gui-e2e-require
     (member :ordinary-key-consumed *listener-gui-e2e-controller-observations*)
     "ordinary key was not injected during wait")
    (listener-gui-e2e-require
     (and (plusp *listener-gui-e2e-beep-count*)
          (member :wholine-progress *listener-gui-e2e-controller-observations*))
     "ordinary key was not beeped or wholine progress was absent")
    (listener-gui-e2e-require
     (member :queued-command-deferred *listener-gui-e2e-controller-observations*)
     "cross-thread command was not observed deferred")
    (let ((queued (climi::queue-read-no-hang
                   (clim-internals::frame-command-queue frame))))
      (listener-gui-e2e-require
       (equal '(rplaca::com-no-op) queued)
       "cross-thread command did not remain queued until settlement")
      (clim:execute-frame-command frame queued))
    (listener-gui-e2e-prompt (clim:frame-standard-output frame) frame)
    (listener-gui-e2e-require
     (= (1+ prompt-count) *listener-gui-e2e-prompt-count*)
     "next prompt did not appear after settlement")
    (listener-gui-e2e-snapshot
     frame "slow-provider" :next-prompt-during-turn nil
     :next-prompt-after-turn t :ordinary-key-consumed t :beeped t
     :wholine-progress "[working]"
     :queued-command "deferred" :inline-presentations 1)))

(defun listener-gui-e2e-run-cancel (frame phase gesture-name)
  (let ((*listener-gui-e2e-provider-mode* :success))
    (setf *listener-gui-e2e-controller-phase* phase)
    (let ((clim:*application-frame* frame))
      (rplaca::com-say (format nil "hello cancel ~A" gesture-name)))
    (setf *listener-gui-e2e-controller-phase* nil)
    (let ((turn (rplaca::rplaca-listener-pending-assistant-turn frame)))
      (listener-gui-e2e-require
       (and turn (eq :cancelled (rplaca::assistant-turn-status turn))
            (search "cancelled" (string-downcase
                                 (rplaca::assistant-turn-primary-text turn))))
       "~A cancellation did not render inline" gesture-name))
    (listener-gui-e2e-prompt (clim:frame-standard-output frame) frame)
    (listener-gui-e2e-snapshot frame (format nil "cancel-~A" gesture-name)
                               :cancelled t :returned-to-prompt t)))

(defun listener-gui-e2e-run-provider-failure (frame)
  (let ((state (listener-gui-e2e-provider-double :failure nil nil)))
    (listener-gui-e2e-require
     (and (wait-for-gui-e2e-stream state)
          (search "deterministic provider seam failure"
                  (princ-to-string (rplaca::stream-state-error-p state))))
     "failing provider did not fail at the provider seam"))
  (listener-gui-e2e-write-evidence
   "17-e2e-negative.txt"
   '("provider-seam=failure" "network-attempts=0" "status=error-observed"))
  (listener-gui-e2e-snapshot frame "negative-provider"
                             :provider-seam "failure" :network-attempts 0))

(defun display-listener-gui-e2e-secondary (frame pane)
  (declare (ignore frame))
  (write-string "isolated inspect/media secondary" pane))

(clim:define-application-frame listener-gui-e2e-secondary-frame () ()
  (:panes (display :application
                   :display-function 'display-listener-gui-e2e-secondary))
  (:layouts (default display)))

(defun listener-gui-e2e-run-secondary-frame (primary-frame)
  (let* ((manager (clim:frame-manager primary-frame))
         (secondary
           (clim:make-application-frame
            'listener-gui-e2e-secondary-frame
            :frame-manager manager
            :pretty-name "RPLACA E2E Secondary"))
         (process
           (clim-sys:make-process
            (lambda ()
              (unwind-protect
                   (clim:run-frame-top-level secondary)
                (ignore-errors (clim:disown-frame manager secondary))))
            :name "listener gui e2e secondary frame")))
    (clim-sys:process-wait-with-timeout
     "secondary frame graft"
     5
     (lambda ()
       (let ((sheet (ignore-errors (clim:frame-top-level-sheet secondary))))
         (and sheet (ignore-errors (clim:sheet-grafted-p sheet))))))
    (listener-gui-e2e-require
     (and (clim-sys:process-state process)
          (not (eq secondary primary-frame))
          (not (eq (clim:frame-standard-output secondary)
                   (clim:frame-standard-output primary-frame))))
     "inspect/media secondary frame was not isolated")
    (let ((sheet (clim:frame-top-level-sheet secondary)))
      (clim:queue-event
       sheet
       (make-instance 'clim:window-manager-delete-event :sheet sheet)))
    (clim-sys:process-wait-with-timeout
     "secondary frame stop" 5
     (lambda () (not (clim-sys:process-state process))))
    (listener-gui-e2e-require
     (not (clim-sys:process-state process))
     "inspect/media secondary frame did not close")
    t))

(defun listener-gui-e2e-run-facets (frame)
  (let ((turn (rplaca::make-assistant-turn
               :primary-text "facet body"
               :tool-uses (list (list :name "read"))
               :reasoning (list "reason")
               :metadata (list :model "e2e")
               :artifact-refs (list "artifact.txt")
               :media-refs (list "media.png")
               :inspect-payload (list :value 42))))
    (rplaca::emit-listener-assistant-turn frame turn)
    (dolist (kind (rplaca::listener-turn-nonempty-facet-kinds turn))
      (listener-gui-e2e-layout-transition
       (lambda ()
         (let ((clim:*application-frame* frame))
           (rplaca::com-show-turn-details
            (rplaca::make-turn-facet :turn turn :kind kind)))))
      (listener-gui-e2e-require
       (and (eq 'rplaca::listener+details (clim:frame-current-layout frame))
            (eq (clim:frame-standard-output frame)
                (clim:get-frame-pane frame 'rplaca::interactor)))
       "facet ~A changed the standard output or failed to open" kind)
      (listener-gui-e2e-layout-transition
       (lambda ()
         (let ((clim:*application-frame* frame))
           (rplaca::com-close-details)))))
    (listener-gui-e2e-require
     (eq 'rplaca::listener-only (clim:frame-current-layout frame))
     "details did not close")
    (listener-gui-e2e-run-secondary-frame frame)
    (listener-gui-e2e-snapshot
     frame "facet-details" :facet-count 6 :details-closed t
     :secondary-frame "isolated-inspect-media-frame"
     :transport "programmatic-presentation-command")))

(defun listener-gui-e2e-append-session-event (session event &key parent-id)
  (multiple-value-bind (written id)
      (if parent-id
          (rplaca::append-session-tree-event session event :parent-id parent-id)
          (rplaca::append-session-tree-event session event))
    (declare (ignore written))
    id))

(defun listener-gui-e2e-replay-message (sender text timestamp &key metadata)
  `((:event . "message") (:sender . ,sender) (:text . ,text)
    (:timestamp . ,timestamp) (:read-only-p . t)
    ,@(when metadata `((:metadata . ,metadata)))))

(defun listener-gui-e2e-make-replay-session (name)
  (let* ((session (rplaca::load-or-create-session name :display-name name))
         (user-id
           (listener-gui-e2e-append-session-event
            session (listener-gui-e2e-replay-message "USER" "user one" 10)))
         (assistant-id
           (listener-gui-e2e-append-session-event
            session
            (listener-gui-e2e-replay-message
             "ASSISTANT" "assistant one" 20
             :metadata '((:model . "e2e-model")))
            :parent-id user-id)))
    (listener-gui-e2e-append-session-event
     session
     (listener-gui-e2e-replay-message "ASSISTANT" "assistant two" 30)
     :parent-id assistant-id)
    session))

(defun listener-gui-e2e-run-resume (frame)
  (let* ((session-name "listener-e2e-replay")
         (session (listener-gui-e2e-make-replay-session session-name)))
    (declare (ignore session))
    (let ((clim:*application-frame* frame))
      (rplaca::com-resume-session session-name)
      (rplaca::com-resume-session session-name))
    (listener-gui-e2e-require
     (= 1 (hash-table-count
           (rplaca::rplaca-listener-replayed-session-branches frame)))
     "session replay was not idempotent")
    (listener-gui-e2e-snapshot frame "resume-session"
                               :chronological-replay t :replay-count 1)))

(defun listener-gui-e2e-run-package-directory-output (frame)
  (let* ((buffer (rplaca::rplaca-listener-conversation-buffer frame))
         (original (rplaca::buffer-working-directory buffer))
         (target (uiop:ensure-directory-pathname
                  (listener-gui-e2e-artifact-path "directory/"))))
    (ensure-directories-exist (merge-pathnames ".keep" target))
    (let ((clim:*application-frame* frame))
      (rplaca::com-package (find-package :cl-user))
      (rplaca::com-push-directory target)
      (rplaca::com-pop-directory))
    (listener-gui-e2e-require
     (and (equal original (rplaca::buffer-working-directory buffer))
          (eq (clim:frame-standard-output frame)
              (clim:frame-error-output frame)))
     "package/directory/output binding failed")
    (dolist (layout '(rplaca::listener-only rplaca::listener+details))
      (listener-gui-e2e-layout-transition
       (lambda () (setf (clim:frame-current-layout frame) layout)))
      (rplaca::listener-adopt-current-appearance frame)
      (rplaca::handle-listener-safe-reload-redisplay frame))
    (listener-gui-e2e-layout-transition
     (lambda ()
       (setf (clim:frame-current-layout frame) 'rplaca::listener-only)))
    (listener-gui-e2e-snapshot
     frame "package-directory-output" :package "CL-USER"
     :directory-restored t :output-bound-to-interactor t
     :safe-reload "async-completion-redisplay" :appearance-layouts 2)))

(defun listener-gui-e2e-run-scenarios (frame)
  (listener-gui-e2e-run-dispatch frame)
  (listener-gui-e2e-run-editor frame)
  (listener-gui-e2e-run-slow-turn frame)
  (listener-gui-e2e-run-cancel frame :escape "escape")
  (listener-gui-e2e-run-cancel frame :abort "ctrl-c")
  (listener-gui-e2e-run-provider-failure frame)
  (listener-gui-e2e-run-facets frame)
  (listener-gui-e2e-run-resume frame)
  (listener-gui-e2e-run-package-directory-output frame)
  (listener-gui-e2e-write-evidence
   "17-e2e.txt"
   '("suite=listener" "status=passed" "network-attempts=0"
     "slow-provider=prompt-blocked,wholine-progress,key-consumed-beeped,inline-result,next-prompt"
     "cancellation=escape,ctrl-c"
     "facets=6,details-open-close"
     "resume=chronological-once"
     "close-mid-wait=clean"
     "input-transport=programmatic-frame-command"
     "xtest-limitation=accept-based interactor under WM-less Xvfb"))
  (listener-gui-e2e-event "listener-suite-complete" :ok t)
  (setf *listener-gui-e2e-controller-phase* :close)
  (let ((clim:*application-frame* frame))
    (rplaca::com-say "hello close frame mid-wait")))

(defclass listener-gui-e2e-control-event (clim:window-manager-event) ())

(defmethod clim:handle-event
    ((sheet clime:top-level-sheet-mixin)
     (event listener-gui-e2e-control-event))
  (declare (ignore event))
  (let ((frame (clim:pane-frame sheet)))
    (when (typep frame 'rplaca::rplaca-listener)
      (handler-case
          (listener-gui-e2e-run-scenarios frame)
        (error (condition)
          (listener-gui-e2e-event "listener-suite-failed"
                                  :message (princ-to-string condition))
          (error condition))))))

(defun listener-gui-e2e-controller (frame)
  (loop :until (rplaca::listener-grafted-top-level-sheet frame)
        :do (sleep 0.05))
  (let ((sheet (rplaca::listener-grafted-top-level-sheet frame)))
    (listener-gui-e2e-event "frame-ready" :frame "rplaca-listener")
    (listener-gui-e2e-snapshot frame "initial" :transport "grafted-frame")
    (clim:queue-event sheet
                      (make-instance 'listener-gui-e2e-control-event
                                     :sheet sheet))
    (loop :with handled := nil
          :for phase := *listener-gui-e2e-controller-phase*
          :until (eq phase :close)
          :do (unless (eq phase handled)
                (case phase
                  (:slow
                   (sleep 0.2)
                   (when (search "[working]" (listener-gui-e2e-wholine frame))
                     (push :wholine-progress
                           *listener-gui-e2e-controller-observations*))
                   (listener-gui-e2e-queue-key frame nil :character #\x)
                   (climi::queue-append
                    (clim-internals::frame-command-queue frame)
                    '(rplaca::com-no-op))
                   (push :ordinary-key-consumed
                         *listener-gui-e2e-controller-observations*)
                   (push :queued-command-deferred
                         *listener-gui-e2e-controller-observations*))
                  (:escape
                   (sleep 0.2)
                   (listener-gui-e2e-queue-key frame :escape))
                  (:abort
                   (sleep 0.2)
                   (listener-gui-e2e-queue-key frame nil
                                               :character #\c :control t)))
                (setf handled phase))
              (sleep 0.03))
    (sleep 0.2)
    (let ((process (clim-internals::frame-process frame)))
      (clim-sys:process-interrupt process
                                  (lambda () (clim:frame-exit frame))))))

(defun run-listener-gui-e2e (&key (window-title "RPLACA E2E"))
  (setf rplaca::*debug-log-file*
        (pathname (or (uiop:getenv "RPLACA_DEBUG_LOG")
                      (listener-gui-e2e-artifact-path "debug.log"))))
  (rplaca::initialize-rplaca-runtime)
  (rplaca::reset-interaction-state)
  (setf *listener-gui-e2e-controller-phase* nil
        *listener-gui-e2e-controller-observations* nil
        *listener-gui-e2e-prompt-count* 0
        *listener-gui-e2e-beep-count* 0)
  (let* ((frame-manager (clim:find-frame-manager :port (clim:find-port)))
         (buffer (rplaca::make-initial-chat-buffer
                  "rplaca:e2e" "agent" :working-directory (truename ".")))
         (frame (clim:make-application-frame
                 'rplaca::rplaca-listener
                 :frame-manager frame-manager
                 :conversation-buffer buffer
                 :pending-session-name "rplaca:e2e"
                 :pretty-name window-title
                 :appearance-profile (rplaca::make-appearance-profile)
                 :listener-context (rplaca::listener-context-for-buffer buffer)))
         (*listener-gui-e2e-original-prompt-function*
           (symbol-function 'rplaca::listener-print-prompt))
         (*listener-gui-e2e-original-beep-function*
           (symbol-function 'clim:beep))
         (original-provider
           (symbol-function 'rplaca::provider-request-streaming)))
    (unwind-protect
         (progn
           (setf (symbol-function 'rplaca::listener-print-prompt)
                 #'listener-gui-e2e-prompt
                 (symbol-function 'clim:beep)
                 #'listener-gui-e2e-beep
                 (symbol-function 'rplaca::provider-request-streaming)
                 #'listener-gui-e2e-provider-request-streaming)
           (clim-sys:make-process
            (lambda () (listener-gui-e2e-controller frame))
            :name "listener gui e2e controller")
           (clim:run-frame-top-level frame))
      (setf (symbol-function 'rplaca::listener-print-prompt)
            *listener-gui-e2e-original-prompt-function*
            (symbol-function 'clim:beep)
            *listener-gui-e2e-original-beep-function*
            (symbol-function 'rplaca::provider-request-streaming)
            original-provider)
      (ignore-errors
        (clim:disown-frame frame-manager frame)))
    (sleep 0.5)
    (listener-gui-e2e-event "listener-close-mid-wait"
                            :late-event-error nil :runtime-active nil)
    (listener-gui-e2e-event "frame-stopped" :frame "rplaca-listener")
    t))

(test listener-gui-e2e-dispatch-matrix-is-complete
  (let ((cases (listener-gui-e2e-dispatch-cases)))
    (is (equal '(:eval :package :enter-say :exit-say :interpolation
                 :shell :no-op :leading-space)
               (mapcar (lambda (case) (getf case :name)) cases)))
    (is (every (lambda (case)
                 (member (getf case :transport)
                         '(:grafted-editor :programmatic-frame-command)))
               cases))))

(test listener-gui-e2e-failing-provider-stops-at-seam
  (let ((network-attempts 0))
    (let ((state
            (listener-gui-e2e-provider-double
             :failure nil nil
             :network-attempt-function (lambda () (incf network-attempts)))))
      (is-true (wait-for-gui-e2e-stream state))
      (is (search "deterministic provider seam failure"
                  (princ-to-string (rplaca::stream-state-error-p state)))))
    (is (= 0 network-attempts))))
