(require :asdf)

(defun configure-registry ()
  (let* ((script (or *load-pathname* *compile-file-pathname*))
         (backend-root (truename
                        (merge-pathnames "../"
                                         (make-pathname :directory (pathname-directory script)))))
         (repo-root (truename (merge-pathnames "../../" backend-root)))
         (mcclim-root (or (uiop:getenv "MCCLIM_SOURCE_ROOT")
                          "/home/tay/reference/external_src/McCLIM/")))
    (pushnew backend-root asdf:*central-registry* :test #'equal)
    (pushnew repo-root asdf:*central-registry* :test #'equal)
    (when (probe-file mcclim-root)
      (asdf:initialize-source-registry
       `(:source-registry
         (:tree ,(namestring backend-root))
         (:tree ,(namestring (truename mcclim-root)))
         :inherit-configuration))
      (pushnew (truename mcclim-root) asdf:*central-registry* :test #'equal))))

(configure-registry)
(asdf:load-system :mcclim-charms)
(setf (symbol-value (find-symbol "*DEFAULT-SERVER-PATH*" "CLIM")) '(:charms))

(in-package #:mcclim-charms)

(defun fail (control &rest args)
  (apply #'format *error-output* control args)
  (terpri *error-output*)
  (force-output *error-output*)
  (uiop:quit 1))

(defun pass (control &rest args)
  (apply #'format *error-output* control args)
  (terpri *error-output*)
  (force-output *error-output*))

(defun read-curses-event-or-fail (port label)
  (loop repeat 100
        for event = (or (%pop-event port)
                        (%read-curses-event port))
        when event
          return event
        do (sleep 0.01)
        finally (fail "missing event for ~A" label)))

(defun require-event (event class label)
  (unless (typep event class)
    (fail "~A expected ~S, got ~S" label class event))
  event)

(defun require-key (event expected-name expected-character label)
  (require-event event 'key-press-event label)
  (unless (and (eql (keyboard-event-key-name event) expected-name)
               (eql (keyboard-event-character event) expected-character))
    (fail "~A expected key ~S/~S, got ~S/~S"
          label
          expected-name
          expected-character
          (keyboard-event-key-name event)
          (keyboard-event-character event)))
  event)

(defun unget-mouse (bstate x y)
  (cffi:with-foreign-object (event '(:struct ll::mevent))
    (setf (cffi:foreign-slot-value event '(:struct ll::mevent) 'll::id) 0
          (cffi:foreign-slot-value event '(:struct ll::mevent) 'll::x) x
          (cffi:foreign-slot-value event '(:struct ll::mevent) 'll::y) y
          (cffi:foreign-slot-value event '(:struct ll::mevent) 'll::z) 0
          (cffi:foreign-slot-value event '(:struct ll::mevent) 'll::bstate) bstate)
    (ll::%ungetmouse event)))

(defun run-keyboard-interactions (port)
  (require-key (read-curses-event-or-fail port :external-key) #\z #\z :external-key)
  (dolist (case `((#\a #\a #\a)
                  (,ll:KEY_UP :up nil)
                  (,ll:KEY_DOWN :down nil)
                  (,ll:KEY_LEFT :left nil)
                  (,ll:KEY_RIGHT :right nil)
                  (,ll:KEY_HOME :home nil)
                  (,ll:KEY_BACKSPACE :backspace nil)
                  (,ll:KEY_NPAGE :next nil)
                  (,ll:KEY_PPAGE :prior nil)))
    (destructuring-bind (code expected-name expected-character) case
      (ll:ungetch (if (characterp code) (char-code code) code))
      (require-key (read-curses-event-or-fail port expected-name)
                   expected-name
                   expected-character
                   expected-name))))

(defun run-mouse-interactions (port)
  (flet ((require-pointer (state class label)
           (%queue-event port
                         (charms-mouse-event->clim-event
                          port
                          (%event-root-sheet-for-port port)
                          state
                          11
                          7
                          0
                          0))
           (let ((event (require-event (%pop-event port) class label)))
             (unless (and (= (pointer-event-x event) 11)
                          (= (pointer-event-y event) 7))
               (fail "~A expected pointer coordinates 11,7, got ~A,~A"
                     label
                     (pointer-event-x event)
                     (pointer-event-y event)))
             event))
         (require-synthetic-release (label)
           (%queue-event port
                         (make-instance 'pointer-button-release-event
                                        :pointer (port-pointer port)
                                        :button +pointer-left-button+
                                        :x 11
                                        :y 7
                                        :sheet (%event-root-sheet-for-port port)
                                        :modifier-state 0))
           (require-event (%pop-event port) 'pointer-button-release-event label)))
    (require-pointer ll:BUTTON1_PRESSED 'pointer-button-press-event :left-press)
    (require-synthetic-release :left-press-release)
    (require-pointer ll:BUTTON1_RELEASED 'pointer-button-release-event :left-release)
    (require-pointer ll:BUTTON1_DOUBLE_CLICKED 'pointer-double-click-event :left-double-click)
    (require-pointer (logior ll:BUTTON1_PRESSED ll:REPORT_MOUSE_POSITION)
                     'pointer-button-hold-event
                     :left-drag)
    (require-pointer ll:REPORT_MOUSE_POSITION 'pointer-motion-event :motion)
    (require-pointer (logior ll:BUTTON2_PRESSED ll:BUTTON_SHIFT)
                     'pointer-button-press-event
                     :middle-shift-press)
    (require-synthetic-release :middle-shift-release)
    (require-pointer (logior ll:BUTTON3_PRESSED ll:BUTTON_CTRL ll:BUTTON_ALT)
                     'pointer-button-press-event
                     :right-control-meta-press)
    (require-synthetic-release :right-control-meta-release)
    (let ((wheel-up (require-pointer #x10000 'pointer-scroll-event :wheel-up))
          (wheel-down (require-pointer #x200000 'pointer-scroll-event :wheel-down)))
      (unless (and (= (pointer-event-button wheel-up) +pointer-wheel-up+)
                   (= (pointer-event-delta-y wheel-up) -1)
                   (= (pointer-event-button wheel-down) +pointer-wheel-down+)
                   (= (pointer-event-delta-y wheel-down) 1))
        (fail "wheel events translated incorrectly: ~S ~S" wheel-up wheel-down))))
  (%queue-event port
                (make-instance 'pointer-button-press-event
                               :pointer (port-pointer port)
                               :button +pointer-left-button+
                               :x 11
                               :y 7
                               :sheet (%event-root-sheet-for-port port)
                               :modifier-state 0))
  (%queue-event port
                (make-instance 'pointer-button-release-event
                               :pointer (port-pointer port)
                               :button +pointer-left-button+
                               :x 11
                               :y 7
                               :sheet (%event-root-sheet-for-port port)
                               :modifier-state 0))
  (let* ((press (require-event (read-curses-event-or-fail port :left-click-press)
                               'pointer-button-press-event
                               :left-click-press))
         (release (require-event (read-curses-event-or-fail port :left-click-release)
                                 'pointer-button-release-event
                                 :left-click-release)))
    (unless (and (= (pointer-event-x press) 11)
                 (= (pointer-event-y press) 7)
                 (= (pointer-event-x release) 11)
                 (= (pointer-event-y release) 7))
      (fail "left-click expected coordinates 11,7, got ~A,~A and ~A,~A"
            (pointer-event-x press)
            (pointer-event-y press)
            (pointer-event-x release)
            (pointer-event-y release)))))

(defun run-resize-timeout-and-queued-interactions (port sheet)
  (ll:ungetch ll:KEY_RESIZE)
  (require-event (read-curses-event-or-fail port :resize)
                 'window-configuration-event
                 :resize)
  (when (%read-curses-event port)
    (fail "non-blocking timeout path returned an unexpected event"))
  (%queue-event port (make-instance 'timer-event :sheet sheet :qualifier :e2e))
  (require-event (%pop-event port) 'timer-event :timer)
  (%queue-event port (make-instance 'climi::lambda-event
                                    :sheet sheet
                                    :thunk (lambda ())))
  (require-event (%pop-event port) 'climi::lambda-event :wakeup))

(defun main ()
  (let ((*initialize-curses-on-port-create* nil))
    (let* ((port (make-instance 'charms-port))
           (graft (make-graft port)))
      (setf (charms-port-keyboard-focus port) graft)
      (with-charms-port (port)
        (format *error-output* "MCCLIM-CHARMS-INTERACTION-READY~%")
        (force-output *error-output*)
        (run-keyboard-interactions port)
        (run-mouse-interactions port)
        (run-resize-timeout-and-queued-interactions port graft)
        (pass "MCCLIM-CHARMS-INTERACTION-PASS")))))

(handler-case
    (progn
      (main)
      (uiop:quit 0))
  (error (condition)
    (fail "interaction e2e failed: ~A" condition)))
