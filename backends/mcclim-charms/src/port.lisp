(in-package #:mcclim-charms)

(defmethod find-port-type ((type (eql :charms)))
  (values 'charms-port 'identity))

(defmethod initialize-instance :after ((port charms-port) &rest initargs)
  (declare (ignore initargs))
  (push (make-instance 'charms-frame-manager :port port)
        (slot-value port 'climi::frame-managers))
  (when *initialize-curses-on-port-create*
    (multiple-value-bind (value condition)
        (safe-call (lambda () (initialize-charms-port port)))
      (declare (ignore value))
      (when condition
        (warn "Could not initialize charms port immediately: ~A" condition)))))

(defmethod destroy-port :before ((port charms-port))
  (finalize-charms-port port))

(defmethod print-object ((object charms-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S ~DX~D"
            :id (charms-port-id object)
            (charms-port-width object)
            (charms-port-height object))))

(defmethod make-graft
    ((port charms-port) &key (orientation :default) (units :device))
  (make-instance 'charms-graft
                 :port port
                 :mirror t
                 :orientation orientation
                 :units units))

(defmethod make-medium ((port charms-port) sheet)
  (make-instance 'charms-medium :port port :sheet sheet))

(defmethod text-style-mapping
    ((port charms-port) (text-style text-style) &optional character-set)
  (declare (ignore port text-style character-set))
  nil)

(defmethod (setf text-style-mapping) (font-name
                                      (port charms-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)

(defmethod port-modifier-state ((port charms-port))
  (charms-port-modifier-state port))

(defmethod (setf port-keyboard-input-focus) (focus (port charms-port))
  (setf (charms-port-keyboard-focus port) focus))

(defmethod port-keyboard-input-focus ((port charms-port))
  (charms-port-keyboard-focus port))

(defmethod port-force-output ((port charms-port))
  (when-let ((window (charms-port-window port)))
    (charms:refresh-window window)))

(defmethod set-sheet-pointer-cursor ((port charms-port) sheet cursor)
  (declare (ignore port sheet cursor))
  nil)

(defmethod distribute-event :around ((port charms-port) event)
  (call-next-method))

(defmethod distribute-event :before ((port charms-port) (event pointer-event))
  (declare (ignore port))
  (when (uiop:getenv "MCCLIM_CHARMS_TRACE_EVENTS")
    (format *error-output* "dispatch pointer sheet=~S computed=~S event=~S~%"
            (event-sheet event)
            (climi::compute-pointer-event-sheet event)
            event)
    (force-output *error-output*)))

(defmethod process-next-event ((port charms-port) &key wait-function (timeout nil))
  (labels ((next ()
             (or (%pop-event port)
                 (when (charms-port-initialized-p port)
                   (%read-curses-event port))))
           (dispatch-one (event)
             (when event
               (distribute-event port event)
               event)))
    (cond ((maybe-funcall wait-function)
           (values nil :wait-function))
          ((dispatch-one (next))
           (values t :event))
          ((not (null timeout))
           (let ((deadline (+ (get-internal-real-time)
                              (round (* timeout internal-time-units-per-second)))))
             (loop
               (when (maybe-funcall wait-function)
                 (return (values nil :wait-function)))
               (when-let ((event (next)))
                 (dispatch-one event)
                 (return (values t :event)))
               (when (>= (get-internal-real-time) deadline)
                 (return (values nil :timeout)))
               (sleep 0.01))))
          ((not (null wait-function))
           (loop
             (when (maybe-funcall wait-function)
               (return (values nil :wait-function)))
             (when-let ((event (next)))
               (dispatch-one event)
               (return (values t :event)))
             (sleep 0.01)))
          (t
           (loop
             (when-let ((event (next)))
               (dispatch-one event)
               (return (values t :event)))
             (sleep 0.01))))))
