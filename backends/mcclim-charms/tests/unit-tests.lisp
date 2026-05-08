(in-package #:mcclim-charms/tests)

(in-suite mcclim-charms-suite)

(test server-path-registration
  (multiple-value-bind (port-class init-function)
      (clim-backend:find-port-type :charms)
    (is (eq port-class 'backend:charms-port))
    (is (eq init-function 'identity))))

(test port-can-be-constructed-without-terminal
  (let ((backend:*initialize-curses-on-port-create* nil))
    (let ((port (make-instance 'backend:charms-port)))
      (is (typep port 'backend:charms-port))
      (is-false (backend:charms-port-initialized-p port))
      (multiple-value-bind (width height)
          (backend:charms-port-size port)
        (is (= 0 width))
        (is (= 0 height))))))

(test graft-reflects-port-size
  (let ((backend:*initialize-curses-on-port-create* nil))
    (let* ((port (make-instance 'backend:charms-port))
           (graft (clim-backend:make-graft port)))
      (setf (slot-value port 'mcclim-charms::width) 132
            (slot-value port 'mcclim-charms::height) 43)
      (is (= 132 (clim:graft-width graft)))
      (is (= 43 (clim:graft-height graft))))))

(test text-metrics-are-terminal-cell-based
  (let ((backend:*initialize-curses-on-port-create* nil))
    (let* ((port (make-instance 'backend:charms-port))
           (medium (make-instance 'backend:charms-medium :port port :sheet nil)))
      (multiple-value-bind (width height cursor-x cursor-y baseline)
          (clim:text-size medium "hello")
        (is (= 5 width))
        (is (= 1 height))
        (is (= 5 cursor-x))
        (is (= 0 cursor-y))
        (is (= 1 baseline))))))

(test key-code-translation
  (multiple-value-bind (key-name key-char modifiers)
      (backend:charms-key-code->gesture (char-code #\a))
    (is (eql #\a key-name))
    (is (eql #\a key-char))
    (is (not (null modifiers)))))

(test pixmap-allocation
  (let ((backend:*initialize-curses-on-port-create* nil))
    (let* ((port (make-instance 'backend:charms-port))
           (medium (make-instance 'backend:charms-medium :port port :sheet nil))
           (pixmap (clim:allocate-pixmap medium 4 3)))
      (is (= 4 (clim:pixmap-width pixmap)))
      (is (= 3 (clim:pixmap-height pixmap)))
      (is (= 1 (clim:pixmap-depth pixmap)))
      (clim:deallocate-pixmap pixmap))))

(test vector-coordinate-drawing-sequences
  (let ((backend:*initialize-curses-on-port-create* nil))
    (let* ((port (make-instance 'backend:charms-port))
           (medium (make-instance 'backend:charms-medium :port port :sheet nil)))
      (finishes (clim:medium-draw-points* medium #(1 1 2 2)))
      (finishes (clim:medium-draw-lines* medium #(1 1 3 3 4 4 6 6)))
      (finishes (clim:medium-draw-polygon* medium #(1 1 3 3 4 1) t nil))
      (finishes (clim:medium-draw-rectangles* medium #(1 1 3 3 4 4 6 6) nil)))))

(test mouse-event-translation-covers-buttons-motion-and-modifiers
  (let ((backend:*initialize-curses-on-port-create* nil))
    (let* ((port (make-instance 'backend:charms-port))
           (sheet (clim-backend:make-graft port)))
      (flet ((event-for (state)
               (backend:charms-mouse-event->clim-event port sheet state 7 9 0 0)))
        (let ((event (event-for charms/ll:BUTTON1_PRESSED)))
          (is (typep event 'clim:pointer-button-press-event))
          (is (= clim:+pointer-left-button+ (clim:pointer-event-button event)))
          (is (= 7 (clim:pointer-event-x event)))
          (is (= 9 (clim:pointer-event-y event))))
        (let ((event (event-for charms/ll:BUTTON2_RELEASED)))
          (is (typep event 'clim:pointer-button-release-event))
          (is (= clim:+pointer-middle-button+ (clim:pointer-event-button event))))
        (let ((event (event-for charms/ll:BUTTON3_CLICKED)))
          (is (typep event 'clim:pointer-click-event))
          (is (= clim:+pointer-right-button+ (clim:pointer-event-button event))))
        (let ((event (event-for charms/ll:BUTTON1_DOUBLE_CLICKED)))
          (is (typep event 'clim:pointer-double-click-event)))
        (let ((event (event-for (logior charms/ll:BUTTON1_PRESSED
                                         charms/ll:REPORT_MOUSE_POSITION))))
          (is (typep event 'clim:pointer-button-hold-event)))
        (let ((event (event-for charms/ll:REPORT_MOUSE_POSITION)))
          (is (typep event 'clim:pointer-motion-event)))
        (let ((event (event-for #x10000)))
          (is (typep event 'clim-extensions:pointer-scroll-event))
          (is (= clim:+pointer-wheel-up+ (clim:pointer-event-button event)))
          (is (= 0 (clim-extensions:pointer-event-delta-x event)))
          (is (= -1 (clim-extensions:pointer-event-delta-y event))))
        (let ((event (event-for #x200000)))
          (is (typep event 'clim-extensions:pointer-scroll-event))
          (is (= clim:+pointer-wheel-down+ (clim:pointer-event-button event)))
          (is (= 0 (clim-extensions:pointer-event-delta-x event)))
          (is (= 1 (clim-extensions:pointer-event-delta-y event))))
        (let ((event (event-for (logior charms/ll:BUTTON1_PRESSED
                                         charms/ll:BUTTON_SHIFT
                                         charms/ll:BUTTON_CTRL
                                         charms/ll:BUTTON_ALT))))
          (is (not (zerop (logand (clim:event-modifier-state event)
                                  clim:+shift-key+))))
          (is (not (zerop (logand (clim:event-modifier-state event)
                                  clim:+control-key+))))
          (is (not (zerop (logand (clim:event-modifier-state event)
                                  clim:+meta-key+)))))))))
