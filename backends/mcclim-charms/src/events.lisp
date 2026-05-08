(in-package #:mcclim-charms)

(defun charms-key-code->gesture (code)
  "Translate a curses key code into a McCLIM key-name and character."
  (cond ((and (integerp code)
              (<= 0 code 255))
         (let ((char (code-char code)))
           (values char char 0)))
        ((eql code ll:KEY_UP) (values :up nil 0))
        ((eql code ll:KEY_DOWN) (values :down nil 0))
        ((eql code ll:KEY_LEFT) (values :left nil 0))
        ((eql code ll:KEY_RIGHT) (values :right nil 0))
        ((eql code ll:KEY_HOME) (values :home nil 0))
        ((eql code ll:KEY_BACKSPACE) (values :backspace nil 0))
        ((eql code ll:KEY_NPAGE) (values :next nil 0))
        ((eql code ll:KEY_PPAGE) (values :prior nil 0))
        (t (values :unknown nil 0))))

(defun %event-root-sheet-for-port (port)
  (let ((graft (first (slot-value port 'climi::grafts))))
    (or (first (sheet-enabled-children graft))
        graft)))

(defun %keyboard-event-sheet-for-port (port)
  (or (charms-port-keyboard-focus port)
      (%event-root-sheet-for-port port)))

(defun %queue-event (port event)
  (setf (charms-port-event-queue port)
        (nconc (charms-port-event-queue port) (list event)))
  event)

(defun %pop-event (port)
  (let ((events (charms-port-event-queue port)))
    (when events
      (setf (charms-port-event-queue port) (rest events))
      (first events))))

(defun %mouse-state-modifiers (bstate)
  (logior (if (zerop (logand bstate (logior ll:BUTTON_SHIFT #x4000000))) 0 +shift-key+)
          (if (zerop (logand bstate (logior ll:BUTTON_CTRL #x2000000))) 0 +control-key+)
          (if (zerop (logand bstate (logior ll:BUTTON_ALT #x8000000))) 0 +meta-key+)))

(defconstant +ncurses-v2-button4-mask+ #x000f8000)
(defconstant +ncurses-v2-button5-mask+ #x01f00000)
(defconstant +ncurses-v2-button4-pressed+ #x00010000)
(defconstant +ncurses-v2-button5-pressed+ #x00200000)
(defconstant +ncurses-v2-report-mouse-position+ #x10000000)

(defun %mouse-primary-button-state-p (bstate)
  (not (zerop (logand bstate
                      (logior ll:BUTTON1_PRESSED
                              ll:BUTTON1_RELEASED
                              ll:BUTTON1_CLICKED
                              ll:BUTTON1_DOUBLE_CLICKED
                              ll:BUTTON1_TRIPLE_CLICKED
                              ll:BUTTON2_PRESSED
                              ll:BUTTON2_RELEASED
                              ll:BUTTON2_CLICKED
                              ll:BUTTON2_DOUBLE_CLICKED
                              ll:BUTTON2_TRIPLE_CLICKED
                              ll:BUTTON3_PRESSED
                              ll:BUTTON3_RELEASED
                              ll:BUTTON3_CLICKED
                              ll:BUTTON3_DOUBLE_CLICKED)))))

(defun %mouse-wheel-button (bstate)
  (unless (%mouse-primary-button-state-p bstate)
    (cond ((not (zerop (logand bstate +ncurses-v2-button4-mask+)))
           +pointer-wheel-up+)
          ((not (zerop (logand bstate +ncurses-v2-button5-mask+)))
           +pointer-wheel-down+)
          (t nil))))

(defun %mouse-wheel-deltas (button)
  (case button
    (#.+pointer-wheel-up+ (values 0 -1))
    (#.+pointer-wheel-down+ (values 0 1))
    (#.+pointer-wheel-left+ (values -1 0))
    (#.+pointer-wheel-right+ (values 1 0))
    (otherwise (values 0 0))))

(defun %mouse-position-report-p (bstate)
  (not (zerop (logand bstate
                      (logior ll:REPORT_MOUSE_POSITION
                              +ncurses-v2-report-mouse-position+)))))

(defun %mouse-button (bstate)
  (cond ((%mouse-wheel-button bstate))
        ((not (zerop (logand bstate
                             (logior ll:BUTTON1_PRESSED
                                     ll:BUTTON1_RELEASED
                                     ll:BUTTON1_CLICKED
                                     ll:BUTTON1_DOUBLE_CLICKED
                                     ll:BUTTON1_TRIPLE_CLICKED))))
         +pointer-left-button+)
        ((not (zerop (logand bstate
                             (logior ll:BUTTON2_PRESSED
                                     ll:BUTTON2_RELEASED
                                     ll:BUTTON2_CLICKED
                                     ll:BUTTON2_DOUBLE_CLICKED
                                     ll:BUTTON2_TRIPLE_CLICKED))))
         +pointer-middle-button+)
        ((not (zerop (logand bstate
                             (logior ll:BUTTON3_PRESSED
                                     ll:BUTTON3_RELEASED
                                     ll:BUTTON3_CLICKED
                                     ll:BUTTON3_DOUBLE_CLICKED
                                     ll:BUTTON3_TRIPLE_CLICKED))))
         +pointer-right-button+)
        (t +pointer-left-button+)))

(defun %mouse-event-class (bstate)
  (let* ((released-mask (logior ll:BUTTON1_RELEASED
                                ll:BUTTON2_RELEASED
                                ll:BUTTON3_RELEASED))
         (clicked-mask (logior ll:BUTTON1_CLICKED
                               ll:BUTTON2_CLICKED
                               ll:BUTTON3_CLICKED))
         (double-clicked-mask (logior ll:BUTTON1_DOUBLE_CLICKED
                                      ll:BUTTON2_DOUBLE_CLICKED
                                      ll:BUTTON3_DOUBLE_CLICKED
                                      ll:BUTTON1_TRIPLE_CLICKED
                                      ll:BUTTON2_TRIPLE_CLICKED
                                      ll:BUTTON3_TRIPLE_CLICKED))
         (button-mask (logior ll:BUTTON1_PRESSED
                              ll:BUTTON2_PRESSED
                              ll:BUTTON3_PRESSED
                              released-mask
                              clicked-mask
                              double-clicked-mask))
         (position-report-p (%mouse-position-report-p bstate))
         (button-p (not (zerop (logand bstate button-mask)))))
    (cond ((%mouse-wheel-button bstate) 'pointer-scroll-event)
          ((and position-report-p button-p) 'pointer-button-hold-event)
          (position-report-p 'pointer-motion-event)
          ((not (zerop (logand bstate released-mask)))
           'pointer-button-release-event)
          ((not (zerop (logand bstate double-clicked-mask)))
           'pointer-double-click-event)
          ((not (zerop (logand bstate clicked-mask)))
           'pointer-click-event)
          (t 'pointer-button-press-event))))

(defun charms-mouse-event->clim-event (port sheet bstate x y z id)
  (declare (ignore z id))
  (let* ((pointer (port-pointer port))
         (button (%mouse-button bstate))
         (class (%mouse-event-class bstate))
         (modifier-state (%mouse-state-modifiers bstate)))
    (when (typep pointer 'charms-pointer)
      (setf (charms-pointer-x pointer) (round x)
            (charms-pointer-y pointer) (round y)))
    (cond ((eq class 'pointer-scroll-event)
           (multiple-value-bind (delta-x delta-y)
               (%mouse-wheel-deltas button)
             (make-instance class
                            :pointer pointer
                            :button button
                            :x x
                            :y y
                            :sheet sheet
                            :modifier-state modifier-state
                            :delta-x delta-x
                            :delta-y delta-y)))
          ((eq class 'pointer-motion-event)
        (make-instance class
                       :pointer pointer
                       :x x
                       :y y
                       :sheet sheet
                       :modifier-state modifier-state))
          (t
           (make-instance class
                          :pointer pointer
                          :button button
                          :x x
                          :y y
                          :sheet sheet
                          :modifier-state modifier-state)))))

(defun %mouse-click-p (bstate)
  (not (zerop (logand bstate
                      (logior ll:BUTTON1_CLICKED
                              ll:BUTTON2_CLICKED
                              ll:BUTTON3_CLICKED)))))

(defun %mouse-press-p (bstate)
  (not (zerop (logand bstate
                      (logior ll:BUTTON1_PRESSED
                              ll:BUTTON2_PRESSED
                              ll:BUTTON3_PRESSED)))))

(defun %make-mouse-button-event (class port sheet bstate x y)
  (let ((pointer (port-pointer port)))
    (make-instance class
                   :pointer pointer
                   :button (%mouse-button bstate)
                   :x x
                   :y y
                   :sheet sheet
                   :modifier-state (%mouse-state-modifiers bstate))))

(defun %key-press-event (port key-name key-char modifiers)
  (make-instance 'key-press-event
                 :key-name key-name
                 :key-character key-char
                 :x 0
                 :y 0
                 :sheet (%keyboard-event-sheet-for-port port)
                 :modifier-state modifiers))

(defun %read-next-curses-code (window)
  (let ((code (ll:wgetch (charms-window-pointer window))))
    (and code (not (minusp code)) code)))

(defun %xterm-button (button-code)
  (case (logand button-code 3)
    (0 +pointer-left-button+)
    (1 +pointer-middle-button+)
    (2 +pointer-right-button+)
    (otherwise +pointer-left-button+)))

(defun %xterm-mouse-event (port sheet button-code x y release-p)
  (let* ((pointer (port-pointer port))
         (wheel-p (not (zerop (logand button-code 64))))
         (button (cond ((and wheel-p (zerop (logand button-code 1)))
                        +pointer-wheel-up+)
                       (wheel-p +pointer-wheel-down+)
                       (t (%xterm-button button-code))))
         (class (cond (wheel-p 'pointer-scroll-event)
                      (release-p 'pointer-button-release-event)
                      (t 'pointer-button-press-event))))
    (when (typep pointer 'charms-pointer)
      (setf (charms-pointer-x pointer) (round x)
            (charms-pointer-y pointer) (round y)))
    (if wheel-p
        (multiple-value-bind (delta-x delta-y)
            (%mouse-wheel-deltas button)
          (make-instance class
                         :pointer pointer
                         :button button
                         :x x
                         :y y
                         :sheet sheet
                         :modifier-state 0
                         :delta-x delta-x
                         :delta-y delta-y))
        (make-instance class
                       :pointer pointer
                       :button button
                       :x x
                       :y y
                       :sheet sheet
                       :modifier-state 0))))

(defun %parse-sgr-mouse-codes (port sheet codes)
  (let* ((chars (map 'string #'code-char codes))
         (final (and (plusp (length chars))
                     (char chars (1- (length chars))))))
    (when (and final (find final "Mm"))
      (let ((parts (uiop:split-string (subseq chars 0 (1- (length chars)))
                                      :separator ";")))
        (when (= (length parts) 3)
          (%xterm-mouse-event port sheet
                              (parse-integer (first parts))
                              (parse-integer (second parts))
                              (parse-integer (third parts))
                              (char= final #\m)))))))

(defun %read-raw-xterm-mouse-event (port sheet window)
  (let ((second (%read-next-curses-code window)))
    (unless (eql second (char-code #\[))
      (return-from %read-raw-xterm-mouse-event nil))
    (let ((third (%read-next-curses-code window)))
      (cond ((eql third (char-code #\M))
             (let ((button (%read-next-curses-code window))
                   (x (%read-next-curses-code window))
                   (y (%read-next-curses-code window)))
               (when (and button x y)
                 (%xterm-mouse-event port sheet
                                     (- button 32)
                                     (- x 32)
                                     (- y 32)
                                     (= (- button 32) 3)))))
            ((eql third (char-code #\<))
             (let ((codes '()))
               (loop repeat 32
                     for code = (%read-next-curses-code window)
                     while code
                     do (push code codes)
                     until (or (eql code (char-code #\M))
                               (eql code (char-code #\m))))
               (%parse-sgr-mouse-codes port sheet (nreverse codes))))
            (t nil)))))

(defun %trace-curses-event (control &rest args)
  (when (uiop:getenv "MCCLIM_CHARMS_TRACE_EVENTS")
    (apply #'format *error-output* control args)
    (terpri *error-output*)
    (force-output *error-output*)))

(defmethod handle-event ((sheet charms-graft)
                         (event clim-extensions:window-manager-focus-event))
  (declare (ignore sheet event))
  nil)

(defmethod dispatch-event ((sheet charms-graft)
                           (event clim-extensions:window-manager-focus-event))
  (declare (ignore sheet event))
  nil)

(defmethod handle-event :before ((pane push-button-pane)
                                 (event pointer-button-press-event))
  (declare (ignore event))
  (setf (slot-value pane 'climi::armed) t))

(defmethod handle-event :before ((pane push-button-pane)
                                 (event pointer-button-release-event))
  (declare (ignore event))
  (setf (slot-value pane 'climi::armed) t))

(defmethod handle-event :after ((pane push-button-pane)
                                (event pointer-button-release-event))
  (declare (ignore event))
  (%trace-curses-event "push-button release label=~S armed=~S pressed=~S"
                       (gadget-label pane)
                       (slot-value pane 'climi::armed)
                       (slot-value pane 'climi::pressedp)))

(defmethod dispatch-event :after ((pane push-button-pane)
                                  (event pointer-button-release-event))
  (declare (ignore event))
  (%trace-curses-event "dispatch push-button release label=~S armed=~S pressed=~S"
                       (gadget-label pane)
                       (slot-value pane 'climi::armed)
                       (slot-value pane 'climi::pressedp))
  (when (and (slot-value pane 'climi::armed)
             (slot-value pane 'climi::pressedp))
    (setf (slot-value pane 'climi::pressedp) nil)
    (activate-callback pane (gadget-client pane) (gadget-id pane))))

(defmethod dispatch-event :before ((pane push-button-pane)
                                   (event pointer-button-press-event))
  (declare (ignore event))
  (setf (slot-value pane 'climi::armed) t
        (slot-value pane 'climi::pressedp) t))

(defmethod dispatch-event :before ((pane push-button-pane)
                                   (event pointer-button-release-event))
  (declare (ignore event))
  (setf (slot-value pane 'climi::armed) t))

(defun %read-curses-event (port)
  (let* ((window (charms-port-window port))
         (root-sheet (%event-root-sheet-for-port port)))
    (when (and window root-sheet)
      (let ((code (ll:wgetch (charms-window-pointer window))))
        (cond ((or (null code) (minusp code)) nil)
              ((eql code ll:KEY_MOUSE)
               (multiple-value-bind (bstate x y z id)
                   (ll:getmouse)
                 (declare (ignore z id))
                 (when (typep (port-pointer port) 'charms-pointer)
                   (setf (charms-pointer-x (port-pointer port)) (round x)
                         (charms-pointer-y (port-pointer port)) (round y)))
                 (let ((event (charms-mouse-event->clim-event
                               port root-sheet bstate x y 0 0)))
                   (%trace-curses-event "mouse bstate=~X x=~A y=~A event=~S"
                                        bstate x y event)
                   (if (and (or (typep event 'pointer-button-press-event)
                                (typep event 'pointer-button-release-event)
                                (typep event 'pointer-click-event))
                            (not (%mouse-position-report-p bstate)))
                       (progn
                         (%queue-event port
                                       (%make-mouse-button-event
                                        'pointer-button-release-event
                                        port root-sheet bstate x y))
                         (%make-mouse-button-event
                          'pointer-button-press-event
                          port root-sheet bstate x y))
                       event))))
              ((eql code ll:KEY_RESIZE)
               (multiple-value-bind (width height)
                   (terminal-size-from-window window)
                 (setf (charms-port-width port) width
                       (charms-port-height port) height)
                 (make-instance 'window-configuration-event
                                :sheet root-sheet
                                :region (make-bounding-rectangle
                                         0 0 width height))))
              (t
               (multiple-value-bind (key-name key-char modifiers)
                   (charms-key-code->gesture code)
                 (%trace-curses-event "key code=~A name=~S char=~S"
                                      code key-name key-char)
                 (if (eql code 27)
                     (or (let ((event (%read-raw-xterm-mouse-event
                                       port root-sheet window)))
                           (when event
                             (%trace-curses-event "raw xterm mouse event=~S"
                                                  event))
                           event)
                         (%key-press-event port key-name key-char modifiers))
                     (%key-press-event port key-name key-char modifiers)))))))))
