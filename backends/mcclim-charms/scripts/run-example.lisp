(require :asdf)

(defparameter *example-launchers*
  '(("demodemo" :frame "CLIM-DEMO:DEMODEMO"
     :description "Launcher grid for McCLIM examples")
    ("calculator" :frame "CLIM-DEMO.CALCULATOR:CALCULATOR-APP")
    ("clim-fig" :frame "CLIM-DEMO.CLIM-FIG:CLIM-FIG")
    ("method-browser" :frame "CLIM-DEMO::METHOD-BROWSER")
    ("address-book" :frame "CLIM-DEMO.ADDRESS-BOOK:ADDRESS-BOOK")
    ("dingus" :frame "CLIM-DEMO.DINGUS:DINGUS")
    ("av-dingus" :function "CLIM-DEMO.DINGUS::RUN-DINGUS")
    ("modifier" :frame "CLIM-DEMO.MODIFIER-DEMO:MODIFIER-DEMO")
    ("puzzle" :frame "CLIM-DEMO::PUZZLE")
    ("colorslider" :frame "CLIM-DEMO.COLORSLIDER:COLORSLIDER")
    ("logic-cube" :frame "CLIM-DEMO::LOGIC-CUBE")
    ("checkers" :frame "CLIM-DEMO.CHECKERS:CLIM-CHECKERS")
    ("draggable-graph" :frame "CLIM-DEMO::DRAGGABLE-GRAPH-DEMO")
    ("tabdemo" :frame "CLIM-DEMO.TABDEMO:TABDEMO")
    ("town-example" :frame "CLIM-DEMO.TOWN-EXAMPLE:TOWN-EXAMPLE")
    ("graph-toy" :frame "CLIM-DEMO::GRAPH-TOY")
    ("traffic-lights" :frame "CLIM-DEMO::TRAFFIC-LIGHTS")
    ("image-transform-demo" :frame "CLIM-DEMO.IMAGE-TRANSFORM-DEMO:IMAGE-TRANSFORM-DEMO")
    ("file-manager" :frame "CLIM-DEMO.FILE-MANAGER:FILE-MANAGER")
    ("stopwatch" :frame "CLIM-DEMO.STOPWATCH:STOPWATCH")
    ("sheet-geometry" :frame "CLIM-DEMO.SHEET-GEOMETRY:FRAME")
    ("stream-test" :frame "CLIM-DEMO::STREAM-TEST")
    ("presentation-test" :frame "CLIM-DEMO::SUMMATION")
    ("summation" :frame "CLIM-DEMO::SUMMATION")
    ("menu-test" :frame "CLIM-DEMO.MENU-TEST:MENU-TEST")
    ("slider-test" :frame "CLIM-DEMO.SLIDER-TEST:SLIDER-TEST")
    ("gadget-test" :frame "CLIM-DEMO::GADGET-TEST")
    ("text-gadgets" :frame "CLIM-DEMO.TEXT-GADGETS::MY-FRAME")
    ("accepting-values-test" :frame "CLIM-DEMO.ACCEPTING-VALUES:AV-TEST")
    ("hierarchy-tool" :frame "CLIM-DEMO.HIERARCHY:HIERARCHY")
    ("tracking-pointer" :frame "CLIM-DEMO::TRACKING-POINTER-TEST")
    ("selection" :frame "CLIM-DEMO::SELECTION-DEMO")
    ("dnd-commented" :frame "CLIM-DEMO.DRAG-AND-DROP-EXAMPLE:DND-COMMENTED")
    ("dragndrop" :frame "CLIM-DEMO::DRAGNDROP")
    ("dragndrop-translator" :frame "CLIM-DEMO::DRAG-TEST")
    ("bordered-output-examples" :frame "CLIM-DEMO::BORDERED-OUTPUT")
    ("borders-and-outlines" :frame "CLIM-DEMO.BORDERS-AND-OUTLINES:FRAME")
    ("tabledemo" :frame "CLIM-DEMO.TABLES-WITH-BORDERS:TABLES-WITH-BORDERS")
    ("text-transformation-test" :frame "CLIM-DEMO.DRAW-TEXT-TEST:DRAW-TEXT-TEST")
    ("seos-baseline" :frame "CLIM-DEMO.SEOS-BASELINE:SEOS-BASELINE")
    ("seos-wrfg" :frame "CLIM-DEMO.SEOS-WRFG:SEOS-WRFG")
    ("indentation" :frame "CLIM-DEMO::INDENTATION")
    ("graph-formatting-test" :frame "CLIM-DEMO.GRAPH-FORMATTING-TEST:GRAPH-FORMATTING-TEST")
    ("flipping-ink" :frame "CLIM-DEMO::FLIPPING-INK")
    ("patterns" :frame "CLIM-DEMO.PATTERNS:PATTERN-DESIGN-TEST")
    ("patterns-overlap" :frame "CLIM-DEMO::PATTERNS-OVERLAP")
    ("wrfg-test" :frame "CLIM-DEMO::WRFG-TEST")
    ("nested-clipping" :frame "CLIM-DEMO.NESTED-CLIPPING:NESTED-CLIPPING")
    ("misc-tests" :frame "CLIM-DEMO.MISC:MISC-TESTS")
    ("drawing-tests" :frame "CLIM-DEMO.DRAWING-TESTS:DRAWING-TESTS")
    ("frame-sheet-name-test" :frame "CLIM-DEMO.NAMES-AND-ICONS:FRAME-SHEET-NAME-TEST")
    ("reinitialize-frame" :frame "CLIM-DEMO.REINITIALIZE-FRAME:EXAMPLE-FRAME")
    ("asynchronous-commands" :frame "CLIM-DEMO.EXECUTE-FRAME-COMMAND:HOMOGENOUS")
    ("indirect-gestures" :frame "CLIM-DEMO.INDIRECT-GESTURES:THE-GAME")
    ("timer-gestures" :frame "CLIM-DEMO.TIMER-GESTURES:LE-DOT")
    ("presentation-translators-test" :frame "CLIM-DEMO.PRESENTATION-TRANSLATORS-TEST:PRESENTATION-TRANSLATORS-TEST")
    ("text-size-test" :frame "CLIM-DEMO::TEXT-SIZE-TEST")
    ("render-image-tests" :frame "CLIM-DEMO::RENDER-IMAGE-TESTS")
    ("pixmaps" :frame "CLIM-DEMO.PIXMAPS:PIXMAPS")
    ("drawing-benchmark" :frame "CLIM-DEMO::DRAWING-BENCHMARK")
    ("coordinate-swizzling" :frame "CLIM-DEMO.COORD-SWIZZLING:COORDINATE-SWIZZLING")
    ("unique-id-test" :frame "CLIM-DEMO.UNIQUE-ID-TEST:UNIQUE-ID-TEST")
    ("animation-pulse" :frame "CLIM-DEMO.ANIMATION-PULSE:ANIMATION-PULSE")
    ("concurrent-draw" :frame "CLIM-DEMO.CONCURRENT-DRAW:CONCURRENT-DRAW")
    ("concurrent-text" :frame "CLIM-DEMO.CONCURRENT-TEXT:CONCURRENT-TEXT")
    ("concurrent-grid" :frame "CLIM-DEMO.CONCURRENT-GRID:CONCURRENT-GRID")))

(defun launcher-verbose-p ()
  (member (uiop:getenv "MCCLIM_CHARMS_LAUNCHER_VERBOSE")
          '("1" "true" "TRUE" "yes" "YES")
          :test #'string=))

(defun launcher-log-path ()
  (let* ((script (or *load-pathname* *compile-file-pathname*))
         (backend-root (truename
                        (merge-pathnames "../"
                                         (make-pathname :directory (pathname-directory script))))))
    (merge-pathnames "artifacts/launcher-load.log" backend-root)))

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
      (pushnew (truename mcclim-root) asdf:*central-registry* :test #'equal)
      (pushnew (truename (merge-pathnames "Examples/" mcclim-root))
               asdf:*central-registry*
               :test #'equal))))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: run-example.sh [--list] [--chooser] [--check] EXAMPLE~%")
  (format stream "       run-example.sh demodemo~%~%")
  (format stream "Options:~%")
  (format stream "  --list        Print launchable example names and exit.~%")
  (format stream "  --chooser     Run CLIM-DEMO:DEMODEMO, the McCLIM example browser.~%")
  (format stream "  --check-chooser~%")
  (format stream "                Load systems and resolve CLIM-DEMO:DEMODEMO without running it.~%")
  (format stream "  --check NAME  Load systems and resolve NAME without starting its frame.~%")
  (format stream "  --help        Show this help.~%"))

(defun list-examples ()
  (format t "Launchable McCLIM examples for the charms backend:~%")
  (dolist (entry *example-launchers*)
    (destructuring-bind (name &rest plist) entry
      (format t "  ~A~@[ - ~A~]~%" name (getf plist :description)))))

(defun find-launcher (name)
  (find (string-downcase name) *example-launchers*
        :key #'first
        :test #'string=))

(defun symbol-from-designator (designator)
  (let ((*package* (find-package :cl-user)))
    (read-from-string designator)))

(defun load-examples ()
  (flet ((load-quietly ()
           (let ((*compile-verbose* nil)
                 (*load-verbose* nil))
             (configure-registry)
             (asdf:load-system :mcclim-charms)
             (setf (symbol-value (find-symbol "*DEFAULT-SERVER-PATH*" "CLIM")) '(:charms))
             (asdf:load-system :clim-examples))))
    (if (launcher-verbose-p)
        (load-quietly)
        (let ((log-path (launcher-log-path)))
          (ensure-directories-exist log-path)
          (with-open-file (log log-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (let ((*standard-output* log)
                  (*error-output* log)
                  (*trace-output* log))
              (load-quietly)))))))

(defun resolve-launch-target (entry)
  (destructuring-bind (name &rest plist) entry
    (declare (ignore name))
    (cond ((getf plist :frame)
           (let ((class (symbol-from-designator (getf plist :frame))))
             (unless (find-class class nil)
               (error "Frame class ~A is not defined." class))
             (values :frame class)))
          ((getf plist :function)
           (let ((function (symbol-from-designator (getf plist :function))))
             (unless (fboundp function)
               (error "Function ~A is not defined." function))
             (values :function function)))
          (t
           (error "Launcher entry has no frame or function target: ~S" entry)))))

(defun launch-example (entry check-only)
  (load-examples)
  (multiple-value-bind (kind target)
      (resolve-launch-target entry)
    (format t "Using McCLIM server path ~S~%"
            (symbol-value (find-symbol "*DEFAULT-SERVER-PATH*" "CLIM")))
    (format t "~:[Launching~;Resolved~] ~A ~A~%"
            check-only
            (ecase kind
              (:frame "frame")
              (:function "function"))
            target)
    (force-output)
    (unless check-only
      (ecase kind
        (:frame
         (funcall (symbol-function (find-symbol "RUN-FRAME-TOP-LEVEL" "CLIM"))
                  (funcall (symbol-function (find-symbol "MAKE-APPLICATION-FRAME" "CLIM"))
                           target)))
        (:function
         (funcall target))))))

(defun launch-example-chooser (check-only)
  (load-examples)
  (let ((chooser (symbol-from-designator "CLIM-DEMO:DEMODEMO")))
    (unless (fboundp chooser)
      (error "Example chooser function ~A is not defined." chooser))
    (format t "Using McCLIM server path ~S~%"
            (symbol-value (find-symbol "*DEFAULT-SERVER-PATH*" "CLIM")))
    (format t "~:[Launching~;Resolved~] example chooser function ~A~%"
            check-only
            chooser)
    (force-output)
    (unless check-only
      (let ((frame (funcall (symbol-function
                             (find-symbol "MAKE-APPLICATION-FRAME" "CLIM"))
                            (find-symbol "DEMODEMO" "CLIM-DEMO")
                            :top-level-lambda
                            (lambda (frame)
                              (let* ((sheet
                                       (funcall
                                        (symbol-function
                                         (find-symbol "FRAME-TOP-LEVEL-SHEET"
                                                      "CLIM"))
                                        frame))
                                      (port
                                       (funcall
                                        (symbol-function
                                         (find-symbol "PORT" "CLIM"))
                                        sheet)))
                                (loop
                                  (funcall
                                   (symbol-function
                                    (find-symbol "PROCESS-NEXT-EVENT" "CLIM"))
                                   port)))))))
        (funcall (symbol-function (find-symbol "RUN-FRAME-TOP-LEVEL" "CLIM"))
                 frame)))))

(defun main ()
  (let ((args (remove "--" (uiop:command-line-arguments) :test #'string=)))
    (cond ((or (null args)
               (member "--help" args :test #'string=)
               (member "-h" args :test #'string=))
           (usage)
           (uiop:quit (if args 0 2)))
          ((member "--list" args :test #'string=)
           (list-examples)
           (uiop:quit 0))
          ((member "--chooser" args :test #'string=)
           (launch-example-chooser nil)
           (uiop:quit 0))
          ((member "--check-chooser" args :test #'string=)
           (launch-example-chooser t)
           (uiop:quit 0))
          ((string= (first args) "--check")
           (let ((entry (and (second args) (find-launcher (second args)))))
             (unless entry
               (usage *error-output*)
               (format *error-output* "~&Unknown or missing example for --check.~%")
               (uiop:quit 2))
             (launch-example entry t)
             (uiop:quit 0)))
          (t
           (let ((entry (find-launcher (first args))))
             (unless entry
               (usage *error-output*)
               (format *error-output* "~&Unknown example: ~A~%" (first args))
               (uiop:quit 2))
             (launch-example entry nil)
             (uiop:quit 0))))))

(handler-case
    (main)
  (error (condition)
    (format *error-output* "~&example launch failed: ~A~%" condition)
    (uiop:quit 1)))
