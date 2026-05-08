(in-package #:mcclim-charms/tests)

(in-suite mcclim-charms-suite)

(defparameter *expected-clim-examples*
  '("package"
    "text-size-util"
    "seos-baseline"
    "seos-wrfg"
    "calculator"
    "colorslider"
    "menu-test"
    "address-book"
    "av-dingus"
    "traffic-lights"
    "clim-fig"
    "puzzle"
    "transformations-test"
    "town-example"
    "tabdemo"
    "tabledemo"
    "image-transform-demo"
    "stream-test"
    "presentation-test"
    "dragndrop"
    "gadget-test"
    "text-gadgets"
    "method-browser"
    "stopwatch"
    "dragndrop-translator"
    "draggable-graph"
    "text-size-test"
    "drawing-benchmark"
    "logic-cube"
    "checkers"
    "views"
    "font-selector"
    "bordered-output-examples"
    "borders-and-outlines"
    "misc-tests"
    "drawing-tests"
    "pixmaps"
    "render-image-tests"
    "image-viewer"
    "accepting-values-test"
    "graph-toy"
    "coordinate-swizzling"
    "hierarchy-tool"
    "patterns"
    "flipping-ink"
    "patterns-overlap"
    "text-transformation-test"
    "indentation"
    "selection"
    "frame-sheet-name-test"
    "dnd-commented"
    "tracking-pointer"
    "sheet-geometry"
    "file-manager"
    "presentation-translators-test"
    "graph-formatting-test"
    "asynchronous-commands"
    "reinitialize-frame"
    "nested-clipping"
    "indirect-gestures"
    "timer-gestures"
    "wrfg-test"
    "unique-id-test"
    "animation-pulse"
    "concurrent-draw"
    "concurrent-text"
    "concurrent-grid"
    "modifier"
    "slider-test"
    "small-tests"
    "demodemo"))

(test examples-manifest-covers-clim-examples-asd
  (let ((manifest (backend:charms-example-manifest)))
    (is (equal *expected-clim-examples*
               (mapcar (lambda (entry) (getf entry :file)) manifest)))
    (is (every (lambda (entry)
                 (and (getf entry :load)
                      (getf entry :launch)
                      (getf entry :snapshot)
                      (getf entry :scripted-interaction)
                      (getf entry :interactions)
                      (getf entry :shutdown)))
               manifest))
    (is (every (lambda (entry)
                 (let ((scenario (getf entry :interactions)))
                   (and (member :resize scenario)
                        (member :timer scenario)
                        (member :wakeup scenario)
                        (member :shutdown scenario)
                        (or (not (getf entry :interactive))
                            (and (member :keyboard scenario)
                                 (member :special-keys scenario)
                                 (member :mouse-click scenario)
                                 (member :mouse-drag scenario)
                                 (member :pointer-motion scenario))))))
               manifest))))
