;;;; Opt-in real CLX/Xvfb proof for the public McCLIM font-listing boundary.
;;;; The shell wrapper supplies QUICKLISP setup, project registry, and DISPLAY.

(ql:quickload :rplaca)

(let* ((port (or (clim:find-port)
                 (error "CLIM did not create a port for DISPLAY=~S."
                        (uiop:getenv "DISPLAY"))))
       (families (clim-extensions:port-all-font-families port))
       (native-class-name
         (or (find-symbol "TRUETYPE-FACE" "MCCLIM-TRUETYPE")
             (error "Pinned McCLIM did not define TRUETYPE-FACE.")))
       (native-class
         (or (find-class native-class-name nil)
             (error "Pinned McCLIM did not define the native TTF class.")))
       (native-face
         (loop :for family :in families
               :thereis
               (find-if (lambda (face) (typep face native-class))
                        (clim-extensions:font-family-all-faces family))))
       (listed-sizes
         (and native-face
              (clim-extensions:font-face-all-sizes native-face)))
       (inventory (rplaca:enumerate-port-font-inventory port))
       (choices (rplaca:appearance-font-inventory-choices inventory))
       (choice
         (and native-face listed-sizes
              (find-if
               (lambda (candidate)
                 (and
                  (string=
                   (rplaca:enumerated-font-choice-family-display candidate)
                   (clim-extensions:font-family-name
                    (clim-extensions:font-face-family native-face)))
                  (string=
                   (rplaca:enumerated-font-choice-face-display candidate)
                   (clim-extensions:font-face-name native-face))
                  (= (rplaca:enumerated-font-choice-size candidate)
                     (first listed-sizes))))
               choices)))
       (graft (clim:find-graft :port port)))
  ;; The general runtime may retain a valid empty inventory for stale resources.
  ;; This pinned Guix regression is stricter: it must prove that the compatibility
  ;; path did not silently discard every native TrueType face.
  (unless (and native-face listed-sizes choice (plusp (length choices)))
    (error "No usable listed-size native TrueType choice survived enumeration."))
  (let ((medium (clim:make-medium port graft)))
    (unwind-protect
         (let* ((style
                  (rplaca:resolve-enumerated-font-choice
                   inventory choice :medium medium :scope :transcript))
                (mapping (clim:text-style-mapping port style))
                (ascent (clim:text-style-ascent style medium))
                (descent (clim:text-style-descent style medium))
                (width (clim:text-style-width style medium))
                (fixed-width-p (clim:text-style-fixed-width-p style medium)))
           (unless (and (typep style 'clim:text-style) mapping
                        (realp ascent) (not (minusp ascent))
                        (realp descent) (not (minusp descent))
                        (realp width) (plusp width))
             (error "Descriptor metrics round trip failed for ~S." choice))
           (format t
                   "CLX_FONT_INVENTORY_PROBE_OK count=~D native-ttf=true listed-sizes=~D family=~S face=~S size=~S mapping=~S ascent=~S descent=~S width=~S fixed-width=~S~%"
                   (length choices) (length listed-sizes)
                   (rplaca:enumerated-font-choice-family-display choice)
                   (rplaca:enumerated-font-choice-face-display choice)
                   (rplaca:enumerated-font-choice-size choice)
                   mapping ascent descent width fixed-width-p))
      (clim:deallocate-medium port medium))))
