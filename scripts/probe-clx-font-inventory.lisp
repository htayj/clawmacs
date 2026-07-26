;;;; Opt-in real CLX/Xvfb proof for the public McCLIM font-listing boundary.
;;;; The shell wrapper supplies QUICKLISP setup, project registry, and DISPLAY.

(ql:quickload :clawmacs)

(let* ((port (or (clim:find-port)
                 (error "CLIM did not create a port for DISPLAY=~S."
                        (uiop:getenv "DISPLAY"))))
       (inventory (clawmacs:enumerate-port-font-inventory port))
       (choices (clawmacs:appearance-font-inventory-choices inventory))
       (choice (first choices))
       (graft (clim:find-graft :port port)))
  (unless choice
    (error "The CLX port exposed no public enumerated font choices."))
  (clim:with-sheet-medium (medium graft)
    (let* ((style
             (clawmacs:resolve-enumerated-font-choice
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
              "CLX_FONT_INVENTORY_PROBE_OK count=~D family=~S face=~S size=~S mapping=~S ascent=~S descent=~S width=~S fixed-width=~S~%"
              (length choices)
              (clawmacs:enumerated-font-choice-family-display choice)
              (clawmacs:enumerated-font-choice-face-display choice)
              (clawmacs:enumerated-font-choice-size choice)
              mapping ascent descent width fixed-width-p))))
