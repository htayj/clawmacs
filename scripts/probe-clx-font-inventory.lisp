;;;; Opt-in real CLX/Xvfb proof for the public McCLIM font-listing boundary.
;;;; The shell wrapper supplies QUICKLISP setup, project registry, and DISPLAY.

(ql:quickload :clawmacs)

(let* ((port (or (clim:find-port)
                 (error "CLIM did not create a port for DISPLAY=~S."
                        (uiop:getenv "DISPLAY"))))
       (inventory (clawmacs:enumerate-port-font-inventory port))
       (choices (clawmacs:appearance-font-inventory-choices inventory))
       (choice (first choices)))
  (unless choice
    (error "The CLX port exposed no public enumerated font choices."))
  (let ((style (clawmacs:resolve-enumerated-font-choice inventory choice)))
    (unless (and (typep style 'clim:text-style)
                 (clim:text-style-mapping port style))
      (error "Descriptor-to-text-style mapping round trip failed for ~S." choice))
    (format t "CLX font inventory probe passed: ~D choices; ~S / ~S / ~S~%"
            (length choices)
            (clawmacs:enumerated-font-choice-family-display choice)
            (clawmacs:enumerated-font-choice-face-display choice)
            (clawmacs:enumerated-font-choice-size choice))))
