(in-package :rplaca/tests)

(in-suite appearance-suite)

;;;; Public-protocol fakes.  The production resolver calls only the documented
;;;; McCLIM extension and CLIM generic functions; these subclasses make their
;;;; semantics deterministic without an X server.

(defclass test-font-port ()
  ((families :initarg :families :accessor test-font-port-families)
   (invalidations :initform 0 :accessor test-font-port-invalidations)
   (mapping-valid-p :initarg :mapping-valid-p :initform t
                    :accessor test-font-port-mapping-valid-p)))

(defclass test-font-family (clim-extensions:font-family)
  ((faces :initarg :faces :initform nil :accessor test-font-family-faces)))

(defclass test-font-face (clim-extensions:font-face)
  ((sizes :initarg :sizes :reader test-font-face-sizes)
   (scalable-p :initarg :scalable-p :reader test-font-face-scalable-p)))

(defclass unreadable-test-font-face (test-font-face) ())

(defclass protocol-gap-test-font-face (clim-extensions:font-face)
  ((sizes :initarg :sizes :reader protocol-gap-test-font-face-sizes)))

(defclass unreadable-test-font-family (test-font-family) ())

(defclass invalid-test-font-family (test-font-family) ())

(defclass unreadable-test-font-port (test-font-port) ())

(define-condition unreadable-test-font-stream (stream-error) ())

(defclass test-font-medium ()
  ((ascent :initarg :ascent :initform 8 :reader test-font-medium-ascent)
   (descent :initarg :descent :initform 2 :reader test-font-medium-descent)
   (width :initarg :width :initform 8 :reader test-font-medium-width)
   (fixed-p :initarg :fixed-p :initform t :reader test-font-medium-fixed-p)))

(defmethod clim-extensions:port-all-font-families
    ((port test-font-port) &key invalidate-cache &allow-other-keys)
  (when invalidate-cache (incf (test-font-port-invalidations port)))
  (test-font-port-families port))

(defmethod clim-extensions:font-family-all-faces ((family test-font-family))
  (test-font-family-faces family))

(defmethod clim-extensions:font-family-all-faces
    ((family unreadable-test-font-family))
  (declare (ignore family))
  (error 'unreadable-test-font-stream))

(defmethod clim-extensions:font-family-all-faces
    ((family invalid-test-font-family))
  (declare (ignore family))
  (error "invalid public family object"))

(defmethod clim-extensions:port-all-font-families
    ((port unreadable-test-font-port) &key invalidate-cache &allow-other-keys)
  (declare (ignore port invalidate-cache))
  (error 'unreadable-test-font-stream))

(defmethod clim-extensions:font-face-all-sizes ((face test-font-face))
  (copy-list (test-font-face-sizes face)))

(defmethod clim-extensions:font-face-all-sizes ((face unreadable-test-font-face))
  (declare (ignore face))
  ;; Models a public McCLIM descriptor whose backing font stream has gone
  ;; away.  Enumeration must retain independent usable faces and must never
  ;; let this implementation detail unwind the GUI event that requested it.
  (error 'unreadable-test-font-stream))

(defmethod clim-extensions:font-face-all-sizes
    ((face protocol-gap-test-font-face))
  (copy-list (protocol-gap-test-font-face-sizes face)))

(defmethod clim-extensions:font-face-scalable-p ((face test-font-face))
  (test-font-face-scalable-p face))

(defmethod clim-extensions:font-face-text-style ((face test-font-face) &optional size)
  (clim:make-text-style (clim-extensions:font-family-name
                         (clim-extensions:font-face-family face))
                        (clim-extensions:font-face-name face)
                        size))

(defmethod clim-extensions:font-face-text-style
    ((face protocol-gap-test-font-face) &optional size)
  (clim:make-text-style (clim-extensions:font-family-name
                         (clim-extensions:font-face-family face))
                        (clim-extensions:font-face-name face)
                        size))

(defmethod clim:text-style-mapping ((port test-font-port) (style clim:text-style)
                                    &optional character-set)
  (declare (ignore style character-set))
  (and (test-font-port-mapping-valid-p port) :mapped))

(defmethod clim:text-style-ascent ((style clim:text-style) (medium test-font-medium))
  (declare (ignore style)) (test-font-medium-ascent medium))

(defmethod clim:text-style-descent ((style clim:text-style) (medium test-font-medium))
  (declare (ignore style)) (test-font-medium-descent medium))

(defmethod clim:text-style-width ((style clim:text-style) (medium test-font-medium))
  (declare (ignore style)) (test-font-medium-width medium))

(defmethod clim:text-style-fixed-width-p ((style clim:text-style) (medium test-font-medium))
  (declare (ignore style)) (test-font-medium-fixed-p medium))

(defun make-test-font-family (port name faces)
  (let ((family (make-instance 'test-font-family :port port :name name)))
    (setf (test-font-family-faces family) faces)
    family))

(defun make-test-font-face (family name &key (sizes '(10 12)) scalable-p)
  (make-instance 'test-font-face :family family :name name
                 :sizes sizes :scalable-p scalable-p))

(defun make-test-font-inventory (&key scalable-p (sizes '(10 12))
                                      (mapping-valid-p t) duplicate-family-p
                                      (metric-medium
                                        (make-instance 'test-font-medium)))
  (let* ((port (make-instance 'test-font-port :families nil
                              :mapping-valid-p mapping-valid-p))
         (first (make-test-font-family port "Test Family" nil))
         (first-face (make-test-font-face first "Regular"
                                          :sizes sizes :scalable-p scalable-p)))
    (setf (test-font-family-faces first) (list first-face)
          (test-font-port-families port)
          (if duplicate-family-p
              (let* ((second (make-test-font-family port "Test Family" nil))
                     (second-face (make-test-font-face second "Regular"
                                                       :sizes sizes :scalable-p scalable-p)))
                (setf (test-font-family-faces second) (list second-face))
                (list first second))
              (list first)))
    (values port (enumerate-port-font-inventory
                  port :metric-medium metric-medium))))

(defun test-enumerated-choice (&optional (size 10))
  (make-enumerated-font-choice :family-display "Test Family"
                               :face-display "Regular" :size size))

(test enumerated-font-choices-are-sorted-data-only-and-defensively-copied
  (let* ((port (make-instance 'test-font-port :families nil))
         (family (make-test-font-family port "Zed" nil))
         (face (make-test-font-face family "Regular" :sizes '(12 10))))
    (setf (test-font-family-faces family) (list face)
          (test-font-port-families port) (list family))
    (let* ((inventory (enumerate-port-font-inventory port))
           (choices (appearance-font-inventory-choices inventory)))
      (is (equal '("Zed" "Regular" 10)
                 (list (enumerated-font-choice-family-display (first choices))
                       (enumerated-font-choice-face-display (first choices))
                       (enumerated-font-choice-size (first choices)))))
      (setf (char (enumerated-font-choice-family-display (first choices)) 0) #\X)
      (is (string= "Zed"
                   (enumerated-font-choice-family-display
                    (first (appearance-font-inventory-choices inventory)))))
      (is (eq :enumerated (font-choice-kind (first choices)))))))

(test font-inventory-isolates-an-unreadable-public-face
  (let* ((port (make-instance 'test-font-port :families nil))
         (family (make-test-font-family port "Mixed" nil))
         (usable (make-test-font-face family "Readable" :sizes '(12)))
         (unreadable (make-instance 'unreadable-test-font-face
                                    :family family :name "Stale"
                                    :sizes '(12) :scalable-p nil)))
    (setf (test-font-family-faces family) (list unreadable usable)
          (test-font-port-families port) (list family))
    (let* ((inventory (enumerate-port-font-inventory port :generation 7))
           (choices (appearance-font-inventory-choices inventory)))
      (is (= 7 (appearance-font-inventory-generation inventory)))
      (is (= 1 (length choices)))
      (is (string= "Readable"
                   (enumerated-font-choice-face-display (first choices)))))))

(test font-inventory-all-unreadable-faces-yields-a-valid-empty-choice-set
  (let* ((port (make-instance 'test-font-port :families nil))
         (family (make-test-font-family port "Unavailable" nil))
         (unreadable (make-instance 'unreadable-test-font-face
                                    :family family :name "Stale"
                                    :sizes '(12) :scalable-p nil)))
    (setf (test-font-family-faces family) (list unreadable)
          (test-font-port-families port) (list family))
    (let ((inventory (enumerate-port-font-inventory port :generation 7)))
      (is (= 7 (appearance-font-inventory-generation inventory)))
      (is (null (appearance-font-inventory-choices inventory))))))

(test font-inventory-uses-listed-sizes-without-scalable-p-method
  "Pinned McCLIM native TrueType faces omit the documented scalable predicate."
  (let* ((port (make-instance 'test-font-port :families nil))
         (family (make-test-font-family port "Protocol Gap" nil))
         (face (make-instance 'protocol-gap-test-font-face
                              :family family :name "Regular"
                              :sizes '(14 10 12))))
    (setf (test-font-family-faces family) (list face)
          (test-font-port-families port) (list family))
    (let ((choices
            (appearance-font-inventory-choices
             (enumerate-port-font-inventory port))))
      (is (equal '(10 12 14)
                 (mapcar #'enumerated-font-choice-size choices))))))

(test font-inventory-isolates-an-unreadable-public-family
  (let* ((port (make-instance 'test-font-port :families nil))
         (unreadable
           (make-instance 'unreadable-test-font-family
                          :port port :name "Stale Family"))
         (usable (make-test-font-family port "Readable Family" nil))
         (face (make-test-font-face usable "Regular" :sizes '(12))))
    (setf (test-font-family-faces usable) (list face)
          (test-font-port-families port) (list unreadable usable))
    (let ((choices
            (appearance-font-inventory-choices
             (enumerate-port-font-inventory port))))
      (is (= 1 (length choices)))
      (is (string= "Readable Family"
                   (enumerated-font-choice-family-display
                    (first choices)))))))

(test font-inventory-treats-an-unreadable-port-cache-as-valid-empty-data
  (let* ((port (make-instance 'unreadable-test-font-port :families nil))
         (inventory
           (enumerate-port-font-inventory port :generation 9)))
    (is (= 9 (appearance-font-inventory-generation inventory)))
    (is (null (appearance-font-inventory-choices inventory)))))

(test font-inventory-does-not-hide-non-stream-family-errors
  (let* ((port (make-instance 'test-font-port :families nil))
         (invalid
           (make-instance 'invalid-test-font-family
                          :port port :name "Invalid Family")))
    (setf (test-font-port-families port) (list invalid))
    (signals error (enumerate-port-font-inventory port))))

(test enumerated-font-resolution-is-exact-and-reports-family-face-ambiguity
  (multiple-value-bind (port inventory) (make-test-font-inventory)
    (declare (ignore port))
    (signals missing-font-family
      (resolve-enumerated-font-choice
       inventory (make-enumerated-font-choice :family-display "other"
                                               :face-display "Regular" :size 10)))
    (signals missing-font-face
      (resolve-enumerated-font-choice
       inventory (make-enumerated-font-choice :family-display "Test Family"
                                               :face-display "Other" :size 10))))
  (multiple-value-bind (port inventory) (make-test-font-inventory :duplicate-family-p t)
    (declare (ignore port))
    (signals ambiguous-font-family
      (resolve-enumerated-font-choice inventory (test-enumerated-choice)))))

(test enumerated-font-resolution-validates-listed-size-boundaries
  (multiple-value-bind (port inventory) (make-test-font-inventory :sizes '(10 12))
    (declare (ignore port))
    (is (typep (resolve-enumerated-font-choice inventory (test-enumerated-choice 10))
               'clim:text-style))
    (signals invalid-fixed-font-size
      (resolve-enumerated-font-choice inventory (test-enumerated-choice 11))))
  ;; Even when the backend face would report itself scalable, enumeration uses
  ;; only its public listed sizes because pinned McCLIM's native TrueType face
  ;; does not implement FONT-FACE-SCALABLE-P.
  (multiple-value-bind (port inventory) (make-test-font-inventory :scalable-p t :sizes '(10))
    (declare (ignore port))
    (is (typep (resolve-enumerated-font-choice inventory (test-enumerated-choice 10))
               'clim:text-style))
    (signals invalid-fixed-font-size
      (resolve-enumerated-font-choice inventory (test-enumerated-choice 11)))))

(test enumerated-font-resolution-validates-mapping-metrics-and-editable-width
  (multiple-value-bind (port inventory) (make-test-font-inventory :mapping-valid-p nil)
    (declare (ignore port))
    (signals font-mapping-invalid
      (resolve-enumerated-font-choice inventory (test-enumerated-choice))))
  (multiple-value-bind (port inventory) (make-test-font-inventory)
    (declare (ignore port))
    (signals font-metrics-invalid
      (resolve-enumerated-font-choice inventory (test-enumerated-choice)
                                      :medium (make-instance 'test-font-medium :width 0)))
    (signals fixed-width-font-required
      (resolve-enumerated-font-choice inventory (test-enumerated-choice)
                                      :medium (make-instance 'test-font-medium :fixed-p nil)
                                      :scope :compose))
    (is (typep (resolve-enumerated-font-choice inventory (test-enumerated-choice)
                                               :medium (make-instance 'test-font-medium :fixed-p nil)
                                               :scope :transcript)
               'clim:text-style))))

(test enumerated-font-resolution-requires-metric-context-without-cache-poisoning
  "Every successful named-font resolution validates metrics and editable width."
  (multiple-value-bind (port inventory)
      (make-test-font-inventory :metric-medium nil)
    (declare (ignore port))
    (dolist (scope '(:compose :minibuffer :transcript))
      (let ((condition
              (handler-case
                  (progn
                    (resolve-enumerated-font-choice
                     inventory (test-enumerated-choice) :scope scope)
                    nil)
                (font-metric-medium-unavailable (caught) caught))))
        (is (typep condition 'font-metric-medium-unavailable))
        (is (eq :font-metric-medium
                (appearance-condition-axis condition)))
        (is (equal (list :choice '("Test Family" "Regular" 10)
                         :scope scope)
                   (appearance-condition-value condition)))))
    ;; A missing frame metric context is infrastructure state, not a negative
    ;; fact about the named font.  Retrying with a valid context must succeed.
    (is (= 0 (hash-table-count
              (rplaca::%appearance-font-inventory-negative-cache inventory))))
    (is (typep
         (resolve-enumerated-font-choice
          inventory (test-enumerated-choice)
          :medium (make-instance 'test-font-medium)
          :scope :compose)
         'clim:text-style))))

(test font-negative-cache-is-inventory-local-and-ports-do-not-leak
  (multiple-value-bind (first-port first) (make-test-font-inventory)
    (multiple-value-bind (second-port second) (make-test-font-inventory)
      (declare (ignore first-port second-port))
      (signals missing-font-family
        (resolve-enumerated-font-choice
         first (make-enumerated-font-choice :family-display "missing"
                                             :face-display "Regular" :size 10)))
      (is (= 1 (hash-table-count (rplaca::%appearance-font-inventory-negative-cache first))))
      (is (= 0 (hash-table-count (rplaca::%appearance-font-inventory-negative-cache second))))
      (is (typep (resolve-enumerated-font-choice second (test-enumerated-choice))
                 'clim:text-style)))))

(test font-inventory-explicitly-invalidates-only-the-requested-port
  (multiple-value-bind (port inventory) (make-test-font-inventory)
    (declare (ignore inventory))
    (enumerate-port-font-inventory port :invalidate-cache t)
    (is (= 1 (test-font-port-invalidations port)))))
