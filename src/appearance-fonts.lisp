(in-package :clawmacs)

;;;; Port-local named font inventory
;;;;
;;;; CLIM typography is portable data.  McCLIM's font-listing extension is not:
;;;; its family and face objects belong to one backend port and its user-facing
;;;; names need not be usable as CLIM text-style components.  This file keeps
;;;; those backend objects private to an inventory and exports data descriptors
;;;; only.  It never changes a text-style mapping.

(define-condition missing-font-family (font-unavailable) ())
(define-condition missing-font-face (font-unavailable) ())
(define-condition invalid-scalable-font-size (font-size-unavailable) ())
(define-condition invalid-fixed-font-size (font-size-unavailable) ())
(define-condition font-mapping-invalid (font-unavailable) ())
(define-condition font-metrics-invalid (font-unavailable) ())
(define-condition font-metric-medium-unavailable (font-metrics-invalid) ())
(define-condition fixed-width-font-required (font-unavailable) ())

(defstruct (portable-font-descriptor
            (:constructor %make-portable-font-descriptor (&key family face size))
            (:conc-name %portable-font-descriptor-))
  "Portable CLIM family/face/size data, separate from enumerated port fonts."
  family face size)

(defun make-portable-font-descriptor (&key family face size)
  "Construct a copied portable CLIM typography descriptor."
  (%make-portable-font-descriptor
   :family (copy-appearance-value family)
   :face (copy-appearance-value face)
   :size (copy-appearance-value size)))

(defun portable-font-descriptor-family (descriptor)
  (copy-appearance-value (%portable-font-descriptor-family descriptor)))

(defun portable-font-descriptor-face (descriptor)
  (copy-appearance-value (%portable-font-descriptor-face descriptor)))

(defun portable-font-descriptor-size (descriptor)
  (copy-appearance-value (%portable-font-descriptor-size descriptor)))

(defstruct (enumerated-font-choice
            (:constructor %make-enumerated-font-choice
                (&key family-display face-display size))
            (:conc-name %enumerated-font-choice-))
  "Immutable user choice, never a backend font-family or font-face object."
  (family-display "" :type string :read-only t)
  (face-display "" :type string :read-only t)
  (size 0 :type real :read-only t))

(defun valid-enumerated-font-size-p (size)
  "Return true for a finite, strictly positive named-font size."
  (and (realp size) (plusp size)))

(defun make-enumerated-font-choice (&key family-display face-display size)
  "Construct a safe, immutable enumerated font choice.

Display names deliberately remain display names; they are resolved only by
exact comparison against one target port's public font inventory."
  (unless (and (stringp family-display) (plusp (length family-display))
               (stringp face-display) (plusp (length face-display))
               (realp size))
    (error-appearance-condition 'invalid-appearance-component
                                :axis :enumerated-font-choice
                                :value (list family-display face-display size)))
  (%make-enumerated-font-choice :family-display (copy-seq family-display)
                                :face-display (copy-seq face-display)
                                :size size))

(defun enumerated-font-choice-family-display (choice)
  (copy-seq (%enumerated-font-choice-family-display choice)))

(defun enumerated-font-choice-face-display (choice)
  (copy-seq (%enumerated-font-choice-face-display choice)))

(defun enumerated-font-choice-size (choice)
  (%enumerated-font-choice-size choice))

(defun font-choice-kind (choice)
  (typecase choice
    (portable-font-descriptor :portable)
    (enumerated-font-choice :enumerated)
    (t (error-appearance-condition 'invalid-appearance-component
                                   :axis :font-choice :value choice))))

(defstruct (%appearance-font-entry
            (:constructor %make-appearance-font-entry
                (&key family-display face-display family face scalable-p sizes)))
  ;; FACE is the sole retained backend object.  It is intentionally private.
  family-display face-display family face scalable-p sizes)

(defstruct (appearance-font-inventory
            (:constructor %make-appearance-font-inventory
                (&key port generation entries choices metric-medium negative-cache))
            (:conc-name %appearance-font-inventory-))
  "Frame-owned opaque port inventory with data-only public choice access."
  port
  (generation 0 :type (integer 0 *) :read-only t)
  (entries nil :type list :read-only t)
  (choices nil :type list :read-only t)
  ;; Private frame-local validation context.  Public inventory accessors never
  ;; expose the pane medium.
  metric-medium
  negative-cache)

(defun appearance-font-inventory-generation (inventory)
  (%appearance-font-inventory-generation inventory))

(defun appearance-font-inventory-choices (inventory)
  "Return fresh immutable choice data in deterministic display order."
  (mapcar (lambda (choice)
            (make-enumerated-font-choice
             :family-display (%enumerated-font-choice-family-display choice)
             :face-display (%enumerated-font-choice-face-display choice)
             :size (%enumerated-font-choice-size choice)))
          (%appearance-font-inventory-choices inventory)))

(defun enumerated-font-choice-key (choice)
  (list (%enumerated-font-choice-family-display choice)
        (%enumerated-font-choice-face-display choice)
        (%enumerated-font-choice-size choice)))

(defun font-choice< (left right)
  (let ((left-family (%enumerated-font-choice-family-display left))
        (right-family (%enumerated-font-choice-family-display right))
        (left-face (%enumerated-font-choice-face-display left))
        (right-face (%enumerated-font-choice-face-display right)))
    (or (string< left-family right-family)
        (and (string= left-family right-family)
             (or (string< left-face right-face)
                 (and (string= left-face right-face)
                      (< (%enumerated-font-choice-size left)
                         (%enumerated-font-choice-size right))))))))

(defun public-font-sizes (face scalable-p)
  "Copy and sort public FACE sizes; omit an unreadable public descriptor.

The public McCLIM font protocol may expose a family whose backing font stream
has already been closed by the backend.  That is an unavailable descriptor,
not a reason to let a refresh event unwind the application frame.  Keep the
inventory conservative: a face without readable sizes contributes no selectable
choice, while other public faces on the same port remain available."
  (handler-case
      (let ((sizes (remove-if-not #'valid-enumerated-font-size-p
                                  (copy-list
                                   (clim-extensions:font-face-all-sizes face)))))
        (cond (sizes (sort sizes #'<))
              ;; A scalable face has infinitely many valid sizes.  One neutral
              ;; displayed choice keeps the public choice contract finite while
              ;; resolution still accepts every positive requested size.
              (scalable-p (list 12))
              (t nil)))
    ;; SBCL reports a closed backing FD as a STREAM-ERROR.  Catch only that
    ;; unavailable-descriptor condition here; malformed protocol objects and
    ;; programming errors must still surface to the caller.
    (stream-error () nil)))

(defun enumerate-port-font-inventory
    (port &key invalidate-cache generation metric-medium)
  "Enumerate PORT through McCLIM's documented public font protocol.

The returned inventory is local to this invocation.  It contains private
protocol objects solely for exact later resolution and exposes only copied
display descriptors."
  (let ((entries nil) (choices nil))
    (dolist (family (clim-extensions:port-all-font-families
                     port :invalidate-cache invalidate-cache))
      (let ((family-display (copy-seq (clim-extensions:font-family-name family))))
        (dolist (face (clim-extensions:font-family-all-faces family))
          ;; A font backend can retain a stale face object after its source has
          ;; gone away.  Treat each public descriptor independently so a single
          ;; bad TTF cannot prevent a frame-local refresh or unrelated faces.
          (handler-case
              (let* ((face-display (copy-seq (clim-extensions:font-face-name face)))
                     (scalable-p
                       (not (null (clim-extensions:font-face-scalable-p face))))
                     (sizes (public-font-sizes face scalable-p)))
                (push (%make-appearance-font-entry
                       :family-display family-display :face-display face-display
                       :family family :face face :scalable-p scalable-p :sizes sizes)
                      entries)
                (dolist (size sizes)
                  (push (make-enumerated-font-choice
                         :family-display family-display :face-display face-display :size size)
                        choices)))
            (stream-error () nil)))))
    (%make-appearance-font-inventory
     :port port
     :generation (or generation 0)
     :entries entries
     :choices (sort choices #'font-choice<)
     :metric-medium metric-medium
     :negative-cache (make-hash-table :test #'equal))))

(defun matching-font-entries (inventory family-display face-display &key family)
  (remove-if-not
   (lambda (entry)
     (and (string= family-display (%appearance-font-entry-family-display entry))
          (or (null face-display)
              (string= face-display (%appearance-font-entry-face-display entry)))
          (or (null family) (eq family (%appearance-font-entry-family entry)))))
   (%appearance-font-inventory-entries inventory)))

(defun inventory-choice-summaries (inventory &key family-display)
  (mapcar #'enumerated-font-choice-key
          (remove-if-not
           (lambda (choice)
             (or (null family-display)
                 (string= family-display
                           (%enumerated-font-choice-family-display choice))))
           (%appearance-font-inventory-choices inventory))))

(defun signal-font-resolution-error (inventory choice condition-class &key scope)
  "Signal a copied, bounded diagnostic and retain only negative facts locally."
  (let* ((choice-key (enumerated-font-choice-key choice))
         ;; Metric and fixed-width failures depend on the requested scope.
         ;; They must not poison a later noneditable resolution of the same
         ;; descriptor in this otherwise bundle-local cache.
         (key (append choice-key
                      (and scope (list :scope scope))
                      (list :condition condition-class)))
         (cached (gethash key (%appearance-font-inventory-negative-cache inventory)))
         (class (or cached condition-class)))
    (unless cached
      (setf (gethash key (%appearance-font-inventory-negative-cache inventory)) class))
    (error-appearance-condition
     class
     :axis :font-choice
     :value key
     :port (%appearance-font-inventory-port inventory)
     :available-choices (inventory-choice-summaries
                         inventory :family-display (first choice-key))
     :suggested-repairs '(:refresh-font-inventory :choose-listed-font))))

(defun signal-font-metric-medium-unavailable (inventory choice scope)
  "Reject named-font resolution without a public metric context.

This infrastructure failure is deliberately not entered in the inventory's
negative font cache: the same descriptor may be valid when retried from an
adopted frame with its public pane medium."
  (error-appearance-condition
   'font-metric-medium-unavailable
   :axis :font-metric-medium
   :value (list :choice (enumerated-font-choice-key choice) :scope scope)
   :port (%appearance-font-inventory-port inventory)
   :available-choices
   (inventory-choice-summaries
    inventory
    :family-display (%enumerated-font-choice-family-display choice))
   :suggested-repairs '(:use-adopted-frame-pane :retry-font-resolution)))

(defun validate-enumerated-font-metrics (inventory style medium choice scope)
  "Validate public mapping and metrics without changing the port mapping cache."
  (unless (typep style 'clim:text-style)
    (signal-font-resolution-error inventory choice 'font-mapping-invalid :scope scope))
  (let ((mapping (ignore-errors
                   (clim:text-style-mapping
                    (%appearance-font-inventory-port inventory) style))))
    (unless mapping
      (signal-font-resolution-error inventory choice 'font-mapping-invalid :scope scope)))
  (unless medium
    (signal-font-metric-medium-unavailable inventory choice scope))
  (let ((ascent (ignore-errors (clim:text-style-ascent style medium)))
        (descent (ignore-errors (clim:text-style-descent style medium)))
        (width (ignore-errors (clim:text-style-width style medium))))
    (unless (and (realp ascent) (not (minusp ascent))
                 (realp descent) (not (minusp descent))
                 (realp width) (plusp width))
      (signal-font-resolution-error inventory choice 'font-metrics-invalid :scope scope))
    (when (member scope '(:compose :compose-pane :minibuffer :minibuffer-pane))
      (unless (ignore-errors (clim:text-style-fixed-width-p style medium))
        (signal-font-resolution-error
         inventory choice 'fixed-width-font-required :scope scope))))
  style)

(defun resolve-enumerated-font-choice (inventory choice &key medium scope)
  "Resolve CHOICE exactly against INVENTORY and validate its public CLIM style.

Only the resulting text style is returned to internal callers; raw family and
face protocol objects never cross the descriptor boundary.  Noneditable
scopes may use a named font.  Compose and minibuffer additionally require a
fixed-width style, and every named-font change remains restart-required at the
frame activation layer."
  (unless (and (typep inventory 'appearance-font-inventory)
               (typep choice 'enumerated-font-choice))
    (error-appearance-condition 'invalid-appearance-component
                                :axis :font-resolution :value (list inventory choice)))
  (let* ((family-display (%enumerated-font-choice-family-display choice))
         (face-display (%enumerated-font-choice-face-display choice))
         (size (%enumerated-font-choice-size choice))
         (families (matching-font-entries inventory family-display nil)))
    (when (null families)
      (signal-font-resolution-error inventory choice 'missing-font-family :scope scope))
    (let ((unique-families (remove-duplicates
                            (mapcar #'%appearance-font-entry-family families)
                            :test #'eq)))
      (when (> (length unique-families) 1)
        (signal-font-resolution-error inventory choice 'ambiguous-font-family :scope scope))
      (let ((faces (matching-font-entries inventory family-display face-display
                                          :family (first unique-families))))
      (when (null faces)
        (signal-font-resolution-error inventory choice 'missing-font-face :scope scope))
      (when (> (length faces) 1)
        (signal-font-resolution-error inventory choice 'ambiguous-font-face :scope scope))
      (let ((entry (first faces)))
        (if (%appearance-font-entry-scalable-p entry)
            (unless (valid-enumerated-font-size-p size)
              (signal-font-resolution-error inventory choice
                                            'invalid-scalable-font-size :scope scope))
            (unless (member size (%appearance-font-entry-sizes entry) :test #'=)
              (signal-font-resolution-error inventory choice
                                            'invalid-fixed-font-size :scope scope)))
        (let ((style (handler-case
                         (clim-extensions:font-face-text-style
                          (%appearance-font-entry-face entry) size)
                       (error ()
                         (signal-font-resolution-error
                          inventory choice 'font-size-unavailable :scope scope)))))
          (validate-enumerated-font-metrics
           inventory style
           (or medium (%appearance-font-inventory-metric-medium inventory))
           choice scope)))))))
