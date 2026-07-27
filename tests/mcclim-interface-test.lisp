(in-package :clawmacs/tests)

(in-suite mcclim-interface-suite)

(defmacro with-mcclim-test-function-override
    ((name lambda-list &body implementation) &body body)
  "Temporarily replace NAME during a serial McCLIM unit test."
  (let ((original (gensym "ORIGINAL")))
    `(let ((,original (symbol-function ',name)))
       (unwind-protect
            (progn
              (setf (symbol-function ',name)
                    (lambda ,lambda-list ,@implementation))
              ,@body)
         (setf (symbol-function ',name) ,original)))))

(defun handle-chat-appearance-activation-with-fake-port (frame candidate port)
  "Run one frame-process activation through a deterministic opaque port seam."
  (with-mcclim-test-function-override
      (clawmacs::chat-frame-appearance-live-port (requested-frame)
        (is (eq frame requested-frame))
        port)
    (clawmacs::handle-chat-frame-appearance-activation frame candidate)))

(defun refresh-chat-appearance-bundle-with-fake-port (frame port)
  "Construct a post-adoption bundle through the public-style test resolver."
  (with-mcclim-test-function-override
      (clawmacs::chat-frame-appearance-live-port (requested-frame)
        (is (eq frame requested-frame))
        port)
    (clawmacs::refresh-chat-frame-appearance-port-bundle frame)))

(defun refresh-chat-font-inventory-with-fake-port (frame port &key (invalidate-cache t))
  "Run one complete port-local font transaction through a deterministic fake."
  (with-mcclim-test-function-override
      (clawmacs::chat-frame-appearance-live-port (requested-frame)
        (is (eq frame requested-frame))
        port)
    (with-mcclim-test-function-override
        (clawmacs::chat-frame-font-metric-medium (requested-frame)
          (is (eq frame requested-frame))
          (make-instance 'test-font-medium))
      (clawmacs::refresh-chat-frame-font-inventory
       frame :invalidate-cache invalidate-cache))))

(defclass synthetic-chat-compose-pane
    (clawmacs::clawmacs-chat-compose-pane)
  ((test-frame :accessor synthetic-chat-compose-pane-frame))
  (:metaclass esa-utils:modual-class)
  (:documentation "Initialized compose pane used for headless event tests."))

(defmethod clim:pane-frame ((pane synthetic-chat-compose-pane))
  (synthetic-chat-compose-pane-frame pane))

(defvar *synthetic-compose-gadget-value-writes* nil
  "When numeric, count test writes through the public Drei gadget setter.")

(defvar *synthetic-compose-gadget-value-reads* nil
  "When numeric, count test reads through the public Drei gadget getter.")

(defmethod clim:gadget-value :around ((pane synthetic-chat-compose-pane))
  (when (integerp *synthetic-compose-gadget-value-reads*)
    (incf *synthetic-compose-gadget-value-reads*))
  (call-next-method))

(defmethod (setf clim:gadget-value) :around
    (new-value (pane synthetic-chat-compose-pane) &key invoke-callback)
  (when (integerp *synthetic-compose-gadget-value-writes*)
    (incf *synthetic-compose-gadget-value-writes*))
  (call-next-method new-value pane :invoke-callback invoke-callback))

(defun make-synthetic-chat-compose-pane (frame)
  "Return a headless compose pane whose public PANE-FRAME is FRAME."
  (let ((pane (make-instance 'synthetic-chat-compose-pane
                             :initial-contents "")))
    (setf (synthetic-chat-compose-pane-frame pane) frame)
    pane))

(defun make-geometry-test-chat-compose-pane ()
  "Construct a compose pane with the chat frame's production initargs.

The pane is intentionally not installed in an unadopted application frame:
these tests exercise construction-time space requirements only."
  (let ((height (clawmacs::chat-compose-desired-pixel-height)))
    (make-instance 'clawmacs::clawmacs-chat-compose-pane
                   :initial-contents ""
                   :ncolumns 90
                   :nlines 5
                   :height height
                   :min-height height
                   :max-height height
                   :end-of-line-action :wrap*
                   :minibuffer nil
                   :scroll-bars nil
                   :border-width 0
                   :activation-gestures '(:return)
                   :activate-callback #'clawmacs::compose-pane-activated)))

(defun test-direct-command-table-keystrokes (table)
  "Return stable snapshots of TABLE's non-inherited keystroke entries."
  (let ((entries nil))
    (clim:map-over-command-table-keystrokes
     (lambda (name gesture item)
       (push (list name
                   (copy-tree gesture)
                   (clim:command-menu-item-type item)
                   (copy-tree (clim:command-menu-item-value item)))
             entries))
     table
     :inherited nil)
    (nreverse entries)))

(test classic-appearance-wire-adapter-preserves-the-exact-role-contract
  "Legacy presentation faces map to the resolved v1 role stacks exactly."
  (dolist (entry '((:default-text (:default-text))
                   (:selector-title (:selector-title))
                   (:selector-header (:selector-header))
                   (:selector-entry (:selector-entry))
                   (:selector-selected (:selector-entry :selector-selection))
                   (:selector-separator (:selector-separator))
                   (:selector-footer (:selector-footer))
                   (:tool-result (:tool-result))
                   (:system (:system))
                   (:disabled (:default-text :disabled))
                   (:error (:error))))
    (is (equal (second entry)
               (clawmacs::chat-appearance-wire-role-stack (first entry)))))
  (let* ((frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "classic-appearance-wire"
                                      :session-persistence-mode :ephemeral)))
         (generic-selected
           (clawmacs::chat-frame-resolve-appearance-role
            frame '(:selector-entry :selector-selection)))
         (minibuffer-selected
           (clawmacs::chat-frame-resolve-appearance-role
            frame '(:minibuffer-pane :selector-entry :selector-selection
                    :minibuffer-selection-emphasis)))
         (generic-style (resolved-appearance-role-style generic-selected))
         (minibuffer-style
           (resolved-appearance-role-style minibuffer-selected)))
    (is (equal '(0.10 0.38 0.65)
               (appearance-ink-spec-foreground
                (appearance-role-style-foreground-ink generic-style))))
    (is-true (appearance-unspecified-p
              (appearance-typography-spec-face
               (appearance-role-style-typography generic-style))))
    (is (eq :bold
            (appearance-typography-spec-face
             (appearance-role-style-typography minibuffer-style))))
    (is (equal '(:marker ">")
               (appearance-decoration-spec-parameters
                (appearance-role-style-decoration minibuffer-style))))
    (is-false
     (equal (clawmacs::chat-frame-appearance-role-key
             frame '(:selector-entry :selector-selection))
            (clawmacs::chat-frame-appearance-role-key
             frame '(:minibuffer-pane :selector-entry :selector-selection
                     :minibuffer-selection-emphasis))))
    (is (equal '(:transcript-pane :transcript-user)
               (clawmacs::chat-message-appearance-role-stack
                (make-message :user))))
    (is (equal '(:transcript-pane :transcript-tool)
               (clawmacs::chat-message-appearance-role-stack
                (make-message :tool-result))))))

(test chat-frame-appearance-state-is-isolated-by-frame
  "Profiles, role keys, and runtime unknown-role diagnostics never cross frames."
  (let* ((default-profile (make-appearance-profile))
         (overridden-profile
           (make-appearance-profile
            :role-overrides
            (list
             (cons :transcript-user
                   (make-appearance-role-style
                    :foreground-ink
                    (make-appearance-ink-spec :foreground '(0.80 0.20 0.30)))))))
         (first (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "appearance-frame-one"
                                      :session-persistence-mode :ephemeral)
                 :appearance-profile default-profile))
         (second (clim:make-application-frame
                  'clawmacs::clawmacs-chat-frame
                  :buffer (make-buffer "appearance-frame-two"
                                       :session-persistence-mode :ephemeral)
                  :appearance-profile overridden-profile))
         (first-role (clawmacs::chat-frame-resolve-appearance-role
                      first '(:transcript-pane :transcript-user)))
         (second-role (clawmacs::chat-frame-resolve-appearance-role
                       second '(:transcript-pane :transcript-user))))
    (is (equal '(0.10 0.25 0.55)
               (appearance-ink-spec-foreground
                (appearance-role-style-foreground-ink
                 (resolved-appearance-role-style first-role)))))
    (is (equal '(0.80 0.20 0.30)
               (appearance-ink-spec-foreground
                (appearance-role-style-foreground-ink
                 (resolved-appearance-role-style second-role)))))
    (is-false (eq (clawmacs::chat-frame-appearance-resolved-roles first)
                  (clawmacs::chat-frame-appearance-resolved-roles second)))
    (is-false (equal (clawmacs::chat-frame-appearance-role-key
                      first '(:transcript-pane :transcript-user))
                     (clawmacs::chat-frame-appearance-role-key
                      second '(:transcript-pane :transcript-user))))
    (clawmacs::chat-frame-resolve-appearance-role first '(:unknown-wire-face))
    (clawmacs::chat-frame-resolve-appearance-role first '(:unknown-wire-face))
    (clawmacs::chat-frame-resolve-appearance-role second '(:unknown-wire-face))
    (is (= 1 (length (clawmacs::chat-frame-appearance-runtime-diagnostics first))))
    (is (= 1 (length (clawmacs::chat-frame-appearance-runtime-diagnostics second))))
    (is (equal '(0 :unknown-wire-face)
               (appearance-diagnostic-deduplication-key
                (first (clawmacs::chat-frame-appearance-runtime-diagnostics first)))))))

(test chat-frame-live-appearance-activation-publishes-only-at-the-event-boundary
  "The caller queues an immutable candidate; event handling publishes and invalidates keys."
  (let* ((frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "appearance-live-event"
                                      :session-persistence-mode :ephemeral)))
         (role-stack '(:transcript-pane :transcript-user))
         (old-key (clawmacs::chat-frame-appearance-role-key frame role-stack))
         (old-revision (clawmacs::chat-frame-appearance-revision frame))
         (candidate
           (make-appearance-candidate
            (make-appearance-profile
             :role-overrides
             (list
              (cons :transcript-user
                    (make-appearance-role-style
                     :foreground-ink
                     (make-appearance-ink-spec :foreground '(0.80 0.20 0.30))))))))
         (queued nil)
         (redisplays 0)
         (port (make-symbol "FAKE-PORT")))
    (with-mcclim-test-function-override
        (clawmacs::queue-chat-frame-appearance-activation-event
            (requested-frame requested-candidate)
          (is (eq frame requested-frame))
          (setf queued requested-candidate)
          t)
      (is-true
       (clawmacs::request-chat-frame-appearance-activation frame candidate)))
    ;; Requesting from the caller leaves all active frame state alone.
    (is (eq candidate queued))
    (is (= old-revision (clawmacs::chat-frame-appearance-revision frame)))
    (is (equal old-key
               (clawmacs::chat-frame-appearance-role-key frame role-stack)))
    (with-mcclim-test-function-override
        (clawmacs::request-chat-frame-redisplay (requested-frame)
          (is (eq frame requested-frame))
          (incf redisplays)
          requested-frame)
      ;; This is the body invoked only by the CLIM appearance event handler.
      (let ((result
              (handle-chat-appearance-activation-with-fake-port
               frame queued port)))
        (is (eq :ready (appearance-activation-result-status result)))))
    (is (= (1+ old-revision) (clawmacs::chat-frame-appearance-revision frame)))
    (is (= 1 redisplays))
    (is-false (equal old-key
                     (clawmacs::chat-frame-appearance-role-key frame role-stack)))
    (is (equal '(0.80 0.20 0.30)
               (appearance-ink-spec-foreground
                (appearance-role-style-foreground-ink
                 (resolved-appearance-role-style
                  (clawmacs::chat-frame-resolve-appearance-role
                   frame role-stack))))))))

(test chat-frame-dark-activation-stages-without-partial-active-changes
  "Surface-changing dark stays staged: profile, bundle, and role keys remain active."
  (let* ((frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "appearance-dark-stage"
                                      :session-persistence-mode :ephemeral)))
         (role-stack '(:transcript-pane :transcript-user))
         (old-profile (clawmacs::chat-frame-appearance-profile frame))
         (old-key (clawmacs::chat-frame-appearance-role-key frame role-stack))
         (old-cache (clawmacs::chat-frame-appearance-resolved-roles frame))
         (candidate
           (make-appearance-candidate
            (make-appearance-profile :selected-theme :dark)))
         (port (make-symbol "FAKE-PORT"))
         (initial (refresh-chat-appearance-bundle-with-fake-port frame port))
         (result
           (handle-chat-appearance-activation-with-fake-port frame candidate port)))
    (is (eq :restart-required (appearance-activation-result-status result)))
    (is (eq candidate (clawmacs::chat-frame-appearance-staged-candidate frame)))
    (is (eq :classic (appearance-profile-selected-theme
                      (clawmacs::chat-frame-appearance-profile frame))))
    (is (eq old-profile (clawmacs::chat-frame-appearance-profile frame)))
    (is (eq initial (clawmacs::chat-frame-appearance-active-bundle frame)))
    (is (eq old-cache (clawmacs::chat-frame-appearance-resolved-roles frame)))
    (is (equal old-key
               (clawmacs::chat-frame-appearance-role-key frame role-stack)))))

(test chat-frame-appearance-activation-failure-preserves-active-and-valid-staged-state
  "Resolution failure records a condition but cannot replace active or prior staged state."
  (let* ((frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "appearance-activation-failure"
                                      :session-persistence-mode :ephemeral)))
         (role-stack '(:transcript-pane :transcript-user))
         (old-key (clawmacs::chat-frame-appearance-role-key frame role-stack))
         (dark (make-appearance-candidate
                (make-appearance-profile :selected-theme :dark)))
         (broken (make-appearance-candidate
                  (make-appearance-profile :selected-theme :no-such-theme)))
         (port (make-symbol "FAKE-PORT"))
         (initial (refresh-chat-appearance-bundle-with-fake-port frame port)))
    (handle-chat-appearance-activation-with-fake-port frame dark port)
    (let ((result
            (handle-chat-appearance-activation-with-fake-port frame broken port)))
      (is (eq :failed (appearance-activation-result-status result)))
      (is-true (appearance-activation-result-diagnostics result)))
    (is (eq dark (clawmacs::chat-frame-appearance-staged-candidate frame)))
    (is (eq :classic (appearance-profile-selected-theme
                      (clawmacs::chat-frame-appearance-profile frame))))
    (is (eq initial (clawmacs::chat-frame-appearance-active-bundle frame)))
    (is (equal old-key
               (clawmacs::chat-frame-appearance-role-key frame role-stack)))
    (is-true (clawmacs::chat-frame-appearance-activation-diagnostics frame))))

(test chat-frame-appearance-live-activation-isolated-by-frame-and-noop-is-inert
  "Publishing one frame never clears another frame's cache; no-op has no side effects."
  (let* ((first (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "appearance-live-first"
                                      :session-persistence-mode :ephemeral)))
         (second (clim:make-application-frame
                  'clawmacs::clawmacs-chat-frame
                  :buffer (make-buffer "appearance-live-second"
                                       :session-persistence-mode :ephemeral)))
         (role-stack '(:transcript-pane :transcript-user))
         (second-key (clawmacs::chat-frame-appearance-role-key second role-stack))
         (live
           (make-appearance-candidate
            (make-appearance-profile
             :role-overrides
             (list
              (cons :transcript-user
                    (make-appearance-role-style
                     :foreground-ink
                    (make-appearance-ink-spec :foreground '(0.80 0.20 0.30))))))))
         (first-port (make-symbol "FAKE-FIRST-PORT"))
         (first-result
           (handle-chat-appearance-activation-with-fake-port first live first-port))
         (revision (clawmacs::chat-frame-appearance-revision first))
         (noop-result
           (handle-chat-appearance-activation-with-fake-port
            first (make-appearance-candidate
                   (clawmacs::chat-frame-appearance-profile first)) first-port)))
    (is (eq :ready (appearance-activation-result-status first-result)))
    (is (eq :no-op (appearance-activation-result-status noop-result)))
    (is (= revision (clawmacs::chat-frame-appearance-revision first)))
    (is (eq :classic (appearance-profile-selected-theme
                      (clawmacs::chat-frame-appearance-profile second))))
    (is (equal second-key
               (clawmacs::chat-frame-appearance-role-key second role-stack)))))

(test chat-frame-port-bundle-waits-for-adoption-and-remains-frame-local
  "Construction is profile-only; each adopted frame receives only its own port bundle."
  (let* ((first (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "appearance-port-first"
                                      :session-persistence-mode :ephemeral)))
         (second (clim:make-application-frame
                  'clawmacs::clawmacs-chat-frame
                  :buffer (make-buffer "appearance-port-second"
                                       :session-persistence-mode :ephemeral)))
         (first-port (make-symbol "FIRST-PORT"))
         (second-port (make-symbol "SECOND-PORT")))
    ;; An ordinary unadopted frame has only a portable profile, never a fake
    ;; runtime bundle or a port-derived font inventory.
    (is (null (clawmacs::chat-frame-appearance-active-bundle first)))
    (is (null (clawmacs::refresh-chat-frame-appearance-port-bundle first)))
    (let ((first-bundle
            (refresh-chat-appearance-bundle-with-fake-port first first-port))
          (second-bundle
            (refresh-chat-appearance-bundle-with-fake-port second second-port)))
      (is (eq first-bundle (clawmacs::chat-frame-appearance-active-bundle first)))
      (is (eq second-bundle (clawmacs::chat-frame-appearance-active-bundle second)))
      (is-false (eq first-bundle second-bundle))
      (is (eq first-port (resolved-appearance-bundle-port-identity first-bundle)))
      (is (eq second-port (resolved-appearance-bundle-port-identity second-bundle)))
      (is-false (equal (resolved-appearance-bundle-bundle-key first-bundle)
                       (resolved-appearance-bundle-bundle-key second-bundle)))
      ;; Refreshing one frame's explicitly supplied generation cannot replace
      ;; the other frame's active bundle.
      (setf (slot-value first 'clawmacs::appearance-font-inventory-generation) 1)
      (refresh-chat-appearance-bundle-with-fake-port first first-port)
      (is (= 1 (resolved-appearance-bundle-font-inventory-generation
                (clawmacs::chat-frame-appearance-active-bundle first))))
      (is (eq second-bundle
              (clawmacs::chat-frame-appearance-active-bundle second))))))

(test chat-font-refresh-publishes-only-a-complete-target-frame-transaction
  "A successful explicit refresh changes one frame's generation and bundle only."
  (let* ((first (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "font-refresh-first" :session-persistence-mode :ephemeral)))
         (second (clim:make-application-frame
                  'clawmacs::clawmacs-chat-frame
                  :buffer (make-buffer "font-refresh-second" :session-persistence-mode :ephemeral)))
         (first-port (make-instance 'test-font-port :families nil))
         (second-port (make-instance 'test-font-port :families nil)))
    (dolist (port (list first-port second-port))
      (let* ((family (make-test-font-family port "Test Family" nil))
             (face (make-test-font-face family "Regular" :sizes '(10))))
        (setf (test-font-family-faces family) (list face)
              (test-font-port-families port) (list family))))
    (let ((initial (refresh-chat-font-inventory-with-fake-port first first-port
                                                                :invalidate-cache nil))
          (other (refresh-chat-font-inventory-with-fake-port second second-port
                                                              :invalidate-cache nil)))
      (is (eq :ready (appearance-activation-result-status initial)))
      (is (eq :ready (appearance-activation-result-status other)))
      ;; Production refresh obtains the adopted frame's metric context and
      ;; stores it privately in that frame-local inventory.  A later caller
      ;; therefore cannot bypass compose fixed-width validation by omitting
      ;; :MEDIUM.
      (let* ((inventory
               (clawmacs::chat-frame-appearance-font-inventory first))
             (choice
               (first (appearance-font-inventory-choices inventory))))
        (is (typep
             (resolve-enumerated-font-choice
              inventory choice :scope :compose)
             'clim:text-style)))
      (let ((old-key (resolved-appearance-bundle-bundle-key
                      (clawmacs::chat-frame-appearance-active-bundle first)))
            (other-bundle (clawmacs::chat-frame-appearance-active-bundle second)))
        (let ((result (refresh-chat-font-inventory-with-fake-port first first-port)))
          (is (eq :ready (appearance-activation-result-status result)))
          (is (= 1 (clawmacs::chat-frame-appearance-font-inventory-generation first)))
          (is (= 0 (clawmacs::chat-frame-appearance-font-inventory-generation second)))
          (is (= 1 (test-font-port-invalidations first-port)))
          (is (eq other-bundle (clawmacs::chat-frame-appearance-active-bundle second)))
          (is-false (equal old-key
                           (resolved-appearance-bundle-bundle-key
                            (clawmacs::chat-frame-appearance-active-bundle first)))))))))

(test chat-font-refresh-advances-past-an-unreadable-public-descriptor
  "One stale public face cannot fail the frame-owned refresh transaction."
  (let* ((frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "font-refresh-stale-face"
                                      :session-persistence-mode :ephemeral)))
         (port (make-instance 'test-font-port :families nil))
         (family (make-test-font-family port "Mixed" nil))
         (usable (make-test-font-face family "Readable" :sizes '(10)))
         (unreadable (make-instance 'unreadable-test-font-face
                                    :family family :name "Stale"
                                    :sizes '(10) :scalable-p nil)))
    (setf (test-font-family-faces family) (list unreadable usable)
          (test-font-port-families port) (list family))
    (let ((result (refresh-chat-font-inventory-with-fake-port frame port)))
      (is (eq :ready (appearance-activation-result-status result)))
      (is (= 1 (clawmacs::chat-frame-appearance-font-inventory-generation frame)))
      (let ((choices (appearance-font-inventory-choices
                      (clawmacs::chat-frame-appearance-font-inventory frame))))
        (is (= 1 (length choices)))
        (is (string= "Readable"
                     (enumerated-font-choice-face-display (first choices))))))))

(test chat-font-refresh-failure-rolls-back-and-isolates-two-frames
  "Enumeration errors never replace active state, generations, or a sibling frame."
  (let* ((first (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "font-refresh-fail-first" :session-persistence-mode :ephemeral)))
         (second (clim:make-application-frame
                  'clawmacs::clawmacs-chat-frame
                  :buffer (make-buffer "font-refresh-fail-second" :session-persistence-mode :ephemeral)))
         (port (make-instance 'test-font-port :families nil)))
    (let* ((family (make-test-font-family port "Test Family" nil))
           (face (make-test-font-face family "Regular" :sizes '(10))))
      (setf (test-font-family-faces family) (list face)
            (test-font-port-families port) (list family)))
    (refresh-chat-font-inventory-with-fake-port first port :invalidate-cache nil)
    (let ((old-bundle (clawmacs::chat-frame-appearance-active-bundle first))
          (old-generation (clawmacs::chat-frame-appearance-font-inventory-generation first))
          (second-profile (clawmacs::chat-frame-appearance-profile second)))
      (with-mcclim-test-function-override
          (clawmacs::chat-frame-appearance-live-port (frame)
            (if (eq frame first) port nil))
        (with-mcclim-test-function-override
            (clawmacs::enumerate-port-font-inventory (requested-port &rest arguments)
              (declare (ignore requested-port arguments))
              (error-appearance-condition 'font-unavailable :axis :font :value :broken))
          (let ((result (clawmacs::refresh-chat-frame-font-inventory first)))
            (is (eq :failed (appearance-activation-result-status result)))))
      (is (eq old-bundle (clawmacs::chat-frame-appearance-active-bundle first)))
      (is (= old-generation (clawmacs::chat-frame-appearance-font-inventory-generation first)))
      (is-true (clawmacs::chat-frame-appearance-font-refresh-diagnostics first))
      (is (eq second-profile (clawmacs::chat-frame-appearance-profile second)))))))

(test chat-font-refresh-request-is-an-event-handoff-and-restart-is-inert
  "The caller only queues; an unsafe candidate never partially publishes."
  (let* ((frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer (make-buffer "font-refresh-event" :session-persistence-mode :ephemeral)))
         (port (make-instance 'test-font-port :families nil))
         (family (make-test-font-family port "Test Family" nil))
         (face (make-test-font-face family "Regular" :sizes '(10)))
         (queued nil))
    (setf (test-font-family-faces family) (list face)
          (test-font-port-families port) (list family))
    (refresh-chat-font-inventory-with-fake-port frame port :invalidate-cache nil)
    (let ((old-bundle (clawmacs::chat-frame-appearance-active-bundle frame))
          (old-inventory (clawmacs::chat-frame-appearance-font-inventory frame))
          (old-generation (clawmacs::chat-frame-appearance-font-inventory-generation frame)))
      (with-mcclim-test-function-override
          (clawmacs::queue-chat-frame-font-inventory-refresh-event (requested-frame)
            (is (eq frame requested-frame))
            (setf queued t)
            t)
        (is-true (clawmacs::refresh-font-inventory-command frame)))
      ;; The request caller owns no publish path.
      (is-true queued)
      (is (eq old-bundle (clawmacs::chat-frame-appearance-active-bundle frame)))
      (is (= old-generation (clawmacs::chat-frame-appearance-font-inventory-generation frame)))
      (with-mcclim-test-function-override
          (clawmacs::chat-frame-appearance-live-port (requested-frame)
            (is (eq frame requested-frame)) port)
        (with-mcclim-test-function-override
            (clawmacs::appearance-bundles-same-typography-p (left right)
              (declare (ignore left right)) nil)
          (let ((result (clawmacs::refresh-chat-frame-font-inventory frame)))
            (is (eq :restart-required (appearance-activation-result-status result)))))
      (is (eq old-bundle (clawmacs::chat-frame-appearance-active-bundle frame)))
      (is (eq old-inventory (clawmacs::chat-frame-appearance-font-inventory frame)))
      (is (= old-generation (clawmacs::chat-frame-appearance-font-inventory-generation frame)))))))

(test chat-frame-construction-passes-a-fresh-classic-profile
  "Startup construction carries profile data without changing pane construction."
  (let ((captured-profiles nil)
        (buffer (make-buffer "appearance-startup-profile"
                             :session-persistence-mode :ephemeral)))
    (with-mcclim-test-function-override
        (clim:make-application-frame (class &rest initargs)
          (is (eq 'clawmacs::clawmacs-chat-frame class))
          (push (getf initargs :appearance-profile) captured-profiles)
          :captured-chat-frame)
      (with-mcclim-test-function-override
          (clim:run-frame-top-level (frame)
            (is (eq :captured-chat-frame frame))
            :ran)
        (is (eq :ran (clawmacs::run-clawmacs-chat-frame buffer)))
        (is (eq :ran (clawmacs::run-clawmacs-chat-frame buffer)))))
    (is (= 2 (length captured-profiles)))
    (dolist (profile captured-profiles)
      (is (typep profile 'clawmacs::appearance-profile))
      (is (eq :classic (appearance-profile-selected-theme profile))))
    (is-false (eq (first captured-profiles) (second captured-profiles)))))

(test compose-meta-x-prefers-frame-command-and-preserves-the-key-form
  "M-x resolves to Clawmacs and ESA parses its list-valued key as data."
  (clawmacs::init-default-keymap)
  (clawmacs::install-chat-frame-keybindings)
  (let* ((buffer (make-buffer "compose-meta-x"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (clawmacs::*chat-interaction-state*
           (clawmacs::chat-frame-interaction-state frame))
         (pane (let ((pane (make-instance 'synthetic-chat-compose-pane)))
                 (setf (synthetic-chat-compose-pane-frame pane) frame)
                 pane))
         (frame-table (clim:frame-command-table frame))
         (event (make-instance 'clim:key-press-event
                               :sheet nil
                               :x 0
                               :y 0
                               :key-name nil
                               :key-character #\x
                               :modifier-state
                               (clim:make-modifier-state :meta)))
         (additional-tables
           (drei-syntax:additional-command-tables pane frame-table))
         (frame-item
           (clim:find-keystroke-item event frame-table :errorp nil))
         (exclusive-item
           (clim:find-keystroke-item
            event
            (clim:find-command-table 'drei:exclusive-gadget-table)
            :errorp nil))
         (clim:*application-frame* frame)
         (clawmacs::*buffer-ring* nil))
    ;; Drei's public extension hook must put the frame-local table ahead of
    ;; EXCLUSIVE-GADGET-TABLE, whose own M-x command uses a blocking ACCEPT.
    (is (eq frame-table (first additional-tables)))
    (is (eq 'clawmacs::clawmacs-chat-compose-editing-table
            (second additional-tables)))
    (is (eq (find-symbol "COM-DREI-EXTENDED-COMMAND" :drei)
            (first (clim:command-menu-item-value exclusive-item))))
    ;; ESA evaluates supplied command arguments.  The key must therefore be a
    ;; quoted form in the table and become literal data only after parsing.
    (is (equal '(clawmacs::com-chat-dispatch-key '(:meta #\x))
               (clim:command-menu-item-value frame-item)))
    (is (equal '(clawmacs::com-chat-dispatch-key (:meta #\x))
               (esa:esa-partial-command-parser
                frame-table
                (make-string-input-stream "")
                (clim:command-menu-item-value frame-item)
                0)))
    ;; Exercise the same Drei/ESA gesture lookup and argument-evaluation path
    ;; used by the live compose pane; direct EXECUTE-FRAME-COMMAND would skip
    ;; both of the regressions guarded above.
    (setf (buffer-keymap buffer) clawmacs::*default-keymap*)
    (with-mcclim-test-function-override
        (clawmacs::make-command-selector-items (&key buffer)
          (declare (ignore buffer))
          (list (list :display "toggle"
                      :command 'clawmacs::toggle-debug-mode-command)))
      (with-mcclim-test-function-override
          (clawmacs::request-chat-frame-redisplay (requested-frame)
            (is (eq frame requested-frame))
            requested-frame)
        (clawmacs::process-chat-compose-drei-event pane event)))
    (is-true clawmacs::*minibuffer-active*)
    (is (string= "M-x" clawmacs::*minibuffer-prompt*))))

(test compose-prefix-survives-esas-identical-command-table-assignment
  "ESA's per-turn table assignment cannot invalidate a retained prefix."
  (clawmacs::init-default-keymap)
  (clawmacs::install-chat-frame-keybindings)
  (let* ((buffer (make-buffer "compose-prefix-refresh"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (pane (make-instance 'synthetic-chat-compose-pane))
         (control-c
           (make-instance 'clim:key-press-event
                          :sheet pane :x 0 :y 0
                          :key-name nil
                          :key-character #\c
                          :modifier-state
                          (clim:make-modifier-state :control)))
         (standalone-shift
           (make-instance 'clim:key-press-event
                          :sheet pane :x 0 :y 0
                          ;; Exercise a backend spelling that pinned ESA does
                          ;; not itself classify as modifier-only.
                          :key-name :lshift
                          :key-character nil
                          :modifier-state
                          (clim:make-modifier-state :shift)))
         (uppercase-v
           (make-instance 'clim:key-press-event
                          :sheet pane :x 0 :y 0
                          :key-name nil
                          :key-character #\V
                          ;; CLX has already used Shift to select uppercase V
                          ;; and removes Shift from the character event.
                          :modifier-state (clim:make-modifier-state)))
         (table (clim:frame-command-table frame))
         (clim:*application-frame* frame)
         (clawmacs::*buffer-ring* nil))
    (setf (synthetic-chat-compose-pane-frame pane) frame
          (buffer-keymap buffer) clawmacs::*default-keymap*)
    (setf (clim:frame-command-table frame) table)
    (clawmacs::process-chat-compose-drei-event pane control-c)
    (setf (clim:frame-command-table frame) table)
    ;; A standalone modifier is not part of the command sequence and must not
    ;; disturb the retained C-c prefix.
    (clim:handle-event pane standalone-shift)
    (clawmacs::process-chat-compose-drei-event pane uppercase-v)
    (is-true (buffer-show-reasoning-p buffer))))

(defun test-tool-use-block (id name)
  `((:type . "tool_use")
    (:id . ,id)
    (:name . ,name)
    (:input . nil)))

(defun test-tool-result-block (id content)
  `((:type . "tool_result")
    (:tool--use--id . ,id)
    (:content . ,content)))

(test message-help-thread-constructor-failure-is-contained
  "Metadata help resource exhaustion must not escape the CLIM command path."
  (let ((debug-event nil)
        (clawmacs::*message-help-runtime-reservations*
          (make-hash-table :test #'eq)))
    (with-mcclim-test-function-override
        (clawmacs::make-message-help-worker-thread (function)
          (declare (ignore function))
          (error "simulated message-help thread failure"))
      (with-mcclim-test-function-override
          (clawmacs::file-debug-event (event-name &rest payload)
            (declare (ignore payload))
            (setf debug-event event-name))
        ;; Any escaped constructor condition would fail at this call.
        (is (null
             (clawmacs::open-message-help-window (make-message :agent))))
        (is (string= "message-help-thread-start-error" debug-event))
        (is (= 0 (clawmacs::message-help-active-count-snapshot)))))))

(test message-help-frame-construction-failure-is-contained
  "A help-frame allocation failure must remain inside the user command path."
  (let ((debug-event nil)
        (clawmacs::*message-help-runtime-reservations*
          (make-hash-table :test #'eq)))
    (with-mcclim-test-function-override
        (clawmacs::make-message-help-frame (message)
          (declare (ignore message))
          (error "simulated message-help frame construction failure"))
      (with-mcclim-test-function-override
          (clawmacs::file-debug-event (event-name &rest payload)
            (declare (ignore payload))
            (setf debug-event event-name))
        (is (null
             (clawmacs::open-message-help-window (make-message :agent))))
        (is (string= "message-help-frame-construction-error"
                     debug-event))
        (is (= 0 (clawmacs::message-help-active-count-snapshot)))))))

(test message-help-reservation-covers-the-independent-frame-lifetime
  "The reservation remains visible until the help frame top level exits."
  (let ((worker-function nil)
        (debug-events nil)
        (clawmacs::*message-help-runtime-reservations*
          (make-hash-table :test #'eq)))
    (with-mcclim-test-function-override
        (clawmacs::make-message-help-frame (message)
          (declare (ignore message))
          :synthetic-help-frame)
      (with-mcclim-test-function-override
          (clawmacs::make-message-help-worker-thread (function)
            (setf worker-function function)
            :synthetic-worker)
        (with-mcclim-test-function-override
            (clawmacs::file-debug-event (event-name &rest payload)
              (declare (ignore payload))
              (push event-name debug-events))
          (is (eq :synthetic-help-frame
                  (clawmacs::open-message-help-window
                   (make-message :agent))))
          (is (= 1 (clawmacs::message-help-active-count-snapshot)))
          ;; The synthetic frame has no CLIM top-level method.  OPEN's worker
          ;; boundary contains that expected error and still releases exactly.
          (funcall worker-function)
          (is (= 0 (clawmacs::message-help-active-count-snapshot)))
          (is (member "message-help-frame-error"
                      debug-events :test #'string=)))))))

(test message-help-start-is-refused-while-reload-owns-admission
  "A help frame cannot begin construction during live reload ownership."
  (let ((constructor-called-p nil)
        (debug-event nil)
        (clawmacs::*message-help-runtime-reservations*
          (make-hash-table :test #'eq))
        (clawmacs::*safe-reload-active-request*
          (clawmacs::make-safe-reload-request
           :token (gensym "ACTIVE-RELOAD-"))))
    (with-mcclim-test-function-override
        (clawmacs::make-message-help-frame (message)
          (declare (ignore message))
          (setf constructor-called-p t)
          :must-not-exist)
      (with-mcclim-test-function-override
          (clawmacs::file-debug-event (event-name &rest payload)
            (declare (ignore payload))
            (setf debug-event event-name))
        (is (null
             (clawmacs::open-message-help-window (make-message :agent))))
        (is-false constructor-called-p)
        (is (= 0 (clawmacs::message-help-active-count-snapshot)))
        (is (string= "message-help-admission-refused" debug-event))))))

(test chat-recurse-launch-failure-is-contained-at-command-boundary
  "A child-process launch failure is visible but cannot unwind the chat frame."
  (let* ((buf (make-buffer "recurse-launch-failure"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buf))
         (clawmacs::*chat-interaction-state*
           (clawmacs::chat-frame-interaction-state frame))
         (debug-events nil))
    (with-mcclim-test-function-override
        (clawmacs::launch-chat-recurse (ignored-buffer)
          (declare (ignore ignored-buffer))
          (error "simulated recurse process failure"))
      (with-mcclim-test-function-override
          (clawmacs::file-debug-event (event-name &rest payload)
            (declare (ignore payload))
            (push event-name debug-events))
        ;; EXECUTE-FRAME-COMMAND is the same ESA boundary used by the menu.
        (clim:execute-frame-command frame '(clawmacs::com-chat-recurse))
        (let ((status (message-prev (buffer-input-message buf))))
          (is (eq :system (message-sender status)))
          (is (search "Unable to open recurse frame"
                      (message-text status)))
          (is (search "simulated recurse process failure"
                      (message-text status))))
        (is (member "chat-recurse-launch-error"
                    debug-events
                    :test #'string=))))))

(test chat-frame-command-errors-remain-inside-the-running-frame
  "An ordinary CLIM command error is logged, displayed, and contained."
  (let* ((buf (make-buffer "frame-command-error"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buf))
         (redisplay-requests 0)
         (debug-events nil))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (with-mcclim-test-function-override
        (clawmacs::request-chat-frame-redisplay (requested-frame)
          (is (eq frame requested-frame))
          (incf redisplay-requests)
          requested-frame)
      (with-mcclim-test-function-override
          (clawmacs::file-debug-event (event-name &rest payload)
            (push (cons event-name payload) debug-events))
        ;; Any escaped condition fails this test at the call site.
        (is (null
             (clim:execute-frame-command
              frame '(error "simulated frame command failure"))))))
    (let ((diagnostic (message-prev (buffer-input-message buf))))
      (is (eq :system (message-sender diagnostic)))
      (is (search "UI action failed" (message-text diagnostic)))
      (is (search "simulated frame command failure"
                  (message-text diagnostic))))
    (is (= 1 redisplay-requests))
    (is (member "ui-action-error" debug-events
                :key #'car :test #'string=))
    (is (eq :running (clawmacs::chat-frame-lifecycle-state frame)))))

(test chat-compose-minibuffer-callback-errors-remain-inside-the-running-frame
  "A failing prompt callback deactivates cleanly without exiting the frame."
  (let* ((buf (make-buffer "minibuffer-callback-error"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buf))
         (clawmacs::*chat-interaction-state*
           (clawmacs::chat-frame-interaction-state frame))
         (pane (make-synthetic-chat-compose-pane frame))
         (event (make-instance 'clim:key-press-event
                               :sheet nil
                               :x 0
                               :y 0
                               :key-name nil
                               :key-character #\Return
                               :modifier-state (clim:make-modifier-state)))
         (redisplay-requests 0)
         (debug-events nil))
    (setf clawmacs::*minibuffer-active* t
          clawmacs::*minibuffer-mode* :prompt
          clawmacs::*minibuffer-prompt* "Failing prompt"
          clawmacs::*minibuffer-input* "confirmed value"
          clawmacs::*minibuffer-point* 15
          clawmacs::*minibuffer-callback*
          (lambda (value)
            (declare (ignore value))
            (error "simulated minibuffer callback failure"))
          (buffer-keymap buf) (clawmacs::make-keymap :minibuffer-error)
          (clawmacs::chat-frame-lifecycle-state frame) :running)
    (with-mcclim-test-function-override
        (clawmacs::request-chat-frame-redisplay (requested-frame)
          (is (eq frame requested-frame))
          (incf redisplay-requests)
          requested-frame)
      (with-mcclim-test-function-override
          (clawmacs::file-debug-event (event-name &rest payload)
            (declare (ignore payload))
            (push event-name debug-events))
        ;; MINIBUFFER-CONFIRM invokes the callback through compose dispatch.
        (is (null (clim:handle-event pane event)))))
    (is-false clawmacs::*minibuffer-active*)
    (let ((diagnostic (message-prev (buffer-input-message buf))))
      (is (eq :system (message-sender diagnostic)))
      (is (search "simulated minibuffer callback failure"
                  (message-text diagnostic))))
    ;; Reconciliation and the subsequently appended UI diagnostic each make
    ;; one coalesced redisplay request.
    (is (= 2 redisplay-requests))
    (is (member "ui-action-error" debug-events :test #'string=))
    (is (eq :running (clawmacs::chat-frame-lifecycle-state frame)))))

(test transcript-collapses-consecutive-tool-activity
  (let ((buf (make-buffer "tool-collapse" :session-persistence-mode :ephemeral)))
    (buffer-insert-read-only-message
     buf :agent "(read ...)"
     :raw-content (list (test-tool-use-block "toolu-1" "read"))
     :record-p nil)
    (buffer-insert-read-only-message
     buf :tool-result "[read result: ok]"
     :raw-content (list (test-tool-result-block "toolu-1" "ok"))
     :record-p nil)
    (buffer-insert-read-only-message
     buf :agent "(grep ...)\n(read ...)"
     :raw-content (list (test-tool-use-block "toolu-2" "grep")
                        (test-tool-use-block "toolu-3" "read"))
     :record-p nil)
    (buffer-insert-read-only-message
     buf :tool-result "[grep result: ok]\n[read result: ok]"
     :raw-content (list (test-tool-result-block "toolu-2" "ok")
                        (test-tool-result-block "toolu-3" "ok"))
     :record-p nil)
    (buffer-insert-read-only-message buf :user "next" :record-p nil)
    (let ((items (clawmacs::chat-transcript-display-items buf)))
      (is (= 2 (length items)))
      (is (clawmacs::chat-tool-activity-summary-p (first items)))
      (is (equal '(("read" . 2) ("grep" . 1))
                 (clawmacs::chat-tool-activity-summary-tool-counts (first items))))
      (is (= 3 (clawmacs::chat-tool-activity-summary-result-count (first items))))
      (is (string= "next" (message-text (second items)))))))

(test transcript-tool-collapse-can-be-disabled
  (let ((buf (make-buffer "tool-collapse-off" :session-persistence-mode :ephemeral)))
    (setf (buffer-collapse-tool-activity-p buf) nil)
    (buffer-insert-read-only-message
     buf :agent "(read ...)"
     :raw-content (list (test-tool-use-block "toolu-1" "read"))
     :record-p nil)
    (buffer-insert-read-only-message
     buf :tool-result "[read result: ok]"
     :raw-content (list (test-tool-result-block "toolu-1" "ok"))
     :record-p nil)
    (let ((items (clawmacs::chat-transcript-display-items buf)))
      (is (= 2 (length items)))
      (is-false (clawmacs::chat-tool-activity-summary-p (first items)))
      (is-false (clawmacs::chat-tool-activity-summary-p (second items))))))

(test transcript-hidden-tool-results-do-not-break-collapsed-run
  (let ((buf (make-buffer "tool-collapse-hidden-results" :session-persistence-mode :ephemeral)))
    (setf (buffer-show-tool-results-p buf) nil)
    (buffer-insert-read-only-message
     buf :agent "(read ...)"
     :raw-content (list (test-tool-use-block "toolu-1" "read"))
     :record-p nil)
    (buffer-insert-read-only-message
     buf :tool-result "[read result: ok]"
     :raw-content (list (test-tool-result-block "toolu-1" "ok"))
     :record-p nil)
    (buffer-insert-read-only-message
     buf :agent "(grep ...)"
     :raw-content (list (test-tool-use-block "toolu-2" "grep"))
     :record-p nil)
    (let ((items (clawmacs::chat-transcript-display-items buf)))
      (is (= 1 (length items)))
      (is (clawmacs::chat-tool-activity-summary-p (first items)))
      (is (equal '(("read" . 1) ("grep" . 1))
                 (clawmacs::chat-tool-activity-summary-tool-counts (first items))))
      (is (= 0 (clawmacs::chat-tool-activity-summary-result-count (first items)))))))

(test chat-frame-is-esa-application
  "The chat frame exposes the ESA frame and buffer protocol."
  (let* ((buf (make-buffer "esa-frame" :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf)))
    (is (typep frame 'esa:esa-frame-mixin))
    (is (eq buf (esa:esa-current-buffer frame)))
    (is (member buf (esa:buffers frame) :test #'eq))
    (is (null (esa:windows frame)))
    (is (eq frame (esa:esa-current-window frame)))
    (is (eq (esa:find-applicable-command-table frame)
            (clim:frame-command-table frame)))))

(test chat-frame-buffer-transition-round-trips-drafts-and-points
  "A buffer switch saves source text/selection and restores the target."
  (let* ((source (make-buffer "transition-source"
                              :session-persistence-mode :ephemeral))
         (target (make-buffer "transition-target"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'clawmacs::clawmacs-chat-compose-pane
                                 :initial-contents "source draft"))
         (target-message (buffer-input-message target))
         (clawmacs::*buffer-ring* (list target)))
    (setf (clim:gadget-value compose) "source draft"
          (drei-buffer:offset (drei:point (drei:current-view compose))) 6
          (drei-buffer:offset (drei:mark (drei:current-view compose))) 2)
    (set-message-text target-message "target draft")
    (set-message-point-from-absolute-offset target-message 3)
    (clawmacs:set-message-mark-from-absolute-offset target-message 8)
    (with-mcclim-test-function-override
        (clawmacs::request-chat-frame-redisplay (requested-frame)
          (is (eq frame requested-frame))
          requested-frame)
      (is (eq :switched
              (clawmacs::call-with-chat-frame-buffer-transition
               frame
               (lambda (command-buffer)
                 (is (eq source command-buffer))
                 (is (eq source (current-buffer)))
                 (switch-to-buffer target)
                 :switched)
               :compose-pane compose))))
    (is (eq target (clawmacs::chat-frame-buffer frame)))
    (is (eq target (current-buffer)))
    (is (string= "source draft"
                 (message-text (buffer-input-message source))))
    (is (= 6 (message-point-absolute-offset
              (buffer-input-message source))))
    (is (= 2 (message-mark-absolute-offset
              (buffer-input-message source))))
    (is (string= "target draft" (clim:gadget-value compose)))
    (is (= 3 (clawmacs::chat-compose-pane-point-offset compose)))
    (is (= 8 (clawmacs::chat-compose-pane-mark-offset compose)))
    (with-mcclim-test-function-override
        (clawmacs::request-chat-frame-redisplay (requested-frame)
          (is (eq frame requested-frame))
          requested-frame)
      (clawmacs::call-with-chat-frame-buffer-transition
       frame
       (lambda (command-buffer)
         (is (eq target command-buffer))
         (switch-to-buffer source))
       :compose-pane compose))
    (is (string= "source draft" (clim:gadget-value compose)))
    (is (= 6 (clawmacs::chat-compose-pane-point-offset compose)))
    (is (= 2 (clawmacs::chat-compose-pane-mark-offset compose)))))

(test chat-frame-buffer-transition-does-not-rewrite-an-unchanged-draft
  "Selector-only commands preserve Drei undo state and avoid O(draft) writes."
  (let* ((draft (make-string 100000 :initial-element #\x))
         (source (make-buffer "transition-noop-source"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents draft))
         (clawmacs::*buffer-ring* (list source)))
    (setf (synthetic-chat-compose-pane-frame compose) frame)
    (set-message-text (buffer-input-message source) draft)
    (let ((*synthetic-compose-gadget-value-writes* 0))
      (with-mcclim-test-function-override
          (clawmacs::request-chat-frame-redisplay (requested-frame)
            (is (eq frame requested-frame))
            requested-frame)
        (is (eq :unchanged
                (clawmacs::call-with-chat-frame-buffer-transition
                 frame
                 (lambda (buffer)
                   (is (eq source buffer))
                   :unchanged)
                 :compose-pane compose))))
      (is (= 0 *synthetic-compose-gadget-value-writes*)))))

(test switch-buffer-modal-keys-do-not-materialize-the-compose-draft
  "Sustained selector filtering stays O(selector), not O(compose draft)."
  (let* ((draft (make-string 100000 :initial-element #\x))
         (source (make-buffer "transition-modal-fast-source"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents draft))
         (state (clawmacs::chat-frame-interaction-state frame))
         (event (make-instance 'clim:key-press-event
                               :sheet compose :x 0 :y 0
                               :key-name nil
                               :key-character #\x
                               :modifier-state
                               (clim:make-modifier-state)))
         (clawmacs::*buffer-ring* (list source))
         (clawmacs::*chat-interaction-state* state))
    (setf (synthetic-chat-compose-pane-frame compose) frame)
    (set-message-text (buffer-input-message source) draft)
    (with-mcclim-test-function-override
        (clawmacs::request-chat-frame-redisplay (requested-frame)
          (is (eq frame requested-frame))
          requested-frame)
      ;; This is the selector-opening transition.  It establishes the frame's
      ;; compose/model synchronization token once.
      (clawmacs::call-with-chat-frame-buffer-transition
       frame
       (lambda (buffer)
         (declare (ignore buffer))
         (clawmacs::minibuffer-activate
          "Large Draft"
          (loop :for index :below 20
                :collect (list :display (format nil "candidate-~D" index)))
          #'identity))
       :compose-pane compose)
      (let ((*synthetic-compose-gadget-value-reads* 0)
            (*synthetic-compose-gadget-value-writes* 0)
            (started (get-internal-real-time)))
        (dotimes (index 200)
          (declare (ignore index))
          (is-true
           (clawmacs::dispatch-chat-compose-event-to-buffer compose event)))
        (let ((elapsed-seconds
                (/ (- (get-internal-real-time) started)
                   internal-time-units-per-second)))
          (is (< elapsed-seconds 5)))
        (is (= 0 *synthetic-compose-gadget-value-reads*))
        (is (= 0 *synthetic-compose-gadget-value-writes*))))
    (is (string= draft (clim:gadget-value compose)))
    (is (string= draft (message-text (buffer-input-message source))))))

(test drei-compose-edit-invalidates-the-modal-synchronization-token
  "A later selector must save ordinary Drei edits before using its fast path."
  (let* ((source (make-buffer "transition-invalidation-source"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents "draft"))
         (state (clawmacs::chat-frame-interaction-state frame))
         (event (make-instance 'clim:key-press-event
                               :sheet compose :x 0 :y 0
                               :key-name nil
                               :key-character #\z
                               :modifier-state
                               (clim:make-modifier-state)))
         (clim:*application-frame* frame)
         (clawmacs::*buffer-ring* (list source))
         (clawmacs::*chat-interaction-state* state))
    (setf (synthetic-chat-compose-pane-frame compose) frame
          (clawmacs::chat-frame-compose-synchronized-buffer frame) source
          (clawmacs::chat-frame-lifecycle-state frame) :running)
    (clim:handle-event compose event)
    (is (null (clawmacs::chat-frame-compose-synchronized-buffer frame)))
    ;; The headless pane has no live port, so apply the same public Drei
    ;; gesture processor explicitly after proving the frame adapter invalidated
    ;; its token.
    (clawmacs::process-chat-compose-drei-event compose event)
    (is (string= "zdraft" (clim:gadget-value compose)))
    (clawmacs::minibuffer-activate
     "After Edit" (list (list :display "candidate")) #'identity)
    (with-mcclim-test-function-override
        (clawmacs::request-chat-frame-redisplay (requested-frame)
          (is (eq frame requested-frame))
          requested-frame)
      (is-true
       (clawmacs::dispatch-chat-compose-event-to-buffer compose event)))
    (is (string= "zdraft" (message-text (buffer-input-message source))))))

(test chat-frame-buffer-transition-marks-edited-files-dirty
  "Saving a file-buffer draft through Drei preserves project dirty state."
  (let* ((source (make-buffer "transition-file-source"
                              :kind :file
                              :original-text "saved text"
                              :session-persistence-mode :ephemeral))
         (target (make-buffer "transition-file-target"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents "edited text"))
         (clawmacs::*buffer-ring* (list source target)))
    (setf (synthetic-chat-compose-pane-frame compose) frame)
    (set-message-text (buffer-input-message source) "saved text")
    (with-mcclim-test-function-override
        (clawmacs::request-chat-frame-redisplay (requested-frame)
          (is (eq frame requested-frame))
          requested-frame)
      (clawmacs::call-with-chat-frame-buffer-transition
       frame
       (lambda (buffer)
         (is (eq source buffer))
         (switch-to-buffer target))
       :compose-pane compose))
    (is (string= "edited text" (file-buffer-text source)))
    (is-true (buffer-dirty-p source))))

(test chat-frame-buffer-transition-reconciles-a-switch-before-an-error
  "An ordinary command failure cannot leave frame and compose state divergent."
  (let* ((source (make-buffer "transition-error-source"
                              :session-persistence-mode :ephemeral))
         (target (make-buffer "transition-error-target"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents "source text"))
         (target-message (buffer-input-message target))
         (caught nil)
         (clawmacs::*buffer-ring* (list source target)))
    (setf (synthetic-chat-compose-pane-frame compose) frame)
    (set-message-text (buffer-input-message source) "source text")
    (set-message-text target-message "target after error")
    (handler-case
        (with-mcclim-test-function-override
            (clawmacs::request-chat-frame-redisplay (requested-frame)
              (is (eq frame requested-frame))
              requested-frame)
          (clawmacs::call-with-chat-frame-buffer-transition
           frame
           (lambda (buffer)
             (declare (ignore buffer))
             (switch-to-buffer target)
             (error "simulated failure after switch"))
           :compose-pane compose))
      (error (condition)
        (setf caught condition)))
    (is-true caught)
    (is (search "simulated failure after switch" (princ-to-string caught)))
    (is (eq target (current-buffer)))
    (is (eq target (clawmacs::chat-frame-buffer frame)))
    (is (string= "target after error" (clim:gadget-value compose)))))

(test chat-frame-buffer-transition-rolls-back-a-failed-error-reconciliation
  "A broken target load restores source frame, ring, and visible editor state."
  (let* ((source (make-buffer "transition-rollback-source"
                              :session-persistence-mode :ephemeral))
         (target (make-buffer "transition-rollback-target"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents "rollback source"))
         (original-sync
           (symbol-function 'clawmacs::sync-chat-compose-pane-from-buffer))
         (caught nil)
         (clawmacs::*buffer-ring* (list source target)))
    (setf (synthetic-chat-compose-pane-frame compose) frame)
    (set-message-text (buffer-input-message source) "rollback source")
    (set-message-text (buffer-input-message target) "unloadable target")
    (with-mcclim-test-function-override
        (clawmacs::sync-chat-compose-pane-from-buffer
            (requested-pane requested-buffer &key force)
          (if (eq requested-buffer target)
              (error "simulated target load failure")
              (funcall original-sync requested-pane requested-buffer
                       :force force)))
      (with-mcclim-test-function-override
          (clawmacs::request-chat-frame-redisplay (requested-frame)
            (is (eq frame requested-frame))
            requested-frame)
        (handler-case
            (clawmacs::call-with-chat-frame-buffer-transition
             frame
             (lambda (buffer)
               (declare (ignore buffer))
               (switch-to-buffer target)
               (error "simulated command failure"))
             :compose-pane compose)
          (error (condition)
            (setf caught condition)))))
    (is-true caught)
    (let ((text (princ-to-string caught)))
      (is (search "simulated command failure" text))
      (is (search "simulated target load failure" text)))
    (is (eq source (current-buffer)))
    (is (eq source (clawmacs::chat-frame-buffer frame)))
    (is (eq source
            (clawmacs::chat-frame-compose-synchronized-buffer frame)))
    (is (string= "rollback source" (clim:gadget-value compose)))))

(test chat-frame-compose-initialization-without-a-pane-keeps-model-authority
  "A pre-generation NIL pane cannot claim model/editor synchronization."
  (let* ((source (make-buffer "initial-compose-no-pane"
                              :session-persistence-mode :ephemeral))
         (message (buffer-input-message source))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source)))
    (set-message-text message "restored before panes")
    (set-message-point-from-absolute-offset message 8)
    (clawmacs:set-message-mark-from-absolute-offset message 2)
    (is (null (clawmacs::initialize-chat-frame-compose-pane frame nil)))
    (is (null (clawmacs::chat-frame-compose-synchronized-buffer frame)))
    (is (string= "restored before panes" (message-text message)))
    (is (= 8 (message-point-absolute-offset message)))
    (is (= 2 (message-mark-absolute-offset message)))))

(test chat-frame-top-level-pane-initialization-hydrates-the-initial-draft
  "The post-generation top-level seam hydrates before marking the frame live."
  (let* ((source (make-buffer "initial-compose-source"
                              :session-persistence-mode :ephemeral))
         (message (buffer-input-message source))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents ""))
         (original-find-pane (symbol-function 'clim:find-pane-named))
         (clawmacs::*buffer-ring* (list source)))
    (setf (synthetic-chat-compose-pane-frame compose) frame
          (clawmacs::chat-frame-lifecycle-state frame) :starting)
    (set-message-text message "restored initial draft")
    (set-message-point-from-absolute-offset message 12)
    (clawmacs:set-message-mark-from-absolute-offset message 3)
    (with-mcclim-test-function-override
        (clim:find-pane-named (requested-frame pane-name)
          (if (eq requested-frame frame)
              (case pane-name
                (clawmacs::compose compose)
                (clawmacs::transcript nil)
                (otherwise
                 (funcall original-find-pane requested-frame pane-name)))
              (funcall original-find-pane requested-frame pane-name)))
      (clawmacs::initialize-chat-frame-top-level-panes frame))
    (is (string= "restored initial draft" (clim:gadget-value compose)))
    (is (= 12 (clawmacs::chat-compose-pane-point-offset compose)))
    (is (= 3 (clawmacs::chat-compose-pane-mark-offset compose)))
    (is (eq source
            (clawmacs::chat-frame-compose-synchronized-buffer frame)))
    (is (eq :starting (clawmacs::chat-frame-lifecycle-state frame)))
    (let ((*synthetic-compose-gadget-value-writes* 0))
      (with-mcclim-test-function-override
          (clawmacs::request-chat-frame-redisplay (requested-frame)
            (is (eq frame requested-frame))
            requested-frame)
        (clawmacs::call-with-chat-frame-buffer-transition
         frame #'identity
         :compose-pane compose
         :state-only t))
      (is (= 0 *synthetic-compose-gadget-value-writes*)))
    (is (string= "restored initial draft" (message-text message)))))

(test esa-current-buffer-setter-is-the-canonical-compose-transition
  "Extensions using ESA's setter save and load drafts through the frame API."
  (let* ((source (make-buffer "esa-setter-source"
                              :session-persistence-mode :ephemeral))
         (target (make-buffer "esa-setter-target"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents "edited source"))
         (original-find-pane (symbol-function 'clim:find-pane-named))
         ;; TARGET intentionally starts outside the ring: the canonical setter
         ;; must register and adopt extension-created buffers itself.
         (clawmacs::*buffer-ring* (list source)))
    (setf (synthetic-chat-compose-pane-frame compose) frame)
    (set-message-text (buffer-input-message source) "old source")
    (set-message-text (buffer-input-message target) "loaded target")
    (let ((*synthetic-compose-gadget-value-reads* 0))
      (with-mcclim-test-function-override
          (clim:find-pane-named (requested-frame pane-name)
            (if (and (eq requested-frame frame)
                     (eq pane-name 'clawmacs::compose))
                compose
                (funcall original-find-pane requested-frame pane-name)))
        (with-mcclim-test-function-override
            (clawmacs::request-chat-frame-redisplay (requested-frame)
              (is (eq frame requested-frame))
              requested-frame)
          (setf (esa:esa-current-buffer frame) target)))
      ;; One source save read; forced target replacement performs no second
      ;; outgoing-draft materialization.
      (is (= 1 *synthetic-compose-gadget-value-reads*)))
    (is (string= "edited source"
                 (message-text (buffer-input-message source))))
    (is (string= "loaded target" (clim:gadget-value compose)))
    (is (member target clawmacs::*buffer-ring* :test #'eq))
    (is (eq target (current-buffer)))
    (is (eq target (esa:esa-current-buffer frame)))))

(test esa-current-buffer-same-target-adopts-live-drei-without-replacement
  "A direct same-target setter saves newer editor state and preserves undo."
  (let* ((source (make-buffer "esa-setter-same-source"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents "newer live draft"))
         (original-find-pane (symbol-function 'clim:find-pane-named))
         (clawmacs::*buffer-ring* (list source)))
    (setf (synthetic-chat-compose-pane-frame compose) frame
          (drei-buffer:offset (drei:point (drei:current-view compose))) 9
          (drei-buffer:offset (drei:mark (drei:current-view compose))) 3)
    (set-message-text (buffer-input-message source) "stale model draft")
    (let ((*synthetic-compose-gadget-value-writes* 0))
      (with-mcclim-test-function-override
          (clim:find-pane-named (requested-frame pane-name)
            (if (and (eq requested-frame frame)
                     (eq pane-name 'clawmacs::compose))
                compose
                (funcall original-find-pane requested-frame pane-name)))
        (with-mcclim-test-function-override
            (clawmacs::request-chat-frame-redisplay (requested-frame)
              (is (eq frame requested-frame))
              requested-frame)
          (setf (esa:esa-current-buffer frame) source)))
      (is (= 0 *synthetic-compose-gadget-value-writes*)))
    (is (string= "newer live draft"
                 (message-text (buffer-input-message source))))
    (is (= 9 (message-point-absolute-offset
              (buffer-input-message source))))
    (is (= 3 (message-mark-absolute-offset
              (buffer-input-message source))))
    (is (string= "newer live draft" (clim:gadget-value compose)))))

(test switch-buffer-keyboard-confirmation-round-trips-compose-state
  "The real frame-command/modal-key path switches drafts without stale text."
  (clawmacs::init-default-keymap)
  (let* ((source (make-buffer "keyboard-switch-source"
                              :session-persistence-mode :ephemeral))
         (target (make-buffer "keyboard-switch-target"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'synthetic-chat-compose-pane
                                 :initial-contents "keyboard source draft"))
         (state (clawmacs::chat-frame-interaction-state frame))
         (return-event
           (make-instance 'clim:key-press-event
                          :sheet compose :x 0 :y 0
                          :key-name nil
                          :key-character #\Return
                          :modifier-state (clim:make-modifier-state)))
         (target-message (buffer-input-message target))
         (original-find-pane (symbol-function 'clim:find-pane-named))
         (clim:*application-frame* frame)
         (clawmacs::*chat-interaction-state* state)
         (clawmacs::*buffer-ring* (list source target)))
    (setf (synthetic-chat-compose-pane-frame compose) frame
          (buffer-keymap source) clawmacs::*default-keymap*
          (buffer-keymap target) clawmacs::*default-keymap*
          (clim:gadget-value compose) "keyboard source draft"
          (drei-buffer:offset (drei:point (drei:current-view compose))) 8)
    (set-message-text target-message "keyboard target draft")
    (set-message-point-from-absolute-offset target-message 4)
    (with-mcclim-test-function-override
        (clim:find-pane-named (requested-frame pane-name)
          (if (and (eq frame requested-frame)
                   (eq 'clawmacs::compose pane-name))
              compose
              (funcall original-find-pane requested-frame pane-name)))
      (with-mcclim-test-function-override
          (clawmacs::request-chat-frame-redisplay (requested-frame)
            (is (eq frame requested-frame))
            requested-frame)
        (clim:execute-frame-command
         frame '(clawmacs::com-chat-dispatch-key (:ctrl-x #\b)))
        (is-true clawmacs::*minibuffer-active*)
        (is (string= "keyboard source draft"
                     (message-text (buffer-input-message source))))
        (let ((target-index
                (position target clawmacs::*minibuffer-filtered-items*
                          :key (lambda (item) (getf item :buffer))
                          :test #'eq)))
          (is-true (integerp target-index))
          (setf clawmacs::*minibuffer-selected-index* target-index))
        (is-true
         (clawmacs::dispatch-chat-compose-event-to-buffer
          compose return-event))))
    (is-false clawmacs::*minibuffer-active*)
    (is (eq target (clawmacs::chat-frame-buffer frame)))
    (is (string= "keyboard target draft" (clim:gadget-value compose)))
    (is (= 4 (clawmacs::chat-compose-pane-point-offset compose)))
    (is (= 8 (message-point-absolute-offset
              (buffer-input-message source))))))

(test switch-buffer-presentation-confirmation-round-trips-compose-state
  "A semantic pointer choice uses the same draft/point transaction as keys."
  (let* ((source (make-buffer "pointer-switch-source"
                              :session-persistence-mode :ephemeral))
         (target (make-buffer "pointer-switch-target"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer source))
         (compose (make-instance 'clawmacs::clawmacs-chat-compose-pane
                                 :initial-contents "pointer source draft"))
         (state (clawmacs::chat-frame-interaction-state frame))
         (target-item (list :buffer target
                            :name (buffer-name target)
                            :display "pointer target"))
         (source-item (list :buffer source
                            :name (buffer-name source)
                            :display "pointer source"))
         (target-message (buffer-input-message target))
         (clawmacs::*chat-interaction-state* state)
         (clawmacs::*buffer-ring* (list source target)))
    (setf (clim:gadget-value compose) "pointer source draft"
          (drei-buffer:offset (drei:point (drei:current-view compose))) 7)
    (set-message-text target-message "pointer target draft")
    (set-message-point-from-absolute-offset target-message 5)
    (clawmacs::minibuffer-activate
     "Switch Buffer"
     (list source-item target-item)
     (lambda (item)
       (switch-to-buffer (getf item :buffer))))
    (let ((ref (clawmacs::make-chat-interaction-candidate-ref
                state
                (clawmacs::chat-interaction-state-generation state)
                :minibuffer 1 target-item)))
      (with-mcclim-test-function-override
          (clim:find-pane-named (requested-frame pane-name)
            (declare (ignore requested-frame))
            (and (eq 'clawmacs::compose pane-name) compose))
        (with-mcclim-test-function-override
            (clawmacs::request-chat-frame-redisplay (requested-frame)
              (is (eq frame requested-frame))
              requested-frame)
          (is-true
           (clawmacs::choose-chat-interaction-candidate frame ref)))))
    (is-false clawmacs::*minibuffer-active*)
    (is (eq target (clawmacs::chat-frame-buffer frame)))
    (is (string= "pointer source draft"
                 (message-text (buffer-input-message source))))
    (is (= 7 (message-point-absolute-offset
              (buffer-input-message source))))
    (is (string= "pointer target draft" (clim:gadget-value compose)))
    (is (= 5 (clawmacs::chat-compose-pane-point-offset compose)))))

(test chat-frame-redisplay-request-before-graft-does-not-wedge
  "A failed pre-graft wakeup leaves dirty state retryable, not pending forever."
  (let* ((buf (make-buffer "redisplay-before-graft"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf)))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (clawmacs::request-chat-frame-redisplay frame)
    (is-true (clawmacs::chat-frame-redisplay-dirty-p frame))
    (is-false (clawmacs::chat-frame-redisplay-pending-p frame))
    (is-false (clawmacs::chat-frame-redisplay-handling-p frame))))

(test concurrent-chat-redisplay-requests-reserve-one-wakeup
  "Many worker notifications coalesce without losing the dirty state."
  (let* ((buf (make-buffer "redisplay-coalescing"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf))
         (count-lock (bt:make-lock "redisplay-queue-count"))
         (queue-count 0)
         (wrong-frame-p nil)
         (workers nil))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (with-mcclim-test-function-override
        (clawmacs::queue-chat-frame-redisplay-event (requested-frame)
          (bt:with-lock-held (count-lock)
            (unless (eq frame requested-frame)
              (setf wrong-frame-p t))
            (incf queue-count))
          t)
      (setf workers
            (loop :repeat 64
                  :collect
                  (bt:make-thread
                   (lambda ()
                     (clawmacs::request-chat-frame-redisplay frame)))))
      (dolist (worker workers)
        (bt:join-thread worker))
      (is-false wrong-frame-p)
      (is (= 1 queue-count))
      (is-true (clawmacs::chat-frame-redisplay-dirty-p frame))
      (is-true (clawmacs::chat-frame-redisplay-pending-p frame)))))

(test lone-chat-redisplay-enqueue-failure-recovers-without-new-request
  "One transient enqueue failure is retried without another worker notification."
  (let* ((buf (make-buffer "redisplay-retry"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf))
         (attempts 0)
         (clawmacs::*chat-redisplay-enqueue-max-attempts* 2))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (with-mcclim-test-function-override
        (clawmacs::queue-chat-frame-redisplay-event (requested-frame)
          (declare (ignore requested-frame))
          (incf attempts)
          (> attempts 1))
      (clawmacs::request-chat-frame-redisplay frame)
      (is (= 2 attempts))
      (is-true (clawmacs::chat-frame-redisplay-dirty-p frame))
      (is-true (clawmacs::chat-frame-redisplay-pending-p frame)))))

(test request-during-failed-redisplay-enqueue-is-not-lost
  "A requester that observes an in-flight reservation is transferred on failure."
  (let* ((buf (make-buffer "redisplay-failure-race"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf))
         (first-enqueue-entered
           (bt:make-semaphore :name "redisplay-first-enqueue-entered"))
         (release-first-enqueue
           (bt:make-semaphore :name "redisplay-release-first-enqueue"))
         (count-lock (bt:make-lock "redisplay-race-count"))
         (attempts 0))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (with-mcclim-test-function-override
        (clawmacs::queue-chat-frame-redisplay-event (requested-frame)
          (declare (ignore requested-frame))
          (let ((attempt
                  (bt:with-lock-held (count-lock)
                    (incf attempts))))
            (if (= attempt 1)
                (progn
                  (bt:signal-semaphore first-enqueue-entered)
                  (bt:wait-on-semaphore release-first-enqueue :timeout 2)
                  nil)
                t)))
      (let ((first-request
              (bt:make-thread
               (lambda ()
                 (clawmacs::request-chat-frame-redisplay frame))
               :name "redisplay-failing-request")))
        (is-true
         (bt:wait-on-semaphore first-enqueue-entered :timeout 2))
        ;; This request sees PENDING and returns without enqueuing itself.
        (clawmacs::request-chat-frame-redisplay frame)
        (bt:signal-semaphore release-first-enqueue)
        (bt:join-thread first-request))
      (is (= 2 attempts))
      (is-true (clawmacs::chat-frame-redisplay-dirty-p frame))
      (is-true (clawmacs::chat-frame-redisplay-pending-p frame)))))

(test redisplay-enqueue-retry-is-bounded-and-iterative
  "A failing queue cannot spin despite newer requests during every attempt."
  (let* ((buf (make-buffer "redisplay-iterative-retry"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buf))
         (attempts 0)
         (clawmacs::*chat-redisplay-enqueue-max-attempts* 3))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (with-mcclim-test-function-override
        (clawmacs::queue-chat-frame-redisplay-event (requested-frame)
          (incf attempts)
          ;; Each newer request observes PENDING.  The owning iterative retry
          ;; must stop at the cap and release that reservation on final failure.
          (clawmacs::request-chat-frame-redisplay requested-frame)
          nil)
      (clawmacs::request-chat-frame-redisplay frame))
    (is (= clawmacs::*chat-redisplay-enqueue-max-attempts* attempts))
    (is-true (clawmacs::chat-frame-redisplay-dirty-p frame))
    (is-false (clawmacs::chat-frame-redisplay-pending-p frame))))

(test identical-chat-frame-command-table-assignment-does-not-rebuild-menu-gadgets
  "ESA's per-turn EQ assignment cannot recreate an otherwise unchanged menu."
  (let* ((buf (make-buffer "equal-menu-table-assignment"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf))
         (same-table (clim:frame-command-table frame))
         (fresh-table (clim:make-command-table
                       nil :inherit-from '(clawmacs::clawmacs-chat-frame)))
         (notifications 0))
    (with-mcclim-test-function-override
        (clime:note-frame-command-table-changed
            (frame-manager requested-frame new-table)
          (declare (ignore frame-manager new-table))
          (is (eq frame requested-frame))
          (incf notifications))
      (is (eq same-table
              (setf (clim:frame-command-table frame) same-table)))
      (is (= 0 notifications))
      (is (eq fresh-table
              (setf (clim:frame-command-table frame) fresh-table)))
      (is (= 1 notifications)))))

(test frame-command-keeps-the-stable-menu-table
  "A state-mutating command redisplays content without replacing menu gadgets."
  (let* ((buf (make-buffer "stable-menu-command"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf))
         (table (clim:frame-command-table frame))
         (before (buffer-show-tool-results-p buf)))
    (clim:execute-frame-command
     frame '(clawmacs::com-chat-toggle-tool-results))
    (is (not (eql before (buffer-show-tool-results-p buf))))
    (is (eq table (clim:frame-command-table frame)))))

(test chat-frame-redisplay-wakeup-is-not-a-window-repaint-event
  "The async wakeup carries no window region and targets a real sheet at runtime."
  (let ((event (make-instance 'clawmacs::clawmacs-chat-redisplay-event
                              :sheet nil)))
    (is (typep event 'clim:window-manager-event))
    (is-false (typep event 'clim:window-event))))

(test runtime-stopped-hook-is-delivered-by-clim-redisplay
  "The frame process claims a reaper's pending public completion exactly once."
  (let* ((buf (make-buffer "runtime-stopped-clim-delivery"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buf))
         (frame-thread (bt:current-thread))
         (hook-events nil)
         (clawmacs::*buffer-display-wakeup-hook* nil)
         (clawmacs::*after-buffer-display-change-hook*
           (list
            (lambda (hook-buffer reason)
              (when (eq hook-buffer buf)
                (push (list (bt:current-thread)
                            reason
                            (clawmacs::buffer-runtime-stopping-p hook-buffer)
                            (clawmacs::buffer-runtime-teardown hook-buffer))
                      hook-events))))))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (bt:with-lock-held ((clawmacs::buffer-runtime-lock buf))
      (setf (clawmacs::buffer-runtime-stopped-notification-p buf) t
            (clawmacs::buffer-runtime-stopping-p buf) nil
            (clawmacs::buffer-runtime-teardown buf) nil))
    (bt:with-lock-held ((clawmacs::chat-frame-redisplay-lock frame))
      (setf (clawmacs::chat-frame-redisplay-dirty-p frame) t
            (clawmacs::chat-frame-redisplay-pending-p frame) t))
    (with-mcclim-test-function-override
        (clim:redisplay-frame-pane (requested-frame pane &key force-p)
          (declare (ignore requested-frame pane force-p))
          :redisplayed)
      (is (eq frame (clawmacs::handle-chat-frame-redisplay frame))))
    (is (= 1 (length hook-events)))
    (destructuring-bind (thread reason stopping-p teardown)
        (first hook-events)
      (is (eq frame-thread thread))
      (is (eq :runtime-stopped reason))
      (is-false stopping-p)
      (is (null teardown)))
    (is-false (clawmacs::buffer-runtime-stopped-notification-p buf))
    (is-false
     (clawmacs::deliver-buffer-runtime-stopped-notification buf))))

(test oauth-completion-is-applied-by-normal-clim-redisplay
  "The OAuth worker publishes state; the frame process claims and applies it."
  (let* ((buf (make-buffer "oauth-clim-redisplay"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buf))
         (flow (clawmacs::make-openai-oauth-flow :buffer buf))
         (frame-thread (bt:current-thread))
         (public-events nil)
         (clawmacs::*openai-oauth-pending* nil)
         (clawmacs::*openai-oauth-pending-lock*
           (bt:make-lock "test-clim-openai-oauth-pending"))
         (clawmacs::*buffer-display-wakeup-hook* nil)
         (clawmacs::*after-buffer-display-change-hook*
           (list
            (lambda (hook-buffer reason)
              (when (eq hook-buffer buf)
                (push (list (bt:current-thread) reason) public-events))))))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running
          (buffer-status buf) :oauth)
    (is-true (clawmacs::publish-openai-oauth-pending-flow flow))
    ;; This is the worker-side operation: record terminal state and request a
    ;; display change.  It must not mutate the buffer itself.
    (clawmacs::openai-oauth-flow-set-result
     flow :success t :token "test-token")
    (is (eq :oauth (buffer-status buf)))
    (is (eq flow (clawmacs::openai-oauth-pending-flow)))
    (is (null public-events))
    (bt:with-lock-held ((clawmacs::chat-frame-redisplay-lock frame))
      (setf (clawmacs::chat-frame-redisplay-dirty-p frame) t
            (clawmacs::chat-frame-redisplay-pending-p frame) t))
    (with-mcclim-test-function-override
        (clim:redisplay-frame-pane (requested-frame pane &key force-p)
          (declare (ignore requested-frame pane force-p))
          :redisplayed)
      (is (eq frame (clawmacs::handle-chat-frame-redisplay frame))))
    (is (null (clawmacs::openai-oauth-pending-flow)))
    (is (eq :idle (buffer-status buf)))
    (is (member :message public-events :key #'second :test #'eq))
    (is-true
     (every (lambda (event)
              (eq frame-thread (first event)))
            public-events))
    (is-true
     (loop :for message := (buffer-first-message buf)
             :then (message-next message)
           :while (and message
                       (not (eq message (buffer-input-message buf))))
           :thereis (search "Login successful" (message-text message))))))

(test asynchronous-redisplay-error-is-contained-and-later-request-recovers
  "A failing redisplay phase does not unwind the frame top level or wedge latches."
  (let* ((buf (make-buffer "redisplay-error-boundary"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf))
         (calls 0))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (with-mcclim-test-function-override
        (clim:redisplay-frame-pane (requested-frame pane &key force-p)
          (declare (ignore requested-frame pane force-p))
          (incf calls)
          (when (= calls 1)
            (error "injected redisplay failure"))
          :redisplayed)
      (bt:with-lock-held ((clawmacs::chat-frame-redisplay-lock frame))
        (setf (clawmacs::chat-frame-redisplay-dirty-p frame) t
              (clawmacs::chat-frame-redisplay-pending-p frame) t))
      (is (eq frame
              (clawmacs::handle-chat-frame-redisplay-safely frame)))
      (is (eq :running (clawmacs::chat-frame-lifecycle-state frame)))
      (is-false (clawmacs::chat-frame-redisplay-handling-p frame))
      (is-false (clawmacs::chat-frame-redisplay-pending-p frame))
      (bt:with-lock-held ((clawmacs::chat-frame-redisplay-lock frame))
        (setf (clawmacs::chat-frame-redisplay-dirty-p frame) t
              (clawmacs::chat-frame-redisplay-pending-p frame) t))
      (is (eq frame
              (clawmacs::handle-chat-frame-redisplay-safely frame)))
      (is (> calls 1))
      (is (eq :running (clawmacs::chat-frame-lifecycle-state frame)))
      (is-false (clawmacs::chat-frame-redisplay-handling-p frame)))))

(test redisplay-request-arriving-during-failed-handler-is-not-stranded
  "The error boundary queues dirty work after HANDLING is released."
  (let* ((buf (make-buffer "redisplay-error-concurrent-request"
                           :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buf))
         (redisplay-calls 0)
         (queue-calls 0))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (with-mcclim-test-function-override
        (clawmacs::queue-chat-frame-redisplay-event (requested-frame)
          (declare (ignore requested-frame))
          (incf queue-calls)
          t)
      (with-mcclim-test-function-override
          (clim:redisplay-frame-pane (requested-frame pane &key force-p)
            (declare (ignore force-p))
            ;; Count the transcript phase, not the independently redisplayed
            ;; info/minibuffer panes in the successful retry.
            (when (eq pane 'clawmacs::transcript)
              (incf redisplay-calls)
              (when (= redisplay-calls 1)
                ;; A worker-equivalent request lands while HANDLING is true,
                ;; then the current display phase fails before its epilogue.
                (clawmacs::request-chat-frame-redisplay requested-frame)
                (error "injected redisplay failure after concurrent request")))
            :redisplayed)
        (bt:with-lock-held ((clawmacs::chat-frame-redisplay-lock frame))
          (setf (clawmacs::chat-frame-redisplay-dirty-p frame) t
                (clawmacs::chat-frame-redisplay-pending-p frame) t))
        (is (eq frame
                (clawmacs::handle-chat-frame-redisplay-safely frame)))
        (is (= 1 queue-calls))
        (is-true (clawmacs::chat-frame-redisplay-dirty-p frame))
        (is-true (clawmacs::chat-frame-redisplay-pending-p frame))
        (is-false (clawmacs::chat-frame-redisplay-handling-p frame))
        ;; Consume the queued retry.  It succeeds and leaves every latch clear.
        (is (eq frame
                (clawmacs::handle-chat-frame-redisplay-safely frame)))
        (is (= 2 redisplay-calls))
        (is (= 1 queue-calls))
        (is-false (clawmacs::chat-frame-redisplay-dirty-p frame))
        (is-false (clawmacs::chat-frame-redisplay-pending-p frame))
        (is-false (clawmacs::chat-frame-redisplay-handling-p frame))))))

(test chat-frame-cleanup-continues-after-one-buffer-cancellation-fails
  "Frame teardown retires its hook and visits every buffer despite one error."
  (let* ((first (make-buffer "cleanup-first"
                             :session-persistence-mode :ephemeral))
         (second (make-buffer "cleanup-second"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer first))
         (hook (lambda (buffer reason)
                 (declare (ignore buffer reason))))
         (visited nil)
         (clawmacs::*buffer-ring* (list first second))
         (clawmacs::*buffer-display-wakeup-hook* nil)
         (clawmacs::*after-buffer-display-change-hook* nil))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running)
    (clawmacs::add-hook 'clawmacs::*buffer-display-wakeup-hook* hook)
    (with-mcclim-test-function-override
        (clawmacs::cancel-buffer-runtime-operations (buffer)
          (push (buffer-name buffer) visited)
          (when (eq buffer first)
            (error "injected cancellation failure"))
          buffer)
      (is (eq frame (clawmacs::cleanup-chat-frame-runtime frame hook))))
    (is (equal '("cleanup-first" "cleanup-second") (nreverse visited)))
    (is-false (member hook clawmacs::*buffer-display-wakeup-hook*
                      :test #'eq))
    (is (eq :stopped
            (clawmacs::chat-frame-lifecycle-state frame)))))

(test chat-frame-cleanup-makes-late-reaper-headless-and-self-finalizing
  "The last frame may retire its wake hook before a blocked owner exits."
  (let* ((buffer (make-buffer "cleanup-late-headless-reaper"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (hook (lambda (ignored-buffer ignored-reason)
                 (declare (ignore ignored-buffer ignored-reason))))
         (release (bt:make-semaphore :name "cleanup-late-reaper-release"))
         (worker
           (bt:make-thread
            (lambda ()
              (bt:wait-on-semaphore release :timeout 5.0))
            :name "cleanup-late-headless-owner"))
         (operation
           (clawmacs::make-interactive-buffer-operation
            :kind :cleanup-headless
            :buffer-generation (clawmacs::buffer-runtime-generation buffer)
            :worker worker))
         (clawmacs::*buffer-ring* (list buffer))
         (clawmacs::*buffer-display-wakeup-hook* nil)
         (clawmacs::*after-buffer-display-change-hook* nil))
    (setf (clawmacs::chat-frame-lifecycle-state frame) :running
          (buffer-pending-tool-calls buffer)
          (list (clawmacs::canonical-tool-use-block
                 "cleanup-tool" "read" nil)))
    (bt:with-lock-held ((clawmacs::buffer-runtime-lock buffer))
      (setf (buffer-pending-interactive-operation buffer) operation
            (buffer-status buffer) :working))
    (clawmacs::add-hook 'clawmacs::*buffer-display-wakeup-hook* hook)
    (unwind-protect
         (progn
           (let ((started-at (get-internal-real-time)))
             (is (eq frame
                     (clawmacs::cleanup-chat-frame-runtime frame hook)))
             (is (< (/ (- (get-internal-real-time) started-at)
                       (float internal-time-units-per-second 1.0))
                    1.0)))
           (is (eq :stopped (clawmacs::chat-frame-lifecycle-state frame)))
           (is-false (member hook clawmacs::*buffer-display-wakeup-hook*
                             :test #'eq))
           (is-true (clawmacs::buffer-disposing-p buffer))
           (is-true (clawmacs::buffer-runtime-stopping-p buffer))
           (is-false (clawmacs::buffer-disposed-p buffer))
           (bt:signal-semaphore release)
           (is-true
            (loop :repeat 400
                  :when (clawmacs::buffer-disposed-p buffer) :return t
                  :do (sleep 0.005)
                  :finally (return nil)))
           (is-false (clawmacs::buffer-disposing-p buffer))
           (is-false (clawmacs::buffer-runtime-stopping-p buffer))
           (is (null (clawmacs::buffer-runtime-teardown buffer)))
           (let ((result-message
                   (find :tool-result
                         (test-buffer-history-messages buffer)
                         :key #'message-sender)))
             (is-true result-message)
             (is (search "cancelled"
                         (message-text result-message)
                         :test #'char-equal))))
      (bt:signal-semaphore release)
      (when (bt:thread-alive-p worker)
        (bt:join-thread worker)))))

(test mcclim-compose-routes-modal-input-to-minibuffer
  "When M-x or another selector is active, compose keys are normalized for Clawmacs."
  (let* ((clawmacs::*chat-interaction-state*
           (clawmacs::make-chat-interaction-state))
         (event (make-instance 'clim:key-press-event
                               :sheet nil
                               :x 0
                               :y 0
                               :key-name nil
                               :key-character #\a
                               :modifier-state (clim:make-modifier-state)))
         (control-event (make-instance 'clim:key-press-event
                                       :sheet nil
                                       :x 0
                                       :y 0
                                       :key-name nil
                                       :key-character #\g
                                       :modifier-state
                                       (clim:make-modifier-state :control))))
    (setf clawmacs::*minibuffer-active* t
          clawmacs::*minibuffer-mode* :prompt
          clawmacs::*minibuffer-prompt* "M-x")
    (is-true (clawmacs::chat-compose-application-input-active-p))
    (is (eql #\a (clawmacs::chat-compose-event-key event)))
    (clawmacs::handle-key-event (make-buffer "modal-compose"
                                             :session-persistence-mode :ephemeral)
                                (clawmacs::chat-compose-event-key event))
    (is (string= "a" clawmacs::*minibuffer-input*))
    (is (eql (code-char 7) (clawmacs::chat-compose-event-key control-event)))))

(test frame-command-activation-focuses-the-modal-input-owner
  "Selectors opened outside compose cannot leave keyboard input on ESA's stream."
  (clawmacs::init-default-keymap)
  (let* ((current (make-buffer "focus-current"
                               :session-persistence-mode :ephemeral))
         (other (make-buffer "focus-other"
                             :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer current))
         (focused-frames nil)
         (clawmacs::*buffer-ring* (list current other)))
    (setf (buffer-keymap current) clawmacs::*default-keymap*
          (buffer-keymap other) clawmacs::*default-keymap*)
    (with-mcclim-test-function-override
        (clawmacs::focus-chat-compose-pane (requested-frame)
          (push requested-frame focused-frames)
          :compose)
      (with-mcclim-test-function-override
          (clawmacs::request-chat-frame-redisplay (requested-frame)
            requested-frame)
        (clim:execute-frame-command
         frame
         '(clawmacs::com-chat-dispatch-key (:ctrl-x #\b)))))
    (is-true
     (clawmacs::interaction-minibuffer-active-p
      (clawmacs::chat-frame-interaction-state frame)))
    (is (equal (list frame) focused-frames))))

(test mcclim-compose-normalizes-backtab-variants-for-modal-input
  "Shift-Tab/Backtab backend variants route to the minibuffer previous item key."
  (let ((shift-tab (make-instance 'clim:key-press-event
                                  :sheet nil
                                  :x 0
                                  :y 0
                                  :key-name :tab
                                  :key-character nil
                                  :modifier-state
                                  (clim:make-modifier-state :shift)))
        (shift-char-tab (make-instance 'clim:key-press-event
                                       :sheet nil
                                       :x 0
                                       :y 0
                                       :key-name nil
                                       :key-character #\Tab
                                       :modifier-state
                                       (clim:make-modifier-state :shift)))
        (backtab (make-instance 'clim:key-press-event
                                :sheet nil
                                :x 0
                                :y 0
                                :key-name :backtab
                                :key-character nil
                                :modifier-state (clim:make-modifier-state)))
        (iso-left-tab (make-instance 'clim:key-press-event
                                     :sheet nil
                                     :x 0
                                     :y 0
                                     :key-name :iso-left-tab
                                     :key-character nil
                                     :modifier-state (clim:make-modifier-state)))
        (meta-tab (make-instance 'clim:key-press-event
                                 :sheet nil
                                 :x 0
                                 :y 0
                                 :key-name nil
                                 :key-character #\Tab
                                 :modifier-state
                                 (clim:make-modifier-state :meta))))
    (is (eq :backtab (clawmacs::chat-compose-event-key shift-tab)))
    (is (eq :backtab (clawmacs::chat-compose-event-key shift-char-tab)))
    (is (eq :backtab (clawmacs::chat-compose-event-key backtab)))
    (is (eq :backtab (clawmacs::chat-compose-event-key iso-left-tab)))
    (is (equal '(:meta #\Tab)
               (clawmacs::chat-compose-event-key meta-tab)))))

(test mcclim-minibuffer-semantic-text-includes-visible-candidate-rows
  "Semantic E2E minibuffer text keeps the prompt summary and lists visible rows."
  (let ((clawmacs::*chat-interaction-state*
          (clawmacs::make-chat-interaction-state))
        (clawmacs::*minibuffer-max-height* 3))
    (setf clawmacs::*minibuffer-active* t
          clawmacs::*minibuffer-mode* :completion
          clawmacs::*minibuffer-prompt* "M-x"
          clawmacs::*minibuffer-input* "td"
          clawmacs::*minibuffer-point* 2
          clawmacs::*minibuffer-filtered-items*
          (list (list :display "toggle-debug-mode-command")
                (list :display "toggle-tool-results-command")
                (list :display "toggle-metadata-output-command"))
          clawmacs::*minibuffer-selected-index* 1)
    (let ((text (clawmacs::chat-frame-e2e-minibuffer-text)))
      (is (search "M-x: td  [toggle-tool-results-command]  (2/3)" text))
      (is (search "  toggle-debug-mode-command" text))
      (is (search "> toggle-tool-results-command" text))
      (is-false (search "toggle-metadata-output-command" text)))))

(test chat-interaction-state-is-owned-by-each-frame
  "Two frames never share transient minibuffer or callback state."
  (let* ((first-buffer (make-buffer "interaction-first"
                                    :session-persistence-mode :ephemeral))
         (second-buffer (make-buffer "interaction-second"
                                     :session-persistence-mode :ephemeral))
         (first-frame (clim:make-application-frame
                       'clawmacs::clawmacs-chat-frame
                       :buffer first-buffer))
         (second-frame (clim:make-application-frame
                        'clawmacs::clawmacs-chat-frame
                        :buffer second-buffer))
         (first-state (clawmacs::chat-frame-interaction-state first-frame))
         (second-state (clawmacs::chat-frame-interaction-state second-frame)))
    (is-false (eq first-state second-state))
    (clawmacs::call-chat-frame-ui-action-safely
     first-frame "first interaction"
     (lambda ()
       (clawmacs::minibuffer-prompt "First" #'identity
                                    :initial-input "one")))
    (clawmacs::call-chat-frame-ui-action-safely
     second-frame "second interaction"
     (lambda ()
       (clawmacs::minibuffer-prompt "Second" #'identity
                                    :initial-input "two")))
    (is (string= "First"
                 (clawmacs::interaction-minibuffer-prompt first-state)))
    (is (string= "one"
                 (clawmacs::interaction-minibuffer-input first-state)))
    (is (string= "Second"
                 (clawmacs::interaction-minibuffer-prompt second-state)))
    (is (string= "two"
                 (clawmacs::interaction-minibuffer-input second-state)))))

(test chat-interaction-callback-preserves-nested-frame-binding
  "A callback can enter a second frame and returns to the first state pointer."
  (let* ((first-buffer (make-buffer "nested-first"
                                    :session-persistence-mode :ephemeral))
         (second-buffer (make-buffer "nested-second"
                                     :session-persistence-mode :ephemeral))
         (first-frame (clim:make-application-frame
                       'clawmacs::clawmacs-chat-frame
                       :buffer first-buffer))
         (second-frame (clim:make-application-frame
                        'clawmacs::clawmacs-chat-frame
                        :buffer second-buffer))
         (first-state (clawmacs::chat-frame-interaction-state first-frame))
         (second-state (clawmacs::chat-frame-interaction-state second-frame))
         (observed nil))
    (clawmacs::call-chat-frame-ui-action-safely
     first-frame "nested callback"
     (lambda ()
       (clawmacs::minibuffer-prompt
        "Outer"
        (lambda (value)
          (push (list :before (eq clawmacs::*chat-interaction-state*
                                  first-state)
                      :value value)
                observed)
          (clawmacs::call-chat-frame-ui-action-safely
           second-frame "inner callback"
           (lambda ()
             (clawmacs::minibuffer-prompt "Inner" #'identity)))
          (push (list :after (eq clawmacs::*chat-interaction-state*
                                 first-state))
                observed)))
       (setf clawmacs::*minibuffer-input* "accepted"
             clawmacs::*minibuffer-point* 8)
       (clawmacs::minibuffer-confirm)))
    (is-true (getf (second observed) :before))
    (is (string= "accepted" (getf (second observed) :value)))
    (is-true (getf (first observed) :after))
    (is-false (clawmacs::interaction-minibuffer-active-p first-state))
    (is-true (clawmacs::interaction-minibuffer-active-p second-state))
    (is (string= "Inner"
                 (clawmacs::interaction-minibuffer-prompt second-state)))))

(test chat-frame-cleanup-clears-transient-callback-ownership
  "Frame cleanup releases callbacks and buffer references from its state."
  (let* ((buffer (make-buffer "interaction-cleanup"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (state (clawmacs::chat-frame-interaction-state frame))
         (generation (clawmacs::chat-interaction-state-generation state)))
    (clawmacs::call-chat-frame-ui-action-safely
     frame "prepare cleanup"
     (lambda ()
       (clawmacs::minibuffer-activate
        "Owned callback"
        (list (list :display "one"))
        (lambda (item) item))
       (setf clawmacs::*session-tree-selector-buffer* buffer
             clawmacs::*slash-completion-buffer* buffer
             clawmacs::*skill-completion-buffer* buffer)))
    (clawmacs::cleanup-chat-frame-runtime frame nil)
    (is-false (clawmacs::interaction-minibuffer-active-p state))
    (is (null (clawmacs::interaction-minibuffer-callback state)))
    (is (null (clawmacs::interaction-session-tree-buffer state)))
    (is (null (clawmacs::interaction-slash-completion-buffer state)))
    (is (null (clawmacs::interaction-skill-completion-buffer state)))
    (is (> (clawmacs::chat-interaction-state-generation state) generation))
    (is (eq :stopped (clawmacs::chat-frame-lifecycle-state frame)))))

(test chat-interaction-candidate-requires-exact-state-and-generation
  "Candidate refs from another frame or an older generation are rejected."
  (let* ((buffer (make-buffer "candidate-generation"
                              :session-persistence-mode :ephemeral))
         (other-buffer (make-buffer "candidate-other"
                                    :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (other-frame (clim:make-application-frame
                       'clawmacs::clawmacs-chat-frame
                       :buffer other-buffer))
         (state (clawmacs::chat-frame-interaction-state frame))
         (chosen nil)
         (first-item (list :display "first"))
         (second-item (list :display "second")))
    (clawmacs::call-chat-frame-ui-action-safely
     frame "candidate setup"
     (lambda ()
       (clawmacs::minibuffer-activate
        "Pick"
        (list first-item second-item)
        (lambda (item) (setf chosen item)))))
    (let* ((generation
             (clawmacs::chat-interaction-state-generation state))
           (ref (clawmacs::make-chat-interaction-candidate-ref
                 state generation :minibuffer 0 first-item)))
      (is-true (clawmacs::chat-interaction-candidate-current-p frame ref))
      (is-false
       (clawmacs::chat-interaction-candidate-current-p other-frame ref))
      (clawmacs::call-chat-frame-ui-action-safely
       frame "advance candidate"
       #'clawmacs::minibuffer-next-item)
      (is-false (clawmacs::chat-interaction-candidate-current-p frame ref))
      (is (null chosen))
      (let ((current-ref
              (clawmacs::make-chat-interaction-candidate-ref
               state
               (clawmacs::chat-interaction-state-generation state)
               :minibuffer 1 second-item)))
        (is-true
         (clawmacs::choose-chat-interaction-candidate frame current-ref))
        (is (eq second-item chosen))))))

(test retained-interaction-candidates-are-visible-in-declared-pane
  "Session-tree, slash, and skill candidates all have visible semantic rows."
  (let* ((buffer (make-buffer "visible-interactions"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (state (clawmacs::chat-frame-interaction-state frame)))
    (let ((clawmacs::*chat-interaction-state* state))
      (setf clawmacs::*session-tree-selector-active* t
            clawmacs::*session-tree-selector-buffer* buffer
            clawmacs::*session-tree-selector-filtered-items*
            (list (list :id "entry-1"
                        :tree-prefix "|- "
                        :kind-label "user"
                        :content "visible branch"
                        :active-p t
                        :display "unused")))
      (clawmacs::touch-chat-interaction-state))
    (let ((text (clawmacs::chat-frame-e2e-screen-text frame)))
      (is (search "Session Tree" text))
      (is (search "visible branch" text)))
    (clawmacs::clear-chat-interaction-state state)
    (let ((clawmacs::*chat-interaction-state* state))
      (setf clawmacs::*slash-completion-active* t
            clawmacs::*slash-completion-buffer* buffer
            clawmacs::*slash-completion-query* "he"
            clawmacs::*slash-completion-filtered-items*
            (list (list :name "help" :display "/help")))
      (clawmacs::touch-chat-interaction-state))
    (let ((text (clawmacs::chat-frame-e2e-screen-text frame)))
      (is (search "Slash command: /he" text))
      (is (search "/help" text)))
    (clawmacs::clear-chat-interaction-state state)
    (let ((clawmacs::*chat-interaction-state* state))
      (setf clawmacs::*skill-completion-active* t
            clawmacs::*skill-completion-buffer* buffer
            clawmacs::*skill-completion-query* "demo"
            clawmacs::*skill-completion-filtered-items*
            (list (list :display "$demo")))
      (clawmacs::touch-chat-interaction-state))
    (let ((text (clawmacs::chat-frame-e2e-screen-text frame)))
      (is (search "Skill mention: $demo" text))
      (is (search "$demo" text)))))

(test obsolete-overlay-flags-no-longer-capture-compose-input
  "Old buffer/model/think flags cannot create an invisible modal key sink."
  (clawmacs::init-default-keymap)
  (let ((clawmacs::*chat-interaction-state*
          (clawmacs::make-chat-interaction-state))
        (clawmacs::*buffer-selector-active* t)
        (clawmacs::*model-selector-active* t)
        (clawmacs::*think-selector-active* t)
        (clawmacs::*buffer-ring* nil))
    (let ((buffer (make-buffer "obsolete-overlays"
                               :session-persistence-mode :ephemeral)))
      (setf (buffer-keymap buffer) clawmacs::*default-keymap*)
      (is-false (clawmacs::chat-compose-application-input-active-p buffer))
      (clawmacs::handle-key-event buffer #\z)
      (is (string= "z" (message-text (buffer-input-message buffer)))))))

(test legacy-model-command-opens-visible-minibuffer
  "SELECT-MODEL-COMMAND delegates to the standard minibuffer presentation."
  (let* ((buffer (make-buffer "visible-model-selector"
                              :session-persistence-mode :ephemeral))
         (clawmacs::*chat-interaction-state*
           (clawmacs::make-chat-interaction-state))
         (clawmacs::*model-selector-active* nil))
    (with-mcclim-test-function-override
        (clawmacs::available-models-for-selector (ignored-buffer)
          (declare (ignore ignored-buffer))
          (list (list :provider :openai-codex
                      :model "gpt-test"
                      :active-p t)))
      (clawmacs::select-model-command buffer))
    (is-false clawmacs::*model-selector-active*)
    (is-true clawmacs::*minibuffer-active*)
    (is (string= "Select Model" clawmacs::*minibuffer-prompt*))
    (is (search "gpt-test"
                (clawmacs::minibuffer-item-display
                 (first clawmacs::*minibuffer-filtered-items*))))))

(test mcclim-buffer-presentation-function-supplies-semantic-transcript
  "Custom buffer presentation hooks feed the same semantic path GUI E2E reads."
  (let ((clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry)))
    (flet ((semantic-entries (buffer columns)
             (list (list :text (format nil "custom view for ~A" (buffer-name buffer))
                         :face :selector-title
                         :unique-id :header)
                   (list :text (format nil "columns=~D" columns)
                         :face :selector-footer))))
      (register-buffer-type :semantic-view
                            :major-mode "semantic"
                            :presentation-function #'semantic-entries)
      (let* ((buffer (make-buffer "semantic-buffer"
                                  :kind :semantic-view
                                  :session-persistence-mode :ephemeral))
             (text (clawmacs::chat-frame-e2e-transcript-text buffer)))
        (is (search "custom view for semantic-buffer" text))
        (is (search "columns=100" text))))))

(test disabled-e2e-snapshot-instrumentation-never-runs-presenters
  "Production redisplay does not evaluate the E2E semantic mirror."
  (let ((clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry))
        (presentation-calls 0)
        (emitted-event nil))
    (flet ((counted-entries (buffer columns)
             (declare (ignore buffer columns))
             (incf presentation-calls)
             (list (list :text "instrumented view"))))
      (register-buffer-type :instrumented-view
                            :major-mode "instrumented"
                            :presentation-function #'counted-entries)
      (let* ((buffer
               (make-buffer "instrumented-buffer"
                            :kind :instrumented-view
                            :session-persistence-mode :ephemeral))
             (frame
               (clim:make-application-frame
                'clawmacs::clawmacs-chat-frame
                :buffer buffer)))
        (let ((clawmacs::*debug-log-file* nil)
              (clawmacs::*e2e-events-enabled-override* t))
          (clawmacs::emit-chat-frame-e2e-snapshot
           frame :reason "disabled-no-log"))
        (is (= 0 presentation-calls))
        (let ((clawmacs::*debug-log-file*
                #P"/tmp/clawmacs-disabled-e2e-snapshot.jsonl"))
          (with-mcclim-test-function-override
              (clawmacs::e2e-events-enabled-p () nil)
            (clawmacs::emit-chat-frame-e2e-snapshot
             frame :reason "disabled-no-events")))
        (is (= 0 presentation-calls))
        (let ((clawmacs::*debug-log-file*
                #P"/tmp/clawmacs-enabled-e2e-snapshot.jsonl")
              (clawmacs::*e2e-events-enabled-override* t))
          (with-mcclim-test-function-override
              (clawmacs::file-debug-event (event-name &rest payload)
                (declare (ignore payload))
                (setf emitted-event event-name))
            (clawmacs::emit-chat-frame-e2e-snapshot
             frame :reason "enabled")))
        (is (= 1 presentation-calls))
        (is (string= "ui-snapshot" emitted-event))))))

(test redisplay-e2e-snapshot-carries-repeat-state
  "A handled snapshot says whether another redisplay cycle is already queued."
  (let* ((buffer
           (make-buffer "redisplay-snapshot-repeat"
                        :session-persistence-mode :ephemeral))
         (frame
           (clim:make-application-frame
            'clawmacs::clawmacs-chat-frame
            :buffer buffer))
         (payloads nil)
         (clawmacs::*debug-log-file*
           #P"/tmp/clawmacs-redisplay-snapshot-repeat.jsonl")
         (clawmacs::*e2e-events-enabled-override* t))
    (with-mcclim-test-function-override
        (clawmacs::file-debug-event (event-name &rest payload)
          (when (string= event-name "ui-snapshot")
            (push payload payloads)))
      (clawmacs::emit-chat-frame-e2e-snapshot
       frame :reason "redisplay-handled" :repeat t)
      (clawmacs::emit-chat-frame-e2e-snapshot
       frame :reason "redisplay-handled" :repeat nil)
      (clawmacs::emit-chat-frame-e2e-snapshot
       frame :reason "pane-rendered" :pane "transcript"))
    (setf payloads (nreverse payloads))
    (is (= 3 (length payloads)))
    (is-true (getf (first payloads) :repeat))
    (is (member :repeat (second payloads)))
    (is-false (getf (second payloads) :repeat))
    (is-false (member :repeat (third payloads)))))

(test mcclim-input-presentation-function-appends-semantic-overlay
  "Input presentation hooks append package-owned overlays to screen snapshots."
  (let ((clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry)))
    (flet ((input-entries (buffer columns)
             (declare (ignore columns))
             (list (list :text (format nil "input overlay for ~A"
                                       (buffer-name buffer))
                         :face :selector-header))))
      (register-buffer-type :overlay-view
                            :major-mode "overlay"
                            :input-presentation-function #'input-entries)
      (let* ((buffer (make-buffer "overlay-buffer"
                                  :kind :overlay-view
                                  :session-persistence-mode :ephemeral))
             (frame (clim:make-application-frame
                     'clawmacs::clawmacs-chat-frame
                     :buffer buffer))
             (text (clawmacs::chat-frame-e2e-screen-text frame)))
        (is (search "No messages yet." text))
        (is (search "input overlay for overlay-buffer" text))))))

(test mcclim-compose-pane-declares-fixed-geometry-at-construction
  "The real compose pane declares its wrap policy and fixed geometry to CLIM."
  (let ((clawmacs::*chat-compose-visible-rows* 5)
        (clawmacs::*chat-compose-line-height* 24))
    (let* ((pane (make-geometry-test-chat-compose-pane))
           (expected (clawmacs::chat-compose-desired-pixel-height))
           (space (clim:compose-space pane)))
      (is (typep pane 'clawmacs::clawmacs-chat-compose-pane))
      (is (= expected (clim:space-requirement-height space)))
      (is (= expected (clim:space-requirement-min-height space)))
      (is (= expected (clim:space-requirement-max-height space)))
      (is (eq :wrap* (clim:stream-end-of-line-action pane))))))

(test mcclim-compose-pane-notifications-do-not-mutate-space-requirements
  "Graft and region notifications leave compose geometry to its constructor."
  (let ((pane (make-geometry-test-chat-compose-pane)))
    (is (typep pane 'clawmacs::clawmacs-chat-compose-pane))
    (is-false
     (cl:find-method #'clim:note-sheet-grafted nil
                       (list (find-class 'clawmacs::clawmacs-chat-compose-pane))
                       nil))
    (is-false
     (cl:find-method #'clim:note-sheet-region-changed nil
                       (list (find-class 'clawmacs::clawmacs-chat-compose-pane))
                       nil))))

(test mcclim-compose-drei-bindings-are-pane-scoped
  "Compose bindings use Drei's pane extension hook without global mutation."
  (let* ((indent-table (clim:find-command-table 'drei:indent-table))
         (deletion-table (clim:find-command-table 'drei:deletion-table))
         (indent-before (test-direct-command-table-keystrokes indent-table))
         (deletion-before
           (test-direct-command-table-keystrokes deletion-table))
         (compose (make-instance 'clawmacs::clawmacs-chat-compose-pane))
         (plain-drei (make-instance 'drei:drei-gadget-pane))
         (compose-tables
           (drei-syntax:additional-command-tables compose indent-table))
         (plain-tables
           (drei-syntax:additional-command-tables plain-drei indent-table)))
    ;; Reinstallation is idempotent and touches only Clawmacs' table.
    (clawmacs::install-chat-compose-drei-keybindings)
    (is (equal indent-before
               (test-direct-command-table-keystrokes indent-table)))
    (is (equal deletion-before
               (test-direct-command-table-keystrokes deletion-table)))
    (is (member 'clawmacs::clawmacs-chat-compose-editing-table
                compose-tables))
    (is-false
     (member 'clawmacs::clawmacs-chat-compose-editing-table plain-tables))
    (let ((bindings
            (test-direct-command-table-keystrokes
             (clim:find-command-table
              'clawmacs::clawmacs-chat-compose-editing-table))))
      (dolist (gesture '((#\Newline :control)
                         (#\w :control)
                         (#\Backspace :control)
                         (#\Rubout :control)))
        (is (find gesture bindings :key #'second :test #'equal))))))

(test mcclim-minibuffer-pane-compose-space-is-fixed-height
  "The chat minibuffer pane computes space without backend font metrics."
  (let* ((pane (make-instance 'clawmacs::clawmacs-chat-minibuffer-pane
                               :display-function 'clawmacs::display-chat-minibuffer-pane
                               :display-time :command-loop
                               :width 900))
         (space (clim:compose-space pane)))
    (is (= clawmacs::*chat-minibuffer-line-height*
           (clim:space-requirement-height space)))
    (is (= clawmacs::*chat-minibuffer-line-height*
           (clim:space-requirement-min-height space)))
    (is (= clawmacs::*chat-minibuffer-line-height*
           (clim:space-requirement-max-height space)))))

(test mcclim-minibuffer-row-height-uses-live-clim-metrics
  "A grafted pane's CLIM line metric supersedes the conservative startup fallback."
  (let ((pane (make-instance 'clawmacs::clawmacs-chat-minibuffer-pane
                             :display-function
                             'clawmacs::display-chat-minibuffer-pane
                             :display-time :command-loop
                             :width 900))
        (clawmacs::*chat-minibuffer-line-height* 24))
    (with-mcclim-test-function-override
        (clim:stream-line-height (stream &key text-style)
          (declare (ignore stream text-style))
          29)
      (is (= 29 (clawmacs::chat-minibuffer-row-pixel-height pane))))))

(test mcclim-minibuffer-sizing-reads-the-frame-owned-interaction
  "Pane sizing cannot silently consult another frame or the fallback global state."
  (let* ((buffer (make-buffer "frame-owned-minibuffer-size"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (pane (make-instance 'clawmacs::clawmacs-chat-minibuffer-pane
                              :display-function
                              'clawmacs::display-chat-minibuffer-pane
                              :display-time :command-loop
                              :width 900))
         (state (clawmacs::chat-frame-interaction-state frame))
         (unrelated-state (clawmacs::make-chat-interaction-state)))
    (let ((clawmacs::*chat-interaction-state* state))
      (clawmacs::minibuffer-activate
       "Frame Owned"
       (list (list :display "one")
             (list :display "two")
             (list :display "three"))
       #'identity))
    (let ((clawmacs::*chat-interaction-state* unrelated-state)
          (expected-height
            (clawmacs::chat-minibuffer-content-pixel-height pane 4)))
      (with-mcclim-test-function-override
          (clim:find-pane-named (requested-frame pane-name)
            (is (eq frame requested-frame))
            (is (eq 'clawmacs::minibuffer pane-name))
            pane)
        (clawmacs::update-chat-minibuffer-space-requirements frame))
      (let ((space (clim:compose-space pane)))
        (is (= expected-height
               (clim:space-requirement-height space)))
        (is (= expected-height
               (clim:space-requirement-min-height space)))
        (is (= expected-height
               (clim:space-requirement-max-height space)))))))

(test mcclim-minibuffer-sizing-propagates-to-the-frame
  "An expanded selector asks CLIM to resize the top-level frame hierarchy."
  (let* ((buffer (make-buffer "frame-resized-minibuffer"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (pane (make-instance 'clawmacs::clawmacs-chat-minibuffer-pane
                              :display-function
                              'clawmacs::display-chat-minibuffer-pane
                              :display-time :command-loop
                              :width 900))
         (state (clawmacs::chat-frame-interaction-state frame))
         (resize-frame-p nil))
    (let ((clawmacs::*chat-interaction-state* state))
      (clawmacs::minibuffer-activate
       "Resize Frame"
       (list (list :display "one")
             (list :display "two"))
       #'identity))
    (with-mcclim-test-function-override
        (clim:find-pane-named (requested-frame pane-name)
          (declare (ignore requested-frame pane-name))
          pane)
      (with-mcclim-test-function-override
          (clim:change-space-requirements
              (requested-pane &rest arguments
               &key resize-frame &allow-other-keys)
            (declare (ignore arguments))
            (is (eq pane requested-pane))
            (setf resize-frame-p resize-frame))
        (clawmacs::update-chat-minibuffer-space-requirements frame)))
    (is-true resize-frame-p)))

(test mcclim-minibuffer-sizing-skips-redundant-frame-layout
  "An unchanged collapsed pane does not make CLIM relayout the whole frame."
  (let* ((buffer (make-buffer "unchanged-minibuffer-size"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (pane (make-instance 'clawmacs::clawmacs-chat-minibuffer-pane
                              :display-function
                              'clawmacs::display-chat-minibuffer-pane
                              :display-time :command-loop
                              :width 900))
         (change-called-p nil))
    (with-mcclim-test-function-override
        (clim:find-pane-named (requested-frame pane-name)
          (declare (ignore requested-frame pane-name))
          pane)
      (with-mcclim-test-function-override
          (clim:change-space-requirements
              (requested-pane &rest arguments)
            (declare (ignore requested-pane arguments))
            (setf change-called-p t))
        (clawmacs::update-chat-minibuffer-space-requirements frame)))
    (is-false change-called-p)))

(test mcclim-minibuffer-sizing-skips-repeated-expanded-layout
  "An already-expanded selector does not repeatedly resize the top-level frame."
  (let* ((buffer (make-buffer "unchanged-expanded-minibuffer-size"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer))
         (pane (make-instance 'clawmacs::clawmacs-chat-minibuffer-pane
                              :display-function
                              'clawmacs::display-chat-minibuffer-pane
                              :display-time :command-loop
                              :width 900))
         (state (clawmacs::chat-frame-interaction-state frame))
         (repeat-change-called-p nil))
    (let ((clawmacs::*chat-interaction-state* state))
      (clawmacs::minibuffer-activate
       "Expanded"
       (list (list :display "one")
             (list :display "two")
             (list :display "three"))
       #'identity))
    (with-mcclim-test-function-override
        (clim:find-pane-named (requested-frame pane-name)
          (declare (ignore requested-frame pane-name))
          pane)
      ;; First apply the real CLIM space requirement, then prove the guard
      ;; suppresses the identical request on the next redisplay pass.
      (clawmacs::update-chat-minibuffer-space-requirements frame)
      (with-mcclim-test-function-override
          (clim:change-space-requirements
              (requested-pane &rest arguments)
            (declare (ignore requested-pane arguments))
            (setf repeat-change-called-p t))
        (clawmacs::update-chat-minibuffer-space-requirements frame)))
    (is-false repeat-change-called-p)))

(test mcclim-custom-presentation-buffers-append-system-feedback
  "Whole-buffer custom presenters still surface system feedback messages."
  (let ((clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry)))
    (flet ((entries (buffer columns)
             (declare (ignore buffer columns))
             (list (list :text "custom dashboard" :face :selector-title))))
      (register-buffer-type :feedback-view
                            :major-mode "feedback"
                            :presentation-function #'entries)
      (let ((buffer (make-buffer "feedback-buffer"
                                 :kind :feedback-view
                                 :session-persistence-mode :ephemeral)))
        (buffer-insert-system-message buffer "[custom feedback]")
        (let ((text (clawmacs::chat-frame-e2e-transcript-text buffer)))
          (is (search "custom dashboard" text))
          (is (search "[custom feedback]" text)))))))

(test mcclim-presentation-hook-errors-render-as-feedback
  "Presentation hook failures become visible error entries instead of aborting redisplay."
  (let ((clawmacs::*buffer-type-registry*
          (clawmacs::make-buffer-type-registry)))
    (flet ((broken-entries (buffer columns)
             (declare (ignore buffer columns))
             (error "broken presenter")))
      (register-buffer-type :broken-view
                            :major-mode "broken"
                            :presentation-function #'broken-entries)
      (let* ((buffer (make-buffer "broken-buffer"
                                  :kind :broken-view
                                  :session-persistence-mode :ephemeral))
             (text (clawmacs::chat-frame-e2e-transcript-text buffer)))
          (is (search "Presentation error" text))
          (is (search "broken presenter" text))))))

(test appearance-editor-stages-and-previews-without-mutating-active-profile
  "Opening and switching the editor changes only immutable staged state."
  (let* ((buffer (make-buffer "appearance-editor-stage"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame :buffer buffer))
         (active (clawmacs::chat-frame-appearance-profile frame))
         (clim:*application-frame* frame)
         (clawmacs::*buffer-ring* (list buffer)))
    (clawmacs::customize-appearance-command buffer)
    (let ((editor (clawmacs::chat-frame-appearance-editor-buffer frame)))
      (is (eq editor (current-buffer)))
      (is (eq :appearance-editor (buffer-kind editor)))
      (clawmacs::switch-appearance-theme-command frame :dark)
      (is (eq active (clawmacs::chat-frame-appearance-profile frame)))
      (is (eq :dark
              (appearance-profile-selected-theme
               (appearance-candidate-profile
                (clawmacs::chat-frame-appearance-staged-candidate frame)))))
      (let ((entries (clawmacs::appearance-editor-display-entries editor)))
        (is (find 'clawmacs::appearance-theme-ref entries
                  :key (lambda (entry) (getf entry :presentation-type))))
        (is (find 'clawmacs::appearance-role-ref entries
                  :key (lambda (entry) (getf entry :presentation-type))))
        (is (some (lambda (entry)
                    (and (getf entry :appearance-profile)
                         (getf entry :role-stack)))
                  entries))))))

(test appearance-editor-font-choices-are-dependent-and-data-only
  "Family, face, and size choices derive only from the frame inventory."
  (let* ((buffer (make-buffer "appearance-editor-fonts"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame :buffer buffer))
         (choices
           (list (make-enumerated-font-choice
                  :family-display "Alpha" :face-display "Book" :size 12)
                 (make-enumerated-font-choice
                  :family-display "Alpha" :face-display "Bold" :size 14)
                 (make-enumerated-font-choice
                  :family-display "Alpha" :face-display "Book" :size 16)
                 (make-enumerated-font-choice
                  :family-display "Beta" :face-display "Regular" :size 10)))
         (inventory
           (clawmacs::%make-appearance-font-inventory
            :port :test :generation 1 :entries nil :choices choices
            :metric-medium nil :negative-cache (make-hash-table :test #'equal))))
    (setf (slot-value frame 'clawmacs::appearance-font-inventory) inventory)
    (is (equal '("Alpha" "Beta")
               (clawmacs::appearance-editor-font-families frame)))
    (is (equal '("Bold" "Book")
               (clawmacs::appearance-editor-font-faces frame "Alpha")))
    (is (equal '(12 16)
               (clawmacs::appearance-editor-font-sizes
                frame "Alpha" "Book")))
    (is (null (clawmacs::appearance-editor-font-sizes
               frame "Beta" "Book")))))

(test appearance-editor-apply-is-a-queue-and-reload-failure-retains-state
  "Apply never publishes directly and failed reload preserves active and staging."
  (let* ((buffer (make-buffer "appearance-editor-actions"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame :buffer buffer))
         (active (clawmacs::chat-frame-appearance-profile frame))
         (candidate (make-appearance-candidate
                     (make-appearance-profile :selected-theme :dark)))
         (queued nil))
    (setf (slot-value frame 'clawmacs::appearance-staged-candidate) candidate)
    (with-mcclim-test-function-override
        (clawmacs::request-chat-frame-appearance-activation
            (requested-frame requested-candidate)
          (setf queued (list requested-frame requested-candidate))
          t)
      (is (eq :queued
              (getf (clawmacs::apply-staged-appearance-command frame)
                    :status))))
    (is (equal (list frame candidate) queued))
    (is (eq active (clawmacs::chat-frame-appearance-profile frame)))
    (with-mcclim-test-function-override
        (clawmacs:reload-appearance-file-profile (requested-active)
          (is (eq active requested-active))
          (values requested-active nil))
      (is (eq :retained-active
              (getf (clawmacs::reload-appearance-file-command frame)
                    :status))))
    (is (eq candidate
            (clawmacs::chat-frame-appearance-staged-candidate frame)))
    (is (eq active (clawmacs::chat-frame-appearance-profile frame)))))

(test package-appearance-catalog-plan-preserves-active-staged-and-persisted-state
  "A same-owner catalog replacement validates, but never clears, frame-local profiles.

The fake adopted port keeps this a deterministic frame transaction test: it
exercises the normal complete-bundle resolver without opening a CLX window."
  (let* ((buffer (make-buffer "appearance-package-preservation"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame :buffer buffer))
         (active (clawmacs::chat-frame-appearance-profile frame))
         (staged (make-appearance-candidate active))
         (persisted (make-appearance-profile :selected-theme :classic))
         (old-catalog (clawmacs::chat-frame-appearance-catalog frame))
         (replacement
           (make-appearance-catalog
            :role-definitions (appearance-catalog-role-definitions old-catalog)
            :theme-definitions (appearance-catalog-theme-definitions old-catalog)
            :built-in-overlays (appearance-catalog-built-in-overlays old-catalog)
            :generation (1+ (appearance-catalog-generation old-catalog))))
         (clawmacs::*appearance-configuration-access-count* 23)
         (clawmacs::*appearance-startup-resolution-count* 29))
    (setf (slot-value frame 'clawmacs::appearance-staged-candidate) staged
          (clawmacs::chat-frame-appearance-persisted-profile frame) persisted)
    (with-mcclim-test-function-override
        (clawmacs::chat-frame-appearance-live-port (requested-frame)
          (declare (ignore requested-frame)) :package-test-port)
      (let ((plan (clawmacs::package-appearance-frame-transition-plan
                   frame replacement)))
        (is (eq :no-op (getf plan :status)))
        (is (eq active (getf plan :profile)))
        (is (typep (getf plan :bundle) 'resolved-appearance-bundle))
        (is (eq staged (getf plan :staged-candidate)))
        (is (eq persisted (getf plan :persisted-profile)))))
    ;; The planner/event preparation path is declaration-only: it neither
    ;; reads preferences nor starts initialization, and it leaves both
    ;; non-active profile objects untouched for the committed event.
    (is (eq active (clawmacs::chat-frame-appearance-profile frame)))
    (is (eq staged (clawmacs::chat-frame-appearance-staged-candidate frame)))
    (is (eq persisted (clawmacs::chat-frame-appearance-persisted-profile frame)))
    (is (= 23 clawmacs::*appearance-configuration-access-count*))
    (is (= 29 clawmacs::*appearance-startup-resolution-count*))))

(test package-appearance-active-theme-fallback-is-classified-and-published-atomically
  "Package-theme removal falls back only when its complete bundle is live-safe."
  (labels ((package-theme-catalog (overlays)
             (let ((classic (make-classic-appearance-catalog)))
               (make-appearance-catalog
                :role-definitions
                (appearance-catalog-role-definitions classic)
                :theme-definitions
                (append
                 (appearance-catalog-theme-definitions classic)
                 (list
                  (make-appearance-theme-definition
                   :id '(:package "org.example.theme" "active")
                   :parent-theme :classic
                   :role-overlays overlays
                   :owner "org.example.theme")))
                :built-in-overlays
                (appearance-catalog-built-in-overlays classic)
                :generation 7)))
           (make-frame-for-catalog (catalog)
             (let* ((frame
                      (clim:make-application-frame
                       'clawmacs::clawmacs-chat-frame
                       :buffer
                       (make-buffer
                        (format nil "package-theme-~D"
                                (get-internal-real-time))
                        :session-persistence-mode :ephemeral)))
                    (profile
                      (make-appearance-profile
                       :selected-theme
                       '(:package "org.example.theme" "active")))
                    (bundle
                      (resolve-appearance-profile-bundle
                       catalog profile
                       :profile-revision 0
                       :font-inventory-generation 0
                       :port-identity :package-test-port)))
               (setf (slot-value frame 'clawmacs::appearance-catalog) catalog
                     (clawmacs::chat-frame-appearance-profile frame) profile
                     (slot-value frame 'clawmacs::appearance-active-bundle)
                     bundle)
               frame)))
    (let* ((candidate (make-classic-appearance-catalog))
           (old-catalog (package-theme-catalog nil))
           (frame (make-frame-for-catalog old-catalog)))
      (with-mcclim-test-function-override
          (clawmacs::chat-frame-appearance-live-port (requested-frame)
            (declare (ignore requested-frame))
            :package-test-port)
        (let* ((plan
                 (clawmacs::package-appearance-frame-transition-plan
                  frame candidate))
               (bundle (getf plan :bundle))
               (token
                 (clawmacs::make-appearance-package-transition-token)))
          (is (eq :no-op (getf plan :status)))
          (is (eq :classic
                  (appearance-profile-selected-theme (getf plan :profile))))
          (setf (clawmacs::appearance-package-transition-token-state token)
                :committed)
          (let ((clawmacs::*package-appearance-catalog* candidate))
            (clawmacs::publish-admitted-package-appearance-frame-state
             frame candidate (getf plan :profile) bundle))
          (is (eq candidate
                  (clawmacs::chat-frame-appearance-catalog frame)))
          (is (eq bundle
                  (clawmacs::chat-frame-appearance-active-bundle frame)))
          (is (eq :classic
                  (appearance-profile-selected-theme
                   (clawmacs::chat-frame-appearance-profile frame))))
          (is (= (resolved-appearance-bundle-profile-revision bundle)
                 (clawmacs::chat-frame-appearance-revision frame))))))
    (let* ((red-default
             (make-appearance-role-style
              :foreground-ink
              (make-appearance-ink-spec :foreground '(:rgb 0.8 0.1 0.1))))
           (old-catalog
             (package-theme-catalog (list (cons :default-text red-default))))
           (frame (make-frame-for-catalog old-catalog))
           (candidate (make-classic-appearance-catalog)))
      (with-mcclim-test-function-override
          (clawmacs::chat-frame-appearance-live-port (requested-frame)
            (declare (ignore requested-frame))
            :package-test-port)
        (let ((old-profile (clawmacs::chat-frame-appearance-profile frame))
              (old-bundle
                (clawmacs::chat-frame-appearance-active-bundle frame))
              (plan
                (clawmacs::package-appearance-frame-transition-plan
                 frame candidate)))
          (is (eq :failed (getf plan :status)))
          (is (eq old-profile
                  (clawmacs::chat-frame-appearance-profile frame)))
          (is (eq old-bundle
                  (clawmacs::chat-frame-appearance-active-bundle frame))))))
    (let* ((old-catalog (package-theme-catalog nil))
           (frame (make-frame-for-catalog old-catalog))
           (active-classic (make-appearance-profile :selected-theme :classic))
           (staged
             (make-appearance-candidate
              (make-appearance-profile
               :selected-theme '(:package "org.example.theme" "active"))))
           (candidate (make-classic-appearance-catalog)))
      (setf (clawmacs::chat-frame-appearance-profile frame) active-classic
            (slot-value frame 'clawmacs::appearance-staged-candidate) staged
            (slot-value frame 'clawmacs::appearance-active-bundle)
            (resolve-appearance-profile-bundle
             old-catalog active-classic
             :profile-revision 0
             :font-inventory-generation 0
             :port-identity :package-test-port))
      (with-mcclim-test-function-override
          (clawmacs::chat-frame-appearance-live-port (requested-frame)
            (declare (ignore requested-frame))
            :package-test-port)
        (let ((plan
                (clawmacs::package-appearance-frame-transition-plan
                 frame candidate)))
          (is (eq :failed (getf plan :status)))
          (is (eq staged
                  (clawmacs::chat-frame-appearance-staged-candidate frame))))))))

(test appearance-editor-command-surface-is-static-and-discoverable
  "The menu and M-x registry expose the primary commands without legacy drift."
  (let* ((frame-table
           (clim:find-command-table 'clawmacs::clawmacs-chat-frame))
         (appearance-table
           (clim:command-menu-item-value
            (clim:find-menu-item "Appearance" frame-table :errorp t))))
    (is (equal '("Customize Appearance..."
                 "Switch Appearance Theme..."
                 "Describe Current Appearance"
                 "Apply Staged Appearance"
                 "Save Appearance"
                 "Revert Staged Appearance"
                 "Reload Appearance File"
                 "Refresh Font Inventory")
               (let ((labels nil))
                 (clim:map-over-command-table-menu-items
                  (lambda (name keystroke item)
                    (declare (ignore keystroke item))
                    (push name labels))
                  appearance-table :inherited nil)
                 (nreverse labels))))
    (dolist (command
             '(clawmacs::customize-appearance-command
               clawmacs::customize-face-command
               clawmacs::switch-appearance-theme-command
               clawmacs::describe-current-appearance-command
               clawmacs::apply-staged-appearance-command
               clawmacs::save-appearance-command
               clawmacs::revert-staged-appearance-command
               clawmacs::reload-appearance-file-command
               clawmacs::refresh-font-inventory-command))
      (is (clawmacs::find-command-metadata command)))
    (is-false (fboundp 'clawmacs::customize-appearance))
    (is-false (fboundp 'clawmacs::customize-drawing-style-command))))

(test customize-face-compatibility-diagnostic-is-bounded
  "The deprecated forwarder reports once and only on explicit invocation."
  (let* ((buffer (make-buffer "appearance-editor-compat"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame :buffer buffer))
         (clim:*application-frame* frame)
         (clawmacs::*buffer-ring* (list buffer))
         (clawmacs::*customize-face-deprecation-reported-p* nil)
         (events 0))
    (with-mcclim-test-function-override
        (clawmacs::file-debug-event (event &rest fields)
          (declare (ignore fields))
          (when (string= event "deprecated-command")
            (incf events)))
      (is (= 0 events))
      (clawmacs::customize-face-command buffer)
      (clawmacs::customize-face-command buffer)
      (is (= 1 events)))))

(test appearance-editor-supports-tagged-package-identifiers
  "Theme and role completion use stable owner/local strings for tagged IDs."
  (let* ((buffer (make-buffer "appearance-editor-package-ids"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame :buffer buffer))
         (catalog (clawmacs::chat-frame-appearance-catalog frame))
         (role-id '(:package "demo" "accent"))
         (theme-id '(:package "demo" "night"))
         (extended
           (make-appearance-catalog
            :generation (appearance-catalog-generation catalog)
            :role-definitions
            (append (appearance-catalog-role-definitions catalog)
                    (list (make-appearance-role-definition
                           :id role-id :kind :content
                           :fallback-role :default-text :owner "demo")))
            :theme-definitions
            (append (appearance-catalog-theme-definitions catalog)
                    (list (make-appearance-theme-definition
                           :id theme-id :parent-theme :classic
                           :role-overlays nil :owner "demo")))
            :built-in-overlays
            (appearance-catalog-built-in-overlays catalog))))
    (setf (slot-value frame 'clawmacs::appearance-catalog) extended)
    (is (equal role-id
               (find role-id
                     (clawmacs::appearance-editor-role-ids frame)
                     :test #'equal)))
    (is (equal theme-id
               (find theme-id
                     (clawmacs::appearance-editor-theme-ids frame)
                     :test #'equal)))
    (is (string= "demo/accent"
                 (clawmacs::appearance-editor-id-string role-id)))
    (is (string= "demo/night"
                 (clawmacs::appearance-editor-id-string theme-id)))))

(test appearance-editor-validates-selected-font-through-port-inventory
  "A named font is resolved against the exact frame inventory before staging."
  (let* ((buffer (make-buffer "appearance-editor-font-resolution"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame :buffer buffer))
         (choice (make-enumerated-font-choice
                  :family-display "Alpha" :face-display "Book" :size 12))
         (inventory
           (clawmacs::%make-appearance-font-inventory
            :port :test :generation 1 :entries nil :choices (list choice)
            :metric-medium :test-medium
            :negative-cache (make-hash-table :test #'equal)))
         (resolved 0))
    (setf (slot-value frame 'clawmacs::appearance-font-inventory) inventory
          (clawmacs::chat-frame-appearance-editor-font-family frame) "Alpha"
          (clawmacs::chat-frame-appearance-editor-font-face frame) "Book")
    (with-mcclim-test-function-override
        (clawmacs:resolve-enumerated-font-choice
            (requested-inventory requested-choice &key medium scope)
          (is (eq inventory requested-inventory))
          (is (string= "Alpha"
                       (enumerated-font-choice-family-display requested-choice)))
          (is (string= "Book"
                       (enumerated-font-choice-face-display requested-choice)))
          (is (= 12 (enumerated-font-choice-size requested-choice)))
          (is (null medium))
          (is (eq :default-text scope))
          (incf resolved)
          '(:fix :roman 12))
      (clawmacs::appearance-editor-stage-role-font frame 12))
    (is (= 1 resolved))
    (is (eq :choose-font
            (getf (clawmacs::chat-frame-appearance-editor-status frame)
                  :operation)))))

(test appearance-editor-superseded-apply-cannot-discard-newer-staging
  "Queued candidates become inert after a newer stage or explicit Revert."
  (let* ((buffer (make-buffer "appearance-editor-stale-apply"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame :buffer buffer))
         (old (make-appearance-candidate
               (make-appearance-profile :selected-theme :dark)))
         (new (make-appearance-candidate
               (make-appearance-profile :selected-theme :classic)))
         (handled 0))
    (setf (slot-value frame 'clawmacs::appearance-staged-candidate) new)
    (with-mcclim-test-function-override
        (clawmacs::handle-chat-frame-appearance-activation
            (requested-frame requested-candidate)
          (declare (ignore requested-frame requested-candidate))
          (incf handled))
      (is (eq :no-op
              (appearance-activation-result-status
               (clawmacs::handle-queued-chat-frame-appearance-activation
                frame old)))))
    (is (= 0 handled))
    (is (eq new (clawmacs::chat-frame-appearance-staged-candidate frame)))
    (clawmacs::revert-staged-appearance-command frame)
    (with-mcclim-test-function-override
        (clawmacs::handle-chat-frame-appearance-activation
            (requested-frame requested-candidate)
          (declare (ignore requested-frame requested-candidate))
          (incf handled))
      (clawmacs::handle-queued-chat-frame-appearance-activation frame new))
    (is (= 0 handled))
    (is (null (clawmacs::chat-frame-appearance-staged-candidate frame)))))

(test appearance-editor-revert-reports-no-staged-profile
  "The editor distinguishes preview fallback from actual staged state."
  (let* ((buffer (make-buffer "appearance-editor-revert-label"
                              :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame :buffer buffer))
         (clim:*application-frame* frame)
         (clawmacs::*buffer-ring* (list buffer)))
    (clawmacs::customize-appearance-command buffer)
    (clawmacs::revert-staged-appearance-command frame)
    (let ((entries
            (clawmacs::appearance-editor-display-entries
             (clawmacs::chat-frame-appearance-editor-buffer frame))))
      (is (find "Staged: none" entries :test #'string=
                :key (lambda (entry) (getf entry :text)))))))

(test appearance-editor-keys-dispatch-the-primary-frame-command
  "C-h F and C-c F enter through the canonical frame command."
  (clawmacs::init-default-keymap)
  (clawmacs::install-chat-frame-keybindings)
  (let ((table (clim:find-command-table 'clawmacs::clawmacs-chat-frame)))
    (flet ((binding (prefix)
             (let* ((prefix-event
                      (make-instance
                       'clim:key-press-event :sheet nil :x 0 :y 0
                       :key-name nil :key-character prefix
                       :modifier-state
                       (clim:make-modifier-state :control)))
                    (prefix-item
                      (clim:find-keystroke-item
                       prefix-event table :errorp nil))
                    (prefix-table
                      (clim:find-command-table
                       (clim:command-menu-item-value prefix-item)))
                    (final-event
                      (make-instance
                       'clim:key-press-event :sheet nil :x 0 :y 0
                       :key-name nil :key-character #\F
                       :modifier-state
                       (clim:make-modifier-state :shift)))
                    (final-item
                      (clim:find-keystroke-item
                       final-event prefix-table :errorp nil)))
               (clim:command-menu-item-value final-item))))
      (is (equal '(clawmacs::com-chat-customize-appearance)
                 (binding #\h)))
      (is (equal '(clawmacs::com-chat-customize-appearance)
                 (binding #\c))))))

(defun make-package-transition-test-frame ()
  "Return a headless frame with one fully resolved classic bundle."
  (let* ((catalog (make-classic-appearance-catalog))
         (profile (make-appearance-profile))
         (frame
           (clim:make-application-frame
            'clawmacs::clawmacs-chat-frame
            :buffer
            (make-buffer
             (format nil "package-transition-~D"
                     (get-internal-real-time))
             :session-persistence-mode :ephemeral)))
         (bundle
           (resolve-appearance-profile-bundle
            catalog profile :profile-revision 0
            :font-inventory-generation 0 :port-identity :test-port)))
    (setf (slot-value frame 'clawmacs::appearance-catalog) catalog
          (clawmacs::chat-frame-appearance-profile frame) profile
          (slot-value frame 'clawmacs::appearance-active-bundle) bundle)
    frame))

(defun make-package-transition-test-reservation (frame token &key origin-p)
  "Return an admitted revision-only transition for FRAME."
  (let* ((catalog (clawmacs::chat-frame-appearance-catalog frame))
         (profile (clawmacs::chat-frame-appearance-profile frame))
         (bundle
           (resolve-appearance-profile-bundle
            catalog profile
            :profile-revision
            (1+ (clawmacs::chat-frame-appearance-revision frame))
            :font-inventory-generation 0 :port-identity :test-port)))
    (list
     :frame frame :sheet :test-sheet
     :catalog catalog :profile profile :bundle bundle
     :target-revision
     (resolved-appearance-bundle-profile-revision bundle)
     :token token
     :expected-catalog catalog
     :expected-profile profile
     :expected-bundle
     (clawmacs::chat-frame-appearance-active-bundle frame)
     :expected-revision
     (clawmacs::chat-frame-appearance-revision frame)
     :expected-staged
     (clawmacs::chat-frame-appearance-staged-candidate frame)
     :expected-persisted
     (clawmacs::chat-frame-appearance-persisted-profile frame)
     :expected-font-inventory
     (clawmacs::chat-frame-appearance-font-inventory frame)
     :expected-font-generation
     (clawmacs::chat-frame-appearance-font-inventory-generation frame)
     :expected-resolved-roles
     (clawmacs::copy-package-runtime-hash-table
      (clawmacs::chat-frame-appearance-resolved-roles frame))
     :expected-role-keys
     (clawmacs::copy-package-runtime-hash-table
      (clawmacs::chat-frame-appearance-role-keys frame))
     :origin-frame-p origin-p)))

(test package-appearance-barrier-handles-origin-frame-synchronously
  "A command-origin frame never waits for an event it cannot consume."
  (let* ((frame (make-package-transition-test-frame))
         (token (clawmacs::make-appearance-package-transition-token))
         (reservation
           (make-package-transition-test-reservation
            frame token :origin-p t))
         (committed-p nil))
    (setf (clawmacs::appearance-package-transition-token-expected-count token)
          1)
    (is-true
     (clawmacs::finalize-package-appearance-frame-transition
      token (list reservation)
      (lambda () (setf committed-p t))
      (lambda () (setf committed-p nil))))
    (is-true committed-p)
    (is (eq :committed
            (clawmacs::appearance-package-transition-token-state token)))
    (is (= 1 (clawmacs::chat-frame-appearance-revision frame)))))

(test package-appearance-barrier-aborts-on-origin-cas-change
  "Every captured frame state component remains a compare-and-swap precondition."
  (let* ((frame (make-package-transition-test-frame))
         (token (clawmacs::make-appearance-package-transition-token))
         (reservation
           (make-package-transition-test-reservation
            frame token :origin-p t))
         (committed-p nil))
    (setf (clawmacs::appearance-package-transition-token-expected-count token)
          1
          (slot-value frame 'clawmacs::appearance-staged-candidate)
          (make-appearance-candidate (make-appearance-profile)))
    (signals error
      (clawmacs::finalize-package-appearance-frame-transition
       token (list reservation)
       (lambda () (setf committed-p t))
       (lambda () (setf committed-p nil))))
    (is-false committed-p)
    (is (= 0 (clawmacs::chat-frame-appearance-revision frame)))))

(test package-appearance-barrier-coordinates-two-owning-processes
  "No frame commits until every owning process has parked at the barrier."
  (let* ((left (make-package-transition-test-frame))
         (right (make-package-transition-test-frame))
         (token (clawmacs::make-appearance-package-transition-token))
         (left-reservation
           (make-package-transition-test-reservation left token))
         (right-reservation
           (make-package-transition-test-reservation right token))
         (threads
           (list
            (bt:make-thread
             (lambda ()
               (clawmacs::handle-package-appearance-frame-reservation
                left left-reservation token)))
            (bt:make-thread
             (lambda ()
               (clawmacs::handle-package-appearance-frame-reservation
                right right-reservation token))))))
    (setf (clawmacs::appearance-package-transition-token-expected-count token)
          2)
    (unwind-protect
         (is-true
          (clawmacs::finalize-package-appearance-frame-transition
           token (list left-reservation right-reservation)
           (lambda () t) (lambda () nil)))
      (dolist (thread threads)
        (bt:join-thread thread)))
    (is (= 1 (clawmacs::chat-frame-appearance-revision left)))
    (is (= 1 (clawmacs::chat-frame-appearance-revision right)))))

(test package-appearance-barrier-refuses-a-nonresponsive-frame
  "A missing frame acknowledgement cannot publish the process catalog."
  (let* ((frame (make-package-transition-test-frame))
         (token (clawmacs::make-appearance-package-transition-token))
         (reservation
           (make-package-transition-test-reservation frame token))
         (committed-p nil)
         (clawmacs::*appearance-package-transition-timeout-seconds* 0.05))
    (setf (clawmacs::appearance-package-transition-token-expected-count token)
          1)
    (signals error
      (clawmacs::finalize-package-appearance-frame-transition
       token (list reservation)
       (lambda () (setf committed-p t))
       (lambda () (setf committed-p nil))))
    (is-false committed-p)
    (is (= 0 (clawmacs::chat-frame-appearance-revision frame)))))

(test package-appearance-committed-frame-settlement-recovers-forward
  "A prepared stalled owner resumes the committed target; globals never unwind."
  (let* ((frame (make-package-transition-test-frame))
         (token (clawmacs::make-appearance-package-transition-token))
         (reservation
           (make-package-transition-test-reservation frame token))
         (resume-entered-p nil)
         (resume-frame-correct-p nil)
         (release-resume-p nil)
         (global-committed-p nil)
         (global-rolled-back-p nil)
         (clawmacs::*appearance-package-transition-timeout-seconds* 0.5)
         (resume-hook
           (lambda (requested-frame requested-reservation)
             (declare (ignore requested-reservation))
             (setf resume-frame-correct-p (eq frame requested-frame))
             (setf resume-entered-p t)
             (loop :until release-resume-p :do (sleep 0.005))))
         (thread
           (bt:make-thread
            (lambda ()
              (let
                  ((clawmacs::*appearance-package-prepared-frame-resume-hook*
                     resume-hook))
                (clawmacs::handle-package-appearance-frame-reservation
                 frame reservation token))))))
    (setf (clawmacs::appearance-package-transition-token-expected-count token)
          1)
    (unwind-protect
         (handler-bind
             ((warning
                (lambda (condition)
                  (let ((restart (find-restart 'muffle-warning condition)))
                    (when restart (invoke-restart restart))))))
           (is-true
            (clawmacs::finalize-package-appearance-frame-transition
             token (list reservation)
             (lambda () (setf global-committed-p t))
             (lambda () (setf global-rolled-back-p t))))
           (is-true resume-entered-p)
           (is-true resume-frame-correct-p)
           (is-true global-committed-p)
           (is-false global-rolled-back-p)
           (is (eq :committed
                   (clawmacs::appearance-package-transition-token-state
                    token)))
           (is (= 0 (clawmacs::chat-frame-appearance-revision frame)))
           (setf release-resume-p t)
           (bt:join-thread thread)
           (setf thread nil)
           (is (= 1 (clawmacs::chat-frame-appearance-revision frame))))
      (setf release-resume-p t)
      (when thread (bt:join-thread thread)))))

(test package-appearance-reservation-rejects-a-stale-frame-plan
  "A user or font change between planning and reservation forces re-planning."
  (let* ((frame (make-package-transition-test-frame))
         (catalog (clawmacs::chat-frame-appearance-catalog frame))
         (token (clawmacs::make-appearance-package-transition-token))
         (plan nil))
    (with-mcclim-test-function-override
        (clawmacs::chat-frame-appearance-live-port (requested-frame)
          (is (eq frame requested-frame))
          :test-port)
      (setf plan
            (clawmacs::package-appearance-frame-transition-plan
             frame catalog)))
    (setf (slot-value frame 'clawmacs::appearance-staged-candidate)
          (make-appearance-candidate
           (make-appearance-profile :selected-theme :dark)))
    (incf (slot-value frame 'clawmacs::appearance-font-inventory-generation))
    (with-mcclim-test-function-override
        (clawmacs::chat-frame-grafted-top-level-sheet (requested-frame)
          (is (eq frame requested-frame))
          :test-sheet)
      (is (null
           (clawmacs::reserve-package-appearance-frame-transition
            frame plan catalog token))))))

(test package-appearance-batch-refuses-user-edit-and-fully-restores
  "An overlapping edit returns busy; package A can then roll back exactly."
  (let* ((frame (make-package-transition-test-frame))
         (clim:*application-frame* frame)
         (batch nil))
    (clawmacs::register-package-appearance-live-chat-frame frame)
    (unwind-protect
         (progn
           (setf batch (clawmacs::begin-package-appearance-frame-batch))
           (signals error
             (clawmacs::appearance-editor-stage-profile
              frame
              (make-appearance-profile :selected-theme :dark)
              :overlapping-user-edit))
           (is (null
                (clawmacs::chat-frame-appearance-staged-candidate frame)))
           ;; Model package A's coordinated commit and checkpoint, followed by
           ;; package B's entrypoint failure and the outer batch restoration.
           (let* ((token
                    (clawmacs::make-appearance-package-transition-token))
                  (reservation
                    (make-package-transition-test-reservation
                     frame token :origin-p t)))
             (setf
              (clawmacs::appearance-package-transition-token-expected-count
               token)
              1)
             (clawmacs::finalize-package-appearance-frame-transition
              token (list reservation) (lambda () t) (lambda () nil))
             (clawmacs::checkpoint-package-appearance-frame-batch
              (list reservation)))
           (is (= 1 (clawmacs::chat-frame-appearance-revision frame)))
           (clawmacs::restore-package-appearance-frame-batch batch)
           (is (= 0 (clawmacs::chat-frame-appearance-revision frame)))
           (is (null
                (clawmacs::chat-frame-appearance-staged-candidate frame))))
      (when batch
        (clawmacs::end-package-appearance-frame-batch batch))
      (clawmacs::unregister-package-appearance-live-chat-frame frame))))

(test package-appearance-batch-refuses-font-refresh-before-mutation
  "A queued font event reports busy without changing inventory or generation."
  (let* ((frame (make-package-transition-test-frame))
         (old-inventory
           (clawmacs::chat-frame-appearance-font-inventory frame))
         (old-generation
           (clawmacs::chat-frame-appearance-font-inventory-generation frame))
         (called-p nil)
         (batch nil))
    (clawmacs::register-package-appearance-live-chat-frame frame)
    (unwind-protect
         (progn
           (setf batch (clawmacs::begin-package-appearance-frame-batch))
           (with-mcclim-test-function-override
               (clawmacs::refresh-chat-frame-font-inventory
                   (requested-frame &key invalidate-cache)
                 (declare (ignore requested-frame invalidate-cache))
                 (setf called-p t))
             (let ((result
                     (clawmacs::handle-chat-frame-font-inventory-refresh
                      frame)))
               (is (eq :failed
                       (appearance-activation-result-status result)))))
           (is-false called-p)
           (is (eq old-inventory
                   (clawmacs::chat-frame-appearance-font-inventory frame)))
           (is (= old-generation
                  (clawmacs::chat-frame-appearance-font-inventory-generation
                   frame))))
      (when batch
        (clawmacs::end-package-appearance-frame-batch batch))
      (clawmacs::unregister-package-appearance-live-chat-frame frame))))

(test package-appearance-frame-registration-adopts-one-committed-catalog
  "Registration cannot interleave between global publication and enumeration."
  (let* ((frame (make-package-transition-test-frame))
         (candidate
           (make-appearance-catalog
            :role-definitions
            (appearance-catalog-role-definitions
             (make-classic-appearance-catalog))
            :theme-definitions
            (appearance-catalog-theme-definitions
             (make-classic-appearance-catalog))
            :built-in-overlays
            (appearance-catalog-built-in-overlays
             (make-classic-appearance-catalog))
            :generation 99))
         (old-catalog clawmacs::*package-appearance-catalog*)
         (finished-p nil)
         (thread nil))
    (bt:acquire-lock clawmacs::*package-appearance-catalog-lock*)
    (unwind-protect
         (progn
           (setf thread
                 (bt:make-thread
                  (lambda ()
                    (clawmacs::register-package-appearance-live-chat-frame
                     frame)
                    (setf finished-p t))))
           (sleep 0.02)
           (is-false finished-p)
           (setf clawmacs::*package-appearance-catalog* candidate)
           (bt:release-lock clawmacs::*package-appearance-catalog-lock*)
           (bt:join-thread thread)
           (setf thread nil)
           (is-true finished-p)
           (is (eq candidate
                   (clawmacs::chat-frame-appearance-catalog frame))))
      (when thread
        (ignore-errors
          (bt:release-lock clawmacs::*package-appearance-catalog-lock*))
        (bt:join-thread thread))
      (clawmacs::unregister-package-appearance-live-chat-frame frame)
      (bt:with-lock-held (clawmacs::*package-appearance-catalog-lock*)
        (setf clawmacs::*package-appearance-catalog* old-catalog)))))

(test package-appearance-planning-muffles-warnings-but-refuses-strict-errors
  "Warning-only contrast remains admissible; strict contrast remains fatal."
  (let* ((frame (make-package-transition-test-frame))
         (catalog (clawmacs::chat-frame-appearance-catalog frame)))
    (with-mcclim-test-function-override
        (clawmacs::chat-frame-appearance-live-port (requested-frame)
          (declare (ignore requested-frame))
          :test-port)
      (with-mcclim-test-function-override
          (clawmacs::validate-appearance-profile-contrast
              (requested-catalog requested-profile &key role-stacks)
            (declare
             (ignore requested-catalog requested-profile role-stacks))
            (clawmacs::warn-appearance-condition
             'appearance-contrast-warning
             :origin :test :axis :contrast :value :warning))
        (handler-bind
            ((warning
               (lambda (condition)
                 (error "Unmuffled planning warning: ~A" condition))))
          (is (member
               (getf
                (clawmacs::package-appearance-frame-transition-plan
                 frame catalog)
                :status)
               '(:ready :no-op)))))
      (with-mcclim-test-function-override
          (clawmacs::validate-appearance-profile-contrast
              (requested-catalog requested-profile &key role-stacks)
            (declare
             (ignore requested-catalog requested-profile role-stacks))
            (clawmacs::error-appearance-condition
             'appearance-contrast-warning
             :origin :test :axis :contrast :value :strict))
        (is (eq :failed
                (getf
                 (clawmacs::package-appearance-frame-transition-plan
                  frame catalog)
                 :status)))))))
