;;;; Real CLX frame-lifecycle proof.  The shell wrapper supplies a fresh Xvfb.

(ql:quickload :clawmacs)

(defvar *probe-clx-port*
  (or (clim:find-port)
      (error "The private Xvfb did not produce a CLX port.")))
(defvar *probe-frame-manager*
  (or (clim:find-frame-manager :port *probe-clx-port*)
      (error "The private CLX port did not produce a frame manager.")))

(defvar *probe-frame-thread-errors* nil)
(defvar *probe-frame-thread-error-lock*
  (bt:make-lock "appearance live probe frame errors"))

(defstruct (probe-frame-call
             (:constructor make-probe-frame-call (label expected-thread function)))
  label
  expected-thread
  function
  (lock (bt:make-lock "appearance live probe frame call"))
  done-p
  values
  condition
  actual-thread)

(defclass probe-frame-call-event (clim:window-manager-event)
  ((call :initarg :call :reader probe-frame-call-event-call)))

(defmethod clim:handle-event
    ((sheet clime:top-level-sheet-mixin) (event probe-frame-call-event))
  "Run one probe assertion on the real frame process that owns SHEET."
  (let* ((call (probe-frame-call-event-call event))
         (frame (ignore-errors (clim:pane-frame sheet)))
         (values nil)
         (condition nil))
    (handler-case
        (setf values
              (multiple-value-list
               (funcall (probe-frame-call-function call) frame)))
      (error (caught)
        (setf condition caught)))
    (bt:with-lock-held ((probe-frame-call-lock call))
      (setf (probe-frame-call-actual-thread call) (bt:current-thread)
            (probe-frame-call-values call) values
            (probe-frame-call-condition call) condition
            (probe-frame-call-done-p call) t))))

(defun probe-record-frame-thread-error (name condition)
  (bt:with-lock-held (*probe-frame-thread-error-lock*)
    (push (list name condition) *probe-frame-thread-errors*)))

(defun probe-check-frame-thread-errors ()
  (let ((errors
          (bt:with-lock-held (*probe-frame-thread-error-lock*)
            (copy-list *probe-frame-thread-errors*))))
    (when errors
      (error "Frame event process failed: ~{~S~^, ~}" errors))))

(defun probe-wait (predicate label &key (seconds 15))
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop do (probe-check-frame-thread-errors)
          until (funcall predicate)
          do (when (>= (get-internal-real-time) deadline)
               (error "Timed out waiting for ~A." label))
             (sleep 0.05))))

(defun probe-call-on-frame (frame expected-thread label function &key (seconds 15))
  "Call FUNCTION on FRAME's actual event process and return its values."
  (let* ((call (make-probe-frame-call label expected-thread function))
         (sheet (or (clawmacs::chat-frame-grafted-top-level-sheet frame)
                    (error "Cannot queue ~A before frame adoption." label))))
    (clim:queue-event
     sheet
     (make-instance 'probe-frame-call-event :sheet sheet :call call))
    (probe-wait
     (lambda ()
       (bt:with-lock-held ((probe-frame-call-lock call))
         (probe-frame-call-done-p call)))
     label :seconds seconds)
    (bt:with-lock-held ((probe-frame-call-lock call))
      (unless (eq expected-thread (probe-frame-call-actual-thread call))
        (error "~A ran on ~S instead of owning frame process ~S."
               label
               (probe-frame-call-actual-thread call)
               expected-thread))
      (when (probe-frame-call-condition call)
        (error "~A failed on its owning frame process: ~A"
               label (probe-frame-call-condition call)))
      (values-list (probe-frame-call-values call)))))

(defun probe-profile-theme (frame)
  (clawmacs:appearance-profile-selected-theme
   (clawmacs::chat-frame-appearance-profile frame)))

(defun probe-start-frame (name profile)
  (let* ((buffer (clawmacs:make-buffer name :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'clawmacs::clawmacs-chat-frame
                 :buffer buffer :appearance-profile profile :pretty-name name))
         (thread (bt:make-thread
                  (lambda ()
                    (handler-case
                        ;; Port selection belongs to the owning event process.
                        ;; Passing :FRAME-MANAGER during construction makes
                        ;; pinned McCLIM synchronously adopt the frame on this
                        ;; probe's coordinator thread before ownership starts.
                        (clim:run-frame-top-level frame
                                                  :port *probe-clx-port*)
                      (error (condition)
                        (probe-record-frame-thread-error name condition))))
                  :name name)))
    (values frame thread)))

(defun probe-package-definition (owner)
  (clawmacs:make-package-definition
   :name owner :description "appearance live probe"
   :root #P"/tmp/" :entrypoint #P"/tmp/appearance-live-probe.lisp"))

(defun probe-publish-package-theme (first second)
  "Publish one package theme through both real frame event processes."
  (let* ((owner "org.clawmacs.appearance-live-probe")
         (theme-id (list :package owner "theme"))
         (definition (probe-package-definition owner))
         (before-generation
           (clawmacs:appearance-catalog-generation
            (clawmacs:current-package-appearance-catalog)))
         (clawmacs::*current-clawmacs-package* owner)
         (clawmacs::*current-package-resource-types* '(:appearance))
         (clawmacs::*package-appearance-entrypoint-staging*
           (clawmacs::begin-package-appearance-entrypoint-staging definition)))
    (clawmacs:register-package-appearance-theme
     (clawmacs:make-appearance-theme-definition
      :id theme-id :parent-theme :classic :role-overlays nil))
    (clawmacs::commit-package-appearance-entrypoint-staging
     clawmacs::*package-appearance-entrypoint-staging*)
    (let ((catalog (clawmacs:current-package-appearance-catalog)))
      (unless (and (= (1+ before-generation)
                      (clawmacs:appearance-catalog-generation catalog))
                   (eq catalog
                       (clawmacs::chat-frame-appearance-catalog first))
                   (eq catalog
                       (clawmacs::chat-frame-appearance-catalog second))
                   (gethash owner clawmacs::*package-appearance-declarations*))
        (error "Package theme publication did not settle on both frames."))
      (format t
              "APPEARANCE_LIVE_PACKAGE_PUBLISH_OK frames=2 generation=~D~%"
              (clawmacs:appearance-catalog-generation catalog))
      (values definition owner theme-id catalog))))

(defun probe-package-removal-rollback
    (first second first-thread definition owner theme-id published-catalog)
  "Prove a staged package theme makes removal refuse without partial mutation."
  (probe-call-on-frame
   first first-thread "stage package theme"
   (lambda (frame)
     (clawmacs::appearance-editor-stage-profile
      frame
      (clawmacs:make-appearance-profile :selected-theme theme-id)
      :probe-package-theme)))
  (let ((first-profile (clawmacs::chat-frame-appearance-profile first))
        (second-profile (clawmacs::chat-frame-appearance-profile second))
        (first-bundle (clawmacs::chat-frame-appearance-active-bundle first))
        (second-bundle (clawmacs::chat-frame-appearance-active-bundle second))
        (first-revision (clawmacs::chat-frame-appearance-revision first))
        (second-revision (clawmacs::chat-frame-appearance-revision second))
        (declarations
          (gethash owner clawmacs::*package-appearance-declarations*))
        (refused-p nil))
    (handler-case
        (clawmacs::prepare-package-appearance-removal definition)
      (clawmacs:appearance-error ()
        (setf refused-p t)))
    (unless (and refused-p
                 (eq published-catalog
                     (clawmacs:current-package-appearance-catalog))
                 (eq published-catalog
                     (clawmacs::chat-frame-appearance-catalog first))
                 (eq published-catalog
                     (clawmacs::chat-frame-appearance-catalog second))
                 (eq declarations
                     (gethash owner
                              clawmacs::*package-appearance-declarations*))
                 (eq first-profile
                     (clawmacs::chat-frame-appearance-profile first))
                 (eq second-profile
                     (clawmacs::chat-frame-appearance-profile second))
                 (eq first-bundle
                     (clawmacs::chat-frame-appearance-active-bundle first))
                 (eq second-bundle
                     (clawmacs::chat-frame-appearance-active-bundle second))
                 (= first-revision
                    (clawmacs::chat-frame-appearance-revision first))
                 (= second-revision
                    (clawmacs::chat-frame-appearance-revision second)))
      (error "Refused package-theme removal changed committed state."))
    (format t
            "APPEARANCE_LIVE_PACKAGE_REMOVAL_ROLLBACK_OK frames=2 catalog-unchanged=true~%")))

(let ((first nil) (second nil) (first-thread nil) (second-thread nil))
  (unwind-protect
       (progn
         ;; These are actual CLIM application frames, adopted onto the same
         ;; public CLX port and run on independent normal event processes.
         (multiple-value-setq (first first-thread)
           (probe-start-frame "appearance-live-classic"
                              (clawmacs:make-appearance-profile
                               :selected-theme :classic)))
         ;; McCLIM lazily establishes the default frame manager/CLX port.
         ;; Let the first frame complete that ordinary adoption before asking
         ;; a second owning event process to share the same port.
         (probe-wait
          (lambda ()
            (and (clawmacs::chat-frame-grafted-top-level-sheet first)
                 (clawmacs::chat-frame-appearance-active-bundle first)))
          "first adopted CLX frame" :seconds 30)
         (multiple-value-setq (second second-thread)
           (probe-start-frame "appearance-live-dark"
                              (clawmacs:make-appearance-profile
                               :selected-theme :dark)))
         (probe-wait (lambda ()
                       (and (clawmacs::chat-frame-grafted-top-level-sheet first)
                            (clawmacs::chat-frame-grafted-top-level-sheet second)
                            (clawmacs::chat-frame-appearance-active-bundle first)
                            (clawmacs::chat-frame-appearance-active-bundle second)))
                     "two adopted CLX frames" :seconds 30)
         (unless (and (eq :classic (probe-profile-theme first))
                      (eq :dark (probe-profile-theme second))
                      (not (eq (clawmacs::chat-frame-appearance-active-bundle first)
                               (clawmacs::chat-frame-appearance-active-bundle second)))
                      (not (eq (clawmacs::chat-frame-appearance-resolved-roles first)
                               (clawmacs::chat-frame-appearance-resolved-roles second))))
           (error "Frame-local appearance profile or cache isolation failed."))
         (format t "APPEARANCE_LIVE_TWO_FRAME_OK first=classic second=dark distinct-bundles=true distinct-caches=true~%")
         (probe-call-on-frame
          first first-thread "stage first frame profile"
          (lambda (frame)
            (clawmacs::appearance-editor-stage-profile
             frame
             (clawmacs:make-appearance-profile :selected-theme :dark)
             :probe)))
         (probe-call-on-frame
          second second-thread "stage second frame profile"
          (lambda (frame)
            (clawmacs::appearance-editor-stage-profile
             frame
             (clawmacs:make-appearance-profile :selected-theme :classic)
             :probe)))
         (unless (and (eq :dark (clawmacs:appearance-profile-selected-theme
                                 (clawmacs::appearance-editor-staged-profile first)))
                      (eq :classic (clawmacs:appearance-profile-selected-theme
                                    (clawmacs::appearance-editor-staged-profile second))))
           (error "Frame-local staged profiles leaked."))
         (format t "APPEARANCE_LIVE_STAGED_PROFILES_OK first=dark second=classic~%")
         ;; The public commands only queue work; the refresh handlers themselves
         ;; run on the two independent owning frame processes.
         (clawmacs::refresh-font-inventory-command first)
         (probe-wait
          (lambda ()
            (plusp
             (clawmacs::chat-frame-appearance-font-inventory-generation first)))
          "first frame-local public font refresh")
         (probe-call-on-frame first first-thread "first font refresh owner barrier"
                              (lambda (frame) (declare (ignore frame)) t))
         ;; Package appearance transactions deliberately refuse overlapping
         ;; user edits.  Complete the first public refresh before queueing the
         ;; second; this proves both owners without manufacturing contention.
         (clawmacs::refresh-font-inventory-command second)
         (probe-wait
          (lambda ()
            (plusp
             (clawmacs::chat-frame-appearance-font-inventory-generation second)))
          "second frame-local public font refresh")
         (unless (and (not (eq (clawmacs::chat-frame-appearance-font-inventory first)
                              (clawmacs::chat-frame-appearance-font-inventory second)))
                      (= 1 (clawmacs::chat-frame-appearance-font-inventory-generation first))
                      (= 1 (clawmacs::chat-frame-appearance-font-inventory-generation second)))
           (error "Frame-local font inventory leaked or generation was not committed."))
         (probe-call-on-frame second second-thread "second font refresh owner barrier"
                              (lambda (frame) (declare (ignore frame)) t))
         (format t "APPEARANCE_LIVE_FONT_INVENTORIES_OK first-generation=1 second-generation=1 distinct=true owner-processes=true~%")
         (let* ((safe-profile
                  (clawmacs:make-appearance-profile
                   :selected-theme :classic
                   :role-overrides
                   (list (cons :transcript-user
                                (clawmacs:make-appearance-role-style
                                 :foreground-ink
                                 (clawmacs:make-appearance-ink-spec :foreground :red)))))))
           (probe-call-on-frame
            first first-thread "stage live-safe profile"
            (lambda (frame)
              (clawmacs::appearance-editor-stage-profile
               frame safe-profile :probe-live)))
           (let ((before-result
                   (clawmacs::chat-frame-appearance-last-activation-result first)))
             (clawmacs::request-chat-frame-appearance-activation
              first (clawmacs::chat-frame-appearance-staged-candidate first))
             (probe-wait
              (lambda ()
                (let ((result
                        (clawmacs::chat-frame-appearance-last-activation-result
                         first)))
                  (and (not (eq before-result result))
                       (eq :ready
                           (clawmacs:appearance-activation-result-status
                            result)))))
              "live-safe activation"))
           (unless
               (equal
                (clawmacs::appearance-profile-structural-key safe-profile)
                (clawmacs::appearance-profile-structural-key
                 (clawmacs::chat-frame-appearance-profile first)))
             (error "Live-safe activation did not publish the staged profile."))
           (probe-call-on-frame first first-thread "live activation owner barrier"
                                (lambda (frame) (declare (ignore frame)) t))
           (format t "APPEARANCE_LIVE_SAFE_ACTIVATION_OK status=ready owner-process=true~%")
           (let ((before-profile (clawmacs::chat-frame-appearance-profile first))
                 (before-bundle (clawmacs::chat-frame-appearance-active-bundle first))
                 (bad (clawmacs:make-appearance-profile :selected-theme :missing)))
             (probe-call-on-frame
              first first-thread "stage invalid rollback profile"
              (lambda (frame)
                (clawmacs::appearance-editor-stage-profile
                 frame bad :probe-failure)))
             (let ((before-result
                     (clawmacs::chat-frame-appearance-last-activation-result first)))
               (clawmacs::request-chat-frame-appearance-activation
                first (clawmacs::chat-frame-appearance-staged-candidate first))
               (probe-wait
                (lambda ()
                  (let ((result
                          (clawmacs::chat-frame-appearance-last-activation-result
                           first)))
                    (and (not (eq before-result result))
                         (eq :failed
                             (clawmacs:appearance-activation-result-status
                              result)))))
                "failed activation rollback"))
             (unless (and (eq before-profile (clawmacs::chat-frame-appearance-profile first))
                          (eq before-bundle (clawmacs::chat-frame-appearance-active-bundle first)))
               (error "Failed activation changed committed frame state."))
             (probe-call-on-frame first first-thread "failed activation owner barrier"
                                  (lambda (frame) (declare (ignore frame)) t))
             (format t "APPEARANCE_LIVE_ACTIVATION_ROLLBACK_OK status=failed owner-process=true~%")
             ;; Replace the deliberately invalid staged candidate before the
             ;; package catalog transaction validates all staged profiles.
             (probe-call-on-frame
              first first-thread "reset failed activation staging"
              (lambda (frame)
                (clawmacs::appearance-editor-stage-profile
                 frame before-profile :probe-reset-after-failure)))))
         (multiple-value-bind (definition owner theme-id catalog)
             (probe-publish-package-theme first second)
           (probe-package-removal-rollback
            first second first-thread definition owner theme-id catalog)))
    (dolist (frame (list first second))
      (when frame
        (let ((sheet
                (clawmacs::chat-frame-grafted-top-level-sheet frame)))
          (when sheet
            (ignore-errors
              (clim:queue-event
               sheet
               (make-instance 'clim:window-manager-delete-event
                              :sheet sheet)))))))
    (dolist (thread (list first-thread second-thread))
      (when thread
        (when (eq :probe-timeout
                  (sb-thread:join-thread
                   thread :timeout 10 :default :probe-timeout))
          (error "Timed out joining a live CLX frame thread."))))
    (when (or (and first-thread (bt:thread-alive-p first-thread))
              (and second-thread (bt:thread-alive-p second-thread)))
      (error "A live CLX frame thread survived probe teardown."))
    (probe-check-frame-thread-errors)
    (format t "APPEARANCE_LIVE_TWO_FRAME_TEARDOWN_OK threads=0~%")))
