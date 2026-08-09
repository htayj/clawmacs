(in-package :rplaca/tests)

(in-suite listener-parity-suite)

(defparameter *listener-parity-command-inventory*
  '((rplaca::com-model . "Model")
    (rplaca::com-think-level . "Think-Level")
    (rplaca::com-buffer . "Buffer")
    (rplaca::com-skill . "Skill")
    (rplaca::com-package . "Package")
    (rplaca::com-project . "Project")
    (rplaca::com-file . "File")
    (rplaca::com-safe-reload . "Safe Reload")
    (rplaca::com-help . "Help")
    (rplaca::com-manual . "Manual")
    (rplaca::com-info . "Info")
    (rplaca::com-reload-appearance . "Reload Appearance")))

(defmacro with-listener-parity-function-override
    ((name lambda-list &body implementation) &body body)
  (let ((original (gensym "ORIGINAL")))
    `(let ((,original (and (fboundp ',name) (symbol-function ',name))))
       (unwind-protect
            (progn
              (setf (symbol-function ',name)
                    (lambda ,lambda-list ,@implementation))
              ,@body)
         (if ,original
             (setf (symbol-function ',name) ,original)
             (fmakunbound ',name))))))

(defun make-listener-parity-buffer (name)
  (rplaca::make-buffer name
                       :agent-name "tester"
                       :kind :chat
                       :session-persistence-mode :ephemeral))

(defun make-listener-parity-frame (buffer)
  (clim:make-application-frame 'rplaca::rplaca-listener
                               :conversation-buffer buffer
                               :listener-context
                               (rplaca::make-listener-context)))

(test listener-parity-command-names-are-present-and-completable
  (let ((table (clim:find-command-table 'rplaca::rplaca-listener)))
    (dolist (entry *listener-parity-command-inventory*)
      (is-true (clim:command-present-in-command-table-p (car entry) table))
      (is (eq (car entry)
              (clim:find-command-from-command-line-name
               (cdr entry) table :errorp nil))))))

(test listener-selector-presentation-types-exist
  (dolist (type '(rplaca::listener-model-choice
                  rplaca::listener-think-level-choice
                  rplaca::listener-buffer-choice
                  rplaca::listener-session-choice
                  rplaca::listener-skill-choice
                  rplaca::rplaca-package
                  rplaca::listener-project-choice
                  rplaca::listener-file-choice))
    (is-true (clim:presentation-type-specifier-p type))))

(test listener-selector-entry-builders-use-existing-helpers
  (let ((buffer (make-listener-parity-buffer "selector-entries"))
        (calls nil))
    (with-listener-parity-function-override
        (rplaca::available-models-for-selector (actual)
          (push (list :models actual) calls)
          (list (list :provider :stub :model "model")))
      (with-listener-parity-function-override
          (rplaca::available-think-levels-for-selector (actual)
            (push (list :think actual) calls)
            (list (list :level "high" :display "high")))
        (with-listener-parity-function-override
            (rplaca::skill-selector-items (&key include-disabled include-enabled-marker)
              (push (list :skills include-disabled include-enabled-marker) calls)
              (list (list :skill :stub :display "stub")))
          (with-listener-parity-function-override
              (rplaca::project-selector-items (&optional active)
                (push (list :projects active) calls)
                (list (list :project :stub :display "stub")))
            (is (= 1 (length (rplaca::listener-model-choice-entries buffer))))
            (is (= 1 (length (rplaca::listener-think-level-choice-entries buffer))))
            (is (= 1 (length (rplaca::listener-skill-choice-entries buffer))))
            (is (= 1 (length (rplaca::listener-project-choice-entries buffer))))))))
    (is (member (list :models buffer) calls :test #'equal))
    (is (member (list :think buffer) calls :test #'equal))
    (is (find :skills calls :key #'car))
    (is (find :projects calls :key #'car))))

(test listener-selector-commands-apply-existing-helpers
  (let* ((buffer (make-listener-parity-buffer "selector-apply"))
         (other (make-listener-parity-buffer "selector-other"))
         (frame (make-listener-parity-frame buffer))
         (calls nil)
         (clim:*application-frame* frame))
    (with-listener-parity-function-override
        (rplaca::apply-buffer-model-selection (actual provider model)
          (push (list :model actual provider model) calls)
          (values :kept nil))
      (with-listener-parity-function-override
          (rplaca::apply-buffer-think-level-selection (actual entry)
            (push (list :think actual entry) calls))
        (with-listener-parity-function-override
            (rplaca::listener-activate-session-buffer (actual-frame actual-buffer)
              (push (list :buffer actual-frame actual-buffer) calls))
          (rplaca::com-model (list :provider :stub :model "m" :display "stub/m"))
          (rplaca::com-think-level (list :level "high" :display "high"))
          (rplaca::com-buffer other))))
    (is (find :model calls :key #'car))
    (is (find :think calls :key #'car))
    (is (member (list :buffer frame other) calls :test #'equal))))

(test listener-skill-package-project-and-file-commands-apply-domain-helpers
  (let* ((buffer (make-listener-parity-buffer "selector-domain"))
         (frame (make-listener-parity-frame buffer))
         (skill (rplaca::make-skill :name "stub" :path #P"/tmp/stub/SKILL.md"))
         (project (rplaca::make-project :name "project"
                                       :root #P"/tmp/project/"))
         (calls nil)
         (clim:*application-frame* frame))
    (with-listener-parity-function-override
        (rplaca::skill-enabled-p (actual)
          (declare (ignore actual)) nil)
      (with-listener-parity-function-override
          (rplaca::set-skill-enabled (actual enabled-p)
            (push (list :skill actual enabled-p) calls))
        (with-listener-parity-function-override
            (rplaca::project-open-file (actual-project path)
              (push (list :file actual-project path) calls))
          (rplaca::com-skill (list :skill skill))
          (rplaca::com-package (find-package :keyword))
          (rplaca::com-project (list :project project))
          (rplaca::com-file (list :project project :path "src/main.lisp")))))
    (is (member (list :skill skill t) calls :test #'equal))
    (is (member (list :file project "src/main.lisp") calls :test #'equal))
    (is (string= "KEYWORD"
                 (rplaca::listener-context-package-name
                  (rplaca::rplaca-listener-context frame))))
    (is (string= "project" (rplaca::buffer-project-name buffer)))
    (is (equal #P"/tmp/project/" (rplaca::buffer-working-directory buffer)))))

(test listener-safe-reload-command-targets-active-listener
  (let* ((buffer (make-listener-parity-buffer "reload-listener"))
         (frame (make-listener-parity-frame buffer))
         (captured nil)
         (clim:*application-frame* frame))
    (with-listener-parity-function-override
        (rplaca::start-interactive-safe-reload (actual-buffer &optional actual-frame)
          (setf captured (list actual-buffer actual-frame))
          :started)
      (is (eq :started (rplaca::com-safe-reload))))
    (is (equal (list buffer frame) captured))))

(test listener-safe-reload-completion-queues-to-listener-and-refreshes-it
  (let* ((buffer (make-listener-parity-buffer "reload-listener-event"))
         (frame (make-listener-parity-frame buffer))
         (sheet (cons :listener-sheet nil))
         (request (rplaca::make-safe-reload-request
                   :mode :interactive :buffer buffer :frame frame))
         (queued nil)
         (refreshed nil))
    (with-listener-parity-function-override
        (clim:frame-top-level-sheet (actual-frame)
          (is (eq frame actual-frame)) sheet)
      (with-listener-parity-function-override
          (clim:sheet-grafted-p (actual-sheet)
            (is (eq sheet actual-sheet)) t)
        (with-listener-parity-function-override
            (clim:queue-event (actual-sheet event)
              (setf queued (list actual-sheet event)))
          (is-true (rplaca::safe-reload-queue-completion-event request)))))
    (is (eq sheet (first queued)))
    (is (typep (second queued) 'rplaca::rplaca-safe-reload-completion-event))
    (is (eq request
            (rplaca::safe-reload-completion-event-request (second queued))))
    (with-listener-parity-function-override
        (rplaca::handle-listener-safe-reload-redisplay (actual-frame)
          (setf refreshed actual-frame))
      (rplaca::redisplay-safe-reload-frame-now frame))
    (is (eq frame refreshed))))

(test listener-help-populates-details-and-close-restores-listener-layout
  (let* ((buffer (make-listener-parity-buffer "listener-help"))
         (frame (make-listener-parity-frame buffer))
         (layout-calls nil)
         (clim:*application-frame* frame))
    (with-listener-parity-function-override
        (rplaca::listener-set-details-layout (actual)
          (push (list :details actual) layout-calls))
      (with-listener-parity-function-override
          (rplaca::listener-set-listener-layout (actual)
            (push (list :listener actual) layout-calls))
        (rplaca::com-help)
        (is (stringp (car (rplaca::rplaca-listener-selected-detail frame))))
        (is (eq :text (cdr (rplaca::rplaca-listener-selected-detail frame))))
        (rplaca::com-close-details)))
    (is (null (rplaca::rplaca-listener-selected-detail frame)))
    (is (equal (list :listener frame) (first layout-calls)))
    (is (equal (list :details frame) (second layout-calls)))))

(test listener-appearance-installs-all-nine-concrete-seams
  (dolist (seam '(rplaca::*appearance-package-live-frame-provider*
                  rplaca::*appearance-package-frame-transition-planner*
                  rplaca::*appearance-package-frame-transition-reserver*
                  rplaca::*appearance-package-frame-transition-publisher*
                  rplaca::*appearance-package-frame-transition-finalizer*
                  rplaca::*appearance-package-batch-checkpoint-function*
                  rplaca::*package-appearance-batch-begin-function*
                  rplaca::*package-appearance-batch-restore-function*
                  rplaca::*package-appearance-batch-end-function*))
    (is-true (functionp (symbol-value seam)))
    (is-false (member (symbol-value seam)
                      (rplaca::listener-appearance-default-seam-functions)
                      :test #'eq))))

(test listener-appearance-registers-and-unregisters-live-frames
  (let* ((buffer (make-listener-parity-buffer "appearance-live"))
         (frame (make-listener-parity-frame buffer)))
    (unwind-protect
         (progn
           (rplaca::register-package-appearance-live-listener-frame frame)
           (is (member frame (rplaca::package-appearance-live-listener-frames)
                       :test #'eq)))
      (rplaca::unregister-package-appearance-live-listener-frame frame))
    (is-false (member frame (rplaca::package-appearance-live-listener-frames)
                      :test #'eq))))

(test listener-appearance-plan-refuses-stale-frame-and-applies-current-plan
  (let* ((buffer (make-listener-parity-buffer "appearance-plan"))
         (frame (make-listener-parity-frame buffer))
         (catalog (rplaca::make-classic-appearance-catalog))
         (plan (rplaca::listener-package-appearance-frame-transition-plan
                frame catalog)))
    (is (member (getf plan :status) '(:ready :no-op)))
    (let ((reservation
            (rplaca::reserve-listener-package-appearance-frame-transition
             frame plan catalog
             (rplaca::make-appearance-package-transition-token))))
      (is-true reservation)
      (incf (rplaca::rplaca-listener-appearance-revision frame))
      (is-false
       (rplaca::listener-package-appearance-reservation-current-p reservation)))))

(defun listener-parity-next-catalog (catalog)
  (rplaca::make-appearance-catalog
   :role-definitions
   (rplaca::appearance-catalog-role-definitions catalog)
   :theme-definitions
   (rplaca::appearance-catalog-theme-definitions catalog)
   :built-in-overlays
   (rplaca::appearance-catalog-built-in-overlays catalog)
   :generation (1+ (rplaca::appearance-catalog-generation catalog))))

(test listener-appearance-publishes-to-two-live-listener-frames
  (let* ((old-catalog (rplaca::current-package-appearance-catalog))
         (catalog (listener-parity-next-catalog old-catalog))
         (one (make-listener-parity-frame
               (make-listener-parity-buffer "appearance-one")))
         (two (make-listener-parity-frame
               (make-listener-parity-buffer "appearance-two")))
         (frames (list one two)))
    (let ((rplaca::*package-appearance-catalog* old-catalog)
          (rplaca::*package-appearance-live-listener-frames* frames))
      (let ((plans (rplaca::package-appearance-frame-plan frames catalog)))
        (rplaca::publish-package-appearance-catalog
         catalog plans "listener-parity" nil)))
    (is (eq catalog (rplaca::rplaca-listener-appearance-catalog one)))
    (is (eq catalog (rplaca::rplaca-listener-appearance-catalog two)))
    (is (= 1 (rplaca::rplaca-listener-appearance-revision one)))
    (is (= 1 (rplaca::rplaca-listener-appearance-revision two)))))

(test listener-appearance-refuses-invalid-profile-transition
  (let* ((frame (make-listener-parity-frame
                 (make-listener-parity-buffer "appearance-refused")))
         (catalog (rplaca::make-appearance-catalog
                   :role-definitions nil
                   :theme-definitions nil
                   :built-in-overlays nil)))
    (is (eq :failed
            (getf (rplaca::listener-package-appearance-frame-transition-plan
                   frame catalog)
                  :status)))))

(test listener-appearance-batch-rolls-back-two-listener-frames
  (let* ((base (rplaca::current-package-appearance-catalog))
         (catalog (listener-parity-next-catalog base))
         (one (make-listener-parity-frame
               (make-listener-parity-buffer "appearance-rollback-one")))
         (two (make-listener-parity-frame
               (make-listener-parity-buffer "appearance-rollback-two")))
         (frames (list one two))
         (rplaca::*package-appearance-catalog* base)
         (rplaca::*package-appearance-live-listener-frames* frames)
         (snapshot (funcall rplaca::*package-appearance-batch-begin-function*)))
    (unwind-protect
         (progn
           (let ((plans (rplaca::package-appearance-frame-plan frames catalog)))
             (rplaca::publish-package-appearance-catalog
              catalog plans "listener-parity-batch" nil))
           (is (eq catalog (rplaca::rplaca-listener-appearance-catalog one)))
           (funcall rplaca::*package-appearance-batch-restore-function* snapshot)
           (is (eq base (rplaca::rplaca-listener-appearance-catalog one)))
           (is (eq base (rplaca::rplaca-listener-appearance-catalog two))))
      (funcall rplaca::*package-appearance-batch-end-function* snapshot))))
