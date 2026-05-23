(in-package :clawmacs/tests)

(in-suite clawmacs-suite)

(defclass menu-unmanaged-top-level-sheet-pane-test ()
  ())

(defun test-menu-sheet-not-grafted-condition (&optional (object (make-instance 'menu-unmanaged-top-level-sheet-pane-test)))
  (handler-case
      (error "Sheet ~s is not grafted." object)
    (simple-error (condition)
      condition)))

(defun test-tool-use-block (id name)
  `((:type . "tool_use")
    (:id . ,id)
    (:name . ,name)
    (:input . nil)))

(defun test-tool-result-block (id content)
  `((:type . "tool_result")
    (:tool--use--id . ,id)
    (:content . ,content)))

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

(test menu-sheet-not-grafted-error-classification-is-narrow
  "Only transient McCLIM menu top-level-sheet graft errors are recoverable."
  (is-true
   (clawmacs::transient-menu-sheet-not-grafted-error-p
    (test-menu-sheet-not-grafted-condition)))
  (is-false
   (clawmacs::transient-menu-sheet-not-grafted-error-p
    (test-menu-sheet-not-grafted-condition 'ordinary-pane)))
  (is-false
   (clawmacs::transient-menu-sheet-not-grafted-error-p
    (handler-case
        (error "Different menu problem: ~s"
               (make-instance 'menu-unmanaged-top-level-sheet-pane-test))
      (simple-error (condition)
        condition)))))

(test chat-top-level-recovers-from-transient-menu-graft-errors
  "The chat frame top-level can resume after McCLIM menu sheet graft races."
  (let* ((buf (make-buffer "menu-recovery" :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf))
         (attempts 0))
    (is (eq :resumed
            (clawmacs::call-chat-top-level-with-menu-error-recovery
             frame
             (lambda ()
               (incf attempts)
               (when (= attempts 1)
                 (error (test-menu-sheet-not-grafted-condition)))
               :resumed))))
    (is (= 2 attempts))))

(test chat-top-level-does-not-intercept-non-menu-errors
  "Non-menu top-level errors are left for the normal debugger/context path."
  (let* ((buf (make-buffer "menu-recovery-non-menu" :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame 'clawmacs::clawmacs-chat-frame
                                             :buffer buf))
         (condition (make-condition 'simple-error
                                    :format-control "Ordinary top-level failure"
                                    :format-arguments nil)))
    (handler-case
        (clawmacs::call-chat-top-level-with-menu-error-recovery
         frame
         (lambda ()
           (error condition)))
      (simple-error (caught)
        (is (eq condition caught))))))

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

(test mcclim-compose-routes-modal-input-to-minibuffer
  "When M-x or another selector is active, compose keys are normalized for Clawmacs."
  (let* ((event (make-instance 'clim:key-press-event
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
                                       (clim:make-modifier-state :control)))
         (clawmacs::*minibuffer-active* t)
         (clawmacs::*minibuffer-mode* :prompt)
         (clawmacs::*minibuffer-prompt* "M-x")
         (clawmacs::*minibuffer-input* "")
         (clawmacs::*minibuffer-point* 0)
         (clawmacs::*minibuffer-items* nil)
         (clawmacs::*minibuffer-filtered-items* nil)
         (clawmacs::*minibuffer-match-positions* nil)
         (clawmacs::*minibuffer-selected-index* 0)
         (clawmacs::*minibuffer-scroll-offset* 0)
         (clawmacs::*minibuffer-callback* nil))
    (is-true (clawmacs::chat-compose-application-input-active-p))
    (is (eql #\a (clawmacs::chat-compose-event-key event)))
    (clawmacs::handle-key-event (make-buffer "modal-compose"
                                             :session-persistence-mode :ephemeral)
                                (clawmacs::chat-compose-event-key event))
    (is (string= "a" clawmacs::*minibuffer-input*))
    (is (eql (code-char 7) (clawmacs::chat-compose-event-key control-event)))))

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
  (let ((clawmacs::*minibuffer-active* t)
        (clawmacs::*minibuffer-mode* :completion)
        (clawmacs::*minibuffer-prompt* "M-x")
        (clawmacs::*minibuffer-input* "td")
        (clawmacs::*minibuffer-point* 2)
        (clawmacs::*minibuffer-items* nil)
        (clawmacs::*minibuffer-filtered-items*
          (list (list :display "toggle-debug-mode-command")
                (list :display "toggle-tool-results-command")
                (list :display "toggle-metadata-output-command")))
        (clawmacs::*minibuffer-match-positions* nil)
        (clawmacs::*minibuffer-selected-index* 1)
        (clawmacs::*minibuffer-scroll-offset* 0)
        (clawmacs::*minibuffer-max-height* 3)
        (clawmacs::*minibuffer-callback* nil))
    (let ((text (clawmacs::chat-frame-e2e-minibuffer-text)))
      (is (search "M-x: td  [toggle-tool-results-command]  (2/3)" text))
      (is (search "  toggle-debug-mode-command" text))
      (is (search "> toggle-tool-results-command" text))
      (is-false (search "toggle-metadata-output-command" text)))))

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

(test mcclim-compose-pane-prefers-five-visible-rows
  "The compose pane prefers a compact fixed height of five visible rows."
  (let ((clawmacs::*chat-compose-visible-rows* 5)
        (clawmacs::*chat-compose-line-height* 24))
    (let* ((pane (make-instance 'clawmacs::clawmacs-chat-compose-pane
                                 :initial-contents ""
                                 :ncolumns 90
                                 :nlines 5
                                 :minibuffer nil
                                 :scroll-bars nil
                                 :border-width 0
                                 :activation-gestures '(:return)
                                 :activate-callback #'clawmacs::compose-pane-activated))
           (expected (clawmacs::chat-compose-desired-pixel-height))
           (space-before (clim:compose-space pane)))
      (is (= 100 (clim:space-requirement-height space-before)))
      (clawmacs::configure-chat-compose-pane pane)
      (let ((space-after (clim:compose-space pane)))
        (is (= expected (clim:space-requirement-height space-after)))
        (is (= expected (clim:space-requirement-min-height space-after)))
        (is (= expected (clim:space-requirement-max-height space-after)))))))

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
