(in-package :clawmacs/tests)

(in-suite clawmacs-suite)

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
