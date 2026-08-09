(in-package :rplaca/tests)

(in-suite listener-presentation-suite)

;;; ===========================================================================
;;; Todo 10: final assistant-turn renderer, detail translators, mode/stop/compose
;;; commands, wholine progress, and frame keybindings.
;;;
;;; Deterministic and headless where possible.  Presentation-record structure
;;; (single-box, exact-one record) is verified by the real CLX manual proof;
;;; these tests verify the text shape, facet presence, command state effects,
;;; and the store-before-layout ordering invariant via notinline seams.
;;; ===========================================================================

;;; --------------------------------------------------------------------------
;;; Test seams and fixtures.
;;; --------------------------------------------------------------------------

(defmacro with-presentation-override ((name lambda-list &body impl) &body body)
  "Temporarily replace NAME during one listener-presentation test, then restore."
  (let ((original (gensym "ORIGINAL"))
        (existed (gensym "EXISTED-P")))
    `(let ((,existed (fboundp ',name))
           (,original (and (fboundp ',name) (symbol-function ',name))))
       (unwind-protect
            (progn
              (setf (symbol-function ',name)
                    (lambda ,lambda-list ,@impl))
              ,@body)
         (if ,existed
             (setf (symbol-function ',name) ,original)
             (fmakunbound ',name))))))

(defmacro with-presentation-overrides (bindings &body body)
  (if bindings
      `(with-presentation-override ,(first bindings)
         (with-presentation-overrides ,(rest bindings) ,@body))
      `(progn ,@body)))

(defun make-presentation-frame (&key (mode :eval))
  "Allocate a rplaca-listener frame with a real conversation buffer."
  (let ((rplaca::*package-appearance-catalog* (rplaca::make-classic-appearance-catalog)))
    (clim:make-application-frame
     'rplaca::rplaca-listener
     :conversation-buffer
     (rplaca::make-buffer "listener-presentation-test"
                          :agent-name "tester"
                          :working-directory (truename ".")
                          :kind :chat
                          :session-persistence-mode :persistent
                          :session nil)
     :listener-context (rplaca::make-listener-context :input-mode mode)
     :appearance-profile (rplaca::make-appearance-profile))))

(defun emit-turn-to-string (frame turn)
  "Run emit-listener-assistant-turn with frame-standard-output bound to a
string stream and return the captured text."
  (let ((output (make-string-output-stream)))
    (with-presentation-overrides
        ((clim:frame-standard-output (ignored-frame)
           (declare (ignore ignored-frame))
           output))
      (rplaca::emit-listener-assistant-turn frame turn))
    (get-output-stream-string output)))

(defun count-substring (needle haystack)
  (loop :with count := 0
        :with start := 0
        :for position := (search needle haystack :start2 start)
        :while position
        :do (incf count)
            (setf start (+ position (length needle)))
        :finally (return count)))

;;; --------------------------------------------------------------------------
;;; Final renderer: bounded primary body once + facet summaries.
;;; --------------------------------------------------------------------------

(test emit-assistant-turn-primary-body-appears-exactly-once
  (let* ((frame (make-presentation-frame))
         (turn (rplaca::make-assistant-turn :primary-text "hello world")))
    (let ((text (emit-turn-to-string frame turn)))
      (is (= 1 (count-substring "hello world" text))
          "primary body must appear exactly once, got: ~S" text))))

(test emit-assistant-turn-facets-appear-only-when-data-exists
  (let* ((frame (make-presentation-frame))
         (turn (rplaca::make-assistant-turn
                :primary-text "body"
                :tool-uses (list (list :name "read"))
                :reasoning (list "a thought")
                :metadata (list :model "m")
                :artifact-refs (list "art")
                :media-refs (list "med")
                :inspect-payload (list :inspect t))))
    (let ((text (emit-turn-to-string frame turn)))
      ;; Every non-empty facet emits a compact label presentation.
      (is (= 1 (count-substring "[tools]" text)))
      (is (= 1 (count-substring "[reasoning]" text)))
      (is (= 1 (count-substring "[metadata]" text)))
      (is (= 1 (count-substring "[artifacts]" text)))
      (is (= 1 (count-substring "[media]" text)))
      (is (= 1 (count-substring "[inspect]" text)))
      ;; Primary body still exactly once.
      (is (= 1 (count-substring "body" text))))))

(test emit-assistant-turn-empty-facets-are-absent
  (let* ((frame (make-presentation-frame))
         (turn (rplaca::make-assistant-turn :primary-text "only body")))
    (let ((text (emit-turn-to-string frame turn)))
      (is (= 1 (count-substring "only body" text)))
      (is (zerop (count-substring "[tools]" text)))
      (is (zerop (count-substring "[reasoning]" text)))
      (is (zerop (count-substring "[metadata]" text)))
      (is (zerop (count-substring "[artifacts]" text)))
      (is (zerop (count-substring "[media]" text)))
      (is (zerop (count-substring "[inspect]" text))))))

(test emit-assistant-turn-does-not-emit-transcript-or-system-messages
  "No token streaming/typeout: the renderer emits only the primary body and
facet labels, never a transcript view or system markers."
  (let* ((frame (make-presentation-frame))
         (turn (rplaca::make-assistant-turn :primary-text "clean body")))
    (let ((text (emit-turn-to-string frame turn)))
      (is (not (search "[system]" text)))
      (is (not (search "transcript" text))))))

(test listener-turn-nonempty-facet-kinds-canonical-order
  (let ((full (rplaca::make-assistant-turn
               :primary-text "b"
               :tool-uses (list :x) :reasoning (list :y)
               :metadata (list :k :v) :artifact-refs (list "a")
               :media-refs (list "m") :inspect-payload :p)))
    (is (equal '(:tools :reasoning :metadata :artifacts :media :inspect)
               (rplaca::listener-turn-nonempty-facet-kinds full))))
  (let ((tools-only (rplaca::make-assistant-turn
                     :primary-text "b" :tool-uses (list :x))))
    (is (equal '(:tools) (rplaca::listener-turn-nonempty-facet-kinds tools-only))))
  (let ((empty (rplaca::make-assistant-turn :primary-text "b")))
    (is (null (rplaca::listener-turn-nonempty-facet-kinds empty)))))

;;; --------------------------------------------------------------------------
;;; Detail commands: store selection BEFORE layout switch, restore on close.
;;; --------------------------------------------------------------------------

(test close-details-presentation-type-exists
  (let ((output (with-output-to-string (s)
                  (clim:present :close 'rplaca::close-details :stream s))))
    (is (search "[close]" output))))

(test com-show-turn-details-stores-selection-before-layout-switch
  (let* ((frame (make-presentation-frame))
         (turn (rplaca::make-assistant-turn
                :primary-text "body" :tool-uses (list (list :name "read"))))
         (facet (rplaca::make-turn-facet :turn turn :kind :tools))
         (selection-at-switch nil)
         (switch-called nil))
    ;; The notinline layout seam records the selection state at the exact
    ;; moment the layout switch is requested, proving store-before-switch.
    (with-presentation-override
        (rplaca::listener-set-details-layout (f)
          (setf selection-at-switch (rplaca::rplaca-listener-selected-detail f)
                switch-called t))
      (let ((clim:*application-frame* frame))
        (rplaca::com-show-turn-details facet)))
    (is-true switch-called)
    (is (eq turn (car selection-at-switch))
        "selection must already be stored when the layout switch fires")
    (is (eq :tools (cdr selection-at-switch)))
    (is (equal (cons turn :tools)
               (rplaca::rplaca-listener-selected-detail frame)))))

(test com-close-details-clears-selection-before-layout-switch
  (let* ((frame (make-presentation-frame))
         (turn (rplaca::make-assistant-turn :primary-text "body"))
         (selection-at-switch :not-cleared)
         (switch-called nil))
    (rplaca::set-rplaca-listener-selected-detail frame turn :reasoning)
    (with-presentation-override
        (rplaca::listener-set-listener-layout (f)
          (setf selection-at-switch (rplaca::rplaca-listener-selected-detail f)
                switch-called t))
      (let ((clim:*application-frame* frame))
        (rplaca::com-close-details)))
    (is-true switch-called)
    (is (null selection-at-switch)
        "selection must already be cleared when the layout switch fires")
    (is (null (rplaca::rplaca-listener-selected-detail frame)))))

(test display-turn-details-emits-close-presentation-when-detail-selected
  (let* ((frame (make-presentation-frame))
         (turn (rplaca::make-assistant-turn
                :primary-text "body" :tool-uses (list (list :name "read")))))
    (rplaca::set-rplaca-listener-selected-detail frame turn :tools)
    (let ((text (with-output-to-string (s)
                  (rplaca::display-turn-details frame s))))
      (is (search "[close]" text))
      (is (search "read" text)))))

;;; --------------------------------------------------------------------------
;;; Mode commands: update context and prompt, testable headlessly.
;;; --------------------------------------------------------------------------

(test com-lisp-mode-sets-eval-context-and-prompt
  (let ((frame (make-presentation-frame :mode :say)))
    (is (eq :say (rplaca::listener-context-input-mode
                  (rplaca::rplaca-listener-context frame))))
    (let ((clim:*application-frame* frame))
      (rplaca::com-lisp-mode))
    (is (eq :eval (rplaca::listener-context-input-mode
                   (rplaca::rplaca-listener-context frame))))
    (is (string= "CL-USER> "
                 (with-output-to-string (s)
                   (rplaca::listener-print-prompt s frame))))))

(test com-say-mode-sets-say-context-and-prompt
  (let ((frame (make-presentation-frame :mode :eval)))
    (let ((clim:*application-frame* frame))
      (rplaca::com-say-mode))
    (is (eq :say (rplaca::listener-context-input-mode
                   (rplaca::rplaca-listener-context frame))))
    (is (string= "CL-USER!> "
                 (with-output-to-string (s)
                   (rplaca::listener-print-prompt s frame))))))

(test com-lisp-mode-and-com-say-mode-are-command-table-commands
  (let ((table (clim:find-command-table 'rplaca::rplaca-listener)))
    (is-true (clim:command-present-in-command-table-p
              'rplaca::com-lisp-mode table))
    (is-true (clim:command-present-in-command-table-p
              'rplaca::com-say-mode table))
    (is-true (clim:command-present-in-command-table-p
              'rplaca::com-stop-response table))
    (is-true (clim:command-present-in-command-table-p
              'rplaca::com-compose table))
    (is-true (clim:command-present-in-command-table-p
              'rplaca::com-show-turn-details table))
    (is-true (clim:command-present-in-command-table-p
              'rplaca::com-close-details table))))

;;; --------------------------------------------------------------------------
;;; Stop Response: busy-guarded and idempotent (does not duplicate todo9).
;;; --------------------------------------------------------------------------

(test com-stop-response-is-no-op-when-not-busy
  (let ((frame (make-presentation-frame))
        (stop-count 0))
    (with-presentation-overrides
        ((rplaca::buffer-agent-busy-p (buffer)
           (declare (ignore buffer))
           nil)
         (rplaca::stop-streaming-response (buffer)
           (declare (ignore buffer))
           (incf stop-count)
           t))
      (let ((clim:*application-frame* frame))
        (rplaca::com-stop-response)))
    (is (zerop stop-count)
        "Stop Response must not call stop-streaming-response when not busy")))

(test com-stop-response-calls-stop-when-busy-and-is-idempotent
  (let ((frame (make-presentation-frame))
        (busy t)
        (stop-count 0))
    (with-presentation-overrides
        ((rplaca::buffer-agent-busy-p (buffer)
           (declare (ignore buffer))
           busy)
         (rplaca::stop-streaming-response (buffer)
           (declare (ignore buffer))
           ;; Idempotent: the second invocation finds nothing owned.
           (incf stop-count)
           (prog1 (eq busy t)
             (setf busy nil))))
      (let ((clim:*application-frame* frame))
        (rplaca::com-stop-response)
        (rplaca::com-stop-response)
        (rplaca::com-stop-response)))
    ;; First call stops the owned runtime; subsequent calls find not-busy.
    (is (= 1 stop-count)
        "Stop Response must be idempotent: stop called once for one busy turn")))

;;; --------------------------------------------------------------------------
;;; Compose: multiline text submitted once to com-say, blank/busy rejected.
;;; --------------------------------------------------------------------------

(test com-compose-submits-multiline-text-once-to-com-say
  (let ((frame (make-presentation-frame))
        (say-arg nil)
        (say-count 0))
    (with-presentation-overrides
        ((rplaca::listener-read-compose-text (f)
           (declare (ignore f))
           (coerce (list #\l #\i #\n #\e #\1 #\Newline #\l #\i #\n #\e #\2) 'string))
         (rplaca::com-say (text)
           (setf say-arg text)
           (incf say-count)))
      (let ((clim:*application-frame* frame))
        (rplaca::com-compose)))
    (is (= 1 say-count) "Compose must hand multiline text to com-say exactly once")
    (is (string= "line1
line2" say-arg))))

(test com-compose-rejects-blank-before-sending
  (let ((frame (make-presentation-frame))
        (output (make-string-output-stream))
        (say-count 0))
    (with-presentation-overrides
        ((rplaca::listener-read-compose-text (f)
           (declare (ignore f))
           "   ")
         (rplaca::com-say (text)
           (declare (ignore text))
           (incf say-count))
         (clim:frame-standard-output (f)
           (declare (ignore f))
           output))
      (let ((clim:*application-frame* frame))
        (rplaca::com-compose)))
    (is (zerop say-count))
    (is (search "blank" (get-output-stream-string output)))))

(test com-compose-rejects-busy-before-reading-or-sending
  (let* ((frame (make-presentation-frame))
         (buffer (rplaca::rplaca-listener-conversation-buffer frame))
         (read-count 0)
         (say-count 0))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-presentation-overrides
        ((rplaca::listener-read-compose-text (f)
           (declare (ignore f))
           (incf read-count)
           "should-not-reach")
         (rplaca::com-say (text)
           (declare (ignore text))
           (incf say-count))
         (clim:frame-standard-output (f)
           (declare (ignore f))
           (make-string-output-stream)))
      (let ((clim:*application-frame* frame))
        (rplaca::com-compose)))
    (is (zerop read-count) "Compose must reject busy before reading compose text")
    (is (zerop say-count))))

;;; --------------------------------------------------------------------------
;;; Wholine: progress + Esc hint during active turn, cleared when idle.
;;; --------------------------------------------------------------------------

(defun make-active-await-request (frame phase)
  (let ((req (rplaca::make-listener-await-request
              :frame frame
              :buffer (rplaca::rplaca-listener-conversation-buffer frame)
              :dispatch-result :started
              :lifecycle-generation 0
              :wait-generation 0
              :expected-runtime-generation 0)))
    (setf (rplaca::listener-await-request-phase req) phase)
    req))

(test display-listener-wholine-shows-progress-and-esc-hint-during-wait
  (let ((frame (make-presentation-frame)))
    (setf (rplaca::rplaca-listener-progress frame) "[working]")
    (setf (rplaca::rplaca-listener-active-await-request frame)
          (make-active-await-request frame :waiting))
    (let ((text (with-output-to-string (s)
                  (rplaca::display-listener-wholine frame s))))
      (is (search "[working]" text))
      (is (search "Esc" text)))))

(test display-listener-wholine-clears-progress-and-hint-when-idle
  (let ((frame (make-presentation-frame)))
    (setf (rplaca::rplaca-listener-progress frame) nil)
    (setf (rplaca::rplaca-listener-active-await-request frame) nil)
    (let ((text (with-output-to-string (s)
                  (rplaca::display-listener-wholine frame s))))
      (is (not (search "[working]" text)))
      (is (not (search "Esc" text))))))

(test display-listener-wholine-progress-clears-after-settlement
  "When the await request is settled/retired, no progress or hint shows even if
the progress slot still holds a stale label."
  (let ((frame (make-presentation-frame)))
    (setf (rplaca::rplaca-listener-progress frame) "[working]")
    (setf (rplaca::rplaca-listener-active-await-request frame)
          (make-active-await-request frame :settled))
    (let ((text (with-output-to-string (s)
                  (rplaca::display-listener-wholine frame s))))
      (is (not (search "[working]" text)))
      (is (not (search "Esc" text))))))
