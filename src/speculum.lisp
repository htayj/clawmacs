(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Speculum: McCLIM self-visibility tools
;;; --------------------------------------------------------------------------

(defparameter *speculum-refresh-wait-attempts* 20
  "Number of short sleeps used while waiting for a requested McCLIM redraw.")

(defparameter *speculum-refresh-wait-interval* 0.05
  "Seconds to sleep between Speculum redraw wait polls.")

(defparameter *speculum-default-message-limit* 10
  "Default number of recent messages included in Speculum state snapshots.")

(defparameter *speculum-max-message-chars* 1000
  "Maximum text retained per message in Speculum state snapshots.")

(defparameter *speculum-inspect-allowlist*
  '("clawmacs-frame"
    "live-frame-count"
    "frame-state"
    "frame-metrics"
    "visible-buffer"
    "recent-messages"
    "render-snapshot"
    "pane-sizes"
    "minibuffer"
    "selectors"
    "skill-completion"
    "approval"
    "key-prefixes")
  "State names accepted by speculum_inspect.")

(defparameter +speculum-missing+ (gensym "SPECULUM-MISSING-")
  "Internal marker for absent provider arguments.")

(defun speculum-trim-string (value)
  "Return VALUE as a whitespace-trimmed string, or NIL."
  (when value
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 (princ-to-string value))))

(defun speculum-blank-string-p (value)
  "Return true when VALUE is NIL or trims to the empty string."
  (let ((text (speculum-trim-string value)))
    (or (null text) (zerop (length text)))))

(defun speculum-symbol-name (value)
  "Return VALUE as a lowercase display string."
  (cond
    ((null value) nil)
    ((symbolp value) (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(defun speculum-class-name (object)
  "Return OBJECT's class name as a lowercase string."
  (handler-case
      (speculum-symbol-name (class-name (class-of object)))
    (error () "")))

(defun speculum-truncate-string (value max-chars)
  "Return VALUE truncated to MAX-CHARS characters."
  (let ((text (or value "")))
    (if (and max-chars (> (length text) max-chars))
        (concatenate 'string (subseq text 0 max-chars) "...")
        text)))

(defun speculum-split-lines (text)
  "Split TEXT into lines without retaining newline characters."
  (let ((value (or text "")))
    (loop :with len := (length value)
          :for start := 0 :then (1+ pos)
          :for pos := (position #\Newline value :start start)
          :collect (subseq value start (or pos len))
          :while pos)))

(defun speculum-sequence-list (value)
  "Return VALUE as a list, accepting vectors for provider array arguments."
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (list value))))

(defun speculum-positive-integer (value field-name default)
  "Return VALUE as a positive integer, or DEFAULT when absent."
  (cond
    ((null value) default)
    ((and (integerp value) (plusp value)) value)
    (t
     (error "~A must be a positive integer, got ~S." field-name value))))

(defun speculum-tool-arg (args &rest keys)
  "Return the first tool argument in ARGS matching KEYS."
  (apply #'tool-arg args keys))

(defun speculum-tool-arg-value (args keys &optional default)
  "Return the first tool argument in ARGS matching KEYS, preserving NIL."
  (loop :for (key . value) :in (tool-args-alist args)
        :when (member key keys :test #'tool-key=)
          :return value
        :finally (return default)))

(defun speculum-tool-bool (value default)
  "Return VALUE as a provider boolean, defaulting absent values to DEFAULT."
  (if (eq value +speculum-missing+)
      default
      (not (null value))))

(defun speculum-current-frame ()
  "Return the primary live Clawmacs McCLIM frame, or NIL."
  (or (and (boundp '*clawmacs-frame*)
           *clawmacs-frame*
           (not (ignore-errors (frame-quit-flag *clawmacs-frame*)))
           *clawmacs-frame*)
      (find-if (lambda (frame)
                 (not (ignore-errors (frame-quit-flag frame))))
               (mcclim-live-frames))))

(defun speculum-no-frame-result (&optional (reason "No live Clawmacs McCLIM frame is available."))
  "Return a structured unavailable result for non-GUI contexts."
  (list :ok nil
        :available nil
        :reason reason
        :live-frame-count (length (mcclim-live-frames))))

(defun speculum-render-sequence (frame)
  "Return FRAME's current render sequence, or zero."
  (or (and frame (ignore-errors (frame-render-sequence frame))) 0))

(defun speculum-refresh-frame (frame)
  "Request a standard McCLIM redisplay for FRAME and wait briefly."
  (if (null frame)
      (list :requested nil :completed nil :reason "No live frame.")
      (let ((before (speculum-render-sequence frame))
            (queued nil)
            (queue-error nil))
        (handler-case
            (progn
              (mcclim-queue-display-change
               frame (frame-visible-buffer frame) :speculum :force-p t)
              (setf queued t))
          (error (condition)
            (setf queue-error (format nil "~A" condition))))
        (let ((completed nil))
          (when queued
            (loop :repeat *speculum-refresh-wait-attempts*
                  :do (when (> (speculum-render-sequence frame) before)
                        (setf completed t)
                        (return))
                      (sleep *speculum-refresh-wait-interval*)))
          (list :requested queued
                :completed completed
                :sequence-before before
                :sequence-after (speculum-render-sequence frame)
                :error queue-error)))))

(defun speculum-message-summary (message)
  "Return an agent-readable summary of MESSAGE."
  (list :sender (speculum-symbol-name (message-sender message))
        :text (speculum-truncate-string (message-text message)
                                        *speculum-max-message-chars*)
        :read-only (not (null (message-read-only-p message)))
        :entry-id (or (message-entry-id message) "")
        :parent-entry-id (or (message-parent-entry-id message) "")))

(defun speculum-recent-messages (buffer limit)
  "Return up to LIMIT recent transcript messages for BUFFER."
  (if (null buffer)
      #()
      (let ((items nil))
        (loop :for message := (buffer-first-message buffer)
                :then (message-next message)
              :while (and message (not (eq message (buffer-input-message buffer))))
              :do (push (speculum-message-summary message) items))
        (coerce (last (nreverse items) limit) 'vector))))

(defun speculum-approval-state (buffer)
  "Return BUFFER's pending tool approval state, if present."
  (let ((approval (and buffer (buffer-approval-pending buffer))))
    (if approval
        (list :pending t
              :tool-name (or (getf approval :tool-name) "")
              :display (speculum-truncate-string
                        (or (getf approval :display-expanded)
                            (getf approval :display-raw)
                            "")
                        2000))
        (list :pending nil))))

(defun speculum-buffer-summary (buffer &key (message-limit *speculum-default-message-limit*))
  "Return a structured summary of BUFFER."
  (if (null buffer)
      (list :available nil)
      (list :available t
            :name (buffer-name buffer)
            :agent (buffer-agent-name buffer)
            :kind (speculum-symbol-name (buffer-kind buffer))
            :status (speculum-symbol-name (buffer-status buffer))
            :major-mode (buffer-major-mode buffer)
            :message-count (max 0 (1- (buffer-message-count buffer)))
            :input (speculum-truncate-string
                    (message-text (buffer-input-message buffer))
                    *speculum-max-message-chars*)
            :scroll-offset (buffer-scroll-offset buffer)
            :show-tool-results (not (null (buffer-show-tool-results-p buffer)))
            :show-reasoning (not (null (buffer-show-reasoning-p buffer)))
            :show-metadata (not (null (buffer-show-metadata-p buffer)))
            :enabled-packages (coerce (copy-list (buffer-enabled-packages buffer))
                                      'vector)
            :approval (speculum-approval-state buffer)
            :recent-messages (speculum-recent-messages buffer message-limit))))

(defun speculum-frame-summary (frame)
  "Return a structured summary of FRAME."
  (if (null frame)
      (list :available nil)
      (list :available t
            :class (speculum-class-name frame)
            :state (speculum-symbol-name
                    (ignore-errors (clim:frame-state frame)))
            :quit (not (null (ignore-errors (frame-quit-flag frame))))
            :follow-current-buffer
            (not (null (ignore-errors (frame-follow-current-buffer-p frame))))
            :char-width (or (ignore-errors (frame-char-width frame)) 0)
            :char-height (or (ignore-errors (frame-char-height frame)) 0)
            :pane-space-char-height
            (or (ignore-errors (frame-pane-space-char-height frame)) 0)
            :render-sequence (speculum-render-sequence frame)
            :live-frame-count (length (mcclim-live-frames)))))

(defun speculum-pane-summary (frame pane-name)
  "Return a structured summary of PANE-NAME in FRAME."
  (let ((pane (and frame (ignore-errors (clim:find-pane-named frame pane-name)))))
    (if (null pane)
        (list :name (string-downcase (symbol-name pane-name))
              :available nil)
        (multiple-value-bind (pixel-width pixel-height)
            (handler-case
                (pane-pixel-size pane)
              (error () (values 0 0)))
          (let* ((char-w (max 1 (or (ignore-errors (frame-char-width frame)) 1)))
                 (char-h (max 1 (or (ignore-errors (frame-char-height frame)) 1))))
            (multiple-value-bind (cols rows)
                (if (and (plusp pixel-width) (plusp pixel-height))
                    (pane-grid-dimensions pane char-w char-h)
                    (values 0 0))
              (list :name (string-downcase (symbol-name pane-name))
                    :available t
                    :class (speculum-class-name pane)
                    :pixel-width pixel-width
                    :pixel-height pixel-height
                    :cols cols
                    :rows rows)))))))

(defun speculum-panes-summary (frame)
  "Return summaries for the standard Clawmacs McCLIM panes."
  (coerce (mapcar (lambda (name)
                    (speculum-pane-summary frame name))
                  '(main-pane input-pane who-line-pane modeline-pane minibuffer-pane))
          'vector))

(defun speculum-selected-minibuffer-display ()
  "Return the selected minibuffer candidate display string, or NIL."
  (let ((items *minibuffer-filtered-items*)
        (index *minibuffer-selected-index*))
    (when (and items (<= 0 index) (< index (length items)))
      (minibuffer-item-display (nth index items)))))

(defun speculum-minibuffer-state ()
  "Return current minibuffer state."
  (list :active (not (null *minibuffer-active*))
        :mode (speculum-symbol-name *minibuffer-mode*)
        :prompt *minibuffer-prompt*
        :input *minibuffer-input*
        :point *minibuffer-point*
        :selected-index *minibuffer-selected-index*
        :selected (or (speculum-selected-minibuffer-display) "")
        :filtered-count (length *minibuffer-filtered-items*)))

(defun speculum-session-tree-selected ()
  "Return the selected session tree item id, or the empty string."
  (let ((item (ignore-errors (session-tree-selector-current-item))))
    (or (and item (getf item :id)) "")))

(defun speculum-selectors-state ()
  "Return current selector overlay state."
  (list :buffer-selector-active (not (null *buffer-selector-active*))
        :buffer-selector-index *buffer-selector-index*
        :session-tree-selector-active (not (null *session-tree-selector-active*))
        :session-tree-selector-index *session-tree-selector-index*
        :session-tree-selector-filter
        (speculum-symbol-name *session-tree-selector-filter-mode*)
        :session-tree-selector-search *session-tree-selector-search*
        :session-tree-selector-selected (speculum-session-tree-selected)
        :model-selector-active (not (null *model-selector-active*))
        :model-selector-index *model-selector-index*
        :think-selector-active (not (null *think-selector-active*))
        :think-selector-index *think-selector-index*))

(defun speculum-selected-skill-display ()
  "Return the selected skill-completion candidate display string, or NIL."
  (let ((items *skill-completion-filtered-items*)
        (index *skill-completion-selected-index*))
    (when (and items (<= 0 index) (< index (length items)))
      (minibuffer-item-display (nth index items)))))

(defun speculum-skill-completion-state ()
  "Return current skill completion popup state."
  (list :active (not (null *skill-completion-active*))
        :query *skill-completion-query*
        :token (or *skill-completion-token-text* "")
        :selected-index *skill-completion-selected-index*
        :selected (or (speculum-selected-skill-display) "")
        :filtered-count (length *skill-completion-filtered-items*)))

(defun speculum-key-prefix-state ()
  "Return pending key prefix state relevant to the McCLIM input path."
  (list :meta-pending (not (null *meta-pending*))
        :alt-pending (not (null *alt-pending*))
        :c-x-pending (not (null *cx-pending*))
        :c-c-pending (not (null *cc-pending*))
        :c-h-pending (not (null *ch-pending*))
        :alt-emulates-meta (not (null *alt-emulates-meta*))))

(defun speculum-interaction-state (frame buffer)
  "Return current GUI interaction state."
  (declare (ignore frame))
  (list :minibuffer (speculum-minibuffer-state)
        :selectors (speculum-selectors-state)
        :skill-completion (speculum-skill-completion-state)
        :approval (speculum-approval-state buffer)
        :key-prefixes (speculum-key-prefix-state)))

(defun speculum-render-state (frame)
  "Return FRAME's latest render snapshot, or an unavailable marker."
  (or (and frame (ignore-errors (frame-last-render-snapshot frame)))
      '((:ready . nil))))

(defun speculum-scope-name (value)
  "Normalize a Speculum scope designator."
  (let ((text (if (speculum-blank-string-p value)
                  "all"
                  (string-downcase (speculum-trim-string value)))))
    (with-output-to-string (out)
      (loop :for char :across text
            :do (write-char (if (char= char #\_) #\- char) out)))))

(defun speculum-window-state-data (args)
  "Return structured state for the current Clawmacs McCLIM window."
  (let ((frame (speculum-current-frame))
        (scope (speculum-scope-name (speculum-tool-arg args :scope "scope")))
        (message-limit
          (speculum-positive-integer
           (speculum-tool-arg args :message-limit "message_limit" "message-limit")
           "message-limit"
           *speculum-default-message-limit*)))
    (if (null frame)
        (speculum-no-frame-result)
        (let ((buffer (frame-visible-buffer frame)))
          (cond
            ((string= scope "summary")
             (list :ok t
                   :available t
                   :scope scope
                   :frame (speculum-frame-summary frame)
                   :buffer (speculum-buffer-summary buffer :message-limit 0)
                   :render (speculum-render-state frame)))
            ((string= scope "frame")
             (list :ok t :available t :scope scope
                   :frame (speculum-frame-summary frame)
                   :buffer (speculum-buffer-summary buffer
                                                    :message-limit message-limit)))
            ((string= scope "panes")
             (list :ok t :available t :scope scope
                   :panes (speculum-panes-summary frame)))
            ((string= scope "render")
             (list :ok t :available t :scope scope
                   :render (speculum-render-state frame)))
            ((string= scope "interaction")
             (list :ok t :available t :scope scope
                   :interaction (speculum-interaction-state frame buffer)))
            ((string= scope "all")
             (list :ok t
                   :available t
                   :scope scope
                   :frame (speculum-frame-summary frame)
                   :buffer (speculum-buffer-summary buffer
                                                    :message-limit message-limit)
                   :panes (speculum-panes-summary frame)
                   :render (speculum-render-state frame)
                   :interaction (speculum-interaction-state frame buffer)))
            (t
             (list :ok nil
                   :available t
                   :scope scope
                   :reason (format nil "Unknown speculum scope ~S." scope))))))))

(defun speculum-window-id-string-p (value)
  "Return true when VALUE is a simple decimal or hexadecimal X11 window id."
  (let ((text (speculum-trim-string value)))
    (and text
         (plusp (length text))
         (or (every #'digit-char-p text)
             (and (> (length text) 2)
                  (char= (char text 0) #\0)
                  (member (char text 1) '(#\x #\X) :test #'char=)
                  (loop :for index :from 2 :below (length text)
                        :always (digit-char-p (char text index) 16)))))))

(defun speculum-normalize-window-id (value)
  "Return VALUE as a safe X11 window id string, or NIL when absent."
  (cond
    ((null value) nil)
    ((integerp value) (princ-to-string value))
    ((and (stringp value) (speculum-window-id-string-p value))
     (speculum-trim-string value))
    (t
     (error "window-id must be a decimal or hexadecimal X11 window id, got ~S."
            value))))

(defun speculum-run-program (argv &key directory)
  "Run ARGV and return OK, STDOUT, STDERR, EXIT-CODE."
  (handler-case
      (multiple-value-bind (stdout stderr exit-code)
          (uiop:run-program argv
                            :directory directory
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (values t stdout stderr exit-code))
    (error (condition)
      (values nil "" (format nil "~A" condition) -1))))

(defun speculum-find-window-id (&optional window-id)
  "Return values WINDOW-ID and lookup metadata."
  (let ((explicit (speculum-normalize-window-id window-id)))
    (if explicit
        (values explicit (list :source "argument" :ok t))
        (multiple-value-bind (ran stdout stderr exit-code)
            (speculum-run-program '("xdotool" "search" "--name" "^Clawmacs$"))
          (let* ((lines (remove-if #'speculum-blank-string-p
                                   (speculum-split-lines stdout)))
                 (found (car (last lines))))
            (if (and ran (zerop exit-code) found
                     (speculum-window-id-string-p found))
                (values found
                        (list :source "xdotool"
                              :ok t
                              :exit-code exit-code
                              :stdout stdout
                              :stderr stderr))
                (values nil
                        (list :source "xdotool"
                              :ok nil
                              :exit-code exit-code
                              :stdout stdout
                              :stderr stderr))))))))

(defun speculum-default-screenshot-path ()
  "Return a default sandbox-local PNG pathname for a Speculum screenshot."
  (validate-sandbox-path
   (format nil "screenshots/speculum/speculum-~D-~D.png"
           (get-universal-time)
           (get-internal-real-time))))

(defun speculum-screenshot-path (value)
  "Return VALUE as a validated screenshot pathname, or the default path."
  (let ((path (if (speculum-blank-string-p value)
                  (speculum-default-screenshot-path)
                  (validate-sandbox-path value))))
    (ensure-directories-exist path)
    path))

(defun speculum-file-size (path)
  "Return PATH's byte size, or NIL if it cannot be read."
  (handler-case
      (with-open-file (stream path :element-type '(unsigned-byte 8))
        (file-length stream))
    (error () nil)))

(defun speculum-screenshot-data (args)
  "Capture the current McCLIM window and return structured screenshot data."
  (let ((frame (speculum-current-frame)))
    (if (null frame)
        (speculum-no-frame-result)
        (let* ((refresh-p
                 (speculum-tool-bool
                  (speculum-tool-arg-value args '(:refresh "refresh")
                                           +speculum-missing+)
                  t))
               (path (speculum-screenshot-path
                      (speculum-tool-arg args :path "path")))
               (refresh (when refresh-p (speculum-refresh-frame frame))))
          (multiple-value-bind (window-id lookup)
              (speculum-find-window-id
               (speculum-tool-arg args :window-id "window_id" "window-id"))
            (if (null window-id)
                (list :ok nil
                      :available t
                      :reason "Could not find the Clawmacs X11 window."
                      :path (namestring path)
                      :refresh refresh
                      :window-lookup lookup
                      :frame (speculum-frame-summary frame)
                      :render (speculum-render-state frame))
                (multiple-value-bind (ran stdout stderr exit-code)
                    (speculum-run-program
                     (list "import" "-window" window-id (namestring path)))
                  (let ((file-bytes (and (probe-file path)
                                         (speculum-file-size path))))
                    (list :ok (and ran (zerop exit-code)
                                   file-bytes (plusp file-bytes))
                          :available t
                          :path (namestring path)
                          :window-id window-id
                          :refresh refresh
                          :window-lookup lookup
                          :command "import -window"
                          :exit-code exit-code
                          :stdout stdout
                          :stderr stderr
                          :file-bytes (or file-bytes 0)
                          :frame (speculum-frame-summary frame)
                          :render (speculum-render-state frame))))))))))

(defun speculum-normalize-inspect-name (value)
  "Normalize an inspect name to lowercase hyphenated text."
  (let ((text (string-downcase (or (speculum-trim-string value) ""))))
    (with-output-to-string (out)
      (loop :for char :across text
            :do (write-char (if (char= char #\_) #\- char) out)))))

(defun speculum-inspection-value (name frame message-limit)
  "Return an allowlisted inspection result for NAME."
  (let* ((normalized (speculum-normalize-inspect-name name))
         (buffer (and frame (frame-visible-buffer frame))))
    (list :name (or (speculum-trim-string name) "")
          :key normalized
          :available
          (cond
            ((not (member normalized *speculum-inspect-allowlist*
                          :test #'string=))
             nil)
            ((string= normalized "live-frame-count") t)
            (t (not (null frame))))
          :value
          (cond
            ((not (member normalized *speculum-inspect-allowlist*
                          :test #'string=))
             nil)
            ((string= normalized "clawmacs-frame")
             (speculum-frame-summary frame))
            ((string= normalized "live-frame-count")
             (length (mcclim-live-frames)))
            ((string= normalized "frame-state")
             (and frame (speculum-symbol-name
                         (ignore-errors (clim:frame-state frame)))))
            ((string= normalized "frame-metrics")
             (and frame
                  (list :char-width (frame-char-width frame)
                        :char-height (frame-char-height frame)
                        :pane-space-char-height
                        (frame-pane-space-char-height frame)
                        :render-sequence (frame-render-sequence frame))))
            ((string= normalized "visible-buffer")
             (speculum-buffer-summary buffer :message-limit 0))
            ((string= normalized "recent-messages")
             (speculum-recent-messages buffer message-limit))
            ((string= normalized "render-snapshot")
             (speculum-render-state frame))
            ((string= normalized "pane-sizes")
             (speculum-panes-summary frame))
            ((string= normalized "minibuffer")
             (speculum-minibuffer-state))
            ((string= normalized "selectors")
             (speculum-selectors-state))
            ((string= normalized "skill-completion")
             (speculum-skill-completion-state))
            ((string= normalized "approval")
             (speculum-approval-state buffer))
            ((string= normalized "key-prefixes")
             (speculum-key-prefix-state))
            (t nil))
          :reason
          (cond
            ((not (member normalized *speculum-inspect-allowlist*
                          :test #'string=))
             "Unknown speculum inspection name.")
            ((and (null frame)
                  (not (string= normalized "live-frame-count")))
             "No live Clawmacs McCLIM frame is available.")
            (t nil)))))

(defun speculum-inspect-data (args)
  "Inspect allowlisted McCLIM state names."
  (let* ((frame (speculum-current-frame))
         (message-limit
           (speculum-positive-integer
            (speculum-tool-arg args :message-limit "message_limit" "message-limit")
            "message-limit"
            *speculum-default-message-limit*))
         (names (speculum-sequence-list
                 (speculum-tool-arg args :names "names"))))
    (if (null names)
        (list :ok t
              :available (not (null frame))
              :allowlist (coerce *speculum-inspect-allowlist* 'vector))
        (list :ok t
              :available (not (null frame))
              :results (coerce (mapcar (lambda (name)
                                          (speculum-inspection-value
                                           name frame message-limit))
                                        names)
                               'vector)
              :allowlist (coerce *speculum-inspect-allowlist* 'vector)))))

(defun speculum-tool-screenshot (args)
  "Provider adapter for speculum_screenshot."
  (speculum-screenshot-data args))

(defun speculum-tool-window-state (args)
  "Provider adapter for speculum_window_state."
  (speculum-window-state-data args))

(defun speculum-tool-inspect (args)
  "Provider adapter for speculum_inspect."
  (speculum-inspect-data args))
