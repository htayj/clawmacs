(in-package :clawmacs/tests)

(def-suite render-suite
  :description "Rendering helper tests"
  :in clawmacs-suite)

(in-suite render-suite)

(test format-modeline-basic
  "format-modeline produces a left/right aligned string with major mode."
  (let* ((buf (make-buffer "test:session" :agent-name "claude"
                           :working-directory #P"/tmp/")))
    (setf (buffer-token-count buf) 1000
          (buffer-context-limit buf) 200000
          (buffer-status buf) :idle)
    (let ((ml (clawmacs::format-modeline buf 100 :provider-model "anthropic/test-model")))
      (is (= 100 (length ml)))
      ;; Major mode defaults to "chat"
      (is (search "[chat]" ml))
      (is (search "test:session" ml))
      (is (search "anthropic/test-model" ml))
      (is (search "1000/200000" ml))
      (is (search "IDLE" ml)))))

(test format-modeline-major-mode
  "format-modeline displays the provided major-mode."
  (let* ((buf (make-buffer "s1" :agent-name "agent"
                           :working-directory #P"/tmp/")))
    (setf (buffer-status buf) :idle)
    (let ((ml (clawmacs::format-modeline buf 120
                :major-mode "buffer-selector"
                :provider-model "zai/glm-5")))
      (is (= 120 (length ml)))
      (is (search "[buffer-selector]" ml))
      (is (search "zai/glm-5" ml)))))

(test format-modeline-provider-model-position
  "provider/model appears between agent name and working directory."
  (let* ((buf (make-buffer "s1" :agent-name "myagent"
                           :working-directory #P"/home/")))
    (setf (buffer-status buf) :idle)
    (let ((ml (clawmacs::format-modeline buf 120
                :provider-model "openai-codex/o4-mini")))
      ;; The order in the modeline should be: [chat] s1 | myagent | openai-codex/o4-mini | /home/
      (let ((mode-pos (search "[chat]" ml))
            (name-pos (search "s1" ml))
            (agent-pos (search "myagent" ml))
            (pm-pos (search "openai-codex/o4-mini" ml))
            (wd-pos (search "/home/" ml)))
        (is (not (null mode-pos)))
        (is (not (null name-pos)))
        (is (not (null agent-pos)))
        (is (not (null pm-pos)))
        (is (not (null wd-pos)))
        ;; Verify ordering
        (is (< mode-pos name-pos))
        (is (< name-pos agent-pos))
        (is (< agent-pos pm-pos))
        (is (< pm-pos wd-pos))))))

(test format-modeline-truncates-to-width
  "format-modeline truncates when content exceeds width."
  (let* ((buf (make-buffer "very-long-session-name" :agent-name "long-agent-name"
                           :working-directory #P"/a/very/long/path/that/goes/on/forever/")))
    (setf (buffer-token-count buf) 999999
          (buffer-context-limit buf) 999999
          (buffer-status buf) :thinking)
    (let ((ml (clawmacs::format-modeline buf 40 :provider-model "anthropic/claude-sonnet-4.6")))
      (is (= 40 (length ml))))))

(test calculate-input-height-minimum
  "Input height is at least 3 rows."
  (let ((buf (make-buffer "test")))
    ;; width=80 so wrapping doesn't affect a 1-line empty message
    (is (= 3 (clawmacs::calculate-input-height buf 30 80)))))

(test calculate-input-height-maximum
  "Input height is capped at (floor terminal-height 3)."
  (let ((buf (make-buffer "test")))
    (dotimes (i 20)
      (message-insert-newline (buffer-input-message buf)))
    ;; 21 lines, terminal height 30, max = 10
    (is (= 10 (clawmacs::calculate-input-height buf 30 80)))))

;;; --------------------------------------------------------------------------
;;; Line Wrapping Tests
;;; --------------------------------------------------------------------------

(test wrapped-line-count-short
  "Short lines take 1 row."
  (is (= 1 (clawmacs::wrapped-line-count "" 40)))
  (is (= 1 (clawmacs::wrapped-line-count "hello" 40)))
  (is (= 1 (clawmacs::wrapped-line-count (make-string 40 :initial-element #\x) 40))))

(test wrapped-line-count-wrapping
  "Long lines wrap to multiple rows."
  (is (= 2 (clawmacs::wrapped-line-count (make-string 41 :initial-element #\x) 40)))
  (is (= 2 (clawmacs::wrapped-line-count (make-string 80 :initial-element #\x) 40)))
  (is (= 3 (clawmacs::wrapped-line-count (make-string 81 :initial-element #\x) 40))))

(test message-visual-height-basic
  "A single-line message takes 1 visual row."
  (let ((m (make-message :user)))
    (is (= 1 (clawmacs::message-visual-height m 80)))))

(test message-visual-height-multiline
  "Multi-line messages sum up visual rows."
  (let ((m (make-message :user)))
    (message-insert-newline m)
    (message-insert-newline m)
    ;; 3 empty lines = 3 rows
    (is (= 3 (clawmacs::message-visual-height m 80)))))

(test message-visual-height-with-wrapping
  "Long lines in messages wrap, increasing visual height."
  (let ((m (make-message :user)))
    ;; prefix "user> " = 6 chars, so display-width = 80 - 6 = 74
    ;; Insert 150 chars = ceiling(150/74) = 3 visual rows
    (dotimes (i 150)
      (message-insert-char m #\x))
    (is (= 3 (clawmacs::message-visual-height m 80)))))

;;; --------------------------------------------------------------------------
;;; Buffer Selector Rendering Tests
;;; --------------------------------------------------------------------------

(test format-selector-line-fits-width
  "format-selector-line output is exactly the given width."
  (let ((line (clawmacs::format-selector-line "▸ " "my-session" "claude" "idle" "5" 80)))
    (is (= 80 (length line)))
    (is (search "my-session" line))
    (is (search "claude" line))
    (is (search "idle" line))
    (is (search "5" line))))

(test format-selector-line-truncates-at-narrow-width
  "format-selector-line truncates to fit narrow terminals."
  (let ((line (clawmacs::format-selector-line "  " "very-long-session-name" "claude" "thinking" "12" 40)))
    (is (= 40 (length line)))))
