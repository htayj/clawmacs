(in-package :clawmacs/tests)

(def-suite render-suite
  :description "Rendering helper tests"
  :in clawmacs-suite)

(in-suite render-suite)

(test format-modeline-basic
  "format-modeline produces a left/right aligned string with major mode."
  (let* ((buf (make-buffer "test:session" :agent-name "coder"
                           :working-directory #P"/tmp/")))
    (setf (buffer-token-count buf) 1000
          (buffer-context-limit buf) 200000
          (buffer-status buf) :idle)
    (let ((ml (clawmacs::format-modeline buf 100 :provider-model "zai/test-model")))
      (is (= 100 (length ml)))
      ;; Major mode defaults to "chat"
      (is (search "[chat]" ml))
      (is (search "test:session" ml))
      (is (search "zai/test-model" ml))
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

(test resolve-modeline-provider-model-includes-think-level
  "Modeline provider/model text includes think level when a buffer override is set."
  (let ((buf (make-buffer "s1" :agent-name "myagent"
                          :working-directory #P"/home/")))
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.4")
    (set-buffer-think-level-override buf "high")
    (let ((pm (clawmacs::resolve-modeline-provider-model buf)))
      (is (search "openai-codex/gpt-5.4" pm))
      (is (search "[think:high]" pm)))))

(test format-who-line-default-includes-redraw-hint
  "Default who-line help mentions the redraw keybinding."
  (let ((*minibuffer-active* nil)
        (*buffer-selector-active* nil)
        (*model-selector-active* nil)
        (*think-selector-active* nil)
        (*customize-face-state* nil)
        (*openai-oauth-pending* nil)
        (*deny-message-mode* nil)
        (clawmacs::*buffer-ring* nil))
    (let ((buf (make-buffer "s1" :agent-name "agent"
                            :working-directory #P"/tmp/")))
      (multiple-value-bind (row1 row2)
          (clawmacs::format-who-line buf 120)
        (declare (ignore row1))
        (is (search "C-l: redraw" row2))))))

(test format-who-line-scratch-describes-editing
  "Scratch buffers advertise editing rather than chat send semantics."
  (let ((*minibuffer-active* nil)
        (*buffer-selector-active* nil)
        (*model-selector-active* nil)
        (*think-selector-active* nil)
        (*customize-face-state* nil)
        (*openai-oauth-pending* nil)
        (*deny-message-mode* nil)
        (clawmacs::*buffer-ring* nil))
    (let ((buf (make-buffer "*scratch*" :kind :scratch)))
      (multiple-value-bind (row1 row2)
          (clawmacs::format-who-line buf 120)
        (is (search "Scratch buffer" row1))
        (is (search "RET: newline" row1))
        (is (search "switch" row2))))))

(test format-modeline-truncates-to-width
  "format-modeline truncates when content exceeds width."
  (let* ((buf (make-buffer "very-long-session-name" :agent-name "long-agent-name"
                           :working-directory #P"/a/very/long/path/that/goes/on/forever/")))
    (setf (buffer-token-count buf) 999999
          (buffer-context-limit buf) 999999
          (buffer-status buf) :thinking)
    (let ((ml (clawmacs::format-modeline buf 40 :provider-model "openai-codex/gpt-5.4")))
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

(test scratch-buffer-scroll-geometry-bottom-aligns-long-text
  "Scratch render geometry uses full-window rows and bottom-aligned scrolling."
  (let ((buf (make-buffer "*scratch*" :kind :scratch)))
    (setf (scratch-buffer-text buf) (format nil "1~%2~%3~%4~%5"))
    (multiple-value-bind (start scroll max-scroll)
        (clawmacs::scratch-buffer-scroll-geometry buf 3 80)
      (is (= -2 start))
      (is (= 0 scroll))
      (is (= 2 max-scroll)))
    (setf (buffer-scroll-offset buf) 1)
    (multiple-value-bind (start scroll max-scroll)
        (clawmacs::scratch-buffer-scroll-geometry buf 3 80)
      (is (= -1 start))
      (is (= 1 scroll))
      (is (= 2 max-scroll)))))

(test ensure-croatoan-locale-calls-setlocale
  "Croatoan backend initializes libc locale before starting ncurses."
  (let ((captured-category nil)
        (captured-locale nil))
    (with-function-override (ncurses:setlocale (category locale)
                              (setf captured-category category
                                    captured-locale locale)
                              "C.UTF-8")
      (is (string= "C.UTF-8" (clawmacs::ensure-croatoan-locale))))
    (is (= ncurses:+lc-all+ captured-category))
    (is (string= "" captured-locale))))

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

(test message-display-lines-include-reasoning-when-enabled
  "Reasoning blocks are display-only lines hidden unless explicitly enabled."
  (let ((m (make-message :agent)))
    (clawmacs::set-message-text m "Final answer")
    (setf (message-raw-content m)
          (list (clawmacs::canonical-text-block "Final answer")
                (clawmacs::canonical-reasoning-block "provider thoughts")))
    (is (equal '("Final answer")
               (clawmacs::message-display-line-strings m)))
    (is (equal '("Final answer" ";; reasoning" "provider thoughts")
               (clawmacs::message-display-line-strings
                m
                :show-reasoning-p t)))
    (is (= 1 (clawmacs::message-visual-height m 80)))
    (is (= 3 (clawmacs::message-visual-height
              m 80
              :show-reasoning-p t)))))

(test reasoning-only-message-visibility-follows-buffer-toggle
  "Messages containing only reasoning are hidden until reasoning output is enabled."
  (let ((buf (make-buffer "reasoning-toggle"))
        (m (make-message :agent)))
    (setf (message-raw-content m)
          (list (clawmacs::canonical-reasoning-block "provider thoughts")))
    (is (not (clawmacs::message-visible-for-buffer-p m buf)))
    (setf (buffer-show-reasoning-p buf) t)
    (is (clawmacs::message-visible-for-buffer-p m buf))))

(test message-display-lines-note-missing-requested-reasoning
  "Reasoning output shows an unavailable marker when the provider returns none."
  (let ((m (make-message :agent)))
    (clawmacs::set-message-text m "Final answer")
    (setf (message-raw-content m)
          (list (clawmacs::canonical-text-block "Final answer")))
    (clawmacs::put-message-metadata
     m
     :provider :openai-codex
     :reasoning-summary-mode "detailed")
    (is (equal '("Final answer")
               (clawmacs::message-display-line-strings m)))
    (is (equal '("Final answer"
                 ";; reasoning"
                 ";; no provider-supplied reasoning blocks captured")
               (clawmacs::message-display-line-strings
                m
                :show-reasoning-p t)))))

(test message-display-lines-include-metadata-when-enabled
  "Message metadata is display-only text hidden unless explicitly enabled."
  (let ((m (make-message :agent)))
    (clawmacs::set-message-text m "Final answer")
    (clawmacs::put-message-metadata
     m
     :agent "agent"
     :provider :openai-codex
     :model "gpt-5.4"
     :think-level "high"
     :reasoning-summary-mode "detailed"
     :stop-reason "end_turn"
     :content-block-count 2
     :tool-call-count 1
     :reasoning-block-count 1
     :input-tokens 2006
     :cached-input-tokens 1920
     :uncached-input-tokens 86
     :output-tokens 300
     :total-tokens 2306
     :cache-hit-rate 0.9571286)
    (is (equal '("Final answer")
               (clawmacs::message-display-line-strings m)))
    (let ((lines (clawmacs::message-display-line-strings
                  m
                  :show-metadata-p t)))
      (is (member ";; metadata" lines :test #'string=))
      (is (member ";; provider/model: openai-codex/gpt-5.4"
                  lines
                  :test #'string=))
      (is (member ";; reasoning-summary: detailed"
                  lines
                  :test #'string=))
      (is (member ";; stop-reason: end_turn" lines :test #'string=))
      (is (member ";; tokens: input=2006 cached=1920 uncached=86 output=300 total=2306 cache-hit=95.7%"
                  lines
                  :test #'string=))
      (is (= 11 (length lines))))
    (is (= 1 (clawmacs::message-visual-height m 80)))
    (is (= 12 (clawmacs::message-visual-height
              m 80
              :show-metadata-p t)))))

(test init-global-faces-registers-tool-highlight-faces
  "Global theme initialization includes dedicated tool-call/result faces."
  (clawmacs::init-global-faces)
  (dolist (name '(:tool-call :tool-call-paren :tool-call-keyword
                   :tool-call-string :tool-call-comment :tool-call-number
                   :tool-result :tool-result-paren :tool-result-keyword
                   :tool-result-string :tool-result-comment :tool-result-number
                   :compaction-summary))
    (is (typep (clawmacs::global-face name) 'face))))

(test tool-displays-use-lisp-shaped-text
  "Tool call/result display strings are formatted for Lisp-oriented rendering."
  (let* ((tool-use '((:type . "tool_use")
                     (:id . "toolu_1")
                     (:name . "lisp_eval")
                     (:input . ((:code . "(+ 1 2)")
                                (:package . "CLAWMACS")))))
         (tool-call (clawmacs::format-tool-call-display tool-use))
         (tool-result (clawmacs::format-tool-result-display
                       "lisp_eval"
                       (clawmacs::lisp-data-string
                        (list :code "(+ 1 2)"
                              :result "3"
                              :values 1)))))
    (is (string= "(lisp_eval :code \"(+ 1 2)\" :package \"CLAWMACS\")"
                 tool-call))
    (is (search ";; lisp_eval" tool-result))
    (is (search "(+ 1 2)" tool-result))
    (is (search ";; => 1 value" tool-result))
    (is (search "3" tool-result))))

(test tool-line-display-spans-highlight-lisp-syntax
  "Tool line syntax spans distinguish punctuation, keywords, strings, and numbers."
  (let* ((spans (clawmacs::tool-line-display-spans
                 "(lisp_eval :code \"(+ 1 2)\" :limit 7)"
                 :tool-call))
         (faces (mapcar #'car spans))
         (texts (mapcar #'cdr spans)))
    (is (member :tool-call-paren faces))
    (is (member :tool-call-keyword faces))
    (is (member :tool-call-string faces))
    (is (member :tool-call-number faces))
    (is (equal "(" (first texts)))
    (is (search "\"(+ 1 2)\"" (format nil "~{~A~}" texts)))))

;;; --------------------------------------------------------------------------
;;; Buffer Selector Rendering Tests
;;; --------------------------------------------------------------------------

(test format-selector-line-fits-width
  "format-selector-line output is exactly the given width."
  (let ((line (clawmacs::format-selector-line "▸ " "my-session" "coder" "idle" "5" 80)))
    (is (= 80 (length line)))
    (is (search "my-session" line))
    (is (search "coder" line))
    (is (search "idle" line))
    (is (search "5" line))))

(test format-selector-line-truncates-at-narrow-width
  "format-selector-line truncates to fit narrow terminals."
  (let ((line (clawmacs::format-selector-line "  " "very-long-session-name" "coder" "thinking" "12" 40)))
    (is (= 40 (length line)))))

;;; --------------------------------------------------------------------------
;;; Model Selector Rendering Tests
;;; --------------------------------------------------------------------------

(test format-model-selector-line-fits-width
  "format-model-selector-line output is exactly the given width."
  (let ((line (clawmacs::format-model-selector-line "▸ " "zai" "glm-5" 80)))
    (is (= 80 (length line)))
    (is (search "zai" line))
    (is (search "glm-5" line))))

(test format-model-selector-line-with-active-marker
  "format-model-selector-line shows the active marker."
  (let ((line (clawmacs::format-model-selector-line "▸*" "zai" "glm-5" 80)))
    (is (= 80 (length line)))
    (is (search "▸*" line))
    (is (search "zai" line))
    (is (search "glm-5" line))))

(test format-model-selector-line-truncates-at-narrow-width
  "format-model-selector-line truncates for narrow terminals."
  (let ((line (clawmacs::format-model-selector-line "  " "openai-codex" "codex-mini-latest" 30)))
    (is (= 30 (length line)))))

;;; --------------------------------------------------------------------------
;;; Think Selector Rendering Tests
;;; --------------------------------------------------------------------------

(test format-think-selector-line-fits-width
  "format-think-selector-line output is exactly the given width."
  (let ((line (clawmacs::format-think-selector-line "▸ " "high" 40)))
    (is (= 40 (length line)))
    (is (search "▸ " line))
    (is (search "high" line))))

(test format-think-selector-line-truncates-at-narrow-width
  "format-think-selector-line truncates for narrow terminals."
  (let ((line (clawmacs::format-think-selector-line "  " "default" 8)))
    (is (= 8 (length line)))))
