(in-package :clawmacs/tests)

(def-suite render-suite
  :description "Rendering helper tests"
  :in clawmacs-suite)

(in-suite render-suite)


(test modeline-field-data-preserves-presentation-metadata
  "Native McCLIM modeline rendering can present semantic fields."
  (let ((directory #P"/tmp/")
        (buf (make-buffer "test:session" :agent-name "coder"
                          :working-directory #P"/tmp/")))
    (let* ((fields (clawmacs::modeline-field-data
                    buf :major-mode "chat"
                    :provider-model "zai/test-model"))
           (buffer-field (find :buffer fields :key (lambda (field)
                                                     (getf field :id))))
           (directory-field (find :directory fields :key (lambda (field)
                                                           (getf field :id)))))
      (is (eq buf (getf buffer-field :object)))
      (is (eq 'clawmacs::buffer-ref
              (getf buffer-field :presentation-type)))
      (is (equal directory (getf directory-field :object)))
      (is (eq 'pathname
              (getf directory-field :presentation-type)))
      (is (find :provider-model fields :key (lambda (field)
                                              (getf field :id)))))))


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


(test who-line-field-data-preserves-presentation-metadata
  "Native McCLIM who-line rendering can present semantic status fields."
  (let ((*minibuffer-active* nil)
        (*buffer-selector-active* nil)
        (*model-selector-active* nil)
        (*think-selector-active* nil)
        (*customize-face-state* nil)
        (*openai-oauth-pending* nil)
        (*deny-message-mode* nil)
        (clawmacs::*buffer-ring* nil))
    (let* ((directory #P"/tmp/")
           (buf (make-buffer "s1" :agent-name "agent"
                             :working-directory directory))
           (fields (clawmacs::who-line-field-data buf))
           (buffer-field (find :buffer fields :key (lambda (field)
                                                     (getf field :id))))
           (directory-field (find :directory fields :key (lambda (field)
                                                           (getf field :id)))))
      (is (eq buf (getf buffer-field :object)))
      (is (eq 'clawmacs::buffer-ref
              (getf buffer-field :presentation-type)))
      (is (equal directory (getf directory-field :object)))
      (is (eq 'pathname
              (getf directory-field :presentation-type))))))


(test calculate-input-height-minimum
  "Input height is at least 3 rows."
  (let ((buf (make-buffer "test")))
    ;; width=80 so wrapping doesn't affect a 1-line empty message
    (is (= 3 (clawmacs::calculate-input-height buf 30 80)))))

(test calculate-input-height-maximum
  "Input height is capped at (floor terminal-height 2) for normal chat input."
  (let ((buf (make-buffer "test")))
    (dotimes (i 20)
      (message-insert-newline (buffer-input-message buf)))
    ;; 21 lines, terminal height 30, max = 15
    (is (= 15 (clawmacs::calculate-input-height buf 30 80)))))


(test mcclim-desired-input-pane-rows-keep-chat-input-stable
  "Chat compose uses a stable Drei pane height."
  (let ((buf (make-buffer "chat-wrap")))
    (set-message-text (buffer-input-message buf)
                      (make-string 120 :initial-element #\x))
    (is (= clawmacs::*mcclim-chat-input-pane-rows*
           (clawmacs::mcclim-desired-input-pane-rows buf 24 20)))))

(test mcclim-desired-input-pane-rows-keep-document-input-stable
  "Document editors use a stable shared Drei pane height."
  (let ((buf (make-buffer "scratch" :kind :scratch)))
    (set-message-text (buffer-input-message buf)
                      (make-string 120 :initial-element #\x))
    (is (= clawmacs::*mcclim-document-input-pane-rows*
           (clawmacs::mcclim-desired-input-pane-rows buf 24 20)))))

(test mcclim-desired-input-pane-rows-hide-noneditable-special-buffers
  "Read-only and presentation-driven special buffers do not reserve a Drei pane."
  (dolist (kind '(:help :info :customize :font-editor))
    (let ((buf (make-buffer (string-downcase (symbol-name kind)) :kind kind)))
      (is (= 0 (clawmacs::mcclim-desired-input-pane-rows buf 24 20))))))



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

;;; --------------------------------------------------------------------------
;;; Logical Line Height Tests
;;; --------------------------------------------------------------------------

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

(test message-visual-height-with-long-line
  "Long physical lines remain one logical CLIM stream line."
  (let ((m (make-message :user)))
    (dotimes (i 150)
      (message-insert-char m #\x))
    (is (= 1 (clawmacs::message-visual-height m 80)))))

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
    (is (= 11 (clawmacs::message-visual-height
              m 80
              :show-metadata-p t)))))

(test message-display-blocks-detect-markdown-image-lines
  "Standalone Markdown image lines become display-only image blocks."
  (let ((m (make-message :agent)))
    (clawmacs::set-message-text
     m
     (format nil "Here is the image:~%![Probe image](screenshots/mcclim/probe.png)"))
    (let ((blocks (clawmacs::message-display-blocks m)))
      (is (= 2 (length blocks)))
      (is (eq :text (getf (first blocks) :type)))
      (is (eq :image (getf (second blocks) :type)))
      (let ((reference (getf (second blocks) :reference)))
        (is (string= "Probe image"
                     (clawmacs::display-image-reference-alt reference)))
        (is (string= "screenshots/mcclim/probe.png"
                     (clawmacs::display-image-reference-path reference)))))))

(test init-global-faces-registers-tool-highlight-faces
  "Global theme initialization includes dedicated tool-call/result faces."
  (clawmacs::init-global-faces)
  (dolist (name '(:tool-call :tool-call-paren :tool-call-keyword
                   :tool-call-string :tool-call-comment :tool-call-number
                   :tool-result :tool-result-paren :tool-result-keyword
                   :tool-result-string :tool-result-comment :tool-result-number
                   :compaction-summary))
    (is (typep (clawmacs::global-face name) 'drawing-style))))

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

(test mcclim-rendered-message-presentation-type-detects-tool-shapes
  "Transcript messages use dedicated presentation types for tool calls/results."
  (let ((chat (make-message :agent))
        (tool-call (make-message :agent))
        (tool-result (make-message :tool-result)))
    (setf (message-raw-content tool-call)
          '(((:type . "tool_use")
             (:id . "toolu_1")
             (:name . "lisp_eval")
             (:input . ((:code . "(+ 1 2)"))))))
    (is (eq 'clawmacs::chat-message
            (clawmacs::mcclim-rendered-message-presentation-type chat)))
    (is (eq 'clawmacs::tool-call
            (clawmacs::mcclim-rendered-message-presentation-type tool-call)))
    (is (eq 'clawmacs::tool-result
            (clawmacs::mcclim-rendered-message-presentation-type tool-result)))))
