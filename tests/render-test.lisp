(in-package :clawmacs/tests)

(def-suite render-suite
  :description "Rendering helper tests"
  :in clawmacs-suite)

(in-suite render-suite)

(test format-modeline-basic
  "format-modeline produces a left/right aligned string."
  (let* ((buf (make-buffer "test:session" :agent-name "claude"
                           :working-directory #P"/tmp/")))
    (setf (buffer-token-count buf) 1000
          (buffer-context-limit buf) 200000
          (buffer-status buf) :idle)
    (let ((ml (clawmacs::format-modeline buf 60)))
      (is (= 60 (length ml)))
      (is (search "test:session" ml))
      (is (search "1000/200000" ml))
      (is (search "IDLE" ml)))))

(test calculate-input-height-minimum
  "Input height is at least 3 rows."
  (let ((buf (make-buffer "test")))
    (is (= 3 (clawmacs::calculate-input-height buf 30)))))

(test calculate-input-height-maximum
  "Input height is capped at (floor terminal-height 3)."
  (let ((buf (make-buffer "test")))
    (dotimes (i 20)
      (message-insert-newline (buffer-input-message buf)))
    (is (= 10 (clawmacs::calculate-input-height buf 30)))))
