(in-package :clawmacs/tests)

(in-suite clawmacs-suite)

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
