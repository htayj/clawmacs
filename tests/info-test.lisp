(in-package :clawmacs/tests)

(in-suite info-suite)

(defun info-test-scenario ()
  "Return an installed manual plus a useful non-Top node for Info tests."
  (or (loop :for manual :in '("bash" "coreutils" "make" "readline"
                              "git" "sbcl" "asdf" "find" "grep")
            :for location := (clawmacs::info-location-for-manual manual :node "Top")
            :for document := (clawmacs::info-fetch-location location)
            :for next-target := (and (not (clawmacs::info-document-error-p document))
                                     (clawmacs::info-document-next-target document))
            :when (and next-target
                       (string-equal manual
                                     (clawmacs::info-location-manual next-target)))
              :return (list :manual manual
                            :next-node (clawmacs::info-location-node next-target)))
      (error "No suitable installed system Info manual was found for tests.")))

(defun info-test-followable-link-index (info)
  "Return an in-manual non-nav link index for INFO, or NIL."
  (let* ((state (clawmacs::info-buffer-state info))
         (document (clawmacs::info-state-document state))
         (manual (clawmacs::info-document-manual document)))
    (position-if
     (lambda (link)
       (and (not (eq :nav (clawmacs::info-link-kind link)))
            (string-equal manual
                          (clawmacs::info-location-manual
                           (clawmacs::info-link-target link)))
            (not (string= "Top"
                          (clawmacs::info-location-node
                           (clawmacs::info-link-target link))))))
     (clawmacs::info-document-links document))))

(test info-program-is-available
  "The manual browser can discover system Info manuals."
  (is (clawmacs::info-program-available-p)))

(test info-directory-command-opens-info-buffer
  "C-h i style directory browsing opens a dedicated Info buffer."
  (with-interactive-command-test-buffer (buf)
    (clawmacs::info-directory-command buf)
    (let* ((info (current-buffer))
           (state (clawmacs::info-buffer-state info))
           (document (clawmacs::info-state-document state))
           (text (clawmacs::info-buffer-text info)))
      (is (clawmacs::info-buffer-p info))
      (is (string= "*info*" (buffer-name info)))
      (is (string= "dir" (clawmacs::info-document-manual document)))
      (is (string= "Top" (clawmacs::info-document-node document)))
      (is (search "top of the INFO tree" text :test #'char-equal))
      (is (plusp (length (clawmacs::info-document-links document)))))))

(test info-open-manual-command-loads-system-manual
  "The manual-open command resolves a system Info manual and node."
  (let ((scenario (info-test-scenario)))
    (with-interactive-command-test-buffer (buf)
      (clawmacs::info-open-manual-command
       buf
       (format nil "~A Top" (getf scenario :manual)))
      (let* ((info (current-buffer))
             (document (clawmacs::info-state-document
                        (clawmacs::info-buffer-state info)))
             (text (clawmacs::info-buffer-text info)))
        (is (clawmacs::info-buffer-p info))
        (is (string= (getf scenario :manual)
                     (clawmacs::info-document-manual document)))
        (is (string= "Top" (clawmacs::info-document-node document)))
        (is (not (clawmacs::info-document-error-p document)))
        (is (plusp (length text)))))))

(test clawmacs-manual-command-opens-clawmacs-manual-buffer
  "The Clawmacs manual command resolves the local manual or the placeholder."
  (with-interactive-command-test-buffer (buf)
    (clawmacs::clawmacs-manual-command buf)
    (let* ((info (current-buffer))
           (state (clawmacs::info-buffer-state info))
           (location (clawmacs::info-state-location state))
           (text (clawmacs::info-buffer-text info)))
      (is (clawmacs::info-buffer-p info))
      (is (string= "clawmacs" (clawmacs::info-location-manual location)))
      (is (search "Clawmacs" text :test #'char-equal)))))

(test info-goto-follow-and-history-work
  "Info buffers can move to a node, follow a selected link, and walk history."
  (let ((scenario (info-test-scenario)))
    (with-interactive-command-test-buffer (buf)
      (clawmacs::info-open-manual-command
       buf
       (format nil "~A Top" (getf scenario :manual)))
      (let ((info (current-buffer)))
        (is (string= "Top"
                     (clawmacs::info-location-node
                      (clawmacs::info-state-location
                       (clawmacs::info-buffer-state info)))))
        (is (eq :redraw
                (clawmacs::info-goto-node-command info
                                                  (getf scenario :next-node))))
        (is (string= (getf scenario :next-node)
                     (clawmacs::info-location-node
                      (clawmacs::info-state-location
                       (clawmacs::info-buffer-state info)))))
        (is (eq :redraw (clawmacs::info-go-back info)))
        (is (string= "Top"
                     (clawmacs::info-location-node
                      (clawmacs::info-state-location
                       (clawmacs::info-buffer-state info)))))
        (is (eq :redraw (clawmacs::info-go-forward info)))
        (is (string= (getf scenario :next-node)
                     (clawmacs::info-location-node
                      (clawmacs::info-state-location
                       (clawmacs::info-buffer-state info)))))
        (is (eq :redraw (clawmacs::info-go-back info)))
        (let ((follow-index (info-test-followable-link-index info)))
          (is (not (null follow-index)))
          (setf (clawmacs::info-state-selected-link-index
                 (clawmacs::info-buffer-state info))
                follow-index))
        (is (eq :redraw (clawmacs::info-follow-selected-link info)))
        (is (not (string= "Top"
                          (clawmacs::info-location-node
                           (clawmacs::info-state-location
                            (clawmacs::info-buffer-state info))))))))))

(test info-buffer-state-round-trips-through-serializer
  "Info buffer state serializer preserves location and history."
  (let ((scenario (info-test-scenario)))
    (with-interactive-command-test-buffer (buf)
      (clawmacs::info-open-manual-command
       buf
       (format nil "~A Top" (getf scenario :manual)))
      (let ((info (current-buffer)))
        (clawmacs::info-goto-node-command info (getf scenario :next-node))
        (let* ((saved (clawmacs::info-serialize-buffer-state info))
               (restored (make-buffer "*info-restore*" :kind :info)))
          (clawmacs::info-restore-buffer-state restored saved)
          (let ((restored-state (clawmacs::info-buffer-state restored)))
            (is (string= (getf scenario :next-node)
                         (clawmacs::info-location-node
                          (clawmacs::info-state-location restored-state))))
            (is (= 1 (length (clawmacs::info-state-history restored-state))))))))))
