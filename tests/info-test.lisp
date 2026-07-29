(in-package :rplaca/tests)

(in-suite info-suite)

(defun info-test-scenario ()
  "Return an installed manual plus a useful non-Top node for Info tests."
  (or (loop :for manual :in '("bash" "coreutils" "make" "readline"
                              "git" "sbcl" "asdf" "find" "grep")
            :for location := (rplaca::info-location-for-manual manual :node "Top")
            :for document := (rplaca::info-fetch-location location)
            :for next-target := (and (not (rplaca::info-document-error-p document))
                                     (rplaca::info-document-next-target document))
            :when (and next-target
                       (string-equal manual
                                     (rplaca::info-location-manual next-target)))
              :return (list :manual manual
                            :next-node (rplaca::info-location-node next-target)))
      (error "No suitable installed system Info manual was found for tests.")))

(defun info-test-followable-link-index (info)
  "Return an in-manual non-nav link index for INFO, or NIL."
  (let* ((state (rplaca::info-buffer-state info))
         (document (rplaca::info-state-document state))
         (manual (rplaca::info-document-manual document)))
    (position-if
     (lambda (link)
       (and (not (eq :nav (rplaca::info-link-kind link)))
            (string-equal manual
                          (rplaca::info-location-manual
                           (rplaca::info-link-target link)))
            (not (string= "Top"
                          (rplaca::info-location-node
                           (rplaca::info-link-target link))))))
     (rplaca::info-document-links document))))

(test info-program-is-available
  "The manual browser can discover system Info manuals."
  (is (rplaca::info-program-available-p)))

(test info-directory-command-opens-info-buffer
  "C-h i style directory browsing opens a dedicated Info buffer."
  (with-interactive-command-test-buffer (buf)
    (rplaca::info-directory-command buf)
    (let* ((info (current-buffer))
           (state (rplaca::info-buffer-state info))
           (document (rplaca::info-state-document state))
           (text (rplaca::info-buffer-text info)))
      (is (rplaca::info-buffer-p info))
      (is (string= "*info*" (buffer-name info)))
      (is (string= "dir" (rplaca::info-document-manual document)))
      (is (string= "Top" (rplaca::info-document-node document)))
      (is (search "top of the INFO tree" text :test #'char-equal))
      (is (plusp (length (rplaca::info-document-links document)))))))

(test info-open-manual-command-loads-system-manual
  "The manual-open command resolves a system Info manual and node."
  (let ((scenario (info-test-scenario)))
    (with-interactive-command-test-buffer (buf)
      (rplaca::info-open-manual-command
       buf
       (format nil "~A Top" (getf scenario :manual)))
      (let* ((info (current-buffer))
             (document (rplaca::info-state-document
                        (rplaca::info-buffer-state info)))
             (text (rplaca::info-buffer-text info)))
        (is (rplaca::info-buffer-p info))
        (is (string= (getf scenario :manual)
                     (rplaca::info-document-manual document)))
        (is (string= "Top" (rplaca::info-document-node document)))
        (is (not (rplaca::info-document-error-p document)))
        (is (plusp (length text)))))))

(test rplaca-manual-command-opens-rplaca-manual-buffer
  "The RPLACA manual command resolves the local manual or the placeholder."
  (with-interactive-command-test-buffer (buf)
    (rplaca::rplaca-manual-command buf)
    (let* ((info (current-buffer))
           (state (rplaca::info-buffer-state info))
           (location (rplaca::info-state-location state))
           (text (rplaca::info-buffer-text info)))
      (is (rplaca::info-buffer-p info))
      (is (string= "rplaca" (rplaca::info-location-manual location)))
      (is (search "RPLACA" text :test #'char-equal)))))

(test info-goto-follow-and-history-work
  "Info buffers can move to a node, follow a selected link, and walk history."
  (let ((scenario (info-test-scenario)))
    (with-interactive-command-test-buffer (buf)
      (rplaca::info-open-manual-command
       buf
       (format nil "~A Top" (getf scenario :manual)))
      (let ((info (current-buffer)))
        (is (string= "Top"
                     (rplaca::info-location-node
                      (rplaca::info-state-location
                       (rplaca::info-buffer-state info)))))
        (is (eq :redraw
                (rplaca::info-goto-node-command info
                                                  (getf scenario :next-node))))
        (is (string= (getf scenario :next-node)
                     (rplaca::info-location-node
                      (rplaca::info-state-location
                       (rplaca::info-buffer-state info)))))
        (is (eq :redraw (rplaca::info-go-back info)))
        (is (string= "Top"
                     (rplaca::info-location-node
                      (rplaca::info-state-location
                       (rplaca::info-buffer-state info)))))
        (is (eq :redraw (rplaca::info-go-forward info)))
        (is (string= (getf scenario :next-node)
                     (rplaca::info-location-node
                      (rplaca::info-state-location
                       (rplaca::info-buffer-state info)))))
        (is (eq :redraw (rplaca::info-go-back info)))
        (let ((follow-index (info-test-followable-link-index info)))
          (is (not (null follow-index)))
          (setf (rplaca::info-state-selected-link-index
                 (rplaca::info-buffer-state info))
                follow-index))
        (is (eq :redraw (rplaca::info-follow-selected-link info)))
        (is (not (string= "Top"
                          (rplaca::info-location-node
                           (rplaca::info-state-location
                            (rplaca::info-buffer-state info))))))))))

(test info-buffer-state-round-trips-through-serializer
  "Info buffer state serializer preserves location and history."
  (let ((scenario (info-test-scenario)))
    (with-interactive-command-test-buffer (buf)
      (rplaca::info-open-manual-command
       buf
       (format nil "~A Top" (getf scenario :manual)))
      (let ((info (current-buffer)))
        (rplaca::info-goto-node-command info (getf scenario :next-node))
        (let* ((saved (rplaca::info-serialize-buffer-state info))
               (restored (make-buffer "*info-restore*" :kind :info)))
          (rplaca::info-restore-buffer-state restored saved)
          (let ((restored-state (rplaca::info-buffer-state restored)))
            (is (string= (getf scenario :next-node)
                         (rplaca::info-location-node
                          (rplaca::info-state-location restored-state))))
            (is (= 1 (length (rplaca::info-state-history restored-state))))))))))
