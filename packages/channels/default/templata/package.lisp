(in-package :clawmacs)

(defun templata-find-model-entry (buffer raw-name)
  "Return the unique model selector entry matching RAW-NAME, or NIL."
  (let* ((query (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                              (or raw-name ""))))
         (entries (available-models-for-selector buffer))
         (matches
           (remove-if-not
            (lambda (entry)
              (let ((display (string-downcase
                              (model-selector-display
                               (getf entry :provider)
                               (getf entry :model))))
                    (model (string-downcase (or (getf entry :model) ""))))
                (or (string= query display)
                    (string= query model))))
            entries)))
    (when (= 1 (length matches))
      (first matches))))

(defun templata-resolve-session-name (raw-name)
  "Return the saved session name matching RAW-NAME exactly or by unique prefix."
  (let* ((query (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (or raw-name "")))
         (sessions (or (list-saved-sessions) nil)))
    (cond
      ((zerop (length query)) nil)
      ((member query sessions :test #'string=) query)
      (t
       (let ((matches (remove-if-not
                       (lambda (name)
                         (and (>= (length name) (length query))
                              (string= query name
                                       :end2 (length query))))
                       sessions)))
         (when (= 1 (length matches))
           (first matches)))))))

(defun templata-slash-model (buffer args input-text)
  "Handle /model slash commands."
  (declare (ignore input-text))
  (if (null args)
      (minibuffer-select-model-command buffer)
      (let ((entry (templata-find-model-entry
                    buffer
                    (format nil "~{~A~^ ~}" args))))
        (if entry
            (multiple-value-bind (think-status think-level)
                (apply-buffer-model-selection buffer
                                              (getf entry :provider)
                                              (getf entry :model))
              (record-model-selection-history (model-selector-display
                                               (getf entry :provider)
                                               (getf entry :model)))
              (insert-model-selection-message buffer
                                              (getf entry :provider)
                                              (getf entry :model)
                                              think-status
                                              think-level))
            (buffer-insert-system-message
             buffer
             (format nil "[No unique model match for ~S. Use /model with no arguments to pick from the selector.]"
                     (format nil "~{~A~^ ~}" args)))))))

(defun templata-slash-session (buffer args input-text)
  "Handle /session slash commands."
  (declare (ignore args input-text))
  (session-tree-command buffer))

(defun templata-slash-resume (buffer args input-text)
  "Handle /resume slash commands."
  (declare (ignore input-text))
  (if (null args)
      (load-session-command buffer)
      (let* ((raw-name (format nil "~{~A~^ ~}" args))
             (session-name (templata-resolve-session-name raw-name)))
        (cond
          ((null session-name)
           (buffer-insert-system-message
            buffer
            "[Use /resume SESSION-NAME to load a saved session by exact name or unique prefix.]"))
          (t
           (let ((loaded (load-session session-name)))
             (if loaded
                 (switch-to-buffer loaded)
                 (buffer-insert-system-message
                  buffer
                  (format nil "[No saved session named ~A.]" session-name)))))))))

(defun templata-slash-new (buffer args input-text)
  "Handle /new slash commands."
  (declare (ignore buffer args input-text))
  (new-buffer-command nil))

(defun templata-slash-export (buffer args input-text)
  "Handle /export slash commands."
  (declare (ignore args input-text))
  (let ((path (save-session buffer)))
    (buffer-insert-system-message
     buffer
     (format nil "[Session saved to ~A]" path))
    path))

(defun templata-slash-reload (buffer args input-text)
  "Handle /reload slash commands."
  (declare (ignore args input-text))
  (reload-skills)
  (reload-package-channels)
  (reload-active-packages :buffer buffer)
  (buffer-insert-system-message
   buffer
   "[Reloaded skills, package manifests, and on-disk prompt templates.]"))
