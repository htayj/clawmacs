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

(defun templata-parse-export-request (buffer args)
  "Return MODE, HANDLER, PATH, SHOW-REASONING-P, and SHOW-METADATA-P for /export."
  (let ((mode :export)
        (handler "local-copy")
        (handler-supplied-p nil)
        (path nil)
        (show-reasoning-p (buffer-show-reasoning-p buffer))
        (show-metadata-p (buffer-show-metadata-p buffer)))
    (dolist (arg args)
      (cond
        ((string= arg "share")
         (setf mode :share))
        ((string= arg "--reasoning")
         (setf show-reasoning-p t))
        ((string= arg "--no-reasoning")
         (setf show-reasoning-p nil))
        ((string= arg "--metadata")
         (setf show-metadata-p t))
        ((string= arg "--no-metadata")
         (setf show-metadata-p nil))
        ((and (eq mode :share)
              (not handler-supplied-p))
         (setf handler arg
               handler-supplied-p t))
        ((null path)
         (setf path arg))
        (t
         (error "Too many /export arguments: ~{~A~^ ~}" args))))
    (values mode handler path show-reasoning-p show-metadata-p)))

(defun templata-display-export-result (value)
  "Return VALUE as a concise user-facing export/share description."
  (typecase value
    (pathname (namestring value))
    (t (princ-to-string value))))

(defun templata-slash-export (buffer args input-text)
  "Handle /export slash commands."
  (declare (ignore input-text))
  (handler-case
      (multiple-value-bind (mode handler path show-reasoning-p show-metadata-p)
          (templata-parse-export-request buffer args)
        (let* ((export-info (export-buffer-session-html
                             buffer
                             :path path
                             :show-reasoning-p show-reasoning-p
                             :show-metadata-p show-metadata-p))
               (export-path (getf export-info :path))
               (share-info (and (eq mode :share)
                                (share-session-export
                                 buffer export-info :handler handler))))
          (buffer-insert-system-message
           buffer
           (if share-info
               (format nil "[Session exported to ~A and shared via ~A: ~A]"
                       export-path
                       (getf share-info :handler)
                       (templata-display-export-result
                        (getf share-info :result)))
               (format nil "[Session exported to ~A]" export-path)))
          (if share-info
              (getf share-info :result)
              export-path)))
    (error (condition)
      (buffer-insert-system-message
       buffer
       (format nil "[Export failed: ~A]" condition))
      nil)))

(defun templata-slash-reload (buffer args input-text)
  "Handle /reload slash commands."
  (declare (ignore args input-text))
  (call-with-package-runtime-maintenance
   (lambda ()
     (reload-skills)
     (reload-package-channels)
     (reload-active-packages :buffer buffer))
   :operation "reload skills and active packages"
   :buffer buffer)
  (buffer-insert-system-message
   buffer
   "[Reloaded skills, package manifests, and on-disk prompt templates.]"))
