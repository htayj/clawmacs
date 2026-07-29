(in-package :rplaca)

(defun media-tool-argument (args &rest names)
  "Return values VALUE and SUPPLIED-P for the first argument in NAMES."
  (dolist (name names (values nil nil))
    (multiple-value-bind (value supplied-p)
        (tool-argument-value args name)
      (when supplied-p
        (return-from media-tool-argument (values value t))))))

(defun media-tool-string (value field-name &key allow-nil)
  "Normalize VALUE as a media tool string field."
  (cond
    ((null value)
     (if allow-nil nil (error "~A is required." field-name)))
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (if (plusp (length trimmed))
           trimmed
           (if allow-nil nil (error "~A must be a non-empty string." field-name)))))
    (t (error "~A must be a string, got ~S." field-name value))))

(defun media-tool-referenced-image-paths (args)
  "Read optional reference image paths; the media core validates them."
  (multiple-value-bind (value supplied-p)
      (media-tool-argument args :referenced-image-paths "referenced_image_paths")
    (unless supplied-p
      (return-from media-tool-referenced-image-paths nil))
    value))

(defun media-tool-reject-provider-selection (args)
  "Reject transport, provider, and destination selectors from agent tool calls."
  (dolist (field '(:provider "provider" :provider-id "provider_id"
                   :model "model" :model-id "model_id"
                   :output-path "output_path" :output-directory "output_directory"))
    (multiple-value-bind (_value supplied-p)
        (tool-argument-value args field)
      (declare (ignore _value))
      (when supplied-p
        (error "media_generate_image does not accept ~A; media provider selection is user configuration."
               field)))))

(defun media-tool-current-buffer ()
  "Return the buffer whose Artifactum session owns generated media."
  (or *current-tool-buffer*
      (current-buffer)
      (error "media_generate_image requires a current buffer.")))

(defun media-generate-image-tool (args)
  "Generate one image through the user-selected media provider."
  (media-tool-reject-provider-selection args)
  (let* ((prompt (media-tool-string
                  (tool-arg args :prompt "prompt") "prompt"))
         (paths (media-tool-referenced-image-paths args))
         (request (make-media-generation-request
                   :image prompt :referenced-image-paths paths))
         ;; Publish the operation before START-FN runs, so an interactive
         ;; cancellation arriving at the start boundary can prevent billing.
         (operation (begin-media-operation request)))
    (register-current-tool-cancellation-function
     (lambda ()
       (cancel-media-operation operation)))
    (run-media-operation operation)
    (when (eq (media-operation-status operation) :succeeded)
      (persist-media-operation-assets (media-tool-current-buffer) operation))
    (lisp-data-string
     (media-operation-data operation :include-artifacts-p t))))

(deftool media-generate-image-tool
  :name "media_generate_image"
  :description "Generate one image with the user-configured media provider. The provider and billing route are never selected by the model."
  :call-style :raw-args
  :execution :background
  :args ((prompt :type "string"
                 :description "Text prompt for the generated image.")
         (referenced-image-paths :type "array" :required nil
                                 :items ((:type . "string"))
                                 :description "Optional list of at most five existing absolute image file paths for reference or editing.")))

(register-package-prompt-section
 "media"
 "## Generated media with media

- Use `media_generate_image` to create one durable image through the media
  provider selected by the user. Do not request a provider, model, or output
  path: those remain user configuration and artifact storage decisions.
- Reference images must be existing absolute local paths; at most five are
  accepted. Generated outputs become Artifactum records in the active session.
- The current media package supports image generation. Its provider contract
  also supports asynchronous video jobs for future provider packages."
 :title "Generated media with media"
 :package "media")
