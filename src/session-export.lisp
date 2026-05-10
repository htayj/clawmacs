(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Session Export And Sharing
;;; --------------------------------------------------------------------------

(defstruct (session-share-handler
            (:constructor make-session-share-handler
                (&key name description function)))
  "Registered handler for sharing an exported session artifact."
  (name "" :type string :read-only t)
  (description "" :type string :read-only t)
  (function nil :type function :read-only t))

(defvar *session-share-handler-table* (make-hash-table :test #'equal)
  "Registry of share handlers keyed by normalized handler name.")

(defun normalize-session-share-handler-name (name)
  "Return NAME as a lowercase share-handler key."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (string name))))
    (unless (plusp (length trimmed))
      (error "Session share handler name must be non-empty."))
    (string-downcase trimmed)))

(defun register-session-share-handler (name function &key description)
  "Register FUNCTION as a named session share handler."
  (let* ((normalized (normalize-session-share-handler-name name))
         (handler (make-session-share-handler
                   :name normalized
                   :description (or description "")
                   :function function)))
    (setf (gethash normalized *session-share-handler-table*) handler)
    handler))

(defun session-export-default-html-path (session)
  "Return a default HTML export path for SESSION."
  (let* ((directory (merge-pathnames #P"exports/" (session-directory session)))
         (leaf (or (session-current-leaf-id session) "branch"))
         (basename (format nil "~A-~A-~D"
                           (session-safe-component
                            (session-display-name-or-name session))
                           leaf
                           (get-universal-time))))
    (merge-pathnames (make-pathname :name basename :type "html") directory)))

(defun normalize-session-export-path (session requested-path)
  "Return REQUESTED-PATH as an HTML pathname for SESSION."
  (let ((default-path (session-export-default-html-path session)))
    (cond
      ((null requested-path) default-path)
      (t
       (let* ((pathname (etypecase requested-path
                          (pathname requested-path)
                          (string (pathname requested-path))))
              (resolved (if (uiop:directory-pathname-p pathname)
                            (merge-pathnames (file-namestring default-path)
                                             pathname)
                            pathname)))
         (if (pathname-type resolved)
             resolved
             (make-pathname :type "html" :defaults resolved)))))))

(defun session-export-buffer-messages (buffer)
  "Return finalized messages visible in BUFFER's active branch."
  (loop :for msg := (buffer-first-message buffer) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buffer))))
        :collect msg))

(defun session-export-timestamp-string (timestamp)
  "Return TIMESTAMP as a stable local wall-clock string."
  (when timestamp
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time timestamp)
      (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
              year month date hour minute second))))

(defun session-export-css-token (value)
  "Return VALUE normalized for use as a CSS class token."
  (let ((text (string-downcase (string value))))
    (with-output-to-string (stream)
      (loop :for char :across text
            :do (write-char (if (or (alphanumericp char)
                                    (char= char #\-))
                                char
                                #\-)
                            stream)))))

(defstruct (session-export-image-reference
            (:constructor make-session-export-image-reference
                (&key path alt raw-text)))
  "Image reference parsed from a Markdown image line during session export."
  (path "" :type string :read-only t)
  (alt "" :type string :read-only t)
  (raw-text "" :type string :read-only t))

(defun session-export-trim-image-path (path)
  "Normalize a Markdown image PATH for HTML export."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) path)))
    (if (alexandria:starts-with-subseq "file://" trimmed)
        (subseq trimmed 7)
        trimmed)))

(defun session-export-parse-image-line (text)
  "Parse TEXT as a standalone Markdown image line, or return NIL."
  (let* ((trimmed (string-trim '(#\Space #\Tab) text))
         (len (length trimmed)))
    (when (and (>= len 5)
               (alexandria:starts-with-subseq "![" trimmed)
               (char= (char trimmed (1- len)) #\)))
      (let ((separator (search "](" trimmed :start2 2)))
        (when separator
          (let* ((alt (subseq trimmed 2 separator))
                 (path-start (+ separator 2))
                 (path-end (1- len))
                 (path (and (< path-start path-end)
                            (session-export-trim-image-path
                             (subseq trimmed path-start path-end)))))
            (when (and path (plusp (length path)))
              (make-session-export-image-reference
               :path path
               :alt alt
               :raw-text text))))))))

(defun session-export-reasoning-lines (message visible-text)
  "Return optional reasoning sidecar lines for MESSAGE."
  (let ((blocks (and (message-raw-content message)
                     (content-reasoning-blocks (message-raw-content message))))
        (visible (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (or visible-text "")))
        (lines nil))
    (dolist (reasoning blocks)
      (let ((text (or reasoning "")))
        (unless (or (blank-string-p text)
                    (string= visible
                             (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          text)))
          (push ";; reasoning" lines)
          (dolist (line (split-string-by-newline text))
            (push line lines)))))
    (cond
      (lines (nreverse lines))
      ((message-metadata-value
        (message-metadata message)
        :reasoning-summary-mode)
       (list ";; reasoning"
             ";; no provider-supplied reasoning blocks captured"))
      (t nil))))

(defun session-export-metadata-lines (metadata)
  "Return optional metadata sidecar lines for METADATA."
  (when metadata
    (let ((agent (message-metadata-value metadata :agent))
          (provider (message-metadata-value metadata :provider))
          (model (message-metadata-value metadata :model))
          (think-level (message-metadata-value metadata :think-level))
          (reasoning-summary-mode
            (message-metadata-value metadata :reasoning-summary-mode))
          (stop-reason (message-metadata-value metadata :stop-reason))
          (content-block-count
            (message-metadata-value metadata :content-block-count))
          (tool-call-count
            (message-metadata-value metadata :tool-call-count))
          (reasoning-block-count
            (message-metadata-value metadata :reasoning-block-count))
          (usage-line
            (format-token-usage-summary
             (token-usage-from-metadata metadata))))
      (remove nil
              (list ";; metadata"
                    (and agent (format nil ";; agent: ~A" agent))
                    (and provider model
                         (format nil ";; provider/model: ~(~A~)/~A"
                                 provider model))
                    (format nil ";; think: ~A" (or think-level "default"))
                    (and reasoning-summary-mode
                         (format nil ";; reasoning-summary: ~A"
                                 reasoning-summary-mode))
                    (and stop-reason
                         (format nil ";; stop-reason: ~A" stop-reason))
                    (and content-block-count
                         (format nil ";; content-blocks: ~D"
                                 content-block-count))
                    (and tool-call-count
                         (format nil ";; tool-calls: ~D" tool-call-count))
                    (and reasoning-block-count
                         (format nil ";; reasoning-blocks: ~D"
                                 reasoning-block-count))
                    (and usage-line
                         (format nil ";; ~A" usage-line)))))))

(defun session-export-message-lines
    (message &key show-reasoning-p show-metadata-p)
  "Return MESSAGE lines plus optional reasoning and metadata sidecars."
  (let* ((body-lines (split-string-by-newline (message-text message)))
         (visible-text (join-lines-with-newlines body-lines))
         (sidecar-lines
           (append (and show-reasoning-p
                        (session-export-reasoning-lines message visible-text))
                   (and show-metadata-p
                        (session-export-metadata-lines
                         (message-metadata message))))))
    (if sidecar-lines
        (append (if (and (= (length body-lines) 1)
                         (blank-string-p (first body-lines)))
                    nil
                    body-lines)
                sidecar-lines)
        body-lines)))

(defun session-export-message-blocks
    (message &key show-reasoning-p show-metadata-p)
  "Return text/image blocks for MESSAGE during HTML export."
  (loop :for text :in (session-export-message-lines
                       message
                       :show-reasoning-p show-reasoning-p
                       :show-metadata-p show-metadata-p)
        :for image := (session-export-parse-image-line text)
        :collect (if image
                     (list :type :image :reference image)
                     (list :type :text :text text))))

(defun session-export-image-block-html (reference)
  "Return HTML for one inline image REFERENCE."
  (let* ((path (session-export-image-reference-path reference))
         (alt (session-export-image-reference-alt reference))
         (caption (if (blank-string-p alt) path alt)))
    (format nil
            "<figure class=\"message-image\"><img src=\"~A\" alt=\"~A\" /><figcaption>~A</figcaption></figure>"
            (html-escape path)
            (html-escape alt)
            (html-escape caption))))

(defun session-export-message-body-html
    (message &key show-reasoning-p show-metadata-p)
  "Return HTML for MESSAGE body blocks."
  (with-output-to-string (stream)
    (let ((pending-lines nil))
      (labels ((flush-pending-lines ()
                 (when pending-lines
                   (format stream
                           "<pre class=\"message-text\">~A</pre>"
                           (html-escape
                            (join-lines-with-newlines
                             (nreverse pending-lines))))
                   (setf pending-lines nil))))
        (dolist (block (session-export-message-blocks
                        message
                        :show-reasoning-p show-reasoning-p
                        :show-metadata-p show-metadata-p))
          (ecase (getf block :type)
            (:text
             (push (getf block :text) pending-lines))
            (:image
             (flush-pending-lines)
             (write-string
              (session-export-image-block-html (getf block :reference))
              stream))))
        (flush-pending-lines)))))

(defun session-export-message-html
    (message &key show-reasoning-p show-metadata-p)
  "Return HTML for one exported MESSAGE."
  (let* ((sender (string-downcase (symbol-name (message-sender message))))
         (timestamp (session-export-timestamp-string
                     (message-timestamp message))))
    (format nil
            "<section class=\"message sender-~A\"><header class=\"message-header\"><span class=\"sender\">~A</span>~@[<span class=\"timestamp\">~A</span>~]</header><div class=\"message-body\">~A</div></section>"
            (session-export-css-token sender)
            (html-escape sender)
            timestamp
            (session-export-message-body-html
             message
             :show-reasoning-p show-reasoning-p
             :show-metadata-p show-metadata-p))))

(defun session-export-page-html
    (buffer session &key show-reasoning-p show-metadata-p)
  "Return a standalone HTML export for BUFFER's active branch."
  (let* ((title (session-display-name-or-name session))
         (summary (session-summary-string session :buffer buffer))
         (messages (session-export-buffer-messages buffer)))
    (with-output-to-string (stream)
      (format stream "<!doctype html><html><head><meta charset=\"utf-8\" />")
      (format stream "<title>~A</title>" (html-escape title))
      (write-string
       "<style>
body { margin: 0; font-family: sans-serif; background: #111; color: #f2f2f2; }
main { max-width: 960px; margin: 0 auto; padding: 24px; }
h1 { margin: 0 0 8px; font-size: 28px; }
p.meta { margin: 0 0 16px; color: #c7c7c7; }
details.session-summary { margin: 0 0 24px; }
details.session-summary pre { white-space: pre-wrap; overflow-wrap: anywhere; }
section.message { margin: 0 0 16px; padding: 12px; border: 1px solid #333; border-radius: 6px; background: #181818; }
.message-header { display: flex; gap: 12px; align-items: baseline; margin: 0 0 8px; font-size: 14px; color: #c7c7c7; }
.message-header .sender { font-weight: 700; color: #ffffff; text-transform: lowercase; }
.message-body { display: block; }
.message-text { margin: 0 0 10px; white-space: pre-wrap; overflow-wrap: anywhere; }
.message-image { margin: 0 0 10px; }
.message-image img { max-width: 100%; height: auto; display: block; border-radius: 6px; }
.message-image figcaption { margin-top: 6px; color: #c7c7c7; font-size: 14px; }
</style>"
       stream)
      (write-string "</head><body><main>" stream)
      (format stream "<h1>~A</h1>" (html-escape title))
      (format stream
              "<p class=\"meta\">Working directory: ~A<br />Generated: ~A<br />Reasoning: ~A<br />Metadata: ~A</p>"
              (html-escape
               (session-path-string (buffer-working-directory buffer)))
              (html-escape
               (or (session-export-timestamp-string (get-universal-time))
                   "unknown"))
              (if show-reasoning-p "shown" "hidden")
              (if show-metadata-p "shown" "hidden"))
      (format stream
              "<details class=\"session-summary\"><summary>Session summary</summary><pre>~A</pre></details>"
              (html-escape summary))
      (dolist (message messages)
        (write-string
         (session-export-message-html
          message
          :show-reasoning-p show-reasoning-p
          :show-metadata-p show-metadata-p)
         stream))
      (write-string "</main></body></html>" stream))))

(defun export-buffer-session-html
    (buffer &key path
                  (show-reasoning-p nil show-reasoning-supplied-p)
                  (show-metadata-p nil show-metadata-supplied-p))
  "Write BUFFER's active branch as a standalone HTML export.
Returns a plist describing the export."
  (when (or (scratch-buffer-p buffer)
            (document-buffer-p buffer))
    (error "Buffer ~A does not have an exportable durable session."
           (buffer-name buffer)))
  (let* ((session (or (buffer-session buffer)
                      (ensure-buffer-session buffer)))
         (resolved-path (normalize-session-export-path session path))
         (resolved-show-reasoning-p
           (if show-reasoning-supplied-p
               show-reasoning-p
               (buffer-show-reasoning-p buffer)))
         (resolved-show-metadata-p
           (if show-metadata-supplied-p
               show-metadata-p
               (buffer-show-metadata-p buffer)))
         (html (session-export-page-html
                buffer session
                :show-reasoning-p resolved-show-reasoning-p
                :show-metadata-p resolved-show-metadata-p)))
    (save-session buffer)
    (ensure-directories-exist resolved-path)
    (with-open-file (stream resolved-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string html stream))
    (list :path resolved-path
          :session session
          :show-reasoning-p resolved-show-reasoning-p
          :show-metadata-p resolved-show-metadata-p)))

(defun call-session-share-hook-handler (buffer export-info)
  "Run *SESSION-SHARE-HOOK* until one hook returns a non-nil share result."
  (dolist (hook *session-share-hook*)
    (let ((result (call-hook-safely hook '*session-share-hook*
                                    buffer export-info)))
      (when result
        (return-from call-session-share-hook-handler result))))
  (error "No *session-share-hook* function returned a share result."))

(defun session-share-local-copy-handler (_buffer export-info)
  "Copy EXPORT-INFO's file into the session's local shares directory."
  (declare (ignore _buffer))
  (let* ((source (pathname (getf export-info :path)))
         (session (getf export-info :session))
         (directory (merge-pathnames #P"shares/" (session-directory session)))
         (target (merge-pathnames (file-namestring source) directory)))
    (when (probe-file target)
      (setf target
            (merge-pathnames
             (make-pathname
              :name (format nil "~A-~D"
                            (or (pathname-name source) "share")
                            (get-universal-time))
              :type (or (pathname-type source) "html"))
             directory)))
    (ensure-directories-exist target)
    (uiop:copy-file source target)
    target))

(defun ensure-default-session-share-handlers ()
  "Register built-in session share handlers when absent."
  (unless (gethash "local-copy" *session-share-handler-table*)
    (register-session-share-handler
     "local-copy"
     #'session-share-local-copy-handler
     :description "Copy the exported HTML into the session shares directory."))
  (unless (gethash "hook" *session-share-handler-table*)
    (register-session-share-handler
     "hook"
     #'call-session-share-hook-handler
     :description "Delegate sharing to functions on *session-share-hook*.")))

(defun find-session-share-handler (name)
  "Return the registered session share handler NAME, or NIL."
  (ensure-default-session-share-handlers)
  (gethash (normalize-session-share-handler-name
            (or name "local-copy"))
           *session-share-handler-table*))

(defun list-session-share-handlers ()
  "Return registered session share handlers sorted by name."
  (ensure-default-session-share-handlers)
  (let ((handlers nil))
    (maphash (lambda (_name handler)
               (declare (ignore _name))
               (push handler handlers))
             *session-share-handler-table*)
    (sort handlers #'string< :key #'session-share-handler-name)))

(defun share-session-export (buffer export-info &key (handler "local-copy"))
  "Invoke HANDLER for EXPORT-INFO and return a plist summary."
  (let ((resolved (find-session-share-handler handler)))
    (unless resolved
      (error "Unknown session share handler: ~A" handler))
    (list :handler (session-share-handler-name resolved)
          :result (funcall (session-share-handler-function resolved)
                           buffer export-info))))
