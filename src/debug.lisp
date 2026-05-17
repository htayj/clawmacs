(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Debug Logging
;;; --------------------------------------------------------------------------

(defvar *debug-mode* nil
  "When non-nil, all API requests and responses are echoed into the chat
window as debug messages. Toggle interactively with C-c C-d.")

(defvar *debug-log-file* nil
  "When non-nil, a pathname to a file where detailed debug log entries are
appended. Set via the --debug-log <path> command-line flag. Unlike
*debug-mode* (which shows condensed info in the chat buffer), this logs
raw NDJSON lines, stream state transitions, CLI spawn args, stderr
output, and other low-level details useful for post-mortem debugging.")

(defvar *e2e-events-enabled-override* nil
  "Test override for structured GUI E2E event logging.")

(defvar *debug-event-sequence* 0
  "Monotonic sequence number for structured E2E debug events.")

(defvar *debug-event-lock* (bt:make-lock "clawmacs e2e debug events")
  "Lock guarding structured debug event sequence numbers.")

(defun env-truthy-p (name)
  "Return true when environment variable NAME is set to a truthy value."
  (let ((value (uiop:getenv name)))
    (and value
         (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     value)))
           (and (plusp (length trimmed))
                (not (member (string-downcase trimmed)
                             '("0" "false" "no" "off")
                             :test #'string=)))))))

(defun e2e-events-enabled-p ()
  "Return true when structured GUI E2E debug events should be written."
  (or *e2e-events-enabled-override*
      (env-truthy-p "CLAWMACS_GUI_E2E")
      (env-truthy-p "CLAWMACS_E2E_EVENTS")))

(defun debug-log (buf text)
  "Insert TEXT as a debug message in BUF when *debug-mode* is enabled.
Returns the message object, or nil if debug mode is off."
  (when *debug-mode*
    (buffer-insert-system-message buf text)))

(defun file-debug-log (category format-string &rest format-args)
  "Append a timestamped debug entry to *debug-log-file* when set.
CATEGORY is a short tag (e.g. \"cli-spawn\", \"ndjson\", \"stream-event\").
Thread-safe: opens, writes, and closes the file on each call."
  (when *debug-log-file*
    (ignore-errors
      (let ((line (format nil "[~A] [~A] ~?~%"
                          (format-timestamp (get-universal-time))
                          category
                          format-string format-args)))
        (with-open-file (f *debug-log-file*
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create
                           :external-format :utf-8)
          (write-string line f)
          (force-output f))))))

(defun debug-event-json-key (key)
  "Return KEY as a JSON keyword with hyphens encoded as underscores."
  (let* ((name (etypecase key
                 (keyword (symbol-name key))
                 (symbol (symbol-name key))
                 (string key)))
         (json-name (with-output-to-string (stream)
                      (loop :for char :across (string-upcase name)
                            :do (if (char= char #\-)
                                    (write-string "--" stream)
                                    (write-char char stream))))))
    (intern json-name :keyword)))

(defun sensitive-debug-event-key-p (key)
  "Return true when KEY names a sensitive field that should be redacted."
  (let ((name (string-downcase
               (etypecase key
                 (keyword (symbol-name key))
                 (symbol (symbol-name key))
                 (string key)))))
    (or (search "token" name)
        (search "secret" name)
        (search "password" name)
        (search "api-key" name)
        (search "api_key" name))))

(defun sanitize-debug-event-string (value)
  "Return VALUE bounded to a reasonable structured-event field size."
  (let ((limit 10000))
    (if (> (length value) limit)
        (format nil "~A…[truncated ~D chars]"
                (subseq value 0 limit)
                (- (length value) limit))
        value)))

(defun sanitize-debug-event-value (value)
  "Return VALUE in a JSON-safe, non-secret shape for E2E event logging."
  (cond
    ((null value) nil)
    ((eq value t) t)
    ((stringp value) (sanitize-debug-event-string value))
    ((numberp value) value)
    ((characterp value) (string value))
    ((keywordp value) (string-downcase (symbol-name value)))
    ((symbolp value) (string-downcase (symbol-name value)))
    ((pathnamep value) (namestring value))
    ((vectorp value)
     (map 'vector #'sanitize-debug-event-value value))
    ((listp value)
     (mapcar #'sanitize-debug-event-value value))
    (t (sanitize-debug-event-string (princ-to-string value)))))

(defun debug-event-payload-alist (payload)
  "Return PAYLOAD plist as a sanitized JSON alist."
  (loop :for (key value) :on payload :by #'cddr
        :while key
        :collect (cons (debug-event-json-key key)
                       (if (sensitive-debug-event-key-p key)
                           "[REDACTED]"
                           (sanitize-debug-event-value value)))))

(defun next-debug-event-sequence ()
  "Return the next structured debug event sequence number."
  (bt:with-lock-held (*debug-event-lock*)
    (incf *debug-event-sequence*)))

(defun file-debug-event (event-name &rest payload)
  "Append a structured GUI E2E debug event when env-gated logging is enabled.
EVENT-NAME is a stable string. PAYLOAD is a plist of sanitized scalar fields."
  (when (and *debug-log-file* (e2e-events-enabled-p))
    (let* ((now (get-universal-time))
           (event `((:event . ,(sanitize-debug-event-value event-name))
                    (:sequence . ,(next-debug-event-sequence))
                    (:timestamp . ,(format-timestamp now))
                    ,@(debug-event-payload-alist payload)))
           (json (cl-json:encode-json-to-string event)))
      (file-debug-log "e2e-event" "~A" json))))

(defun format-timestamp (universal-time)
  "Format UNIVERSAL-TIME as ISO 8601 local time string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))
