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

(defun format-timestamp (universal-time)
  "Format UNIVERSAL-TIME as ISO 8601 local time string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))
