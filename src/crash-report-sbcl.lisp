(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; SBCL fatal-hook, stack, thread, and private-file adapter
;;; --------------------------------------------------------------------------

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

#+sbcl
(defun crash-platform-process-id ()
  (sb-posix:getpid))

#+sbcl
(defun crash-platform-next-sequence ()
  (sb-ext:atomic-incf (car *crash-report-sequence-cell*)))

#+sbcl
(defun crash-platform-claim-report (state)
  (null
   (sb-ext:compare-and-swap
    (crash-report-claim-state-claimed-p state) nil t)))

#+sbcl
(defun crash-platform-current-debugger-hook ()
  sb-ext:*invoke-debugger-hook*)

#+sbcl
(defun crash-platform-set-debugger-hook (hook)
  (setf sb-ext:*invoke-debugger-hook* hook))

(defun crash-platform-argv-summary ()
  "Return allowlisted process metadata without raw argument values."
  #+sbcl
  (let* ((argv (copy-list sb-ext:*posix-argv*))
         (executable (and argv
                          (file-namestring (or (first argv) ""))))
         (safe-flags
           (remove-duplicates
            (remove-if-not
             (lambda (argument)
               (member argument
                       '("--noinform" "--disable-debugger" "--script"
                         "--load" "--eval")
                       :test #'string=))
             argv)
            :test #'string=)))
    (list (cons :executable (or executable "unknown"))
          (cons :argument-count (max 0 (1- (length argv))))
          (cons :allowlisted-flags safe-flags)))
  #-sbcl
  (list (cons :executable "unknown")
        (cons :argument-count 0)
        (cons :allowlisted-flags nil)))

#+sbcl
(defun crash-platform-known-frame-symbol (name)
  "Return an allowlisted function symbol found in SBCL debug NAME."
  (labels ((known-package-p (symbol)
             (let ((package (symbol-package symbol)))
               (and package
                    (let ((name (package-name package)))
                      (or (string= name "CLAWMACS")
                          (string= name "CL")
                          (string= name "COMMON-LISP")
                          (string= name "UIOP")
                          (string= name "ASDF")
                          (string= name "BORDEAUX-THREADS")
                          (string= name "BT")
                          (string= name "CLIM")
                          (string= name "CLIME")
                          (string= name "ESA")
                          (string= name "DREI")
                          (uiop:string-prefix-p "CLAWMACS/" name)
                          (uiop:string-prefix-p "SB-" name))))))
           (walk (object)
             (cond
               ((and (symbolp object) (known-package-p object)) object)
               ((consp object)
                (or (walk (car object)) (walk (cdr object))))
               (t nil))))
    (walk name)))

#+sbcl
(defun crash-platform-frame-source (frame)
  "Return a bounded basename/form location for FRAME, or NIL."
  (handler-case
      (let* ((location (sb-di:frame-code-location frame))
             (source (sb-di:code-location-debug-source location))
             (namestring (sb-di:debug-source-namestring source))
             (form (sb-di:code-location-form-number location)))
        (when namestring
          (format nil "~A~@[ form ~D~]"
                  (file-namestring namestring)
                  form)))
    (condition () nil)))

#+sbcl
(defun crash-platform-safe-backtrace (count)
  "Return function/source-only frames, never arguments or locals."
  (let ((frames nil))
    (handler-case
        (sb-debug:map-backtrace
         (lambda (frame)
           (let* ((debug-function (sb-di:frame-debug-fun frame))
                  (raw-name (sb-di:debug-fun-name debug-function))
                  (symbol (crash-platform-known-frame-symbol raw-name))
                  (source (and symbol (crash-platform-frame-source frame))))
             (push
              (if symbol
                  (format nil "~A::~A~@[ (~A)~]"
                          (package-name (symbol-package symbol))
                          (symbol-name symbol)
                          source)
                  "<external>")
              frames)))
         :from :interrupted-frame
         :count count)
      (condition () nil))
    (nreverse frames)))

#-sbcl
(defun crash-platform-safe-backtrace (count)
  (declare (ignore count))
  nil)

#+sbcl
(defun crash-platform-thread-role (thread current)
  "Classify THREAD without disclosing its raw name."
  (let ((name (string-downcase (or (sb-thread:thread-name thread) ""))))
    (cond
      ((search "main thread" name) :main)
      ((or (search "frame" name) (search "clim" name)) :frame)
      ((or (search "provider" name)
           (search "stream" name)
           (search "oauth" name)
           (search "http" name))
       :provider-worker)
      ((or (search "tool" name)
           (search "pipeline" name)
           (search "subprocess" name))
       :tool-worker)
      ((search "subagent" name) :subagent-worker)
      ((search "clawmacs" name) :clawmacs-worker)
      ((eq thread current) :other)
      (t :other))))

#+sbcl
(defun crash-platform-thread-inventory (limit)
  "Return bounded role-only thread metadata."
  (let ((current sb-thread:*current-thread*))
    (loop :for thread :in (sb-thread:list-all-threads)
          :repeat limit
          :collect
          (list :role (crash-platform-thread-role thread current)
                :current-p (eq thread current)
                :alive-p (not (null
                               (ignore-errors
                                 (sb-thread:thread-alive-p thread))))
                :os-tid (ignore-errors
                          (sb-thread:thread-os-tid thread))))))

#-sbcl
(defun crash-platform-thread-inventory (limit)
  (declare (ignore limit))
  nil)

#+sbcl
(defun crash-platform-lstat (path)
  "Return PATH's non-following stat record, or NIL when it does not exist."
  (handler-case
      (let ((native (namestring path)))
        ;; A trailing slash makes POSIX follow a final directory symlink even
        ;; for LSTAT. UIOP directory pathnames carry that slash, so remove it
        ;; at this non-following validation boundary.
        (sb-posix:lstat
         (if (and (> (length native) 1)
                  (char= #\/ (char native (1- (length native)))))
             (subseq native 0 (1- (length native)))
             native)))
    (sb-posix:syscall-error () nil)))

#+sbcl
(defun crash-platform-ensure-private-directory (directory)
  "Create DIRECTORY privately, or validate an existing owner-private leaf."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (stat (crash-platform-lstat directory)))
    (if stat
        (let ((mode (sb-posix:stat-mode stat)))
          (when (sb-posix:s-islnk mode)
            (error "Crash report directory must not be a symbolic link: ~A"
                   directory))
          (unless (sb-posix:s-isdir mode)
            (error "Crash report path is not a directory: ~A" directory))
          (unless (= (sb-posix:stat-uid stat) (sb-posix:getuid))
            (error "Crash report directory is not owned by this user: ~A"
                   directory))
          (unless (= #o700 (logand mode #o777))
            (error "Existing crash report directory must have mode 0700: ~A"
                   directory)))
        (progn
      (handler-case
          (sb-posix:mkdir (namestring directory) #o700)
        (sb-posix:syscall-error ()
              (error "Unable to create crash report directory ~A."
                     directory)))
          (sb-posix:chmod (namestring directory) #o700)))
    directory))

#-sbcl
(defun crash-platform-ensure-private-directory (directory)
  (declare (ignore directory))
  (error "Private crash reports require the SBCL platform adapter."))

#+sbcl
(defun crash-platform-write-private-file (path content)
  "Create PATH O_EXCL at 0600, write CONTENT, and fsync it."
  (let ((descriptor nil)
        (stream nil))
    (unwind-protect
         (progn
           (setf descriptor
                 (sb-posix:open
                  (namestring path)
                  (logior sb-posix:o-wronly
                          sb-posix:o-creat
                          sb-posix:o-excl)
                  #o600))
           (sb-posix:fchmod descriptor #o600)
           (setf stream
                 (sb-sys:make-fd-stream
                  descriptor
                  :output t
                  :element-type 'character
                  :external-format :utf-8
                  :buffering :full
                  :auto-close t))
           (write-string content stream)
           (finish-output stream)
           (sb-posix:fsync descriptor)
           (close stream)
           (setf stream nil
                 descriptor nil)
           path)
      (when stream
        (ignore-errors (close stream :abort t)))
      (when descriptor
        (ignore-errors (sb-posix:close descriptor))))))

#-sbcl
(defun crash-platform-write-private-file (path content)
  (declare (ignore path content))
  (error "Private crash reports require the SBCL platform adapter."))

#+sbcl
(defun crash-platform-atomic-publish (source target)
  "Publish SOURCE as TARGET atomically without replacing an existing target."
  (sb-posix:link (namestring source) (namestring target))
  (handler-case
      (sb-posix:unlink (namestring source))
    (condition (condition)
      ;; Roll back the newly linked final name if the temporary name could not
      ;; be retired, preserving the all-or-nothing publication contract.
      (ignore-errors (sb-posix:unlink (namestring target)))
      (error condition)))
  target)

#-sbcl
(defun crash-platform-atomic-publish (source target)
  (when (probe-file target)
    (error "Crash report target already exists: ~A" target))
  (rename-file source target))

#+sbcl
(defun crash-platform-fsync-directory (directory)
  (let ((descriptor nil))
    (unwind-protect
         (progn
           (setf descriptor
                 (sb-posix:open (namestring directory) sb-posix:o-rdonly 0))
           (sb-posix:fsync descriptor))
      (when descriptor
        (ignore-errors (sb-posix:close descriptor))))))

#-sbcl
(defun crash-platform-fsync-directory (directory)
  (declare (ignore directory))
  nil)
