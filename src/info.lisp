(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Info / Manual Buffers
;;; --------------------------------------------------------------------------

(defvar *info-buffer-name* "*info*"
  "Default buffer name for the built-in Info manual browser.")

(defvar *info-buffer-states* (make-hash-table :test #'eq)
  "Per-buffer Info browser state.")

(defvar *rplaca-info-manual-path* nil
  "Optional pathname to a generated RPLACA Info manual.")

(defvar *info-manual-cache* (make-hash-table :test #'equal)
  "Cache of parsed Info manuals keyed by source location.")

(defstruct (info-location
            (:constructor %make-info-location
                (source-kind manual node &key file-path))
            (:copier nil))
  "A resolved Info destination."
  (source-kind :system :type keyword)
  (manual "dir" :type string)
  (node "Top" :type string)
  (file-path nil :type (or null pathname)))

(defstruct (info-link
            (:constructor make-info-link
                (&key label target kind)))
  "One clickable Info navigation target."
  (label "" :type string)
  (target (%make-info-location :system "dir" "Top") :type info-location)
  (kind :xref :type keyword))

(defstruct (info-segment
            (:constructor make-info-segment
                (&key (text "") (face :default-text) link-index)))
  "One styled display fragment in an Info buffer line."
  (text "" :type string)
  (face :default-text :type keyword)
  (link-index nil :type (or null integer)))

(defstruct (info-document
            (:constructor make-info-document
                (&key manual node title header-file
                      next-target prev-target up-target top-target
                      lines links error-p)))
  "Rendered Info node metadata and display lines."
  (manual "dir" :type string)
  (node "Top" :type string)
  (title "Top" :type string)
  (header-file nil :type (or null string))
  (next-target nil :type (or null info-location))
  (prev-target nil :type (or null info-location))
  (up-target nil :type (or null info-location))
  (top-target nil :type (or null info-location))
  (lines nil :type list)
  (links nil :type list)
  (error-p nil :type boolean))

(defstruct (info-node-record
            (:constructor make-info-node-record
                (&key node header body-lines)))
  "One parsed node from an Info manual."
  (node "Top" :type string)
  (header nil :type list)
  (body-lines nil :type list))

(defstruct (info-manual-data
            (:constructor make-info-manual-data
                (&key manual files nodes)))
  "Parsed node index for one Info manual."
  (manual "dir" :type string)
  (files nil :type list)
  (nodes (make-hash-table :test #'equalp) :type hash-table))

(defstruct (info-state
            (:constructor make-info-state
                (&key location document history forward-history
                      selected-link-index)))
  "Per-buffer Info browser state."
  (location nil :type (or null info-location))
  (document nil :type (or null info-document))
  (history nil :type list)
  (forward-history nil :type list)
  (selected-link-index nil :type (or null integer)))

(defun info-buffer-p (buf)
  "Return true when BUF is an Info/manual browser buffer."
  (and buf (eq (buffer-kind buf) :info)))

(defun info-buffer-state (buf)
  "Return BUF's Info state, creating it when needed."
  (unless (info-buffer-p buf)
    (error "Not an Info buffer: ~A" (and buf (buffer-name buf))))
  (or (gethash buf *info-buffer-states*)
      (setf (gethash buf *info-buffer-states*)
            (make-info-state))))

(defun info-buffer-text (buf)
  "Return BUF's current plain-text Info node."
  (unless (info-buffer-p buf)
    (error "Not an Info buffer: ~S" buf))
  (let ((msg (message-prev (buffer-input-message buf))))
    (if msg
        (message-text msg)
        "")))

(defun info-trim-string (value)
  "Return VALUE trimmed of surrounding ASCII whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (or value "")))

(defun info-split-lines (string)
  "Split STRING into a list of lines."
  (loop :with len := (length (or string ""))
        :for start := 0 :then (1+ end)
        :for end := (position #\Newline string :start start)
        :collect (subseq string start (or end len))
        :while end))

(defun info-split-on-char (string char)
  "Split STRING on CHAR and return every field."
  (loop :with len := (length string)
        :for start := 0 :then (1+ pos)
        :for pos := (position char string :start start)
        :collect (subseq string start (or pos len))
        :while pos))

(defun info-string-prefix-ci-p (prefix string)
  "Return true when STRING begins with PREFIX, case-insensitively."
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (string-equal prefix string
                       :end1 prefix-length
                       :end2 prefix-length))))

(defun info-string-search-ci (needle string &key (start 0))
  "Return the first case-insensitive occurrence of NEEDLE in STRING."
  (search needle string :start2 start :test #'char-equal))

(defun info-string-suffix-ci-p (suffix string)
  "Return true when STRING ends with SUFFIX, case-insensitively."
  (let ((suffix-length (length suffix))
        (string-length (length string)))
    (and (<= suffix-length string-length)
         (string-equal suffix string
                       :start2 (- string-length suffix-length)))))

(defun info-ensure-directory-pathname (designator)
  "Return DESIGNATOR as an existing directory pathname, or NIL."
  (let ((directory (and designator
                        (ignore-errors
                          (uiop:ensure-directory-pathname
                           (pathname designator))))))
    (and directory
         (ignore-errors (uiop:directory-exists-p directory)))))

(defun info-search-path-candidates ()
  "Return candidate directories that may contain Info manuals."
  (append
   (mapcar #'info-ensure-directory-pathname
           (remove-if #'blank-string-p
                      (uiop:split-string (or (uiop:getenv "INFOPATH") "")
                                         :separator ":")))
   (list (info-ensure-directory-pathname
          (merge-pathnames #P".guix-home/profile/share/info/"
                           (user-homedir-pathname)))
         (info-ensure-directory-pathname
          (merge-pathnames #P".guix-profile/share/info/"
                           (user-homedir-pathname)))
         (info-ensure-directory-pathname #P"/run/current-system/profile/share/info/"))
   (ignore-errors (directory #P"/gnu/store/*-profile/share/info/"))
   (ignore-errors (directory #P"/gnu/store/*-info-dir/share/info/"))))

(defun info-search-paths ()
  "Return existing Info search directories in preference order."
  (let ((seen (make-hash-table :test #'equal))
        (paths nil))
    (dolist (candidate (remove nil (info-search-path-candidates)))
      (let* ((directory (info-ensure-directory-pathname candidate))
             (key (and directory (namestring directory))))
        (when (and key (not (gethash key seen)))
          (setf (gethash key seen) t)
          (push directory paths))))
    (nreverse paths)))

(defun info-path-gzip-p (path)
  "Return true when PATH names a gzip-compressed file."
  (info-string-suffix-ci-p ".gz" (file-namestring path)))

(defun info-manual-filename-stem (path)
  "Return PATH's file name with a trailing .gz removed."
  (let ((name (file-namestring path)))
    (if (info-path-gzip-p path)
        (subseq name 0 (- (length name) 3))
        name)))

(defun info-manual-file-base-name (path)
  "Return the logical manual name represented by PATH, or NIL."
  (let ((stem (info-manual-filename-stem path)))
    (cond
      ((string-equal stem "dir")
       "dir")
      ((let ((pos (search ".info-" stem :from-end t :test #'char-equal)))
         (and pos
              (every #'digit-char-p (subseq stem (+ pos 6)))))
       (subseq stem 0 (search ".info-" stem :from-end t :test #'char-equal)))
      ((info-string-suffix-ci-p ".info" stem)
       (subseq stem 0 (- (length stem) 5)))
      (t nil))))

(defun info-manual-piece-index (path)
  "Return PATH's split-manual piece number, or 0 for the primary file."
  (let* ((stem (info-manual-filename-stem path))
         (pos (search ".info-" stem :from-end t :test #'char-equal)))
    (if pos
        (or (ignore-errors (parse-integer stem :start (+ pos 6)))
            0)
        0)))

(defun info-manual-file< (left right)
  "Return true when LEFT should sort before RIGHT within one manual."
  (let ((left-index (info-manual-piece-index left))
        (right-index (info-manual-piece-index right)))
    (if (= left-index right-index)
        (string-lessp (file-namestring left)
                      (file-namestring right))
        (< left-index right-index))))

(defun info-directory-manual-files (manual directory)
  "Return the files for MANUAL found in DIRECTORY."
  (let ((files
          (remove-if-not
           (lambda (path)
             (and (pathname-name path)
                  (string-equal manual
                                (or (info-manual-file-base-name path) ""))))
           (ignore-errors (uiop:directory-files directory)))))
    (sort files #'info-manual-file<)))

(defun info-system-manual-files (manual)
  "Return the first matching set of system files for MANUAL."
  (loop :for directory :in (info-search-paths)
        :for files := (info-directory-manual-files manual directory)
        :when files
          :return files))

(defun info-location-manual-files (location)
  "Return the underlying Info files for LOCATION."
  (case (info-location-source-kind location)
    (:file
     (let* ((file (and (info-location-file-path location)
                       (ignore-errors (probe-file (info-location-file-path location)))))
            (directory (and file
                            (info-ensure-directory-pathname
                             (uiop:pathname-directory-pathname file))))
            (matches (and directory
                          (info-directory-manual-files
                           (info-location-manual location)
                           directory))))
       (cond
         (matches matches)
         (file (list file))
         (t nil))))
    ((:system :synthetic)
     (info-system-manual-files (info-location-manual location)))
    (otherwise
     nil)))

(defun info-octets-to-string (octets)
  "Decode OCTETS as UTF-8 text."
  #+sbcl
  (sb-ext:octets-to-string octets :external-format :utf-8)
  #-sbcl
  (map 'string #'code-char octets))

(defun info-read-file-string (path)
  "Return PATH's contents as a UTF-8 string."
  (if (info-path-gzip-p path)
      (with-open-file (stream path :element-type '(unsigned-byte 8))
        (info-octets-to-string
         (chipz:decompress nil 'chipz:gzip stream)))
      (with-open-file (stream path :direction :input :external-format :utf-8)
        (let ((data (make-string (file-length stream))))
          (read-sequence data stream)
          data))))

(defun info-parse-node-records-into (text nodes)
  "Parse TEXT into NODES, an equalp hash table keyed by node name."
  (dolist (chunk (info-split-on-char text (code-char 31)))
    (let* ((trimmed (string-left-trim '(#\Return #\Newline) chunk))
           (lines (and (plusp (length trimmed))
                       (info-split-lines trimmed)))
           (header-line (first lines))
           (header (and header-line
                        (info-string-prefix-ci-p "File:" header-line)
                        (info-parse-header-line header-line)))
           (node (and header (info-header-value header "node")))
           (header-text (and header-line
                             (info-header-trailing-text header-line))))
      (when (and node (plusp (length (info-trim-string node))))
        (setf (gethash node nodes)
              (make-info-node-record :node node
                                     :header header
                                     :body-lines (append (and header-text
                                                              (list header-text))
                                                         (rest lines)))))))
  nodes)

(defun info-manual-cache-key (location)
  "Return the cache key for LOCATION."
  (case (info-location-source-kind location)
    (:file
     (list :file
           (and (info-location-file-path location)
                (namestring (info-location-file-path location)))))
    (:synthetic
     (list :synthetic (string-downcase (info-location-manual location))))
    (otherwise
     (list :system (string-downcase (info-location-manual location))))))

(defun info-load-manual-data (location)
  "Return parsed manual data for LOCATION, or NIL when unavailable."
  (let ((cache-key (info-manual-cache-key location)))
    (or (gethash cache-key *info-manual-cache*)
        (let ((files (info-location-manual-files location)))
          (when files
            (let ((nodes (make-hash-table :test #'equalp)))
              (dolist (file files)
                (info-parse-node-records-into (info-read-file-string file) nodes))
              (setf (gethash cache-key *info-manual-cache*)
                    (make-info-manual-data
                     :manual (info-location-manual location)
                     :files files
                     :nodes nodes))))))))

(defun info-find-node-record (location)
  "Return LOCATION's parsed node record, or NIL when unavailable."
  (let* ((manual-data (info-load-manual-data location))
         (nodes (and manual-data (info-manual-data-nodes manual-data))))
    (and nodes
         (gethash (info-location-node location) nodes))))

(defun normalize-info-manual-name (value)
  "Return VALUE as a normalized manual name."
  (let* ((trimmed (info-trim-string (string value)))
         (unwrapped (if (and (> (length trimmed) 2)
                             (char= (char trimmed 0) #\()
                             (char= (char trimmed (1- (length trimmed))) #\)))
                        (subseq trimmed 1 (1- (length trimmed)))
                        trimmed)))
    (if (plusp (length unwrapped))
        unwrapped
        "dir")))

(defun normalize-info-node-name (value)
  "Return VALUE as a normalized Info node name."
  (let ((trimmed (info-trim-string (and value (string value)))))
    (if (plusp (length trimmed))
        trimmed
        "Top")))

(defun normalize-info-file-path (value)
  "Return VALUE as a normalized optional Info file pathname."
  (cond
    ((null value) nil)
    ((pathnamep value) value)
    ((stringp value)
     (let ((trimmed (info-trim-string value)))
       (and (plusp (length trimmed))
            (pathname trimmed))))
    (t
     (error "Invalid Info file path: ~S" value))))

(defun make-info-location (source-kind manual node &key file-path)
  "Return a normalized Info location."
  (%make-info-location source-kind
                       (normalize-info-manual-name manual)
                       (normalize-info-node-name node)
                       :file-path (normalize-info-file-path file-path)))

(defun copy-info-location (location)
  "Return a shallow copy of LOCATION."
  (and location
       (make-info-location (info-location-source-kind location)
                           (info-location-manual location)
                           (info-location-node location)
                           :file-path (info-location-file-path location))))

(defun info-location= (left right)
  "Return true when LEFT and RIGHT describe the same Info destination."
  (and left
       right
       (eq (info-location-source-kind left)
           (info-location-source-kind right))
       (string-equal (info-location-manual left)
                     (info-location-manual right))
       (string= (info-location-node left)
                (info-location-node right))
       (equal (and (info-location-file-path left)
                   (namestring (info-location-file-path left)))
              (and (info-location-file-path right)
                   (namestring (info-location-file-path right))))))

(defun info-directory-location (&optional (node "Top"))
  "Return the top-level Info directory LOCATION."
  (make-info-location :system "dir" node))

(defun rplaca-info-manual-candidate-paths ()
  "Return possible generated Info manual paths for RPLACA."
  (remove nil
          (list
           (and *rplaca-info-manual-path*
                (pathname *rplaca-info-manual-path*))
           (ignore-errors
             (asdf:system-relative-pathname "rplaca"
                                            #P"docs/rplaca.info"))
           (ignore-errors
             (asdf:system-relative-pathname "rplaca"
                                            #P"docs/manual/rplaca.info"))
           (ignore-errors
             (asdf:system-relative-pathname "rplaca"
                                            #P"manual/rplaca.info"))
           (ignore-errors
             (asdf:system-relative-pathname "rplaca"
                                            #P"doc/rplaca.info")))))

(defun resolve-rplaca-info-manual-file ()
  "Return the first existing local RPLACA Info manual, or NIL."
  (loop :for candidate :in (rplaca-info-manual-candidate-paths)
        :for path := (and candidate
                          (ignore-errors (probe-file candidate)))
        :when path
          :return path))

(defun resolve-rplaca-info-location (&optional (node "Top"))
  "Return a LOCATION for the RPLACA manual."
  (let ((path (resolve-rplaca-info-manual-file)))
    (if path
        (make-info-location :file "rplaca" node :file-path path)
        (make-info-location :synthetic "rplaca" node))))

(defun info-location-for-manual (manual &key (node "Top") current-location)
  "Resolve MANUAL and NODE into an Info location."
  (let ((manual-name (normalize-info-manual-name manual))
        (node-name (normalize-info-node-name node)))
    (cond
      ((string-equal manual-name "dir")
       (info-directory-location node-name))
      ((and current-location
            (string-equal manual-name
                          (info-location-manual current-location))
            (member (info-location-source-kind current-location)
                    '(:file :synthetic)))
       (make-info-location (info-location-source-kind current-location)
                           manual-name
                           node-name
                           :file-path (info-location-file-path current-location)))
      ((string-equal manual-name "rplaca")
       (resolve-rplaca-info-location node-name))
      (t
       (make-info-location :system manual-name node-name)))))

(defun serialize-info-location (location)
  "Return LOCATION as an alist suitable for persistence."
  `((:source-kind . ,(string-downcase
                      (symbol-name (info-location-source-kind location))))
    (:manual . ,(info-location-manual location))
    (:node . ,(info-location-node location))
    ,@(when (info-location-file-path location)
        `((:file-path . ,(namestring (info-location-file-path location)))))))

(defun restore-info-location (data)
  "Restore DATA into an Info location."
  (let* ((raw-kind (cdr (assoc :source-kind data)))
         (source-kind
           (or (and raw-kind
                    (ignore-errors
                      (intern (string-upcase (string raw-kind)) :keyword)))
               :system)))
    (make-info-location source-kind
                        (or (cdr (assoc :manual data)) "dir")
                        (or (cdr (assoc :node data)) "Top")
                        :file-path (cdr (assoc :file-path data)))))

(defun info-serialize-buffer-state (buf)
  "Return BUF's Info browser state for persistence."
  (let ((state (info-buffer-state buf)))
    `((:location
       . ,(and (info-state-location state)
               (serialize-info-location (info-state-location state))))
      (:history
       . ,(coerce (mapcar #'serialize-info-location
                          (info-state-history state))
                  'vector))
      (:forward-history
       . ,(coerce (mapcar #'serialize-info-location
                          (info-state-forward-history state))
                  'vector))
      (:selected-link-index . ,(info-state-selected-link-index state)))))

(defun info-populate-buffer-text (buf text)
  "Replace BUF's plain-text body with TEXT."
  (buffer-clear-history-before-input buf)
  (buffer-insert-system-message buf text :record-p nil)
  (setf (buffer-scroll-offset buf) most-positive-fixnum)
  (notify-buffer-display-change buf :info)
  buf)

(defun info-restore-buffer-state (buf persisted-state)
  "Restore PERSISTED-STATE into BUF."
  (let* ((state (info-buffer-state buf))
         (location-data (cdr (assoc :location persisted-state)))
         (location (and location-data
                        (restore-info-location location-data)))
         (history (loop :for item :in (coerce (or (cdr (assoc :history
                                                              persisted-state))
                                                  #())
                                              'list)
                        :collect (restore-info-location item)))
         (forward (loop :for item :in (coerce (or (cdr (assoc :forward-history
                                                              persisted-state))
                                                  #())
                                              'list)
                        :collect (restore-info-location item))))
    (setf (info-state-history state) history
          (info-state-forward-history state) forward
          (info-state-selected-link-index state)
          (cdr (assoc :selected-link-index persisted-state)))
    (if location
        (progn
          (info-visit-location buf location :push-history-p nil :clear-forward-p nil)
          (let* ((document (info-state-document state))
                 (saved-index (cdr (assoc :selected-link-index persisted-state)))
                 (max-index (and document
                                 (1- (length (info-document-links document))))))
            (when (and (integerp saved-index)
                       max-index
                       (>= max-index 0))
              (setf (info-state-selected-link-index state)
                    (min (max 0 saved-index) max-index)))))
        (info-populate-buffer-text buf "No Info location stored for this buffer.")))
  buf)

(defun info-program-available-p ()
  "Return true when at least one Info manual directory is available."
  (or (not (null (resolve-rplaca-info-manual-file)))
      (not (null (info-system-manual-files "dir")))))

(defun info-parse-header-line (line)
  "Return an alist of Info header fields parsed from LINE."
  (let ((fields nil))
    (dolist (part (info-split-on-char
                   (substitute #\, #\Tab (or line ""))
                   #\,))
      (let* ((trimmed (info-trim-string part))
             (colon (position #\: trimmed)))
        (when colon
          (let* ((key (string-downcase (subseq trimmed 0 colon)))
                 (value (info-trim-string (subseq trimmed (1+ colon)))))
            (when (plusp (length value))
              (push (cons key value) fields))))))
    (nreverse fields)))

(defun info-header-trailing-text (line)
  "Return any non-field trailing prose carried on an Info header LINE."
  (let ((chunks nil))
    (dolist (part (info-split-on-char (or line "") #\Tab))
      (let ((trimmed (info-trim-string part)))
        (when (and (plusp (length trimmed))
                   (null (position #\: trimmed)))
          (push trimmed chunks))))
    (let ((text (format nil "~{~A~^ ~}" (nreverse chunks))))
      (unless (blank-string-p text)
        text))))

(defun info-header-value (header key)
  "Return the value for KEY from parsed HEADER."
  (cdr (assoc key header :test #'string=)))

(defun parse-explicit-info-location (text &optional current-location)
  "Parse `(manual)node` TEXT into an Info location."
  (let* ((trimmed (info-trim-string text))
         (close (and (plusp (length trimmed))
                     (char= (char trimmed 0) #\()
                     (position #\) trimmed :start 1))))
    (unless close
      (error "Expected an Info target like (manual)Node, got ~S." text))
    (let ((manual (subseq trimmed 1 close))
          (node (info-trim-string (subseq trimmed (1+ close)))))
      (info-location-for-manual manual
                                :node (if (plusp (length node)) node "Top")
                                :current-location current-location))))

(defun parse-info-target-spec (target-spec current-location)
  "Parse TARGET-SPEC relative to CURRENT-LOCATION."
  (let ((trimmed (info-trim-string target-spec)))
    (cond
      ((zerop (length trimmed))
       (error "Blank Info target spec."))
      ((char= (char trimmed 0) #\()
       (parse-explicit-info-location trimmed current-location))
      (current-location
       (info-location-for-manual (info-location-manual current-location)
                                 :node trimmed
                                 :current-location current-location))
      (t
       (error "Unqualified Info target requires a current manual: ~S."
              target-spec)))))

(defun parse-info-open-designator (designator)
  "Parse DESIGNATOR for an interactive manual-open command."
  (let ((trimmed (info-trim-string designator)))
    (cond
      ((zerop (length trimmed))
       (info-directory-location))
      ((char= (char trimmed 0) #\()
       (parse-explicit-info-location trimmed nil))
      (t
       (let* ((split (or (position-if (lambda (ch)
                                        (member ch '(#\Space #\Tab)))
                                      trimmed)
                         (length trimmed)))
              (manual (subseq trimmed 0 split))
              (node (info-trim-string (subseq trimmed split))))
         (info-location-for-manual manual
                                   :node (if (plusp (length node))
                                             node
                                             "Top")))))))

(defun parse-info-goto-designator (designator current-location)
  "Parse DESIGNATOR while visiting CURRENT-LOCATION."
  (let ((trimmed (info-trim-string designator)))
    (cond
      ((zerop (length trimmed))
       (copy-info-location current-location))
      ((char= (char trimmed 0) #\()
       (parse-explicit-info-location trimmed current-location))
      (t
       (info-location-for-manual (info-location-manual current-location)
                                 :node trimmed
                                 :current-location current-location)))))

(defun info-error-document (location message &key detail)
  "Return an error document for LOCATION."
  (let* ((manual (if location
                     (info-location-manual location)
                     "dir"))
         (node (if location
                   (info-location-node location)
                   "Top"))
         (lines
           (list
            (list (make-info-segment :text (format nil "Info Error: ~A" message)
                                     :face :system))
            (list (make-info-segment
                   :text (format nil "Manual: ~A  Node: ~A" manual node)
                   :face :selector-footer))
            (list (make-info-segment :text "" :face :default-text))
            (list (make-info-segment :text (or detail "") :face :default-text)))))
    (make-info-document :manual manual
                        :node node
                        :title "Info Error"
                        :top-target (and location
                                         (info-location-for-manual manual
                                                                   :node "Top"
                                                                   :current-location
                                                                   location))
                        :lines lines
                        :links nil
                        :error-p t)))

(defun synthetic-rplaca-info-document (location)
  "Return the placeholder RPLACA manual document for LOCATION."
  (let* ((dir-target (info-directory-location))
         (lines
           (list
            (list (make-info-segment :text "RPLACA Manual"
                                     :face :selector-title))
            (list (make-info-segment
                   :text "***************"
                   :face :selector-separator))
            (list (make-info-segment
                   :text ""
                   :face :default-text))
            (list (make-info-segment
                   :text "The RPLACA Texinfo manual is not installed yet."
                   :face :default-text))
            (list (make-info-segment
                   :text "When a generated rplaca.info file is present, this buffer will"
                   :face :default-text))
            (list (make-info-segment
                   :text "open it automatically through the same Info browser."
                   :face :default-text))
            (list (make-info-segment :text "" :face :default-text))
            (list (make-info-segment :text "*note System Directory: (dir)Top."
                                     :face :selector-entry
                                     :link-index 0)))))
    (make-info-document
     :manual (info-location-manual location)
     :node (info-location-node location)
     :title "RPLACA Manual"
     :up-target dir-target
     :top-target (copy-info-location location)
     :lines lines
     :links (list (make-info-link :label "System Directory"
                                  :target dir-target
                                  :kind :xref)))))

(defun info-line-ruler-p (line)
  "Return true when LINE is a simple Info underline/separator."
  (let ((trimmed (info-trim-string line)))
    (and (plusp (length trimmed))
         (every (lambda (ch)
                  (member ch '(#\* #\= #\-)
                          :test #'char=))
                trimmed))))

(defun info-base-face-for-line (line next-line)
  "Return the base face used to display LINE."
  (cond
    ((info-line-ruler-p line) :selector-separator)
    ((and next-line (info-line-ruler-p next-line)) :selector-title)
    ((blank-string-p line) :default-text)
    ((string-equal (info-trim-string line) "* Menu:") :selector-header)
    ((and (plusp (length line))
          (not (find (char line 0) '(#\Space #\Tab))))
     :selector-header)
    (t :default-text)))

(defun info-register-link (links label target kind)
  "Append a new link to LINKS and return two values: index and updated list."
  (let ((index (length links)))
    (values index
            (append links
                    (list (make-info-link :label label
                                          :target target
                                          :kind kind))))))

(defun info-append-plain-segment (segments text face)
  "Append a plain TEXT segment to SEGMENTS when TEXT is non-empty."
  (if (zerop (length text))
      segments
      (append segments
              (list (make-info-segment :text text :face face)))))

(defun info-parse-menu-line (line face location links)
  "Parse LINE as an Info menu item when applicable."
  (let ((trimmed (info-trim-string line)))
    (unless (and (info-string-prefix-ci-p "* " trimmed)
                 (not (info-string-prefix-ci-p "*note " trimmed)))
      (return-from info-parse-menu-line (values nil links)))
    (let* ((colon (position #\: line :start 2))
           (label (and colon
                       (info-trim-string (subseq line 2 colon)))))
      (unless (and colon (plusp (length label)))
        (return-from info-parse-menu-line (values nil links)))
      (if (and (< (1+ colon) (length line))
               (char= (char line (1+ colon)) #\:))
          (multiple-value-bind (index updated-links)
              (info-register-link links
                                  label
                                  (info-location-for-manual
                                   (info-location-manual location)
                                   :node label
                                   :current-location location)
                                  :menu)
            (values
             (list
              (make-info-segment :text (subseq line 0 (+ colon 2))
                                 :face face
                                 :link-index index)
              (make-info-segment :text (subseq line (+ colon 2))
                                 :face face))
             updated-links))
          (let* ((target-start (or (position-if-not
                                    (lambda (ch)
                                      (member ch '(#\Space #\Tab)))
                                    line
                                    :start (1+ colon))
                                   (1+ colon)))
                 (period (position #\. line :start target-start))
                 (target-end (or period (length line)))
                 (target-spec (info-trim-string
                               (subseq line target-start target-end)))
                 (link-end (if period (1+ period) target-end)))
            (unless (plusp (length target-spec))
              (return-from info-parse-menu-line (values nil links)))
            (multiple-value-bind (index updated-links)
                (info-register-link links
                                    label
                                    (parse-info-target-spec target-spec location)
                                    :menu)
              (values
               (list
                (make-info-segment :text (subseq line 0 link-end)
                                   :face face
                                   :link-index index)
                (make-info-segment :text (subseq line link-end)
                                   :face face))
               updated-links)))))))

(defun info-parse-xref-line (line face location links)
  "Parse inline `*note ...` references from LINE."
  (let ((segments nil)
        (cursor 0)
        (updated-links links))
    (loop
      :for note-pos := (info-string-search-ci "*note " line :start cursor)
      :do (if (null note-pos)
              (progn
                (setf segments
                      (info-append-plain-segment segments
                                                 (subseq line cursor)
                                                 face))
                (return))
              (progn
                (setf segments
                      (info-append-plain-segment segments
                                                 (subseq line cursor note-pos)
                                                 face))
                (let* ((label-start (+ note-pos 6))
                       (colon (position #\: line :start label-start))
                       (label (and colon
                                   (info-trim-string
                                    (subseq line label-start colon)))))
                  (if (or (null colon) (blank-string-p label))
                      (progn
                        (setf segments
                              (info-append-plain-segment segments
                                                         (subseq line note-pos)
                                                         face))
                        (return))
                      (if (and (< (1+ colon) (length line))
                               (char= (char line (1+ colon)) #\:))
                          (multiple-value-bind (index links-after)
                              (info-register-link
                               updated-links
                               label
                               (info-location-for-manual
                                (info-location-manual location)
                                :node label
                                :current-location location)
                               :xref)
                            (setf updated-links links-after
                                  segments
                                  (append segments
                                          (list
                                           (make-info-segment
                                            :text (subseq line note-pos
                                                          (+ colon 2))
                                            :face face
                                            :link-index index)))
                                  cursor (+ colon 2)))
                          (let* ((target-start
                                   (or (position-if-not
                                        (lambda (ch)
                                          (member ch '(#\Space #\Tab)))
                                        line
                                        :start (1+ colon))
                                       (1+ colon)))
                                 (period (position #\. line :start target-start))
                                 (target-end (or period (length line)))
                                 (target-spec
                                   (info-trim-string
                                    (subseq line target-start target-end)))
                                 (link-end (if period (1+ period) target-end)))
                            (if (blank-string-p target-spec)
                                (progn
                                  (setf segments
                                        (info-append-plain-segment
                                         segments
                                         (subseq line note-pos link-end)
                                         face))
                                  (setf cursor link-end))
                                (multiple-value-bind (index links-after)
                                    (info-register-link
                                     updated-links
                                     label
                                     (parse-info-target-spec target-spec location)
                                     :xref)
                                  (setf updated-links links-after
                                        segments
                                        (append segments
                                                (list
                                                 (make-info-segment
                                                  :text (subseq line note-pos
                                                                link-end)
                                                  :face face
                                                  :link-index index)))
                                        cursor link-end))))))))))
    (values segments updated-links)))

(defun info-build-display-model (location body-lines next-target prev-target
                                   up-target top-target)
  "Return two values: display lines and link list for one Info node."
  (let ((lines nil)
        (links nil))
    (labels ((push-line (segments)
               (push segments lines))
             (push-nav-link (label target)
               (multiple-value-bind (index updated-links)
                   (info-register-link links
                                       label
                                       target
                                       :nav)
                 (setf links updated-links)
                 (make-info-segment
                  :text (format nil "[~A]" label)
                  :face :selector-entry
                  :link-index index))))
      (push-line
       (list
        (make-info-segment
         :text (format nil "Manual: ~A    Node: ~A"
                       (info-location-manual location)
                       (info-location-node location))
         :face :selector-header)))
      (let ((nav-segments nil))
        (dolist (spec
                 (remove nil
                         (list (and next-target
                                    (list (format nil "Next: ~A"
                                                  (info-location-node next-target))
                                          next-target))
                               (and prev-target
                                    (list (format nil "Prev: ~A"
                                                  (info-location-node prev-target))
                                          prev-target))
                               (and up-target
                                    (list (format nil "Up: ~A"
                                                  (info-location-node up-target))
                                          up-target))
                               (list "Top" top-target)
                               (list "Dir" (info-directory-location)))))
          (when nav-segments
            (setf nav-segments
                  (append nav-segments
                          (list (make-info-segment :text "  "
                                                   :face :selector-footer)))))
          (setf nav-segments
                (append nav-segments
                        (list (push-nav-link (first spec) (second spec)))))) 
        (push-line nav-segments))
      (push-line (list (make-info-segment :text "" :face :default-text)))
      (loop :for tail :on body-lines
            :for line := (first tail)
            :for next-line := (second tail)
            :for face := (info-base-face-for-line line next-line)
            :do (multiple-value-bind (menu-segments updated-links)
                    (info-parse-menu-line line face location links)
                  (setf links updated-links)
                  (if menu-segments
                      (push-line menu-segments)
                      (multiple-value-bind (xref-segments xref-links)
                          (info-parse-xref-line line face location links)
                        (setf links xref-links)
                        (push-line (or xref-segments
                                       (list (make-info-segment :text line
                                                                :face face)))))))))
      (values (nreverse lines) links)))

(defun info-node-record-document (location record)
  "Build a rendered document for LOCATION from parsed node RECORD."
  (let* ((header (info-node-record-header record))
         (manual (info-location-manual location))
         (node (or (info-header-value header "node")
                   (info-node-record-node record)
                   (info-location-node location)))
         (title (or (loop :for line :in (info-node-record-body-lines record)
                          :for trimmed := (info-trim-string line)
                          :unless (blank-string-p trimmed)
                            :return trimmed)
                    node))
         (next-target
           (let ((value (info-header-value header "next")))
             (and value
                  (parse-info-target-spec value location))))
         (prev-target
           (let ((value (info-header-value header "prev")))
             (and value
                  (parse-info-target-spec value location))))
         (up-target
           (let ((value (info-header-value header "up")))
             (and value
                  (parse-info-target-spec value location))))
         (top-target
           (info-location-for-manual manual
                                     :node "Top"
                                     :current-location location)))
    (multiple-value-bind (display-lines links)
        (info-build-display-model location
                                  (info-node-record-body-lines record)
                                  next-target prev-target up-target top-target)
      (make-info-document :manual manual
                          :node node
                          :title title
                          :header-file (info-header-value header "file")
                          :next-target next-target
                          :prev-target prev-target
                          :up-target up-target
                          :top-target top-target
                          :lines display-lines
                          :links links))))

(defun info-fetch-location (location)
  "Return an Info document for LOCATION."
  (case (info-location-source-kind location)
    (:synthetic
     (synthetic-rplaca-info-document location))
    (otherwise
     (let ((record (info-find-node-record location)))
       (cond
         ((null (info-load-manual-data location))
          (info-error-document
           location
           (format nil "Info manual ~A is not installed."
                   (info-location-manual location))
           :detail "No matching Info files were found on the current system."))
         ((null record)
          (info-error-document
           location
           (format nil "Node ~A was not found in manual ~A."
                   (info-location-node location)
                   (info-location-manual location))
           :detail "Use `g` to jump to another node or open the directory with C-h i."))
         (t
          (info-node-record-document location record)))))))

(defun info-document-display-text (document)
  "Return DOCUMENT as plain text."
  (with-output-to-string (stream)
    (loop :for line :in (info-document-lines document)
          :for first-line-p := t :then nil
          :do (unless first-line-p
                (terpri stream))
              (dolist (segment line)
                (write-string (info-segment-text segment) stream)))))

(defun info-reset-link-selection (state)
  "Reset STATE's selected link to the first available link, or NIL."
  (let* ((document (info-state-document state))
         (links (and document (info-document-links document))))
    (setf (info-state-selected-link-index state)
          (and links
               (plusp (length links))
               (or (position-if (lambda (link)
                                  (not (eq :nav (info-link-kind link))))
                                links)
                   0))))
  state)

(defun info-visit-location (buf location &key (push-history-p t)
                                         (clear-forward-p t))
  "Load LOCATION into BUF and refresh its display."
  (let* ((state (info-buffer-state buf))
         (current (info-state-location state)))
    (when (and push-history-p
               current
               (not (info-location= current location)))
      (setf (info-state-history state)
            (cons (copy-info-location current)
                  (info-state-history state))))
    (when clear-forward-p
      (setf (info-state-forward-history state) nil))
    (setf (info-state-location state) (copy-info-location location)
          (info-state-document state) (info-fetch-location location))
    (info-reset-link-selection state)
    (info-populate-buffer-text buf
                               (info-document-display-text
                                (info-state-document state)))
    buf))

(defun make-info-buffer (&key (name *info-buffer-name*)
                              (location (info-directory-location))
                              (working-directory (truename "."))
                              (add-to-ring-p t))
  "Create a new Info/manual browser buffer."
  (let ((buf (make-buffer name
                          :agent-name "info"
                          :kind :info
                          :working-directory (uiop:ensure-directory-pathname
                                              working-directory)
                          :session-persistence-mode :ephemeral
                          :major-mode "info")))
    (initialize-buffer-display-defaults buf)
    (setf (gethash buf *info-buffer-states*)
          (make-info-state))
    (info-visit-location buf location :push-history-p nil :clear-forward-p t)
    (when add-to-ring-p
      (add-buffer-to-ring buf))
    buf))

(defun show-info-location (location &key (buffer-name *info-buffer-name*))
  "Display LOCATION in a reusable Info buffer."
  (let ((existing (find-buffer-by-name buffer-name)))
    (if (and existing (info-buffer-p existing))
        (progn
          (info-visit-location existing location :push-history-p t :clear-forward-p t)
          (switch-to-buffer existing))
        (switch-to-buffer
         (make-info-buffer :name buffer-name
                           :location location
                           :add-to-ring-p t)))))

(defun info-current-document (buf)
  "Return BUF's current Info document, or NIL."
  (let ((state (and buf (info-buffer-state buf))))
    (and state (info-state-document state))))

(defun info-current-link (buf)
  "Return BUF's currently selected Info link, or NIL."
  (let* ((state (info-buffer-state buf))
         (document (info-state-document state))
         (index (info-state-selected-link-index state)))
    (and document
         index
         (nth index (info-document-links document)))))

(defun info-select-next-link (buf)
  "Advance BUF's selected link, wrapping at the end."
  (let* ((state (info-buffer-state buf))
         (links (and (info-state-document state)
                     (info-document-links (info-state-document state)))))
    (when links
      (setf (info-state-selected-link-index state)
            (mod (1+ (or (info-state-selected-link-index state) -1))
                 (length links)))
      (notify-buffer-display-change buf :info-link)
      t)))

(defun info-select-previous-link (buf)
  "Move BUF's selected link backward, wrapping at the start."
  (let* ((state (info-buffer-state buf))
         (links (and (info-state-document state)
                     (info-document-links (info-state-document state)))))
    (when links
      (setf (info-state-selected-link-index state)
            (mod (1- (or (info-state-selected-link-index state) 0))
                 (length links)))
      (notify-buffer-display-change buf :info-link)
      t)))

(defun info-follow-link (buf link)
  "Follow LINK inside BUF."
  (when link
    (info-visit-location buf (info-link-target link)
                         :push-history-p t
                         :clear-forward-p t)
    :redraw))

(defun info-follow-selected-link (buf)
  "Follow BUF's currently selected link."
  (let ((link (info-current-link buf)))
    (when link
      (info-follow-link buf link))))

(defun info-visit-document-target (buf accessor)
  "Visit BUF's current document target returned by ACCESSOR."
  (let* ((document (info-current-document buf))
         (target (and document (funcall accessor document))))
    (when target
      (info-visit-location buf target :push-history-p t :clear-forward-p t)
      :redraw)))

(defun info-go-next-node (buf)
  "Visit BUF's Next node when one exists."
  (info-visit-document-target buf #'info-document-next-target))

(defun info-go-prev-node (buf)
  "Visit BUF's Prev node when one exists."
  (info-visit-document-target buf #'info-document-prev-target))

(defun info-go-up-node (buf)
  "Visit BUF's Up node when one exists."
  (info-visit-document-target buf #'info-document-up-target))

(defun info-go-top-node (buf)
  "Visit BUF's Top node."
  (info-visit-document-target buf #'info-document-top-target))

(defun info-go-directory (buf)
  "Visit the top-level Info directory."
  (info-visit-location buf (info-directory-location)
                       :push-history-p t
                       :clear-forward-p t)
  :redraw)

(defun info-go-back (buf)
  "Visit BUF's previous Info location from history."
  (let* ((state (info-buffer-state buf))
         (history (info-state-history state))
         (current (info-state-location state)))
    (when history
      (setf (info-state-history state) (rest history))
      (when current
        (setf (info-state-forward-history state)
              (cons (copy-info-location current)
                    (info-state-forward-history state))))
      (info-visit-location buf (first history)
                           :push-history-p nil
                           :clear-forward-p nil)
      :redraw)))

(defun info-go-forward (buf)
  "Visit BUF's forward Info history when present."
  (let* ((state (info-buffer-state buf))
         (forward (info-state-forward-history state))
         (current (info-state-location state)))
    (when forward
      (setf (info-state-forward-history state) (rest forward))
      (when current
        (setf (info-state-history state)
              (cons (copy-info-location current)
                    (info-state-history state))))
      (info-visit-location buf (first forward)
                           :push-history-p nil
                           :clear-forward-p nil)
      :redraw)))

(defun info-directory-command (buffer)
  "Open the top-level Info directory from the system."
  (declare (ignore buffer))
  (show-info-location (info-directory-location)))
(defcommand info-directory-command)

(defun info-open-manual-command (buffer manual)
  "Open a system Info manual or explicit `(manual)Node` location."
  (declare (ignore buffer))
  (show-info-location (parse-info-open-designator manual)))
(defcommand info-open-manual-command
  :prompts ((manual :prompt "Info manual or (manual)node")))

(defun rplaca-manual-command (buffer)
  "Open the RPLACA manual, falling back to a placeholder when absent."
  (declare (ignore buffer))
  (show-info-location (resolve-rplaca-info-location "Top")))
(defcommand rplaca-manual-command)

(defun info-goto-node-command (buffer target)
  "In an Info buffer, visit TARGET in the current manual or explicit manual."
  (unless (info-buffer-p buffer)
    (error "Current buffer is not an Info buffer."))
  (info-visit-location buffer
                       (parse-info-goto-designator
                        target
                        (or (info-state-location (info-buffer-state buffer))
                            (info-directory-location)))
                       :push-history-p t
                       :clear-forward-p t)
  :redraw)
(defcommand info-goto-node-command
  :prompts ((target :prompt "Info node or (manual)node")))

(register-buffer-type
 :info
 :description "Read-only Texinfo/Info manual browser buffer."
 :major-mode "info"
 :presentation-function nil
 :input-presentation-function nil
 :serialize-state-function 'info-serialize-buffer-state
 :restore-state-function 'info-restore-buffer-state)
