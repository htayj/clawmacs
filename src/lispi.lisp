(in-package :lispi)

;;; --------------------------------------------------------------------------
;;; Configuration
;;; --------------------------------------------------------------------------

(defvar *sandbox-root* nil
  "When non-nil, restricts file operations to this directory subtree.")

(defvar *file-read-default-limit* 2000
  "Default maximum lines returned by the read tool.")

(defvar *find-default-limit* 1000
  "Default maximum file paths returned by the find tool.")

(defvar *grep-default-limit* 100
  "Default maximum line matches returned by the grep tool.")

(defvar *grep-max-file-bytes* 1048576
  "Maximum file size searched by grep.")

(defvar *grep-max-line-length* 500
  "Maximum characters displayed for each grep match line.")

(defvar *search-ignored-directory-names*
  '(".git" ".hg" ".svn" ".direnv" ".cache" "node_modules" "target"
    "dist" "build" "__pycache__")
  "Directory names skipped by find and grep traversal.")

(defvar *diff-display-max-lines* 40
  "Maximum diff lines shown in approval displays.")

(defvar *lisp-eval-default-package* "CL-USER"
  "Default package used by lisp_eval when :package is omitted.")

;;; --------------------------------------------------------------------------
;;; Lisp Data
;;; --------------------------------------------------------------------------

(defun lisp-data-string (object)
  "Return OBJECT as a printed Lisp data string."
  (with-output-to-string (stream)
    (let ((*print-case* :downcase)
          (*print-circle* t)
          (*print-escape* t)
          (*print-pretty* t)
          (*print-readably* nil))
      (write object :stream stream))))

(defun lisp-data-read (string)
  "Read one Lisp data form from STRING with read-time evaluation disabled."
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun tool-error-result-data (condition)
  "Return a Lisp data payload string for a failed tool execution."
  (lisp-data-string (list :error (format nil "~A" condition))))

(defun tool-denied-result-data (reason)
  "Return a Lisp data payload string for a denied tool execution."
  (lisp-data-string (list :denied t
                          :reason (or reason "User denied"))))

;;; --------------------------------------------------------------------------
;;; Tool Argument Helpers
;;; --------------------------------------------------------------------------

(defun tool-key-name (key)
  "Return KEY as a lowercase Lisp data key name."
  (cond
    ((keywordp key) (string-downcase (symbol-name key)))
    ((symbolp key) (string-downcase (symbol-name key)))
    ((stringp key) (string-downcase key))
    (t (format nil "~(~A~)" key))))

(defun tool-key= (left right)
  "Return true when LEFT and RIGHT name the same Lisp data key."
  (string= (tool-key-name left) (tool-key-name right)))

(defun tool-plist-p (args)
  "Return true when ARGS looks like a keyword plist."
  (and (listp args)
       (or (null args)
           (keywordp (first args)))))

(defun tool-args-alist (args)
  "Normalize ALIST or keyword PLIST tool ARGS into an alist."
  (cond
    ((null args) nil)
    ((tool-plist-p args)
     (loop :for (key value) :on args :by #'cddr
           :collect (cons key value)))
    ((listp args) args)
    (t
     (error "Tool arguments must be a Lisp alist or keyword plist, got ~S"
            args))))

(defun tool-arg (args &rest keys)
  "Return the first value in Lisp data ARGS associated with one of KEYS."
  (loop :for (key . value) :in (tool-args-alist args)
        :when (member key keys :test #'tool-key=)
          :return value))

;;; --------------------------------------------------------------------------
;;; File Helpers
;;; --------------------------------------------------------------------------

(defun string-prefix-p (prefix string)
  "Return true when STRING starts with PREFIX."
  (let ((prefix-length (length prefix))
        (string-length (length string)))
    (and (<= prefix-length string-length)
         (string= prefix string
                  :start1 0
                  :end1 prefix-length
                  :start2 0
                  :end2 prefix-length))))

(defun count-substring-occurrences (needle haystack &key (start 0) end
                                                        (test #'char=))
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (let* ((needle (string needle))
         (haystack (string haystack))
         (needle-length (length needle))
         (limit (or end (length haystack))))
    (unless (plusp needle-length)
      (error "NEEDLE must not be empty."))
    (loop :with position := start
          :for match := (search needle haystack
                                :start2 position
                                :end2 limit
                                :test test)
          :while match
          :count t
          :do (setf position (+ match needle-length)))))

(defun split-lines (text)
  "Split TEXT into lines without retaining newline characters."
  (loop :with len := (length text)
        :for start := 0 :then (1+ pos)
        :for pos := (position #\Newline text :start start)
        :collect (subseq text start (or pos len))
        :while pos))

(defun join-lines (lines)
  "Join LINES with newline separators."
  (format nil "~{~A~^~%~}" lines))

(defun utf-8-byte-length (text)
  "Return TEXT's encoded length in UTF-8 octets."
  (length (flexi-streams:string-to-octets text :external-format :utf-8)))

(defun lisp-whitespace-char-p (char)
  "Return true when CHAR is Common Lisp source whitespace."
  (member char '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun lisp-token-delimiter-char-p (char)
  "Return true when CHAR terminates an unescaped Lisp token."
  (or (lisp-whitespace-char-p char)
      (member char '(#\( #\) #\" #\; #\' #\` #\,) :test #'char=)))

(defun starts-with-at-p (text start needle)
  "Return true when TEXT contains NEEDLE at START."
  (let ((end (+ start (length needle))))
    (and (<= end (length text))
         (string= needle text :start2 start :end2 end))))

(defun balanced-parentheses-diagnostic (text)
  "Return NIL when TEXT has balanced Lisp parentheses, otherwise a diagnostic."
  (let ((length (length text))
        (stack nil))
    (labels ((diagnostic (kind position message)
               (list :kind kind :position position :message message))
             (scan-string-end (index)
               (let ((pos (1+ index))
                     (escaped nil))
                 (loop :while (< pos length)
                       :for char := (char text pos)
                       :do (cond
                             (escaped
                              (setf escaped nil)
                              (incf pos))
                             ((char= char #\\)
                              (setf escaped t)
                              (incf pos))
                             ((char= char #\")
                              (return (values nil (1+ pos))))
                             (t
                              (incf pos)))
                       :finally
                          (return
                            (values
                             (diagnostic :unterminated-string
                                         index
                                         "Unterminated string literal.")
                             length)))))
             (scan-bar-token-end (index)
               (let ((pos (1+ index))
                     (escaped nil))
                 (loop :while (< pos length)
                       :for char := (char text pos)
                       :do (cond
                             (escaped
                              (setf escaped nil)
                              (incf pos))
                             ((char= char #\\)
                              (setf escaped t)
                              (incf pos))
                             ((char= char #\|)
                              (return (values nil (1+ pos))))
                             (t
                              (incf pos)))
                       :finally
                          (return
                            (values
                             (diagnostic :unterminated-token-escape
                                         index
                                         "Unterminated |...| token escape.")
                             length)))))
             (scan-character-literal-end (index)
               (let ((body-start (+ index 2)))
                 (cond
                   ((>= body-start length)
                    length)
                   ((lisp-token-delimiter-char-p (char text body-start))
                    (1+ body-start))
                   (t
                    (loop :for pos :from body-start :below length
                          :for char := (char text pos)
                          :when (lisp-token-delimiter-char-p char)
                            :return pos
                          :finally (return length))))))
             (scan-block-comment-end (index)
               (let ((depth 1)
                     (pos (+ index 2)))
                 (loop :while (< pos length)
                       :do (cond
                             ((starts-with-at-p text pos "#|")
                              (incf depth)
                              (incf pos 2))
                             ((starts-with-at-p text pos "|#")
                              (decf depth)
                              (incf pos 2)
                              (when (zerop depth)
                                (return (values nil pos))))
                             (t
                              (incf pos)))
                       :finally
                          (return
                            (values
                             (diagnostic :unterminated-block-comment
                                         index
                                         "Unterminated #| block comment.")
                             length)))))
             (scan-line-comment-end (index)
               (let ((newline (position #\Newline text :start index)))
                 (if newline (1+ newline) length))))
      (loop :for pos := 0 :then next-pos
            :while (< pos length)
            :for char := (char text pos)
            :for next-pos := (cond
                               ((lisp-whitespace-char-p char)
                                (1+ pos))
                               ((char= char #\;)
                                (scan-line-comment-end pos))
                               ((starts-with-at-p text pos "#|")
                                (multiple-value-bind (diagnostic next)
                                    (scan-block-comment-end pos)
                                  (when diagnostic
                                    (return diagnostic))
                                  next))
                               ((starts-with-at-p text pos "#\\")
                                (scan-character-literal-end pos))
                               ((char= char #\")
                                (multiple-value-bind (diagnostic next)
                                    (scan-string-end pos)
                                  (when diagnostic
                                    (return diagnostic))
                                  next))
                               ((char= char #\|)
                                (multiple-value-bind (diagnostic next)
                                    (scan-bar-token-end pos)
                                  (when diagnostic
                                    (return diagnostic))
                                  next))
                               ((char= char #\\)
                                (min length (+ pos 2)))
                               ((char= char #\()
                                (push pos stack)
                                (1+ pos))
                               ((char= char #\))
                                (if stack
                                    (progn
                                      (pop stack)
                                      (1+ pos))
                                    (return
                                      (diagnostic :unexpected-close-paren
                                                  pos
                                                  "Unexpected closing parenthesis."))))
                               (t
                                (1+ pos)))
            :finally
               (return
                 (when stack
                   (diagnostic :missing-close-paren
                               (first stack)
                               "Missing closing parenthesis.")))))))

(defun balanced-parentheses-p (text)
  "Return true when TEXT has balanced Lisp parentheses."
  (null (balanced-parentheses-diagnostic text)))

(defun assert-balanced-parentheses (text context)
  "Signal an error if TEXT has unbalanced Lisp parentheses."
  (let ((diagnostic (balanced-parentheses-diagnostic text)))
    (when diagnostic
      (error "~A would leave unbalanced parentheses: ~A at position ~D"
             context
             (getf diagnostic :message)
             (getf diagnostic :position))))
  text)

(defun validate-sandbox-path (path)
  "Validate that PATH is within *SANDBOX-ROOT*. Returns the resolved pathname.
Signals an error if the path escapes the sandbox.
Works for both existing and not-yet-existing files."
  (let* ((sandbox (or *sandbox-root* (truename ".")))
         (resolved (merge-pathnames (pathname path) sandbox))
         (dir (make-pathname :directory (pathname-directory resolved)
                             :device (pathname-device resolved)))
         (dir-str (namestring (if (probe-file dir)
                                  (truename dir)
                                  dir)))
         (sandbox-str (namestring sandbox)))
    (unless (string-prefix-p sandbox-str dir-str)
      (error "Path ~A is outside the sandbox (~A)" path sandbox-str))
    resolved))

(defun sandbox-root-pathname ()
  "Return the effective sandbox root as a directory pathname."
  (uiop:ensure-directory-pathname (or *sandbox-root* (truename "."))))

(defun sort-pathnames (pathnames)
  "Return PATHNAMES sorted by namestring."
  (sort (copy-list pathnames) #'string< :key #'namestring))

(defun ignored-search-directory-p (directory)
  "Return true when DIRECTORY should be skipped by search tools."
  (let* ((components (pathname-directory (uiop:ensure-directory-pathname directory)))
         (name (car (last components))))
    (member name *search-ignored-directory-names* :test #'string=)))

(defun resolve-search-root (path)
  "Resolve PATH to an existing file or directory inside the sandbox."
  (let* ((resolved (if (or (null path)
                           (and (stringp path)
                                (or (string= path "")
                                    (string= path "."))))
                       (sandbox-root-pathname)
                       (validate-sandbox-path path)))
         (directory (uiop:directory-exists-p resolved))
         (file (and (null directory) (probe-file resolved))))
    (or directory
        file
        (error "Path not found: ~A" (or path ".")))))

(defun sandbox-relative-namestring (pathname)
  "Return PATHNAME relative to the effective sandbox root when possible."
  (let* ((root (namestring (sandbox-root-pathname)))
         (full (namestring pathname)))
    (if (string-prefix-p root full)
        (subseq full (length root))
        full)))

(defun pathname-file-name (pathname)
  "Return PATHNAME's filename component as a string."
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (cond
      ((and name type)
       (format nil "~A.~A" name type))
      (name
       (string name))
      (type
       (string type))
      (t
       (namestring pathname)))))

(defun wildcard-pattern-p (pattern)
  "Return true when PATTERN contains simple glob wildcards."
  (or (position #\* pattern)
      (position #\? pattern)))

(defun wildcard-match-p (pattern string &key ignore-case)
  "Return true when STRING matches PATTERN with * and ? wildcards."
  (let* ((pattern (if ignore-case (string-upcase pattern) pattern))
         (string (if ignore-case (string-upcase string) string))
         (pattern-length (length pattern))
         (string-length (length string))
         (memo (make-hash-table :test #'equal)))
    (labels ((match (pattern-index string-index)
               (let ((key (cons pattern-index string-index)))
                 (multiple-value-bind (cached present-p) (gethash key memo)
                   (if present-p
                       cached
                       (setf (gethash key memo)
                             (cond
                               ((= pattern-index pattern-length)
                                (= string-index string-length))
                               ((char= (char pattern pattern-index) #\*)
                                (or (match (1+ pattern-index) string-index)
                                    (and (< string-index string-length)
                                         (match pattern-index
                                                (1+ string-index)))))
                               ((and (< string-index string-length)
                                     (or (char= (char pattern pattern-index) #\?)
                                         (char= (char pattern pattern-index)
                                                (char string string-index))))
                                (match (1+ pattern-index)
                                       (1+ string-index)))
                               (t nil))))))))
      (match 0 0))))

(defun pattern-match-p (pattern candidate &key ignore-case)
  "Return true when PATTERN matches CANDIDATE by wildcard or substring."
  (if (wildcard-pattern-p pattern)
      (wildcard-match-p pattern candidate :ignore-case ignore-case)
      (not (null (search pattern
                         candidate
                         :test (if ignore-case #'char-equal #'char=))))))

(defun file-pattern-match-p (pattern pathname &key ignore-case)
  "Return true when PATTERN matches PATHNAME's relative path or filename."
  (let ((relative (sandbox-relative-namestring pathname))
        (name (pathname-file-name pathname)))
    (or (pattern-match-p pattern relative :ignore-case ignore-case)
        (pattern-match-p pattern name :ignore-case ignore-case))))

(defun map-search-files (root function)
  "Call FUNCTION for each file under ROOT. Stop when FUNCTION returns :STOP."
  (block done
    (labels ((walk (path)
               (cond
                 ((uiop:directory-exists-p path)
                  (let ((dir (uiop:ensure-directory-pathname path)))
                    (unless (ignored-search-directory-p dir)
                      (dolist (file (sort-pathnames
                                     (or (ignore-errors
                                           (uiop:directory-files dir))
                                         nil)))
                        (when (eq :stop (funcall function file))
                          (return-from done t)))
                      (dolist (subdir (sort-pathnames
                                       (or (ignore-errors
                                             (uiop:subdirectories dir))
                                           nil)))
                        (walk subdir)))))
                 ((probe-file path)
                  (when (eq :stop (funcall function path))
                    (return-from done t))))))
      (walk root)
      nil)))

(defun file-byte-length (pathname)
  "Return PATHNAME length in octets, or NIL if it cannot be read."
  (handler-case
      (with-open-file (stream pathname
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (file-length stream))
    (error () nil)))

(defun readable-grep-file-string (pathname)
  "Return PATHNAME content when it is small enough and readable as text."
  (let ((length (file-byte-length pathname)))
    (when (and length
               (<= length *grep-max-file-bytes*))
      (handler-case
          (uiop:read-file-string pathname)
        (error () nil)))))

(defun truncate-grep-line (line)
  "Return LINE truncated for grep display."
  (if (> (length line) *grep-max-line-length*)
      (concatenate 'string
                   (subseq line 0 *grep-max-line-length*)
                   "...")
      line))

(defun compute-simple-diff (old-text new-text)
  "Compute a simple line-by-line diff between OLD-TEXT and NEW-TEXT."
  (let ((old-lines (if old-text
                       (loop :for start := 0 :then (1+ pos)
                             :for pos := (position #\Newline old-text :start start)
                             :collect (subseq old-text start (or pos (length old-text)))
                             :while pos)
                       nil))
        (new-lines (loop :for start := 0 :then (1+ pos)
                         :for pos := (position #\Newline new-text :start start)
                         :collect (subseq new-text start (or pos (length new-text)))
                         :while pos)))
    (with-output-to-string (s)
      (cond
        ((null old-lines)
         (format s "--- (new file)~%+++ ~A~%" "new")
         (dolist (line new-lines)
           (format s "+~A~%" line)))
        (t
         (format s "--- old~%+++ new~%")
         (let ((max-lines *diff-display-max-lines*)
               (old-set (make-hash-table :test #'equal))
               (new-set (make-hash-table :test #'equal)))
           (dolist (line old-lines)
             (setf (gethash line old-set) t))
           (dolist (line new-lines)
             (setf (gethash line new-set) t))
           (let ((removed (remove-if (lambda (line)
                                       (gethash line new-set))
                                     old-lines)))
             (dolist (line (subseq removed 0 (min (length removed) max-lines)))
               (format s "-~A~%" line)))
           (let ((added (remove-if (lambda (line)
                                     (gethash line old-set))
                                   new-lines)))
             (dolist (line (subseq added 0 (min (length added) max-lines)))
               (format s "+~A~%" line)))))))))

;;; --------------------------------------------------------------------------
;;; File Tools
;;; --------------------------------------------------------------------------

(defun execute-read (args)
  "Read a file within the sandbox and return plain text contents."
  (let* ((path (tool-arg args :path))
         (offset (or (tool-arg args :offset) 1))
         (limit (or (tool-arg args :limit) *file-read-default-limit*)))
    (unless path
      (error "path parameter is required"))
    (unless (and (integerp offset) (plusp offset))
      (error "offset must be a positive 1-indexed line number"))
    (unless (and (integerp limit) (plusp limit))
      (error "limit must be a positive number of lines"))
    (let ((resolved (validate-sandbox-path path)))
      (unless (probe-file resolved)
        (error "File not found: ~A" path))
      (let* ((content (uiop:read-file-string resolved))
             (lines (split-lines content))
             (total-lines (length lines))
             (start-index (1- offset)))
        (when (>= start-index total-lines)
          (error "Offset ~D is beyond end of file (~D lines total)"
                 offset total-lines))
        (let* ((end-index (min (+ start-index limit) total-lines))
               (selected (subseq lines start-index end-index))
               (result (join-lines selected)))
          (if (< end-index total-lines)
              (format nil "~A~%~%[Showing lines ~D-~D of ~D. Use offset=~D to continue.]"
                      result
                      offset
                      end-index
                      total-lines
                      (1+ end-index))
              result))))))

(defun execute-find (args)
  "Search for files by name or glob pattern within the sandbox."
  (let* ((pattern (tool-arg args :pattern :query))
         (path (tool-arg args :path))
         (limit (or (tool-arg args :limit) *find-default-limit*))
         (ignore-case (tool-arg args :ignore-case :ignorecase :ignore--case)))
    (unless (and pattern (plusp (length pattern)))
      (error "pattern parameter is required"))
    (unless (and (integerp limit) (plusp limit))
      (error "limit must be a positive number of results"))
    (let ((root (resolve-search-root path))
          (matches nil)
          (truncated nil))
      (map-search-files
       root
       (lambda (file)
         (when (file-pattern-match-p pattern file :ignore-case ignore-case)
           (push (sandbox-relative-namestring file) matches)
           (when (>= (length matches) limit)
             (setf truncated t)
             :stop))))
      (setf matches (nreverse matches))
      (cond
        ((null matches)
         (format nil "No files found matching ~S in ~A."
                 pattern
                 (or path ".")))
        (truncated
         (format nil "~{~A~^~%~}~%~%[Showing first ~D matching files. Narrow :pattern or raise :limit to continue.]"
                 matches
                 limit))
        (t
         (format nil "~{~A~^~%~}" matches))))))

(defun execute-grep (args)
  "Search file contents for a literal pattern within the sandbox."
  (let* ((pattern (tool-arg args :pattern :query))
         (path (tool-arg args :path))
         (glob (tool-arg args :glob))
         (limit (or (tool-arg args :limit) *grep-default-limit*))
         (ignore-case (tool-arg args :ignore-case :ignorecase :ignore--case)))
    (unless (and pattern (plusp (length pattern)))
      (error "pattern parameter is required"))
    (unless (and (integerp limit) (plusp limit))
      (error "limit must be a positive number of matches"))
    (let ((root (resolve-search-root path))
          (matches nil)
          (truncated nil))
      (map-search-files
       root
       (lambda (file)
         (when (or (null glob)
                   (file-pattern-match-p glob file :ignore-case ignore-case))
           (let ((content (readable-grep-file-string file)))
             (when content
               (loop :for line :in (split-lines content)
                     :for line-number :from 1
                     :when (search pattern
                                   line
                                   :test (if ignore-case
                                             #'char-equal
                                             #'char=))
                       :do (push (format nil "~A:~D:~A"
                                         (sandbox-relative-namestring file)
                                         line-number
                                         (truncate-grep-line line))
                                 matches)
                           (when (>= (length matches) limit)
                             (setf truncated t)
                             (return :stop))))))))
      (setf matches (nreverse matches))
      (cond
        ((null matches)
         (format nil "No matches for ~S in ~A."
                 pattern
                 (or path ".")))
        (truncated
         (format nil "~{~A~^~%~}~%~%[Showing first ~D matches. Narrow :pattern, :path, or :glob, or raise :limit to continue.]"
                 matches
                 limit))
        (t
         (format nil "~{~A~^~%~}" matches))))))

(defun execute-write (args)
  "Create or overwrite a file within the sandbox and return plain text."
  (let ((path (tool-arg args :path))
        (content (tool-arg args :content)))
    (unless path
      (error "path parameter is required"))
    (when (null content)
      (error "content parameter is required"))
    (assert-balanced-parentheses content "write")
    (let ((resolved (validate-sandbox-path path)))
      (ensure-directories-exist resolved)
      (with-open-file (s resolved
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :external-format :utf-8)
        (write-string content s))
      (format nil "Successfully wrote ~D bytes to ~A"
              (utf-8-byte-length content)
              path))))

(defun file-write-approval-display (args)
  "Approval display for write: shows what will be created or overwritten."
  (handler-case
      (let* ((path (tool-arg args :path))
             (new-content (tool-arg args :content))
             (sandbox (or *sandbox-root* (truename ".")))
             (resolved (merge-pathnames (pathname path) sandbox)))
        (when path
          (if (probe-file resolved)
              (compute-simple-diff (uiop:read-file-string resolved)
                                   (or new-content ""))
              (format nil "--- CREATING new file: ~A ---~%~{+~A~^~%~}"
                      path
                      (split-lines (or new-content ""))))))
    (error () nil)))

(defun execute-edit (args)
  "Edit a file by replacing one exact occurrence of :old-text with :new-text."
  (let ((path (tool-arg args :path))
        (old-text (tool-arg args :old-text :oldtext :old--text))
        (new-text (tool-arg args :new-text :newtext :new--text)))
    (unless path
      (error "path parameter is required"))
    (when (or (null old-text)
              (zerop (length old-text)))
      (error ":old-text parameter is required and must not be empty"))
    (when (null new-text)
      (error ":new-text parameter is required (use empty string to delete)"))
    (let ((resolved (validate-sandbox-path path)))
      (unless (probe-file resolved)
        (error "File not found: ~A" path))
      (let* ((content (uiop:read-file-string resolved))
             (pos (search old-text content))
             (count (count-substring-occurrences old-text content)))
        (unless pos
          (error ":old-text not found in ~A" path))
        (when (> count 1)
          (error ":old-text found ~A times in ~A (must be unique). Provide more context."
                 count path))
        (let ((new-content (concatenate 'string
                                        (subseq content 0 pos)
                                        new-text
                                        (subseq content (+ pos (length old-text)))))
              (diff nil))
          (assert-balanced-parentheses new-content "edit")
          (setf diff (compute-simple-diff content new-content))
          (with-open-file (s resolved
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :external-format :utf-8)
            (write-string new-content s))
          (format nil "Successfully replaced text in ~A.~%~A"
                  path
                  diff))))))

(defun file-edit-approval-display (args)
  "Approval display for edit: shows diff of the replacement."
  (handler-case
      (let* ((path (tool-arg args :path))
             (old-text (tool-arg args :old-text :oldtext :old--text))
             (new-text (tool-arg args :new-text :newtext :new--text))
             (sandbox (or *sandbox-root* (truename ".")))
             (resolved (merge-pathnames (pathname path) sandbox)))
        (when (and path old-text new-text (probe-file resolved))
          (let* ((content (uiop:read-file-string resolved))
                 (pos (search old-text content)))
            (when pos
              (let ((new-content (concatenate 'string
                                              (subseq content 0 pos)
                                              new-text
                                              (subseq content (+ pos (length old-text))))))
                (compute-simple-diff content new-content))))))
    (error () nil)))

;;; --------------------------------------------------------------------------
;;; Lisp Eval Tool
;;; --------------------------------------------------------------------------

(defstruct lisp-eval-record
  "One captured lisp_eval execution record."
  code
  package
  result
  output
  error-output
  condition
  timestamp)

(defvar *last-eval-result* nil
  "Multiple-value list from the last successful lisp_eval execution.")

(defvar *last-eval-condition* nil
  "Condition object from the last failed lisp_eval execution, or NIL.")

(defvar *lisp-eval-history* nil
  "Newest-first list of captured lisp_eval execution records.")

(defvar *lisp-eval-history-limit* 50
  "Maximum number of lisp_eval records retained in memory.")

(defvar *lisp-eval-max-output-chars* 20000
  "Maximum characters retained per lisp_eval result/output/error-output field.")

(defun truncate-lisp-eval-text (text)
  "Return values TRUNCATED-TEXT and TRUNCATED-P for TEXT."
  (let ((string (or text "")))
    (if (> (length string) *lisp-eval-max-output-chars*)
        (values (concatenate 'string
                             (subseq string 0 *lisp-eval-max-output-chars*)
                             (format nil "~%[truncated at ~D characters]"
                                     *lisp-eval-max-output-chars*))
                t)
        (values string nil))))

(defun push-lisp-eval-record (record)
  "Push RECORD into *LISP-EVAL-HISTORY* and enforce the retention limit."
  (push record *lisp-eval-history*)
  (when (> (length *lisp-eval-history*) *lisp-eval-history-limit*)
    (setf *lisp-eval-history*
          (subseq *lisp-eval-history* 0 *lisp-eval-history-limit*)))
  record)

(defun lisp-eval-preview (value &optional (max-length 160))
  "Return a compact printed preview for VALUE."
  (let ((text (handler-case
                  (let ((*print-length* 20)
                        (*print-level* 4)
                        (*print-circle* t)
                        (*print-pretty* nil))
                    (prin1-to-string value))
                (error (condition)
                  (format nil "#<error printing value: ~A>" condition)))))
    (if (> (length text) max-length)
        (concatenate 'string (subseq text 0 max-length) "...")
        text)))

(defun eval-history-to-string (&key (limit 10))
  "Return a compact newest-first summary of lisp_eval history."
  (with-output-to-string (out)
    (if *lisp-eval-history*
        (loop :for record :in *lisp-eval-history*
              :for index :from 1
              :while (<= index limit)
              :do (format out "~D. package=~A time=~A~%   code: ~A~%"
                          index
                          (lisp-eval-record-package record)
                          (lisp-eval-record-timestamp record)
                          (lisp-eval-preview
                           (lisp-eval-record-code record)))
                  (if (lisp-eval-record-condition record)
                      (format out "   error: ~A~%"
                              (lisp-eval-record-condition record))
                      (format out "   result: ~A~%"
                              (lisp-eval-preview
                               (lisp-eval-record-result record))))
                  (when (plusp (length (or (lisp-eval-record-output record)
                                           "")))
                    (format out "   output: ~A~%"
                            (lisp-eval-preview
                             (lisp-eval-record-output record))))
                  (when (plusp (length (or (lisp-eval-record-error-output record)
                                           "")))
                    (format out "   error-output: ~A~%"
                            (lisp-eval-preview
                             (lisp-eval-record-error-output record)))))
        (format out "No lisp_eval history captured.~%"))))

(defun execute-lisp-eval (args)
  "Evaluate arbitrary Common Lisp code and return a printed Lisp data payload."
  (let* ((code (tool-arg args :code))
         (package-name (or (tool-arg args :package) *lisp-eval-default-package*)))
    (unless code
      (error "code parameter is required"))
    (let ((package (or (find-package (string-upcase package-name))
                       (find-package :cl-user)))
          (output-stream (make-string-output-stream))
          (error-output-stream (make-string-output-stream))
          (results nil)
          (result-output "")
          (condition-text nil)
          (truncated-fields nil))
      (handler-case
          (let ((*package* package)
                (*standard-output* output-stream)
                (*trace-output* error-output-stream)
                (*error-output* error-output-stream))
            (let ((form (read-from-string code)))
              (setf results (multiple-value-list (eval form))
                    *last-eval-result* results
                    *last-eval-condition* nil
                    result-output (format nil "~{~S~^~%~}" results))))
        (error (condition)
          (setf *last-eval-result* nil
                *last-eval-condition* condition
                condition-text (format nil "~A" condition))))
      (let ((output (get-output-stream-string output-stream))
            (error-output (get-output-stream-string error-output-stream)))
        (multiple-value-bind (result-text result-truncated-p)
            (truncate-lisp-eval-text result-output)
          (multiple-value-bind (output-text output-truncated-p)
              (truncate-lisp-eval-text output)
            (multiple-value-bind (error-output-text error-output-truncated-p)
                (truncate-lisp-eval-text error-output)
              (setf truncated-fields
                    (remove nil
                            (list (when result-truncated-p "result")
                                  (when output-truncated-p "output")
                                  (when error-output-truncated-p "error-output"))))
              (push-lisp-eval-record
               (make-lisp-eval-record :code code
                                      :package (package-name package)
                                      :result results
                                      :output output-text
                                      :error-output error-output-text
                                      :condition condition-text
                                      :timestamp (get-universal-time)))
              (lisp-data-string
               (append (list :code code
                             :package (package-name package)
                             :values (length results)
                             :result result-text
                             :output output-text
                             :error-output error-output-text
                             :truncated (mapcar (lambda (field)
                                                  (intern (string-upcase field)
                                                          :keyword))
                                                truncated-fields))
                       (when condition-text
                         (list :error condition-text)))))))))))

;;; --------------------------------------------------------------------------
;;; Tool Specs
;;; --------------------------------------------------------------------------

(defun default-tool-specs ()
  "Return Lisp data specs for the default lispi agent tools."
  (list
   (list
    :name "read"
    :description
    "Read the contents of a text file within the sandbox. Output is truncated to 2000 lines by default; use offset and limit to continue through large files."
    :schema
    `((:type . "object")
      (:properties
       . (("path" . ((:type . "string")
                     (:description . "Lisp data :path, relative to the sandbox or absolute within it.")))
          ("offset" . ((:type . "integer")
                       (:description . "Lisp data :offset, the 1-indexed line number to start reading from.")))
          ("limit" . ((:type . "integer")
                      (:description . "Lisp data :limit, the maximum number of lines to read.")))))
      (:required . #("path")))
   :permission :agent-allowed
   :execute-fn #'execute-read)
   (list
    :name "find"
    :description
    "Search for files by name or glob pattern within the sandbox. Returns matching file paths relative to the sandbox."
    :schema
    `((:type . "object")
      (:properties
       . (("pattern" . ((:type . "string")
                        (:description . "Lisp data :pattern, a filename substring or wildcard pattern such as *.lisp or src/*.lisp.")))
          ("path" . ((:type . "string")
                     (:description . "Lisp data :path, the directory or file to search. Default: sandbox root.")))
          ("limit" . ((:type . "integer")
                      (:description . "Lisp data :limit, the maximum number of file paths to return.")))
          ("ignore-case" . ((:type . "boolean")
                            (:description . "Lisp data :ignore-case, true for case-insensitive matching.")))))
      (:required . #("pattern")))
    :permission :agent-allowed
    :execute-fn #'execute-find)
   (list
    :name "grep"
    :description
    "Search file contents for a literal pattern within the sandbox. Returns matching lines with file paths and line numbers."
    :schema
    `((:type . "object")
      (:properties
       . (("pattern" . ((:type . "string")
                        (:description . "Lisp data :pattern, the literal text to search for.")))
          ("path" . ((:type . "string")
                     (:description . "Lisp data :path, the directory or file to search. Default: sandbox root.")))
          ("glob" . ((:type . "string")
                     (:description . "Lisp data :glob, optional wildcard pattern limiting searched files, such as *.lisp.")))
          ("ignore-case" . ((:type . "boolean")
                            (:description . "Lisp data :ignore-case, true for case-insensitive matching.")))
          ("limit" . ((:type . "integer")
                      (:description . "Lisp data :limit, the maximum number of matching lines to return.")))))
      (:required . #("pattern")))
    :permission :agent-allowed
    :execute-fn #'execute-grep)
   (list
    :name "write"
    :description
    "Create or overwrite a text file within the sandbox. Parent directories are created automatically."
    :schema
    `((:type . "object")
      (:properties
       . (("path" . ((:type . "string")
                     (:description . "Lisp data :path, relative to the sandbox or absolute within it.")))
          ("content" . ((:type . "string")
                        (:description . "Lisp data :content, the complete file content to write. Parentheses must be balanced.")))))
      (:required . #("path" "content")))
    :permission :agent-allowed
    :execute-fn #'execute-write
    :approval-display-fn #'file-write-approval-display)
   (list
    :name "edit"
    :description
    "Edit a text file within the sandbox by replacing one exact :old-text occurrence with :new-text."
    :schema
    `((:type . "object")
      (:properties
       . (("path" . ((:type . "string")
                     (:description . "Lisp data :path, relative to the sandbox or absolute within it.")))
          ("old-text" . ((:type . "string")
                         (:description . "Lisp data :old-text, the exact text to find and replace. Must occur exactly once.")))
          ("new-text" . ((:type . "string")
                         (:description . "Lisp data :new-text, the replacement text. Use an empty string to delete :old-text. The resulting file's parentheses must be balanced.")))))
      (:required . #("path" "old-text" "new-text")))
    :permission :agent-allowed
    :execute-fn #'execute-edit
    :approval-display-fn #'file-edit-approval-display)))
