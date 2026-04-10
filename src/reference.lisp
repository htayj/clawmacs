(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Common Lisp Standard Reference
;;; --------------------------------------------------------------------------

(defparameter *common-lisp-spec-site-url* "https://cl-community-spec.github.io/"
  "Base URL for the bundled CL Community Spec site.")

(defparameter *common-lisp-spec-index-path*
  (asdf:system-relative-pathname "clawmacs"
                                 #P"vendor/cl-community-spec/searchable_terms.json")
  "Path to the vendored CL Community Spec search index.")

(defparameter *common-lisp-spec-root*
  (uiop:ensure-directory-pathname
   (asdf:system-relative-pathname "clawmacs"
                                  #P"vendor/cl-community-spec/"))
  "Directory containing the vendored CL Community Spec snapshot.")

(defvar *common-lisp-spec-index-cache* nil
  "Memoized parsed CL Community Spec search index.")

(defvar *common-lisp-spec-index-cache-path* nil
  "Pathname for *common-lisp-spec-index-cache* so tests can safely rebind it.")

(defun string-prefix-equal-p (prefix string)
  "Return non-nil when STRING begins with PREFIX, case-insensitively."
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (string-equal prefix string
                       :end1 prefix-length
                       :end2 prefix-length))))

(defun string-contains-ci-p (needle haystack)
  "Return non-nil when HAYSTACK contains NEEDLE, case-insensitively."
  (and (plusp (length needle))
       (search needle haystack :test #'char-equal)))

(defun normalize-designator-string (designator)
  "Normalize DESIGNATOR to a printable string."
  (typecase designator
    (string designator)
    (symbol (symbol-name designator))
    (pathname (namestring designator))
    (t (prin1-to-string designator))))

(defun last-directory-component (pathname)
  "Return the last directory component of PATHNAME, or NIL."
  (let ((components (pathname-directory (uiop:ensure-directory-pathname pathname))))
    (car (last components))))

(defun skipped-doc-directory-p (pathname)
  "Return non-nil when PATHNAME should be skipped during doc scans."
  (let ((component (last-directory-component pathname)))
    (and (stringp component)
         (or (string-prefix-equal-p "." component)
             (member component
                     '("node_modules" "dist" "build" "__pycache__")
                     :test #'string=)))))

(defun text-doc-pathname-p (pathname)
  "Return non-nil when PATHNAME is likely to be a searchable text document."
  (let* ((name (string-downcase (or (pathname-name pathname) "")))
         (type (string-downcase (or (pathname-type pathname) "")))
         (full-name (if (plusp (length type))
                        (format nil "~A.~A" name type)
                        name)))
    (and (not (alexandria:ends-with-subseq "~" full-name))
         (or (member type '("asd" "lisp" "lsp" "cl" "md" "markdown" "org" "txt" "text")
                     :test #'string=)
             (string-prefix-equal-p "readme" full-name)
             (string-prefix-equal-p "changelog" full-name)
             (string-prefix-equal-p "license" full-name)))))

(defun collect-files-recursively (directory predicate)
  "Return files under DIRECTORY for which PREDICATE returns non-nil."
  (labels ((walk (dir)
             (nconc (loop :for file :in (uiop:directory-files dir)
                          :when (funcall predicate file)
                            :collect file)
                    (loop :for subdir :in (uiop:subdirectories dir)
                          :unless (skipped-doc-directory-p subdir)
                            :nconc (walk subdir)))))
    (walk (uiop:ensure-directory-pathname directory))))

(defun split-lines (string)
  "Split STRING into a list of lines."
  (loop :with len := (length string)
        :for start := 0 :then (1+ end)
        :for end := (position #\Newline string :start start)
        :collect (subseq string start (or end len))
        :while end))

(defun collapse-inline-whitespace (string)
  "Collapse repeated spaces and tabs in STRING."
  (with-output-to-string (out)
    (let ((pending-space nil)
          (wrote-anything nil))
      (loop :for char :across string
            :do (cond
                  ((member char '(#\Space #\Tab #\Return #\Page) :test #'char=)
                   (setf pending-space t))
                  (t
                   (when (and pending-space wrote-anything)
                     (write-char #\Space out))
                   (setf pending-space nil)
                   (write-char char out)
                   (setf wrote-anything t)))))))

(defun normalize-text-lines (string)
  "Trim STRING into readable plain text with collapsed blank lines."
  (with-output-to-string (out)
    (let ((saw-blank-line nil))
      (dolist (line (split-lines string))
        (let ((clean (string-trim '(#\Space #\Tab) (collapse-inline-whitespace line))))
          (cond
            ((zerop (length clean))
             (unless saw-blank-line
               (terpri out)
               (setf saw-blank-line t)))
            (t
             (write-string clean out)
             (terpri out)
             (setf saw-blank-line nil))))))))

(defun decode-html-entity (entity)
  "Decode a single HTML ENTITY name, or return NIL when unknown."
  (cond
    ((string= entity "amp") "&")
    ((string= entity "lt") "<")
    ((string= entity "gt") ">")
    ((string= entity "quot") "\"")
    ((or (string= entity "#39") (string= entity "apos")) "'")
    ((string= entity "nbsp") " ")
    ((and (> (length entity) 2)
          (char= (char entity 0) #\#)
          (char-equal (char entity 1) #\x))
     (let ((code (parse-integer entity :start 2 :radix 16 :junk-allowed t)))
       (when code
         (string (or (code-char code) #\?)))))
    ((and (> (length entity) 1)
          (char= (char entity 0) #\#))
     (let ((code (parse-integer entity :start 1 :junk-allowed t)))
       (when code
         (string (or (code-char code) #\?)))))
    (t nil)))

(defun decode-html-entities (string)
  "Decode a subset of HTML entities in STRING."
  (with-output-to-string (out)
    (loop :with length := (length string)
          :for index :from 0 :below length
          :for char := (char string index)
          :do (if (char= char #\&)
                  (let ((end (position #\; string :start index)))
                    (if end
                        (let* ((entity (subseq string (1+ index) end))
                               (decoded (decode-html-entity entity)))
                          (if decoded
                              (progn
                                (write-string decoded out)
                                (setf index end))
                              (write-char char out)))
                        (write-char char out)))
                  (write-char char out)))))

(defun html-title (html)
  "Extract the page title from HTML."
  (let ((start (search "<title>" html :test #'char-equal))
        (end (search "</title>" html :test #'char-equal)))
    (when (and start end (> end start))
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (decode-html-entities
                    (subseq html (+ start (length "<title>")) end))))))

(defun html-tag-name (tag)
  "Return two values: tag name and whether TAG is a closing tag."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) tag))
         (closingp (and (> (length trimmed) 0)
                        (char= (char trimmed 0) #\/)))
         (start (if closingp 1 0))
         (end (or (position-if (lambda (char)
                                 (member char '(#\Space #\Tab #\Newline #\Return #\/)
                                         :test #'char=))
                               trimmed
                               :start start)
                  (length trimmed))))
    (values (string-downcase (subseq trimmed start end)) closingp)))

(defun block-tag-p (tag-name)
  "Return non-nil when TAG-NAME should introduce a line break."
  (member tag-name
          '("div" "p" "section" "article" "header" "footer"
            "h1" "h2" "h3" "h4" "h5" "h6"
            "pre" "table" "tr" "ul" "ol" "li"
            "dl" "dt" "dd" "blockquote" "hr")
          :test #'string=))

(defun extract-content-fragment (html)
  "Return the main content fragment from a CL Community Spec HTML page."
  (let* ((body-start (or (search "<div class=\"section" html :test #'char-equal)
                         (search "<body" html :test #'char-equal)
                         0))
         (body-end (or (search "<script" html :start2 body-start :test #'char-equal)
                       (search "</body" html :start2 body-start :test #'char-equal)
                       (length html))))
    (subseq html body-start body-end)))

(defun html-to-text (html)
  "Convert a small subset of HTML into readable plain text."
  (let* ((fragment (extract-content-fragment html))
         (length (length fragment)))
    (labels ((emit-newline (stream)
               (write-char #\Newline stream)))
      (with-output-to-string (out)
        (loop :with index := 0
              :with ignore-tag := nil
              :while (< index length)
              :do (let ((char (char fragment index)))
                    (cond
                      ((char= char #\<)
                       (let ((end (or (position #\> fragment :start index)
                                      (1- length))))
                         (multiple-value-bind (tag-name closingp)
                             (html-tag-name (subseq fragment (1+ index) end))
                           (cond
                             (ignore-tag
                              (when (and closingp (string= tag-name ignore-tag))
                                (setf ignore-tag nil)))
                             ((member tag-name '("script" "style" "svg") :test #'string=)
                              (unless closingp
                                (setf ignore-tag tag-name)))
                             ((string= tag-name "br")
                              (emit-newline out))
                             ((and (string= tag-name "li") (not closingp))
                              (emit-newline out)
                              (write-string "- " out))
                             ((block-tag-p tag-name)
                              (emit-newline out))))
                         (setf index end)))
                      (ignore-tag nil)
                      (t
                       (write-char char out))))
                    (incf index))))))

(defun truncate-text (string max-chars)
  "Truncate STRING at MAX-CHARS, appending an omission marker when needed."
  (if (and max-chars (> (length string) max-chars))
      (concatenate 'string
                   (subseq string 0 max-chars)
                   (format nil "~%~%[truncated at ~D characters]" max-chars))
      string))

(defun parse-json-string (string start)
  "Parse a JSON string from STRING starting at START.
Returns two values: the decoded string and the index after the closing quote."
  (unless (char= (char string start) #\")
    (error "Expected JSON string at index ~D" start))
  (let ((length (length string)))
    (with-output-to-string (out)
      (loop :with index := (1+ start)
            :while (< index length)
            :do (let ((char (char string index)))
                  (cond
                    ((char= char #\")
                     (return-from parse-json-string
                       (values (get-output-stream-string out) (1+ index))))
                    ((char= char #\\)
                     (incf index)
                     (when (>= index length)
                       (error "Unterminated JSON escape sequence"))
                     (let ((escaped (char string index)))
                       (case escaped
                         (#\" (write-char #\" out))
                         (#\\ (write-char #\\ out))
                         (#\/ (write-char #\/ out))
                         (#\b (write-char #\Backspace out))
                         (#\f (write-char #\Page out))
                         (#\n (write-char #\Newline out))
                         (#\r (write-char #\Return out))
                         (#\t (write-char #\Tab out))
                         (#\u
                          (let* ((hex-start (1+ index))
                                 (hex-end (+ hex-start 4)))
                            (unless (<= hex-end length)
                              (error "Incomplete JSON unicode escape"))
                            (let ((code (parse-integer string
                                                       :start hex-start
                                                       :end hex-end
                                                       :radix 16)))
                              (write-char (or (code-char code) #\?) out))
                            (setf index (1- hex-end))))
                         (t (write-char escaped out)))))
                    (t
                     (write-char char out))))
                (incf index))
      (error "Unterminated JSON string"))))

(defun skip-json-whitespace (string start)
  "Return the next index in STRING at or after START that is not whitespace."
  (loop :for index :from start :below (length string)
        :for char := (char string index)
        :unless (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=)
          :do (return index)
        :finally (return (length string))))

(defun parse-flat-json-string-map (string)
  "Parse STRING as a JSON object mapping string keys to string values."
  (let ((index (skip-json-whitespace string 0))
        (entries nil))
    (unless (and (< index (length string))
                 (char= (char string index) #\{))
      (error "Expected JSON object"))
    (incf index)
    (loop
      (setf index (skip-json-whitespace string index))
      (when (>= index (length string))
        (error "Unterminated JSON object"))
      (when (char= (char string index) #\})
        (return (nreverse entries)))
      (multiple-value-bind (key next-index)
          (parse-json-string string index)
        (setf index (skip-json-whitespace string next-index))
        (unless (char= (char string index) #\:)
          (error "Expected colon after JSON key"))
        (setf index (skip-json-whitespace string (1+ index)))
        (multiple-value-bind (value value-index)
            (parse-json-string string index)
          (push (cons key value) entries)
          (setf index (skip-json-whitespace string value-index))
          (cond
            ((char= (char string index) #\,)
             (incf index))
            ((char= (char string index) #\})
             (return (nreverse entries)))
            (t
             (error "Expected comma or closing brace in JSON object"))))))))

(defun load-common-lisp-spec-index ()
  "Load and memoize the CL Community Spec search index."
  (let ((index-path (probe-file *common-lisp-spec-index-path*)))
    (unless index-path
      (error "Missing CL Community Spec index: ~A" *common-lisp-spec-index-path*))
    (unless (and *common-lisp-spec-index-cache*
                 *common-lisp-spec-index-cache-path*
                 (equal *common-lisp-spec-index-cache-path* index-path))
      (setf *common-lisp-spec-index-cache*
            (parse-flat-json-string-map (uiop:read-file-string index-path))
            *common-lisp-spec-index-cache-path* index-path))
    *common-lisp-spec-index-cache*))

(defun ensure-common-lisp-spec-root ()
  "Ensure the vendored CL Community Spec pages directory exists."
  (let* ((root (uiop:ensure-directory-pathname *common-lisp-spec-root*))
         (pages-dir (merge-pathnames #P"pages/" root)))
    (unless (probe-file pages-dir)
      (error "Missing CL Community Spec pages: ~A" pages-dir))
    root))

(defun common-lisp-spec-page-pathname (page)
  "Return the vendored pathname for PAGE."
  (merge-pathnames (format nil "pages/~A" page)
                   (ensure-common-lisp-spec-root)))

(defun common-lisp-spec-page-url (page)
  "Return the upstream URL for PAGE."
  (format nil "~Apages/~A" *common-lisp-spec-site-url* page))

(defun common-lisp-spec-available-p ()
  "Return non-nil when the bundled CL Community Spec can be used."
  (let ((root (uiop:ensure-directory-pathname *common-lisp-spec-root*)))
    (and (probe-file *common-lisp-spec-index-path*)
         (probe-file (merge-pathnames #P"pages/" root)))))

(defun common-lisp-spec-match-rank (query term)
  "Return a rank for TERM relative to QUERY. Lower is better."
  (cond
    ((string-equal query term) 0)
    ((string-prefix-equal-p query term) 1)
    ((string-contains-ci-p query term) 2)
    (t 3)))

(defun find-common-lisp-spec-entry (designator)
  "Return a plist describing the CL Community Spec entry for DESIGNATOR."
  (let* ((query (normalize-designator-string designator))
         (match (find query (load-common-lisp-spec-index)
                      :key #'car
                      :test #'string-equal)))
    (when match
      (let ((page (cdr match)))
        (list :term (car match)
              :page page
              :path (common-lisp-spec-page-pathname page)
              :url (common-lisp-spec-page-url page))))))

(defun describe-common-lisp-symbol-to-string (designator &key (max-chars 12000))
  "Return a plain-text description of DESIGNATOR from the CL Community Spec."
  (let ((entry (find-common-lisp-spec-entry designator)))
    (unless entry
      (return-from describe-common-lisp-symbol-to-string
        (search-common-lisp-spec-to-string (normalize-designator-string designator) :limit 10)))
    (let* ((path (getf entry :path))
           (html (uiop:read-file-string path))
           (title (or (html-title html) (getf entry :term)))
           (text (normalize-text-lines
                  (decode-html-entities
                   (html-to-text html)))))
      (format nil "~A~%~A~%~%Reference: CL Community Spec~%Local Page: ~A~%Upstream URL: ~A~%~%~A"
              title
              (make-string (min 72 (length title)) :initial-element #\-)
              path
              (getf entry :url)
              (truncate-text text max-chars)))))

(defun search-common-lisp-spec-to-string (query &key (limit 12))
  "Search the CL Community Spec index for QUERY and format the matches."
  (let* ((query-string (normalize-designator-string query))
         (matches
           (sort (loop :for (term . page) :in (load-common-lisp-spec-index)
                       :when (string-contains-ci-p query-string term)
                         :collect (list :term term
                                        :page page
                                        :rank (common-lisp-spec-match-rank query-string term)))
                 (lambda (left right)
                   (or (< (getf left :rank) (getf right :rank))
                       (and (= (getf left :rank) (getf right :rank))
                            (string-lessp (getf left :term) (getf right :term))))))))
    (if (null matches)
        (format nil "No CL Community Spec entries found for ~S." query-string)
        (with-output-to-string (out)
          (format out "CL Community Spec matches for ~S~%~%" query-string)
          (loop :for match :in matches
                :for count :from 1
                :while (<= count limit)
                :do (format out "~A. ~A~%   Page: ~A~%   URL: ~A~%~%"
                            count
                            (getf match :term)
                            (getf match :page)
                            (common-lisp-spec-page-url (getf match :page))))))))

;;; --------------------------------------------------------------------------
;;; Local Library Discovery
;;; --------------------------------------------------------------------------

(defun dependency-display-name (dependency)
  "Normalize DEPENDENCY to a readable string."
  (cond
    ((stringp dependency) dependency)
    ((symbolp dependency) (string-downcase (symbol-name dependency)))
    (t (prin1-to-string dependency))))

(defun defsystem-form-p (form)
  "Return non-nil when FORM looks like a defsystem form."
  (and (consp form)
       (symbolp (first form))
       (string= "DEFSYSTEM" (symbol-name (first form)))))

(defun find-defsystem-options (pathname system-name)
  "Return the keyword options plist for SYSTEM-NAME in PATHNAME, or NIL."
  (let ((*read-eval* nil))
    (with-open-file (stream pathname :direction :input)
      (loop :for form := (read stream nil :eof)
            :until (eq form :eof)
            :when (and (defsystem-form-p form)
                       (string-equal (normalize-designator-string (second form))
                                     system-name))
              :do (return (cddr form))))))

(defun find-system-root (system-designator)
  "Return the source root for SYSTEM-DESIGNATOR."
  (let* ((system-name (normalize-designator-string system-designator))
         (system (asdf:find-system system-name nil)))
    (unless system
      (error "Unknown ASDF system: ~A" system-name))
    (or (let ((dir (ignore-errors (asdf:system-source-directory system-name))))
          (when dir
            (uiop:ensure-directory-pathname dir)))
        (let ((path (ignore-errors (asdf:component-pathname system))))
          (when path
            (if (uiop:directory-pathname-p path)
                (uiop:ensure-directory-pathname path)
                (uiop:pathname-directory-pathname path))))
        (error "Unable to determine source root for system ~A" system-name))))

(defun find-system-asd-files (system-designator)
  "Return candidate .asd files for SYSTEM-DESIGNATOR."
  (directory (merge-pathnames "*.asd" (find-system-root system-designator))))

(defun system-metadata (system-designator)
  "Return metadata plist for SYSTEM-DESIGNATOR from its .asd file."
  (let* ((system-name (normalize-designator-string system-designator))
         (asd-files (find-system-asd-files system-name)))
    (or (loop :for file :in asd-files
              :for options := (find-defsystem-options file system-name)
              :when options
                :do (return (list :system system-name
                                  :asd-path file
                                  :description (getf options :description)
                                  :version (getf options :version)
                                  :author (getf options :author)
                                  :depends-on (mapcar #'dependency-display-name
                                                     (or (getf options :depends-on)
                                                         '())))))
        (list :system system-name
              :asd-path (first asd-files)
              :description nil
              :version nil
              :author nil
              :depends-on nil))))

(defun package-name-designator (designator)
  "Normalize PACKAGE DESIGNATOR to a string."
  (typecase designator
    (package (string-downcase (package-name designator)))
    (symbol (string-downcase (symbol-name designator)))
    (string (string-downcase designator))
    (t (string-downcase (prin1-to-string designator)))))

(defun list-project-systems ()
  "Return the direct ASDF systems used by clawmacs."
  (let* ((metadata (system-metadata "clawmacs"))
         (systems (cons "clawmacs" (copy-list (getf metadata :depends-on)))))
    (sort (remove-duplicates systems :test #'string-equal) #'string-lessp)))

(defun extract-package-names-from-string (string)
  "Return package names mentioned in defpackage forms inside STRING."
  (let ((markers '("(defpackage" "(cl:defpackage" "(uiop:define-package"))
        (packages nil))
    (labels ((next-marker-position (start)
               (loop :with best := nil
                     :for marker :in markers
                     :for pos := (search marker string :start2 start :test #'char-equal)
                     :when (and pos (or (null best) (< pos best)))
                       :do (setf best pos)
                     :finally (return best))))
      (loop :for pos := (next-marker-position 0) :then (next-marker-position (1+ pos))
            :while pos
            :do (let* ((after-open (or (position #\Space string :start pos)
                                       (position #\Tab string :start pos)
                                       (position #\Newline string :start pos)
                                       (length string)))
                       (name-start (skip-json-whitespace string after-open)))
                  (handler-case
                      (multiple-value-bind (name next-index)
                          (read-from-string string nil nil :start name-start)
                        (declare (ignore next-index))
                        (when name
                          (pushnew (package-name-designator name)
                                   packages
                                   :test #'string-equal)))
                    (error () nil)))))
    (sort packages #'string-lessp)))

(defun list-system-packages (system-designator)
  "Return a sorted list of package names defined by SYSTEM-DESIGNATOR."
  (let* ((root (find-system-root system-designator))
         (files (collect-files-recursively
                 root
                 (lambda (pathname)
                   (let ((name (string-downcase (or (pathname-name pathname) "")))
                         (type (string-downcase (or (pathname-type pathname) ""))))
                     (and (string= type "lisp")
                          (or (string-contains-ci-p "package" name)
                              (string= name "packages")))))))
         (packages nil))
    (dolist (file files)
      (dolist (package (extract-package-names-from-string (uiop:read-file-string file)))
        (pushnew package packages :test #'string-equal)))
    (sort packages #'string-lessp)))

(defun list-package-functions (package-designator)
  "Return the exported function symbols of PACKAGE-DESIGNATOR."
  (let ((package (find-package (string-upcase (package-name-designator package-designator)))))
    (unless package
      (error "Unknown package: ~A" package-designator))
    (let ((symbols nil))
      (do-external-symbols (symbol package)
        (when (fboundp symbol)
          (push symbol symbols)))
      (sort symbols #'string< :key #'symbol-name))))

(defun list-package-variables (package-designator)
  "Return the exported bound variable symbols of PACKAGE-DESIGNATOR."
  (let ((package (find-package (string-upcase (package-name-designator package-designator)))))
    (unless package
      (error "Unknown package: ~A" package-designator))
    (let ((symbols nil))
      (do-external-symbols (symbol package)
        (when (boundp symbol)
          (push symbol symbols)))
      (sort symbols #'string< :key #'symbol-name))))

(defun list-package-types (package-designator)
  "Return the exported type symbols of PACKAGE-DESIGNATOR."
  (let ((package (find-package (string-upcase (package-name-designator package-designator)))))
    (unless package
      (error "Unknown package: ~A" package-designator))
    (let ((symbols nil))
      (do-external-symbols (symbol package)
        (when (find-class symbol nil)
          (push symbol symbols)))
      (sort symbols #'string< :key #'symbol-name))))

(defun truncate-printed (value &optional (max-length 200))
  "Return a printed representation of VALUE limited to MAX-LENGTH."
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

(defun function-lambda-list-string (symbol)
  "Return a readable lambda list string for SYMBOL, or NIL."
  (handler-case
      (let ((fn (fdefinition symbol)))
        (cond
          ((typep fn 'generic-function)
           #+sbcl
           (let ((lambda-list (sb-mop:generic-function-lambda-list fn)))
             (format nil "(~{~A~^ ~})" lambda-list))
           #-sbcl nil)
          (t
           #+sbcl
           (let ((lambda-list (sb-introspect:function-lambda-list symbol)))
             (format nil "(~{~A~^ ~})" lambda-list))
           #-sbcl nil)))
    (error () nil)))

(defun package-symbol-kinds (symbol)
  "Return a list of human-readable kind labels for SYMBOL."
  (remove nil
          (list (when (macro-function symbol) "Macro")
                (when (and (fboundp symbol)
                           (not (macro-function symbol))
                           (typep (fdefinition symbol) 'generic-function))
                  "Generic Function")
                (when (and (fboundp symbol)
                           (not (macro-function symbol))
                           (not (typep (fdefinition symbol) 'generic-function)))
                  "Function")
                (when (boundp symbol)
                  (ecase (variable-kind symbol)
                    (:constant "Constant")
                    (:parameter "Variable")
                    (:variable "Variable")))
                (when (find-class symbol nil)
                  (type-kind-label (type-kind symbol))))))

(defun resolve-library-symbol (system-designator symbol-designator &optional package-designator)
  "Resolve SYMBOL-DESIGNATOR for SYSTEM-DESIGNATOR.
Returns two values: symbol and package."
  (let* ((symbol-name (string-upcase (normalize-designator-string symbol-designator)))
         (packages (if package-designator
                       (list (package-name-designator package-designator))
                       (list-system-packages system-designator))))
    (loop :for package-name :in packages
          :for package := (find-package (string-upcase package-name))
          :when package
            :do (multiple-value-bind (symbol status)
                    (find-symbol symbol-name package)
                  (when status
                    (return (values symbol package)))))))

(defun search-file-lines (pathname query)
  "Return matching line plists from PATHNAME for QUERY."
  (loop :with results := nil
        :with line-number := 0
        :for line :in (split-lines (uiop:read-file-string pathname))
        :do (incf line-number)
            (when (string-contains-ci-p query line)
              (push (list :path pathname
                          :line line-number
                          :excerpt (string-trim '(#\Space #\Tab) line))
                    results))
        :finally (return (nreverse results))))

(defun search-system-docs (system-designator query &key (limit 12))
  "Search local docs and source text for QUERY within SYSTEM-DESIGNATOR."
  (let* ((system-name (normalize-designator-string system-designator))
         (root (find-system-root system-name))
         (hits nil))
    (block search-done
      (dolist (file (collect-files-recursively root #'text-doc-pathname-p))
        (dolist (hit (search-file-lines file (normalize-designator-string query)))
          (push hit hits)
          (when (>= (length hits) limit)
            (return-from search-done nil)))))
    (setf hits (nreverse hits))
    (if (null hits)
        (format nil "No local documentation hits for ~S in system ~A."
                query system-name)
        (with-output-to-string (out)
          (format out "Local documentation hits for ~S in system ~A~%~%"
                  (normalize-designator-string query)
                  system-name)
          (loop :for hit :in hits
                :for count :from 1
                :do (format out "~A. ~A:~D~%   ~A~%~%"
                            count
                            (enough-namestring (getf hit :path) root)
                            (getf hit :line)
                            (getf hit :excerpt)))))))

(defun describe-system-to-string (system-designator)
  "Return a human-readable summary of SYSTEM-DESIGNATOR."
  (let* ((metadata (system-metadata system-designator))
         (root (find-system-root system-designator))
         (packages (list-system-packages system-designator))
         (depends-on (getf metadata :depends-on))
         (readmes (collect-files-recursively
                   root
                   (lambda (pathname)
                     (let ((name (string-downcase
                                  (or (file-namestring pathname) ""))))
                       (and (not (alexandria:ends-with-subseq "~" name))
                            (or (string-prefix-equal-p "readme" name)
                                (string-prefix-equal-p "changelog" name))))))))
    (with-output-to-string (out)
      (format out "~A~%~A~%~%" (getf metadata :system)
              (make-string (min 72 (length (getf metadata :system)))
                           :initial-element #\-))
      (format out "Root: ~A~%" root)
      (when (getf metadata :asd-path)
        (format out "ASD: ~A~%" (getf metadata :asd-path)))
      (when (getf metadata :version)
        (format out "Version: ~A~%" (getf metadata :version)))
      (when (getf metadata :author)
        (format out "Author: ~A~%" (getf metadata :author)))
      (when (getf metadata :description)
        (format out "~%~A~%" (getf metadata :description)))
      (when depends-on
        (format out "~%Depends On: ~{~A~^, ~}~%" depends-on))
      (when packages
        (format out "~%Packages: ~{~A~^, ~}~%" packages))
      (when readmes
        (format out "~%Docs: ~{~A~^, ~}~%"
                (mapcar (lambda (pathname)
                          (enough-namestring pathname root))
                        readmes))))))

(defun describe-library-symbol-to-string (system-designator symbol-designator
                                           &optional package-designator)
  "Return a human-readable summary of SYMBOL-DESIGNATOR for SYSTEM-DESIGNATOR."
  (let* ((system-name (normalize-designator-string system-designator))
         (symbol-name (normalize-designator-string symbol-designator))
         (resolved (multiple-value-list
                    (resolve-library-symbol system-name symbol-name package-designator))))
    (destructuring-bind (&optional symbol package) resolved
      (unless symbol
        (return-from describe-library-symbol-to-string
          (format nil "~A was not found in the packages for system ~A.~%~%~A"
                  symbol-name
                  system-name
                  (search-system-docs system-name symbol-name :limit 8))))
      (with-output-to-string (out)
        (format out "~A::~A~%~A~%~%"
                (package-name package)
                symbol
                (make-string (min 72 (+ 2 (length (package-name package))
                                        (length (symbol-name symbol))))
                             :initial-element #\-))
        (format out "Kinds: ~{~A~^, ~}~%"
                (package-symbol-kinds symbol))
        (let ((lambda-list (and (fboundp symbol)
                                (function-lambda-list-string symbol))))
          (when lambda-list
            (format out "Arguments: ~A~%" lambda-list)))
        (when (boundp symbol)
          (format out "Value: ~A~%" (truncate-printed (symbol-value symbol))))
        (let ((function-doc (and (fboundp symbol)
                                 (documentation symbol 'function)))
              (variable-doc (and (boundp symbol)
                                 (documentation symbol 'variable)))
              (type-doc (and (find-class symbol nil)
                             (documentation (find-class symbol) t))))
          (when function-doc
            (format out "~%Function Doc:~%~A~%" function-doc))
          (when (and variable-doc (not (string= variable-doc function-doc)))
            (format out "~%Variable Doc:~%~A~%" variable-doc))
          (when (and type-doc (not (string= type-doc function-doc)))
            (format out "~%Type Doc:~%~A~%" type-doc)))
        (format out "~%~A"
                (search-system-docs system-name symbol-name :limit 8))))))
