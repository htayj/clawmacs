(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Organa model
;;; --------------------------------------------------------------------------

(defparameter *organa-todo-keywords*
  '("TODO" "NEXT" "WAITING" "BLOCKED" "DONE" "CANCELLED")
  "Org TODO keywords understood by Organa.")

(defparameter *organa-done-keywords* '("DONE" "CANCELLED")
  "Org TODO keywords treated as complete by Organa.")

(defparameter *organa-default-view* :dashboard
  "Default Organa buffer presentation view.")

(defvar *organa-buffer-view-table* (make-hash-table :test #'eq)
  "Frame-local enough view state keyed by Organa buffer objects.")

(defstruct organa-todo
  "Parsed org headline relevant to project TODO management."
  id
  persistent-id-p
  status
  title
  tags
  level
  start
  end
  properties
  depends-on
  parent
  children)

(defstruct organa-model
  "Parsed org file model."
  path
  text
  lines
  todos
  roots)

(defstruct organa-location
  "A file location resolved from tool arguments or a user path."
  project
  path
  pathname
  text)

(clim:define-presentation-type organa-todo-ref ())
(clim:define-presentation-type organa-dependency-ref ())

(clim:define-presentation-method clim:presentation-typep
    (object (type organa-todo-ref))
  (organa-todo-p object))

(clim:define-presentation-method clim:presentation-typep
    (object (type organa-dependency-ref))
  (stringp object))

(defun organa-blank-string-p (value)
  "Return true when VALUE is NIL or only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun organa-string (value field-name &key allow-nil)
  "Normalize VALUE as a string argument named FIELD-NAME."
  (cond
    ((null value)
     (if allow-nil
         nil
         (error "~A is required." field-name)))
    ((stringp value) value)
    ((symbolp value) (symbol-name value))
    (t
     (error "~A must be a string, got ~S." field-name value))))

(defun organa-trim (text)
  "Trim ASCII whitespace from TEXT."
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))

(defun organa-split-lines (text)
  "Split TEXT into lines without line terminators."
  (let ((lines nil)
        (start 0)
        (length (length text)))
    (loop
      :for pos := (position #\Newline text :start start)
      :do (cond
            (pos
             (push (subseq text start pos) lines)
             (setf start (1+ pos)))
            (t
             (when (< start length)
               (push (subseq text start length) lines))
             (return))))
    (nreverse lines)))

(defun organa-join-lines (lines)
  "Join LINES into org file text with a trailing newline."
  (with-output-to-string (stream)
    (dolist (line lines)
      (write-string line stream)
      (terpri stream))))

(defun organa-line-starts-with-p (prefix line)
  "Return true when LINE begins with PREFIX."
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length line))
         (string= prefix line :end2 prefix-length))))

(defun organa-tokenize-spaces (text)
  "Split TEXT on ASCII spaces and tabs, removing empty fields."
  (let ((tokens nil)
        (start nil))
    (labels ((finish (end)
               (when start
                 (push (subseq text start end) tokens)
                 (setf start nil))))
      (loop :for index :from 0 :below (length text)
            :for char := (char text index)
            :do (if (member char '(#\Space #\Tab) :test #'char=)
                    (finish index)
                    (unless start
                      (setf start index))))
      (finish (length text)))
    (nreverse tokens)))

(defun organa-string-list (text)
  "Return whitespace-separated non-empty tokens from TEXT."
  (remove-if #'organa-blank-string-p
             (organa-tokenize-spaces (or text ""))))

(defun organa-status-p (token)
  "Return true when TOKEN is a known TODO status."
  (and token
       (member (string-upcase token) *organa-todo-keywords* :test #'string=)))

(defun organa-done-status-p (status)
  "Return true when STATUS is complete."
  (member (string-upcase (or status "")) *organa-done-keywords*
          :test #'string=))

(defun organa-normalize-status (status)
  "Normalize STATUS or signal a clear error."
  (let ((value (string-upcase (organa-trim (organa-string status "status")))))
    (unless (organa-status-p value)
      (error "Unsupported org TODO status ~S. Use one of: ~{~A~^, ~}."
             status
             *organa-todo-keywords*))
    value))

(defun organa-heading-level (line)
  "Return LINE's org headline level, or NIL."
  (let ((count 0))
    (loop :while (and (< count (length line))
                      (char= (char line count) #\*))
          :do (incf count))
    (and (plusp count)
         (< count (length line))
         (member (char line count) '(#\Space #\Tab) :test #'char=)
         count)))

(defun organa-heading-rest (line level)
  "Return the content of a headline LINE after LEVEL stars."
  (organa-trim (subseq line level)))

(defun organa-parse-tags (title)
  "Return TITLE without final org tags and the tag list."
  (let* ((trimmed (organa-trim title))
         (space-pos (position #\Space trimmed :from-end t))
         (last-token (if space-pos
                         (subseq trimmed (1+ space-pos))
                         trimmed)))
    (if (and (> (length last-token) 1)
             (char= (char last-token 0) #\:)
             (char= (char last-token (1- (length last-token))) #\:))
        (values (organa-trim
                 (if space-pos (subseq trimmed 0 space-pos) ""))
                (remove-if #'organa-blank-string-p
                           (uiop:split-string last-token :separator ":")))
        (values trimmed nil))))

(defun organa-parse-heading (line)
  "Parse org headline LINE.
Returns LEVEL, STATUS, TITLE, and TAGS."
  (let* ((level (organa-heading-level line))
         (rest (and level (organa-heading-rest line level)))
         (tokens (and rest (organa-tokenize-spaces rest)))
         (first-token (first tokens))
         (status (and (organa-status-p first-token)
                      (string-upcase first-token)))
         (title-text (if status
                         (organa-trim
                          (subseq rest (length first-token)))
                         rest)))
    (multiple-value-bind (title tags)
        (organa-parse-tags title-text)
      (values level status title tags))))

(defun organa-property-line (line)
  "Parse one org property line, returning KEY and VALUE or NIL."
  (let ((trimmed (organa-trim line)))
    (when (and (> (length trimmed) 2)
               (char= (char trimmed 0) #\:))
      (let ((second-colon (position #\: trimmed :start 1)))
        (when second-colon
          (values (string-upcase (subseq trimmed 1 second-colon))
                  (organa-trim (subseq trimmed (1+ second-colon)))))))))

(defun organa-parse-properties (lines start end)
  "Return property alist for headline at START with subtree ending at END."
  (let ((properties nil)
        (drawer-start (1+ start)))
    (when (and (< drawer-start end)
               (string= ":PROPERTIES:"
                        (string-upcase (organa-trim (nth drawer-start lines)))))
      (loop :for index :from (+ drawer-start 1) :below end
            :for line := (nth index lines)
            :for trimmed := (string-upcase (organa-trim line))
            :until (string= trimmed ":END:")
            :do (multiple-value-bind (key value)
                    (organa-property-line line)
                  (when key
                    (push (cons key value) properties)))))
    (nreverse properties)))

(defun organa-property (todo key)
  "Return TODO property KEY."
  (cdr (assoc (string-upcase key) (organa-todo-properties todo)
              :test #'string=)))

(defun organa-parse-text (text &key path)
  "Parse org TEXT into an ORGANA-MODEL."
  (let* ((lines (organa-split-lines text))
         (headline-indices
           (loop :for line :in lines
                 :for index :from 0
                 :when (organa-heading-level line)
                   :collect index))
         (todos nil)
         (roots nil)
         (stack nil))
    (loop :for index :in headline-indices
          :for nexts :on (append (rest headline-indices) (list (length lines)))
          :for end := (first nexts)
          :do (multiple-value-bind (level status title tags)
                  (organa-parse-heading (nth index lines))
                (when status
                  (let* ((properties (organa-parse-properties lines index end))
                         (persistent-id (cdr (assoc "ID" properties
                                                    :test #'string=)))
                         (id (or persistent-id
                                 (format nil "line-~D" (1+ index))))
                         (depends-on (organa-string-list
                                      (cdr (assoc "ORGANA_DEPENDS"
                                                  properties
                                                  :test #'string=))))
                         (todo (make-organa-todo
                                :id id
                                :persistent-id-p (not (null persistent-id))
                                :status status
                                :title title
                                :tags tags
                                :level level
                                :start index
                                :end end
                                :properties properties
                                :depends-on depends-on)))
                    (loop :while (and stack
                                       (>= (organa-todo-level (first stack))
                                           level))
                          :do (pop stack))
                    (if stack
                        (let ((parent (first stack)))
                          (setf (organa-todo-parent todo) parent)
                          (setf (organa-todo-children parent)
                                (append (organa-todo-children parent)
                                        (list todo))))
                        (push todo roots))
                    (push todo stack)
                    (push todo todos)))))
    (make-organa-model :path path
                       :text text
                       :lines lines
                       :todos (nreverse todos)
                       :roots (nreverse roots))))

(defun organa-todo-by-id-table (model)
  "Return a hash table mapping TODO ids to TODO objects."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (todo (organa-model-todos model) table)
      (setf (gethash (organa-todo-id todo) table) todo))))

(defun organa-find-todo (model selector)
  "Find a todo in MODEL by id, transient line id, or exact title."
  (let* ((value (organa-trim (organa-string selector "todo selector")))
         (by-id (find value (organa-model-todos model)
                      :key #'organa-todo-id
                      :test #'string=)))
    (or by-id
        (let ((matches
                (remove-if-not
                 (lambda (todo)
                   (string-equal value (organa-todo-title todo)))
                 (organa-model-todos model))))
          (cond
            ((null matches)
             (error "No org TODO matches selector ~S." selector))
            ((rest matches)
             (error "Selector ~S matches multiple org TODO titles; use an ID."
                    selector))
            (t (first matches)))))))

(defun organa-slug-char (char)
  "Return CHAR normalized for an Organa ID slug, or NIL."
  (cond
    ((alphanumericp char) (char-downcase char))
    ((member char '(#\- #\_) :test #'char=) char)
    (t #\-)))

(defun organa-title-slug (title)
  "Return a compact slug for TITLE."
  (let ((slug
          (with-output-to-string (stream)
            (let ((dash-p nil))
              (loop :for char :across title
                    :for safe := (organa-slug-char char)
                    :do (cond
                          ((null safe) nil)
                          ((char= safe #\-)
                           (unless dash-p
                             (write-char safe stream)
                             (setf dash-p t)))
                          (t
                           (write-char safe stream)
                           (setf dash-p nil))))))))
    (let ((trimmed (string-trim "-" slug)))
      (subseq (if (plusp (length trimmed)) trimmed "todo")
              0
              (min 24 (max 1 (length trimmed)))))))

(defun organa-existing-id-table (model)
  "Return a hash table of persistent and transient IDs in MODEL."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (todo (organa-model-todos model) table)
      (setf (gethash (organa-todo-id todo) table) t))))

(defun organa-new-id (model title)
  "Return a new Organa ID not used in MODEL."
  (let ((existing (organa-existing-id-table model))
        (base (format nil "organa-~A-~36R"
                      (organa-title-slug title)
                      (get-universal-time))))
    (loop :for n :from 0
          :for candidate := (if (zerop n)
                                base
                                (format nil "~A-~D" base n))
          :unless (gethash candidate existing)
            :return candidate)))

(defun organa-replace-line (lines index new-line)
  "Return LINES with INDEX replaced by NEW-LINE."
  (loop :for line :in lines
        :for i :from 0
        :collect (if (= i index) new-line line)))

(defun organa-subseq-lines (lines start end)
  "Return a copy of LINES from START to END."
  (loop :for line :in lines
        :for i :from 0
        :when (and (<= start i) (< i end))
          :collect line))

(defun organa-remove-lines (lines start end)
  "Return LINES without the range START..END."
  (loop :for line :in lines
        :for i :from 0
        :unless (and (<= start i) (< i end))
          :collect line))

(defun organa-insert-lines (lines index inserted)
  "Return LINES with INSERTED spliced at INDEX."
  (append (subseq lines 0 index)
          inserted
          (subseq lines index)))

(defun organa-property-drawer-range (lines todo)
  "Return property drawer start and END-line index for TODO, or NIL NIL."
  (let ((start (1+ (organa-todo-start todo))))
    (when (and (< start (organa-todo-end todo))
               (string= ":PROPERTIES:"
                        (string-upcase (organa-trim (nth start lines)))))
      (loop :for index :from (1+ start) :below (organa-todo-end todo)
            :for line := (nth index lines)
            :when (string= ":END:" (string-upcase (organa-trim line)))
              :do (return (values start index))))))

(defun organa-property-line-index (lines drawer-start drawer-end key)
  "Return the line index for KEY in a property drawer."
  (loop :for index :from (1+ drawer-start) :below drawer-end
        :for line := (nth index lines)
        :do (multiple-value-bind (property-key _value)
                (organa-property-line line)
              (declare (ignore _value))
              (when (and property-key
                         (string= property-key (string-upcase key)))
                (return index)))))

(defun organa-set-property-in-lines (lines todo key value)
  "Return LINES with TODO property KEY set to VALUE."
  (let ((property-line (format nil ":~A: ~A" (string-upcase key) value)))
    (multiple-value-bind (drawer-start drawer-end)
        (organa-property-drawer-range lines todo)
      (cond
        ((and drawer-start drawer-end)
         (let ((existing (organa-property-line-index
                          lines drawer-start drawer-end key)))
           (if existing
               (organa-replace-line lines existing property-line)
               (organa-insert-lines lines drawer-end (list property-line)))))
        (t
         (organa-insert-lines
          lines
          (1+ (organa-todo-start todo))
          (list ":PROPERTIES:" property-line ":END:")))))))

(defun organa-remove-property-in-lines (lines todo key)
  "Return LINES with TODO property KEY removed when present."
  (multiple-value-bind (drawer-start drawer-end)
      (organa-property-drawer-range lines todo)
    (if (and drawer-start drawer-end)
        (let ((existing (organa-property-line-index
                         lines drawer-start drawer-end key)))
          (if existing
              (organa-remove-lines lines existing (1+ existing))
              lines))
        lines)))

(defun organa-ensure-id-in-lines (lines selector &key path)
  "Ensure selected TODO has a persistent ID.
Returns new lines, the persistent id, and the selected TODO after reparsing."
  (let* ((model (organa-parse-text (organa-join-lines lines) :path path))
         (todo (organa-find-todo model selector)))
    (if (organa-todo-persistent-id-p todo)
        (values lines (organa-todo-id todo) todo)
        (let* ((id (organa-new-id model (organa-todo-title todo)))
               (new-lines (organa-set-property-in-lines lines todo "ID" id))
               (new-model (organa-parse-text (organa-join-lines new-lines)
                                             :path path))
               (new-todo (organa-find-todo new-model id)))
          (values new-lines id new-todo)))))

(defun organa-rewrite-heading-status (line new-status)
  "Return LINE with its TODO status replaced by NEW-STATUS."
  (let* ((level (organa-heading-level line))
         (stars (make-string level :initial-element #\*))
         (rest (organa-heading-rest line level))
         (tokens (organa-tokenize-spaces rest))
         (first-token (first tokens))
         (tail (if (organa-status-p first-token)
                   (organa-trim (subseq rest (length first-token)))
                   rest)))
    (format nil "~A ~A ~A" stars new-status tail)))

(defun organa-set-status-in-lines (lines selector status &key path)
  "Return LINES with selected TODO set to STATUS."
  (let* ((model (organa-parse-text (organa-join-lines lines) :path path))
         (todo (organa-find-todo model selector))
         (normalized (organa-normalize-status status))
         (line (nth (organa-todo-start todo) lines)))
    (organa-replace-line lines
                         (organa-todo-start todo)
                         (organa-rewrite-heading-status line normalized))))

(defun organa-add-todo-to-lines
    (lines title &key (status "TODO") parent path)
  "Return LINES with a new TODO appended or added under PARENT."
  (let* ((trimmed-title (organa-trim (organa-string title "title")))
         (normalized-status (organa-normalize-status status))
         (model (organa-parse-text (organa-join-lines lines) :path path)))
    (when (organa-blank-string-p trimmed-title)
      (error "title must not be blank."))
    (let* ((parent-todo (and (not (organa-blank-string-p parent))
                             (organa-find-todo model parent)))
           (level (if parent-todo (1+ (organa-todo-level parent-todo)) 1))
           (id (organa-new-id model trimmed-title))
           (heading (format nil "~A ~A ~A"
                            (make-string level :initial-element #\*)
                            normalized-status
                            trimmed-title))
           (entry (list heading
                        ":PROPERTIES:"
                        (format nil ":ID: ~A" id)
                        ":END:"))
           (insert-at (if parent-todo
                          (organa-todo-end parent-todo)
                          (length lines))))
      (values (organa-insert-lines lines insert-at entry) id))))

(defun organa-move-todo-in-lines (lines selector after-selector &key path)
  "Return LINES with selected TODO moved after AFTER-SELECTOR.
Blank AFTER-SELECTOR moves the TODO before the first headline."
  (let* ((model (organa-parse-text (organa-join-lines lines) :path path))
         (todo (organa-find-todo model selector))
         (moving-lines (organa-subseq-lines lines
                                            (organa-todo-start todo)
                                            (organa-todo-end todo)))
         (remaining (organa-remove-lines lines
                                         (organa-todo-start todo)
                                         (organa-todo-end todo))))
    (if (organa-blank-string-p after-selector)
        (let* ((remaining-model (organa-parse-text
                                 (organa-join-lines remaining)
                                 :path path))
               (first-todo (first (organa-model-todos remaining-model)))
               (insert-at (if first-todo (organa-todo-start first-todo) 0)))
          (organa-insert-lines remaining insert-at moving-lines))
        (let* ((target-in-moving-subtree-p
                 (let ((target (ignore-errors
                                 (organa-find-todo model after-selector))))
                   (and target
                        (<= (organa-todo-start todo)
                            (organa-todo-start target))
                        (< (organa-todo-start target)
                           (organa-todo-end todo)))))
               (remaining-model (organa-parse-text
                                 (organa-join-lines remaining)
                                 :path path)))
          (when target-in-moving-subtree-p
            (error "Cannot move a TODO after one of its own children."))
          (let ((target (organa-find-todo remaining-model after-selector)))
            (organa-insert-lines remaining
                                 (organa-todo-end target)
                                 moving-lines))))))

(defun organa-link-dependency-in-lines
    (lines selector depends-on-selector &key path remove-p)
  "Return LINES with dependency relation updated."
  (multiple-value-bind (lines-with-id todo-id _todo)
      (organa-ensure-id-in-lines lines selector :path path)
    (declare (ignore _todo))
    (multiple-value-bind (lines-with-both dependency-id _dependency)
        (organa-ensure-id-in-lines lines-with-id depends-on-selector :path path)
      (declare (ignore _dependency))
      (let* ((model (organa-parse-text (organa-join-lines lines-with-both)
                                       :path path))
             (todo (organa-find-todo model todo-id))
             (dependencies (remove-duplicates
                            (if remove-p
                                (remove dependency-id
                                        (copy-list (organa-todo-depends-on todo))
                                        :test #'string=)
                                (append (copy-list
                                         (organa-todo-depends-on todo))
                                        (list dependency-id)))
                            :test #'string=)))
        (if dependencies
            (organa-set-property-in-lines
             lines-with-both todo "ORGANA_DEPENDS"
             (format nil "~{~A~^ ~}" dependencies))
            (organa-remove-property-in-lines
             lines-with-both todo "ORGANA_DEPENDS"))))))

;;; --------------------------------------------------------------------------
;;; File/project IO
;;; --------------------------------------------------------------------------

(defun organa-read-file-text (pathname &key allow-missing)
  "Read PATHNAME as text."
  (cond
    ((probe-file pathname)
     (uiop:read-file-string pathname))
    (allow-missing "")
    (t
     (error "Org file does not exist: ~A" pathname))))

(defun organa-write-file-text (pathname text)
  "Write TEXT to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text stream))
  pathname)

(defun organa-resolve-location (path &key project allow-missing)
  "Resolve PATH and optional PROJECT to an ORGANA-LOCATION."
  (let ((path-string (organa-string path "path")))
    (when (organa-blank-string-p path-string)
      (error "path must not be blank."))
    (if (organa-blank-string-p project)
        (let* ((pathname (validate-sandbox-path path-string))
               (text (organa-read-file-text pathname
                                            :allow-missing allow-missing)))
          (make-organa-location :path path-string
                                :pathname pathname
                                :text text))
        (let* ((project-name (organa-string project "project"))
               (resource-path (project-resource-name path-string))
               (text (handler-case
                         (project-read-file project-name resource-path)
                       (error (e)
                         (if allow-missing
                             ""
                             (error "~A" e))))))
          (make-organa-location :project project-name
                                :path resource-path
                                :text text)))))

(defun organa-location-display (location)
  "Return user-facing display name for LOCATION."
  (if (organa-location-project location)
      (format nil "~A:~A"
              (organa-location-project location)
              (organa-location-path location))
      (namestring (organa-location-pathname location))))

(defun organa-location-model (location)
  "Parse LOCATION into an Organa model."
  (organa-parse-text (organa-location-text location)
                     :path (organa-location-display location)))

(defun organa-write-location (location text)
  "Persist TEXT to LOCATION."
  (if (organa-location-project location)
      (project-write-file (organa-location-project location)
                          (organa-location-path location)
                          text)
      (organa-write-file-text (organa-location-pathname location) text))
  text)

(defun organa-read-buffer-location (buffer)
  "Return the ORGANA-LOCATION displayed by BUFFER."
  (unless (eq (buffer-kind buffer) :organa)
    (error "Current buffer is not an Organa TODO buffer: ~A"
           (buffer-name buffer)))
  (let ((path (buffer-resource-path buffer)))
    (when (organa-blank-string-p path)
      (error "Organa buffer has no org file path."))
    (organa-resolve-location path :allow-missing t)))

(defun organa-refresh-buffer (buffer location)
  "Refresh BUFFER metadata from LOCATION and request redisplay."
  (setf (buffer-original-text buffer) (organa-location-text location)
        (buffer-dirty-p buffer) nil)
  (notify-buffer-display-change buffer :organa)
  buffer)

(defun organa-apply-buffer-mutation (buffer mutator)
  "Apply MUTATOR to BUFFER's org file and refresh BUFFER."
  (let* ((location (organa-read-buffer-location buffer))
         (lines (organa-split-lines (organa-location-text location))))
    (multiple-value-bind (new-lines result)
        (funcall mutator lines (organa-location-display location))
      (let ((new-text (organa-join-lines new-lines)))
        (organa-write-location location new-text)
        (setf (organa-location-text location) new-text)
        (organa-refresh-buffer buffer location)
        result))))

;;; --------------------------------------------------------------------------
;;; Views
;;; --------------------------------------------------------------------------

(defun organa-view-for-buffer (buffer)
  "Return BUFFER's selected Organa view."
  (or (gethash buffer *organa-buffer-view-table*)
      *organa-default-view*))

(defun (setf organa-view-for-buffer) (view buffer)
  "Set BUFFER's selected Organa view."
  (setf (gethash buffer *organa-buffer-view-table*) view))

(defun organa-cycle-view (view)
  "Return the next Organa view after VIEW."
  (case view
    (:dashboard :kanban)
    (:kanban :dependency)
    (:dependency :outline)
    (otherwise :dashboard)))

(defun organa-status-counts (model)
  "Return alist of status counts for MODEL."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (status *organa-todo-keywords*)
      (setf (gethash status table) 0))
    (dolist (todo (organa-model-todos model))
      (incf (gethash (organa-todo-status todo) table 0)))
    (loop :for status :in *organa-todo-keywords*
          :collect (cons status (gethash status table 0)))))

(defun organa-dependencies-complete-p (todo by-id)
  "Return true when TODO's dependencies are complete or missing."
  (every (lambda (id)
           (let ((dependency (gethash id by-id)))
             (or (null dependency)
                 (organa-done-status-p
                  (organa-todo-status dependency)))))
         (organa-todo-depends-on todo)))

(defun organa-ready-todos (model)
  "Return incomplete TODOs that are not blocked by incomplete dependencies."
  (let ((by-id (organa-todo-by-id-table model)))
    (remove-if-not
     (lambda (todo)
       (and (not (organa-done-status-p (organa-todo-status todo)))
            (organa-dependencies-complete-p todo by-id)))
     (organa-model-todos model))))

(defun organa-blocked-todos (model)
  "Return incomplete TODOs with incomplete dependencies."
  (let ((by-id (organa-todo-by-id-table model)))
    (remove-if-not
     (lambda (todo)
       (and (not (organa-done-status-p (organa-todo-status todo)))
            (not (organa-dependencies-complete-p todo by-id))))
     (organa-model-todos model))))

(defun organa-fit (text width)
  "Fit TEXT to WIDTH display columns."
  (let* ((string (or text ""))
         (safe-width (max 0 width)))
    (cond
      ((zerop safe-width) "")
      ((<= (length string) safe-width) string)
      ((<= safe-width 1) (subseq string 0 safe-width))
      (t
       (concatenate 'string
                    (subseq string 0 (1- safe-width))
                    ">")))))

(defun organa-render-row (text &key todo face object presentation-type)
  "Return a render row plist."
  (list :text text
        :todo todo
        :face face
        :object object
        :presentation-type presentation-type))

(defun organa-todo-label (todo &key (include-id-p nil))
  "Return one-line TODO label."
  (let ((base (format nil "~A ~A"
                      (organa-todo-status todo)
                      (organa-todo-title todo))))
    (if include-id-p
        (format nil "~A  [~A]" base (organa-todo-id todo))
        base)))

(declaim (ftype function organa-dependency-rows))

(defun organa-dashboard-rows (model)
  "Return dashboard presentation rows for MODEL."
  (let* ((counts (organa-status-counts model))
         (ready (organa-ready-todos model))
         (blocked (organa-blocked-todos model)))
    (append
     (list
      (organa-render-row
       (format nil "Status: ~{~A=~D~^  ~}"
               (loop :for (status . count) :in counts
                     :collect status
                     :collect count)))
      (organa-render-row "")
      (organa-render-row "Ready next"))
     (if ready
         (loop :for todo :in (subseq ready 0 (min 6 (length ready)))
               :collect (organa-render-row
                         (format nil "  -> ~A" (organa-todo-label todo))
                         :todo todo
                         :face :ready))
         (list (organa-render-row "  No unblocked active TODOs.")))
     (list (organa-render-row "")
           (organa-render-row "Blocked work"))
     (if blocked
         (loop :for todo :in (subseq blocked 0 (min 6 (length blocked)))
               :collect (organa-render-row
                         (format nil "  x  ~A waits on ~{~A~^, ~}"
                                 (organa-todo-label todo)
                                 (organa-todo-depends-on todo))
                         :todo todo
                         :face :blocked))
         (list (organa-render-row "  No dependency-blocked TODOs.")))
     (list (organa-render-row "")
           (organa-render-row "Dependency chains"))
     (organa-dependency-rows model :compact-p t))))

(defun organa-column-cell (todo width)
  "Return compact kanban cell text for TODO."
  (if todo
      (organa-fit (organa-todo-title todo) width)
      (make-string width :initial-element #\Space)))

(defun organa-kanban-rows (model cols)
  "Return kanban rows for MODEL."
  (let* ((lanes '("TODO" "NEXT" "WAITING" "BLOCKED" "DONE"))
         (lane-width (max 10 (floor (- cols (1- (length lanes))) (length lanes))))
         (lane-items
           (mapcar (lambda (status)
                     (remove-if-not
                      (lambda (todo)
                        (string= status (organa-todo-status todo)))
                      (organa-model-todos model)))
                   lanes))
         (max-len (reduce #'max lane-items :key #'length :initial-value 0)))
    (append
     (list (organa-render-row
            (format nil "~{~A~^|~}"
                    (mapcar (lambda (lane) (organa-fit lane lane-width))
                            lanes))))
     (loop :for row :from 0 :below max-len
           :collect
           (organa-render-row
            (format nil "~{~A~^|~}"
                    (loop :for items :in lane-items
                          :for todo := (nth row items)
                          :collect (organa-column-cell todo lane-width)))
            :todo (loop :for items :in lane-items
                        :for todo := (nth row items)
                        :when todo :return todo))))))

(defun organa-dependency-title (id by-id)
  "Return display title for dependency ID."
  (let ((todo (gethash id by-id)))
    (if todo
        (format nil "~A (~A)" (organa-todo-title todo)
                (organa-todo-status todo))
        (format nil "~A (missing)" id))))

(defun organa-dependency-rows (model &key compact-p)
  "Return dependency chain rows for MODEL."
  (let ((by-id (organa-todo-by-id-table model))
        (rows nil))
    (dolist (todo (organa-model-todos model))
      (when (organa-todo-depends-on todo)
        (push
         (organa-render-row
          (format nil "~A <- ~{~A~^ <- ~}"
                  (organa-todo-label todo)
                 (mapcar (lambda (id)
                            (organa-dependency-title id by-id))
                          (organa-todo-depends-on todo)))
          :object (first (organa-todo-depends-on todo))
          :presentation-type 'organa-dependency-ref
          :face :dependency)
         rows)))
    (let ((result (nreverse rows)))
      (cond
        ((null result)
         (list (organa-render-row "  No dependency chains yet.")))
        (compact-p
         (subseq result 0 (min 8 (length result))))
        (t result)))))

(defun organa-outline-rows (model)
  "Return outline rows for MODEL."
  (loop :for todo :in (organa-model-todos model)
        :collect
        (organa-render-row
         (format nil "~VT~A~@[ deps: ~{~A~^, ~}~] [~A]"
                 (* 2 (1- (organa-todo-level todo)))
                 (organa-todo-label todo)
                 (organa-todo-depends-on todo)
                 (organa-todo-id todo))
         :todo todo
         :face :outline)))

(defun organa-view-rows (model view cols)
  "Return render rows for MODEL in VIEW."
  (case view
    (:kanban (organa-kanban-rows model cols))
    (:dependency (organa-dependency-rows model))
    (:outline (organa-outline-rows model))
    (otherwise (organa-dashboard-rows model))))

(defun organa-next-status (status)
  "Return the next Organa workflow status after STATUS."
  (let* ((normalized (organa-normalize-status status))
         (position (position normalized *organa-todo-keywords*
                             :test #'string=)))
    (nth (mod (1+ (or position 0))
              (length *organa-todo-keywords*))
         *organa-todo-keywords*)))

(defun organa-todo-selector (todo)
  "Return the best selector string for TODO."
  (or (organa-todo-id todo)
      (organa-todo-title todo)))

(defun organa-cycle-todo-status (buffer todo)
  "Advance TODO to its next workflow status in BUFFER."
  (unless (and buffer todo)
    (return-from organa-cycle-todo-status nil))
  (let ((next-status (organa-next-status (organa-todo-status todo))))
    (organa-apply-buffer-mutation
     buffer
     (lambda (lines path)
       (values (organa-set-status-in-lines lines
                                           (organa-todo-selector todo)
                                           next-status
                                           :path path)
               next-status)))))

(defun organa-focus-todo-by-id (buffer todo-id)
  "Switch BUFFER to outline view and scroll TODO-ID into view."
  (unless (and buffer todo-id)
    (return-from organa-focus-todo-by-id nil))
  (let* ((location (organa-read-buffer-location buffer))
         (model (organa-location-model location))
         (todo-index (position todo-id (organa-model-todos model)
                               :key #'organa-todo-id
                               :test #'string=)))
    (if todo-index
        (progn
          (setf (organa-view-for-buffer buffer) :outline
                (buffer-scroll-offset buffer) (max 0 (- todo-index 2)))
          (notify-buffer-display-change buffer :organa-focus)
          todo-id)
        (buffer-insert-system-message
         buffer
         (format nil "[Organa dependency target not found: ~A]" todo-id)
         :record-p nil))))

(defun organa-todo-description (todo)
  "Return help text for TODO."
  (with-output-to-string (stream)
    (format stream "TODO: ~A~%~%" (organa-todo-title todo))
    (format stream "ID: ~A~%" (organa-todo-id todo))
    (format stream "Status: ~A~%" (organa-todo-status todo))
    (format stream "Level: ~D~%" (organa-todo-level todo))
    (format stream "Lines: ~D-~D~%"
            (1+ (organa-todo-start todo))
            (organa-todo-end todo))
    (when (organa-todo-depends-on todo)
      (format stream "Depends on: ~{~A~^, ~}~%"
              (organa-todo-depends-on todo)))
    (when (organa-todo-tags todo)
      (format stream "Tags: ~{~A~^, ~}~%" (organa-todo-tags todo)))))

(defun organa-row-face (row)
  "Return the global face name for one Organa display ROW."
  (case (getf row :face)
    (:ready :tool-result)
    (:blocked :system)
    (:dependency :selector-entry)
    (:outline :default-text)
    (otherwise :default-text)))

(defun organa-stream-entry (row-data cols)
  "Return one CLIM stream entry plist for Organa ROW-DATA."
  (let* ((text (organa-fit (getf row-data :text) cols))
         (todo (getf row-data :todo))
         (object (or (getf row-data :object) todo))
         (presentation-type
           (or (getf row-data :presentation-type)
               (and todo 'organa-todo-ref))))
    (list :text text
          :face (organa-row-face row-data)
          :object object
          :presentation-type presentation-type)))

(defun organa-display-entries (buffer cols)
  "Return CLIM stream entries for BUFFER's current Organa view."
  (let* ((location (organa-read-buffer-location buffer))
         (model (organa-location-model location))
         (view (organa-view-for-buffer buffer))
         (header (format nil "Organa ~A | ~A | ~D TODO~:P"
                         (string-downcase (symbol-name view))
                         (organa-location-display location)
                         (length (organa-model-todos model))))
         (hint "M-x organa-add-todo-command, organa-link-todo-command, organa-cycle-view-command")
         (footer "File-backed view. Use presentations, M-x commands, or organa_* tools.")
         (rows (append (list (organa-render-row header)
                             (organa-render-row hint)
                             (organa-render-row ""))
                       (organa-view-rows model view cols)
                       (list (organa-render-row "")
                             (organa-render-row footer :face :dependency)))))
    (mapcar (lambda (row-data)
              (organa-stream-entry row-data cols))
            rows)))

(define-buffer-type :organa
  :description "Org-mode TODO project management buffer."
  :major-mode "organa"
  :document-p t
  :presentation-function 'organa-display-entries)

;;; --------------------------------------------------------------------------
;;; User commands
;;; --------------------------------------------------------------------------

(defun organa-find-open-buffer (pathname)
  "Return an open Organa buffer for PATHNAME."
  (let ((target (namestring pathname)))
    (find-if (lambda (buffer)
               (and (eq (buffer-kind buffer) :organa)
                    (string= target (or (buffer-resource-path buffer) ""))))
             *buffer-ring*)))

(defun organa-open-todo-file (path)
  "Open PATH as an Organa TODO buffer."
  (let* ((location (organa-resolve-location path :allow-missing t))
         (pathname (organa-location-pathname location))
         (existing (and pathname (organa-find-open-buffer pathname))))
    (if existing
        (switch-to-buffer existing)
        (let ((buffer (make-buffer
                       (format nil "organa:~A"
                               (file-namestring pathname))
                       :agent-name "organa"
                       :kind :organa
                       :working-directory
                       (uiop:pathname-directory-pathname pathname)
                       :resource-path (namestring pathname)
                       :original-text (organa-location-text location)
                       :dirty-p nil
                       :major-mode "organa")))
          (initialize-buffer-display-defaults buffer)
          (setf (organa-view-for-buffer buffer) *organa-default-view*)
          (add-buffer-to-ring buffer)
          (switch-to-buffer buffer)))))

(defun organa-open-todo-file-command (buffer path)
  "Open an org-mode TODO file in an Organa project buffer."
  (declare (ignore buffer))
  (organa-open-todo-file path))

(defcommand organa-open-todo-file-command
  :prompts ((path :prompt "Org TODO file path"))
  :docstring "Open an org-mode TODO file in an Organa project buffer.")

(defun organa-add-todo-command (buffer title)
  "Add a TODO to the current Organa file."
  (organa-apply-buffer-mutation
   buffer
   (lambda (lines path)
     (multiple-value-bind (new-lines id)
         (organa-add-todo-to-lines lines title :path path)
       (values new-lines id)))))

(defcommand organa-add-todo-command
  :prompts ((title :prompt "Todo title"))
  :docstring "Add a TODO to the current Organa file.")

(defun organa-set-todo-status-command (buffer todo status)
  "Set TODO's org status in the current Organa file."
  (organa-apply-buffer-mutation
   buffer
   (lambda (lines path)
     (values (organa-set-status-in-lines lines todo status :path path)
             status))))

(defcommand organa-set-todo-status-command
  :prompts ((todo :prompt "Todo ID or title")
            (status :prompt "Status"))
  :docstring "Set a TODO status in the current Organa file.")

(defun organa-move-todo-command (buffer todo after)
  "Move TODO after another TODO in the current Organa file.
Blank AFTER moves the TODO before the first headline."
  (organa-apply-buffer-mutation
   buffer
   (lambda (lines path)
     (values (organa-move-todo-in-lines lines todo after :path path)
             todo))))

(defcommand organa-move-todo-command
  :prompts ((todo :prompt "Todo ID or title")
            (after :prompt "Move after ID/title (blank for top)"))
  :docstring "Reorder a TODO in the current Organa file.")

(defun organa-link-todo-command (buffer todo depends-on)
  "Make TODO depend on DEPENDS-ON in the current Organa file."
  (organa-apply-buffer-mutation
   buffer
   (lambda (lines path)
     (values (organa-link-dependency-in-lines lines todo depends-on :path path)
             todo))))

(defcommand organa-link-todo-command
  :prompts ((todo :prompt "Todo ID or title")
            (depends-on :prompt "Depends on ID/title"))
  :docstring "Persist a dependency link between TODOs in the current Organa file.")

(defun organa-unlink-todo-command (buffer todo depends-on)
  "Remove TODO's dependency on DEPENDS-ON in the current Organa file."
  (organa-apply-buffer-mutation
   buffer
   (lambda (lines path)
     (values (organa-link-dependency-in-lines
              lines todo depends-on :path path :remove-p t)
             todo))))

(defcommand organa-unlink-todo-command
  :prompts ((todo :prompt "Todo ID or title")
            (depends-on :prompt "Dependency ID/title to remove"))
  :docstring "Remove a persisted dependency link between TODOs.")

(defun organa-cycle-view-command (buffer)
  "Cycle the current Organa buffer through dashboard, kanban, dependency, and outline views."
  (unless (eq (buffer-kind buffer) :organa)
    (error "Current buffer is not an Organa TODO buffer."))
  (setf (organa-view-for-buffer buffer)
        (organa-cycle-view (organa-view-for-buffer buffer)))
  (notify-buffer-display-change buffer :organa-view)
  buffer)

(defcommand organa-cycle-view-command
  :docstring "Cycle the current Organa buffer view.")

(define-clawmacs-chat-frame-command
    (com-organa-cycle-todo-status :name nil)
    ((todo 'organa-todo-ref))
  (clim:with-application-frame (frame)
    (organa-cycle-todo-status (chat-frame-buffer frame) todo)))

(define-clawmacs-chat-frame-command
    (com-organa-focus-dependency :name nil)
    ((dependency 'organa-dependency-ref))
  (clim:with-application-frame (frame)
    (organa-focus-todo-by-id (chat-frame-buffer frame) dependency)))

(define-clawmacs-chat-frame-command
    (com-organa-describe-todo :name nil)
    ((todo 'organa-todo-ref))
  (switch-to-buffer
   (make-help-buffer
    (format nil "*help:organa:~A*" (organa-todo-id todo))
    (organa-todo-description todo))))

(clim:define-presentation-to-command-translator select-organa-todo
    (organa-todo-ref com-organa-cycle-todo-status
     clawmacs-chat-frame
     :gesture :select
     :documentation "Cycle TODO status"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator describe-organa-todo
    (organa-todo-ref com-organa-describe-todo
     clawmacs-chat-frame
     :gesture :describe
     :documentation "Describe TODO"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-organa-dependency
    (organa-dependency-ref com-organa-focus-dependency
     clawmacs-chat-frame
     :gesture :select
     :documentation "Focus dependency target"
     :menu t)
    (object)
  (list object))

;;; --------------------------------------------------------------------------
;;; Tool result helpers
;;; --------------------------------------------------------------------------

(defun organa-todo-plist (todo)
  "Return TODO as Lisp data."
  (list :id (organa-todo-id todo)
        :persistent-id (organa-todo-persistent-id-p todo)
        :status (organa-todo-status todo)
        :title (organa-todo-title todo)
        :level (organa-todo-level todo)
        :line (1+ (organa-todo-start todo))
        :depends-on (coerce (copy-list (organa-todo-depends-on todo))
                            'vector)
        :tags (coerce (copy-list (organa-todo-tags todo)) 'vector)))

(defun organa-model-summary-plist (model &key (view "dashboard"))
  "Return MODEL summary as Lisp data."
  (let ((status-counts
          (loop :for (status . count) :in (organa-status-counts model)
                :collect (list :status status :count count))))
    (list :path (or (organa-model-path model) "")
          :view view
          :counts (coerce status-counts 'vector)
          :ready (coerce (mapcar #'organa-todo-plist
                                 (organa-ready-todos model))
                         'vector)
          :blocked (coerce (mapcar #'organa-todo-plist
                                   (organa-blocked-todos model))
                           'vector)
          :todos (coerce (mapcar #'organa-todo-plist
                                 (organa-model-todos model))
                         'vector))))

(defun organa-tool-location (args &key allow-missing)
  "Resolve Organa tool ARGS to a location."
  (let ((project (tool-arg args :project "project"))
        (path (tool-arg args :path "path")))
    (organa-resolve-location path :project project :allow-missing allow-missing)))

(defun organa-tool-overview (args)
  "Return structured overview for an org-mode TODO file."
  (let* ((location (organa-tool-location args))
         (model (organa-location-model location))
         (view (or (tool-arg args :view "view") "dashboard")))
    (lisp-data-string (organa-model-summary-plist model :view view))))

(defun organa-tool-add-todo (args)
  "Add a TODO to an org-mode TODO file."
  (let* ((location (organa-tool-location args :allow-missing t))
         (lines (organa-split-lines (organa-location-text location)))
         (title (tool-arg args :title "title"))
         (status (or (tool-arg args :status "status") "TODO"))
         (parent (tool-arg args :parent "parent")))
    (multiple-value-bind (new-lines id)
        (organa-add-todo-to-lines lines title
                                  :status status
                                  :parent parent
                                  :path (organa-location-display location))
      (let* ((new-text (organa-join-lines new-lines))
             (model (organa-parse-text new-text
                                       :path (organa-location-display
                                              location))))
        (organa-write-location location new-text)
        (lisp-data-string
         (list :ok t
               :id id
               :path (organa-location-display location)
               :summary (organa-model-summary-plist model)))))))

(defun organa-tool-set-status (args)
  "Set a TODO status in an org-mode TODO file."
  (let* ((location (organa-tool-location args))
         (lines (organa-split-lines (organa-location-text location)))
         (todo (tool-arg args :todo "todo"))
         (status (tool-arg args :status "status"))
         (new-lines (organa-set-status-in-lines
                     lines todo status
                     :path (organa-location-display location)))
         (new-text (organa-join-lines new-lines))
         (model (organa-parse-text new-text
                                   :path (organa-location-display location))))
    (organa-write-location location new-text)
    (lisp-data-string
     (list :ok t
           :path (organa-location-display location)
           :todo todo
           :status (organa-normalize-status status)
           :summary (organa-model-summary-plist model)))))

(defun organa-tool-move-todo (args)
  "Move a TODO in an org-mode TODO file."
  (let* ((location (organa-tool-location args))
         (lines (organa-split-lines (organa-location-text location)))
         (todo (tool-arg args :todo "todo"))
         (after (tool-arg args :after "after"))
         (new-lines (organa-move-todo-in-lines
                     lines todo after
                     :path (organa-location-display location)))
         (new-text (organa-join-lines new-lines)))
    (organa-write-location location new-text)
    (lisp-data-string
     (list :ok t
           :path (organa-location-display location)
           :todo todo
           :after after))))

(defun organa-tool-link-dependency (args)
  "Link or unlink TODO dependency in an org-mode TODO file."
  (let* ((location (organa-tool-location args))
         (lines (organa-split-lines (organa-location-text location)))
         (todo (tool-arg args :todo "todo"))
         (depends-on (tool-arg args :depends-on "depends_on"))
         (remove-p (not (null (tool-arg args :remove "remove"))))
         (new-lines (organa-link-dependency-in-lines
                     lines todo depends-on
                     :path (organa-location-display location)
                     :remove-p remove-p))
         (new-text (organa-join-lines new-lines))
         (model (organa-parse-text new-text
                                   :path (organa-location-display location))))
    (organa-write-location location new-text)
    (lisp-data-string
     (list :ok t
           :path (organa-location-display location)
           :todo todo
           :depends-on depends-on
           :removed remove-p
           :summary (organa-model-summary-plist model)))))

(defun organa-tool-approval-display (args)
  "Return approval context for Organa file mutations."
  (let ((project (tool-arg args :project "project"))
        (path (tool-arg args :path "path"))
        (todo (tool-arg args :todo "todo"))
        (title (tool-arg args :title "title")))
    (format nil "Organa TODO file: ~@[~A:~]~A~@[~%TODO: ~A~]~@[~%Title: ~A~]"
            project
            path
            todo
            title)))

;;; --------------------------------------------------------------------------
;;; Package surface
;;; --------------------------------------------------------------------------

(register-package-prompt-section
 "organa"
 "## Project TODO management with organa

- Organa manages org-mode TODO files as project plans. Use it when the user
  wants structured TODO management, lightweight project planning, dependency
  chains, kanban-style review, or an agenda-like view over an org file.
- Prefer the `organa_*` tools instead of raw `lisp_eval` or ad hoc file edits.
- `organa_todo_overview` reads TODOs, status counts, unblocked ready work, and
  blocked dependency chains.
- `organa_todo_add`, `organa_todo_set_status`, `organa_todo_move`, and
  `organa_todo_link_dependency` write directly to the org file.
- Dependencies are persisted inside each TODO's org property drawer using
  `ID` and `ORGANA_DEPENDS`. Existing org content is preserved as ordinary org
  text where possible.
- In the McCLIM UI, open an org file with `M-x organa-open-todo-file-command`.
  Organa buffers can cycle dashboard, kanban, dependency, and outline views."
 :title "Organa org TODO project management"
 :package "organa")

(deftool organa-tool-overview
  :name "organa_todo_overview"
  :description "Read an org-mode TODO file and return TODOs, status counts, ready work, and dependency-blocked work."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. When supplied, path is project-relative.")
         (path :type "string"
               :description "Sandbox-local org file path, or project-relative path when project is supplied.")
         (view :type "string" :required nil
               :description "Optional intended view: dashboard, kanban, dependency, or outline.")))

(deftool organa-tool-add-todo
  :name "organa_todo_add"
  :description "Add a TODO heading to an org-mode TODO file. Creates an ID property for the new TODO."
  :permission :agent-with-permission
  :call-style :raw-args
  :approval-display-fn organa-tool-approval-display
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. When supplied, path is project-relative.")
         (path :type "string"
               :description "Sandbox-local org file path, or project-relative path when project is supplied.")
         (title :type "string"
                :description "New TODO title.")
         (status :type "string" :required nil
                 :description "Initial status. Defaults to TODO.")
         (parent :type "string" :required nil
                 :description "Optional parent TODO ID or exact title.")))

(deftool organa-tool-set-status
  :name "organa_todo_set_status"
  :description "Set an org TODO status by ID or exact title."
  :permission :agent-with-permission
  :call-style :raw-args
  :approval-display-fn organa-tool-approval-display
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. When supplied, path is project-relative.")
         (path :type "string"
               :description "Sandbox-local org file path, or project-relative path when project is supplied.")
         (todo :type "string"
               :description "TODO ID, transient line id from overview, or exact title.")
         (status :type "string"
                 :description "New status: TODO, NEXT, WAITING, BLOCKED, DONE, or CANCELLED.")))

(deftool organa-tool-move-todo
  :name "organa_todo_move"
  :description "Move a TODO subtree before the first headline or after another TODO subtree."
  :permission :agent-with-permission
  :call-style :raw-args
  :approval-display-fn organa-tool-approval-display
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. When supplied, path is project-relative.")
         (path :type "string"
               :description "Sandbox-local org file path, or project-relative path when project is supplied.")
         (todo :type "string"
               :description "TODO ID, transient line id from overview, or exact title to move.")
         (after :type "string" :required nil
                :description "TODO ID or exact title to move after. Blank or omitted moves to the top.")))

(deftool organa-tool-link-dependency
  :name "organa_todo_link_dependency"
  :description "Persist or remove a dependency relation between two org TODOs using ID and ORGANA_DEPENDS properties."
  :permission :agent-with-permission
  :call-style :raw-args
  :approval-display-fn organa-tool-approval-display
  :args ((project :type "string" :required nil
                  :description "Optional Clawmacs project name. When supplied, path is project-relative.")
         (path :type "string"
               :description "Sandbox-local org file path, or project-relative path when project is supplied.")
         (todo :type "string"
               :description "TODO ID, transient line id from overview, or exact title that should be blocked.")
         (depends-on :type "string"
                     :description "TODO ID, transient line id from overview, or exact title that must finish first.")
         (remove :type "boolean" :required nil
                 :description "When true, remove this dependency instead of adding it.")))

(defdoc organa-open-todo-file-command
  :category "organa"
  :usage "(organa-open-todo-file-command BUFFER PATH)"
  :returns "buffer - An Organa project TODO buffer."
  :side-effects "Opens or creates PATH as an Organa buffer and switches to it."
  :see-also (organa-add-todo-command organa-cycle-view-command))

(defdoc organa-add-todo-command
  :category "organa"
  :usage "(organa-add-todo-command BUFFER TITLE)"
  :returns "string - The new TODO id."
  :side-effects "Writes a new TODO heading with an ID property to the current Organa file."
  :see-also (organa-set-todo-status-command organa-link-todo-command))

(defdoc organa-set-todo-status-command
  :category "organa"
  :usage "(organa-set-todo-status-command BUFFER TODO STATUS)"
  :returns "string - The selected status."
  :side-effects "Rewrites TODO's org heading status in the current Organa file."
  :see-also (organa-add-todo-command organa-cycle-view-command))

(defdoc organa-move-todo-command
  :category "organa"
  :usage "(organa-move-todo-command BUFFER TODO AFTER)"
  :returns "string - The moved TODO selector."
  :side-effects "Moves TODO's org subtree before the first headline or after another TODO subtree."
  :see-also (organa-add-todo-command organa-link-todo-command))

(defdoc organa-link-todo-command
  :category "organa"
  :usage "(organa-link-todo-command BUFFER TODO DEPENDS-ON)"
  :returns "string - The TODO selector."
  :side-effects "Ensures both TODOs have IDs and writes TODO's ORGANA_DEPENDS property."
  :see-also (organa-unlink-todo-command organa-cycle-view-command))

(defdoc organa-unlink-todo-command
  :category "organa"
  :usage "(organa-unlink-todo-command BUFFER TODO DEPENDS-ON)"
  :returns "string - The TODO selector."
  :side-effects "Removes DEPENDS-ON from TODO's ORGANA_DEPENDS property."
  :see-also (organa-link-todo-command organa-cycle-view-command))

(defdoc organa-cycle-view-command
  :category "organa"
  :usage "(organa-cycle-view-command BUFFER)"
  :returns "buffer - The current buffer."
  :side-effects "Cycles dashboard, kanban, dependency, and outline presentation views."
  :see-also (organa-open-todo-file-command))
