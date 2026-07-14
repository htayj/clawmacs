(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Sexed: agent-oriented s-expression editing
;;; --------------------------------------------------------------------------

(defstruct sexed-node
  "Internal source-span node for a Lisp form."
  id
  type
  start
  token-start
  opener-start
  closer-start
  end
  depth
  parent
  prefixes
  children
  head
  name)

(defparameter +sexed-missing+ (gensym "SEXED-MISSING-")
  "Internal marker for absent selector keys.")

(defparameter *sexed-read-form-count-limit* 2000
  "Structural form count above which sexed read tools return a safety page.")

(defun sexed-whitespace-char-p (char)
  "Return T when CHAR is whitespace for source scanning."
  (member char '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun sexed-delimiter-char-p (char)
  "Return T when CHAR terminates an atom token."
  (or (sexed-whitespace-char-p char)
      (member char '(#\( #\) #\" #\; #\' #\` #\,) :test #'char=)))

(defun sexed-starts-with-p (text start needle)
  "Return T when TEXT contains NEEDLE at START."
  (let ((end (+ start (length needle))))
    (and (<= end (length text))
         (string= needle text :start2 start :end2 end))))

(defun sexed-substring (text start end)
  "Return TEXT slice START..END, clamped to TEXT length."
  (let ((safe-start (max 0 (min start (length text))))
        (safe-end (max 0 (min end (length text)))))
    (subseq text safe-start (max safe-start safe-end))))

(defun sexed-collapse-whitespace (text)
  "Collapse repeated whitespace in TEXT for preview display."
  (with-output-to-string (out)
    (let ((pending-space nil)
          (wrote-anything nil))
      (loop :for char :across text
            :do (cond
                  ((sexed-whitespace-char-p char)
                   (setf pending-space t))
                  (t
                   (when (and pending-space wrote-anything)
                     (write-char #\Space out))
                   (setf pending-space nil
                         wrote-anything t)
                   (write-char char out)))))))

(defun sexed-truncate-preview (text max-chars)
  "Return TEXT truncated to MAX-CHARS with an ellipsis marker."
  (let ((clean (sexed-collapse-whitespace
                (string-trim '(#\Space #\Tab #\Newline #\Return)
                             text))))
    (if (and max-chars (> (length clean) max-chars))
        (concatenate 'string (subseq clean 0 max-chars) "...")
        clean)))

(defun sexed-designator-string (designator)
  "Normalize DESIGNATOR to a string for selector matching."
  (typecase designator
    (null nil)
    (string designator)
    (symbol (symbol-name designator))
    (t (princ-to-string designator))))

(defun sexed-symbol-tail (string)
  "Return STRING after its last package marker, when present."
  (let ((pos (position #\: string :from-end t)))
    (if pos
        (subseq string (1+ pos))
        string)))

(defun sexed-token-match-p (token designator)
  "Return T when TOKEN matches DESIGNATOR exactly or by package-stripped tail."
  (let ((wanted (sexed-designator-string designator)))
    (and token
         wanted
         (or (string-equal token wanted)
             (string-equal (sexed-symbol-tail token) wanted)))))

(defun sexed-node-text (node text)
  "Return NODE's source text."
  (sexed-substring text (sexed-node-start node) (sexed-node-end node)))

(defun sexed-node-token-text (node text)
  "Return NODE's token text without reader prefixes."
  (sexed-substring text (sexed-node-token-start node) (sexed-node-end node)))

(defun sexed-node-designator-text (node text)
  "Return a compact text designator for NODE."
  (case (sexed-node-type node)
    ((:atom :string)
     (sexed-node-token-text node text))
    (t
     (sexed-truncate-preview (sexed-node-text node text) 80))))

(defun sexed-node-prefix-text (node text)
  "Return NODE's reader-prefix source text."
  (if (< (sexed-node-start node) (sexed-node-token-start node))
      (sexed-substring text (sexed-node-start node) (sexed-node-token-start node))
      ""))

(defun sexed-diagnostic (kind position message)
  "Build a sexed diagnostic plist."
  (list :kind kind :position position :message message))

(defun sexed-parse-text (text)
  "Parse TEXT into sexed nodes.
Returns three values: all nodes in source order, diagnostics, and root nodes."
  (let ((length (length text))
        (next-id 0)
        (nodes nil)
        (roots nil)
        (diagnostics nil))
    (labels ((record-diagnostic (kind position message)
               (push (sexed-diagnostic kind position message) diagnostics))
             (allocate-node (&key type start token-start opener-start depth parent prefixes)
               (let ((node (make-sexed-node
                            :id next-id
                            :type type
                            :start start
                            :token-start token-start
                            :opener-start opener-start
                            :depth depth
                            :parent parent
                            :prefixes prefixes
                            :children nil)))
                 (incf next-id)
                 (push node nodes)
                 node))
             (parse-block-comment (index)
               (let ((depth 1)
                     (pos (+ index 2)))
                 (loop :while (< pos length)
                       :do (cond
                             ((sexed-starts-with-p text pos "#|")
                              (incf depth)
                              (incf pos 2))
                             ((sexed-starts-with-p text pos "|#")
                              (decf depth)
                              (incf pos 2)
                              (when (zerop depth)
                                (return-from parse-block-comment pos)))
                             (t
                              (incf pos))))
                 (record-diagnostic :unterminated-block-comment
                                    index
                                    "Unterminated #| block comment.")
                 length))
             (skip-ignored (index)
               (let ((pos index))
                 (loop :while (< pos length)
                       :for char := (char text pos)
                       :do (cond
                             ((sexed-whitespace-char-p char)
                              (incf pos))
                             ((char= char #\;)
                              (let ((newline (position #\Newline text :start pos)))
                                (setf pos (if newline (1+ newline) length))))
                             ((sexed-starts-with-p text pos "#|")
                              (setf pos (parse-block-comment pos)))
                             (t
                              (return pos)))
                       :finally (return pos))))
             (parse-prefixes (index)
               (let ((pos index)
                     (prefix-start nil)
                     (prefixes nil))
                 (loop
                   (setf pos (skip-ignored pos))
                   (when (>= pos length)
                     (return (values prefix-start (nreverse prefixes) pos)))
                   (cond
                     ((member (char text pos) '(#\' #\`) :test #'char=)
                      (unless prefix-start
                        (setf prefix-start pos))
                      (push (string (char text pos)) prefixes)
                      (incf pos))
                     ((char= (char text pos) #\,)
                      (unless prefix-start
                        (setf prefix-start pos))
                      (if (and (< (1+ pos) length)
                               (char= (char text (1+ pos)) #\@))
                          (progn
                            (push ",@" prefixes)
                            (incf pos 2))
                          (progn
                            (push "," prefixes)
                            (incf pos))))
                     ((or (sexed-starts-with-p text pos "#'")
                          (sexed-starts-with-p text pos "#."))
                      (unless prefix-start
                        (setf prefix-start pos))
                      (push (subseq text pos (+ pos 2)) prefixes)
                      (incf pos 2))
                     (t
                      (return (values prefix-start (nreverse prefixes) pos)))))))
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
                              (return (1+ pos)))
                             (t
                              (incf pos)))
                       :finally
                          (record-diagnostic :unterminated-string
                                             index
                                             "Unterminated string literal.")
                          (return length))))
             (scan-token-end (index)
               (let ((pos index)
                     (escaped nil)
                     (bar-escaped nil)
                     (inside-bar nil))
                 (loop :while (< pos length)
                       :for char := (char text pos)
                       :do (cond
                             (inside-bar
                              (cond
                                (bar-escaped
                                 (setf bar-escaped nil)
                                 (incf pos))
                                ((char= char #\\)
                                 (setf bar-escaped t)
                                 (incf pos))
                                ((char= char #\|)
                                 (setf inside-bar nil)
                                 (incf pos))
                                (t
                                 (incf pos))))
                             (escaped
                              (setf escaped nil)
                              (incf pos))
                             ((char= char #\\)
                              (setf escaped t)
                              (incf pos))
                             ((char= char #\|)
                              (setf inside-bar t)
                              (incf pos))
                             ((sexed-delimiter-char-p char)
                              (return pos))
                             (t
                              (incf pos)))
                       :finally
                          (when inside-bar
                            (record-diagnostic :unterminated-token-escape
                                               index
                                               "Unterminated |...| token escape."))
                          (return pos))))
             (scan-character-literal-end (index)
               (let ((body-start (+ index 2)))
                 (cond
                   ((>= body-start length)
                    (record-diagnostic :unterminated-character-literal
                                       index
                                       "Unterminated #\\ character literal.")
                    length)
                   ((sexed-delimiter-char-p (char text body-start))
                    (1+ body-start))
                   (t
                    (scan-token-end index)))))
             (finalize-list-node (node children close-position end-position)
               (let ((ordered-children (nreverse children)))
                 (setf (sexed-node-children node) ordered-children
                       (sexed-node-closer-start node) close-position
                       (sexed-node-end node) end-position)
                 (when ordered-children
                   (let ((head-node (first ordered-children))
                         (name-node (second ordered-children)))
                     (when (eq :atom (sexed-node-type head-node))
                       (setf (sexed-node-head node)
                             (sexed-node-designator-text head-node text)))
                     (when name-node
                       (setf (sexed-node-name node)
                             (sexed-node-designator-text name-node text)))))
                 node))
             (parse-list (index parent depth source-start token-start
                         opener-start prefixes type)
               (let ((node (allocate-node :type type
                                          :start source-start
                                          :token-start token-start
                                          :opener-start opener-start
                                          :depth depth
                                          :parent parent
                                          :prefixes prefixes))
                     (children nil)
                     (pos (1+ opener-start)))
                 (loop
                   (setf pos (skip-ignored pos))
                   (cond
                     ((>= pos length)
                      (record-diagnostic :missing-close-paren
                                         opener-start
                                         "Missing closing parenthesis.")
                      (finalize-list-node node children nil length)
                      (return (values node length)))
                     ((char= (char text pos) #\))
                      (finalize-list-node node children pos (1+ pos))
                      (return (values node (1+ pos))))
                     (t
                      (multiple-value-bind (child next-pos)
                          (parse-form pos (sexed-node-id node) (1+ depth))
                        (when child
                          (push child children))
                        (setf pos (max next-pos (1+ pos)))))))))
             (parse-string-node (index parent depth source-start prefixes)
               (let* ((end (scan-string-end index))
                      (node (allocate-node :type :string
                                           :start source-start
                                           :token-start index
                                           :depth depth
                                           :parent parent
                                           :prefixes prefixes)))
                 (setf (sexed-node-end node) end)
                 (values node end)))
             (parse-atom-node (index parent depth source-start prefixes
                              &optional forced-end)
               (let* ((end (or forced-end (scan-token-end index)))
                      (safe-end (if (= end index) (min length (1+ index)) end))
                      (node (allocate-node :type :atom
                                           :start source-start
                                           :token-start index
                                           :depth depth
                                           :parent parent
                                           :prefixes prefixes)))
                 (setf (sexed-node-end node) safe-end)
                 (values node safe-end)))
             (parse-sharp-string-atom (index parent depth source-start prefixes)
               (let ((string-start (+ index 2)))
                 (if (and (< string-start length)
                          (char= (char text string-start) #\"))
                     (parse-atom-node index parent depth source-start prefixes
                                      (scan-string-end string-start))
                     (parse-atom-node index parent depth source-start prefixes))))
             (parse-dispatch-form (index parent depth source-start prefixes)
               (cond
                 ((sexed-starts-with-p text index "#|")
                  (record-diagnostic :unexpected-comment
                                     index
                                     "Internal parser error: comment reached form parser.")
                  (values nil (parse-block-comment index)))
                 ((sexed-starts-with-p text index "#(")
                  (parse-list index parent depth source-start index
                              (1+ index)
                              prefixes
                              :vector))
                 ((sexed-starts-with-p text index "#\\")
                  (parse-atom-node index parent depth source-start prefixes
                                   (scan-character-literal-end index)))
                 ((and (< (+ index 2) length)
                       (alpha-char-p (char text (1+ index)))
                       (char= (char text (+ index 2)) #\"))
                  (parse-sharp-string-atom index parent depth source-start prefixes))
                 (t
                  (parse-atom-node index parent depth source-start prefixes))))
             (parse-form (index parent depth)
               (let ((initial (skip-ignored index)))
                 (multiple-value-bind (prefix-start prefixes pos)
                     (parse-prefixes initial)
                   (let ((source-start (or prefix-start pos)))
                     (cond
                       ((>= pos length)
                        (when prefix-start
                          (record-diagnostic :dangling-reader-prefix
                                             prefix-start
                                             "Reader prefix has no following form."))
                        (values nil length))
                       ((char= (char text pos) #\()
                        (parse-list pos parent depth source-start pos pos prefixes :list))
                       ((char= (char text pos) #\))
                        (record-diagnostic :unexpected-close-paren
                                           pos
                                           "Unexpected closing parenthesis.")
                        (values nil (1+ pos)))
                       ((char= (char text pos) #\")
                        (parse-string-node pos parent depth source-start prefixes))
                       ((char= (char text pos) #\#)
                        (parse-dispatch-form pos parent depth source-start prefixes))
                       (t
                        (parse-atom-node pos parent depth source-start prefixes))))))))
      (let ((pos 0))
        (loop
          (setf pos (skip-ignored pos))
          (when (>= pos length)
            (return))
          (multiple-value-bind (node next-pos)
              (parse-form pos nil 0)
            (when node
              (push node roots))
            (setf pos (max next-pos (1+ pos))))))
      (values (nreverse nodes)
              (nreverse diagnostics)
              (nreverse roots)))))

(defun sexed-balanced-p (text)
  "Return T when TEXT has balanced s-expression structure."
  (null (sexed-diagnostics text)))

(defun balanced-parentheses-p (text)
  "Alias for SEXED-BALANCED-P with a name agents often infer."
  (sexed-balanced-p text))

(defun sexed-diagnostics (text)
  "Return parser diagnostics for TEXT."
  (nth-value 1 (sexed-parse-text text)))

(defun sexed-node-plist (node text &key (preview-chars 96))
  "Return an agent-friendly plist describing NODE."
  (list :id (sexed-node-id node)
        :type (sexed-node-type node)
        :depth (sexed-node-depth node)
        :parent (sexed-node-parent node)
        :start (sexed-node-start node)
        :end (sexed-node-end node)
        :head (sexed-node-head node)
        :name (sexed-node-name node)
        :prefix (sexed-node-prefix-text node text)
        :child-count (length (sexed-node-children node))
        :preview (sexed-truncate-preview (sexed-node-text node text) preview-chars)))

(defun sexed-structural-form-node-p (node &key include-atoms)
  "Return T when NODE should count as a structural read item."
  (or include-atoms
      (member (sexed-node-type node) '(:list :vector) :test #'eq)))

(defun sexed-filter-nodes (nodes text &key head name depth max-depth containing
                                      (nth +sexed-missing+)
                                      include-atoms limit)
  "Filter NODES according to agent-facing selector fields."
  (let ((matches
          (remove-if-not
           (lambda (node)
             (and (or include-atoms
                      (member (sexed-node-type node) '(:list :vector) :test #'eq))
                  (or (null depth) (= (sexed-node-depth node) depth))
                  (or (null max-depth) (<= (sexed-node-depth node) max-depth))
                  (or (null head) (sexed-token-match-p (sexed-node-head node) head))
                  (or (null name) (sexed-token-match-p (sexed-node-name node) name))
                  (or (null containing)
                      (search (sexed-designator-string containing)
                              (sexed-node-text node text)
                              :test #'char-equal))))
           nodes)))
    (unless (eq nth +sexed-missing+)
      (setf matches (let ((node (nth nth matches)))
                      (if node (list node) nil))))
    (when limit
      (setf matches (subseq matches 0 (min limit (length matches)))))
    matches))

(defun sexed-find-forms (text &key head name depth max-depth containing nth
                                include-atoms limit)
  "Return source-span plists for forms in TEXT matching the supplied filters."
  (multiple-value-bind (nodes diagnostics)
      (sexed-parse-text text)
    (when diagnostics
      (error "Cannot inspect unbalanced text: ~A" diagnostics))
    (mapcar (lambda (node)
              (sexed-node-plist node text))
            (sexed-filter-nodes nodes text
                                :head head
                                :name name
                                :depth depth
                                :max-depth max-depth
                                :containing containing
                                :nth (if nth nth +sexed-missing+)
                                :include-atoms include-atoms
                                :limit limit))))

(defun sexed-getf (plist indicator &optional (default +sexed-missing+))
  "GETF wrapper that can distinguish NIL from missing keys."
  (loop :for (key value) :on plist :by #'cddr
        :when (eq key indicator)
          :do (return value)
        :finally (return default)))

(defun sexed-selector-plist (selector)
  "Normalize SELECTOR to a plist."
  (cond
    ((integerp selector)
     (list :id selector))
    ((and (listp selector) (evenp (length selector)))
     selector)
    (t
     (error "Invalid sexed selector: ~S" selector))))

(defun sexed-find-node-by-id (nodes id)
  "Return the node with ID from NODES, or NIL."
  (find id nodes :key #'sexed-node-id :test #'=))

(defun sexed-resolve-selector (nodes text selector &key scope)
  "Resolve SELECTOR to exactly one node from NODES."
  (let* ((plist (sexed-selector-plist selector))
         (id (sexed-getf plist :id))
         (include-atoms (not (null (sexed-getf plist :include-atoms nil)))))
    (if (not (eq id +sexed-missing+))
        (let ((node (or (find id (or scope nodes) :key #'sexed-node-id :test #'=)
                        (error "No sexed form with id ~A." id))))
          (unless (or include-atoms
                      (member (sexed-node-type node) '(:list :vector)
                              :test #'eq))
            (error "Selector id ~A refers to an atom. Use an id from an outline, or set include-atoms explicitly."
                   id))
          node)
        (let* ((nth (sexed-getf plist :nth))
               (matches
                 (sexed-filter-nodes (or scope nodes)
                                     text
                                     :head (let ((value (sexed-getf plist :head nil)))
                                             value)
                                     :name (let ((value (sexed-getf plist :name nil)))
                                             value)
                                     :depth (let ((value (sexed-getf plist :depth nil)))
                                              value)
                                     :max-depth (let ((value (sexed-getf plist :max-depth nil)))
                                                  value)
                                     :containing (let ((value (sexed-getf plist :containing nil)))
                                                   value)
                                     :nth (if (eq nth +sexed-missing+)
                                              +sexed-missing+
                                              nth)
                                     :include-atoms include-atoms
                                     :limit nil)))
          (case (length matches)
            (0 (error "No sexed form matches selector ~S." selector))
            (1 (first matches))
            (t (error "Selector ~S is ambiguous; matched ids ~{~A~^, ~}."
                      selector
                      (mapcar #'sexed-node-id matches))))))))

(defun sexed-parse-balanced (text)
  "Parse TEXT or signal when diagnostics are present."
  (multiple-value-bind (nodes diagnostics roots)
      (sexed-parse-text text)
    (when diagnostics
      (error "Unbalanced s-expression text: ~A" diagnostics))
    (values nodes roots)))

(defun sexed-form-text (text selector)
  "Return the source text for the form selected by SELECTOR."
  (multiple-value-bind (nodes)
      (sexed-parse-balanced text)
    (let ((node (sexed-resolve-selector nodes text selector)))
      (sexed-node-text node text))))

(defun sexed-outline-to-string (text &key depth max-depth head limit
                                       (preview-chars 96))
  "Return an outline of source forms in TEXT."
  (multiple-value-bind (nodes diagnostics)
      (sexed-parse-text text)
    (with-output-to-string (out)
      (labels ((write-node (node)
                 (format out "[~D] d~D ~(~A~)~@[ ~A~]~@[ ~A~] ~D..~D  ~A~%"
                         (sexed-node-id node)
                         (sexed-node-depth node)
                         (sexed-node-type node)
                         (sexed-node-head node)
                         (sexed-node-name node)
                         (sexed-node-start node)
                         (sexed-node-end node)
                         (sexed-truncate-preview
                          (sexed-node-text node text)
                          preview-chars)))
               (write-nodes (nodes)
                 (dolist (node nodes)
                   (write-node node))))
        (when diagnostics
          (format out "Diagnostics:~%")
          (dolist (diagnostic diagnostics)
            (format out "  ~A at ~D: ~A~%"
                    (getf diagnostic :kind)
                    (getf diagnostic :position)
                    (getf diagnostic :message)))
          (terpri out))
        (let ((matches (sexed-filter-nodes nodes text
                                           :depth depth
                                           :max-depth max-depth
                                           :head head
                                           :limit limit)))
          (cond
            (matches
             (write-nodes matches))
            ((or depth max-depth head)
             (format out "No forms matched for the supplied filters.~%")
             (let ((fallback (sexed-filter-nodes nodes text
                                                 :max-depth 0
                                                 :limit 20)))
               (when fallback
                 (format out "Unfiltered top-level forms:~%")
                 (write-nodes fallback))))
            (t
             (format out "No forms matched.~%"))))))))

(defun sexed-structural-form-count (nodes)
  "Return the count of list/vector forms in NODES."
  (count-if #'sexed-structural-form-node-p nodes))

(defun sexed-node-within-p (node container)
  "Return T when NODE's source span is inside CONTAINER."
  (and (>= (sexed-node-start node) (sexed-node-start container))
       (<= (sexed-node-end node) (sexed-node-end container))))

(defun sexed-read-scope (nodes text selector)
  "Return NODES scoped to SELECTOR, plus the selected node when present."
  (if selector
      (let ((node (sexed-resolve-selector nodes text selector)))
        (values (remove-if-not (lambda (candidate)
                                 (sexed-node-within-p candidate node))
                               nodes)
                node))
      (values nodes nil)))

(defun sexed-read-level-nodes (nodes selected-node level include-atoms)
  "Return structural read nodes at LEVEL relative to SELECTED-NODE or file."
  (let ((base-depth (if selected-node
                        (sexed-node-depth selected-node)
                        0)))
    (remove-if-not
     (lambda (node)
       (and (sexed-structural-form-node-p node :include-atoms include-atoms)
            (= (- (sexed-node-depth node) base-depth) level)))
     nodes)))

(defun sexed-read-page-slice (nodes offset limit)
  "Return a 1-indexed page slice of NODES."
  (let* ((start (min (length nodes) (max 0 (1- offset))))
         (end (min (length nodes) (+ start limit))))
    (values (subseq nodes start end)
            (and (< end (length nodes)) (1+ end)))))

(defun sexed-write-read-page-node (stream node text preview-chars)
  "Write one node line for a sexed structural read page."
  (format stream "[~D] d~D ~(~A~)~@[ ~A~]~@[ ~A~] ~D..~D  ~A~%"
          (sexed-node-id node)
          (sexed-node-depth node)
          (sexed-node-type node)
          (sexed-node-head node)
          (sexed-node-name node)
          (sexed-node-start node)
          (sexed-node-end node)
          (sexed-truncate-preview (sexed-node-text node text) preview-chars)))

(defun sexed-read-page-to-string (text nodes diagnostics
                                  &key source tool-name selector
                                    offset limit level include-atoms
                                    reason total-structural-forms
                                    (preview-chars 96))
  "Return a structural safety page for TEXT."
  (multiple-value-bind (scope selected-node)
      (sexed-read-scope nodes text selector)
    (let* ((scope-structural-forms (sexed-structural-form-count scope))
           (items (sexed-read-level-nodes scope selected-node level
                                          include-atoms))
           (item-count (length items)))
      (multiple-value-bind (page next-offset)
          (sexed-read-page-slice items offset limit)
        (with-output-to-string (out)
          (format out "Sexed structural read page~%")
          (format out "Source: ~A~%" source)
          (format out "Mode: ~A~%"
                  (ecase reason
                    (:safety "safety fallback for a structurally large file")
                    (:explicit "explicit structural page request")))
          (format out "Total structural forms: ~D~%" total-structural-forms)
          (when selected-node
            (format out "Scope: selector id ~D (~D structural form~:P)~%"
                    (sexed-node-id selected-node)
                    scope-structural-forms))
          (unless selected-node
            (format out "Scope: whole file~%"))
          (format out "Page: offset ~D, limit ~D, level ~D, include-atoms ~A~%"
                  offset limit level (if include-atoms "true" "false"))
          (format out "Items at this level: ~D; returned: ~D~%"
                  item-count (length page))
          (when diagnostics
            (format out "Diagnostics:~%")
            (dolist (diagnostic diagnostics)
              (format out "  ~A at ~D: ~A~%"
                      (getf diagnostic :kind)
                      (getf diagnostic :position)
                      (getf diagnostic :message))))
          (when next-offset
            (format out "Continue: call ~A with offset ~D, limit ~D, level ~D~@[ and the same selector~].~%"
                    tool-name next-offset limit level selected-node))
          (format out "Drill down: call ~A with selector {id: <id>} and level 1. Use full=true only when exact source text is required.~%"
                  tool-name)
          (terpri out)
          (cond
            (page
             (dolist (node page)
               (sexed-write-read-page-node out node text preview-chars)))
            (t
             (format out "No structural items matched this page. Try offset 1, a different level, or a narrower selector.~%"))))))))

(defun sexed-read-text-with-safety (text &key source tool-name selector
                                      (offset 1)
                                      (limit *sexed-read-form-count-limit*)
                                      (level 0)
                                      include-atoms
                                      full
                                      paginate)
  "Return TEXT unless structural safety pagination is needed or requested."
  (cond
    (full
     (if selector
         (sexed-form-text text selector)
         text))
    (t
     (multiple-value-bind (nodes diagnostics)
         (sexed-parse-text text)
       (let* ((total-structural-forms (sexed-structural-form-count nodes))
              (safety-p (> total-structural-forms
                           *sexed-read-form-count-limit*)))
         (if (or paginate safety-p)
             (sexed-read-page-to-string
              text nodes diagnostics
              :source source
              :tool-name tool-name
              :selector selector
              :offset offset
              :limit limit
              :level level
              :include-atoms include-atoms
              :reason (if safety-p :safety :explicit)
              :total-structural-forms total-structural-forms)
             text))))))

(defun sexed-read-file-text-with-safety (path &rest options)
  "Read PATH, returning exact text or a structural safety page."
  (apply #'sexed-read-text-with-safety
         (sexed-read-file-text path)
         :source path
         :tool-name "sexed_file_read"
         options))

(defun sexed-read-project-file-text-with-safety (project path &rest options)
  "Read PROJECT/PATH, returning exact text or a structural safety page."
  (apply #'sexed-read-text-with-safety
         (project-read-file project path)
         :source (format nil "~A/~A" project path)
         :tool-name "sexed_project_read"
         options))

(defun sexed-ensure-balanced (text context)
  "Signal an error unless TEXT is structurally balanced."
  (let ((diagnostics (sexed-diagnostics text)))
    (when diagnostics
      (error "~A is not balanced: ~A" context diagnostics)))
  text)

(defun sexed-replace-span (text start end replacement)
  "Replace TEXT span START..END with REPLACEMENT."
  (concatenate 'string
               (subseq text 0 start)
               replacement
               (subseq text end)))

(defun sexed-insertion-prefix-needed-p (text position insertion)
  "Return T when INSERTION needs a leading space at POSITION in TEXT."
  (and (plusp (length insertion))
       (not (sexed-whitespace-char-p (char insertion 0)))
       (> position 0)
       (let ((previous (char text (1- position))))
         (and (not (sexed-whitespace-char-p previous))
              (not (char= previous #\())))))

(defun sexed-insertion-suffix-needed-p (text position insertion)
  "Return T when INSERTION needs a trailing space at POSITION in TEXT."
  (and (plusp (length insertion))
       (not (sexed-whitespace-char-p (char insertion (1- (length insertion)))))
       (< position (length text))
       (let ((next (char text position)))
         (and (not (sexed-whitespace-char-p next))
              (not (char= next #\)))))))

(defun sexed-normalize-insertion (text position insertion)
  "Add separator whitespace around INSERTION when adjacent forms touch."
  (concatenate 'string
               (if (sexed-insertion-prefix-needed-p text position insertion)
                   " "
                   "")
               insertion
               (if (sexed-insertion-suffix-needed-p text position insertion)
                   " "
                   "")))

(defun sexed-leading-newline-count (text)
  "Return the number of leading newline characters in TEXT."
  (loop :for index :below (length text)
        :while (char= #\Newline (char text index))
        :count t))

(defun sexed-top-level-insert-after-prefix (text position insertion node)
  "Return whitespace prefix for inserting INSERTION after top-level NODE."
  (declare (ignore text))
  (if (and (zerop (sexed-node-depth node))
           (plusp (length insertion))
           (plusp position))
      (make-string (max 0 (- 2 (sexed-leading-newline-count insertion)))
                   :initial-element #\Newline)
      ""))

(defun sexed-replace-form (text selector new-text)
  "Return TEXT with SELECTOR's form replaced by NEW-TEXT."
  (sexed-ensure-balanced new-text "Replacement text")
  (multiple-value-bind (nodes)
      (sexed-parse-balanced text)
    (let* ((node (sexed-resolve-selector nodes text selector))
           (result (sexed-replace-span text
                                       (sexed-node-start node)
                                       (sexed-node-end node)
                                       new-text)))
      (sexed-ensure-balanced result "Edited text"))))

(defun sexed-delete-form (text selector)
  "Return TEXT with SELECTOR's form removed."
  (multiple-value-bind (nodes)
      (sexed-parse-balanced text)
    (let* ((node (sexed-resolve-selector nodes text selector))
           (result (sexed-replace-span text
                                       (sexed-node-start node)
                                       (sexed-node-end node)
                                       "")))
      (sexed-ensure-balanced result "Edited text"))))

(defun sexed-insert-before-form (text selector new-text)
  "Return TEXT with NEW-TEXT inserted before SELECTOR's form."
  (sexed-ensure-balanced new-text "Inserted text")
  (multiple-value-bind (nodes)
      (sexed-parse-balanced text)
    (let* ((node (sexed-resolve-selector nodes text selector))
           (insertion (sexed-normalize-insertion text
                                                 (sexed-node-start node)
                                                 new-text))
           (result (sexed-replace-span text
                                       (sexed-node-start node)
                                       (sexed-node-start node)
                                       insertion)))
      (sexed-ensure-balanced result "Edited text"))))

(defun sexed-insert-after-form (text selector new-text)
  "Return TEXT with NEW-TEXT inserted after SELECTOR's form."
  (sexed-ensure-balanced new-text "Inserted text")
  (multiple-value-bind (nodes)
      (sexed-parse-balanced text)
    (let* ((node (sexed-resolve-selector nodes text selector))
           (position (sexed-node-end node))
           (prefix (sexed-top-level-insert-after-prefix text position
                                                        new-text
                                                        node))
           (insertion (if (plusp (length prefix))
                          (concatenate 'string prefix new-text)
                          (sexed-normalize-insertion text position new-text)))
           (result (sexed-replace-span text
                                       position
                                       position
                                       insertion)))
      (sexed-ensure-balanced result "Edited text"))))

(defun sexed-insert-form-before (text selector new-text)
  "Alias for SEXED-INSERT-BEFORE-FORM with natural command ordering."
  (sexed-insert-before-form text selector new-text))

(defun sexed-insert-form-after (text selector new-text)
  "Alias for SEXED-INSERT-AFTER-FORM with natural command ordering."
  (sexed-insert-after-form text selector new-text))

(defun sexed-wrap-form (text selector prefix suffix)
  "Return TEXT with SELECTOR's form wrapped by PREFIX and SUFFIX."
  (multiple-value-bind (nodes)
      (sexed-parse-balanced text)
    (let* ((node (sexed-resolve-selector nodes text selector))
           (wrapped (concatenate 'string prefix (sexed-node-text node text) suffix))
           (result (sexed-replace-span text
                                       (sexed-node-start node)
                                       (sexed-node-end node)
                                       wrapped)))
      (sexed-ensure-balanced result "Edited text"))))

(defun sexed-list-like-node-p (node)
  "Return T when NODE has surrounding parentheses."
  (member (sexed-node-type node) '(:list :vector) :test #'eq))

(defun sexed-splice-form (text selector)
  "Return TEXT with SELECTOR's outer list delimiters removed."
  (multiple-value-bind (nodes)
      (sexed-parse-balanced text)
    (let ((node (sexed-resolve-selector nodes text selector)))
      (unless (sexed-list-like-node-p node)
        (error "Cannot splice non-list form id ~D." (sexed-node-id node)))
      (unless (and (sexed-node-opener-start node)
                   (sexed-node-closer-start node))
        (error "Cannot splice unbalanced form id ~D." (sexed-node-id node)))
      (let* ((inner (sexed-substring text
                                     (1+ (sexed-node-opener-start node))
                                     (sexed-node-closer-start node)))
             (result (sexed-replace-span text
                                         (sexed-node-start node)
                                         (sexed-node-end node)
                                         inner)))
        (sexed-ensure-balanced result "Edited text")))))

(defun sexed-descendant-nodes (node nodes)
  "Return descendants of NODE from NODES."
  (remove-if-not (lambda (candidate)
                   (and (> (sexed-node-start candidate) (sexed-node-start node))
                        (< (sexed-node-end candidate) (sexed-node-end node))))
                 nodes))

(defun sexed-raise-form (text selector child-selector)
  "Return TEXT with SELECTOR replaced by one selected child or descendant form."
  (multiple-value-bind (nodes)
      (sexed-parse-balanced text)
    (let* ((node (sexed-resolve-selector nodes text selector))
           (descendants (sexed-descendant-nodes node nodes))
           (child (sexed-resolve-selector nodes text child-selector
                                          :scope descendants))
           (result (sexed-replace-span text
                                       (sexed-node-start node)
                                       (sexed-node-end node)
                                       (sexed-node-text child text))))
      (sexed-ensure-balanced result "Edited text"))))

(defun sexed-node-siblings (node roots)
  "Return NODE's siblings from ROOTS or its parent's children."
  (if (sexed-node-parent node)
      (let ((parent (labels ((find-parent (nodes)
                              (loop :for candidate :in nodes
                                    :when (= (sexed-node-id candidate)
                                             (sexed-node-parent node))
                                      :return candidate
                                    :thereis (find-parent
                                               (sexed-node-children candidate)))))
                      (find-parent roots))))
        (sexed-node-children parent))
      roots))

(defun sexed-next-siblings (node roots count)
  "Return COUNT siblings after NODE."
  (let* ((siblings (sexed-node-siblings node roots))
         (position (position (sexed-node-id node)
                             siblings
                             :key #'sexed-node-id
                             :test #'=))
         (start (and position (1+ position)))
         (end (and start (+ start count))))
    (unless (and position (<= end (length siblings)))
      (error "Form id ~D does not have ~D following sibling~:P."
             (sexed-node-id node)
             count))
    (subseq siblings start end)))

(defun sexed-slurp-forward (text selector &key (count 1))
  "Return TEXT after moving COUNT following sibling forms into SELECTOR's list."
  (unless (plusp count)
    (error "COUNT must be positive."))
  (multiple-value-bind (nodes roots)
      (sexed-parse-balanced text)
    (let ((node (sexed-resolve-selector nodes text selector)))
      (unless (sexed-list-like-node-p node)
        (error "Cannot slurp into non-list form id ~D." (sexed-node-id node)))
      (unless (sexed-node-closer-start node)
        (error "Cannot slurp into unbalanced form id ~D." (sexed-node-id node)))
      (let* ((siblings (sexed-next-siblings node roots count))
             (last-sibling (car (last siblings)))
             (close-index (sexed-node-closer-start node))
             (close-char (char text close-index))
             (result (concatenate 'string
                                  (subseq text 0 close-index)
                                  (subseq text (sexed-node-end node)
                                          (sexed-node-end last-sibling))
                                  (string close-char)
                                  (subseq text (sexed-node-end last-sibling)))))
        (sexed-ensure-balanced result "Edited text")))))

(defun sexed-barf-forward (text selector &key (count 1))
  "Return TEXT after moving COUNT trailing child forms out of SELECTOR's list."
  (unless (plusp count)
    (error "COUNT must be positive."))
  (multiple-value-bind (nodes)
      (sexed-parse-balanced text)
    (let* ((node (sexed-resolve-selector nodes text selector))
           (children (sexed-node-children node)))
      (unless (sexed-list-like-node-p node)
        (error "Cannot barf from non-list form id ~D." (sexed-node-id node)))
      (unless (sexed-node-closer-start node)
        (error "Cannot barf from unbalanced form id ~D." (sexed-node-id node)))
      (unless (< count (length children))
        (error "Cannot barf ~D child form~:P from form id ~D with ~D child~:P."
               count
               (sexed-node-id node)
               (length children)))
      (let* ((first-moving-index (- (length children) count))
             (previous-child (nth (1- first-moving-index) children))
             (segment-start (sexed-node-end previous-child))
             (close-index (sexed-node-closer-start node))
             (close-char (char text close-index))
             (moved-segment (subseq text segment-start close-index))
             (result (concatenate 'string
                                  (subseq text 0 segment-start)
                                  (string close-char)
                                  moved-segment
                                  (subseq text (sexed-node-end node)))))
        (sexed-ensure-balanced result "Edited text")))))

(defun sexed-resolve-path (path)
  "Resolve PATH against the current tool working directory."
  (lispi:resolve-tool-path path))

(defun sexed-read-file-text (path)
  "Read resolved PATH as a string."
  (let ((resolved (sexed-resolve-path path)))
    (unless (probe-file resolved)
      (error "File not found: ~A" path))
    (uiop:read-file-string resolved)))

(defun sexed-write-file-text (path text)
  "Write TEXT to resolved PATH and return an edit summary."
  (let ((resolved (sexed-resolve-path path))
        (balanced-text (sexed-ensure-balanced text "Written file text")))
    (ensure-directories-exist resolved)
    (with-open-file (stream resolved
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string balanced-text stream))
    (list :status :ok
          :path (namestring resolved)
          :bytes-written (length balanced-text)
          :balanced t)))

(defun sexed-update-file (path edit-fn)
  "Apply EDIT-FN to PATH's text, write the result, and return a summary."
  (let* ((old-text (sexed-read-file-text path))
         (new-text (funcall edit-fn old-text)))
    (sexed-write-file-text path new-text)))

(defun sexed-file-outline-to-string (path &rest options)
  "Return a sexed outline for PATH."
  (apply #'sexed-outline-to-string (sexed-read-file-text path) options))

(defun sexed-file-form-text (path selector)
  "Return source text for SELECTOR in PATH."
  (sexed-form-text (sexed-read-file-text path) selector))

(defun sexed-replace-file-form (path selector new-text)
  "Replace SELECTOR in PATH with NEW-TEXT and return a summary plist."
  (sexed-update-file path
                     (lambda (text)
                       (sexed-replace-form text selector new-text))))

(defun sexed-delete-file-form (path selector)
  "Delete SELECTOR in PATH and return a summary plist."
  (sexed-update-file path
                     (lambda (text)
                       (sexed-delete-form text selector))))

(defun sexed-insert-before-file-form (path selector new-text)
  "Insert NEW-TEXT before SELECTOR in PATH and return a summary plist."
  (sexed-update-file path
                     (lambda (text)
                       (sexed-insert-before-form text selector new-text))))

(defun sexed-insert-after-file-form (path selector new-text)
  "Insert NEW-TEXT after SELECTOR in PATH and return a summary plist."
  (sexed-update-file path
                     (lambda (text)
                       (sexed-insert-after-form text selector new-text))))

(defun sexed-insert-file-form-before (path selector new-text)
  "Alias for SEXED-INSERT-BEFORE-FILE-FORM with natural command ordering."
  (sexed-insert-before-file-form path selector new-text))

(defun sexed-insert-file-form-after (path selector new-text)
  "Alias for SEXED-INSERT-AFTER-FILE-FORM with natural command ordering."
  (sexed-insert-after-file-form path selector new-text))

(defun sexed-wrap-file-form (path selector prefix suffix)
  "Wrap SELECTOR in PATH with PREFIX and SUFFIX and return a summary plist."
  (sexed-update-file path
                     (lambda (text)
                       (sexed-wrap-form text selector prefix suffix))))

(defun sexed-splice-file-form (path selector)
  "Splice SELECTOR in PATH and return a summary plist."
  (sexed-update-file path
                     (lambda (text)
                       (sexed-splice-form text selector))))

(defun sexed-raise-file-form (path selector child-selector)
  "Raise CHILD-SELECTOR out of SELECTOR in PATH and return a summary plist."
  (sexed-update-file path
                     (lambda (text)
                       (sexed-raise-form text selector child-selector))))

(defun sexed-slurp-forward-file-form (path selector &key (count 1))
  "Slurp following forms into SELECTOR in PATH and return a summary plist."
  (sexed-update-file path
                     (lambda (text)
                       (sexed-slurp-forward text selector :count count))))

(defun sexed-barf-forward-file-form (path selector &key (count 1))
  "Barf trailing child forms out of SELECTOR in PATH and return a summary plist."
  (sexed-update-file path
                     (lambda (text)
                       (sexed-barf-forward text selector :count count))))

(defun sexed-update-project-file (project path edit-fn)
  "Apply EDIT-FN to PROJECT/PATH, save the result, and return a summary."
  (let* ((old-text (project-read-file project path))
         (new-text (funcall edit-fn old-text))
         (summary (project-save-file project path new-text)))
    (append summary (list :balanced t))))

(defun sexed-project-outline-to-string (project path &rest options)
  "Return a sexed outline for PROJECT/PATH."
  (apply #'sexed-outline-to-string (project-read-file project path) options))

(defun sexed-project-form-text (project path selector)
  "Return source text for SELECTOR in PROJECT/PATH."
  (sexed-form-text (project-read-file project path) selector))

(defun sexed-write-project-file-text (project path text)
  "Write TEXT to PROJECT/PATH after balance validation and return a summary."
  (let ((balanced-text (sexed-ensure-balanced text "Written project file text")))
    (append (project-save-file project path balanced-text)
            (list :balanced t))))

(defun sexed-replace-project-form (project path selector new-text)
  "Replace SELECTOR in PROJECT/PATH with NEW-TEXT and return a summary plist."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-replace-form text selector new-text))))

(defun sexed-delete-project-form (project path selector)
  "Delete SELECTOR in PROJECT/PATH and return a summary plist."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-delete-form text selector))))

(defun sexed-insert-before-project-form (project path selector new-text)
  "Insert NEW-TEXT before SELECTOR in PROJECT/PATH and return a summary plist."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-insert-before-form text selector new-text))))

(defun sexed-insert-after-project-form (project path selector new-text)
  "Insert NEW-TEXT after SELECTOR in PROJECT/PATH and return a summary plist."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-insert-after-form text selector new-text))))

(defun sexed-insert-project-form-before (project path selector new-text)
  "Alias for SEXED-INSERT-BEFORE-PROJECT-FORM with natural command ordering."
  (sexed-insert-before-project-form project path selector new-text))

(defun sexed-insert-project-form-after (project path selector new-text)
  "Alias for SEXED-INSERT-AFTER-PROJECT-FORM with natural command ordering."
  (sexed-insert-after-project-form project path selector new-text))

(defun sexed-wrap-project-form (project path selector prefix suffix)
  "Wrap SELECTOR in PROJECT/PATH with PREFIX and SUFFIX."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-wrap-form text selector prefix suffix))))

(defun sexed-splice-project-form (project path selector)
  "Splice SELECTOR in PROJECT/PATH and return a summary plist."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-splice-form text selector))))

(defun sexed-raise-project-form (project path selector child-selector)
  "Raise CHILD-SELECTOR out of SELECTOR in PROJECT/PATH."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-raise-form text selector child-selector))))

(defun sexed-slurp-forward-project-form (project path selector &key (count 1))
  "Slurp following forms into SELECTOR in PROJECT/PATH."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-slurp-forward text selector
                                                     :count count))))

(defun sexed-barf-forward-project-form (project path selector &key (count 1))
  "Barf trailing child forms out of SELECTOR in PROJECT/PATH."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-barf-forward text selector
                                                   :count count))))

;;; --------------------------------------------------------------------------
;;; Sexed Agent Tool Adapters
;;; --------------------------------------------------------------------------

(defun sexed-tool-key-name (key)
  "Return KEY normalized for provider argument matching."
  (let ((previous-hyphen-p nil))
    (with-output-to-string (out)
      (loop :for char :across (tool-key-name key)
            :for normalized := (if (char= char #\_) #\- char)
            :do (cond
                  ((char= normalized #\-)
                   (unless previous-hyphen-p
                     (write-char normalized out))
                   (setf previous-hyphen-p t))
                  (t
                   (write-char normalized out)
                   (setf previous-hyphen-p nil)))))))

(defun sexed-tool-key= (left right)
  "Return true when LEFT and RIGHT name the same Sexed tool argument."
  (string= (sexed-tool-key-name left)
           (sexed-tool-key-name right)))

(defun sexed-tool-arg-value (args key &optional default)
  "Return KEY's value from tool ARGS, distinguishing missing from NIL."
  (loop :for (arg-key . value) :in (tool-args-alist args)
        :when (sexed-tool-key= arg-key key)
          :return (values value t)
        :finally (return (values default nil))))

(defun sexed-tool-required-arg (args key)
  "Return required KEY from tool ARGS."
  (multiple-value-bind (value present-p)
      (sexed-tool-arg-value args key)
    (unless present-p
      (error "Missing required sexed tool argument :~A." (tool-key-name key)))
    value))

(defun sexed-tool-required-string (args key)
  "Return required string KEY from tool ARGS."
  (let ((value (sexed-tool-required-arg args key)))
    (unless (stringp value)
      (error "Sexed tool argument :~A must be a string, got ~S."
             (tool-key-name key)
             value))
    value))

(defun sexed-tool-arg-value-any (args keys &optional default)
  "Return the first supplied value for KEYS from tool ARGS."
  (loop :for key :in keys
        :do (multiple-value-bind (value present-p)
                (sexed-tool-arg-value args key)
              (when present-p
                (return (values value t))))
        :finally (return (values default nil))))

(defun sexed-tool-required-string-any (args keys label)
  "Return a required string from the first supplied key in KEYS."
  (multiple-value-bind (value present-p)
      (sexed-tool-arg-value-any args keys)
    (unless present-p
      (error "Missing required sexed tool argument :~A." label))
    (unless (stringp value)
      (error "Sexed tool argument :~A must be a string, got ~S."
             label
             value))
    value))

(defun sexed-tool-required-new-text (args)
  "Return replacement or insertion text from common provider key names."
  (sexed-tool-required-string-any
   args
   '(:new-text :newtext :replacement :replacement-text :replacementtext
     :inserted-text :insertedtext :insert-text :inserttext :content)
   "new-text"))

(defun sexed-tool-optional-integer (args key default)
  "Return optional integer KEY from tool ARGS."
  (multiple-value-bind (value present-p)
      (sexed-tool-arg-value args key)
    (cond
      ((not present-p) (values default nil))
      ((or (null value) (integerp value)) (values value t))
      (t
       (error "Sexed tool argument :~A must be an integer, got ~S."
              (tool-key-name key)
              value)))))

(defun sexed-tool-optional-boolean (args key default)
  "Return optional boolean KEY from tool ARGS."
  (multiple-value-bind (value present-p)
      (sexed-tool-arg-value args key)
    (cond
      ((not present-p) (values default nil))
      ((or (eq value t) (eq value nil)) (values value t))
      ((stringp value)
       (let ((trimmed (string-downcase
                       (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    value))))
         (cond
           ((string= trimmed "true") (values t t))
           ((string= trimmed "false") (values nil t))
           ((blank-string-p trimmed) (values default t))
           (t
            (error "Sexed tool argument :~A must be a boolean, got ~S."
                   (tool-key-name key)
                   value)))))
      (t
       (error "Sexed tool argument :~A must be a boolean, got ~S."
              (tool-key-name key)
              value)))))

(defun sexed-tool-keyword (key)
  "Normalize KEY into a keyword symbol."
  (intern (string-upcase (sexed-tool-key-name key)) :keyword))

(defun sexed-tool-normalize-plist (plist)
  "Normalize PLIST keys into keywords."
  (loop :for (key value) :on plist :by #'cddr
        :append (list (sexed-tool-keyword key) value)))

(defun sexed-tool-alist-p (value)
  "Return true when VALUE looks like an alist."
  (and (listp value)
       (every #'consp value)))

(defun sexed-tool-normalize-alist (alist)
  "Normalize ALIST keys into a keyword plist."
  (loop :for (key . value) :in alist
        :append (list (sexed-tool-keyword key) value)))

(defun sexed-tool-selector (value)
  "Normalize a provider selector value into a sexed selector."
  (cond
    ((integerp value) value)
    ((stringp value)
     (handler-case
         (sexed-tool-selector (lisp-data-read value))
       (error ()
         (error "Selector string must contain Lisp data such as (:id 0), got ~S."
                value))))
    ((tool-plist-p value)
     (sexed-tool-normalize-plist value))
    ((sexed-tool-alist-p value)
     (sexed-tool-normalize-alist value))
    (t
     (error "Invalid sexed selector: ~S." value))))

(defun sexed-tool-required-selector (args)
  "Return required :SELECTOR from tool ARGS."
  (sexed-tool-selector (sexed-tool-required-arg args :selector)))

(defun sexed-tool-optional-selector (args)
  "Return optional :SELECTOR from tool ARGS."
  (multiple-value-bind (value present-p)
      (sexed-tool-arg-value args :selector)
    (if present-p
        (values (sexed-tool-selector value) t)
        (values nil nil))))

(defun sexed-tool-options (args)
  "Return outline options from tool ARGS as keyword arguments."
  (let ((options nil))
    (dolist (key '(:depth :max-depth :head :limit :preview-chars))
      (multiple-value-bind (value present-p)
          (sexed-tool-arg-value args key)
        (when (and present-p
                   (not (and (stringp value)
                             (blank-string-p value))))
          (setf options (append options (list key value))))))
    options))

(defun sexed-tool-edit-operation (args)
  "Return the normalized edit operation requested by tool ARGS."
  (let* ((raw (sexed-tool-required-arg args :operation))
         (name (substitute #\- #\_ (tool-key-name raw)))
         (operation (intern (string-upcase name) :keyword)))
    (unless (member operation
                    '(:replace :delete :insert-before :insert-after
                      :wrap :splice :raise :slurp-forward :barf-forward)
                    :test #'eq)
      (error "Unsupported sexed edit operation: ~S." raw))
    operation))

(defun sexed-tool-count (args)
  "Return edit count from tool ARGS."
  (or (sexed-tool-optional-integer args :count 1) 1))

(defun sexed-tool-positive-integer (value key)
  "Return VALUE or signal if it is not a positive integer."
  (unless (and (integerp value) (plusp value))
    (error "Sexed tool argument :~A must be a positive integer, got ~S."
           (tool-key-name key)
           value))
  value)

(defun sexed-tool-nonnegative-integer (value key)
  "Return VALUE or signal if it is not a non-negative integer."
  (unless (and (integerp value) (not (minusp value)))
    (error "Sexed tool argument :~A must be a non-negative integer, got ~S."
           (tool-key-name key)
           value))
  value)

(defun sexed-tool-read-options (args)
  "Return keyword options for safety-paginated read tools."
  (multiple-value-bind (selector selector-present-p)
      (sexed-tool-optional-selector args)
    (multiple-value-bind (offset offset-present-p)
        (sexed-tool-optional-integer args :offset 1)
      (multiple-value-bind (limit limit-present-p)
          (sexed-tool-optional-integer args
                                       :limit
                                       *sexed-read-form-count-limit*)
        (multiple-value-bind (level level-present-p)
            (sexed-tool-optional-integer args :level 0)
          (multiple-value-bind (include-atoms include-atoms-present-p)
              (sexed-tool-optional-boolean args :include-atoms nil)
            (multiple-value-bind (full full-present-p)
                (sexed-tool-optional-boolean args :full nil)
              (declare (ignore full-present-p))
              (list :selector selector
                    :offset (sexed-tool-positive-integer offset :offset)
                    :limit (sexed-tool-positive-integer limit :limit)
                    :level (sexed-tool-nonnegative-integer level :level)
                    :include-atoms include-atoms
                    :full full
                    :paginate (or selector-present-p
                                  offset-present-p
                                  limit-present-p
                                  level-present-p
                                  include-atoms-present-p)))))))))

(defun sexed-tool-list (value label)
  "Return VALUE as a list, accepting provider vectors."
  (cond
    ((listp value) value)
    ((vectorp value) (coerce value 'list))
    (t
     (error "Sexed tool argument :~A must be a list or vector, got ~S."
            label
            value))))

(defun sexed-tool-required-edits (args)
  "Return required batch edit specs from ARGS."
  (let ((edits (sexed-tool-list (sexed-tool-required-arg args :edits)
                                "edits")))
    (when (null edits)
      (error "Sexed tool argument :edits must contain at least one edit."))
    edits))

(defun sexed-tool-apply-text-edit (text args)
  "Apply a structural edit described by ARGS to TEXT."
  (let ((selector (sexed-tool-required-selector args)))
    (ecase (sexed-tool-edit-operation args)
      (:replace
       (sexed-replace-form text selector
                           (sexed-tool-required-new-text args)))
      (:delete
       (sexed-delete-form text selector))
      (:insert-before
       (sexed-insert-before-form text selector
                                 (sexed-tool-required-new-text args)))
      (:insert-after
       (sexed-insert-after-form text selector
                                (sexed-tool-required-new-text args)))
      (:wrap
       (sexed-wrap-form text
                        selector
                        (sexed-tool-required-string args :prefix)
                        (sexed-tool-required-string args :suffix)))
      (:splice
       (sexed-splice-form text selector))
      (:raise
       (sexed-raise-form
        text
        selector
        (sexed-tool-selector (sexed-tool-required-arg args :child-selector))))
      (:slurp-forward
       (sexed-slurp-forward text selector :count (sexed-tool-count args)))
      (:barf-forward
       (sexed-barf-forward text selector :count (sexed-tool-count args))))))

(defun sexed-tool-apply-text-edits (text edits)
  "Apply EDITS sequentially to TEXT and return values NEW-TEXT and COUNT."
  (let ((current text)
        (count 0))
    (dolist (edit edits)
      (setf current (sexed-tool-apply-text-edit current edit))
      (incf count))
    (values (sexed-ensure-balanced current "Edited text")
            count)))

(defun sexed-tool-text-diagnostics (args)
  "Return diagnostics for :TEXT."
  (let ((text (sexed-tool-required-string args :text)))
    (list :balanced (sexed-balanced-p text)
          :diagnostics (sexed-diagnostics text))))

(defun sexed-tool-text-outline (args)
  "Return a structural outline for :TEXT."
  (apply #'sexed-outline-to-string
         (sexed-tool-required-string args :text)
         (sexed-tool-options args)))

(defun sexed-tool-text-form-text (args)
  "Return selected source form text from :TEXT."
  (sexed-form-text (sexed-tool-required-string args :text)
                   (sexed-tool-required-selector args)))

(defun sexed-tool-text-edit (args)
  "Return :TEXT after a structural edit."
  (sexed-tool-apply-text-edit (sexed-tool-required-string args :text) args))

(defun sexed-tool-text-edits (args)
  "Return :TEXT after applying a batch of structural edits."
  (multiple-value-bind (text count)
      (sexed-tool-apply-text-edits (sexed-tool-required-string args :text)
                                   (sexed-tool-required-edits args))
    (list :text text
          :edits-applied count
          :balanced t)))

(defun sexed-tool-file-read (args)
  "Read a Lisp source file."
  (apply #'sexed-read-file-text-with-safety
         (sexed-tool-required-string args :path)
         (sexed-tool-read-options args)))

(defun sexed-tool-file-write (args)
  "Write a Lisp source file."
  (sexed-write-file-text
   (sexed-tool-required-string args :path)
   (sexed-tool-required-string args :content)))

(defun sexed-tool-file-outline (args)
  "Return a structural outline for a Lisp source file."
  (apply #'sexed-file-outline-to-string
         (sexed-tool-required-string args :path)
         (sexed-tool-options args)))

(defun sexed-tool-file-form-text (args)
  "Return selected source form text from a Lisp source file."
  (sexed-file-form-text (sexed-tool-required-string args :path)
                        (sexed-tool-required-selector args)))

(defun sexed-tool-file-edit (args)
  "Structurally edit a Lisp source file."
  (let* ((path (sexed-tool-required-string args :path))
         (selector (sexed-tool-required-selector args)))
    (ecase (sexed-tool-edit-operation args)
      (:replace
       (sexed-replace-file-form path selector
                                (sexed-tool-required-new-text args)))
      (:delete
       (sexed-delete-file-form path selector))
      (:insert-before
       (sexed-insert-before-file-form path selector
                                      (sexed-tool-required-new-text args)))
      (:insert-after
       (sexed-insert-after-file-form path selector
                                     (sexed-tool-required-new-text args)))
      (:wrap
       (sexed-wrap-file-form path
                             selector
                             (sexed-tool-required-string args :prefix)
                             (sexed-tool-required-string args :suffix)))
      (:splice
       (sexed-splice-file-form path selector))
      (:raise
       (sexed-raise-file-form
        path
        selector
        (sexed-tool-selector (sexed-tool-required-arg args :child-selector))))
      (:slurp-forward
       (sexed-slurp-forward-file-form path selector
                                      :count (sexed-tool-count args)))
      (:barf-forward
       (sexed-barf-forward-file-form path selector
                                     :count (sexed-tool-count args))))))

(defun sexed-tool-file-edits (args)
  "Apply a batch of structural edits to a Lisp source file."
  (let* ((path (sexed-tool-required-string args :path))
         (old-text (sexed-read-file-text path)))
    (multiple-value-bind (new-text count)
        (sexed-tool-apply-text-edits old-text (sexed-tool-required-edits args))
      (append (sexed-write-file-text path new-text)
              (list :edits-applied count)))))

(defun sexed-tool-project-read (args)
  "Read PROJECT/PATH as Lisp source text."
  (apply #'sexed-read-project-file-text-with-safety
         (sexed-tool-required-string args :project)
         (sexed-tool-required-string args :path)
         (sexed-tool-read-options args)))

(defun sexed-tool-project-write (args)
  "Write PROJECT/PATH as Lisp source text."
  (sexed-write-project-file-text
   (sexed-tool-required-string args :project)
   (sexed-tool-required-string args :path)
   (sexed-tool-required-string args :content)))

(defun sexed-tool-project-outline (args)
  "Return a structural outline for PROJECT/PATH."
  (apply #'sexed-project-outline-to-string
         (sexed-tool-required-string args :project)
         (sexed-tool-required-string args :path)
         (sexed-tool-options args)))

(defun sexed-tool-project-form-text (args)
  "Return selected source form text from PROJECT/PATH."
  (sexed-project-form-text (sexed-tool-required-string args :project)
                           (sexed-tool-required-string args :path)
                           (sexed-tool-required-selector args)))

(defun sexed-tool-project-edit (args)
  "Structurally edit PROJECT/PATH."
  (let* ((project (sexed-tool-required-string args :project))
         (path (sexed-tool-required-string args :path))
         (selector (sexed-tool-required-selector args)))
    (ecase (sexed-tool-edit-operation args)
      (:replace
       (sexed-replace-project-form project path selector
                                   (sexed-tool-required-new-text args)))
      (:delete
       (sexed-delete-project-form project path selector))
      (:insert-before
       (sexed-insert-before-project-form project path selector
                                         (sexed-tool-required-new-text args)))
      (:insert-after
       (sexed-insert-after-project-form project path selector
                                        (sexed-tool-required-new-text args)))
      (:wrap
       (sexed-wrap-project-form project
                                path
                                selector
                                (sexed-tool-required-string args :prefix)
                                (sexed-tool-required-string args :suffix)))
      (:splice
       (sexed-splice-project-form project path selector))
      (:raise
       (sexed-raise-project-form
        project
        path
        selector
        (sexed-tool-selector (sexed-tool-required-arg args :child-selector))))
      (:slurp-forward
       (sexed-slurp-forward-project-form project path selector
                                         :count (sexed-tool-count args)))
      (:barf-forward
       (sexed-barf-forward-project-form project path selector
                                        :count (sexed-tool-count args))))))

(defun sexed-tool-project-edits (args)
  "Apply a batch of structural edits to a project file."
  (let* ((project (sexed-tool-required-string args :project))
         (path (sexed-tool-required-string args :path))
         (old-text (project-read-file project path)))
    (multiple-value-bind (new-text count)
        (sexed-tool-apply-text-edits old-text (sexed-tool-required-edits args))
      (append (sexed-write-project-file-text project path new-text)
              (list :edits-applied count)))))
