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

(defun sexed-selector-guidance ()
  "Return recovery guidance for failed agent-facing selectors."
  "Tip: Do not guess sexed selectors. Discover form ids with sexed-outline-to-string or sexed-project-outline-to-string, or verify filters with sexed-find-forms :limit, then retry with an exact :id or verified :head/:name selector.")

(defun sexed-resolve-selector (nodes text selector &key scope)
  "Resolve SELECTOR to exactly one node from NODES."
  (let* ((plist (sexed-selector-plist selector))
         (id (sexed-getf plist :id)))
    (if (not (eq id +sexed-missing+))
        (or (find id (or scope nodes) :key #'sexed-node-id :test #'=)
            (error "No sexed form with id ~A. ~A"
                   id
                   (sexed-selector-guidance)))
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
                                     :include-atoms
                                     (not (null (sexed-getf plist :include-atoms nil)))
                                     :limit nil)))
          (case (length matches)
            (0 (error "No sexed form matches selector ~S. ~A"
                      selector
                      (sexed-selector-guidance)))
            (1 (first matches))
            (t (error "Selector ~S is ambiguous; matched ids ~{~A~^, ~}. Retry with one of those :id values or add :name/:depth/:nth after verifying the outline. ~A"
                      selector
                      (mapcar #'sexed-node-id matches)
                      (sexed-selector-guidance))))))))

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
        (if matches
            (dolist (node matches)
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
            (format out "No forms matched.~%"))))))

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

(defun sexed-top-level-insertion-prefix-needed-p (text position insertion)
  "Return T when a top-level INSERTION needs a leading newline."
  (and (plusp (length insertion))
       (not (char= (char insertion 0) #\Newline))
       (> position 0)
       (not (char= (char text (1- position)) #\Newline))))

(defun sexed-top-level-insertion-suffix-needed-p (text position insertion)
  "Return T when a top-level INSERTION needs a trailing newline."
  (and (plusp (length insertion))
       (not (char= (char insertion (1- (length insertion))) #\Newline))
       (< position (length text))
       (not (char= (char text position) #\Newline))))

(defun sexed-normalize-insertion (text position insertion &key top-level-p)
  "Add separator whitespace around INSERTION when adjacent forms touch."
  (if top-level-p
      (concatenate 'string
                   (if (sexed-top-level-insertion-prefix-needed-p text
                                                                   position
                                                                   insertion)
                       (string #\Newline)
                       "")
                   insertion
                   (if (sexed-top-level-insertion-suffix-needed-p text
                                                                   position
                                                                   insertion)
                       (string #\Newline)
                       ""))
      (concatenate 'string
                   (if (sexed-insertion-prefix-needed-p text position insertion)
                       " "
                       "")
                   insertion
                   (if (sexed-insertion-suffix-needed-p text position insertion)
                       " "
                       ""))))

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
                                                 new-text
                                                 :top-level-p (zerop (sexed-node-depth node))))
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
           (insertion (sexed-normalize-insertion text
                                                 (sexed-node-end node)
                                                 new-text
                                                 :top-level-p (zerop (sexed-node-depth node))))
           (result (sexed-replace-span text
                                       (sexed-node-end node)
                                       (sexed-node-end node)
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
  "Resolve PATH using the clawmacs sandbox path policy."
  (validate-sandbox-path path))

(defun sexed-read-file-text (path)
  "Read PATH as a string after sandbox validation."
  (let ((resolved (sexed-resolve-path path)))
    (unless (probe-file resolved)
      (error "File not found: ~A" path))
    (uiop:read-file-string resolved)))

(defun sexed-write-file-text (path text)
  "Write TEXT to PATH after sandbox validation and return an edit summary."
  (let ((resolved (sexed-resolve-path path)))
    (with-open-file (stream resolved
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :error)
      (write-string text stream))
    (list :status :ok
          :path (namestring resolved)
          :bytes-written (length text)
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

(defun sexed-source-form-to-string (form)
  "Render FORM as readable lowercase Common Lisp source."
  (let ((*package* (find-package :clawmacs))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-readably* t)
        (*print-circle* t))
    (write-to-string form
                     :escape t
                     :pretty t
                     :readably t
                     :case :downcase
                     :circle t)))

(defun sexed-source-list-form-to-string (form)
  "Render FORM as source, requiring a list form suitable for insertion."
  (unless (consp form)
    (error "A quoted Lisp list form is expected, got ~S. Use the non-WITH-FORM helper for raw source text."
           form))
  (sexed-source-form-to-string form))

(defun sexed-project-outline-to-string (project path &rest options)
  "Return a sexed outline for PROJECT/PATH."
  (apply #'sexed-outline-to-string (project-read-file project path) options))

(defun sexed-project-form-text (project path selector)
  "Return source text for SELECTOR in PROJECT/PATH."
  (sexed-form-text (project-read-file project path) selector))

(defun sexed-replace-project-form (project path selector new-text)
  "Replace SELECTOR in PROJECT/PATH with NEW-TEXT and return a summary plist."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-replace-form text selector new-text))))

(defun sexed-replace-project-form-with-form (project path selector form)
  "Replace SELECTOR in PROJECT/PATH with FORM rendered as Lisp source."
  (sexed-replace-project-form project
                              path
                              selector
                              (sexed-source-form-to-string form)))

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

(defun sexed-insert-before-project-form-with-form (project path selector form)
  "Insert FORM rendered as Lisp source before SELECTOR in PROJECT/PATH."
  (sexed-insert-before-project-form project
                                    path
                                    selector
                                    (sexed-source-list-form-to-string form)))

(defun sexed-insert-after-project-form (project path selector new-text)
  "Insert NEW-TEXT after SELECTOR in PROJECT/PATH and return a summary plist."
  (sexed-update-project-file project path
                             (lambda (text)
                               (sexed-insert-after-form text selector new-text))))

(defun sexed-insert-after-project-form-with-form (project path selector form)
  "Insert FORM rendered as Lisp source after SELECTOR in PROJECT/PATH."
  (sexed-insert-after-project-form project
                                   path
                                   selector
                                   (sexed-source-list-form-to-string form)))

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

(defun sexed-ensure-config-project ()
  "Ensure the built-in config project is available and return its name."
  (unless (find-project "config")
    (load-project-definitions))
  "config")

(defun sexed-ensure-init-file (&key (content ""))
  "Ensure config/init.lisp exists, creating it with CONTENT when missing."
  (let ((project (sexed-ensure-config-project))
        (path "init.lisp"))
    (handler-case
        (let ((text (project-read-file project path)))
          (list :status :exists
                :project project
                :path path
                :bytes-read (length text)))
      (error ()
        (project-create-file project path :content content)))))

(defun sexed-init-outline-to-string (&rest options)
  "Return a sexed outline for the user's config init.lisp."
  (sexed-ensure-init-file)
  (apply #'sexed-project-outline-to-string
         (sexed-ensure-config-project)
         "init.lisp"
         options))

(defun sexed-init-form-text (selector)
  "Return source text for SELECTOR in the user's config init.lisp."
  (sexed-ensure-init-file)
  (sexed-project-form-text (sexed-ensure-config-project)
                           "init.lisp"
                           selector))

(defun sexed-replace-init-form (selector new-text)
  "Replace SELECTOR in the user's config init.lisp with NEW-TEXT."
  (sexed-ensure-init-file)
  (sexed-replace-project-form (sexed-ensure-config-project)
                              "init.lisp"
                              selector
                              new-text))

(defun sexed-insert-before-init-form (selector new-text)
  "Insert NEW-TEXT before SELECTOR in the user's config init.lisp."
  (sexed-ensure-init-file)
  (sexed-insert-before-project-form (sexed-ensure-config-project)
                                    "init.lisp"
                                    selector
                                    new-text))

(defun sexed-insert-after-init-form (selector new-text)
  "Insert NEW-TEXT after SELECTOR in the user's config init.lisp."
  (sexed-ensure-init-file)
  (sexed-insert-after-project-form (sexed-ensure-config-project)
                                   "init.lisp"
                                   selector
                                   new-text))

(defun sexed-update-staged-project-file (project path edit-fn
                                          &key change-set)
  "Apply EDIT-FN to PROJECT/PATH text and stage the result in CHANGE-SET."
  (let* ((old-text (change-set-project-file-text project path change-set))
         (new-text (funcall edit-fn old-text)))
    (sexed-ensure-balanced new-text "Staged project edit result")
    (let ((entry (stage-project-file project path new-text
                                     :change-set change-set)))
      (list :status :staged
            :change-set (change-set-id (ensure-change-set change-set))
            :project (change-set-entry-project-name entry)
            :path (change-set-entry-path entry)
            :bytes-staged (length new-text)
            :balanced t))))

(defun sexed-stage-replace-project-form (project path selector new-text
                                          &key change-set)
  "Stage replacement of SELECTOR in PROJECT/PATH with NEW-TEXT."
  (sexed-update-staged-project-file
   project path
   (lambda (text)
     (sexed-replace-form text selector new-text))
   :change-set change-set))

(defun sexed-stage-replace-project-form-with-form (project path selector form
                                                   &key change-set)
  "Stage replacement of SELECTOR in PROJECT/PATH with FORM rendered as source."
  (sexed-stage-replace-project-form project
                                    path
                                    selector
                                    (sexed-source-form-to-string form)
                                    :change-set change-set))

(defun sexed-stage-delete-project-form (project path selector
                                         &key change-set)
  "Stage deletion of SELECTOR in PROJECT/PATH."
  (sexed-update-staged-project-file
   project path
   (lambda (text)
     (sexed-delete-form text selector))
   :change-set change-set))

(defun sexed-stage-insert-before-project-form (project path selector new-text
                                                &key change-set)
  "Stage insertion of NEW-TEXT before SELECTOR in PROJECT/PATH."
  (sexed-update-staged-project-file
   project path
   (lambda (text)
     (sexed-insert-before-form text selector new-text))
   :change-set change-set))

(defun sexed-stage-insert-before-project-form-with-form (project path selector form
                                                         &key change-set)
  "Stage insertion of FORM rendered as Lisp source before SELECTOR."
  (sexed-stage-insert-before-project-form project
                                          path
                                          selector
                                          (sexed-source-list-form-to-string form)
                                          :change-set change-set))

(defun sexed-stage-insert-after-project-form (project path selector new-text
                                               &key change-set)
  "Stage insertion of NEW-TEXT after SELECTOR in PROJECT/PATH."
  (sexed-update-staged-project-file
   project path
   (lambda (text)
     (sexed-insert-after-form text selector new-text))
   :change-set change-set))

(defun sexed-stage-insert-after-project-form-with-form (project path selector form
                                                        &key change-set)
  "Stage insertion of FORM rendered as Lisp source after SELECTOR."
  (sexed-stage-insert-after-project-form project
                                         path
                                         selector
                                         (sexed-source-list-form-to-string form)
                                         :change-set change-set))

(defun sexed-stage-insert-project-form-before (project path selector new-text
                                                &key change-set)
  "Stage insertion of NEW-TEXT before SELECTOR in PROJECT/PATH."
  (sexed-stage-insert-before-project-form project path selector new-text
                                          :change-set change-set))

(defun sexed-stage-insert-project-form-after (project path selector new-text
                                               &key change-set)
  "Stage insertion of NEW-TEXT after SELECTOR in PROJECT/PATH."
  (sexed-stage-insert-after-project-form project path selector new-text
                                         :change-set change-set))

(defun sexed-stage-wrap-project-form (project path selector prefix suffix
                                       &key change-set)
  "Stage wrapping SELECTOR in PROJECT/PATH with PREFIX and SUFFIX."
  (sexed-update-staged-project-file
   project path
   (lambda (text)
     (sexed-wrap-form text selector prefix suffix))
   :change-set change-set))

(defun sexed-stage-splice-project-form (project path selector
                                         &key change-set)
  "Stage splicing SELECTOR out of PROJECT/PATH."
  (sexed-update-staged-project-file
   project path
   (lambda (text)
     (sexed-splice-form text selector))
   :change-set change-set))

(defun sexed-stage-raise-project-form (project path selector child-selector
                                        &key change-set)
  "Stage raising CHILD-SELECTOR out of SELECTOR in PROJECT/PATH."
  (sexed-update-staged-project-file
   project path
   (lambda (text)
     (sexed-raise-form text selector child-selector))
   :change-set change-set))

(defun sexed-stage-slurp-forward-project-form (project path selector
                                                &key (count 1) change-set)
  "Stage slurping COUNT following siblings into SELECTOR in PROJECT/PATH."
  (sexed-update-staged-project-file
   project path
   (lambda (text)
     (sexed-slurp-forward text selector :count count))
   :change-set change-set))

(defun sexed-stage-barf-forward-project-form (project path selector
                                               &key (count 1) change-set)
  "Stage barfing COUNT trailing children out of SELECTOR in PROJECT/PATH."
  (sexed-update-staged-project-file
   project path
   (lambda (text)
     (sexed-barf-forward text selector :count count))
   :change-set change-set))

(defun sexed-stage-replace-init-form (selector new-text &key change-set)
  "Stage replacement of SELECTOR in the user's config init.lisp."
  (sexed-ensure-init-file)
  (sexed-stage-replace-project-form (sexed-ensure-config-project)
                                    "init.lisp"
                                    selector
                                    new-text
                                    :change-set change-set))

(defun sexed-stage-insert-before-init-form (selector new-text &key change-set)
  "Stage insertion of NEW-TEXT before SELECTOR in the user's config init.lisp."
  (sexed-ensure-init-file)
  (sexed-stage-insert-before-project-form (sexed-ensure-config-project)
                                          "init.lisp"
                                          selector
                                          new-text
                                          :change-set change-set))

(defun sexed-stage-insert-after-init-form (selector new-text &key change-set)
  "Stage insertion of NEW-TEXT after SELECTOR in the user's config init.lisp."
  (sexed-ensure-init-file)
  (sexed-stage-insert-after-project-form (sexed-ensure-config-project)
                                         "init.lisp"
                                         selector
                                         new-text
                                         :change-set change-set))

(defun sexed-update-message (message edit-fn)
  "Apply EDIT-FN to editable MESSAGE text and return a summary plist."
  (when (message-read-only-p message)
    (error "Cannot structurally edit read-only message."))
  (let ((new-text (funcall edit-fn (message-text message))))
    (set-message-text message new-text)
    (list :status :ok
          :bytes-written (length new-text)
          :balanced t)))

(defun sexed-replace-message-form (message selector new-text)
  "Replace SELECTOR in editable MESSAGE with NEW-TEXT."
  (sexed-update-message message
                        (lambda (text)
                          (sexed-replace-form text selector new-text))))

(defun sexed-delete-message-form (message selector)
  "Delete SELECTOR from editable MESSAGE."
  (sexed-update-message message
                        (lambda (text)
                          (sexed-delete-form text selector))))

(defun sexed-insert-before-message-form (message selector new-text)
  "Insert NEW-TEXT before SELECTOR in editable MESSAGE."
  (sexed-update-message message
                        (lambda (text)
                          (sexed-insert-before-form text selector new-text))))

(defun sexed-insert-after-message-form (message selector new-text)
  "Insert NEW-TEXT after SELECTOR in editable MESSAGE."
  (sexed-update-message message
                        (lambda (text)
                          (sexed-insert-after-form text selector new-text))))

(defun sexed-insert-message-form-before (message selector new-text)
  "Alias for SEXED-INSERT-BEFORE-MESSAGE-FORM with natural command ordering."
  (sexed-insert-before-message-form message selector new-text))

(defun sexed-insert-message-form-after (message selector new-text)
  "Alias for SEXED-INSERT-AFTER-MESSAGE-FORM with natural command ordering."
  (sexed-insert-after-message-form message selector new-text))

(defun sexed-wrap-message-form (message selector prefix suffix)
  "Wrap SELECTOR in editable MESSAGE with PREFIX and SUFFIX."
  (sexed-update-message message
                        (lambda (text)
                          (sexed-wrap-form text selector prefix suffix))))

(defun sexed-splice-message-form (message selector)
  "Splice SELECTOR in editable MESSAGE."
  (sexed-update-message message
                        (lambda (text)
                          (sexed-splice-form text selector))))

(defun sexed-raise-message-form (message selector child-selector)
  "Raise CHILD-SELECTOR out of SELECTOR in editable MESSAGE."
  (sexed-update-message message
                        (lambda (text)
                          (sexed-raise-form text selector child-selector))))

(defun sexed-slurp-forward-message-form (message selector &key (count 1))
  "Slurp following forms into SELECTOR in editable MESSAGE."
  (sexed-update-message message
                        (lambda (text)
                          (sexed-slurp-forward text selector :count count))))

(defun sexed-barf-forward-message-form (message selector &key (count 1))
  "Barf trailing child forms out of SELECTOR in editable MESSAGE."
  (sexed-update-message message
                        (lambda (text)
                          (sexed-barf-forward text selector :count count))))

(defun sexed-scratch-message ()
  "Return the editable scratch buffer message, creating the scratch buffer first."
  (buffer-input-message (ensure-scratch-buffer)))

(defun sexed-scratch-text ()
  "Return scratch buffer text, creating the scratch buffer first."
  (scratch-buffer-text (ensure-scratch-buffer)))

(defun sexed-scratch-outline-to-string (&rest options)
  "Return a sexed outline for the scratch buffer."
  (apply #'sexed-outline-to-string (sexed-scratch-text) options))

(defun sexed-scratch-form-text (selector)
  "Return source text for SELECTOR in the scratch buffer."
  (sexed-form-text (sexed-scratch-text) selector))

(defun sexed-scratch-result (summary)
  "Add final scratch text to SUMMARY."
  (append summary (list :final-text (sexed-scratch-text))))

(defun sexed-replace-scratch-form (selector new-text)
  "Replace SELECTOR in the scratch buffer with NEW-TEXT."
  (sexed-scratch-result
   (sexed-replace-message-form (sexed-scratch-message) selector new-text)))

(defun sexed-delete-scratch-form (selector)
  "Delete SELECTOR from the scratch buffer."
  (sexed-scratch-result
   (sexed-delete-message-form (sexed-scratch-message) selector)))

(defun sexed-insert-before-scratch-form (selector new-text)
  "Insert NEW-TEXT before SELECTOR in the scratch buffer."
  (sexed-scratch-result
   (sexed-insert-before-message-form (sexed-scratch-message) selector new-text)))

(defun sexed-insert-after-scratch-form (selector new-text)
  "Insert NEW-TEXT after SELECTOR in the scratch buffer."
  (sexed-scratch-result
   (sexed-insert-after-message-form (sexed-scratch-message) selector new-text)))

(defun sexed-insert-scratch-form-before (selector new-text)
  "Alias for SEXED-INSERT-BEFORE-SCRATCH-FORM with natural command ordering."
  (sexed-insert-before-scratch-form selector new-text))

(defun sexed-insert-scratch-form-after (selector new-text)
  "Alias for SEXED-INSERT-AFTER-SCRATCH-FORM with natural command ordering."
  (sexed-insert-after-scratch-form selector new-text))

(defun sexed-wrap-scratch-form (selector prefix suffix)
  "Wrap SELECTOR in the scratch buffer with PREFIX and SUFFIX."
  (sexed-scratch-result
   (sexed-wrap-message-form (sexed-scratch-message) selector prefix suffix)))

(defun sexed-splice-scratch-form (selector)
  "Splice SELECTOR in the scratch buffer."
  (sexed-scratch-result
   (sexed-splice-message-form (sexed-scratch-message) selector)))

(defun sexed-raise-scratch-form (selector child-selector)
  "Raise CHILD-SELECTOR out of SELECTOR in the scratch buffer."
  (sexed-scratch-result
   (sexed-raise-message-form (sexed-scratch-message) selector child-selector)))

(defun sexed-slurp-forward-scratch-form (selector &key (count 1))
  "Slurp following forms into SELECTOR in the scratch buffer."
  (sexed-scratch-result
   (sexed-slurp-forward-message-form (sexed-scratch-message)
                                     selector
                                     :count count)))

(defun sexed-barf-forward-scratch-form (selector &key (count 1))
  "Barf trailing child forms out of SELECTOR in the scratch buffer."
  (sexed-scratch-result
   (sexed-barf-forward-message-form (sexed-scratch-message)
                                    selector
                                    :count count)))
