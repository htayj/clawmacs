(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Slop: Symbol Lookup and Origin Probe
;;; --------------------------------------------------------------------------

(defparameter *slop-default-result-limit* 100
  "Default maximum number of SLOP results returned by provider tools.")

(defvar *slop-project-index-cache* (make-hash-table :test #'equal)
  "Cache of parsed project indexes keyed by normalized project name.")

(defstruct slop-definition
  id
  project
  path
  kind
  namespace
  name
  normalized-name
  package
  qualified-name
  start
  end
  name-start
  name-end
  line
  column
  form-id
  preview
  body)

(defstruct slop-binding
  id
  project
  path
  name
  normalized-name
  package
  start
  end
  line
  column
  node-id
  scope-node-id
  scope-start
  scope-end
  owner-definition-id
  preview)

(defstruct slop-reference
  id
  project
  path
  role
  namespace
  name
  normalized-name
  package
  qualified-name
  start
  end
  line
  column
  node-id
  form-id
  definition-id
  binding-id
  enclosing-definition-id
  preview)

(defstruct slop-file-index
  project
  path
  truename
  write-date
  text
  nodes
  roots
  definitions
  bindings
  references
  diagnostics)

(defstruct slop-project-index
  project
  file-indexes
  indexed-at)

(defun slop-string (value)
  "Return VALUE as a string suitable for display and matching."
  (typecase value
    (null nil)
    (string value)
    (symbol (symbol-name value))
    (t (princ-to-string value))))

(defun slop-normalize-name (value)
  "Return VALUE as a lowercase comparison string."
  (let ((string (slop-string value)))
    (and string (string-downcase string))))

(defun slop-token-prefix-p (prefix token)
  "Return true when TOKEN starts with PREFIX."
  (and token
       (<= (length prefix) (length token))
       (string= prefix token :end2 (length prefix))))

(defun slop-token-contains-p (needle token)
  "Return true when TOKEN contains NEEDLE."
  (and token (search needle token :test #'char=)))

(defun slop-numeric-token-p (token)
  "Return true when TOKEN reads as a number."
  (handler-case
      (let ((*read-eval* nil))
        (multiple-value-bind (object position)
            (read-from-string token nil nil)
          (and (= position (length token))
               (numberp object))))
    (error () nil)))

(defun slop-lambda-list-marker-p (token)
  "Return true when TOKEN is a lambda-list marker such as &OPTIONAL."
  (and (plusp (length token))
       (char= #\& (char token 0))))

(defun slop-keyword-token-p (token)
  "Return true when TOKEN is a keyword token."
  (and (plusp (length token))
       (char= #\: (char token 0))))

(defun slop-symbol-token-p (token)
  "Return true when TOKEN is useful as a source symbol occurrence."
  (and token
       (plusp (length token))
       (not (slop-numeric-token-p token))
       (not (slop-token-prefix-p "#" token))
       (not (string= "." token))
       (not (string-equal "nil" token))
       (not (string-equal "t" token))))

(defun slop-bindable-symbol-token-p (token)
  "Return true when TOKEN can name a lexical binding."
  (and (slop-symbol-token-p token)
       (not (slop-keyword-token-p token))
       (not (slop-lambda-list-marker-p token))
       (not (slop-token-contains-p ":" token))))

(defun slop-token-parts (token default-package)
  "Return package, name, and marker for TOKEN in DEFAULT-PACKAGE."
  (cond
    ((slop-token-prefix-p "#:" token)
     (values nil (subseq token 2) :uninterned))
    ((slop-keyword-token-p token)
     (values "keyword" (subseq token 1) :keyword))
    (t
     (let ((internal (search "::" token :test #'char=)))
       (cond
         (internal
          (values (subseq token 0 internal)
                  (subseq token (+ internal 2))
                  :internal))
         ((position #\: token)
          (let ((pos (position #\: token)))
            (values (subseq token 0 pos)
                    (subseq token (1+ pos))
                    :external)))
         (t
          (values default-package token :unqualified)))))))

(defun slop-token-normalized-name (token)
  "Return TOKEN's package-stripped lowercase name."
  (multiple-value-bind (_package name)
      (slop-token-parts token nil)
    (declare (ignore _package))
    (slop-normalize-name name)))

(defun slop-qualified-name (package name)
  "Return PACKAGE:NAME display text, or NAME when PACKAGE is absent."
  (if package
      (format nil "~A:~A" (string-downcase package) name)
      name))

(defun slop-node-token (node text)
  "Return NODE's atom token text, or NIL."
  (when (eq :atom (sexed-node-type node))
    (sexed-node-token-text node text)))

(defun slop-node-bindable-token-p (node text)
  "Return true when NODE is an atom suitable as a lexical binding name."
  (let ((token (slop-node-token node text)))
    (and token (slop-bindable-symbol-token-p token))))

(defun slop-node-map (nodes)
  "Return a hash table mapping node ids to nodes."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (node nodes table)
      (setf (gethash (sexed-node-id node) table) node))))

(defun slop-node-line-column (text offset)
  "Return 1-based line and column for OFFSET in TEXT."
  (let ((line 1)
        (column 1))
    (loop :for index :below (min offset (length text))
          :for char := (char text index)
          :do (if (char= char #\Newline)
                  (setf line (1+ line)
                        column 1)
                  (incf column)))
    (values line column)))

(defun slop-offset-for-line-column (text line column)
  "Return a 0-based offset for 1-based LINE and COLUMN in TEXT."
  (unless (and line column (plusp line) (plusp column))
    (error "Line and column must be positive integers."))
  (let ((current-line 1)
        (current-column 1))
    (loop :for index :from 0 :below (length text)
          :do (when (and (= current-line line)
                         (= current-column column))
                (return-from slop-offset-for-line-column index))
              (if (char= (char text index) #\Newline)
                  (setf current-line (1+ current-line)
                        current-column 1)
                  (incf current-column)))
    (if (and (= current-line line)
             (= current-column column))
        (length text)
        (error "No position for line ~D column ~D." line column))))

(defun slop-preview-around (text start end &optional (radius 48))
  "Return a compact source preview around START..END."
  (let ((preview-start (max 0 (- start radius)))
        (preview-end (min (length text) (+ end radius))))
    (sexed-truncate-preview (subseq text preview-start preview-end) 160)))

(defun slop-make-definition-id (path node)
  "Return a stable source-local definition id."
  (format nil "~A#def-~D" path (sexed-node-id node)))

(defun slop-make-binding-id (path node)
  "Return a stable source-local binding id."
  (format nil "~A#bind-~D" path (sexed-node-id node)))

(defun slop-make-reference-id (path node)
  "Return a stable source-local reference id."
  (format nil "~A#ref-~D" path (sexed-node-id node)))

(defun slop-definition-kind-and-namespace (head)
  "Return definition kind and namespace for HEAD."
  (let ((head-name (and head (string-downcase head))))
    (cond
      ((member head-name '("defun") :test #'string=)
       (values :function :function))
      ((member head-name '("defmacro") :test #'string=)
       (values :macro :function))
      ((member head-name '("defgeneric") :test #'string=)
       (values :generic-function :function))
      ((member head-name '("defmethod") :test #'string=)
       (values :method :function))
      ((member head-name '("defcommand") :test #'string=)
       (values :command :function))
      ((member head-name '("deftool") :test #'string=)
       (values :tool :function))
      ((member head-name '("defvar") :test #'string=)
       (values :special-variable :variable))
      ((member head-name '("defparameter") :test #'string=)
       (values :parameter :variable))
      ((member head-name '("defconstant") :test #'string=)
       (values :constant :variable))
      ((member head-name '("defclass") :test #'string=)
       (values :class :type))
      ((member head-name '("defstruct") :test #'string=)
       (values :structure :type))
      ((member head-name '("define-condition") :test #'string=)
       (values :condition :type))
      ((member head-name '("deftype") :test #'string=)
       (values :type :type))
      ((member head-name '("defpackage") :test #'string=)
       (values :package :package))
      ((member head-name '("defsystem") :test #'string=)
       (values :system :system))
      ((member head-name '("defdoc") :test #'string=)
       (values :documentation :documentation))
      (t
       (values nil nil)))))

(defun slop-definition-head-p (head)
  "Return true when HEAD starts a known definition form."
  (not (null (nth-value 0 (slop-definition-kind-and-namespace head)))))

(defun slop-nth-child (node index)
  "Return NODE's INDEX child."
  (nth index (sexed-node-children node)))

(defun slop-definition-name-node (node)
  "Return NODE's name child when NODE is a definition form."
  (let ((head (string-downcase (or (sexed-node-head node) ""))))
    (cond
      ((member head '("defun" "defmacro" "defgeneric" "defmethod"
                      "defclass" "defstruct" "define-condition"
                      "deftype" "defvar" "defparameter" "defconstant"
                      "defcommand" "deftool" "defdoc" "defpackage"
                      "defsystem")
               :test #'string=)
       (slop-nth-child node 1))
      (t nil))))

(defun slop-package-name-from-node (node text default-package)
  "Return package name encoded by NODE."
  (let ((token (slop-node-token node text)))
    (if token
        (multiple-value-bind (_package name)
            (slop-token-parts token default-package)
          (declare (ignore _package))
          (slop-normalize-name name))
        default-package)))

(defun slop-mark-package-context (node package table)
  "Mark NODE and descendants with PACKAGE in TABLE."
  (setf (gethash (sexed-node-id node) table) package)
  (dolist (child (sexed-node-children node))
    (slop-mark-package-context child package table)))

(defun slop-package-context-map (roots text)
  "Return node-id -> package-name context for ROOTS."
  (let ((table (make-hash-table :test #'eql))
        (current-package "cl-user"))
    (dolist (root roots table)
      (let ((head (string-downcase (or (sexed-node-head root) ""))))
        (cond
          ((string= head "in-package")
           (let ((name-node (slop-nth-child root 1)))
             (when name-node
               (setf current-package
                     (slop-package-name-from-node name-node text current-package))))
           (slop-mark-package-context root current-package table))
          (t
           (slop-mark-package-context root current-package table)))))))

(defun slop-node-package (node package-map)
  "Return NODE's package context."
  (or (gethash (sexed-node-id node) package-map)
      "cl-user"))

(defun slop-collect-definitions (project path text nodes package-map)
  "Collect definition records from NODES."
  (let ((definitions nil))
    (dolist (node nodes (nreverse definitions))
      (multiple-value-bind (kind namespace)
          (slop-definition-kind-and-namespace (sexed-node-head node))
        (when kind
          (let* ((name-node (slop-definition-name-node node))
                 (token (and name-node (slop-node-token name-node text))))
            (when (and token (slop-symbol-token-p token))
              (multiple-value-bind (token-package token-name)
                  (slop-token-parts token (slop-node-package node package-map))
                (let* ((package (if (eq namespace :package)
                                    nil
                                    (and token-package
                                         (string-downcase token-package))))
                       (name (string-downcase token-name))
                       (qualified-name (slop-qualified-name package name)))
                  (multiple-value-bind (line column)
                      (slop-node-line-column text (sexed-node-token-start name-node))
                    (push (make-slop-definition
                           :id (slop-make-definition-id path node)
                           :project project
                           :path path
                           :kind kind
                           :namespace namespace
                           :name name
                           :normalized-name name
                           :package package
                           :qualified-name qualified-name
                           :start (sexed-node-start node)
                           :end (sexed-node-end node)
                           :name-start (sexed-node-token-start name-node)
                           :name-end (sexed-node-end name-node)
                           :line line
                           :column column
                           :form-id (sexed-node-id node)
                           :preview (sexed-truncate-preview
                                     (sexed-node-text node text)
                                     160)
                           :body (sexed-node-text node text))
                          definitions)))))))))))

(defun slop-definition-containing-offset (definitions offset)
  "Return the smallest definition containing OFFSET."
  (first
   (sort (remove-if-not
          (lambda (definition)
            (and (<= (slop-definition-start definition) offset)
                 (<= offset (slop-definition-end definition))))
          (copy-list definitions))
         #'<
         :key (lambda (definition)
                (- (slop-definition-end definition)
                   (slop-definition-start definition))))))

(defun slop-direct-list-child-after (node start-index)
  "Return the first list child of NODE after START-INDEX."
  (loop :for child :in (nthcdr start-index (sexed-node-children node))
        :when (member (sexed-node-type child) '(:list :vector) :test #'eq)
          :return child))

(defun slop-collect-pattern-binding-nodes (node text)
  "Collect bindable atom nodes from destructuring pattern NODE."
  (let ((result nil))
    (labels ((walk (candidate)
               (cond
                 ((slop-node-bindable-token-p candidate text)
                  (push candidate result))
                 ((member (sexed-node-type candidate) '(:list :vector) :test #'eq)
                  (dolist (child (sexed-node-children candidate))
                    (walk child))))))
      (walk node))
    (nreverse result)))

(defun slop-collect-lambda-list-binding-nodes (lambda-list text)
  "Collect simple variable binding nodes from LAMBDA-LIST."
  (let ((result nil))
    (dolist (child (sexed-node-children lambda-list) (nreverse result))
      (cond
        ((slop-node-bindable-token-p child text)
         (push child result))
        ((member (sexed-node-type child) '(:list :vector) :test #'eq)
         (let ((first (first (sexed-node-children child))))
           (cond
             ((and first (slop-node-bindable-token-p first text))
              (push first result))
             (t
              (dolist (pattern-node (slop-collect-pattern-binding-nodes child text))
                (push pattern-node result))))))))))

(defun slop-collect-let-binding-nodes (binding-list text)
  "Collect variable binding nodes from a LET-style binding list."
  (let ((result nil))
    (dolist (spec (sexed-node-children binding-list) (nreverse result))
      (cond
        ((slop-node-bindable-token-p spec text)
         (push spec result))
        ((member (sexed-node-type spec) '(:list :vector) :test #'eq)
         (let ((name-node (first (sexed-node-children spec))))
           (when (and name-node (slop-node-bindable-token-p name-node text))
             (push name-node result))))))))

(defun slop-register-binding (bindings project path text node package
                               scope-node scope-start scope-end owner)
  "Register NODE as a lexical binding."
  (let* ((token (slop-node-token node text))
         (name (slop-token-normalized-name token)))
    (multiple-value-bind (line column)
        (slop-node-line-column text (sexed-node-token-start node))
      (push (make-slop-binding
             :id (slop-make-binding-id path node)
             :project project
             :path path
             :name name
             :normalized-name name
             :package package
             :start (sexed-node-token-start node)
             :end (sexed-node-end node)
             :line line
             :column column
             :node-id (sexed-node-id node)
             :scope-node-id (and scope-node (sexed-node-id scope-node))
             :scope-start scope-start
             :scope-end scope-end
             :owner-definition-id (and owner (slop-definition-id owner))
             :preview (slop-preview-around text
                                           (sexed-node-token-start node)
                                           (sexed-node-end node)))
            bindings)))
  bindings)

(defun slop-collect-bindings (project path text nodes definitions package-map)
  "Collect lexical bindings from NODES."
  (let ((bindings nil))
    (dolist (node nodes (nreverse bindings))
      (let* ((head (string-downcase (or (sexed-node-head node) "")))
             (children (sexed-node-children node))
             (owner (slop-definition-containing-offset definitions
                                                       (sexed-node-start node)))
             (package (slop-node-package node package-map)))
        (labels ((register-nodes (binding-nodes scope-start scope-end)
                   (dolist (binding-node binding-nodes)
                     (setf bindings
                           (slop-register-binding
                            bindings project path text binding-node package
                            node scope-start scope-end owner)))))
          (cond
            ((member head '("defun" "defmacro" "defgeneric" "deftype")
                     :test #'string=)
             (let ((lambda-list (slop-nth-child node 2)))
               (when (and lambda-list
                          (member (sexed-node-type lambda-list)
                                  '(:list :vector)
                                  :test #'eq))
                 (register-nodes
                  (slop-collect-lambda-list-binding-nodes lambda-list text)
                  (sexed-node-end lambda-list)
                  (sexed-node-end node)))))
            ((string= head "defmethod")
             (let ((lambda-list (slop-direct-list-child-after node 2)))
               (when lambda-list
                 (register-nodes
                  (slop-collect-lambda-list-binding-nodes lambda-list text)
                  (sexed-node-end lambda-list)
                  (sexed-node-end node)))))
            ((string= head "lambda")
             (let ((lambda-list (slop-nth-child node 1)))
               (when (and lambda-list
                          (member (sexed-node-type lambda-list)
                                  '(:list :vector)
                                  :test #'eq))
                 (register-nodes
                  (slop-collect-lambda-list-binding-nodes lambda-list text)
                  (sexed-node-end lambda-list)
                  (sexed-node-end node)))))
            ((member head '("let" "let*") :test #'string=)
             (let ((binding-list (slop-nth-child node 1)))
               (when binding-list
                 (dolist (binding-node
                          (slop-collect-let-binding-nodes binding-list text))
                   (let ((scope-start
                           (if (string= head "let*")
                               (sexed-node-end binding-node)
                               (sexed-node-end binding-list))))
                     (setf bindings
                           (slop-register-binding
                            bindings project path text binding-node package
                            node scope-start (sexed-node-end node) owner)))))))
            ((member head '("multiple-value-bind" "destructuring-bind")
                     :test #'string=)
             (let ((pattern (slop-nth-child node 1))
                   (value-form (slop-nth-child node 2)))
               (when pattern
                 (register-nodes
                  (slop-collect-pattern-binding-nodes pattern text)
                  (if value-form
                      (sexed-node-end value-form)
                      (sexed-node-end pattern))
                  (sexed-node-end node)))))
            ((member head '("dolist" "dotimes") :test #'string=)
             (let* ((spec (slop-nth-child node 1))
                    (var (and spec (first (sexed-node-children spec)))))
               (when (and var (slop-node-bindable-token-p var text))
                 (register-nodes (list var)
                                 (sexed-node-end spec)
                                 (sexed-node-end node)))))
            ((member head '("do" "do*") :test #'string=)
             (let ((varspecs (slop-nth-child node 1)))
               (when varspecs
                 (dolist (binding-node
                          (slop-collect-let-binding-nodes varspecs text))
                   (setf bindings
                         (slop-register-binding
                          bindings project path text binding-node package
                          node (sexed-node-end varspecs)
                          (sexed-node-end node) owner))))))))))))

(defun slop-bindings-by-node-id (bindings)
  "Return a table keyed by binding node id."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (binding bindings table)
      (setf (gethash (slop-binding-node-id binding) table) binding))))

(defun slop-definitions-by-name-node-id (definitions)
  "Return a table keyed by definition name node id."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (definition definitions table)
      (setf (gethash (slop-definition-name-start definition) table)
            definition))))

(defun slop-definition-for-name-node (definitions node text)
  "Return definition whose name span is NODE."
  (find-if (lambda (definition)
             (and (= (slop-definition-name-start definition)
                     (sexed-node-token-start node))
                  (= (slop-definition-name-end definition)
                     (sexed-node-end node))))
           definitions))

(defun slop-parent-node (node node-map)
  "Return NODE's parent from NODE-MAP."
  (and (sexed-node-parent node)
       (gethash (sexed-node-parent node) node-map)))

(defun slop-first-child-p (node parent)
  "Return true when NODE is PARENT's first child."
  (let ((first (first (sexed-node-children parent))))
    (and first (= (sexed-node-id node) (sexed-node-id first)))))

(defun slop-second-child-p (node parent)
  "Return true when NODE is PARENT's second child."
  (let ((second (second (sexed-node-children parent))))
    (and second (= (sexed-node-id node) (sexed-node-id second)))))

(defun slop-node-has-prefix-p (node prefix)
  "Return true when NODE has reader PREFIX."
  (member prefix (sexed-node-prefixes node) :test #'string=))

(defun slop-quote-context-p (node text node-map)
  "Return true when NODE is inside quoted source data."
  (labels ((quoted-parent-p (candidate)
             (let ((parent (slop-parent-node candidate node-map)))
               (and parent
                    (string= "quote"
                             (string-downcase
                              (or (sexed-node-head parent) "")))
                    (not (slop-first-child-p candidate parent))))))
    (loop :for current := node :then (slop-parent-node current node-map)
          :while current
          :when (or (slop-node-has-prefix-p current "'")
                    (slop-node-has-prefix-p current "`")
                    (quoted-parent-p current))
            :return t
          :finally (return nil))))

(defun slop-function-reference-node-p (node parent text)
  "Return true when NODE is a FUNCTION/#' reference."
  (or (slop-node-has-prefix-p node "#'")
      (and parent
           (string= "function"
                    (string-downcase (or (sexed-node-head parent) "")))
           (slop-second-child-p node parent))))

(defun slop-declaration-context-p (node node-map)
  "Return true when NODE is inside declaration syntax."
  (loop :for current := node :then (slop-parent-node current node-map)
        :while current
        :for head := (string-downcase (or (sexed-node-head current) ""))
        :when (member head '("declare" "declaim" "proclaim")
                      :test #'string=)
          :return t
        :finally (return nil)))

(defun slop-defpackage-context-p (node node-map)
  "Return true when NODE is inside a DEFPACKAGE form."
  (loop :for current := node :then (slop-parent-node current node-map)
        :while current
        :for head := (string-downcase (or (sexed-node-head current) ""))
        :when (string= head "defpackage")
          :return t
        :finally (return nil)))

(defun slop-excluded-operator-p (head)
  "Return true when HEAD is syntax rather than a function call for v1."
  (let ((name (and head (string-downcase head))))
    (or (slop-definition-head-p name)
        (member name
                '("quote" "function" "lambda" "if" "progn" "prog1" "prog2"
                  "let" "let*" "setq" "psetq" "setf" "block" "return-from"
                  "tagbody" "go" "catch" "throw" "unwind-protect"
                  "eval-when" "locally" "the" "load-time-value"
                  "symbol-macrolet" "flet" "labels" "macrolet"
                  "multiple-value-call" "multiple-value-prog1" "progv"
                  "and" "or" "cond" "case" "ccase" "ecase" "typecase"
                  "ctypecase" "etypecase" "loop" "do" "do*" "dolist"
                  "dotimes" "multiple-value-bind" "destructuring-bind")
                :test #'string=))))

(defun slop-collect-set-target-node-ids (nodes text)
  "Return a table of atom node ids used as variable set targets."
  (let ((table (make-hash-table :test #'eql)))
    (labels ((mark (node)
               (when (and node (slop-node-bindable-token-p node text))
                 (setf (gethash (sexed-node-id node) table) t)))
             (mark-every-other (children start-index)
               (loop :for tail :on (nthcdr start-index children) :by #'cddr
                     :do (mark (first tail)))))
      (dolist (node nodes table)
        (let ((head (string-downcase (or (sexed-node-head node) "")))
              (children (sexed-node-children node)))
          (cond
            ((member head '("setq" "psetq") :test #'string=)
             (mark-every-other children 1))
            ((string= head "setf")
             (mark-every-other children 1))
            ((member head '("incf" "decf" "pop") :test #'string=)
             (mark (slop-nth-child node 1)))
            ((member head '("push" "pushnew") :test #'string=)
             (mark (slop-nth-child node 2)))
            ((member head '("rotatef" "shiftf") :test #'string=)
             (dolist (child (rest children))
               (mark child)))))))))

(defun slop-resolve-lexical-binding (bindings name offset)
  "Return the innermost lexical binding named NAME visible at OFFSET."
  (first
   (sort (remove-if-not
          (lambda (binding)
            (and (string= name (slop-binding-normalized-name binding))
                 (<= (slop-binding-scope-start binding) offset)
                 (<= offset (slop-binding-scope-end binding))))
          (copy-list bindings))
         #'<
         :key (lambda (binding)
                (- (slop-binding-scope-end binding)
                   (slop-binding-scope-start binding))))))

(defun slop-reference (project path text node role namespace package name
                        &key definition binding enclosing-definition form-id)
  "Create a SLOP reference record for NODE."
  (multiple-value-bind (line column)
      (slop-node-line-column text (sexed-node-token-start node))
    (make-slop-reference
     :id (slop-make-reference-id path node)
     :project project
     :path path
     :role role
     :namespace namespace
     :name name
     :normalized-name (slop-normalize-name name)
     :package package
     :qualified-name (slop-qualified-name package name)
     :start (sexed-node-token-start node)
     :end (sexed-node-end node)
     :line line
     :column column
     :node-id (sexed-node-id node)
     :form-id form-id
     :definition-id (and definition (slop-definition-id definition))
     :binding-id (and binding (slop-binding-id binding))
     :enclosing-definition-id (and enclosing-definition
                                   (slop-definition-id enclosing-definition))
     :preview (slop-preview-around text
                                   (sexed-node-token-start node)
                                   (sexed-node-end node)))))

(defun slop-collect-references (project path text nodes definitions bindings
                                 package-map)
  "Collect symbol references from NODES."
  (let* ((node-map (slop-node-map nodes))
         (binding-table (slop-bindings-by-node-id bindings))
         (set-targets (slop-collect-set-target-node-ids nodes text))
         (references nil))
    (dolist (node nodes (nreverse references))
      (let ((token (slop-node-token node text)))
        (when (and token (slop-symbol-token-p token))
          (let* ((parent (slop-parent-node node node-map))
                 (definition (slop-definition-for-name-node definitions node text))
                 (binding (gethash (sexed-node-id node) binding-table))
                 (enclosing (slop-definition-containing-offset
                             definitions
                             (sexed-node-token-start node)))
                 (package-context (slop-node-package node package-map)))
            (multiple-value-bind (token-package raw-name marker)
                (slop-token-parts token package-context)
              (let* ((package (and token-package
                                   (string-downcase token-package)))
                     (name (slop-normalize-name raw-name)))
                (cond
                  (definition
                   (push (slop-reference
                          project path text node :definition
                          (slop-definition-namespace definition)
                          (slop-definition-package definition)
                          (slop-definition-name definition)
                          :definition definition
                          :enclosing-definition definition
                          :form-id (slop-definition-form-id definition))
                         references))
                  (binding
                   (push (slop-reference
                          project path text node :binding :variable
                          (slop-binding-package binding)
                          (slop-binding-name binding)
                          :binding binding
                          :enclosing-definition enclosing
                          :form-id (and parent (sexed-node-id parent)))
                         references))
                  ((or (eq marker :keyword)
                       (eq marker :uninterned)
                       (slop-lambda-list-marker-p token)
                       (slop-declaration-context-p node node-map)
                       (slop-defpackage-context-p node node-map)
                       (and (slop-quote-context-p node text node-map)
                            (not (slop-function-reference-node-p
                                  node parent text)))))
                  ((slop-function-reference-node-p node parent text)
                   (push (slop-reference
                          project path text node :reference :function
                          package name
                          :enclosing-definition enclosing
                          :form-id (and parent (sexed-node-id parent)))
                         references))
                  ((and parent (slop-first-child-p node parent))
                   (unless (slop-excluded-operator-p token)
                     (push (slop-reference
                            project path text node :call :function
                            package name
                            :enclosing-definition enclosing
                            :form-id (sexed-node-id parent))
                           references)))
                  ((gethash (sexed-node-id node) set-targets)
                   (let ((resolved (and (not (slop-token-contains-p ":" token))
                                        (slop-resolve-lexical-binding
                                         bindings name
                                         (sexed-node-token-start node)))))
                     (push (slop-reference
                            project path text node :set :variable
                            package name
                            :binding resolved
                            :enclosing-definition enclosing
                            :form-id (and parent (sexed-node-id parent)))
                           references)))
                  ((not (slop-token-contains-p ":" token))
                   (let ((resolved (slop-resolve-lexical-binding
                                    bindings name
                                    (sexed-node-token-start node))))
                     (push (slop-reference
                            project path text node :access :variable
                            package name
                            :binding resolved
                            :enclosing-definition enclosing
                            :form-id (and parent (sexed-node-id parent)))
                           references))))))))))))

(defun slop-project-lisp-paths (project &optional path)
  "Return Lisp source paths in PROJECT, optionally restricted to PATH."
  (let* ((all (remove-if-not #'project-lisp-source-path-p
                             (project-list-files project :limit nil))))
    (if (null path)
        all
        (let* ((resource (project-resource-name path :allow-directory t))
               (prefix (if (and (plusp (length resource))
                                (char= #\/ (char resource
                                                 (1- (length resource)))))
                           resource
                           (concatenate 'string resource "/"))))
          (cond
            ((and (project-lisp-source-path-p resource)
                  (ignore-errors
                    (project-resolve-path project resource
                                          :require-exists t)))
             (list resource))
            ((member resource all :test #'string=)
             (list resource))
            (t
             (remove-if-not (lambda (candidate)
                              (alexandria:starts-with-subseq prefix candidate))
                            all)))))))

(defun slop-index-file (project path)
  "Return a SLOP file index for PROJECT/PATH."
  (let* ((project-name (project-name (ensure-project project)))
         (truename (project-resolve-path project path :require-exists t))
         (write-date (file-write-date truename))
         (text (project-read-file project path)))
    (multiple-value-bind (nodes diagnostics roots)
        (sexed-parse-text text)
      (let* ((package-map (slop-package-context-map roots text))
             (definitions (slop-collect-definitions project-name path text
                                                    nodes package-map))
             (bindings (slop-collect-bindings project-name path text nodes
                                              definitions package-map))
             (references (slop-collect-references project-name path text nodes
                                                  definitions bindings
                                                  package-map)))
        (make-slop-file-index
         :project project-name
         :path path
         :truename truename
         :write-date write-date
         :text text
         :nodes nodes
         :roots roots
         :definitions definitions
         :bindings bindings
         :references references
         :diagnostics diagnostics)))))

(defun slop-project-index-current-p (index project paths)
  "Return true when INDEX still matches PROJECT/PATHS mtimes."
  (and index
       (let ((file-indexes (slop-project-index-file-indexes index)))
         (and (= (length file-indexes) (length paths))
              (every (lambda (path)
                       (let* ((file-index
                                (find path file-indexes
                                      :key #'slop-file-index-path
                                      :test #'string=))
                              (truename (and file-index
                                             (ignore-errors
                                              (project-resolve-path
                                               project path
                                               :require-exists t)))))
                         (and file-index
                              truename
                              (= (or (file-write-date truename) 0)
                                 (or (slop-file-index-write-date file-index)
                                     0)))))
                     paths)))))

(defun slop-index-project (project-designator &key path)
  "Return a cached, mtime-invalidated SLOP project index."
  (let* ((project (ensure-project project-designator))
         (project-key (normalize-project-name (project-name project)))
         (paths (sort (slop-project-lisp-paths project path) #'string<))
         (cache-key (list project-key (or path "")))
         (cached (gethash cache-key *slop-project-index-cache*)))
    (if (slop-project-index-current-p cached project paths)
        cached
        (let ((index (make-slop-project-index
                      :project (project-name project)
                      :file-indexes (mapcar (lambda (resource-path)
                                              (slop-index-file project
                                                               resource-path))
                                            paths)
                      :indexed-at (get-universal-time))))
          (setf (gethash cache-key *slop-project-index-cache*) index)
          index))))

(defun slop-index-definitions (index)
  "Return all definitions in INDEX."
  (loop :for file-index :in (slop-project-index-file-indexes index)
        :append (slop-file-index-definitions file-index)))

(defun slop-index-bindings (index)
  "Return all lexical bindings in INDEX."
  (loop :for file-index :in (slop-project-index-file-indexes index)
        :append (slop-file-index-bindings file-index)))

(defun slop-index-references (index)
  "Return all references in INDEX."
  (loop :for file-index :in (slop-project-index-file-indexes index)
        :append (slop-file-index-references file-index)))

(defun slop-index-file-by-path (index path)
  "Return file index for PATH."
  (find path (slop-project-index-file-indexes index)
        :key #'slop-file-index-path
        :test #'string=))

(defun slop-definition->plist (definition &key include-body)
  "Return DEFINITION as an agent-facing plist."
  (append
   (list :id (slop-definition-id definition)
         :project (slop-definition-project definition)
         :path (slop-definition-path definition)
         :kind (slop-definition-kind definition)
         :namespace (slop-definition-namespace definition)
         :name (slop-definition-name definition)
         :package (slop-definition-package definition)
         :qualified-name (slop-definition-qualified-name definition)
         :line (slop-definition-line definition)
         :column (slop-definition-column definition)
         :start (slop-definition-start definition)
         :end (slop-definition-end definition)
         :form-id (slop-definition-form-id definition)
         :preview (slop-definition-preview definition))
   (when include-body
     (list :body (slop-definition-body definition)))))

(defun slop-binding->plist (binding)
  "Return BINDING as an agent-facing plist."
  (list :id (slop-binding-id binding)
        :project (slop-binding-project binding)
        :path (slop-binding-path binding)
        :kind :lexical-variable
        :namespace :variable
        :name (slop-binding-name binding)
        :package (slop-binding-package binding)
        :line (slop-binding-line binding)
        :column (slop-binding-column binding)
        :start (slop-binding-start binding)
        :end (slop-binding-end binding)
        :node-id (slop-binding-node-id binding)
        :scope-node-id (slop-binding-scope-node-id binding)
        :scope-start (slop-binding-scope-start binding)
        :scope-end (slop-binding-scope-end binding)
        :owner-definition-id (slop-binding-owner-definition-id binding)
        :preview (slop-binding-preview binding)))

(defun slop-reference->plist (reference)
  "Return REFERENCE as an agent-facing plist."
  (list :id (slop-reference-id reference)
        :project (slop-reference-project reference)
        :path (slop-reference-path reference)
        :role (slop-reference-role reference)
        :namespace (slop-reference-namespace reference)
        :name (slop-reference-name reference)
        :package (slop-reference-package reference)
        :qualified-name (slop-reference-qualified-name reference)
        :line (slop-reference-line reference)
        :column (slop-reference-column reference)
        :start (slop-reference-start reference)
        :end (slop-reference-end reference)
        :node-id (slop-reference-node-id reference)
        :form-id (slop-reference-form-id reference)
        :definition-id (slop-reference-definition-id reference)
        :binding-id (slop-reference-binding-id reference)
        :enclosing-definition-id
        (slop-reference-enclosing-definition-id reference)
        :preview (slop-reference-preview reference)))

(defun slop-line-range-plist (text start end)
  "Return a plist describing START..END as source lines and columns."
  (multiple-value-bind (start-line start-column)
      (slop-node-line-column text start)
    (multiple-value-bind (end-line end-column)
        (slop-node-line-column text end)
      (list :start-line start-line
            :start-column start-column
            :end-line end-line
            :end-column end-column))))

(defun slop-node-source-plist (node text &key role (include-text t))
  "Return NODE as a compact source-context plist."
  (multiple-value-bind (line column)
      (slop-node-line-column text (sexed-node-start node))
    (append
     (list :role role
           :head (sexed-node-head node)
           :line line
           :column column
           :start (sexed-node-start node)
           :end (sexed-node-end node)
           :line-range
           (slop-line-range-plist text
                                  (sexed-node-start node)
                                  (sexed-node-end node)))
     (when include-text
       (list :text (sexed-node-text node text))))))

(defun slop-kind-match-p (kind wanted)
  "Return true when KIND matches optional WANTED."
  (or (null wanted)
      (string= (slop-normalize-name kind)
               (slop-normalize-name wanted))))

(defun slop-namespace-match-p (namespace wanted)
  "Return true when NAMESPACE matches optional WANTED."
  (or (null wanted)
      (string= (slop-normalize-name namespace)
               (slop-normalize-name wanted))))

(defun slop-package-match-p (package wanted)
  "Return true when PACKAGE matches optional WANTED."
  (or (null wanted)
      (string= (or (slop-normalize-name package) "")
               (slop-normalize-name wanted))))

(defun slop-name-match-p (name pattern substring)
  "Return true when NAME matches optional PATTERN."
  (or (null pattern)
      (let ((needle (slop-normalize-name pattern))
            (haystack (slop-normalize-name name)))
        (if substring
            (search needle haystack :test #'char=)
            (string= needle haystack)))))

(defun slop-parse-symbol-designator (symbol &optional package)
  "Return package and normalized name for SYMBOL."
  (let ((token (slop-string symbol)))
    (unless (and token (slop-symbol-token-p token))
      (error "Invalid symbol designator: ~S." symbol))
    (multiple-value-bind (token-package name)
        (slop-token-parts token package)
      (values (and token-package (string-downcase token-package))
              (slop-normalize-name name)))))

(defun slop-filter-definitions (definitions &key name-pattern symbol package
                                            namespace kind substring)
  "Return definitions matching filters."
  (multiple-value-bind (symbol-package symbol-name)
      (if symbol
          (slop-parse-symbol-designator symbol package)
          (values package nil))
    (remove-if-not
     (lambda (definition)
       (and (slop-name-match-p (slop-definition-name definition)
                               (or symbol-name name-pattern)
                               substring)
            (slop-package-match-p (slop-definition-package definition)
                                  symbol-package)
            (slop-namespace-match-p (slop-definition-namespace definition)
                                    namespace)
            (slop-kind-match-p (slop-definition-kind definition) kind)))
     definitions)))

(defun slop-filter-references (references &key symbol package namespace role
                                            substring)
  "Return references matching filters."
  (multiple-value-bind (symbol-package symbol-name)
      (if symbol
          (slop-parse-symbol-designator symbol package)
          (values package nil))
    (remove-if-not
     (lambda (reference)
       (and (or (null symbol-name)
                (slop-name-match-p (slop-reference-name reference)
                                   symbol-name
                                   substring))
            (slop-package-match-p (slop-reference-package reference)
                                  symbol-package)
            (slop-namespace-match-p (slop-reference-namespace reference)
                                    namespace)
            (or (null role)
                (string= (slop-normalize-name role)
                         (slop-normalize-name
                          (slop-reference-role reference))))))
     references)))

(defun slop-limit-list (items limit)
  "Limit ITEMS to LIMIT entries when LIMIT is non-nil."
  (if limit
      (subseq items 0 (min limit (length items)))
      items))

(defun slop-project-symbols (project &key path package kind name-pattern
                                     substring include-body
                                     (limit *slop-default-result-limit*))
  "Return definitions in PROJECT matching symbol filters."
  (let* ((index (slop-index-project project :path path))
         (definitions
           (slop-filter-definitions
            (slop-index-definitions index)
            :package package
            :kind kind
            :name-pattern name-pattern
            :substring substring)))
    (list :project (slop-project-index-project index)
          :count (length definitions)
          :definitions
          (mapcar (lambda (definition)
                    (slop-definition->plist definition
                                            :include-body include-body))
                  (slop-limit-list definitions limit)))))

(defun slop-find-definitions (project symbol &key path package namespace kind
                                      include-body
                                      (limit *slop-default-result-limit*))
  "Return definitions for SYMBOL in PROJECT."
  (let* ((index (slop-index-project project :path path))
         (definitions
           (slop-filter-definitions (slop-index-definitions index)
                                    :symbol symbol
                                    :package package
                                    :namespace namespace
                                    :kind kind)))
    (list :project (slop-project-index-project index)
          :symbol symbol
          :count (length definitions)
          :definitions
          (mapcar (lambda (definition)
                    (slop-definition->plist definition
                                            :include-body include-body))
                  (slop-limit-list definitions limit)))))

(defun slop-find-definitions-batch (project symbols
                                    &key path package namespace kind
                                         include-body
                                         (per-symbol-limit
                                          *slop-default-result-limit*))
  "Return definitions for multiple SYMBOLS in one indexed PROJECT pass."
  (unless symbols
    (error "Provide at least one symbol for batch definition lookup."))
  (let* ((index (slop-index-project project :path path))
         (results
           (mapcar
            (lambda (symbol)
              (let ((definitions
                      (slop-filter-definitions
                       (slop-index-definitions index)
                       :symbol symbol
                       :package package
                       :namespace namespace
                       :kind kind)))
                (list :symbol symbol
                      :count (length definitions)
                      :definitions
                      (mapcar (lambda (definition)
                                (slop-definition->plist
                                 definition
                                 :include-body include-body))
                              (slop-limit-list definitions
                                               per-symbol-limit)))))
            symbols)))
    (list :project (slop-project-index-project index)
          :count (length results)
          :total-definitions
          (reduce #'+ results :key (lambda (result)
                                     (getf result :count))
                  :initial-value 0)
          :results results)))

(defun slop-find-definition-by-id (index id)
  "Find definition ID in INDEX."
  (find id (slop-index-definitions index)
        :key #'slop-definition-id
        :test #'string=))

(defun slop-find-binding-by-id (index id)
  "Find lexical binding ID in INDEX."
  (find id (slop-index-bindings index)
        :key #'slop-binding-id
        :test #'string=))

(defun slop-definition-id-path (definition-id)
  "Return the path component encoded in DEFINITION-ID, when present."
  (let ((marker (and definition-id
                     (search "#def-" definition-id :test #'char=))))
    (and marker (subseq definition-id 0 marker))))

(defun slop-definition-id-hints (index definition-id
                                 &key symbol package namespace kind
                                      (limit 10))
  "Return candidate current definitions related to stale DEFINITION-ID."
  (let* ((path (slop-definition-id-path definition-id))
         (definitions
           (cond
             (symbol
              (slop-filter-definitions
               (slop-index-definitions index)
               :symbol symbol
               :package package
               :namespace namespace
               :kind kind))
             (path
              (remove-if-not
               (lambda (definition)
                 (string= path (slop-definition-path definition)))
               (slop-index-definitions index)))
             (t
              (slop-index-definitions index)))))
    (mapcar #'slop-definition->plist
            (slop-limit-list definitions limit))))

(defun slop-reference-matches-definition-p (reference definition)
  "Return true when REFERENCE points to DEFINITION's symbol namespace."
  (and (eq (slop-reference-namespace reference)
           (slop-definition-namespace definition))
       (string= (slop-reference-normalized-name reference)
                (slop-definition-normalized-name definition))
       (or (null (slop-definition-package definition))
           (null (slop-reference-package reference))
           (string= (slop-definition-package definition)
                    (slop-reference-package reference)))))

(defun slop-find-references (project &key path symbol package namespace
                                      definition-id role substring
                                      (limit *slop-default-result-limit*))
  "Return symbol references in PROJECT."
  (let* ((index (slop-index-project project :path path))
         (definition (and definition-id
                          (slop-find-definition-by-id index definition-id)))
         (stale-definition-id-p (and definition-id (null definition)))
         (references
           (cond
             (definition
              (remove-if-not
               (lambda (reference)
                 (and (not (eq :definition (slop-reference-role reference)))
                      (slop-reference-matches-definition-p reference definition)
                      (or (null role)
                          (string= (slop-normalize-name role)
                                   (slop-normalize-name
                                   (slop-reference-role reference))))))
               (slop-index-references index)))
             ((and stale-definition-id-p (null symbol))
              nil)
             (t
              (remove :definition
                      (slop-filter-references
                       (slop-index-references index)
                       :symbol symbol
                       :package package
                       :namespace namespace
                       :role role
                      :substring substring)
                      :key #'slop-reference-role
                      :test #'eq)))))
    (append
     (list :project (slop-project-index-project index))
     (when stale-definition-id-p
       (list :status :definition-id-not-found
             :requested-definition-id definition-id
             :fallback (and symbol :symbol)
             :matching-definitions
             (slop-definition-id-hints index definition-id
                                       :symbol symbol
                                       :package package
                                       :namespace namespace
                                       :limit 10)))
     (list :count (length references)
           :references
           (mapcar #'slop-reference->plist
                   (slop-limit-list references limit))))))

(defun slop-definition-at-offset (definitions offset)
  "Return a definition whose name or form contains OFFSET."
  (or (find-if (lambda (definition)
                 (and (<= (slop-definition-name-start definition) offset)
                      (< offset (slop-definition-name-end definition))))
               definitions)
      (slop-definition-containing-offset definitions offset)))

(defun slop-binding-at-offset (bindings offset)
  "Return a binding whose name contains OFFSET."
  (find-if (lambda (binding)
             (and (<= (slop-binding-start binding) offset)
                  (< offset (slop-binding-end binding))))
           bindings))

(defun slop-reference-at-offset (references offset)
  "Return a reference whose token contains OFFSET."
  (find-if (lambda (reference)
             (and (<= (slop-reference-start reference) offset)
                  (< offset (slop-reference-end reference))))
           references))

(defun slop-symbol-at (project path &key offset line column)
  "Return the SLOP symbol at PROJECT/PATH location."
  (let* ((index (slop-index-project project :path path))
         (resource-path (project-resource-name path))
         (file-index (or (slop-index-file-by-path index resource-path)
                         (error "No indexed Lisp file for ~A." path)))
         (text (slop-file-index-text file-index))
         (position (or offset
                       (slop-offset-for-line-column text line column)))
         (definition (slop-definition-at-offset
                      (slop-file-index-definitions file-index)
                      position))
         (binding (slop-binding-at-offset
                   (slop-file-index-bindings file-index)
                   position))
         (reference (slop-reference-at-offset
                     (slop-file-index-references file-index)
                     position)))
    (list :project (slop-file-index-project file-index)
          :path resource-path
          :offset position
          :definition (and definition
                           (slop-definition->plist definition))
          :binding (and binding (slop-binding->plist binding))
          :reference (and reference (slop-reference->plist reference)))))

(defun slop-resolve-one-definition (index &key definition-id symbol path
                                         package namespace kind
                                         (default-namespace :function))
  "Resolve a single definition from INDEX."
  (cond
    (definition-id
     (or (slop-find-definition-by-id index definition-id)
         (if symbol
             (slop-resolve-one-definition
              index
              :symbol symbol
              :path path
              :package package
              :namespace namespace
              :kind kind
              :default-namespace default-namespace)
             (error "Unknown SLOP definition id: ~A." definition-id))))
    (symbol
     (let ((matches
             (slop-filter-definitions
              (slop-index-definitions index)
              :symbol symbol
              :package package
              :namespace (or namespace default-namespace)
              :kind kind)))
       (setf matches
             (if path
                 (remove-if-not
                  (lambda (definition)
                    (string= (project-resource-name path)
                             (slop-definition-path definition)))
                  matches)
                 matches))
       (case (length matches)
         (0 (error "No function definition found for ~A." symbol))
         (1 (first matches))
         (t (error "Ambiguous function definition ~A; candidates are ~{~A~^, ~}."
                   symbol
                   (mapcar #'slop-definition-id matches))))))
    (t
     (error "Provide :definition-id or :symbol."))))

(defun slop-file-index-for-definition (index definition)
  "Return the indexed file containing DEFINITION."
  (or (slop-index-file-by-path index (slop-definition-path definition))
      (error "No indexed file for definition path ~A."
             (slop-definition-path definition))))

(defun slop-root-containing-span (file-index start end)
  "Return the top-level root in FILE-INDEX containing START..END."
  (find-if (lambda (root)
             (and (<= (sexed-node-start root) start)
                  (<= end (sexed-node-end root))))
           (slop-file-index-roots file-index)))

(defun slop-package-root-name (root text)
  "Return the package named by a DEFPACKAGE or IN-PACKAGE ROOT."
  (let ((head (string-downcase (or (sexed-node-head root) ""))))
    (when (member head '("defpackage" "in-package") :test #'string=)
      (let ((name-node (slop-nth-child root 1)))
        (and name-node
             (slop-package-name-from-node name-node text nil))))))

(defun slop-definition-package-forms (roots text definition root-index)
  "Return package-related top-level forms relevant to DEFINITION."
  (let ((forms nil)
        (definition-package (slop-definition-package definition)))
    (loop :for root :in roots
          :for index :from 0
          :for head := (string-downcase (or (sexed-node-head root) ""))
          :while (< index root-index)
          :do (cond
                ((and (string= head "defpackage")
                      (or (null definition-package)
                          (string= (or (slop-package-root-name root text) "")
                                   definition-package)))
                 (push (slop-node-source-plist root text
                                               :role :defpackage)
                       forms))
                ((and (string= head "in-package")
                      (or (null definition-package)
                          (string= (or (slop-package-root-name root text) "")
                                   definition-package)))
                 (push (slop-node-source-plist root text
                                               :role :in-package)
                       forms))))
    (nreverse forms)))

(defun slop-definition-context (project &key path symbol definition-id
                                          package namespace kind
                                          (before-forms 1)
                                          (after-forms 1))
  "Return a definition body with nearby top-level and package context."
  (unless (and (integerp before-forms) (not (minusp before-forms))
               (integerp after-forms) (not (minusp after-forms)))
    (error "before-forms and after-forms must be non-negative integers."))
  (let* ((index (slop-index-project project :path path))
         (definition (slop-resolve-one-definition
                      index
                      :definition-id definition-id
                      :symbol symbol
                      :path path
                      :package package
                      :namespace namespace
                      :kind kind
                      :default-namespace nil))
         (file-index (slop-file-index-for-definition index definition))
         (text (slop-file-index-text file-index))
         (roots (slop-file-index-roots file-index))
         (root (or (slop-root-containing-span
                    file-index
                    (slop-definition-start definition)
                    (slop-definition-end definition))
                   (error "No top-level source form found for definition ~A."
                          (slop-definition-id definition))))
         (root-index (position root roots :test #'eq))
         (start-index (max 0 (- root-index before-forms)))
         (end-index (min (length roots) (+ root-index after-forms 1)))
         (nearby-forms
           (loop :for candidate :in (subseq roots start-index end-index)
                 :for index :from start-index
                 :for role := (cond
                                ((= index root-index) :definition)
                                ((< index root-index) :before)
                                (t :after))
                 :collect (slop-node-source-plist candidate text
                                                  :role role))))
    (list :project (slop-project-index-project index)
          :path (slop-file-index-path file-index)
          :definition (slop-definition->plist definition :include-body t)
          :line-range (slop-line-range-plist
                       text
                       (slop-definition-start definition)
                       (slop-definition-end definition))
          :package-forms
          (slop-definition-package-forms roots text definition root-index)
          :nearby-forms nearby-forms)))

(defun slop-find-callers (project &key path symbol definition-id
                                  (limit *slop-default-result-limit*))
  "Return function callers grouped by enclosing definition."
  (let* ((index (slop-index-project project :path path))
         (definition (slop-resolve-one-definition
                      index
                      :definition-id definition-id
                      :symbol symbol
                      :path path))
         (call-references
           (remove-if-not
            (lambda (reference)
              (and (eq :call (slop-reference-role reference))
                   (slop-reference-matches-definition-p reference definition)))
            (slop-index-references index)))
         (groups (make-hash-table :test #'equal)))
    (dolist (reference call-references)
      (push reference
            (gethash (or (slop-reference-enclosing-definition-id reference)
                         ":top-level")
                     groups)))
    (let ((callers nil)
          (definitions (slop-index-definitions index)))
      (maphash (lambda (caller-id calls)
                 (let ((caller (find caller-id definitions
                                     :key #'slop-definition-id
                                     :test #'string=)))
                   (push (list :caller-id caller-id
                               :caller (and caller
                                            (slop-definition->plist caller))
                               :call-count (length calls)
                               :calls (mapcar #'slop-reference->plist
                                              (nreverse calls)))
                         callers)))
               groups)
      (setf callers (sort callers #'string<
                          :key (lambda (caller)
                                 (getf caller :caller-id))))
      (list :project (slop-project-index-project index)
            :definition (slop-definition->plist definition)
            :count (length callers)
            :callers (slop-limit-list callers limit)))))

(defun slop-find-callees (project &key path symbol definition-id
                                  (limit *slop-default-result-limit*))
  "Return function calls from a selected definition."
  (let* ((index (slop-index-project project :path path))
         (definition (slop-resolve-one-definition
                      index
                      :definition-id definition-id
                      :symbol symbol
                      :path path))
         (calls
           (remove-if-not
            (lambda (reference)
              (and (eq :call (slop-reference-role reference))
                   (string= (slop-definition-id definition)
                            (or (slop-reference-enclosing-definition-id
                                 reference)
                                ""))))
            (slop-index-references index)))
         (seen (make-hash-table :test #'equal))
         (callees nil))
    (dolist (call calls)
      (let ((key (slop-reference-qualified-name call)))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push (list :name (slop-reference-name call)
                      :package (slop-reference-package call)
                      :qualified-name (slop-reference-qualified-name call)
                      :first-call (slop-reference->plist call)
                      :call-count
                      (count key calls
                             :key #'slop-reference-qualified-name
                             :test #'string=))
                callees))))
    (list :project (slop-project-index-project index)
          :definition (slop-definition->plist definition)
          :count (length callees)
          :callees (slop-limit-list (sort (nreverse callees)
                                          #'string<
                                          :key (lambda (callee)
                                                 (getf callee
                                                       :qualified-name)))
                                    limit))))

(defun slop-definitions-matching-reference (index reference)
  "Return definitions that may be the target of function call REFERENCE."
  (remove-if-not
   (lambda (definition)
     (slop-reference-matches-definition-p reference definition))
   (slop-index-definitions index)))

(defun slop-call-references-from-definition (index definition)
  "Return call references whose enclosing definition is DEFINITION."
  (remove-if-not
   (lambda (reference)
     (and (eq :call (slop-reference-role reference))
          (string= (slop-definition-id definition)
                   (or (slop-reference-enclosing-definition-id reference)
                       ""))))
   (slop-index-references index)))

(defun slop-call-references-to-definition (index definition)
  "Return call references that target DEFINITION."
  (remove-if-not
   (lambda (reference)
     (and (eq :call (slop-reference-role reference))
          (slop-reference-matches-definition-p reference definition)))
   (slop-index-references index)))

(defun slop-trace-direction-list (direction)
  "Normalize DIRECTION into one or more trace directions."
  (let ((name (slop-normalize-name (or direction "callees"))))
    (cond
      ((member name '("callee" "callees" "out" "outbound") :test #'string=)
       '(:callees))
      ((member name '("caller" "callers" "in" "inbound") :test #'string=)
       '(:callers))
      ((string= name "both")
       '(:callees :callers))
      (t
       (error "Unknown slop trace direction: ~A." direction)))))

(defun slop-trace-edge (direction depth caller callee call)
  "Return an agent-facing trace edge."
  (list :direction direction
        :depth depth
        :caller-id (and caller (slop-definition-id caller))
        :caller (and caller
                     (slop-definition-qualified-name caller))
        :callee-id (and callee (slop-definition-id callee))
        :callee (if callee
                    (slop-definition-qualified-name callee)
                    (slop-reference-qualified-name call))
        :resolved (not (null callee))
        :call (slop-reference->plist call)))

(defun slop-trace-calls (project &key path symbol definition-id
                                  direction
                                  (max-depth 2)
                                  include-body
                                  (limit *slop-default-result-limit*))
  "Trace calls outward, inward, or both from a selected definition."
  (unless (and (integerp max-depth) (not (minusp max-depth)))
    (error "max-depth must be a non-negative integer."))
  (let* ((index (slop-index-project project :path path))
         (root (slop-resolve-one-definition
                index
                :definition-id definition-id
                :symbol symbol
                :path path))
         (directions (slop-trace-direction-list direction))
         (visited (make-hash-table :test #'equal))
         (nodes (make-hash-table :test #'equal))
         (queue nil)
         (edges nil)
         (truncated nil))
    (labels ((remember-node (definition depth)
               (let ((id (slop-definition-id definition)))
                 (unless (gethash id nodes)
                   (setf (gethash id nodes)
                         (list :depth depth
                               :definition
                               (slop-definition->plist
                                definition
                                :include-body include-body))))))
             (enqueue (definition depth)
               (let ((id (slop-definition-id definition)))
                 (remember-node definition depth)
                 (unless (gethash id visited)
                   (setf (gethash id visited) depth)
                   (setf queue
                         (nconc queue (list (cons definition depth)))))))
             (record-edge (edge)
               (if (or (null limit) (< (length edges) limit))
                   (push edge edges)
                   (setf truncated t)))
             (trace-callees (definition depth)
               (dolist (call (slop-call-references-from-definition index
                                                                    definition))
                 (let ((targets (slop-definitions-matching-reference
                                 index call)))
                   (cond
                     (targets
                      (dolist (target targets)
                        (record-edge
                         (slop-trace-edge :callees
                                          (1+ depth)
                                          definition
                                          target
                                          call))
                        (when (< depth max-depth)
                          (enqueue target (1+ depth)))))
                     (t
                      (record-edge
                       (slop-trace-edge :callees
                                        (1+ depth)
                                        definition
                                        nil
                                        call)))))))
             (trace-callers (definition depth)
               (dolist (call (slop-call-references-to-definition index
                                                                  definition))
                 (let* ((caller-id (slop-reference-enclosing-definition-id
                                    call))
                        (caller (and caller-id
                                     (slop-find-definition-by-id
                                      index caller-id))))
                   (record-edge
                    (slop-trace-edge :callers
                                     (1+ depth)
                                     caller
                                     definition
                                     call))
                   (when (and caller (< depth max-depth))
                     (enqueue caller (1+ depth)))))))
      (enqueue root 0)
      (loop :while queue
            :do (let* ((entry (pop queue))
                       (definition (car entry))
                       (depth (cdr entry)))
                  (when (< depth max-depth)
                    (when (member :callees directions :test #'eq)
                      (trace-callees definition depth))
                    (when (member :callers directions :test #'eq)
                      (trace-callers definition depth)))))
      (let ((node-list nil))
        (maphash (lambda (_id node)
                   (declare (ignore _id))
                   (push node node-list))
                 nodes)
        (list :project (slop-project-index-project index)
              :root (slop-definition->plist root :include-body include-body)
              :direction (or direction "callees")
              :max-depth max-depth
              :node-count (length node-list)
              :nodes (sort node-list #'< :key (lambda (node)
                                                (getf node :depth)))
              :edge-count (length edges)
              :truncated truncated
              :edges (nreverse edges))))))

(defun slop-binding-from-location (index project path offset line column)
  "Resolve a lexical binding from a location."
  (let* ((resource-path (project-resource-name path))
         (file-index (or (slop-index-file-by-path index resource-path)
                         (error "No indexed Lisp file for ~A." path)))
         (text (slop-file-index-text file-index))
         (position (or offset
                       (slop-offset-for-line-column text line column)))
         (binding (or (slop-binding-at-offset
                       (slop-file-index-bindings file-index)
                       position)
                      (let ((reference (slop-reference-at-offset
                                        (slop-file-index-references file-index)
                                        position)))
                        (and reference
                             (slop-reference-binding-id reference)
                             (find (slop-reference-binding-id reference)
                                   (slop-file-index-bindings file-index)
                                   :key #'slop-binding-id
                                   :test #'string=))))))
    (or binding
        (error "No lexical binding found at ~A:~A." project path))))

(defun slop-find-variable-uses (project &key path binding-id offset line column
                                        (limit *slop-default-result-limit*))
  "Return binding, access, and set occurrences for one lexical variable."
  (let* ((index (slop-index-project project :path path))
         (binding (cond
                    (binding-id
                     (or (slop-find-binding-by-id index binding-id)
                         (error "Unknown SLOP binding id: ~A." binding-id)))
                    (path
                     (slop-binding-from-location index project path
                                                 offset line column))
                    (t
                     (error "Provide :binding-id or :path plus location."))))
         (references
           (remove-if-not
            (lambda (reference)
              (string= (or (slop-reference-binding-id reference) "")
                       (slop-binding-id binding)))
            (slop-index-references index))))
    (list :project (slop-project-index-project index)
          :binding (slop-binding->plist binding)
          :count (length references)
          :uses (mapcar #'slop-reference->plist
                        (slop-limit-list references limit)))))

(defun slop-valid-new-variable-name-p (name)
  "Return true when NAME is a simple unqualified lexical variable name."
  (and (slop-bindable-symbol-token-p name)
       (not (slop-token-prefix-p "*" name))
       (not (slop-token-contains-p ":" name))))

(defun slop-special-variable-name-p (name)
  "Return true when NAME follows the *SPECIAL* naming convention."
  (and (> (length name) 1)
       (char= #\* (char name 0))
       (char= #\* (char name (1- (length name))))))

(defun slop-binding-scope-overlap-p (left right)
  "Return true when LEFT and RIGHT lexical scopes overlap."
  (and (< (slop-binding-scope-start left) (slop-binding-scope-end right))
       (< (slop-binding-scope-start right) (slop-binding-scope-end left))))

(defun slop-rename-collisions (bindings binding new-name)
  "Return bindings that would collide with renaming BINDING to NEW-NAME."
  (remove-if-not
   (lambda (candidate)
     (and (not (string= (slop-binding-id candidate)
                        (slop-binding-id binding)))
          (string= (slop-binding-normalized-name candidate)
                   (slop-normalize-name new-name))
          (slop-binding-scope-overlap-p binding candidate)))
   bindings))

(defun slop-apply-span-replacements (text replacements)
  "Apply REPLACEMENTS of (START END NEW-TEXT) from the end of TEXT backward."
  (let ((result text))
    (dolist (replacement
             (sort (copy-list replacements) #'>
                   :key #'first)
             result)
      (destructuring-bind (start end new-text) replacement
        (setf result
              (concatenate 'string
                           (subseq result 0 start)
                           new-text
                           (subseq result end)))))))

(defun slop-rename-variable (project &key path binding-id offset line column
                                     new-name)
  "Rename one lexical variable binding and its references on disk."
  (unless (and new-name (slop-valid-new-variable-name-p new-name))
    (error "New lexical variable name must be a simple unqualified symbol: ~S."
           new-name))
  (let* ((index (slop-index-project project :path path))
         (binding (cond
                    (binding-id
                     (or (slop-find-binding-by-id index binding-id)
                         (error "Unknown SLOP binding id: ~A." binding-id)))
                    (path
                     (slop-binding-from-location index project path
                                                 offset line column))
                    (t
                     (error "Provide :binding-id or :path plus location."))))
         (old-name (slop-binding-name binding)))
    (when (slop-special-variable-name-p old-name)
      (error "SLOP only renames lexical variables, not special-style names: ~A."
             old-name))
    (let* ((resource-path (slop-binding-path binding))
           (file-index (or (slop-index-file-by-path index resource-path)
                           (error "No indexed file for binding path ~A."
                                  resource-path)))
           (bindings (slop-file-index-bindings file-index))
           (collisions (slop-rename-collisions bindings binding new-name)))
      (when collisions
        (error "Renaming ~A to ~A would collide with binding ids ~{~A~^, ~}."
               old-name
               new-name
               (mapcar #'slop-binding-id collisions)))
      (let* ((references
               (remove-if-not
                (lambda (reference)
                  (string= (or (slop-reference-binding-id reference) "")
                           (slop-binding-id binding)))
                (slop-file-index-references file-index)))
             (replacements
               (remove-duplicates
                (cons (list (slop-binding-start binding)
                            (slop-binding-end binding)
                            new-name)
                      (mapcar (lambda (reference)
                                (list (slop-reference-start reference)
                                      (slop-reference-end reference)
                                      new-name))
                              references))
                :test #'equal))
             (old-text (slop-file-index-text file-index))
             (new-text (slop-apply-span-replacements old-text replacements)))
        (sexed-ensure-balanced new-text "Renamed Lisp source")
        (project-save-file project resource-path new-text)
        (clrhash *slop-project-index-cache*)
        (list :status :ok
              :project (project-name (ensure-project project))
              :path resource-path
              :binding-id (slop-binding-id binding)
              :old-name old-name
              :new-name new-name
              :replacements (length replacements)
              :updated-uses (length references)
              :diff (compute-simple-diff old-text new-text))))))

(defparameter *slop-mention-skipped-file-types*
  '("png" "jpg" "jpeg" "gif" "webp" "ico" "pdf" "zip" "gz" "xz" "bz2"
    "fasl" "o" "so" "dylib" "a" "class" "jar" "sqlite" "db")
  "Project file extensions skipped by SLOP mention search.")

(defun slop-mention-readable-path-p (path)
  "Return true when PATH is likely to be a text file worth mention-searching."
  (let ((type (string-downcase (or (pathname-type (pathname path)) ""))))
    (not (member type *slop-mention-skipped-file-types* :test #'string=))))

(defun slop-project-text-paths (project &optional path)
  "Return project text paths, optionally restricted to PATH."
  (let ((all (remove-if-not #'slop-mention-readable-path-p
                            (project-list-files project :limit nil))))
    (if (null path)
        all
        (let* ((resource (project-resource-name path :allow-directory t))
               (prefix (if (and (plusp (length resource))
                                (char= #\/ (char resource
                                                 (1- (length resource)))))
                           resource
                           (concatenate 'string resource "/"))))
          (cond
            ((member resource all :test #'string=)
             (list resource))
            (t
             (remove-if-not
              (lambda (candidate)
                (alexandria:starts-with-subseq prefix candidate))
              all)))))))

(defun slop-mention-symbol-character-p (char)
  "Return true when CHAR is a likely Lisp/documentation symbol constituent."
  (or (alphanumericp char)
      (find char "!$%&*+-./:<=>?@[]^_{}~#:" :test #'char=)))

(defun slop-mention-whole-symbol-match-p (text start end)
  "Return true when START..END is not embedded in a larger symbol."
  (and (or (zerop start)
           (not (slop-mention-symbol-character-p
                 (char text (1- start)))))
       (or (>= end (length text))
           (not (slop-mention-symbol-character-p
                 (char text end))))))

(defun slop-find-text-occurrences (text query &key substring case-sensitive)
  "Return START offsets where QUERY occurs in TEXT."
  (let ((positions nil)
        (start 0)
        (test (if case-sensitive #'char= #'char-equal)))
    (loop :for position := (search query text
                                   :start2 start
                                   :test test)
          :while position
          :for end := (+ position (length query))
          :do (when (or substring
                        (slop-mention-whole-symbol-match-p text
                                                           position
                                                           end))
                (push position positions))
              (setf start (1+ position)))
    (nreverse positions)))

(defun slop-mention-plist (project path text query position)
  "Return an agent-facing mention plist for QUERY at POSITION."
  (let ((end (+ position (length query))))
    (multiple-value-bind (line column)
        (slop-node-line-column text position)
      (list :project project
            :path path
            :line line
            :column column
            :start position
            :end end
            :query query
            :preview (slop-preview-around text position end)))))

(defun slop-find-mentions (project query &key path substring case-sensitive
                                           (limit *slop-default-result-limit*))
  "Find text mentions of QUERY across project source, docs, tests, and config."
  (unless (and (stringp query) (plusp (length query)))
    (error "Mention query must be a non-empty string."))
  (let* ((project-object (ensure-project project))
         (project-name (project-name project-object))
         (mentions nil)
         (truncated nil))
    (dolist (resource-path (slop-project-text-paths project-object path))
      (when (or (null limit) (< (length mentions) limit))
        (handler-case
            (let ((text (project-read-file project-object resource-path)))
              (dolist (position (slop-find-text-occurrences
                                 text query
                                 :substring substring
                                 :case-sensitive case-sensitive))
                (if (or (null limit) (< (length mentions) limit))
                    (push (slop-mention-plist project-name
                                              resource-path
                                              text
                                              query
                                              position)
                          mentions)
                    (setf truncated t))))
          (error () nil))))
    (list :project project-name
          :query query
          :count (length mentions)
          :truncated truncated
          :mentions (nreverse mentions))))

;;; --------------------------------------------------------------------------
;;; Provider tool adapters
;;; --------------------------------------------------------------------------

(defun slop-tool-key-name (key)
  "Return KEY as a normalized provider argument name."
  (cond
    ((keywordp key) (string-downcase (symbol-name key)))
    ((symbolp key) (string-downcase (symbol-name key)))
    ((stringp key) (string-downcase key))
    (t (string-downcase (princ-to-string key)))))

(defun slop-tool-key= (left right)
  "Return true when provider keys LEFT and RIGHT are equivalent."
  (string= (slop-tool-key-name left)
           (slop-tool-key-name right)))

(defun slop-tool-arg (args key &optional default)
  "Return provider ARGS value for KEY."
  (loop :for (arg-key . value) :in (tool-args-alist args)
        :when (slop-tool-key= arg-key key)
          :return value
        :finally (return default)))

(defun slop-tool-required-string (args key)
  "Return required string KEY from ARGS."
  (let ((value (slop-tool-arg args key)))
    (unless (and value (stringp value))
      (error "Missing required slop tool argument :~A." key))
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
      (when (blank-string-p trimmed)
        (error "Missing required slop tool argument :~A." key))
      trimmed)))

(defun slop-tool-optional-string (args key)
  "Return optional string KEY from ARGS, treating blank strings as absent."
  (let ((value (slop-tool-arg args key)))
    (when value
      (unless (stringp value)
        (error "SLOP argument :~A must be a string." key))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))
        (unless (blank-string-p trimmed)
          trimmed)))))

(defun slop-tool-string-list (value key)
  "Normalize VALUE as a list of non-blank strings for tool argument KEY."
  (let ((items (cond
                 ((null value) nil)
                 ((vectorp value) (coerce value 'list))
                 ((and (listp value) (not (stringp value))) value)
                 ((stringp value) (list value))
                 (t
                  (error "SLOP argument :~A must be a string array." key)))))
    (loop :for item :in items
          :for trimmed := (and (stringp item)
                               (string-trim '(#\Space #\Tab #\Newline
                                               #\Return)
                                            item))
          :unless (stringp item)
            :do (error "SLOP argument :~A must contain only strings." key)
          :unless (blank-string-p trimmed)
            :collect trimmed)))

(defun slop-tool-required-string-list (args key)
  "Return required string-list KEY from ARGS."
  (let ((items (slop-tool-string-list (slop-tool-arg args key) key)))
    (unless items
      (error "Missing required slop tool argument :~A." key))
    items))

(defun slop-tool-optional-integer (args key)
  "Return optional integer KEY from ARGS."
  (let ((value (slop-tool-arg args key)))
    (when value
      (unless (integerp value)
        (error "SLOP argument :~A must be an integer." key))
      value)))

(defun slop-tool-optional-boolean (args key)
  "Return optional boolean KEY from ARGS."
  (let ((value (slop-tool-arg args key)))
    (and value t)))

(defun slop-tool-project-symbols (args)
  "Provider adapter for slop_project_symbols."
  (slop-project-symbols
   (slop-tool-required-string args :project)
   :path (slop-tool-optional-string args :path)
   :package (slop-tool-optional-string args :package)
   :kind (slop-tool-optional-string args :kind)
   :name-pattern (slop-tool-optional-string args :name-pattern)
   :substring (slop-tool-optional-boolean args :substring)
   :include-body (slop-tool-optional-boolean args :include-body)
   :limit (or (slop-tool-optional-integer args :limit)
              *slop-default-result-limit*)))

(defun slop-tool-symbol-at (args)
  "Provider adapter for slop_symbol_at."
  (slop-symbol-at
   (slop-tool-required-string args :project)
   (slop-tool-required-string args :path)
   :offset (slop-tool-optional-integer args :offset)
   :line (slop-tool-optional-integer args :line)
   :column (slop-tool-optional-integer args :column)))

(defun slop-tool-find-definitions (args)
  "Provider adapter for slop_find_definitions."
  (slop-find-definitions
   (slop-tool-required-string args :project)
   (slop-tool-required-string args :symbol)
   :path (slop-tool-optional-string args :path)
   :package (slop-tool-optional-string args :package)
   :namespace (slop-tool-optional-string args :namespace)
   :kind (slop-tool-optional-string args :kind)
   :include-body (slop-tool-optional-boolean args :include-body)
   :limit (or (slop-tool-optional-integer args :limit)
              *slop-default-result-limit*)))

(defun slop-tool-find-definitions-batch (args)
  "Provider adapter for slop_find_definitions_batch."
  (slop-find-definitions-batch
   (slop-tool-required-string args :project)
   (slop-tool-required-string-list args :symbols)
   :path (slop-tool-optional-string args :path)
   :package (slop-tool-optional-string args :package)
   :namespace (slop-tool-optional-string args :namespace)
   :kind (slop-tool-optional-string args :kind)
   :include-body (slop-tool-optional-boolean args :include-body)
   :per-symbol-limit
   (or (slop-tool-optional-integer args :per-symbol-limit)
       *slop-default-result-limit*)))

(defun slop-tool-find-references (args)
  "Provider adapter for slop_find_references."
  (slop-find-references
   (slop-tool-required-string args :project)
   :path (slop-tool-optional-string args :path)
   :symbol (slop-tool-optional-string args :symbol)
   :package (slop-tool-optional-string args :package)
   :namespace (slop-tool-optional-string args :namespace)
   :definition-id (slop-tool-optional-string args :definition-id)
   :role (slop-tool-optional-string args :role)
   :substring (slop-tool-optional-boolean args :substring)
   :limit (or (slop-tool-optional-integer args :limit)
              *slop-default-result-limit*)))

(defun slop-tool-find-callers (args)
  "Provider adapter for slop_find_callers."
  (slop-find-callers
   (slop-tool-required-string args :project)
   :path (slop-tool-optional-string args :path)
   :symbol (slop-tool-optional-string args :symbol)
   :definition-id (slop-tool-optional-string args :definition-id)
   :limit (or (slop-tool-optional-integer args :limit)
              *slop-default-result-limit*)))

(defun slop-tool-find-callees (args)
  "Provider adapter for slop_find_callees."
  (slop-find-callees
   (slop-tool-required-string args :project)
   :path (slop-tool-optional-string args :path)
   :symbol (slop-tool-optional-string args :symbol)
   :definition-id (slop-tool-optional-string args :definition-id)
   :limit (or (slop-tool-optional-integer args :limit)
              *slop-default-result-limit*)))

(defun slop-tool-trace-calls (args)
  "Provider adapter for slop_trace_calls."
  (slop-trace-calls
   (slop-tool-required-string args :project)
   :path (slop-tool-optional-string args :path)
   :symbol (slop-tool-optional-string args :symbol)
   :definition-id (slop-tool-optional-string args :definition-id)
   :direction (slop-tool-optional-string args :direction)
   :max-depth (or (slop-tool-optional-integer args :max-depth) 2)
   :include-body (slop-tool-optional-boolean args :include-body)
   :limit (or (slop-tool-optional-integer args :limit)
              *slop-default-result-limit*)))

(defun slop-tool-find-mentions (args)
  "Provider adapter for slop_find_mentions."
  (slop-find-mentions
   (slop-tool-required-string args :project)
   (slop-tool-required-string args :query)
   :path (slop-tool-optional-string args :path)
   :substring (slop-tool-optional-boolean args :substring)
   :case-sensitive (slop-tool-optional-boolean args :case-sensitive)
   :limit (or (slop-tool-optional-integer args :limit)
              *slop-default-result-limit*)))

(defun slop-tool-definition-context (args)
  "Provider adapter for slop_definition_context."
  (slop-definition-context
   (slop-tool-required-string args :project)
   :path (slop-tool-optional-string args :path)
   :symbol (slop-tool-optional-string args :symbol)
   :definition-id (slop-tool-optional-string args :definition-id)
   :package (slop-tool-optional-string args :package)
   :namespace (slop-tool-optional-string args :namespace)
   :kind (slop-tool-optional-string args :kind)
   :before-forms (or (slop-tool-optional-integer args :before-forms) 1)
   :after-forms (or (slop-tool-optional-integer args :after-forms) 1)))

(defun slop-tool-find-variable-uses (args)
  "Provider adapter for slop_find_variable_uses."
  (slop-find-variable-uses
   (slop-tool-required-string args :project)
   :path (slop-tool-optional-string args :path)
   :binding-id (slop-tool-optional-string args :binding-id)
   :offset (slop-tool-optional-integer args :offset)
   :line (slop-tool-optional-integer args :line)
   :column (slop-tool-optional-integer args :column)
   :limit (or (slop-tool-optional-integer args :limit)
              *slop-default-result-limit*)))

(defun slop-tool-rename-variable (args)
  "Provider adapter for slop_rename_variable."
  (slop-rename-variable
   (slop-tool-required-string args :project)
   :path (slop-tool-optional-string args :path)
   :binding-id (slop-tool-optional-string args :binding-id)
   :offset (slop-tool-optional-integer args :offset)
   :line (slop-tool-optional-integer args :line)
   :column (slop-tool-optional-integer args :column)
   :new-name (slop-tool-required-string args :new-name)))
