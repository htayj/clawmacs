(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Logical Windows
;;; --------------------------------------------------------------------------

(declaim (type integer *rplaca-window-counter*))
(defvar *rplaca-window-counter* 0
  "Counter used to assign process-local logical window ids.")

(declaim (ftype (function () integer) next-rplaca-window-id))
(defun next-rplaca-window-id ()
  "Return a fresh process-local logical window id."
  (incf *rplaca-window-counter*))

(defclass rplaca-window ()
  ((id :initarg :id
       :reader rplaca-window-id
       :type integer
       :documentation "Stable process-local id for this logical window.")
   (buffer :initarg :buffer
           :accessor rplaca-window-buffer
           :type (or null buffer)
           :documentation "Buffer displayed by this logical window."))
  (:documentation
   "An Emacs-style logical window.

RPLACA windows are not CLIM sheets.  The McCLIM frame owns one transcript
pane and renders these logical windows into sub-regions of that pane, keeping
CLIM pane layout simple while exposing Emacs-like split/delete/window cycling
semantics."))

(declaim (ftype (function (&key (:buffer t) (:id (or null integer)))
                          rplaca-window)
                make-rplaca-window))
(defun make-rplaca-window (&key buffer id)
  "Return a logical window displaying BUFFER."
  (declare (type (or null integer) id))
  (make-instance 'rplaca-window
    :id (or id (next-rplaca-window-id))
    :buffer buffer))

(defstruct (rplaca-window-node
            (:constructor %make-rplaca-window-node
                (&key kind window orientation first second)))
  "A node in the logical window tree.

KIND is :LEAF or :SPLIT.  Leaf nodes hold WINDOW.  Split nodes hold
ORIENTATION plus FIRST and SECOND child nodes.  ORIENTATION is :VERTICAL for a
top/bottom split and :HORIZONTAL for a left/right split, matching CLIM's
VERTICALLY/HORIZONTALLY layout terminology."
  (kind nil :type (or null keyword))
  (window nil :type (or null rplaca-window))
  (orientation nil :type (or null keyword))
  (first nil :type (or null rplaca-window-node))
  (second nil :type (or null rplaca-window-node)))

(declaim
 (ftype (function (rplaca-window) rplaca-window-node)
        make-rplaca-window-leaf)
 (ftype (function (keyword rplaca-window-node rplaca-window-node)
                  rplaca-window-node)
        make-rplaca-window-split)
 (ftype (function (t) rplaca-window-node) make-rplaca-window-tree)
 (ftype (function (t) boolean) rplaca-window-node-leaf-p
        rplaca-window-node-split-p)
 (ftype (function (t) list) rplaca-window-tree-windows)
 (ftype (function (t) integer) rplaca-window-tree-count)
 (ftype (function (t integer) (or null rplaca-window))
        rplaca-window-tree-find-window)
 (ftype (function (t integer) (or null rplaca-window-node))
        rplaca-window-tree-find-node)
 (ftype (function (t integer keyword) (or null rplaca-window))
        split-rplaca-window-tree)
 (ftype (function (t integer &key (:reverse t)) (or null rplaca-window))
        rplaca-window-tree-next-window)
 (ftype (function (t integer) (values t (or null rplaca-window) boolean))
        delete-rplaca-window-from-tree
        delete-other-rplaca-windows)
 (ftype (function (t list t) boolean) rplaca-window-tree-replace-dead-buffers))

(defun make-rplaca-window-leaf (window)
  "Return a leaf node for WINDOW."
  (%make-rplaca-window-node :kind :leaf :window window))

(defun make-rplaca-window-split (orientation first second)
  "Return a split node with ORIENTATION, FIRST child, and SECOND child."
  (unless (member orientation '(:vertical :horizontal) :test #'eq)
    (error "Unknown window split orientation: ~S" orientation))
  (%make-rplaca-window-node :kind :split
                              :orientation orientation
                              :first first
                              :second second))

(defun make-rplaca-window-tree (buffer)
  "Return a new window tree containing one logical window for BUFFER."
  (make-rplaca-window-leaf
   (make-rplaca-window :buffer buffer)))

(defun rplaca-window-node-leaf-p (node)
  "Return true when NODE is a leaf node."
  (and node (eq (rplaca-window-node-kind node) :leaf)))

(defun rplaca-window-node-split-p (node)
  "Return true when NODE is a split node."
  (and node (eq (rplaca-window-node-kind node) :split)))

(defun rplaca-window-tree-windows (tree)
  "Return TREE's logical windows in display order."
  (labels ((walk (node)
             (cond
               ((null node) nil)
               ((rplaca-window-node-leaf-p node)
                (list (rplaca-window-node-window node)))
               ((rplaca-window-node-split-p node)
                (append (walk (rplaca-window-node-first node))
                        (walk (rplaca-window-node-second node))))
               (t nil))))
    (remove nil (walk tree))))

(defun rplaca-window-tree-count (tree)
  "Return the number of logical windows in TREE."
  (length (rplaca-window-tree-windows tree)))

(defun rplaca-window-tree-find-window (tree window-id)
  "Return the logical window with WINDOW-ID in TREE, or NIL."
  (find window-id (rplaca-window-tree-windows tree)
        :key #'rplaca-window-id
        :test #'eql))

(defun rplaca-window-tree-find-node (tree window-id)
  "Return the leaf node for WINDOW-ID in TREE, or NIL."
  (labels ((walk (node)
             (cond
               ((null node) nil)
               ((rplaca-window-node-leaf-p node)
                (and (eql window-id
                          (rplaca-window-id
                           (rplaca-window-node-window node)))
                     node))
               ((rplaca-window-node-split-p node)
                (or (walk (rplaca-window-node-first node))
                    (walk (rplaca-window-node-second node))))
               (t nil))))
    (walk tree)))

(defun split-rplaca-window-tree (tree window-id orientation)
  "Split WINDOW-ID in TREE and return the newly created logical window.

ORIENTATION is :VERTICAL for split-window-below behavior and :HORIZONTAL for
split-window-right behavior.  The existing window keeps its buffer; the new
window starts by displaying the same buffer."
  (let ((node (rplaca-window-tree-find-node tree window-id)))
    (when node
      (let* ((old-window (rplaca-window-node-window node))
             (new-window (make-rplaca-window
                          :buffer (rplaca-window-buffer old-window))))
        (setf (rplaca-window-node-kind node) :split
              (rplaca-window-node-window node) nil
              (rplaca-window-node-orientation node) orientation
              (rplaca-window-node-first node)
              (make-rplaca-window-leaf old-window)
              (rplaca-window-node-second node)
              (make-rplaca-window-leaf new-window))
        new-window))))

(defun rplaca-window-tree-next-window (tree window-id &key reverse)
  "Return the next logical window after WINDOW-ID in display order."
  (let* ((windows (rplaca-window-tree-windows tree))
         (count (length windows))
         (index (position window-id windows
                          :key #'rplaca-window-id
                          :test #'eql)))
    (when (and index (plusp count))
      (nth (mod (+ index (if reverse -1 1)) count) windows))))

(defun delete-rplaca-window-from-tree (tree window-id)
  "Delete WINDOW-ID from TREE.

Returns three values: the new tree root, the replacement selected window, and a
boolean indicating whether a window was deleted.  The final remaining window is
never deleted."
  (let ((windows (rplaca-window-tree-windows tree)))
    (cond
      ((or (null (cdr windows))
           (null (find window-id windows
                       :key #'rplaca-window-id
                       :test #'eql)))
       (values tree
               (or (first windows)
                   (and tree
                        (rplaca-window-node-leaf-p tree)
                        (rplaca-window-node-window tree)))
               nil))
      (t
       (let ((replacement (or (rplaca-window-tree-next-window tree window-id)
                              (first windows))))
         (labels ((walk (node)
                    (cond
                      ((null node) (values nil nil))
                      ((rplaca-window-node-leaf-p node)
                       (if (eql window-id
                                (rplaca-window-id
                                 (rplaca-window-node-window node)))
                           (values nil t)
                           (values node nil)))
                      ((rplaca-window-node-split-p node)
                       (multiple-value-bind (new-first deleted-first-p)
                           (walk (rplaca-window-node-first node))
                         (multiple-value-bind (new-second deleted-second-p)
                             (walk (rplaca-window-node-second node))
                           (cond
                             ((and deleted-first-p (null new-first))
                              (values new-second t))
                             ((and deleted-second-p (null new-second))
                              (values new-first t))
                             ((or deleted-first-p deleted-second-p)
                              (setf (rplaca-window-node-first node) new-first
                                    (rplaca-window-node-second node) new-second)
                              (values node t))
                             (t
                              (values node nil)))))))))
           (multiple-value-bind (new-root deleted-p)
               (walk tree)
             (values new-root replacement deleted-p))))))))

(defun delete-other-rplaca-windows (tree window-id)
  "Return a new tree containing only WINDOW-ID, preserving that window object."
  (let ((window (rplaca-window-tree-find-window tree window-id)))
    (if window
        (values (make-rplaca-window-leaf window) window t)
        (values tree nil nil))))

(defun rplaca-window-tree-replace-dead-buffers (tree live-buffers fallback)
  "Replace window buffers not in LIVE-BUFFERS with FALLBACK.

Returns true when any window buffer was changed."
  (let ((changed-p nil))
    (dolist (window (rplaca-window-tree-windows tree) changed-p)
      (unless (member (rplaca-window-buffer window) live-buffers :test #'eq)
        (setf (rplaca-window-buffer window) fallback
              changed-p t)))))

;;; --------------------------------------------------------------------------
;;; Window Layout
;;; --------------------------------------------------------------------------

(defstruct rplaca-window-layout-entry
  "A rendered grid rectangle for one logical window."
  (window nil :type (or null rplaca-window))
  (row nil :type (or null integer))
  (col nil :type (or null integer))
  (rows nil :type (or null integer))
  (cols nil :type (or null integer)))

(defstruct rplaca-window-separator
  "A grid rectangle used as visual separation between logical windows."
  (orientation nil :type (or null keyword))
  (row nil :type (or null integer))
  (col nil :type (or null integer))
  (rows nil :type (or null integer))
  (cols nil :type (or null integer)))

(declaim (ftype (function (integer) (values integer integer integer))
                split-window-space)
         (ftype (function (t integer integer) (values list list))
                rplaca-window-tree-layout))

(defun split-window-space (size)
  "Return FIRST, SEPARATOR, and SECOND sizes for splitting SIZE cells."
  (declare (type integer size))
  (cond
    ((<= size 1)
     (values size 0 0))
    ((= size 2)
     (values 1 0 1))
    (t
     (let* ((available (1- size))
            (first (floor available 2))
            (second (- available first)))
       (values first 1 second)))))

(defun rplaca-window-tree-layout (tree rows cols)
  "Return layout entries and separators for TREE over ROWS by COLS cells."
  (declare (type integer rows cols))
  (labels ((walk (node row col height width)
             (cond
               ((or (null node) (<= height 0) (<= width 0))
                (values nil nil))
               ((rplaca-window-node-leaf-p node)
                (values (list (make-rplaca-window-layout-entry
                               :window (rplaca-window-node-window node)
                               :row row
                               :col col
                               :rows height
                               :cols width))
                        nil))
               ((eq (rplaca-window-node-orientation node) :horizontal)
                (multiple-value-bind (first-width sep-width second-width)
                    (split-window-space width)
                  (multiple-value-bind (first-entries first-separators)
                      (walk (rplaca-window-node-first node)
                            row col height first-width)
                    (multiple-value-bind (second-entries second-separators)
                        (walk (rplaca-window-node-second node)
                              row
                              (+ col first-width sep-width)
                              height
                              second-width)
                      (values (append first-entries second-entries)
                              (append first-separators
                                      (when (plusp sep-width)
                                        (list
                                         (make-rplaca-window-separator
                                          :orientation :vertical
                                          :row row
                                          :col (+ col first-width)
                                          :rows height
                                          :cols sep-width)))
                                      second-separators))))))
               ((eq (rplaca-window-node-orientation node) :vertical)
                (multiple-value-bind (first-height sep-height second-height)
                    (split-window-space height)
                  (multiple-value-bind (first-entries first-separators)
                      (walk (rplaca-window-node-first node)
                            row col first-height width)
                    (multiple-value-bind (second-entries second-separators)
                        (walk (rplaca-window-node-second node)
                              (+ row first-height sep-height)
                              col
                              second-height
                              width)
                      (values (append first-entries second-entries)
                              (append first-separators
                                      (when (plusp sep-height)
                                        (list
                                         (make-rplaca-window-separator
                                          :orientation :horizontal
                                          :row (+ row first-height)
                                          :col col
                                          :rows sep-height
                                          :cols width)))
                                      second-separators))))))
               (t
                (values nil nil)))))
    (walk tree 0 0 rows cols)))
