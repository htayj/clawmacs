(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Logical Windows
;;; --------------------------------------------------------------------------

(defvar *clawmacs-window-counter* 0
  "Counter used to assign process-local logical window ids.")

(defun next-clawmacs-window-id ()
  "Return a fresh process-local logical window id."
  (incf *clawmacs-window-counter*))

(defclass clawmacs-window ()
  ((id :initarg :id
       :reader clawmacs-window-id
       :type integer
       :documentation "Stable process-local id for this logical window.")
   (buffer :initarg :buffer
           :accessor clawmacs-window-buffer
           :type (or null buffer)
           :documentation "Buffer displayed by this logical window."))
  (:documentation
   "An Emacs-style logical window.

Clawmacs windows are not CLIM sheets.  The McCLIM frame owns one transcript
pane and renders these logical windows into sub-regions of that pane, keeping
CLIM pane layout simple while exposing Emacs-like split/delete/window cycling
semantics."))

(defun make-clawmacs-window (&key buffer id)
  "Return a logical window displaying BUFFER."
  (make-instance 'clawmacs-window
    :id (or id (next-clawmacs-window-id))
    :buffer buffer))

(defstruct (clawmacs-window-node
            (:constructor %make-clawmacs-window-node
                (&key kind window orientation first second)))
  "A node in the logical window tree.

KIND is :LEAF or :SPLIT.  Leaf nodes hold WINDOW.  Split nodes hold
ORIENTATION plus FIRST and SECOND child nodes.  ORIENTATION is :VERTICAL for a
top/bottom split and :HORIZONTAL for a left/right split, matching CLIM's
VERTICALLY/HORIZONTALLY layout terminology."
  kind
  window
  orientation
  first
  second)

(defun make-clawmacs-window-leaf (window)
  "Return a leaf node for WINDOW."
  (%make-clawmacs-window-node :kind :leaf :window window))

(defun make-clawmacs-window-split (orientation first second)
  "Return a split node with ORIENTATION, FIRST child, and SECOND child."
  (unless (member orientation '(:vertical :horizontal) :test #'eq)
    (error "Unknown window split orientation: ~S" orientation))
  (%make-clawmacs-window-node :kind :split
                              :orientation orientation
                              :first first
                              :second second))

(defun make-clawmacs-window-tree (buffer)
  "Return a new window tree containing one logical window for BUFFER."
  (make-clawmacs-window-leaf
   (make-clawmacs-window :buffer buffer)))

(defun clawmacs-window-node-leaf-p (node)
  "Return true when NODE is a leaf node."
  (and node (eq (clawmacs-window-node-kind node) :leaf)))

(defun clawmacs-window-node-split-p (node)
  "Return true when NODE is a split node."
  (and node (eq (clawmacs-window-node-kind node) :split)))

(defun clawmacs-window-tree-windows (tree)
  "Return TREE's logical windows in display order."
  (labels ((walk (node)
             (cond
               ((null node) nil)
               ((clawmacs-window-node-leaf-p node)
                (list (clawmacs-window-node-window node)))
               ((clawmacs-window-node-split-p node)
                (append (walk (clawmacs-window-node-first node))
                        (walk (clawmacs-window-node-second node))))
               (t nil))))
    (remove nil (walk tree))))

(defun clawmacs-window-tree-count (tree)
  "Return the number of logical windows in TREE."
  (length (clawmacs-window-tree-windows tree)))

(defun clawmacs-window-tree-find-window (tree window-id)
  "Return the logical window with WINDOW-ID in TREE, or NIL."
  (find window-id (clawmacs-window-tree-windows tree)
        :key #'clawmacs-window-id
        :test #'eql))

(defun clawmacs-window-tree-find-node (tree window-id)
  "Return the leaf node for WINDOW-ID in TREE, or NIL."
  (labels ((walk (node)
             (cond
               ((null node) nil)
               ((clawmacs-window-node-leaf-p node)
                (and (eql window-id
                          (clawmacs-window-id
                           (clawmacs-window-node-window node)))
                     node))
               ((clawmacs-window-node-split-p node)
                (or (walk (clawmacs-window-node-first node))
                    (walk (clawmacs-window-node-second node))))
               (t nil))))
    (walk tree)))

(defun split-clawmacs-window-tree (tree window-id orientation)
  "Split WINDOW-ID in TREE and return the newly created logical window.

ORIENTATION is :VERTICAL for split-window-below behavior and :HORIZONTAL for
split-window-right behavior.  The existing window keeps its buffer; the new
window starts by displaying the same buffer."
  (let ((node (clawmacs-window-tree-find-node tree window-id)))
    (when node
      (let* ((old-window (clawmacs-window-node-window node))
             (new-window (make-clawmacs-window
                          :buffer (clawmacs-window-buffer old-window))))
        (setf (clawmacs-window-node-kind node) :split
              (clawmacs-window-node-window node) nil
              (clawmacs-window-node-orientation node) orientation
              (clawmacs-window-node-first node)
              (make-clawmacs-window-leaf old-window)
              (clawmacs-window-node-second node)
              (make-clawmacs-window-leaf new-window))
        new-window))))

(defun clawmacs-window-tree-next-window (tree window-id &key reverse)
  "Return the next logical window after WINDOW-ID in display order."
  (let* ((windows (clawmacs-window-tree-windows tree))
         (count (length windows))
         (index (position window-id windows
                          :key #'clawmacs-window-id
                          :test #'eql)))
    (when (and index (plusp count))
      (nth (mod (+ index (if reverse -1 1)) count) windows))))

(defun delete-clawmacs-window-from-tree (tree window-id)
  "Delete WINDOW-ID from TREE.

Returns three values: the new tree root, the replacement selected window, and a
boolean indicating whether a window was deleted.  The final remaining window is
never deleted."
  (let ((windows (clawmacs-window-tree-windows tree)))
    (cond
      ((or (null (cdr windows))
           (null (find window-id windows
                       :key #'clawmacs-window-id
                       :test #'eql)))
       (values tree
               (or (first windows)
                   (and tree
                        (clawmacs-window-node-leaf-p tree)
                        (clawmacs-window-node-window tree)))
               nil))
      (t
       (let ((replacement (or (clawmacs-window-tree-next-window tree window-id)
                              (first windows))))
         (labels ((walk (node)
                    (cond
                      ((null node) (values nil nil))
                      ((clawmacs-window-node-leaf-p node)
                       (if (eql window-id
                                (clawmacs-window-id
                                 (clawmacs-window-node-window node)))
                           (values nil t)
                           (values node nil)))
                      ((clawmacs-window-node-split-p node)
                       (multiple-value-bind (new-first deleted-first-p)
                           (walk (clawmacs-window-node-first node))
                         (multiple-value-bind (new-second deleted-second-p)
                             (walk (clawmacs-window-node-second node))
                           (cond
                             ((and deleted-first-p (null new-first))
                              (values new-second t))
                             ((and deleted-second-p (null new-second))
                              (values new-first t))
                             ((or deleted-first-p deleted-second-p)
                              (setf (clawmacs-window-node-first node) new-first
                                    (clawmacs-window-node-second node) new-second)
                              (values node t))
                             (t
                              (values node nil)))))))))
           (multiple-value-bind (new-root deleted-p)
               (walk tree)
             (values new-root replacement deleted-p))))))))

(defun delete-other-clawmacs-windows (tree window-id)
  "Return a new tree containing only WINDOW-ID, preserving that window object."
  (let ((window (clawmacs-window-tree-find-window tree window-id)))
    (if window
        (values (make-clawmacs-window-leaf window) window t)
        (values tree nil nil))))

(defun clawmacs-window-tree-replace-dead-buffers (tree live-buffers fallback)
  "Replace window buffers not in LIVE-BUFFERS with FALLBACK.

Returns true when any window buffer was changed."
  (let ((changed-p nil))
    (dolist (window (clawmacs-window-tree-windows tree) changed-p)
      (unless (member (clawmacs-window-buffer window) live-buffers :test #'eq)
        (setf (clawmacs-window-buffer window) fallback
              changed-p t)))))

;;; --------------------------------------------------------------------------
;;; Window Layout
;;; --------------------------------------------------------------------------

(defstruct clawmacs-window-layout-entry
  "A rendered grid rectangle for one logical window."
  window
  row
  col
  rows
  cols)

(defstruct clawmacs-window-separator
  "A grid rectangle used as visual separation between logical windows."
  orientation
  row
  col
  rows
  cols)

(defun split-window-space (size)
  "Return FIRST, SEPARATOR, and SECOND sizes for splitting SIZE cells."
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

(defun clawmacs-window-tree-layout (tree rows cols)
  "Return layout entries and separators for TREE over ROWS by COLS cells."
  (labels ((walk (node row col height width)
             (cond
               ((or (null node) (<= height 0) (<= width 0))
                (values nil nil))
               ((clawmacs-window-node-leaf-p node)
                (values (list (make-clawmacs-window-layout-entry
                               :window (clawmacs-window-node-window node)
                               :row row
                               :col col
                               :rows height
                               :cols width))
                        nil))
               ((eq (clawmacs-window-node-orientation node) :horizontal)
                (multiple-value-bind (first-width sep-width second-width)
                    (split-window-space width)
                  (multiple-value-bind (first-entries first-separators)
                      (walk (clawmacs-window-node-first node)
                            row col height first-width)
                    (multiple-value-bind (second-entries second-separators)
                        (walk (clawmacs-window-node-second node)
                              row
                              (+ col first-width sep-width)
                              height
                              second-width)
                      (values (append first-entries second-entries)
                              (append first-separators
                                      (when (plusp sep-width)
                                        (list
                                         (make-clawmacs-window-separator
                                          :orientation :vertical
                                          :row row
                                          :col (+ col first-width)
                                          :rows height
                                          :cols sep-width)))
                                      second-separators))))))
               ((eq (clawmacs-window-node-orientation node) :vertical)
                (multiple-value-bind (first-height sep-height second-height)
                    (split-window-space height)
                  (multiple-value-bind (first-entries first-separators)
                      (walk (clawmacs-window-node-first node)
                            row col first-height width)
                    (multiple-value-bind (second-entries second-separators)
                        (walk (clawmacs-window-node-second node)
                              (+ row first-height sep-height)
                              col
                              second-height
                              width)
                      (values (append first-entries second-entries)
                              (append first-separators
                                      (when (plusp sep-height)
                                        (list
                                         (make-clawmacs-window-separator
                                          :orientation :horizontal
                                          :row (+ row first-height)
                                          :col col
                                          :rows sep-height
                                          :cols width)))
                                      second-separators))))))
               (t
                (values nil nil)))))
    (walk tree 0 0 rows cols)))
