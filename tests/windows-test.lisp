(in-package :clawmacs/tests)

(in-suite windows-suite)

(defun make-test-window-buffer (name)
  (make-buffer name :agent-name "test"))

(test logical-window-tree-splits-and-cycles
  "Logical windows split in display order and cycle like Emacs windows."
  (let* ((buf (make-test-window-buffer "window-a"))
         (tree (make-clawmacs-window-tree buf))
         (first (first (clawmacs-window-tree-windows tree)))
         (second (split-clawmacs-window-tree
                  tree (clawmacs-window-id first) :horizontal)))
    (is (= 2 (clawmacs-window-tree-count tree)))
    (is (eq buf (clawmacs-window-buffer second)))
    (is (eq second
            (clawmacs-window-tree-next-window
             tree (clawmacs-window-id first))))
    (is (eq first
            (clawmacs-window-tree-next-window
             tree (clawmacs-window-id second))))))

(test logical-window-tree-delete-collapses-splits
  "Deleting a logical window collapses its parent split and selects a neighbor."
  (let* ((buf (make-test-window-buffer "window-b"))
         (tree (make-clawmacs-window-tree buf))
         (first (first (clawmacs-window-tree-windows tree)))
         (second (split-clawmacs-window-tree
                  tree (clawmacs-window-id first) :vertical)))
    (multiple-value-bind (new-tree selected deleted-p)
        (delete-clawmacs-window-from-tree tree (clawmacs-window-id first))
      (is (not (null deleted-p)))
      (is (eq second selected))
      (is (= 1 (clawmacs-window-tree-count new-tree)))
      (is (eq second (first (clawmacs-window-tree-windows new-tree)))))))

(test logical-window-tree-keeps-final-window
  "Deleting the only logical window is a no-op."
  (let* ((buf (make-test-window-buffer "window-c"))
         (tree (make-clawmacs-window-tree buf))
         (first (first (clawmacs-window-tree-windows tree))))
    (multiple-value-bind (new-tree selected deleted-p)
        (delete-clawmacs-window-from-tree tree (clawmacs-window-id first))
      (is (not deleted-p))
      (is (eq tree new-tree))
      (is (eq first selected)))))

(test logical-window-tree-delete-other-windows
  "Deleting other windows preserves the selected window object."
  (let* ((buf (make-test-window-buffer "window-d"))
         (tree (make-clawmacs-window-tree buf))
         (first (first (clawmacs-window-tree-windows tree)))
         (second (split-clawmacs-window-tree
                  tree (clawmacs-window-id first) :horizontal)))
    (multiple-value-bind (new-tree selected deleted-p)
        (delete-other-clawmacs-windows tree (clawmacs-window-id second))
      (is (not (null deleted-p)))
      (is (eq second selected))
      (is (= 1 (clawmacs-window-tree-count new-tree)))
      (is (eq second (first (clawmacs-window-tree-windows new-tree)))))))

(test logical-window-tree-replaces-dead-buffers
  "Window buffers that leave the ring can be redirected to a live fallback."
  (let* ((live (make-test-window-buffer "window-live"))
         (dead (make-test-window-buffer "window-dead"))
         (tree (make-clawmacs-window-tree dead))
         (window (first (clawmacs-window-tree-windows tree))))
    (is (clawmacs-window-tree-replace-dead-buffers tree (list live) live))
    (is (eq live (clawmacs-window-buffer window)))
    (is (not (clawmacs-window-tree-replace-dead-buffers
              tree (list live) live)))))

(test logical-window-tree-layout-reserves-separators
  "Tree layout returns CLIM grid rectangles and separator rectangles."
  (let* ((buf (make-test-window-buffer "window-layout"))
         (tree (make-clawmacs-window-tree buf))
         (first (first (clawmacs-window-tree-windows tree))))
    (split-clawmacs-window-tree tree (clawmacs-window-id first) :horizontal)
    (multiple-value-bind (entries separators)
        (clawmacs-window-tree-layout tree 20 81)
      (is (= 2 (length entries)))
      (is (= 1 (length separators)))
      (is (= 40 (clawmacs-window-layout-entry-cols (first entries))))
      (is (= 40 (clawmacs-window-layout-entry-cols (second entries))))
      (is (eq :vertical
              (clawmacs-window-separator-orientation
               (first separators)))))))
