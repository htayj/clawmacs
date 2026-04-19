(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Selector State
;;; --------------------------------------------------------------------------

(defvar *buffer-selector-active* nil
  "When non-nil, the buffer selector overlay is displayed.")

(defvar *buffer-selector-index* 0
  "The currently highlighted index in the buffer selector.")

(defvar *buffer-selector-scroll* 0
  "Scroll offset for the buffer selector (first visible entry index).")

(defvar *model-selector-active* nil
  "When non-nil, the model selector overlay is displayed.")

(defvar *model-selector-index* 0
  "The currently highlighted index in the model selector.")

(defvar *model-selector-scroll* 0
  "Scroll offset for the model selector (first visible entry index).")

(defvar *model-selector-entries* nil
  "List of model entries for the selector, each a plist
(:provider :keyword :model \"string\" :active-p bool).")

(defvar *think-selector-active* nil
  "When non-nil, the think-level selector overlay is displayed.")

(defvar *think-selector-index* 0
  "The currently highlighted index in the think-level selector.")

(defvar *think-selector-scroll* 0
  "Scroll offset for the think-level selector (first visible entry index).")

(defvar *think-selector-entries* nil
  "List of think-level entries for the selector, each a plist
(:provider :keyword :model \"string\" :level (or null string) :active-p bool).")

(defvar *session-tree-selector-active* nil
  "When non-nil, the session tree selector overlay is displayed.")

(defvar *session-tree-selector-buffer* nil
  "Buffer whose session tree is currently being selected.")

(defvar *session-tree-selector-items* nil
  "All flattened session tree selector items before filtering.")

(defvar *session-tree-selector-filtered-items* nil
  "Filtered flattened session tree selector items.")

(defvar *session-tree-selector-index* 0
  "The currently highlighted index in the session tree selector.")

(defvar *session-tree-selector-scroll* 0
  "Scroll offset for the session tree selector.")

(defvar *session-tree-selector-search* ""
  "Search text for the session tree selector.")

(defvar *session-tree-selector-filter-mode* :default
  "Session tree selector filter mode.")

(defvar *session-tree-selector-folded-ids* nil
  "Entry ids whose descendants are hidden in the session tree selector.")

(defvar *session-tree-selector-callback* nil
  "Function called with the selected session tree item.")

(defvar *session-tree-selector-label-callback* nil
  "Function called with a session tree item when the user edits its label.")

;;; --------------------------------------------------------------------------
;;; Minibuffer State
;;; --------------------------------------------------------------------------

(defvar *minibuffer-active* nil
  "When non-nil, the minibuffer is active and the cursor is in it.")

(defvar *minibuffer-mode* :completion
  "The current minibuffer mode: :COMPLETION or :PROMPT.")

(defvar *minibuffer-prompt* ""
  "The prompt string displayed in the minibuffer (e.g. \"Select Model\").")

(defvar *minibuffer-input* ""
  "The current input text in the minibuffer.")

(defvar *minibuffer-point* 0
  "Cursor position within *minibuffer-input*.")

(defvar *minibuffer-items* nil
  "Complete list of candidate items for the minibuffer completion.
Each item is a plist with at least a :display key.")

(defvar *minibuffer-filtered-items* nil
  "Candidates after fuzzy-filtering by *minibuffer-input*.
Subset of *minibuffer-items*.")

(defvar *minibuffer-selected-index* 0
  "Index of the currently selected candidate in *minibuffer-filtered-items*.")

(defvar *minibuffer-callback* nil
  "Function called with the selected item plist when the user confirms.
Set to nil when inactive.")

(defvar *minibuffer-max-height* 12
  "Maximum number of rows the minibuffer can expand to (including prompt).")

(defvar *minibuffer-scroll-offset* 0
  "Index of the first visible candidate in the minibuffer.
When the selected item moves beyond the visible window, this offset
is adjusted so that the selection is always visible.")

(defvar *minibuffer-match-positions* nil
  "List of match-position lists parallel to *minibuffer-filtered-items*.
Each element is a sorted list of character indices (into the corresponding
item's display string) that were matched by the current query.
NIL entries mean no query is active or no positions were recorded.")

(defvar *model-selection-history* nil
  "List of recently selected model display strings (most recent first).
Used for recency sorting in the minibuffer model selector.")

(defvar *buffer-selection-history* nil
  "List of recently selected buffer names (most recent first).
Used for recency sorting in the minibuffer buffer selector.")

;;; --------------------------------------------------------------------------
;;; Minibuffer Functions
;;; --------------------------------------------------------------------------

(defun minibuffer-item-display (item)
  "Get the display string for a minibuffer candidate item.
If ITEM is a string, returns it directly. Otherwise returns the :display plist value."
  (if (stringp item)
      item
      (or (getf item :display) "")))

(defun minibuffer-item-match-text (item)
  "Return the text used to fuzzy-match ITEM in the minibuffer."
  (if (stringp item)
      item
      (or (getf item :match-text)
          (minibuffer-item-display item))))

(defun minibuffer-activate (prompt items callback
                            &key (mode :completion) (initial-input ""))
  "Activate the minibuffer with PROMPT text, a list of candidate ITEMS,
and a CALLBACK function to call on confirmation."
  (deactivate-skill-completion)
  (setf *minibuffer-active* t
        *minibuffer-mode* mode
        *minibuffer-prompt* prompt
        *minibuffer-input* initial-input
        *minibuffer-point* (length initial-input)
        *minibuffer-items* items
        *minibuffer-filtered-items* nil
        *minibuffer-match-positions* nil
        *minibuffer-selected-index* 0
        *minibuffer-scroll-offset* 0
        *minibuffer-callback* callback)
  (minibuffer-update-filter))

(defun minibuffer-prompt (prompt callback &key (initial-input ""))
  "Activate the minibuffer in prompt mode and submit raw input to CALLBACK."
  (minibuffer-activate prompt nil callback
                       :mode :prompt
                       :initial-input initial-input))

(defun minibuffer-deactivate ()
  "Deactivate the minibuffer, clearing all state."
  (setf *minibuffer-active* nil
        *minibuffer-mode* :completion
        *minibuffer-prompt* ""
        *minibuffer-input* ""
        *minibuffer-point* 0
        *minibuffer-items* nil
        *minibuffer-filtered-items* nil
        *minibuffer-match-positions* nil
        *minibuffer-selected-index* 0
        *minibuffer-scroll-offset* 0
        *minibuffer-callback* nil))

(defun minibuffer-update-filter ()
  "Re-filter *minibuffer-items* based on *minibuffer-input*.
When a non-empty query is present the matching candidates are scored and
sorted by relevance (highest score first) so the best match floats to the
top automatically.  Matched character positions are precomputed and stored in
*minibuffer-match-positions* for the renderer to use for highlighting.
Clamps *minibuffer-selected-index* to the new filtered list length and
resets the scroll offset."
  (let ((query *minibuffer-input*))
    (cond
      ((eq *minibuffer-mode* :prompt)
       (setf *minibuffer-filtered-items* nil
             *minibuffer-match-positions* nil))
      ((zerop (length query))
       ;; No query — show all items in their original order, no highlights.
       (setf *minibuffer-filtered-items* (copy-list *minibuffer-items*)
             *minibuffer-match-positions* (make-list (length *minibuffer-items*)
                                                     :initial-element nil)))
      (t
       ;; Query present — filter, score, sort, record positions.
       (let* ((matched (remove-if-not
                        (lambda (item)
                          (fuzzy-match-p query (minibuffer-item-match-text item)))
                        *minibuffer-items*))
              ;; Pair each item with its relevance score.
              (scored (mapcar (lambda (item)
                                (cons (or (fuzzy-score query
                                                       (minibuffer-item-match-text item))
                                          0)
                                      item))
                              matched))
              ;; Sort descending by score (stable-sort preserves original order
              ;; for items that score equally).
              (sorted (stable-sort scored #'> :key #'car))
              (sorted-items (mapcar #'cdr sorted)))
         (setf *minibuffer-filtered-items* sorted-items
               *minibuffer-match-positions*
               (mapcar (lambda (item)
                         (fuzzy-match-positions query
                                                (minibuffer-item-match-text item)))
                       sorted-items))))))
  ;; Clamp selected index to valid range.
  (setf *minibuffer-selected-index*
        (max 0 (min *minibuffer-selected-index*
                    (1- (max 1 (length *minibuffer-filtered-items*))))))
  ;; Reset scroll and ensure the selection is visible.
  (setf *minibuffer-scroll-offset* 0)
  (minibuffer-ensure-visible))

(defun minibuffer-insert-char (char)
  "Insert CHAR at the current point in the minibuffer input and re-filter."
  (setf *minibuffer-input*
        (concatenate 'string
                     (subseq *minibuffer-input* 0 *minibuffer-point*)
                     (string char)
                     (subseq *minibuffer-input* *minibuffer-point*)))
  (incf *minibuffer-point*)
  (minibuffer-update-filter))

(defun minibuffer-delete-backward ()
  "Delete the character before point in the minibuffer input and re-filter."
  (when (plusp *minibuffer-point*)
    (setf *minibuffer-input*
          (concatenate 'string
                       (subseq *minibuffer-input* 0 (1- *minibuffer-point*))
                       (subseq *minibuffer-input* *minibuffer-point*)))
    (decf *minibuffer-point*)
    (minibuffer-update-filter)))

(defun minibuffer-visible-item-count ()
  "Return the number of candidate rows visible in the minibuffer.
This is the total minibuffer height minus 1 (for the prompt line)."
  (1- (min *minibuffer-max-height*
           (1+ (length *minibuffer-filtered-items*)))))

(defun minibuffer-ensure-visible ()
  "Adjust *minibuffer-scroll-offset* so that *minibuffer-selected-index*
is within the visible window of candidates."
  (let ((visible (minibuffer-visible-item-count)))
    (when (plusp visible)
      ;; If selection is above the visible window, scroll up
      (when (< *minibuffer-selected-index* *minibuffer-scroll-offset*)
        (setf *minibuffer-scroll-offset* *minibuffer-selected-index*))
      ;; If selection is below the visible window, scroll down
      (when (>= *minibuffer-selected-index*
                (+ *minibuffer-scroll-offset* visible))
        (setf *minibuffer-scroll-offset*
              (1+ (- *minibuffer-selected-index* visible)))))))

(defun minibuffer-next-item ()
  "Move the selection to the next candidate in the filtered list."
  (when (< *minibuffer-selected-index*
           (1- (length *minibuffer-filtered-items*)))
    (incf *minibuffer-selected-index*)
    (minibuffer-ensure-visible)))

(defun minibuffer-prev-item ()
  "Move the selection to the previous candidate in the filtered list."
  (when (plusp *minibuffer-selected-index*)
    (decf *minibuffer-selected-index*)
    (minibuffer-ensure-visible)))

(defun minibuffer-confirm ()
  "Confirm the current selection, invoke the callback, and deactivate."
  (let ((item (when (plusp (length *minibuffer-filtered-items*))
                (nth *minibuffer-selected-index* *minibuffer-filtered-items*)))
        (mode *minibuffer-mode*)
        (input *minibuffer-input*)
        (cb *minibuffer-callback*))
    (minibuffer-deactivate)
    (when cb
      (case mode
        (:prompt
         (funcall cb input))
        (t
         (when item
           (funcall cb item)))))))

(defun minibuffer-cancel ()
  "Cancel the minibuffer without invoking the callback."
  (minibuffer-deactivate))

(defun sort-models-by-recency (items)
  "Sort model ITEMS by recency (from *model-selection-history*) then alphabetically.
Items that were selected more recently appear first. Items not in the history
are sorted with the currently active model first, then alphabetically."
  (stable-sort (copy-list items)
               (lambda (a b)
                 (let* ((a-disp (getf a :display))
                        (b-disp (getf b :display))
                        (history *model-selection-history*)
                        (a-pos (position a-disp history :test #'string=))
                        (b-pos (position b-disp history :test #'string=)))
                   (cond
                     ;; Both in history: lower position (more recent) first
                     ((and a-pos b-pos) (< a-pos b-pos))
                     ;; Only a in history: a first
                     (a-pos t)
                     ;; Only b in history: b first
                     (b-pos nil)
                     ;; Neither: active model first, then alphabetical
                     ((and (getf a :active-p) (not (getf b :active-p))) t)
                     ((and (getf b :active-p) (not (getf a :active-p))) nil)
                     (t (string< a-disp b-disp)))))))

(defun sort-buffers-by-recency (items)
  "Sort buffer ITEMS by recency (from *buffer-selection-history*) then alphabetically.
Items that were selected more recently appear first. Items not in the history
are sorted with the current buffer first, then alphabetically."
  (stable-sort (copy-list items)
               (lambda (a b)
                 (let* ((a-disp (getf a :display))
                        (b-disp (getf b :display))
                        (history *buffer-selection-history*)
                        (a-pos (position a-disp history :test #'string=))
                        (b-pos (position b-disp history :test #'string=)))
                   (cond
                     ;; Both in history: lower position (more recent) first
                     ((and a-pos b-pos) (< a-pos b-pos))
                     ;; Only a in history: a first
                     (a-pos t)
                     ;; Only b in history: b first
                     (b-pos nil)
                     ;; Neither: current buffer first, then alphabetical
                     ((and (getf a :current-p) (not (getf b :current-p))) t)
                     ((and (getf b :current-p) (not (getf a :current-p))) nil)
                     (t (string< a-disp b-disp)))))))

;;; --------------------------------------------------------------------------
;;; Session Tree Selector
;;; --------------------------------------------------------------------------

(defun session-tree-blank-string-p (value)
  "Return true when VALUE is NIL or contains only whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (string value))))))

(defun session-tree-normalize-one-line (text &key (limit 96))
  "Return TEXT as a compact one-line selector snippet."
  (let* ((source (or text ""))
         (normalized
           (with-output-to-string (out)
             (let ((space-p nil))
               (loop :for char :across source
                     :do (if (find char '(#\Space #\Tab #\Newline #\Return)
                                   :test #'char=)
                             (unless space-p
                               (write-char #\Space out)
                               (setf space-p t))
                             (progn
                               (write-char char out)
                               (setf space-p nil)))))))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                               normalized)))
    (if (> (length trimmed) limit)
        (concatenate 'string (subseq trimmed 0 (max 0 (- limit 3))) "...")
        trimmed)))

(defun session-tree-event-sender (event)
  "Return EVENT's sender string, if any."
  (or (session-alist-value event :sender) ""))

(defun session-tree-event-kind-label (event)
  "Return a compact kind label for EVENT."
  (let ((kind (session-event-kind event)))
    (cond
      ((string= kind "message")
       (string-downcase (session-tree-event-sender event)))
      ((string= kind "branch-summary") "branch")
      ((string= kind "model-change") "model")
      ((string= kind "think-level-change") "think")
      ((string= kind "session-info") "session")
      (t kind))))

(defun session-tree-event-display-text (event)
  "Return EVENT's selector display body."
  (let ((kind (session-event-kind event)))
    (cond
      ((string= kind "message")
       (let ((text (session-tree-normalize-one-line
                    (session-alist-value event :text))))
         (if (session-tree-blank-string-p text)
             "[empty]"
             text)))
      ((string= kind "branch-summary")
       (session-tree-normalize-one-line
        (session-alist-value event :summary)))
      ((string= kind "compaction")
       (format nil "compaction: ~A"
               (or (session-alist-value event :reason) "manual")))
      ((string= kind "model-change")
       (format nil "~A/~A"
               (or (session-alist-value event :provider) "provider")
               (or (session-alist-value event :model) "model")))
      ((string= kind "think-level-change")
       (format nil "think ~A"
               (or (session-alist-value event :think-level) "default")))
      ((string= kind "label")
       (format nil "label ~A"
               (or (session-alist-value event :label) "(clear)")))
      (t
       (session-tree-normalize-one-line
        (or (session-alist-value event :summary)
            (session-alist-value event :text)
            kind))))))

(defun session-tree-active-id-p (entry-id active-ids)
  "Return true when ENTRY-ID is on ACTIVE-IDS."
  (and entry-id (member entry-id active-ids :test #'string=)))

(defun session-tree-sort-nodes-for-selector (nodes active-ids)
  "Return NODES with active branch nodes before side branches."
  (stable-sort
   (copy-list nodes)
   (lambda (a b)
     (let* ((a-id (session-event-id (session-tree-node-entry a)))
            (b-id (session-event-id (session-tree-node-entry b)))
            (a-active (session-tree-active-id-p a-id active-ids))
            (b-active (session-tree-active-id-p b-id active-ids)))
       (cond
         ((and a-active (not b-active)) t)
         ((and b-active (not a-active)) nil)
         (t (< (session-event-timestamp (session-tree-node-entry a))
               (session-event-timestamp (session-tree-node-entry b)))))))))

(defun session-tree-selector-flatten-nodes (nodes active-ids)
  "Flatten NODES into selector item plists."
  (labels ((walk (node depth prefix last-p ancestors)
             (let* ((event (session-tree-node-entry node))
                    (id (session-event-id event))
                    (children (session-tree-sort-nodes-for-selector
                               (session-tree-node-children node)
                               active-ids))
                    (connector (cond
                                 ((zerop depth) "")
                                 (last-p "`- ")
                                 (t "|- ")))
                    (tree-prefix (concatenate 'string prefix connector))
                    (next-prefix (concatenate 'string
                                             prefix
                                             (cond
                                               ((zerop depth) "")
                                               (last-p "   ")
                                               (t "|  "))))
                    (kind (session-event-kind event))
                    (kind-label (session-tree-event-kind-label event))
                    (label (session-tree-node-label node))
                    (content (session-tree-event-display-text event))
                    (active-p (session-tree-active-id-p id active-ids))
                    (match-text (format nil "~A ~A ~A ~A"
                                        id kind-label (or label "") content))
                    (item (list :id id
                                :parent-id (session-event-parent-id event)
                                :event event
                                :kind kind
                                :kind-label kind-label
                                :sender (session-tree-event-sender event)
                                :label label
                                :content content
                                :tree-prefix tree-prefix
                                :depth depth
                                :active-p active-p
                                :children-count (length children)
                                :ancestor-ids ancestors
                                :match-text match-text))
                    (items (list item)))
               (loop :for child :in children
                     :for index :from 0
                     :for child-last-p := (= index (1- (length children)))
                     :do (setf items
                               (append items
                                       (walk child
                                             (1+ depth)
                                             next-prefix
                                             child-last-p
                                             (cons id ancestors)))))
               items)))
    (let ((ordered-roots (session-tree-sort-nodes-for-selector nodes active-ids)))
      (loop :for node :in ordered-roots
            :for index :from 0
            :for last-p := (= index (1- (length ordered-roots)))
            :append (walk node 0 "" last-p nil)))))

(defun session-tree-selector-build-items (buffer)
  "Return flattened selector items for BUFFER's session tree."
  (let ((session (and buffer (buffer-session buffer))))
    (when session
      (session-tree-selector-flatten-nodes
       (session-tree-roots session)
       (session-active-path-ids session)))))

(defun session-tree-selector-filter-kind-p (item)
  "Return true when ITEM passes the active kind filter."
  (let ((kind (getf item :kind))
        (sender (getf item :sender)))
    (case *session-tree-selector-filter-mode*
      (:user-only
       (and (string= kind "message")
            (string= sender "USER")))
      (:no-tools
       (and (not (member kind
                         '("label" "model-change" "think-level-change"
                           "session-info")
                         :test #'string=))
            (not (string= sender "TOOL-RESULT"))))
      (:labeled-only
       (not (null (getf item :label))))
      (:all t)
      (otherwise
       (not (member kind
                    '("label" "model-change" "think-level-change"
                      "session-info")
                    :test #'string=))))))

(defun session-tree-selector-folded-hidden-p (item)
  "Return true when ITEM is hidden by a folded ancestor."
  (some (lambda (ancestor-id)
          (member ancestor-id *session-tree-selector-folded-ids*
                  :test #'string=))
        (getf item :ancestor-ids)))

(defun session-tree-selector-search-match-p (item)
  "Return true when ITEM matches the active session tree search."
  (or (session-tree-blank-string-p *session-tree-selector-search*)
      (fuzzy-match-p *session-tree-selector-search*
                     (getf item :match-text))))

(defun session-tree-selector-update-filter ()
  "Refresh session tree selector items and filtered view."
  (setf *session-tree-selector-items*
        (session-tree-selector-build-items *session-tree-selector-buffer*))
  (setf *session-tree-selector-filtered-items*
        (remove-if-not
         (lambda (item)
           (and (session-tree-selector-filter-kind-p item)
                (not (session-tree-selector-folded-hidden-p item))
                (session-tree-selector-search-match-p item)))
         *session-tree-selector-items*))
  (dolist (item *session-tree-selector-filtered-items*)
    (setf (getf item :folded-p)
          (member (getf item :id) *session-tree-selector-folded-ids*
                  :test #'string=)))
  (setf *session-tree-selector-index*
        (max 0 (min *session-tree-selector-index*
                    (1- (max 1
                             (length *session-tree-selector-filtered-items*))))))
  *session-tree-selector-filtered-items*)

(defun session-tree-selector-current-item ()
  "Return the currently selected tree item, if any."
  (when (and *session-tree-selector-filtered-items*
             (<= 0 *session-tree-selector-index*)
             (< *session-tree-selector-index*
                (length *session-tree-selector-filtered-items*)))
    (nth *session-tree-selector-index*
         *session-tree-selector-filtered-items*)))

(defun session-tree-selector-preselect (entry-id)
  "Move selection to ENTRY-ID when it is visible."
  (let ((index (and entry-id
                    (position entry-id *session-tree-selector-filtered-items*
                              :key (lambda (item) (getf item :id))
                              :test #'string=))))
    (when index
      (setf *session-tree-selector-index* index
            *session-tree-selector-scroll* (max 0 (- index 5))))))

(defun session-tree-selector-activate
    (buffer callback &key label-callback initial-entry-id)
  "Activate the session tree selector for BUFFER."
  (setf *session-tree-selector-active* t
        *session-tree-selector-buffer* buffer
        *session-tree-selector-index* 0
        *session-tree-selector-scroll* 0
        *session-tree-selector-search* ""
        *session-tree-selector-filter-mode* :default
        *session-tree-selector-folded-ids* nil
        *session-tree-selector-callback* callback
        *session-tree-selector-label-callback* label-callback)
  (session-tree-selector-update-filter)
  (session-tree-selector-preselect
   (or initial-entry-id
       (and (buffer-session buffer)
            (session-effective-leaf-id (buffer-session buffer)))))
  (session-tree-selector-update-filter))

(defun session-tree-selector-deactivate ()
  "Deactivate the session tree selector."
  (setf *session-tree-selector-active* nil
        *session-tree-selector-buffer* nil
        *session-tree-selector-items* nil
        *session-tree-selector-filtered-items* nil
        *session-tree-selector-index* 0
        *session-tree-selector-scroll* 0
        *session-tree-selector-search* ""
        *session-tree-selector-filter-mode* :default
        *session-tree-selector-folded-ids* nil
        *session-tree-selector-callback* nil
        *session-tree-selector-label-callback* nil))

(defun session-tree-selector-cycle-filter ()
  "Cycle the session tree selector filter mode."
  (let* ((modes '(:default :no-tools :user-only :labeled-only :all))
         (pos (or (position *session-tree-selector-filter-mode* modes)
                  0)))
    (setf *session-tree-selector-filter-mode*
          (nth (mod (1+ pos) (length modes)) modes))))

(defun session-tree-selector-set-filter (mode)
  "Set the session tree selector filter MODE."
  (setf *session-tree-selector-filter-mode* mode
        *session-tree-selector-folded-ids* nil)
  (session-tree-selector-update-filter))

(defun session-tree-selector-delete-search-char ()
  "Delete one search character in the session tree selector."
  (when (plusp (length *session-tree-selector-search*))
    (setf *session-tree-selector-search*
          (subseq *session-tree-selector-search*
                  0
                  (1- (length *session-tree-selector-search*))))
    (setf *session-tree-selector-folded-ids* nil)
    (session-tree-selector-update-filter)))

(defun session-tree-selector-insert-search-char (char)
  "Append CHAR to the session tree selector search."
  (setf *session-tree-selector-search*
        (concatenate 'string *session-tree-selector-search* (string char))
        *session-tree-selector-folded-ids* nil)
  (session-tree-selector-update-filter))

(defun session-tree-selector-toggle-fold (item fold-p)
  "Fold or unfold ITEM according to FOLD-P."
  (let ((id (getf item :id)))
    (when (and id (plusp (getf item :children-count)))
      (if fold-p
          (pushnew id *session-tree-selector-folded-ids* :test #'string=)
          (setf *session-tree-selector-folded-ids*
                (remove id *session-tree-selector-folded-ids*
                        :test #'string=)))
      (session-tree-selector-update-filter))))

(defun handle-session-tree-selector-key (key)
  "Handle a key event while the session tree selector is active."
  (let ((base-key (if (and (listp key) (= (length key) 2)
                           (member (first key) '(:meta :alt :ctrl-x :ctrl-c)))
                      (second key)
                      key))
        (count (length *session-tree-selector-filtered-items*)))
    (cond
      ((and (characterp base-key) (char= base-key (code-char 7)))
       (if (plusp (length *session-tree-selector-search*))
           (progn
             (setf *session-tree-selector-search* "")
             (session-tree-selector-update-filter))
           (session-tree-selector-deactivate)))
      ((and (characterp base-key) (char= base-key #\q))
       (session-tree-selector-deactivate))
      ((and (characterp base-key) (or (char= base-key #\Return)
                                      (char= base-key #\Newline)))
       (let ((item (session-tree-selector-current-item))
             (callback *session-tree-selector-callback*))
         (session-tree-selector-deactivate)
         (when (and item callback)
           (funcall callback item))))
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (when (< *session-tree-selector-index* (1- count))
         (incf *session-tree-selector-index*))
       (session-tree-selector-update-filter))
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (when (plusp *session-tree-selector-index*)
         (decf *session-tree-selector-index*))
       (session-tree-selector-update-filter))
      ((or (eq base-key :page-down) (eq base-key :right))
       (let ((item (session-tree-selector-current-item)))
         (if (and (eq base-key :right) item
                  (member (getf item :id) *session-tree-selector-folded-ids*
                          :test #'string=))
             (session-tree-selector-toggle-fold item nil)
             (setf *session-tree-selector-index*
                   (min (max 0 (1- count))
                        (+ *session-tree-selector-index* 10)))))
       (session-tree-selector-update-filter))
      ((or (eq base-key :page-up) (eq base-key :left))
       (let ((item (session-tree-selector-current-item)))
         (if (and (eq base-key :left) item
                  (plusp (getf item :children-count)))
             (session-tree-selector-toggle-fold item t)
             (setf *session-tree-selector-index*
                   (max 0 (- *session-tree-selector-index* 10)))))
       (session-tree-selector-update-filter))
      ((or (eq base-key :backspace)
           (and (characterp base-key)
                (or (char= base-key #\Backspace)
                    (char= base-key #\Rubout))))
       (session-tree-selector-delete-search-char))
      ((and (characterp base-key) (char= base-key (code-char 4)))
       (session-tree-selector-set-filter :default))
      ((and (characterp base-key) (char= base-key (code-char 20)))
       (session-tree-selector-set-filter :no-tools))
      ((and (characterp base-key) (char= base-key (code-char 21)))
       (session-tree-selector-set-filter :user-only))
      ((and (characterp base-key) (char= base-key (code-char 1)))
       (session-tree-selector-set-filter :all))
      ((and (characterp base-key) (char= base-key (code-char 18)))
       (session-tree-selector-set-filter :labeled-only))
      ((and (characterp base-key) (char= base-key (code-char 15)))
       (session-tree-selector-cycle-filter)
       (setf *session-tree-selector-folded-ids* nil)
       (session-tree-selector-update-filter))
      ((and (characterp base-key) (char= base-key #\L))
       (let ((item (session-tree-selector-current-item))
             (callback *session-tree-selector-label-callback*))
         (session-tree-selector-deactivate)
         (when (and item callback)
           (funcall callback item))))
      ((and (characterp base-key) (graphic-char-p base-key))
       (session-tree-selector-insert-search-char base-key))
      (t nil))))

(defun handle-minibuffer-key (key)
  "Handle a key event while the minibuffer is active.
Supports: C-g (cancel), Return (confirm), C-n/Down and C-p/Up (navigate),
Backspace (delete), C-a/C-e (move), C-u (kill all), and self-insert."
  (let ((base-key (if (and (listp key) (= (length key) 2)
                           (member (first key) '(:meta :alt :ctrl-x :ctrl-c)))
                      (second key)
                      key)))
    (cond
      ;; C-g: cancel
      ((and (characterp base-key) (char= base-key (code-char 7)))
       (minibuffer-cancel))
      ;; Return/Newline: confirm selection
      ((and (characterp base-key) (or (char= base-key #\Return)
                                       (char= base-key #\Newline)))
       (minibuffer-confirm))
      ;; C-n or Down arrow: next item
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (minibuffer-next-item))
      ;; C-p or Up arrow: previous item
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (minibuffer-prev-item))
      ;; Backspace: delete character before point
      ((or (eq base-key :backspace)
           (and (characterp base-key) (or (char= base-key #\Backspace)
                                           (char= base-key #\Rubout))))
       (minibuffer-delete-backward))
      ;; C-a: beginning of input
      ((and (characterp base-key) (char= base-key #\Soh))
       (setf *minibuffer-point* 0))
      ;; C-e: end of input
      ((and (characterp base-key) (char= base-key #\Enq))
       (setf *minibuffer-point* (length *minibuffer-input*)))
      ;; C-u: kill all input
      ((and (characterp base-key) (char= base-key (code-char 21)))
       (setf *minibuffer-input* ""
             *minibuffer-point* 0)
       (minibuffer-update-filter))
      ;; Self-insert: printable characters
      ((and (characterp base-key) (graphic-char-p base-key))
       (minibuffer-insert-char base-key))
      ;; Everything else: ignore
      (t nil))))


(defun handle-buffer-selector-key (key)
  "Handle a key event while the buffer selector is active.
Strips any meta/ctrl-x prefix so the selector has simple key bindings."
  (let ((base-key (if (and (listp key) (= (length key) 2)
                           (member (first key) '(:meta :alt :ctrl-x :ctrl-c)))
                      (second key)
                      key))
        (num-buffers (length *buffer-ring*)))
    (cond
      ;; C-g: cancel selector
      ((and (characterp base-key) (char= base-key (code-char 7)))
       (setf *buffer-selector-active* nil))
      ;; q: cancel selector
      ((and (characterp base-key) (char= base-key #\q))
       (setf *buffer-selector-active* nil))
      ;; Enter: select highlighted buffer and close
      ((and (characterp base-key) (or (char= base-key #\Return)
                                       (char= base-key #\Newline)))
       (let ((selected (nth *buffer-selector-index* *buffer-ring*)))
         (when selected
           (switch-to-buffer selected)))
       (setf *buffer-selector-active* nil))
      ;; C-p or Up arrow: move highlight up
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (when (plusp *buffer-selector-index*)
         (decf *buffer-selector-index*)))
      ;; C-n or Down arrow: move highlight down
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (when (< *buffer-selector-index* (1- num-buffers))
         (incf *buffer-selector-index*)))
      ;; n: create new session buffer and switch to it
      ((and (characterp base-key) (char= base-key #\n))
       (let ((new-buf (make-chat-buffer (next-buffer-name)
                                        :agent-name *default-agent-name*
                                        :working-directory (truename ".")
                                        :add-to-ring-p t)))
         (autosave-session-snapshot new-buf)
         (switch-to-buffer new-buf))
       (setf *buffer-selector-active* nil))
      ;; k: kill highlighted buffer (unless it is the last one)
      ((and (characterp base-key) (char= base-key #\k))
       (when (> num-buffers 1)
         (let ((target (nth *buffer-selector-index* *buffer-ring*)))
           (when target
             (kill-buffer-from-ring target)
             (setf *buffer-selector-index*
                   (min *buffer-selector-index*
                        (max 0 (1- (length *buffer-ring*)))))))))
      ;; Everything else: ignore
      (t nil))))

(defun handle-model-selector-key (key buf)
  "Handle a key event while the model selector is active.
Strips any meta/ctrl-x/ctrl-c prefix so the selector has simple key bindings.
On Enter, sets the buffer's provider and model overrides to the selected entry."
  (let ((base-key (if (and (listp key) (= (length key) 2)
                           (member (first key) '(:meta :alt :ctrl-x :ctrl-c)))
                      (second key)
                      key))
        (num-entries (length *model-selector-entries*)))
    (cond
      ;; C-g: cancel selector
      ((and (characterp base-key) (char= base-key (code-char 7)))
       (setf *model-selector-active* nil))
      ;; q: cancel selector
      ((and (characterp base-key) (char= base-key #\q))
       (setf *model-selector-active* nil))
      ;; Enter: select highlighted model and apply to current buffer
      ((and (characterp base-key) (or (char= base-key #\Return)
                                       (char= base-key #\Newline)))
       (let ((selected (nth *model-selector-index* *model-selector-entries*)))
         (when selected
           (let ((provider (getf selected :provider))
                 (model (getf selected :model)))
             (multiple-value-bind (think-status think-level)
                 (apply-buffer-model-selection buf provider model)
               (record-model-selection-history
                (model-selector-display provider model))
               (insert-model-selection-message buf
                                               provider
                                               model
                                               think-status
                                               think-level)))))
       (setf *model-selector-active* nil))
      ;; C-p or Up arrow: move highlight up
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (when (plusp *model-selector-index*)
         (decf *model-selector-index*)))
      ;; C-n or Down arrow: move highlight down
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (when (< *model-selector-index* (1- num-entries))
         (incf *model-selector-index*)))
      ;; Everything else: ignore
      (t nil))))

(defun handle-think-selector-key (key buf)
  "Handle a key event while the think-level selector is active."
  (let ((base-key (if (and (listp key) (= (length key) 2)
                           (member (first key) '(:meta :alt :ctrl-x :ctrl-c)))
                      (second key)
                      key))
        (num-entries (length *think-selector-entries*)))
    (cond
      ((and (characterp base-key) (char= base-key (code-char 7)))
       (setf *think-selector-active* nil))
      ((and (characterp base-key) (char= base-key #\q))
       (setf *think-selector-active* nil))
      ((and (characterp base-key) (or (char= base-key #\Return)
                                      (char= base-key #\Newline)))
       (let ((selected (nth *think-selector-index* *think-selector-entries*)))
         (when selected
           (apply-buffer-think-level-selection buf selected)))
       (setf *think-selector-active* nil))
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (when (plusp *think-selector-index*)
         (decf *think-selector-index*)))
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (when (< *think-selector-index* (1- num-entries))
         (incf *think-selector-index*)))
      (t nil))))
