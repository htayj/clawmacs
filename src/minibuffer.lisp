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

(defclass chat-interaction-state ()
  ((generation :initform 0
               :accessor chat-interaction-state-generation)
   (minibuffer-active-p :initform nil
                        :accessor interaction-minibuffer-active-p)
   (minibuffer-mode :initform :completion
                    :accessor interaction-minibuffer-mode)
   (minibuffer-prompt :initform ""
                      :accessor interaction-minibuffer-prompt)
   (minibuffer-input :initform ""
                     :accessor interaction-minibuffer-input)
   (minibuffer-point :initform 0
                     :accessor interaction-minibuffer-point)
   (minibuffer-items :initform nil
                     :accessor interaction-minibuffer-items)
   (minibuffer-filtered-items :initform nil
                              :accessor interaction-minibuffer-filtered-items)
   (minibuffer-match-positions :initform nil
                               :accessor interaction-minibuffer-match-positions)
   (minibuffer-selected-index :initform 0
                              :accessor interaction-minibuffer-selected-index)
   (minibuffer-default-index :initform 0
                             :accessor interaction-minibuffer-default-index)
   (minibuffer-scroll-offset :initform 0
                             :accessor interaction-minibuffer-scroll-offset)
   (minibuffer-callback :initform nil
                        :accessor interaction-minibuffer-callback)
   (session-tree-active-p :initform nil
                          :accessor interaction-session-tree-active-p)
   (session-tree-buffer :initform nil
                        :accessor interaction-session-tree-buffer)
   (session-tree-items :initform nil
                       :accessor interaction-session-tree-items)
   (session-tree-filtered-items
    :initform nil
    :accessor interaction-session-tree-filtered-items)
   (session-tree-index :initform 0
                       :accessor interaction-session-tree-index)
   (session-tree-scroll :initform 0
                        :accessor interaction-session-tree-scroll)
   (session-tree-search :initform ""
                        :accessor interaction-session-tree-search)
   (session-tree-filter-mode :initform :default
                             :accessor interaction-session-tree-filter-mode)
   (session-tree-folded-ids :initform nil
                            :accessor interaction-session-tree-folded-ids)
   (session-tree-callback :initform nil
                          :accessor interaction-session-tree-callback)
   (session-tree-label-callback
    :initform nil
    :accessor interaction-session-tree-label-callback)
   (slash-completion-active-p
    :initform nil
    :accessor interaction-slash-completion-active-p)
   (slash-completion-buffer :initform nil
                            :accessor interaction-slash-completion-buffer)
   (slash-completion-query :initform ""
                           :accessor interaction-slash-completion-query)
   (slash-completion-token-start
    :initform 0
    :accessor interaction-slash-completion-token-start)
   (slash-completion-token-end
    :initform 0
    :accessor interaction-slash-completion-token-end)
   (slash-completion-token-text
    :initform nil
    :accessor interaction-slash-completion-token-text)
   (slash-completion-dismissed-token
    :initform nil
    :accessor interaction-slash-completion-dismissed-token)
   (slash-completion-items :initform nil
                           :accessor interaction-slash-completion-items)
   (slash-completion-filtered-items
    :initform nil
    :accessor interaction-slash-completion-filtered-items)
   (slash-completion-match-positions
    :initform nil
    :accessor interaction-slash-completion-match-positions)
   (slash-completion-selected-index
    :initform 0
    :accessor interaction-slash-completion-selected-index)
   (slash-completion-scroll-offset
    :initform 0
    :accessor interaction-slash-completion-scroll-offset)
   (skill-completion-active-p
    :initform nil
    :accessor interaction-skill-completion-active-p)
   (skill-completion-buffer :initform nil
                            :accessor interaction-skill-completion-buffer)
   (skill-completion-query :initform ""
                           :accessor interaction-skill-completion-query)
   (skill-completion-token-start
    :initform 0
    :accessor interaction-skill-completion-token-start)
   (skill-completion-token-end
    :initform 0
    :accessor interaction-skill-completion-token-end)
   (skill-completion-token-text
    :initform nil
    :accessor interaction-skill-completion-token-text)
   (skill-completion-dismissed-token
    :initform nil
    :accessor interaction-skill-completion-dismissed-token)
   (skill-completion-items :initform nil
                           :accessor interaction-skill-completion-items)
   (skill-completion-filtered-items
    :initform nil
    :accessor interaction-skill-completion-filtered-items)
   (skill-completion-match-positions
    :initform nil
    :accessor interaction-skill-completion-match-positions)
   (skill-completion-selected-index
    :initform 0
    :accessor interaction-skill-completion-selected-index)
   (skill-completion-scroll-offset
    :initform 0
    :accessor interaction-skill-completion-scroll-offset)
   (meta-pending-p :initform nil
                   :accessor interaction-meta-pending-p)
   (alt-pending-p :initform nil
                  :accessor interaction-alt-pending-p)
   (control-x-pending-p :initform nil
                        :accessor interaction-control-x-pending-p)
   (control-c-pending-p :initform nil
                        :accessor interaction-control-c-pending-p)
   (control-h-pending-p :initform nil
                        :accessor interaction-control-h-pending-p))
  (:documentation
   "Frame-owned transient keyboard, selector, and completion state."))

(defun make-chat-interaction-state ()
  "Return a fresh transient interaction state for one application frame."
  (make-instance 'chat-interaction-state))

(defvar *chat-interaction-state* (make-chat-interaction-state)
  "Dynamically bound pointer to the current frame's interaction state.")

(defun touch-chat-interaction-state (&optional (state *chat-interaction-state*))
  "Invalidate semantic candidate presentations retained from STATE."
  (incf (chat-interaction-state-generation state)))

(defun clear-chat-interaction-state (&optional (state *chat-interaction-state*))
  "Clear STATE in place, releasing callbacks and buffer references.

The object identity remains stable for its owning frame.  Its generation is
advanced first so every presentation created from the old state is stale."
  (touch-chat-interaction-state state)
  (setf (interaction-minibuffer-active-p state) nil
        (interaction-minibuffer-mode state) :completion
        (interaction-minibuffer-prompt state) ""
        (interaction-minibuffer-input state) ""
        (interaction-minibuffer-point state) 0
        (interaction-minibuffer-items state) nil
        (interaction-minibuffer-filtered-items state) nil
        (interaction-minibuffer-match-positions state) nil
        (interaction-minibuffer-selected-index state) 0
        (interaction-minibuffer-default-index state) 0
        (interaction-minibuffer-scroll-offset state) 0
        (interaction-minibuffer-callback state) nil
        (interaction-session-tree-active-p state) nil
        (interaction-session-tree-buffer state) nil
        (interaction-session-tree-items state) nil
        (interaction-session-tree-filtered-items state) nil
        (interaction-session-tree-index state) 0
        (interaction-session-tree-scroll state) 0
        (interaction-session-tree-search state) ""
        (interaction-session-tree-filter-mode state) :default
        (interaction-session-tree-folded-ids state) nil
        (interaction-session-tree-callback state) nil
        (interaction-session-tree-label-callback state) nil
        (interaction-slash-completion-active-p state) nil
        (interaction-slash-completion-buffer state) nil
        (interaction-slash-completion-query state) ""
        (interaction-slash-completion-token-start state) 0
        (interaction-slash-completion-token-end state) 0
        (interaction-slash-completion-token-text state) nil
        (interaction-slash-completion-dismissed-token state) nil
        (interaction-slash-completion-items state) nil
        (interaction-slash-completion-filtered-items state) nil
        (interaction-slash-completion-match-positions state) nil
        (interaction-slash-completion-selected-index state) 0
        (interaction-slash-completion-scroll-offset state) 0
        (interaction-skill-completion-active-p state) nil
        (interaction-skill-completion-buffer state) nil
        (interaction-skill-completion-query state) ""
        (interaction-skill-completion-token-start state) 0
        (interaction-skill-completion-token-end state) 0
        (interaction-skill-completion-token-text state) nil
        (interaction-skill-completion-dismissed-token state) nil
        (interaction-skill-completion-items state) nil
        (interaction-skill-completion-filtered-items state) nil
        (interaction-skill-completion-match-positions state) nil
        (interaction-skill-completion-selected-index state) 0
        (interaction-skill-completion-scroll-offset state) 0
        (interaction-meta-pending-p state) nil
        (interaction-alt-pending-p state) nil
        (interaction-control-x-pending-p state) nil
        (interaction-control-c-pending-p state) nil
        (interaction-control-h-pending-p state) nil)
  state)

;; Source-compatible internal names expand directly to slots on the one
;; dynamically bound state pointer.  There is no scalar snapshot or copyback:
;; nested frame/UI calls simply rebind the pointer, as CLIM does for
;; CLIM:*APPLICATION-FRAME*.
(define-symbol-macro *minibuffer-active*
  (interaction-minibuffer-active-p *chat-interaction-state*))
(define-symbol-macro *minibuffer-mode*
  (interaction-minibuffer-mode *chat-interaction-state*))
(define-symbol-macro *minibuffer-prompt*
  (interaction-minibuffer-prompt *chat-interaction-state*))
(define-symbol-macro *minibuffer-input*
  (interaction-minibuffer-input *chat-interaction-state*))
(define-symbol-macro *minibuffer-point*
  (interaction-minibuffer-point *chat-interaction-state*))
(define-symbol-macro *minibuffer-items*
  (interaction-minibuffer-items *chat-interaction-state*))
(define-symbol-macro *minibuffer-filtered-items*
  (interaction-minibuffer-filtered-items *chat-interaction-state*))
(define-symbol-macro *minibuffer-match-positions*
  (interaction-minibuffer-match-positions *chat-interaction-state*))
(define-symbol-macro *minibuffer-selected-index*
  (interaction-minibuffer-selected-index *chat-interaction-state*))
(define-symbol-macro *minibuffer-default-index*
  (interaction-minibuffer-default-index *chat-interaction-state*))
(define-symbol-macro *minibuffer-scroll-offset*
  (interaction-minibuffer-scroll-offset *chat-interaction-state*))
(define-symbol-macro *minibuffer-callback*
  (interaction-minibuffer-callback *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-active*
  (interaction-session-tree-active-p *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-buffer*
  (interaction-session-tree-buffer *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-items*
  (interaction-session-tree-items *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-filtered-items*
  (interaction-session-tree-filtered-items *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-index*
  (interaction-session-tree-index *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-scroll*
  (interaction-session-tree-scroll *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-search*
  (interaction-session-tree-search *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-filter-mode*
  (interaction-session-tree-filter-mode *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-folded-ids*
  (interaction-session-tree-folded-ids *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-callback*
  (interaction-session-tree-callback *chat-interaction-state*))
(define-symbol-macro *session-tree-selector-label-callback*
  (interaction-session-tree-label-callback *chat-interaction-state*))
(define-symbol-macro *slash-completion-active*
  (interaction-slash-completion-active-p *chat-interaction-state*))
(define-symbol-macro *slash-completion-buffer*
  (interaction-slash-completion-buffer *chat-interaction-state*))
(define-symbol-macro *slash-completion-query*
  (interaction-slash-completion-query *chat-interaction-state*))
(define-symbol-macro *slash-completion-token-start*
  (interaction-slash-completion-token-start *chat-interaction-state*))
(define-symbol-macro *slash-completion-token-end*
  (interaction-slash-completion-token-end *chat-interaction-state*))
(define-symbol-macro *slash-completion-token-text*
  (interaction-slash-completion-token-text *chat-interaction-state*))
(define-symbol-macro *slash-completion-dismissed-token*
  (interaction-slash-completion-dismissed-token *chat-interaction-state*))
(define-symbol-macro *slash-completion-items*
  (interaction-slash-completion-items *chat-interaction-state*))
(define-symbol-macro *slash-completion-filtered-items*
  (interaction-slash-completion-filtered-items *chat-interaction-state*))
(define-symbol-macro *slash-completion-match-positions*
  (interaction-slash-completion-match-positions *chat-interaction-state*))
(define-symbol-macro *slash-completion-selected-index*
  (interaction-slash-completion-selected-index *chat-interaction-state*))
(define-symbol-macro *slash-completion-scroll-offset*
  (interaction-slash-completion-scroll-offset *chat-interaction-state*))
(define-symbol-macro *skill-completion-active*
  (interaction-skill-completion-active-p *chat-interaction-state*))
(define-symbol-macro *skill-completion-buffer*
  (interaction-skill-completion-buffer *chat-interaction-state*))
(define-symbol-macro *skill-completion-query*
  (interaction-skill-completion-query *chat-interaction-state*))
(define-symbol-macro *skill-completion-token-start*
  (interaction-skill-completion-token-start *chat-interaction-state*))
(define-symbol-macro *skill-completion-token-end*
  (interaction-skill-completion-token-end *chat-interaction-state*))
(define-symbol-macro *skill-completion-token-text*
  (interaction-skill-completion-token-text *chat-interaction-state*))
(define-symbol-macro *skill-completion-dismissed-token*
  (interaction-skill-completion-dismissed-token *chat-interaction-state*))
(define-symbol-macro *skill-completion-items*
  (interaction-skill-completion-items *chat-interaction-state*))
(define-symbol-macro *skill-completion-filtered-items*
  (interaction-skill-completion-filtered-items *chat-interaction-state*))
(define-symbol-macro *skill-completion-match-positions*
  (interaction-skill-completion-match-positions *chat-interaction-state*))
(define-symbol-macro *skill-completion-selected-index*
  (interaction-skill-completion-selected-index *chat-interaction-state*))
(define-symbol-macro *skill-completion-scroll-offset*
  (interaction-skill-completion-scroll-offset *chat-interaction-state*))
(define-symbol-macro *meta-pending*
  (interaction-meta-pending-p *chat-interaction-state*))
(define-symbol-macro *alt-pending*
  (interaction-alt-pending-p *chat-interaction-state*))
(define-symbol-macro *cx-pending*
  (interaction-control-x-pending-p *chat-interaction-state*))
(define-symbol-macro *cc-pending*
  (interaction-control-c-pending-p *chat-interaction-state*))
(define-symbol-macro *ch-pending*
  (interaction-control-h-pending-p *chat-interaction-state*))
;;; --------------------------------------------------------------------------
;;; Minibuffer State
;;; --------------------------------------------------------------------------

(defvar *minibuffer-max-height* 12
  "Maximum number of rows the minibuffer can expand to (including prompt).")

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
        *minibuffer-default-index* 0
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
  (touch-chat-interaction-state)
  (setf *minibuffer-active* nil
        *minibuffer-mode* :completion
        *minibuffer-prompt* ""
        *minibuffer-input* ""
        *minibuffer-point* 0
        *minibuffer-items* nil
        *minibuffer-filtered-items* nil
        *minibuffer-match-positions* nil
        *minibuffer-selected-index* 0
        *minibuffer-default-index* 0
        *minibuffer-scroll-offset* 0
        *minibuffer-callback* nil))

(defun minibuffer-update-filter ()
  "Re-filter *minibuffer-items* based on *minibuffer-input*.
When a non-empty query is present the matching candidates are scored and
sorted by relevance (highest score first) so the best match floats to the
top automatically.  Matched character positions are precomputed and stored in
*minibuffer-match-positions* for the renderer to use for highlighting.
Non-empty query edits select that best match; an empty query preserves an
explicitly preselected item.  The scroll offset is reset in either case."
  (let ((query *minibuffer-input*))
    (cond
      ((eq *minibuffer-mode* :prompt)
       (setf *minibuffer-filtered-items* nil
             *minibuffer-match-positions* nil))
      ((zerop (length query))
       ;; No query — show all items in their original order, no highlights.
       (setf *minibuffer-filtered-items* (copy-list *minibuffer-items*)
             *minibuffer-match-positions* (make-list (length *minibuffer-items*)
                                                     :initial-element nil)
             *minibuffer-selected-index* *minibuffer-default-index*))
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
                       sorted-items)
               *minibuffer-selected-index* 0)))))
  ;; Clamp selected index to valid range.
  (setf *minibuffer-selected-index*
        (max 0 (min *minibuffer-selected-index*
                    (1- (max 1 (length *minibuffer-filtered-items*))))))
  ;; Reset scroll and ensure the selection is visible.
  (setf *minibuffer-scroll-offset* 0)
  (minibuffer-ensure-visible)
  (touch-chat-interaction-state))

(defun minibuffer-preselect-index (index)
  "Make INDEX the empty-query default and current minibuffer selection."
  (when (and (integerp index) (<= 0 index))
    (setf *minibuffer-default-index* index
          *minibuffer-selected-index* index)
    (minibuffer-ensure-visible)
    index))

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

(defun minibuffer-delete-forward ()
  "Delete the character at point in the minibuffer input and re-filter."
  (when (< *minibuffer-point* (length *minibuffer-input*))
    (setf *minibuffer-input*
          (concatenate 'string
                       (subseq *minibuffer-input* 0 *minibuffer-point*)
                       (subseq *minibuffer-input* (1+ *minibuffer-point*))))
    (minibuffer-update-filter)))

(defun minibuffer-backward-char ()
  "Move the minibuffer point one character backward."
  (when (plusp *minibuffer-point*)
    (decf *minibuffer-point*)))

(defun minibuffer-forward-char ()
  "Move the minibuffer point one character forward."
  (when (< *minibuffer-point* (length *minibuffer-input*))
    (incf *minibuffer-point*)))

(defun minibuffer-forward-word-position (&optional (point *minibuffer-point*))
  "Return the position at the end of the word after POINT."
  (let ((pos point)
        (len (length *minibuffer-input*)))
    (loop :while (and (< pos len)
                      (not (word-char-p (char *minibuffer-input* pos))))
          :do (incf pos))
    (loop :while (and (< pos len)
                      (word-char-p (char *minibuffer-input* pos)))
          :do (incf pos))
    pos))

(defun minibuffer-backward-word-position (&optional (point *minibuffer-point*))
  "Return the position at the beginning of the word before POINT."
  (let ((pos point))
    (loop :while (and (> pos 0)
                      (not (word-char-p (char *minibuffer-input* (1- pos)))))
          :do (decf pos))
    (loop :while (and (> pos 0)
                      (word-char-p (char *minibuffer-input* (1- pos))))
          :do (decf pos))
    pos))

(defun minibuffer-forward-word ()
  "Move the minibuffer point forward by one word."
  (setf *minibuffer-point* (minibuffer-forward-word-position)))

(defun minibuffer-backward-word ()
  "Move the minibuffer point backward by one word."
  (setf *minibuffer-point* (minibuffer-backward-word-position)))

(defun minibuffer-kill-region (start end)
  "Kill minibuffer input between START and END and re-filter."
  (let ((start (max 0 (min start (length *minibuffer-input*))))
        (end (max 0 (min end (length *minibuffer-input*)))))
    (when (< start end)
      (kill-ring-push (subseq *minibuffer-input* start end))
      (setf *minibuffer-input*
            (concatenate 'string
                         (subseq *minibuffer-input* 0 start)
                         (subseq *minibuffer-input* end))
            *minibuffer-point* start)
      (minibuffer-update-filter))))

(defun minibuffer-kill-line ()
  "Kill from minibuffer point to the end of input."
  (minibuffer-kill-region *minibuffer-point* (length *minibuffer-input*)))

(defun minibuffer-kill-word ()
  "Kill from minibuffer point to the end of the next word."
  (minibuffer-kill-region *minibuffer-point*
                          (minibuffer-forward-word-position)))

(defun minibuffer-backward-kill-word ()
  "Kill from the beginning of the previous word to minibuffer point."
  (minibuffer-kill-region (minibuffer-backward-word-position)
                          *minibuffer-point*))

(defun minibuffer-yank ()
  "Insert the current kill-ring head at minibuffer point and re-filter."
  (let ((text (kill-ring-top)))
    (when text
      (setf *minibuffer-input*
            (concatenate 'string
                         (subseq *minibuffer-input* 0 *minibuffer-point*)
                         text
                         (subseq *minibuffer-input* *minibuffer-point*))
            *minibuffer-point* (+ *minibuffer-point* (length text)))
      (minibuffer-update-filter))))

(defun minibuffer-visible-item-count ()
  "Return the number of candidate rows visible in the minibuffer.
This is the total minibuffer height minus 1 (for the prompt line)."
  (max 0
       (1- (min *minibuffer-max-height*
                (1+ (length *minibuffer-filtered-items*))))))

(defun minibuffer-visible-candidate-rows ()
  "Return visible completion rows as plists for renderers and tests.
Each row contains :INDEX, :ITEM, :DISPLAY, and :SELECTED-P.  Prompt-mode
minibuffers do not expose completion rows."
  (when (and (eq *minibuffer-mode* :completion)
             *minibuffer-filtered-items*)
    (let* ((visible (minibuffer-visible-item-count))
           (count (length *minibuffer-filtered-items*))
           (start (max 0 (min *minibuffer-scroll-offset* count)))
           (end (min count (+ start visible))))
      (loop :for item :in (subseq *minibuffer-filtered-items* start end)
            :for index :from start
            :collect (list :index index
                           :item item
                           :display (minibuffer-item-display item)
                           :selected-p (= index *minibuffer-selected-index*))))))

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
  "Move the selection to the next candidate in the filtered list, wrapping."
  (let ((count (length *minibuffer-filtered-items*)))
    (when (plusp count)
      (setf *minibuffer-selected-index*
            (mod (1+ *minibuffer-selected-index*) count))
      (minibuffer-ensure-visible)
      (touch-chat-interaction-state))))

(defun minibuffer-prev-item ()
  "Move the selection to the previous candidate in the filtered list, wrapping."
  (let ((count (length *minibuffer-filtered-items*)))
    (when (plusp count)
      (setf *minibuffer-selected-index*
            (mod (1- *minibuffer-selected-index*) count))
      (minibuffer-ensure-visible)
      (touch-chat-interaction-state))))

(defun minibuffer-confirm ()
  "Confirm the current selection, invoke the callback, and deactivate.
When completion mode has no selected item, keep the minibuffer open so the user
can revise the query instead of losing it silently."
  (let ((item (when (plusp (length *minibuffer-filtered-items*))
                (nth *minibuffer-selected-index* *minibuffer-filtered-items*)))
        (mode *minibuffer-mode*)
        (input *minibuffer-input*)
        (cb *minibuffer-callback*))
    (cond
      ((and (eq mode :completion) (null item))
       nil)
      (t
       (minibuffer-deactivate)
       (when cb
         (case mode
           (:prompt
            (funcall cb input))
           (t
            (funcall cb item))))))))

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
  "Sort buffer ITEMS by recorded recency, then current buffer-ring order.
Items that were selected more recently appear first. Items not in the history
retain their input order, with the current buffer promoted when necessary."
  (stable-sort (copy-list items)
               (lambda (a b)
                 (let* ((a-name (getf a :name))
                        (b-name (getf b :name))
                        (history *buffer-selection-history*)
                        (a-pos (position a-name history :test #'string=))
                        (b-pos (position b-name history :test #'string=)))
                   (cond
                     ;; Both in history: lower position (more recent) first
                     ((and a-pos b-pos) (< a-pos b-pos))
                     ;; Only a in history: a first
                     (a-pos t)
                     ;; Only b in history: b first
                     (b-pos nil)
                     ;; Neither: current buffer first, then stable input order.
                     ((and (getf a :current-p) (not (getf b :current-p))) t)
                     ((and (getf b :current-p) (not (getf a :current-p))) nil)
                     ;; Stable sort preserves the buffer ring's MRU order.
                     (t nil))))))

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
  (session-tree-selector-ensure-visible)
  (touch-chat-interaction-state)
  *session-tree-selector-filtered-items*)

(defun session-tree-selector-visible-item-count ()
  "Return the number of session-tree candidates visible in the minibuffer pane."
  (max 0
       (1- (min *minibuffer-max-height*
                (1+ (max 1
                         (length *session-tree-selector-filtered-items*)))))))

(defun session-tree-selector-ensure-visible ()
  "Keep the selected session-tree item in its visible row window."
  (let ((visible (session-tree-selector-visible-item-count)))
    (when (plusp visible)
      (when (< *session-tree-selector-index* *session-tree-selector-scroll*)
        (setf *session-tree-selector-scroll* *session-tree-selector-index*))
      (when (>= *session-tree-selector-index*
                (+ *session-tree-selector-scroll* visible))
        (setf *session-tree-selector-scroll*
              (1+ (- *session-tree-selector-index* visible)))))))

(defun session-tree-selector-visible-candidate-rows ()
  "Return visible session-tree rows for the declared CLIM minibuffer pane."
  (when *session-tree-selector-active*
    (let* ((visible (session-tree-selector-visible-item-count))
           (count (length *session-tree-selector-filtered-items*))
           (start (max 0 (min *session-tree-selector-scroll* count)))
           (end (min count (+ start visible))))
      (loop :for item :in (subseq *session-tree-selector-filtered-items*
                                  start end)
            :for index :from start
            :collect (list :index index
                           :item item
                           :display
                           (format nil "~A~A~@[ [~A]~]  ~A: ~A"
                                   (getf item :tree-prefix "")
                                   (if (getf item :active-p) "*" " ")
                                   (getf item :label)
                                   (getf item :kind-label "entry")
                                   (getf item :content ""))
                           :selected-p
                           (= index *session-tree-selector-index*))))))

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
  (touch-chat-interaction-state)
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

(defun minibuffer-control-key-character (key)
  "Return KEY encoded as the control character used by minibuffer commands."
  (cond
    ((null key) nil)
    ((not (characterp key)) key)
    ((char-equal key #\Space) (code-char 0))
    ((alpha-char-p key)
     (code-char (1+ (- (char-code (char-downcase key))
                      (char-code #\a)))))
    (t key)))

(defun minibuffer-base-key (key)
  "Return KEY stripped or encoded for minibuffer command dispatch."
  (cond
    ((and (listp key) (= (length key) 2)
          (member (first key) '(:meta :alt :ctrl-x :ctrl-c)))
     (second key))
    ((and (listp key) (= (length key) 2)
          (member (first key) '(:ctrl :control)))
     (minibuffer-control-key-character (second key)))
    (t key)))

(defun minibuffer-tab-key-p (key)
  "Return true when KEY denotes a plain Tab gesture."
  (or (eq key :tab)
      (and (characterp key) (char= key #\Tab))))

(defun minibuffer-backtab-key-p (key)
  "Return true when KEY denotes a Backtab/Shift-Tab gesture."
  (member key '(:backtab :shift-tab :iso-left-tab) :test #'eq))

(defun minibuffer-prefixed-tab-key-p (key prefixes)
  "Return true when KEY is a prefixed Tab/Backtab gesture."
  (and (listp key)
       (= (length key) 2)
       (member (first key) prefixes :test #'eq)
       (or (minibuffer-tab-key-p (second key))
           (minibuffer-backtab-key-p (second key)))))

(defun minibuffer-prefixed-key-p (key prefixes candidates)
  "Return true when KEY has one of PREFIXES and one of CANDIDATES."
  (and (listp key)
       (= (length key) 2)
       (member (first key) prefixes :test #'eq)
       (some (lambda (candidate)
               (let ((actual (second key)))
                 (cond
                   ((and (characterp actual) (characterp candidate))
                    (char-equal actual candidate))
                   (t (eql actual candidate)))))
             candidates)))

(defun minibuffer-meta-key-p (key &rest candidates)
  "Return true when KEY is a Meta/Alt key for one of CANDIDATES."
  (minibuffer-prefixed-key-p key '(:meta :alt) candidates))

(defun minibuffer-control-key-p (key &rest candidates)
  "Return true when KEY is a Control key for one of CANDIDATES."
  (minibuffer-prefixed-key-p key '(:ctrl :control) candidates))

(defun minibuffer-prefixed-backspace-key-p (key prefixes)
  "Return true when KEY is a prefixed Backspace/Rubout gesture."
  (minibuffer-prefixed-key-p key prefixes
                             '(#\Backspace #\Rubout :backspace :delete :rubout)))

(defun minibuffer-completion-next-key-p (key base-key)
  "Return true when KEY should move to the next completion candidate."
  (and (eq *minibuffer-mode* :completion)
       (not (minibuffer-prefixed-tab-key-p key '(:meta :alt :shift)))
       (not (minibuffer-backtab-key-p base-key))
       (or (eq base-key :down)
           (minibuffer-tab-key-p base-key)
           (and (characterp base-key)
                (char= base-key (code-char 14))))))

(defun minibuffer-completion-prev-key-p (key base-key)
  "Return true when KEY should move to the previous completion candidate."
  (and (eq *minibuffer-mode* :completion)
       (or (eq base-key :up)
           (minibuffer-backtab-key-p base-key)
           (minibuffer-prefixed-tab-key-p key '(:meta :alt :shift))
           (and (characterp base-key)
                (char= base-key (code-char 16))))))

(defun handle-minibuffer-key (key)
  "Handle a key event while the minibuffer is active.
Supports: C-g (cancel), Return (confirm), completion navigation with
C-n/Down/Tab and C-p/Up/M-Tab/Backtab, Emacs-style text editing keys
(C-a/C-e/C-b/C-f/M-b/M-f/C-d/M-d/C-k/C-u/C-w/C-y), Backspace, and
self-insert."
  (let ((base-key (minibuffer-base-key key)))
    (cond
      ;; C-g: cancel
      ((and (characterp base-key) (char= base-key (code-char 7)))
       (minibuffer-cancel))
      ;; Return/Newline: confirm selection
      ((and (characterp base-key) (or (char= base-key #\Return)
                                       (char= base-key #\Newline)))
       (minibuffer-confirm))
      ;; C-n, Down arrow, or Tab: next completion item
      ((minibuffer-completion-next-key-p key base-key)
       (minibuffer-next-item))
      ;; C-p, Up arrow, M-Tab, or Backtab: previous completion item
      ((minibuffer-completion-prev-key-p key base-key)
       (minibuffer-prev-item))
      ;; M-b: backward word
      ((minibuffer-meta-key-p key #\b)
       (minibuffer-backward-word))
      ;; M-f: forward word
      ((minibuffer-meta-key-p key #\f)
       (minibuffer-forward-word))
      ;; M-d: kill word
      ((minibuffer-meta-key-p key #\d)
       (minibuffer-kill-word))
      ;; M-Backspace/C-Backspace/C-w: backward kill word
      ((or (minibuffer-prefixed-backspace-key-p key '(:meta :alt :ctrl :control))
           (and (characterp base-key) (char= base-key (code-char 23))))
       (minibuffer-backward-kill-word))
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
      ;; C-b: backward char
      ((or (and (characterp base-key) (char= base-key (code-char 2)))
           (minibuffer-control-key-p key #\b))
       (minibuffer-backward-char))
      ;; C-f: forward char
      ((or (and (characterp base-key) (char= base-key (code-char 6)))
           (minibuffer-control-key-p key #\f))
       (minibuffer-forward-char))
      ;; C-d: delete char at point
      ((or (and (characterp base-key) (char= base-key (code-char 4)))
           (minibuffer-control-key-p key #\d))
       (minibuffer-delete-forward))
      ;; C-k: kill to end of input
      ((or (and (characterp base-key) (char= base-key (code-char 11)))
           (minibuffer-control-key-p key #\k))
       (minibuffer-kill-line))
      ;; C-u: kill all input
      ((and (characterp base-key) (char= base-key (code-char 21)))
       (setf *minibuffer-input* ""
             *minibuffer-point* 0)
       (minibuffer-update-filter))
      ;; C-y: yank
      ((or (and (characterp base-key) (char= base-key (code-char 25)))
           (minibuffer-control-key-p key #\y))
       (minibuffer-yank))
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
