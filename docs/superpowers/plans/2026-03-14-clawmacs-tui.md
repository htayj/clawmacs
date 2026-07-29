# Clawmacs TUI Chat Buffer Implementation Plan

> **Historical document.** This completed Croatoan-era plan is retained as
> project history. Its in-process permission and sandbox design was removed in
> July 2026; current RPLACA is full-trust and relies on user-controlled
> external containment when desired.

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a working terminal chat UI in Common Lisp with CLOS-based buffer/message/face/command abstractions and access control.

**Architecture:** A single-buffer TUI using croatoan. The buffer owns a doubly-linked list of messages (each a mini-buffer with lines, point, mark). The input area is the mutable tail message. A face system provides per-sender visual identity. A defcommand macro defines commands as generic functions with permission-based access control. The agent is stubbed to echo input.

**Tech Stack:** SBCL, croatoan (ncurses), alexandria, fiveam (tests)

**Spec:** `docs/superpowers/specs/2026-03-14-clawmacs-tui-design.md`

**Standards:** `docs/DESIGN.md` (functional-first, strict SBCL typing), `docs/COMMITS.md` (conventional commits)

---

## File Structure

| File | Responsibility |
|------|---------------|
| `clawmacs.asd` | ASDF system definition (main + test system) |
| `src/packages.lisp` | Package declaration with exports |
| `src/faces.lisp` | color-spec, face, face-set, resolve-face |
| `src/message.lisp` | line DLL, message DLL, cursor, point/mark, text operations |
| `src/buffer.lisp` | Buffer class, message list management, send flow |
| `src/commands.lisp` | defcommand macro, access control conditions, command table |
| `src/keymap.lisp` | Keymap class, lookup, default keymap |
| `src/render.lisp` | Croatoan rendering: history, modeline, input |
| `src/main.lisp` | Entry point, event loop, stub agent |
| `tests/packages.lisp` | Test package declaration |
| `tests/faces-test.lisp` | Face system tests |
| `tests/message-test.lisp` | Message/line data structure tests |
| `tests/buffer-test.lisp` | Buffer data structure tests |
| `tests/commands-test.lisp` | Command system and access control tests |
| `tests/keymap-test.lisp` | Keymap lookup and parent chain tests |
| `tests/render-test.lisp` | Pure render helper tests (format-modeline, input height) |

**Note:** `keymap.lisp` is split from `commands.lisp` because keymaps are a data structure concern (lookup, parent chain) while commands are a protocol concern (defcommand macro, access control, condition system). They converge at the event loop.

**Test runner command:**

```bash
sbcl --noinform --non-interactive \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :clawmacs/tests)' \
  --eval '(let ((results (5am:run :clawmacs-suite))) (unless (5am:results-status results) (sb-ext:exit :code 1)))'
```

---

## Chunk 1: Foundation

### Task 1: Project Scaffold

**Files:**
- Create: `clawmacs.asd`
- Create: `src/packages.lisp`
- Create: `tests/packages.lisp`

- [ ] **Step 1: Create the ASDF system definition**

Create `clawmacs.asd`:

```lisp
(defsystem "clawmacs"
  :description "A Lisp-native Emacs-inspired LLM chat interface"
  :version "0.1.0"
  :depends-on ("croatoan" "alexandria")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "faces")
                             (:file "message")
                             (:file "buffer")
                             (:file "commands")
                             (:file "keymap")
                             (:file "render")
                             (:file "main")))))

(defsystem "clawmacs/tests"
  :description "Tests for clawmacs"
  :depends-on ("clawmacs" "fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "faces-test")
                             (:file "message-test")
                             (:file "buffer-test")
                             (:file "commands-test")
                             (:file "keymap-test")
                             (:file "render-test")))))
```

- [ ] **Step 2: Create the main package declaration**

Create `src/packages.lisp`:

```lisp
(defpackage :clawmacs
  (:use :cl)
  (:export
   ;; Color specs
   #:color-spec
   #:make-color-spec
   #:color-spec-type
   #:color-spec-value

   ;; Faces
   #:face
   #:face-name
   #:face-foreground
   #:face-background
   #:face-bold-p
   #:face-underline-p
   #:face-reverse-p
   #:face-parent
   #:face-transform
   #:resolved-face
   #:resolved-face-foreground
   #:resolved-face-background
   #:resolved-face-bold-p
   #:resolved-face-underline-p
   #:resolved-face-reverse-p
   #:resolve-face

   ;; Face sets
   #:face-set
   #:face-set-owner
   #:face-set-faces
   #:make-face-set
   #:get-face

   ;; Lines
   #:line
   #:make-line
   #:line-content
   #:line-next
   #:line-prev

   ;; Messages
   #:message
   #:make-message
   #:message-first-line
   #:message-last-line
   #:message-point-line
   #:message-point-offset
   #:message-mark-line
   #:message-mark-offset
   #:message-sender
   #:message-timestamp
   #:message-face-set
   #:message-read-only-p
   #:message-next
   #:message-prev
   #:message-text
   #:message-line-count

   ;; Message editing
   #:message-insert-char
   #:message-insert-newline
   #:message-delete-char-backward
   #:message-move-beginning-of-line
   #:message-move-end-of-line
   #:message-kill-line
   #:message-yank

   ;; Kill ring
   #:*kill-ring*
   #:kill-ring-push
   #:kill-ring-top

   ;; Buffer
   #:buffer
   #:make-buffer
   #:buffer-name
   #:buffer-first-message
   #:buffer-last-message
   #:buffer-input-message
   #:buffer-agent-name
   #:buffer-working-directory
   #:buffer-token-count
   #:buffer-context-limit
   #:buffer-status
   #:buffer-face-registry
   #:buffer-keymap
   #:buffer-finalize-input
   #:buffer-insert-agent-message
   #:buffer-message-count

   ;; Commands
   #:*current-caller*
   #:*sandbox-root*
   #:*command-table*
   #:command-metadata
   #:command-metadata-name
   #:command-metadata-permission
   #:command-metadata-docstring
   #:command-metadata-keybindings
   #:defcommand
   #:check-permission
   #:permission-denied
   #:permission-required
   #:list-available-commands

   ;; Keymaps
   #:keymap
   #:make-keymap
   #:keymap-name
   #:keymap-bindings
   #:keymap-parent
   #:keymap-bind
   #:keymap-lookup
   #:*default-keymap*

   ;; Rendering
   #:render-history
   #:render-modeline
   #:render-input
   #:render-buffer

   ;; Main
   #:clawmacs-main
   #:send-to-agent-with-context))
```

- [ ] **Step 3: Create the test package declaration**

Create `tests/packages.lisp`:

```lisp
(defpackage :clawmacs/tests
  (:use :cl :fiveam :clawmacs))

(in-package :clawmacs/tests)

(def-suite clawmacs-suite
  :description "All clawmacs tests")

(def-suite faces-suite
  :description "Face system tests"
  :in clawmacs-suite)

(def-suite message-suite
  :description "Message and line tests"
  :in clawmacs-suite)

(def-suite buffer-suite
  :description "Buffer tests"
  :in clawmacs-suite)

(def-suite commands-suite
  :description "Command system tests"
  :in clawmacs-suite)
```

- [ ] **Step 4: Create stub files so the system loads**

Create each source file with just the `(in-package :clawmacs)` header:

- `src/faces.lisp`
- `src/message.lisp`
- `src/buffer.lisp`
- `src/commands.lisp`
- `src/keymap.lisp`
- `src/render.lisp`
- `src/main.lisp`

And each test file with `(in-package :clawmacs/tests)`:

- `tests/faces-test.lisp`
- `tests/message-test.lisp`
- `tests/buffer-test.lisp`
- `tests/commands-test.lisp`
- `tests/keymap-test.lisp`
- `tests/render-test.lisp`

- [ ] **Step 5: Verify the system loads**

Run:

```bash
sbcl --noinform --non-interactive \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :clawmacs)' \
  --eval '(format t "~%System loaded successfully.~%")'
```

Expected: `System loaded successfully.` with no errors.

- [ ] **Step 6: Verify the test system loads**

Run:

```bash
sbcl --noinform --non-interactive \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :clawmacs/tests)' \
  --eval '(5am:run! :clawmacs-suite)'
```

Expected: `0 tests, 0 failures` (no tests defined yet).

- [ ] **Step 7: Commit**

```bash
git add clawmacs.asd src/ tests/
git commit -m "feat: Add project scaffold with ASDF system and packages

Set up the clawmacs ASDF system with croatoan and alexandria
dependencies, a single clawmacs package with all planned exports,
and a fiveam-based test system with suite hierarchy."
```

---

### Task 2: Face System

**Files:**
- Modify: `src/faces.lisp`
- Create: `tests/faces-test.lisp`

- [ ] **Step 1: Write failing tests for color-spec**

In `tests/faces-test.lisp`:

```lisp
(in-package :clawmacs/tests)
(in-suite faces-suite)

(test color-spec-creation
  "Color specs can be created with CGA, 256-color, and hex types."
  (let ((cga (make-color-spec :cga 8))
        (c256 (make-color-spec :256 196))
        (hex (make-color-spec :hex "#ff0000")))
    (is (eq :cga (color-spec-type cga)))
    (is (= 8 (color-spec-value cga)))
    (is (eq :256 (color-spec-type c256)))
    (is (= 196 (color-spec-value c256)))
    (is (eq :hex (color-spec-type hex)))
    (is (string= "#ff0000" (color-spec-value hex)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run the test runner command. Expected: FAIL — `make-color-spec` undefined.

- [ ] **Step 3: Implement color-spec**

In `src/faces.lisp`:

```lisp
(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Color Spec
;;; --------------------------------------------------------------------------

(defstruct (color-spec (:constructor %make-color-spec))
  "A color specification supporting CGA (0-15), 256-color (0-255), and hex."
  (color-type :cga :type keyword :read-only t)
  (value      0    :read-only t))

(declaim (ftype (function (keyword t) color-spec) make-color-spec))
(defun make-color-spec (color-type value)
  "Create a color-spec. COLOR-TYPE is :CGA, :256, or :HEX.
VALUE is an integer (CGA/256) or a string (hex)."
  (ecase color-type
    (:cga (check-type value (integer 0 15))
          (%make-color-spec :color-type :cga :value value))
    (:256 (check-type value (integer 0 255))
          (%make-color-spec :color-type :256 :value value))
    (:hex (check-type value string)
          (%make-color-spec :color-type :hex :value value))))

(declaim (ftype (function (color-spec) keyword) color-spec-type))
(defun color-spec-type (cs)
  "Return the type of a color-spec."
  (color-spec-color-type cs))
```

**Note:** `color-spec-type` wraps the struct accessor `color-spec-color-type` because the exported symbol is `color-spec-type` (not `color-spec-color-type`). The struct slot is named `color-type` to avoid the clash with `type` as a CL symbol.

- [ ] **Step 4: Run tests to verify color-spec passes**

Run the test runner. Expected: 1 test, 0 failures.

- [ ] **Step 5: Write failing tests for face and resolve-face**

Append to `tests/faces-test.lisp`:

```lisp
(test face-creation
  "Faces can be created with attributes and parent."
  (let ((f (make-instance 'face
             :name :default
             :background (make-color-spec :cga 0)
             :foreground (make-color-spec :cga 15))))
    (is (eq :default (face-name f)))
    (is (= 0 (color-spec-value (face-background f))))
    (is (= 15 (color-spec-value (face-foreground f))))
    (is (null (face-parent f)))
    (is (null (face-bold-p f)))))

(test resolve-face-no-inheritance
  "Resolving a face with all attributes set returns them directly."
  (let* ((bg (make-color-spec :cga 0))
         (fg (make-color-spec :cga 15))
         (f (make-instance 'face
              :name :default
              :background bg
              :foreground fg
              :bold-p nil
              :underline-p nil
              :reverse-p nil))
         (resolved (resolve-face f)))
    (is (eq fg (resolved-face-foreground resolved)))
    (is (eq bg (resolved-face-background resolved)))
    (is (null (resolved-face-bold-p resolved)))
    (is (null (resolved-face-underline-p resolved)))
    (is (null (resolved-face-reverse-p resolved)))))

(test resolve-face-with-inheritance
  "Resolving a face inherits nil attributes from parent."
  (let* ((parent-bg (make-color-spec :cga 0))
         (parent-fg (make-color-spec :cga 15))
         (parent (make-instance 'face
                   :name :default
                   :background parent-bg
                   :foreground parent-fg
                   :bold-p nil
                   :underline-p nil
                   :reverse-p nil))
         (child-fg (make-color-spec :cga 4))
         (child (make-instance 'face
                  :name :error
                  :foreground child-fg
                  :bold-p t
                  :parent parent))
         (resolved (resolve-face child)))
    (is (eq child-fg (resolved-face-foreground resolved)))
    (is (eq parent-bg (resolved-face-background resolved)))
    (is (eq t (resolved-face-bold-p resolved)))
    (is (null (resolved-face-underline-p resolved)))
    (is (null (resolved-face-reverse-p resolved)))))

(test resolve-face-deep-inheritance
  "Resolve-face walks multiple levels of parent chain."
  (let* ((root-bg (make-color-spec :cga 0))
         (root-fg (make-color-spec :cga 15))
         (root (make-instance 'face
                 :name :root
                 :background root-bg
                 :foreground root-fg
                 :bold-p nil
                 :underline-p nil
                 :reverse-p nil))
         (mid (make-instance 'face
                :name :mid
                :bold-p t
                :parent root))
         (leaf (make-instance 'face
                 :name :leaf
                 :underline-p t
                 :parent mid))
         (resolved (resolve-face leaf)))
    (is (eq root-fg (resolved-face-foreground resolved)))
    (is (eq root-bg (resolved-face-background resolved)))
    (is (eq t (resolved-face-bold-p resolved)))
    (is (eq t (resolved-face-underline-p resolved)))
    (is (null (resolved-face-reverse-p resolved)))))
```

- [ ] **Step 6: Run tests to verify they fail**

Run the test runner. Expected: FAIL — `face` class and `resolve-face` undefined.

- [ ] **Step 7: Implement face class and resolve-face**

Append to `src/faces.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Face
;;; --------------------------------------------------------------------------

(defclass face ()
  ((name       :initarg :name
               :reader face-name
               :type keyword)
   (foreground :initarg :foreground
               :accessor face-foreground
               :initform nil
               :type (or null color-spec))
   (background :initarg :background
               :accessor face-background
               :initform nil
               :type (or null color-spec))
   (bold-p     :initarg :bold-p
               :accessor face-bold-p
               :initform nil
               :type (or null boolean))
   (underline-p :initarg :underline-p
                :accessor face-underline-p
                :initform nil
                :type (or null boolean))
   (reverse-p  :initarg :reverse-p
               :accessor face-reverse-p
               :initform nil
               :type (or null boolean))
   (parent     :initarg :parent
               :accessor face-parent
               :initform nil
               :type (or null face))
   (transform  :initarg :transform
               :accessor face-transform
               :initform nil
               :documentation "Stub for future color transforms. Ignored by resolve-face."))
  (:documentation "A named set of visual attributes with optional parent for inheritance."))

;;; --------------------------------------------------------------------------
;;; Resolved Face
;;; --------------------------------------------------------------------------

(defstruct resolved-face
  "A fully-resolved face with no nil attributes. Result of resolve-face."
  (foreground  (error "foreground required") :type color-spec :read-only t)
  (background  (error "background required") :type color-spec :read-only t)
  (bold-p      nil                           :type boolean    :read-only t)
  (underline-p nil                           :type boolean    :read-only t)
  (reverse-p   nil                           :type boolean    :read-only t))

(declaim (ftype (function (face) resolved-face) resolve-face))
(defun resolve-face (face)
  "Walk the parent chain of FACE, filling in nil attributes from ancestors.
Returns a RESOLVED-FACE with all attributes set. Ignores transform specs."
  (let (foreground background bold-p underline-p reverse-p
        (bold-set nil) (underline-set nil) (reverse-set nil))
    (loop :for current := face :then (face-parent current)
          :while current
          :do (when (and (null foreground) (face-foreground current))
                (setf foreground (face-foreground current)))
              (when (and (null background) (face-background current))
                (setf background (face-background current)))
              (when (and (not bold-set) (not (null (slot-value current 'bold-p))))
                (setf bold-p (face-bold-p current)
                      bold-set t))
              (when (and (not underline-set) (not (null (slot-value current 'underline-p))))
                (setf underline-p (face-underline-p current)
                      underline-set t))
              (when (and (not reverse-set) (not (null (slot-value current 'reverse-p))))
                (setf reverse-p (face-reverse-p current)
                      reverse-set t)))
    (make-resolved-face :foreground foreground
                        :background background
                        :bold-p (or bold-p nil)
                        :underline-p (or underline-p nil)
                        :reverse-p (or reverse-p nil))))
```

**Note on boolean inheritance:** The booleans `bold-p`, `underline-p`, `reverse-p` use a separate `*-set` flag to distinguish "explicitly set to nil" from "not set (inherit)". A face slot value of `nil` means "inherit from parent." To explicitly set bold off, the face must have `bold-p` set to `nil` with the slot bound (via `:initarg`). This is handled by checking `(slot-value current 'bold-p)` — if the initarg was provided (even as nil), `slot-boundp` would be true. However, since the `:initform` is `nil`, we need the separate tracking. This design means: a face with `:bold-p nil` explicitly passed inherits bold from parent. To override bold to off, a future iteration could use a sentinel value like `:off`.

**Simplification for this iteration:** Since all non-root faces either set a boolean attribute or leave it to inherit, and root faces always set all booleans, the current approach works. The `nil` initform means "inherit." A face that explicitly passes `:bold-p nil` is saying "inherit." A face that passes `:bold-p t` is saying "bold on." Bold-off at a non-root face is not needed this iteration.

- [ ] **Step 8: Run tests to verify they pass**

Run the test runner. Expected: 4 tests (color-spec + 3 face tests), 0 failures.

- [ ] **Step 9: Write failing tests for face-set and get-face**

Append to `tests/faces-test.lisp`:

```lisp
(test face-set-creation-and-lookup
  "Face sets store faces by name and support lookup."
  (let* ((default-face (make-instance 'face
                         :name :default
                         :background (make-color-spec :cga 0)
                         :foreground (make-color-spec :cga 15)
                         :bold-p nil
                         :underline-p nil
                         :reverse-p nil))
         (error-face (make-instance 'face
                       :name :error
                       :foreground (make-color-spec :cga 4)
                       :parent default-face))
         (fs (make-face-set :agent-1 (list default-face error-face))))
    (is (eq :agent-1 (face-set-owner fs)))
    (is (eq default-face (get-face fs :default)))
    (is (eq error-face (get-face fs :error)))
    (is (null (get-face fs :nonexistent)))))
```

- [ ] **Step 10: Run tests to verify they fail**

Run the test runner. Expected: FAIL — `make-face-set` and `get-face` undefined.

- [ ] **Step 11: Implement face-set and get-face**

Append to `src/faces.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Face Set
;;; --------------------------------------------------------------------------

(defclass face-set ()
  ((owner :initarg :owner
          :reader face-set-owner
          :type keyword)
   (faces :initarg :faces
          :reader face-set-faces
          :type hash-table))
  (:documentation "A collection of named faces belonging to a sender."))

(declaim (ftype (function (keyword list) face-set) make-face-set))
(defun make-face-set (owner face-list)
  "Create a face-set for OWNER from a list of face objects.
Each face is stored by its name in a hash-table."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (f face-list)
      (setf (gethash (face-name f) ht) f))
    (make-instance 'face-set :owner owner :faces ht)))

(declaim (ftype (function (face-set keyword) (or null face)) get-face))
(defun get-face (face-set name)
  "Look up a face by NAME in FACE-SET. Returns nil if not found."
  (gethash name (face-set-faces face-set)))
```

- [ ] **Step 12: Run tests to verify they pass**

Run the test runner. Expected: 5 tests, 0 failures.

- [ ] **Step 13: Commit**

```bash
git add src/faces.lisp tests/faces-test.lisp
git commit -m "feat(faces): Add face system with color specs, inheritance, and face sets

Implement the face system as specified: color-spec supports CGA,
256-color, and hex formats. Face objects have optional attributes
with parent-chain inheritance. resolve-face walks the chain and
returns a fully-specified resolved-face struct. Face-sets group
faces by sender identity. Transform specs are present in the data
model but ignored by resolve-face (stubbed for future iteration)."
```

---

### Task 3: Line and Message Data Structures

**Files:**
- Modify: `src/message.lisp`
- Create: `tests/message-test.lisp`

- [ ] **Step 1: Write failing tests for line creation and linking**

In `tests/message-test.lisp`:

```lisp
(in-package :clawmacs/tests)
(in-suite message-suite)

(test line-creation
  "Lines can be created with content."
  (let ((l (make-line "hello")))
    (is (string= "hello" (line-content l)))
    (is (null (line-next l)))
    (is (null (line-prev l)))))

(test line-linking
  "Lines can be linked into a doubly-linked list."
  (let* ((l1 (make-line "first"))
         (l2 (make-line "second"))
         (l3 (make-line "third")))
    (setf (line-next l1) l2
          (line-prev l2) l1
          (line-next l2) l3
          (line-prev l3) l2)
    (is (eq l2 (line-next l1)))
    (is (eq l1 (line-prev l2)))
    (is (eq l3 (line-next l2)))
    (is (eq l2 (line-prev l3)))
    (is (null (line-prev l1)))
    (is (null (line-next l3)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Expected: FAIL — `make-line` undefined.

- [ ] **Step 3: Implement line class**

In `src/message.lisp`:

```lisp
(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Line
;;; --------------------------------------------------------------------------

(defclass line ()
  ((content :initarg :content
            :accessor line-content
            :initform ""
            :type string)
   (next    :initarg :next
            :accessor line-next
            :initform nil
            :type (or null line))
   (prev    :initarg :prev
            :accessor line-prev
            :initform nil
            :type (or null line)))
  (:documentation "A single line of text in a message. Lines form a doubly-linked list."))

(declaim (ftype (function (&optional string) line) make-line))
(defun make-line (&optional (content ""))
  "Create a new unlinked line with CONTENT."
  (make-instance 'line :content content))
```

- [ ] **Step 4: Run tests to verify they pass**

Expected: 2 tests pass.

- [ ] **Step 5: Write failing tests for message creation**

Append to `tests/message-test.lisp`:

```lisp
(test message-creation
  "Messages are created with a single empty line, point at (first-line, 0)."
  (let ((m (make-message :user)))
    (is (eq :user (message-sender m)))
    (is (not (null (message-first-line m))))
    (is (eq (message-first-line m) (message-last-line m)))
    (is (string= "" (line-content (message-first-line m))))
    (is (eq (message-first-line m) (message-point-line m)))
    (is (= 0 (message-point-offset m)))
    (is (null (message-mark-line m)))
    (is (not (message-read-only-p m)))
    (is (null (message-next m)))
    (is (null (message-prev m)))))

(test message-text-extraction
  "message-text returns the full text content of a message."
  (let* ((l1 (make-line "hello"))
         (l2 (make-line "world"))
         (m (make-message :user)))
    ;; Replace the default empty line with our lines
    (setf (slot-value m 'first-line) l1
          (slot-value m 'last-line) l2
          (line-next l1) l2
          (line-prev l2) l1)
    (is (string= "hello
world" (message-text m)))))

(test message-line-count
  "message-line-count returns the number of lines."
  (let* ((m (make-message :user)))
    (is (= 1 (message-line-count m)))
    ;; Add a second line
    (let ((l2 (make-line "second")))
      (setf (line-next (message-first-line m)) l2
            (line-prev l2) (message-first-line m)
            (slot-value m 'last-line) l2))
    (is (= 2 (message-line-count m)))))
```

- [ ] **Step 6: Run tests to verify they fail**

Expected: FAIL — `make-message`, `message` class undefined.

- [ ] **Step 7: Implement message class**

Append to `src/message.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Message
;;; --------------------------------------------------------------------------

(defclass message ()
  ((first-line    :initarg :first-line
                  :accessor message-first-line
                  :type line)
   (last-line     :initarg :last-line
                  :accessor message-last-line
                  :type line)
   (point-line    :initarg :point-line
                  :accessor message-point-line
                  :type line)
   (point-offset  :initarg :point-offset
                  :accessor message-point-offset
                  :initform 0
                  :type fixnum)
   (mark-line     :initarg :mark-line
                  :accessor message-mark-line
                  :initform nil
                  :type (or null line))
   (mark-offset   :initarg :mark-offset
                  :accessor message-mark-offset
                  :initform nil
                  :type (or null fixnum))
   (sender        :initarg :sender
                  :accessor message-sender
                  :type keyword)
   (timestamp     :initarg :timestamp
                  :accessor message-timestamp
                  :initform nil
                  :type (or null integer))
   (face-set      :initarg :face-set
                  :accessor message-face-set
                  :initform nil
                  :type (or null face-set))
   (read-only-p   :initarg :read-only-p
                  :accessor message-read-only-p
                  :initform nil
                  :type boolean)
   (next          :initarg :next
                  :accessor message-next
                  :initform nil
                  :type (or null message))
   (prev          :initarg :prev
                  :accessor message-prev
                  :initform nil
                  :type (or null message)))
  (:documentation
   "A message in the chat buffer. Contains a doubly-linked list of lines,
a point and optional mark for intra-message cursor/selection, sender
identity, and links to adjacent messages in the buffer."))

(declaim (ftype (function (keyword &key (:face-set (or null face-set))
                                       (:read-only-p boolean))
                          message)
                make-message))
(defun make-message (sender &key face-set (read-only-p nil))
  "Create a new message with a single empty line. Point starts at offset 0."
  (let* ((initial-line (make-line ""))
         (msg (make-instance 'message
                :first-line initial-line
                :last-line initial-line
                :point-line initial-line
                :point-offset 0
                :sender sender
                :face-set face-set
                :read-only-p read-only-p)))
    msg))

(declaim (ftype (function (message) string) message-text))
(defun message-text (msg)
  "Return the full text of MSG as a single string with newlines between lines."
  (with-output-to-string (s)
    (loop :for current := (message-first-line msg) :then (line-next current)
          :while current
          :for first := t :then nil
          :do (unless first (write-char #\Newline s))
              (write-string (line-content current) s))))

(declaim (ftype (function (message) fixnum) message-line-count))
(defun message-line-count (msg)
  "Count the number of lines in MSG."
  (loop :for current := (message-first-line msg) :then (line-next current)
        :while current
        :count t))
```

- [ ] **Step 8: Run tests to verify they pass**

Expected: 5 tests pass (2 line + 3 message).

- [ ] **Step 9: Write failing tests for message text editing**

Append to `tests/message-test.lisp`:

```lisp
(test message-insert-char
  "Inserting a character at point advances point."
  (let ((m (make-message :user)))
    (message-insert-char m #\h)
    (message-insert-char m #\i)
    (is (string= "hi" (line-content (message-first-line m))))
    (is (= 2 (message-point-offset m)))))

(test message-insert-newline
  "Inserting a newline splits the current line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    ;; Move point back to after 'a'
    (setf (message-point-offset m) 1)
    (message-insert-newline m)
    (is (= 2 (message-line-count m)))
    (is (string= "a" (line-content (message-first-line m))))
    (is (string= "b" (line-content (message-last-line m))))
    (is (eq (message-last-line m) (message-point-line m)))
    (is (= 0 (message-point-offset m)))))

(test message-delete-char-backward
  "Deleting backward removes char before point."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (message-insert-char m #\c)
    (message-delete-char-backward m)
    (is (string= "ab" (line-content (message-first-line m))))
    (is (= 2 (message-point-offset m)))))

(test message-delete-char-backward-joins-lines
  "Deleting backward at start of line joins with previous line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-newline m)
    (message-insert-char m #\b)
    ;; Point is at line 2, offset 1. Move to offset 0.
    (setf (message-point-offset m) 0)
    (message-delete-char-backward m)
    (is (= 1 (message-line-count m)))
    (is (string= "ab" (line-content (message-first-line m))))
    (is (= 1 (message-point-offset m)))))

(test message-move-beginning-of-line
  "C-a moves point to beginning of current line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (message-move-beginning-of-line m)
    (is (= 0 (message-point-offset m)))))

(test message-move-end-of-line
  "C-e moves point to end of current line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (message-move-beginning-of-line m)
    (message-move-end-of-line m)
    (is (= 2 (message-point-offset m)))))

(test message-kill-line
  "C-k kills from point to end of line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-char m #\b)
    (message-insert-char m #\c)
    (setf (message-point-offset m) 1)
    (message-kill-line m)
    (is (string= "a" (line-content (message-point-line m))))
    (is (= 1 (message-point-offset m)))
    (is (string= "bc" (kill-ring-top)))))

(test message-kill-line-at-end-joins
  "C-k at end of line joins with next line."
  (let ((m (make-message :user)))
    (message-insert-char m #\a)
    (message-insert-newline m)
    (message-insert-char m #\b)
    ;; Move to end of first line
    (setf (message-point-line m) (message-first-line m)
          (message-point-offset m) 1)
    (message-kill-line m)
    (is (= 1 (message-line-count m)))
    (is (string= "ab" (line-content (message-first-line m))))))

(test message-yank
  "C-y inserts the top of the kill ring at point."
  (let ((m (make-message :user)))
    (kill-ring-push "hello")
    (message-yank m)
    (is (string= "hello" (line-content (message-first-line m))))
    (is (= 5 (message-point-offset m)))))
```

- [ ] **Step 10: Run tests to verify they fail**

Expected: FAIL — editing functions undefined.

- [ ] **Step 11: Implement kill ring**

Append to `src/message.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Kill Ring
;;; --------------------------------------------------------------------------

(defvar *kill-ring* nil
  "The global kill ring. A list of strings, most recent first.")

(defvar *kill-ring-max* 60
  "Maximum number of entries in the kill ring.")

(declaim (ftype (function (string) string) kill-ring-push))
(defun kill-ring-push (string)
  "Push STRING onto the kill ring. Trims ring to *kill-ring-max* entries."
  (push string *kill-ring*)
  (when (> (length *kill-ring*) *kill-ring-max*)
    (setf *kill-ring* (subseq *kill-ring* 0 *kill-ring-max*)))
  string)

(declaim (ftype (function () (or null string)) kill-ring-top))
(defun kill-ring-top ()
  "Return the most recent kill ring entry, or nil if empty."
  (first *kill-ring*))
```

- [ ] **Step 12: Implement message editing operations**

Append to `src/message.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Message Editing Operations
;;; --------------------------------------------------------------------------

(declaim (ftype (function (message character) message) message-insert-char))
(defun message-insert-char (msg char)
  "Insert CHAR at point in MSG. Advances point by 1."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl))
         (new-content (concatenate 'string
                                   (subseq content 0 po)
                                   (string char)
                                   (subseq content po))))
    (setf (line-content pl) new-content
          (message-point-offset msg) (1+ po)))
  msg)

(declaim (ftype (function (message) message) message-insert-newline))
(defun message-insert-newline (msg)
  "Insert a newline at point, splitting the current line into two."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl))
         (before (subseq content 0 po))
         (after (subseq content po))
         (new-line (make-line after)))
    ;; Update current line to hold only the text before point
    (setf (line-content pl) before)
    ;; Link new-line into the DLL after current line
    (setf (line-next new-line) (line-next pl)
          (line-prev new-line) pl)
    (when (line-next pl)
      (setf (line-prev (line-next pl)) new-line))
    (setf (line-next pl) new-line)
    ;; Update last-line if we split the last line
    (when (eq pl (message-last-line msg))
      (setf (message-last-line msg) new-line))
    ;; Move point to beginning of new line
    (setf (message-point-line msg) new-line
          (message-point-offset msg) 0))
  msg)

(declaim (ftype (function (message) message) message-delete-char-backward))
(defun message-delete-char-backward (msg)
  "Delete the character before point. If at start of line, join with previous line."
  (let ((pl (message-point-line msg))
        (po (message-point-offset msg)))
    (cond
      ;; Normal case: delete char before point in current line
      ((> po 0)
       (let* ((content (line-content pl))
              (new-content (concatenate 'string
                                        (subseq content 0 (1- po))
                                        (subseq content po))))
         (setf (line-content pl) new-content
               (message-point-offset msg) (1- po))))
      ;; At start of line with a previous line: join lines
      ((line-prev pl)
       (let* ((prev (line-prev pl))
              (prev-len (length (line-content prev)))
              (merged (concatenate 'string (line-content prev) (line-content pl))))
         (setf (line-content prev) merged)
         ;; Unlink current line
         (setf (line-next prev) (line-next pl))
         (when (line-next pl)
           (setf (line-prev (line-next pl)) prev))
         ;; Update last-line if needed
         (when (eq pl (message-last-line msg))
           (setf (message-last-line msg) prev))
         ;; Move point to the join position in prev line
         (setf (message-point-line msg) prev
               (message-point-offset msg) prev-len)))
      ;; At start of first line: do nothing
      (t nil)))
  msg)

(declaim (ftype (function (message) message) message-move-beginning-of-line))
(defun message-move-beginning-of-line (msg)
  "Move point to the beginning of the current line."
  (setf (message-point-offset msg) 0)
  msg)

(declaim (ftype (function (message) message) message-move-end-of-line))
(defun message-move-end-of-line (msg)
  "Move point to the end of the current line."
  (setf (message-point-offset msg)
        (length (line-content (message-point-line msg))))
  msg)

(declaim (ftype (function (message) message) message-kill-line))
(defun message-kill-line (msg)
  "Kill from point to end of line. If at end of line, join with next line.
Killed text is pushed to the kill ring."
  (let* ((pl (message-point-line msg))
         (po (message-point-offset msg))
         (content (line-content pl)))
    (cond
      ;; Text after point on this line: kill it
      ((< po (length content))
       (let ((killed (subseq content po)))
         (setf (line-content pl) (subseq content 0 po))
         (kill-ring-push killed)))
      ;; At end of line with a next line: join
      ((line-next pl)
       (let ((next (line-next pl)))
         (setf (line-content pl)
               (concatenate 'string content (line-content next)))
         ;; Unlink next line
         (setf (line-next pl) (line-next next))
         (when (line-next next)
           (setf (line-prev (line-next next)) pl))
         (when (eq next (message-last-line msg))
           (setf (message-last-line msg) pl))
         ;; Push newline as killed text
         (kill-ring-push (string #\Newline))))
      ;; At end of last line: do nothing
      (t nil)))
  msg)

(declaim (ftype (function (message) message) message-yank))
(defun message-yank (msg)
  "Insert the top of the kill ring at point."
  (let ((text (kill-ring-top)))
    (when text
      ;; Insert character by character, handling newlines
      (loop :for char :across text
            :do (if (char= char #\Newline)
                    (message-insert-newline msg)
                    (message-insert-char msg char)))))
  msg)
```

- [ ] **Step 13: Run tests to verify they pass**

Run the test runner. Expected: all message tests pass (14 tests total: 2 line + 3 message + 9 editing).

- [ ] **Step 14: Commit**

```bash
git add src/message.lisp tests/message-test.lisp
git commit -m "feat(message): Add line DLL, message objects, and text editing

Implement the message data model: lines as a doubly-linked list,
messages with point/mark cursor state and sender identity. Text
editing operations include insert-char, insert-newline, backward
delete (with line joining), beginning/end of line movement,
kill-line (with kill ring), and yank. Each message is a self-
contained mini-buffer suitable for both chat history and input."
```

---

### Task 4: Buffer Data Structure

**Files:**
- Modify: `src/buffer.lisp`
- Create: `tests/buffer-test.lisp`

- [ ] **Step 1: Write failing tests for buffer creation**

In `tests/buffer-test.lisp`:

```lisp
(in-package :clawmacs/tests)
(in-suite buffer-suite)

(test buffer-creation
  "A new buffer has one message (the input message)."
  (let ((buf (make-buffer "test-session"
                          :agent-name "echo-agent"
                          :working-directory #P"/tmp/")))
    (is (string= "test-session" (buffer-name buf)))
    (is (string= "echo-agent" (buffer-agent-name buf)))
    (is (not (null (buffer-input-message buf))))
    (is (eq (buffer-first-message buf) (buffer-input-message buf)))
    (is (eq (buffer-last-message buf) (buffer-input-message buf)))
    (is (not (message-read-only-p (buffer-input-message buf))))
    (is (= 1 (buffer-message-count buf)))
    (is (eq :idle (buffer-status buf)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Expected: FAIL — `make-buffer` undefined.

- [ ] **Step 3: Implement buffer class**

In `src/buffer.lisp`:

```lisp
(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Buffer
;;; --------------------------------------------------------------------------

(defclass buffer ()
  ((name              :initarg :name
                      :accessor buffer-name
                      :type string)
   (first-message     :initarg :first-message
                      :accessor buffer-first-message
                      :type message)
   (last-message      :initarg :last-message
                      :accessor buffer-last-message
                      :type message)
   (agent-name        :initarg :agent-name
                      :accessor buffer-agent-name
                      :initform "agent"
                      :type string)
   (working-directory :initarg :working-directory
                      :accessor buffer-working-directory
                      :initform (truename ".")
                      :type pathname)
   (token-count       :initarg :token-count
                      :accessor buffer-token-count
                      :initform 0
                      :type integer)
   (context-limit     :initarg :context-limit
                      :accessor buffer-context-limit
                      :initform 200000
                      :type integer)
   (status            :initarg :status
                      :accessor buffer-status
                      :initform :idle
                      :type keyword)
   (face-registry     :initarg :face-registry
                      :accessor buffer-face-registry
                      :type hash-table)
   (keymap            :initarg :keymap
                      :accessor buffer-keymap
                      :initform nil))
  (:documentation
   "A chat buffer containing a doubly-linked list of messages.
The last message is always the input message (read-only-p = nil).
Invariant: last-message and input-message always refer to the same object."))

(defun buffer-input-message (buf)
  "Return the input message (alias for last-message).
Enforces the invariant that it is not read-only."
  (let ((msg (buffer-last-message buf)))
    (assert (not (message-read-only-p msg)) ()
            "Invariant violated: input message is read-only")
    msg))

(declaim (ftype (function (string &key (:agent-name string)
                                       (:working-directory pathname)
                                       (:context-limit integer))
                          buffer)
                make-buffer))
(defun make-buffer (name &key (agent-name "agent")
                              (working-directory (truename "."))
                              (context-limit 200000))
  "Create a new buffer with a single empty input message."
  (let* ((input-msg (make-message :user))
         (registry (make-hash-table :test #'eq))
         (buf (make-instance 'buffer
                :name name
                :first-message input-msg
                :last-message input-msg
                :agent-name agent-name
                :working-directory working-directory
                :context-limit context-limit
                :face-registry registry)))
    buf))

(declaim (ftype (function (buffer) fixnum) buffer-message-count))
(defun buffer-message-count (buf)
  "Count the number of messages in BUF."
  (loop :for current := (buffer-first-message buf) :then (message-next current)
        :while current
        :count t))
```

- [ ] **Step 4: Run tests to verify they pass**

Expected: buffer-creation passes.

- [ ] **Step 5: Write failing tests for buffer-finalize-input and buffer-insert-agent-message**

Append to `tests/buffer-test.lisp`:

```lisp
(test buffer-finalize-input
  "Finalizing input makes it read-only and creates a new input message."
  (let ((buf (make-buffer "test")))
    ;; Type something into the input
    (message-insert-char (buffer-input-message buf) #\h)
    (message-insert-char (buffer-input-message buf) #\i)
    ;; Finalize
    (buffer-finalize-input buf)
    ;; Old message should be read-only
    (let ((first-msg (buffer-first-message buf)))
      (is (message-read-only-p first-msg))
      (is (eq :user (message-sender first-msg)))
      (is (not (null (message-timestamp first-msg))))
      (is (string= "hi" (message-text first-msg))))
    ;; New input message should exist
    (is (= 2 (buffer-message-count buf)))
    (is (not (message-read-only-p (buffer-input-message buf))))
    (is (string= "" (message-text (buffer-input-message buf))))
    ;; Invariant: last-message is input
    (is (eq (buffer-last-message buf) (buffer-input-message buf)))))

(test buffer-insert-agent-message
  "Agent messages are inserted before the input message."
  (let ((buf (make-buffer "test" :agent-name "echo")))
    ;; Finalize a user message first
    (message-insert-char (buffer-input-message buf) #\h)
    (message-insert-char (buffer-input-message buf) #\i)
    (buffer-finalize-input buf)
    ;; Insert an agent message
    (buffer-insert-agent-message buf "Echo: hi")
    ;; Should have 3 messages: user, agent, input
    (is (= 3 (buffer-message-count buf)))
    (let ((agent-msg (message-next (buffer-first-message buf))))
      (is (string= "Echo: hi" (message-text agent-msg)))
      (is (message-read-only-p agent-msg))
      (is (eq :echo (message-sender agent-msg))))
    ;; Input should still be last
    (is (not (message-read-only-p (buffer-input-message buf))))))
```

- [ ] **Step 6: Run tests to verify they fail**

Expected: FAIL — `buffer-finalize-input` and `buffer-insert-agent-message` undefined.

- [ ] **Step 7: Implement buffer operations**

Append to `src/buffer.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Buffer Operations
;;; --------------------------------------------------------------------------

(declaim (ftype (function (buffer) buffer) buffer-finalize-input))
(defun buffer-finalize-input (buf)
  "Finalize the current input message: make it read-only, timestamp it,
and create a new empty input message at the tail."
  (let ((input (buffer-input-message buf)))
    ;; Finalize the current input
    (setf (message-read-only-p input) t
          (message-timestamp input) (get-universal-time)
          (message-sender input) :user)
    ;; Create new input message
    (let ((new-input (make-message :user)))
      ;; Link into the DLL
      (setf (message-prev new-input) input
            (message-next input) new-input
            (buffer-last-message buf) new-input)))
  buf)

(declaim (ftype (function (buffer string) message) buffer-insert-agent-message))
(defun buffer-insert-agent-message (buf text)
  "Create a read-only agent message with TEXT and insert it before the input message."
  (let* ((agent-keyword (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (agent-msg (make-message agent-keyword :read-only-p t))
         (input (buffer-input-message buf)))
    ;; Set the message content
    (setf (line-content (message-first-line agent-msg)) text)
    ;; Set timestamp
    (setf (message-timestamp agent-msg) (get-universal-time))
    ;; Insert before the input message
    (let ((before-input (message-prev input)))
      (setf (message-prev agent-msg) before-input
            (message-next agent-msg) input
            (message-prev input) agent-msg)
      (if before-input
          (setf (message-next before-input) agent-msg)
          ;; Agent message is now the first message
          (setf (buffer-first-message buf) agent-msg)))
    agent-msg))
```

- [ ] **Step 8: Run tests to verify they pass**

Expected: all buffer tests pass (3 tests).

- [ ] **Step 9: Commit**

```bash
git add src/buffer.lisp tests/buffer-test.lisp
git commit -m "feat(buffer): Add buffer with message list management

Implement the buffer class owning a doubly-linked list of messages.
buffer-finalize-input makes the current input read-only, timestamps
it, and creates a new empty tail message. buffer-insert-agent-message
creates a read-only agent message and inserts it before the input.
The invariant that last-message is always the mutable input is
enforced by buffer-input-message via assertion."
```

---

### Task 5: Command System and Access Control

**Files:**
- Modify: `src/commands.lisp`
- Create: `tests/commands-test.lisp`

- [ ] **Step 1: Write failing tests for command registration and access control**

In `tests/commands-test.lisp`:

```lisp
(in-package :clawmacs/tests)
(in-suite commands-suite)

(test command-metadata-registration
  "defcommand registers metadata in the command table."
  ;; Clear the command table for test isolation
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(clawmacs:defcommand test-cmd (:permission :user-only)
             "A test command."
             (buffer)
             (declare (ignore buffer))
             :test-result))
    (let ((meta (gethash 'test-cmd *command-table*)))
      (is (not (null meta)))
      (is (eq :user-only (command-metadata-permission meta)))
      (is (string= "A test command." (command-metadata-docstring meta))))))

(test permission-denied-for-agent-on-user-only
  "An agent calling a :user-only command signals permission-denied."
  (let ((*command-table* (make-hash-table :test #'eq))
        (*current-caller* :some-agent))
    (eval '(clawmacs:defcommand restricted-cmd (:permission :user-only)
             "Restricted."
             (buffer)
             (declare (ignore buffer))
             :ok))
    (signals permission-denied
      (check-permission 'restricted-cmd))))

(test permission-passes-for-user-on-user-only
  "A user calling a :user-only command succeeds."
  (let ((*command-table* (make-hash-table :test #'eq))
        (*current-caller* :user))
    (eval '(clawmacs:defcommand allowed-cmd (:permission :user-only)
             "Allowed."
             (buffer)
             (declare (ignore buffer))
             :ok))
    (finishes (check-permission 'allowed-cmd))))

(test agent-allowed-passes-for-any-caller
  "An :agent-allowed command can be called by anyone."
  (let ((*command-table* (make-hash-table :test #'eq))
        (*current-caller* :some-agent))
    (eval '(clawmacs:defcommand open-cmd (:permission :agent-allowed)
             "Open."
             (buffer)
             (declare (ignore buffer))
             :ok))
    (finishes (check-permission 'open-cmd))))

(test list-available-commands-filters-by-caller
  "list-available-commands excludes :user-only commands for agent callers."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(clawmacs:defcommand user-cmd (:permission :user-only)
             "User only." (buffer) (declare (ignore buffer)) nil))
    (eval '(clawmacs:defcommand agent-cmd (:permission :agent-allowed)
             "Agent ok." (buffer) (declare (ignore buffer)) nil))
    (let ((*current-caller* :user))
      (let ((cmds (list-available-commands)))
        (is (member 'user-cmd cmds))
        (is (member 'agent-cmd cmds))))
    (let ((*current-caller* :some-agent))
      (let ((cmds (list-available-commands)))
        (is (not (member 'user-cmd cmds)))
        (is (member 'agent-cmd cmds))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Expected: FAIL — command system undefined.

- [ ] **Step 3: Implement command system**

In `src/commands.lisp`:

```lisp
(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Special Variables
;;; --------------------------------------------------------------------------

(defvar *current-caller* :user
  "The current caller context. Bound to :USER for interactive use,
or an agent keyword (e.g., :CLAUDE-OPUS) during agent command dispatch.")

(defvar *sandbox-root* nil
  "When non-nil, restricts file operations to this directory subtree.")

;;; --------------------------------------------------------------------------
;;; Command Metadata
;;; --------------------------------------------------------------------------

(defstruct command-metadata
  "Metadata for a registered command."
  (name        (error "name required")       :type symbol   :read-only t)
  (permission  :user-only                    :type keyword  :read-only t)
  (docstring   ""                            :type string   :read-only t)
  (keybindings nil                           :type list     :read-only t))

(defvar *command-table* (make-hash-table :test #'eq)
  "Global table mapping command symbols to command-metadata.")

;;; --------------------------------------------------------------------------
;;; Conditions
;;; --------------------------------------------------------------------------

(define-condition permission-denied (error)
  ((command :initarg :command :reader permission-denied-command))
  (:report (lambda (c stream)
             (format stream "Permission denied: ~A is not available to ~A"
                     (permission-denied-command c) *current-caller*))))

(define-condition permission-required (error)
  ((command :initarg :command :reader permission-required-command))
  (:report (lambda (c stream)
             (format stream "Permission required: ~A needs approval for ~A"
                     *current-caller* (permission-required-command c)))))

;;; --------------------------------------------------------------------------
;;; Access Control
;;; --------------------------------------------------------------------------

(declaim (ftype (function (symbol) (values)) check-permission))
(defun check-permission (command-name)
  "Check whether *CURRENT-CALLER* has permission to execute COMMAND-NAME.
Signals PERMISSION-DENIED or PERMISSION-REQUIRED as appropriate."
  (let* ((metadata (gethash command-name *command-table*))
         (permission (command-metadata-permission metadata)))
    (ecase permission
      (:agent-allowed
       ;; Anyone can call
       (values))
      (:agent-with-permission
       (unless (eq *current-caller* :user)
         (restart-case
             (error 'permission-required :command command-name)
           (grant-permission ()
             :report "Grant permission for this command"
             (values))
           (deny-permission ()
             :report "Deny permission for this command"
             (error 'permission-denied :command command-name)))))
      (:user-only
       (unless (eq *current-caller* :user)
         (error 'permission-denied :command command-name))
       (values)))))

;;; --------------------------------------------------------------------------
;;; Command Listing
;;; --------------------------------------------------------------------------

(declaim (ftype (function () list) list-available-commands))
(defun list-available-commands ()
  "Return a list of command symbols available to *CURRENT-CALLER*.
Filters out :USER-ONLY commands when caller is not :USER."
  (let ((result nil))
    (maphash (lambda (name metadata)
               (let ((perm (command-metadata-permission metadata)))
                 (when (or (eq *current-caller* :user)
                           (not (eq perm :user-only)))
                   (push name result))))
             *command-table*)
    result))

;;; --------------------------------------------------------------------------
;;; defcommand Macro
;;; --------------------------------------------------------------------------

(defmacro defcommand (name (&key (permission :user-only) (keys nil))
                      docstring (buffer-var) &body body)
  "Define a command as a generic function with access control.

Expands to:
1. Registration of command metadata in *command-table*
2. A generic function definition
3. An :around method that checks permissions
4. A primary method with BODY

Example:
  (defcommand send-message (:permission :user-only :keys ((#\\Return)))
    \"Send the current input.\"
    (buffer)
    (buffer-finalize-input buffer))"
  (let ((meta-var (gensym "META")))
    `(progn
       ;; Register metadata
       (let ((,meta-var (make-command-metadata
                         :name ',name
                         :permission ,permission
                         :docstring ,docstring
                         :keybindings ',keys)))
         (setf (gethash ',name *command-table*) ,meta-var))

       ;; Define the generic function (idempotent in CLOS)
       (defgeneric ,name (,buffer-var)
         (:documentation ,docstring))

       ;; Define the access control :around method
       (defmethod ,name :around ((,buffer-var buffer))
         (check-permission ',name)
         (call-next-method))

       ;; Define the primary method
       (defmethod ,name ((,buffer-var buffer))
         ,@body)

       ',name)))
```

- [ ] **Step 4: Run tests to verify they pass**

Run the test runner. Expected: all 5 command tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/commands.lisp tests/commands-test.lisp
git commit -m "feat(commands): Add defcommand macro with access control

Implement the command system: defcommand defines generic functions
with permission-based access control (:user-only, :agent-with-
permission, :agent-allowed). An :around method checks *current-
caller* and signals permission-denied or permission-required
conditions with restarts. list-available-commands filters by
caller context, making :user-only commands invisible to agents."
```

---

## Chunk 2: TUI

### Task 6: Keymaps

**Files:**
- Modify: `src/keymap.lisp`
- Create: `tests/keymap-test.lisp`

- [ ] **Step 1: Write failing tests for keymap**

In `tests/keymap-test.lisp`:

```lisp
(in-package :clawmacs/tests)

(def-suite keymap-suite
  :description "Keymap tests"
  :in clawmacs-suite)

(in-suite keymap-suite)

(test keymap-bind-and-lookup
  "Binding a key and looking it up returns the command."
  (let ((km (make-keymap :test)))
    (keymap-bind km #\a 'some-command)
    (is (eq 'some-command (keymap-lookup km #\a)))
    (is (null (keymap-lookup km #\b)))))

(test keymap-parent-chain-lookup
  "Lookup falls back to parent keymap."
  (let* ((parent (make-keymap :parent))
         (child (make-keymap :child :parent parent)))
    (keymap-bind parent #\a 'parent-command)
    (keymap-bind child #\b 'child-command)
    ;; Child finds its own binding
    (is (eq 'child-command (keymap-lookup child #\b)))
    ;; Child falls back to parent
    (is (eq 'parent-command (keymap-lookup child #\a)))
    ;; Parent doesn't see child binding
    (is (null (keymap-lookup parent #\b)))))

(test keymap-child-overrides-parent
  "Child bindings shadow parent bindings for the same key."
  (let* ((parent (make-keymap :parent))
         (child (make-keymap :child :parent parent)))
    (keymap-bind parent #\a 'parent-version)
    (keymap-bind child #\a 'child-version)
    (is (eq 'child-version (keymap-lookup child #\a)))
    (is (eq 'parent-version (keymap-lookup parent #\a)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Expected: FAIL -- `make-keymap` undefined.

- [ ] **Step 3: Implement keymap class and lookup**

In `src/keymap.lisp`:

```lisp
(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Keymap
;;; --------------------------------------------------------------------------

(defclass keymap ()
  ((name     :initarg :name
             :reader keymap-name
             :type keyword)
   (bindings :initarg :bindings
             :reader keymap-bindings
             :type hash-table)
   (parent   :initarg :parent
             :reader keymap-parent
             :initform nil
             :type (or null keymap)))
  (:documentation "A key-to-command mapping with optional parent chain for fallback."))

(declaim (ftype (function (keyword &key (:parent (or null keymap))) keymap) make-keymap))
(defun make-keymap (name &key parent)
  "Create a new empty keymap with NAME and optional PARENT for fallback."
  (make-instance 'keymap
    :name name
    :bindings (make-hash-table :test #'equal)
    :parent parent))

(declaim (ftype (function (keymap t symbol) keymap) keymap-bind))
(defun keymap-bind (keymap key command)
  "Bind KEY to COMMAND in KEYMAP. KEY can be a character, keyword, or
a list for multi-key sequences (e.g., '(:ctrl #\\c) for C-c)."
  (setf (gethash key (keymap-bindings keymap)) command)
  keymap)

(declaim (ftype (function (keymap t) (or null symbol)) keymap-lookup))
(defun keymap-lookup (keymap key)
  "Look up KEY in KEYMAP, walking the parent chain if not found."
  (or (gethash key (keymap-bindings keymap))
      (when (keymap-parent keymap)
        (keymap-lookup (keymap-parent keymap) key))))

;;; --------------------------------------------------------------------------
;;; Default Keymap
;;; --------------------------------------------------------------------------

(defvar *default-keymap* nil
  "The default keymap for chat buffers. Initialized by init-default-keymap.")

(defun init-default-keymap ()
  "Build and install the default keymap with standard chat buffer bindings."
  (let ((km (make-keymap :default)))
    ;; RET / C-m -> send-message
    (keymap-bind km #\Return 'send-message)
    ;; C-j -> insert-newline-command
    (keymap-bind km #\Linefeed 'insert-newline-command)
    ;; C-a -> beginning-of-line-command
    (keymap-bind km #\Soh 'beginning-of-line-command)     ; C-a = #\Soh (ASCII 1)
    ;; C-e -> end-of-line-command
    (keymap-bind km #\Enq 'end-of-line-command)           ; C-e = #\Enq (ASCII 5)
    ;; C-k -> kill-line-command
    (keymap-bind km #\Vt 'kill-line-command)              ; C-k = #\Vt  (ASCII 11)
    ;; C-y -> yank-command
    (keymap-bind km #\Em 'yank-command)                   ; C-y = #\Em  (ASCII 25)
    ;; C-d -> delete-char-forward (not in initial spec, but standard)
    ;; Backspace / DEL -> delete-char-backward-command
    (keymap-bind km #\Backspace 'delete-char-backward-command)
    (keymap-bind km #\Rubout 'delete-char-backward-command)
    (setf *default-keymap* km)))
```

**Note:** `C-c C-c` for quit is a two-key sequence. Implementing multi-key prefix dispatch is deferred -- for this iteration, quit can be bound to a single key. We use the ncurses character codes directly (e.g., `#\Soh` = ASCII 1 = C-a). The exact character values may need adjustment when integrated with croatoan's event system, which may represent control characters differently.

- [ ] **Step 4: Run tests to verify they pass**

Run the test runner. Expected: 3 keymap tests pass.

- [ ] **Step 5: Verify full system loads**

Run the system load command. Expected: no errors.

- [ ] **Step 6: Commit**

```bash
git add src/keymap.lisp tests/keymap-test.lisp
git commit -m "feat(keymap): Add keymap with parent-chain lookup and default bindings

Implement keymaps as hash tables with parent chain for fallback
lookup. keymap-bind maps keys to command symbols, keymap-lookup
walks the chain. The default keymap binds standard editing keys
(C-a, C-e, C-k, C-y, backspace) and chat keys (RET to send,
C-j for newline)."
```

---

### Task 7: Define Chat Commands

**Files:**
- Modify: `src/main.lisp` (for now, commands live here since they wire together buffer ops + agent stub)

- [ ] **Step 1: Define all chat commands using defcommand**

In `src/main.lisp`:

```lisp
(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Self-insert support (must be defined before commands that reference it)
;;; --------------------------------------------------------------------------

(defvar *self-insert-char* nil
  "The character to insert for self-insert-command. Bound by the event loop.")

;;; --------------------------------------------------------------------------
;;; Stub Agent
;;; --------------------------------------------------------------------------

(declaim (ftype (function (buffer) buffer) send-to-agent-with-context))
(defun send-to-agent-with-context (buf)
  "Stub agent: echoes the last user message back as an agent message.
Assigns the agent's face-set from the buffer's face registry."
  (let* ((input (buffer-input-message buf))
         ;; The last finalized message is the one before the new input
         (user-msg (message-prev input))
         (user-text (if user-msg (message-text user-msg) ""))
         (agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (agent-msg (buffer-insert-agent-message buf (format nil "Echo: ~A" user-text))))
    ;; Assign the agent's face-set to the new message
    (setf (message-face-set agent-msg)
          (gethash agent-kw (buffer-face-registry buf))))
  buf)

;;; --------------------------------------------------------------------------
;;; Commands
;;; --------------------------------------------------------------------------

(defcommand send-message (:permission :user-only :keys (#\Return))
  "Send the current input message to the agent."
  (buffer)
  (let ((input-text (message-text (buffer-input-message buffer))))
    ;; Only send if there's actual content
    (when (plusp (length (string-trim '(#\Space #\Tab #\Newline) input-text)))
      (buffer-finalize-input buffer)
      ;; Assign user face-set to the new input message
      (setf (message-face-set (buffer-input-message buffer))
            (gethash :user (buffer-face-registry buffer)))
      (send-to-agent-with-context buffer))))

(defcommand insert-newline-command (:permission :user-only :keys (#\Linefeed))
  "Insert a newline in the input message."
  (buffer)
  (message-insert-newline (buffer-input-message buffer)))

(defcommand beginning-of-line-command (:permission :user-only)
  "Move point to the beginning of the current line."
  (buffer)
  (message-move-beginning-of-line (buffer-input-message buffer)))

(defcommand end-of-line-command (:permission :user-only)
  "Move point to the end of the current line."
  (buffer)
  (message-move-end-of-line (buffer-input-message buffer)))

(defcommand kill-line-command (:permission :user-only)
  "Kill from point to the end of the line."
  (buffer)
  (message-kill-line (buffer-input-message buffer)))

(defcommand yank-command (:permission :user-only)
  "Yank the top of the kill ring at point."
  (buffer)
  (message-yank (buffer-input-message buffer)))

(defcommand delete-char-backward-command (:permission :user-only)
  "Delete the character before point."
  (buffer)
  (message-delete-char-backward (buffer-input-message buffer)))

(defcommand self-insert-command (:permission :user-only)
  "Insert a character at point. The character is passed via *self-insert-char*."
  (buffer)
  (when *self-insert-char*
    (message-insert-char (buffer-input-message buffer) *self-insert-char*)))
```

- [ ] **Step 2: Verify it loads**

Run the system load command. Expected: no errors.

- [ ] **Step 3: Commit**

```bash
git add src/main.lisp
git commit -m "feat(commands): Define chat commands and stub agent

Define all initial chat commands via defcommand: send-message,
insert-newline, beginning/end of line, kill-line, yank, backward
delete, and self-insert. The stub agent echoes user input back
as an agent message prefixed with 'Echo: '."
```

---

### Task 8: Rendering

**Files:**
- Modify: `src/render.lisp`

- [ ] **Step 1: Implement the default face sets**

In `src/render.lisp`:

```lisp
(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Default Face Definitions
;;; --------------------------------------------------------------------------

(defun make-default-user-face-set ()
  "Create the default face set for user messages.
Background: CGA dark-gray (#8), foreground: white."
  (make-face-set
   :user
   (list (make-instance 'face
           :name :default
           :background (make-color-spec :cga 8)
           :foreground (make-color-spec :cga 15)
           :bold-p nil
           :underline-p nil
           :reverse-p nil))))

(defun make-default-agent-face-set (agent-keyword)
  "Create the default face set for an agent.
Background: black (#0), foreground: white."
  (make-face-set
   agent-keyword
   (list (make-instance 'face
           :name :default
           :background (make-color-spec :cga 0)
           :foreground (make-color-spec :cga 15)
           :bold-p nil
           :underline-p nil
           :reverse-p nil))))

(defun make-modeline-face ()
  "Create the modeline face. Background: CGA white (#7), foreground: black, bold."
  (make-instance 'face
    :name :modeline
    :background (make-color-spec :cga 7)
    :foreground (make-color-spec :cga 0)
    :bold-p t
    :underline-p nil
    :reverse-p nil))
```

- [ ] **Step 2: Implement croatoan color helper**

Append to `src/render.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Croatoan Color Helpers
;;; --------------------------------------------------------------------------

(defun color-spec-to-croatoan (cs)
  "Convert a color-spec to a croatoan color keyword or integer.
CGA colors map to ncurses standard colors. 256 and hex use extended colors."
  (ecase (color-spec-type cs)
    (:cga
     ;; Map CGA 0-7 to ncurses standard colors, 8-15 to bright variants
     (let ((val (color-spec-value cs)))
       (case val
         (0  :black)
         (1  :red)
         (2  :green)
         (3  :yellow)
         (4  :blue)
         (5  :magenta)
         (6  :cyan)
         (7  :white)
         ;; 8-15: bright colors — use the integer directly for 256-color mode
         (otherwise val))))
    (:256
     (color-spec-value cs))
    (:hex
     ;; For now, fall back to white. Proper hex->256 mapping is a future task.
     :white)))

(defun apply-face-to-window (window resolved-face)
  "Set WINDOW's color pair and attributes from RESOLVED-FACE."
  (let ((fg (color-spec-to-croatoan (resolved-face-foreground resolved-face)))
        (bg (color-spec-to-croatoan (resolved-face-background resolved-face))))
    (setf (croatoan:color-pair window) (list fg bg))
    (setf (croatoan:attributes window)
          (remove nil
                  (list (when (resolved-face-bold-p resolved-face) :bold)
                        (when (resolved-face-underline-p resolved-face) :underline)
                        (when (resolved-face-reverse-p resolved-face) :reverse))))))
```

- [ ] **Step 3: Implement render-modeline**

Append to `src/render.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Modeline Rendering
;;; --------------------------------------------------------------------------

(defun format-modeline (buf width)
  "Format the modeline string for BUF, fitting within WIDTH columns.
Left-aligned: buffer-name | agent-name | working-directory
Right-aligned: token-count/context-limit | status"
  (let* ((left (format nil " ~A | ~A | ~A"
                       (buffer-name buf)
                       (buffer-agent-name buf)
                       (namestring (buffer-working-directory buf))))
         (right (format nil "~A/~A | ~A "
                        (buffer-token-count buf)
                        (buffer-context-limit buf)
                        (buffer-status buf)))
         (padding (max 1 (- width (length left) (length right))))
         (padded (format nil "~A~V,,,A~A" left padding #\Space "" right)))
    ;; Truncate or pad to exact width
    (if (>= (length padded) width)
        (subseq padded 0 width)
        (format nil "~A~V,,,A" padded (- width (length padded)) #\Space ""))))

(defun render-modeline (buf modeline-window)
  "Render the modeline for BUF into MODELINE-WINDOW."
  (let* ((width (croatoan:width modeline-window))
         (text (format-modeline buf width))
         (ml-face (make-modeline-face))
         (resolved (resolve-face ml-face)))
    (apply-face-to-window modeline-window resolved)
    (croatoan:clear modeline-window)
    (croatoan:move modeline-window 0 0)
    (croatoan:add-string modeline-window text)
    (croatoan:refresh modeline-window)))
```

- [ ] **Step 4: Implement render-history and render-input**

Append to `src/render.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Message Rendering
;;; --------------------------------------------------------------------------

(defun message-sender-prefix (msg)
  "Return the display prefix for MSG's sender (e.g., 'user> ', 'agent> ')."
  (format nil "~A> " (string-downcase (symbol-name (message-sender msg)))))

(defun render-message-lines (window msg start-row width &key show-cursor)
  "Render MSG's lines into WINDOW starting at START-ROW.
Returns the number of rows consumed.
If SHOW-CURSOR is true, positions the cursor at MSG's point."
  (let* ((prefix (message-sender-prefix msg))
         (prefix-len (length prefix))
         (face-set (message-face-set msg))
         (face (if face-set
                   (or (get-face face-set :default) (make-modeline-face))
                   (make-modeline-face)))
         (resolved (resolve-face face))
         (row start-row)
         (cursor-y nil)
         (cursor-x nil))
    (apply-face-to-window window resolved)
    (loop :for line := (message-first-line msg) :then (line-next line)
          :for line-idx :from 0
          :while (and line (< row (croatoan:height window)))
          :do
             (croatoan:move window row 0)
             ;; Fill entire row with background color
             (croatoan:add-string window (make-string width :initial-element #\Space))
             (croatoan:move window row 0)
             ;; Show prefix on first line only
             (when (= line-idx 0)
               (croatoan:add-string window prefix))
             ;; Show content (indented by prefix-len on continuation lines)
             ;; NOTE: Lines longer than display-width are truncated, not wrapped.
             ;; Line wrapping is deferred to a future iteration.
             (let* ((indent (if (= line-idx 0) 0 prefix-len))
                    (content (line-content line))
                    (display-width (- width prefix-len))
                    (truncated (if (> (length content) display-width)
                                   (subseq content 0 display-width)
                                   content)))
               (when (> line-idx 0)
                 (croatoan:move window row indent))
               (croatoan:add-string window truncated))
             ;; Track cursor position
             (when (and show-cursor (eq line (message-point-line msg)))
               (setf cursor-y row
                      cursor-x (+ prefix-len (message-point-offset msg))))
             (incf row))
    ;; Position cursor if this is the input message
    (when (and show-cursor cursor-y cursor-x)
      (croatoan:move window cursor-y (min cursor-x (1- width))))
    (- row start-row)))

;;; --------------------------------------------------------------------------
;;; Buffer Rendering
;;; --------------------------------------------------------------------------

(defun calculate-input-height (buf terminal-height)
  "Calculate the height of the input area in rows.
Minimum 3, maximum (floor terminal-height 3)."
  (let* ((input (buffer-input-message buf))
         (line-count (message-line-count input))
         (min-height 3)
         (max-height (floor terminal-height 3)))
    (max min-height (min line-count max-height))))

(defun render-buffer (buf main-window modeline-window)
  "Render the entire buffer: history + input into MAIN-WINDOW, modeline into
MODELINE-WINDOW."
  (let* ((total-height (croatoan:height main-window))
         (width (croatoan:width main-window))
         (input-height (calculate-input-height buf total-height))
         (history-height (- total-height input-height))
         (input-start-row history-height))
    ;; Clear the main window
    (croatoan:clear main-window)

    ;; Collect history messages (all except the input message)
    (let ((history-messages nil))
      (loop :for msg := (buffer-first-message buf) :then (message-next msg)
            :while (and msg (not (eq msg (buffer-input-message buf))))
            :do (push msg history-messages))
      (setf history-messages (nreverse history-messages))

      ;; Render history from bottom up to fill history-height
      ;; Walk backwards through messages, accumulating rows needed
      (let ((rows-used 0)
            (messages-to-render nil))
        ;; Figure out which messages fit
        (loop :for msg :in (reverse history-messages)
              :for msg-lines := (message-line-count msg)
              :while (<= (+ rows-used msg-lines) history-height)
              :do (incf rows-used msg-lines)
                  (push msg messages-to-render))
        ;; Render the messages that fit, starting from the top
        (let ((row (- history-height rows-used)))
          (dolist (msg messages-to-render)
            (let ((consumed (render-message-lines main-window msg row width)))
              (incf row consumed))))))

    ;; Render input message
    (render-message-lines main-window (buffer-input-message buf)
                          input-start-row width
                          :show-cursor t)

    (croatoan:refresh main-window)

    ;; Render modeline
    (render-modeline buf modeline-window)))
```

- [ ] **Step 5: Write tests for pure render helpers**

Create `tests/render-test.lisp`:

```lisp
(in-package :clawmacs/tests)

(def-suite render-suite
  :description "Rendering helper tests"
  :in clawmacs-suite)

(in-suite render-suite)

(test format-modeline-basic
  "format-modeline produces a left/right aligned string."
  (let* ((buf (make-buffer "test:session" :agent-name "claude"
                           :working-directory #P"/tmp/")))
    (setf (buffer-token-count buf) 1000
          (buffer-context-limit buf) 200000
          (buffer-status buf) :idle)
    (let ((ml (clawmacs::format-modeline buf 60)))
      (is (= 60 (length ml)))
      ;; Left side should contain buffer name
      (is (search "test:session" ml))
      ;; Right side should contain token count
      (is (search "1000/200000" ml))
      ;; Right side should contain status
      (is (search "idle" ml)))))

(test calculate-input-height-minimum
  "Input height is at least 3 rows."
  (let ((buf (make-buffer "test")))
    ;; 1-line input, should still get 3 rows
    (is (= 3 (clawmacs::calculate-input-height buf 30)))))

(test calculate-input-height-maximum
  "Input height is capped at (floor terminal-height 3)."
  (let ((buf (make-buffer "test")))
    ;; Add many lines to the input message
    (dotimes (i 20)
      (message-insert-newline (buffer-input-message buf)))
    ;; With terminal height 30, max should be 10
    (is (= 10 (clawmacs::calculate-input-height buf 30)))))
```

**Note:** `format-modeline` and `calculate-input-height` are internal functions tested via `clawmacs::` double-colon access. This is acceptable for testing pure helpers.

Also add `(:file "render-test")` to the test system in `clawmacs.asd`.

- [ ] **Step 6: Run tests to verify they pass**

Run the test runner. Expected: 3 render tests pass.

- [ ] **Step 7: Verify full system loads**

Run the system load command. Expected: no errors (croatoan must be installed).

- [ ] **Step 8: Commit**

```bash
git add src/render.lisp tests/render-test.lisp
git commit -m "feat(render): Add croatoan rendering for buffer, modeline, and input

Implement rendering with croatoan: render-buffer drives the full
display cycle -- history messages fill from the bottom of the
history area, the input message renders below with cursor, and
the modeline occupies the bottom row. Face sets control per-sender
background colors. format-modeline produces the left/right aligned
status line. Color-spec values are mapped to croatoan color pairs."
```

---

### Task 9: Event Loop and Entry Point

**Files:**
- Modify: `src/main.lisp` (append event loop and entry point)

- [ ] **Step 1: Implement the event loop and entry point**

Append to `src/main.lisp`:

```lisp
;;; --------------------------------------------------------------------------
;;; Face Registry Setup
;;; --------------------------------------------------------------------------

(defun init-face-registry (buf)
  "Populate BUF's face registry with default face sets."
  (let* ((registry (buffer-face-registry buf))
         (agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (user-fs (make-default-user-face-set))
         (agent-fs (make-default-agent-face-set agent-kw)))
    (setf (gethash :user registry) user-fs
          (gethash agent-kw registry) agent-fs)
    ;; Set face-set on the input message
    (setf (message-face-set (buffer-input-message buf)) user-fs)
    buf))

;;; --------------------------------------------------------------------------
;;; Event Loop
;;; --------------------------------------------------------------------------

(defun handle-key-event (buf event)
  "Dispatch a key event through the buffer's keymap.
Returns :QUIT if the application should exit, or nil otherwise."
  (let ((*current-caller* :user))
    (cond
      ;; Check for C-c C-c (quit) -- simplified as just C-c for now
      ((and (characterp event) (char= event #\Etx))  ; C-c = ASCII 3
       :quit)

      ;; Look up in keymap
      ((and (characterp event) (keymap-lookup (buffer-keymap buf) event))
       (let ((command (keymap-lookup (buffer-keymap buf) event)))
         (funcall command buf)
         nil))

      ;; Self-insert for printable characters
      ((and (characterp event)
            (graphic-char-p event))
       (let ((*self-insert-char* event))
         (self-insert-command buf))
       nil)

      ;; Unbound key: ignore
      (t nil))))

(defun clawmacs-main (&key (session-name "clawmacs:session-01")
                           (agent-name "echo-agent"))
  "Entry point for clawmacs. Initializes the TUI and runs the event loop."
  ;; Initialize the default keymap
  (init-default-keymap)

  (croatoan:with-screen (scr :input-echoing nil
                             :input-blocking t
                             :cursor-visible t
                             :enable-colors t)
    (let* ((screen-height (croatoan:height scr))
           (screen-width (croatoan:width scr))
           ;; Main window: everything except the bottom modeline row
           (main-win (make-instance 'croatoan:window
                       :height (1- screen-height)
                       :width screen-width
                       :position '(0 0)))
           ;; Modeline window: 1 row at the bottom
           (modeline-win (make-instance 'croatoan:window
                           :height 1
                           :width screen-width
                           :position (list (1- screen-height) 0)))
           ;; Create the buffer
           (buf (make-buffer session-name
                             :agent-name agent-name
                             :working-directory (truename "."))))
      ;; Set up faces and keymap
      (init-face-registry buf)
      (setf (buffer-keymap buf) *default-keymap*)

      ;; Initial render
      (render-buffer buf main-win modeline-win)

      ;; Event loop
      (croatoan:event-case (scr event)
        ;; Terminal resize
        (:resize
         (let ((new-height (croatoan:height scr))
               (new-width (croatoan:width scr)))
           (croatoan:resize main-win (1- new-height) new-width)
           (croatoan:resize modeline-win 1 new-width)
           (croatoan:move-window modeline-win (1- new-height) 0)
           (render-buffer buf main-win modeline-win)))

        ;; All other events
        (otherwise
         (let ((result (handle-key-event buf event)))
           (when (eq result :quit)
             (return-from croatoan:event-case))
           ;; Re-render after any key
           (render-buffer buf main-win modeline-win)))))))
```

- [ ] **Step 2: Verify the full system loads**

```bash
sbcl --noinform --non-interactive \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :clawmacs)' \
  --eval '(format t "~%Full system loaded successfully.~%")'
```

Expected: `Full system loaded successfully.`

- [ ] **Step 3: Run all tests**

Run the test runner command. Expected: all tests pass.

- [ ] **Step 4: Manual smoke test**

Run the application:

```bash
sbcl --noinform \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :clawmacs)' \
  --eval '(clawmacs:clawmacs-main)'
```

Verify:
1. The TUI appears with an empty chat area and modeline at the bottom
2. Typing characters appears in the input area
3. Pressing RET sends the message and an "Echo: " response appears
4. C-a and C-e move to beginning/end of line
5. C-k kills to end of line
6. C-y yanks killed text
7. C-j inserts a newline in the input
8. Backspace deletes backward
9. C-c exits the application
10. The modeline shows session name, agent, directory, token count, and "idle"
11. User messages have dark-gray background, agent messages have black background

- [ ] **Step 5: Commit**

```bash
git add src/main.lisp
git commit -m "feat: Add event loop, entry point, and stub agent integration

Complete the first iteration: the event loop dispatches key events
through the keymap, falling back to self-insert for printable
characters. C-c exits. The stub agent echoes user input. Face
registry initialization assigns per-sender colors. The entry
point clawmacs-main creates the croatoan screen, windows, buffer,
and runs the loop."
```

- [ ] **Step 6: Run all tests one final time**

Run the test runner. Expected: all tests pass.

- [ ] **Step 7: Final commit (if any adjustments were needed)**

```bash
git add -A
git commit -m "chore: Final adjustments from integration testing"
```
