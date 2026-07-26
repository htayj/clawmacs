(in-package :clawmacs)

;;; Appearance preferences are deliberately data only.  In particular, this
;;; file never resolves package declarations or creates symbols from external
;;; owner/local text; package identifiers remain tagged lists until a later
;;; catalog transaction knows how to resolve them.

(defparameter +appearance-config-max-bytes+ 32768)
(defparameter +appearance-config-max-depth+ 16)
(defparameter +appearance-config-max-list-length+ 64)
(defparameter +appearance-config-max-string-length+ 1024)
(defparameter +appearance-config-max-overrides+ 64)
(defparameter +appearance-selector-max-bytes+ 4096)

(defvar *appearance-config-directory*
  (merge-pathnames #P".clawmacs.d/" (user-homedir-pathname)))
(defvar *appearance-config-path*
  (merge-pathnames #P"appearance.sexp" *appearance-config-directory*))
(defvar *appearance-cli-selector* nil)
(defvar *appearance-startup-resolution-count* 0)
(defvar *appearance-configuration-access-count* 0)
(defvar *appearance-config-rename-function* #'rename-file)

(defun appearance-config-pathname () *appearance-config-path*)

(defun appearance-config-error (control &rest arguments)
  (error (apply #'format nil control arguments)))

(defun proper-bounded-list-p (value limit)
  (and (listp value) (<= (length value) limit)))

(defun appearance-config-shape-valid-p (value depth)
  "Check reader output before schema validation, with no executable values."
  (when (> depth +appearance-config-max-depth+)
    (appearance-config-error "Appearance data exceeds reader depth limit."))
  (cond ((or (null value) (eq value t)) t)
        ((or (stringp value) (numberp value) (keywordp value))
         (when (and (stringp value)
                    (> (length value) +appearance-config-max-string-length+))
           (appearance-config-error "Appearance string exceeds limit."))
         t)
        ((consp value)
         (unless (proper-bounded-list-p value +appearance-config-max-list-length+)
           (appearance-config-error "Appearance list is malformed or exceeds limit."))
         (dolist (part value t)
           (appearance-config-shape-valid-p part (1+ depth))))
        (t (appearance-config-error "Unsupported appearance datum: ~S" value))))

(defparameter +appearance-config-keywords+
  '(:clawmacs-appearance :version :theme :strict-contrast :overrides :package
    :foreground :background :typography :decoration :rgb :portable :enumerated
    :family :face :size :marker :none :selection-marker
    :black :white :red :green :blue :cyan :magenta :yellow :gray
    :classic :dark :base :transcript-pane :info-pane :compose-pane
    :minibuffer-pane :help-pane :pointer-documentation :default-text
    :transcript-user :transcript-agent :transcript-tool :transcript-system
    :transcript-empty :system :error :tool-result :modeline :selector-title
    :selector-header :selector-entry :selector-separator :selector-footer
    :selector-selection :minibuffer-selection-emphasis :disabled
    :fix :roman :bold :italic :tiny :very-small :small :normal :large
    :very-large :huge :smaller :larger))

(defun appearance-utf8-character-bytes (character)
  (let ((code (char-code character)))
    (cond ((<= code #x7f) 1)
          ((<= code #x7ff) 2)
          ((<= code #xffff) 3)
          (t 4))))

(defun appearance-token-delimiter-p (character)
  (or (member character '(#\( #\) #\" #\;) :test #'char=)
      (member character '(#\Space #\Tab #\Newline #\Return #\Page)
              :test #'char=)))

(defun appearance-number-token-p (token)
  "Recognize a numeric token without exposing arbitrary text to the reader."
  (and (some #'digit-char-p token)
       (every (lambda (character)
                (or (digit-char-p character)
                    (find character "+-./eEdDfFlLsS" :test #'char=)))
              token)
       (handler-case
           (multiple-value-bind (value end)
               (let ((*read-eval* nil)) (read-from-string token))
             (and (= end (length token)) (numberp value)))
         (error () nil))))

(defun validate-appearance-lexical-bounds (text limit)
  "Validate byte, nesting, list, string, and token bounds before READ.

The scan is string/comment/escape aware.  It accepts only known keywords,
booleans, and numeric tokens, so READ cannot intern external names."
  (let ((bytes 0) (depth 0) (list-counts nil)
        (length (length text)) (index 0))
    (labels ((count-byte (character)
               (incf bytes (appearance-utf8-character-bytes character))
               (when (> bytes limit)
                 (appearance-config-error "Appearance data exceeds byte limit.")))
             (count-item ()
               (when list-counts
                 (incf (car list-counts))
                 (when (> (car list-counts) +appearance-config-max-list-length+)
                   (appearance-config-error "Appearance list exceeds limit."))))
             (consume-token ()
               (let ((start index))
                 (loop while (and (< index length)
                                  (not (appearance-token-delimiter-p
                                        (char text index))))
                       do (count-byte (char text index)) (incf index))
                 (let ((token (subseq text start index)))
                   (cond
                     ((and (> (length token) 1) (char= (char token 0) #\:))
                      (unless (member
                               (find-symbol (string-upcase (subseq token 1))
                                            :keyword)
                               +appearance-config-keywords+ :test #'eq)
                        (appearance-config-error
                         "Unknown appearance token ~A." token)))
                     ((member (string-downcase token) '("nil" "t")
                              :test #'string=))
                     ((appearance-number-token-p token))
                     (t
                      (appearance-config-error
                       "Unknown bare appearance token ~A." token)))))))
      (loop while (< index length) do
        (let ((character (char text index)))
          (cond
            ((char= character #\;)
             (loop while (and (< index length)
                              (not (char= (char text index) #\Newline)))
                   do (count-byte (char text index)) (incf index)))
            ((member character '(#\Space #\Tab #\Newline #\Return #\Page)
                     :test #'char=)
             (count-byte character) (incf index))
            ((char= character #\()
             (count-byte character)
             (count-item)
             (incf depth)
             (when (> depth +appearance-config-max-depth+)
               (appearance-config-error
                "Appearance data exceeds reader depth limit."))
             (push 0 list-counts)
             (incf index))
            ((char= character #\))
             (count-byte character)
             (when (zerop depth)
               (appearance-config-error "Unbalanced appearance list."))
             (pop list-counts)
             (decf depth)
             (incf index))
            ((char= character #\")
             (count-item)
             (count-byte character)
             (incf index)
             (let ((string-characters 0) (string-bytes 0) (closed-p nil))
               (loop while (< index length) do
                 (let ((inner (char text index)))
                   (count-byte inner)
                   (cond
                     ((char= inner #\")
                      (incf index) (setf closed-p t) (return))
                     ((char= inner #\\)
                      (incf index)
                      (when (>= index length)
                        (appearance-config-error
                         "Unterminated appearance string escape."))
                      (let ((escaped (char text index)))
                        (count-byte escaped)
                        (incf string-characters)
                        (incf string-bytes
                              (appearance-utf8-character-bytes escaped))
                        (incf index)))
                     (t
                      (incf string-characters)
                      (incf string-bytes
                            (appearance-utf8-character-bytes inner))
                      (incf index)))
                   (when (or (> string-characters
                                +appearance-config-max-string-length+)
                             (> string-bytes
                                +appearance-config-max-string-length+))
                     (appearance-config-error
                      "Appearance string exceeds limit."))))
               (unless closed-p
                 (appearance-config-error
                  "Unterminated appearance string."))))
            ((or (char= character #\#) (char= character #\\))
             (appearance-config-error
              "Reader dispatch and escaped symbols are not allowed."))
            (t
             (count-item)
             (consume-token)))))
      (unless (zerop depth)
        (appearance-config-error "Unbalanced appearance list."))
      t)))

(defun read-one-appearance-form (text &key (limit +appearance-config-max-bytes+))
  "Read one inert bounded form after rejecting token forms that could intern."
  (validate-appearance-lexical-bounds text limit)
  (let ((*read-eval* nil))
    (with-input-from-string (stream text)
      (let ((form (read stream nil :appearance-eof)))
        (when (eq form :appearance-eof)
          (appearance-config-error "Appearance data is empty."))
        (when (not (eq (read stream nil :appearance-eof) :appearance-eof))
          (appearance-config-error "Appearance data contains trailing forms."))
        (appearance-config-shape-valid-p form 0)
        form))))

(defun ascii-lowercase-identifier-p (string)
  (and (stringp string) (plusp (length string))
       (every (lambda (character)
                (or (and (char>= character #\a) (char<= character #\z))
                    (and (char>= character #\0) (char<= character #\9))
                    (char= character #\-) (char= character #\.)))
              string)))

(defun valid-package-owner-p (owner)
  (and (stringp owner) (<= (length owner) 128)
       (ascii-lowercase-identifier-p owner)
       (let ((pieces (uiop:split-string owner :separator ".")))
         (and (= (length pieces) (length (remove-duplicates pieces :test #'string=)))
              (every (lambda (piece)
                       (and (plusp (length piece))
                            (char>= (char piece 0) #\a) (char<= (char piece 0) #\z)
                            (every (lambda (character)
                                     (or (and (char>= character #\a) (char<= character #\z))
                                         (and (char>= character #\0) (char<= character #\9))
                                         (char= character #\-)))
                                   piece)))
                     pieces)))))

(defun valid-package-local-p (local)
  (and (stringp local) (<= (length local) 128)
       (ascii-lowercase-identifier-p local)
       (char>= (char local 0) #\a) (char<= (char local 0) #\z)
       (not (find #\. local))))

(defun package-appearance-id-p (value)
  (and (proper-bounded-list-p value 3) (= (length value) 3)
       (eq (first value) :package)
       (valid-package-owner-p (second value))
       (valid-package-local-p (third value))))

(defun core-theme-id-p (value) (member value '(:classic :dark) :test #'eq))
(defun core-role-id-p (value)
  (member value '(:base :transcript-pane :info-pane :compose-pane :minibuffer-pane
                  :help-pane :pointer-documentation :default-text :transcript-user
                  :transcript-agent :transcript-tool :transcript-system
                  :transcript-empty :system :error :tool-result :modeline
                  :selector-title :selector-header :selector-entry
                  :selector-separator :selector-footer :selector-selection
                  :minibuffer-selection-emphasis :disabled)
          :test #'eq))

(defun valid-theme-id-p (value) (or (core-theme-id-p value) (package-appearance-id-p value)))
(defun valid-role-id-p (value) (or (core-role-id-p value) (package-appearance-id-p value)))

(defun plist-with-keys-p (value keys &key require-value)
  (and (proper-bounded-list-p value (* 2 (length keys))) (evenp (length value))
       (let ((seen nil))
         (loop for (key item) on value by #'cddr
               always (and (member key keys :test #'eq)
                           (not (member key seen :test #'eq))
                           (progn (push key seen) (or (not require-value) item)))))))

(defun finite-unit-real-p (value)
  (and (realp value)
       (handler-case (and (<= 0 value 1) (= value value)) (error () nil))))

(defun parse-appearance-ink (value)
  (cond ((member value '(:black :white :red :green :blue :cyan :magenta :yellow :gray)
                :test #'eq) value)
        ((and (proper-bounded-list-p value 4) (= (length value) 4)
              (eq (first value) :rgb)
              (every #'finite-unit-real-p (rest value)))
         (copy-tree value))
        (t (appearance-config-error "Invalid persisted appearance ink: ~S" value))))

(defun parse-appearance-typography (value)
  (let ((body (cond ((and (consp value) (eq (first value) :portable)) (rest value))
                    ((and (consp value) (eq (first value) :enumerated)) (rest value))
                    (t value))))
    (unless (plist-with-keys-p body '(:family :face :size))
      (appearance-config-error "Invalid persisted typography: ~S" value))
    (apply #'make-appearance-typography-spec body)))

(defun parse-appearance-decoration (value)
  (cond ((eq value :none) (make-appearance-decoration-spec :kind :none))
        ((eq value :selection-marker)
         (make-appearance-decoration-spec :kind :selection-marker))
        ((and (plist-with-keys-p value '(:marker))
              (stringp (getf value :marker)))
         (make-appearance-decoration-spec :kind :selection-marker :parameters value))
        (t (appearance-config-error "Invalid persisted decoration: ~S" value))))

(defun parse-appearance-axis-plist (value)
  (unless (and (plist-with-keys-p value '(:foreground :background :typography :decoration))
               (plusp (length value)))
    (appearance-config-error "Invalid appearance override: ~S" value))
  (make-appearance-role-style
   :foreground-ink (if (member :foreground value :test #'eq)
                       (make-appearance-ink-spec :foreground
                                                 (parse-appearance-ink (getf value :foreground)))
                       *appearance-unspecified*)
   :surface (if (member :background value :test #'eq)
                (make-appearance-surface-spec :background
                                              (parse-appearance-ink (getf value :background)))
                *appearance-unspecified*)
   :typography (if (member :typography value :test #'eq)
                   (parse-appearance-typography (getf value :typography))
                   *appearance-unspecified*)
   :decoration (if (member :decoration value :test #'eq)
                   (parse-appearance-decoration (getf value :decoration))
                   *appearance-unspecified*)))

(defun parse-appearance-overrides (value)
  (unless (and (proper-bounded-list-p value +appearance-config-max-overrides+)
               (<= (length value) +appearance-config-max-overrides+))
    (appearance-config-error "Invalid appearance override list."))
  (let ((seen nil))
    (mapcar (lambda (entry)
              (unless (and (consp entry) (valid-role-id-p (first entry)))
                (appearance-config-error "Invalid appearance override role: ~S" entry))
              (let ((role (first entry)) (style (parse-appearance-axis-plist (rest entry))))
                (when (member role seen :test #'equal)
                  (appearance-config-error "Duplicate appearance override role."))
                (push role seen)
                (cons (copy-tree role) style)))
            value)))

(defun parse-appearance-profile-form (form)
  (unless (and (consp form) (eq (first form) :clawmacs-appearance)
               (plist-with-keys-p (rest form) '(:version :theme :strict-contrast :overrides)))
    (appearance-config-error "Invalid appearance configuration schema."))
  (let ((clauses (rest form)))
  (unless (and (eql (getf clauses :version) 1) (valid-theme-id-p (getf clauses :theme)))
    (appearance-config-error "Unsupported appearance configuration version or theme."))
  (let ((strict (if (member :strict-contrast clauses :test #'eq)
                    (getf clauses :strict-contrast) nil))
        (overrides (if (member :overrides clauses :test #'eq) (getf clauses :overrides) nil)))
    (unless (typep strict 'boolean)
      (appearance-config-error "Appearance strict contrast must be boolean."))
    (make-appearance-profile :selected-theme (copy-tree (getf clauses :theme))
                             :strict-contrast strict
                             :role-overrides (parse-appearance-overrides overrides)))))

(defun read-appearance-profile-file (&optional (path (appearance-config-pathname)))
  "Read one valid profile.  A missing default file is reported as :MISSING."
  (incf *appearance-configuration-access-count*)
  (if (not (probe-file path))
      (values nil :missing)
      (let ((bytes (with-open-file (stream path :element-type '(unsigned-byte 8))
                     (file-length stream))))
        (when (> bytes +appearance-config-max-bytes+)
          (appearance-config-error "Appearance file exceeds byte limit."))
        (let ((profile (parse-appearance-profile-form
                        (read-one-appearance-form (uiop:read-file-string path)))))
          (values profile :valid)))))

(defun appearance-id-external-string (id)
  (cond ((keywordp id) (string-downcase (symbol-name id)))
        ((package-appearance-id-p id) (format nil "~A/~A" (second id) (third id)))
        (t (appearance-config-error "Cannot serialize appearance identifier: ~S" id))))

(defun serialize-appearance-typography (spec)
  (let ((result nil))
    (dolist (pair (list (cons :family (appearance-typography-spec-family spec))
                        (cons :face (appearance-typography-spec-face spec))
                        (cons :size (appearance-typography-spec-size spec))))
      (unless (appearance-unspecified-p (cdr pair))
        (setf result (append result (list (car pair) (cdr pair))))))
    result))

(defun serialize-appearance-style (style)
  (let ((result nil) (ink (appearance-role-style-foreground-ink style))
        (surface (appearance-role-style-surface style))
        (type (appearance-role-style-typography style))
        (decoration (appearance-role-style-decoration style)))
    (unless (appearance-unspecified-p ink)
      (setf result (append result (list :foreground (appearance-ink-spec-foreground ink)))))
    (unless (appearance-unspecified-p surface)
      (setf result (append result (list :background (appearance-surface-spec-background surface)))))
    (unless (appearance-unspecified-p type)
      (setf result (append result (list :typography (serialize-appearance-typography type)))))
    (unless (appearance-unspecified-p decoration)
      (let ((parameters (appearance-decoration-spec-parameters decoration)))
        (setf result
              (append result
                      (list :decoration
                            (if (appearance-unspecified-p parameters)
                                (appearance-decoration-spec-kind decoration)
                                parameters))))))
    result))

(defun serialize-appearance-profile (profile)
  (with-output-to-string (stream)
    (let ((*print-pretty* t) (*print-case* :downcase))
      (write (list :clawmacs-appearance :version 1
                   :theme (appearance-profile-selected-theme profile)
                   :strict-contrast (appearance-profile-strict-contrast profile)
                   :overrides (mapcar (lambda (entry)
                                        (cons (car entry) (serialize-appearance-style (cdr entry))))
                                      (appearance-profile-role-overrides profile)))
             :stream stream :readably t)
      (terpri stream))))

(defun write-appearance-profile-file (profile &optional (path (appearance-config-pathname)))
  "Explicit atomic save.  No caller or startup path invokes this automatically."
  (let* ((text (serialize-appearance-profile profile))
         (directory (uiop:pathname-directory-pathname path))
         (temporary (merge-pathnames (format nil ".appearance-~D.tmp" (random most-positive-fixnum)) directory)))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction :output :if-exists :supersede
                                              :external-format :utf-8)
             (write-string text stream)
             (finish-output stream))
           (funcall *appearance-config-rename-function* temporary path)
           path)
      (when (probe-file temporary) (ignore-errors (delete-file temporary))))))

(defun parse-appearance-theme-selector (text)
  "Convert external core or owner/local syntax without interning TEXT."
  (unless (and (stringp text) (plusp (length text))
               (<= (length (flexi-streams:string-to-octets text :external-format :utf-8)) 257))
    (appearance-config-error "Invalid appearance theme selector."))
  (cond ((member text '("classic" "dark") :test #'string=)
         (if (string= text "classic") :classic :dark))
        ((= 1 (count #\/ text))
         (let ((parts (uiop:split-string text :separator "/")))
           (let ((id (list :package (first parts) (second parts))))
             (unless (package-appearance-id-p id)
               (appearance-config-error "Invalid package appearance selector."))
             id)))
        (t (appearance-config-error "Invalid appearance theme selector."))))

(defun parse-appearance-startup-arguments (&optional (args (uiop:command-line-arguments)))
  "Parse only the spaced theme selector; all appearance CLI errors are local."
  (let ((values nil) (invalid nil) (tail args))
    (loop while tail for arg = (pop tail) do
      (cond ((string= arg "--appearance-theme")
             (if (or (null tail) (null (first tail)))
                 (setf invalid t)
                 (push (pop tail) values)))
            ((and (>= (length arg) 19)
                  (string= arg "--appearance-theme=" :end1 19 :end2 19))
             (setf invalid t))))
    (when (> (length values) 1) (setf invalid t))
    (if invalid
        (list :valid-p nil :diagnostic "Invalid appearance command-line selector.")
        (if (null values)
            (list :valid-p t :present-p nil)
            (handler-case
                (list :valid-p t :present-p t
                      :theme (parse-appearance-theme-selector (first values)))
              (error ()
                (list :valid-p nil
                      :diagnostic
                      "Invalid appearance command-line selector.")))))))

(defun parse-appearance-environment-selector ()
  (let ((value (uiop:getenv "CLAWMACS_APPEARANCE_THEME")))
    (cond ((null value) (list :present-p nil :valid-p t))
          (t (handler-case (list :present-p t :valid-p t
                                 :theme (parse-appearance-theme-selector value))
               (error () (list :present-p t :valid-p nil
                                :diagnostic "Invalid appearance environment selector.")))))))

(defun profile-with-theme (profile theme)
  (make-appearance-profile :selected-theme theme
                           :strict-contrast (appearance-profile-strict-contrast profile)
                           :role-overrides (appearance-profile-role-overrides profile)))

(defun resolve-startup-appearance-profile (&key (path (appearance-config-pathname))
                                                  (cli *appearance-cli-selector*)
                                                  (environment (parse-appearance-environment-selector)))
  "Compose file, environment, then CLI at the GUI-only startup boundary."
  (incf *appearance-startup-resolution-count*)
  (handler-case
      (multiple-value-bind (file-profile status) (read-appearance-profile-file path)
        (declare (ignore status))
        (let ((profile (or file-profile (make-appearance-profile))))
          (cond ((and (getf environment :present-p) (getf environment :valid-p))
                 (setf profile (profile-with-theme profile (getf environment :theme))))
                ((getf environment :present-p)
                 (warn "~A" (getf environment :diagnostic))))
          (cond ((and cli (getf cli :valid-p) (getf cli :present-p))
                 (setf profile (profile-with-theme profile (getf cli :theme))))
                ((and cli (not (getf cli :valid-p)))
                 (warn "~A" (getf cli :diagnostic))))
          profile))
    (error (condition)
      (warn "Appearance file ignored; starting with classic: ~A" condition)
      (make-appearance-profile))))

(defun reload-appearance-file-profile (active-profile &key (path (appearance-config-pathname)))
  "Return the newly loaded profile and T, or preserve ACTIVE-PROFILE and NIL."
  (handler-case
      (multiple-value-bind (profile status) (read-appearance-profile-file path)
        (if (eq status :valid) (values profile t) (values active-profile nil)))
    (error (condition)
      (warn "Appearance reload retained the active profile: ~A" condition)
      (values active-profile nil))))

(defun save-staged-appearance-profile (staged-profile &key (path (appearance-config-pathname)))
  "Persist a valid staged candidate without applying it to any frame."
  (unless (typep staged-profile 'appearance-profile)
    (appearance-config-error "Only an appearance profile may be saved."))
  (write-appearance-profile-file staged-profile path))
