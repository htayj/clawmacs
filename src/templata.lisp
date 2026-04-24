(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Prompt Templates And Slash Commands
;;; --------------------------------------------------------------------------

(defstruct prompt-template
  "A prompt template resource loaded from disk or registered programmatically."
  name
  description
  body
  path
  scope
  package)

(defstruct slash-command
  "A composer-level slash command."
  name
  description
  argument-hint
  handler
  package)

(defvar *slash-command-table* (make-hash-table :test #'equal)
  "Registry mapping normalized slash command names to SLASH-COMMAND entries.")

(defvar *prompt-template-user-directory*
  (merge-pathnames #P".clawmacs.d/prompts/" (user-homedir-pathname))
  "Global prompt-template directory.")

(defvar *prompt-template-project-directory-name* ".clawmacs/prompts/"
  "Project-relative prompt-template directory name.")

(defvar *prompt-template-package-directory-name* "prompts/"
  "Package-relative prompt-template directory name.")

(defun slash-command-whitespace-char-p (char)
  "Return true when CHAR separates slash command tokens."
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun normalize-slash-command-name (name)
  "Normalize slash command NAME to a lowercase string without the leading slash."
  (let* ((raw (string-trim '(#\Space #\Tab #\Newline #\Return) (string name)))
         (trimmed (if (and (plusp (length raw))
                           (char= (char raw 0) #\/))
                      (subseq raw 1)
                      raw)))
    (unless (plusp (length trimmed))
      (error "Slash command name must be non-empty."))
    (string-downcase trimmed)))

(defun parse-shellish-arguments (text)
  "Split TEXT into shell-like arguments supporting simple quotes and escapes."
  (let ((args nil)
        (current nil)
        (quote-char nil)
        (escape-next-p nil))
    (labels ((emit-current ()
               (when current
                 (push (coerce (nreverse current) 'string) args)
                 (setf current nil))))
      (loop :for char :across text
            :do (cond
                  (escape-next-p
                   (push char current)
                   (setf escape-next-p nil))
                  ((char= char #\\)
                   (setf escape-next-p t))
                  (quote-char
                   (if (char= char quote-char)
                       (setf quote-char nil)
                       (push char current)))
                  ((or (char= char #\")
                       (char= char #\'))
                   (setf quote-char char))
                  ((slash-command-whitespace-char-p char)
                   (emit-current))
                  (t
                   (push char current))))
      (when quote-char
        (error "Unterminated quoted slash argument."))
      (when escape-next-p
        (push #\\ current))
      (emit-current))
    (nreverse args)))

(defun parse-slash-command-line (text)
  "Return values NAME and ARGS for slash command TEXT, or NIL values.
ARGS is a list of shell-like arguments with simple quote handling."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (unless (and (plusp (length trimmed))
                 (char= (char trimmed 0) #\/))
      (return-from parse-slash-command-line (values nil nil)))
    (let ((split (position-if #'slash-command-whitespace-char-p trimmed)))
      (if (or (null split) (= split 1))
          (values (and (> (length trimmed) 1)
                       (normalize-slash-command-name trimmed))
                  nil)
          (values (normalize-slash-command-name (subseq trimmed 0 split))
                  (parse-shellish-arguments
                   (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                     (subseq trimmed split))))))))

(defun prompt-template-fallback-description (body name)
  "Return the first non-empty line of BODY, or NAME when BODY is blank."
  (or (loop :for line :in (split-lines body)
            :for trimmed := (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         line)
            :when (plusp (length trimmed))
              :return trimmed)
      name))

(defun parse-prompt-template-markdown (name contents &key path scope package)
  "Parse markdown template CONTENTS into a PROMPT-TEMPLATE."
  (multiple-value-bind (frontmatter-lines body)
      (extract-skill-frontmatter contents)
    (let* ((frontmatter (and frontmatter-lines
                             (parse-skill-frontmatter-lines frontmatter-lines)))
           (body-text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (or body contents)))
           (description (or (getf frontmatter :description)
                            (prompt-template-fallback-description body-text name))))
      (make-prompt-template
       :name (string-downcase (string name))
       :description description
       :body body-text
       :path path
       :scope scope
       :package package))))

(defun discover-prompt-templates-in-directory (directory &key scope package)
  "Load top-level markdown templates from DIRECTORY."
  (let ((dir (ignore-errors (uiop:ensure-directory-pathname directory))))
    (unless (and dir (probe-file dir))
      (return-from discover-prompt-templates-in-directory nil))
    (let ((templates nil))
      (dolist (path (sort (copy-list (uiop:directory-files dir))
                          #'string<
                          :key #'namestring))
        (when (and (pathname-type path)
                   (string-equal (pathname-type path) "md")
                   (not (hidden-pathname-p path)))
          (push (parse-prompt-template-markdown
                 (pathname-name path)
                 (uiop:read-file-string path)
                 :path path
                 :scope scope
                 :package package)
                templates)))
      (nreverse templates))))

(defun project-prompt-template-directory (buffer)
  "Return BUFFER's project prompt-template directory, or NIL."
  (let ((working-directory (and buffer (buffer-working-directory buffer))))
    (when working-directory
      (merge-pathnames *prompt-template-project-directory-name*
                       (uiop:ensure-directory-pathname working-directory)))))

(defun package-prompt-template-directory (definition)
  "Return DEFINITION's prompt-template directory."
  (or (package-definition-prompt-template-directory definition)
      (merge-pathnames *prompt-template-package-directory-name*
                       (package-definition-root definition))))

(defun prompt-template-package-directory-entries (&key buffer)
  "Return active package prompt-template directories for BUFFER."
  (let ((entries nil))
    (dolist (package-name (active-package-names :buffer buffer))
      (let* ((definition (find-installed-package package-name))
             (directory (and definition
                             (package-prompt-template-directory definition))))
        (when (and directory (probe-file directory))
          (push (list :directory directory
                      :scope :package
                      :package package-name)
                entries))))
    (nreverse entries)))

(defun prompt-template-search-directories (&key buffer)
  "Return visible prompt-template directories for BUFFER in precedence order."
  (append
   (let ((project-dir (project-prompt-template-directory buffer)))
     (if (and project-dir (probe-file project-dir))
         (list (list :directory project-dir :scope :project))
         nil))
   (prompt-template-package-directory-entries :buffer buffer)
   (if (probe-file *prompt-template-user-directory*)
       (list (list :directory *prompt-template-user-directory*
                   :scope :global))
       nil)))

(defun discover-prompt-templates (&key buffer)
  "Return prompt templates visible for BUFFER with local precedence."
  (let ((templates nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (entry (prompt-template-search-directories :buffer buffer))
      (dolist (template (discover-prompt-templates-in-directory
                         (getf entry :directory)
                         :scope (getf entry :scope)
                         :package (getf entry :package)))
        (unless (gethash (prompt-template-name template) seen)
          (setf (gethash (prompt-template-name template) seen) t)
          (push template templates))))
    (nreverse templates)))

(defun find-prompt-template (name &key buffer)
  "Return prompt template NAME visible for BUFFER, or NIL."
  (find (normalize-slash-command-name name)
        (discover-prompt-templates :buffer buffer)
        :key #'prompt-template-name
        :test #'string=))

(defun template-argument-slice (args start &optional count)
  "Return a slice of ARGS using 1-based START and optional COUNT."
  (let* ((start-index (max 0 (1- (or start 1))))
         (tail (nthcdr start-index args)))
    (if count
        (loop :for item :in tail
              :repeat (max 0 count)
              :collect item)
        (copy-list tail))))

(defun prompt-template-placeholder-expansion (body start args)
  "Return values REPLACEMENT and NEXT-INDEX for a placeholder in BODY at START."
  (let ((length (length body)))
    (when (or (>= (1+ start) length)
              (not (char= (char body start) #\$)))
      (return-from prompt-template-placeholder-expansion (values nil nil)))
    (let ((next (char body (1+ start))))
      (cond
        ((digit-char-p next)
         (let ((scan (1+ start)))
           (loop :while (and (< scan length)
                             (digit-char-p (char body scan)))
                 :do (incf scan))
           (let* ((index (parse-integer body :start (1+ start) :end scan))
                  (value (nth (1- index) args)))
             (values (or value "") scan))))
        ((char= next #\@)
         (values (format nil "~{~A~^ ~}" args)
                 (+ start 2)))
        ((and (<= (+ start 10) length)
              (string= "ARGUMENTS" body
                       :start1 0
                       :end1 9
                       :start2 (1+ start)
                       :end2 (+ start 10)))
         (values (format nil "~{~A~^ ~}" args)
                 (+ start 10)))
        ((char= next #\{)
         (let ((close (position #\} body :start (+ start 2))))
           (unless close
             (return-from prompt-template-placeholder-expansion
               (values nil nil)))
           (let ((inner (subseq body (+ start 2) close)))
             (when (and (>= (length inner) 2)
                        (string= "@:" inner :end2 2))
               (let* ((parts (split-lines
                              (substitute #\Newline #\: inner)))
                      (start-part (nth 1 parts))
                      (count-part (nth 2 parts))
                      (slice (template-argument-slice
                              args
                              (parse-integer start-part)
                              (and count-part (parse-integer count-part)))))
                 (values (format nil "~{~A~^ ~}" slice)
                         (1+ close)))))))
        (t
         (values nil nil))))))

(defun expand-prompt-template-body (body args)
  "Expand BODY placeholders against ARGS.
Supports $1, $2, $@, $ARGUMENTS, ${@:N}, and ${@:N:M}."
  (with-output-to-string (out)
    (loop :with length := (length body)
          :with index := 0
          :while (< index length)
          :do (multiple-value-bind (replacement next-index)
                  (prompt-template-placeholder-expansion body index args)
                (if replacement
                    (progn
                      (write-string replacement out)
                      (setf index next-index))
                    (progn
                      (write-char (char body index) out)
                      (incf index)))))))

(defun slash-command-active-p (command &key buffer agent-name)
  "Return true when COMMAND is visible in the active package context."
  (let ((package (slash-command-package command)))
    (or (null package)
        (package-active-p package
                          :buffer buffer
                          :agent-name (or agent-name
                                          (and buffer
                                               (buffer-agent-name buffer)))))))

(defun register-slash-command (name handler &key description argument-hint package)
  "Register a slash command NAME handled by HANDLER."
  (let* ((normalized-name (normalize-slash-command-name name))
         (raw-owner (or package *current-clawmacs-package*))
         (owner (and raw-owner (manifest-package-name raw-owner)))
         (command (make-slash-command
                   :name normalized-name
                   :description (or description "")
                   :argument-hint (and argument-hint
                                       (string-trim
                                        '(#\Space #\Tab #\Newline #\Return)
                                        argument-hint))
                   :handler handler
                   :package owner)))
    (setf (gethash normalized-name *slash-command-table*) command)
    command))

(defun list-slash-commands (&key buffer agent-name)
  "Return active slash commands sorted by name."
  (let ((commands nil))
    (maphash (lambda (_name command)
               (declare (ignore _name))
               (when (slash-command-active-p command
                                             :buffer buffer
                                             :agent-name agent-name)
                 (push command commands)))
             *slash-command-table*)
    (sort commands #'string< :key #'slash-command-name)))

(defun find-slash-command (name &key buffer agent-name)
  "Return the active slash command named NAME, or NIL."
  (let* ((normalized-name (normalize-slash-command-name name))
         (command (gethash normalized-name *slash-command-table*)))
    (when (and command
               (slash-command-active-p command
                                       :buffer buffer
                                       :agent-name agent-name))
      command)))

(defun process-slash-command (buffer input-text)
  "Dispatch INPUT-TEXT as a slash command when one is registered.
Returns values HANDLED-P and RESULT."
  (multiple-value-bind (name args)
      (parse-slash-command-line input-text)
    (when name
      (let ((command (find-slash-command name
                                         :buffer buffer
                                         :agent-name (buffer-agent-name buffer))))
        (when command
          (values t
                  (funcall (slash-command-handler command)
                           buffer
                           args
                           input-text)))))))

(defun expand-slash-template-input (buffer input-text)
  "Return expanded INPUT-TEXT when it names a visible prompt template."
  (multiple-value-bind (name args)
      (parse-slash-command-line input-text)
    (when name
      (let ((template (find-prompt-template name :buffer buffer)))
        (when template
          (expand-prompt-template-body (prompt-template-body template)
                                       args))))))

(defun template-argument-summary (template)
  "Return a short argument hint string inferred from TEMPLATE."
  (let* ((body (or (and template (prompt-template-body template)) ""))
         (body-length (length body))
         (max-index 0)
         (rest-p nil))
    (labels ((rest-placeholder-p (index)
               (let ((next (and (< (1+ index) body-length)
                                (char body (1+ index)))))
                 (or (char= next #\@)
                     (and (<= (+ index 10) body-length)
                          (string= "ARGUMENTS" body
                                   :start1 0
                                   :end1 9
                                   :start2 (1+ index)
                                   :end2 (+ index 10)))
                     (and (char= next #\{)
                          (let ((close (position #\} body :start (+ index 2))))
                            (and close
                                 (let ((inner (subseq body (+ index 2) close)))
                                   (and (>= (length inner) 2)
                                        (string= "@:" inner
                                                 :end1 2
                                                 :end2 2)))))))))
             (positional-placeholder-index (index)
               (let ((next (and (< (1+ index) body-length)
                                (char body (1+ index)))))
                 (when (digit-char-p next)
                   (let ((scan (1+ index)))
                     (loop :while (and (< scan body-length)
                                       (digit-char-p (char body scan)))
                           :do (incf scan))
                     (parse-integer body :start (1+ index) :end scan))))))
      (loop :for index :from 0 :below body-length
            :when (char= (char body index) #\$)
              :do (let ((position-index
                          (positional-placeholder-index index)))
                    (cond
                      (position-index
                       (setf max-index (max max-index position-index)))
                      ((rest-placeholder-p index)
                       (setf rest-p t))))))
    (let ((required
            (loop :for idx :from 1 :to max-index
                  :collect (format nil "<arg~D>" idx))))
      (cond
        ((and required rest-p)
         (format nil "~{~A~^ ~} [args...]" required))
        (required
         (format nil "~{~A~^ ~}" required))
        (rest-p
         "[args...]")
        (t "")))))

(defun slash-command-display-text (name description argument-hint)
  "Return a single-line display string for slash completion."
  (with-output-to-string (stream)
    (format stream "/~A" name)
    (when (and argument-hint (plusp (length argument-hint)))
      (format stream "  ~A" argument-hint))
    (when (and description (plusp (length description)))
      (format stream "  - ~A" description))))

(defun slash-command-selector-items (&key buffer agent-name)
  "Return visible slash-command and prompt-template completion items."
  (let ((items nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (command (list-slash-commands :buffer buffer :agent-name agent-name))
      (let ((name (slash-command-name command)))
        (setf (gethash name seen) t)
        (push (list :name name
                    :kind :command
                    :display (slash-command-display-text
                              name
                              (slash-command-description command)
                              (slash-command-argument-hint command))
                    :match-text (format nil "~A ~A ~A"
                                        name
                                        (or (slash-command-argument-hint command) "")
                                        (or (slash-command-description command) ""))
                    :description (or (slash-command-description command) "")
                    :argument-hint (or (slash-command-argument-hint command) ""))
              items)))
    (dolist (template (discover-prompt-templates :buffer buffer))
      (let ((name (prompt-template-name template)))
        (unless (gethash name seen)
          (let ((argument-hint (template-argument-summary template)))
            (setf (gethash name seen) t)
            (push (list :name name
                        :kind :template
                        :display (slash-command-display-text
                                  name
                                  (prompt-template-description template)
                                  argument-hint)
                        :match-text (format nil "~A ~A ~A"
                                            name
                                            argument-hint
                                            (or (prompt-template-description
                                                 template)
                                                ""))
                        :description (or (prompt-template-description template)
                                         "")
                        :argument-hint argument-hint)
                  items)))))
    (sort items #'string< :key (lambda (item)
                                 (getf item :name)))))
