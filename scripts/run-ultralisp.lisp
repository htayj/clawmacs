(require :asdf)

(load "scripts/build-cache.lisp" :verbose nil :print nil)

(defun clawmacs-ultralisp-setup-path ()
  (or (let ((setup (uiop:getenv "CLAWMACS_ULTRALISP_SETUP")))
        (when (and setup (plusp (length setup)))
          setup))
      (let ((setup (uiop:getenv "CLAWMACS_QUICKLISP_SETUP")))
        (when (and setup (plusp (length setup)))
          setup))
      (namestring (merge-pathnames #P"quicklisp/setup.lisp"
                                   (user-homedir-pathname)))))

(defun clawmacs-ensure-ultralisp-dist ()
  ;; Ultralisp is a Quicklisp-compatible dist.  The local setup file loads the
  ;; Quicklisp client; dependency resolution below comes from the Ultralisp dist.
  (let ((dist (find-symbol "FIND-DIST" "QL-DIST"))
        (install-dist (find-symbol "INSTALL-DIST" "QL-DIST")))
    (unless (and dist install-dist)
      (error "Quicklisp dist support is unavailable; cannot enable Ultralisp."))
    (unless (funcall (symbol-function dist) "ultralisp")
      (format *error-output* "Installing Ultralisp dist...~%")
      (funcall (symbol-function install-dist)
               "http://dist.ultralisp.org/"
               :prompt nil))))

(defun clawmacs-directory-pathname (pathname)
  "Return PATHNAME's containing directory as a directory pathname."
  (make-pathname :name nil :type nil :version nil :defaults pathname))

(defun clawmacs-env-pathname (name)
  "Return NAME's non-empty environment value as a pathname, or NIL."
  (let ((value (uiop:getenv name)))
    (when (and value (plusp (length value)))
      (uiop:parse-native-namestring value))))

(defun clawmacs-existing-font-directory (pathname)
  "Return PATHNAME as a directory, or PATHNAME's parent when it names a file."
  (let ((truename (ignore-errors (truename pathname))))
    (cond
      ((null truename) nil)
      ((uiop:directory-pathname-p truename) truename)
      (t (clawmacs-directory-pathname truename)))))

(defun clawmacs-native-truetype-font-path ()
  "Return a compact native TrueType font directory for McCLIM, when known.

McCLIM's CLX TrueType port eagerly registers every `*.ttf' in
`mcclim-truetype:*truetype-font-path*'.  On machines with large font
collections, the default native path may be `/usr/share/fonts/TTF/' and can
exhaust SBCL's heap before the frame opens.  Prefer an explicit override, then
fall back to the small DejaVu bundle that McCLIM already depends on."
  (or (let ((font-path (clawmacs-env-pathname "CLAWMACS_TRUETYPE_FONT_PATH")))
        (and font-path (clawmacs-existing-font-directory font-path)))
      (let ((font-path (clawmacs-env-pathname "CLAWMACS_FONT_PATH")))
        (and font-path (clawmacs-existing-font-directory font-path)))
      (let* ((package (find-package "CL-DEJAVU"))
             (symbol (and package (find-symbol "FONT-PATHNAME" package))))
        (when (and symbol (fboundp symbol))
          (clawmacs-directory-pathname
           (funcall (symbol-function symbol) "DejaVuSans.ttf"))))))

(defun clawmacs-configure-native-truetype-font-path ()
  "Constrain McCLIM native TrueType discovery to a compact font directory."
  (let* ((package (find-package "MCCLIM-TRUETYPE"))
         (symbol (and package (find-symbol "*TRUETYPE-FONT-PATH*" package)))
         (path (clawmacs-native-truetype-font-path)))
    (when (and symbol path (probe-file path))
      (setf (symbol-value symbol) path)
      (format *error-output* ";; native truetype font path: ~A~%" path))))

(let ((*standard-output* *error-output*)
      (*trace-output* *error-output*))
  (let ((setup (clawmacs-ultralisp-setup-path)))
    (unless (probe-file setup)
      (error "Missing Quicklisp-compatible setup file for Ultralisp: ~A~%~A"
             setup
             "Set CLAWMACS_ULTRALISP_SETUP, CLAWMACS_QUICKLISP_SETUP, or install ~/quicklisp/setup.lisp."))
    (load setup))
  (clawmacs-ensure-ultralisp-dist)
  (clawmacs/build-cache:maybe-clean-build-cache
   :environment-variable "CLAWMACS_RUN_CLEAN_BUILD")
  (push (truename ".") asdf:*central-registry*)
  (funcall (symbol-function (find-symbol "QUICKLOAD" "QL")) :clawmacs)
  (clawmacs-configure-native-truetype-font-path))

(clawmacs:clawmacs-main
 :session-name (or (let ((name (uiop:getenv "CLAWMACS_SESSION_NAME")))
                     (when (and name (plusp (length name)))
                       name))
                   "clawmacs:native-session-01"))
