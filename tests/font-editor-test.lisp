(in-package :rplaca/tests)
(in-suite font-editor-suite)

(defun font-editor-temp-directory (label)
  "Return a fresh temporary directory for font editor tests."
  (make-pathname :directory
                 (list :absolute "tmp"
                       (format nil "rplaca-font-editor-~A-~A-~A"
                               label
                               (get-universal-time)
                               (gensym)))))

(defun write-font-editor-test-file (path content)
  "Write CONTENT to PATH as UTF-8 text."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  path)

(defparameter *test-ast-font*
  (format nil
          "0 KSTID TEST;DEMO KST~%6 HEIGHT~%5 BASE LINE~%0 COLUMN POSITION ADJUSTMENT~C65 CHARACTER CODE TEST;DEMO KST~%3 RASTER WIDTH~%4 CHARACTER WIDTH~%0 LEFT KERN~%***~%* *~%***~C66 CHARACTER CODE TEST;DEMO KST~%3 RASTER WIDTH~%4 CHARACTER WIDTH~%0 LEFT KERN~%** ~%* *~%** ~%"
          #\Page
          #\Page))

(defparameter *test-bdf-font*
  "STARTFONT 2.1
FONT TESTFONT
SIZE 6 72 72
FONTBOUNDINGBOX 4 6 0 -1
STARTPROPERTIES 2
FONT_ASCENT 5
FONT_DESCENT 1
ENDPROPERTIES
CHARS 1
STARTCHAR A
ENCODING 65
DWIDTH 4 0
BBX 3 3 0 1
BITMAP
E0
A0
E0
ENDCHAR
ENDFONT
")

(test import-ast-font-parses-cadr-pages
  "AST import should preserve font metrics and glyph bitmaps."
  (let* ((path (write-font-editor-test-file
                (merge-pathnames "demo.ast"
                                 (font-editor-temp-directory "ast"))
                *test-ast-font*))
         (font (rplaca:import-ast-font path))
         (glyph-a (rplaca::bitmap-font-glyph font 65))
         (glyph-b (rplaca::bitmap-font-glyph font 66)))
    (is (string= "DEMO" (rplaca:bitmap-font-name font)))
    (is (= 6 (rplaca:bitmap-font-line-spacing font)))
    (is (= 5 (rplaca:bitmap-font-baseline font)))
    (is (= 4 (rplaca::bitmap-glyph-advance-width glyph-a)))
    (is (= 3 (rplaca::bitmap-glyph-width glyph-a)))
    (is (= 6 (rplaca::bitmap-glyph-height glyph-a)))
    (is (= 1 (aref (rplaca::bitmap-glyph-bitmap glyph-a) 0 0)))
    (is (= 1 (aref (rplaca::bitmap-glyph-bitmap glyph-a) 1 0)))
    (is (= 1 (aref (rplaca::bitmap-glyph-bitmap glyph-b) 1 2)))))

(test import-bdf-font-parses-glyph-metrics
  "BDF import should preserve font and glyph metrics."
  (let* ((path (write-font-editor-test-file
                (merge-pathnames "demo.bdf"
                                 (font-editor-temp-directory "bdf"))
                *test-bdf-font*))
         (font (rplaca:import-bdf-font path))
         (glyph (rplaca::bitmap-font-glyph font 65)))
    (is (string= "TESTFONT" (rplaca:bitmap-font-name font)))
    (is (= 6 (rplaca:bitmap-font-line-spacing font)))
    (is (= 5 (rplaca:bitmap-font-baseline font)))
    (is (= 4 (rplaca::bitmap-glyph-advance-width glyph)))
    (is (= 0 (rplaca::bitmap-glyph-x-offset glyph)))
    (is (= 1 (rplaca::bitmap-glyph-y-offset glyph)))
    (is (= 3 (rplaca::bitmap-glyph-width glyph)))
    (is (= 3 (rplaca::bitmap-glyph-height glyph)))
    (is (= 1 (aref (rplaca::bitmap-glyph-bitmap glyph) 0 0)))
    (is (= 1 (aref (rplaca::bitmap-glyph-bitmap glyph) 1 0)))
    (is (= 1 (aref (rplaca::bitmap-glyph-bitmap glyph) 1 2)))))

(test rplacafont-roundtrip-preserves-font-data-and-canonical-tag
  "Native `.rplacafont' save/load preserves data and writes the canonical tag."
  (let* ((font (rplaca:make-empty-bitmap-font
                :name "ROUNDTRIP"
                :line-spacing 6
                :baseline 5
                :default-width 4))
         (glyph (rplaca::ensure-bitmap-font-glyph font 65))
         (path (merge-pathnames "demo.rplacafont"
                                (font-editor-temp-directory "rplacafont"))))
    (setf (rplaca::bitmap-glyph-name glyph) "A")
    (setf (rplaca::bitmap-glyph-advance-width glyph) 4)
    (rplaca::resize-glyph-bitmap glyph 3 3)
    (setf (aref (rplaca::bitmap-glyph-bitmap glyph) 0 0) 1
          (aref (rplaca::bitmap-glyph-bitmap glyph) 0 1) 1
          (aref (rplaca::bitmap-glyph-bitmap glyph) 1 1) 1)
    (rplaca:write-rplacafont-file font path)
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (let ((*read-eval* nil))
        (is (eq :rplacafont (first (read stream))))))
    (let* ((loaded (rplaca:read-rplacafont-file path))
           (loaded-glyph (rplaca::bitmap-font-glyph loaded 65)))
      (is (string= "ROUNDTRIP" (rplaca:bitmap-font-name loaded)))
      (is (= 6 (rplaca:bitmap-font-line-spacing loaded)))
      (is (= 5 (rplaca:bitmap-font-baseline loaded)))
      (is (= 4 (rplaca::bitmap-glyph-advance-width loaded-glyph)))
      (is (= 1 (aref (rplaca::bitmap-glyph-bitmap loaded-glyph) 0 0)))
      (is (= 1 (aref (rplaca::bitmap-glyph-bitmap loaded-glyph) 0 1)))
      (is (= 1 (aref (rplaca::bitmap-glyph-bitmap loaded-glyph) 1 1))))))

(test legacy-clawfont-remains-read-only-import-compatible
  "Legacy `.clawfont' data remains readable but cannot be a native write target."
  (let* ((font (rplaca:make-empty-bitmap-font :name "LEGACY"))
         (directory (font-editor-temp-directory "legacy-clawfont"))
         (legacy-path (merge-pathnames "legacy.clawfont" directory)))
    (write-font-editor-test-file
     legacy-path
     (format nil "~S~%"
             (list :clawfont (rplaca::serialize-bitmap-font font))))
    (is (eq :legacy-clawfont
            (rplaca::detect-font-file-format legacy-path)))
    (is (string= "LEGACY"
                 (rplaca:bitmap-font-name
                  (rplaca:read-font-file legacy-path))))
    (signals error
      (rplaca:write-rplacafont-file font legacy-path))))

(test rplacafont-public-api-and-save-prompt-are-canonical
  "Only RPLACA-named native font APIs are public and the UI prompts canonically."
  (multiple-value-bind (symbol status)
      (find-symbol "WRITE-RPLACAFONT-FILE" :rplaca)
    (is (eq :external status))
    (is (fboundp symbol)))
  (multiple-value-bind (symbol status)
      (find-symbol "READ-RPLACAFONT-FILE" :rplaca)
    (is (eq :external status))
    (is (fboundp symbol)))
  (is (not (eq :external
               (nth-value 1 (find-symbol "WRITE-CLAWFONT-FILE" :rplaca)))))
  (is (not (eq :external
               (nth-value 1 (find-symbol "READ-CLAWFONT-FILE" :rplaca)))))
  (let ((*buffer-ring* nil)
        (rplaca::*font-editor-states* (make-hash-table :test #'eq)))
    (unwind-protect
         (let ((buf (rplaca:make-font-editor-buffer :add-to-ring-p nil)))
           (rplaca:font-editor-save-font-command buf)
           (is (string= "Save .rplacafont as"
                        rplaca::*minibuffer-prompt*))
           (is (search ".rplacafont" rplaca::*minibuffer-input*)))
      (rplaca::minibuffer-deactivate))))

(test restored-font-editor-state-never-reuses-a-legacy-save-path
  "Legacy save paths are discarded while canonical save paths survive restore."
  (let ((*buffer-ring* nil)
        (rplaca::*font-editor-states* (make-hash-table :test #'eq)))
    (let* ((source (rplaca:make-font-editor-buffer :add-to-ring-p nil))
           (legacy-target (rplaca:make-font-editor-buffer :add-to-ring-p nil))
           (canonical-target
             (rplaca:make-font-editor-buffer :add-to-ring-p nil))
           (persisted (rplaca::font-editor-serialize-buffer-state source))
           (save-entry (assoc :save-path persisted)))
      (setf (cdr save-entry) "/tmp/legacy.clawfont")
      (rplaca::font-editor-restore-buffer-state legacy-target persisted)
      (is (null (rplaca::font-editor-state-save-path
                 (rplaca:font-editor-buffer-state legacy-target))))
      (setf (cdr save-entry) "/tmp/canonical.rplacafont")
      (rplaca::font-editor-restore-buffer-state canonical-target persisted)
      (is (equal #P"/tmp/canonical.rplacafont"
                 (rplaca::font-editor-state-save-path
                  (rplaca:font-editor-buffer-state canonical-target)))))))

(test font-editor-buffer-toggle-and-serialize
  "Font editor buffers should edit glyph pixels and persist buffer state."
  (let ((*buffer-ring* nil)
        (rplaca::*font-editor-states* (make-hash-table :test #'eq)))
    (rplaca::init-default-keymap)
    (let* ((buf (rplaca:make-font-editor-buffer :add-to-ring-p nil))
           (state (rplaca:font-editor-buffer-state buf)))
      (is (rplaca:font-editor-buffer-p buf))
      (is (= 65 (rplaca::font-editor-state-selected-code state)))
      (rplaca::font-editor-toggle-pixel buf 2 3)
      (let* ((glyph (rplaca:font-editor-current-glyph buf))
             (bitmap (rplaca::bitmap-glyph-bitmap glyph))
             (persisted (rplaca::font-editor-serialize-buffer-state buf))
             (restored (rplaca:make-font-editor-buffer :add-to-ring-p nil)))
        (is (= 1 (aref bitmap 3 2)))
        (is-true (rplaca::font-editor-state-dirty-p state))
        (rplaca::font-editor-restore-buffer-state restored persisted)
        (is (= 1 (aref (rplaca::bitmap-glyph-bitmap
                        (rplaca:font-editor-current-glyph restored))
                       3 2)))
        (is (= 65 (rplaca::font-editor-state-selected-code
                   (rplaca:font-editor-buffer-state restored))))))))

(test extract-genera-bdf-fonts-copies-and-normalizes-versioned-names
  "Genera extraction should copy BDF files outside the repo with stable names."
  (let* ((source-dir (font-editor-temp-directory "genera-source"))
         (target-dir (font-editor-temp-directory "genera-target"))
         (versioned (merge-pathnames "cptfont.bdf.~3~" source-dir))
         (plain (merge-pathnames "tvfont.bdf" source-dir)))
    (write-font-editor-test-file versioned *test-bdf-font*)
    (write-font-editor-test-file plain *test-bdf-font*)
    (let ((results (rplaca:extract-genera-bdf-fonts
                    :source-directories (list source-dir)
                    :target-directory target-dir)))
      (is (= 2 (length results)))
      (is (probe-file (merge-pathnames "cptfont.bdf" target-dir)))
      (is (probe-file (merge-pathnames "tvfont.bdf" target-dir))))))

(test extract-genera-bdf-fonts-reuses-existing-target-files
  "Genera extraction should reuse already extracted BDFs in the target dir."
  (let* ((target-dir (font-editor-temp-directory "genera-existing"))
         (existing (merge-pathnames "existing.bdf" target-dir)))
    (write-font-editor-test-file existing *test-bdf-font*)
    (let ((results (rplaca:extract-genera-bdf-fonts
                    :source-directories nil
                    :target-directory target-dir)))
      (is (= 1 (length results)))
      (is (equal (namestring existing)
                 (namestring (first results)))))))
