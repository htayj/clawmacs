(in-package :clawmacs/tests)
(in-suite font-editor-suite)

(defun font-editor-temp-directory (label)
  "Return a fresh temporary directory for font editor tests."
  (make-pathname :directory
                 (list :absolute "tmp"
                       (format nil "clawmacs-font-editor-~A-~A-~A"
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
         (font (clawmacs:import-ast-font path))
         (glyph-a (clawmacs::bitmap-font-glyph font 65))
         (glyph-b (clawmacs::bitmap-font-glyph font 66)))
    (is (string= "DEMO" (clawmacs:bitmap-font-name font)))
    (is (= 6 (clawmacs:bitmap-font-line-spacing font)))
    (is (= 5 (clawmacs:bitmap-font-baseline font)))
    (is (= 4 (clawmacs::bitmap-glyph-advance-width glyph-a)))
    (is (= 3 (clawmacs::bitmap-glyph-width glyph-a)))
    (is (= 6 (clawmacs::bitmap-glyph-height glyph-a)))
    (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap glyph-a) 0 0)))
    (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap glyph-a) 1 0)))
    (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap glyph-b) 1 2)))))

(test import-bdf-font-parses-glyph-metrics
  "BDF import should preserve font and glyph metrics."
  (let* ((path (write-font-editor-test-file
                (merge-pathnames "demo.bdf"
                                 (font-editor-temp-directory "bdf"))
                *test-bdf-font*))
         (font (clawmacs:import-bdf-font path))
         (glyph (clawmacs::bitmap-font-glyph font 65)))
    (is (string= "TESTFONT" (clawmacs:bitmap-font-name font)))
    (is (= 6 (clawmacs:bitmap-font-line-spacing font)))
    (is (= 5 (clawmacs:bitmap-font-baseline font)))
    (is (= 4 (clawmacs::bitmap-glyph-advance-width glyph)))
    (is (= 0 (clawmacs::bitmap-glyph-x-offset glyph)))
    (is (= 1 (clawmacs::bitmap-glyph-y-offset glyph)))
    (is (= 3 (clawmacs::bitmap-glyph-width glyph)))
    (is (= 3 (clawmacs::bitmap-glyph-height glyph)))
    (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap glyph) 0 0)))
    (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap glyph) 1 0)))
    (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap glyph) 1 2)))))

(test clawfont-roundtrip-preserves-font-data
  "Native `.clawfont' save/load should preserve editor glyph data."
  (let* ((font (clawmacs:make-empty-bitmap-font
                :name "ROUNDTRIP"
                :line-spacing 6
                :baseline 5
                :default-width 4))
         (glyph (clawmacs::ensure-bitmap-font-glyph font 65))
         (path (merge-pathnames "demo.clawfont"
                                (font-editor-temp-directory "clawfont"))))
    (setf (clawmacs::bitmap-glyph-name glyph) "A")
    (setf (clawmacs::bitmap-glyph-advance-width glyph) 4)
    (clawmacs::resize-glyph-bitmap glyph 3 3)
    (setf (aref (clawmacs::bitmap-glyph-bitmap glyph) 0 0) 1
          (aref (clawmacs::bitmap-glyph-bitmap glyph) 0 1) 1
          (aref (clawmacs::bitmap-glyph-bitmap glyph) 1 1) 1)
    (clawmacs:write-clawfont-file font path)
    (let* ((loaded (clawmacs:read-clawfont-file path))
           (loaded-glyph (clawmacs::bitmap-font-glyph loaded 65)))
      (is (string= "ROUNDTRIP" (clawmacs:bitmap-font-name loaded)))
      (is (= 6 (clawmacs:bitmap-font-line-spacing loaded)))
      (is (= 5 (clawmacs:bitmap-font-baseline loaded)))
      (is (= 4 (clawmacs::bitmap-glyph-advance-width loaded-glyph)))
      (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap loaded-glyph) 0 0)))
      (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap loaded-glyph) 0 1)))
      (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap loaded-glyph) 1 1))))))

(test font-editor-buffer-toggle-and-serialize
  "Font editor buffers should edit glyph pixels and persist buffer state."
  (let ((*buffer-ring* nil)
        (clawmacs::*font-editor-states* (make-hash-table :test #'eq)))
    (clawmacs::init-default-keymap)
    (let* ((buf (clawmacs:make-font-editor-buffer :add-to-ring-p nil))
           (state (clawmacs:font-editor-buffer-state buf)))
      (is (clawmacs:font-editor-buffer-p buf))
      (is (= 65 (clawmacs::font-editor-state-selected-code state)))
      (clawmacs::font-editor-toggle-pixel buf 2 3)
      (let* ((glyph (clawmacs:font-editor-current-glyph buf))
             (bitmap (clawmacs::bitmap-glyph-bitmap glyph))
             (persisted (clawmacs::font-editor-serialize-buffer-state buf))
             (restored (clawmacs:make-font-editor-buffer :add-to-ring-p nil)))
        (is (= 1 (aref bitmap 3 2)))
        (is-true (clawmacs::font-editor-state-dirty-p state))
        (clawmacs::font-editor-restore-buffer-state restored persisted)
        (is (= 1 (aref (clawmacs::bitmap-glyph-bitmap
                        (clawmacs:font-editor-current-glyph restored))
                       3 2)))
        (is (= 65 (clawmacs::font-editor-state-selected-code
                   (clawmacs:font-editor-buffer-state restored))))))))

(test extract-genera-bdf-fonts-copies-and-normalizes-versioned-names
  "Genera extraction should copy BDF files outside the repo with stable names."
  (let* ((source-dir (font-editor-temp-directory "genera-source"))
         (target-dir (font-editor-temp-directory "genera-target"))
         (versioned (merge-pathnames "cptfont.bdf.~3~" source-dir))
         (plain (merge-pathnames "tvfont.bdf" source-dir)))
    (write-font-editor-test-file versioned *test-bdf-font*)
    (write-font-editor-test-file plain *test-bdf-font*)
    (let ((results (clawmacs:extract-genera-bdf-fonts
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
    (let ((results (clawmacs:extract-genera-bdf-fonts
                    :source-directories nil
                    :target-directory target-dir)))
      (is (= 1 (length results)))
      (is (equal (namestring existing)
                 (namestring (first results)))))))
