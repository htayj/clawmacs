(in-package :clawmacs/tests)
(in-suite faces-suite)

(test clim-ink-creation
  "Compact color helpers return CLIM inks."
  (let ((cga (make-cga-ink 8))
        (c256 (make-256-color-ink 196))
        (hex (make-hex-ink "#ff0000")))
    (is (not (null cga)))
    (is (not (null c256)))
    (is (not (null hex)))))

(test legacy-color-spec-creation
  "Legacy color-spec values still exist for old init files."
  (let ((cga (make-color-spec :cga 8))
        (c256 (make-color-spec :256 196))
        (hex (make-color-spec :hex "#ff0000")))
    (is (eq :cga (color-spec-type cga)))
    (is (= 8 (color-spec-value cga)))
    (is (eq :256 (color-spec-type c256)))
    (is (= 196 (color-spec-value c256)))
    (is (eq :hex (color-spec-type hex)))
    (is (string= "#ff0000" (color-spec-value hex)))))

(test drawing-style-creation
  "Drawing styles store CLIM-native inks and text styles."
  (let* ((bg (make-cga-ink 15))
         (ink (make-cga-ink 0))
         (ts (make-clim-text-style :fix :bold :normal))
         (style (make-instance 'drawing-style
                  :name :default
                  :background-ink bg
                  :ink ink
                  :text-style ts
                  :drawing-options '(:line-thickness 2))))
    (is (eq :default (drawing-style-name style)))
    (is (eq bg (drawing-style-background-ink style)))
    (is (eq ink (drawing-style-ink style)))
    (is (eq ts (drawing-style-text-style style)))
    (is (equal '(:line-thickness 2)
               (drawing-style-drawing-options style)))
    (is (null (drawing-style-parent style)))))

(test legacy-face-initargs-coerce-color-specs
  "Old :foreground/:background initargs are accepted and converted to inks."
  (let ((style (make-instance 'face
                 :name :legacy
                 :background (make-color-spec :cga 15)
                 :foreground (make-color-spec :cga 0))))
    (is (typep style 'drawing-style))
    (is (not (typep (face-background style) 'color-spec)))
    (is (not (typep (face-foreground style) 'color-spec)))))

(test resolve-drawing-style-no-inheritance
  "Resolving a complete drawing style returns its CLIM drawing values."
  (let* ((bg (make-cga-ink 15))
         (ink (make-cga-ink 0))
         (ts (make-clim-text-style :fix :roman :normal))
         (style (make-instance 'drawing-style
                  :name :default
                  :background-ink bg
                  :ink ink
                  :text-style ts
                  :underline-p nil))
         (resolved (resolve-drawing-style style)))
    (is (eq ink (resolved-drawing-style-ink resolved)))
    (is (eq bg (resolved-drawing-style-background-ink resolved)))
    (is (eq ts (resolved-drawing-style-text-style resolved)))
    (is (null (resolved-drawing-style-underline-p resolved)))))

(test resolve-drawing-style-with-inheritance
  "Resolving a drawing style inherits nil attributes from parent."
  (let* ((parent-bg (make-cga-ink 15))
         (parent-ink (make-cga-ink 0))
         (parent (make-instance 'drawing-style
                   :name :default
                   :background-ink parent-bg
                   :ink parent-ink
                   :drawing-options '(:line-thickness 1)
                   :underline-p nil))
         (child-ink (make-cga-ink 4))
         (child (make-instance 'drawing-style
                  :name :error
                  :ink child-ink
                  :drawing-options '(:line-dashes t)
                  :underline-p t
                  :parent parent))
         (resolved (resolve-drawing-style child)))
    (is (eq child-ink (resolved-drawing-style-ink resolved)))
    (is (eq parent-bg (resolved-drawing-style-background-ink resolved)))
    (is (eq t (resolved-drawing-style-underline-p resolved)))
    (is (eq t (getf (resolved-drawing-style-drawing-options resolved)
                    :line-dashes)))
    (is (= 1 (getf (resolved-drawing-style-drawing-options resolved)
                   :line-thickness)))))

(test resolve-face-compatibility
  "Resolve-face returns compatibility accessors backed by CLIM values."
  (let* ((bg (make-cga-ink 15))
         (ink (make-cga-ink 0))
         (style (make-instance 'face
                  :name :default
                  :background-ink bg
                  :ink ink
                  :bold-p t))
         (resolved (resolve-face style)))
    (is (eq ink (resolved-face-foreground resolved)))
    (is (eq bg (resolved-face-background resolved)))
    (is (eq t (resolved-face-bold-p resolved)))
    (is (not (null (resolved-face-text-style resolved))))
    (is (not (null (getf (resolved-face-drawing-options resolved)
                         :text-style))))))

(test drawing-style-set-creation-and-lookup
  "Drawing style sets store styles by name and support lookup."
  (let* ((default-style (make-instance 'drawing-style
                          :name :default
                          :background-ink (make-cga-ink 15)
                          :ink (make-cga-ink 0)))
         (error-style (make-instance 'drawing-style
                        :name :error
                        :ink (make-cga-ink 4)
                        :parent default-style))
         (style-set (make-drawing-style-set
                     :agent-1
                     (list default-style error-style)))
         (face-set (make-face-set
                    :agent-1
                    (list default-style error-style))))
    (is (eq :agent-1 (drawing-style-set-owner style-set)))
    (is (eq default-style (get-drawing-style style-set :default)))
    (is (eq error-style (get-drawing-style style-set :error)))
    (is (null (get-drawing-style style-set :nonexistent)))
    (is (eq default-style (get-face face-set :default)))
    (is (eq error-style (get-face face-set :error)))))
