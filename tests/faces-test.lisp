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
