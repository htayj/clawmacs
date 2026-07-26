(in-package :clawmacs/tests)

(in-suite appearance-suite)

(test appearance-specifications-distinguish-unspecified-components
  "Every appearance axis has an explicit internal inheritance value."
  (let ((typography (make-appearance-typography-spec :face :bold))
        (ink (make-appearance-ink-spec :foreground '(:rgb 0.1 0.2 0.3)))
        (surface (make-appearance-surface-spec))
        (decoration (make-appearance-decoration-spec :kind :none)))
    (is (appearance-unspecified-p (appearance-typography-spec-family typography)))
    (is (eq :bold (appearance-typography-spec-face typography)))
    (is (appearance-unspecified-p (appearance-typography-spec-size typography)))
    (is (equal '(:rgb 0.1 0.2 0.3) (appearance-ink-spec-foreground ink)))
    (is (appearance-unspecified-p (appearance-surface-spec-background surface)))
    (is (eq :none (appearance-decoration-spec-kind decoration)))))

(test appearance-role-axis-validation-respects-role-kind
  "Surface-only background declarations cannot silently apply to content roles."
  (let* ((content (make-appearance-role-definition
                   :id :default-text :kind :content))
         (surface (make-appearance-role-definition
                   :id :transcript-pane :kind :surface))
         (style (make-appearance-role-style
                 :surface (make-appearance-surface-spec :background :white))))
    (signals unsupported-role-axis
      (validate-appearance-role-style content style :origin :test))
    (is (eq style (validate-appearance-role-style surface style :origin :test)))
    (is-true (appearance-role-supports-axis-p content :foreground-ink))
    (is-false (appearance-role-supports-axis-p content :surface))))

(test appearance-profile-and-theme-are-declaration-data
  "Profiles and themes retain portable data rather than resolved port objects."
  (let* ((theme (make-appearance-theme-definition
                 :id :classic
                 :role-overlays (list (cons :default-text
                                             (make-appearance-role-style)))))
         (profile (make-appearance-profile
                   :selected-theme :classic
                   :role-overrides (list (cons :default-text
                                                (make-appearance-role-style))))))
    (is (eq :classic (appearance-theme-definition-id theme)))
    (is (eq :classic (appearance-profile-selected-theme profile)))
    (is-false (appearance-profile-strict-contrast profile))
    (is (= 1 (length (appearance-profile-role-overrides profile))))))

(test appearance-declarations-defensively-copy-mutable-inputs
  "Published declarations neither retain nor expose mutable caller data."
  (let* ((parameters (vector "sample"))
         (overlays (list (cons :default-text
                               (make-appearance-role-style))))
         (theme (make-appearance-theme-definition
                 :id :classic
                 :role-overlays overlays))
         (decoration (make-appearance-decoration-spec
                      :kind :selection-marker :parameters parameters)))
    (setf (aref parameters 0) "changed"
          (caar overlays) :error)
    (is (eq :default-text (caar (appearance-theme-definition-role-overlays theme))))
    (is (string= "sample" (aref (appearance-decoration-spec-parameters decoration)
                                  0)))
    (let ((published (appearance-theme-definition-role-overlays theme)))
      (setf (caar published) :error)
      (is (eq :default-text
              (caar (appearance-theme-definition-role-overlays theme)))))))

(test appearance-profile-validates-strict-contrast-and-style-components
  "Only profile strictness accepts NIL; axes are typed and role kinds are fixed."
  (is-true (appearance-profile-strict-contrast
            (make-appearance-profile :strict-contrast t)))
  (signals invalid-appearance-component
    (make-appearance-profile :strict-contrast :yes))
  (signals invalid-appearance-component
    (make-appearance-ink-spec :foreground nil))
  (signals invalid-appearance-component
    (make-appearance-ink-spec :foreground :none))
  (signals invalid-appearance-component
    (make-appearance-role-style :typography :bold))
  (signals invalid-appearance-component
    (make-appearance-role-definition :id :bad :kind :unknown)))

(test appearance-condition-payloads-are-defensive-and-contrast-is-a-warning
  "Conditions preserve diagnostic data without retaining mutable caller values."
  (let* ((payload (list "original"))
         (captured nil))
    (handler-bind ((appearance-contrast-warning
                     (lambda (condition)
                       (setf captured condition)
                       (muffle-warning condition))))
      (warn 'appearance-contrast-warning :value payload :role :default-text))
    (setf (car payload) "changed")
    (is (typep captured 'warning))
    (is (string= "original" (car (appearance-condition-value captured))))
    (let ((reported (appearance-condition-value captured)))
      (setf (car reported) "changed-again")
      (is (string= "original"
                   (car (appearance-condition-value captured)))))))

(test appearance-vocabulary-accessors-return-fresh-lists
  "The internal vocabulary cannot be modified through its public accessors."
  (let ((sizes (appearance-logical-sizes))
        (kinds (appearance-role-kinds)))
    (setf (first sizes) :changed
          (first kinds) :changed)
    (is (eq :tiny (first (appearance-logical-sizes))))
    (is (eq :surface (first (appearance-role-kinds))))))

(defun test-appearance-style (&key foreground background family face size decoration)
  "Build only the explicitly requested typed style axes for appearance tests."
  (let ((arguments nil))
    (when (or family face size)
      (let ((typography-arguments nil))
        (when family
          (setf typography-arguments
                (append typography-arguments (list :family family))))
        (when face
          (setf typography-arguments
                (append typography-arguments (list :face face))))
        (when size
          (setf typography-arguments
                (append typography-arguments (list :size size))))
        (setf arguments
              (append arguments
                      (list :typography
                            (apply #'make-appearance-typography-spec
                                   typography-arguments))))))
    (when foreground
      (setf arguments
            (append arguments
                    (list :foreground-ink
                          (make-appearance-ink-spec :foreground foreground)))))
    (when background
      (setf arguments
            (append arguments
                    (list :surface
                          (make-appearance-surface-spec :background background)))))
    (when decoration
      (setf arguments
            (append arguments
                    (list :decoration
                          (make-appearance-decoration-spec :kind decoration)))))
    (apply #'make-appearance-role-style arguments)))

(defun make-test-appearance-catalog (&key role-cycle-p theme-cycle-p)
  "Return a compact catalog exercising fallbacks, parent themes, and stacks."
  (make-appearance-catalog
   :role-definitions
   (list (make-appearance-role-definition :id :base :kind :surface)
         (make-appearance-role-definition :id :transcript :kind :surface
                                          :fallback-role :base)
         (make-appearance-role-definition :id :default-text :kind :content
                                          :fallback-role (and role-cycle-p :title))
         (make-appearance-role-definition :id :title :kind :content
                                          :fallback-role :default-text)
         (make-appearance-role-definition :id :selected :kind :state))
   :theme-definitions
   (list (make-appearance-theme-definition
          :id :root
          :parent-theme (and theme-cycle-p :child)
          :role-overlays
          (list (cons :default-text
                      (test-appearance-style :foreground :black
                                             :family :fix :size :normal))
                (cons :base (test-appearance-style :background :black))))
         (make-appearance-theme-definition
          :id :child
          :parent-theme :root
          :role-overlays
          (list (cons :title (test-appearance-style :face :bold :size :larger))
                (cons :selected (test-appearance-style :foreground :white
                                                       :face :bold)))))))

(test appearance-role-resolution-obeys-the-exact-layer-order
  "Every source layer wins over lower layers while role fallback stays root-first."
  (let* ((catalog (make-test-appearance-catalog))
         (resolved
           (resolve-appearance-role
            catalog :child :title
            :file-overrides (list (cons :default-text
                                        (test-appearance-style :foreground :red)))
            :init-overrides (list (cons :title
                                        (test-appearance-style :foreground :green)))
            :environment-overrides (list (cons :title
                                               (test-appearance-style :foreground :blue)))
            :command-line-overrides (list (cons :title
                                                (test-appearance-style :foreground :cyan)))
            :unsaved-overrides (list (cons :title
                                        (test-appearance-style :foreground :white))))))
         (style (resolved-appearance-role-style resolved)))
    (is (eq :white
            (appearance-ink-spec-foreground
             (appearance-role-style-foreground-ink style))))
    (is (eq :bold
            (appearance-typography-spec-face
             (appearance-role-style-typography style))))
    (is (eq :large
            (appearance-typography-spec-size
             (appearance-role-style-typography style))))
    (is (equal :unsaved
               (cdr (assoc :foreground-ink
                           (resolved-appearance-role-provenance resolved)))))))

(test appearance-typography-merges-components-and-clamps-relative-sizes
  "Family/face/size cascade independently and each relative request is consumed."
  (let* ((catalog (make-test-appearance-catalog))
         (resolved (resolve-appearance-role
                    catalog :child :title
                    :file-overrides
                    (list (cons :title
                                (test-appearance-style :size :larger)))))
         (typography (appearance-role-style-typography
                      (resolved-appearance-role-style resolved))))
    (is (eq :fix (appearance-typography-spec-family typography)))
    (is (eq :bold (appearance-typography-spec-face typography)))
    (is (eq :very-large (appearance-typography-spec-size typography)))
    (is (eq :huge (resolve-appearance-relative-size :huge :larger :test)))
    (is (eq :tiny (resolve-appearance-relative-size :tiny :smaller :test)))
    (signals relative-size-base-invalid
      (resolve-appearance-relative-size :not-a-logical-size :larger :test))))

(test appearance-cycles-and-configuration-unknown-roles-are-fatal
  "Graphs and stored/configuration role references reject invalid declarations."
  (signals appearance-role-cycle
    (appearance-role-fallback-chain (make-test-appearance-catalog :role-cycle-p t)
                                    :default-text))
  (signals appearance-theme-cycle
    (appearance-theme-parent-chain (make-test-appearance-catalog :theme-cycle-p t)
                                   :child))
  (signals unknown-appearance-role
    (resolve-appearance-role (make-test-appearance-catalog) :child :missing))
  (signals unknown-appearance-role
    (resolve-appearance-role
     (make-test-appearance-catalog) :child :title
     :file-overrides (list (cons :missing
                                 (test-appearance-style :foreground :red))))))

(test appearance-stack-composes-surface-content-and-state
  "A stack overlays surface, content, then state without sharing profile state."
  (let* ((resolved (resolve-appearance-role-stack
                    (make-test-appearance-catalog) :child
                    '(:transcript :title :selected)))
         (style (resolved-appearance-role-style resolved)))
    (is (eq :black
            (appearance-surface-spec-background
             (appearance-role-style-surface style))))
    (is (eq :white
            (appearance-ink-spec-foreground
             (appearance-role-style-foreground-ink style))))
    (is (eq :bold
            (appearance-typography-spec-face
             (appearance-role-style-typography style))))))

(test appearance-runtime-unknown-roles-fall-back-with-a-diagnostic
  "Runtime display data cannot fail redisplay because a wire role is unknown."
  (let ((resolved (resolve-runtime-appearance-role-stack
                   (make-test-appearance-catalog) :child '(:unknown))))
    (is (= 1 (length (resolved-appearance-role-diagnostics resolved))))
    (is-false (appearance-condition-fatal-p
               (first (resolved-appearance-role-diagnostics resolved))))
    (is (eq :black
            (appearance-ink-spec-foreground
             (appearance-role-style-foreground-ink
              (resolved-appearance-role-style resolved)))))))

(test appearance-structural-keys-ignore-no-op-identity-and-classic-is-golden
  "Cache keys describe effective output, and classic retains the exact literals."
  (let* ((catalog (make-test-appearance-catalog))
         (first (resolve-appearance-role catalog :child :title))
         (second (resolve-appearance-role catalog :child :title
                                          :unsaved-overrides nil))
         (different (resolve-appearance-role catalog :child :default-text))
         (classic (resolve-appearance-role (make-classic-appearance-catalog)
                                           :classic :transcript-user)))
    (is (equal (resolved-appearance-role-structural-key first)
               (resolved-appearance-role-structural-key second)))
    (is-false (equal (resolved-appearance-role-structural-key first)
                     (resolved-appearance-role-structural-key different)))
    (is (equal '(0.10 0.25 0.55)
               (appearance-ink-spec-foreground
                (appearance-role-style-foreground-ink
                 (resolved-appearance-role-style classic)))))))
