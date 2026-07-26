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
