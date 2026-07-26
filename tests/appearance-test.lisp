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
    (is (= 1 (length (appearance-profile-role-overrides profile))))))
