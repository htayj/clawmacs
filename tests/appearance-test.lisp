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
  (let* ((parameters (list :marker (copy-seq "sample")))
         (overlays (list (cons :default-text
                               (make-appearance-role-style))))
         (theme (make-appearance-theme-definition
                 :id :classic
                 :role-overlays overlays))
         (decoration (make-appearance-decoration-spec
                      :kind :selection-marker :parameters parameters)))
    (setf (char (second parameters) 0) #\c
          (caar overlays) :error)
    (is (eq :default-text (caar (appearance-theme-definition-role-overlays theme))))
    (is (string= "sample"
                 (second (appearance-decoration-spec-parameters decoration))))
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
    (make-appearance-ink-spec :foreground :unspecified))
  (signals invalid-appearance-component
    (make-appearance-role-style :typography :bold))
  (signals invalid-appearance-component
    (make-appearance-role-definition :id :bad :kind :unknown)))

(test appearance-condition-factory-copies-payloads-before-signaling
  "The required factory and warning boundary preserve no caller-owned payload."
  (let* ((payload (list (copy-seq "original")))
         (condition (make-appearance-condition 'appearance-contrast-warning
                                               :value payload :role :default-text))
         (captured nil))
    (setf (char (first payload) 0) #\c)
    (handler-bind ((appearance-contrast-warning
                     (lambda (warning)
                       (setf captured warning)
                       (muffle-warning warning))))
      (warn condition))
    (is (typep captured 'warning))
    (is (string= "original" (car (appearance-condition-value captured))))
    (let ((reported (appearance-condition-value captured)))
      (setf (char (first reported) 0) #\c)
      (is (string= "original"
                   (car (appearance-condition-value captured)))))))

(test appearance-warning-factory-boundary-copies-signaled-payloads
  "WARN-APPEARANCE-CONDITION copies payloads before a handler can observe them."
  (let ((payload (list (copy-seq "original")))
        (captured nil))
    (handler-bind ((appearance-contrast-warning
                     (lambda (warning)
                       (setf captured warning)
                       (muffle-warning warning))))
      (warn-appearance-condition 'appearance-contrast-warning
                                 :value payload :role :default-text))
    (setf (char (first payload) 0) #\c)
    (is (string= "original" (car (appearance-condition-value captured))))))

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

(defun make-test-appearance-catalog (&key role-cycle-p theme-cycle-p (generation 0))
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
                                                       :face :bold)))))
   :generation generation))

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
                                           (test-appearance-style :foreground :white)))))
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

(test appearance-chain-builders-return-root-to-leaf-order
  "Fallback and parent builders independently preserve the cascade direction."
  (let ((catalog (make-test-appearance-catalog)))
    (is (equal '(:default-text :title)
               (mapcar #'appearance-role-definition-id
                       (appearance-role-fallback-chain catalog :title))))
    (is (equal '(:root :child)
               (mapcar #'appearance-theme-definition-id
                       (appearance-theme-parent-chain catalog :child))))))

(test appearance-catalog-rejects-invalid-graphs-and-configuration-roles
  "Graphs are rejected before publication; stored/configuration roles are fatal."
  (signals appearance-role-cycle
    (make-test-appearance-catalog :role-cycle-p t))
  (signals appearance-theme-cycle
    (make-test-appearance-catalog :theme-cycle-p t))
  (signals missing-appearance-parent
    (make-appearance-catalog
     :role-definitions
     (list (make-appearance-role-definition :id :missing-child :kind :content
                                             :fallback-role :absent))))
  (signals invalid-appearance-fallback
    (make-appearance-catalog
     :role-definitions
     (list (make-appearance-role-definition :id :surface :kind :surface
                                             :fallback-role :content)
           (make-appearance-role-definition :id :content :kind :content))))
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
               (appearance-diagnostic-condition
                (first (resolved-appearance-role-diagnostics resolved)))))
    (is (eq :black
            (appearance-ink-spec-foreground
             (appearance-role-style-foreground-ink
              (resolved-appearance-role-style resolved)))))))

(test appearance-runtime-unknown-role-does-not-mask-a-later-valid-role
  "A valid same-kind wire role wins over a preceding fallback from an unknown."
  (let ((resolved (resolve-runtime-appearance-role-stack
                   (make-test-appearance-catalog) :child '(:unknown :title))))
    (is (= 1 (length (resolved-appearance-role-diagnostics resolved))))
    (is (eq :bold
            (appearance-typography-spec-face
             (appearance-role-style-typography
              (resolved-appearance-role-style resolved)))))))

(test appearance-runtime-diagnostics-have-bounded-deduplication-keys
  "Each unknown role gets one bounded event keyed by catalog generation and role."
  (let ((resolved (resolve-runtime-appearance-role-stack
                   (make-test-appearance-catalog :generation 17) :child
                   '(:unknown-one :unknown-two :title))))
    (is (= 2 (length (resolved-appearance-role-diagnostics resolved))))
    (is (equal '(:unknown-one :unknown-two)
               (mapcar #'appearance-diagnostic-unknown-role
                       (resolved-appearance-role-diagnostics resolved))))
    (is (equal '(17 :unknown-one)
               (appearance-diagnostic-deduplication-key
                (first (resolved-appearance-role-diagnostics resolved)))))))

(test appearance-runtime-diagnostics-bound-distinct-unknown-roles
  "The pure resolver cannot produce an unbounded diagnostic list."
  (let ((resolved (resolve-runtime-appearance-role-stack
                   (make-test-appearance-catalog) :child
                   (loop for index below 20
                         collect (intern (format nil "UNKNOWN-~D" index)
                                         :keyword)))))
    (is (= 16 (length (resolved-appearance-role-diagnostics resolved))))))

(test appearance-supported-axis-accessors-do-not-expose-constant-lists
  "Default axis vocabulary copies are fresh even when they originate in constants."
  (let* ((role (make-appearance-role-definition :id :content :kind :content))
         (axes (appearance-role-supported-axes role)))
    (setf (first axes) :changed)
    (is (eq :typography (first (appearance-role-supported-axes role))))))

(test appearance-structural-keys-and-classic-goldens-are-deterministic
  "Keys are output-only and classic retains every current literal/style golden."
  (let* ((catalog (make-test-appearance-catalog))
         (first (resolve-appearance-role catalog :child :title))
         (second (resolve-appearance-role catalog :child :title
                                          :unsaved-overrides nil))
         (different (resolve-appearance-role catalog :child :default-text))
         (classic-catalog (make-classic-appearance-catalog))
         (classic (resolve-appearance-role classic-catalog :classic
                                           :transcript-user)))
    (is (equal (resolved-appearance-role-structural-key first)
               (resolved-appearance-role-structural-key second)))
    (is-false (equal (resolved-appearance-role-structural-key first)
                     (resolved-appearance-role-structural-key different)))
    (is (equal
         (appearance-role-style-key
          (make-appearance-role-style
           :decoration (make-appearance-decoration-spec
                        :kind :selection-marker
                        :parameters (list :marker ">"))))
         (appearance-role-style-key
          (make-appearance-role-style
           :decoration (make-appearance-decoration-spec
                        :kind :selection-marker
                        :parameters (list :marker ">"))))))
    (is (equal '(:typography :unspecified
                 :foreground-ink :unspecified
                 :surface :unspecified
                 :decoration (:selection-marker (:marker ">")))
               (appearance-role-style-key
                (make-appearance-role-style
                 :decoration
                 (make-appearance-decoration-spec
                  :kind :selection-marker
                  :parameters (list :marker ">"))))))
    (is (equal '(0.10 0.25 0.55)
               (appearance-ink-spec-foreground
                (appearance-role-style-foreground-ink
                 (resolved-appearance-role-style classic)))))
    (dolist (golden '((:transcript-agent (0.10 0.10 0.10))
                      (:transcript-tool (0.12 0.34 0.18))
                      (:tool-result (0.12 0.34 0.18))
                      (:transcript-system (0.36 0.36 0.36))
                      (:transcript-empty (0.45 0.45 0.45))
                      (:selector-title (0.16 0.22 0.45))
                      (:selector-header (0.18 0.36 0.20))
                      (:selector-footer (0.35 0.35 0.35))
                      (:selector-selection (0.10 0.38 0.65))
                      (:selector-entry (0.20 0.20 0.20))
                      (:system (0.45 0.45 0.45))
                      (:disabled (0.45 0.45 0.45))
                      (:error (0.60 0.12 0.12))))
      (let* ((resolved (resolve-appearance-role classic-catalog :classic
                                                 (first golden)))
             (foreground (appearance-ink-spec-foreground
                          (appearance-role-style-foreground-ink
                           (resolved-appearance-role-style resolved)))))
        (is (equal (second golden) foreground))))
    (let* ((title (resolved-appearance-role-style
                   (resolve-appearance-role classic-catalog :classic
                                            :selector-title)))
           (selection (resolved-appearance-role-style
                       (resolve-appearance-role classic-catalog :classic
                                                :selector-selection)))
           (minibuffer-emphasis
             (resolved-appearance-role-style
              (resolve-appearance-role classic-catalog :classic
                                       :minibuffer-selection-emphasis)))
           (generic-selected
             (resolved-appearance-role-style
              (resolve-appearance-role-stack
               classic-catalog :classic
               '(:selector-entry :selector-selection))))
           (minibuffer-selected
             (resolved-appearance-role-style
              (resolve-appearance-role-stack
               classic-catalog :classic
               '(:minibuffer-pane :selector-entry :selector-selection
                 :minibuffer-selection-emphasis)))))
      (is (eq :bold (appearance-typography-spec-face
                     (appearance-role-style-typography title))))
      (is-true (appearance-unspecified-p
                (appearance-typography-spec-face
                 (appearance-role-style-typography selection))))
      (is (equal '(:marker ">")
                 (appearance-decoration-spec-parameters
                  (appearance-role-style-decoration selection))))
      (is (eq :bold (appearance-typography-spec-face
                     (appearance-role-style-typography minibuffer-emphasis))))
      (is-true (appearance-unspecified-p
                (appearance-typography-spec-face
                 (appearance-role-style-typography generic-selected))))
      (is (eq :bold (appearance-typography-spec-face
                     (appearance-role-style-typography minibuffer-selected))))
      (is (equal '(:marker ">")
                 (appearance-decoration-spec-parameters
                  (appearance-role-style-decoration minibuffer-selected)))))))

(defun appearance-test-dark-foreground (catalog role)
  "Return ROLE's exact dark foreground declaration for palette goldens."
  (appearance-ink-spec-foreground
   (appearance-role-style-foreground-ink
    (resolved-appearance-role-style
     (resolve-appearance-role catalog :dark role)))) )

(defun appearance-test-dark-surface (catalog role)
  "Return ROLE's exact dark surface declaration for palette goldens."
  (appearance-surface-spec-background
   (appearance-role-style-surface
    (resolved-appearance-role-style
     (resolve-appearance-role catalog :dark role)))))

(test appearance-dark-profile-has-the-exact-opaque-rgb-palette
  "Every Section-5 dark palette declaration is exact, opaque RGB data."
  (let ((catalog (make-classic-appearance-catalog)))
    (dolist (golden '((:base (:rgb 13/255 17/255 23/255))
                      (:transcript-pane (:rgb 13/255 17/255 23/255))
                      (:compose-pane (:rgb 13/255 17/255 23/255))
                      (:help-pane (:rgb 13/255 17/255 23/255))
                      (:info-pane (:rgb 22/255 27/255 34/255))
                      (:minibuffer-pane (:rgb 22/255 27/255 34/255))))
      (is (equal (second golden)
                 (appearance-test-dark-surface catalog (first golden)))))
    (dolist (golden '((:default-text (:rgb 230/255 237/255 243/255))
                      (:transcript-agent (:rgb 230/255 237/255 243/255))
                      (:modeline (:rgb 230/255 237/255 243/255))
                      (:selector-entry (:rgb 230/255 237/255 243/255))
                      (:transcript-user (:rgb 121/255 192/255 255/255))
                      (:transcript-tool (:rgb 126/255 231/255 135/255))
                      (:tool-result (:rgb 126/255 231/255 135/255))
                      (:selector-header (:rgb 126/255 231/255 135/255))
                      (:transcript-system (:rgb 177/255 186/255 196/255))
                      (:system (:rgb 177/255 186/255 196/255))
                      (:transcript-empty (:rgb 139/255 148/255 158/255))
                      (:selector-separator (:rgb 139/255 148/255 158/255))
                      (:selector-footer (:rgb 139/255 148/255 158/255))
                      (:error (:rgb 255/255 123/255 114/255))
                      (:selector-title (:rgb 165/255 214/255 255/255))
                      (:selector-selection (:rgb 255/255 255/255 255/255))
                      (:disabled (:rgb 139/255 148/255 158/255))))
      (is (equal (second golden)
                 (appearance-test-dark-foreground catalog (first golden)))))))

(test appearance-dark-profile-preserves-classic-typography-and-pointer-documentation
  "Dark changes no typography and leaves pointer documentation outside its base."
  (let ((catalog (make-classic-appearance-catalog)))
    (dolist (role '(:default-text :transcript-user :transcript-agent
                    :transcript-tool :transcript-system :transcript-empty
                    :system :error :tool-result :modeline :selector-title
                    :selector-header :selector-entry :selector-separator
                    :selector-footer :selector-selection
                    :minibuffer-selection-emphasis :disabled))
      (is (equal
           (appearance-role-style-key
            (make-appearance-role-style
             :typography
             (appearance-role-style-typography
              (resolved-appearance-role-style
               (resolve-appearance-role catalog :classic role)))))
           (appearance-role-style-key
            (make-appearance-role-style
             :typography
             (appearance-role-style-typography
              (resolved-appearance-role-style
               (resolve-appearance-role catalog :dark role))))))))
    (let ((pointer-style
            (resolved-appearance-role-style
             (resolve-appearance-role catalog :dark :pointer-documentation))))
      (is-true (appearance-unspecified-p
                (appearance-surface-spec-background
                 (appearance-role-style-surface pointer-style)))))))

(test appearance-dark-profile-contrast-goldens-cover-every-built-in-stack
  "All active dark stacks compose surface < content < state at 4.5:1 or better."
  (let ((catalog (make-classic-appearance-catalog)))
    (dolist (golden '(((:transcript-pane :default-text) 16.02d0)
                      ((:transcript-pane :transcript-agent) 16.02d0)
                      ((:help-pane :default-text) 16.02d0)
                      ((:compose-pane :default-text) 16.02d0)
                      ((:transcript-pane :transcript-user) 9.73d0)
                      ((:transcript-pane :transcript-tool) 12.32d0)
                      ((:transcript-pane :tool-result) 12.32d0)
                      ((:transcript-pane :transcript-system) 9.63d0)
                      ((:transcript-pane :system) 9.63d0)
                      ((:transcript-pane :transcript-empty) 6.15d0)
                      ((:transcript-pane :error) 7.51d0)
                      ((:info-pane :modeline) 14.64d0)
                      ((:minibuffer-pane :selector-title) 11.25d0)
                      ((:minibuffer-pane :selector-header) 11.26d0)
                      ((:minibuffer-pane :selector-entry) 14.64d0)
                      ((:minibuffer-pane :selector-separator) 5.62d0)
                      ((:minibuffer-pane :selector-footer) 5.62d0)
                      ((:minibuffer-pane :selector-entry :selector-selection) 17.30d0)
                      ((:minibuffer-pane :selector-entry :disabled) 5.62d0)))
      (let* ((stack (first golden))
             (style (resolved-appearance-role-style
                     (resolve-appearance-role-stack catalog :dark stack)))
             (ratio (clawmacs::appearance-contrast-ratio
                     (appearance-ink-spec-foreground
                      (appearance-role-style-foreground-ink style))
                     (appearance-surface-spec-background
                      (appearance-role-style-surface style)))))
        (is (<= (abs (- ratio (second golden))) 0.01d0))
        (is (>= ratio 4.5d0))))
    (is-true (clawmacs::validate-appearance-profile-contrast
              catalog (make-appearance-profile :selected-theme :dark)))))

(test appearance-dark-profile-contrast-policy-is-typed-and-never-recolors
  "User-touched low contrast warns unless strict; built-in failures are fatal."
  (let* ((catalog (make-classic-appearance-catalog))
         (override (test-appearance-style :foreground :black))
         (profile (make-appearance-profile
                   :selected-theme :dark
                   :role-overrides (list (cons :error override))))
         (captured nil))
    (handler-bind
        ((appearance-contrast-warning
           (lambda (warning)
             (setf captured warning)
             (muffle-warning warning))))
      (is-true (clawmacs::validate-appearance-profile-contrast catalog profile)))
    (is (equal '(:transcript-pane :error) (appearance-condition-role captured)))
    (is (equal '((:foreground-ink . :unsaved)
                 (:surface :theme :dark :owner :builtin))
               (appearance-condition-origin captured)))
    (is (equal '(:transcript-pane :error) (appearance-condition-path captured)))
    (is (eq :contrast (appearance-condition-axis captured)))
    (is (numberp (appearance-condition-value captured)))
    (is-false (appearance-condition-fatal-p captured))
    (is (equal '(:increase-foreground-contrast :change-surface)
               (appearance-condition-suggested-repairs captured)))
    (is (eq :black
            (appearance-ink-spec-foreground
             (appearance-role-style-foreground-ink override))))
    (signals appearance-contrast-warning
      (clawmacs::validate-appearance-profile-contrast
       catalog (make-appearance-profile
                :selected-theme :dark :strict-contrast t
                :role-overrides (list (cons :error override)))))
    (let* ((dark (find-appearance-theme-definition catalog :dark))
           (broken-dark
             (make-appearance-theme-definition
              :id :dark :parent-theme :classic :owner :builtin
              :role-overlays
              (cons (cons :error (test-appearance-style :foreground :black))
                    (remove :error (appearance-theme-definition-role-overlays dark)
                            :key #'car :test #'eq))))
           (broken-catalog
             (make-appearance-catalog
              :role-definitions (appearance-catalog-role-definitions catalog)
              :theme-definitions
              (cons broken-dark
                    (remove :dark (appearance-catalog-theme-definitions catalog)
                            :key #'appearance-theme-definition-id :test #'eq))
              :built-in-overlays (appearance-catalog-built-in-overlays catalog))))
      (signals appearance-contrast-warning
        (clawmacs::validate-appearance-profile-contrast
         broken-catalog (make-appearance-profile :selected-theme :dark))))))

(defun appearance-test-catalog-with-package-theme (owner)
  "Return a dark-derived package theme whose error foreground has low contrast."
  (let* ((catalog (make-classic-appearance-catalog))
         (package-theme-id '(:package "org.example.appearance" "low-dark"))
         (package-theme
           (make-appearance-theme-definition
            :id package-theme-id
            :parent-theme :dark
            :owner owner
            :role-overlays
            (list (cons :error (test-appearance-style :foreground :black))))))
    (values
     (make-appearance-catalog
      :role-definitions (appearance-catalog-role-definitions catalog)
      :theme-definitions
      (append (appearance-catalog-theme-definitions catalog)
              (list package-theme))
      :built-in-overlays (appearance-catalog-built-in-overlays catalog))
     package-theme-id)))

(defun appearance-test-catalog-with-package-defaults (owner)
  "Return package-owned surface/content role defaults with low contrast."
  (let* ((catalog (make-classic-appearance-catalog))
         (surface-id '(:package "org.example.appearance" "low-surface"))
         (content-id '(:package "org.example.appearance" "low-content")))
    (values
     (make-appearance-catalog
      :role-definitions
      (append
       (appearance-catalog-role-definitions catalog)
       (list (make-appearance-role-definition
              :id surface-id :kind :surface :owner owner)
             (make-appearance-role-definition
              :id content-id :kind :content :owner owner)))
      :theme-definitions (appearance-catalog-theme-definitions catalog)
      :built-in-overlays
      (append
       (appearance-catalog-built-in-overlays catalog)
       (list (cons surface-id
                   (test-appearance-style
                    :background '(:rgb 13/255 17/255 23/255)))
             (cons content-id
                   (test-appearance-style :foreground :black)))))
     (list surface-id content-id))))

(test appearance-contrast-provenance-identifies-built-in-contributions-by-owner
  "Core origins are explicitly owned; theme names and NIL never imply built-in."
  (let* ((catalog (make-classic-appearance-catalog))
         (resolved (resolve-appearance-role-stack
                    catalog :dark '(:transcript-pane :error)))
         (provenance (resolved-appearance-role-provenance resolved)))
    (is (every (lambda (role)
                 (eq :builtin (appearance-role-definition-owner role)))
               (appearance-catalog-role-definitions catalog)))
    (is (every (lambda (theme)
                 (eq :builtin (appearance-theme-definition-owner theme)))
               (appearance-catalog-theme-definitions catalog)))
    (is (equal '((:foreground-ink :theme :dark :owner :builtin)
                 (:surface :theme :dark :owner :builtin))
               provenance))
    (is-true (clawmacs::appearance-built-in-contrast-provenance-p provenance))
    (is-false
     (clawmacs::appearance-built-in-contrast-provenance-p
      '((:foreground-ink :theme :dark :owner nil)
        (:surface :theme :dark :owner :builtin))))))

(test appearance-package-theme-contrast-provenance-warns-and-strictly-fails
  "A package theme contributes at the theme layer and never counts as built-in."
  (multiple-value-bind (catalog theme-id)
      (appearance-test-catalog-with-package-theme "org.example.appearance")
    (let ((captured nil))
      (handler-bind
          ((appearance-contrast-warning
             (lambda (warning)
               (setf captured warning)
               (muffle-warning warning))))
        (is-true
         (clawmacs::validate-appearance-profile-contrast
          catalog (make-appearance-profile :selected-theme theme-id))))
      (is-false (appearance-condition-fatal-p captured))
      (is (equal
           `((:foreground-ink :theme ,theme-id
                              :owner "org.example.appearance")
             (:surface :theme :dark :owner :builtin))
           (appearance-condition-origin captured)))
      (is (equal '(:transcript-pane :error)
                 (appearance-condition-role captured)))
      (signals appearance-contrast-warning
        (clawmacs::validate-appearance-profile-contrast
         catalog (make-appearance-profile
                  :selected-theme theme-id :strict-contrast t))))))

(test appearance-package-role-default-contrast-provenance-warns-and-strictly-fails
  "Package role defaults retain the defaults layer and owner-aware policy."
  (multiple-value-bind (catalog role-stack)
      (appearance-test-catalog-with-package-defaults "org.example.appearance")
    (let ((captured nil))
      (handler-bind
          ((appearance-contrast-warning
             (lambda (warning)
               (setf captured warning)
               (muffle-warning warning))))
        (is-true
         (clawmacs::validate-appearance-profile-contrast
          catalog (make-appearance-profile :selected-theme :dark)
          :role-stacks (list role-stack))))
      (is-false (appearance-condition-fatal-p captured))
      (is (equal
           `((:foreground-ink :role-default ,(second role-stack)
                              :owner "org.example.appearance")
             (:surface :role-default ,(first role-stack)
                       :owner "org.example.appearance"))
           (appearance-condition-origin captured)))
      (signals appearance-contrast-warning
        (clawmacs::validate-appearance-profile-contrast
         catalog
         (make-appearance-profile :selected-theme :dark :strict-contrast t)
         :role-stacks (list role-stack))))))

(test appearance-nil-owned-contribution-warns-and-mixed-owners-remain-visible
  "NIL ownership is non-built-in and mixed effective contributors are preserved."
  (multiple-value-bind (catalog theme-id)
      (appearance-test-catalog-with-package-theme nil)
    (let ((captured nil))
      (handler-bind
          ((appearance-contrast-warning
             (lambda (warning)
               (setf captured warning)
               (muffle-warning warning))))
        (is-true
         (clawmacs::validate-appearance-profile-contrast
          catalog (make-appearance-profile :selected-theme theme-id))))
      (is-false (appearance-condition-fatal-p captured))
      (is (equal
           `((:foreground-ink :theme ,theme-id :owner nil)
             (:surface :theme :dark :owner :builtin))
           (appearance-condition-origin captured)))
      (is-false
       (clawmacs::appearance-built-in-contrast-provenance-p
        (appearance-condition-origin captured))))))

(test appearance-classic-custom-contrast-is-validated-while-built-in-is-inert
  "Classic skips its unknown backend surface only until a custom stack touches it."
  (let* ((catalog (make-classic-appearance-catalog))
         (overrides
           (list (cons :base
                       (test-appearance-style
                        :background '(:rgb 13/255 17/255 23/255)))
                 (cons :default-text
                       (test-appearance-style :foreground :black))))
         (profile (make-appearance-profile
                   :selected-theme :classic :role-overrides overrides))
         (captured nil))
    (is-true
     (clawmacs::validate-appearance-profile-contrast
      catalog (make-appearance-profile :selected-theme :classic)
      :role-stacks '((:transcript-pane :default-text))))
    (handler-bind
        ((appearance-contrast-warning
           (lambda (warning)
             (setf captured warning)
             (muffle-warning warning))))
      (is-true
       (clawmacs::validate-appearance-profile-contrast
        catalog profile
        :role-stacks '((:transcript-pane :default-text)))))
    (is-false (appearance-condition-fatal-p captured))
    (is (equal '((:foreground-ink . :unsaved)
                 (:surface . :unsaved))
               (appearance-condition-origin captured)))
    (signals appearance-contrast-warning
      (clawmacs::validate-appearance-profile-contrast
       catalog
       (make-appearance-profile
        :selected-theme :classic :strict-contrast t
        :role-overrides overrides)
       :role-stacks '((:transcript-pane :default-text))))))
