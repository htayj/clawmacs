(in-package :clawmacs/tests)

(in-suite appearance-config-suite)

(defun temporary-appearance-path ()
  (merge-pathnames (format nil "clawmacs-appearance-~D.sexp" (random most-positive-fixnum))
                   (uiop:temporary-directory)))

(defmacro with-temporary-appearance-path ((path) &body body)
  `(let ((,path (temporary-appearance-path)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,path)))))

(defun config-form (&optional (theme :dark) (overrides nil))
  (list :clawmacs-appearance :version 1 :theme theme :strict-contrast nil
        :overrides overrides))

(test appearance-config-rejects-reader-and-shape-boundaries
  (signals error (clawmacs::read-one-appearance-form "#. (progn 1)"))
  (signals error (clawmacs::read-one-appearance-form "(:classic) (:dark)"))
  (signals error (clawmacs::read-one-appearance-form "(aaaaaaaa)"))
  (signals error (clawmacs::read-one-appearance-form (make-string 32769 :initial-element #\x)))
  (signals error (clawmacs::read-one-appearance-form
                  (concatenate 'string (make-string 17 :initial-element #\()
                               ":classic" (make-string 17 :initial-element #\)))))
  (signals error (clawmacs::appearance-config-shape-valid-p
                  (make-list 65 :initial-element :classic) 0)))

(test appearance-config-rejects-oversized-string-leaf
  (signals error
    (clawmacs::read-one-appearance-form
     (format nil "(:clawmacs-appearance :version 1 :theme :classic :overrides ((:base :decoration (:marker ~S))))"
             (make-string 1025 :initial-element #\x)))))

(test appearance-config-no-external-symbol-interning
  (dolist (name '("ATTACKER-APPEARANCE-NAME" "CLASSIC?" "?CLASSIC"))
    (is-false (find-symbol name :clawmacs))
    (is-false (find-symbol name :keyword)))
  (dolist (text '("(attacker-appearance-name)"
                  "(:attacker-appearance-name)"
                  "(:classic?)" "(:?classic)" "(123?)"
                  "(\\:classic)" "(+)" "(.)" "(1..2)"))
    (signals error (clawmacs::read-one-appearance-form text)))
  (is (equal '(1/2 1.0e2 -0.25)
             (clawmacs::read-one-appearance-form
              "(1/2 1.0e2 -0.25)")))
  (is (equal '("literal :classic? #. attacker")
             (clawmacs::read-one-appearance-form
              "(\"literal :classic? #. attacker\")")))
  (is (equal '(:classic)
             (clawmacs::read-one-appearance-form
              "(; ignored :classic? #. attacker
:classic)")))
  (dolist (name '("ATTACKER-APPEARANCE-NAME" "CLASSIC?" "?CLASSIC"))
    (is-false (find-symbol name :clawmacs))
    (is-false (find-symbol name :keyword))))

(test appearance-config-public-boundaries-accept-n-and-reject-n-plus-one
  (let ((at-byte-limit
          (concatenate 'string "(:classic)"
                       (make-string (- 32768 (length "(:classic)"))
                                    :initial-element #\Space)))
        (over-byte-limit
          (concatenate 'string "(:classic)"
                       (make-string (- 32769 (length "(:classic)"))
                                    :initial-element #\Space)))
        (at-string-limit (make-string 1024 :initial-element #\x))
        (over-string-limit (make-string 1025 :initial-element #\x)))
    (is (equal '(:classic)
               (clawmacs::read-one-appearance-form at-byte-limit)))
    (signals error (clawmacs::read-one-appearance-form over-byte-limit))
    (is (= 1024 (length (first (clawmacs::read-one-appearance-form
                                (format nil "(~S)" at-string-limit))))))
    (signals error
      (clawmacs::read-one-appearance-form
       (format nil "(~S)" over-string-limit))))
  (is-true
   (clawmacs::read-one-appearance-form
    (concatenate 'string (make-string 16 :initial-element #\()
                 ":classic" (make-string 16 :initial-element #\)))))
  (signals error
    (clawmacs::read-one-appearance-form
     (concatenate 'string (make-string 17 :initial-element #\()
                  ":classic" (make-string 17 :initial-element #\)))))
  (is (= 64 (length (clawmacs::read-one-appearance-form
                     (format nil "(~{~A~^ ~})"
                             (make-list 64 :initial-element ":classic"))))))
  (signals error
    (clawmacs::read-one-appearance-form
     (format nil "(~{~A~^ ~})"
             (make-list 65 :initial-element ":classic")))))

(test appearance-config-rejects-nil-unknown-and-malformed-data
  (dolist (form (list (config-form :classic '((:base :foreground nil)))
                      (config-form :classic '((:base :unknown :white)))
                      (config-form :classic '((:base :foreground :orange)))
                      (config-form :classic '((:base :foreground :white :foreground :black)))
                      (list :clawmacs-appearance :version 2 :theme :classic)))
    (signals error (clawmacs::parse-appearance-profile-form form))))

(test appearance-config-preserves-package-identifiers-and-round-trips
  (let* ((form (config-form '(:package "org.example.plugin" "outline-dark")
                            '(((:package "org.example.plugin" "outline-heading")
                               :foreground (:rgb 0.4 0.6 0.9)))))
         (profile (clawmacs::parse-appearance-profile-form form))
         (serialized (serialize-appearance-profile profile))
         (round-trip (clawmacs::parse-appearance-profile-form
                      (clawmacs::read-one-appearance-form serialized))))
    (is (equal '(:package "org.example.plugin" "outline-dark")
               (appearance-profile-selected-theme profile)))
    (is (equal (appearance-profile-selected-theme profile)
               (appearance-profile-selected-theme round-trip)))
    (let ((style (cdar (appearance-profile-role-overrides round-trip))))
      (is (equal '(:rgb 0.4 0.6 0.9)
                 (appearance-ink-spec-foreground
                  (appearance-role-style-foreground-ink style)))))))

(test appearance-config-atomic-write-preserves-old-file-on-rename-failure
  (with-temporary-appearance-path (path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string "old" stream))
    (let ((clawmacs::*appearance-config-rename-function*
            (lambda (&rest ignored) (declare (ignore ignored)) (error "rename failed"))))
      (signals error (write-appearance-profile-file (make-appearance-profile :selected-theme :dark) path)))
    (is (string= "old" (uiop:read-file-string path)))))

(test appearance-config-selector-grammar-and-invalid-layer-fallback
  (is (eq :dark (parse-appearance-theme-selector "dark")))
  (is (equal '(:package "org.example.plugin" "outline-dark")
             (parse-appearance-theme-selector "org.example.plugin/outline-dark")))
  (signals error (parse-appearance-theme-selector "Dark"))
  (let ((cli (parse-appearance-startup-arguments
              '("--appearance-theme" "dark" "--appearance-theme" "classic"))))
    (is-false (getf cli :valid-p)))
  (let ((cli (parse-appearance-startup-arguments '("--appearance-theme=dark"))))
    (is-false (getf cli :valid-p)))
  (let ((cli (parse-appearance-startup-arguments nil)))
    (is-true (getf cli :valid-p))
    (is-false (getf cli :present-p)))
  (let ((warnings 0))
    (handler-bind ((warning (lambda (condition)
                              (declare (ignore condition))
                              (incf warnings)
                              (muffle-warning))))
      (let ((profile
              (resolve-startup-appearance-profile
               :path (temporary-appearance-path)
               :environment '(:present-p t :valid-p t :theme :dark)
               :cli (parse-appearance-startup-arguments nil))))
        (is (eq :dark (appearance-profile-selected-theme profile)))))
    (is (= 0 warnings)))
  (let ((profile (resolve-startup-appearance-profile
                  :path (temporary-appearance-path)
                  :environment '(:present-p t :valid-p t :theme :dark)
                  :cli '(:valid-p nil))))
    (is (eq :dark (appearance-profile-selected-theme profile)))))

(test appearance-config-preserves-selection-marker-parameters
  (let* ((profile
           (clawmacs::parse-appearance-profile-form
            (config-form :classic
                         '((:selector-selection
                            :decoration (:marker "=>"))))))
         (round-trip
           (clawmacs::parse-appearance-profile-form
            (clawmacs::read-one-appearance-form
             (serialize-appearance-profile profile))))
         (decoration
           (appearance-role-style-decoration
            (cdar (appearance-profile-role-overrides round-trip)))))
    (is (eq :selection-marker
            (appearance-decoration-spec-kind decoration)))
    (is (equal '(:marker "=>")
               (appearance-decoration-spec-parameters decoration)))))

(test appearance-config-file-failure-forces-classic-and-reload-rolls-back
  (with-temporary-appearance-path (path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string "(:clawmacs-appearance :version 99)" stream))
    (let ((warnings 0))
      (handler-bind ((warning (lambda (condition) (declare (ignore condition))
                                (incf warnings) (muffle-warning))))
        (let ((profile (resolve-startup-appearance-profile
                        :path path :environment '(:present-p t :valid-p t :theme :dark)
                        :cli '(:valid-p t :theme :dark))))
          (is (eq :classic (appearance-profile-selected-theme profile)))
          (is (= 1 warnings))))
    (let ((active (make-appearance-profile :selected-theme :dark)))
      (multiple-value-bind (profile reloaded-p)
          (reload-appearance-file-profile active :path path)
        (is-false reloaded-p)
        (is (eq :dark (appearance-profile-selected-theme profile))))))))

(test appearance-config-no-init-and-prompt-isolation-markers
  (let ((path (temporary-appearance-path))
        (clawmacs::*appearance-startup-resolution-count* 0)
        (clawmacs::*appearance-configuration-access-count* 0))
    (write-appearance-profile-file (make-appearance-profile :selected-theme :dark) path)
    ;; A GUI resolver does not consult the init inhibition flag, matching --no-init.
    (let ((clawmacs::*inhibit-user-init* t))
      (is (eq :dark (appearance-profile-selected-theme
                     (resolve-startup-appearance-profile :path path
                                                         :environment '(:present-p nil :valid-p t)
                                                         :cli '(:valid-p nil))))))
    ;; Prompt paths call neither reader nor resolver; this marker is their unit proof.
    (is (= 1 clawmacs::*appearance-startup-resolution-count*))
    (is (= 1 clawmacs::*appearance-configuration-access-count*))
    (delete-file path)))

(test appearance-config-save-staged-profile-does-not-apply
  (with-temporary-appearance-path (path)
    (let ((staged (make-appearance-profile :selected-theme :dark :strict-contrast t)))
      (save-staged-appearance-profile staged :path path)
      (multiple-value-bind (saved status) (read-appearance-profile-file path)
        (is (eq :valid status))
        (is (eq :dark (appearance-profile-selected-theme saved)))
        (is-true (appearance-profile-strict-contrast saved))))))
