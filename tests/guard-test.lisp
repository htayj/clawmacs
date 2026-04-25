(in-package :clawmacs/tests)

(in-suite guard-suite)

(defun temp-guard-policy-path ()
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "clawmacs-guard-~A"
                                                      (list (get-universal-time)
                                                            (get-internal-real-time)
                                                            (gensym)))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames "guard.json" base)))

(defun temp-guard-project-directory ()
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "clawmacs-guard-project-~A"
                                                      (list (get-universal-time)
                                                            (get-internal-real-time)
                                                            (gensym)))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (uiop:ensure-directory-pathname base)))

(defmacro with-guard-policy-path-override ((path) &body body)
  `(let ((clawmacs::*approval-policy-path* ,path)
         (clawmacs::*approval-policy-registry* nil)
         (clawmacs::*approval-policy-project-registry-cache*
           (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-guard-tools-restored (&body body)
  `(with-tool-table-restored
     (let ((clawmacs::*approval-policy-registry* nil)
           (clawmacs::*approval-policy-project-registry-cache*
             (make-hash-table :test #'equal)))
       ,@body)))

(defun guard-test-buffer (name working-directory)
  (make-buffer name
               :agent-name "agent"
               :working-directory (uiop:ensure-directory-pathname working-directory)))

(test guard-policy-round-trips-defaults-overrides-and-history
  "Guard policy JSON persists defaults, overrides, and audit history."
  (let ((path (temp-guard-policy-path)))
    (with-guard-policy-path-override (path)
      (clawmacs::set-approval-policy-default-permission :agent-with-permission)
      (clawmacs::set-approval-policy-default-sandbox-permission :read-only)
      (clawmacs::set-approval-policy-default-network-permission :deny)
      (clawmacs::set-approval-policy-default-working-directory-permission
       :project-root)
      (clawmacs::set-approval-policy-tool-permission "write" :user-only)
      (clawmacs::set-approval-policy-sandbox-permission "write" :full-access)
      (clawmacs::set-approval-policy-network-permission "netcons_run" :allow)
      (clawmacs::set-approval-policy-working-directory-permission "write"
                                                                 :workspace)
      (clawmacs::approval-policy-record-history-entry
       nil "write" :denied
       :policy :interactive
       :reason "prompt denied"
       :entry '((:path . "/tmp/guard-policy.txt")))
      (clawmacs::save-approval-policy)
      (setf clawmacs::*approval-policy-registry* nil)
      (progn
        (clawmacs::load-approval-policy)
        (is (eq :agent-with-permission
                (clawmacs:approval-policy-default-permission)))
        (is (eq :read-only
                (clawmacs:approval-policy-default-sandbox-permission)))
        (is (eq :deny
                (clawmacs:approval-policy-default-network-permission)))
        (is (eq :project-root
                (clawmacs:approval-policy-default-working-directory-permission)))
        (is (eq :user-only
                (clawmacs:approval-policy-tool-permission "write")))
        (is (eq :full-access
                (clawmacs:approval-policy-sandbox-permission "write")))
        (is (eq :allow
                (clawmacs:approval-policy-network-permission "netcons_run")))
        (is (eq :workspace
                (clawmacs:approval-policy-working-directory-permission
                 "write")))
        (is (= 1 (length (clawmacs:approval-policy-history-entries))))
        (let ((history (clawmacs:approval-policy-history-entries)))
          (is (string= "write" (cdr (assoc :tool-name (first history)))))
          (is (string= "denied" (cdr (assoc :decision (first history)))))
          (is (search "Network default: deny"
                      (clawmacs:approval-policy-history-to-string))))))))

(test guard-project-local-policy-isolated-from-user-policy
  "Project-local guard policy stays separate from the user policy."
  (let ((user-path (temp-guard-policy-path))
        (project-dir (temp-guard-project-directory)))
    (with-guard-policy-path-override (user-path)
      (clawmacs::set-approval-policy-default-permission :agent-allowed)
      (clawmacs::save-approval-policy)
      (let* ((project-buffer (guard-test-buffer "guard-project" project-dir))
             (project-path (clawmacs::approval-policy-path-for-buffer
                            project-buffer))
             (other-buffer (guard-test-buffer "guard-other"
                                              (temp-guard-project-directory))))
        (clawmacs::set-approval-policy-tool-permission
         "write" :user-only :buffer project-buffer)
        (clawmacs::set-approval-policy-default-network-permission
         :deny :buffer project-buffer)
        (clawmacs::save-approval-policy :buffer project-buffer)
        (setf clawmacs::*approval-policy-registry* nil
              clawmacs::*approval-policy-project-registry-cache*
              (make-hash-table :test #'equal))
        (is (string= (namestring project-path)
                     (namestring (clawmacs::approval-policy-path-for-buffer
                                  project-buffer))))
        (is (eq :user-only
                (clawmacs:approval-policy-tool-permission
                 "write" :buffer project-buffer)))
        (is (null
             (clawmacs:approval-policy-tool-permission
              "write" :buffer other-buffer)))
        (is (eq :agent-allowed
                (clawmacs:approval-policy-default-permission)))
        (is (null
             (clawmacs:approval-policy-default-permission
              :buffer other-buffer)))
        (is (eq :deny
                (clawmacs:approval-policy-default-network-permission
                 :buffer project-buffer)))
        (is (null
             (clawmacs:approval-policy-default-network-permission
              :buffer other-buffer)))))))

(test guard-review-hook-captures-recorded-decisions
  "The approval review hook receives audit entries for recorded decisions."
  (let ((path (temp-guard-policy-path))
        (events nil))
    (with-guard-policy-path-override (path)
      (let ((clawmacs::*approval-review-hook*
              (list (lambda (buffer tool-name decision policy entry)
                      (push (list :buffer buffer
                                  :tool-name tool-name
                                  :decision decision
                                  :policy policy
                                  :entry entry)
                            events)))))
        (clawmacs::approval-policy-record-history-entry
         nil "write" :denied
         :policy :prompt-mode
         :reason "hook test"
         :entry '((:path . "/tmp/guard-hook.txt")))
        (is (= 1 (length events)))
        (let ((event (first events)))
          (is (string= "write" (getf event :tool-name)))
          (is (eq :denied (getf event :decision)))
          (is (eq :prompt-mode (getf event :policy)))
          (is (string= "hook test"
                       (cdr (assoc :reason (getf event :entry))))))))))

(test guard-sandbox-and-network-policies-affect-discovery
  "Sandbox and network guard settings affect provider tool discovery."
  (with-guard-tools-restored
    (clawmacs::init-tools)
    (let ((path (temp-guard-policy-path))
          (buffer (guard-test-buffer "guard-discovery"
                                     (temp-guard-project-directory))))
      (with-guard-policy-path-override (path)
        (clawmacs::set-approval-policy-default-sandbox-permission :read-only)
        (clawmacs::set-approval-policy-default-network-permission :deny)
        (clawmacs::set-approval-policy-sandbox-permission "write" :full-access)
        (clawmacs::set-approval-policy-network-permission "netcons_run"
                                                          :allow)
        (clawmacs:set-package-enablement-scope "lispi" :global)
        (clawmacs:load-active-packages)
        (let* ((*current-caller* :agent)
               (tool-names (mapcar (lambda (tool)
                                     (cdr (assoc :name tool)))
                                   (coerce (clawmacs::tool-definitions-for-api
                                            :buffer buffer)
                                           'list))))
          (is (member "write" tool-names :test #'string=))
          (is (not (member "edit" tool-names :test #'string=)))
          (is (eq :full-access
                  (clawmacs::effective-tool-sandbox-permission
                   "write" :buffer buffer)))
          (is (eq :allow
                  (clawmacs::effective-tool-network-toggle
                   "netcons_run" :buffer buffer))))))))
