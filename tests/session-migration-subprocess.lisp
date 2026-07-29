(require :asdf)

(load (or (uiop:getenv "RPLACA_QUICKLISP_SETUP")
          (error "RPLACA_QUICKLISP_SETUP is required")))

(push (truename (or (uiop:getenv "RPLACA_TEST_REPO_ROOT") "."))
      asdf:*central-registry*)

(ql:quickload :rplaca :silent t)

(let* ((canonical
         (uiop:ensure-directory-pathname
          (or (uiop:getenv "RPLACA_TEST_CANONICAL_SESSIONS")
              (error "RPLACA_TEST_CANONICAL_SESSIONS is required"))))
       (legacy
         (uiop:ensure-directory-pathname
          (or (uiop:getenv "RPLACA_TEST_LEGACY_SESSIONS")
              (error "RPLACA_TEST_LEGACY_SESSIONS is required"))))
       (barrier
         (uiop:ensure-directory-pathname
          (or (uiop:getenv "RPLACA_TEST_SESSION_BARRIER")
              (error "RPLACA_TEST_SESSION_BARRIER is required"))))
       (ready
         (merge-pathnames
          (format nil "ready-~D"
                  #+sbcl (sb-posix:getpid)
                  #-sbcl 0)
          barrier))
       (expected-ready-count
         (parse-integer
          (or (uiop:getenv "RPLACA_TEST_SESSION_BARRIER_COUNT") "2")))
       (crash-before-publish-p
         (string= (or (uiop:getenv
                       "RPLACA_TEST_SESSION_CRASH_BEFORE_PUBLISH")
                      "")
                  "1")))
  (let ((rplaca::+default-sessions-dir+ canonical)
        (rplaca::+legacy-sessions-dir+ legacy)
        (rplaca::*sessions-dir* canonical)
        (rplaca::*session-migration-before-publish-hook*
          (and crash-before-publish-p
               (lambda ()
                 #+sbcl (sb-ext:exit :code 77 :abort t)
                 #-sbcl (error "Injected migration crash."))))
        (rplaca::*session-migration-after-selection-hook*
          (lambda ()
            (ensure-directories-exist ready)
            (with-open-file (stream ready
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-line "ready" stream))
            (loop repeat 200
                  until (>= (length (uiop:directory-files barrier))
                            expected-ready-count)
                  do (sleep 0.05)
                  finally
                     (unless (>= (length (uiop:directory-files barrier))
                                 expected-ready-count)
                       (error
                        "Timed out waiting after legacy selection."))))))
    (rplaca::materialize-legacy-sessions-before-mutation)
    (unless (rplaca::completed-session-migration-p canonical legacy)
      (error "Published session migration did not validate."))))
