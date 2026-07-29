(require :asdf)

(let ((setup (uiop:getenv "CLAWMACS_QUICKLISP_SETUP")))
  (unless (and setup (plusp (length setup)))
    (error "Missing CLAWMACS_QUICKLISP_SETUP"))
  (load setup :verbose nil :print nil))

(push (truename ".") asdf:*central-registry*)
(funcall (symbol-function (find-symbol "QUICKLOAD" "QL"))
         :clawmacs :silent t)

(let ((mode (or (uiop:getenv "CLAWMACS_CRASH_TEST_MODE") "main")))
  (setf
   (symbol-function 'clawmacs::initialize-clawmacs-runtime)
   (cond
     ((string= mode "main")
      (lambda ()
        (error "intentional crash prompt sentinel 86f4d337")))
     ((string= mode "worker")
      (lambda ()
        (bt:make-thread
         (lambda ()
           (error "intentional worker crash sentinel 34ecb3ce"))
         :name "clawmacs crash integration worker")
        (loop (sleep 1))))
     ((string= mode "handled")
      (lambda ()
        (handler-case
            (error "ordinary handled sentinel 3554f85b")
          (error () nil))))
     (t
      (error "Unknown CLAWMACS_CRASH_TEST_MODE: ~A" mode))))
  (clawmacs:clawmacs-main
   :session-name "crash-report-integration"
   :run-frame nil))
