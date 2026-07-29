(require :asdf)
(asdf:load-system :mcclim-clx)

(defpackage #:clawmacs-mcclim-menu-boundary-probe
  (:use #:clim #:clim-lisp))

(in-package #:clawmacs-mcclim-menu-boundary-probe)

(defun record-probe (name &rest fields)
  (format *trace-output* "~&MENU-BOUNDARY-PROBE ~S ~S~%" name fields)
  (finish-output *trace-output*))

(define-application-frame menu-boundary-probe ()
    ()
  (:pretty-name "McCLIM Menu Boundary Probe")
  (:pane :application))

(define-command-table first-menu
  :menu (("First Action" :command com-first-action)
         ("Quit" :command com-quit)))

(define-command-table second-menu
  :menu (("Second Action" :command com-second-action)))

(make-command-table
 'menu-boundary-probe
 :inherit-from '(global-command-table)
 :menu '(("First" :menu first-menu)
         ("Second" :menu second-menu))
 :errorp nil)

(define-menu-boundary-probe-command com-first-action ()
  (record-probe :first-action))

(define-menu-boundary-probe-command com-second-action ()
  (record-probe :second-action))

(define-menu-boundary-probe-command com-quit ()
  (record-probe :quit)
  (frame-exit *application-frame*))

(record-probe :provenance
              :mcclim (asdf:system-source-directory :mcclim)
              :mcclim-clx (asdf:system-source-directory :mcclim-clx))

(let ((frame (make-application-frame 'menu-boundary-probe
                                     :width 900
                                     :height 548)))
  (record-probe :constructed :state (frame-state frame))
  (run-frame-top-level frame)
  (record-probe :stopped))
