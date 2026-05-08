(in-package #:mcclim-charms)

(defun %set-xterm-button-event-tracking (enabled-p)
  (format *terminal-io* "~C[?1002;1006~A" #\Esc (if enabled-p "h" "l"))
  (force-output *terminal-io*))

(defun %configure-window (window)
  (charms:disable-echoing)
  (charms:enable-raw-input :interpret-control-characters t)
  (charms:enable-extra-keys window)
  (charms:enable-non-blocking-mode window)
  ;; Let ncurses assemble xterm mouse escape sequences instead of delivering
  ;; the bytes as separate key events when input arrives through a PTY.
  (safe-call (lambda () (setf ll:*ESCDELAY* 200)))
  (safe-call (lambda () (ll:wtimeout (charms-window-pointer window) 200)))
  (safe-call (lambda () (ll:start-color)))
  (safe-call (lambda ()
               (ll:mousemask (logior ll:ALL_MOUSE_EVENTS
                                      ll:REPORT_MOUSE_POSITION))))
  (safe-call (lambda () (%set-xterm-button-event-tracking t)))
  window)

(defun initialize-charms-port (port)
  "Initialize curses for PORT and record the terminal dimensions.
This function is idempotent. It deliberately owns terminal mode setup because
McCLIM backends, not applications, are responsible for display-server state."
  (unless (charms-port-initialized-p port)
    (let ((window (charms:initialize)))
      (%configure-window window)
      (setf (charms-port-window port) window)
      (multiple-value-bind (width height)
          (terminal-size-from-window window)
        (setf (charms-port-width port) width
              (charms-port-height port) height))
      (setf (charms-port-initialized-p port) t)))
  port)

(defun finalize-charms-port (port)
  (when (charms-port-initialized-p port)
    (safe-call (lambda () (%set-xterm-button-event-tracking nil)))
    (setf (charms-port-initialized-p port) nil
          (charms-port-window port) nil)
    (charms:finalize))
  port)

(defmacro with-charms-port ((port) &body body)
  `(unwind-protect
        (progn
          (initialize-charms-port ,port)
          ,@body)
     (finalize-charms-port ,port)))
