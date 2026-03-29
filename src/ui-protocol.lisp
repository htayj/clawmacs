(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; UI Backend Protocol
;;; --------------------------------------------------------------------------

(defclass ui-backend ()
  ()
  (:documentation "Base class for UI backends. Subclass and implement BACKEND-RUN."))

(defvar *ui-backend* nil
  "The active UI backend. Set in ~/.clawmacs.d/init.lisp before startup,
or left nil to use the default croatoan terminal backend.")

(defgeneric backend-run (backend initial-buffer)
  (:documentation "Run the UI. Blocking call that initializes the display, runs the event loop,
and tears down on exit. The backend must:
  1. Set *scroll-page-size* based on display dimensions
  2. Render INITIAL-BUFFER
  3. Read input events, normalize to abstract key specs, call HANDLE-KEY-EVENT
  4. Poll UPDATE-STREAMING-RESPONSE and re-render when a buffer is streaming
  5. Re-render after every event
  6. Return when HANDLE-KEY-EVENT returns :QUIT"))
