(in-package #:mcclim-charms)

(defmethod adopt-frame :after
    ((fm charms-frame-manager) (frame application-frame))
  (declare (ignore fm frame))
  nil)

(defmethod note-space-requirements-changed :after ((graft charms-graft) pane)
  (declare (ignore graft pane))
  nil)
