(in-package #:mcclim-charms)

(defmethod graft-width ((graft charms-graft) &key (units :device))
  (declare (ignore units))
  (charms-port-width (port graft)))

(defmethod graft-height ((graft charms-graft) &key (units :device))
  (declare (ignore units))
  (charms-port-height (port graft)))
