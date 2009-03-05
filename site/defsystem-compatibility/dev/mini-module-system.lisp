(in-package #:defsystem-compatibility)

;;?? Gary King 2006-01-17: a hack for GBBOpen - just ignores the sub-files
(defmethod %map-system-files ((system-names cl-user::mini-module) function
                             system-closure? include-pathname?)
  (declare (ignore system-closure? include-pathname?))
  (values nil))
