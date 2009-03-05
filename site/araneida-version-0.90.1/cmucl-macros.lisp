(in-package :unix)

(defmacro defset-socket-integer-opt (name level option)
  `(defun ,name (socket &optional (activated-p t))
     ,(format nil "Set (or with optional ACTIVATED-P NIL, unset) the ~A option on SOCKET" option)
     (let ((aval (make-alien int)))
       (unwind-protect
           (progn
             (setf (deref aval) (if activated-p 1 0))
             (unix-setsockopt socket ,level ,option aval 4))
         (free-alien aval)))))
