(in-package :http-server)

;;; not used.

;;; `perhaps' MP wrappers.  Debugging support being much better when
;;; threads are not used, sometimes we want to fake it.



(defparameter *multiprocessing* nil)    ;shall we dance?

(defun pmp-maybe-make-process (func)
  (if *multiprocessing*
      (mp-make-process func)
    (funcall func)))

(defun pmp-accept-connection (fd) 
  (declare (optimize (speed 3))
           (inline accept-tcp-connection))
  ;(format t "multi ~A~% " *multiprocessing*)
  (if (and t *multiprocessing*)
      ;; this thing is slow and consy; it looks like it keeps allocating
      ;; unix:fd-sets every time it gets woken up
      (mp-wait-until-fd-usable fd :input))
  ;(format t "accepting ~% ")
  (accept-tcp-connection fd))

(defun pmp-set-current-process-name (name)
  (if *multiprocessing*
      (mp-set-process-name (mp-current-process) name)))

            
                      