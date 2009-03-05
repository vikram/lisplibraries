;;;; 
;;;; time.lisp
;;;; 
;;;; Created: 2003-11-07 by Zach Beane <xach@xach.com>
;;;; 
;;;; Getting time at higher resolutions than one second.
;;;; 
;;;;
;;;; See the file COPYING for license information.
;;;; time.lisp,v 1.6 2003/11/14 20:48:33 xach Exp


(in-package #:timer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-precise-time ()
    (declare (optimize (speed 3) (safety 0)))
    "Return a double-float value representing the current time in
seconds. The absolute value is meaningless, but it is useful for
relative comparision between two precise times."
    (/ (get-internal-real-time)
       #.(float internal-time-units-per-second 1.0d0))))

(defconstant +universal-time-offset+ (- (get-universal-time)
					(truncate (get-precise-time))))

(defun universal-time-to-precise-time (universal-time)
  (- universal-time +universal-time-offset+))
