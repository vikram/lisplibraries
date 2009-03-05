;;;; 
;;;; timers.lisp
;;;; 
;;;; Created: 2003-11-07 by Zach Beane <xach@xach.com>
;;;; 
;;;; Creating and manipulating timer structures.
;;;; 
;;;;
;;;; See the file COPYING for license information.
;;;; timers.lisp,v 1.5 2003/11/19 17:21:54 xach Exp

(in-package #:timer)

(defstruct (timer
             (:conc-name %timer-)
             (:constructor %make-timer))
  name
  function
  expire-time
  repeat-time
  thread)



(defmethod print-object ((object timer) stream)
  (print-unreadable-object (object stream :identity t)
    (let ((name (%timer-name object)))
      (if name
          (format stream "TIMER ~A" name)
          (princ "TIMER (unnamed)" stream)))))


(defun make-timer (function &key name (thread t))
  (%make-timer :name name :function function :thread thread))


(defun timer-name (timer)
  (%timer-name timer))


(defun timer-expired-p (timer &optional (delta 0.0))
  (or (null (%timer-expire-time timer))
      (let ((compare-time (+ (get-precise-time) delta)))
        (> compare-time (%timer-expire-time timer)))))
