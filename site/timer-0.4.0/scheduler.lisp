;;;; 
;;;; scheduler.lisp
;;;; 
;;;; Created: 2003-11-07 by Zach Beane <xach@xach.com>
;;;; 
;;;; Controlling the queue of scheduled events and running expired
;;;; timers.
;;;; 
;;;; 
;;;; See the file COPYING for license information.
;;;; scheduler.lisp,v 1.9 2003/11/19 17:21:39 xach Exp

(in-package #:timer)

;;;
;;; Control messages
;;;

(defun make-control-message (command timer)
  (cons command timer))


(defun control-message-command (message)
  (car message))


(defun control-message-timer (message)
  (cdr message))


;;;
;;; Control stack
;;;

(defvar *control-stack* (make-array 10
                                    :adjustable t
                                    :fill-pointer 0))

(defvar *control-stack-lock* (sb-thread:make-mutex))
(defvar *control-stack-waitqueue* (sb-thread:make-waitqueue))


(defmacro with-control-stack-lock (&body body)
  `(sb-thread:with-mutex (*control-stack-lock*)
     ,@body))


(defun next-control-message ()
  (with-control-stack-lock
    (loop (if (zerop (length *control-stack*))
              (sb-thread:condition-wait *control-stack-waitqueue*
                                        *control-stack-lock*)
              (return-from next-control-message
                (vector-pop *control-stack*))))))


(defun add-control-message (message)
  (with-control-stack-lock
    (vector-push-extend message *control-stack*))
  (sb-thread:condition-notify *control-stack-waitqueue*))


;;;
;;; Public interface (all manipulate the control stack)
;;;

(defun schedule-timer (timer absolute-time &optional repeat-time)
  (setf (%timer-expire-time timer) (universal-time-to-precise-time absolute-time)
        (%timer-repeat-time timer) repeat-time)
  (add-control-message (make-control-message :schedule timer))
  (values))


(defun schedule-timer-relative (timer relative-time &optional repeat-time)
  (setf (%timer-expire-time timer) (+ (get-precise-time) relative-time)
        (%timer-repeat-time timer) repeat-time)
  (add-control-message (make-control-message :schedule timer)))


(defun unschedule-timer (timer)
  (add-control-message (make-control-message :unschedule timer)))


;;; Not public, but related

(defun reschedule-timer (timer)
  (setf (%timer-expire-time timer) (+ (get-precise-time)
                                      (%timer-repeat-time timer)))
  (add-control-message (make-control-message :reschedule timer)))
        

;;;
;;; Expiring timers
;;;

(defun expire-timer (timer)
  (with-slots (function repeat-time)
      timer
    (if (%timer-thread timer)
	(sb-thread:make-thread function)
	(funcall function))
    (when repeat-time
      (reschedule-timer timer))))


(defun expire-pending-timers ()
  (loop
   (let ((next-timer (peek-schedule)))
     (unless next-timer
       (return-from expire-pending-timers))
     (if (> (get-precise-time) (%timer-expire-time next-timer))
         (expire-timer (priority-queue-extract-maximum *schedule*))
         (return-from expire-pending-timers)))))
       
          
   
   


;;;
;;; The scheduler
;;;

(defvar *schedule* (make-priority-queue :key #'%timer-expire-time))


(defun peek-schedule ()
  (priority-queue-maximum *schedule*))


(defun process-control-message (message)
  (ecase (control-message-command message)
    ((:schedule :reschedule)
     (priority-queue-insert *schedule* (control-message-timer message)))
    (:unschedule
     (let ((timer (control-message-timer message)))
       (priority-queue-remove *schedule* timer)
       (with-control-stack-lock
         (setf *control-stack*
               (delete-if #'(lambda (message)
                              (and (eq (control-message-command message)
                                       :reschedule)
                                   (eq (control-message-timer message)
                                       timer)))
                          *control-stack*)))))))

(defun time-to-next-timer ()
  (let ((timer (peek-schedule)))
    (when timer
      (- (%timer-expire-time timer) (get-precise-time)))))


(defmacro with-optional-timeout (delay &body body)
  (let ((d (gensym)))
    `(let ((,d ,delay))
       (cond ((null ,d)
              ,@body)
             ((<= ,d (float (/ 1 internal-time-units-per-second))) ; XXX NOT 0
              (error 'sb-ext:timeout))
             (t (sb-ext:with-timeout ,d
                  ,@body))))))


(defun scheduler ()
  (loop
   (expire-pending-timers)
   (let ((delay (time-to-next-timer)))
     (handler-case
         (with-optional-timeout delay
           (let ((message (next-control-message)))
             (process-control-message message)))
       (sb-ext:timeout () nil)))))
       


;;;
;;; Starting the system
;;;

(defvar *timers-enabled-p* nil)
(defvar *timers-enabled-mutex* (sb-thread:make-mutex))

(defun enable-timers ()
  (sb-thread:with-mutex (*timers-enabled-mutex*)
    (unless *timers-enabled-p*
      (setf *timers-enabled-p* t)
      (sb-thread:make-thread #'(lambda ()
				 (unwind-protect
				      (scheduler)
				   ;; XXX this doesn't seem to run if
				   ;;  the thread is killed
				   (setf *timers-enabled-p* nil))))
      (values))))


(defun timers-enabled-p ()
  *timers-enabled-p*)

