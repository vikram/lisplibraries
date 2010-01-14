;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          threads.lisp
;;;; Purpose:       Multithreading Utilities
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  April 2005
;;;; NOTES:
;;;; - Depends on the port library for mp access

(in-package :utils)


;; =================
;; Mailbox class
;; =================

(defclass-exported mp-mailbox ()
  ((get-lock :initform (port:make-lock))
   (put-lock :initform (port:make-lock))
   (data :accessor mailbox-contents :initform nil))
  (:documentation "A multithreading mailbox implementation for lisp"))

(defmethod-exported initialize-instance :after ((mb mp-mailbox) &rest args)
  "Ensure that our get lock is locked as we are initialized in an
   empty state"
  (declare (ignore args))
  (with-slots (get-lock) mb
    (port:get-lock get-lock)))

(defmethod-exported mp-write ((mb mp-mailbox) msg)
  "Write to a mailbox"
  (with-slots (get-lock put-lock data) mb
    (port:get-lock put-lock) ;; when empty
    (setf data msg)
    (port:giveup-lock get-lock)))

(defmethod-exported mp-read ((mb mp-mailbox))
  "Read from a mailbox"
  (with-slots (get-lock put-lock data) mb
    (port:get-lock get-lock)
    (let ((value data))
      (setf data nil)
      (port:giveup-lock put-lock)
      value)))

(defmethod-exported mp-empty-p ((mb mp-mailbox))
  "The mailbox is empty when you a get would block.
   Return t when get would block and the mailbox is empty."
  (with-slots (get-lock) mb
    (when (port:locked-p get-lock))))

;; (let ((writebox (make-instance 'mp-mailbox))
;;       (readbox (make-instance 'mp-mailbox)))
;;   (defun-exported test-thread ()
;;     (let ((data (mp-read writebox)))
;;       (format t "Thread has read ~A writing...~%" data)
;;       (mp-write readbox data)
;;       (format t "Thread is exiting~%")
;;       data))
;;   (defun-exported talk-to-thread (arg)
;;     (mp-write writebox arg)
;;     (mp-read readbox)))

;; ======================
;; Process interactions
;; ======================

(defun-exported kill-procs-by-substring (substring)
  (let ((regex (pregex:create-scanner substring)))
    (ignore-errors
      (mapc #'port:kill-process 
	      (select-if #'(lambda (x) 
			     (if (pregex:scan regex
					      (port:process-name x))
				 t))
			 (port:all-processes))))))

    
    
