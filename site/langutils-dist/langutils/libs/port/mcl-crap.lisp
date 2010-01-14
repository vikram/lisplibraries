;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mcl-crap.lisp
;;;; Purpose:       Mockups of functions not prossent en mcl but port call them ...
;;;; Disclamer:	    This functions dont do anything, they are a work arround for make the system compile in mcl.
;;;; Programmer:    Jose Espinosa
;;;; Date Started:  April 2005
;;;;


(in-package :ccl)

#+digitool
(defun make-socket (&key remote-host remote-port port format)
  (declare (ignore remote-host remote-port port format))
  (error 'not-implemented :proc (list :make-socket))
  nil)

#+digitool
(defun process-yield (&rest args)
  (declare (ignore args))
  (ccl:process-allow-schedule))

#+digitool
(defun process-state (process)
  (process.whostate process))

#+openmcl
(defun process-run-reasons (p)
  (declare (ignore p)) 
  ;(process.run-reasons (require-type p 'process))
  (error 'not-implemented :proc (list :process-run-reasons))
  nil)

#+openmcl
(defvar *all-processes*)

#+openmcl
(defun make-process-queue (name &optional size)
  (declare (ignore name size))
  ;(%cons-process-queue name size)
  (error 'not-implemented :proc (list :make-process-queue))
  nil)

#+openmcl
(defun process-enqueue (queue &optional queue-value (whostate "Lock"))
  (declare (ignore queue queue-value whostate))
  ;(process-enqueue-with-timeout queue nil queue-value whostate)
  (error 'not-implemented :proc (list :process-enqueue))
  nil)

#+openmcl
(defun process-dequeue (queue &optional queue-value (error-p t))
  (declare (ignore queue queue-value error-p))
  (error 'not-implemented :proc (list :process-dequeue))
  nil)

#+openmcl
(defmacro with-process-enqueued ((queue &optional queue-value whostate 
					(signal-dequeue-errors t sdq?)) 
				 &body body)
  (declare (ignore queue queue-value whostate signal-dequeue-errors))
  `(progn ,@body))

#+openmcl
(defun process-lock (name)
  (declare (ignore name))
  ;(%cons-process-queue name size)
  (error 'not-implemented :proc (list :process-lock))
  nil)

(export 'make-socket)
(export 'process-yield)
(export 'process-state)
(export 'process-active-p)
(export 'process-run-reasons)
(export '*all-processes*)
(export 'make-process-queue)
(export 'process-enqueue)
(export 'process-dequeue)
(export 'with-process-enqueued)
(export 'process-lock)
(export 'process-unlock)