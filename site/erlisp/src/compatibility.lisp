;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file implements various wrappers around implementation specific
;;;; functionality.
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Threads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+allegro (eval-when (:compile-top-level :load-top-level :execute)
	    (require :process))

(defun current-thread ()
  #+sbcl sb-thread:*current-thread*
  #+(or allegro cmu) mp:*current-process*
  #+openmcl ccl::*current-process*
  #-(or sbcl allegro cmu openmcl)
  (error  "Threads are not yet implemented for your compiler."))

(defun make-thread (thunk)
  #+sbcl (sb-thread:make-thread thunk)
  #+allegro (mp:process-run-function "erlisp-thread" thunk)
  #+cmu (mp:make-process thunk)
  #+openmcl (ccl::process-run-function '(:name "erlisp-thread") thunk)
  #-(or sbcl allegro cmu openmcl)
  (error  "Threads are not yet implemented for your compiler."))

(defun make-mutex ()
  #+sbcl (sb-thread:make-mutex)
  #+allegro (mp:make-process-lock)
  #+cmu (mp:make-lock)
  #+openmcl (ccl::make-lock)
  #-(or sbcl allegro cmu openmcl) 
  (error "Threads are not yet implemented for your compiler."))

(defmacro with-mutex ((mutex) &body body)
  #+sbcl `(sb-thread:with-mutex (,mutex) ,@body)
  #+allegro `(mp:with-process-lock (,mutex) ,@body)
  #+cmu `(mp:with-lock-held (,mutex) ,@body)
  #+openmcl `(ccl::with-lock-grabbed (,mutex) ,@body)
  #-(or sbcl allegro cmu openmcl) 
  (error "Threads are not yet implemented for your compiler."))

;to find out how to implement this on your compiler
;(macroexpand-1 '(with-mutex (*mutex*) nil))
(defun lock-mutex (mutex)
  #+sbcl (sb-thread:get-mutex mutex nil t)
  #+allegro (mp:process-lock mutex mp:*current-process* "Lock" nil)
  #+cmu (mp::lock-wait mutex "Lock")
  #+openmcl (ccl::grab-lock mutex)
  #-(or sbcl allegro cmu openmcl)
  (error  "Threads are not yet implemented for your compiler."))

(defun unlock-mutex (mutex)
  #+sbcl (sb-thread:release-mutex mutex)
  #+allegro (mp:process-unlock mutex)
  #+cmu (setf (mp::lock-process mutex) nil)
  #+openmcl (ccl::release-lock mutex)
  #-(or sbcl allegro cmu openmcl) 
  (error "Threads are not yet implemented for your compiler."))

#+cmu (defstruct erlisp-event
        mutex
        active)

(defun make-event ()
  #+sbcl (sb-thread:make-waitqueue)
  #+cmu (make-erlisp-event :mutex (make-mutex))
  #+allegro (mp:make-gate nil)
  #+openmcl (ccl:make-semaphore)
  #-(or sbcl allegro cmu openmcl) 
  (error "Threads are not yet implemented for your compiler."))

(defun event-wait (event mutex)
;note: this function should appear inside with-mutex block
  #+sbcl (sb-thread:condition-wait event mutex)
  #+allegro (progn (unlock-mutex mutex)
                   (mp:process-wait "wait for message" 
				      #'mp:gate-open-p
				      event)
                   (lock-mutex mutex)
                   (mp:close-gate event))
  #+cmu (progn (setf (erlisp-event-active event) nil)
	       (unlock-mutex mutex) 
	       (do ()
		   ((when (erlisp-event-active event)
		      (lock-mutex mutex)
		      t))
		 (mp:process-yield)))
  #+openmcl (progn (unlock-mutex mutex)
                   (unwind-protect
                        (ccl:wait-on-semaphore event)
                     (lock-mutex mutex)))
  #-(or sbcl allegro cmu openmcl) 
  (error "Threads are not yet implemented for your compiler."))

(defun event-notify (event)
  #+sbcl (sb-thread:condition-notify event)
  #+cmu (with-mutex ((erlisp-event-mutex event))
          (setf (erlisp-event-active event) t))
  #+allegro (mp:open-gate event)
  #+openmcl (ccl:signal-semaphore event)
  #-(or sbcl allegro cmu openmcl) 
  (error "Threads are not yet implemented for your compiler."))

#+openmcl
(define-condition openmcl-timeout (condition) ())

(defmacro with-timeout ((milliseconds &rest after-timeout) &body body)
  #+sbcl `(sb-ext:with-timeout (/ ,milliseconds 1000)
            (handler-case (progn ,@body)
              (sb-ext:timeout () ,@after-timeout)))
  #+allegro `(mp:with-timeout ((/ ,milliseconds 1000) ,@after-timeout)
		       ,@body)
  #+cmu `(mp:with-timeout ((/ ,milliseconds 1000.0) ,@after-timeout)
		       ,@body)
  #+openmcl `(let ((interrupt-thread nil))
               (setf interrupt-thread
                     (ccl:process-run-function 'timeout
                       (let ((process ccl:*current-process*))
                         (lambda ()
                           (sleep (/ ,milliseconds 1000.0))
                           (ccl:process-interrupt process
                              (lambda ()
                                (signal 'openmcl-timeout)))))))
               (prog1
                   (handler-case
                       (progn ,@body)
                     (openmcl-timeout () ,@after-timeout))
                 (if interrupt-thread
                     (ccl:process-kill interrupt-thread))))
  #-(or sbcl allegro cmu openmcl) 
  (error "Timeouts are not yet implemented for your compiler."))
