(in-package :araneida)

(defun getenv (&rest args)
  (declare (ignore args))
  nil) ;; FIXME

(defun forcibly-close-stream (s)
  (let ((fd (sys:fd-stream-fd s)))
    (multiple-value-bind (r e) (ignore-errors (close s) t)
      (unless r
	(format t "Unable to close fd ~A: ~A, trying harder ~%" fd e)
	(multiple-value-bind (r e) (ignore-errors (close s :abort t) t)
	  (unless r
	    (format t "still unable to close ~A: ~A, try harder ~%" fd e)
	    (multiple-value-bind (r e)
		(ignore-errors (unix:unix-close fd) t)
	      (unless r
		(format t "Even unix-close failed on ~A:~A, giving up~%"
			fd e)))))))))

(defun platform-handle-debugger-condition (condition)
  (typecase condition
    (cmucl-timeout
     (format *trace-output* "timed out processing request: ~A~%" condition))
    (t
     (format *trace-output* "Caught error: ~A~%" condition)
     (debug:backtrace 15 *trace-output*))))

(defun listener-accept-stream (listener)
  (let ((socket (ext:accept-tcp-connection
                 (http-listener-socket listener))))
    (sys:make-fd-stream socket :element-type 'base-char
                        :input t :output t)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-file-error-handlers (form handler-message)
    `(handler-case
      ,form
      (kernel:simple-file-error (c)
       (signal 'http-not-found 
        :message ,handler-message)))))

(defun host-close-socket (socket)
  (ext:close-socket socket))

(defun host-make-listener-socket (address port)
  (ext:create-inet-listener port :stream
                            :reuse-address t
                            :host
                            (ext:htonl (loop for i from 3 to 0 summing
                                             (* (expt 256 i)
                                                (elt address (- 3 i)))))))

(defun host-add-fd-handler (listener handler)
  (sys:add-fd-handler
   (http-listener-socket listener) :input handler))

(defun host-remove-fd-handler (handler)
  (sys:remove-fd-handler handler))

(defmacro host-without-other-handlers (&body body)
  `(let ((cl::*descriptor-handlers* nil))
    ,@body))

;;; host-with-timeout

;;; adapted from code by Derek L Davies <ddavies@world.std.com> posted
;;; to cmucl-help@cons.org on 2000-07-13.

;;; Munged code from CMUCL's multiprocessing START-SIGALRM-YIELD
;;; function.  Might be dangerous: needs to be heavily tested.
;;;
;;; Examples:
;;;
;;; (with-timeout (5 (progn (format t "Timed out!~%") 'time-is-up))
;;;   (do ((i 0 (+ i 1))) ((> i 3) 'done)
;;;     (sleep 3) (format t "hi~%")))
;;; => TIME-IS-UP
;;;
;;; (with-timeout (30 (progn (format t "Timed out!~%") 'time-is-up))
;;;   (do ((i 0 (+ i 1))) ((> i 3) 'done)
;;;    (sleep 3) (format t "hi~%")))
;;; => DONE

(define-condition cmucl-timeout (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "A timeout occured.~%"))))

(defvar *nested-with-timeout* nil)

(defmacro host-with-timeout (timeout-symbol &rest body)
  (let ((catch-tag (gensym))
        (secs (gensym))
        (usecs (gensym)))
    `(unwind-protect
	 (catch ',catch-tag
	   (if *nested-with-timeout*
	       (progn
		 (format *debug-io* "Nested with-timeout running under CMUCL.")
		 (setq *nested-with-timeout* nil))
	       (progn
		 (prog2
		     (progn
		       (setq *nested-with-timeout* t)
		       ;; Disable the gencgc pointer filter to improve interrupt safety.
		       #+(and gencgc nil)
		       (setf (alien:extern-alien "enable_pointer_filter" alien:unsigned) 0)
		       (flet ((sigalrm-handler (signal code scp)
				(declare (ignore signal code scp))
				(when (<= lisp::*free-interrupt-context-index* 1)
				  (progn
				    (setq *nested-with-timeout* nil)
				    (throw ',catch-tag (signal 'cmucl-timeout))))))
			 (sys:enable-interrupt :sigalrm #'sigalrm-handler))
		       (multiple-value-bind (,secs ,usecs)
			   (floor (floor (* 1000000.0 ,timeout-symbol)) 1000000)
			 (unix:unix-setitimer :real 0 0 ,secs ,usecs)))
		     (progn ,@body)
		   ;; This resets the timer.  If we fail to do this we'll get a throw after
		   ;; we've exited the catch.
		   (unix:unix-setitimer :real 0 0 0 0)
		   (setq *nested-with-timeout* nil))))))))