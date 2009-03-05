(in-package :araneida)

;;; There is still work to be done abstracting out the commonality
;;; between this file and serve-event-http-listener.lisp

(defvar *handler-timeout* 60
  "Maximum number of seconds to spend on a request.  Applies both to
normal processing and to time spent in the debugger, should there be
an unhandled error")

(defun add-listener-thread (listener)
  (let ((thread (make-http-thread :last-hit 0)))
    (setf (http-thread-pid thread)
	  (host-run-thread
           'listener-thread
	   (lambda ()
	     (loop
	      (threaded-http-listener-accept-one-request listener thread)))))
    thread))

(defun threaded-http-listener-accept-one-request (listener thread)
  (handler-case
      (with-accept-flets 
	  (let ((s (accept listener)))
	    (host-with-timeout *handler-timeout*
			       (setf (http-thread-last-hit thread) nil)
			       (unwind-protect
				    (do-it listener s)
				 (setf (http-thread-last-hit thread) (get-universal-time))
				 (forcibly-close-stream s)))))
    (end-of-file () (let ((r (find-restart 'abort-response)))
		      (when r
			(invoke-restart r))))))

(defmethod start-listening ((listener threaded-http-listener)
			    &key (threads 5))
  (let ((socket
	 (host-make-listener-socket (http-listener-address listener)
                                    (http-listener-port listener))))
    (setf (http-listener-socket listener) socket)
    (dotimes (i threads)
      (push (add-listener-thread listener) (http-listener-threads listener)))
    (setf (http-listener-thread listener)
	  (host-run-thread
           'master-thread
	   (lambda ()
	     (loop (master-thread-one-iter listener)))))))

(defun master-thread-one-iter (listener)
  (let ((min (http-listener-min-spare listener))
	(max (http-listener-max-spare listener))
	(spares 0))
    ;; loop over the children.  count 1 for every stale thread
    ;; when spare-count > max, kill stale threads as they're
    ;; encountered when we get to the end, if spare-count < min,
    ;; start another if no change required, sleep a bit
    (dolist (this (http-listener-threads listener))
      (when (numberp (http-thread-last-hit this))
	(cond ((not (host-thread-alivep (http-thread-pid this)))
	       (format t ";; Thread!  Is!  Dead!  Ahaaaa (~A)~%"
		       (http-thread-pid this))
	       (setf (http-thread-pid this) nil))
	      ((> max spares)
	       #+nil
	       (format t "thread ~a last used ~A, ~A spare so far~%"
		       (http-thread-pid this)
		       (http-thread-last-hit this) spares)
	       (incf spares))
	      (t (format t ";; thread ~A last used ~A, killing~%"
			 (http-thread-pid this)
			 (http-thread-last-hit this))
		 (host-thread-kill (http-thread-pid this))
		 (setf (http-thread-pid this) nil)))))
    (setf (http-listener-threads listener)
	  (remove-if #'null (http-listener-threads listener)
		     :key #'http-thread-pid))
    (when (< spares min)
      (dotimes (i (- min spares))
	(format t ";; ~A spare threads < ~A, adding another~%" spares min)
	(push (add-listener-thread listener)
	      (http-listener-threads listener))))
    (when (<= min spares max)		; no change
      ;; (format t "no change ~%")
      (sleep 60))
    (sleep 1)))
	    
(defmethod stop-listening ((listener threaded-http-listener)
			   &key abort &allow-other-keys)
  (declare (ignore abort))		;FIXME we always abort.  not very nice
  (dolist (thread (http-listener-threads listener))
    (when (host-thread-alivep (http-thread-pid thread))
      (host-thread-kill (http-thread-pid thread))))
  (awhen (http-listener-thread listener) (host-thread-kill it))
  (host-close-socket (http-listener-socket listener))
  (setf (http-listener-threads listener) nil
	(http-listener-thread listener) nil
        (http-listener-socket listener) nil))
