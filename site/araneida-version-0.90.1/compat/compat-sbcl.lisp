(in-package :araneida)

(defparameter *open-external-format-arguments*
  '(:element-type (unsigned-byte 8) :external-format :iso-8859-1))

(eval-when (:load-toplevel :execute)
  (sb-sys:ignore-interrupt SB-UNIX:SIGPIPE))

(defun getenv (var)
  (sb-ext:posix-getenv var))

(defun forcibly-close-stream (s)
  (let ((fd (sb-sys:fd-stream-fd s)))
    (multiple-value-bind (r e) (ignore-errors (close s) t)
      (unless r
	(format t "Unable to close fd ~A: ~A, trying harder ~%" fd e)
	(multiple-value-bind (r e) (ignore-errors (close s :abort t) t)
	  (unless r
	    (format t "still unable to close ~A: ~A, try harder ~%" fd e)
	    (multiple-value-bind (r e)
		(ignore-errors (sb-unix:unix-close fd) t)
	      (unless r
		(format t "Even unix-close failed on ~A:~A, giving up~%"
			fd e)))))))))

(defun platform-handle-debugger-condition (condition)
  (typecase condition
    (sb-bsd-sockets::bad-file-descriptor-error 
     (format *trace-output*
             "accept: bad file descriptor error (socket shut down?)~%~A~%"
             condition))
    (sb-bsd-sockets::socket-error
     (format *trace-output* "socket error ~A~%" condition))
    (sb-ext:timeout
     (format *trace-output* "timed out processing request: ~A~%" condition))
    (t
     (format *trace-output* "Caught error: ~A~%" condition)
     (sb-debug:backtrace 15 *trace-output*))))

(defun listener-accept-stream (listener)
  (let ((socket (sb-bsd-sockets:socket-accept
                 (http-listener-socket listener))))
    (sb-bsd-sockets:socket-make-stream socket :element-type 'character
				       :external-format :iso-8859-1
                                       :name "socket"
                                       :input t :output t :buffering :full)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-file-error-handlers (form handler-message)
    `(handler-case
      ,form
      (sb-int:simple-file-error (c)
       (signal 'http-not-found 
        :message ,handler-message))
      (sb-int:simple-stream-error (c)
       (signal 'http-not-found 
        :message ,handler-message)))))

#+sb-thread
(progn
  (defun host-run-thread (id thunk)
    (let ((error-output *error-output*)
	  (standard-output *standard-output*)
	  (trace-output *trace-output*))
      (sb-thread:make-thread (lambda ()	; rebind the outputs - we need those!
			       (let ((*error-output* error-output)
				     (*standard-output* standard-output)
				     (*trace-output* trace-output))
				 (funcall thunk)))
			     :name (format nil "Araneida Server Thread ~A" id))))
  
  (defun host-thread-alivep (pid)
    (sb-thread:thread-alive-p pid))
  
  (defun host-thread-kill (pid)
    (sb-thread:terminate-thread pid)))

(defun host-close-socket (socket)
  (sb-bsd-sockets:socket-close socket))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro host-with-timeout (timeout &body body)
    `(sb-ext:with-timeout ,timeout
      ,@body)))

(defun host-make-listener-socket (address port)
  (let ((socket
	 (make-instance 'sb-bsd-sockets:inet-socket :type :stream
                        :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket address port)
    (sb-bsd-sockets:socket-listen socket 15)
    socket))

(defvar *araneida-fd-handlers* nil
  "Our file descriptor handlers for SERVE-EVENT")

(defun host-add-fd-handler (listener handler)
  (let ((socket (http-listener-socket listener)))
    (let ((handler-obj (sb-sys:add-fd-handler
                        (sb-bsd-sockets:socket-file-descriptor socket)
                        :input
                        handler)))
      (push handler-obj *araneida-fd-handlers*)
      handler-obj)))

(defun host-remove-fd-handler (handler)
  (setq *araneida-fd-handlers*
        (delete handler *araneida-fd-handlers* :test #'eq))
  (sb-sys:remove-fd-handler handler))

(defmacro host-without-other-handlers (&body body)
  "Run BODY without araneida's handlers therefore ensuring only one
   request is handled at a time. Other handlers are present and may
   be removed/added within BODY."
  (let ((h (gensym)))
  `(let ((,h nil))
    (unwind-protect
         (let ((sb-impl::*descriptor-handlers*
                (remove-if #'(lambda (x)
                               (member x *araneida-fd-handlers*
                                       :test #'eq))
                           sb-impl::*descriptor-handlers*)))
           (unwind-protect
                (progn
                  ,@body)
             (setq ,h sb-impl::*descriptor-handlers*)))
      (setq sb-impl::*descriptor-handlers*
            (append ,h *araneida-fd-handlers*))))))