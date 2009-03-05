(in-package :araneida)

(defun getenv (var)
  "") ; FIXME

(defun forcibly-close-stream (s)
  (multiple-value-bind (r e) (ignore-errors (close s) t)
    (unless r
      (format t "Unable to close: ~A, trying harder ~%" e)
      (multiple-value-bind (r e) (ignore-errors (close s :abort t) t)
        (unless r
          (format t "Even close :abort t failed:~A, giving up~%"
                  e))))))

(defun platform-handle-debugger-condition (condition)
  (typecase condition
    (t
     (format *trace-output* "Caught error: ~A~%" condition)
     (sys::backtrace)
     )))

(defun listener-accept-stream (listener)
  (ext:get-socket-stream
   (ext:socket-accept
    (http-listener-socket listener))))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-file-error-handlers (form handler-message)
    `(handler-case
      ,form
      (file-error (c)
       (signal 'http-not-found 
        :message (funcall ,handler-message c))))))

(defun host-run-thread (id thunk)
  (declare (ignore id))
  (ext:make-thread thunk))

(defun host-thread-alivep (pid)
  (ext:thread-alive-p pid))

(defun host-thread-kill (pid)
  (ext:destroy-thread pid))

(defun host-close-socket (socket)
  (ext:server-socket-close socket))

(define-condition abcl-timeout (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "A timeout occured.~%"))))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro host-with-timeout (timeout &body body)
    `(let ((interrupt-thread nil))
      (setf interrupt-thread
       (ext:make-thread
        (let ((process (ext:current-thread)))
          (lambda ()
            (sleep ,timeout)
            (ext:interrupt-thread process
                                  (lambda ()
                                    (signal 'abcl-timeout)))))))
      (unwind-protect
           (progn ,@body)
        (if interrupt-thread
            (ext:destroy-thread interrupt-thread))))))
  
(defun host-make-listener-socket (address port)
  (declare (ignore address))
  (ext:make-server-socket port))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro slot-definition-name (&rest args)
    `(sys::slot-definition-name ,@args))
  (defmacro class-slots (&rest args)
    `(sys::class-slots ,@args)))
