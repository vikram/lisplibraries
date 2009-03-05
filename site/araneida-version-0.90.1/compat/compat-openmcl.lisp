(in-package :araneida)

(defun getenv (var)
  (ccl::getenv var))

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
    (ccl:socket-error
     (format *trace-output* "socket error ~A~%" condition))
    (t
     (format *trace-output* "Caught error: ~A~%" condition)
     (ccl:print-call-history :detailed-p nil))))

(defun listener-accept-stream (listener)
  (ccl:accept-connection
   (http-listener-socket listener)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-file-error-handlers (form handler-message)
    `(handler-case
      ,form
      (ccl::simple-file-error (c)
       (signal 'http-not-found 
        :message (funcall ,handler-message c)))
      (ccl::simple-stream-error (c)
       (signal 'http-not-found 
        :message (funcall ,handler-message c))))))

(defun host-run-thread (id thunk)
  (ccl:process-run-function id thunk))

(defun host-thread-alivep (pid)
  t) ;FIXME!

(defun host-thread-kill (pid)
  (ccl:process-kill pid))

(defun host-close-socket (socket)
  (close socket))

(define-condition openmcl-timeout (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "A timeout occured.~%"))))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro host-with-timeout (timeout &body body)
    `(let ((interrupt-thread nil))
      (setf interrupt-thread
       (ccl:process-run-function 'timeout
        (let ((process ccl:*current-process*))
          (lambda ()
            (sleep ,timeout)
            (ccl:process-interrupt process
                                   (lambda ()
                                     (signal 'openmcl-timeout)))))))
      (unwind-protect
           (progn ,@body)
        (if interrupt-thread
            (ccl:process-kill interrupt-thread))))))
  
(defun host-make-listener-socket (address port)
  (ccl:make-socket :connect :passive
                   :local-port port
                   :local-host (format nil "~{~A~^.~}"
                                       (coerce
                                        address
                                        'list))
                   :reuse-address t))
