(in-package :araneida)

(defun getenv (var)
  (sys:getenv var))

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
    (excl:socket-error
     (format *trace-output* "socket error ~A~%" condition))
    (t
     (format *trace-output* "Caught error: ~A~%" condition)
     #+nil (tpl::backtrace-command) ;this has been causing bus errors
     )))

(defun listener-accept-stream (listener)
  (acl-socket:accept-connection
   (http-listener-socket listener)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-file-error-handlers (form handler-message)
    `(handler-case
      ,form
      (file-error (c)
       (signal 'http-not-found 
        :message (funcall ,handler-message c))))))

(defun host-run-thread (id thunk)
  (mp:process-run-function (prin1-to-string id) thunk))

(defun host-thread-alivep (pid)
  t) ;FIXME!

(defun host-thread-kill (pid)
  (mp:process-kill pid))

(defun host-close-socket (socket)
  (close socket))

(define-condition allegro-timeout (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "A timeout occured.~%"))))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro host-with-timeout (timeout &body body)
    `(let ((interrupt-thread nil))
      (setf interrupt-thread
       (mp:process-run-function "timeout"
        (let ((process mp:*current-process*))
          (lambda ()
            (sleep ,timeout)
            (mp:process-interrupt process
                                  (lambda ()
                                    (signal 'allegro-timeout)))))))
      (unwind-protect
           (progn ,@body)
        (if interrupt-thread
            (mp:process-kill interrupt-thread))))))
  
(defun host-make-listener-socket (address port)
  (acl-socket:make-socket :connect :passive
                          :local-port port
                          :local-host (format nil "~{~A~^.~}"
                                              (coerce
                                               address
                                               'list))
                          :reuse-address t))
