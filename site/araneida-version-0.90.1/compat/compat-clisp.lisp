(in-package :araneida)

(defun getenv (var)
  (ext:getenv var))

(defun forcibly-close-stream (s)
  (socket:socket-status s)
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
     ;; no backtrace under clisp
     )))

(defun listener-accept-stream (listener)
  (socket:socket-accept (http-listener-socket listener)
                        :buffered nil
                        :element-type 'character
                        :external-format (ext:make-encoding
                                          :charset 'charset:iso-8859-1
                                          :line-terminator :unix)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-file-error-handlers (form handler-message)
    `(handler-case
      ,form
      (system::simple-file-error (c)
       (signal 'http-not-found 
        :message ,handler-message))
      (system::simple-stream-error (c)
       (signal 'http-not-found 
        :message ,handler-message)))))

(defun host-close-socket (socket)
  (socket:socket-server-close socket))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro host-with-timeout (timeout &body body)
    `(progn ,@body))) ;;; FIXME!

(defun host-make-listener-socket (address port)
  (declare (ignore address))
  (socket:socket-server port))

(defvar *fd-handlers* nil)

(defun host-add-fd-handler (listener handler)
  (car (push (cons
              (http-listener-socket listener)
              handler) *fd-handlers*)))

(defun host-remove-fd-handler (handler)
  (setf *fd-handlers* (remove handler *fd-handlers*)))

(defmacro host-without-other-handlers (&body body)
  `(progn ,@body))

(defun host-serve-events ()
  (let ((just-sockets (mapcar #'car *fd-handlers*)))
    (loop
     (progn
       (loop for i in *fd-handlers*
             for j in (socket:socket-status just-sockets)
             do (if j
                    (funcall (cdr i) (car i))))
       ))))