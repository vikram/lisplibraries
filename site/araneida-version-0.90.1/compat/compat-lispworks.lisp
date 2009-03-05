(in-package :araneida)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defun getenv (var)
  (hcl:getenv var))

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
    (comm::socket-error
     (format *trace-output* "socket error ~A~%" condition))
    (t
     (format *trace-output* "Caught error: ~A~%" condition)
     #+nil (tpl::backtrace-command) ;this has been causing bus errors
     )))

(defun socket-input-available (socket)
  (comm::socket-listen socket))

(defun accept-connection (passive-socket
			      &key (wait t))
  (if (or wait (socket-input-available passive-socket))
      (make-instance 'comm:socket-stream
                     :socket (comm::get-fd-from-socket passive-socket)
                     :direction :io
		     ; lispworks can write UTF-8 output no problem if you use unsigned-byte
		     ; thanks to Bob Hutchinson (hutch at recursive.ca)
		     ; -- Alan Shields [14 November 2005]
                     :element-type '(unsigned-byte 8))))

(defun listener-accept-stream (listener)
  (accept-connection
   (http-listener-socket listener)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-file-error-handlers (form handler-message)
    `(handler-case
      ,form
      (file-error (c)
       (signal 'http-not-found 
        :message (funcall ,handler-message c))))))

(defun make-process (&key (name "Anonymous") reset-action run-reasons arrest-reasons (priority 0) quantum
                          resume-hook suspend-hook initial-bindings run-immediately)
  (declare (ignore priority quantum reset-action resume-hook suspend-hook run-immediately))
  (let ((mp:*process-initial-bindings* initial-bindings))
    (mp:create-process name :run-reasons run-reasons :arrest-reasons arrest-reasons)))

(defun process-run-function (name-or-options preset-function &rest preset-arguments)
  (let ((process (ctypecase name-or-options
                   (string (make-process :name name-or-options))
                   (list (apply #'make-process name-or-options)))))
    (apply #'mp::process-preset process preset-function preset-arguments)
    (push :enable (mp:process-run-reasons process))
    process))

(defun host-run-thread (id thunk)
  (process-run-function (prin1-to-string id) thunk))

(defun host-thread-alivep (pid)
  t) ;FIXME!

(defun host-thread-kill (pid)
  (mp:process-kill pid))

(defun host-close-socket (socket)
  (comm::close-socket socket))

(define-condition lispworks-timeout (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "A timeout occured.~%"))))

;revised version from alain picard
(defun invoke-with-timeout (timeout bodyfn timeoutfn)
  (block timeout
    (let* ((process mp:*current-process*)
           (unscheduled-p nil)
           (timer (mp:make-timer
                   #'(lambda ()
                       (mp:process-interrupt process
                                             #'(lambda ()
                                                 (unless unscheduled-p
                                                   (return-from timeout
                                                     (funcall timeoutfn)))))))))
      (mp:schedule-timer-relative timer timeout)
      (unwind-protect (funcall bodyfn)
        (lw:without-interrupts
         (mp:unschedule-timer timer)
         (setf unscheduled-p t))))))


(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate
and evaluate TIMEOUT-FORMS."
  `(invoke-with-timeout ,seconds #'(lambda () ,@body)
                        #'(lambda () ,@timeout-forms)))


(eval-when (:compile-toplevel :load-toplevel)
  (defmacro host-with-timeout (timeout &body body)
    `(with-timeout (,timeout) ,@body)))
  
(defun host-make-listener-socket (address port)
  (let ((comm::*use_so_reuseaddr* t))
    (multiple-value-bind (socket error-location error-code)
        (comm::create-tcp-socket-for-service port 
                                             :address (format nil "~{~A~^.~}"
                                                              (coerce
                                                               address
                                                               'list)))
      (cond (socket socket)
            (t (error "Could not create listener socket: ~A ~A" error-location error-code))))))
