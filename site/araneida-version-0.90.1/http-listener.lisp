(in-package :araneida)

(defgeneric start-listening (listener &key &allow-other-keys))
(defgeneric stop-listening (listener &key abort &allow-other-keys))
(defgeneric listening-p (listener))
(defmethod listening-p ((listener http-listener))
  (and (http-listener-socket listener) t))

;; Be honest, this was a bad name anyway.  We don't call break, we merely 
;; omit to handle the condition
(defvar *break-on-handler-errors* nil 
  "deprecated; see *restart-on-handler-errors* instead")

(defvar *restart-on-handler-errors* t
  "Controls the disposition of errors signalled during handler methods.  If T, a backtrace will be printed to *TRACE-OUTPUT* and the ABORT-RESPONSE restart will be invoked to continue with the next request.  It may also be a designator for a function: if so it will be called with the consition signalled and should handle it, or return T or NIL which will be handled as above")


(defmacro with-accept-flets (&body body)
  `(labels ((do-it (listener s)
	      (let ((r (read-request-from-stream listener s)))
		(handler-case
		    (handle-request-using-listener
		     listener (http-listener-handler listener) r)
		  (response-sent () nil)
		  (http-error (c) 
		    (request-send-error r (http-error-code c) 
					:log-message (http-error-message c)
					:client-message (http-error-client-message c))))))
	    (accept (listener)
             (listener-accept-stream listener)))
     (with-simple-restart
	 (abort-response "Abort this response and answer another request")
       ;; expectation is that socket-accept will not block, because we 
       ;; are invoked when select() says something is ready.  we really
       ;; ought to set the master socket non-blocking to be sure.
       (let ((*debugger-hook* #'handler-debugger-hook))
	 ,@body))))

(defgeneric handle-request-using-listener (http-listener handler request))

(defmethod handle-request-using-listener ((l http-listener) handler request)
  (handle-request (http-listener-handler l) request))

(defun function-designator-p (n)
  ;; there really ought to be a better way to do this.
  (cond ((functionp n) t)
	((member n '(t nil)) nil)
	((keywordp n) nil)
	((symbolp n) t)
	((and (consp n) (eql (car n) 'setf) (symbolp (cadr n))) t)
	(t nil)))

(defun handler-debugger-hook (condition old-hook)
  (declare (ignore old-hook))
  (when 
      (or *break-on-handler-errors*
	  (if (function-designator-p *restart-on-handler-errors*)
              (funcall *restart-on-handler-errors* condition)
	      *restart-on-handler-errors*))
    (platform-handle-debugger-condition condition)
    (invoke-restart 'abort-response)))






