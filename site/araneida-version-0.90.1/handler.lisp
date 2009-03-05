(in-package :araneida)

(defclass handler () ())
(defclass legacy-handler (handler) ())
(defclass dispatching-handler (handler)
  ((child-handlers :initform nil :accessor child-handlers)))

(define-condition response-sent (condition) ())

;;; overall request handler
(defgeneric handle-request (handler request))

;;; default method for which calls the following "sub-handlers"

;;; who does the user say he is?   Is he correct?
(defgeneric handle-request-authentication (handler method request))

;;; is the user allowed to see this resource?
(defgeneric handle-request-authorization (handler method request))
;;;  (default method calls request-authorized-p, request-not-authorized)
(defgeneric request-authorized-p (handler method request))
(defgeneric request-not-authorized (handler method request))

;;; send the resource back, or do whever else is appropriate at this
;;; stage for the requested method.  If -response returns (values NIL
;;; foo), we send a 404 and log the foo to a log stream, or to the
;;; browser if no log stream is set up
(defgeneric handle-request-response (handler method request))
;;; can be used to write stuff to a log file, or some other cleanup action
;;; that may take place after the response to the client has gone (i.e.
;;; request stream may be closed by now
(defgeneric handle-request-logging (handler method request))

(defmethod handle-request ((handler t) request)
  (etypecase handler
    (cons (apply (car handler) request (cdr handler)))
    (function (funcall handler request))
    (symbol (funcall handler request))))

(defmethod handle-request ((handler handler) request)
  (let ((method (request-method request))
	(handled-p nil))
    (handle-request-authentication handler method request)
    (handle-request-authorization handler method request)
    (multiple-value-bind (handled whynot)
	(handle-request-response handler method request)
      (setf handled-p handled)
      (or handled
	  (signal 'http-not-found :message whynot))
      (handle-request-logging handler method request))
    handled-p))

(defmethod handle-request-authentication ((handler handler) method request ) t)
(defmethod handle-request-logging ((handler handler) method request ) t)

(defmethod request-authorized-p ((handler handler) method request ) t)

(defvar *log-stream* *trace-output*)

;;; we can't send a 40x here, because we don't know if the problem was failed
;;; http auth, missing cookie, blocked ip address, etc.  So, realistically,
;;; "it is an error" to override request-authorized-p without also providing
;;; the corresponding request-not-authorized
(defmethod request-not-authorized ((handler handler) method request )
  (request-send-error request 500))

(defmethod handle-request-authorization ((handler handler) method request)
  (cond ((request-authorized-p handler method request) t)
	(t (request-not-authorized handler method request)
	   (signal 'response-sent))))
      

  

;;; dispatching handlers contain other handlers which they (as the
;;; name suggests, really) dispatch requests to again

(defmethod handle-request-response
    ((handler dispatching-handler) method request)
  (labels ((match-p (url prefix)
	     (and (>= (length url) (length prefix))
		  (string= prefix url :end2 (length prefix)))))
    (let* ((handled-by (request-handled-by request))
	   (offset (or (second (first handled-by)) 0))
	   (urlstring (request-urlstring request))
	   (rest-of-url (request-unhandled-part request))
	   (handlers (assoc rest-of-url (child-handlers handler)
			    :test #'match-p))
	   (handler 
	    (if (and (caddr handlers)
		     (eql (length rest-of-url) (length (car handlers))))
		(caddr handlers)
		(cadr handlers))))
      (when handler
	(let ((new-offset (+ offset (length (car handlers)))))
	  ;(format t "~A handlers ~A handler ~A~%" urlstring handlers handler)
	  (push (list handler new-offset) (request-handled-by request))
	  (setf (request-base-url request)
		(parse-urlstring (subseq urlstring 0 new-offset))))
	(handle-request handler request)))))

;;; XXX nasty large amount of ought-to-be-refactorable cut&paste code follows

(defgeneric install-handler (parent child discriminator exact-p)
  (:documentation   "Install CHILD as a sub-handler of PARENT.  DISCRIMINATOR should be a portion of request urlstring that the sub-handler should be selected for.  EXACT-P controls whether CHILD will be selected only for requests that exactly match DISCRIMINATOR or for all requests prefixed with DISCRIMINATOR.  Bugs: this docstring is completely incomprehensible"))

(defmethod install-handler ((parent dispatching-handler) child
			    discriminator exact-p)
  (let* ((discriminator (if (typep discriminator 'url)
			    (urlstring discriminator)
			    discriminator))
	 (existing (assoc discriminator (child-handlers parent)
			  :test #'string=)))
    (if existing
	(if exact-p
	    (setf (third existing) child)
	    (setf (second existing) child))
	(setf (child-handlers parent)
	      (merge 'list
		     (child-handlers parent)
		     (list
		      (list discriminator
			    (if exact-p nil child)
			    (if exact-p child nil)))
		     #'string> :key #'car))))
  (child-handlers parent))

(defgeneric find-handler (parent discriminator exact-p)
  (:documentation "Find the handler for DISCRIMINATOR, EXACT-P from the list of sub-handlers for PARENT"))

(defmethod find-handler ((parent dispatching-handler) 
			      discriminator exact-p)
  (let ((existing (assoc discriminator (child-handlers parent)
			 :test #'string=)))
    (when existing
      (if exact-p (third existing) (second existing) ))))


(defgeneric uninstall-handler (parent discriminator exact-p)
  (:documentation "Remove the handler for DISCRIMINATOR, EXACT-P from the list of sub-handlers for PARENT"))

(defmethod uninstall-handler ((parent dispatching-handler) 
			      discriminator exact-p)
  (let ((existing (assoc discriminator (child-handlers parent)
			 :test #'string=)))
    (when existing
      (if exact-p
	  (setf (third existing) nil)
	  (setf (second existing) nil))
      (when (and (null (second existing)) (null (third existing)))
	(setf (child-handlers parent)
	      (remove existing (child-handlers parent)))))
    (child-handlers parent)))
  


;;; legacy handler

(defmethod handle-request-response ((handler legacy-handler) method request)
  (catch 'done
    (handler-bind ((stream-error
		    (lambda (c)
		      (format t "peer probably hung up:~A ~%" c)
		      (throw 'done nil)))
		   (error
		    (lambda (c)
		      (block nil
			(format t "got error! url=~A c=~A~%"
				(request-url request) c)
			(setf (slot-value request 'condition) c)
			(dispatch-request request :error)
			(throw 'done nil)))))
      (progn
	(dispatch-request request :authentication)
	(unless (open-stream-p (request-stream request)) (throw 'done nil))
	(dispatch-request request :authorization)
	(unless (open-stream-p (request-stream request))  (throw 'done nil))
	(dispatch-request request :response))))
  (let* ((s (request-stream request)))
    (forcibly-close-stream s))
  (dispatch-request request :log)
  t)
