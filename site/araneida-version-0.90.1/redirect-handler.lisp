(in-package :araneida)

;;; when moving resources around, it's important not to break old URLs

(defclass redirect-handler (handler)
  ((location :initarg :location :accessor redirect-location)))

(defmethod handle-request-response ((handler redirect-handler) method request)
  (let* ((loc (redirect-location handler))
	 (base (typecase loc
		 (url loc) 
		 (string (merge-url (request-base-url request) loc)))))
    (request-redirect request 
		      (merge-url base (request-unhandled-part request)))))

