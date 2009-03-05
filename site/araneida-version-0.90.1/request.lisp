(in-package :araneida)


(defgeneric request-path-info (request)
  (:documentation "Returns portion of the requested URL after the base-url"))
(defmethod request-path-info ((r request))
  (let ((path (url-path (request-url r)))
        (ppath (url-path (request-base-url r))))
    (subseq path (length ppath) nil)))

(defgeneric request-unhandled-part (request)
  (:documentation "Returns portion of request unhandled (??)"))
(defmethod request-unhandled-part ((request request))
  (let* ((handled-by (request-handled-by request))
	 (offset (or (second (first handled-by)) 0))
	 (urlstring (request-urlstring request)))
    (subseq urlstring offset)))

(defgeneric request-header (request name)
  (:documentation "Returns a list containing the values of all header lines in REQUEST given by the keyword NAME"))

(defmethod request-header ((r request) name)
  (cdr (assoc name (request-headers r) :test #'string=)))

(defgeneric request-if-modified-since (request &optional default)
  (:documentation "Retrieve and parse the date in the If-Modified-Since header field.  Return DEFAULT if the header is absent or unparseable"))
(defmethod request-if-modified-since ((request request)
                                      &optional (default nil))
  (let ((if-mod-since (car (request-header request :if-modified-since))))
    (if if-mod-since
             (or (date:parse-time (car (split if-mod-since 2 '(#\;))))
                 default)
      default)))

(define-condition cookie-not-found (serious-condition)
  ((cookie-name :initarg :cookie-name
		:reader cookie-not-found-cookie-name))
  (:report (lambda (condition stream)
	     (format stream "Cookie ~A was not found"
		     (cookie-not-found-cookie-name condition)))))

(defgeneric request-cookie (request cookie-name &key on-fail)
  (:documentation "Returns the value of the cookie named COOKIE-NAME
If the cookie is not found, the default is to return nil. :on-fail can also
be set to :signal-condition, whereupon it will signal a cookie-not-found condition.

Valid values for on-fail:
  :return-nil
  :signal-condition"))
(defmethod request-cookie ((request request) name &key (on-fail :return-nil))
  (let ((cookie (find-if (lambda (c) (string-equal (rfc2109:cookie-name c) name))
			  (request-cookies request))))
    (if cookie
	(rfc2109:cookie-value cookie)
	(ecase on-fail
	  (:return-nil nil)
	  (:signal-condition (error 'cookie-not-found :cookie-name name))))))

(defgeneric request-safe-cookie (request cookie-name domain-restrict &key on-fail)
  (:documentation "Returns the value of the cookie named COOKIE-NAME, guarding against spoofing attacks.
The cookie will only be used if its domain matches domain-restrict (a string), or if its domain is blank.
If the cookie is not found, the default is to return nil. :on-fail can also
be set to :signal-condition, whereupon it will signal a cookie-not-found condition.

Valid values for on-fail:
  :return-nil
  :signal-condition"))
(defmethod request-safe-cookie ((request request) name domain-restrict &key (on-fail :return-nil))
  (let ((cookie (find-if (lambda (c) (string-equal (rfc2109:cookie-name c) name))
			  (request-safe-cookies request domain-restrict))))
    (if cookie
	(rfc2109:cookie-value cookie)
	(ecase on-fail
	  (:return-nil nil)
	  (:signal-condition (error 'cookie-not-found :cookie-name name))))))
	
  
(defgeneric request-cookies (request)
  (:documentation "Returns cookie-structs for all cookies returned (see rfc2109 package for details)
This is rarely used directly. REQUEST-COOKIE is the better choice for most uses."))
(defmethod request-cookies ((request request))
  (loop for cookie-string in (request-header request :cookie)
	appending (rfc2109:parse-cookies cookie-string)))

(defgeneric request-safe-cookies (request domain-restriction)
  (:documentation "Returns cookie-structs for all cookies returned, avoiding spoofing attacks
domain-restriction is a string such as 'my.test.domain' which should match your website's domain
See the RFC2109 package for details
This is rarely used directly. REQUEST-SAFE-COOKIE is the better choice for most uses."))
(defmethod request-safe-cookies ((request request) domain-restriction)
  (loop for cookie-string in (request-header request :cookie)
	appending (rfc2109:safe-parse-cookies cookie-string domain-restriction)))

(defun cookie-string (name value &key comment domain max-age path secure)
  "Returns a cookie string suitable for setting
See documentation for RFC2109:COOKIE-STRING for details."
  (rfc2109:cookie-string name value :comment comment :domain domain :max-age max-age :path path :secure secure))


;; this takes an alist not a request, hence the name
(defun body-param (name alist)
  "Look in the request body ALIST for the value of the parameter NAME"
  (cadr (assoc name alist :test #'string=)))

(defun body-params (name alist &key (case-sensitive nil))
  "Look in the request body ALIST for the values of the parameters starting NAME, returning a list of KEY VALUE pairs"
  ;; find all parameters starting NAME
  (let ((equal (if case-sensitive #'string= #'string-equal))
        (len (length name)))
    (flet ((starts-with-name (string)
             (and (>= (length string) len)
                  (funcall equal string name :end1 len))))
      (remove-if-not #'starts-with-name alist :key #'car))))

(defgeneric dispatch-request (request handlers &optional discriminator)
  (:documentation "Find the best match for REQUEST in the list HANDLERS"))
(defmethod dispatch-request ((request request) handlers &optional discriminator)
  (unless discriminator (setf discriminator (request-url request)))
  (destructuring-bind
        (method match prefix func &optional needs-discriminator)
      (find-export (urlstring discriminator) handlers (request-method request))
    (declare (ignore match))
    (unless method (return-from dispatch-request nil))
    (setf (request-base-url request) (parse-urlstring prefix))
    (let ((rest-of-url
           (subseq (urlstring discriminator)
                   (length (urlstring (request-base-url request)))
                   nil)))
      (cond ((and needs-discriminator (consp func))
             (apply (car func) request handlers discriminator
                    rest-of-url (cdr func)))
            ((consp func)
             (apply (car func) request rest-of-url (cdr func)))
            (needs-discriminator
             (funcall func request handlers discriminator rest-of-url))
            (t
             (funcall func request rest-of-url))))))

;;; rfc 1945 p26
(defvar *http-error-codes*
  '((400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")))

(defgeneric request-send-headers (request &key
                                 content-type
                                 content-length
                                 expires
				 cache-control
				 location
				 refresh
				 pragma
				 set-cookie
				 conditional
				 www-authenticate
                                 extra-http-headers
                                 last-modified
                                 response-text
                                 response-code)
  (:documentation "Send HTTP/1.0 headers in response to REQUEST.  If the request HTTP version
is less than 1.0, do nothing.  If CONDITIONAL is true, may signal RESPONSE-SENT
instead of returning normally."))
(defmethod request-send-headers ((request request) &key
                                 (content-type "text/html")
                                 content-length
                                 expires
				 cache-control
				 location
				 refresh
				 pragma
				 set-cookie
				 conditional
				 www-authenticate
                                 extra-http-headers
                                 (last-modified (get-universal-time))
                                 (response-text "OK")
                                 (response-code 200))
  (when (< (request-http-version request) 1.0)
    (return-from request-send-headers response-code))
  (let ((stream (request-stream request))
        (cr (code-char 13))
        (lf (code-char 10)))
    (labels ((perhaps (if name &optional then)
	       (if if (princ (s. name ": " (or then if)  (format nil "~C~C" cr lf))
			     stream)))
	     (date (d) 
	       (if (numberp d) (date:universal-time-to-http-date d) d)))
      (when (and conditional
		 (<= last-modified
		     (request-if-modified-since request 0)))
	(setf response-code 304 response-text "Not modified"))
      (when (eql response-code 304)
	;;  "the response {SHOULD,MUST} NOT include other
	;;  entity-headers; this prevents inconsistencies between
	;;  cached entity-bodies and updated headers.
	(setf content-length nil content-type nil))
      (format stream "HTTP/1.0 ~D ~A~C~C~
Date: ~A~C~C~
Server: ~A~C~C~
Connection: close~C~C"
	      response-code response-text cr lf
	      (date:universal-time-to-http-date (get-universal-time)) cr lf
	      *araneida-product-tokens* cr lf cr lf)
      (perhaps content-type "Content-Type")
      (perhaps last-modified "Last-Modified" (date last-modified))
      (perhaps content-length "Content-Length")
      (if set-cookie
	  (let ((cookies (if (listp set-cookie) set-cookie (list set-cookie))))
	    (dolist (cookie cookies) (perhaps cookie "Set-Cookie"))))
      (perhaps cache-control "Cache-Control" )
      (perhaps refresh "Refresh" )
      (perhaps location "Location" )
      (perhaps pragma "Pragma" )
      (perhaps expires "Expires"  (date expires))
      (perhaps www-authenticate "WWW-Authenticate")
      (mapc #'(lambda (header)
                (format stream "~A: ~A~C~C" (car header) (cdr header) cr lf))
            extra-http-headers)
      (format stream "~C~C" cr lf)
      (when (eql response-code 304)
	;; "The 304 response MUST NOT contain a message-body" (rfc2616)
	(signal 'response-sent)))
    response-code))

(defgeneric request-send-error (request error-code &key log-message client-message)
  (:documentation "Send the client HTTP headers and HTML body for an error message
with numeric code ERROR-CODE. LOG-MESSAGE is sent to *log-stream*, while CLIENT-MESSAGE
is sent on to the user - replacing the default text. CLIENT-MESSAGE is passed to format,
so it should not be an HTML list"))
(defmethod request-send-error ((request request) error-code &key log-message client-message)
  (let ((stream (request-stream request))
        (error-text (cdr (assoc error-code *http-error-codes*))))
    (when *log-stream*
      (format *log-stream* "~&Logged error: ~A ~A ~@[~A~] while processing URL <~A>~%" 
	      error-code error-text log-message (urlstring (request-url request))))
    (request-send-headers request
                          :response-code error-code :response-text error-text)
    (html-stream stream
		 `(html (head (title ,(s. error-code) " " ,error-text))
		   (body
		    (h2 ,error-text)
		    (p "Was asked for URL "
		     (tt ,(urlstring (request-url request)))
		     ", but it didn't happen for us.  Sorry")
		    ,@(when client-message
			    `((H3 "Additional information: ")
			      (pre ,(html-escape (format nil "~a" client-message))))))))
  (signal 'response-sent)))

(defgeneric request-redirect (request new-url &rest headers)
  (:documentation "Redirects request to NEW-URL, appending HEADERS to the redirect"))
(defmethod request-redirect ((request request) new-url &rest headers)  
  (let ((urlstring (urlstring
                    (if (typep new-url 'url)
			new-url
			(merge-url (request-url request) new-url)))))
    (apply #'request-send-headers
	   request
	   :location urlstring
	   :expires "Fri, 30 Oct 1998 14:19:41 GMT"
	   :pragma "no-cache"
	   :response-code 302 :response-text "Redirected"
	   headers)
    (format (request-stream request)
	    "~%<h1>Redirected</h1><p>Continue <a href=\"~A\">~A</a>"
	    urlstring urlstring)
    (signal 'response-sent)))

(defun copy-request (from)
  (let ((to (make-instance 'request)))
    (labels ((set-slot (name)
	       (if (slot-boundp from name)
		   (setf (slot-value to name)  (slot-value from name))
		   (slot-makunbound to name))))
      (dolist (i '(url urlstring http-version handled-by user 
		   method stream headers body unparsed-body condition))
	(set-slot i)))
    to))



