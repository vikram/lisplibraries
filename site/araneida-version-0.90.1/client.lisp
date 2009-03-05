(in-package :araneida)

;;; a presently fairly basic HTTP client

(defvar *http-proxy*
  (aif (getenv "http_proxy")
       (parse-urlstring it)))

(defun coerce-inet-address-designator (host)
  "An inet address is a (vector (unsigned-byte 8) 4).  An inet address designator a string to be resolved by the system hostname resolver, or a string comprising a dotted quad.  This function may signal errors from hostname resolution"
  (cond ((typep host '(vector (unsigned-byte 8) 4)) host)
	((some #'alpha-char-p host)
	 (host-ent-address (get-host-by-name host)))
	(t (make-inet-address host))))

(defun stream-to-url (url &key
		      (proxy *http-proxy*)
		      proxy-host proxy-port
		      (buffering :full))
  "Open a stream to the appropriate server for a request of URL.
Default value for the host is taken from the request's URL, but may be
overridden by the PROXY argument, which may itself be overridden by the
PROXY-HOST and PROXY-PORT arguments."
  (when proxy 
    (unless (typep proxy 'url) (setf proxy (parse-urlstring proxy))))
  (let ((peer-host (coerce-inet-address-designator
		    (or proxy-host
			(and proxy (url-host proxy))
			(url-host url))))
	(peer-port (or proxy-port
		       (and proxy (url-port proxy))
		       (url-port url))))
    ;; we can only do proxying via http
    (assert (or (not proxy) (typep proxy 'http-url)))
    ;; open client socket to peer-host, peer-port, and stuff
    (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
      (socket-connect s peer-host peer-port)
      (socket-make-stream s :input t :output t :buffering buffering))))

(defmacro with-url-stream ((stream url
				   &rest args
				   &key
				   (proxy *http-proxy*)
				   proxy-host proxy-port
				   (buffering :full))
			   &body body)
  `(let (,stream)
     (unwind-protect
	  (progn
	    (setf ,stream (stream-to-url ,url ,@args))
	    ,@body)
       (when ,stream (close ,stream)))))

(defun send-request (request &key
		     (proxy *http-proxy*)
		     proxy-host proxy-port)
  "Sends REQUEST to an HTTP server.  Default value for the host is
taken from the request URL, but may be overridden by the PROXY
argument, which may itself be overridden by the PROXY-HOST and
PROXY-PORT arguments.  Returns a stream from which the response
headers and body may be read"
  (let* ((url (request-url request))
         (out (stream-to-url url :proxy proxy :proxy-host proxy-host
		      	     :proxy-port proxy-port)))
    (setf (request-stream request) out)
    (format out "GET ~A HTTP/1.0~C~%" (urlstring url) (code-char 13))
    (format out "Host: ~A~C~%" (url-endpoint url) (code-char 13))
    
    (format out "~C~%" (code-char 13))
    ;; XXX should output headers
    ;; XXX should output cookies if they've been set
    ;; XXX should output request-body if set
    (force-output out)
    request))


	      



