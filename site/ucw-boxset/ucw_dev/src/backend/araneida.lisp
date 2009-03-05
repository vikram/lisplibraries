;; -*- lisp -*-

(in-package :it.bese.ucw) 

;;;; ** The araneida backend

(deflogger araneida ()
  :level +debug+
  :appender (make-instance 'brief-stream-log-appender :stream t))

(defclass araneida-backend (backend)
  ((listener :accessor listener :initarg :listener :initform nil)
   (default-url :accessor default-url :initarg :default-url)
   (listener-class :accessor listener-class :initarg :listener-class
		   :initform
		   ;; we prefer araneida-serve-event if that's
		   ;; available, otherwise we fall back to araneida
		   ;; threads
                   #+araneida-threads
		   'araneida:threaded-http-listener
		   #+(and (not araneida-threads) araneida-serve-event)
		   'araneida:serve-event-http-listener
		   #-(or araneida-threads araneida-serve-event)
		   (error "Can't start araneida in threaded mode nor in event loop."))
   (server :accessor server :initarg :server)
   (directories :accessor directories :initarg :directories :initform '()))
  (:documentation "A UCW backend attached to an araneida server."))

(defclass ucw-handler (araneida:handler)
  ((backend :accessor backend :initarg :backend))
  (:documentation "Dispatching handler with multiple match capability."))

(defmethod araneida:handle-request-response ((handler ucw-handler) method request)
  (declare (ignore method))
  (let ((req (make-instance 'araneida-request :request request))
        (res (make-instance 'araneida-response :request request)))
    (or (handle-request (server (backend handler)) req res)
        (block handle
          (dolist* ((url-base . directory-pathname) (directories (backend handler)))
            (when-bind file (map-query-path-to-file (query-path req) url-base directory-pathname)
              (serve-file file req res)
              (return-from araneida:handle-request-response t)))
          nil))))

(defmethod initialize-instance :after ((backend araneida-backend) &key (port 8080) (host "127.0.0.1"))
  (unless (listener backend)
    (setf (listener backend) (make-instance (listener-class backend)
                                            :handler (make-instance 'ucw-handler :backend backend)
                                            :port port)
	  (default-url backend) (araneida:make-url :scheme "http" :host host :port port)
	  (araneida::http-listener-default-hostname (listener backend)) host
	  araneida::*default-url-defaults* (default-url backend))))

(defmethod initialize-backend ((backend araneida-backend) &key server &allow-other-keys)
  (setf (server backend) server)
  backend)

(defmethod startup-backend ((backend araneida-backend) &rest init-args)
  "Start the RERL."
  (declare (ignore init-args))
  (ucw.backend.info "Starting up ARANEIDA backend ~S on ~A"
		    backend
		    (araneida:urlstring (default-url backend)))
  (araneida:start-listening (listener backend))
  #+clisp (araneida:host-serve-events))

(defmethod shutdown-backend ((backend araneida-backend) &rest init-args)
  (declare (ignore init-args))
  (ucw.backend.info "Stopping ARANEIDA backend ~S on ~A."
		    backend
		    (default-url backend))
  (araneida:stop-listening (listener backend))
  backend) 

(defmethod publish-directory ((backend araneida-backend) directory-pathname url-base)
  (ucw.backend.dribble "Publishing local directory ~S at ~S." directory-pathname url-base)
  (push (cons url-base directory-pathname) (directories backend))
  backend)

;;;; request/response

(defclass araneida-request (request)
  ((request :accessor request :initarg :request)))

(defmethod query-path ((request araneida-request))
  (araneida:url-path (araneida:request-url (request request))))

(defun all-params (request)
  (iterate
    (with request = (request request))
    (with params = (list))
    (initially
     (ucw.backend.dribble "Request header: ~S."
                          (araneida:request-header request :content-type)))
    (for (name . value) in (append
                            (parse-query-parameters (araneida:url-query (araneida:request-url request)))
                            (when (araneida:request-unparsed-body request)
                              (parse-query-parameters (araneida:request-unparsed-body request)))))
    (let ((param (assoc name params :test #'string-equal)))
      (if param
	  (setf (cdr param) (cons value (cdr param)))
	  (push (list name value) params)))
    (finally (return params))))

(defun param-value-or-list (values)
  (if (consp values)
      (if (second values)
	  ;; a list with more than 1 element, pas the whole thing
	  values
	  ;; a list with multiple values
	  (car values))
      ;; a single value, just pass it
      values))

(defmethod get-parameter ((request araneida-request) parameter-name)
  (param-value-or-list
   (cdr (assoc parameter-name (all-params request) :test #'string-equal))))

(defmethod map-parameters ((request araneida-request) lambda)
  (dolist* ((name . values) (all-params request))
    (funcall lambda name (param-value-or-list values))))

(defmethod get-header ((request araneida-request) header-name)
  (flet ((grab-header (header-keyword)
           (first (araneida:request-header (request request) header-keyword))))
    (switch (header-name :test #'string=)
      ("Cookie" (grab-header :cookie))
      ("Referer" (grab-header :referer))
      ("Connection" (grab-header :connection))
      ("Keep-Alive" (grab-header :keep-alive))
      ("Accept-Charset" (grab-header :accept-charset))
      ("Accept-Encoding" (grab-header :accept-encoding))
      ("Accept-Language" (grab-header :accept-language))
      ("Accept" (grab-header :accept))
      ("User-Agent" (grab-header :user-agent))
      ("Host" (grab-header :host))
      (t (grab-header (intern (string-upcase header-name) :keyword))))))

(defmethod close-request ((r araneida-request))
  nil)

(defclass araneida-response (response)
  ((request :accessor request :initarg :request)
   (headers :accessor headers :initform '())
   (html-stream :accessor html-stream :initform (make-string-output-stream))))

(defmethod status ((response araneida-response))
  (cdr (assoc "Status" (headers response) :test #'string-equal)))

(defmethod (setf status) (new (response araneida-response))
  (aif (assoc "Status" (headers response) :test #'string-equal)
       (rplacd it new)
       (setf (headers response) (acons "Status" new (headers response)))))

(defmethod clear-response ((response araneida-response))
  (setf (headers response) '()
	(html-stream response) (make-string-output-stream))
  response)

(defmethod (setf get-header) (value (r araneida-response) name)
  (if (assoc name (headers r) :test #'string-equal)
      (setf (cdr (assoc name (headers r) :test #'string-equal)) value)
      (add-header r name value)))

(defmethod add-header ((response araneida-response) header-name value)
  (push (cons header-name value) (headers response)))

(defmethod send-headers ((response araneida-response))
  (araneida-send-headers response nil))

(defun araneida-content-type (response)
  (when-bind content-type (cdr (assoc "Content-Type" (headers response)
                                      :test #'string-equal))
	     (multiple-value-bind (type attributes)
		 (rfc2388:parse-header-value content-type)
	       (values type
		       (cdr (assoc "charset" attributes :test #'string=))))))

(defun araneida-send-headers (response &optional content)
  (let (status content-length expires cache-control location refresh pragma set-cookie
        conditional www-authenticate last-modified extra-headers
        (content-type (or (araneida-content-type response) "text/html")))

    (dolist* ((&whole header-cons name . value) (headers response))
      (switch (name :test #'string-equal)
	("Status"           (setf status           value))
	("Content-Length"   (setf content-length   value))
	("Expires"          (setf expires          value))
	("Cache-Control"    (setf cache-control    value))
	("Location"         (setf location         value))
	("Refresh"          (setf refresh          value))
	("Pragma"           (setf pragma           value))
	("Set-Cookie"       (setf set-cookie       value))
	("Conditional"      (setf conditional      value))
	("WWW-Authenticate" (setf www-authenticate value))
	("Last-Modified"    (setf last-modified    value))
	(t (push header-cons extra-headers))))

    (araneida:request-send-headers (request response)
				   :response-code status
				   :content-type content-type
				   :content-length (or content-length
                                                       (and content (length content)))
				   :expires expires
				   :cache-control cache-control
				   :location location
				   :refresh refresh
				   :pragma pragma
				   :set-cookie set-cookie
				   :conditional conditional
				   :www-authenticate www-authenticate
				   :last-modified last-modified
				   :extra-http-headers extra-headers)))

(defun get-content (content-type content-type/charset r)
  (if (starts-with content-type "text")
      (string-to-octets (get-output-stream-string (html-stream r))
			(switch (content-type/charset :test #'string=)
			  ("utf-8" :utf-8)
			  (("latin-1" "iso-8859-1") :iso-8859-1)
			  (t :us-ascii)))
      ;; um, it's not text. this is really wrong
      (string-to-octets (get-output-stream-string (html-stream r))
			:iso-8859-1)))

(defun make-binary-stream (araneida-request-obj)
  (let ((stream (araneida:request-stream araneida-request-obj)))
   #+clisp (unless (equal (stream-element-type stream) '(unsigned-byte 8))
                    (setf (stream-element-type stream) '(unsigned-byte 8)))
   stream
   ))

(defmethod network-stream ((response araneida-response))
  (make-binary-stream (request response)))

(defmethod network-stream ((request araneida-request))
  (make-binary-stream (request request)))

(defmethod network-stream ((request araneida:request))
  (make-binary-stream request))

(defmethod send-response ((r araneida-response))
  (multiple-value-bind (content-type  content-type/charset)
      (araneida-content-type r)
    (let ((content (get-content content-type content-type/charset r)))
      (araneida-send-headers r content)
      (write-sequence content (network-stream (request r))))))

(defmethod make-backend ((backend araneida:http-listener) &key host port)
  (make-instance 'ucw:araneida-backend
		 :listener backend
		 :default-url (araneida:make-url :scheme "http" :host host :port port)))

;; Copyright (c) 2003-2006 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
