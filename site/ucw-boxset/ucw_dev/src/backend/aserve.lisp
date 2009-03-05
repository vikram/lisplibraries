;; -*- lisp -*-

;;;; ** aserve/portableaserve backend for UCW

(in-package :it.bese.ucw)

(defclass aserve-backend (backend) 
  ((wserver :accessor wserver :initarg :wserver :initform (make-instance 'net.aserve:wserver))
   (ucwserver :accessor ucwserver :initarg :ucwserver)
   (host :accessor host :initarg :host :initform "127.0.0.1")
   (port :accessor port :initarg :port :initform 3001)
   (num-listeners :accessor num-listeners
		  :initarg :num-listeners
		  :initform  #+(or (and sbcl sb-thread)
				   (not sbcl)) 20
                             #+(and sbcl (not sb-thread)) 0)))

(defmethod initialize-backend ((s aserve-backend) &key server &allow-other-keys)
  (declare (ignore host port init-args))
  (net.aserve::debug-on :notrap)
  (with-slots (wserver ucwserver) s
    (when (notany #'(lambda (locator)
		      (eq (class-name (class-of locator))
			  'net.aserve::locator-pattern))
		  (net.aserve::wserver-locators wserver))
      (setf (net.aserve::wserver-locators wserver)
	    (append (net.aserve::wserver-locators wserver)
		    (list (make-instance 'net.aserve::locator-pattern :name :pattern)))))
    (setf ucwserver server)))

(defmethod register-url-handler ((backend aserve-backend) (entry-point standard-entry-point)
                                 (handler function))
  (with-slots (wserver) backend
    (net.aserve:publish 
     :path (strcat (application.url-prefix
		    (entry-point.application entry-point))
		   (entry-point.url entry-point))
     :function (lambda (request entity)
		 (funcall handler
			  (make-aserve-request request entity)
			  (make-aserve-response request entity)))
     :server wserver)))

(defmethod unregister-url-handler ((backend aserve-backend) (entry-point standard-entry-point))
  (with-slots (wserver) backend
    (net.aserve:publish
     :path (strcat (application.url-prefix
		    (entry-point.application entry-point))
		   (entry-point.url entry-point))
     :remove t
     :server wserver)))

(defmethod publish-directory ((backend aserve-backend) directory-pathname url-base)
  (with-slots (wserver) backend
    (net.aserve:publish-directory :prefix url-base
				  :destination (namestring directory-pathname)
				  :server wserver)))


(defmethod startup-backend ((backend aserve-backend) &rest init-args)
  (declare (ignore init-args))
  (with-slots (wserver) backend
    (net.aserve:start :port (port backend)
		      :host (host backend)
		      :listeners (num-listeners backend) 
		      :server wserver)))

(defmethod shutdown-backend ((backend aserve-backend) &rest init-args)
  (declare (ignore init-args))
  (with-slots (wserver) backend
    (net.aserve:shutdown :server wserver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; request

(defclass aserve-request (net.aserve:http-request request) 
  ((request :accessor request :initarg :request)))

(defun make-aserve-request (request entity)
  (declare (ignore entity))
  (make-instance 'aserve-request :request request))

(defmethod get-header ((request aserve-request) header-name)
  (switch (header-name :test #'string-equal)
    ("Date"             (net.aserve:reply-header-slot-value (request request) :date))
    ("Content-Type"     (net.aserve:reply-header-slot-value (request request) :content-type))
    ("Content-Length"   (net.aserve:reply-header-slot-value (request request) :content-length))
    (t (net.aserve:reply-header-slot-value
        (request request)
        (read-from-string (format nil ":~A" header-name))))))

(defmethod shutdown ((request aserve-request)))

#+#.(cl:if (cl:find-package "PURI") `(and) `(or))
(defmethod query-path ((request aserve-request))
  (puri:uri-path (net.aserve:request-uri (request request))))

#+#.(cl:if (cl:find-package "NET.URI") `(and) `(or))
(defmethod query-path ((request aserve-request))
  (net.uri:uri-path (net.aserve:request-uri (request request))))

(defmethod get-parameter ((request aserve-request) parameter-name)
  (net.aserve:request-query-value parameter-name (request request)))

(defmethod map-parameters ((request aserve-request) lambda)
  (dolist* ((name . value) (net.aserve:request-query (request request)))
    (funcall lambda name value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; response

(defclass aserve-response (response)
  ((request :accessor request :initarg :request)
   (entity :accessor entity :initarg :entity)
   (html-stream :accessor html-stream :initform (make-string-output-stream))))

(defun make-aserve-response (request entity)
  (make-instance 'aserve-response :request request :entity entity))

(defmethod shutdown ((response aserve-response))
  (net.aserve:with-http-response ((request response) (entity response)
				  :response (if (get-header response "Status")
                                                (net.aserve::code-to-response
                                                 (parse-integer
                                                  (get-header response "Status")))
                                                net.aserve:*response-ok*)
                                  :content-type (or (get-header response "Content-Type") "text/html"))
    (net.aserve:with-http-body ((request response) (entity response))
      (write-string (get-output-stream-string (html-stream response)) 
		    net.aserve::*html-stream*))))

(defmethod get-header ((response aserve-response) header-name)
  (switch (header-name :test #'string-equal)
    ("Date"             (net.aserve:reply-header-slot-value (request response) :date))
    ("Content-Type"     (net.aserve:reply-header-slot-value (request response) :content-type))
    ("Content-Length"   (net.aserve:reply-header-slot-value (request response) :content-length))
    (t (net.aserve:reply-header-slot-value
	(request response)
	(read-from-string (format nil ":~A" header-name))))))

(defmethod (setf get-header) (value (response aserve-response) header-name)
  (switch (header-name :test #'string-equal)
    ("Date"             (error "Can't update header Date"))
    ("Content-Type"     (setf (net.aserve:reply-header-slot-value (request response) :content-type)   value))
    ("Content-Length"   (setf (net.aserve:reply-header-slot-value (request response) :content-length) value))
    (t (setf (net.aserve:reply-header-slot-value
	      (request response)
	      (read-from-string (format nil ":~A" header-name)))
	     value))))

(defmethod add-header ((response aserve-response) header-name value)
  (push (cons header-name value)
	(net.aserve::request-reply-headers (request response))))

(defmethod clear-response ((response aserve-response))
  (setf (net.aserve::request-reply-headers (request response)) '())
  (setf (html-stream response) (make-string-output-stream))
  response)

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
