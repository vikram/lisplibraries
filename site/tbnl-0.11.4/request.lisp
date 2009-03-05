;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TBNL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/request.lisp,v 1.39 2006/09/30 13:20:25 edi Exp $

;;; Copyright (c) 2004-2006, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:tbnl)

(defclass request ()
  ((headers-in :initarg :headers-in
               :documentation "An alist of the incoming
headers. Note that these might be the headers coming in from
mod_lisp which are different from the headers sent by the
client.")
   (cookies-in :initform nil
               :documentation "An alist of the cookies sent by the client.")
   (get-parameters :initform nil
                   :documentation "An alist of the GET parameters sent
by the client.")
   (post-parameters :initform nil
                    :documentation "An alist of the POST parameters
sent by the client.")
   (script-name :initform nil
              :documentation "The URI requested by the client without
the query string.")
   (query-string :initform nil
                 :documentation "The query string of this request.")
   (session :initform nil
            :accessor session
            :documentation "The session object associated with this
request.")
   (aux-data :initform nil
             :accessor aux-data
             :documentation "Used to keep a user-modifiable alist with
arbitrary data during the request.")
   (raw-post-data :initform nil
                  :documentation "The raw string sent as the body of a
POST request, populated only if not a multipart/form-data request."))
  (:documentation "Objects of this class hold all the information
about an incoming request. They are created automatically by TBNL and
can be accessed by the corresponding handler."))

(defun parse-rfc2388-form-data (stream content-type-header)
  "Create an alist of POST parameters from the stream STREAM which is
supposed to be of content type 'multipart/form-data'."
  (let* ((parsed-content-type-header (rfc2388:parse-header content-type-header :value))
	 (boundary (or (cdr (rfc2388:find-parameter "BOUNDARY"
                                                    (rfc2388:header-parameters parsed-content-type-header)))
		       (return-from parse-rfc2388-form-data))))
    (loop for part in (rfc2388:parse-mime stream boundary)
          for headers = (rfc2388:mime-part-headers part)
          for content-disposition-header = (rfc2388:find-content-disposition-header headers)
          for name = (cdr (rfc2388:find-parameter "NAME" (rfc2388:header-parameters content-disposition-header)))
          when name
            collect (cons name
                          (let ((contents (rfc2388:mime-part-contents part)))
                            (if (pathnamep contents)
                              (list contents
                                    (rfc2388:get-file-name headers)
                                    (rfc2388:content-type part :as-string t))
                              contents))))))

(defun get-post-data (&optional (request *request*))
  "Reads the posted data from the stream and stores the raw contents
in the corresponding slot of the REQUEST object."
  (let* ((headers-in (headers-in request))
         (content-length (string-assoc "content-length" headers-in)))
    (setf (slot-value request 'raw-post-data)
          (cond (content-length
                 #+:lispworks
                 (when (input-chunking-p)
                   ;; see RFC 2616, section 4.4
                   (log-message :warn "Got Content-Length header although input chunking is on."))
                 (let ((content (make-string (parse-integer content-length
                                                            :junk-allowed t))))
                   (read-sequence content (string-assoc "content-stream" headers-in))
                   content))
                #+:lispworks
                ((input-chunking-p)
                 (let ((buffer (make-string +buffer-length+))
                       (content (make-array 0 :element-type 'base-char :adjustable t)))
                   (loop with stream = (string-assoc "content-stream" headers-in)
                         for index = 0 then (+ index pos)
                         for pos = (read-sequence buffer stream)
                         do (adjust-array content (+ index pos))
                            (replace content buffer :start1 index :end2 pos)
                         while (= pos +buffer-length+))
                   content))))))

(defmethod initialize-instance :after ((request request) &rest init-args)
  "The only initarg for a REQUEST object is :HEADERS-IN. All other
slot values are computed in this :AFTER method."
  (declare (ignore init-args))
  (with-slots ((headers-in headers-in)
               (cookies-in cookies-in)
               (get-parameters get-parameters)
               (post-parameters post-parameters)
               (script-name script-name)
               (query-string query-string)
               (session session))
              request
     (handler-case*
         (progn
           ;; compute SCRIPT-NAME and QUERY-STRING slots from
           ;; REQUEST_URI environment variable
           (let* ((uri (request-uri request))
                  (match-start (position #\? uri)))
             (cond
              (match-start
               (setq script-name (subseq uri 0 match-start)
                     query-string (subseq uri (1+ match-start))))
              (t (setq script-name uri))))
           ;; some clients send requests like
           ;; "GET http://server/foo.html HTTP/1.0"...
           (setq script-name (cl-ppcre:regex-replace "^https?://[^/]+" script-name ""))
           ;; compute GET parameters from query string and cookies from
           ;; the incoming 'Cookie' header
           (setq get-parameters
                 (form-url-encoded-list-to-alist
                  (cl-ppcre:split "&" query-string))
                 cookies-in
                 (form-url-encoded-list-to-alist
                  (cl-ppcre:split ";"
                                  (string-assoc "Cookie" headers-in)) +utf-8+)
                 session (session-verify request)
                 *session* session)
           ;; if the content-type is 'application/x-www-form-urlencoded'
           ;; or 'multipart/form-data', compute the post parameters from
           ;; the content body
           (let ((content-type (string-assoc "content-type" headers-in)))
             (setq post-parameters
                   (cond ((starts-with-p content-type "application/x-www-form-urlencoded"
                                         :test #'char-equal)
                          (and (or (string-assoc "content-length" headers-in)
                                   #+:lispworks (input-chunking-p))
                               (form-url-encoded-list-to-alist
                                (cl-ppcre:split "&"
                                                (get-post-data request)))))
                         ((starts-with-p content-type "multipart/form-data;"
                                         :test #'char-equal)
                          (setf (slot-value request 'raw-post-data) t)
                          (handler-case*
                              (let ((*request* request))
                                (parse-rfc2388-form-data (string-assoc "content-stream"
                                                                       headers-in)
                                                         content-type))
                            (error (msg)
                              (log-message :error
                                           "While parsing multipart/form-data parameters: ~A"
                                           msg)
                              nil)))))))
       (error (cond)
         (log-message* "Error when creating REQUEST object: ~A" cond)
         ;; we assume it's not our fault...
         (setf (return-code) +http-bad-request+)))))

(defun recompute-request-parameters (&key (request *request*)
                                          (external-format *tbnl-default-external-format*))
  "Recomputes the GET and POST parameters for the REQUEST object
REQUEST.  This only makes sense if you're using a different external
format."
  (with-slots ((headers-in headers-in)
               (get-parameters get-parameters)
               (post-parameters post-parameters)
               (raw-post-data raw-post-data)
               (query-string query-string))
      request
    (setq get-parameters
            (form-url-encoded-list-to-alist (cl-ppcre:split "&" query-string)
                                            external-format)
          post-parameters
            (and raw-post-data
                 (starts-with-p (string-assoc "content-type" headers-in)
                                "application/x-www-form-urlencoded"
                                :test #'char-equal)
                 (form-url-encoded-list-to-alist (cl-ppcre:split "&" raw-post-data)
                                                 external-format))))
  (values))
                                                
(declaim (inline script-name query-string get-parameters post-parameters headers-in cookies-in))
(defun script-name (&optional (request *request*))
  "Returns the file name of the REQUEST object REQUEST. That's
the requested URI without the query string \(i.e the GET
parameters)."
  (slot-value request 'script-name))

(defun query-string (&optional (request *request*))
  "Returns the query string of the REQUEST object REQUEST. That's
the part behind the question mark \(i.e. the GET parameters)."
  (slot-value request 'query-string))

(defun get-parameters (&optional (request *request*))
  "Returns an alist of the GET parameters associated with the
REQUEST object REQUEST."
  (slot-value request 'get-parameters))

(defun post-parameters (&optional (request *request*))
  "Returns an alist of the POST parameters associated with the
REQUEST object REQUEST."
  (slot-value request 'post-parameters))

(defun headers-in (&optional (request *request*))
  "Returns an alist of the incoming headers associated with the
REQUEST object REQUEST."
  (slot-value request 'headers-in))

(defun cookies-in (&optional (request *request*))
  "Returns an alist of all cookies associated with the REQUEST
object REQUEST."
  (slot-value request 'cookies-in))

(declaim (inline header-in))
(defun header-in (name &optional (request *request*))
  "Returns the incoming header with name NAME as captured in the
REQUEST object REQUEST.  Search is case-insensitive."
  (string-assoc name (headers-in request)))

(defun authorization (&optional (request *request*))
  "Returns as two values the user and password \(if any) as captured
in the 'AUTHORIZATION' header of the REQUEST object REQUEST."
  (let* ((authorization (header-in "Authorization" request))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (cl-ppcre:scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (user &optional password)
          (cl-ppcre:split ":"
                          (base64:base64-string-to-string
                           (subseq authorization start)))
        (values user password)))))

(defun remote-addr (&optional (request *request*))
  "Returns the 'REMOTE_ADDR' header \(if sent by the front-end,
otherwise the IP address of the remote host if available) as captured
in the REQUEST object REQUEST. See also REAL-REMOTE-ADDR."
  (or (header-in "remote-ip-addr" request)
      (socket-remote-host)))

(defun real-remote-addr (&optional (request *request*))
  "Returns the 'X-Forwarded-For' incoming http header value captured
in the REQUEST object REQUEST if it exists.  Otherwise returns the
value of REMOTE-ADDR."
  (or (header-in "X-Forwarded-For" request)
      (remote-addr request)))

(defun server-addr (&optional (request *request*))
  "Returns the 'SERVER_ADDR' header \(if sent by the front-end,
otherwise the IP address of the TBNL host if available) as captured in
the REQUEST object REQUEST."
  (or (header-in "server-ip-addr" request)
      (socket-local-host)))

(defun remote-port (&key (request *request*) (as-number t))
  "Returns the 'REMOTE_PORT' header \(if sent by the front-end,
otherwise the IP port of the remote host) as captured in the REQUEST
object REQUEST.  If AS-NUMBER is true, which is the default, the value
is returned as a number, otherwise as a string."
  (let ((remote-port (or (header-in "remote-ip-port" request)
                         (socket-remote-port))))
    (cond ((null remote-port) nil)
          ((and as-number (stringp remote-port))
           (parse-integer remote-port :junk-allowed t))
          ((and (not as-number) (numberp remote-port))
           (format nil "~A" remote-port))
          (t remote-port))))

(defun server-port (&key (request *request*) (as-number t))
  "Returns the IP port where the request came in \(if sent by the
front-end).  If AS-NUMBER-P is true, which is the default, the
value is returned as a number."
  (let ((server-port (or (header-in "server-ip-port" request)
                         (socket-local-port))))
    (cond ((null server-port) nil)
          ((and as-number (stringp server-port))
           (parse-integer server-port :junk-allowed t))
          ((and (not as-number) (numberp server-port))
           (format nil "~A" server-port))
          (t server-port))))

(defun host (&optional (request *request*))
  "Returns the 'Host' incoming http header value captured in the
REQUEST object REQUEST."
  (header-in "Host" request))

(defun request-uri (&optional (request *request*))
  "Returns the 'REQUEST_URI' header (if sent by the front-end) as
captured in the REQUEST object REQUEST."
  (header-in "url" request))

(defun request-method (&key (request *request*) (as-keyword t))
  "Returns the 'REQUEST_METHOD' header \(if sent by the front-end)
as captured in the REQUEST object REQUEST.  If AS-KEYWORD is
true, which is the default, the value will be returned as a
keyword like :GET or :POST."
  (let ((request-method (header-in "method" request)))
    (if (and request-method as-keyword)
      (nth-value 0 (intern (string-upcase request-method) :keyword))
      request-method)))

(defun server-protocol (&key (request *request*) (as-keyword t))
  "Returns the 'SERVER_PROTOCOL' environent value \(if sent by
the front-end) as captured in the REQUEST object REQUEST.  If
AS-KEYWORD is true, which is the default, the value will be
returned as a keyword like :HTTP/1.0 or :HTTP/1.1."
  (let ((server-protocol (header-in "server-protocol" request)))
    (if (and server-protocol as-keyword)
      (nth-value 0 (intern (string-upcase server-protocol) :keyword))
      server-protocol)))

(defun mod-lisp-id (&optional (request *request*))
  "Returns the 'Server ID' sent by mod_lisp as captured in the
REQUEST object REQUEST.  This value is set in Apache's server
configuration file and is of course only available if mod_lisp is
the front-end."
  (header-in "server-id" request))

(defun ssl-session-id (&optional (request *request*))
  "Returns the 'SSL_SESSION_ID' header \(if sent by the front-end)
as captured in the REQUEST object REQUEST."
  (header-in "ssl-session-id" request))

(defun user-agent (&optional (request *request*))
  "Returns the 'User-Agent' incoming http header value captured
in the REQUEST object REQUEST."
  (header-in "User-Agent" request))

(defun cookie-in (name &optional (request *request*))
  "Returns the cookie with the name NAME \(if any) as sent by the
browser and captured in the REQUEST object REQUEST."
  (string-assoc* name (cookies-in request)))

(defun referer (&optional (request *request*))
  "Returns the 'Referer' \(sic!) incoming http header value captured
in the REQUEST object REQUEST."
  (header-in "Referer" request))

(declaim (inline get-parameter))
(defun get-parameter (name &optional (request *request*))
  "Returns the GET parameter with name NAME as captured in the
REQUEST object REQUEST.  Search is case-sensitive."
  (string-assoc* name (get-parameters request)))

(declaim (inline post-parameter))
(defun post-parameter (name &optional (request *request*))
  "Returns the POST parameter with name NAME as captured in the
REQUEST object REQUEST.  Search is case-sensitive."
  (string-assoc* name (post-parameters request)))

(declaim (inline parameter))
(defun parameter (name &optional (request *request*))
  "Returns the GET or the POST parameter with name NAME as
captured in the REQUEST object REQUEST.  If both a GET and a POST
parameter with the same name exist the GET parameter is
returned. Search is case-sensitive."
  (or (get-parameter name request)
      (post-parameter name request)))

(defun handle-if-modified-since (time &optional (request *request*))
  "Handles the If-Modified-Since header of the REQUEST.  Date
string is compared to the one generated from the supplied TIME."
  (let ((if-modified-since (header-in "If-Modified-Since" request))
        (time-string (rfc-1123-date time)))
    ;; Simple string compare is sufficient. See RFC 2616 14.25
    (when (and if-modified-since
               (equal if-modified-since time-string))
      (setf (return-code) +http-not-modified+)
      (throw 'tbnl-handler-done nil))
    (values)))

(defun raw-post-data (&optional (request *request*))
  "Returns the raw string sent as the body of a POST request."
  (or (slot-value request 'raw-post-data)
      (get-post-data request)))

(defun aux-request-value (symbol &optional (request *request*))
  "Returns the value associated with SYMBOL from the request object
REQUEST \(the default is the current request) if it exists."
  (when request
    (let ((found (assoc symbol
                        (aux-data request)
                        :test #'eq)))
      (values (cdr found) found))))

(defsetf aux-request-value (symbol &optional request)
    (new-value)
  "Sets the value associated with SYMBOL from the request object
REQUEST \(default is *REQUEST*).  If there is already a value
associated with SYMBOL it will be replaced."
  (with-rebinding (symbol)
    (with-unique-names (place %request)
      `(let* ((,%request (or ,request *request*))
              (,place (assoc ,symbol (aux-data ,%request)
                             :test #'eq)))
         (cond
           (,place
            (setf (cdr ,place) ,new-value))
           (t
            (push (cons ,symbol ,new-value)
                  (aux-data ,%request))
            ,new-value))))))

(defun delete-aux-request-value (symbol &optional (request *request*))
  "Removes the value associated with SYMBOL from the request object
REQUEST."
  (when request
    (setf (aux-data request)
            (delete symbol (aux-data request)
                    :key #'car :test #'eq)))
  (values))
