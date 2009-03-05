;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** A Trivial HTTP Server

;;;; We don't actually expect anyone to use this backend but 1) it's
;;;; convenient when getting starting and 2) the mod_lisp backend
;;;; reuses most of it.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass httpd-backend (backend)
    ((port :accessor port :initarg :port :initform 8080)
     (host :accessor host :initarg :host :initform "127.0.0.1")
     (socket :accessor socket)
     (server :accessor server :initarg :server)
     (handlers :accessor handlers :initform '())))
  
  (defclass httpd-message (message)
    ((headers :accessor headers :initform '())
     (network-stream :accessor network-stream :initarg :network-stream)))

  (defclass httpd-request (httpd-message request)
    ((parameters :accessor parameters :initform '())
     (raw-uri :accessor raw-uri :initform nil)
     (query-path :accessor query-path :initform nil)
     (raw-body :accessor raw-body :initform nil)
     (peer-address :accessor peer-address :initform nil)
     (http-method :accessor http-method :initform nil)))

  (defclass httpd-response (httpd-message response)
    ((request :accessor request :initarg :request :initform nil)
     (html-stream :accessor html-stream :initform (make-string-output-stream))
     (status :accessor status :initform "200 OK")
     (external-format :accessor external-format :initform nil)
     (content :accessor content :initform nil))))

;;;; Backend methods
(defmethod initialize-backend ((backend httpd-backend) &key server &allow-other-keys)
  (when (and (null *mime-types*) (probe-file *default-mime-types-file*))
    (read-mime-types *default-mime-types-file*))
  (setf (server backend) server)
  backend)

(defmethod startup-backend :before ((backend httpd-backend) &rest initargs)
  (declare (ignore initargs))
  (setf (socket backend) (open-server :host (host backend) :port (port backend))))

(defmethod handle-request ((backend httpd-backend) (request httpd-request) (response httpd-response))
  (ucw.backend.info "Handling request from ~S for ~S" (peer-address request) (raw-uri request))
  (or (block handle
        (dolist* ((can-match handler url-base) (handlers backend))
          (declare (ignore url-base))
          (when (funcall can-match (query-path request))
            (funcall handler request response)
            (return-from handle t)))
        nil)
      (handle-request (server backend) request response)
      ;; if we get here there's no handler defined for the request
      (handle-404 backend request response)))

(defmethod handle-404 ((backend httpd-backend) (request httpd-request) (response httpd-response))
  (setf (get-header response "Status") "404 Not Found"
        (get-header response "Content-Type") "text/html")
  (with-yaclml-stream (html-stream response)
    (<:html
     (<:head (<:title "404 Not Found"))
     (<:body
      (<:p (<:as-html (raw-uri request)) " not found."))))
  (close-request request)
  (send-response response))

(defmethod send-error ((stream stream) message)
  "Ignore trying to read the request or anything. Just send an
error message. This is a very low level bailout function."
  (let ((response (make-response stream)))
    (setf (get-header response "Status") "500 Internal Server Error"
          (get-header response "Content-Type") "text/html")
    (with-yaclml-stream (html-stream response)
      (<:html
       (<:head (<:title "500 Internal Server Error"))
       (<:body
        (<:p "Server Error.")
        (<:p (<:as-html message)))))
    (send-response response)))

;;;; The single threaded server

(defun abort-backend-request (&optional condition)
  (ucw.backend.info "Gracefully aborting httpd request because: ~S" condition)
  (throw 'abort-backend-request nil))

(defmethod startup-backend ((backend httpd-backend) &rest init-args)
  "Start the RERL."
  (declare (ignore init-args))
  (let (stream peer-address request response)
    (labels ((serve-one-request ()
               (multiple-value-setq (stream peer-address)
                 (accept-connection (socket backend) :element-type '(unsigned-byte 8)))
               (setf request (read-request backend stream)
                     response (make-response request)
                     (peer-address request) peer-address)
               (handle-request backend request response))
             (handle-request-error (condition)
               (ucw.backend.error "While handling a request on ~S: ~A" stream condition)
               (when *debug-on-error*
                 (restart-case
                     (swank:swank-debugger-hook condition nil)
                   (kill-worker ()
                     :report "Kill this worker."
                     (values))))
               (abort-backend-request condition))
             (handle-request/internal ()
               (catch 'abort-backend-request
                 (handler-bind ((stream-error (lambda (c)
                                                (when (eq (stream-error-stream c) stream)
                                                  (abort-backend-request c))))
                                (error #'handle-request-error))
                   (unwind-protect
                        (serve-one-request)
                     (ignore-errors (close stream)))))))
      (unwind-protect
           (loop (handle-request/internal))
        (ignore-errors
	  (swank-backend:close-socket (socket backend))))))
  backend)

(defmethod shutdown-backend ((backend httpd-backend) &rest init-args)
  "This would stop the single therad httpd backend if that made any sense.

Stopping the single therad backend requires nothing more than
getting STARTUP-BACKEND to return (either normally or by chosing
you implementation's abort restart after a break)."
  (declare (ignore init-args))
  backend)

(defmethod publish-directory ((backend httpd-backend) directory-pathname url-base)
  (push (list (lambda (request-url)
                (ucw.backend.dribble "Trying to match '~S' under url-base '~S' to serve it as a file from '~S'"
                                     request-url url-base directory-pathname)
                (starts-with request-url url-base))
              (lambda (request response)
                (aif (map-query-path-to-file (query-path request)
                                             url-base
                                             directory-pathname)
                     (progn
                       (ucw.backend.debug "Serving '~S' as a file under url-base '~S'" it url-base)
                       (serve-file it request response))
                     (progn
                       (ucw.backend.debug "Failed to serve '~S' as a file under url-base '~S'" (query-path request) url-base)
                       (handle-404 backend request response))))
              url-base)
        (handlers backend)))

;;;; Message headers methods

(defmethod get-header ((message httpd-message) header-name)
  (cdr (assoc header-name (headers message) :test #'string-equal)))

(defmethod (setf get-header) (value (message httpd-message) header-name)
  (aif (assoc header-name (headers message) :test #'string-equal)
       (setf (cdr it) value)
       (push (cons header-name value) (headers message)))
  value)

(defmethod add-header ((message httpd-message) header-name value)
  (push (cons header-name value) (headers message))
  value)

(defmethod delete-header ((message httpd-message) header-name)
  (setf (headers message)
        (delete-if #'(lambda (item)
                       (string-equal (car item)
                                      header-name))
                   (headers message))))


;;;; Request handling

(defun read-line-from-network (stream &optional (eof-error-p t))
  "A simple state machine which reads chars from STREAM until it
  gets a CR-LF sequence or the end of the stream."
  (declare (optimize (speed 3)))
  (let ((buffer (make-array 50
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (labels ((read-next-char ()
               (let ((byte (read-byte stream eof-error-p stream)))
                 (if (eq stream byte)
                     (return-from read-line-from-network buffer)
                     (return-from read-next-char byte))))
             (cr ()
               (let ((next-byte (read-next-char)))
                 (case next-byte
                   (#.+linefeed+ ;; LF
                      (return-from read-line-from-network buffer))
                   (t ;; add both the cr and this char to the buffer
                    (vector-push-extend #.+carriage-return+ buffer)
                    (vector-push-extend next-byte buffer)
                    (next)))))
             (next ()
               (let ((next-byte (read-next-char)))
                 (case next-byte
                   (#.+carriage-return+ ;; CR
                      (cr))
		   (#.+linefeed+ ;; LF
		      (return-from read-line-from-network buffer))
                   (t
                    (vector-push-extend next-byte buffer)
                    (next))))))
      (next))))

(defun accumulate-parameters (assoc-list)
  "Accumulates same parameters into lists. Otherwise
  multiple-selection lists won't have a list value and
  <ucw:select would fail."
  (let ((result '()))
    (dolist* ((name . value) assoc-list)
      (unless (string= name "")
	(aif (assoc name result :test #'string=)
	     (if (and (cdr it) (listp (cdr it)))
		 (setf (cdr it) (cons value (cdr it)))
		 (setf (cdr it) (list value (cdr it))))
	     (push (cons name value) result))))
;;; reverse the (cdr it) so that writer lambda's see the values
;;; in correct order. 
    (dolist (it result)
      (when (and (cdr it) (listp (cdr it)))
	(setf (cdr it) (nreverse (cdr it)))))
;;; rever the result so that map-parameters see the request
;;; parameters in correct order.
    (nreverse result)))

(defmethod read-request ((backend httpd-backend) stream)
  "Reads an HTTP request message from STREAM. Returns a httpd-request object."
  (let* ((request (make-instance 'httpd-request :network-stream stream))
         (line (read-line-from-network stream))
         (pieces (split-on-space line)))
    (ucw.backend.dribble "In read-request, first line in :us-ascii is ~S, pieces are ~S"
                         (octets-to-string line :us-ascii) pieces)
    (destructuring-bind (http-method uri &optional protocol) pieces
      (declare (ignore protocol))
      (setf (raw-uri request) (coerce (octets-to-string uri #.(or (external-format-for :url) :iso-8859-1)) 'simple-string)
            (http-method request) (coerce (octets-to-string http-method :us-ascii) 'simple-string)
            (headers request) (read-request-headers stream))
      (aif (position #\? (raw-uri request))
           (setf (query-path request) (make-displaced-array (raw-uri request) 0 it)
                 (parameters request) (parse-query-parameters
                                       (make-displaced-array (raw-uri request)
                                                             (1+ it))))
           (setf (query-path request) (raw-uri request)
                 (parameters request) '()))
      (setf (query-path request) (unescape-as-uri (query-path request)))
      (setf (parameters request) (append (parameters request)
                                         (accumulate-parameters
                                          (parse-request-body stream
                                                              (get-header request "Content-Length")
                                                              (get-header request "Content-Type"))))))
    request))

(defmethod get-parameter ((request httpd-request) name)
  (loop
     with value = '()
     for (k . v) in (parameters request)
     when (string= k name)
       do (if value
              (if (consp value)
                  (push v value)
                  (setf value (list v value)))
              (setf value v))
     finally (return value)))

(defmethod map-parameters ((request httpd-request) lambda)
  (dolist* ((name . value) (parameters request))
    (unless (string= name "")
      (funcall lambda name (if (stringp value)
                               (copy-seq value)
                               value)))))

(defun read-request-headers (stream)
  (iterate
    (for header-line = (read-line-from-network stream))
    (until (= 0 (length header-line)))
    (for (name . value) = (split-header-line header-line))
    (collect (cons (octets-to-string name :us-ascii)
                   (octets-to-string value :iso-8859-1)))))

(defmethod close-request ((request httpd-request))
  request)

;;;; Response objects

(defmethod make-response ((request httpd-request))
  (make-instance 'httpd-response
                 :request request
                 :network-stream (network-stream request)))

(defmethod make-response ((stream stream))
  (make-instance 'httpd-response :network-stream stream))

(defmethod clear-response ((response httpd-response))
  (setf (html-stream response) (make-string-output-stream)
        (headers response) '()))

;;;; httpd-response objects special case the "Status" header.

(defmethod get-header ((response httpd-response) header-name)
  (if (string= "Status" header-name)
      (status response)
      (call-next-method)))

(defmethod (setf get-header) (value (response httpd-response) header-name)
  (if (string= "Status" header-name)
      (setf (status response) value)
      (call-next-method)))

(defun write-crlf (stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun write-header-line (name value stream)
  (write-sequence (string-to-octets name :us-ascii) stream)
  ;; ": "
  (write-byte 58 stream)
  (write-byte 32 stream)
  (write-sequence (string-to-octets value :iso-8859-1) stream)
  (write-crlf stream))

(defmethod encoding ((response httpd-response))
  (or (external-format response)
      (call-next-method)))

(defun httpd-send-headers (response &optional calculate-content-length-from-body)
  (ucw.backend.dribble "Sending headers for ~S (Status: ~S)." response (status response))
  (let ((stream (network-stream response)))
    (write-sequence #.(string-to-octets "HTTP/1.1 " :us-ascii) stream)
    (write-sequence (string-to-octets (status response) :us-ascii) stream)
    (write-byte 32 stream)
    (write-crlf stream)
    (dolist* ((name . value) (headers response))
      (unless (null value)
        (ucw.backend.dribble "Sending header ~S: ~S" name value)
        (write-header-line name value stream)))
    (when calculate-content-length-from-body
      (setf (content response)
            (string-to-octets (get-output-stream-string (html-stream response))
                              (encoding response)))
      (unless (assoc "Content-Length" (headers response) :test #'string-equal)
        ;; Content-Length may be defined and NIL, we don't print header then
        (write-header-line "Content-Length" (princ-to-string (length (content response))) stream)))
    (write-crlf stream)
    response))

(defmethod send-headers ((response httpd-response))
  (httpd-send-headers response nil))

(defmethod send-response ((response httpd-response))
  (httpd-send-headers response t)
  (unless (and (request response)
               (string= "HEAD" (http-method (request response))))
    (ucw.backend.dribble "HTTPD: Sending ~S (~D bytes) as body"
                         (content response) (length (content response)))
    (write-sequence (content response) (network-stream response))))

;;;; Debugging the backend

(defparameter *httpd-trace-functions*
  '(swank-backend:send
    swank-backend:spawn
    swank-backend:receive
    initialize-backend
    startup-backend
    shutdown-backend
    abort-backend-request
    close-request
    send-response
    httpd-send-headers
    httpd-controller-loop
    httpd-worker-loop/handle
    httpd-worker-loop
    httpd-accept-loop
    get-header
    (setf get-header)
    add-header
    read-request
    parse-query-parameters
    parse-request-body
    rfc2388-callback
    grab-param
    read-line-from-network
    split-on-space
    split-header-line
    read-request-headers
    make-response
    get-parameter
    map-parameters
    rfc2388:parse-header-value
    rfc2388:get-header))

(defun trace-httpd-backend ()
  (eval `(trace ,@*httpd-trace-functions*)))

(defun untrace-httpd-backend ()
  (eval `(untrace ,@*httpd-trace-functions*)))

;; Copyright (c) 2005-2006 Edward Marco Baringer
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
