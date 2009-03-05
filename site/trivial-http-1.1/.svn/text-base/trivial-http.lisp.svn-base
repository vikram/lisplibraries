(defpackage :trivial-http
  (:use :cl :trivial-sockets)
  (:nicknames :thttp)
  (:export :http-get :http-post :escape-url-query))
(in-package :trivial-http)

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
  (let ((path-start (position #\/ url :start 7)))
    (let ((port-start (position #\: url :start 7 :end path-start)))
      (if port-start (parse-integer url :start (1+ port-start) :junk-allowed t) 80))))

(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
	 (host-end (min (or (position #\/ url :start 7) (length url))
			(or port-start (length url)))))
    (subseq url 7 host-end)))

(defconstant +crlf+
  (if (boundp '+crlf+)
      (symbol-value '+crlf+)
      (concatenate 'string
                   (string (code-char 13))
                   (string (code-char 10)))))

(defun response-read-code (stream)
  (let* ((l (read-line stream))
            (space (position #\Space l)))
       (parse-integer l :start (1+ space) :junk-allowed t)))

(defun response-read-headers (stream)
  (loop for line = (read-line stream nil nil)
     until (or (eql (length line) 0)
	       (eql (elt line 0) (code-char 13))
	       (eql (elt line 0) (code-char 10)))
     collect
       (let ((colon (position #\: line)))
	 (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
	       (string-trim (list #\Space (code-char 13) (code-char 10))
			    (subseq line (1+ colon)))))))

(defun http-get (url)
  (let* ((host (url-host url))
         (port (url-port url))
         (stream (open-stream host port)))
    (format stream "GET ~A HTTP/1.0~AHost: ~A~AUser-Agent: Trivial HTTP for Common Lisp~A~A"
	    url +crlf+ host +crlf+ +crlf+ +crlf+)
    (force-output stream)
    (list
     (response-read-code stream)
     (response-read-headers stream)
     stream)))

(defun http-post (url content-type content)
  (let* ((host (url-host url))
         (port (url-port url))
         (stream (open-stream host port)))
    (format stream "POST ~A HTTP/1.0~AHost: ~A~AUser-Agent: Trivial HTTP for Common Lisp~AContent-Type: ~A~AContent-Length: ~D~A~A~A" url +crlf+ host +crlf+ +crlf+ content-type +crlf+ (length content) +crlf+ +crlf+ content)
    (force-output stream)
    (list
     (response-read-code stream)
     (response-read-headers stream)
     stream)))

;; this next method stolen from Araneida

(defun url-reserved-character-p (c)
  (not (or (member c '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\) ))
           (alphanumericp c))))

(defun escape-url-query (query)
  (apply #'concatenate 'string
   (loop for c across query
         if (url-reserved-character-p c)
         collect (format nil "%~2,'0X" (char-code c))
         else
         collect (string c))))
