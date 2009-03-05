;; -*- lisp -*-

(in-package :it.bese.ucw) 

;;;; ** The mod_lisp backend

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass mod-lisp-backend (httpd-backend)
    ()
    (:default-initargs :port 3001))

  (defclass multithread-mod-lisp-backend (multithread-httpd-backend mod-lisp-backend)
    ())

  (defclass mod-lisp-worker (httpd-worker)
    ())
  
  (defclass mod-lisp-request (httpd-request)
    ())

  (defclass mod-lisp-response (httpd-response)
    ()))

;;;; The mod-lisp-request class methods. Most of the methods are
;;;; actually those defined by the httpd-backend, here we just replace
;;;; the header handling functions.

(defmethod make-response ((request mod-lisp-request))
  (make-instance 'mod-lisp-response
                 :network-stream (network-stream request)
                 :request request))

(defmethod read-request ((backend mod-lisp-backend) apache-stream)
  "Read the request (in mod-lisp's format) from the server's
  apache stream. Returns a new request object. Creates a fresh
  stream for each request."
  (let ((request (make-instance 'mod-lisp-request :network-stream apache-stream)))
    (ucw.backend.dribble "Reading request from ~S" apache-stream)
    (iterate
      (for key = (octets-to-string (read-line-from-network apache-stream) :us-ascii))
      (until (string= "end" key))
      (for value = (octets-to-string (read-line-from-network apache-stream)
				     (if (string= key "url")
					 #.(or (external-format-for :url) :iso-8859-1)
					 :iso-8859-1)))
      (when (string= key "remote-ip-addr")
	(setf (peer-address request) value))
      (when (string= key "url")
	(setf (raw-uri request) value)
	(aif (position #\? value)
	     (setf (query-path request) (make-displaced-array (raw-uri request) 0 it)
		   (parameters request) (parse-query-parameters
					 (make-displaced-array (raw-uri request)
							       (1+ it))))
	     (setf (query-path request) value
		   (parameters request) '()))
	(setf (query-path request) (unescape-as-uri (query-path request))))
      (ucw.backend.dribble "~S=~S" key value)
      (setf (get-header request key) value))
    (setf (parameters request) (append (parameters request)
				       (accumulate-parameters
					(parse-request-body apache-stream
							    (get-header request "Content-Length")
							    (get-header request "Content-Type")))))
    request))

(defmethod publish-directory ((backend mod-lisp-backend) directory-pathname url-base)
  (ucw.backend.warn
   "Attempting to publish ~S at ~S but mod_lisp backend does not support publish-directory."
   directory-pathname url-base))

;;;; mod-lisp-response

(defmacro with-mod-lisp-error-handler (() &body body)
  (with-unique-names (error-handler)
    `(block ,error-handler
       (handler-bind ((error (lambda (c)
                               (restart-case
                                   (when *debug-on-error* (swank:swank-debugger-hook c nil))
                                 (continue ()
                                   :report "Shutdown the response."
                                   t))
                               (ucw.backend.error "Error while shutdown'ing response: ~S." c)
                               (ignore-errors
                                 (close (network-stream response)))
                               (return-from ,error-handler nil))))
         ,@body))))

(defun mod-lisp-send-headers (response &optional calculate-content-length-from-body)
  (ucw.backend.dribble "Sending headers for ~S (Status: ~S)." response (status response))
  (with-mod-lisp-error-handler ()
    (with-slots (network-stream html-stream headers) response
      (mod-lisp-write-line "Status" network-stream)
      (mod-lisp-write-line (status response) network-stream)
      (iterate
        (for (key . value) in headers)
        (if (consp value)
            (dolist (v value)
              (mod-lisp-write-line key network-stream)
              (mod-lisp-write-line v network-stream))
            (progn
              (mod-lisp-write-line key network-stream)
              (mod-lisp-write-line value network-stream))))
      (when calculate-content-length-from-body
        (setf (content response)
              (string-to-octets (get-output-stream-string html-stream)
                                (encoding response)))
        (mod-lisp-write-line "Content-Length"
                             network-stream)
        (mod-lisp-write-line (format nil "~D" (length (content response)))
                             network-stream))
      (mod-lisp-write-line "end" network-stream))))

(defmethod send-headers ((response mod-lisp-response))
  (mod-lisp-send-headers response nil))

(defmethod send-response ((response mod-lisp-response))
  (ucw.backend.dribble "Sending mod-lisp response.")
  (mod-lisp-send-headers response t)
  (ucw.backend.dribble "Sending ----")
  (ucw.backend.dribble (content response))
  (ucw.backend.dribble "Done ----")
  (with-mod-lisp-error-handler ()
    (write-sequence (content response) (network-stream response))))

;;;; Helper functions

(defun mod-lisp-write-line (line stream)
  (write-sequence (string-to-octets line :us-ascii) stream)
  (write-byte #.(char-code #\Newline) stream))

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
