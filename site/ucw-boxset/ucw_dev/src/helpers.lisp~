;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; * Miscalaneous helper code

(defun new-random-key (hash-table key-length)
  (loop
     for key = (strcat "_" (random-string key-length))
     while (gethash key hash-table)
     finally (return key)))

(defun insert-with-new-key (hash-table key-length value)
  "helper method. generates random strings of length key-length until
  it finds one that isn't a key in hash-table and sets value to
  that. returns the new id."
  (let ((key (new-random-key hash-table key-length)))
    (setf (gethash key hash-table) value)
    key))

;; in many cases we would like to replace this simple all or nothing
;; locking mechanism with a reader/writer locking mechanism.

(defmacro with-lock-held (lock &body body)
  `(swank::call-with-lock-held ,lock (lambda () ,@body)))

;;;; ** Simple URL manipulation

;;;; May be replaced with something better (puri?) should the need
;;;; arise.

(defclass uri ()
  ((scheme   :initarg :scheme   :initform nil :accessor uri.scheme)
   (host     :initarg :host     :initform nil :accessor uri.host)
   (port     :initarg :port     :initform nil :accessor uri.port)
   (path     :initarg :path     :initform nil :accessor uri.path)
   (query    :initarg :query    :initform nil :accessor uri.query)
   (fragment :initarg :fragment :initform nil :accessor uri.fragment)))

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :type t :identity nil)
    (write-uri uri stream)))

(defun write-uri-sans-query (uri stream)
  "Write URI to STREAM, only write scheme, host and path."
  (with-slots (scheme host path)
      uri
    (when scheme
      (write-string scheme stream)
      (write-string "://" stream))
    (when host
      (write-string host stream)
      (write-string "/" stream))
    
    (if (or (position #\? path)
            (position #\= path)
            (position #\+ path))
        (loop
           for char across path
           do (case char
                (#\? (write-string "%3F" stream))
                (#\= (write-string "%3D" stream))
                (#\+ (write-string "%2B" stream))
                (t (write-char char stream))))
        (write-string path stream))))

(defun write-uri (uri stream)
  (with-slots (scheme host path query fragment)
      uri
    (write-uri-sans-query uri stream)
    (when query
      (flet ((write-query-part (name value)
               (write-as-uri name stream)
               (write-char #\= stream)
               (write-as-uri value stream)))
        (write-string "?" stream)
        (write-query-part (car (first query))
                          (cdr (first query)))
        (iterate
          (for (name . value) in (cdr query))
          (write-char #\& stream)
          (write-query-part name value))))
    (when fragment
      (write-char #\# stream)
      (write-string fragment stream))))

(defun print-uri-to-string (uri)
  (with-output-to-string (string)
    (write-uri uri string)))

(defun print-uri-to-string-sans-query (uri)
  (with-output-to-string (string)
    (write-uri-sans-query uri string)))

(defun read-from-client-string (str &rest opt-args)
  "Read a string from an untrusted source (don't eval anything)."
  (let ((*read-eval* nil))
    (apply #'read-from-string str opt-args))) ;;*read-eval*=nil

;; Copyright (c) 2003-2005 Edward Marco Baringer
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
