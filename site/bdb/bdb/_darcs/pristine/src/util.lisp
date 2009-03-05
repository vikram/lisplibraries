;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Utilities

(defun make-cbuffer-writer (fn)
  (lambda (data buf)
    (let ((stream (make-cbuffer-output-stream :cbuffer buf)))
      (funcall fn data stream))))

(defun make-cbuffer-reader (fn)
  (lambda (buf)
    (if (cbuffer-p buf)
	(let ((stream (make-cbuffer-input-stream buf)))
	  (funcall fn stream))
	buf)))

(defun make-get-buffer (data)
  (let ((size (max (if data (cbuffer-size data) 0)
		    1024))
	(buf (alloc-cbuffer)))
    (when (< (cbuffer-length buf) size)
      (cbuffer-resize buf size))
    (when data
      (memcpy (cbuffer-data buf)
	      (cbuffer-data data)
	      (cbuffer-size data))
      (setf (cbuffer-size buf)
	    (cbuffer-size data)))
    buf))