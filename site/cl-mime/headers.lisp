;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; headers.lisp: Tools for handling headers
;;;; Copyright (C) 2004 Robert Marlow <bobstopper@bobturf.org>
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(in-package :mime)

(defgeneric get-header (mime-obj header)
  (:documentation
   "Returns a cons of header name (in keyword format) and value"))
    

(defmethod get-header ((mime-obj mime) (header (eql :content-type)))
  (cons header
	(format nil "~A/~A~A~A"
		(content-type mime-obj)
		(content-subtype mime-obj)
		;; Required parameters for particular MIME types
		(typecase mime-obj
		  (text-mime
		   (format nil "; charset=~A"
			   (charset mime-obj)))
		  (multipart-mime
		   (format nil "; boundary=\"~A\""
			   (boundary mime-obj)))
		  (otherwise ""))
		;; All remaining parameters defined by the user
		(format nil "~{~{;~%~5,5T~A=\"~A\"~}~}"
			(mapcar
			 (lambda (parm-pair)
			   (cons (string-downcase (symbol-name (car parm-pair)))
				 (cdr parm-pair)))
			 (content-type-parameters mime-obj))))))


(defmethod get-header ((mime-obj mime) (header (eql :content-disposition)))
  (when (content-disposition mime-obj)
    (cons header
	  (format nil "~A~A"
		  (content-disposition mime-obj)
		  (format nil "~{~{;~%~5,5T~A=\"~A\"~}~}"
			  (mapcar
			   (lambda (parm-pair)
			     (cons (string-downcase (symbol-name (car parm-pair)))
				   (cdr parm-pair)))
			   (content-disposition-parameters mime-obj)))))))


(defmethod get-header ((mime-obj mime) (header symbol))
  (aif (slot-value mime-obj (intern (string header) :mime))
       (cons (ensure-keyword header) it)))


(defun get-mime-headers (mime-obj)
  "Retrieves all known headers in mime-obj"
  (declare (mime mime-obj))
  (delete nil (mapcar (lambda (header)
			(get-header mime-obj header))
		      '(:mime-version
			:content-type
			:content-transfer-encoding
			:content-description
			:content-id
			:content-disposition))))
