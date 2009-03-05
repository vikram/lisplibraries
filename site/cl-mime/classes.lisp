;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; classes.lisp: MIME classes and generalised method definitions
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

(defclass mime ()
  ((content-type
    :reader content-type
    :initarg :type
    :initform (error "MIME objects must have a type"))
   (content-subtype
    :reader content-subtype
    :initarg :subtype
    :initform (error "MIME objects must have a subtype"))
   (content-type-parameters
    :accessor content-type-parameters
    :initarg :parameters
    :type list
    :initform nil)
   (content-id
    :accessor content-id
    :initarg :id
    :initform nil)
   (content-description
    :accessor content-description
    :initform nil
    :initarg :description)
   (content-transfer-encoding
    :accessor content-transfer-encoding
    :initarg :encoding
    :initform :7bit
    :documentation
    "Encoding to use when printing the MIME content.
May be :7BIT :BASE64 or :QUOTED-PRINTABLE")
   (content-encoding
    :accessor content-encoding
    :initarg :content-encoding
    :initform :7bit
    :documentation "Encoding the MIME content is currently in.
May be :7BIT :BASE64 or :QUOTED-PRINTABLE")
   (content-disposition
    :accessor content-disposition
    :initarg :disposition
    :initform nil)
   (content-disposition-parameters
    :accessor content-disposition-parameters
    :initarg :disposition-parameters
    :type list
    :initform nil)
   (content
    :accessor content
    :initform nil
    :initarg :content)
   (mime-version
    :accessor mime-version
    :initarg :version
    :initform "1.0"))
  (:documentation "Standard MIME Object Representation"))


(defclass text-mime (mime)
  ((content-type
    :reader content-type
    :initform "text")
   (content-subtype
    :reader content-subtype
    :initform "plain"
    :initarg :subtype)
   (charset
    :accessor charset
    :initarg :charset
    :initform "us-ascii"))
  (:documentation "Text MIME Object Representation"))


(defclass multipart-mime (mime)
  ((content-type
    :reader content-type
    :initform "multipart")
   (content
    :accessor content
    :initarg :content
    :type list)
   (boundary
    :accessor boundary
    :initarg :boundary
    :initform (make-boundary))
   (prologue
    :accessor prologue
    :initform nil
    :initarg :prologue
    :type string)
   (epilogue
    :accessor epilogue
    :initform nil
    :initarg :epilogue
    :type string))
  (:documentation "Multipart Mime Object Representation"))
    
;;; This boundary contains text which should never appear in the
;;; message body. Hopefully that big random number converted to base
;;; 36 (all numbers and alphabet) will be good enough for guaranteeing that.
(defun make-boundary ()
  "This just makes a boundary out of random junk"
  (format nil "=_cl-mime~36,,,,R" (* (get-universal-time)
				     (random 100000000000000000000))))


;;; This content-id needs to be unique for every message. The assumption
;;; here is that in the world make-content-id should only be
;;; called a few (less than 1000000000000000000) times per second. We hope
;;; short-site-name actually returning something to narrow 
;;; the value down more.
(defun make-content-id ()
  "Make a Content-ID header value"
  (format nil "~A.~A.cl-mime@~A"
	  (get-universal-time)
	  (random 1000000000000000000)
	  (short-site-name)))


(defun get-content-type-parameter (mime-obj parameter-name)
  "Provided a parameter name in the form of a keyword, will get the
corresponding value from the parameter list of the Content-Type header"
  (second (assoc
	   (ensure-keyword parameter-name)
	   (content-type-parameters mime-obj))))


(defun get-content-disposition-parameter (mime-obj parameter-name)
  "Provided a parameter name in the form of a keyword, will get the
corresponding value from the parameter list of the Content-Disposition
header"
  (second (assoc
	   (ensure-keyword parameter-name)
	   (content-disposition-parameters mime-obj))))

