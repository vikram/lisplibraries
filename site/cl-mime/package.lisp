;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; package.lisp: Package definition
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



(defpackage :cl-mime
  (:documentation "A package for constructing MIME objects for printing and
parsing MIME formatted strings or streams.")
  (:nicknames :mime)
  (:use :cl :cl-ppcre :base64)
  (:export :text-mime
	   :multipart-mime
	   :mime
	   :lookup-mime
	   :make-content-id
	   :content-type
	   :content-subtype
	   :content-type-parameters
	   :content-id
	   :content-description
	   :content-transfer-encoding
	   :content-disposition
	   :content-disposition-parameters
	   :mime-version
	   :charset
	   :boundary
	   :prologue
	   :epilogue
	   :content
	   :get-header
	   :get-mime-headers
	   :get-content-type-parameter
	   :get-content-disposition-parameter
	   :header-value
	   :header-parms
	   :header-comments
	   :print-mime
	   :print-headers
	   :parse-mime
	   :parse-body
	   :parse-headers
	   :decode-content
	   :encode-content))

(in-package :mime)
