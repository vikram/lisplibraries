;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; encoding.lisp: Tools for converting content encoding
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


(defun encode-content (mime)
  (if (eql (content-transfer-encoding mime)
	   (content-encoding mime))
      (content mime)
      (let ((content (decode-content mime)))
	(ecase (content-transfer-encoding mime)
	  (:7bit content)
	  (:base64 
	   (typecase content
	     (string (string-to-base64-string content :columns 75))
	     ((array (unsigned-byte 8))
	      (usb8-array-to-base64-string content :columns 75))))
	  (:quoted-printable (qprint:encode content 75))))))


(defun decode-content (mime)
  (ecase (content-encoding mime)
    (:7bit (content mime))
    (:base64 (base64-string-to-usb8-array (content mime)))
    (:quoted-printable (qprint:decode (content mime)))))

