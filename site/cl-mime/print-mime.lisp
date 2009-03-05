;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; print-mime.lisp: Tools for printing a mime object
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

(defun add-to-print-headers (key alist headers-out)
  "Adds a requested header from the provided alist to a printable
headers string"
  (aif (assoc key alist)
       (concatenate 'string headers-out
		    (format nil "~A: ~A~%"
			    (if (eql :mime-version (car it))
				"MIME-Version"
				(string-capitalize (symbol-name (car it))))
			    (cond
			      ((eql :content-id (car it))
			       (format nil "<~A>" (cdr it)))
			      ((eql :content-transfer-encoding (car it))
			       (string-downcase (symbol-name (cdr it))))
			      (t
			       (cdr it)))))
       headers-out))


;;; Note that the way we print the headers, other than type-compulsory
;;; content-type parameters, all parameters are folded as per RFC822, once
;;; for each parameter such that each parameter begins a new line. This is
;;; so header lines don't get too long and so we don't have to fuss about
;;; too much counting line length and figuring out where to break up long
;;; lines. I couldn't find anything in the RFC which prohibited this so
;;; I'm taking the easy route.

(defun print-headers (stream headers version-p)
  "Prints headers in the provided assoc-list"
  (let ((headers-to-print '(:content-type
			    :content-transfer-encoding
			    :content-description
			    :content-id
			    :content-disposition))
	(headers-out ""))
    (if version-p
	(push :mime-version headers-to-print))
    (mapc (lambda (header-key)
	    (setq headers-out
		  (add-to-print-headers header-key
					headers
					headers-out)))
	  headers-to-print)
    (format stream "~A" headers-out)))


(defgeneric print-mime (stream mime-obj headers-p version-p)
  (:documentation
   "Prints a mime object's contents, optionally with headers"))


(defmethod print-mime (stream (mime-obj mime) headers-p version-p)
  (format stream "~A~A"
	  (if headers-p
	      (concatenate 'string
			   (print-headers nil (get-mime-headers mime-obj)
					  version-p)
			   (string #\newline))
	      "")
	  (encode-content mime-obj)))


(defmethod print-mime (stream (mime-obj multipart-mime) headers-p version-p)
  (format stream "~A~%~A~{~{--~A~%~:/mime:print-mime/~%~}~}~%--~A--~%~A"
	  (if headers-p
	      (concatenate 'string
			   (print-headers nil (get-mime-headers mime-obj)
					  version-p)
			   (string #\newline))
	    "")
	  (aif (prologue mime-obj)
	       (concatenate 'string it (string #\newline))
	       "")
	  (mapcar (lambda (mime-child)
		    (list
		     (boundary mime-obj)
		     mime-child))
		  (content mime-obj))
	  (boundary mime-obj)
	  (aif (epilogue mime-obj)
	       (concatenate 'string it (string #\newline))
	       "")))


