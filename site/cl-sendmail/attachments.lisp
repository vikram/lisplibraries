;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; attachments.lisp: Generate emails with attachments
;;;; Copyright (C) 2006 Robert Marlow <bobstopper@bobturf.org>
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


(in-package :sendmail)


(defun build-attachments (mail-output-stream)
  "Converts MAIL-OUTPUT-STREAM to a multipart MIME email. Converts all
attachments to MIME parts of an attachment disposition and adds them
to the MIME email. Converts the content of the stream to a MIME attachment
according to the original MIME type of the stream and encodes it 
appropriately."
  (change-class mail-output-stream
		'multipart-mail-output-stream
		:type "multipart"
		:subtype (if (string-equal (content-type
					    mail-output-stream)
					   "text")
			     "mixed"
			     (content-subtype mail-output-stream))
		:content
		;; Add text input
		(cons (make-instance
		       (cond
			 ((string-equal (content-type
					 mail-output-stream)
					"text")
			  'text-mime)
			 ((string-equal (content-type
					 mail-output-stream)
					"multipart")
			  'multipart-mime)
			 (t 'mime))
		       :type (content-type mail-output-stream)
		       :subtype (content-subtype mail-output-stream)
		       :content
		       (or (content mail-output-stream)
			   (get-output-stream-string
			    (mail-output-stream-stream
			     mail-output-stream))))
		      (mapcar
		       (lambda (attachment)
			 (typecase attachment
			   (mime attachment)
			   (pathname 
			    (multiple-value-bind
				  (type subtype)
				(lookup-mime attachment)
			      (make-instance
			       'mime
			       :type type
			       :subtype subtype
			       :content
			       (read-file attachment)
			       :encoding
			       (if (string-equal type
						 "text")
				   :quoted-printable
				   :base64)
			       :disposition "attachment"
			       :disposition-parameters
			       `((:filename
				  ,(format 
				    nil "~A~@[.~A~]"
				    (pathname-name 
				     (pathname attachment))
				    (pathname-type
				     (pathname attachment))))))))))
		       (attachments mail-output-stream)))))
  

