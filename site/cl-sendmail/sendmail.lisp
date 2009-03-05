;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sendmail.lisp: The Main Program
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


(in-package :sendmail)

(defparameter *sendmail* 
  #P"/usr/lib/sendmail"
  "The location of the sendmail program")


(defun send-email (mail-output-stream)
  "Handles the actual sending of the email via the sendmail program"
  (unless (listp (to mail-output-stream))
    (setf (to mail-output-stream) (list (to mail-output-stream))))
  (unless (listp (cc mail-output-stream))
    (setf (cc mail-output-stream) (list (cc mail-output-stream))))
  (unless (listp (bcc mail-output-stream))
    (setf (bcc mail-output-stream) (list (bcc mail-output-stream))))
  (let ((sendmail (process-input
		   (run-program *sendmail*
				`("-f" ,(or (from mail-output-stream)
					    (sb-unix:uid-username 
					     (sb-unix:unix-getuid)))
				  ,@(to mail-output-stream)
				  ,@(cc mail-output-stream)
				  ,@(bcc mail-output-stream))
				:input :stream
				:wait nil)))
	(mail-output-stream
	 (cond
	   ((attachments mail-output-stream)
	    (build-attachments mail-output-stream))
	   
	   ((and
	     (string-equal (content-type mail-output-stream) "text")
	     (string-equal (content-subtype mail-output-stream) "html"))
	    (build-xhtml-email mail-output-stream))
	   
	   ((string-equal (content-type mail-output-stream) "text")
	    (change-class mail-output-stream
			  'text-mail-output-stream
			  :content (or (content mail-output-stream)
				       (get-output-stream-string
					(mail-output-stream-stream
					 mail-output-stream)))))
	   
	   ((string-equal (content-type mail-output-stream) "multipart")
	    (change-class mail-output-stream
			  'multipart-mail-output-stream)))))

    (mapc (lambda (header value)
	    (when value
	      (format sendmail "~A: ~A~{,~A~}~%" 
		      header
		      (if (listp value)
			  (first value)
			  value)
		      (if (listp value)
			  (rest value)
			  nil))))
	  (list "To" "Cc" "From" "Reply-To" "Subject" "Message-Id" "In-Reply-To" "References")
	  (list (to mail-output-stream)
		(cc mail-output-stream)
		(from mail-output-stream)
		(reply-to mail-output-stream)
		(subject mail-output-stream)
		(message-id mail-output-stream)
		(in-reply-to mail-output-stream)
		(references mail-output-stream)))
    
    (print-mime sendmail mail-output-stream t t)

    (close sendmail)))
      

;;; Deprecated
(defun make-mail-stream (to &key subject cc bcc reply-to mailer)
  (declare (ignore mailer))
  (make-instance 'mail-output-stream
		 :subject subject
		 :to to
		 :cc cc
		 :bcc bcc
		 :other-headers 
		 `(,(when reply-to `("Reply-To" ,reply-to)))))


(defmacro with-email ((stream to &key
			      cc
			      bcc
			      subject
			      from
			      reply-to
			      message-id
			      in-reply-to
			      references
			      (type "text")
			      (subtype "plain")
			      attachments
			      other-headers)
		      &body body)
  "Binds STREAM to a MAIL-OUTPUT-STREAM created according to the other 
arguments then executes BODY within that context. Automatically closes
the stream and sends the email upon completion."
  `(let ((,stream (make-instance 'mail-output-stream
				 :to ,to
				 :cc ,cc
				 :bcc ,bcc
				 :subject ,subject
				 :from ,from
				 :reply-to ,reply-to
				 :type ,type
				 :subtype ,subtype
				 :attachments ,attachments
				 :message-id ,message-id
				 :in-reply-to ,in-reply-to
				 :references ,references
				 :other-headers ,other-headers)))
    (unwind-protect
	 (progn
	   ,@body)
      (close ,stream))))

