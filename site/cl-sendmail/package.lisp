;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; package.lisp: The cl-sendmail package
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


(defpackage :cl-sendmail
  (:use :cl :sb-gray :sb-unix :sb-ext :sb-bsd-sockets :mime :base64
	:xmls :xmls-tools)
  (:nicknames :sendmail)
  (:export :mailer-program-error
	   :mail-output-stream
	   :to
	   :cc
	   :bcc
	   :subject
	   :content-type
	   :attachments
	   :other-headers
	   :*smtp-server*
	   :*smtp-port*
	   :*mail-domain*
	   :make-mail-stream
	   :with-email
	   :xmls-xhtml-to-mime))

