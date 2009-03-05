;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cl-sendmail.asd: System definition
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


(defpackage :cl-sendmail-system (:use #:cl #:asdf))
(in-package :cl-sendmail-system)

(defsystem :cl-sendmail
  :name "CL-SENDMAIL"
  :author "Robert Marlow <rob@bobturf.org>"
  :maintainer "Robert Marlow <rob@bobturf.org>"
  :serial t
  :version "0.5.0"
  :depends-on (:sb-bsd-sockets :cl-mime :xmls :xmls-tools)
  :components ((:file "package")
	       (:file "utilities")
	       (:file "classes")
	       (:file "attachments")
	       (:file "xhtml")
	       (:file "sendmail")))
