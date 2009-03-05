;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; xmls-valid.lisp: System Definition
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


(defpackage :xmls-tools-system (:use #:asdf #:cl))
(in-package :xmls-tools-system)

(defsystem "xmls-tools"
  :name "xmls-tools"
  :version "0.2.0"
  :author "Robert Marlow <bobstopper@bobturf.org>"
  :maintainer "Robert Marlow <bobstopper@bobturf.org>"
  :depends-on (:xmls)
  :serial t
  :components ((:file "package")
	       (:file "utilities")
	       (:file "xmls-valid")
	       (:file "parser")
	       (:file "xhtml2text")))

