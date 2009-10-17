;;; -*- package: ASDF; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  wilbur.asd
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The contents of this file are subject to the NOKOS License Version 1.0a (the
;;;   "License"); you may not use this file except in compliance with the License. 
;;;
;;;   Software distributed under the License is distributed on an "AS IS" basis, WITHOUT
;;;   WARRANTY OF ANY KIND, either express or implied. See the License for the specific
;;;   language governing rights and limitations under the License. 
;;;
;;;   The Original Software is 
;;;     WILBUR2: Nokia Semantic Web Toolkit for CLOS
;;;
;;;   Copyright (c) 2001-2005 Nokia and others. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Version: $Id: wilbur2-file-header.lisp,v 1.1 2004/08/10 16:24:46 ora Exp $
;;;
;;;   Purpose: System definition(s) for Wilbur2
;;;
;;;   We no longer support either The CMU Defsystem (by Mark Kantrowitz) nor Franz,
;;;   Inc.'s defsystem (as shipped with Allegro Common Lisp). Instead, after a lot of
;;;   "soul-searching" we -- perhaps a little reluctantly -- have decided to go with
;;;   ASDF. It seems to have become the norm.
;;;
;;;   Wilbur relies on the logical pathname host "wilbur". Here's a sample of how to set
;;;   up the pathname translations on a Unix-style system:
;;;
;;;     (("base;**;*.*"  "/Users/ora/Wilbur/**/*.*") ; this line is the example part
;;;      ("nox;*.*"      "wilbur:base;src;nox;*.*")
;;;      ("core;*.*"     "wilbur:base;src;core;*.*")
;;;      ("goodies;*.*"  "wilbur:base;src;goodies;*.*")
;;;      ("doc;*.*"      "wilbur:base;doc;*.*")
;;;      ("schemata;*.*" "wilbur:base;schemata;*.*"))
;;;
;;;   It is not always easy to understand how the Common Lisp logical pathname
;;;   translations work. Please check the translations (using TRANSLATE-LOGICAL-PATHNAME)
;;;   before assuming that there are bugs in Wilbur. :-)
;;;


(in-package "ASDF")


;;; --------------------------------------------------------------------------------------
;;;
;;;   COMPATIBILITY STUFF
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; OK, this is a hack, but here goes anyway...
  (when (find-package "UFFI")
    (pushnew :uffi *features*)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   ASDF SYSTEM DEFINITION FOR WILBUR2
;;;

(defsystem :wilbur
    :name "wilbur"
    :author "Ora Lassila mailto:ora.lassila@nokia.com"
    :version "2"
    :licence "NOKOS 1.0a"
    :description "WILBUR2: Nokia's Semantic Web Toolkit for CLOS"
    :components ((:module :nox
		  :components ((:file "nox-package")
			       (:file "core-constants" :depends-on ("nox-package"))
			       (:file "xml-util" :depends-on ("core-constants"))
			       (:file "xml-parser" :depends-on ("xml-util"))))
		 (:module :core
		  :components ((:file "wilbur-package")
			       (:file "data" :depends-on ("wilbur-package"))
			       (:file "literal" :depends-on ("data" "wilbur-package"))
			       (:file "rdf-parser" :depends-on ("data" "literal"))
			       (:file "http" :depends-on ("wilbur-package"))
			       (:file "data-sources" :depends-on ("data" "http"))
			       (:file "wilbur-ql" :depends-on ("data"))
			       #+:junk
			       (:file "rdf-reasoner" :depends-on ("wilbur-ql")))
		  :depends-on (:nox))
		 (:module :goodies
		  :components (#+:junk
			       (:file "rdf-inspector")
			       (:file "processes")
			       (:file "index-and-match" :depends-on ("processes"))
			       #+:junk
			       (:file "ivanhoe")
			       #+:junk
			       (:file "daml-parser"))
		  :depends-on (:nox :core))))
