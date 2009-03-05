;;;;
;;;; ACL-COMPAT - EXCL
;;;;
;;;; This is a modified version of Chris Doubles ACL excl wrapper library
;;;; As stated in the changelogs of his original this file includes the 
;;;; IF* macro placed in the public domain by John Foderaro. 
;;;; See: http://www.franz.com/~jkf/ifstar.txt
;;;;

;;;; This file was made by Rudi Schlatte to gather
;;;; not-implementation-specific parts of acl-compat in one place.

;;;; This is the header of Chris Doubles original file. (but without Changelog)
;;;;
;;;; ACL excl wrapper library for Corman Lisp - Version 1.1
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
;;;; 
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source 
;;;;    distribution.
;;;;

;;;; -*- mode: lisp -*-
;;;;
;;;; Package definitions for acl-compat.
;;;;
;;;; Package names follow their Allegro CL counterparts -- for an ACL
;;;; package foo, acl-compat defines a package acl-compat.foo
;;;;
;;;; Some packages have nicknames, which were used as package names by
;;;; previous versions of paserve and acl-compat.  The nicknames are
;;;; deprecated, but are kept for the benefit of people using
;;;; acl-compat in other projects.  New projects should use the
;;;; package names starting with "acl-compat.".
;;;;

(in-package :common-lisp-user)

;;; general
(defpackage :acl-compat.excl
  (:use #:common-lisp
        #+cmu #:ext
        #+clisp #:ext
        #+sbcl #:sb-ext #+sbcl #:sb-gray
        #+(or allegro cormanlisp) :excl
        #+mcl :ccl
        )
  #+lispworks (:import-from :common-lisp #:fixnump)
  #+sbcl (:import-from :sb-int #:fixnump)
  #+sbcl (:import-from :sb-ext #:without-package-locks)
  #+cmu (:import-from :ext #:without-package-locks)
  #+allegro (:shadowing-import-from :excl #:filesys-size
	    #:filesys-write-date #:intern* #:filesys-type #:atomically #:fast)
  (:export
   #:if*
   #:*initial-terminal-io*
   #:*cl-default-special-bindings*
   #:filesys-size
   #:filesys-write-date
   #:stream-input-fn
   #:match-regexp
   #:compile-regexp
   #:*current-case-mode*
   #:intern*
   #:filesys-type
   #:errorset
   #:atomically
   #:fast
   #:without-package-locks
   #:fixnump
   #+(or lispworks mcl) #:socket-error
   #+(or allegro lispworks mcl) #:run-shell-command
   #+(or allegro mcl) #:fasl-read
   #+(or allegro mcl) #:fasl-write
   #+(or allegro cmu scl mcl lispworks) #:string-to-octets
   #+(or allegro cmu scl mcl lispworks) #:write-vector
   ))



