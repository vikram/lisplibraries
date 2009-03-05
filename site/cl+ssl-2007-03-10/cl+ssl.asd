;;; -*- mode: lisp -*-
;;;
;;; Copyright (C) 2001, 2003  Eric Marsden
;;; Copyright (C) 2005  David Lichteblau
;;; "the conditions and ENSURE-SSL-FUNCALL are by Jochen Schmidt."
;;;
;;; See LICENSE for details.

(defpackage :cl+ssl-system
  (:use :cl :asdf))

(in-package :cl+ssl-system)

(defsystem :cl+ssl
  :depends-on (:cffi :trivial-gray-streams :flexi-streams)
  :serial t
  :components
   ((:file "package")
    (:file "reload")
    (:file "conditions")
    (:file "ffi")
    (:file "streams")
    (:file "bio")))

(defparameter *libssl-pathname*
   #+(or :win32 :mswindows) "libssl32.dll"
   #+(and :openmcl :darwinppc-target) "/usr/lib/libssl.dylib"
   #-(or :win32 :mswindows :darwinppc-target) "/usr/lib/libssl.so.0.9.8")
