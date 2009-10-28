;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tf-clisp.lisp --- CLISP trivial-features implementation.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package :cl-user)

;;;; Endianness

(pushnew (ffi:with-foreign-object (ptr '(ffi:c-array ffi:uint8 2))
           (setf (ffi:memory-as ptr 'ffi:uint16 0) #xfeff)
           (ecase (ffi:memory-as ptr 'ffi:uint8 0)
             (#xfe (intern (symbol-name '#:big-endian) '#:keyword))
             (#xff (intern (symbol-name '#:little-endian) '#:keyword))))
         *features*)

;;;; OS

;;; CLISP already exports :UNIX.

#+win32 (pushnew :windows *features*)

#-win32
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (read-from-string
            (format nil ":~(~A~)" (posix:uname-sysname (posix:uname))))
           *features*))

#+(or darwin freebsd netbsd openbsd)
(pushnew :bsd *features*)

;;;; CPU

;;; FIXME: not complete
(pushnew (intern
          (symbol-name
           #+pc386 '#:x86
           #-pc386
           (cond
             ((string= (machine-type) "X86_64") '#:x86-64)
             ((string= (machine-type) "POWER MACINTOSH") '#:ppc)))
          '#:keyword)
         *features*)