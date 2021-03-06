; f2cl0.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; edit here so that the root of the f2cl directory tree is appropriate
; for your installation and the filename extension is valid
;;#+(or sparc sun4)(defvar *f2cl_dir* "/d/sen/f2cl/wrk/")
;;#+vms (defvar *f2cl_dir* "d:[sen.f2cl.wrk]")
;;(defvar *ext* 
;;          #+clisp ".fas" #+allegro ".fasl" #+vms ".fas" #+lucid ".sbin")
;;
;;(defun load-f2cl (x) 
;;(load (concatenate 'string *f2cl_dir* x *ext*) :print nil :verbose t))
;;
;;(load-f2cl "f2cl1" )
;;(load-f2cl "f2cl2" )
;;(load-f2cl "f2cl3" )
;;(load-f2cl "f2cl4" )
;;(load-f2cl "f2cl5" )
;;(load-f2cl "f2cl6" )
;;(load-f2cl "f2cl7" )
;;(load-f2cl "macros" )
;;
;;(format t "~&The f2cl software has been loaded.~%")

(in-package :common-lisp-user)

(defpackage :f2cl-lib
  (:use :cl)
  (:documentation "The package holding all symbols used by the fortran to lisp library.")
  (:nicknames :fortran-to-lisp-library)
  (:export
   ;; constants
   #:%false% #:%true%
   ;; user-settable runtime options
   #:*check-array-bounds*
   ;; types
   #:integer4 #:integer2 #:integer1 #:real8 #:real4 #:complex8 #:complex16
   #:array-double-float #:array-single-float #:array-integer4 #:array-strings
   #:logical
   ;; macros
   #:fref #:fset #:with-array-data #:with-multi-array-data
   #:f2cl-init-string #:fref-string #:fset-string #:f2cl-set-string
   #:f2cl-// #:fstring-/= #:fstring-= #:fstring-> #:fstring->= #:fstring-< #:fstring-<=
   #:fortran_comment #:fdo #:f2cl/ #:arithmetic-if #:computed-goto
   #:assigned-goto
   #:fformat
   #:data-implied-do
   #:int-add #:int-sub #:int-mul
   ;; utilities
   #:array-slice #:array-initialize
   ;; intrinsic functions
   #:abs #:acos #:aimag #:aint #:alog #:alog10 #:amax0 #:amax1
   #:amin1 #:amod #:anint #:asin #:atan #:atan2
   #:cabs #:cexp #:fchar #:clog #:cmplx #:conjg #:ccos
   #:csin #:csqrt #:dabs #:dacos #:dasin
   #:datan #:datan2 #:dble #:dcos #:dcosh #:dexp #:dim
   #:dint #:dlog #:dlog10 #:dmax1 #:dmin1 #:dmod
   #:dnint #:dprod #:dsign #:dsin #:dsinh #:dsqrt #:dtan
   #:dtanh #:ffloat #:iabs #:ichar #:idim #:idint
   #:idnint #:ifix #:index #:int #:isign #:le #:len
   #:lge #:lgt #:flog #:log10 #:lt #:max #:max0
   #:max1 #:min0 #:min1 #:nint #:freal
   #:sign #:sngl #:fsqrt
   ;; other functions
   #:d1mach #:r1mach #:i1mach
   ))

(defpackage :fortran-to-lisp
    (:use :cl)
  (:documentation "the package holding all symbols need by the fortran to lisp converter")
  (:nicknames :f2cl)
  (:export
   ;; main routines
   #:f2cl
   #:f2cl-compile
   ))

;;;-------------------------------------------------------------------------
;;; end of f2cl0.l
;;;
;;; $Id: f2cl0.l,v 1.16 2003/11/13 05:37:31 rtoy Exp $
;;; $Log: f2cl0.l,v $
;;; Revision 1.16  2003/11/13 05:37:31  rtoy
;;; Export WITH-MULTI-ARRAY-DATA.
;;;
;;; Revision 1.15  2002/09/13 17:50:18  rtoy
;;; From Douglas Crosher:
;;;
;;; o Make this work with lower-case Lisps
;;; o Fix a few typos
;;; o Make a safer fortran reader.
;;;
;;; Revision 1.14  2002/04/18 13:03:29  rtoy
;;; Forgot to export I1MACH.
;;;
;;; Revision 1.13  2002/03/11 16:40:21  rtoy
;;; Export INT-ADD, INT-SUB, INT-MUL.
;;;
;;; Revision 1.12  2002/02/17 15:50:17  rtoy
;;; Export with-array-data.
;;;
;;; Revision 1.11  2002/02/10 03:41:53  rtoy
;;; Export ARRAY-STRINGS type.
;;;
;;; Revision 1.10  2002/01/13 16:24:24  rtoy
;;; All of the exported symbols in macros.l have been moved from the F2CL
;;; package to the F2CL-LIB package.
;;;
;;; Revision 1.9  2002/01/05 18:52:12  rtoy
;;; Add in-package.
;;;
;;; Revision 1.8  2001/04/26 17:49:50  rtoy
;;; Export new functions D1MACH and R1MACH
;;;
;;; Revision 1.7  2001/02/26 15:38:22  rtoy
;;; Move *check-array-bounds* from f2cl1.l to macros.l since the generated
;;; code refers to it.  Export this variable too.
;;;
;;; Revision 1.6  2000/09/01 13:51:20  rtoy
;;; AMAX1 and DIM were repeated.
;;;
;;; Revision 1.5  2000/08/05 19:06:34  rtoy
;;; Export F2CL-COMPILE.
;;;
;;; Revision 1.4  2000/07/28 22:07:44  rtoy
;;; It's FORTRAN, not FORTAN!
;;;
;;; Revision 1.3  2000/07/28 16:56:48  rtoy
;;; f2cl0.l isn't the (unused) f2cl loader anymore.  Use it to define the
;;; package used by f2cl.
;;;
;;; Revision 1.2  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-------------------------------------------------------------------------
