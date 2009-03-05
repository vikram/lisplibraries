;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-
;;;
;;;; Copyright (c) 2003,2004 David Lichteblau <david@lichteblau.com>

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cxml-types)

(defun do-not-constant-fold-me (x) x)

#-(or sbcl allegro)
(defun float< (a b)
  (cond
    ((and (numberp a) (numberp b))
     (< a b))
    ((member a '(single-float-nan double-float-nan))
     nil)
    ((member b '(single-float-nan double-float-nan))
     nil)
    ((member a '(single-float-negative-infinity
		 double-float-negative-infinity))
     (not (member b '(single-float-negative-infinity
		      double-float-negative-infinity))))
    ((member b '(single-float-positive-infinity
		 double-float-positive-infinity))
     (not (member a '(single-float-positive-infinity
		      double-float-positive-infinity))))
    (t
     nil)))

#-(or sbcl allegro)
(defun float= (a b)
  (cond
    ((and (numberp a) (numberp b))
     (= a b))
    ((eq a b)
     t)
    ((member a '(single-float-nan double-float-nan))
     (member b '(single-float-nan double-float-nan)))
    ((member b '(single-float-nan double-float-nan))
     nil)
    (t
     nil)))

(defparameter single-float-positive-infinity
  #+sbcl sb-ext:single-float-positive-infinity
  #+allegro excl::*infinity-single*
  #-(or sbcl allegro) 'single-float-positive-infinity)

(defparameter single-float-negative-infinity
  #+sbcl sb-ext:single-float-negative-infinity
  #+allegro excl::*negative-infinity-single*
  #-(or sbcl allegro) 'single-float-negative-infinity)

(defparameter double-float-positive-infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #+allegro excl::*infinity-double*
  #-(or sbcl allegro) 'double-float-positive-infinity)

(defparameter double-float-negative-infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #+allegro excl::*negative-infinity-double*
  #-(or sbcl allegro) 'double-float-negative-infinity)

(defparameter double-float-nan
  #+sbcl (let ((orig (sb-int:get-floating-point-modes)))
	   (unwind-protect
		(progn
		  (sb-int:set-floating-point-modes :traps nil)
		  (/ 0.0d0 (do-not-constant-fold-me 0.0d0)))
	     (apply #'sb-int:set-floating-point-modes orig)))
  #+allegro excl::*nan-double*
  #-(or sbcl allegro) 'double-float-nan)

(defparameter single-float-nan
  #+sbcl (let ((orig (sb-int:get-floating-point-modes)))
	   (unwind-protect
		(progn
		  (sb-int:set-floating-point-modes :traps nil)
		  (/ 0.0f0 (do-not-constant-fold-me 0.0f0)))
	     (apply #'sb-int:set-floating-point-modes orig)))
  #+allegro excl::*nan-single*
  #-(or sbcl allegro) 'single-float-nan)
