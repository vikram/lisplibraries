;;;
;;; This file contains property of Mu Aps.
;;; Copyright (c) 2005.  All rights reserved.
;;;

(defpackage :muproc-test
  (:use :common-lisp :cl-muproc :rtest :cl-muproc.supervisor :cl-muproc.compat)
  (:export
   :test-it))

(defpackage :muproc-test.gensrv1
  (:use :cl :cl-muproc :cl-muproc.generic-server)
  (:export
   :start
   :stop
   ))

(defpackage :muproc-test.super1
  (:use :cl :cl-muproc :cl-muproc.supervisor)
  (:export
   #:start
   ))

(defpackage :muproc-test.super2
  (:use :cl :cl-muproc :cl-muproc.supervisor)
  (:export
   #:start
   ))


;eof