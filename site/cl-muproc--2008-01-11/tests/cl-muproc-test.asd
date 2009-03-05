;;; -*- lisp -*-
;;;
;;; This file contains property of Mu Aps.
;;; Copyright (c) 2005.  All rights reserved.
;;;

(in-package #:cl-user)
(defpackage #:dk.mu.muproc-test.system (:use #:asdf #:cl))
(in-package #:dk.mu.muproc-test.system)

(defsystem cl-muproc-test
  :components
  ((:file "muproc-test-packages")
   (:file "muproc-test-util" :depends-on ("muproc-test-packages"))
   (:file "muproc-test" :depends-on ("muproc-test-util"))
   (:file "gensrv1" :depends-on ("muproc-test-util"))
   (:file "gensrv1-test" :depends-on ("gensrv1" "muproc-test-util"))
   ;(:file "super1" :depends-on ("muproc-test-util"))
   ;(:file "super2" :depends-on ("muproc-test-util"))
   )
  :depends-on (:cl-muproc :rt)
  )

;eof