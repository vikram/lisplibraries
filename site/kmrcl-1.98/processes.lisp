;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: modlisp -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          processes.lisp
;;;; Purpose:       Multiprocessing functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  June 2003
;;;;
;;;; $Id$
;;;; *************************************************************************

(in-package #:kmrcl)


(defun make-process (name func)
  #+allegro (mp:process-run-function name func)
  #+cmu (mp:make-process func :name name)
  #+lispworks (mp:process-run-function name nil func)
  #+sb-thread (sb-thread:make-thread func :name name)
  #+openmcl (ccl:process-run-function name func)
  #-(or allegro cmu lispworks sb-thread openmcl) (funcall func)
  )

(defun destroy-process (process)
  #+cmu (mp:destroy-process process)
  #+allegro (mp:process-kill process)
  #+sb-thread (sb-thread:destroy-thread process)
  #+lispworks (mp:process-kill process)
  #+openmcl (ccl:process-kill process)
  )

(defun make-lock (name)
  #+allegro (mp:make-process-lock :name name)
  #+cmu (mp:make-lock name)
  #+lispworks (mp:make-lock :name name)
  #+sb-thread (sb-thread:make-mutex :name name)
  #+openmcl (ccl:make-lock name)
  )

(defmacro with-lock-held ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock) ,@body)
  #+cmu
  `(mp:with-lock-held (,lock) ,@body)
  #+lispworks
  `(mp:with-lock (,lock) ,@body)
  #+sb-thread
  `(sb-thread:with-recursive-lock (,lock) ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock) ,@body)
  #-(or allegro cmu lispworks sb-thread openmcl)
  `(progn ,@body)
  )


(defmacro with-timeout ((seconds) &body body)
  #+allegro
  `(mp:with-timeout (,seconds) ,@body)
  #+cmu
  `(mp:with-timeout (,seconds) ,@body)
  #+sb-thread
  `(sb-ext:with-timeout ,seconds ,@body)
  #+openmcl
  `(ccl:process-wait-with-timeout "waiting"
                                 (* ,seconds ccl:*ticks-per-second*)
                                 #'(lambda ()
                                     ,@body) nil)
  #-(or allegro cmu sb-thread openmcl)
  `(progn ,@body)
  )

(defun process-sleep (n)
  #+allegro (mp:process-sleep n)
  #-allegro (sleep n))

