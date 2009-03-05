;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          repl.lisp
;;;; Purpose:       A repl server
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: repl.lisp 8573 2004-01-29 23:30:50Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

(defconstant +default-repl-server-port+ 4000)

(defclass repl ()
  ((listener :initarg :listener :accessor listener
	     :initform nil)))

(defun make-repl (&key (port +default-repl-server-port+)
		       announce user-checker remote-host-checker)
  (make-instance 'listener 
    :port port
    :base-name "repl"			 
    :function 'repl-worker
    :function-args (list user-checker announce)
    :format :text
    :wait nil
    :remote-host-checker remote-host-checker
    :catch-errors nil))

(defun init/repl (repl state)
  (init/listener repl state))


(defun repl-worker (conn user-checker announce)
  (when announce
    (format conn "~A~%" announce)
    (force-output conn))
  (when user-checker
    (let (login password)
      (format conn "login: ")
      (finish-output conn)
      (setq login (read-socket-line conn))
      (format conn "password: ")
      (finish-output conn)
      (setq password (read-socket-line conn))
      (unless (funcall user-checker login password)
	(format conn "Invalid login~%")
	(finish-output conn)
	(return-from repl-worker))))
  #+allegro
  (tpl::start-interactive-top-level
   conn
   #'tpl::top-level-read-eval-print-loop
   nil)
  #-allegro
  (repl-on-stream conn)
  )

(defun read-socket-line (stream)
  (string-right-trim-one-char #\return
			      (read-line stream nil nil)))

(defun print-prompt (stream)
  (format stream "~&~A> " (package-name *package*))
  (force-output stream))

(defun repl-on-stream (stream)
  (let ((*standard-input* stream)
	(*standard-output* stream)
	(*terminal-io* stream)
	(*debug-io* stream))
    #|
    #+sbcl
    (if (and (find-package 'sb-aclrepl)
	     (fboundp (intern "REPL-FUN" "SB-ACLREPL")))
	(sb-aclrepl::repl-fun)
	(%repl))
    #-sbcl
    |#
    (%repl)))

(defun %repl ()
  (loop
    (print-prompt *standard-output*)
    (let ((form (read *standard-input*)))
      (format *standard-output* "~&~S~%" (eval form)))))
  
