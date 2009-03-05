;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: modlisp -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          listener.lisp
;;;; Purpose:       Listener and worker processes
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jun 2003
;;;;
;;;; $Id: listener.lisp 8573 2004-01-29 23:30:50Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002-2003 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

;;; Variables and data structures for Listener

(defvar *listener-count* 0
  "used to name listeners")

(defvar *worker-count* 0
  "used to name workers")

(defvar *active-listeners* nil
    "List of active listeners")

(defclass listener ()
  ((port :initarg :port :accessor port) 
   (function :initarg :function :accessor listener-function
	     :initform nil)
   (function-args :initarg :function-args :accessor function-args
		  :initform nil)
   (process :initarg :process :accessor process :initform nil)
   (socket :initarg :socket :accessor socket :initform nil)
   (workers :initform nil :accessor workers
	    :documentation "list of worker threads")
   (name :initform "" :accessor name :initarg :name)
   (base-name :initform "listener" :accessor base-name :initarg :base-name)
   (wait :initform nil :accessor wait :initarg :wait)
   (timeout :initform nil :accessor timeout :initarg :timeout)
   (number-fixed-workers :initform nil :accessor number-fixed-workers
			 :initarg :number-fixed-workers)
   (catch-errors :initform nil :accessor catch-errors :initarg :catch-errors)
   (remote-host-checker :initform nil :accessor remote-host-checker
			:initarg :remote-host-checker)
   (format :initform :text :accessor listener-format :initarg :format)))

(defclass fixed-worker ()
  ((listener :initarg :listener :accessor listener :initform nil)
   (name :initarg :name :accessor name :initform nil)
   (process :initarg :process :accessor process :initform nil)))

(defclass worker (fixed-worker)
  ((connection :initarg :connection :accessor connection :initform nil)
   (thread-fun :initarg :thread-fun :accessor thread-fun :initform nil)))


(defmethod print-object ((obj listener) s)
  (print-unreadable-object (obj s :type t :identity nil)
    (format s "port ~A" (port obj))))

(defmethod print-object ((obj fixed-worker) s)
  (print-unreadable-object (obj s :type t :identity nil)
    (format s "port ~A" (port (listener obj)))))
  
;; High-level API

(defun init/listener (listener state)
  (check-type listener listener)
  (case state
    (:start
     (when (member listener *active-listeners*)
       (cmsg "~&listener ~A already initialized" listener)
       (return-from init/listener))
     (when (listener-startup listener)
       (push listener *active-listeners*)
       listener))
    (:stop
     (unless (member listener *active-listeners*)
       (cmsg "~&listener ~A is not in active list" listener)
       (return-from init/listener listener))
     (listener-shutdown listener)
     (setq *active-listeners* (remove listener *active-listeners*)))
    (:restart
     (init/listener listener :stop)
     (init/listener listener :start))))

(defun stop-all/listener ()
  (dolist (listener *active-listeners*)
    (ignore-errors
       (init/listener listener :stop))))

(defun listener-startup (listener)
  (handler-case
      (progn
	(setf (name listener) (next-server-name (base-name listener)))
	(make-socket-server listener))
    (error (e)
      (format t "~&Error while trying to start listener on port ~A~&  ~A" 
	      (port listener) e)
      (decf *listener-count*)
      nil)
    (:no-error (res)
      (declare (ignore res))
      listener)))

(defun listener-shutdown (listener)
  (dolist (worker (workers listener))
    (when (and (typep worker 'worker)
	       (connection worker))
      (errorset (close-active-socket
		 (connection worker)) nil)
      (setf (connection worker) nil))
    (when (process worker)
      (errorset (destroy-process (process worker)) nil)
      (setf (process worker) nil)))
  (setf (workers listener) nil)
  (with-slots (process socket) listener
    (when socket
      (errorset (close-passive-socket socket) nil)
      (setf socket nil))
    (when process
      (errorset (destroy-process process) nil)
      (setf process nil))))

;; Low-level functions

(defun next-server-name (base-name)
  (format nil "~D-~A-socket-server" (incf *listener-count*) base-name)) 

(defun next-worker-name (base-name)
  (format nil "~D-~A-worker"  (incf *worker-count*) base-name))

(defun make-socket-server (listener)
  #+lispworks
  (progn
    (setf (process listener)
      (comm:start-up-server :process-name (name listener)
			    :service (port listener) 
			    :function
			    #'(lambda (handle) 
				(lw-worker handle listener)))))
  #-lispworks
  (progn
    (setf (socket listener) (create-inet-listener
			     (port listener)
			     :format (listener-format listener)))
    (if (number-fixed-workers listener)
	(start-fixed-number-of-workers listener)
	(setf (process listener) (make-process
				  (name listener)
				  #'(lambda ()
				      (start-socket-server listener))))))
  listener)


(defmethod initialize-instance :after
    ((self worker) &key listener connection name &allow-other-keys)
  (flet ((do-work ()
	   (apply (listener-function listener)
		  connection
		  (function-args listener))))
    (unless connection
      (error "connection not provided to modlisp-worker"))
    (setf (slot-value self 'listener) listener)
    (setf (slot-value self 'name) name)
    (setf (slot-value self 'connection) connection)
    (setf (slot-value self 'thread-fun)
	  #'(lambda ()
	      (unwind-protect
		   (if (catch-errors listener)
		       (handler-case
			   (if (timeout listener)
			       (with-timeout ((timeout listener))
				 (do-work))
			       (do-work))
			 (error (e)
			   (cmsg "Error ~A [~A]" e name)))
		       (if (timeout listener)
			   (with-timeout ((timeout listener))
			     (do-work))
			   (do-work)))
		(progn
		  (errorset (finish-output connection) nil)
		  (errorset (close-active-socket connection) nil)
		  (cmsg-c :threads "~A ended" name)
		  (setf (workers listener)
			(remove self (workers listener)))))))))

(defun accept-and-check-tcp-connection (listener)
  (multiple-value-bind (conn socket) (accept-tcp-connection (socket listener))
    (when (and (remote-host-checker listener)
	       (not (funcall (remote-host-checker listener)
			     (remote-host socket))))
      (cmsg-c :thread "Deny connection from ~A" (remote-host conn))
      (errorset (close-active-socket conn) nil)
      (setq conn nil))
    conn))

(defun start-socket-server (listener)
  (unwind-protect
      (loop 
       (let ((connection (accept-and-check-tcp-connection listener)))
	 (when connection
	   (if (wait listener)
	       (unwind-protect
		    (apply (listener-function listener)
			   connection
			   (function-args listener))
		 (progn
		   (errorset (finish-output connection) nil)
		   (errorset (close-active-socket connection) nil)))
	       (let ((worker (make-instance 'worker :listener listener
					    :connection connection
					    :name (next-worker-name
						   (base-name listener)))))
		 (setf (process worker)
		       (make-process (name worker) (thread-fun worker)))
		 (push worker (workers listener)))))))
    (errorset (close-passive-socket (socket listener)) nil)))

#+lispworks
(defun lw-worker (handle listener)
  (let ((connection (make-instance 'comm:socket-stream
		      :socket handle
		      :direction :io
		      :element-type 'base-char)))
    (if (wait listener)
	(progn
	  (apply (listener-function listener)
		 connection
		 (function-args listener))
	  (finish-output connection))
	(let ((worker (make-instance 'worker :listener listener
				     :connection connection
				     :name (next-worker-name
					    (base-name listener)))))
	  (setf (process worker)
		(make-process (name worker) (thread-fun worker)))
	  (push worker (workers listener))))))

;; Fixed pool of workers

(defun start-fixed-number-of-workers (listener)
  (dotimes (i (number-fixed-workers listener))
    (let ((name (next-worker-name (base-name listener))))
      (push
       (make-instance 'fixed-worker
		      :name name
		      :listener listener
		      :process
		      (make-process
		       name #'(lambda () (fixed-worker name listener))))
       (workers listener)))))


(defun fixed-worker (name listener)
  (loop 
   (let ((connection (accept-and-check-tcp-connection listener)))
     (when connection
       (flet ((do-work ()
		(apply (listener-function listener)
		       connection
		       (function-args listener))))
	 (unwind-protect
	      (handler-case
		  (if (catch-errors listener)
		      (handler-case
			  (if (timeout listener)
			      (with-timeout ((timeout listener))
				(do-work))
			      (do-work))
			(error (e)
			  (cmsg "Error ~A [~A]" e name)))
		      (if (timeout listener)
			  (with-timeout ((timeout listener))
			    (do-work))
			  (do-work)))
		(error (e)
		  (format t "Error: ~A" e)))
	   (errorset (finish-output connection) nil)
	   (errorset (close connection) nil)))))))
  
