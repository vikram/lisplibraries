;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10; -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/port-clisp.lisp,v 1.5 2008/04/08 14:39:17 edi Exp $

;;; Copyright (c) 2008, Geo Carncross <geocar@gmail.com>.
;;; Copyright (c) 2007-2008, Dr. Edmund Weitz.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; make sure code for sockets is loaded
  (require :sockets)) 
  
(in-package :hunchentoot-mp)

(defun make-lock (name)
  "See ECL documentation for MP:MAKE-LOCK."
  #+threads (mp:make-lock :name name) #-threads nil)

(defmacro with-lock ((lock) &body body)
  "See ECL documentation for MP:WITH-LOCK."
  #+threads `(mp:with-lock (,lock) ,@body)
  #-threads `(progn ,@body))

(defun process-run-function (name function &rest args)
  "See ECL documentation for MP:MAKE-PROCESS."
  (declare (ignorable name))
  #+threads (let ((thread (mp:make-process :name name)))
	      (mp:process-preset thread #'(lambda () (apply function args)))
	      (mp:process-enable thread)
	      thread)
  #-threads (progn (apply function args) nil))

(defun process-kill (process)
  "See ECL documentation for MP:PROCESS-KILL."
  #+threads (mp:process-kill process)
  #-threads nil)

#+threads (define-symbol-macro *current-process* mp:*current-process*)
#-threads (define-symbol-macro *current-process* nil)

(in-package :hunchentoot)

(defvar *incf-mutex* #+threads (mp:make-lock :name "incf-mutex") #-threads nil
  "The mutex used for ATOMIC-INCF.")

(defmacro atomic-incf (place &optional (delta 1))
  "Like INCF but protected by a mutex, so other threads can't
interfer."
  #+threads `(mp:with-lock (*incf-mutex*) (incf ,place ,delta))
  #-threads `(incf ,place ,delta))

(defun process-allow-scheduling ()
  "Used to simulate a function like PROCESS-ALLOW-SCHEDULING
which can be found in most other Lisps."
  (sleep .1))

(defun resolve-hostname (name)
  "Converts from different types to represent an IP address to
the canonical representation which is an array with four
integers."
  (typecase name
    (null #(0 0 0 0))
    (string (car (sb-bsd-sockets:host-ent-addresses
                  (sb-bsd-sockets:get-host-by-name name))))
    (integer (make-array 4 :initial-contents (list (ash name -24)
                                                   (logand (ash name -16) #xFF)
                                                   (logand (ash name -8) #xFF)
                                                   (logand name #xFF))))
    (t name)))

(defun start-up-server (&key service address process-name announce function &allow-other-keys)
  "Tries to \(partly) emulate LispWorks' COMM:START-UP-SERVER.  See
<http://www.lispworks.com/documentation/lw50/LWRM/html/lwref-56.htm>
for more info."
  (let (done)
    (flet ((open-socket-and-accept ()
             (handler-bind ((error (lambda (condition)
                                     (funcall announce nil condition)
                                     (setq done condition)
                                     (return-from open-socket-and-accept))))
               (let (socket)
                 (unwind-protect
                     (progn
                       (setf socket (make-instance 'sb-bsd-sockets:inet-socket
                                       :type :stream
                                       :protocol :tcp)
                             (sb-bsd-sockets:sockopt-reuse-address socket) t)
                       (sb-bsd-sockets:socket-bind socket (resolve-hostname address) service)
                       (sb-bsd-sockets:socket-listen socket 5)
                       (funcall announce socket)
                       (setq done socket)
                       (loop (funcall function (sb-bsd-sockets:socket-accept socket))))
                   (when socket
                     (cl:ignore-errors
                       (sb-bsd-sockets:socket-close socket))))))))
      (let ((listener-thread (process-run-function process-name #'open-socket-and-accept)))
        (loop until done do (sleep .1))
        (typecase done
          (sb-bsd-sockets:inet-socket listener-thread)
          (t (values nil done)))))))

(defun format-address (address)
  "Converts an array of four integers denoting an IP address into
the corresponding string representation."
  (format nil "~{~A~^.~}" (coerce address 'list)))

(defun make-socket-stream (socket read-timeout write-timeout)
  "Accepts a socket `handle' SOCKET and creates and returns a
corresponding stream, setting its read and write timeout if
applicable.  Returns three other values - the address the request
arrived at, and the address and port of the remote host."
  (declare (ignore write-timeout))
  (let ((local-host (sb-bsd-sockets:socket-name socket)))
    (multiple-value-bind (remote-host remote-port)
        (sb-bsd-sockets:socket-peername socket)
      (values (sb-bsd-sockets:socket-make-stream socket
                                                 :input t
                                                 :output t
                                                 :element-type 'octet
                                                 :timeout read-timeout
                                                 :buffering :full)
              (format-address local-host)
              (format-address remote-host)
              remote-port))))

(defun get-backtrace (error)
  "This is the function that is used internally by Hunchentoot to
show or log backtraces.  It accepts a condition object ERROR and
returns a string with the corresponding backtrace."
  (declare (ignore error))
  (let* ((si::*tpl-commands* si::tpl-commands)
         (si::*ihs-top* (si::ihs-top)) ;'call-with-debugging-environment))
	 (si::*ihs-current* si::*ihs-top*)
	 (si::*frs-base* (or (si::sch-frs-base si::*frs-top* si::*ihs-base*) (1+ (si::frs-top))))
	 (si::*frs-top* (si::frs-top))
	 (si::*read-suppress* nil)
	 (si::*tpl-level* (1+ si::*tpl-level*))
	 (backtrace (loop for ihs from si::*ihs-base* below si::*ihs-top*
			  collect (list (si::ihs-fun ihs)
					(si::ihs-env ihs)
					nil))))
    (loop for f from si::*frs-base* until si::*frs-top*
	  do (let ((i (- (si::frs-ihs f) si::*ihs-base* 1)))
	       (when (plusp i)
		 (let* ((x (elt backtrace i))
			(name (si::frs-tag f)))
		   (unless (fixnump name)
		     (push name (third x)))))))
    (setf backtrace (nreverse backtrace))
    (with-output-to-string (stream)
      (dolist (i backtrace)
	(format stream "~A~%" (car i))))))
