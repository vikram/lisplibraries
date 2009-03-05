;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: modlisp -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sockets.lisp
;;;; Purpose:       Socket functions
;;;; Programmer:    Kevin M. Rosenberg with excerpts from portableaserve
;;;; Date Started:  Jun 2003
;;;;
;;;; $Id: sockets.lisp 11421 2006-12-31 18:42:10Z kevin $
;;;; *************************************************************************

(in-package #:kmrcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require :sb-bsd-sockets)
  #+lispworks (require "comm")
  #+allegro (require :socket))


#+sbcl
(defun listen-to-inet-port (&key (port 0) (kind :stream) (reuse nil))
  "Create, bind and listen to an inet socket on *:PORT.
setsockopt SO_REUSEADDR if :reuse is not nil"
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (if reuse
        (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
    (sb-bsd-sockets:socket-bind
     socket (sb-bsd-sockets:make-inet-address "0.0.0.0") port)
    (sb-bsd-sockets:socket-listen socket 15)
    socket))

(defun create-inet-listener (port &key (format :text) (reuse-address t))
  #+cmu (declare (ignore format reuse-address))
  #+cmu (ext:create-inet-listener port)
  #+allegro
  (socket:make-socket :connect :passive :local-port port :format format
		      :address-family
		      (if (stringp port)
			  :file
			(if (or (null port) (integerp port))
			    :internet
			  (error "illegal value for port: ~s" port)))
		      :reuse-address reuse-address)
  #+sbcl (declare (ignore format))
  #+sbcl (listen-to-inet-port :port port :reuse reuse-address)
  #+clisp (declare (ignore format reuse-address))
  #+clisp (ext:socket-server port)
  #+openmcl
  (declare (ignore format))
  #+openmcl
  (ccl:make-socket :connect :passive :local-port port
		   :reuse-address reuse-address)
  #-(or allegro clisp cmu sbcl openmcl)
  (warn "create-inet-listener not supported on this implementation")
  )

(defun make-fd-stream (socket &key input output element-type)
  #+cmu
  (sys:make-fd-stream socket :input input :output output
		      :element-type element-type)
  #+sbcl
  (sb-bsd-sockets:socket-make-stream socket :input input :output output
				     :element-type element-type)
  #-(or cmu sbcl) (declare (ignore input output element-type))
  #-(or cmu sbcl) socket
  )


(defun accept-tcp-connection (listener)
  "Returns (VALUES stream socket)"
  #+allegro
  (let ((sock (socket:accept-connection listener)))
    (values sock sock))
  #+clisp
  (let ((sock (ext:socket-accept listener)))
    (values sock sock))
  #+cmu
  (progn
    (mp:process-wait-until-fd-usable listener :input)
    (let ((sock (nth-value 0 (ext:accept-tcp-connection listener))))
      (values (sys:make-fd-stream sock :input t :output t) sock)))
  #+sbcl
  (when (sb-sys:wait-until-fd-usable
	 (sb-bsd-sockets:socket-file-descriptor listener) :input)
    (let ((sock (sb-bsd-sockets:socket-accept listener)))
      (values
       (sb-bsd-sockets:socket-make-stream
	sock :element-type :default :input t :output t)
       sock)))
  #+openmcl
  (let ((sock (ccl:accept-connection listener :wait t)))
    (values sock sock))
  #-(or allegro clisp cmu sbcl openmcl)
  (warn "accept-tcp-connection not supported on this implementation")
  )


(defmacro errorset (form display)
  `(handler-case
    ,form
    (error (e)
     (declare (ignorable e))
     (when ,display
       (format t "~&Error: ~A~%" e)))))

(defun close-passive-socket (socket)
  #+allegro (close socket)
  #+clisp (ext:socket-server-close socket)
  #+cmu (unix:unix-close socket)
  #+sbcl (sb-unix:unix-close
	  (sb-bsd-sockets:socket-file-descriptor socket))
  #+openmcl (close socket)
  #-(or allegro clisp cmu sbcl openmcl)
  (warn "close-passive-socket not supported on this implementation")
  )


(defun close-active-socket (socket)
  #+sbcl (sb-bsd-sockets:socket-close socket)
  #-sbcl (close socket))

(defun ipaddr-to-dotted (ipaddr &key values)
  "Convert from 32-bit integer to dotted string."
  (declare (type (unsigned-byte 32) ipaddr))
  (let ((a (logand #xff (ash ipaddr -24)))
	(b (logand #xff (ash ipaddr -16)))
	(c (logand #xff (ash ipaddr -8)))
	(d (logand #xff ipaddr)))
    (if values
	(values a b c d)
      (format nil "~d.~d.~d.~d" a b c d))))

(defun dotted-to-ipaddr (dotted &key (errorp t))
  "Convert from dotted string to 32-bit integer."
  (declare (string dotted))
  (if errorp
      (let ((ll (delimited-string-to-list dotted #\.)))
	(+ (ash (parse-integer (first ll)) 24)
	   (ash (parse-integer (second ll)) 16)
	   (ash (parse-integer (third ll)) 8)
	   (parse-integer (fourth ll))))
    (ignore-errors
      (let ((ll (delimited-string-to-list dotted #\.)))
	(+ (ash (parse-integer (first ll)) 24)
	   (ash (parse-integer (second ll)) 16)
	   (ash (parse-integer (third ll)) 8)
	   (parse-integer (fourth ll)))))))

#+sbcl
(defun ipaddr-to-hostname (ipaddr &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in IPADDR-TO-HOSTNAME not supported."))
  (sb-bsd-sockets:host-ent-name
   (sb-bsd-sockets:get-host-by-address
    (sb-bsd-sockets:make-inet-address ipaddr))))

#+sbcl
(defun lookup-hostname (host &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in LOOKUP-HOSTNAME not supported."))
  (if (stringp host)
      (sb-bsd-sockets:host-ent-address
       (sb-bsd-sockets:get-host-by-name host))
      (dotted-to-ipaddr (ipaddr-to-dotted host))))


(defun make-active-socket (server port)
  "Returns (VALUES STREAM SOCKET)"
  #+allegro
  (let ((sock (socket:make-socket :remote-host server
				  :remote-port port)))
    (values sock sock))
  #+lispworks
  (let ((sock (comm:open-tcp-stream server port)))
    (values sock sock))
  #+sbcl
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket
			     :type :stream
			     :protocol :tcp)))
    (sb-bsd-sockets:socket-connect sock (lookup-hostname server) port)
    (values
     (sb-bsd-sockets:socket-make-stream
      sock :input t :output t :element-type :default)
     sock))
  #+cmu
  (let ((sock (ext:connect-to-inet-socket server port)))
    (values
     (sys:make-fd-stream sock :input t :output t :element-type 'base-char)
     sock))
  #+clisp
  (let ((sock (ext:socket-connect port server)))
    (values sock sock))
  #+openmcl
  (let ((sock (ccl:make-socket :remote-host server :remote-port port )))
    (values sock sock))
  )

(defun ipaddr-array-to-dotted (array)
  (format nil "~{~D~^.~}" (coerce array 'list))
  #+ignore
  (format nil "~D.~D.~D.~D"
	  (aref 0 array) (aref 1 array) (aref 2 array) (array 3 array)))

(defun remote-host (socket)
  #+allegro (socket:ipaddr-to-dotted (socket:remote-host socket))
  #+lispworks (nth-value 0 (comm:get-socket-peer-address socket))
  #+sbcl (ipaddr-array-to-dotted
	  (nth-value 0 (sb-bsd-sockets:socket-peername socket)))
  #+cmu (nth-value 0 (ext:get-peer-host-and-port socket))
  #+clisp (let* ((peer (ext:socket-stream-peer socket t))
                (stop (position #\Space peer)))
           ;; 2.37-2.39 had do-not-resolve-p backwards
           (if stop (subseq peer 0 stop) peer))
  #+openmcl (ccl:remote-host socket)
  )

