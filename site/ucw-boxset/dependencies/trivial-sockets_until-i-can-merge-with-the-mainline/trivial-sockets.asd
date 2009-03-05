;;; -*- Lisp -*-
(defpackage #:trivial-sockets-system  (:use #:asdf #:cl))
(in-package #:trivial-sockets-system )

(defsystem trivial-sockets
    :version "0.3"
    :components ((:file "defpackage")
		 (:file "errors"  :depends-on ("defpackage"))
		 (:file 
		  #+sbcl "sbcl" 
		  #+cmu "cmucl"
		  #+clisp "clisp"
		  #+acl-socket "allegro"
		  #+openmcl "openmcl"
		  #+lispworks "lispworks"
		  #+armedbear "abcl"
		  :depends-on ("defpackage"))
		 (:file "server" :depends-on ("defpackage"))
		 ))

