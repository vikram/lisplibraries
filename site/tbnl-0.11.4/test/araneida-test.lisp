;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TBNL-ARANEIDA-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/test/araneida-test.lisp,v 1.9 2006/01/03 18:37:55 edi Exp $

;;; Copyright (c) 2005, Bob Hutchison.  All rights reserved.

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

;;; This is based on the example.lisp distributed with Araneida 0.9.

(in-package :tbnl-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; this is for using Araneida behind a reverse-proxying Apache (or
  ;; other HTTP proxy) server - you may need to alter the
  ;; configuration for your local setup
  ;;
  ;; (pushnew :araneida-uses-proxy *features*)

  ;; if you have a threaded SBCL, and want to, you can use the (new,
  ;; whizzy) thread-based server instead of the (older, better tested)
  ;; SERVE-EVENT thing
  #+:araneida-threads (pushnew :araneida-uses-threads *features*))

(defvar *demo-port* 8000)
(defvar *fwd-port* 9024)
(defvar *demo-url*
  (araneida:make-url :scheme "http" :host "localhost" :port *demo-port*))

#-:araneida-uses-proxy
(defvar *araneida-listener*
  (make-instance #+:araneida-uses-threads 'araneida:threaded-http-listener
		 #-:araneida-uses-threads 'araneida:serve-event-http-listener
		 :port *demo-port*))

#+:araneida-uses-proxy
(defvar *araneida-listener*
  (let ((fwd-url (araneida:copy-url *demo-url*)))
    (setf (araneida:url-port fwd-url) *fwd-port*)
    (make-instance #+:araneida-uses-threads 'araneida:threaded-reverse-proxy-listener
		   #-:araneida-uses-threads 'araneida:serve-event-reverse-proxy-listener
		   :translations
		   `((,(araneida:urlstring *demo-url*) ,(araneida:urlstring fwd-url)))
		   :address #(0 0 0 0)
		   :port (araneida:url-port fwd-url))))

(defun setup-tbnl-araneida ()
  "Setup Araneida so it can handle requests for TBNL."
  (let ((tbnl-araneida (make-instance 'tbnl:tbnl-araneida-handler
                                      :port *demo-port* :fwd-port *fwd-port*)))
    (araneida:install-handler (araneida:http-listener-handler *araneida-listener*)
                              tbnl-araneida 
                              (araneida:urlstring (araneida:merge-url *demo-url* "/tbnl"))
                              nil)))

(defun stop-araneida-listener ()
  (when (araneida::http-listener-threads *araneida-listener*)
    (araneida:stop-listening *araneida-listener*)))

(defun start-araneida-listener ()
  (setup-tbnl-araneida)
  (araneida:start-listening *araneida-listener*))