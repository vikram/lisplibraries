;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TBNL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/contrib/create-groups-bind-regex-dispatcher/create-groups-bind-regex-dispatcher.lisp,v 1.1 2005/09/08 10:11:22 edi Exp $

;;; Copyright (c) 2005, Alceste Scalas. All rights reserved.

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

(in-package #:tbnl-contrib)

(defmacro create-groups-bind-regex-dispatcher (regex var-list page-function)
  "Like CREATE-REGEX-DISPATCHER, but call PAGE-FUNCTION with the
variables in VAR-LIST used as additional keyword arguments bound
to the corresponding register groups of REGEX.
Note: VAR-LIST has the same format of its omonymous in
CL-PPCRE:REGISTER-GROUPS-BIND, so you can use the (FN VAR ...)
form if needed."
  `(let ((scanner (cl-ppcre:create-scanner ,regex)))
     (lambda (request)
       (cl-ppcre:register-groups-bind
	   ,var-list
	   (scanner (script-name request))
	 ;; Iff the regex matches, we return a closure that calls the
	 ;; dispatch function with the proper keyword arguments
	 (lambda () (,page-function ,@(build-keyword-list var-list)))))))

(defun build-keyword-list (var-list)
  "Utility function for CREATE-GROUPS-BIND-REGEX-DISPATCHER.
Build a keyword list out of the var-list, ready to be used to
invoke the dispatcher function"
  (mapcan (lambda (var)
	    (list (intern (symbol-name var) :keyword) var))
	  (cleanup-var-list var-list)))

(defun cleanup-var-list (var-list)
  "Flatten all the \(FUNCTION VAR) entries in VAR-LIST, leaving
only variable names."
  (loop for element in var-list
        if (consp element)
          nconc (loop for var in (rest element)
                      collect var)
        else
          collect element))
