;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DRAKMA; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/drakma/specials.lisp,v 1.16 2007/01/01 23:45:55 edi Exp $

;;; Copyright (c) 2006-2007, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :drakma)

(defmacro define-constant (name value &optional doc)
  "A version of DEFCONSTANT for, cough, /strict/ CL implementations."
  ;; See <http://www.sbcl.org/manual/Defining-Constants.html>
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +latin-1+ (make-external-format :latin-1 :eol-style :lf)
  "Default external format when reading headers.")

(define-constant +redirect-codes+ '(301 302 303 307)
  "A list of all HTTP return codes that redirect us to another URI.")

(define-constant +known-methods+ '(:copy
                                   :delete
                                   :delete
                                   :get
                                   :head
                                   :lock
                                   :mkcol
                                   :move
                                   :options
                                   :options*
                                   :post
                                   :propfind
                                   :proppatch
                                   :put
                                   :trace
                                   :unlock)
  "The HTTP methods \(including WebDAV methods) Drakma knows.")

(defconstant +buffer-size+ 8192)

(defvar *drakma-default-external-format* ':latin-1
  "The default value for the external format keyword arguments of
HTTP-REQUEST.")

(defvar *header-stream* nil
  "If this variable is not NIL, it should be bound to a stream to
which incoming and outgoing headers will be written for debugging
purposes.")

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/drakma/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :drakma
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
               
