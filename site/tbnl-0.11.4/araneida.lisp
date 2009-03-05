;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TBNL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/araneida.lisp,v 1.13 2006/09/14 19:29:07 edi Exp $

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

(in-package #:tbnl)

#+:araneida
(defclass tbnl-araneida-handler (araneida:handler)
  ((port :accessor port
         :initform 8000
         :initarg :port)
   (fwd-port :accessor fwd-port
             :initform 9024
             :initarg :fwd-port)))

#+:araneida
(defmethod araneida:handle-request-response ((handler tbnl-araneida-handler) method request)
  (declare (ignore method))
  (let ((command nil)
        (*use-modlisp-headers* nil))
    ;; get the headers from the Araneida request and re-write them
    ;; into COMMAND - Araneida has the headers as, for example,
    ;;
    ;;   ((header1 value1) (header2 value2) ...)
    ;;
    ;; while TBNL wants the headers written like
    ;;
    ;;   ((header1 . value1) (header2 . value2) ...)
    (loop for (name value) in (araneida:request-headers request)
          do (push (cons (cond ((stringp name) name)
                               (t (format nil "~A" name)))
                         (cond ((stringp value) value)
                               (t (format nil "~A" value))))
                   command))
    ;; Araneida parses the body of the request before it decides how
    ;; the request is to be handled; one of the changes made to
    ;; Araneida to accomodate TBNL is to have an accessor,
    ;; REQUEST-UNPARSED-BODY, to a slot in the request that keeps the
    ;; unparsed body (if any); the MAP over the unparsed body is
    ;; necessary in Lispworks, and perhaps in other Lisps
    (with-input-from-string (input (map 'string #'code-char
                                        (araneida:request-unparsed-body request)))
      (with-open-stream (*tbnl-stream* (make-two-way-stream input
                                                            (araneida:request-stream request)))
        ;; add a few headers that TBNL wants
        (push (cons "method" (format nil "~A" (araneida:request-method request)))
              command)
        (push (cons "server-protocol" (format nil "HTTP/~,1,,,'0F"
                                              (araneida::request-http-version request)))
              command)
        (push (cons "url" (araneida:urlstring (araneida:request-url request)))
              command)
        (push (cons "content-stream" *tbnl-stream*)
              command)
        (push (cons "server-ip-port" (format nil "~d" (port handler)))
              command)
        (multiple-value-bind (result code)
            (ignore-errors*
              (process-request command)
              (force-output *tbnl-stream*))
          (declare (ignorable code))
          (when code (format t "Error in TBNL: Code ~S -- Result ~S~%" code result))
          result))))
  t)
