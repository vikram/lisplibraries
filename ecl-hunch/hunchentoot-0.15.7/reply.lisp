;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/reply.lisp,v 1.20 2008/02/13 16:02:18 edi Exp $

;;; Copyright (c) 2004-2008, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :hunchentoot)

(defclass reply ()
  ((content-type :initform *default-content-type*
                 :documentation "The outgoing 'Content-Type' http
header which defaults to the value of *DEFAULT-CONTENT-TYPE*.")
   (content-length :initform nil
                   :documentation "The outgoing 'Content-Length'
http header which defaults NIL.  If this is NIL, Hunchentoot will
compute the content length.")
   (headers-out :initform nil
                :documentation "An alist of the outgoing http headers
not including the 'Set-Cookie', 'Content-Length', and 'Content-Type'
headers.  Use the functions HEADER-OUT and \(SETF HEADER-OUT) to
modify this slot.")
   (return-code :initform +http-ok+
                :documentation "The http return code of this
reply.  The return codes Hunchentoot can handle are defined in
specials.lisp.")
   (external-format :initform *hunchentoot-default-external-format*
                    :documentation "The external format of the reply -
used for character output.")
   (log-messages :initform nil
                 :reader log-messages
                 :documentation "A list \(in reverse chronological
order) of the messages which are to be written to the Apache error
log.  This slot's value should only be modified by the functions
defined in log.lisp.")
   (cookies-out :initform nil
                :documentation "The outgoing cookies.  This slot's
value should only be modified by the functions defined in
cookies.lisp."))
  (:documentation "Objects of this class hold all the information
about an outgoing reply. They are created automatically by
Hunchentoot and can be accessed and modified by the corresponding
handler."))

(defun headers-out (&optional (reply *reply*))
  "Returns an alist of the outgoing headers associated with the
REPLY object REPLY."
  (slot-value reply 'headers-out))

(defun cookies-out (&optional (reply *reply*))
  "Returns an alist of the outgoing cookies associated with the
REPLY object REPLY."
  (slot-value reply 'cookies-out))

(defun (setf cookies-out) (new-value &optional (reply *reply*))
  "Returns an alist of the outgoing cookies associated with the
REPLY object REPLY."
  (setf (slot-value reply 'cookies-out) new-value))

(defun content-type (&optional (reply *reply*))
  "The outgoing 'Content-Type' http header of REPLY."
  (slot-value reply 'content-type))

(defun (setf content-type) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Type' http header of REPLY."
  (setf (slot-value reply 'content-type) new-value))

(defun content-length (&optional (reply *reply*))
  "The outgoing 'Content-Length' http header of REPLY."
  (slot-value reply 'content-length))

(defun (setf content-length) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Length' http header of REPLY."
  (setf (slot-value reply 'content-length) new-value))

(defun return-code (&optional (reply *reply*))
  "The http return code of REPLY.  The return codes Hunchentoot can
handle are defined in specials.lisp."
  (slot-value reply 'return-code))

(defun (setf return-code) (new-value &optional (reply *reply*))
  "Sets the http return code of REPLY."
  (setf (slot-value reply 'return-code) new-value))

(defun reply-external-format (&optional (reply *reply*))
  "The external format of REPLY which is used for character output."
  (slot-value reply 'external-format))

(defun (setf reply-external-format) (new-value &optional (reply *reply*))
  "Sets the external format of REPLY."
  (setf (slot-value reply 'external-format) new-value))

(defun header-out-set-p (name &optional (reply *reply*))
  "Returns a true value if the outgoing http header named NAME has
been specified already.  NAME should be a keyword or a string."
  (assoc name (headers-out reply)))

(defun header-out (name &optional (reply *reply*))
  "Returns the current value of the outgoing http header named NAME.
NAME should be a keyword or a string."
  (cdr (assoc name (headers-out reply))))

(defun cookie-out (name &optional (reply *reply*))
  "Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-out reply) :test #'string=)))

(defsetf header-out (name &optional (reply '*reply*))
    (new-value)
  "Changes the current value of the outgoing http header named NAME (a
keyword or a string).  If a header with this name doesn't exist, it is
created."
  (with-rebinding (name reply)
    (with-unique-names (symbol place)
      `(let* ((,symbol (if (stringp ,name) (make-keyword ,name :destructivep nil) ,name))
              (,place (assoc ,symbol (headers-out ,reply) :test #'string-equal)))
         (cond
           (,place
            (setf (cdr ,place) ,new-value))
           (t
            (push (cons ,symbol ,new-value) (slot-value ,reply 'headers-out))
            ,new-value))))))