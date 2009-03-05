;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; * Global Configuration Variables

(defvar *ucw-tal-root*
  (merge-pathnames (make-pathname :directory '(:relative "wwwroot"))
		   (asdf:component-pathname (asdf:find-system :ucw))))

(defvar *inspect-components* nil
  "When rendering, should we put links next to components that will bring up the inspector?")

(defconstant +default-encoding-for-uri+ :utf-8
  "UTF-8 is the semi-standard encoding for URL:s
 RFC 2396 does not specify how multi-byte characters are encoded, but mentions UTF-8 as an example.
 RFC 2718 Strongly recommends UTF-8 for new URI-schemes.
 RFC 3987 The IRI (International Resource Identifier) proposed standard specifies UTF-8.
 The Javascript ECMA standard specifies that the conversion functions (EncodeURI et al.) use UTF-8,
 http://www.ecma-international.org/publications/files/ecma-st/ECMA-262.pdf
")

(defvar *external-formats* `(:url ,+default-encoding-for-uri+
                             :slime :iso-latin-1-unix
                             :http-emacsen :iso-latin-1-unix
                             :http-lispish :latin-1)

  "The external formats used for url-unescaping, slime and http(via swank) intercommunication")

(defun external-format-for (what)
  (getf ucw::*external-formats* what))

(defun (setf external-format-for) (value what)
  (setf (getf ucw::*external-formats* what) value))

(defvar *debug-on-error* nil
  "The default, system wide, value for
  debug-on-error. Applications may override this.")

(defmethod debug-on-error ((app null))
  "Method used when there is no current application."
  *debug-on-error*)

(defmethod (setf debug-on-error) (value (app null))
  (setf *debug-on-error* value))



;;;; * Default UCW configuration

;;;; This file defines a default configuration for UCW, based on
;;;; settings which should be as general as possible and at the same
;;;; time the most common.

;;;; * The default settings

;;;; General variables
(defvar *ucw-config-file* (arnesi:getenv "CONFIGFILE"))
(defvar *ucw-swank-port* 4005)

;;;; `ucw:create-server' variables
(defvar *ucw-backend-type* :httpd)
(defvar *ucw-backend-host* "127.0.0.1")
(defvar *ucw-backend-port* 8080)
(defvar *ucw-server-class* 'standard-server)

(defvar *ucw-applications-directory* (cl-fad:directory-exists-p "/etc/ucw/applications.d/"))
(defvar *ucw-systems* nil
  "The asdf systems listed here will be loaded as part of the configuration process.")
(defvar *ucw-applications* nil
  "Each element of this list is evaluated and should return an application instance.
\(A CLOS object instance evaluates to itself, so you are free to provide either app
instances or expressions evaluating to app instances.\)")

(defvar *ucw-log-root-directory* (or (arnesi:getenv "LOGROOT")
                                     (merge-pathnames #P"logs/"
                                                      (asdf:component-pathname
                                                       (asdf:find-system :ucw)))))
(defvar *ucw-log-level* +info+
  "This is the default runtime log level for the UCW loggers.")
(defvar *ucw-compile-time-log-level* +debug+
  "UCW logger messages below this level will not be compiled into the code, so they will have no performance penalty.")

#+(or (and sbcl sb-unicode)
      (and allegro ics)
      (and clisp unicode))
(dolist (cell '((:slime        . "utf-8-unix")
                (:url          . :utf-8)
                (:http         . :utf-8)
                (:http-emacsen . :utf-8-unix)
                (:http-lispish . :utf-8)))
  (setf (external-format-for (car cell)) (cdr cell)))

;; Copyright (c) 2003-2005 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
