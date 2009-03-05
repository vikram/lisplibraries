;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TBNL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/specials.lisp,v 1.92 2006/09/30 13:20:25 edi Exp $

;;; Copyright (c) 2004-2006, Dr. Edmund Weitz. All rights reserved.

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

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro defvar-unbound (name &optional (doc-string ""))
    "Convenience macro to declare unbound special variables with a
documentation string."
    `(progn
      (defvar ,name)
      (setf (documentation ',name 'variable) ,doc-string)))
  
  (defmacro defconstant* (name value &optional doc)
    "Make sure VALUE is evaluated only once \(to appease SBCL)."
    `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when doc (list doc)))))

;; common http return codes
(defconstant +http-continue+ 100)
(defconstant +http-switching-protocols+ 101)
(defconstant +http-ok+ 200)
(defconstant +http-created+ 201)
(defconstant +http-accepted+ 202)
(defconstant +http-non-authoritative-information+ 203)
(defconstant +http-no-content+ 204)
(defconstant +http-reset-content+ 205)
(defconstant +http-partial-content+ 206)
(defconstant +http-multiple-choices+ 300)
(defconstant +http-moved-permanently+ 301)
(defconstant +http-moved-temporarily+ 302)
(defconstant +http-see-other+ 303)
(defconstant +http-not-modified+ 304)
(defconstant +http-use-proxy+ 305)
(defconstant +http-temporary-redirect+ 307)
(defconstant +http-bad-request+ 400)
(defconstant +http-authorization-required+ 401)
(defconstant +http-payment-required+ 402)
(defconstant +http-forbidden+ 403)
(defconstant +http-not-found+ 404)
(defconstant +http-method-not-allowed+ 405)
(defconstant +http-not-acceptable+ 406)
(defconstant +http-proxy-authentication-required+ 407)
(defconstant +http-request-time-out+ 408)
(defconstant +http-conflict+ 409)
(defconstant +http-gone+ 410)
(defconstant +http-length-required+ 411)
(defconstant +http-precondition-failed+ 412)
(defconstant +http-request-entity-too-large+ 413)
(defconstant +http-request-uri-too-large+ 414)
(defconstant +http-unsupported-media-type+ 415)
(defconstant +http-requested-range-not-satisfiable+ 416)
(defconstant +http-expectation-failed+ 417)
(defconstant +http-internal-server-error+ 500)
(defconstant +http-not-implemented+ 501)
(defconstant +http-bad-gateway+ 502)
(defconstant +http-service-unavailable+ 503)
(defconstant +http-gateway-time-out+ 504)
(defconstant +http-version-not-supported+ 505)

(defconstant* +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The three-character names of the seven days of the week - needed
for cookie date format.")

(defconstant* +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The three-character names of the twelve months - needed for cookie
date format.")

(defvar *session-cookie-name* "tbnl-session"
  "The name of the cookie \(or the GET parameter) which is used to
store the session on the client side.")

(defvar *rewrite-for-session-urls* t
  "Whether HTML pages should possibly be rewritten for cookie-less
session-management.")

(defvar *content-types-for-url-rewrite*
  '("text/html" "application/xhtml+xml")
  "The content types for which url-rewriting is OK. See
*REWRITE-FOR-SESSION-URLS*.")

(defparameter *the-random-state* (make-random-state t)
  "A fresh random state.")

(defvar-unbound *session-secret*
  "A random value that's used to encode the public session data.")

(defvar *tbnl-port* 3000
  "The port TBNL is listening on.")

(defvar-unbound *tbnl-stream*
  "The stream representing the socket TBNL is listening on.")

(defvar *close-tbnl-stream* nil
  "Set this to T if you want to close the TBNL socket each time a
request has been handled.")

(defvar *tbnl-socket-usage-counter* 0
  "The number of requests serviced with the current socket.")
(declaim (type fixnum *tbnl-socket-usage-counter*))

(defvar *headers-sent* nil
  "Used internally to check whether the reply headers have
already been sent for this request.")

(defvar *file-upload-hook* nil
  "If this is not NIL, it should be a unary function which will
be called with a pathname for each file which is uploaded to
TBNL.  The pathname denotes the temporary file to which the
uploaded file is written.  The hook is called directly before the
file is created.")

(defvar *session-data* nil
  "All sessions of all users currently using TBNL. An alist where the
car is the session's ID and the cdr is the SESSION object itself.")

(defvar *session-max-time* #.(* 30 60)
  "The default time \(in seconds) after which a session times out.")
(declaim (type fixnum *session-max-time*))

(defvar *session-gc-frequency* 50
  "A session GC \(see function SESSION-GC) will happen every
*SESSION-GC-FREQUENCY* requests \(counting only requests which use a
session).")
(declaim (type fixnum *session-gc-frequency*))

(defvar *use-user-agent-for-sessions* t
  "Whether the 'User-Agent' header should be encoded into the session
string. If this value is true a session will cease to be accessible if
the client sends a different 'User-Agent' header.")

(defvar *use-remote-addr-for-sessions* nil
  "Whether the client's remote IP \(as returned by REAL-REMOTE-ADDR)
should be encoded into the session string. If this value is true a
session will cease to be accessible if the client's remote IP changes.

This might for example be an issue if the client uses a proxy server
which doesn't send correct 'X_FORWARDED_FOR' headers.")

(defvar *default-content-type* "text/html; charset=iso-8859-1"
  "The default content-type header which is returned to the client.")

(defvar *show-lisp-errors-p* nil
  "Whether Lisp errors should be shown in HTML output.")

(defvar *show-lisp-backtraces-p* nil
  "Whether Lisp backtraces should be shown in HTML output when an
error occurs. Will only have effect of *SHOW-LISP-ERRORS-P* is
also true.")

(defvar *catch-errors-p* t
  "Whether TBNL should catch and log errors \(or rather invoke
the debugger).")

(defvar *log-lisp-errors-p* t
  "Whether Lisp errors should be logged.")

(defvar *lisp-errors-log-level* :error
  "Log level for Lisp errors.")

(defvar *log-lisp-warnings-p* t
  "Whether Lisp warnings should be logged.")

(defvar *lisp-warnings-log-level* :warning
  "Log level for Lisp warnings.")

(defvar *log-lisp-backtraces-p* nil
  "Whether Lisp backtraces should be logged when an error
occurs. Will only have effect of *LOG-LISP-ERRORS-P* or
*LOG-LISP-BACKTRACES* are also true.")

(defvar *use-apache-log* #+:araneida nil #-:araneida t
  "Whether log messages should be sent as headers \(assuming that
mod_lisp hands them over to Apache).")

(defvar *show-access-log-messages* t
  "Whether routine messages about each request should be logged.  This
will only be done if *USE-APACHE-LOG* is NIL.")

(defvar *log-file*
    (load-time-value
      (let ((tmp-dir
              #+:allegro (system:temporary-directory)
              #+:lispworks (pathname (or (lw:environment-variable "TEMP")
                                         (lw:environment-variable "TMP")
                                         #+:win32 "C:/"
                                         #-:win32 "/tmp/"))
              #-(or :allegro :lispworks) #p"/tmp/"))
        (merge-pathnames "tbnl.log" tmp-dir)))
  "The log file to use if *USE-APACHE-LOG* is false.")

(defvar *log-file-stream* nil
  "The stream corresponding to the log file.")

(defvar *log-file-lock*
    #-:lispworks
    (kmrcl::make-lock "log-file-lock")
    #+:lispworks
    (mp:make-lock :name "log-file-lock")
  "A lock to prevent two threads from writing to the log file at same
time.")

(defvar-unbound *command*
  "The current request as read from *TBNL-STREAM*, converted into an
alist.")
  
(defvar-unbound *error*
  "The last error or warning handled by TBNL.")
  
(defvar-unbound *session*
  "The current SESSION object.")

(defvar-unbound *request*
  "The current REQUEST object.")

(defvar-unbound *reply*
  "The current REPLY object.")

(defvar-unbound *body*
  "The body which was sent to the front-end or to the browser.")

(defvar-unbound *backtrace*
  "The backtrace \(as a string) of the last error.")

(defvar *log-prefix* t
  "The prefix which is printed in front of Apache log messages. This
should be a string or T \(for \"TBNL\", the default) or NIL \(meaning
no prefix).")

(defvar-unbound *listener*
  "The KMRCL:LISTENER object which currently listens on *TBNL-PORT*")

(defvar *debug-mode* nil
  "Whether we're in debug mode.")

(defconstant* +implementation-link+
  #+:cmu "http://www.cons.org/cmucl/"
  #+:sbcl "http://www.sbcl.org/"
  #+:clisp "http://clisp.sf.net/"
  #+:allegro "http://www.franz.com/products/allegrocl/"
  #+:lispworks "http://www.lispworks.com/"
  #+:scl "http://www.scieneer.com/scl/"
  #+:openmcl "http://openmcl.clozure.com/"
  #+:digitool "http://www.digitool.com/"
  #+:cormanlisp "http://www.cormanlisp.com/"
  #+:ecl "http://ecls.sf.net/"
  #+:gcl "http://www.gnu.org/software/gcl/gcl.html")

(defvar *dispatch-table* (list 'default-dispatcher)
  "A list of dispatch functions - see function PROCESS-REQUEST.")

(defvar *default-handler* 'default-handler
  "The name of the function which is always returned by DEFAULT-DISPATCHER.")

(defvar *easy-handler-alist* nil) 

(defvar *http-error-handler* nil
  "Contains NIL \(the default) or a function of one argument which is
called if the content handler has set a return code other than
+HTTP-OK+ or +HTTP-NOT-MODIFIED+.")

(defvar *default-log-level* nil
  "The default log level for LOG-MESSAGE*")

(defvar *session-data-lock*
    #-:lispworks
    (kmrcl::make-lock "session-data-lock")
    #+:lispworks
    (mp:make-lock :name "session-data-lock")
  "A lock to prevent two threads from modifying *SESSION-DATA* at the
same time.")

(defvar *session-removal-hook* (constantly nil)
  "A function of one argument \(a session object) which is called
whenever a session is garbage-collected.")

(defvar *tmp-directory*
    #+(or :win32 :mswindows) "c:\\tbnl-temp\\"
    #-(or :win32 :mswindows) "/tmp/tbnl/"
    "Directory for temporary files created by MAKE-TMP-FILE-NAME.")

(defvar *tmp-files* nil
  "A list of temporary files created while a request was handled.")

(defvar *use-modlisp-headers* nil
  "If this variable is true then outgoing headers are written in a
format mod_lisp can understand, otherwise they're written like plain
HTTP headers.")

(defconstant +latin-1+
    #+:allegro :latin1
    #+:lispworks :latin-1
    #+:sb-unicode :latin-1
    #-(or :allegro :lispworks :sb-unicode) nil)

(defconstant +utf-8+
    #+:allegro :utf8
    #+:lispworks :utf-8
    #+:sb-unicode :utf-8
    #-(or :allegro :lispworks :sb-unicode) nil)

(defvar *tbnl-default-external-format* +latin-1+)

(defconstant +buffer-length+ 8192
  "Length of buffers used for internal purposes.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or (and :allegro :allegro-version>= (version>= 6 0))
        :sb-unicode
        :lispworks4.3
        :lispworks4.4
        :lispworks5.0)
  (pushnew :tbnl-bivalent-streams *features*))

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "*DEBUG-PRINT-VARIABLE-ALIST*" :sb-debug)
    (pushnew :tbnl-sbcl-debug-print-variable-alist *features*)))

#+:lispworks
(defvar *hunchentoot-p* t
  "Whether we are in 'Hunchentoot mode' or just serving as
infrastructure for TBNL.")

(pushnew :tbnl *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>

(defvar *hyperdoc-base-uri* "http://weitz.de/tbnl/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :tbnl
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
