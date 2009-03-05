;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TBNL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/util.lisp,v 1.53 2006/09/30 13:20:25 edi Exp $

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

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lw:with-unique-names))

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'with-rebinding)
          (macro-function 'lw:rebinding)))

#-:lispworks
(defmacro with-rebinding (bindings &body body)
  "Syntax: WITH-REBINDING ( { var | (var prefix) }* ) form*

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

(defun maybe-invoke-debugger (condition)
  "Invokes the debugger if *CATCH-ERRORS-P* is true."
  (unless *catch-errors-p*
    (invoke-debugger condition)))

(defmacro with-debugger (&body body)
  "Executes BODY and invokes the debugger if an error is signaled and
*CATCH-ERRORS-P* is NIL."
  `(handler-bind ((error #'maybe-invoke-debugger))
     ,@body))

(defmacro ignore-errors* (&body body)
  "Like IGNORE-ERRORS, but observes *CATCH-ERRORS-P*."
  `(ignore-errors (with-debugger ,@body)))
       
(defmacro handler-case* (expression &rest clauses)
  "Like HANDLER-CASE, but observes *CATCH-ERRORS-P*."
  `(handler-case (with-debugger ,expression)
     ,@clauses))

(declaim (inline starts-with-p))
(defun starts-with-p (seq subseq &key (test 'eql))
  "Tests whether the sequence SEQ starts with the sequence SUBSEQ."
  (let* ((length (length subseq))
         (mismatch (mismatch subseq seq
                             :test test)))
    (or (null mismatch)
        (<= length mismatch))))

(defun starts-with-one-of-p (seq subseq-list &key (test 'eql))
  "Tests whether the sequence SEQ starts with one of the sequences in
SUBSEQ-LIST."
  (some (lambda (subseq)
          (starts-with-p seq subseq :test test))
        subseq-list))

(defun create-random-string (&optional (n 10) (base 16))
  "Returns a random number \(as a string) with base BASE and N
digits."
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~VR" base
              (random base *the-random-state*)))))

(defun reset-session-secret ()
  "Sets *SESSION-SECRET* to a new random value. All old sessions will
cease to be valid."
  (setq *session-secret*
          (create-random-string 10 36)))

(defun status-line (return-code)
  "Returns a meaningful status line for the http return code
RETURN-CODE \(which should be an integer)."
  (case return-code
    ((#.+http-ok+) "OK")
    ((#.+http-moved-permanently+) "Moved Permanently")
    ((#.+http-moved-temporarily+) "Moved Temporarily")
    ((#.+http-not-modified+) "Not Modified")
    ((#.+http-authorization-required+) "Authorization Required")
    ((#.+http-forbidden+) "Forbidden")
    ((#.+http-not-found+) "Not Found")
    ((#.+http-internal-server-error+) "Internal Server Error")
    ((#.+http-continue+) "Continue")
    ((#.+http-switching-protocols+) "Switching Protocols")
    ((#.+http-created+) "Created")
    ((#.+http-accepted+) "Accepted")
    ((#.+http-non-authoritative-information+) "Non-Authoritative Information")
    ((#.+http-no-content+) "No Content")
    ((#.+http-reset-content+) "Reset Content")
    ((#.+http-partial-content+) "Partial Content")
    ((#.+http-multiple-choices+) "Multiple Choices")
    ((#.+http-see-other+) "See Other")
    ((#.+http-use-proxy+) "Use Proxy")
    ((#.+http-temporary-redirect+) "Temporary Redirect")
    ((#.+http-bad-request+) "Bad Request")
    ((#.+http-payment-required+) "Payment Required")
    ((#.+http-method-not-allowed+) "Method Not Allowed")
    ((#.+http-not-acceptable+) "Not Acceptable")
    ((#.+http-proxy-authentication-required+) "Proxy Authentication Required")
    ((#.+http-request-time-out+) "Request Time-out")
    ((#.+http-conflict+) "Conflict")
    ((#.+http-gone+) "Gone")
    ((#.+http-length-required+) "Length Required")
    ((#.+http-precondition-failed+) "Precondition Failed")
    ((#.+http-request-entity-too-large+) "Request Entity Too Large")
    ((#.+http-request-uri-too-large+) "Request-URI Too Large")
    ((#.+http-unsupported-media-type+) "Unsupported Media Type")
    ((#.+http-requested-range-not-satisfiable+) "Requested range not satisfiable")
    ((#.+http-expectation-failed+) "Expectation Failed")
    ((#.+http-not-implemented+) "Not Implemented")
    ((#.+http-bad-gateway+) "Bad Gateway")
    ((#.+http-service-unavailable+) "Service Unavailable")
    ((#.+http-gateway-time-out+) "Gateway Time-out")
    ((#.+http-version-not-supported+) "Version not supported")))

(defun string-assoc (string a-list)
  "Returns an entry keyed by the string STRING from A-LIST. The search
is case-insensitive."
  (cdr (assoc string a-list :test #'string-equal)))

(defun string-assoc* (string a-list)
  "Returns an entry keyed by the string STRING from A-LIST. The search
is case-sensitive."
  (cdr (assoc string a-list :test #'string=)))

(defun md5-hex (string)
  "Calculates the md5 sum of the string STRING and returns it as a hex string."
  (with-output-to-string (s)
    (loop for code across (md5:md5sum-sequence string)
	  do (format s "~2,'0x" code))))

(defpackage :tbnl-dummy)

(defun read-from-string* (string)
  "Safe version of READ-FROM-STRING: Symbols are interned in the
:TBNL-DUMMY package, *READ-EVAL* is turned off."
  (let ((*package* (find-package :tbnl-dummy))
        (*read-eval* nil))
    (read-from-string string)))

(defmacro debug-value (var expr)
  "Evaluates and returns EXPR but also sets VAR to its value if in
debug mode."
  `(if *debug-mode*
    (setq ,var ,expr)
    ,expr))

(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(defun http-token-p (token)
  "Tests whether TOKEN is a string which is a valid 'token'
according to HTTP/1.1 \(RFC 2068)."
  (and (stringp token)
       (plusp (length token))
       (every (lambda (char)
                (and ;; CHAR is US-ASCII but not control character or ESC
                     (< 31 (char-code char) 127)
                     ;; CHAR is not 'tspecial'
                     (not (find char "()<>@,;:\\\"/[]?={} " :test #'char=))))
              token)))


(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generate time string according to RFC 1123. Default is current time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))

(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

#+:cmu
(defun get-backtrace (error)
  (declare (ignore error))
  (with-output-to-string (s)
    (let ((debug:*debug-print-level* nil)
          (debug:*debug-print-length* nil))
      (debug:backtrace most-positive-fixnum s))))

#+:sbcl
(defun get-backtrace (error)
  (declare (ignore error))
  (with-output-to-string (s)
    #+:tbnl-sbcl-debug-print-variable-alist
    (let ((sb-debug:*debug-print-variable-alist*
            (list* '(*print-level* . nil)
                   '(*print-length* . nil)
                   sb-debug:*debug-print-variable-alist*)))
      (sb-debug:backtrace most-positive-fixnum s))
    #-:tbnl-sbcl-debug-print-variable-alist
    (let ((sb-debug:*debug-print-level* nil)
          (sb-debug:*debug-print-length* nil))
      (sb-debug:backtrace most-positive-fixnum s))))

#+:lispworks
(defun get-backtrace (error)
  (declare (ignore error))
  (with-output-to-string (s)
    (let ((dbg::*debugger-stack* (dbg::grab-stack nil :how-many most-positive-fixnum))
          (*debug-io* s)
          (dbg:*debug-print-level* nil)
          (dbg:*debug-print-length* nil))
      (dbg:bug-backtrace nil))))

#+:allegro
(defun get-backtrace (error)
  (with-output-to-string (s)
    (with-standard-io-syntax
      (let ((*print-readably* nil)
            (*print-miser-width* 40)
            (*print-pretty* t)
            (tpl:*zoom-print-circle* t)
            (tpl:*zoom-print-level* nil)
            (tpl:*zoom-print-length* nil))
        (ignore-errors
          (format *terminal-io* "~
~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
                  error))
        (ignore-errors
          (let ((*terminal-io* s)
                (*standard-output* s))
            (tpl:do-command "zoom"
                            :from-read-eval-print-loop nil
                            :count t
                            :all t)))))))

#+:openmcl
(defun get-backtrace (error)
  (with-output-to-string (s)
    (let ((*debug-io* s))
      (format *terminal-io* "~
~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
              error)
      (ccl:print-call-history :detailed-p nil))))

#-(or :cmu :sbcl :lispworks :allegro :openmcl)
(defun get-backtrace (error)
  (declare (ignore error))
  (format nil "Output of backtrace currently not implemented for ~A"
          (lisp-implementation-type)))

(setf (documentation 'get-backtrace 'function)
        "This is the function that is used internally by TBNL to
show or log backtraces.  It accepts a condition object ERROR and
returns a string with the corresponding backtrace.")

(let ((counter 0))
  (declare (ignorable counter))
  (defun make-tmp-file-name (&optional (prefix "tbnl"))
    (let ((tmp-file-name
            #+:allegro
            (pathname (system:make-temp-file-name prefix *tmp-directory*))
            #-:allegro
            (loop for pathname = (make-pathname :name (format nil "~A-~A"
                                                              prefix (incf counter))
                                      :type nil
                                      :defaults *tmp-directory*)
                  unless (probe-file pathname)
                    return pathname)))
      (push tmp-file-name *tmp-files*)
      (when *file-upload-hook*
        (funcall *file-upload-hook* tmp-file-name))
      tmp-file-name)))

(defun quote-string (string)
  "Quote string according to RFC 2616's definition of quoted-string"
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            unless (or (char< char #\Space)
                       (char= char #\Rubout))
              do (case char
                   ((#\\) (write-string "\\\\" out))
                   ((#\") (write-string "\\\"" out))
                   (otherwise (write-char char out)))))))


(defun tbnl-info-string ()
  "Provides parts of the version info depending on
*USE-MODLISP-HEADERS* and the presence of :ARANEIDA in *FEATURES*.
Can only be called when a request is handled."
  (cond (*use-modlisp-headers*
         (format nil "<a href='http://httpd.apache.org/'>~A</a> / <a href='http://www.fractalconcept.com/asp/html/mod_lisp.html'>mod_lisp~A/~A</a> / "
                 (or (header-in "server-baseversion") "Apache")
                 (or (header-in "modlisp-major-version") "")
                 (or (header-in "modlisp-version") "")))
        (t
         #+:araneida
         "<a href='http://www.cliki.net/Araneida'>Araneida</a> / "
         #-:araneida
         "")))

#+(and :lispworks4.4 (or :win32 :linux))
(let ((id :system-cons-free-chain))
  (unless (scm::patch-id-loaded-p id)
    (error "You need a patch to improve the performance of this code. Request patch ~S for ~A for ~A from lisp-support@lispworks.com using the Report Bug command."
          id (lisp-implementation-type)
          #+:win32 "Windows"
          #+:linux "Linux")))

#+:lispworks
(defun octets-to-string (octets &optional (external-format *tbnl-default-external-format*))
  "Converts an array of type \(UNSIGNED-BYTE 8) to a Lisp string using
the external format EXTERNAL-FORMAT."
  ;; there's also EXTERNAL-FORMAT:DECODE-EXTERNAL-STRING but it's not
  ;; documented and there are certain problems with delivery, see
  ;; <http://thread.gmane.org/gmane.lisp.lispworks.general/4524>
  (let ((octet-count (length octets)))
    (when (zerop octet-count)
      (return-from octets-to-string ""))
    (let ((vector (sys:in-static-area
                   (make-array octet-count
                               :element-type '(unsigned-byte 8)
                               :initial-contents octets))))
      (fli:with-dynamic-lisp-array-pointer
          (ptr vector)
        (fli:convert-from-foreign-string ptr
                                         :external-format external-format
                                         :length octet-count
                                         :null-terminated-p nil)))))

#+:sb-unicode
(defun octets-to-string (octets &optional (external-format *tbnl-default-external-format*))
  "Converts an array of type \(UNSIGNED-BYTE 8) to a Lisp string using
the external format EXTERNAL-FORMAT."
  (sb-ext:octets-to-string octets :external-format external-format))

#+:allegro
(defun octets-to-string (octets &optional (external-format *tbnl-default-external-format*))
  "Converts an array of type \(UNSIGNED-BYTE 8) to a Lisp string using
the external format EXTERNAL-FORMAT."
  (excl:octets-to-string octets :external-format external-format))

#-(or :lispworks :sb-unicode :allegro)
(defun octets-to-string (octets &optional external-format)
  "Converts an array of type \(UNSIGNED-BYTE 8) to a Lisp string using
the external format EXTERNAL-FORMAT."
  (warn "Ignoring external format ~S because a native version of OCTETS-TO-STRING isn't implemented for ~A"
        external-format (lisp-implementation-type))
  (with-output-to-string (out)
    (loop for octet across octets
          do (write-char (code-char octet) out))))

(defun url-decode (string &optional (external-format *tbnl-default-external-format*))
  "Decode a url-encoded STRING which is assumed to be encoded using
the external format EXTERNAL-FORMAT."
  (let ((vector (make-array (length string)
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0)))
    (loop with percent-p and buff
          for char of-type character across string
          for i from 0
          when buff do
            (vector-push (parse-integer string
                                        :start (1- i)
                                        :end (1+ i)
                                        :radix 16)
                         vector)
            (setq buff nil)
          else when percent-p
                 do (setq buff t
                          percent-p nil)
          else when (char= char #\%)
                 do (setq percent-p t)
          else do (vector-push (char-code (case char
                                            ((#\+) #\Space)
                                            (otherwise char)))
                         vector))
    (octets-to-string vector external-format)))

(defun form-url-encoded-list-to-alist (form-url-encoded-list &optional (external-format *tbnl-default-external-format*))
  "Converts a list FORM-URL-ENCODED-LIST of name/value pairs into an
alist.  Both names and values are url-decoded while doing this."
  (mapcar #'(lambda (entry)
              (destructuring-bind (name &optional value)
                  (cl-ppcre:split "=" entry :limit 2)
                (cons (string-trim " " (url-decode name external-format))
                      (url-decode (or value "") external-format))))
          form-url-encoded-list))

#+:lispworks
(defun string-to-octets (string &optional (external-format *tbnl-default-external-format*))
  "Converts a Lisp string to an array of type \(UNSIGNED-BYTE 8) using
the external format EXTERNAL-FORMAT."
  ;; there's also EXTERNAL-FORMAT:ENCODE-LISP-STRING but it's not
  ;; documented
  (when (string= string "")
    (return-from string-to-octets #()))
  (fli:with-dynamic-foreign-objects ()
    (multiple-value-bind (str-ptr str-length octet-count)
        (fli:convert-to-dynamic-foreign-string  string
                                                :external-format external-format
                                                :null-terminated-p nil)
      (declare (ignore str-length))
      (let ((vector (make-array octet-count
                                :element-type '(unsigned-byte 8))))
        (fli:with-coerced-pointer (vec-ptr :type `(:c-array (:unsigned :byte) ,octet-count))
            str-ptr
          (dotimes (i octet-count)
            (setf (aref vector i)
                    (fli:foreign-aref vec-ptr i)))
          vector)))))

#+:sb-unicode
(defun string-to-octets (string &optional (external-format *tbnl-default-external-format*))
  "Converts a Lisp string to an array of type \(UNSIGNED-BYTE 8) using
the external format EXTERNAL-FORMAT."
  (sb-ext:string-to-octets string :external-format external-format))

#+:allegro
(defun string-to-octets (string &optional (external-format *tbnl-default-external-format*))
  "Converts a Lisp string to an array of type \(UNSIGNED-BYTE 8) using
the external format EXTERNAL-FORMAT."
  (excl:string-to-octets string
                         :external-format external-format
                         :null-terminate nil))

#-(or :lispworks :sb-unicode :allegro)
(defun string-to-octets (string &optional external-format)
  "Converts a Lisp string to an array of type \(UNSIGNED-BYTE 8) using
the external format EXTERNAL-FORMAT."
  (warn "Ignoring external format ~S because a native version of STRING-TO-OCTETS isn't implemented for ~A"
        external-format (lisp-implementation-type))
  (let ((vector (make-array (length string)
                            :element-type '(unsigned-byte 8) )))
    (loop for char across string
          for i from 0
          do (setf (aref vector i)
                     (char-code char)))
    vector))

(defun url-encode (string &optional (external-format *tbnl-default-external-format*))
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        (find c "$-_.!*'()," :test #'char=))
                     (write-char c s))
                   ((char= c #\Space)
                     (write-char #\+ s))
                   (t (loop for octet across (string-to-octets (coerce (list c) 'string)
                                                               external-format)
                            do (format s "%~2,'0x" octet)))))))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defun socket-remote-host ()
  (ignore-errors*
    #+:allegro (socket:ipaddr-to-dotted (socket:remote-host *tbnl-stream*))
    #+:lispworks (comm:ip-address-string (comm:socket-stream-peer-address (raw-stream)))))

(defun socket-remote-port ()
  (ignore-errors*
  #+:allegro (socket:ipaddr-to-dotted (socket:remote-port *tbnl-stream*))
  #+:lispworks (comm:ip-address-string (nth-value 1 (comm:socket-stream-peer-address (raw-stream))))))

(defun socket-local-host ()
  (ignore-errors*
    #+:allegro (socket:ipaddr-to-dotted (socket:local-host *tbnl-stream*))
    #+:lispworks (comm:ip-address-string (comm:socket-stream-address (raw-stream)))))

(defun socket-local-port ()
  (ignore-errors*
    #+:allegro (socket:ipaddr-to-dotted (socket:local-port *tbnl-stream*))
    #+:lispworks (comm:ip-address-string (nth-value 1 (comm:socket-stream-address (raw-stream))))))

(defmacro with-lock-held ((lock) &body body)
  #-:lispworks
  `(kmrcl::with-lock-held (,lock)
     ,@body)
  #+:lispworks
  `(mp:with-lock (,lock)
     ,@body))