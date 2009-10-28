;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/util.lisp,v 1.35 2008/04/08 14:39:18 edi Exp $

;;; Copyright (c) 2004-2008, Dr. Edmund Weitz. All rights reserved.

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

#-:lispworks
(defmacro when-let ((var form) &body body)
  "Evaluates FORM and binds VAR to the result, then executes BODY
if VAR has a true value."
  `(let ((,var ,form))
     (when ,var ,@body)))

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
(defmacro with-rebinding (bindings &body body)
  "Renaming LW:REBINDING for better indentation."
  `(lw:rebinding ,bindings ,@body))

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

(defun starts-with-p (seq subseq &key (test 'eql))
  "Tests whether the sequence SEQ starts with the sequence
SUBSEQ.  Individual elements are compared with TEST."
  (let* ((length (length subseq))
         (mismatch (mismatch subseq seq
                             :test test)))
    (or (null mismatch)
        (<= length mismatch))))

(defun starts-with-one-of-p (seq subseq-list &key (test 'eql))
  "Tests whether the sequence SEQ starts with one of the
sequences in SUBSEQ-LIST.  Individual elements are compared with
TEST."
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
  (setq *session-secret* (create-random-string 10 36)))

(defun reason-phrase (return-code)
  "Returns a reason phrase for the HTTP return code RETURN-CODE
\(which should be an integer) or NIL for return codes Hunchentoot
doesn't know."
  (gethash return-code *http-reason-phrase-map*))

(defun make-keyword (string &key (destructivep t))
  "Interns the upcased version of STRING into the KEYWORD package.
Uses NSTRING-UPCASE if DESTRUCTIVEP is true.  Returns NIL if STRING is
not a string."
  (and (stringp string)
       (intern (if destructivep
                 (nstring-upcase string)
                 (string-upcase string)) :keyword)))

(defgeneric assoc (thing alist &key &allow-other-keys)
  (:documentation "LIKE CL:ASSOC, but \'does the right thing\' if
THING is a string or a symbol."))

(defmethod assoc ((thing symbol) alist &key &allow-other-keys)
  "Version of ASSOC for symbols, always uses EQ as test function."
  (cl:assoc thing alist :test #'eq))

(defmethod assoc ((thing string) alist &key (test #'string-equal))
  "Version of ASSOC for strings, uses STRING-EQUAL as default test
function."
  (cl:assoc thing alist :test test))

(defmethod assoc (thing alist &key (test #'eql))
  "Default method - uses EQL as default test like CL:ASSOC."
  (cl:assoc thing alist :test test))

(defun md5-hex (string)
  "Calculates the md5 sum of the string STRING and returns it as a hex string."
  (with-output-to-string (s)
    (loop for code across (md5:md5sum-sequence string)
	  do (format s "~2,'0x" code))))

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
  "Generates a time string according to RFC 1123.  Default is current time."
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

(let ((counter 0))
  (declare (ignorable counter))
  (defun make-tmp-file-name (&optional (prefix "hunchentoot"))
    "Generates a unique name for a temporary file.  This function is
called from the RFC2388 library when a file is uploaded."
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
      ;; maybe call hook for file uploads
      (when *file-upload-hook*
        (funcall *file-upload-hook* tmp-file-name))
      tmp-file-name)))

(defun quote-string (string)
  "Quotes string according to RFC 2616's definition of `quoted-string'."
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

(defun url-decode (string &optional (external-format *hunchentoot-default-external-format*))
  "Decodes a URL-encoded STRING which is assumed to be encoded using
the external format EXTERNAL-FORMAT."
  (let ((vector (make-array (length string)
                            :element-type 'octet
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
    (octets-to-string vector :external-format external-format)))

(defun form-url-encoded-list-to-alist (form-url-encoded-list
                                       &optional (external-format *hunchentoot-default-external-format*))
  "Converts a list FORM-URL-ENCODED-LIST of name/value pairs into an
alist.  Both names and values are url-decoded while doing this."
  (mapcar #'(lambda (entry)
              (destructuring-bind (name &optional value)
                  (split "=" entry :limit 2)
                (cons (string-trim " " (url-decode name external-format))
                      (url-decode (or value "") external-format))))
          form-url-encoded-list))

(defun url-encode (string &optional (external-format *hunchentoot-default-external-format*))
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
          for index from 0
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        ;; note that there's no comma in there - because of cookies
                        (find c "$-_.!*'()" :test #'char=))
                     (write-char c s))
                   (t (loop for octet across (string-to-octets string
                                                               :start index
                                                               :end (1+ index)
                                                               :external-format external-format)
                            do (format s "%~2,'0x" octet)))))))

(defun parse-content-type (content-type-header &optional want-external-format-p)
  "Reads and parses a `Content-Type' header and returns it as three
values - the type, the subtype, and an external format corresponding
to the 'charset' parameter in the header \(or
*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT*), if there is one and if the
content type is \"text\" or WANT-EXTERNAL-FORMAT-P is true.
CONTENT-TYPE-HEADER is supposed to be the corresponding header value
as a string."
  (with-input-from-string (stream content-type-header)
    (let* ((*current-error-message* "Corrupted Content-Type header:")
           (type (read-token stream))
           (subtype (and (or (ignore-errors (assert-char stream #\/))
                             (return-from parse-content-type
                               ;; try to return something meaningful
                               (values "application" "octet-stream"
                                       (and want-external-format-p
                                            *hunchentoot-default-external-format*))))
                         (read-token stream)))
           (parameters (read-name-value-pairs stream))
           (charset (cdr (assoc "charset" parameters)))
           (external-format
            (and (or want-external-format-p
                     (string-equal type "text"))
                 (or (when charset
                       (handler-case
                           (make-external-format (make-keyword charset) :eol-style :lf)
                         (error (condition)
                           (warn "Ignoring external format of name ~S~
because of error:~%~A"
                                 charset condition))))
                     *hunchentoot-default-external-format*))))
      (values type subtype external-format))))

(defun get-token-and-parameters (header)
  (with-input-from-string (stream header)
    (let* ((*current-error-message* (format nil "Corrupted header ~S:" header))
           (token (read-token stream))
           (parameters (read-name-value-pairs stream)))
      (values token parameters))))

(defun keep-alive-p ()
  "Returns a true value unless the incoming request obviates a
keep-alive reply.  The second return value denotes whether the client
has explicitly asked for a persistent connection."
  (let ((connection-values
         ;; the header might consist of different values separated by commas
         (when-let (connection-header (header-in :connection))
           (split "\\s*,\\s*" connection-header))))
    (flet ((connection-value-p (value)
             "Checks whether the string VALUE is one of the
values of the `Connection' header."
             (member value connection-values :test #'string-equal)))
      (let ((keep-alive-requested-p (connection-value-p "keep-alive")))
        (values (and (or (and (eq (server-protocol) :http/1.1)
                              (not (connection-value-p "close")))
                         (and (eq (server-protocol) :http/1.0)
                              keep-alive-requested-p)))
                keep-alive-requested-p)))))

(defun address-string ()
  "Returns a string with information about Hunchentoot suitable for
inclusion in HTML output."
  (format nil "<address>~:[~3*~;<a href='http://httpd.apache.org/'>~A</a> / <a href='http://www.fractalconcept.com/asp/html/mod_lisp.html'>mod_lisp~A~@[/~A~]</a> / ~]<a href='http://weitz.de/hunchentoot/'>Hunchentoot ~A</a> <a href='~A'>(~A ~A)</a>~@[ at ~A~:[ (port ~D)~;~]~]</address>"
          (server-mod-lisp-p *server*)
          (or (header-in :server-baseversion) "Apache")
          (or (header-in :modlisp-major-version) "")
          (header-in :modlisp-version)
          *hunchentoot-version*
          +implementation-link+
          (escape-for-html (lisp-implementation-type))
          (escape-for-html (lisp-implementation-version))
          (or (host *request*) (server-address *server*))
          (scan ":\\d+$" (or (host *request*) ""))
          (server-port)))

(defun server-name-header ()
  "Returns a string which can be used for 'Server' headers."
  (format nil "Hunchentoot ~A" *hunchentoot-version*))

(defun input-chunking-p ()
  "Whether input chunking is currently switched on for \(the socket
stream underlying) *HUNCHENTOOT-STREAM* - note that this will return
NIL if the underlying stream of the flexi stream is not a chunked
stream."
  (chunked-stream-input-chunking-p (flexi-stream-stream *hunchentoot-stream*)))

(defun cleanup-function ()
  "The default for *CLEANUP-FUNCTION*.  Invokes a GC on 32-bit
LispWorks and does nothing on other Lisps."
  #+(and :lispworks (not :lispworks-64bit))
  (hcl:mark-and-sweep 2))
