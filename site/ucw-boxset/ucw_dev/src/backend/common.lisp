;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; Library of functions shared by multiple backends.

(defconstant +Space+ #.(char-code #\Space))
(defconstant +Tab+ #.(char-code #\Tab))
(defconstant +Colon+ #.(char-code #\:))
(defconstant +Linefeed+ 10)
(defconstant +Carriage-Return+ 13)

(defun split-on-space (line)
  "Split line on #\Space."
  (iter outer ; we only need the outer to be able to collect a last one in the finally of the inner
        (iter (with start = 0)
              (for end upfrom 0)
              (for char in-vector line)
              (when (= +Space+ char)
                (in outer (collect (make-displaced-array line start end)))
                (setf start (1+ end)))
              (finally (in outer (collect (make-displaced-array line start)))))
        (while nil)))

(defun split-header-line (line)
  (let* ((colon-position (position +Colon+ line :test #'=))
         (name-length colon-position)
         (value-start (1+ colon-position))
         (value-end (length line)))
    ;; skip any leading space char in the header value
    (iterate
      (for start upfrom value-start)
      (while (< start value-end))
      (while (or (= +Space+ (aref line start))
                 (= +Tab+ (aref line start))))
      (incf start)
      (finally (setf value-start (1- start))))
    (cons (make-displaced-array line 0 name-length)
          (make-displaced-array line value-start value-end))))

(defun add-set-cookie-header (response value)
  (add-header response "Set-Cookie" (if (rfc2109:cookie-p value)
                                        (rfc2109:cookie-string-from-cookie-struct value)
                                        value)))

;;;; servinge static files

(defvar *mime-types* nil)

(defvar *default-mime-types-file* #P"/etc/mime.types")

(defun parse-mime-types (mime-types-file)
  "Parse mime.types file"
  (iterate
    (for line in-file mime-types-file using #'read-line)
    (when (or (string= "" line) (eq #\# (aref line 0)))
      (next-iteration))
    (for splut = (split-sequence:split-sequence-if
		  #'(lambda (char)
		      (member char '(#\Space #\Tab)))
		  line :remove-empty-subseqs t))
    (unless (null splut)
      (collect splut))))

(defun read-mime-types (mime-types-file)
  "Read in mime.types file."
  (iterate
    (for (type . extensions)
	  in (parse-mime-types mime-types-file))
    (aif (assoc type *mime-types* :test #'string-equal)
	 (setf (cdr it) (nconc (cdr it) extensions))
	 (push (cons type extensions) *mime-types*))))

(defun mime-type-extensions (type)
  "Extensions that can be given to file of given MIME type."
  (cdr (assoc type *mime-types* :test #'string-equal)))

(defun extension-mime-type (extension)
  "MIME type associated with file extension."
  (first
   (find-if #'(lambda (typespec)
		(find extension (rest typespec)
		      :test #'string-equal))
	    *mime-types*)))

(defun map-query-path-to-file (query-path url-base directory)
  "Converts QUERY-PATH to a file on the local filesystem assuming
  the application is mapped to URL-BASE and lives in DIRECTORY.

In other words: if query-path is \"<URL-BASE><SOMETHING>\"
returns: \"<DIRECTORY><SOMETHING>\" (assuming said file
exists), otherwise returns NIL."
  ;; NB: We really could (and should) cache this function on
  ;; QUERY-PATH, but we need to figure out how to wipe the cache when
  ;; an application's url-prefix changes. considering how rarely that
  ;; happens it's sucks that we lose the speed increase this could
  ;; bring...
  (multiple-value-bind (starts-with <something>)
      (starts-with query-path url-base :return-suffix t)
    (if starts-with
        (let ((pathname (merge-pathnames <something> directory)))
          (aif (probe-file pathname)
               (when (pathname-name it)
                 it)
               nil))
        nil)))

(defun disallow-response-caching (response)
  "Sets the appropiate response headers that will instruct the clients not to cache this response."
  (setf (get-header response "Expires") #.(date:universal-time-to-http-date +epoch-start+)
        (get-header response "Cache-Control") "no-store"
        (get-header response "Pragma") "no-cache"))

(macrolet ((defserve ((name args &key last-modified expires
                            content-length) &body body)
             (with-unique-names (last-modified-value expires-value content-length-value if-modified-value)
               `(defun ,name ,args
                 ,(when (stringp (first body))
                        (pop body))
                 (let ((,last-modified-value ,last-modified)
                       (,content-length-value ,content-length)
                       (,expires-value ,expires)
                       (,if-modified-value (get-header request "If-Modified-Since")))
                   (when ,expires-value
                     (setf (get-header response "Expires") (date:universal-time-to-http-date (+ (get-universal-time) ,expires-value))))
                   (when ,last-modified-value
                     (setf (get-header response "Last-Modified") (date:universal-time-to-http-date ,last-modified-value)))
                   (setf (get-header response "Date") (date:universal-time-to-http-date (get-universal-time)))
                   (if (and ,last-modified-value
                            ,if-modified-value
                            (<= ,last-modified-value
                                (date:parse-time 
                                 ;; IE sends junk with the date (but sends it after a semicolon)
                                 (subseq ,if-modified-value 0 (position #\; ,if-modified-value :test #'equal)))))
                       (progn
                         (ucw.rerl.server.dribble "defserve: 304 not modified.")
                         (setf (status response) "304 Not Modified"
                               (get-header response "Content-Length") "0")
                         (send-response response))
                       (progn
                         (setf (get-header response "Status") "200 OK")
                         (when ,content-length-value
                           (setf (get-header response "Content-Length") (princ-to-string ,content-length-value)))
                         ,@body)))
                 (close-request request)))))
  
  (defserve (serve-file (filename request response)
                        :last-modified (file-write-date filename) 
                        :expires #.(* 365 24 60 60))
      (with-input-from-file (file filename :element-type '(unsigned-byte 8))
        (setf (get-header response "Content-Type") (or (extension-mime-type (pathname-type filename))
                                                       (switch ((pathname-type filename) :test #'string=)
                                                         ("html" "text/html")
                                                         ("css"  "text/css")
                                                         (t "text/plain")))
              (get-header response "Content-Length") (princ-to-string (file-length file)))
        (send-headers response)
        (loop
            with buffer = (make-array 8192 :element-type '(unsigned-byte 8))
            for end-pos = (read-sequence buffer file)
            until (zerop end-pos) do
            (write-sequence buffer (network-stream request) :end end-pos))))
  
  (defserve (serve-sequence (sequence &key
                                      (request (context.request *context*))
                                      (response (context.response *context*))
                                      (last-modified (get-universal-time))
                                      (content-type "application/octet-stream")
                                      (expires #.(* 60 60)))
                            :last-modified last-modified :expires expires)
    "Write SEQUENCE into the network stream and clean up the request. SEQUENCE
may be a string or a byte vector. When it's a string it will be encoded
according to what the (encoding response) protocol answers."
    (let* ((bytes (if (stringp sequence)
                      (string-to-octets sequence (encoding response))
                      sequence)))
      (setf (get-header response "Content-Type") content-type
            (get-header response "Content-Length") (princ-to-string (length bytes)))
      (send-headers response)
      (write-sequence bytes (network-stream request)))))

;;;; Parsing HTTP request bodies.

;;;; The httpd, mod_lisp and araneida backends use this code.

(defun grab-param (param-string start =-pos end)
  "Returns (KEY . VALUE) of the request param whose first char
  is at START, whose \#= char is at =-POS and whose last char is
  at (1+ END.).

  =-POS may be NIL, END may be equal to =-POS or the last index
  of START."
  (let* ( ;; the index of the first char of the key
         (key-start start)
         ;; the index of the char immediatly after the last char of key
         (key-end (or =-pos end))
         (key (make-displaced-array param-string key-start key-end))
         ;; the index of the first char of the value
         (value-start (if =-pos
                          (1+ =-pos)
                          end))
         ;; the index of the char immediatly after the
         ;; end of the value (may be equal to
         ;; key-start in the case of "" values).
         (value-end end)
         (value (if value-end
                    (make-displaced-array param-string value-start value-end)
                    ""))
         ;; TODO can we use nunescape-as-uri here? if we rename to nparse-query-parameters?
         (unescaped-key (unescape-as-uri key))
         (unescaped-value (unescape-as-uri value)))
    (ucw.backend.dribble "Grabbed parameter ~S with value ~S." unescaped-key unescaped-value)
    (cons unescaped-key unescaped-value)))

(defun parse-query-parameters (param-string)
  (let ((params '()))
    (when (and param-string (< 0 (length param-string)))
      (iterate
        (with start = 0)
        (with =-pos = nil)
        (for char in-vector param-string)
        (for offset upfrom 0)
        (case char
          (#\& ;; end of the current param
           (push (grab-param param-string start =-pos offset) params)
           (setf start (1+ offset)
                 =-pos nil))
          (#\= ;; end of name
           (setf =-pos offset)))
        ;; automatic end of param string
        (finally (push (grab-param param-string start =-pos (1+ offset)) params))))
    (nreverse params)))

(defun make-temp-filename (&optional (prefix "ucw"))
  "Returns a pathname for a temporary file.

This function is used by the rfc2388 parser callback to decide
where to put the mime data."
  (strcat "/tmp/" prefix (princ-to-string (get-universal-time))))

(defun rfc2388-callback (mime-part)
  (ucw.backend.dribble "Processing mime part ~S." mime-part)
  (let* ((header (rfc2388:get-header mime-part "Content-Disposition"))
         (disposition (rfc2388:header-value header))
         (name (rfc2388:get-header-attribute header "name"))
         (filename (rfc2388:get-header-attribute header "filename")))
    (ucw.backend.dribble "Got a mime part. Disposition: ~S; Name: ~S; Filename: ~S" disposition name filename)
    (ucw.backend.dribble "Mime Part: ---~S---~%" (with-output-to-string (dump)
                                                   (rfc2388:print-mime-part mime-part dump)))
    (cond
      ((or (string-equal "file" disposition)
           (not (null filename)))
       (let ((filename (make-temp-filename)))
         (setf (rfc2388:content mime-part) (open filename
                                                 :direction :output
                                                 :element-type '(unsigned-byte 8)))
         (ucw.backend.dribble "Sending mime part data to file ~S (~S)."
			      filename (rfc2388:content mime-part))
         (let ((counter 0))
           (values (lambda (byte)
                     (ucw.backend.dribble "File byte ~4,'0D: ~D~:[~; (~C)~]"
					  counter byte (<= 32 byte 127)
					  (code-char byte))
                     (incf counter)
                     (write-byte byte (rfc2388:content mime-part)))
                   (lambda (mime-part)
                     (ucw.backend.dribble "Done with file ~S." (rfc2388:content mime-part))
                     (ucw.backend.dribble "Closing ~S." (rfc2388:content mime-part))
                     (close (rfc2388:content mime-part))
                     (ucw.backend.dribble "Closed, repoening.")
                     (setf (rfc2388:content mime-part)
                           (open filename
                                 :direction :input
                                 :element-type '(unsigned-byte 8)))
                     (ucw.backend.dribble "Opened ~S." (rfc2388:content mime-part))
                     (cons name mime-part))))))
      ((string-equal "form-data" disposition)
       (ucw.backend.dribble "Grabbing mime-part data as string.")
       (setf (rfc2388:content mime-part) (make-array 10
                                                     :element-type '(unsigned-byte 8)
                                                     :adjustable t
                                                     :fill-pointer 0))
       (let ((counter 0))
         (values (lambda (byte)
                   (ucw.backend.dribble "Form-data byte ~4,'0D: ~D~:[~; (~C)~]."
					counter byte (<= 32 byte 127)
					(code-char byte))
                   (incf counter)
                   (vector-push-extend byte (rfc2388:content mime-part)))
                 (lambda (mime-part)
		   (let ((content (octets-to-string (rfc2388:content mime-part) (or (external-format-for :url) :us-ascii))))
		     (ucw.backend.dribble "Done with form-data ~S: ~S" name content)
		     (cons name content))))))
      (t
       (error "Don't know how to handle the mime-part ~S (disposition: ~S)"
              mime-part header)))))

(defun parse-request-body (stream raw-content-length raw-content-type)
  (if (and raw-content-length raw-content-type)
      (let ((content-length (parse-integer raw-content-length :junk-allowed t)))
        (unless (zerop content-length)
          (multiple-value-bind (content-type attributes) (rfc2388:parse-header-value raw-content-type)
            (eswitch (content-type :test #'string=)
              ("application/x-www-form-urlencoded"
               (let ((buffer (make-array content-length :element-type '(unsigned-byte 8))))
                 (read-sequence buffer stream)
                 (parse-query-parameters
                  (aif (cdr (assoc "charset" attributes :test #'string=))
                       (eswitch (it :test #'string=)
                         ("ASCII" (octets-to-string buffer :us-ascii))
                         ("UTF-8" (octets-to-string buffer :utf-8)))
                       (octets-to-string buffer :iso-8859-1)))))
              ("multipart/form-data"
               (let ((boundary (cdr (assoc "boundary" attributes :test #'string=))))
                 (rfc2388:read-mime stream boundary #'rfc2388-callback)))))))
      (ucw.backend.debug "Skipped parsing request body, Content-Type is '~S', Content-Length is '~S'"
                         raw-content-length raw-content-type)))

(defmethod mime-part-body ((mime-part rfc2388:mime-part))
  (rfc2388:content mime-part))

(defmethod mime-part-headers ((mime-part rfc2388:mime-part))
  (mapcar (lambda (header)
            (cons (rfc2388:header-name header)
                  (rfc2388:header-value header)))
          (rfc2388:headers mime-part)))

(defmethod encoding ((response response))
  (or (awhen (and (not (eq *context* :unbound))
                  (context.application *context*))
        (application.charset it))
      (external-format-for :http)
      :iso-8859-1))

;; Copyright (c) 2003-2006 Edward Marco Baringer
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
