;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TBNL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/modlisp.lisp,v 1.69 2006/09/14 19:54:32 edi Exp $

;;; Copyright (c) 2004-2006, Dr. Edmund Weitz.  All rights reserved.

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

(defun write-header-line (key value)
  "Accepts a KEY and a VALUE and writes them, one line at a time,
to the mod_lisp or HTTP/Araneida socket stream."
  (cond (*use-modlisp-headers*
         (write-header-line/modlisp key value))
        (t (write-header-line/http key value))))

(defun write-header-line/modlisp (key value)
  "Accepts a KEY and a VALUE and writes them, one line at a time,
to the mod_lisp socket stream"
  (write-string key *tbnl-stream*)
  (write-char #\NewLine *tbnl-stream*)
  ;; remove line breaks which would confuse mod_lisp
  (format *tbnl-stream* "~A"
          (cl-ppcre:regex-replace-all "[\\r\\n]" value " "))
  (write-char #\NewLine *tbnl-stream*))

(defun write-header-line/http (key value)
  "Accepts a KEY and a VALUE and writes them, one line at a time,
to the http/Araneida socket stream"
  (format *tbnl-stream*
          (cond ((string= "Status" key)
                 (load-time-value (formatter "HTTP/1.1 ~*~A")))
                (t (load-time-value (formatter "~A: ~A"))))
          key
          (cl-ppcre:regex-replace-all "[\\r\\n]" value " "))
  (write-char #\Return *tbnl-stream*)
  (write-char #\NewLine *tbnl-stream*))

(defun start-output (&optional content)
  "Sends all headers and maybe the content body to *TBNL-STREAM*.
Handles the supported return codes accordingly.  Called by
PROCESS-REQUEST.  Returns the stream to write to."
  (declare (notinline address-string))
  (when *headers-sent*
    (return-from start-output))
  (setq *headers-sent* t)
  (let* ((return-code (return-code *reply*))
         (status-line (status-line return-code))
         (request-method (request-method))
         (head-request-p (eq request-method :head))
         content-modified-p)
    (unless status-line
      (setq content (escape-for-html
                     (format nil "Unknown http return code: ~A" return-code))
            content-modified-p t
            return-code +http-internal-server-error+
            status-line (status-line return-code)))
    (unless (member return-code '(#.+http-ok+ #.+http-not-modified+))
      ;; call error handler, if any - should return NIL if it can't
      ;; handle the error.
      (let (error-handled-p)
        (when *http-error-handler*
          (setq error-handled-p (funcall *http-error-handler* return-code)
                content (or error-handled-p content)
                content-modified-p (or content-modified-p error-handled-p)))
        ;; handle common return codes other than 200, which weren't
        ;; handled by the error handler.
        (unless error-handled-p
          (setf (content-type *reply*)
                  "text/html; charset=iso-8859-1"
                content-modified-p t
                content
                  (format nil "<html><head><title>~D ~A</title></head><body><h1>~A</h1>~A<p><hr>~A</p></body></html>"
                          return-code status-line status-line
                          (case return-code
                            ((#.+http-internal-server-error+)
                             content)
                            ((#.+http-moved-temporarily+ #.+http-moved-permanently+)
                             (format nil "The document has moved <a href='~A'>here</a>"
                                     (header-out "Location")))
                            ((#.+http-authorization-required+)
                             "The server could not verify that you are authorized to access the document requested. Either you supplied the wrong credentials \(e.g., bad password), or your browser doesn't understand how to supply the credentials required.")
                            ((#.+http-forbidden+)
                             (format nil "You don't have permission to access ~A on this server."
                                     (script-name)))
                            ((#.+http-not-found+)
                             (format nil "The requested URL ~A was not found on this server."
                                     (script-name)))
                            ((#.+http-bad-request+)
                             "Your browser sent a request that this server could not understand."))
                          (address-string))))))
    ;; access log message
    (when (and *show-access-log-messages*
               (not *use-apache-log*))
      (ignore-errors*
        (log-message nil "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] \"~A ~A~@[?~A~] ~A\" ~A ~:[~*-~;~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\""
                     (remote-addr) (header-in "X-Forwarded-For")
                     (authorization) request-method (script-name)
                     (query-string) (server-protocol)
                     return-code content (length content)
                     (referer) (user-agent))))
    ;; start with status line
    (write-header-line "Status" (format nil "~D ~A" return-code status-line))
    (let ((content-length (or (and (not content-modified-p)
                                   (content-length *reply*))
                              (length content))))
      (when (and content (plusp content-length))
        (when (starts-with-one-of-p (content-type *reply*)
                                    *content-types-for-url-rewrite*)
          ;; if the Content-Type header starts with one of the strings
          ;; in *CONTENT-TYPES-FOR-URL-REWRITE* then maybe rewrite the
          ;; content
          (setq content
                (maybe-rewrite-urls-for-session content)
                content-length
                (or (and (not content-modified-p)
                         (content-length *reply*))
                    (length content)))))
      ;; write the corresponding headers for the content
      (when (and content content-length)
        (write-header-line "Content-Length" (format nil "~D" content-length))
        (when *use-modlisp-headers*
          (write-header-line "Lisp-Content-Length"
                             (cond (head-request-p "0")
                                   (t (format nil "~D" content-length))))
          (write-header-line "Keep-Socket" "1")
          (setq *close-tbnl-stream* nil))))
    (write-header-line "Content-Type" (content-type *reply*))
    ;; write all headers from the REPLY object
    (loop for (key . value) in (headers-out *reply*)
          do (write-header-line key value))
    ;; now the cookies
    (loop for (nil . cookie) in (cookies-out *reply*)
          do (write-header-line "Set-Cookie" (stringify-cookie cookie)))
    ;; write log messages
    (when *use-modlisp-headers*
      (loop for (log-level . message) in (reverse (log-messages *reply*))
            do (write-header-line (case log-level
                                    ((:emerg) "Log-Emerg")
                                    ((:alert) "Log-Alert")
                                    ((:crit) "Log-Crit")
                                    ((:error) "Log-Error")
                                    ((:warning) "Log-Warning")
                                    ((:notice) "Log-Notice")
                                    ((:info) "Log-Info")
                                    ((:debug) "Log-Debug")
                                    (otherwise "Log"))
                                  message)))
    ;; all headers sent
    (cond (*use-modlisp-headers*
           (write-string "end" *tbnl-stream*))
          (t
           (write-char #\Return *tbnl-stream*)))
    (write-char #\NewLine *tbnl-stream*)
    ;; now optional content
    (cond ((or (null content)
               head-request-p)
           t)
          ((stringp content)
           (write-string content *tbnl-stream*)
           t)
          #+:tbnl-bivalent-streams
          ((typep content 'sequence)
           (ignore-errors*
             (write-sequence content *tbnl-stream*)))
          (t
           nil))
    *tbnl-stream*))

(defun send-headers ()
  "Sends the initial status line and all headers as determined by
the REPLY object *REPLY*.  Returns a stream to which the body of
the reply can be written.  Once this function has been called
further changes to *REPLY* don't have any effect.  Also,
automatic handling of errors \(i.e. sending the corresponding
status code to the browser, etc.) is turned off for this request.
If your handlers return the full body as a string or as an array
of octets you should NOT call this function."
  (start-output))

(defun no-cache ()
  "Adds appropriate headers to completely prevent caching on most browsers."
  (setf (header-out "Expires")
          "Mon, 26 Jul 1997 05:00:00 GMT"
        (header-out "Cache-Control")
          "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
        (header-out "Pragma")
          "no-cache"
        (header-out "Last-Modified")
          (rfc-1123-date))
  (values))

(defun redirect (script-name &key
                           (host (host *request*) host-provided-p)
                           (protocol (if (ssl-session-id *request*)
                                       :https
                                       :http))
                           (add-session-id (not (or host-provided-p
                                                    (cookie-in *session-cookie-name*))))
                           permanently)
  "Redirects the browser to the resource SCRIPT-NAME on host
HOST. PROTOCOL must be one of the keywords :HTTP or :HTTPS. Adds a
session ID if ADD-SESSION-ID is true. If PERMANENTLY is true, a 301
request is sent to the browser, otherwise a 302."
  (let ((url (format nil "~A://~A~A"
                     (ecase protocol
                       ((:http) "http")
                       ((:https) "https"))
                     host script-name)))
    (when add-session-id
      (setq url (add-cookie-value-to-url url :replace-ampersands-p nil)))
    (setf (header-out "Location")
            url
          (return-code *reply*)
            (if permanently
              +http-moved-permanently+
              +http-moved-temporarily+))
    (throw 'tbnl-handler-done nil)))

(defun require-authorization (&optional (realm "TBNL"))
  (setf (header-out "WWW-Authenticate")
          (format nil "Basic realm=\"~A\"" (quote-string realm))
        (return-code *reply*)
          +http-authorization-required+)
  (throw 'tbnl-handler-done nil))

(defun process-request (command)
  "Processes COMMAND as created by GET-REQUEST-DATA using the
corresponding user funtion from *DISPATCH-TABLE*.  Sets up REPLY,
REQUEST, and SESSION objects.  Called by LISTEN-FOR-REQUEST."
  (let (*tmp-files* *headers-sent*)
    (unwind-protect
        (let* ((*session* nil)
               ;; first create a REPLY object so we can immediately start
               ;; logging
               (*reply* (debug-value *reply* (make-instance 'reply)))
               (*request* (debug-value *request*
                                       (make-instance 'request
                                          :headers-in command)))
               backtrace)
          (multiple-value-bind (body error)
              (catch 'tbnl-handler-done
                (handler-bind ((error
                                (lambda (cond)
                                  (debug-value *error* cond)
                                  ;; only generate backtrace if needed
                                  (setq backtrace
                                        (and (or (and *show-lisp-errors-p*
                                                      *show-lisp-backtraces-p*)
                                                 (and *log-lisp-errors-p*
                                                      *log-lisp-backtraces-p*))
                                             (debug-value *backtrace*
                                                          (get-backtrace cond))))
                                  (when *log-lisp-errors-p*
                                    (log-message *lisp-errors-log-level*
                                                 "~A~:[~*~;~%~A~]"
                                                 cond
                                                 *log-lisp-backtraces-p*
                                                 backtrace))
                                  ;; if the headers were already sent
                                  ;; the error happens within the body
                                  ;; and we have to close the stream
                                  (when *headers-sent*
                                    (setq *close-tbnl-stream* t))
                                  (throw 'tbnl-handler-done
                                         (values nil cond))))
                               (warning
                                (lambda (cond)
                                  (debug-value *error* cond)
                                  (when *log-lisp-warnings-p*
                                    (log-message *lisp-warnings-log-level*
                                                 "~A~:[~*~;~%~A~]"
                                                 cond
                                                 *log-lisp-backtraces-p*
                                                 backtrace)))))
                  (with-debugger 
                    ;; skip dispatch if bad request
                    (when (eq (return-code) +http-ok+)
                      ;; read post data to clear stream
                      (raw-post-data)
                      ;; now do the work
                      (dispatch-request *dispatch-table*)))))
            (when error
              (setf (return-code *reply*)
                      +http-internal-server-error+))
            (start-output
             (debug-value *body*
                          (cond ((and error *show-lisp-errors-p*)
                                 (format nil "<pre>~A~:[~*~;~%~%~A~]</pre>"
                                         (escape-for-html (format nil "~A" error))
                                         *show-lisp-backtraces-p*
                                         (escape-for-html (format nil "~A" backtrace))))
                                (error
                                 "An error has occured")
                                (t body)))))
          t)
      (loop for path in *tmp-files*
            when (and (pathnamep path)
                      (probe-file path))
              do (ignore-errors* (delete-file path))))))

(defmethod dispatch-request (dispatch-table)
  "Dispatches *REQUEST* based upon rules in the DISPATCH-TABLE.  This
method provides the default tbnl behavior."
  (loop for dispatcher in dispatch-table
        for action = (funcall dispatcher *request*)
        when action
          return (funcall action)
        finally (setf (return-code *reply*)
                      +http-not-found+)))

(defun read-http-headers ()
  "Reads and parses HTTP headers coming from *TBNL-STREAM* and
converts them into an alist."
  (let (headers)
    (labels ((read-header-line ()
               "Reads one header line, considering continuations."
               (with-output-to-string (header-line)
                 (loop
                   (let* ((line (read-line *tbnl-stream* t))
                          (end (position #\Return line))
                          (next (and (> end 0)
                                     (peek-char nil *tbnl-stream*))))
                     (write-sequence line header-line :end end)
                     (unless (or (eql next #\Space)
                                 (eql next #\Tab))
                       (return))))))
             (split-header (line)
               "Splits line at colon and converts it into a cons."
               (unless (or (not line)
                           (zerop (length line)))
                 (destructuring-bind (key value)
                     (cl-ppcre:split ":" line :limit 2)
                   (cons (nstring-capitalize key)
                         (string-trim " " value)))))
             (add-header (pair)
               "Adds the cons PAIR to the list HEADERS of headers
which are already there.  Takes care of multiple headers with the same
key."
               (let ((existing-header (assoc (car pair) headers :test #'string-equal)))
                 (cond (existing-header
                        (setf (cdr existing-header)
                              (format nil "~A ~A"
                                      (cdr existing-header)
                                      (cdr pair))))
                       (t (push pair headers))))))
      (loop
       (let ((pair (split-header (read-header-line))))
         (unless pair
           (return))
         (add-header pair)))
      headers)))

(defun read-http-request (first-line)
  "Reads incoming HTTP request from Araneida or directly from a
browser via *TBNL-STREAM*.  Assumes the first line is already consumed
and in FIRST-LINE.  Returns an alist of the headers."
  (destructuring-bind (method url-string &optional protocol)
      (cl-ppcre:split " " first-line :limit 3)
    (let ((headers (and protocol (read-http-headers))))
      (push (cons "server-ip-port" (format nil "~A" *tbnl-port*))
            headers)
      (push (cons "method" method)
            headers)
      (push (cons "url" url-string)
            headers)
      (unless protocol
        (setq protocol "HTTP/0.9"))
      (push (cons "server-protocol"
                  (string-trim '(#\Space #\NewLine #\Return) protocol))
            headers)
      (push (cons "content-stream" *tbnl-stream*) headers)
      headers)))

(defun get-request-data ()
  "Reads incoming headers and posted content \(if any) from the
front-end or directly from the HTTP stream via *TBNL-STREAM*.  Returns
the results as an alist."
  (ignore-errors*
    (let ((first-line (read-line *tbnl-stream* nil nil)))
      (cond ((null first-line)
             ;; socket closed - return immediately
             nil)
            ((find #\Space first-line)
             ;; if the first line contains a space we know it doesn't
             ;; come from mod_lisp so we assume Araneida or no front-end
             ;; at all (or Hunchentoot in the case of LispWorks)
             (setq *use-modlisp-headers* nil)
             (let ((headers (read-http-request first-line)))
               (when (equalp (string-assoc "Expect" headers) "100-continue")
                 (write-header-line "Status" (format nil "~D ~A"
                                                     +http-continue+
                                                     (status-line +http-continue+)))
                 (write-char #\Return *tbnl-stream*)
                 (write-char #\Newline *tbnl-stream*)
                 (force-output *tbnl-stream*))
               headers))
            ;; now we assume mod_lisp, so we read alternating
            ;; key/value lines
            (t (setq *use-modlisp-headers* t)
               #+:lispworks
               (setf (stream:stream-read-timeout (raw-stream)) nil)
               (let* ((second-line (read-line *tbnl-stream* t))
                      (headers
                        (loop for key = (read-line *tbnl-stream* nil nil)
                              while (and key
                                         (string-not-equal key "end"))
                              for value = (read-line *tbnl-stream* t)
                              collect (cons key value)))
                      (content-length (string-assoc "content-length" headers)))
                 (push (cons first-line second-line)
                       headers)
                 (when content-length
                   (push (cons "content-stream" *tbnl-stream*)
                         headers))
                 headers))))))

#-:hunchentoot
(defun listen-for-request (*tbnl-stream* command-processor &rest args)
  "Listens on *TBNL-STREAM* for an incoming request.  Packages the
command using GET-REQUEST-DATA and passes it to the COMMAND-PROCESSOR
function \(which is PROCESS-REQUEST).  ARGS are ignored.  Designed to
be called by a KMRCL:LISTENER object."
  (declare (ignore args))
  (unwind-protect
      (loop for *close-tbnl-stream* = t
            for *use-modlisp-headers* = nil
            for *tbnl-socket-usage-counter* from 0
            for command = (debug-value *command* (get-request-data))
            while command
            do (cond ((ignore-errors*
                        (funcall command-processor command))
                      (handler-case*
                          (force-output *tbnl-stream*)
                        (error ()
                          (setq *close-tbnl-stream* t))))
                     (t
                      ;; if an error occured during processing of
                      ;; COMMAND we close this particular connection
                      (setq *close-tbnl-stream* t)))
            until *close-tbnl-stream*)
    (ignore-errors*
      #+:lispworks
      (close *tbnl-stream* :abort t)
      #-:lispworks
      (kmrcl:close-active-socket *tbnl-stream*))))

(defun start-tbnl ()
  "Starts listening on port *TBNL-PORT* if needed.  Initializes
*SESSION-SECRET* if needed.  Returns the newly created or already
existing listener object."
  (unless (boundp '*session-secret*)
    (reset-session-secret))
  (cond ((and (boundp '*listener*)
              *listener*
              (typep *listener*
                     #-:lispworks 'kmrcl:listener
                     #+:lispworks 'server)))
        (t
          (setq *listener*
                #-:lispworks
                (make-instance 'kmrcl:listener
                               :port *tbnl-port*
                               :base-name "tbnl"
                               :function 'listen-for-request
                               :function-args (cons 'process-request nil)
                               :format :text
                               :wait nil
                               :catch-errors t
                               :timeout nil
                               :number-fixed-workers nil
                               :remote-host-checker nil)
                #+:lispworks
                (let ((*hunchentoot-p* nil))
                  (start-server :port *tbnl-port*)))))
  #-:lispworks
  (kmrcl:init/listener *listener* :start)
  *listener*)

(defun stop-tbnl ()
  "Stops the object bound to *LISTENER* if it exists."
  (cond ((and (boundp '*listener*)
              *listener*
              (typep *listener*
                     #-:lispworks 'kmrcl:listener
                     #+:lispworks 'server))
         #-:lispworks
         (kmrcl:init/listener *listener* :stop)
         #+:lispworks
         (stop-server *listener*))
        (t
         (warn "The variable *LISTENER* is not bound to a suitable object")
         (setq *listener* nil)))
  (values))

