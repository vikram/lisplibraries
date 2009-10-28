;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/misc.lisp,v 1.17 2008/03/17 11:40:25 edi Exp $

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

(let ((scanner-hash (make-hash-table :test #'equal)))
  (defun scanner-for-get-param (param-name)
    "Returns a CL-PPCRE scanner which matches a GET parameter in a
URL.  Scanners are memoized in SCANNER-HASH once they are created."
    (or (gethash param-name scanner-hash)
        (setf (gethash param-name scanner-hash)
                (create-scanner
                 `(:alternation
                   ;; session=value at end of URL
                   (:sequence
                    (:char-class #\? #\&)
                    ,param-name
                    #\=
                    (:greedy-repetition 0 nil (:inverted-char-class #\&))
                    :end-anchor)
                   ;; session=value with other parameters following
                   (:sequence
                    (:register (:char-class #\? #\&))
                    ,param-name
                    #\=
                    (:greedy-repetition 0 nil (:inverted-char-class #\&))
                    #\&))))))
  (defun add-cookie-value-to-url (url &key (cookie-name *session-cookie-name*)
                                           (value (session-cookie-value))
                                           (replace-ampersands-p t))
    "Removes all GET parameters named COOKIE-NAME from URL and then
adds a new GET parameter with the name COOKIE-NAME and the value
VALUE.  If REPLACE-AMPERSANDS-P is true all literal ampersands in URL
are replaced with '&amp;'. The resulting URL is returned."
    (unless url
      ;; see URL-REWRITE:*URL-REWRITE-FILL-TAGS*
      (setq url (request-uri *request*)))
    (setq url (regex-replace-all (scanner-for-get-param cookie-name) url "\\1"))
    (when value
      (setq url (format nil "~A~:[?~;&~]~A=~A"
                        url 
                        (find #\? url)
                        cookie-name
                        (url-encode value))))
    (when replace-ampersands-p
      (setq url (regex-replace-all "&" url "&amp;")))
    url))

(defun maybe-rewrite-urls-for-session (html &key (cookie-name *session-cookie-name*)
                                                 (value (session-cookie-value)))
  "Rewrites the HTML page HTML such that the name/value pair
COOKIE-NAME/COOKIE-VALUE is inserted if the client hasn't sent a
cookie of the same name but only if *REWRITE-FOR-SESSION-URLS* is
true.  See the docs for URL-REWRITE:REWRITE-URLS."
  (cond ((or (not *rewrite-for-session-urls*)
             (null value)
             (cookie-in cookie-name))
          html)
        (t
          (with-input-from-string (*standard-input* html)
            (with-output-to-string (*standard-output*)
              (url-rewrite:rewrite-urls
               (lambda (url)
                 (add-cookie-value-to-url url
                                          :cookie-name cookie-name
                                          :value value))))))))

(defmethod dispatch-request (dispatch-table)
  "Dispatches *REQUEST* based upon rules in the DISPATCH-TABLE.
This method provides the default Hunchentoot behavior."
  (loop for dispatcher in dispatch-table
        for action = (funcall dispatcher *request*)
        when action return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

(defun default-dispatcher (request)
  "Default dispatch function which handles every request with the
function stored in *DEFAULT-HANDLER*."
  (declare (ignore request))
  *default-handler*)

(defun default-handler ()
  "The handler that is supposed to serve the request if no other
handler is called."
  (log-message :info "Default handler called for script ~A" (script-name))
  (format nil "<html><head><title>Hunchentoot</title></head><body><h2>Hunchentoot Default Page</h2><p>This is the Hunchentoot default page. You're most likely seeing it because the server administrator hasn't set up a custom default page yet.</p><p>Hunchentoot is a web server written in <a href='http://www.lisp.org/'>Common Lisp</a>.  More info about Hunchentoot can be found at <a href='http://weitz.de/hunchentoot/'>http://weitz.de/hunchentoot/</a>.</p></p><p><hr>~A</p></body></html>"
          (address-string)))

(defun create-prefix-dispatcher (prefix page-function)
  "Creates a dispatch function which will dispatch to the
function denoted by PAGE-FUNCTION if the file name of the current
request starts with the string PREFIX."
  (lambda (request)
    (let ((mismatch (mismatch (script-name request) prefix
                              :test #'char=)))
      (and (or (null mismatch)
               (>= mismatch (length prefix)))
           page-function))))

(defun create-regex-dispatcher (regex page-function)
  "Creates a dispatch function which will dispatch to the
function denoted by PAGE-FUNCTION if the file name of the current
request matches the CL-PPCRE regular expression REGEX."
  (let ((scanner (create-scanner regex)))
    (lambda (request)
      (and (scan scanner (script-name request))
           page-function))))

(defun handle-static-file (path &optional content-type)
  "A function which acts like a Hunchentoot handler for the file
denoted by PATH.  Send a content type header corresponding to
CONTENT-TYPE or \(if that is NIL) tries to determine the content
type via the file's suffix."
  (unless (and (fad:file-exists-p path)
               (not (fad:directory-exists-p path)))
    ;; does not exist
    (setf (return-code) +http-not-found+)
    (throw 'handler-done nil))
  (let ((time (or (file-write-date path) (get-universal-time))))
    (setf (content-type) (or content-type
                             (mime-type path)
                             "application/octet-stream"))
    (handle-if-modified-since time)
    (with-open-file (file path
                     :direction :input
                     :element-type 'octet
                     :if-does-not-exist nil)
      (setf (header-out "Last-Modified") (rfc-1123-date time)
            (content-length) (file-length file))
      (let ((out (send-headers)))
        #+:clisp
        (setf (flexi-stream-element-type *hunchentoot-stream*) 'octet)
        (loop with buf = (make-array +buffer-length+ :element-type 'octet)
              for pos = (read-sequence buf file)
              until (zerop pos)
              do (write-sequence buf out :end pos)
                 (finish-output out))))))

(defun create-static-file-dispatcher-and-handler (uri path &optional content-type)
  "Creates and returns a dispatch function which will dispatch to a
handler function which emits the file denoted by the pathname
designator PATH with content type CONTENT-TYPE if the SCRIPT-NAME of
the request matches the string URI.  If CONTENT-TYPE is NIL tries to
determine the content type via the file's suffix."
  ;; the dispatcher
  (lambda (request)
    (when (equal (script-name request) uri)
      ;; the handler
      (lambda ()
        (handle-static-file path content-type)))))

(defun enough-url (url url-prefix)
  "Returns the relative portion of URL relative to URL-PREFIX, similar
to what ENOUGH-NAMESTRING does for pathnames."
  (subseq url (or (mismatch url url-prefix) (length url-prefix))))

(defun create-folder-dispatcher-and-handler (uri-prefix base-path &optional content-type)
  "Creates and returns a dispatch function which will dispatch to a
handler function which emits the file relative to BASE-PATH that is
denoted by the URI of the request relative to URI-PREFIX.  URI-PREFIX
must be a string ending with a slash, BASE-PATH must be a pathname
designator for an existing directory.  If CONTENT-TYPE is not NIL,
it'll be the content type used for all files in the folder."
  (unless (and (stringp uri-prefix)
               (plusp (length uri-prefix))
               (char= (char uri-prefix (1- (length uri-prefix))) #\/))
    (error "~S must be string ending with a slash." uri-prefix))
  (when (or (pathname-name base-path)
            (pathname-type base-path))
    (error "~S is supposed to denote a directory." base-path))
  (flet ((handler ()
           (let* ((script-name (url-decode (script-name)))
                  (script-path (enough-url (regex-replace-all "\\\\" script-name "/")
                                           uri-prefix))
                  (script-path-directory (pathname-directory script-path)))
             (unless (or (stringp script-path-directory)
                         (null script-path-directory)
                         (and (listp script-path-directory)
                              (eq (first script-path-directory) :relative)
                              (loop for component in (rest script-path-directory)
                                    always (stringp component))))
               (setf (return-code) +http-forbidden+)
               (throw 'handler-done nil))
             (handle-static-file (merge-pathnames script-path base-path) content-type))))
    (create-prefix-dispatcher uri-prefix #'handler)))

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

(defun ssl-p ()
  "Whether the current connection to the client is secure."
  (cond ((server-mod-lisp-p *server*) (ssl-session-id *request*))
        (t #-:hunchentoot-no-ssl (server-ssl-certificate-file *server*)
           #+:hunchentoot-no-ssl nil)))

(defun redirect (target &key (host (host *request*) host-provided-p)
                             port
                             (protocol (if (ssl-p) :https :http))
                             (add-session-id (not (or host-provided-p
                                                      (starts-with-scheme-p target)
                                                      (cookie-in *session-cookie-name*))))
                             code)
  "Redirects the browser to TARGET which should be a string.  If
TARGET is a full URL starting with a scheme, HOST, PORT and PROTOCOL
are ignored.  Otherwise, TARGET should denote the path part of a URL,
PROTOCOL must be one of the keywords :HTTP or :HTTPS, and the URL to
redirect to will be constructed from HOST, PORT, PROTOCOL, and TARGET.
Adds a session ID if ADD-SESSION-ID is true.  If CODE is a 3xx
redirection code, it will be sent as status code.  In case of NIL, a 
302 status code will be sent to the client."
  (setf code (or code +http-moved-temporarily+))
  (check-type code (integer 300 399))
  (let ((url (if (starts-with-scheme-p target)
               target
               (format nil "~A://~A~@[:~A~]~A"
                       (ecase protocol
                         ((:http) "http")
                         ((:https) "https"))
                       (if port                         
                         (first (ppcre:split ":" (or host "")))
                         host)
                       port target))))
    (when add-session-id
      (setq url (add-cookie-value-to-url url :replace-ampersands-p nil)))
    (setf (header-out :location) url
          (return-code *reply*) code)
    (throw 'handler-done nil)))

(defun require-authorization (&optional (realm "Hunchentoot"))
  "Sends back appropriate headers to require basic HTTP authentication
\(see RFC 2617) for the realm REALM."
  (setf (header-out "WWW-Authenticate")
          (format nil "Basic realm=\"~A\"" (quote-string realm))
        (return-code *reply*)
          +http-authorization-required+)
  (throw 'handler-done nil))
