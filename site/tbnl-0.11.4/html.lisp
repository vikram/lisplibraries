;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TBNL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/html.lisp,v 1.31 2006/09/20 19:37:45 edi Exp $

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

(let ((scanner-hash (make-hash-table :test #'equal)))
  (defun scanner-for-get-param (param-name)
    "Returns a CL-PPCRE scanner which matches a GET parameter in a
URL. Scanners are memoized in SCANNER-HASH once they are created."
    (or (gethash param-name scanner-hash)
        (setf (gethash param-name scanner-hash)
                (cl-ppcre:create-scanner
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
VALUE. If REPLACE-AMPERSANDS-P is true all literal ampersands in URL
are replaced with '&amp;'. The resulting URL is returned."
    (unless url
      ;; see URL-REWRITE:*URL-REWRITE-FILL-TAGS*
      (setq url (request-uri *request*)))
    (setq url
            (cl-ppcre:regex-replace-all (scanner-for-get-param cookie-name)
                                        url "\\1"))
    (when value
      (setq url (format nil "~A~:[?~;&~]~A=~A"
                        url 
                        (find #\? url)
                        cookie-name
                        (url-rewrite:url-encode value))))
    (when replace-ampersands-p
      (setq url (cl-ppcre:regex-replace-all "&" url "&amp;")))
    url))

(defun maybe-rewrite-urls-for-session (html &key (cookie-name *session-cookie-name*)
                                                 (value (session-cookie-value)))
  "Rewrites the HTML page HTML such that the name/value pair
COOKIE-NAME/COOKIE-VALUE is inserted if the client hasn't sent a
cookie of the same name but only if *REWRITE-FOR-SESSION-URLS* is
true. See the docs for URL-REWRITE:REWRITE-URLS."
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

(defun default-dispatcher (request)
  "Default dispatch function which handles every request with the
function stored in *DEFAULT-HANDLER*."
  (declare (ignore request))
  *default-handler*)

#-:lispworks
(defun address-string ()
  "Returns a string with information about TBNL and the front-end
suitable for inclusion in HTML output."
  (format nil "<address>~A<a href='http://weitz.de/tbnl/'>TBNL ~A</a> <a href='~A'>(~A ~A)</a> at ~A (Port ~D)</address>"
          (tbnl-info-string)
          *tbnl-version*
          +implementation-link+
          (lisp-implementation-type)
          (lisp-implementation-version)
          (host *request*)
          (server-port)))

(defun default-handler ()
  "The handler that is supposed to serve the request if no other
handler is called."
  (declare (notinline address-string))
  (log-message :info "Default handler called for script ~A" (script-name))
  (format nil "<html><head><title>TBNL</title></head><body><h2>TBNL Default Page</h2><p>This the TBNL default page. You're most likely seeing it because the server administrator hasn't set up his own default page yet.</p><p>TBNL is a toolkit for building dynamic websites with <a href='http://www.lisp.org/'>Common Lisp</a>.  More info about TBNL can be found at <a href='http://weitz.de/tbnl/'>http://weitz.de/tbnl/</a>.</p></p><p><hr>~A</p></body></html>"
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
  (let ((scanner (cl-ppcre:create-scanner regex)))
    (lambda (request)
      (and (cl-ppcre:scan scanner (script-name request))
           page-function))))

(defun handle-static-file (path &optional content-type)
  "A function which acts like a TBNL handler for the file denoted by
PATH.  Send a content type header corresponding to CONTENT-TYPE or
\(if that is NIL) tries to determine the content type via the file's
suffix."
  (unless (or (pathname-name path)
              (pathname-type path))
    ;; not a file
    (setf (return-code) +http-bad-request+)
    (throw 'tbnl-handler-done nil))
  (unless (probe-file path)
    ;; does not exist
    (setf (return-code) +http-not-found+)
    (throw 'tbnl-handler-done nil))
  (let ((buf (make-array +buffer-length+
                         :element-type #+:tbnl-bivalent-streams '(unsigned-byte 8)
                                       #-:tbnl-bivalent-streams 'character))
        (time (or (file-write-date path) (get-universal-time))))
    (setf (content-type) (or content-type
                             (mime-type path)
                             *default-content-type*))
    (handle-if-modified-since time)
    (setf (header-out "Last-Modified") (rfc-1123-date time))
    (let ((out (send-headers)))
      (with-open-file (file path
                            :direction :input
                            :element-type #+:tbnl-bivalent-streams '(unsigned-byte 8)
                                          #-:tbnl-bivalent-streams 'character
                            :if-does-not-exist nil)
        (loop for pos = (read-sequence buf file)
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

(defun create-folder-dispatcher-and-handler (uri-prefix base-path &optional default-content-type)
  "Creates and returns a dispatch function which will dispatch to a
handler function which emits the file relative to BASE-PATH that is
denoted by the URI of the request relative to URI-PREFIX.  URI-PREFIX
must be a string ending with a slash, BASE-PATH must be a pathname
designator for an existing directory.  The content types of the files
are determined via their suffix, falling back to DEFAULT-CONTENT-TYPE
if the suffix is unknown."
  (unless (and (stringp uri-prefix)
               (plusp (length uri-prefix))
               (char= (char uri-prefix (1- (length uri-prefix))) #\/))
    (error "~S must be string ending with a slash." uri-prefix))
  (when (or (pathname-name base-path)
            (pathname-type base-path))
    (error "~S is supposed to denote a directory." base-path))
  (flet ((handler ()
           (let* ((script-path (enough-namestring (cl-ppcre:regex-replace-all "\\\\"
                                                                              (script-name)
                                                                              "/")
                                                  uri-prefix))
                  (script-path-directory (pathname-directory script-path)))
             (unless (or (stringp script-path-directory)
                         (null script-path-directory)
                         (and (listp script-path-directory)
                              (eq (first script-path-directory) :relative)
                              (loop for component in (rest script-path-directory)
                                    always (stringp component))))
               (setf (return-code) +http-forbidden+)
               (throw 'tbnl-handler-done nil))
             (let ((*default-content-type* (or default-content-type
                                               *default-content-type*)))
               (handle-static-file (merge-pathnames script-path base-path))))))
    (create-prefix-dispatcher uri-prefix #'handler)))
             