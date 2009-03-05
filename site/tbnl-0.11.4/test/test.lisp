;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TBNL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/test/test.lisp,v 1.35 2006/09/04 22:54:36 edi Exp $

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

;;; To test TBNL with Apache add something like this to your
;;; httpd.conf
;;;
;;;   LispServer 127.0.0.1 3000 "tbnl"
;;;
;;;   <Location /tbnl>
;;;     SetHandler lisp-handler
;;;   </Location>
;;;
;;; and afterwards restart Apache and evaluate
;;;
;;;   (asdf:oos 'asdf:load-op :tbnl-test)
;;;   (tbnl:start-tbnl)
;;;
;;; then point your browser at <http://localhost/tbnl/test>.  See the
;;; docs for other testing options.

(in-package #:tbnl-test)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defun tbnl-link ()
  (with-html-output (*standard-output*)
    (:a :href "http://weitz.de/tbnl/" "TBNL")))

(defun menu-link ()
  (with-html-output (*standard-output*)
    (:p (:hr
         (:a :href "/tbnl/test" "Back to menu")))))

(defmacro info-table (&rest forms)
  (let ((=value= (gensym)))
    `(with-html-output (*standard-output*)
       (:p (:table :border 1 :cellpadding 2 :cellspacing 0
            (:tr (:td :colspan 2
                  "Some Information "
                  (tbnl-link)
                  " provides about this request \(actual values may depend on front-end):"))
            ,@(loop for form in forms
                    collect `(:tr (:td :valign "top"
                                   (:pre :style "padding: 0px"
                                    (let ((*package* (find-package :cl)))
                                      (pprint ',form))))
                              (:td :valign "top"
                               (:pre :style "padding: 0px"
                                (let ((*package* (find-package :cl)))
                                  (loop for ,=value= in (multiple-value-list ,form)
                                        for first = t then nil
                                        unless first
                                        do (princ ", ")
                                        do (pprint ,=value=)))))))))
       (menu-link))))

(defun authorization-page ()
  (multiple-value-bind (user password)
      (authorization)
    (cond ((and (equal user "nanook")
                (equal password "igloo"))
           (with-html
             (:html
              (:head (:title "TBNL page with Basic Authentication"))
              (:body
               (:h2 (tbnl-link)
                " page with Basic Authentication")
               (info-table (header-in "Authorization")
                           (authorization))))))
          (t
           (require-authorization)))))

(defparameter *test-image*
  (load-time-value
   (with-open-file (in (make-pathname :name "fz" :type "jpg" :version nil
                                      :defaults (load-time-value *load-pathname*))
                       #+:tbnl-bivalent-streams :element-type
                       #+:tbnl-bivalent-streams '(unsigned-byte 8))
     (let ((image-data (make-array (file-length in)
                                   :element-type #-:tbnl-bivalent-streams 'character
                                   #+:tbnl-bivalent-streams '(unsigned-byte 8))))
       (read-sequence image-data in)
       image-data))))

(defun image-ram-page ()
  (setf (content-type)
          "image/jpeg")
  *test-image*)

(let ((count 0))
  (defun info ()
    (with-html
      (:html
       (:head (:title "TBNL Information"))
       (:body
        (:h2 (tbnl-link) " Information Page")
        (:p "This page has been called "
         (:b
          (fmt "~[~;once~;twice~:;~:*~R times~]" (incf count)))
         " since its handler was compiled.")
        (info-table (host)
                    (server-addr)
                    (server-port :as-number t)
                    (request-method :as-keyword t)
                    (script-name)
                    (query-string)
                    (get-parameters)
                    (cookies-in)
                    (remote-addr)
                    (real-remote-addr)
                    (remote-port :as-number t)
                    (user-agent)
                    (referer)
                    (request-uri)
                    (server-protocol :as-keyword t)
                    (mod-lisp-id)
                    (ssl-session-id)))))))

(defun oops ()
  (with-html
    (dotimes (i 3)
      (log-message* "Oops (default) # ~a" i))
    (log-message :emerg "Oops emergency")
    (log-message :alert "Oops alert")
    (log-message :crit "Oops critical")
    (log-message :error "Oops error")
    (log-message :warning "Oops warning")
    (log-message :notice "Oops notice")
    (log-message :info "Oops info")
    (log-message :debug "Oops debug")
    (error "An error was triggered on purpose. Check your ~
Apache error log. Up to 12 messages where logged depending on ~
the Apache log level set in httpd.conf.")
    (:html
     (:body "You'll never see this sentence..."))))

(defun redir ()
  (redirect "/tbnl/test/info.html?redirected=1"))

(defun forbidden ()
  (setf (return-code *reply*) +http-forbidden+)
  nil)

(defun cookie-test ()
  (set-cookie "pumpkin" :value "barking")
  (no-cache)
  (with-html
    (:html
     (:head (:title "TBNL Cookie test"))
     (:body
      (:h2 (tbnl-link)
       " Cookie test")
      (:p "You might have to reload this page to see the cookie value.")
      (info-table (cookie-in "pumpkin")
                  (mapcar #'car (cookies-in)))))))

(defun session-test ()
  (let ((new-foo-value (post-parameter "new-foo-value")))
    (when new-foo-value
      (setf (session-value 'foo) new-foo-value)))
  (let ((new-bar-value (post-parameter "new-bar-value")))
    (when new-bar-value
      (setf (session-value 'bar) new-bar-value)))
  (no-cache)
  (with-html
    (:html
     (:head (:title "TBNL Session test"))
     (:body
      (:h2 (tbnl-link)
       " Session test")
      (:p "Use the forms below to set new values for "
       (:code "FOO")
       " or "
       (:code "BAR")
       ". You can later return to this page to check if
they're still set. Also, try to use another browser at the same
time or try with cookies disabled.")
      (:p (:form :method :post
           "New value for "
           (:code "FOO")
           ": "
           (:input :type :text
            :name "new-foo-value"
            :value (or (session-value 'foo) ""))))
      (:p (:form :method :post
           "New value for "
           (:code "BAR")
           ": "
           (:input :type :text
            :name "new-bar-value"
            :value (or (session-value 'bar) ""))))
      (info-table *session-cookie-name*
                  (cookie-in *session-cookie-name*)
                  (mapcar #'car (cookies-in))
                  (session-value 'foo)
                  (session-value 'bar))))))

(defun parameter-test (&key (method :get) (charset :iso-8859-1))
  (no-cache)
  (recompute-request-parameters :external-format (case charset
                                                   (:iso-8859-1 +latin-1+)
                                                   (:utf-8 +utf-8+)))
  (setf (content-type)
        (format nil "text/html; charset=~A" charset))
  (with-html
    (:html
     (:head (:title (fmt "TBNL ~A parameter Test" method)))
     (:body
      (:h2 (tbnl-link)
       (fmt " ~A parameter Test with charset ~A" method charset))
      (:p "Enter some non-ASCII characters in the input field below
and see what's happening.  Check URL-DECODE and OCTETS-TO-STRING in "
       (:code "util.lisp")
       " to see if this test makes sense for your Lisp.")
      (:p (:form :method method
           "Enter a value: "
           (:input :type :text
            :name "foo")))
      (case method
        (:get (info-table (query-string)
                          (map 'list #'char-code (get-parameter "foo"))
                          (escape-string-all (get-parameter "foo"))))
        (:post (info-table (raw-post-data)
                           (map 'list #'char-code (post-parameter "foo"))
                           (escape-string-all (post-parameter "foo")))))))))

(defun parameter-test-latin1-get ()
  (parameter-test :method :get :charset :iso-8859-1))

(defun parameter-test-latin1-post ()
  (parameter-test :method :post :charset :iso-8859-1))

(defun parameter-test-utf8-get ()
  (parameter-test :method :get :charset :utf-8))

(defun parameter-test-utf8-post ()
  (parameter-test :method :post :charset :utf-8))

;; this should not be the same directory as *TMP-DIRECTORY* and it
;; should be initially empty (or non-existent)
(defvar *tmp-test-directory*
    #+(or :win32 :mswindows) #p"c:\\tbnl-temp\\test\\"
    #-(or :win32 :mswindows) #p"/tmp/tbnl/test/")

(defvar *tmp-test-files* nil)

(let ((counter 0))
  (defun handle-file (post-parameter)
    (when (and post-parameter
               (listp post-parameter))
      (destructuring-bind (path file-name content-type)
          post-parameter
        (let ((new-path (make-pathname :name (format nil "tbnl-test-~A"
                                                     (incf counter))
                                       :type nil
                                       :defaults *tmp-test-directory*)))
          ;; strip directory info sent by Windows browsers
          (when (search "Windows" (user-agent) :test #'char-equal)
            (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
          (rename-file path (ensure-directories-exist new-path))
          (push (list new-path file-name content-type) *tmp-test-files*))))))

(defun clean-tmp-dir ()
  (loop for (path . nil) in *tmp-test-files*
        when (probe-file path)
        do (ignore-errors (delete-file path)))
  (setq *tmp-test-files* nil))

(defun upload-test ()
  (let (post-parameter-p)
    (when (post-parameter "file1")
      (handle-file (post-parameter "file1"))
      (setq post-parameter-p t))
    (when (post-parameter "file2")
      (handle-file (post-parameter "file2"))
      (setq post-parameter-p t))
    (when (post-parameter "clean")
      (clean-tmp-dir)
      (setq post-parameter-p t))
    (when post-parameter-p
      ;; redirect so user can safely use 'Back' button
      (redirect (script-name))))
  (no-cache)
  (with-html
    (:html
     (:head (:title "TBNL file upload test"))
     (:body
      (:h2 (tbnl-link)
       " File upload test")
      (:form :method :post :enctype "multipart/form-data"
       (:p "First file: "
        (:input :type :file
         :name "file1"))
       (:p "Second file: "
        (:input :type :file
         :name "file2"))
       (:p (:input :type :submit)))
      (when *tmp-test-files*
        (htm
         (:p
          (:table :border 1 :cellpadding 2 :cellspacing 0
           (:tr (:td :colspan 3 (:b "Uploaded files")))
           (loop for (path file-name nil) in *tmp-test-files*
                 for counter from 1
                 do (htm
                     (:tr (:td :align "right" (str counter))
                      (:td (:a :href (format nil "files/~A?path=~A"
                                             (url-encode file-name)
                                             (url-encode (namestring path)))
                            (esc file-name)))
                      (:td :align "right"
                       (str (ignore-errors
                              (with-open-file (in path)
                                (file-length in))))
                       "&nbsp;Bytes"))))))
         (:form :method :post
          (:p (:input :type :submit :name "clean" :value "Delete uploaded files")))))
      (menu-link)))))

(defun send-file ()
  (let* ((path (get-parameter "path"))
         (file-info (and path
                         (find (pathname path) *tmp-test-files*
                               :key #'first :test #'equal))))
    (unless file-info
      (setf (return-code *reply*)
            +http-not-found+)
      (return-from send-file))
    (handle-static-file path (third file-info))))

(defparameter *headline*
  (load-time-value              
   (format nil "TBNL Test Menu (see file <code>~A</code>)"
           (merge-pathnames (make-pathname :type "lisp")
                            *load-truename*))))

#+:lispworks
(defun read-utf-8-file (pathspec)
  (coerce
   (external-format:encode-lisp-string
    (with-output-to-string (out nil :element-type 'lw:simple-char)
      (with-open-file (in pathspec
                          :element-type 'lw:simple-char
                          :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              do (write-line line out))))
    :utf-8)
   '(simple-array (unsigned-byte 8) (*))))

#+:lispworks
(defun utf-8 ()
  (setf (content-type)
        "text/html; charset=utf-8")
  ;; demo file stolen from <http://www.w3.org/2001/06/utf-8-test/>
  (read-utf-8-file (load-time-value
                    (merge-pathnames "UTF-8-demo.html"
                                     *load-truename*))))


#+:tbnl-bivalent-streams
(defun stream-direct ()
  (setf (content-type)
        "text/html; charset=utf-8")
  (let ((stream (send-headers))
        (buffer (make-array 1024 :element-type '(unsigned-byte 8))))
    (with-open-file (in (load-time-value
                         (merge-pathnames "UTF-8-demo.html"
                                          *load-truename*))
                        :element-type '(unsigned-byte 8))
      (loop for pos = (read-sequence buffer in)
            until (zerop pos) 
            do (write-sequence buffer stream :end pos)))))

(define-easy-handler (easy-demo :uri "/tbnl/test/easy-demo.html"
                                :default-request-type :post)
    (first-name last-name
                (age :parameter-type 'integer)
                (implementation :parameter-type 'keyword)
                (meal :parameter-type '(hash-table boolean))
                (team :parameter-type 'list))
  (with-html
    (:html
     (:head (:title "TBNL \"easy\" handler example"))
     (:body
      (:h2 (tbnl-link)
       " \"Easy\" handler example")
      (:p (:form :method :post
           (:table :border 1 :cellpadding 2 :cellspacing 0
            (:tr
             (:td "First Name:")
             (:td (:input :type :text
                   :name "first-name"
                   :value (or first-name "Donald"))))
            (:tr
             (:td "Last name:")
             (:td (:input :type :text
                   :name "last-name"
                   :value (or last-name "Duck"))))
            (:tr
             (:td "Age:")
             (:td (:input :type :text
                   :name "age"
                   :value (or age 42))))
            (:tr
             (:td "Implementation:")
             (:td (:select :name "implementation"
                   (loop for (value option) in '((:lispworks "LispWorks")
                                                 (:allegro "AllegroCL")
                                                 (:cmu "CMUCL")
                                                 (:sbcl "SBCL")
                                                 (:clisp "CLISP"))
                         do (htm
                             (:option :value value
                              :selected (eq value implementation)
                              (str option)))))))
            (:tr
             (:td :valign :top "Meal:")
             (:td (loop for choice in '("Burnt weeny sandwich"
                                        "Canard du jour"
                                        "Easy meat"
                                        "Muffin"
                                        "Twenty small cigars"
                                        "Yellow snow")
                        do (htm
                            (:input :type "checkbox"
                             :name (format nil "meal{~A}" choice)
                             :checked (gethash choice meal)
                             (esc choice))
                            (:br)))))
            (:tr
             (:td :valign :top "Team:")
             (:td (loop for player in '("Beckenbauer"
                                        "Cruyff"
                                        "Maradona"
                                        ;; without accent (for SBCL)
                                        "Pele"
                                        "Zidane")
                        do (htm
                            (:input :type "checkbox"
                             :name "team"
                             :value player
                             :checked (member player team :test #'string=)
                             (esc player))
                            (:br)))))
            (:tr
             (:td :colspan 2
              (:input :type "submit"))))))
      (info-table first-name
                  last-name
                  age
                  implementation
                  (loop :for choice :being :the :hash-keys :of meal :collect choice)
                  (gethash "Yellow snow" meal)
                  team)))))
                

(defun menu ()
  (with-html
    (:html
     (:head
      (:title "TBNL Test Menu"))
     (:body
      (:h2 (str *headline*))
      (:table :border 0 :cellspacing 4 :cellpadding 4
       (:tr (:td (:a :href "/tbnl/test/info.html?foo=bar"
                  "Info provided by TBNL")))
       (:tr (:td (:a :href "/tbnl/test/cookie.html"
                  "Cookie test")))
       (:tr (:td (:a :href "/tbnl/test/session.html"
                  "Session test")))
       (:tr (:td (:a :href "/tbnl/test/parameter_latin1_get.html"
                  "GET parameter handling with LATIN-1 charset")))
       (:tr (:td (:a :href "/tbnl/test/parameter_latin1_post.html"
                  "POST parameter handling with LATIN-1 charset")))
       (:tr (:td (:a :href "/tbnl/test/parameter_utf8_get.html"
                  "GET parameter handling with UTF-8 charset")))
       (:tr (:td (:a :href "/tbnl/test/parameter_utf8_post.html"
                  "POST parameter handling with UTF-8 charset")))
       (:tr (:td (:a :href "/tbnl/test/redir.html"
                  "Redirect \(302) to info page above")))
       (:tr (:td (:a :href "/tbnl/test/authorization.html"
                  "Authorization")
             " (user 'nanook', password 'igloo')"))
       (:tr (:td (:a :href "/tbnl/code/test.lisp"
                  "The source code of this test")))
       (:tr (:td (:a :href "/tbnl/test/image.jpg"
                  "Binary data, delivered from file")
             " \(a picture)"))
       (:tr (:td (:a :href "/tbnl/test/image-ram.jpg"
                  "Binary data, delivered from RAM")
             " \(same picture)"))
       (:tr (:td (:a :href "/tbnl/test/easy-demo.html"
                  "\"Easy\" handler example")))
       #+:lispworks
       (:tr (:td (:a :href "/tbnl/test/utf-8.html"
                  "UTF-8 demo")
             " \(currently Lispworks-only)"))
       #+:tbnl-bivalent-streams
       (:tr (:td (:a :href "/tbnl/test/umlaut.txt"
                  "UTF-8 demo")
             " \(writing directly to the stream)"))
       (:tr (:td (:a :href "/tbnl/test/upload.html"
                  "File uploads")))
       (:tr (:td (:a :href "/tbnl/test/forbidden.html"
                  "Forbidden \(403) page")))
       (:tr (:td (:a :href "/tbnl/test/oops.html"
                  "Error handling")
             " \(output depends on settings like "
             (:a :href "http://weitz.de/tbnl/#*show-lisp-errors-p*"
              (:code "*SHOW-LISP-ERRORS-P*"))
             (fmt " \(currently ~S) and " *show-lisp-errors-p*)
             (:a :href "http://weitz.de/tbnl/#*show-lisp-backtraces-p*"
              (:code "*SHOW-LISP-BACKTRACES-P*"))
             (fmt " \(currently ~S)" *show-lisp-backtraces-p*)
             ")"))
       (:tr (:td (:a :href "/tbnl/foo"
                  "URI handled by")
             " "
             (:a :href "http://weitz.de/tbnl/#*default-handler*"
              (:code "*DEFAULT-HANDLER*")))))))))

(setq *dispatch-table*
      (nconc
       (list 'dispatch-easy-handlers
             (create-static-file-dispatcher-and-handler
              "/tbnl/test/image.jpg"
              (make-pathname :name "fz" :type "jpg" :version nil
                             :defaults (load-time-value *load-pathname*))
              "image/jpeg")
             (create-folder-dispatcher-and-handler
              "/tbnl/code/"
              (make-pathname :name nil :type nil :version nil
                             :defaults (load-time-value *load-pathname*))
              "text/plain"))
       (mapcar (lambda (args)
                 (apply #'create-prefix-dispatcher args))
               '(("/tbnl/test/form-test.html" form-test)
                 ("/tbnl/test/forbidden.html" forbidden)
                 ("/tbnl/test/info.html" info)
                 ("/tbnl/test/authorization.html" authorization-page)
                 ("/tbnl/test/image-ram.jpg" image-ram-page)
                 ("/tbnl/test/cookie.html" cookie-test)
                 ("/tbnl/test/session.html" session-test)
                 ("/tbnl/test/parameter_latin1_get.html" parameter-test-latin1-get)
                 ("/tbnl/test/parameter_latin1_post.html" parameter-test-latin1-post)
                 ("/tbnl/test/parameter_utf8_get.html" parameter-test-utf8-get)
                 ("/tbnl/test/parameter_utf8_post.html" parameter-test-utf8-post)
                 ("/tbnl/test/upload.html" upload-test)
                 ("/tbnl/test/redir.html" redir)
                 ("/tbnl/test/oops.html" oops)
                 #+:lispworks
                 ("/tbnl/test/utf-8.html" utf-8)
                 #+:tbnl-bivalent-streams
                 ("/tbnl/test/umlaut.txt" stream-direct)
                 ("/tbnl/test/files/" send-file)
                 ("/tbnl/test" menu)))
       (list #'default-dispatcher)))
