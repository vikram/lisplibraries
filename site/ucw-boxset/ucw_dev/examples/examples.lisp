;;;; -*- lisp -*-

(in-package :it.bese.ucw-user)

;;;; The definiton of the example application

(defvar *example-application*
  (make-instance 'cookie-session-application
                 :url-prefix "/"
                 :tal-generator (make-instance 'yaclml:file-system-generator
                                               :cachep t
                                               :root-directories (list *ucw-tal-root*))
                 :www-roots (list (merge-pathnames "./" *ucw-tal-root*))
                 :debug-on-error t
                 :dispatchers (list (action-dispatcher)
                                    (simple-dispatcher "mul.ucw"
                                      (mul-table-example))
                                    (minimal-dispatcher "mul-direct.ucw"
				      (direct-mul-table-example)))))

;;;; define the window component

(defcomponent example-window (simple-window-component)
  ((body :component
         (switching-container
          :current-component-key 'example-welcome
          :contents
          `((example-welcome .       ,(make-instance 'example-welcome))
            (counter .               ,(make-instance 'counter))
            (transaction-example .   ,(make-instance 'transaction-example))
            (example-form .          ,(make-instance 'example-form))
            (dynamic-form .          ,(make-instance 'dynamic-form))
            (sum .                   ,(make-instance 'sum))
            (file-upload-example .   ,(make-instance 'file-upload-example))
            (timeout-cache-example . ,(make-instance 'timeout-cache-example :timeout 10))
            (hits-cache-example .    ,(make-instance 'hits-cache-example :timeout 5))))
         :accessor example-window.body))
  (:default-initargs :title "UCW Examples" :stylesheet (list "/ucw/ucw.css" "/ucw/examples/examples.css")
                     :content-type "text/html; charset=utf-8;"
                     :javascript '((:src "dojo.js")
                                   (:js (dojo.require "dojo.event.*"))))
  (:documentation "The main window component for the example application.

This component contains the list of all the available components
and simply wraps the rendering of the current component with the
navigation bar."))

(defentry-point "^(index.ucw|)$" (:application *example-application*
                                  :class regexp-dispatcher)
    ()
  (call 'example-window))

(defmethod render ((app example-window))
  (<:div :id "option-menu"
    (flet ((example-link (name text)
	     (<ucw:a :action (switch-component (example-window.body app) name)
               (<:as-html text))))
      (<:ul
        (<:li (example-link 'counter "Counter"))
	(<:li (example-link 'transaction-example "Component Transaction"))
	(<:li (example-link 'example-form "Form demo"))
	(<:li (example-link 'dynamic-form "Dynamic Form demo"))
        (<:li (example-link 'file-upload-example "Form upload example"))
	(<:li (example-link 'sum "Add some numbers."))
        (<:li (example-link 'timeout-cache-example "Timeout cache"))
        (<:li (example-link 'hits-cache-example "Hits cache"))
	(<:li (<ucw:a :href "error.ucw" "Signal an error"))
        (<:li (<ucw:a :href "error-render.ucw" "Signal a render'ing error."))
        (<:li (<:a :href "/admin/index.ucw" "UCW web-admin" ))
        (<:li (<ucw:a :action (jump 'example-window)
                "Start over.")))))
  (<:h1 "UCW Examples")
  (render (example-window.body app)))

;;;; the welcome page

(defcomponent example-welcome (widget-component)
  ()
  (:documentation "The first page seen by the example app. This
component does nothing other than render a litte introductory
text.")
  (:render ()
    (<:p (<:as-html "Click on a link to try a demo."))
    (<:p (<:as-html "You can find the admin application ")
         (<:a :href "/admin/index.ucw" "here")
         (<:as-html ". The default login is admin/admin."))
    (<:p (<:as-html "If the l10n examples are loaded, they are ")
         (<:a :href "/l10n/" "here")
         (<:as-html "."))))

;;;; the transaction demo

(defcomponent transaction-example (transaction-mixin task-component widget-component)
  ()
  (:documentation "An example use of UCW component transactions."))

(defaction start ((ex transaction-example))
  (loop
     (call 'info-message :message "Before start of the transaction.")
     (open-transaction ex)
     (call 'info-message :message "In outer transaction, step 1")
     (call 'info-message :message "In outer transaction, step 2")
     (close-transaction ex)
     (call 'info-message :message "Done." :ok-text "Go to final message.")
     (call 'info-message :message "After transaction." :ok-text "Restart the transaction.")))

;;;; error related examples


(defentry-point "error.ucw" (:application *example-application*)
    ()
  (call 'inexistent-component-foobar))

(defentry-point "error-render.ucw" (:application *example-application*)
    ()
  (call 'error-during-render-component))

(defcomponent error-during-render-component ()
  ()
  (:render (self)
           (error "Error.")))

(defun mul-table-example ()
  (with-yaclml-stream (html-stream (context.response *context*))
    (with-request-params (n) (context.request *context*)
      (let ((n (if n
                   (or (parse-integer n :junk-allowed t)
                       0)
                   0)))
        (<:html
         (<:head (<:title "Multiplication table"))
         (<:body
          (<:h1 "Multiplication table upto " (<:ah n))
          (<:form :action "" :method "GET"
                  (<:p "N: " (<:input :type "text" :name "n") (<:input :type "submit" :value "Calculate")))
          (<:table
           (<:tr
            (<:th)
            (loop
               for i from 1 to n
               do (<:th (<:ah i))))
           (loop
              for i from 1 to n
              do (<:tr
                  (<:th (<:ah i))
                  (loop
                     for j from 1 to n
                     do (<:td (<:ah (* i j)))))))))))))

(defun direct-mul-table-example ()
  ;; just like the above example but write directly to the client
  ;; stream. you can usually tell the difference if N is large.
  (send-headers (context.response *context*))
  (with-request-params (n) (context.request *context*)
    (let ((n (if n
                 (or (parse-integer n :junk-allowed t)
                     0)
                 0)))
      (flet ((send-string (&rest strings)
               (let ((network-stream (ucw::network-stream (context.response *context*))))
                 (dolist (string strings)
                   (write-sequence (string-to-octets (if (stringp string)
                                                         string
                                                         (princ-to-string string))
                                                     :us-ascii)
                                   network-stream))
                 (write-sequence +CR-LF+ network-stream))))
        ;; we can't use YACLML here since the respons'se network stream is an (unsigned-byte 8) stream.
        (send-string "<html>")
        (send-string "<head><title>Multiplication table</title></head>")
        (send-string "<body>")
        (send-string "<h1>Multiplication table upto " n "</h1>")
        (send-string "<form action=\"\">")
        (send-string "<p>N: <input type=\"text\" name=\"n\"><input type=\"submit\" value=\"Calculate\"></p> ")
        (send-string "</form>")
        (send-string "<table>")
        (send-string "<tr>")
        (send-string "<th></th>")
        (loop
           for i from 1 to n
           do (send-string "<th>" i "</th>"))
        (send-string "</tr>")
        (loop
           for i from 1 to n
           do (send-string "<tr>")
           do (send-string "<th>" i "</th>")
           do (loop
                 for j from 1 to n
                 do (send-string "<td>" (* i j) "</td>"))
           do (send-string "</tr>"))
        (send-string "</table>")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003-2005 Edward Marco Baringer
;;; All rights reserved. 
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;; 
;;;  - Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;;  - Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;    of its contributors may be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
