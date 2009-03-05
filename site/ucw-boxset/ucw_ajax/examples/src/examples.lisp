;;;; -*- lisp -*-

(in-package :it.bese.ucw-user)

;;;; The definiton of the example application

(defclass example-application (dirty-component-tracking-application-mixin cookie-session-application)
  ())

(defvar *wwwroot* (merge-pathnames #P"examples/wwwroot/"
                                   (asdf:component-pathname (asdf:find-system :ucw))))

(defvar *example-application*
  (make-instance 'example-application
                 :url-prefix "/"
                 :tal-generator (make-instance 'yaclml:file-system-generator
                                               :cachep t
                                               :root-directories (append (list (merge-pathnames #P"tal/" *wwwroot*))
                                                                         (make-standard-ucw-tal-dir-list)))
                 :debug-on-error t
                 :www-roots (append (list (cons "static/" (merge-pathnames #P"static/" *wwwroot*)))
                                    (make-standard-ucw-www-root-list))
                 :dispatchers (append (make-standard-ucw-dispatchers)
                                      (list (make-simple-dispatcher "mul.ucw"
                                              (mul-table-example))
                                            (make-minimal-dispatcher "mul-direct.ucw"
                                              (direct-mul-table-example))))))

;;;; define the window component

(defcomponent example-window (dojo-split-container simple-window-component)
  ()
  (:default-initargs
      :title "UCW Examples" :stylesheet (list "/ucw/ucw.css" "/static/examples.css")
      :dom-id "main-splitter"
      :icon "/static/favicon.ico"
      :javascript `((:js (progn
                           (dojo.require "dojo.widget.TabContainer")
                           (dojo.require "dojo.widget.DropdownDatePicker")
                           (dojo.require "dojo.widget.ContentPane")
                           (dojo.require "dojo.validate.*")
                           (dojo.require "dojo.widget.SplitContainer")))))
  (:documentation "The main window component for the example application.

This component contains the list of all the available components
and simply wraps the rendering of the current component with the
navigation bar."))

(defentry-point "^(index.ucw|)$" (:application *example-application*
                                  :class regexp-dispatcher)
    ()
  (call 'example-window))

(defmethod initialize-instance :after ((self example-window) &key &allow-other-keys)
  (let ((main-container
         (make-instance 'switching-container
                        :current-component-key 'example-welcome
                        :dom-id "main-container"
                        :contents
                        `((example-welcome .       ,(make-instance 'example-welcome))
                          (counter .               ,(make-instance 'counter))
                          (isolation-example .     ,(make-instance 'isolation-example))
                          (isolation-example-with-backtracking . ,(make-instance 'isolation-example-with-backtracking))
                          (dojo-tab-example .      ,(make-instance 'dojo-tab-example
                                                                   ;; we add a static id, so the client side cookie
                                                                   ;; for the current tab will be static
                                                                   :dom-id "top-level-tab-example"))
                          (transaction-example .   ,(make-instance 'transaction-example))
                          (example-form .          ,(make-instance 'example-form))
                          (dynamic-form .          ,(make-instance 'dynamic-form))
                          (sum .                   ,(make-instance 'sum))
                          (file-upload-example .   ,(make-instance 'file-upload-example))
                          (timeout-cache-example . ,(make-instance 'timeout-cache-example :timeout 10))
                          (hits-cache-example .    ,(make-instance 'hits-cache-example :timeout 5))))))
    (add-component self (make-instance 'dojo-content-pane :body main-container))
    (add-component self
     (make-instance 'dojo-content-pane :body
      (lambda ()
        (<:div :id "option-menu"
               (flet ((example-link (name text)
                        (<ucw:a :action-body (switch-component main-container name)
                                (<:as-html text)))
                      (example-ajax-link (name text)
                        (<ucw:a :action (register-ajax-action ()
                                          (setf (container.current-component-key main-container) name))
                                (<:as-html text))))
                 (<:ul
                  (<:li (example-ajax-link 'example-welcome "Welcome page"))
                  (<:li (example-ajax-link 'counter "Counter"))
                  (<:li (example-ajax-link 'isolation-example "Actions"))
                  (<:li (example-ajax-link 'isolation-example-with-backtracking "Actions with backtracking"))
                  (<:li (example-ajax-link 'dojo-tab-example "A dojo TabContainer"))
                  (<:li (example-ajax-link 'transaction-example "Component Transaction"))
                  (<:li (example-ajax-link 'example-form "Form demo"))
                  (<:li (example-ajax-link 'dynamic-form "Dynamic Form demo"))
                  (<:li (example-ajax-link 'file-upload-example "Form upload example"))
                  (<:li (example-ajax-link 'sum "Add some numbers."))
                  (<:li (example-link 'timeout-cache-example "Timeout cache"))
                  (<:li (example-link 'hits-cache-example "Hits cache"))
                  (<:li (<ucw:a :href "error.ucw" "Signal an error"))
                  (<:li (<ucw:a :href "error-render.ucw" "Signal a render'ing error."))
                  (<:li (<:a :href "/admin/index.ucw" "UCW web-admin" ))
                  (<:li (<ucw:a :action-body (jump 'example-window)
                                "Start over."))))))))))

(defmethod ucw::render-widget-wrapper :around ((self example-window) render-next-method)
  (call-next-method)
  (<:div :id "dojoDebug" :style "position: absolute; top: 1000px; z-index: -100"))

;;;; the welcome page

(defcomponent example-welcome (widget-component)
  ()
  (:documentation "The first page seen by the example app. This
component does nothing other than render a litte introductory
text.")
  (:render ()
    (<:h1 "UCW Examples")
    (<:p (<:as-html "Click on a link to try a demo."))
    (<:p (<:as-html "You can find the admin application ")
         (<:a :href "/admin/index.ucw" "here")
         (<:as-html ". The default login is admin/admin."))
    (<:p (<:as-html "The l10n example is ")
         (<:a :href "/l10n/" "here")
         (<:as-html "."))
    (<:p (<:as-html "The shared-counter single-frame-application example is ")
         (<:a :href "/shared-counter/" "here")
         (<:as-html "."))))

;;;; dojo tab contaianer example

(defun make-example-tab (label closablep component &rest args)
  (let ((tab (make-instance 'dojo-tab :closablep closablep)))
    (add-component tab (apply #'make-instance component args))
    (setf (ucw::label-of tab) label)
    tab))

(defcomponent dojo-tab-example (dojo-tab-container)
  ((initialized :initform nil)
   (description-id :initform (js:gen-js-name-string) :accessor description-id-of))
  (:default-initargs :contents (list (make-example-tab "Form demo" t 'example-form)
                                     (make-example-tab "Add some numbers" t 'sum))
    :do-layout-p nil :css-class "tab-example" :close-button "tab"
    :remember-selected-tab-p t))

(defmethod render :before ((self dojo-tab-example))
  ;; render a 'fake' tab with some description
  (let ((id (description-id-of self)))
    (<:div (@ :dojotype "ContentPane" :label "Description")
           :id id
           (<:p "This is a dojo widget. The cool things are:"
                (<:ol (<:li "it's downloaded with AJAX and dinamically instantiated by the client JS")
                      (<:li "the tabs are also lazy-loaded with AJAX")
                      (<:li "due to these "
                            (<ucw:a :action (register-ajax-action ()
                                              (without-dirtyness-tracking
                                                (let ((tab (make-example-tab "Recurse" t 'dojo-tab-example
                                                                             :remember-selected-tab-p nil)))
                                                  (add-component self tab)
                                                  (ajax-render-new-tab tab))))
                                    "it can contain itself recursively")
                            "... :)")
                      (<:li "as the previous link shows, you can add a new tab with AJAX (without rerendering the entite TabContainer)")
                      (<:li "some tabs are not closable")
                      (<:li "the form in the add some numbers example auto-posts its state"
                            ", so you are free to switch around without losing the value entered"
                            (<:i " (please note that switching between tabs is cached by the dojo TabContainer"
                                 " on the client side, but switching between the examples causes them to be"
                                 " freshly ajax-rerendered)"))
                      (<:li "the currently selected tab (only for the toplevel TabContainer) is stored in a cookie"))))
    ;; demonstrate how to install an on-close-handler on 'fake' dojo widgets
    (<ucw:script `(dojo.add-on-load
                   (lambda ()
                     (if-bind widget (dojo.widget.by-id ,id)
                       (setf widget.extra-args.on-close
                             (lambda ()
                               (return false)))
                       (log.error "Widget with id '" ,id "' was not found, seems like this dojo.add-on-load'ed closure was called before the widgets were instantiated from the ajax answer (is it Opera?)")))))))

;;;; isolation example

(defcomponent abstract-isolation-example (widget-component)
  ((backtracked :accessor backtrackedp :initarg :backtracked)
   (invocation-isolated-ts :initform nil :accessor invocation-isolated-ts-of)
   (unguarded-ts :initform nil :accessor unguarded-ts-of)
   (isolated-ts :initform nil :accessor isolated-ts-of))
  (:render (self)
    (flet ((render-value (value ts)
             (<:as-html " Value, invocation: ")
             (<:b (<:as-html value))
             (<:as-html ", " ts ". ")))
      (macrolet ((render-action (action-params &body body)
                   `(progn
                     (<ucw:a :action (register-action (,@action-params
                                                       :with-call/cc nil
                                                       :make-new-frame (backtrackedp self))
                                       ,@body)
                      "increment")
                     (<:as-html " / ")
                     (<ucw:a :action (register-ajax-action (,@action-params
                                                            :with-call/cc nil
                                                            :make-new-frame (backtrackedp self))
                                       ,@body
                                       (mark-dirty self))
                      "with ajax"))))
        (<:p "This example demonstrates action, isolation and backtracking (or lack of)"
             (if (backtrackedp self)
                 (<:b " with backtracked counters.")
                 (<:as-html "."))
             (<:as-html " The current frame is: ")
             (<:i (<:as-html (ucw::frame.id (ucw::context.current-frame *context*)) "."))
             " Let's see the three possible variants: ")
        (<:ol
         (<:li (<:p "With " (<:b "invocation isolated") " action (default)."
                    (render-value (invocation-isolated-counter-of self)
                                  (invocation-isolated-ts-of self))
                    (render-action (:invocation-isolated t)
                                   (setf (invocation-isolated-ts-of self) (get-universal-time))
                                   (incf (invocation-isolated-counter-of self))))
               (<:p "These actions generate a random invocation id on the client side when they are triggered. "
                    "With the help of this random id, they are only executed once for each user action. The effect is "
                    "that even if you reload the page, the action won't be executed again, but going back "
                    "with the back button and pressing the action again will result in a repeated execution of the action."))
         (<:li (<:p "With an action which is " (<:b "not invocation isolated") "."
                    (render-value (counter-of self) (unguarded-ts-of self))
                    (render-action (:invocation-isolated nil)
                                   (setf (unguarded-ts-of self) (get-universal-time))
                                   (incf (counter-of self))))
               (<:p "These actions have no isolation guard of any kind. This means that whenever the server "
                    "gets a request in which this action can be identified, it will be executed. "
                    "In real life it means that reloading a page that was generated after the execution of an unguarded "
                    "action, will result in the repeated execute of that action. (This was the old UCW behaviour before "
                    "the AJAX branch.) "
                    (when (backtrackedp self)
                      (<:as-is "Please note that with backrtacking it means that the counter should remain the same or be less after a reload "
                               "(depending on how many ajax increments you made since the last non-ajax increment) because then backtracking "
                               "restores the value in the frame you are refreshing in and the action increments it only one."))))
         (<:li (<:p "With " (<:b "isolated") " action."
                    (render-value (isolated-counter-of self) (isolated-ts-of self))
                    (render-action (:isolated t)
                                   (setf (isolated-ts-of self) (get-universal-time))
                                   (incf (isolated-counter-of self))))
               (<:p "Isolated actions are guaranteed to be executed only once, no matter what. This means "
                    "that if you press the action and go back with the back button and press it again, "
                    "then the action will not be executed again (as opposed to invocation isolated actions). "
                    (when (backtrackedp self)
                      (<:as-is "With backtracking it has the effect of the counter decrementing one when the value from the frame is "
                               "restored. The action is not executed again to re-increment it.")))))))))

(defcomponent isolation-example (abstract-isolation-example)
  ((counter :initform 0 :accessor counter-of)
   (isolated-counter :initform 0 :accessor isolated-counter-of)
   (invocation-isolated-counter :initform 0 :accessor invocation-isolated-counter-of))
  (:default-initargs :backtracked nil))

(defcomponent isolation-example-with-backtracking (abstract-isolation-example)
  ((counter :initform 0 :accessor counter-of)
   (isolated-counter :initform 0 :accessor isolated-counter-of)
   (invocation-isolated-counter :initform 0 :accessor invocation-isolated-counter-of))
  (:default-backtrack #'identity)
  (:default-initargs :backtracked t))

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
