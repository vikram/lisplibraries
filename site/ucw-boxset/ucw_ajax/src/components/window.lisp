;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Simple Window

(defclass window-component ()
  ((content-type :accessor window-component.content-type
                 :initarg :content-type
                 :initform nil ; default is text/html with charset from current application
                 :documentation "The Content-Type header for the
                 http response (also used in the meta tag)")))

(defmethod window-component.content-type ((window window-component))
  "Either use slot value, or compute content-type from current application charset."
  (or (slot-value window 'content-type)
      (setf (window-component.content-type window)
            (format nil "text/html~@[; charset=~A~]"
                    (application.charset (context.application *context*))))))

(defmethod render :before ((window window-component))
  (setf (get-header (context.response *context*) "Content-Type")
        (window-component.content-type window)))

(defclass basic-window-features-mixin ()
  ((title :accessor window-component.title
          :initarg :title
          :initform nil)
   (stylesheet :accessor window-component.stylesheet
               :initarg :stylesheet
               :initform nil
               :documentation "The URL of the css file to use as a stylesheet for this window.")
   (icon :accessor window-component.icon
         :initarg :icon
         :initform nil
         :documentation "Optional URL for an icon.")
   (doctype :accessor window-component.doctype
	     :initarg :doctype
	     :initform "-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/transitional.dtd"
	     :documentation "Doctype for this window.")
   (content-prologue :accessor window-component.content-prologue
                     :initarg :content-prologue
                     :initform nil
                     :documentation "Unless nil it's printed <:as-is before any other output. Suitable for <?xml...?> lines.")
   (html-tag-attributes :accessor window-component.html-tag-attributes
                        :initarg :html-tag-attributes
                        :initform (list :xmlns "http://www.w3.org/1999/xhtml")
                        :documentation "A yaclml attribute list that'll be rendered into the <:html tag's attributes.")
   (javascript :accessor window-component.javascript
               :initarg :javascript
               :initform nil
               :documentation "List of javascript includes.

Each element must be a list whose first value is either the
symbol :SRC or :JS. 

 (:SRC url :with-url-prefix) - writes <script src=\"URL\"></script> tag.
   If WITH-URL-PREFIX is provided and is a string then URL is prefixed
   with that string otherwise with application.url-prefix.
 (:JS form) - equivalent to (:SCRIPT (js:js* form))
 (:SCRIPT string) - write <script>STRING</script>.

The elements will be rendered in order."))
  (:metaclass standard-component-class)
  (:documentation "A mixin that renders basic html toplevel tags."))

(defmethod render :wrap-around ((window basic-window-features-mixin))
  "This convience method assumes: 1) the stylesheet is
external (as opposed to inlined) or is not used; 2) the script
file is javascript and is external or is no script is used and 3)
the title is either a literal or a lambda with one argument (the
window)."
  (awhen (window-component.content-prologue window)
    (<:as-is it ~%))
  (<:as-is "<!DOCTYPE html PUBLIC \"" (window-component.doctype window) "\">"
	   #\Newline)
  (<:html :prologue nil
          (@ (window-component.html-tag-attributes window))
          (render-head window)
          (render-body window #'call-next-method)))

(defgeneric render-head (window)
  (:method :around ((window basic-window-features-mixin))
           (<:head (call-next-method)))
  (:method ((window basic-window-features-mixin))
           (<:meta :http-equiv "Content-Type" :content (window-component.content-type window))
           (awhen (window-component.title window)
             (<:title (if (functionp it)
                          (funcall it window)
                          (<:as-html it))))
           (awhen (window-component.icon window)
             (<:link :rel "icon"
                     :type "image/x-icon"
                     :href it))
           (dolist (stylesheet (ensure-list (window-component.stylesheet window)))
             (<:link :rel "stylesheet"
                     :href stylesheet
                     :type "text/css"))))

(defgeneric render-body (window render-next-method)
  (:method :around ((window basic-window-features-mixin) render-next-method)
           (<:body
            (render-scripts window)
            (call-next-method)))
  (:method ((window basic-window-features-mixin) render-next-method)
           (funcall render-next-method)))

(defgeneric render-scripts (window)
  (:method ((window basic-window-features-mixin))
           (let* ((app (context.application *context*))
                  (url-prefix (application.url-prefix app)))
             (dolist* ((type value &key with-url-prefix) (window-component.javascript window))
               (ecase type
                 (:src
                  ;; most browsers (firefox, safari and ie at least) really,
                  ;; really, really don't like empty script tags. The "" forces
                  ;; yaclml to generate a seperate closing tag.
                  (<:script :type "text/javascript" :src (if with-url-prefix
                                                             (strcat (if (stringp with-url-prefix)
                                                                         with-url-prefix
                                                                         url-prefix)
                                                                     value)
                                                             value)
                            ""))
                 (:js
                  (<ucw:script :toplevelp t (if (functionp value)
                                                (funcall value)
                                                value)))
                 (:script
                  (<:script :type "text/javascript" (<:as-is ~% "// <![CDATA[" ~%
                                                             value
                                                             ~% "// ]]>" ~%))))))))

(defclass simple-window-component (basic-window-features-mixin window-component)
  ((dojo-debug-p :accessor dojo-debug-p :initform nil)
   (dojo-debug-at-all-costs-p :accessor dojo-debug-at-all-costs-p :initform nil))
  (:metaclass standard-component-class)
  (:default-initargs
      ;; An example to return fully XML docs. But then don't expect your pages to work out of the box... :)
      ;;:content-type (format nil "text/xml~@[; charset=~A~]"
      ;;                      (application.charset (context.application *context*)))
      ;;:content-prologue (format nil "<?xml version=\"1.0\" encoding=\"~A\"?>"
      ;;                          (application.charset (context.application *context*)))
      :html-tag-attributes (list :xmlns "http://www.w3.org/1999/xhtml"
                                 "xmlns:dojo" "http://www.dojotoolkit.org/2004/dojoml"))
  (:documentation "A convenience class for writing window components. It also ensures that
the scripts required for UCW to work properly are included."))

(defmethod initialize-instance :around ((window simple-window-component) &rest args &key javascript &allow-other-keys)
  (let* ((app (context.application *context*))
         (url-prefix (application.url-prefix app)))
    (remf-keywords args :javascript)
    ;; TODO propagate debug level to the dojo loggers. for now we just check if there's a value at all.
    (setf (dojo-debug-p window) (javascript-debug-level app))
    (setf (dojo-debug-at-all-costs-p window) (and (dojo-debug-p window)
                                                  (not (string= (javascript-debug-level app) "info"))))
    (apply #'call-next-method window
           (append args
                   (list :javascript
                         (append (list (list :js (lambda ()
                                                   (let ((locale (context.locale *context*)))
                                                     (when (consp locale)
                                                       (setf locale (first locale)))
                                                     `(setf dj-config (create
                                                                       :locale ,(substitute #\- #\_
                                                                                            (string-downcase
                                                                                             (locale-name locale)))
                                                                       ,@(if (dojo-debug-p window)
                                                                             (if (dojo-debug-at-all-costs-p window)
                                                                                 '(:is-debug true
                                                                                   :debug-at-all-costs true)
                                                                                 '(:is-debug true))
                                                                             '(:is-debug false))
                                                                       :base-loader-uri ,(strcat url-prefix "dojo/"))))))
                                       '(:src "dojo/dojo.js" :with-url-prefix t)
                                       '(:src "ucw-dynamic/js/functional.js" :with-url-prefix t)
                                       '(:src "ucw-dynamic/js/per-application.js" :with-url-prefix t))
                                 (when (dojo-debug-p window)
                                   (list (list :js `(dojo.require "dojo.debug.console"))))
                                 javascript))))))

;;; set up some UCW specific scripts
(defmethod render-scripts :after ((window simple-window-component))
  (<ucw:script :compile-time-p t
    `(dojo.event.connect window "onbeforeunload" ucw.default-unload-event-handler))
  (when (dojo-debug-at-all-costs-p window)
    (<ucw:script :compile-time-p t
     `(dojo.hostenv.write-includes)))
  (let ((%client-state-entries% nil)
        (session (context.session *context*)))
    (<ucw:script :toplevelp t
                 `(progn
                   (setf ucw.session-id ,(session.id session))
                   (setf ucw.frame-id ,(frame.id (session.current-frame session))))
                 ;; start the poller if it's a single-frame-application-mixin
                 ;; it could be made into a parameter instead of this typep check
                 (when (typep (context.application *context*) 'single-frame-application-mixin)
                   `(ucw.io.polling.start))
                 (awhen (iter (for client-state-entry in %client-state-entries%)
                              (collect `(ucw.client-state.register
                                         ,(client-state-id client-state-entry)
                                         ,(funcall (client-state-getter client-state-entry))
                                         ,(client-state-client-side-getter client-state-entry)
                                         ,(client-state-client-side-setter client-state-entry))))
                   (cons 'progn it)))))

;; Copyright (c) 2003-2005 Edward Marco Baringer
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
