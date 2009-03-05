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
      (format nil "text/html~@[; charset=~A~]"
              (application.charset (context.application *context*)))))

(defmethod render :before ((window window-component))
  (setf (get-header (context.response *context*) "Content-Type")
        (window-component.content-type window)))

(defclass simple-window-component (window-component)
  ((title :accessor window-component.title
          :initarg :title
          :initform nil )
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
   (javascript :accessor window-component.javascript
               :initarg :javascript
               :initform nil
               :documentation "List of javascript includes.

Each element must be a list whose first value is either the
symbol :SRC or :JS. 

 (:SRC url) - writes <script src=\"URL\"></script> tag.
 (:JS form) - equivalent to (:SCRIPT (js:js* form))
 (:SCRIPT string) - write <script>STRING</script>.

The elements will be rendered in order."))
  (:metaclass standard-component-class)
  (:documentation "A convenience class for writing window components."))

(defmethod render :wrapping ((window simple-window-component))
  "This convience method assumes: 1) the stylesheet is
external (as opposed to inlined) or is not used; 2) the script
file is javascript and is external or is no script is used and 3)
the title is simply the value of the title slot in the WINDOW (no
dynamic titles)."
  (<:as-is "<!DOCTYPE html PUBLIC \"" (window-component.doctype window) "\">"
	   #\Newline) 
  (<:html :prologue nil
   (<:head
    (<:meta :http-equiv "Content-Type" :content (window-component.content-type window))
    (<:title (<:as-html (window-component.title window)))
    (awhen (window-component.icon window)
      (<:link :rel "icon"
              :type "image/x-icon"
              :href it))
    (dolist (stylesheet (ensure-list (window-component.stylesheet window)))
      (<:link :rel "stylesheet"
              :href stylesheet
              :type "text/css"))
    (let* ((app (context.application *context*))
           (url-prefix (application.url-prefix app)))
      (<ucw:script `(setf dj-config (create
                                     :is-debug ,(if (debug-on-error app)
                                                    'true
                                                    'false)
                                     :base-loader-uri ,(strcat url-prefix "dojo/"))))
      (<:script :type "text/javascript" :src (strcat url-prefix "dojo/dojo.js"))
      (<:script :type "text/javascript" :src (strcat url-prefix "ucw/js/per-application.js"))
      (dolist* ((type value &key with-url-prefix) (window-component.javascript window))
        (ecase type
          (:src
           ;; most browsers (firefox, safari and ie at least) really,
           ;; really, really don't like empty script tags. The "" forces
           ;; yaclml to generate a seperate closing tag.
           (<:script :type "text/javascript" :src (if with-url-prefix
                                                      (strcat url-prefix value)
                                                      value)
                     ""))
          (:js
           (<:script :type "text/javascript" (<:as-is (js:js* value))))
          (:script
           (<:script :type "text/javascript" (<:as-is value)))))))
   (<:body (call-next-method))))

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
