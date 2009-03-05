;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Collapsible pane component

(defclass collapsible-pane (widget-component)
  ((switch :initarg :switch :accessor switch-of)
   (body :accessor body-of :initarg :body :initform nil :component nil)
   (collapsedp :initform t :initarg :collapsed :accessor collapsedp)
   (server-side-p :initform t :initarg :server-side-p :accessor server-side-p))
  (:default-initargs
      :switch #'render-standard-switch
    :css-class "collapsible"
    :dom-id (js:gen-js-name-string :prefix "_collaps"))
  (:documentation "Component for good.")
  (:metaclass standard-component-class))

(flet ((setup-body-parent (self)
         ;; break the parent relation of the body component when collapsed, so the ajax/dirty component
         ;; renderings will properly detect that this component is not visible currently
         (awhen (body-of self)
           (if (collapsedp self)
               (setf (parent it) nil)
               (setf (parent it) self)))))

  (defmethod initialize-instance :after ((self collapsible-pane) &key &allow-other-keys)
    (setup-body-parent self))
  
  (defmethod (setf body-of) :after (value (self collapsible-pane))
    (setup-body-parent self))

  (defmethod (setf collapsedp) :around (value (self collapsible-pane))
    (let ((old-value (collapsedp self)))
      (when (xor old-value value)
        (call-next-method)
        (setup-body-parent self)
        (mark-dirty self)))))

(defmethod render-standard-switch ((self collapsible-pane)
                                   &key
                                   action
                                   title
                                   (collapsed-title title)
                                   (expanded-title title)
                                   (escape-title t)
                                   (collapsed-image "ucw/images/collapsible-off.png")
                                   (expanded-image "ucw/images/collapsible-on.png"))
  (unless action
    (setf action (register-ajax-action ()
                   (setf (collapsedp self) (not (collapsedp self))))))
  (<ucw:a :action action
          (<:img :src (if (collapsedp self)
                          collapsed-image
                          expanded-image))
          (if (collapsedp self)
              (when collapsed-title
                (if escape-title
                    (<:as-html collapsed-title)
                    (<:as-is collapsed-title)))
              (when expanded-title
                (if escape-title
                    (<:as-html expanded-title)
                    (<:as-is expanded-title))))))

(defmethod render ((self collapsible-pane))
  (unless (server-side-p self)
    (error "TODO: Client side operation is not yet supported"))
  (render-switch self)
  (when (and (not (collapsedp self))
             (body-of self))
    (<:div :class "body"
           (render (body-of self)))))

(defmethod render-switch ((self collapsible-pane))
  (awhen (switch-of self)
    (<:div :class "switch"
           (etypecase it
             (function (funcall it self))
             (string (render-standard-switch self :title it))
             (component (render it))))))

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
