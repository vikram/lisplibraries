;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Template

(defclass template-component (component)
  ((template-name :accessor template-component.template-name
                  :initarg :template-name
                  :initform nil))
  (:documentation "Component which is rendered via a TAL template."))

(defgeneric template-component-environment (component)
  (:documentation "Create the TAL environment for rendering COMPONENT's template.

Methods defined on this generic function must return a TAL
environment: a list of TAL binding sets (see the documentation
for YACLML:MAKE-STANDARD-ENVIRONMENT for details on TAL
environments.)")
  (:method-combination nconc))

(defmethod template-component-environment nconc ((component template-component))
  "Create the basic TAL environment.

Binds the symbol ucw:component to the component object itself,
also puts the object COMPONENT on the environment (after the
binding of ucw:component) so that slots are, by default,
visable."
  (make-standard-environment `((component . ,component)) component))

(defmethod render ((component template-component))
  "Render a template based component.

Calls the component's template. The name of the template is the
value returned by the generic function
template-component.template-name, the template will be rendered
in the environment returned by the generic function
template-component-environment."
  (render-template *context*
                   (template-component.template-name component)
                   (template-component-environment component)))

(defcomponent simple-template-component (template-component)
  ((environment :initarg :environment :initform nil)))

(defmethod template-component-environment nconc ((component simple-template-component))
  (copy-list (slot-value component 'environment)))

(defmacro show (page-name &rest environment)
  `(call 'simple-template-component
         :template-name ,page-name
         :environment (tal-env ,@environment)))

(defmacro show-window (page-name &rest environment)
  `(call 'simple-window-template-component
         :template-name ,page-name
         :environment (tal-env ,@environment)))

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
