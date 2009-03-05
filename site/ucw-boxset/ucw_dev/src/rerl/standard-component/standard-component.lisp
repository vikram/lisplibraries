;;;; -*- lisp -*-

(in-package :it.bese.ucw)

(defclass standard-component (component)
  ((continuation :accessor component.continuation
                 :initarg :continuation
                 :documentation "Our continuation (a 1 arg lambda)")
   (calling-component :accessor component.calling-component
                      :initarg :calling-component
                      :documentation
"The component which transfered control to this component.

When this component ANSWERs it returns control to the
calling-component and modifes the associated place. Only in the
case of top level components will this slot be NIL.")
   (place :initarg :place
          :accessor component.place
          :documentation
"The place (a PLACE object) which holds this component.

Only in the case of top level component will this slot be NIL.")   
   (isolate-hash :initarg :isolate-hash
                 :initform nil
                 :documentation
"A hash table containing the values of actions declared with the :isolate property")
   (parent :initarg :parent :accessor parent :initform nil))
  (:documentation "Super class of all standard components."))

(defmethod component.isolate-hash ((c standard-component))
  (aif (slot-value c 'isolate-hash)
       it
       (setf (slot-value c 'isolate-hash) (make-hash-table :test #'equalp))))

(defun initialize-backtracking (comp frame)
  "Prepare the slots of COMP for backtracking."
  (dolist* ((slot-name . copy-function)
            (component-class.backtrack-slots (class-of comp)))
    (backtrack-slot frame comp slot-name copy-function)))

(defun initialize-component-slots (comp)
  "Setup any nested components (create them and their places) in COMP.
For every component-slot in COMP: if the slot doesn't already contain
a component and a type was given for what the component should be,
then make-instance of the type and set the slot-value.

Then check whether the slot is bound and not null and if
so set its place slot. the second check is done seperatly from the
first so that components passed via initargs will have their place
set."
  (iterate
    (for (slot-name . type) in (component-class.component-slots (class-of comp)))
    (for slot-value = (when (slot-boundp comp slot-name)
                        (slot-value comp slot-name)))
    (when (and (not slot-value)
               (not (null type)))
      ;; the slot SLOT-NAME, which should hold a component, is
      ;; currently unbound or null, set it to a component object of
      ;; the specified type
      (setf slot-value (if (consp type)
                           (eval `(apply #'make-instance ',(first type) (append ',slot-value (list ,@(cdr type)))))
                           (apply #'make-instance type slot-value))
            (slot-value comp slot-name) slot-value))
    (when (and slot-value
               (typep slot-value 'standard-component))
      ;; if the slot has already been initialized (due to regular
      ;; initargs) then simply set its place
      (rebind (slot-name)
        (setf (component.place (slot-value comp slot-name)) (make-place (slot-value comp slot-name))
              (parent (slot-value comp slot-name)) comp)))))

(defmethod (setf mopp:slot-value-using-class) :after
    (new-value
     (class standard-component-class)
     (instance standard-component)
     (slot-def standard-component-effective-slot))
  (when (and new-value
             (typep new-value 'standard-component)
             (slot-boundp slot-def 'component))
    (setf (component.place new-value)
          (make-place (mopp:slot-value-using-class class instance slot-def)))
    (setf (parent new-value) instance)))

(defun initialize-place-slot (comp)
  "Setup the place of COMP's place-slot's component.

NB: This function assumes a component already exists in COMP's place-slot."
  (with-slots (place-slot) (class-of comp)
    (when place-slot
      (setf (component.place comp) (make-place (slot-value comp place-slot))))))

(defmethod shared-initialize :after ((c standard-component) slot-names
                                     &key (frame (context.current-frame *context*))
                                     &allow-other-keys)
  "Perform the standard initialization for C.

This method registers C's transaction slot for backtracking and
any other slot which has been declared as backtracked. It then
proceeds to initialize any component slots in C."
  (declare (ignore slot-names))
  ;; setup the 'special' slots for backtracking
  (backtrack-slot frame c 'parent)
  (backtrack-slot frame c 'calling-component)
  ;; do the rest of the initialization
  (initialize-backtracking c frame)
  (initialize-component-slots c)
  (initialize-place-slot c))

(defgeneric child-components (standard-component &key predicate key)
  (:documentation "Return the children of a component."))

(defmethod child-components ((component standard-component) &key predicate (key #'identity))
  "Find all children components of the given components.
 This will only return children components that were declared on the class
as a component and have components in those slots."
  (let ((component-slot-list (ucw::component-class.component-slots (class-of component))))
    (iterate (for (slot-name . declared-type) in component-slot-list)
	     (when (slot-boundp component slot-name)
	       (let ((val (slot-value component slot-name)))
		 (when (and (typep val 'standard-component)
			    (or (null predicate)
				(funcall predicate (funcall key val))))
		   (collect val)))))))

(defgeneric descendant-p (parent child &optional recursive-p)
  (:documentation "Predicate to use whether child is a child of parent."))

(defmethod descendant-p ((parent standard-component) (child standard-component) &optional recursive-p)
  (or (eq (parent child) parent)
      (when (and recursive-p (parent child))
	(descendant-p parent (parent child)))))

(defmethod render ((component t))
  (error "No RENDER method defined for ~S." component))

(defvar *copy-down-component* nil
  "Holder variable used for the copy-down link in the inspector
  links. not thread safe (but good enough for single user
  debugging).")

(defun render-with-inspector-link (component call-next-method)
  (<:div :style "border: 2px solid #000000;"
    (<:div :style "margin: 0px; padding: 1px; background: #ccccff; float: top right;"
      (<:a :href (print-uri-to-string
                  (compute-url component
                               :action-id
                               (make-new-action
                                (context.current-frame *context*)
                                (lambda ()
                                  (with-call/cc
                                    (call-inspector component component))))))
           (<:tt "Inspect " (<:as-html component)))
      (when (swank::default-connection)
        (<:as-html " - ")
        (<:a :href (print-uri-to-string
                    (compute-url component
                                 :action-id
                                 (make-new-action
                                  (context.current-frame *context*)
                                  (let ((comp component))
                                    (lambda ()
                                      (setf *copy-down-component* comp)
                                      (swank::with-connection ((swank::default-connection))
                                        (swank::eval-in-emacs
                                         `(slime-repl-send-string "ucw::*copy-down-component*")))
                                      (setf *copy-down-component* nil))))))
             (<:tt "Copy to slime repl."))))
    (funcall call-next-method)))

(defmethod render :wrap-around ((component standard-component))
  (let ((*current-component* component))
    (call-next-method)))

(defmethod render :wrapping ((component standard-component))
  "Setup up a convience restart, bind *yaclml-stream* and add inspector links."
  (let ((response (context.response *context*)))
    (restart-case
        (if *inspect-components*
            (render-with-inspector-link component #'call-next-method)
            (call-next-method))
      (retry ()
        :report (lambda (stream)
                  (format stream "Retry rendering the component ~S." component))
        (clear-response response)
        (return-from render (render component))))))

(defmethod render-loop ((comp standard-component))
  (restart-case
      (render comp)
    (retry ()
      :report "Retry calling RENDER."
      (return-from render-loop (render-loop comp)))))

(defmethod compute-url ((component standard-component) &key action-id)
  "Walks up the component tree based at COMPONENT and calls
UPDATE-URL on the components."
  (let ((url (compute-url (context.application *context*) :action-id action-id)))
    (labels ((%compute (component)
               ;; first let the parent update the url
               (when (parent component)
                 (%compute (parent component)))
               ;; now that all of our parents have done whatever they
               ;; want with url we can update it.
               (setf url (update-url component url))))
      (%compute component))))

(defmethod update-url ((comp standard-component) url)
  "Do nothing to the URL."
  url)

(defmacro defcomponent (name supers slots &rest options)
  "Macro for defining a component class.

This macro is used to create component classes and provides
options for easily creating the methods which often accompany a
component definition.

NAME, SUPERS and SLOTS as treated as per defclass. The following
extra options are allowed:

 (:ENTRY-POINT url (&key application class)) - Define an
 entry-point on the url URL which simply calls an instance of
 this component. Any request parameters passed to the entry-point
 are used to initialize the slots in the component. This option
 may appear multiple times.

 (:DEFAULT-BACKTRACK function-designator) - Unless the slots
 already have a :backtrack option FUNCTION-DESIGNATOR is
 added. As with the 'regular' :backtrack options if you pass T
 here it is assumed to mean #'IDENTITY.

 (:RENDER (&optional COMPONENT) &body BODY) - Generate a render
 method specialized to COMPONENT. COMPONENT may be a symbol, in
 which case the method will be specialized on the componnet
 class. If COMPONNET is omited the component is bound to a
 variable with the same name as the class.

 (:ACTION &optional NAME) - Generate a defaction form named
 NAME (which defaults to the name of the component) which simply
 CALL's this component class passing all the arguments passed to
 the action as initargs."
  (macrolet ((destructure-option (option-keyword binding-list &body body)
               `(aif (assoc ,option-keyword options)
                     (destructuring-bind ,binding-list (cdr it)
                       ,@body))))
    (labels ((entry-point-option ()
               (destructure-option :entry-point
                   (url (&key application class))
                 `(defentry-point ,url (,@(if application
                                              `(:application ,application)
                                              '())
                                        ,@(if class
                                              `(:class ,class)
                                              '()))
                      ()
                    (call ',name))))

             (metaclass-option ()
               (or (destructure-option :metaclass
                                       (class-name)
                                       class-name)
                   'standard-component-class))

             (default-backtrack-option ()
               (destructure-option :default-backtrack
                   (copyer)
                 (case copyer
                   (nil nil)
                   ((t) `(:backtrack #'identity))
                   (t `(:backtrack ,copyer)))))

             (action-option ()
               (destructure-option :action
                   (&optional (action-name name))
                 `(defaction ,action-name ((component ,name) &rest initargs)
                    (apply #'call-component self ',name initargs))))

             (render-option ()
               (destructure-option :render
                   ((&optional (component name))
                    &body body)
                 (unless (listp component)
                   (setf component `(,component ,name)))
                 `(defmethod render (,component)
                    ,@body)))
         
             (effective-options ()
               (remove-if (lambda (option)
                            (member (car option) '(:entry-point :metaclass :default-backtrack
                                                   :action :render)))
                          options))

             (process-slot (slot)
               (destructuring-bind (slot-name &rest initargs)
                   (ensure-list slot)
                 (cond
                   ((getf initargs :backtrack)
                    (list* slot-name initargs))
                   ((default-backtrack-option)
                    (append (list slot-name) (default-backtrack-option) initargs))
                   (t
                    (list* slot-name initargs))))))
    
      `(progn
         (defclass ,name ,supers
           ,(mapcar #'process-slot slots)
           ,@(effective-options)
           (:metaclass ,(metaclass-option)))
         ,(entry-point-option)
         ,(action-option)
         ,(render-option)
         ',name))))

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
