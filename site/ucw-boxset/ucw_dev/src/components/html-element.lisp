;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** HTML-elements

(defclass html-element (component)
  ((css-class :initarg :css-class
	      :initform nil
	      :accessor css-class
	      :type (or null string list))
   (dom-id :initarg :dom-id
	   :initform nil
	   :accessor dom-id
	   :type (or null string))
   (css-style :initarg :css-style
	      :initform nil
	      :accessor css-style
	      :type (or null string))
   (extra-tags :initarg :extra-tags
	       :initform nil
	       :accessor extra-tags
	       :documentation "Extra tags list that will 
provided as (@ extra-tags)")
   (events :initarg :events
	   :initform nil
	   :accessor events
	   :type (or null list)))
  (:documentation "An HTML element.

HTML elements control aspects that are relevant to almost all tags.

Firstly they provide a place to store the class, id, and style of the
component. The specific render methods of the components themselves
must pass these values to whatever code is used to render the actual
tag.

Secondly, they allow javascript event handlers to be registered for a
tag. The events slot can be filled with a list of lists in the form

 (event parenscript-statement*)

For example (\"onclick\" (alert \"You clicked!\") (return nil)). If
the element has a dom-id, these event handlers are automatically
added."))

(defmethod render :after ((element html-element))
  "Register event handlers for an HTML element"
  (when (and (events element) (dom-id element))
    (<ucw:script
     `(progn ,@(mapcar (lambda (x) `(dojo.event.connect
                                     (document.get-element-by-id ,(dom-id element))
                                     ,(car x)
                                     (lambda (event)
                                       (progn ,@(cdr x)))))
                       (events element))))))
