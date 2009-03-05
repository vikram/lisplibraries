;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: kmrcl-*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          attrib-class.lisp
;;;; Purpose:       Defines metaclass allowing use of attributes on slots
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: attrib-class.lisp 9058 2004-04-18 17:18:57Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002-2003 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

;; Disable attrib class until understand changes in sbcl/cmucl
;; using COMPUTE-SLOT-ACCESSOR-INFO and defining method
;; for slot access of ALL-ATTRIBUTES. Does this work on Allegro/LW?

;;;; Defines a metaclass that allows the use of attributes (or subslots)
;;;; on slots. Based on example in AMOP, but modified to use ACL's MOP.

(in-package #:kmrcl)

(defclass attributes-class (kmr-mop:standard-class)
  ()
  (:documentation "metaclass that implements attributes on slots. Based
on example from AMOP"))

(defclass attributes-dsd (kmr-mop:standard-direct-slot-definition)
  ((attributes :initarg :attributes :initform nil
	       :accessor dsd-attributes)))

(defclass attributes-esd (kmr-mop:standard-effective-slot-definition)
  ((attributes :initarg :attributes :initform nil 
	       :accessor esd-attributes)))

;; encapsulating macro for Lispworks
(kmr-mop:process-slot-option attributes-class :attributes)

#+(or cmu scl sbcl openmcl)
(defmethod kmr-mop:validate-superclass ((class attributes-class)
					(superclass kmr-mop:standard-class))
  t)

(defmethod kmr-mop:direct-slot-definition-class ((cl attributes-class) #+kmr-normal-dsdc &rest initargs)
  (declare (ignore initargs))
  (kmr-mop:find-class 'attributes-dsd))

(defmethod kmr-mop:effective-slot-definition-class ((cl attributes-class) #+kmr-normal-dsdc &rest initargs)
  (declare (ignore initargs))
  (kmr-mop:find-class 'attributes-esd))

(defmethod kmr-mop:compute-effective-slot-definition
    ((cl attributes-class) #+kmr-normal-cesd name dsds)
  #+kmr-normal-cesd (declare (ignore name))
  (let ((esd (call-next-method)))
    (setf (esd-attributes esd) (remove-duplicates (mapappend #'dsd-attributes dsds)))
    esd))

;; This does not work in Lispworks prior to version 4.3

(defmethod kmr-mop:compute-slots ((class attributes-class))
  (let* ((normal-slots (call-next-method))
	 (alist (mapcar
		 #'(lambda (slot)
		     (cons (kmr-mop:slot-definition-name slot)
			   (mapcar #'(lambda (attr) (list attr))
				   (esd-attributes slot))))
		 normal-slots)))

    (cons (make-instance
	   'attributes-esd
	   :name 'all-attributes
	   :initform `',alist
	   :initfunction #'(lambda () alist)
	   :allocation :instance
	   :documentation "Attribute bucket"
	   :type t
	   )
	  normal-slots)))
  
(defun slot-attribute (instance slot-name attribute)
  (cdr (slot-attribute-bucket instance slot-name attribute)))

(defun (setf slot-attribute) (new-value instance slot-name attribute)
  (setf (cdr (slot-attribute-bucket instance slot-name attribute))
    new-value))

(defun slot-attribute-bucket (instance slot-name attribute)
  (let* ((all-buckets (slot-value instance 'all-attributes))
	 (slot-bucket (assoc slot-name all-buckets)))
    (unless slot-bucket
      (error "The slot named ~S of ~S has no attributes."
	     slot-name instance))
    (let ((attr-bucket (assoc attribute (cdr slot-bucket))))
      (unless attr-bucket
	(error "The slot named ~S of ~S has no attributes named ~S."
	       slot-name instance attribute))
      attr-bucket)))



