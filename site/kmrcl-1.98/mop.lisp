;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mop.lisp
;;;; Purpose:       Imports standard MOP symbols into KMRCL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id$
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

;;; This file imports MOP symbols into KMR-MOP packages and then
;;; re-exports them to hide differences in MOP implementations.

(in-package #:cl-user)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (find-package 'sb-mop)
      (pushnew :kmr-sbcl-mop cl:*features*)
      (pushnew :kmr-sbcl-pcl cl:*features*)))

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (eq (symbol-package 'pcl:find-class)
          (find-package 'common-lisp))
      (pushnew :kmr-cmucl-mop cl:*features*)
      (pushnew :kmr-cmucl-pcl cl:*features*)))

(defpackage #:kmr-mop
  (:use
   #:cl
   #:kmrcl
   #+kmr-sbcl-mop #:sb-mop
   #+kmr-cmucl-mop #:mop
   #+allegro #:mop
   #+lispworks #:clos
   #+clisp #:clos
   #+scl #:clos
   #+openmcl #:openmcl-mop
   )
  )

(in-package #:kmr-mop)

#+lispworks
(defun intern-eql-specializer (slot)
  `(eql ,slot))

(defmacro process-class-option (metaclass slot-name &optional required)
  #+lispworks
  `(defmethod clos:process-a-class-option ((class ,metaclass)
                                           (name (eql ,slot-name))
                                           value)
    (when (and ,required (null value))
      (error "metaclass ~A class slot ~A must have a value" (quote ,metaclass) name))
    (list name `',value))
  #-lispworks
  (declare (ignore metaclass slot-name required))
  )

(defmacro process-slot-option (metaclass slot-name)
  #+lispworks
  `(defmethod clos:process-a-slot-option ((class ,metaclass)
                                          (option (eql ,slot-name))
                                          value
                                          already-processed-options
                                          slot)
    (list* option `',value already-processed-options))
  #-lispworks
  (declare (ignore metaclass slot-name))
  )


(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import
   #+allegro
   '(excl::compute-effective-slot-definition-initargs)
   #+lispworks
   '(clos::compute-effective-slot-definition-initargs)
   #+clisp
   '(clos::compute-effective-slot-definition-initargs)
   #+sbcl
   '(#+kmr-sbcl-mop class-of #-kmr-sbcl-mop sb-pcl:class-of
     #+kmr-sbcl-mop class-name #-kmr-sbcl-mop sb-pcl:class-name
     #+kmr-sbcl-mop class-slots #-kmr-sbcl-mop sb-pcl:class-slots
     #+kmr-sbcl-mop find-class #-kmr-sbcl-mop sb-pcl:find-class
     sb-pcl::standard-class
     sb-pcl:slot-definition-name sb-pcl::finalize-inheritance
     sb-pcl::standard-direct-slot-definition
     sb-pcl::standard-effective-slot-definition sb-pcl::validate-superclass
     sb-pcl::direct-slot-definition-class
     sb-pcl::effective-slot-definition-class
     sb-pcl::compute-effective-slot-definition
     sb-pcl:class-direct-slots
     sb-pcl::compute-effective-slot-definition-initargs
     sb-pcl::slot-value-using-class
     sb-pcl:class-prototype sb-pcl:generic-function-method-class sb-pcl:intern-eql-specializer
     sb-pcl:make-method-lambda sb-pcl:generic-function-lambda-list
     sb-pcl::compute-slots)
   #+cmu
   '(pcl:class-of  pcl:class-name pcl:class-slots pcl:find-class pcl::standard-class
     pcl::slot-definition-name pcl:finalize-inheritance
     pcl::standard-direct-slot-definition pcl::standard-effective-slot-definition
     pcl::validate-superclass pcl:direct-slot-definition-class pcl::effective-slot-definition-class
     pcl:compute-effective-slot-definition
     pcl:class-direct-slots
     pcl::compute-effective-slot-definition-initargs
     pcl::slot-value-using-class
     pcl:class-prototype pcl:generic-function-method-class pcl:intern-eql-specializer
     pcl:make-method-lambda pcl:generic-function-lambda-list
     pcl::compute-slots)
   #+scl
   '(class-of class-name class-slots find-class clos::standard-class
     clos::slot-definition-name clos:finalize-inheritance
     clos::standard-direct-slot-definition clos::standard-effective-slot-definition
     clos::effective-slot-definition-class
     clos:class-direct-slots
     clos::validate-superclass clos:direct-slot-definition-class
     clos:compute-effective-slot-definition
     clos::compute-effective-slot-definition-initargs
     clos::slot-value-using-class
     clos::class-prototype clos:generic-function-method-class clos:intern-eql-specializer
     clos:make-method-lambda clos:generic-function-lambda-list
     clos::compute-slots
     ;; note: make-method-lambda is not fbound
     )
   #+openmcl
   '(openmcl-mop::slot-definition-name openmcl-mop:finalize-inheritance
     openmcl-mop::standard-direct-slot-definition openmcl-mop::standard-effective-slot-definition
     openmcl-mop::validate-superclass openmcl-mop:direct-slot-definition-class openmcl-mop::effective-slot-definition-class
     openmcl-mop:compute-effective-slot-definition
     openmcl-mop:class-direct-slots
     openmcl-mop::compute-effective-slot-definition-initargs
     openmcl-mop::slot-value-using-class
     openmcl-mop:class-prototype openmcl-mop:generic-function-method-class openmcl-mop:intern-eql-specializer
     openmcl-mop:make-method-lambda openmcl-mop:generic-function-lambda-list
     openmcl-mop::compute-slots)   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(class-of class-name class-slots find-class
            standard-class
            slot-definition-name finalize-inheritance
            standard-direct-slot-definition
            standard-effective-slot-definition validate-superclass
            compute-effective-slot-definition-initargs
            direct-slot-definition-class effective-slot-definition-class
            compute-effective-slot-definition
            slot-value-using-class
            class-prototype generic-function-method-class intern-eql-specializer
            make-method-lambda generic-function-lambda-list
            compute-slots
            class-direct-slots
            ;; KMR-MOP encapsulating macros
            process-slot-option
            process-class-option))

  #+sbcl
  (if (find-package 'sb-mop)
      (setq cl:*features* (delete :kmr-sbcl-mop cl:*features*))
      (setq cl:*features* (delete :kmr-sbcl-pcl cl:*features*)))

  #+cmu
  (if (find-package 'mop)
      (setq cl:*features* (delete :kmr-cmucl-mop cl:*features*))
      (setq cl:*features* (delete :kmr-cmucl-pcl cl:*features*)))

  (when (>= (length (generic-function-lambda-list
                     (ensure-generic-function
                      'compute-effective-slot-definition)))
            3)
    (pushnew :kmr-normal-cesd cl:*features*))

  (when (>= (length (generic-function-lambda-list
                     (ensure-generic-function
                      'direct-slot-definition-class)))
            3)
    (pushnew :kmr-normal-dsdc cl:*features*))

  )  ;; eval-when
