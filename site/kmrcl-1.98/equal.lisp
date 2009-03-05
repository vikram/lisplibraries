;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          equal.lisp
;;;; Purpose:       Generalized equal function for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id$
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


(in-package #:kmrcl)


(defun generalized-equal (obj1 obj2)
  (if (not (equal (type-of obj1) (type-of obj2)))
      (progn
        (terpri)
        (describe obj1)
        (describe obj2)
        nil)
    (typecase obj1
      (double-float
       (let ((diff (abs (/ (- obj1 obj2) obj1))))
         (if (> diff (* 10 double-float-epsilon))
             nil
           t)))
      (complex
       (and (generalized-equal (realpart obj1) (realpart obj2))
            (generalized-equal (imagpart obj1) (imagpart obj2))))
      (structure-object
       (generalized-equal-fielded-object obj1 obj2))
      (standard-object
       (generalized-equal-fielded-object obj1 obj2))
      (hash-table
       (generalized-equal-hash-table obj1 obj2)
       )
      (function
       (generalized-equal-function obj1 obj2))
      (string
       (string= obj1 obj2))
      (array
       (generalized-equal-array obj1 obj2))
      (t
       (equal obj1 obj2)))))


(defun generalized-equal-function (obj1 obj2)
  (string= (function-to-string obj1) (function-to-string obj2)))

(defun generalized-equal-array (obj1 obj2)
  (block test
    (when (not (= (array-total-size obj1) (array-total-size obj2)))
      (return-from test nil))
    (dotimes (i (array-total-size obj1))
      (unless (generalized-equal (aref obj1 i) (aref obj2 i))
        (return-from test nil)))
    (return-from test t)))

(defun generalized-equal-hash-table (obj1 obj2)
  (block test
    (when (not (= (hash-table-count obj1) (hash-table-count obj2)))
      (return-from test nil))
    (maphash
     #'(lambda (k v)
         (multiple-value-bind (value found) (gethash k obj2)
           (unless (and found (generalized-equal v value))
             (return-from test nil))))
     obj1)
    (return-from test t)))

(defun generalized-equal-fielded-object (obj1 obj2)
  (block test
    (when (not (equal (class-of obj1) (class-of obj2)))
      (return-from test nil))
    (dolist (field (class-slot-names (class-name (class-of obj1))))
      (unless (generalized-equal (slot-value obj1 field) (slot-value obj2 field))
        (return-from test nil)))
    (return-from test t)))

(defun class-slot-names (c-name)
  "Given a CLASS-NAME, returns a list of the slots in the class."
  #+(or allegro cmu lispworks sbcl scl)
  (mapcar #'kmr-mop:slot-definition-name
          (kmr-mop:class-slots (kmr-mop:find-class c-name)))
  #+(and mcl (not openmcl))
  (let* ((class (find-class c-name nil)))
    (when (typep class 'standard-class)
      (nconc (mapcar #'car (ccl:class-instance-slots class))
             (mapcar #'car (ccl:class-class-slots class)))))
  #-(or allegro lispworks cmu sbcl scl (and mcl (not openmcl)))
  (declare (ignore c-name))
  #-(or allegro lispworks cmu sbcl scl (and mcl (not openmcl)))
  (error "class-slot-names is not defined on this platform")
  )

(defun structure-slot-names (s-name)
  "Given a STRUCTURE-NAME, returns a list of the slots in the structure."
  #+allegro (class-slot-names s-name)
  #+lispworks (structure:structure-class-slot-names
               (find-class s-name))
  #+(or sbcl cmu) (mapcar #'kmr-mop:slot-definition-name
                          (kmr-mop:class-slots (kmr-mop:find-class s-name)))
  #+scl (mapcar #'kernel:dsd-name
                (kernel:dd-slots
                 (kernel:layout-info
                  (kernel:class-layout (find-class s-name)))))
  #+(and mcl (not openmcl))
  (let* ((sd (gethash s-name ccl::%defstructs%))
               (slots (if sd (ccl::sd-slots sd))))
          (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
  #-(or allegro lispworks cmu sbcl scl (and mcl (not openmcl)))
  (declare (ignore s-name))
  #-(or allegro lispworks cmu sbcl scl (and mcl (not openmcl)))
  (error "structure-slot-names is not defined on this platform")
  )

(defun function-to-string (obj)
  "Returns the lambda code for a function. Relies on
Allegro implementation-dependent features."
  (multiple-value-bind (lambda closurep name) (function-lambda-expression obj)
    (declare (ignore closurep))
    (if lambda
          (format nil "#'~s" lambda)
      (if name
          (format nil "#'~s" name)
        (progn
          (print obj)
          (break))))))

