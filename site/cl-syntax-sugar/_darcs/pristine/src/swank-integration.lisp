;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(defun register-readtable-for-swank (&rest package-name/readtable-setup-function-pairs)
  (loop
     :for (package-names setup-function)
     :on package-name/readtable-setup-function-pairs
     :by #'cddr :do
     (with-local-readtable
       (funcall setup-function)
       (dolist (package-name (ensure-list package-names))
         (setf package-name (string package-name))
         (let ((entry (find package-name swank:*readtable-alist* :test #'string= :key #'car)))
           (unless entry
             (setf entry (cons package-name nil))
             (push entry swank:*readtable-alist*))
           (setf (cdr entry) *readtable*))))))
