;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-serializer-system)

(defpackage :cl-serializer
  (:nicknames :serializer)

  (:use :common-lisp :cl-def :bind :alexandria :babel)
  
  (:shadow #:read-string
           #:write-string)

  (:export #:serialize
           #:deserialize))

(in-package :cl-serializer)

(defun transform-function-definer-options (options)
  (if cl-serializer-system:*load-with-debug-p*
      (remove-from-plist options :inline :optimize)
      options))

(defmacro enable-sharp-boolean-syntax ()
  "Copies *readtable* and enables #t and #f readers for T and NIL in the copy."
  '(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharp-boolean-syntax)))

(defun %enable-sharp-boolean-syntax ()
  (set-dispatch-macro-character
   #\# #\t
   (lambda (s c n)
     (declare (ignore s c n))
     t))
  (set-dispatch-macro-character
   #\# #\f
   (lambda (s c n)
     (declare (ignore s c n))
     nil)))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(progn
  (defun setup-swank-readtable-alist (&rest package-name/readtable-setup-function-pairs)
    (loop for (package-names setup-function) :on package-name/readtable-setup-function-pairs :by #'cddr do
          (bind ((*readtable* (copy-readtable)))
            (funcall setup-function)
            (dolist (package-name (ensure-list package-names))
              (setf package-name (string package-name))
              (let ((entry (find package-name swank:*readtable-alist* :test #'string= :key #'car)))
                (unless entry
                  (setf entry (cons package-name nil))
                  (push entry swank:*readtable-alist*))
                (setf (cdr entry) *readtable*))))))
  (setup-swank-readtable-alist
   '(:cl-serializer :cl-serializer-test) 'setup-readtable))
