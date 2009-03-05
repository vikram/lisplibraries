;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

#|
;;; Following the steps outlined below will make your project
;;; load using ASDF and Slime's C-c C-c will work fine, too.

;;; in your .asd:

(defsystem :foo
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "my-package::setup-readtable"
  :components
  (...))


;;; in some of the early loaded files:

(in-package :my-package)

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-readtime-wrapper-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '(:my-package :my-package-test) 'setup-readtable)

|#

(in-package :cl-syntax-sugar)

(defclass readtable-function-mixin ()
  ((setup-readtable-function
    :initform nil
    :initarg :setup-readtable-function
    :accessor setup-readtable-function-of)))

(defclass cl-source-file-with-readtable (asdf:cl-source-file readtable-function-mixin)
  ())

(defclass system-with-readtable (asdf:system readtable-function-mixin)
  ())

(defclass system-connection-with-readtable (asdf::system-connection readtable-function-mixin)
  ())

(defmethod asdf:perform :around ((op asdf:operation) (component cl-source-file-with-readtable))
  (bind ((*readtable* *readtable*)
         (system (loop :for parent = (asdf:component-parent component)
                                  :then (asdf:component-parent parent)
                       :while parent
                       :when (typep parent 'asdf:system)
                       :return parent))
         (setup-readtable-function (or (setup-readtable-function-of component)
                                       (setup-readtable-function-of system))))
    (when setup-readtable-function
      (unless (position #\: (string setup-readtable-function) :test #'char=)
        (error "You probably want to fully qualify your SETUP-READTABLE-FUNCTION ~S"
               setup-readtable-function))
      ;; we need to ignore-errors because the setup-readtable-function is only defined
      ;; after some of the early files are already loaded.
      (awhen (ignore-errors
               (fdefinition
                (read-from-string
                 (string-upcase setup-readtable-function))))
        (funcall it)))
    (call-next-method)))

