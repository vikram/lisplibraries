;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)

;;; Class hierarchy

;; Different implementations have different requirements, so let's
;; make everything available all the time.
(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass dict-mixin ()
  ((dict :initarg :dict
         :initform nil #+(or)(make-hash-table :test 'eq)
         :accessor dict)))

(closer-mop:finalize-inheritance (find-class 'dict-mixin))

(defmethod dict (x)
  (declare (ignore x))
  nil)

(defgeneric has-dict (x)
  (:method ((x dict-mixin)) t)
  (:method (x)              (declare (ignore x)) nil))

(defclass py-meta-type (dict-mixin standard-class)
  ())

(closer-mop:finalize-inheritance (find-class 'py-meta-type))

(defmethod closer-mop:validate-superclass ((class py-meta-type) superclass)
  (declare (ignorable class superclass))
  t)
  
(defclass py-type (dict-mixin standard-class)
  ()      
  (:metaclass py-meta-type))

(closer-mop:finalize-inheritance (find-class 'py-type))

(defmethod closer-mop:validate-superclass ((class py-type) superclass)
  (declare (ignorable class superclass))
  t)

(defmethod closer-mop:validate-superclass ((class standard-class) (superclass py-type))
  (declare (ignorable class superclass))
  t)

(defclass object (standard-object)
  ()
  (:metaclass py-type))

(closer-mop:finalize-inheritance (find-class 'object))

(defclass dicted-object (dict-mixin object)
  ()
  (:metaclass py-type))

(closer-mop:finalize-inheritance (find-class 'dicted-object))

  (defmethod closer-mop:validate-superclass (class (superclass py-meta-type))
    (declare (ignorable class superclass))
    t)
  
  (defmethod closer-mop:validate-superclass ((class standard-class) (superclass py-meta-type))
    (declare (ignorable class superclass))
    t)


(defgeneric my-classp-1 (x)
  (:method ((x py-type)) t)
  (:method ((x py-meta-type)) t)
  (:method (x) (declare (ignore x)) nil))

;;; Instance dicts

(defun eq-hash-table-p (x)
  (and (hash-table-p x)
       (eq (hash-table-test x) 'eq)))

(defun py-hash-table-p (x)
  (and (hash-table-p x)
       (eq (hash-table-test x) 'py-==->lisp-val)))

(deftype eq-hash-table ()
  `(satisfies eq-hash-table-p))

(deftype py-hash-table ()
  `(satisfies py-hash-table-p))

(defun make-eq-hash-table (why)
  (declare (ignore why))
  #+(or)(warn "Make-eq-hash-table: ~A" why)
  (make-hash-table :test 'eq))

(defun py-==->lisp-val (x y)
  (py-val->lisp-bool (py-== x y)))

(defgeneric py-hash (x))

;;; Python dicts are hash tables with custom equality (py-==) and hash functions (py-hash).

#+sbcl
(sb-ext:define-hash-table-test py-==->lisp-val py-hash)
#+cmu
(extensions:define-hash-table-test 'py-hash-table-test #'py-==->lisp-val #'py-hash)

(defun make-py-hash-table ()
  (or #+(or allegro ccl lispworks) (make-hash-table :test 'py-==->lisp-val :hash-function 'py-hash)
      #+cmu (make-hash-table :test 'py-hash-table-test)
      #+sbcl (make-hash-table :test 'py-==->lisp-val)
      #-(or allegro ccl lispworks cmu sbcl) (error "Creating python dict not suported in this environment.")))


;; None and NotImplemented are here, so that other modules like classes can use the compiler macros.

(defclass py-none (object) () (:metaclass py-type))
(defvar *the-none* (make-instance 'py-none))
(defun none-p (x) (eq x *the-none*))
;; This use of load-time-value is only guaranteed to work in modules loaded after this module.
(define-compiler-macro none-p (x) `(eq ,x (load-time-value *the-none*)))

(defclass py-notimplemented (object) () (:metaclass py-type))
(defvar *the-notimplemented* (make-instance 'py-notimplemented))

) ;; eval-when
