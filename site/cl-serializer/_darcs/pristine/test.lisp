;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defpackage :cl-serializer-test
  (:nicknames :serializer-test)

  (:use :common-lisp
        :closer-mop
        :cl-def
        :stefil
        :cl-serializer))

(in-package :cl-serializer-test)

;;;;;;;;
;;; Test

(defsuite* (test :in root-suite))

(defclass standard-object-test ()
  ((slot :initarg :slot :accessor slot-of)))

(defstruct structure-object-test
  (slot
   nil
   :type t))

(defsuite* (test/serialize-deserialize :in test))

(def special-variable *equal-hash-table*)

(defgeneric object-equal-p (object-1 object-2)
  (:method (object-1 object-2)
    (equalp object-1 object-2))

  (:method ((object-1 symbol) (object-2 symbol))
    (or (call-next-method)
        (and (not (symbol-package object-1))
             (not (symbol-package object-2))
             (equal (symbol-name object-1)
                    (symbol-name object-2)))))

  (:method ((object-1 list) (object-2 list))
    (labels ((equal-aux (x y)
               (cond ((eql x y)
                      t)
                     ((and (consp x)
                           (consp y))
                      (and (object-equal-p (car x) (car y))
                           (object-equal-p (cdr x) (cdr y))))
                     (t (object-equal-p x y)))))
      (equal-aux object-1 object-2)))

  (:method ((object-1 array) (object-2 array))
    (and (object-equal-p (array-dimensions object-1)
                         (array-dimensions object-2))
         (object-equal-p (array-element-type object-1)
                         (array-element-type object-2))
         (loop for index :from 0 :below (array-total-size object-1)
               always (object-equal-p (row-major-aref object-1 index) (row-major-aref object-2 index)))))

  (:method ((object-1 hash-table) (object-2 hash-table))
    (and (= (hash-table-count object-1) (hash-table-count object-2))
         (eq (hash-table-test object-1) (hash-table-test object-2))
         (block nil
           (maphash (lambda (key value)
                      (unless (object-equal-p (gethash key object-2) value)
                        (return #f)))
                    object-1)
           #t)))

  (:method ((object-1 structure-object) (object-2 structure-object))
    (or (eq object-1 object-2)
        (let ((class-1 (class-of object-1))
              (class-2 (class-of object-2)))
          (and (eq class-1 class-2)
               (every (lambda (slot)
                        (object-equal-p
                         (slot-value-using-class class-1 object-1 slot)
                         (slot-value-using-class class-2 object-2 slot)))
                      (class-slots class-1))))))

  (:method ((object-1 standard-object) (object-2 standard-object))
    (or (eq object-1 object-2)
        (let ((class-1 (class-of object-1))
              (class-2 (class-of object-2)))
          (and (eq class-1 class-2)
               (every (lambda (slot)
                        (or (and (not (slot-boundp-using-class class-1 object-1 slot))
                                 (not (slot-boundp-using-class class-2 object-2 slot)))
                            (object-equal-p
                             (slot-value-using-class class-1 object-1 slot)
                             (slot-value-using-class class-2 object-2 slot))))
                      (class-slots class-1)))))))

(def definer serialize-deserialize-test (name value)
  `(def test ,(serializer::concatenate-symbol *package* "test/serialize-deserialize/" name) ()
    (is (object-equal-p ,value (deserialize (serialize ,value))))))

(def serialize-deserialize-test nil nil)

(def serialize-deserialize-test t t)

(def serialize-deserialize-test symbol/1 'test)
(def serialize-deserialize-test keyword/1 :test)
(def serialize-deserialize-test uninterned-symbol/1 '#:a)
(def serialize-deserialize-test package/1 (find-package :common-lisp))

(def serialize-deserialize-test integer/1 -1)
(def serialize-deserialize-test integer/2 0)
(def serialize-deserialize-test integer/3 1)
(def serialize-deserialize-test integer/4 255)
(def serialize-deserialize-test integer/5 256)
(def serialize-deserialize-test integer/6 -256)
(def serialize-deserialize-test integer/7 -257)
(def serialize-deserialize-test integer/8 1234567890123456789012345678901234567890)
(def serialize-deserialize-test integer/9 -1234567890123456789012345678901234567890)

(def serialize-deserialize-test float/1 0.0)
(def serialize-deserialize-test float/2 1.1)
(def serialize-deserialize-test float/3 -1.1)
(def serialize-deserialize-test float/4 111.1d0)
(def serialize-deserialize-test float/5 -111.1d0)

(def serialize-deserialize-test rational/1 1/2)
(def serialize-deserialize-test rational/2 -1/2)
(def serialize-deserialize-test rational/3 1234567890/9876543210)
(def serialize-deserialize-test rational/4 -1234567890/9876543210)

(def serialize-deserialize-test complex/1 (complex 1.5d0 -0.33d0))

(def serialize-deserialize-test character/1 #\a)

(def serialize-deserialize-test string/1 "")
(def serialize-deserialize-test string/2 "test")
(def serialize-deserialize-test string/3 "áéíóúöőüűÁÉÍÓÚÖŐÜŰ")

(def serialize-deserialize-test proper-list/1 (list nil t))

(def serialize-deserialize-test dotted-list/1 (cons nil t))

(def serialize-deserialize-test cons/1 (let ((cons (cons nil nil)))
                                         (setf (car cons) cons)
                                         (setf (cdr cons) cons)
                                         cons))

(def serialize-deserialize-test simple-vector/1 (coerce #(1 nil t "a") 'simple-vector))
(def serialize-deserialize-test simple-vector/2 (coerce #(0 255) '(simple-vector 2)))

(def serialize-deserialize-test vector/1 (make-array 2 :adjustable #t))

(def serialize-deserialize-test simple-array/1 (make-array '(2 2)))
(def serialize-deserialize-test simple-array/2 (make-array '(2 2) :element-type '(unsigned-byte 16)))

(def serialize-deserialize-test array/1 (make-array '(2 2) :adjustable #t))

(def serialize-deserialize-test hash-table/1 (let ((object (make-hash-table :test 'eql)))
                                               (setf (gethash 'a object) "alma")
                                               (setf (gethash 1 object) 11)
                                               object))

(def serialize-deserialize-test structure-object/1 (make-structure-object-test))
(def serialize-deserialize-test structure-object/2 (make-structure-object-test :slot 1))

(def serialize-deserialize-test standard-object/1 (make-instance 'standard-object-test))
(def serialize-deserialize-test standard-object/2 (make-instance 'standard-object-test :slot 1))

(def serialize-deserialize-test circularity/1 (let ((instance (make-instance 'standard-object-test)))
                                                (setf (slot-of instance) instance)
                                                instance))

#|

(def function cl-store-serialize (object)
  (flexi-streams:with-output-to-sequence (stream)
    (cl-store:store object stream)))

(defvar k (with-call/cc
            (print "Hello")
            (let/cc k k)
            (print "World")))

(length
 (flexi-streams:with-output-to-sequence (stream)
   (cl-store:store k stream)))
800
(cl:time
 (iter (repeat 1000)
       (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                         (cl-store:store k stream)))
         (cl-store:restore stream))))
Evaluation took:
  2.329 seconds of real time
  2.288143 seconds of user run time
  0.0 seconds of system run time
  [Run times include 0.044 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  35,586,656 bytes consed.
NIL



(length (serialize k))
652
(cl:time
 (iter (repeat 1000)
       (deserialize (serialize k))))
Evaluation took:
  0.232 seconds of real time
  0.228014 seconds of user run time
  0.004001 seconds of system run time
  [Run times include 0.012 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  11,797,056 bytes consed.
NIL










(defvar ii '(a (b c) a b (d e (f g (h i)))))


(length
 (flexi-streams:with-output-to-sequence (stream)
   (cl-store:store ii stream)))
158
(cl:time
 (iter (repeat 10000)
       (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                         (cl-store:store ii stream)))
         (cl-store:restore stream))))
Evaluation took:
  4.189 seconds of real time
  4.120258 seconds of user run time
  0.028002 seconds of system run time
  [Run times include 0.112 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  95,359,584 bytes consed.
NIL


(length (serialize ii))
88
(cl:time
 (iter (repeat 10000)
       (deserialize (serialize ii))))
Evaluation took:
  0.394 seconds of real time
  0.392025 seconds of user run time
  0.0 seconds of system run time
  [Run times include 0.028 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  31,204,976 bytes consed.
NIL
|#
