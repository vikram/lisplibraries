;;;; -*- Mode: Common-Lisp; Package: CL-USER; Base: 10 -*-

#| do something useful with this...
(in-package :moptilities)

(defclass testing-class ()
  ((slot-1 :reader rslot-1-a :reader rslot-1-b
           :writer wslot-1-a :writer wslot-1-b
           :accessor aslot-1-a :accessor aslot-1-b)))

(slot-properties 'testing-class 'slot-1)
|#

(in-package #:moptilities-test)

;;; ---------------------------------------------------------------------------
;;; moptilities-test
;;; ---------------------------------------------------------------------------

(deftestsuite moptilities-test () ())

;;; ---------------------------------------------------------------------------
;;; test-get-class
;;; ---------------------------------------------------------------------------

(deftestsuite test-get-class (moptilities-test)
  ())

(addtest (test-get-class)
  get-class-with-symbol
  (ensure-same (cl:find-class 'error) (get-class 'error)))

(addtest (test-get-class)
  get-class-with-instance
  (ensure-same (cl:find-class 'error) (get-class (make-instance 'error))))

(addtest (test-get-class)
  get-class-with-class
  (ensure-same (cl:find-class 'error) (get-class (cl:find-class 'error))))

(addtest (test-get-class)
  get-class-returns-nil-when-nothing-found
  (ensure (not (get-class (gensym)))))

;;; ---------------------------------------------------------------------------
;;; some basic class structure for testing
;;; ---------------------------------------------------------------------------

(defclass l0-a () ())
(defclass l0-b () ())
(defclass l1-a (l0-a) ())
(defclass l1-b (l0-a) ())
(defclass l2-a (l1-a) ())
(defclass l1-a&b (l0-a l0-b) ())
(defclass l3-a (l2-a) ())


;;; ---------------------------------------------------------------------------
;;; test-superclasses
;;; ---------------------------------------------------------------------------

(deftestsuite test-superclasses (moptilities-test) ())

(addtest (test-superclasses)
  test-l0-a 
  (ensure-same (mapcar #'class-name-of (superclasses 'l0-a :proper? nil))
               (list 'l0-a 'standard-object t) :test #'equal))

(addtest (test-superclasses)
  test-l0-a-proper 
  (ensure-same (mapcar #'class-name-of (superclasses 'l0-a))
               (list 'standard-object t) :test #'equal))

(addtest (test-superclasses)
  test-l1-a 
  (ensure-same (mapcar #'class-name-of (superclasses 'l1-a))
               (list 'l0-a 'standard-object t) :test #'equal))

(addtest (test-superclasses)
  test-l1-a&b 
  (ensure-same (mapcar #'class-name-of (superclasses 'l1-a&b))
               (list 'l0-a 'l0-b 'standard-object t) :test #'equal))

(addtest (test-superclasses)
  test-l3-a 
  (ensure-same (mapcar #'class-name-of (superclasses 'l3-a))
               (list 'l2-a 'l1-a 'l0-a 'standard-object t) :test #'equal))

(addtest (test-superclasses)
  test-l3-a-symbol-and-instance 
  (ensure-same (superclasses 'l3-a) (superclasses (make-instance 'l3-a)) 
               :test #'equal))


;;; ---------------------------------------------------------------------------
;;; direct-superclasses
;;; ---------------------------------------------------------------------------

(deftestsuite test-direct-superclasses (moptilities-test) ())

(addtest (test-direct-superclasses)
  test-l0-a 
  (ensure-same (mapcar #'class-name-of (direct-superclasses 'l0-a))
               (list 'standard-object) :test #'equal))

(addtest (test-direct-superclasses)
  test-l1-a 
  (ensure-same (mapcar #'class-name-of (direct-superclasses 'l1-a))
               (list 'l0-a) :test #'equal))

(addtest (test-direct-superclasses)
  test-l1-a&b 
  (ensure-same (mapcar #'class-name-of (direct-superclasses 'l1-a&b))
               (list 'l0-a 'l0-b) :test #'equal))

(addtest (test-direct-superclasses)
  test-l3-a 
  (ensure-same (mapcar #'class-name-of (direct-superclasses 'l3-a))
               (list 'l2-a) :test #'equal))

(addtest (test-direct-superclasses)
  test-l3-a-symbol-and-instance 
  (ensure-same (direct-superclasses 'l3-a) (direct-superclasses (make-instance 'l3-a)) 
               :test #'equal))


;;; ---------------------------------------------------------------------------
;;; subclasses
;;; ---------------------------------------------------------------------------

(deftestsuite test-subclasses (moptilities-test) ())

;;?? is the order really invariant
(addtest (test-subclasses)
  test-l0-a
  (ensure-same (mapcar #'class-name-of (subclasses 'l0-a :proper? nil))
               '(l0-a l1-a&b l1-b l1-a l2-a l3-a) :test #'equal))

(addtest (test-subclasses)
  test-l0-a-proper
  (ensure-same (mapcar #'class-name-of (subclasses 'l0-a :proper? t))
               '(l1-a&b l1-b l1-a l2-a l3-a) :test #'equal))

(addtest (test-subclasses)
  test-l1-a
  (ensure-same (mapcar #'class-name-of (subclasses 'l1-a))
               '(l2-a l3-a) :test #'equal))

(addtest (test-subclasses)
  test-l1-a&b 
  (ensure-same (mapcar #'class-name-of (subclasses 'l1-a&b))
               nil :test #'equal))

(addtest (test-subclasses)
  test-l3-a 
  (ensure-same (mapcar #'class-name-of (subclasses 'l3-a))
               nil :test #'equal))

(addtest (test-subclasses)
  test-l3-a-symbol-and-instance 
  (ensure-same (subclasses 'l0-a) (subclasses (make-instance 'l0-a)) 
               :test #'equal))


;;; ---------------------------------------------------------------------------
;;; direct-subclasses
;;; ---------------------------------------------------------------------------

(deftestsuite test-direct-subclasses (moptilities-test) ())

(addtest (test-direct-subclasses)
  test-l0-a 
  (ensure-same (sort (mapcar #'class-name-of (direct-subclasses 'l0-a))
                     #'string-lessp)
               (list 'l1-a 'l1-a&b 'l1-b) :test #'equal))

(addtest (test-direct-subclasses)
  test-l1-a 
  (ensure-same (mapcar #'class-name-of (direct-subclasses 'l1-a))
               (list 'l2-a) :test #'equal))

(addtest (test-direct-subclasses)
  test-l1-a&b 
  (ensure-same (mapcar #'class-name-of (direct-subclasses 'l1-a&b))
               nil :test #'equal))

(addtest (test-direct-subclasses)
  test-l3-a 
  (ensure-same (mapcar #'class-name-of (direct-subclasses 'l3-a))
               nil :test #'equal))

(addtest (test-direct-subclasses)
  test-l0-a-symbol-and-instance 
  (ensure-same (direct-subclasses 'l0-a) (direct-subclasses (make-instance 'l0-a)) 
               :test #'equal))


;;; ---------------------------------------------------------------------------
;;; slots 'n stuff
;;; ---------------------------------------------------------------------------

#|

slot-names
slot-properties
get-slot-definition
direct-slot-names

reader-method-p
writer-method-p 
map-methods
remove-methods
remove-method-if
generic-functions
direct-specializers-of
specializers-of
mopu-arglist

map-subclasses
leaf-class-p
leaf-subclasses
subclassp

in-order-p
mopu-class-initargs
eql-specializer-p
class-name-of
copy-template

|#


(deftestsuite test-map-methods (moptilities-test) ())

(addtest (test-map-methods) 
  get-error-for-missing-class
  (ensure-error (map-methods 'fffffff #'print))
  (ensure-error (direct-specializers-of 'fffffff)))

;;; ---------------------------------------------------------------------------

(deftestsuite test-function-arglist (moptilities-test) ())

(defun test-function-arglist-1 (a b &optional c)
  (declare (ignore a b c)))

(addtest (test-function-arglist)
  test-1
  (ensure-same (function-arglist 'test-function-arglist-1) 
               '(a b &optional c) :test 'equal))

(defun test-function-arglist-2 (&key a (b 2))
  (declare (ignore a b)))

(addtest (test-function-arglist)
  test-2
  (ensure-same (function-arglist 'test-function-arglist-2) 
               '(&key a b) :test 'equal))

(defun test-function-arglist-3 (&aux a)
  (declare (ignore a)))

(addtest (test-function-arglist)
  test-3
  (ensure-same (function-arglist 'test-function-arglist-3) 
               nil :test 'equal))

