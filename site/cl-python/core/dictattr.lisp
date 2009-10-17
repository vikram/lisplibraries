;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *user-readtable*)

;;; Funky dict: alist or hashtable

(defun funky-dict-get (dict attr)
  (if (hash-table-p dict)
      (gethash attr dict)
    (cdr (assoc attr dict :test #'eq))))

(defun funky-dict-set (dict attr val)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (hash-table-p dict)
      (progn (setf (gethash attr dict) val)
             dict)
    (let ((old (assoc attr dict :test #'eq)))
      (cond (old
             (progn (setf (cdr old) val)
                    dict))
            ((< (length dict) clpython.package::+dict-alist-to-hashtable-threshold+)
             (acons attr val dict))
            (t (loop with ht = (make-eq-hash-table (format nil "dict #keys > ~A [~A]"
                                                           clpython.package::+dict-alist-to-hashtable-threshold+ dict))
                   for (k . v) in dict
                   do (setf (gethash k ht) v)
                   finally (setf (gethash attr ht) val)
                           (return ht)))))))

(defun funky-dict-del (dict attr)
  (if (hash-table-p dict)
      (values dict (remhash attr dict))
    (clpython.package::alist-remove-prop dict attr)))

(defun funky-dict-map (dict func)
  (if (hash-table-p dict)
      (maphash func dict)
    (loop for (k . v) in dict do (funcall func k v))))

;;; Class dict handling

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun class-slot-ix (name &rest classes)
    (flet ((slot-ix-in (cls-name)
             (let* ((cls (if (typep cls-name 'class) cls-name (find-class cls-name)))
                    (slot (or (find name (closer-mop:class-slots cls) :key #'closer-mop:slot-definition-name)
                             (error "Class ~A has no slot named ~A." cls name))))
               (closer-mop:slot-definition-location slot))))
      (assert (apply #'= (mapcar #'slot-ix-in classes)))
      (slot-ix-in (car classes)))))

(defconstant-once +py-class-dict-slot-index+
    (class-slot-ix 'dict 'py-type 'py-meta-type))

(defconstant-once +py-class-classname-slot-name+
  #+allegro 'excl::name
  #+ccl 'ccl::name
  #+cmu 'pcl::name
  #+lispworks 'clos::name
  #+sbcl 'sb-pcl::name
  #-(or allegro ccl cmu lispworks sbcl) 
  (break "Define slot name containing class name, for this implementation."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +use-standard-instance-access-setf+
    #+allegro t
    #+ccl t
    #+cmu nil ;; CMUCL lacks (SETF PCL:STANDARD-INSTANCE-ACCESS)
    #+lispworks t
    #+sbcl t
    #-(or allegro ccl cmu lispworks sbcl) (error "Define +use-standard-instance-access-setf+ ~
for this implementation"))

  (register-feature :clpython-use-standard-instance-access-setf +use-standard-instance-access-setf+))

(defconstant-once +py-class-classname-slot-index+
  (class-slot-ix +py-class-classname-slot-name+ 'py-type 'py-meta-type))

(defconstant-once +dicted-object-dict-index+
    (class-slot-ix 'dict 'dicted-object))

;; Dave Fox on Lispworks' standard-instance-access functionality:
;;   http://thread.gmane.org/gmane.lisp.lispworks.general/1818/focus=1830
;; "CLOS::STANDARD-INSTANCE-ACCESS is slightly different to the AMOP
;; function STANDARD-INSTANCE-ACCESS, in that it deals with class
;; slots. It allows slot names as well as integers for its LOCATION
;; argument (and so tests that argument). Also it provides a writer.
;; 
;; CLOS::FAST-STANDARD-INSTANCE-ACCESS is essentially the same as the
;; AMOP function STANDARD-INSTANCE-ACCESS, though it also offers a
;; writer."

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +standard-instance-access-func+
    #+lispworks 'clos::fast-standard-instance-access
    #-lispworks 'closer-mop:standard-instance-access))

(defun class.raw-dict (class)
  (#.+standard-instance-access-func+ class +py-class-dict-slot-index+))

(define-compiler-macro class.raw-dict (class)
  `(#.+standard-instance-access-func+ ,class +py-class-dict-slot-index+))

(defun class.raw-classname (class)
  (#.+standard-instance-access-func+ class +py-class-classname-slot-index+))

(define-compiler-macro class.raw-classname (class)
  `(#.+standard-instance-access-func+ ,class +py-class-classname-slot-index+))

(defun (setf class.raw-dict) (new-val class)
  #+clpython-use-standard-instance-access-setf
  (setf (#.+standard-instance-access-func+ class +py-class-dict-slot-index+) new-val)
  #-clpython-use-standard-instance-access-setf
  (setf (slot-value class 'dict) new-val))


(defun class.raw-attr-get (class attr)
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((dict (class.raw-dict class)))
    (funky-dict-get dict attr)))

(defun class.raw-attr-set (class attr val)
  ;(declare (optimize (speed 3) (safety 1) (debug 0)))
  (clear-ca-cache)
  (let* ((dict (class.raw-dict class))
         (dict2 (funky-dict-set dict attr val)))
    (unless (eq dict dict2)
      (setf (class.raw-dict class) dict2))))

(defun class.raw-attr-del (class attr)
  "Returns whether existed."
  ;(declare (optimize (speed 3) (safety 1) (debug 0)))
  (clear-ca-cache)
  (let ((dict (class.raw-dict class)))
    (multiple-value-bind (dict2 found)
        (funky-dict-del dict attr)
      (unless (eq dict dict2)
        (setf (class.raw-dict class) dict2))
      found)))

(defun class.raw-attr-map (class func)
  (let ((dict (class.raw-dict class)))
    (funky-dict-map dict func)))

;;; Class attribute cache

(defstruct (class-attr (:conc-name ca.))
  getattribute class-val-dd getattr class-val-non-dd class-val-class)

(defparameter *ca-cache* (make-hash-table :test 'equal)
  "Mapping from (class, attr) to class-attr struct.")

(defun get-ca (class attr)
  (declare (optimize (speed 3) (safety 0) (debug 0)) #+(or)(class class))
  (let ((cn #+(or)(class-name class)
            (class.raw-classname class)))
    (let ((plist (symbol-plist cn)))
      (loop 
        (when (eq (pop plist) attr)
          (return-from get-ca (car plist)))
        (setf plist (cdr plist))
        (unless plist
          (return-from get-ca (setf (get cn attr) (get-ca-1 class attr))))))
    #+(or)
    (or (get cn attr)
        (setf (get cn attr) (get-ca-1 class attr)))))

(defun get-ca-1 (class attr)
  "Retrieve CLASS-ATTR."
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((key (cons class attr)))
    (declare (dynamic-extent key))
    (or (gethash key *ca-cache*)
        (setf (gethash (cons class attr) *ca-cache*)
          (calculate-ca class attr)))))

(defmacro do-cpl ((c class) &body body)
  "Loop over the superclasses relevant for attribute lookup."
  `(dolist (,c (closer-mop:class-precedence-list ,class))
     (when (typep ,c (ltv-find-class 'dict-mixin))
       ,@body)))

(defun calculate-ca (class attr)
  ;; There are two modes for retrieving an attribute, characterized by whether the
  ;; special methods __getattr__ and __getattribute__ are taken into account.
  ;; Here we create a struct for caching that can be used for both.
  ;; An overview:  the rows indicate the possible situations
  ;;               x means relevant if special methods taken into account
  ;;               # means relevant if special methods not taken into account 
  ;;
  ;;     getattribute | classval-dd | getattr |  | classval-non-dd | instance-dict
  ;; (1)      x             #                             #
  ;; (2)      x             #            x                #
  ;; (3)                    x                       
  ;; (4)                                 x
  ;; (5)                                                  x               x
  ;; (6)                                                                  x
  ;(declare (optimize (speed 3) (safety 1) (debug 0)))
  (let (__getattr__ __getattribute__ attr-val attr-val-class attr-is-dd)
    (do-cpl (c class)
      (unless __getattribute__
        (whereas ((x (class.raw-attr-get c '{__getattribute__})))
          (setf __getattribute__ x)))
      (unless attr-val
        (whereas ((x (class.raw-attr-get c attr)))
          (setf attr-val x
                attr-val-class c
                attr-is-dd (data-descriptor-p x))))
      (unless __getattr__
        (whereas ((x (class.raw-attr-get class '{__getattr__})))
          (setf __getattr__ x))))
    (make-class-attr :getattribute __getattribute__
                     :getattr __getattr__
                     :class-val-dd (when attr-is-dd attr-val)
                     :class-val-non-dd (when (not attr-is-dd) attr-val)
                     :class-val-class attr-val-class)))

(defun clear-ca-cache ()
  ;; TODO: only remove what's outdated, e.g. a class and its sublasses.
  (loop for (class . attr) being the hash-key in *ca-cache*
      do (remprop (class-name class) attr))
  (clrhash *ca-cache*))

;;; Attribute lookup

(defun class.attr-no-magic (class attr)
  "Retrieve class.attr skipping magic hooks. Returns VALUE, FOUND-IN-CLS."
  (let ((ca (get-ca class attr)))
    (values (or (ca.class-val-dd ca)
                (ca.class-val-non-dd ca))
            (ca.class-val-class ca))))

(defun x.class-attr-no-magic.bind (x attr)
  (let ((x.cls (py-class-of x)))
    (whereas ((m (class.attr-no-magic x.cls attr)))
      (bind-val m x x.cls)))) 
  
(define-compiler-macro class.attr-no-magic (class attr)
  ;; Optimize the function calling and structure access.
  `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
     (let ((ca (get-ca ,class ,attr)))
       (or (ca.class-val-dd ca)
           (ca.class-val-non-dd ca)))))

(defun instance.attr-no-magic (inst attr)
  (funky-dict-get (dict inst) attr))

(define-compiler-macro instance.attr-no-magic (inst attr)
  `(funky-dict-get (dict ,inst) ,attr))

(defun instance.attr-get (x attr)
  "Retrieve attribute from instance dict, skipping magic hooks. Instance may be a class."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;(check-type attr symbol)
  (cond ((my-classp-1 x)
         (class.attr-no-magic x attr))
        ((has-dict x)
         #+(or)(funky-dict-get (closer-mop:standard-instance-access x +dicted-object-dict-index+) attr)
         (funky-dict-get (dict x) attr))))

(defun attr (x attr)
  "Retrieve attribute value x.attr using all the magic hooks.
Returns NIL if not found."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;(check-type attr symbol)
  (let* ((x.cls (py-class-of x))
         (ca (get-ca x.cls attr)))
    ;; __getattribute__ (and perhaps __getattr__)
    (whereas ((g (ca.getattribute ca)))
      (let ((ga (ca.getattr ca)))
        (if ga
            (handler-case (return-from attr (py-call (bind-val g x x.cls) (symbol-name attr)))
              ({AttributeError} () (py-call (bind-val ga x x.cls) (symbol-name attr))))
          (return-from attr (py-call (bind-val g x x.cls) (symbol-name attr))))))
    ;; class value that is data descriptor
    (whereas ((v (ca.class-val-dd ca)))
      (return-from attr (bind-val v x x.cls)))
    ;; __dict__
    (when (eq attr '{__dict__})
      (return-from attr (dict x)))
    (when (eq attr '{__class__})
      (return-from attr (py-class-of x)))
    (when (and (eq attr '{__name__})
               (functionp x))
      (return-from attr (function-name x)))
    ;; instance dict
    (whereas ((v (instance.attr-get x attr)))
      (return-from attr v))
    ;; class non-data-descriptor
    (whereas ((v (ca.class-val-non-dd ca)))
      (return-from attr (bind-val v x x.cls)))
    ;; __getattr__
    (whereas ((v (ca.getattr ca)))
      (py-call (bind-val v x x.cls) (symbol-name attr))))
  (py-raise '{AttributeError}
            "Object ~A has no attribute `~A'."
            x attr))

(defun bind-val (val x x.class)
  (assert (and x x.class))
  (whereas ((meth (class.attr-no-magic (py-class-of val) '{__get__})))
    (return-from bind-val (py-call meth val x x.class)))
  val)

(defun (setf attr) (val x attr)
  "Val = NIL indicates attribute deletion. Returns NIL when failed."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (special *the-none*))
  ;(check-type attr symbol)
  (let ((x.cls (py-class-of x)))
    (if (not val)
        (progn
          ;; 1. class.__delattr__
          (whereas ((v (class.attr-no-magic x.cls '{__delattr__})))
            (return-from attr (py-call (bind-val v x x.cls) (symbol-name attr))))
          ;; 2. class.attr.__del__
          (whereas ((cls-attr (class.attr-no-magic x.cls attr))
                    (v (x.class-attr-no-magic.bind cls-attr '{__del__})))
            (if (eq v *the-none*)
                (py-raise '{AttributeError}
                          "Cannot delete attribute ~A of ~A, as ~A.~A.__del__ is None."
                          x attr x attr)
              (return-from attr (py-call v x)))) ;; XXX check args
          ;; XXX check del of __dict__ / __class__ ?
          ;; 3. Instance dict
          (if (my-classp-1 x)
              (when (class.raw-attr-del x attr)
                (return-from attr))
            (whereas ((d (dict x)))
              (multiple-value-bind (d2 found)
                  (funky-dict-del d attr)
                (when found
                  (unless (eq d d2)
                    (setf (dict x) d2))
                  (return-from attr)))))
          ;; 4. Failed
          (py-raise '{AttributeError} "Cannot delete attribute ~A of ~A." attr x))
      (progn
        ;; 1. class.__setattr__
        (whereas ((v (class.attr-no-magic x.cls '{__setattr__})))
          (return-from attr (py-call (bind-val v x x.cls) (symbol-name attr) val)))
        ;; 2. class.attr.__set__
        (whereas ((cls-attr (class.attr-no-magic x.cls attr))
                  (v (x.class-attr-no-magic.bind cls-attr '{__set__})))
          (if (eq v *the-none*)
              (py-raise '{AttributeError}
                        "Cannot set attribute ~A of ~A, as ~A.~A.__set__ is None."
                        x attr x attr)
            (return-from attr (py-call v x val))))
        ;; 3. Specials attributes: __dict__, __class__
        (case attr
          ({__dict__}  (setf (dict x) val)
                       (return-from attr))
          ({__class__} (setf (py-class-of x) val)
                       (return-from attr))
          ({__name__}  (when (and (find-class 'py-function)
                                  (typep x 'py-function))
                         (clpython.user::py-function.__name__-writer x val)
                         (return-from attr))))
        ;; 4. Dict
        (when (my-classp-1 x)
          (class.raw-attr-set x attr val)
          (return-from attr))
        (when (has-dict x)
          (let* ((d (dict x))
                 (d2 (funky-dict-set (dict x) attr val)))
            (unless (eq d d2)
              (setf (dict x) d2))
            (return-from attr)))
        ;; 5. Failed
        (py-raise '{AttributeError} "Cannot set attribute ~A of ~A." attr x)))))


;;; Descriptors

(defun descriptor-p (x)
  (let ((x.class (py-class-of x)))
    (or (class.attr-no-magic x.class '{__get__})
	(class.attr-no-magic x.class '{__set__})
	(class.attr-no-magic x.class '{__delete__}))))

(defun data-descriptor-p (x)
  "Returns DES-P, __SET__"
  (let ((x.class (py-class-of x)))
    (or (class.attr-no-magic x.class '{__set__})
        (class.attr-no-magic x.class '{__delete__}))))
