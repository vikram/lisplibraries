;;;; -*- Mode: Common-Lisp; Package: metabang.moptilities; Base: 10 -*-
;;;; some definitions are from the Art of the MOP

(in-package #:cl-user)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel)
  (require 'sb-introspect)              ; for function-arglist
)

(defpackage #:metabang.moptilities
  (:documentation "Moptilities builds on the Lisp Meta-Object Protocol (**MOP**).")
  (:use #:closer-common-lisp)
  (:nicknames #:mopu #:moptilities)
  (:export
   #:map-methods 
   #:remove-methods
   #:remove-methods-if
   #:remove-generic-function
   #:direct-specializers-of
   #:specializers-of
   
   #:generic-functions
   
   #:finalize-class-if-necessary
   
   #:reader-method-p 
   #:writer-method-p
   #:method-name
   #:get-slot-definition

   #:slot-names
   #:default-initargs
   #:direct-slot-names
   #:slot-properties
      
   #:map-subclasses
   #:superclasses
   #:subclasses
   #:direct-subclasses
   #:direct-superclasses
   #:subclassp
       
   #:finalize-class-if-necessary
   #:leaf-class-p
   #:leaf-subclasses
   
   #:class
   #:get-class
   #:get-method
   #:class-name-of
   #:copy-template                      ; lousy name

   #:eql-specializer-p 
   
   #:function-arglist
   #:mopu-class-initargs
   
   #:when-finalized
   #:care-when-finalized
   #:ignore-finalization
   #:*debugging-finalization*)
  
  (:export 
   #:generic-function-methods
   #:method-specializers
   -   
    #+digitool ;; trouble with deglates-to without this
    #:mopu-find-method
   )
  
  ;; MOP bits and pieces we care about
  (:export
   #:method-generic-function
   #:generic-function-name
   #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   
   #:method-combination-name
   ))

#+Ignore
;; not yet, causes packaging problems downstream
(eval-when (:compile-toplevel)
  (let ((syms (loop for sym being the external-symbols of :c2mop
                    collect sym)))
    (import syms "MOPTILITIES")
    (export syms "MOPTILITIES")))

(in-package #:moptilities)

(defmacro nyi (function-name &rest args)
  "Signals an error saying that `function-name` is not yet implemented.  The `args` are ignored."
  (declare (ignore args))
  `(error "The function ~A is not yet implemented for ~A ~A on ~A."
          ,function-name
	  (lisp-implementation-type)
	  (lisp-implementation-version)
	  (machine-type)))


#+digitool
(defun mopu-find-method (gf-name qualifiers &rest specializers)
  (and (fboundp gf-name)
       (let ((def (fdefinition gf-name)))
         (and def
              (find-method def qualifiers
                           (mapcar #'find-class specializers) nil)))))

(defgeneric get-class (thing &key error?)
  (:documentation "Returns the class of thing or nil if the class cannot be found. Thing can be a class, an object representing a class or a symbol naming a class. Get-class is like find-class only not as particular.")
  (:method ((thing symbol) &key error?)
           (find-class thing error?))
  (:method ((thing standard-object) &key error?)
           (declare (ignore error?))
           (class-of thing))
  (:method ((thing t) &key error?) 
           (declare (ignore error?))
           (class-of thing))
  (:method ((thing class) &key error?)
           (declare (ignore error?))
           thing))

;;; ---------------------------------------------------------------------------

(defgeneric subclassp (child parent)
  (:documentation "Returns t if child and parent both specifies classes and child is a subclass of the parent."))

;;; ---------------------------------------------------------------------------

(defmethod subclassp (child parent)
  (let ((child-class (get-class child))
        (parent-class (get-class parent)))
    (finalize-class-if-necessary child-class)
    (not (null (find parent-class (superclasses child-class))))))

;;; ---------------------------------------------------------------------------

(defun finalize-class-if-necessary (thing)
  "Finalizes thing if necessary. Thing can be a class, object or symbol naming a class. Returns the class of thing."
  (let ((class (get-class thing)))
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (values class)))

;;; ---------------------------------------------------------------------------

(defun superclasses (thing &key (proper? t))
  "Returns a list of superclasses of thing. Thing can be a class, object or symbol naming a class. The list of classes returned is 'proper'; it does not include the class itself."
  (finalize-class-if-necessary thing) 
  (let ((result (class-precedence-list (get-class thing))))
    (if proper? (rest result) result)))

;;; ---------------------------------------------------------------------------

(defun direct-superclasses (thing)
  "Returns the immediate superclasses of thing. Thing can be a class, object or symbol naming a class."
  (class-direct-superclasses (get-class thing)))

;;; ---------------------------------------------------------------------------

(defun direct-subclasses (thing)
  "Returns the immediate subclasses of thing. Thing can be a class, object or symbol naming a class."
  (class-direct-subclasses (get-class thing)))

;;; ---------------------------------------------------------------------------

(defgeneric method-name (method)
  (:method ((method standard-method))
           (generic-function-name (method-generic-function method))))

;;; ---------------------------------------------------------------------------

(defgeneric get-method (function qualifiers &rest specializers)
  (:documentation "")
  (:method ((function symbol) qualifiers &rest specializers)
           (declare (dynamic-extent specializers))
           (if (fboundp function)
             (apply #'get-method
                    (fboundp function)
                    qualifiers specializers)
             nil))
  (:method ((function standard-generic-function) qualifiers &rest specializers)
           (declare (dynamic-extent specializers))
           (find-method function qualifiers
               (mapcar #'get-class specializers) nil)))

;;; ---------------------------------------------------------------------------

(defgeneric get-function (function-designator)
  (:documentation "")
  (:method ((function symbol))
           (if (fboundp function)
             (symbol-function function)
             nil))
  (:method ((function standard-generic-function))
           (values function)))

;;; ---------------------------------------------------------------------------

(defun remove-generic-function (function-designator)
  (let ((function (get-function function-designator))) 
    (mapc (lambda (m) 
            (remove-method function m))
          (generic-function-methods function))
    ;; how do you remove a gf?
    (fmakunbound (generic-function-name function))))

;;; ---------------------------------------------------------------------------

(defgeneric slot-names (class)
  (:documentation "Returns a list of the names of the slots of a class ~
\(including both direct and inherited slots\). It's like ~
class-slot-names but on the class, not an instance of the class.")
  (:method ((class class))
           (finalize-class-if-necessary class)
           (mapcar #'slot-definition-name (class-slots class)))
  (:method ((class symbol))
           ;; on sbcl a defstruct creates a structure-class that can be
           ;; find-class'ed and mostly behaves as normal classes.
           (cond #-sbcl((get-structure class nil)
                        #+(or DIGITOOL OPENMCL)
                        (mapcar #'first (rest (aref (get-structure class nil) 1)))
                        #-(or DIGITOOL OPENMCL)
                        (nyi "slot-names for structures"))
                 ((find-class class nil)
                  (slot-names (find-class class)))
                 (t
                  (error "Cannot find class or structure ~A" class))))
  (:method ((class standard-object))
           (slot-names (class-of class)))
  (:method ((class structure-object))
           (slot-names (type-of class))))

;;; ---------------------------------------------------------------------------

(defgeneric slot-properties (class-specifier slot-name)
  (:documentation "Returns a property list describing the slot named slot-name in class-specifier.")
  (:method ((class symbol) slot-name)
           (slot-properties (find-class class) slot-name))
  (:method ((object standard-object) slot-name)
           (slot-properties (class-of object) slot-name))
  (:method ((class class) slot-name)
           (declare (ignorable slot-name))
           (multiple-value-bind (slot-info indirect-slot?)
                                (get-slot-definition class slot-name)
             `(:name ,(slot-definition-name slot-info)
                     ,@(when (eq (slot-definition-allocation slot-info) :class)
                         `(:allocation ,(slot-definition-allocation slot-info)))
                     :initargs ,(slot-definition-initargs slot-info)
                     :initform ,(slot-definition-initform slot-info)
                     ,@(when (and (not (eq (slot-definition-type slot-info) t))
                                  (not (eq (slot-definition-type slot-info) nil)))
                         `(:type ,(slot-definition-type slot-info)))
                     ,@(unless indirect-slot?
                         `(:readers ,(slot-definition-readers slot-info)
                                    :writers ,(slot-definition-writers slot-info)))
                     :documentation ,(documentation slot-info t)))))

;;; ---------------------------------------------------------------------------

#+NotYet
(defgeneric slot-documentation (class-specifier slot-name)
  (:documentation "Returns the documentation for slot-name in the class specified by class-specifier.")
  (:method (class-specifier slot-name)
           (let ((class (get-class class-specifier)))
             (documentation (get-slot-definition class slot-name) t))))

;;; ---------------------------------------------------------------------------

(defgeneric get-slot-definition (class slot-name)
  (:documentation "Returns the slot-definition for the slot named `slot-name` in the class specified by `class-specifier`. Also returns \(as a second value\) true if the slot is an indirect slot of the class.")
  (:method ((class-specifier t) slot-name)
           (let ((class (get-class class-specifier)))
             (let* ((indirect-slot? nil)
                    (slot-info 
                     (or (find slot-name (class-direct-slots class)
                               :key #'slot-definition-name)
                         (and (setf indirect-slot? t)
                              (find slot-name (class-slots class)
                                    :key #'slot-definition-name)))))
               (values slot-info indirect-slot?)))))
  
;;; ---------------------------------------------------------------------------

(defgeneric direct-slot-names (class-specifier)
  (:documentation "Returns a list of the names of the slots that are defined _directly_ in the class-specifier \(as opposed to slots defined in superclasses\).")
  (:method ((class symbol))
           (direct-slot-names (find-class class)))
  (:method ((class standard-object))
           (direct-slot-names (class-of class)))
  (:method ((class class))
           (mapcar #'slot-definition-name (class-direct-slots class))))

(defgeneric reader-method-p (thing)
  (:documentation "Returns true if thing is a reader method (i.e., a subclass of standard-reader-method).")
  #-ecl
  (:method ((m standard-reader-method))
           (values t))
  (:method ((m t))
           (values nil)))

(defgeneric writer-method-p (thing)
  (:documentation "Returns true if thing is a writer method (i.e., a subclass of standard-writer-method).")
  #-ecl
  (:method ((m standard-writer-method))
           (values t))
  (:method ((m t))
           (values nil)))

(defun map-methods (thing fn)
  "Applys fn to all of the direct methods of thing (which can be a class, object or symbol naming a class). The function should take two arguments: a generic function and a method."
  (let ((class (get-class thing)))
    (if class
      (loop for gf in (specializer-direct-generic-functions class) do
            (loop for m in (generic-function-methods gf) do
                  (when (member class (method-specializers m))
                    (funcall fn gf m))))
      (error "Unable to get-class of ~A" thing))))

(defun remove-methods (class-specifier &rest args 
                                       &key (dry-run? nil) (verbose? dry-run?)
                                       (ignore-errors? nil))
  "Removes all methods associated with class-specifier. Class-specifier can be a class, object representing a class or symbol naming a class. If dry-run? is true \(and verbose? is also true\), then the methods that would be removed are printed but no methods are actually removed. Returns the number of methods that are removed \(or that would have been removed if dry-run? is true\)."
  (declare (ignore verbose? ignore-errors?)
           (dynamic-extent args))
  (apply #'remove-methods-if class-specifier (constantly t) args))

;;; ---------------------------------------------------------------------------

(defun remove-methods-if (class-specifier predicate &key (dry-run? nil) (verbose? dry-run?)
                                          (ignore-errors? nil))
  "Removes all methods associated with class-specifier that pass a predicate. Class-specifier can be a class, object representing a class or symbol naming a class. The predicate should be a function of two arguments: a generic-function and a method. If dry-run? is true \(and verbose? is also true\), then the methods that would be removed are printed but no methods are actually removed. Returns the number of methods that are removed \(or that would have been removed if dry-run? is true\)."
  (let ((class (get-class class-specifier))
        (count 0))
    (if class
      (map-methods class 
                   (lambda (gf m) 
                     (when (funcall predicate gf m)
                       (incf count)
                       (when verbose?
                         (format t "~&~A" m))
                       (unless dry-run?
                         (remove-method gf m)))))
      (unless ignore-errors?
        (error "Class '~A' not found." class-specifier)))
    (values count)))

;;; ---------------------------------------------------------------------------

#+Ignore
(defun display-methods (classname)
  (map-methods classname (lambda (gf m) (format t "~&~A~%  ~A" gf m))))

;;; ---------------------------------------------------------------------------

(defun generic-functions (thing)
  "Returns a list of all of the direct generic-functions associated with thing. Thing can be a class, object, or symbol naming a class."
  (let ((result nil))
    (map-methods thing
                 (lambda (gf m) 
                   (declare (ignore m))
                   (pushnew gf result)))
    result))

;;; ---------------------------------------------------------------------------

(defun direct-specializers-of (thing &key (writers? t) (readers? t)
                                     (other? t) (short-form? t))
  "Returns a list of the direct specializers of thing. Thing can a class, object representing a class or symbol naming a class. The keyword arguments :readers?, :writers?, and :other? control which specializers are returned \(reader methods, writer methods and other methods respectively\). The keyword argument :short-form? controls whether a list of methods is returned or just a list of names."
  (let ((result nil)
        (transform (if short-form? #'method-name #'identity)))
    (map-methods thing
                 (lambda (gf m)
                   (declare (ignore gf))
                   (when (or (and (reader-method-p m)
                                  readers?)
                             (and (writer-method-p m)
                                  writers?)
                             (and (not (reader-method-p m))
                                  (not (writer-method-p m))
                                  other?))
                     (push (funcall transform m) result))))
    (nreverse result)))

;;; ---------------------------------------------------------------------------

(defun specializers-of (class &rest args &key short-form? writers? readers? other?
                              (ignore-classes '(t standard-object)))
  (declare (ignore writers? readers? other? short-form?))
  "Like direct-specializers-of but returns all the specializers, not just the direct ones."
  (remf args :ignore-classes)
  (setf ignore-classes (mapcar #'find-class (if (consp ignore-classes)
                                              ignore-classes
                                              (list ignore-classes))))
  (delete-duplicates
   (apply #'nconc
          (mapcar (lambda (super-class)
                    (unless  (member super-class ignore-classes)
                      (apply #'direct-specializers-of super-class args)))
                  (append (list class) (superclasses class))))))

;;; ---------------------------------------------------------------------------

(defun map-subclasses (class fn &key proper?)
  "Applies fn to each subclass of class. If proper? is true, then
the class itself is not included in the mapping. Proper? defaults to nil."
  (let ((mapped (make-hash-table :test #'eq)))
    (labels ((mapped-p (class)
               (gethash class mapped))
             (do-it (class root)
               (unless (mapped-p class)
                 (setf (gethash class mapped) t)
                 (unless (and proper? root)
                   (funcall fn class))
                 (mapc (lambda (class)
                         (do-it class nil))
                       (class-direct-subclasses class)))))
      (do-it (get-class class) t))))

;;; ---------------------------------------------------------------------------

(defun subclasses (class &key (proper? t))
  "Returns all of the subclasses of the class including the class itself."
  (let ((result nil))
    (map-subclasses class (lambda (class)
                            (push class result))
                    :proper? proper?)
    (nreverse result)))

;;; ---------------------------------------------------------------------------

(defun in-order-p (c1 c2)
  (flet ((in-order-at-subclass-p (sub)
           (let ((cpl (superclasses sub)))
             (not (null (member c2 (cdr (member c1 cpl))))))))
    (or (eq c1 c2)
        (every #'in-order-at-subclass-p 
               (intersection (subclasses c1 :proper? nil)
                             (subclasses c2 :proper? nil))))))

;;; ---------------------------------------------------------------------------
;;; structures
;;; ---------------------------------------------------------------------------

(defun get-structure (name &optional (errorp t))
  "get-structure name

If `name' has been defined as a structure then return its
description.  Otherwise signal an error if errorp is t."
  (declare (ignorable name errorp))
  #+(or digitool openmcl lispworks4 allegro)
  (let ((found (or #+(or digitool openmcl) (ccl::structure-class-p name)
                   #+allegro
                    (let ((plist (excl::symbol-plist name)))
                               (and plist
                                    (listp plist)
                                    (equal  "EXCL::%STRUCTURE-DEFINITION"
                                            (format nil "~S" (first plist)))))
                   #+Lispworks4 (structure:type-structure-predicate name))))
    (cond (found
           (values t))
          (t
           (when errorp
             (error "~s is not the name of a defstruct." name)))))
  #-(or digitool openmcl lispworks allegro)
  (nyi "get-structure")) 

#+Ignore
(defun path-from-class-to-class (sub super)
  "Returns a list representing a path of classes that runs from sub to super" 
  (let ((super-class (get-class super)))
    (let ((candidates (direct-superclasses sub)))
      (cond ((null candidates) nil) 
            ((find super-class candidates)
             (values super-class))
            (t (dolist (c candidates)
                 (let ((it (path-from-class-to-class c super-class))) 
                   (when it 
                     (print it)  
                     (return c)))))))))

(defun function-arglist (symbol)
  "Returns two values, the arglist of symbol"
  #+(or digitool openmcl)
  (ccl:arglist symbol)
  #+lispworks
  (lw:function-lambda-list symbol)
  #+allegro
  (common-lisp-user::arglist symbol)
  #+sbcl
  (sb-introspect:function-arglist (fdefinition symbol))
  #+cmu 
  (cmu-arglist symbol) 
  #-(or digitool openmcl lispworks allegro sbcl cmu)
  (nyi "function-arglist"))

#+cmu
(defun cmu-arglist (x)
  "Adds FUNCTION-ARGLIST compatibility for CMUCL."
    (typecase x
      (symbol (cmu-arglist (or (macro-function x) (symbol-function x))))
      (standard-generic-function (pcl:generic-function-lambda-list x))
      (eval:interpreted-function (eval:interpreted-function-arglist x))
      (compiled-function 
       (values (read-from-string
		(or (kernel:%function-arglist (lisp::%closure-function x))
		    "(unreadable-arglist)"))))))


(defun mopu-class-initargs (thing) 
  (let ((class (get-class thing)))
    (declare (ignorable class))
    #+(or DIGITOOL OPENMCL)
    (ccl::class-slot-initargs class)
    #+lispworks
    (lw-tools::class-initargs class)
    #+allegro
    nil
    #-(or DIGITOOL OPENMCL LISPWORKS4 allegro)
    (nyi "mopu-class-initargs")))

;;; ---------------------------------------------------------------------------

(defgeneric eql-specializer-p (thing)
  (:documentation "If thing is an eql-specializer, returns a representation of thing as \(eql <object>\).")
  #-(or lispworks ecl)
  (:method ((thing eql-specializer))
           (list 'eql (eql-specializer-object thing)))
  (:method ((thing t))
           #-(or lispworks ecl) 
           (values nil)
           #+(or lispworks ecl)
           (typep thing 'eql-specializer))
  #+digitool
  (:method ((thing cons)) 
           ;; don't ask, don't tell
           thing))

;;; ---------------------------------------------------------------------------

(defun default-initargs (class-specifier)
  "Returns a list of default initarg information for the class-specifier. This list consists of triples in the format <initarg value function>. The initarg is the initarg corresponding to the default-initarg; the value is the value it will default to and the function is a function of zero-arguments that returns value... \(this is subject to minor changes\)."
  (delete-duplicates
   (append 
    (class-direct-default-initargs (get-class class-specifier))
    (class-default-initargs (get-class class-specifier)))
   :test #'equal)
  #+Old
  (compute-default-initargs (get-class class-specifier)))
                     

;;; ---------------------------------------------------------------------------
;;; leaf classes
;;; ---------------------------------------------------------------------------

(defun leaf-class-p (thing)
  "Returns true if the class has no subclasses."
  (map-subclasses thing
                  (lambda (subclass)
                    (declare (ignore subclass))
                    (return-from leaf-class-p nil))
                  :proper? t)
  (values t))

;;; ---------------------------------------------------------------------------

(defun leaf-subclasses (thing)
  "Returns a list of subclasses of thing that have no subclasses of their own; i.e., the leaves of the class tree rooted at thing. Thing can be a class, object or symbol naming a class." 
  (let ((result nil))
    (map-subclasses thing 
                    (lambda (class)
                      (when (leaf-class-p class)
                        (push class result))))
    (values result)))

;;; ---------------------------------------------------------------------------

#+No
;;?? Gary King 2005-11-16: bad lisp, down boy
(defgeneric (setf class) (class object)
  (:documentation "")
  (:method ((class symbol) (object standard-object))
           (change-class object (find-class class)))
  (:method ((class standard-object) (object standard-object))
           (setf (class object) (class-of class)))
  (:method ((class class) (object standard-object))
           (change-class object class)))

#| a macro to handle this would be good but we don't have much machinery 
   around to help since moptilities is supposed to down in the foundations.


;;?? grrr, what is minimal! What is the root!!
(defmacro with-unique-names ((&rest vars) &body body)
  "Binds the symbols in VARS to gensyms.  cf with-gensyms."
  (assert (every #'symbolp vars) () "Can't rebind an expression.")
  `(let ,(mapcar #'(lambda (x) `(,x (gensym))) vars)
     ,@body))

;;?? how to munge lambda-list properly...

(defmacro build-generalized-mopu-method (name lambda-list &body body)
  (with-unique-names (args)
    (let ((arglist (rest lambda-list)))
      `(defgeneric ,name ,lambda-list
         ; (:documentation ,documentation)
         (:method ((classname symbol) ,@arglist)
                  ,@body)
         (:method ((object standard-object) &rest ,args ,@arglist)
                  (declare (dynamic-extent ,args))
                  (apply #',name (class-name-of object) ,args))
         (:method ((class class) &rest ,args ,@arglist)
                  (declare (dynamic-extent ,args))
                  (apply #',name (class-name class) ,args))))))
              
;;; ---------------------------------------------------------------------------

(build-generalized-mopu-method direct-specializers-of (class &key writers? readers? other? short-form?)
  ;;?? does this already have a name in the MOP
  (let ((result nil))
    (map-methods classname
                 (lambda (gf m)
                   (declare (ignore gf))
                   (when (or (and (reader-method-p m)
                                  readers?)
                             (and (writer-method-p m)
                                  writers?)
                             (and (not (reader-method-p m))
                                  (not (writer-method-p m))
                                  other?))
                     (push m result))))
    (when short-form?
      (setf result (delete-duplicates (mapcar #'method-name result))))
    (nreverse result)))

(build-generalized-mopu-method mopu-class-initargs (class)
  #+(or DIGITOOL OPENMCL)
  (ccl::class-slot-initargs class)
  #+lispworks
  (lw-tools::class-initargs class)
  #-(or DIGITOOL OPENMCL LISPWORKS4)
  (nyi "mopu-class-initargs"))

(build-generalized-mopu-method leaf-subclasses (class)
  (let ((result nil))
    (map-subclasses class 
                    (lambda (class)
                      (when (leaf-class-p class)
                        (push class result))))
    (values result)))

|#

;;; ---------------------------------------------------------------------------

(defgeneric class-name-of (thing)
  (:documentation "Returns the name of thing's class.")
  (:method ((thing standard-object)) 
           (class-name (class-of thing)))
  (:method ((thing class))
           (class-name thing)))

;;; ---------------------------------------------------------------------------

(defgeneric copy-template (object)
  (:documentation "
Suppose you make an instance of the class foo:

    (defclass foo ()
      ((test :accessor test :initform #'equal :initarg :test)))

    (setf *foo* (make-instance 'foo :test #'eql))

Its `test` slot will be set to #'eql: 

    (test *foo*) => #'eql 

If you want to make a structural clone (for lack of a better term) 
of `*foo*`, you might try: 

    (setf *new-foo* (make-instance (type-of *foo*)))

But `*new-foo*`'s test slot won't be set properly:

    (test *new-foo*) => #'equal

For simple classes, this is no problem but suppose we have 
a graph from [CL-Graph][] and want to make a copy of that: 

    (make-graph (type-of old-graph)
     :vertex-test (vertex-test old-graph) 
     :vertex-key (vertex-key old-graph)
     :edge-test (edge-test old-graph)
     :edge-key (edge-key old-graph)
     :default-edge-type (default-edge-type old-graph)
     :default-edge-class (default-edge-class old-graph)
     :directed-edge-class (directed-edge-class old-graph)
     :undirected-edge-class (undirected-edge-class old-graph))))

Yuck!

Copy-template is a reasonable, though not perfect, solution to this 
problem; it creates a structural copy of an object as such that the copy
has all of its initargs correctly set. 

  [CL-Graph]: http://common-lisp.net/projects/cl-graph/

"))

;;; ---------------------------------------------------------------------------

(defmethod copy-template ((object standard-object))
  (apply 
   #'make-instance 
   (type-of object) 
   (loop
      for (name initargs allocation) in 
        (mapcar #'(lambda (slot)
                    (list (slot-definition-name slot)
                          (slot-definition-initargs slot)
                          (slot-definition-allocation slot)))
                (class-slots (class-of object))) 
      when (and initargs
                (slot-boundp object name)
                (not (eql allocation :class)))
      nconc (list (first initargs)
                  (slot-value object name)))))


(defvar *debugging-finalization* nil
  "When true, finalization messages are printed to *debug-io*")

(defgeneric when-finalized (thing)
  (:documentation "Called just before an object is garbage collected if care-when-finalize has been called on the object."))

(defmethod when-finalized ((object t))
  nil)

(defmethod when-finalized :around ((object t))
  (call-next-method)
  (when *debugging-finalization*
    (format *debug-io* "~%Finalized ~S" object)))

(defun care-when-finalized (object)
  "Ensures the when-finalized is called on the object just before it is garbage collected."
  #+(or digitool openmcl)
  (ccl:terminate-when-unreachable object)
  #+allegro
  (excl:schedule-finalization object 'when-finalized)
  #+sbcl
  (sb-ext:finalize object 'when-finalized)
  #-(or digitool openmcl allegro sbcl)
  (nyi 'care-when-finalized object))

(defun ignore-finalization (object)
  "Prevents care-when-finalized from being called on object just before it is garbage collected."
  (declare (ignorable object))
  #+(or digitool openmcl)
  (ccl:cancel-terminate-when-unreachable object)
  #-(or digitool openmcl)
  (nyi 'care-when-finalized object))

#+(or digitool openmcl)
(defmethod ccl:terminate :after ((object t))
  (when-finalized object))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
