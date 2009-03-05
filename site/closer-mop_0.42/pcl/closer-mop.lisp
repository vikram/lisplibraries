(in-package :closer-mop)

;; Some utility functions.

(defun required-args (lambda-list &optional (collector #'identity))
  (loop for arg in lambda-list
        until (member arg lambda-list-keywords)
        collect (funcall collector arg)))

(defun ensure-finalized (class &optional (errorp t))
  (if (typep class 'class)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (when errorp (error "~S is not a class." class)))
  class)

;; The following is commented out. SBCL now supports compatible standard-class and
;; funcallable-standard-class metaclasses, but this requires that we don't mess with
;; the class hierarchy anymore. So we will try the trick we have already used
;; for generic functions: We just add methods for the existing metaclasses.
;; This is not AMOP-compliant, but if it works it works.

#|
;; We need a new standard-class for various things.

(cl:defclass standard-class (cl:standard-class)
  ())

;; validate-superclass for metaclass classes is a little bit
;; more tricky than for class metaobject classes because
;; we don't want to make all standard-classes compatible to
;; each other.

;; Our validate-superclass may get passed a class-prototype
;; as its second argument, so don't expect its readers to
;; yield useful information. (In ANSI parlance, "the
;; consequences are undefined...")

(cl:defmethod validate-superclass
           ((class standard-class)
            (superclass cl:standard-class))
  (or (when (eq (class-of class) (find-class 'standard-class))
        (or (eq (class-of superclass) (find-class 'cl:standard-class))
            (eq (class-of superclass) (find-class 'standard-class))))
      (call-next-method)
      (when (eq (class-of superclass) (find-class 'cl:standard-class))
        (validate-superclass class (class-prototype (find-class 'standard-class))))))

#+sbcl
(cl:defmethod validate-superclass
           ((class funcallable-standard-class)
            (superclass standard-class))
  (and (eq (class-of class) (find-class 'funcallable-standard-class))
       (eq (class-of superclass) (find-class 'standard-class))))

;; The following macro ensures that the new standard-class is used
;; by default. It would have been useful to fix other deficiencies
;; in a complete redefinition of defclass, but there is no portable
;; way to ensure the necessary compile-time effects as specified
;; by ANSI Common Lisp. Therefore, we just expand to the original
;; cl:defclass.
    
(defmacro defclass (name (&rest supers) &body options)
  (if (member :metaclass options :key #'car)
      `(cl:defclass ,name ,supers ,@options)
    `(cl:defclass ,name ,supers ,@options
       (:metaclass standard-class))))
|#

;; In CMUCL, reader-method-class and writer-method-class are
;; not used during class initialization. The following definitions
;; correct this.

#-sbcl
(defun modify-accessors (class)
  (loop with reader-specializers = (list class)
        with writer-specializers = (list (find-class 't) class)
        for slotd in (class-direct-slots class) do
        (loop for reader in (slot-definition-readers slotd)
              for reader-function = (fdefinition reader)
              for reader-method = (find-method reader-function () reader-specializers)
              for initargs = (list :qualifiers ()
                                   :lambda-list '(object)
                                   :specializers reader-specializers
                                   :function (method-function reader-method)
                                   :slot-definition slotd)
              for method-class = (apply #'reader-method-class class slotd initargs)
              unless (eq method-class (class-of reader-method))
              do (add-method reader-function (apply #'make-instance method-class initargs)))
        (loop for writer in (slot-definition-writers slotd)
              for writer-function = (fdefinition writer)
              for writer-method = (find-method writer-function () writer-specializers)
              for initargs = (list :qualifiers ()
                                   :lambda-list '(new-value object)
                                   :specializers writer-specializers
                                   :function (method-function writer-method)
                                   :slot-definition slotd)
              for method-class = (apply #'writer-method-class class slotd initargs)
              unless (eq method-class (class-of writer-method))
              do (add-method writer-function (apply #'make-instance method-class initargs)))))

;; The following methods additionally create a gensym for the class name
;; unless a name is explicitly provided. AMOP requires classes to be
;; potentially anonymous.

#-sbcl
(cl:defmethod initialize-instance :around
  ((class standard-class) &rest initargs
   &key (name (gensym)))
  (declare (dynamic-extent initargs))
  (prog1 (apply #'call-next-method class :name name initargs)
    (modify-accessors class)))

#-sbcl
(cl:defmethod initialize-instance :around
  ((class funcallable-standard-class) &rest initargs
   &key (name (gensym)))
  (declare (dynamic-extent initargs))
  (prog1 (apply #'call-next-method class :name name initargs)
    (modify-accessors class)))

#-sbcl
(cl:defmethod reinitialize-instance :after
  ((class standard-class) &key)
  (modify-accessors class))

#-sbcl
(cl:defmethod reinitialize-instance :after
  ((class funcallable-standard-class) &key)
  (modify-accessors class))

;;; The following three methods ensure that the dependent protocol
;;; for generic function works.

;; The following method additionally ensures that
;; compute-discriminating-function is triggered.

; Note that for CMUCL, these methods violate the AMOP specification
; by specializing on the original standard-generic-function metaclass. However,
; this is necassary because in CMUCL, only one subclass of
; standard-generic-function can be created, and taking away that option from user
; code doesn't make a lot of sense in our context.

#-sbcl
(cl:defmethod reinitialize-instance :after
  ((gf standard-generic-function) &rest initargs)
  (declare (dynamic-extent initargs))
  (set-funcallable-instance-function
   gf (compute-discriminating-function gf))
  #-cmu
  (map-dependents
   gf (lambda (dep) (apply #'update-dependent gf dep initargs))))

#-(or cmu sbcl)
(cl:defmethod add-method :after
  ((gf standard-generic-function) method)
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'add-method method))))

#-(or cmu sbcl)
(cl:defmethod remove-method :after
  ((gf standard-generic-function) method)
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

#+sbcl
(defun ensure-method (gf lambda-expression 
                         &key (method-class (generic-function-method-class gf))
                         (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly (find-class 't)))))
  (multiple-value-bind
      (method-lambda method-args)
      (make-method-lambda
       gf (class-prototype method-class)
       lambda-expression ())
    (let ((method (apply #'make-instance
                         method-class
                         :qualifiers qualifiers
                         :lambda-list lambda-list
                         :specializers specializers
                         :function (compile nil method-lambda)
                         method-args)))
      (add-method gf method)
      method)))

#|
(defgeneric transform-specializer (specializer)
  (:method (specializer) specializer)
  (:method ((specializer class))
   (class-name specializer))
  (:method ((specializer eql-specializer))
   `(eql ,(eql-specializer-object specializer))))
|#

#-sbcl
(defun ensure-method (gf lambda-expression 
                         &key (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly 't))))
  (funcall (compile nil `(lambda ()
                           (defmethod ,(generic-function-name gf) ,@qualifiers
                             ,(loop for specializer in specializers
                                    for (arg . rest) on lambda-list
                                    collect `(,arg ,specializer) into args
                                    finally (return (nconc args rest)))
                             ,@(cddr lambda-expression))))))

;; The following ensures that effective slot definitions have a documentation in CMUCL.

#+cmu
(defmethod compute-effective-slot-definition :around
  ((class standard-class) name direct-slot-definitions)
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slot-definitions
          for documentation = (documentation direct-slot 't)
          when documentation do
          (setf (documentation effective-slot 't) documentation)
          (loop-finish))
    effective-slot))

;; The following can be used in direct-slot-definition-class to get the correct initargs
;; for a slot. Use it like this:
;;
;; (defmethod direct-slot-definition-class
;;            ((class my-standard-class) &rest initargs)
;;   (declare (dynamic-extent initargs))
;;   (destructuring-bind
;;       (&key key-of-interest &allow-other-keys)
;;       (fix-slot-initargs initargs)
;;     ...))

(defvar *standard-slot-keys*
  '(:name :documentation
    :initargs :initform :initfunction
    :readers :writers))

(defun fix-slot-initargs (initargs)
  #+sbcl initargs
  #+cmu
  (let* ((counts (loop with counts
                       for (key nil) on initargs by #'cddr
                       do (incf (getf counts key 0))
                       finally (return counts)))
         (keys-to-fix (loop for (key value) on counts by #'cddr
                            if (> value 1) collect key)))
    (if keys-to-fix
      (let ((multiple-standard-keys
             (intersection keys-to-fix *standard-slot-keys*)))
        (if multiple-standard-keys
          (error "Too many occurences of ~S in slot initargs ~S."
                 multiple-standard-keys initargs)
          (loop with fixed-keys
                for (key value) on initargs by #'cddr
                if (member key keys-to-fix)
                do (nconcf (getf fixed-keys key) (list value))
                else nconc (list key value) into fixed-initargs
                finally (return (nconc fixed-initargs fixed-keys)))))
      initargs)))

;; In CMUCL, TYPEP and SUBTYPEP don't work as expected
;; in conjunction with class metaobjects.

#-sbcl
(progn
  (defgeneric typep (object type)
    (:method (object type)
     (cl:typep object type))
    (:method (object (type class))
     (cl:typep object (class-name type))))

  (defgeneric subtypep (type1 type2)
    (:method (type1 type2)
     (cl:subtypep type1 type2))
    (:method ((type1 class) type2)
     (cl:subtypep (class-name type1) type2))
    (:method (type1 (type2 class))
     (cl:subtypep type1 (class-name type2)))
    (:method ((type1 class) (type2 class))
     (cl:subtypep (class-name type1)
                  (class-name type2)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
