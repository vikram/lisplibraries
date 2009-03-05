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

;; We need a new standard-generic-function for various things.

(cl:defclass standard-generic-function (cl:standard-generic-function)
  ((initial-methods :initform '()))
  (:metaclass clos:funcallable-standard-class))

;; The following ensures that the new standard-generic-function is used.

(defun ensure-generic-function
       (name &rest args
             &key (generic-function-class 'standard-generic-function)
             &allow-other-keys)
  (declare (dynamic-extent args))
  (when (fboundp name)
    (let ((function (fdefinition name)))
      (unless (typep function 'generic-function)
        (cerror "Discard existing definition and create generic function."
                "~S is already fbound, but not as a generic function." name)
        (fmakunbound name))))
  (if (fboundp name)
    (let ((function (fdefinition name)))
      (apply #'ensure-generic-function-using-class
             function name args))
    (apply #'ensure-generic-function-using-class nil name
           :generic-function-class generic-function-class
           args)))

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

;; We need a new funcallable-standard-class for various things.

(cl:defclass funcallable-standard-class (clos:funcallable-standard-class)
  ())

;; See the comment on validate-superclass for standard-class above.

(cl:defmethod validate-superclass
           ((class funcallable-standard-class)
            (superclass clos:funcallable-standard-class))
  (or (when (eq (class-of class) (find-class 'funcallable-standard-class))
        (or (eq (class-of superclass) (find-class 'clos:funcallable-standard-class))
            (eq (class-of superclass) (find-class 'funcallable-standard-class))))
      (call-next-method)
      (when (eq (class-of superclass) (find-class 'clos:funcallable-standard-class))
        (validate-superclass class (class-prototype (find-class 'funcallable-standard-class))))))

#+lispworks5.0
(cl:defmethod validate-superclass
           ((class funcallable-standard-class)
            (superclass (eql (find-class 'funcallable-standard-object))))
  t)

;; We also need a new funcallable-standard-object because the default one
;; is not an instance of clos:funcallable-standard-class.

#-lispworks5.0
(cl:defclass funcallable-standard-object (clos:funcallable-standard-object)
  ()
  (:metaclass clos:funcallable-standard-class))

;; The following code ensures that possibly incorrect lists of direct
;; superclasses are corrected.

#-lispworks5.0
(defun modify-superclasses (direct-superclasses &optional (standardp t))
  (if (null direct-superclasses)
    (list (if standardp
            (find-class 'standard-object)
            (find-class 'funcallable-standard-object)))
    (let ((standard-object (if standardp
                             (find-class 'standard-object)
                             (find-class 'clos:funcallable-standard-object))))
      (if (eq (car (last direct-superclasses)) standard-object)
        (if standardp
          direct-superclasses
          (append (butlast direct-superclasses)
                  (list (find-class 'funcallable-standard-object))))
        (remove standard-object direct-superclasses)))))

;; During class re/initialization, we take care of the following things:
;; - Optimization of slot accessors is deactivated.
;; - Lists of direct superclasses are corrected.
;; - Removal of direct subclasses.

(cl:defmethod initialize-instance :around
  ((class standard-class) &rest initargs
   #-lispworks5.0 &key
   #-lispworks5.0 (direct-superclasses ()))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method class
         #-lispworks5.0 :direct-superclasses
         #-lispworks5.0 (modify-superclasses direct-superclasses)
         :optimize-slot-access nil
         initargs))

(cl:defmethod reinitialize-instance :around
  ((class standard-class) &rest initargs
   #-lispworks5.0 &key
   #-lispworks5.0 (direct-superclasses () direct-superclasses-p))
  (declare (dynamic-extent initargs))
  #-lispworks5.0
  (progn
    (when direct-superclasses-p
      (setq direct-superclasses (modify-superclasses direct-superclasses))
      (loop for superclass in (copy-list (class-direct-superclasses class))
            unless (member superclass direct-superclasses)
            do (remove-direct-subclass superclass class)))
    (if direct-superclasses-p
      (apply #'call-next-method class
             :direct-superclasses direct-superclasses
             :optimize-slot-access nil
             initargs)
      (apply #'call-next-method class
             :optimize-slot-access nil
             initargs)))
  #+lispworks5.0
  (apply #'call-next-method class
         :optimize-slot-access nil
         initargs))

(cl:defmethod initialize-instance :around
  ((class funcallable-standard-class) &rest initargs
   #-lispworks5.0 &key
   #-lispworks5.0 (direct-superclasses ()))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method class
         #-lispworks5.0 :direct-superclasses
         #-lispworks5.0 (modify-superclasses direct-superclasses nil)
         :optimize-slot-access nil
         initargs))

(cl:defmethod reinitialize-instance :around
  ((class funcallable-standard-class) &rest initargs
   #-lispworks5.0 &key
   #-lispworks5.0 (direct-superclasses () direct-superclasses-p))
  (declare (dynamic-extent initargs))
  #-lispworks5.0
  (progn
    (when direct-superclasses-p
      (setq direct-superclasses (modify-superclasses direct-superclasses nil))
      (loop for superclass in (copy-list (class-direct-superclasses class))
            unless (member superclass direct-superclasses)
            do (remove-direct-subclass superclass class)))
    (if direct-superclasses-p
      (apply #'call-next-method class
             :direct-superclasses direct-superclasses
             :optimize-slot-access nil
             initargs)
      (apply #'call-next-method class
             :optimize-slot-access nil
             initargs)))
  #+lispworks5.0
  (apply #'call-next-method class
         :optimize-slot-access nil
         initargs))

;; The following is necessary for forward-referenced-classes.
;; Since we replace the original funcallable-standard-object with
;; a new one, we have to prevent LispWorks from trying to use
;; the original one when forward-ferenced-classes are resolved.

#-lispworks5.0
(cl:defmethod change-class :around
  ((class forward-referenced-class)
   (new-class funcallable-standard-class)
   &rest initargs
   &key (direct-superclasses ()))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method class new-class
         :optimize-slot-access nil
         :direct-superclasses (modify-superclasses direct-superclasses nil)
         initargs))

;;; In LispWorks, the slot accessors (slot-value-using-class, etc.) are specialized
;;; on slot names instead of effective slot definitions. In order to fix this,
;;; we need to rewire the slot access protocol.

(cl:defmethod slot-value-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find slot (class-slots class)
                     :test #'eq
                     :key #'slot-definition-name)))
    (if slotd
      (slot-value-using-class class object slotd)
      (slot-missing class object slot 'slot-value))))

(cl:defmethod slot-value-using-class
           ((class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (slot-value-using-class
   (load-time-value (class-prototype (find-class 'cl:standard-class)))
   object
   (slot-definition-name slotd)))

(cl:defmethod (setf slot-value-using-class)
           (new-value (class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find slot (class-slots class)
                     :test #'eq
                     :key #'slot-definition-name)))
    (if slotd
      (setf (slot-value-using-class class object slotd)
            new-value)
      (slot-missing class object slot 'setf new-value))))

(cl:defmethod (setf slot-value-using-class)
           (new-value (class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (setf (slot-value-using-class
         (load-time-value (class-prototype (find-class 'cl:standard-class)))
         object
         (slot-definition-name slotd))
        new-value))

(cl:defmethod slot-boundp-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find slot (class-slots class)
                     :test #'eq
                     :key #'slot-definition-name)))
    (if slotd
      (slot-boundp-using-class class object slotd)
      (slot-missing class object slot 'slot-boundp))))

(cl:defmethod slot-boundp-using-class
           ((class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (slot-boundp-using-class
   (load-time-value (class-prototype (find-class 'cl:standard-class)))
   object
   (slot-definition-name slotd)))

(cl:defmethod slot-makunbound-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find slot (class-slots class)
                     :test #'eq
                     :key #'slot-definition-name)))
    (if slotd
      (slot-makunbound-using-class class object slotd)
      (slot-missing class object slot 'slot-makunbound))))

(cl:defmethod slot-makunbound-using-class
           ((class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (slot-makunbound-using-class
   (load-time-value (class-prototype (find-class 'cl:standard-class)))
   object
   (slot-definition-name slotd)))

;; In LispWorks, eql specializers are lists. We cannot change this
;; but we can soften some of the incompatibilities.

(deftype eql-specializer ()
  '(or eql-specializer*
       (satisfies clos:eql-specializer-p)))

(cl:defgeneric eql-specializer-object (eql-specializer)
  (:method ((cons cons))
   (if (clos:eql-specializer-p cons)
     (cadr cons)
     (error "~S is not an eql-specializer." cons))))

(defun intern-eql-specializer (object)
  `(eql ,object))

(cl:defclass eql-specializer* (metaobject)
  ((obj :reader eql-specializer-object
        :initarg eso
        :initform (error "Use intern-eql-specializer to create eql-specializers."))
   (direct-methods :reader specializer-direct-methods
                   :accessor es-direct-methods
                   :initform ())))

(defvar *eql-specializers* (make-hash-table))

(defun intern-eql-specializer* (object)
  (or (gethash object *eql-specializers*)
      (setf (gethash object *eql-specializers*)
            (make-instance 'eql-specializer* 'eso object))))

(cl:defmethod add-direct-method ((specializer eql-specializer*) (method method))
  (pushnew method (es-direct-methods specializer)))

(cl:defmethod remove-direct-method ((specializer eql-specializer*) (method method))
  (removef (es-direct-methods specializer) method))

(cl:defgeneric specializer-direct-generic-functions (specializer)
  (:method ((class class))
   (remove-duplicates
    (mapcar #'method-generic-function
            (specializer-direct-methods class))))
  (:method ((eql-specializer eql-specializer*))
   (remove-duplicates
    (mapcar #'method-generic-function
            (specializer-direct-methods eql-specializer))))
  (:method ((cons cons))
   (specializer-direct-generic-functions
    (intern-eql-specializer*
     (eql-specializer-object cons)))))

;; The following method ensures that remove-method is called.

#-lispworks5.0
(cl:defmethod add-method :before ((gf standard-generic-function) (method method))
  (when-let (old-method (find-method gf (method-qualifiers method)
                                     (method-specializers method) nil))
    (remove-method gf old-method)))

;; The following two methods ensure that add/remove-direct-method is called,
;; and that the dependent protocol for generic function works.

(cl:defmethod add-method :after ((gf standard-generic-function) (method method))
  (loop for specializer in (method-specializers method)
        if (consp specializer)
        do (add-direct-method
            (intern-eql-specializer*
             (eql-specializer-object specializer))
            method)
        #-lispworks5.0 else
        #-lispworks5.0 do
        #-lispworks5.0 (add-direct-method specializer method))
  #+lispworks4.3
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'add-method method))))

(cl:defmethod remove-method :after ((gf standard-generic-function) (method method))
  (loop for specializer in (method-specializers method)
        if (consp specializer)
        do (remove-direct-method
            (intern-eql-specializer*
             (eql-specializer-object specializer))
            method)
        #-lispworks5.0 else
        #-lispworks5.0 do
        #-lispworks5.0 (remove-direct-method specializer method))
  #+lispworks4.3
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

(cl:defgeneric find-method-combination (gf combi combi-options)
  (:method ((gf generic-function) (combi symbol) combi-options)
   (when combi-options
     (error "This implementation of find-method-combination cannot handle method combination options."))
   (clos::find-a-method-combination-type combi)))

;; In LispWorks, make-method-lambda expects different arguments than those
;; specified in AMOP. We just bridge this. The method lambda returned
;; still doesn't conform to AMOP, but may be good enough.

(cl:defgeneric make-method-lambda (gf method lambda-expression env)
  (:method ((gf cl:standard-generic-function)
            (method standard-method)
            lambda-expression env)
   (declare (ignorable env))
   (destructuring-bind
       (lambda (&rest args) &body body)
       lambda-expression
     (declare (ignore lambda))
     (loop with documentation = :unbound
           for (car . cdr) = body then cdr
           while (or (stringp car)
                     (and (consp car) (eq (car car) 'declare)))
           if (stringp car)
           do (setf documentation
                    (if (eq documentation :unbound) car
                      (error "Too many documentation strings in lambda expression ~S."
                             lambda-expression)))
           else append (loop for declaration in (cdr car) 
                             if (eq (car declaration) 'ignore)
                             collect `(ignorable ,@(cdr declaration))
                             and collect `(dynamic-extent ,@(cdr declaration))
                             else collect declaration) into declarations
           finally (multiple-value-bind
                       (method-lambda method-args)
                       (clos:make-method-lambda
                        gf method args declarations
                        `(progn ,car ,@cdr) env)
                     (if (eq documentation :unbound)
                       (return (values method-lambda method-args))
                       (return (values
                                `(lambda ,(cadr method-lambda)
                                   ,documentation
                                   ,@(cddr method-lambda))
                                method-args))))))))

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
    (let ((method  (apply #'make-instance
                          method-class
                          :qualifiers qualifiers
                          :lambda-list lambda-list
                          :specializers specializers
                          :function (compile nil method-lambda)
                          method-args)))
      (add-method gf method)
      method)))

;; helper function for creating a generic function lambda list
;; from a method lambda list.
(defun create-gf-lambda-list (method-lambda-list)
  (loop with stop-keywords = '#.(remove '&optional lambda-list-keywords)
        for arg in method-lambda-list
        until (member arg stop-keywords)
        collect arg into gf-lambda-list
        finally (return (let (rest)
                          (cond ((member '&key method-lambda-list)
                                 (nconc gf-lambda-list '(&key)))
                                ((setq rest (member '&rest method-lambda-list))
                                 (nconc gf-lambda-list (subseq rest 0 2)))
                                (t gf-lambda-list))))))

;; The defmethod macro is needed in order to ensure that make-method-lambda
;; is called. (Unfortunately, this doesn't work in the other CL implementations.)

(defmacro defmethod (&whole form name &body body &environment env)
  (loop for tail = body then (cdr tail)
        until (listp (car tail))
        collect (car tail) into qualifiers
        finally
        (destructuring-bind
            ((&rest specialized-args) &body body) tail
          (loop with documentation = :unbound
                for (car . cdr) = body then cdr
                while (or (stringp car)
                          (and (consp car) (eq (car car) 'declare)))
                if (stringp car)
                do (setq documentation
                         (if (eq documentation :unbound) car
                           (error "Too many documentation strings for defmethod form ~S." form)))
                else append (cdr car) into declarations
                finally
                (let* ((lambda-list (extract-lambda-list specialized-args))
                       (gf-lambda-list (create-gf-lambda-list lambda-list))
                       (gf (if (fboundp name)
                             (ensure-generic-function name)
                             (ensure-generic-function name :lambda-list gf-lambda-list)))
                       (method-class (generic-function-method-class gf))
                       (lambda-expression `(lambda ,lambda-list
                                             (declare ,@declarations)
                                             (block ,name ,car ,@cdr))))
                  (if (equal (compute-applicable-methods
                              #'make-method-lambda
                              (list gf (class-prototype method-class)
                                    lambda-expression env))
                             (list (find-method
                                    #'make-method-lambda '()
                                    (list (find-class 'cl:standard-generic-function)
                                          (find-class 'standard-method)
                                          (find-class 't)
                                          (find-class 't))
                                    nil)))
                    (return-from defmethod `(cl:defmethod ,@(rest form)))
                    (multiple-value-bind
                        (method-lambda method-args)
                        (make-method-lambda
                         gf (class-prototype method-class)
                         lambda-expression env)
                      (with-unique-names (gf method)
                        (return-from defmethod
                          `(let ((,gf (if (fboundp ',name)
                                        (ensure-generic-function ',name)
                                        (ensure-generic-function
                                         ',name :lambda-list ',gf-lambda-list)))
                                 (,method
                                  (make-instance
                                   ',method-class
                                   :qualifiers ',qualifiers
                                   :specializers
                                   (list
                                    ,@(mapcar
                                       (lambda (specializer-name)
                                         (typecase specializer-name
                                           (symbol `(find-class ',specializer-name))
                                           (cons (cond
                                                  ((> (length specializer-name) 2)
                                                   (error "Invalid specializer ~S in defmethod form ~S."
                                                          specializer-name form))
                                                  ((eq (car specializer-name) 'eql)
                                                   `(intern-eql-specializer ,(cadr specializer-name)))
                                                  (t (error "Invalid specializer ~S in defmethod form ~S."
                                                            specializer-name form))))
                                           (t (error "Invalid specializer ~S in defmethod form ~S."
                                                     specializer-name form))))
                                       (extract-specializer-names specialized-args)))
                                   :lambda-list ',lambda-list
                                   :function (function ,method-lambda)
                                   ,@(unless (eq documentation :unbound)
                                       (list :documentation documentation))
                                   ,@method-args)))
                             (add-method ,gf ,method)
                             ,method))))))))))

;; The following macro ensures that the new standard-generic-function
;; is used by default. It also ensures that make-method-lambda is called
;; for the default methods, by expanding into defmethod forms.

(defmacro defgeneric (&whole form name (&rest args) &body options)
  (unless (every #'consp options)
    (error "Illegal generic functions options in defgeneric form ~S." form))
  `(progn
     (let ((generic-function (ignore-errors (fdefinition ',name))))
       (when (and generic-function (typep generic-function 'standard-generic-function))
         (loop for method in (slot-value generic-function 'initial-methods)
               do (remove-method generic-function method))))
     (cl:defgeneric ,name ,args
       ,@(remove :method options :key #'car :test #'eq)
       ,@(unless (member :generic-function-class options :key #'car :test #'eq)
           '((:generic-function-class standard-generic-function))))
     (let ((generic-function (fdefinition ',name)))
       (setf (slot-value generic-function 'initial-methods)
             (list ,@(loop for method-spec in (remove :method options :key #'car :test-not #'eq)
                           collect `(defmethod ,name ,@(cdr method-spec)))))
       generic-function)))

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
  initargs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
