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

(defun extract-lambda-list (lambda-list)
  (loop for (arg . rest) on lambda-list
        until (member arg lambda-list-keywords)
        if (consp arg)
        collect (car arg) into args
        else collect arg into args
        finally (return (if arg
                          (nconc args (cons arg rest))
                          args))))

(defun extract-specializer-names (lambda-list)
  (loop for arg in lambda-list
        until (member arg lambda-list-keywords)
        if (consp arg)
        collect (cadr arg)
        else collect 't))

;; We need a new standard-generic-function for various things.

(cl:defclass standard-generic-function (cl:standard-generic-function)
  ())

;; The following ensures that the new standard-generic-function is used.

(defun ensure-generic-function
       (name &rest args
             &key (generic-function-class 'standard-generic-function)
             &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'cl:ensure-generic-function name
         :generic-function-class generic-function-class
         args))

#|
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
|#

;; The standard accessor classes.

(cl:defclass standard-accessor-method (standard-method)
  ((slotd :initarg :slot-definition
          :reader accessor-method-slot-definition)))

(cl:defclass standard-reader-method (standard-accessor-method)
  ())

(cl:defclass standard-writer-method (standard-accessor-method)
  ())

;; In ECL, reader-method-class and writer-method-class are
;; not used during class initialization. The following definitions
;; correct this.

(cl:defgeneric reader-method-class (class slot-definition &rest initargs)
  (:method ((class class) slot-definition &rest initargs)
   (declare (ignore slot-definition initargs))
   (load-time-value (find-class 'standard-reader-method))))

(cl:defgeneric writer-method-class (class slot-definition &rest initargs)
  (:method ((class class) slot-definition &rest initargs)
   (declare (ignore slot-definition initargs))
   (load-time-value (find-class 'standard-writer-method))))

(cl:defgeneric find-method (gf qualifiers specializers &optional errorp)
  (:method ((gf generic-function) qualifiers specializers &optional (errorp t))
   (cl:find-method gf qualifiers specializers errorp)))

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

;; During class re/initialization, we take care of the following things:
;; - Optimization of slot accessors is deactivated.
;; - Removal of direct subclasses.

(cl:defmethod initialize-instance :around
  ((class standard-class) &rest initargs)
  (declare (dynamic-extent initargs))
  (prog1 (apply #'call-next-method class
                :optimize-slot-access nil
                initargs)
    (modify-accessors class)))

(cl:defmethod reinitialize-instance :around
  ((class standard-class) &rest initargs
   &key (direct-superclasses () direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (when direct-superclasses-p
    (loop for superclass in (copy-list (class-direct-superclasses class))
          unless (member superclass direct-superclasses)
          do (remove-direct-subclass superclass class)))
  (prog1 (apply #'call-next-method class
                :optimize-slot-access nil
                initargs)
    (modify-accessors class)))

;; In ECL, eql specializers are lists. We cannot change this
;; but we can soften some of the incompatibilities.

(defun eql-specializer-p (cons)
  (and (consp cons)
       (eq (car cons) 'eql)
       (null (cddr cons))))

(deftype eql-specializer ()
  '(or eql-specializer*
       (satisfies eql-specializer-p)))

(cl:defgeneric eql-specializer-object (eql-specializer)
  (:method ((cons cons))
   (if (eql-specializer-p cons)
       (cadr cons)
     (error "~S is not an eql-specializer." cons))))

(defun intern-eql-specializer (object)
  `(eql ,object))

(cl:defgeneric specializer-direct-methods (specializer))

(cl:defclass eql-specializer* (standard-object)
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

(defvar *direct-methods* (make-hash-table :test #'eq))

(cl:defgeneric add-direct-method (specializer method)
  (:method ((specializer class) (method method))
   (pushnew method (gethash specializer *direct-methods*)))
  (:method ((specializer eql-specializer*) (method method))
   (pushnew method (es-direct-methods specializer))))

(cl:defgeneric remove-direct-method (specializer method)
  (:method ((specializer class) (method method))
   (removef (gethash specializer *direct-methods*) method))
  (:method ((specializer eql-specializer*) (method method))
   (removef (es-direct-methods specializer) method)))

(cl:defmethod specializer-direct-methods ((class class))
  (gethash class *direct-methods*))

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

(cl:defmethod add-method :before ((gf standard-generic-function) (method method))
  (when-let (old-method (find-method gf (method-qualifiers method)
                                     (method-specializers method) nil))
    (remove-method gf old-method)))

;; The following two methods ensure that add/remove-direct-method is called,
;; and that the dependent protocol for generic function works.

(cl:defmethod add-method :after ((gf standard-generic-function) (method method))
  (loop for specializer in (method-specializers method)
        do (add-direct-method
            (if (consp specializer)
              (intern-eql-specializer*
               (eql-specializer-object specializer))
              specializer)
            method)))

(cl:defgeneric remove-method (generic-function method)
  (:method ((gf generic-function) (method method))
   (cl:remove-method gf method)))

(cl:defmethod remove-method :after ((gf generic-function) (method method))
  (loop for specializer in (method-specializers method)
        do (remove-direct-method
            (if (consp specializer)
              (intern-eql-specializer*
               (eql-specializer-object specializer))
              specializer)
            method)))

(defun ensure-method (gf lambda-expression 
                         &key (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly (find-class 't)))))
  (funcall (compile nil `(lambda ()
                           (defmethod ,(generic-function-name gf) ,@qualifiers
                             ,(loop for specializer in specializers
                                    for (arg . rest) on lambda-list
                                    collect `(,arg ,specializer) into args
                                    finally (return (nconc args rest)))
                             ,@(cddr lambda-expression))))))

(defmacro defgeneric (&whole form name (&rest args) &body options)
  (unless (every #'consp options)
    (error "Illegal generic functions options in defgeneric form ~S." form))
  `(cl:defgeneric ,name ,args
     ,@options
     ,@(unless (member :generic-function-class options :key #'car :test #'eq)
         '((:generic-function-class standard-generic-function)))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
