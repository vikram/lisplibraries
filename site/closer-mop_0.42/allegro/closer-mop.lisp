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

;; We need a new standard-class for various things.

(cl:defclass standard-class (cl:standard-class)
  ((valid-slot-allocations :initform '(:instance :class)
                           :accessor valid-slot-allocations
                           :reader excl::valid-slot-allocation-list)))

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

;; The following macro ensures that the new standard-class is used by default.

(defmacro defclass (name (&rest supers) &body options)
  (if (member :metaclass options :key #'car)
    `(cl:defclass ,name ,supers ,@options)
    `(cl:defclass ,name ,supers ,@options
       (:metaclass standard-class))))

;; Allegro defines an extra check for :allocation kinds. AMOP expects any kind to be
;; permissible, though. This is corrected here.

(defmethod direct-slot-definition-class :before ((class standard-class) &key allocation &allow-other-keys)
  (unless (eq (class-of class) (find-class 'standard-class))
    (pushnew allocation (valid-slot-allocations class))))

;;; In Allegro, slot-boundp-using-class and slot-makunbound-using-class are specialized
;;; on slot names instead of effective slot definitions. In order to fix this,
;;; we need to rewire the slot access protocol.

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

;; We need a new standard-generic-function for various things.

(cl:defclass standard-generic-function (cl:standard-generic-function)
  ()
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

;; The following macro ensures that the new standard-generic-function
;; is used by default.

(defmacro defgeneric (name (&rest args) &body options)
  (if (member :generic-function-class options :key #'car)
      `(cl:defgeneric ,name ,args ,@options)
    `(cl:defgeneric ,name ,args ,@options
       (:generic-function-class standard-generic-function))))

;;; The following three methods ensure that the dependent protocol
;;; for generic function works.

;; The following method additionally ensures that
;; compute-discriminating-function is triggered.

(cl:defmethod reinitialize-instance :after
  ((gf standard-generic-function) &rest initargs)
  (declare (dynamic-extent initargs))
  (set-funcallable-instance-function
   gf (compute-discriminating-function gf))
  (map-dependents
   gf (lambda (dep) (apply #'update-dependent gf dep initargs))))

(cl:defmethod add-method :after
  ((gf standard-generic-function) method)
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'add-method method))))

(cl:defmethod remove-method :after
  ((gf standard-generic-function) method)
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

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
