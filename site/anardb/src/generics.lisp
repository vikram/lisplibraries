(in-package #:anardb)

;; must be defined here as some methods are used in this file
(defstruct store
  (next-id 0)
  dir
  (version 0)
  classnames)

(defgeneric drop (object)
  (:documentation "Remove object from the database"))

(define-condition object-absent (error)
  ((object-id :initarg :object-id :accessor store-object-id) 
   (class :initarg :class)
   (store :initarg :store))
  (:report (lambda (c stream)
	     (with-slots (object-id class store)
		 c
	       (format stream "Object ~A of class ~A not present in ~A"
		       object-id class store)))))

(defmethod drop ((sequence sequence))
   (map nil 'drop (copy-seq sequence)))
(defmethod drop (obj)
  (declare (ignorable obj))
  (values))

(defun dbclass-index-struct-name (class)
  (let ((classname (force-symbol class)))
    (let ((*package* (symbol-package classname)))
      (alexandria:symbolicate '%index-struct-%% classname '%%))))

(defun dbclass-make-index-struct-name (class)
 (let ((struct-name (dbclass-index-struct-name class)))
    (let ((*package* (symbol-package struct-name)))
      (alexandria:symbolicate 'make- struct-name))))

(defun dbclass-index-struct-var (class)
  (let ((classname (force-symbol class)))
    (let ((*package* (symbol-package classname)))
      (alexandria:symbolicate '%index-var-%% classname))))
  
(defun dbclass-slot-index-table-name (class slotname)
  (let ((struct-name (dbclass-index-struct-name class)))
    (let ((*package* (dbclass-package (force-symbol class))))
      `(,(alexandria:symbolicate struct-name '- slotname)
	 ,(dbclass-index-struct-var class)))))

(defun dbclass-index-table-name (class)
  (dbclass-slot-index-table-name class '%class-index%))

(defgeneric dbclass-table-resize (classname size)
  (:method (classname size)
    (declare (ignorable classname size))))

(defgeneric dbclass-retrieve-all-with-slot-value (classname slotname slot-value))

(defgeneric dbclass-package (classname)
  (:method (object) ; this is needed because before the class is defined we need to know the package for it
    (declare (ignorable object))
    *package*))

(defgeneric dbclass-indices (classname))
(defgeneric dbclass-unindexed-slot-names (classname))
(defmethod dbclass-slot-index-table-names (classname)
  (declare (ignorable classname)))

(defun store-tables (store)
  (loop 
	for classname in (store-classnames store)
	collect (dbclass-index-table-value classname)))

(defun table-value-from-name (name)
  (destructuring-bind
	(func sym)
      name
    (funcall func (symbol-value sym))))

(defun dbclass-index-table-value (classname)
  (table-value-from-name (dbclass-index-table-name classname)))

(defun dbclass-all-index-table-values (classname)
  (list* (dbclass-index-table-name classname)
	 (mapcar #'table-value-from-name (dbclass-slot-index-table-names classname))))

(defclass store-object ()
  ((store-object-id :accessor store-object-id :initarg :store-object-id))
  (:documentation "Base class of all classes defined through defdbclass"))

(defgeneric store-object-store (store-object))

(defgeneric store-object-make-lookup-form (store-object))
(defgeneric store-object-make-serialise-form (store-object))

(defgeneric store-object-add (store-object))
(defgeneric store-object-del (store-object))

(defmethod store-object-add ((obj store-object))
  (unless (slot-boundp obj 'store-object-id)
    (setf (store-object-id obj) 
	  (let ((store (store-object-store obj)))
	    (prog1 (store-next-id store)
	      (incf (store-next-id store)))))))

(defmethod store-object-del ((obj store-object))
  (declare (ignorable obj)))

(defgeneric store-object-add-indices (obj)
  (:method (obj)
    (declare (ignorable obj))))

(defmethod initialize-instance :after ((obj store-object) &rest args)
  (declare (ignore args))
  (store-object-add obj)
  (store-object-add-indices obj)
  obj)

(defmethod drop ((obj store-object))
  (store-object-del obj))

(defmacro do-all-instances ((var class) &body body)
  `(loop for ,var being the hash-values of (dbclass-index-table-value ,class)
	 do (locally ,@body)))

(defun retrieve-all-instances (class)
  "Retrieve a list of instances of class CLASS"
  (alexandria:hash-table-values (dbclass-index-table-value class)))
(defgeneric retrieve-instance-by-id (classname id))
(defmethod retrieve-instance-by-id ((class class) id)
  (retrieve-instance-by-id (class-name class) id))

(defun freshen-object (object)
  (typecase object
    (store-object
     (retrieve-instance-by-id (class-name (class-of object)) (store-object-id object)))
    (t object)))


(defun object-absent (&rest args)
  (restart-case
      (apply 'error 'object-absent args)
    (use-value (value)
      :report "Specify a value for the missing object"
      value)))

(defmethod print-object ((store store) stream)
  (print-unreadable-object (store stream :type t)
    (format stream "~A@~A" (store-dir store) (store-version store))
    (let ((owner (ignore-errors (file-lock-pid (store-lockfile store)))))
      (when owner
	(format stream " lock:~A" owner)))
    (loop for class in (store-classnames store) 
	  for count = (hash-table-count (dbclass-index-table-value class))
	  do
	  (format stream " ~A~@[=~A~]"
		  class (unless (zerop count) count)))
    (format stream " :total ~A"
	    (store-next-id store))))


(defun store-deserialise-create-object (class id)
  (let ((store-object (allocate-instance (force-class class))))
    (setf (slot-value store-object 'store-object-id) id)
    (store-object-add store-object)
    store-object))

(defun store-deserialise-finish-object (store-object &rest slot-values)
  (declare (dynamic-extent slot-values) (optimize speed))
  (loop for (slot value) on slot-values by #'cddr do
	(setf (slot-value store-object slot) value))
  (store-object-add-indices store-object)
  store-object)

(defmacro sdo (class id &rest slot-values)
  "store-deserialise-object -- deserialise a store object"
  ; unfortunately it has to be a macro because we need to evaluate the values after this object is created
  ; warning this is reimplemented in small-eval for speed
  `(store-deserialise-finish-object (store-deserialise-create-object ',class ,id)
				    ,@(loop for (slot value) on slot-values by #'cddr 
					    collect
					    `',slot
					    collect
					    value)))
    
