(in-package #:anardb)

(defmacro defdbclass (name direct-supers direct-slots &rest options)
  "Define a class of serialisable objects.

Syntax is like defclass but accessors and initargs are defined by default.

Additional slot option :index specifies whether a slot's value should
be indexed in a hash table. Note that changing an indexed slot's value
is not supported.

Additional class option :finder specifies the name of a helper
function (by default (symbolicate 'find- classname)), which can be
used to lookup class instances.

Additional class option :store specifies the variable pointing to the
store responsible for objects of this class.
"

  (macrolet ((rem-option (name default)
	       (alexandria:once-only (name)
		 (alexandria:with-unique-names (val)
		   `(let ((,val (assoc ,name options)))
		      (cond (,val 
			     (alexandria:deletef options ,val)
			     (assert (not (cddr ,val)))
			     (second ,val))
			    (t
			     ,default)))))))
    (let* ((options (copy-list options))
	   (slot-names (loop for slot-def in direct-slots
			     for list = (alexandria:ensure-list slot-def)
			     when (getf (rest list) :persistent t)
			     collect (first list)))
	   (names-of-indexed-slots (mapcan (lambda(x) 
					     (destructuring-bind (sn &key index &allow-other-keys) 
						 (alexandria:ensure-list x)
					       (when index
						 (list sn)))) direct-slots))
	   (names-of-all-indexed-slots 
	    (append
	     names-of-indexed-slots
	     (mapcan 'dbclass-indices direct-supers)))
	   (names-of-all-unindexed-slots (append (set-difference slot-names names-of-indexed-slots)
						 (mapcan 'dbclass-unindexed-slot-names direct-supers)))
	   (store (rem-option :store '*store*))
	   (define-print-object (rem-option :define-print-object t))
	   (finder
	    (rem-option :find-function-name
			(alexandria:symbolicate 'find- name)))
	   (lookup (alexandria:symbolicate 'lookup- name)))
      (alexandria:with-unique-names (obj stream results)
	`(progn
	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (defmethod dbclass-package ((class (eql ',name)))
	       ,*package*)
	     (defmethod dbclass-indices ((class (eql ',name)))
	       (declare (ignorable class))
	       ,(when names-of-all-indexed-slots `',names-of-all-indexed-slots))
	     (defmethod dbclass-unindexed-slot-names ((class (eql ',name)))
	       (declare (ignorable class))
	       ,(when names-of-all-unindexed-slots `',names-of-all-unindexed-slots))
	     (defmethod dbclass-slot-index-table-names ((class (eql ',name)))
	       ',(mapcar (lambda(index-slot-name)
			   (dbclass-slot-index-table-name name index-slot-name))
			 names-of-indexed-slots))

	     (defstruct (,(dbclass-index-struct-name name)
			  (:constructor ,(dbclass-make-index-struct-name name)))
	       ,@(loop for index-slot-name in names-of-indexed-slots collect
		       `(,index-slot-name (make-hash-table :test 'equal)))
	       (%class-index% (make-hash-table :test 'eql)))

	     (defvar ,(dbclass-index-struct-var name) (,(dbclass-make-index-struct-name name))))

	   (defmethod retrieve-instance-by-id ((class-name (eql ',name)) id)
	     (declare (ignorable class-name))
	     (,lookup id))
	     
	   (defun ,lookup (id)
	     (let ((,obj
		    (gethash id ,(dbclass-index-table-name name))))
	       (if ,obj
		   ,obj
		   (object-absent :class ',name :store ,store :object-id id))))


	   (defmethod dbclass-table-resize ((class-name (eql ',name)) size)
	      ,@(loop for table in
		      (list* (dbclass-index-table-name name) 
			     (loop for slot-name in names-of-all-indexed-slots collect
				   (dbclass-slot-index-table-name name slot-name)))
		      collect `(setf ,table (alexandria:copy-hash-table ,table :size size))))

	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (defclass ,name
		 (,@direct-supers store-object)
	       ((store-object-store :accessor store-object-store :allocation :class :initform ,store)
		,@(mapcar (lambda(slot)
			    (destructuring-bind 
				  (slot-name &rest options)
				(copy-list (alexandria:ensure-list slot))
			      (flet ((default (flag value)
				       (when (eq 'absent (getf options flag 'absent)) 
					 (setf (getf options flag) value))))
				(let ((indexed
				       (remf options :index))
				      (persistent (getf options :persistent t)))
				  (default 	 
				      (cond (indexed
					     (assert (and (not (getf options :writer)) (not (getf options :accessor))) 
						     (slot-name slot) "Cannot have an writer with an indexed slot (sorry not implemented)")
					     :reader)
					    (t
					     :accessor))
				      (alexandria:symbolicate name '- slot-name))
				  (default :initarg
				      (intern (symbol-name slot-name) :keyword))
				  (assert (or (not indexed) persistent))
				  (remf options :persistent))
				`(,slot-name ,@options))))
			  direct-slots)
		,@options)))
	   (pushnew ',name (store-classnames ,store))

	   (defmethod initialize-instance :before ((,obj ,name) &rest initargs)
	     (declare (ignorable initargs))
	     (anardb:assert-in-transaction ,store))

	   (defmethod store-object-add ((,obj ,name))
	     (call-next-method)
	     (setf (gethash (store-object-id ,obj) ,(dbclass-index-table-name name)) ,obj))

	   (defmethod store-object-make-lookup-form ((,obj ,name))
	     (assert (eq ,obj (,lookup (store-object-id ,obj))))
	     `(,',lookup ,(store-object-id ,obj)))

	   (defmethod store-object-add-indices ((,obj ,name))
	     (call-next-method)
	     ,@(loop for slot-name in names-of-indexed-slots collect
		     `(push ,obj (gethash (slot-value ,obj ',slot-name) 
					  ,(dbclass-slot-index-table-name name slot-name))))
	     (values))

	   (defmethod store-object-make-serialise-form ((,obj ,name))
	     (assert (eq ,obj (,lookup (store-object-id ,obj))))
	     (append 
	      `(sdo ,',name ,(store-object-id ,obj))
	      ,@(loop for slot in (append names-of-all-indexed-slots names-of-all-unindexed-slots) collect
		      `(when (slot-boundp ,obj ',slot) 
			 `(,',slot ,(store-object-serialise-form (slot-value ,obj ',slot)))))))

	   (defmethod store-object-del :before ((,obj ,name))
	     (anardb:assert-in-transaction ,store)
	     (remhash (store-object-id ,obj) ,(dbclass-index-table-name name))
	     ,@(loop for slot-name in names-of-indexed-slots collect
		     `(alexandria:deletef (gethash (slot-value ,obj ',slot-name) ,(dbclass-slot-index-table-name name slot-name)) ,obj )))

	   ,@(loop for slotname in names-of-indexed-slots
		   collect `(defmethod dbclass-retrieve-all-with-slot-value ((classname (eql ',name)) (slotname (eql ',slotname)) slot-value)
			      (gethash slot-value ,(dbclass-slot-index-table-name name slotname))))
	   ,@(loop for parent in direct-supers
		   append (loop for slotname in (dbclass-indices parent) collect
				`(defmethod dbclass-retrieve-all-with-slot-value ((classname (eql ',name)) (slotname (eql ',slotname)) slot-value)
				   (dbclass-retrieve-all-with-slot-value ',parent ',slotname slot-value))))
	 

	   ,(when define-print-object
		  `(defmethod print-object ((,obj ,name) ,stream)
		     (print-unreadable-object (,obj ,stream)
		       (call-next-method)
		       (let ((*print-level* (or *print-level* 4)))
			 (format ,stream "~{~{ ~A: ~S~}~}" 
				 (list ,@(loop for slot-name in slot-names collect
					       `(list ',slot-name (if (slot-boundp ,obj ',slot-name) (slot-value ,obj ',slot-name) :unbound)))))))))

	 
	   ,(when finder
		  (let ((slot-map (loop for slot in (append names-of-all-indexed-slots names-of-all-unindexed-slots)
					collect (cons slot (gensym (symbol-name slot))))))
		    (labels 		
			((filter-one (slots)
			   (if slots
			       `(and ,@(loop for s in slots collect `(or (not ,(cdr (assoc s slot-map))) (equalp (slot-value ,obj ',s) ,s))))
			       `(constantly t)))
			 (filter (list slots)
			   (if slots
			       `(remove-if-not (lambda(,obj)
						 ,(filter-one slots)) ,list)
			       list))
			 (find-by (sym indexed-syms unindexed-syms)
			   `(cond 
			      ,@(when sym
				      (list `(,sym
					      ,(filter
						`(dbclass-retrieve-all-with-slot-value ',name ',sym ,sym) (append indexed-syms unindexed-syms)))))
			      ,@(cond 
				 (indexed-syms
				  (list 
				   `(t
				     ,(find-by (pop indexed-syms) indexed-syms unindexed-syms))))
				 (unindexed-syms
				  (list `(t (let (,results)
					      (do-all-instances (,obj ',name)
						(when ,(filter-one unindexed-syms)
						  (push ,obj ,results)))
					      ,results))))))))
		      `(defun ,finder
			   (&key ,@(loop for (slot . sym) in slot-map collect `(,slot nil ,sym) ))
			 (declare (ignorable ,@(mapcar 'cdr slot-map)))
			 ,(find-by nil names-of-all-indexed-slots names-of-all-unindexed-slots)))))
	   (find-class ',name))))))
