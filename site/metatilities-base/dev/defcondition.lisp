(in-package #:metatilities)

#+(or)
(defparameter *defcondition-options*
  '(((:automatic-accessors :generate-accessors) t nil)
    ((:automatic-initargs :generate-initargs) t nil)
    ((:export-p :export?) t nil)
    ((:export-slots-p :export-slots?) t nil))
  "Extra options to defcondition macro. Format is a list of sub-lists. 
Each sublist should be of length three and consists of a list of option 
synonyms, the default value for the option [currently ignored], and whether
or not to signal an error if this option is used as an atom [currently 
ignored]")


;;-- from moptilities
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

(defun finalize-class-if-necessary (thing)
  "Finalizes thing if necessary. Thing can be a class, object or symbol naming a class. Returns the class of thing."
  (let ((class (get-class thing)))
    (unless (mop:class-finalized-p class)
      (mop:finalize-inheritance class))
    (values class)))

(defun class-slot-names (thing)
  (let ((class (get-class thing)))
    (if class
      (mapcar 'mop:slot-definition-name
	      (mop:class-slots (finalize-class-if-necessary class)))
      (progn
	(warn "class for ~a not found)" thing)
	nil))))

(defmacro defcondition* (name/options (&rest super-conditions)
			 slot-names &optional format &rest args)
  ;; name/options can be a symbol or a list consisting of
  ;; (symbol &key exportp documentation)
  (bind:bind (((name &key documentation (exportp t)) 
	       (if (consp name/options) 
		   name/options (list name/options)))
	      (all-slot-names
	       (remove-duplicates
		(loop for super in super-conditions append
		     (class-slot-names super)))))
    (flet ((massage-slot (slot-spec)
	     (cond ((atom slot-spec)
		    (push slot-spec all-slot-names)
		    `(,slot-spec
		      :initarg ,(intern (symbol-name slot-spec) :keyword)))
		   (t
		    (push (first slot-spec) all-slot-names)
		    slot-spec))))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   ,@(when exportp
		   `((export '(,name))))
	   (define-condition ,name ,super-conditions
	     ,(mapcar #'massage-slot slot-names)
	     ,@(when documentation
		     `((:documentation ,documentation)))
	     ,@(when ;; XXX ACL dependency -- this is used inside agraph.
		(and format
		     #+allegro
		     (setf format (excl::newlinify-format-string format)))
		`((:report
		   (lambda (condition stream)
		     (declare (ignorable condition))
		     (let ,(mapcar
			    (lambda (name)
			      `(,name (and (slot-boundp condition ',name)
					   (slot-value condition ',name))))
			    all-slot-names)
		       ,@(when all-slot-names
			       `((declare (ignorable ,@all-slot-names))))
		       (format
			stream ,format ,@args))))))))))))

#+allegro
(defmacro newlinify (format &environment e)
  (if (and (constantp format e)
	   (stringp (sys:constant-value format e)))
      (excl::newlinify-format-string (sys:constant-value format e))
    format))

