
(in-package :weblocks)

(export '(find-view field-info field-info-field field-info-object
	  field-info-path get-object-view-fields map-view-fields
	  map-mixin-fields count-view-fields obtain-view-field-value
	  render-object-view class-from-view render-view
	  render-object-view-impl attributize-presentation))

;;; View rendering utils
(defun find-view (view &optional (signal-error-p t))
  "Finds a view. If 'view' is a symbol, looks up the appropriate view
object. If 'view' is a view object, simply returns it. Otherwise,
signals an error.

If 'view' is a list, finds a scaffold class by calling
'scaffold-class-name' and builds an appropriate scaffold view by
calling 'generate-scaffold-view' with the scaffold class name and the
second argument."
  (or (etypecase view
	(list (generate-scaffold-view (make-instance (scaffold-class-name (first view)))
				      (find-class (second view))))
	(symbol (gethash view *views*))
	(view view))
      (when signal-error-p
	(error "Cannot find view ~A" view))))

(defstruct field-info
  "A structure that holds information about a given field. Information
includes the field instance, the object whose slot the field
represents, and the path to the slot the field represents from the
root object being evaluated, and the field-info of the field's parent
if it was mixed into the view."
  field object parent-info)

(defun get-object-view-fields (obj view-designator &rest args
			       &key include-invisible-p (expand-mixins t) custom-fields
			       &allow-other-keys)
  "Returns a list of 'field-info' structures. If 'include-invisible-p'
is set to true, fields declared as invisible will be returned as
well.

If 'expand-mixins' is set to true (default), mixin fields will be
expanded into inline fields, and will not be present in the results.

If 'custom-fields' is not null, it is expected to be a list with each
element being either a custom field, or a cons cells. If an element is
a custom field, the field is returned at the end of other fields. If
an element is a cons cell, CAR is expected to be a positive integer,
and CDR is expected to be a custom-field. In this case the
custom-field is inserted before the element with the index in the CAR
of the cons cell.

Each custom field can be either a field-info structure or a
view-field. Field-info structures are inserted as is, and view-fields
are wrapped in field-info structures with common-sense defaults."
  (declare (ignore args))
  (labels ((compute-view-field-info-list (view-designator obj parent-field-info)
	     "Computes a full list of view fields, including inherited
	     fields. Returns a list of field-infos."
	     (let ((view (when view-designator
			   (find-view view-designator))))
	       (when view
		 (append (compute-view-field-info-list
			  (view-inherit-from view) obj
			  parent-field-info)
			 (mapcar (lambda (field)
				   (make-field-info :field field :object obj
						    :parent-info parent-field-info))
				 (view-fields view))))))
	   (factor-overriden-fields (field-info-list)
	     "Overrides parent fields redefined in children."
             ;(format t "fil: ~S~%" field-info-list)
             (flet ((field-key (field-info &aux (field (field-info-field field-info)))
                      (cons (view-field-slot-name field) (awhen (field-info-parent-info field-info)
                                                              (view-field-slot-name (field-info-field IT)))))
                    (parent (field-info &aux (field (field-info-field field-info)))
                      (field-info-parent-info field-info))
                    (mixin-p (field-info &aux (field (field-info-field field-info)))
                      (typep field 'mixin-view-field)))
               ;(format t "in: ~S~%" (mapcar (compose #'describe #'field-info-field) field-info-list))
               (let* ((fields (remove-duplicates field-info-list :key #'field-key :from-end nil))
                      (true-inline-fields (remove-duplicates fields :test #'equal
                                                             :key (compose #'view-field-slot-name #'field-info-field)
                                                             :from-end nil))
                      (true-inline-fields (remove-if (lambda (fi) (or (parent fi) (mixin-p fi))) true-inline-fields
                                                     :from-end t))
                      (expanded-mixin-fields (remove-if-not (lambda (fi) (or (parent fi) (mixin-p fi)))
                                                            fields))
                      (expanded-mixin-fields (remove-duplicates expanded-mixin-fields :test #'equal :key #'field-key))
                      (expanded-mixin-fields (remove-if (curry-after #'find true-inline-fields
                                                                     :test #'equal :key (compose #'view-field-slot-name
                                                                                                 #'field-info-field)
                                                                     :from-end nil) expanded-mixin-fields))
                      (merged-fields (sort (union true-inline-fields expanded-mixin-fields)
                                           #'< :key (lambda (field)
                                                      (flet ((pos (field where)
                                                               (let ((r (position (field-key field) where :key #'field-key :test #'equal)))
                                                               ;(format t "field: ~S / where: ~S -> ~S%" (field-key field)
                                                               ;        (mapcar #'field-key where) r)
                                                               r
                                                               )))
                                                        (let ((result (or (pos field fields)
                                                                          (pos field true-inline-fields)
                                                                          (pos field expanded-mixin-fields)
                                                                          0)))
                                                        #+(or)(format t "result for field ~A: ~A~%" field result) result))))))
                 ;(format t "true inline: ~S~%" (mapcar #'field-key true-inline-fields))
                 ;(format t "expanded ~S~%" (mapcar #'field-key expanded-mixin-fields))
                 ;(format t "fields ~S~%" (mapcar #'field-key fields))
                 ;(format t "merged ~S~%" (mapcar (compose #'describe #'field-info-field) merged-fields))
                 merged-fields))) ; XXX this is quite inefficient (at least n^2 + n*log(n))
	   (expand-mixin-fields (field-info-list)
	     "Expands mixin fields into inline fields. Returns two
              values - a list of expanded field-infos, and true if at
              least one field has been expanded."
	     (apply #'append
		    (mapcar (lambda (field-info)
			      (let ((field (field-info-field field-info))
				    (obj (field-info-object field-info)))
				(etypecase field
				  (inline-view-field (list field-info))
				  (mixin-view-field (when (or include-invisible-p
							      (not (view-field-hide-p field)))
						      (compute-view-field-info-list
						       (mixin-view-field-view field)
						       (when obj
							 (or (obtain-view-field-value field obj)
							     (funcall (mixin-view-field-init-form field))))
						       field-info))))))
			    field-info-list)))
	   (custom-field->field-info (custom-field)
	     (etypecase custom-field
	       (field-info custom-field)
	       (view-field (make-field-info :field custom-field
					    :object obj
					    :parent-info nil)))))
    (let* ((initial-step (factor-overriden-fields
			  (compute-view-field-info-list view-designator obj nil)))
	   (results
	    (if expand-mixins
		(loop for field-info-list = initial-step
		   then (factor-overriden-fields
			 (expand-mixin-fields field-info-list))
		   until (notany (lambda (field-info)
				   (typep (field-info-field field-info) 'mixin-view-field))
				 field-info-list)
		   finally (return (if include-invisible-p
				       field-info-list
				       (remove-if #'view-field-hide-p field-info-list
						  :key #'field-info-field))))
		initial-step)))
      (dolist (custom-field custom-fields results)
	(if (consp custom-field)
	    (insert-at (custom-field->field-info (cdr custom-field)) results (car custom-field))
	    (push-end (custom-field->field-info custom-field) results))))))

(defun map-view-fields (fn view obj &rest args &key include-invisible-p custom-fields
			&allow-other-keys)
  "Acts like mapcar for view fields. FN should expect a structure of
type field-info."
  (declare (ignore args))
  (mapcar fn (get-object-view-fields obj view
				     :include-invisible-p include-invisible-p
				     :custom-fields custom-fields)))

(defun map-mixin-fields (fn view obj &rest args)
  (mapc fn (remove-if
	    (lambda (field-info)
	      (not (typep (field-info-field field-info) 'mixin-view-field)))
	    (apply #'get-object-view-fields obj view
		   :expand-mixins nil args))))

(defun count-view-fields (view &key include-invisible-p custom-fields)
  "Counts the number of fields in a given view."
  (length (get-object-view-fields nil view
				  :include-invisible-p include-invisible-p
				  :custom-fields custom-fields)))

(defun slot-reader (class slot-name)
  "Returns a reader, if one is defined, on the slot."
  (let ((slot-dsd (find-slot-dsd class slot-name)))
    (when slot-dsd
      (car (slot-definition-readers slot-dsd)))))

(defun obtain-view-field-value (field obj)
  "Obtains the view field value of object. See documentation for
'reader' slot of 'field' for more details."
  (declare (optimize safety))
  (assert (or (view-field-slot-name field)
	      (slot-boundp field 'reader))
	  nil "Either the field must represent a slot, or it's READER slot must be bound.")
  (let* ((reader-bound-p (slot-boundp field 'reader))
	 (reader (if reader-bound-p
		     (view-field-reader field)
		     (when (view-field-slot-name field)
		       (or (slot-reader (class-of obj) (view-field-slot-name field))
			   (curry-after #'slot-value (view-field-slot-name field)))))))
    (if (or (functionp reader)
	    (and (symbolp reader)
		 (fboundp reader)))
	(handler-case (funcall reader obj)
	  (unbound-slot () nil))
	reader)))

(defun render-object-view (obj view &rest args
			   &key widget &allow-other-keys)
  "A helper function that finds the view and calls
'render-object-view-impl'. Additionally, calls 'dependencies' and adds
the returned items to *page-dependencies*. This is later used by
Weblocks to declare stylesheets and javascript links in the page
header."
  (declare (special *page-dependencies*))
  (setf *page-dependencies*
	(append *page-dependencies* (dependencies (find-view view))))
  (let (*form-submit-dependencies*)
    (declare (special *form-submit-dependencies*))
    ;; this is not the best place to introduce *form-submitp-dependencies*,
    ;; because it only applies to forms, but since render-object-view is
    ;; a function and not a method, we can't place an :around method
    ;; around it, and there is no other method that encapsulates all of
    ;; view rendering --jwr
    (apply #'render-object-view-impl obj (find-view view) widget args)))

(defmethod class-from-view (view &optional (class-name (gensym)))
  "A helper function that generates a class object from a view. The
view fields are enumerated and a CLOS class with slots based on field
names is generated.  This was made a method so the elephant backend
can intercept class names and provide proxies instead."
  (make-class
   (mapcar (lambda (field-info)
	     (view-field-slot-name (field-info-field field-info)))
	   (get-object-view-fields nil view))
   class-name))

(defun render-view (view &rest args &key (class-name (gensym)) &allow-other-keys)
  "A helper function that inspects the view, creates a fitting object
from it only the fly, and calls 'render-object-view'. The function
returns the created object."
  (let ((obj (make-instance (class-from-view view class-name))))
    (apply #'render-object-view obj view args)
    obj))

(defgeneric render-object-view-impl (obj view widget &rest args)
  (:documentation "Renders 'obj' using 'view'.")
  (:method (obj view widget &rest args)
    (apply #'with-view-header view obj widget
	   (lambda (view obj &rest args)
	     (apply #'map-view-fields
		    (lambda (field-info)
		      (let ((field (field-info-field field-info))
			    (obj (field-info-object field-info)))
			(safe-apply (view-field-prefix-fn field) view field obj args)
			(apply #'render-view-field
			       field view widget (view-field-presentation field)
			       (obtain-view-field-value field obj) obj 
			       :field-info field-info
			       args)
			(safe-apply (view-field-suffix-fn field) view field obj args)))
		    view obj args))
	   args)))

(defun attributize-presentation (presentation)
  "Attributizes presentation name."
  (string-remove-right 
   (attributize-name
    (object-class-name
     presentation))
   "-presentation"))

(defmethod print-object ((obj field-info) stream)
  (flet ((field-key (field-info &aux (field (field-info-field field-info)))
                    (cons (view-field-slot-name field) (awhen (field-info-parent-info field-info)
                                                              (view-field-slot-name (field-info-field IT))))))
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "~S" (field-key obj)))))

(defmethod print-object ((obj view-field) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~S" (view-field-slot-name obj))))

