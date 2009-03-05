(in-package araneida)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *url-methods-seen* nil
    "This holds a list of what url methods have been seen - so the defurlmethod macro knows
when/if to declare a companion function. Used at macrotime"))

(defvar *url-methods* nil
  "Holds a list of (method-name type required-parameters key-parameters lambda) for each urlmethod.
Used at runtime to figure out which method to use, and stores the method itself.
Type is either :tainted or :untainted, and determines whether the values are tainted before being
handed over to the urlmethod.")

(defun method-method-name (x)
  (first x))
(defun method-type (x)
  (second x))
(defun method-function-parameters (x)
  (third x))
(defun method-required-parameters (x)
  (fourth x))
(defun method-key-parameters (x)
  (fifth x))
(defun method-all-parameters (x)
  (sixth x))
(defun method-lambda (x)
  (seventh x))

;;;;;;;;;;;; Conditions
(define-condition urlmethod-error (serious-condition)
  ())
(define-condition urlmethod-compile-error (urlmethod-error)
  ())

(define-condition no-urlmethod (urlmethod-error)
  ((method-name :initarg :method-name :reader no-urlmethod-method-name)
   (parameters  :initarg :parameters  :reader no-urlmethod-parameters))
  (:report (lambda (condition stream)
	     (format stream "No urlmethod matched (~A ~@[~A~])"
		     (no-urlmethod-method-name condition)
		     (no-urlmethod-parameters  condition))))
  (:documentation "At compile time we can't tell whether or not a urlmethod call
will have a match. If the call satisfies no method constraints, this is signaled."))

(define-condition too-many-urlmethods-matched (urlmethod-error)
  ((matched-methods :initarg :matched-methods :reader too-many-urlmethods-matched-matched-methods)
   (method-name :initarg :method-name :reader too-many-urlmethods-matched-method-name)
   (parameters  :initarg :parameters  :reader too-many-urlmethods-matched-parameters))
  (:report (lambda (condition stream)
	     (format stream "More than one urlmethod matched the call (~A ~@[~A~]). Methods matched: ~A"
		     (too-many-urlmethods-matched-method-name condition)
		     (too-many-urlmethods-matched-parameters condition)
		     (too-many-urlmethods-matched-matched-methods condition))))
  (:documentation "If precendence does not yield one method from the methods which have satisfied constraints,
this is signaled."))

(define-condition urlmethod-unknown-keyword (urlmethod-compile-error)
  ((unknown :initarg :unknown :reader urlmethod-unknown-keyword-unknown)
   (lambda-list :initarg :lambda-list :reader urlmethod-unknown-keyword-lambda-list))
  (:report (lambda (condition stream)
	     (format stream "Unknown keyword ~A ~@[in lambda list ~A~]"
		     (urlmethod-unknown-keyword-unknown condition)
		     (urlmethod-unknown-keyword-lambda-list condition))))
  (:documentation "Signaled when defurlparameter contains an unknown keyword"))

(define-condition urlmethod-function-parameter-mismatch (urlmethod-compile-error)
  ((other-methods-info :initarg :other-methods-info :reader urlmethod-function-parameter-mismatch-other-methods-info)
   (parameters :initarg :parameters :reader urlmethod-function-parameter-mismatch-parameters))
  (:report (lambda (condition stream)
	     (format stream "~A has a different number of function parameters from previously defined methods: ~A"
		     (urlmethod-function-parameter-mismatch-parameters condition)
		     (urlmethod-function-parameter-mismatch-other-methods-info condition))))
  (:documentation "Function parameters must be constant across all urlmethods of the same name.
parameters refers to the parameters declared in this defurlmethod. other-methods-info refers to the
method-info of previously defined urlmethods of the same name."))

;;;;;;;;;;;; Functions

(defun satisfy-spec-p (spec value)
  "Given a specialization, determine if the value meets that specialization"
  (if (symbolp spec)
      (not (zerop (length value))) ; if it's just a variable, just check to make sure the string isn't blank
      (apply (first (second spec)) value (rest (second spec))))) ; otherwise it's a function call

(defun fulfills-specialization (required-specializations parameters)
  "Given a list of required specializations, determine if the url alist passed (parameters) meets that specialization"
  (if (null required-specializations)
      t
      (let* ((spec (car required-specializations))
	     (param-value (second (assoc (symbol-name (parameter-name-from-url-specialization spec))
					 parameters
					 :test #'string-equal))))
	(if param-value
	    (if (satisfy-spec-p spec param-value)
		(fulfills-specialization (rest required-specializations) parameters)
		nil)
	    nil))))

(defun remove-not-greatest (elements key &key (max #'max) (min #'min))
  "Removes all elements of list that are not equal to the highest return value of key over the list.
Returns the unremoved elements and nil if nothing was removed, t if anything was"
  (let* ((key-list (mapcar key elements))
	 (greatest (apply max key-list))
	 (least    (apply min key-list)))
    (if (eql greatest least)
	(values elements nil)
	(values (remove-if (lambda (elt)
			     (not (eql greatest (funcall key elt))))
			   elements)
		t))))

(defun unit-list-p (list)
  "Returns true if list is a list of one element"
  (and (consp list) (null (cdr list))))

(defun most-specific-method (methods)
  (declare (type cons methods))
  (if (unit-list-p methods)
      methods
      (let ((methods (remove-not-greatest methods (lambda (method) ; # required parameters
						    (length (method-required-parameters method))))))
	(if (unit-list-p methods)
	    methods
	    (let ((methods (remove-not-greatest methods (lambda (method) ; # specialized required parameters
							  (length (remove-if (complement #'specialized-p)
									     (method-required-parameters method)))))))
	      (if (unit-list-p methods)
		  methods
		  (let ((methods (remove-not-greatest methods (lambda (method) ; # key parameters
								(length (method-key-parameters method))))))
		    methods)))))))
	

(defun select-url-method (handler request-method request urlmethod-name)
  "Given a handler, request-method (GET, POST, etc), request, and the name of the urlmethod, performs
specialization and calls the method"
  (declare (type handler handler)
	   (symbol request-method)
	   (type request request)
	   (ignore handler request-method))
  (let* ((urlparameters (mapcar (lambda (x)
				  (list (first x) (if (second x) (untaint #'identity (second x)) nil)))
				(tainted-url-query-alist (request-url request)))) ; call tainted to shut up warnings
	 (matching-methods (remove-if (lambda (method-info)
					(or (not (eql (method-method-name method-info) urlmethod-name))
					    (not (fulfills-specialization (method-required-parameters method-info) urlparameters))))
				      *url-methods*)))
    (if (null matching-methods)
	(error 'no-urlmethod :method-name urlmethod-name :parameters urlparameters)
	(let ((msm (most-specific-method matching-methods)))
	  (if (> (length msm) 1)
	      (error 'too-many-urlmethods-matched :matched-methods msm :method-name urlmethod-name :parameters urlparameters)
	      (first msm))))))

(defun call-url-method (method-info handler request-method request function-parameter-values)
  (let* ((tainted-parameters (tainted-url-query-alist (request-url request)))
	 (parameter-values (mapcar (lambda (x)
				     (let ((value (second (assoc x tainted-parameters :test #'string-equal))))
				       (if value
					   value
					   (let ((key-parameter (find x
								      (method-key-parameters method-info)
								      :key #'parameter-name-from-key-parameter)))
					     (if key-parameter
						 (taint (default-value-for-key-parameter key-parameter))
						 nil)))))
				   (remove-if (lambda (param)
						(member param (method-function-parameters method-info)))
					      (method-all-parameters method-info)))))
    (ecase (method-type method-info)
      (:untainted (apply (method-lambda method-info) (list* handler request-method request
							    (append function-parameter-values
								    (mapcar (lambda (x) (if x (untaint #'identity x) nil)) parameter-values)))))
      (:tainted   (apply (method-lambda method-info) (list* handler request-method request
							    (append function-parameter-values
								    parameter-values)))))))
      
(defun specialized-p (parameter)
  (consp parameter))
(defun defaulted-p (parameter)
  (consp parameter))
(defun default-value-for-key-parameter (parameter)
  (if (consp parameter)
      (second parameter)
      nil))

(defun parameter-name-from-url-specialization (place)
  (if (symbolp place)
      place
      (first place)))
(defun parameter-name-from-key-parameter (place)
  (if (consp place)
      (first place)
      place))
      

(defun equal-urlmethod-signature (signature-1 signature-2)
  (equal (subseq signature-1 0 6)
	 (subseq signature-2 0 6)))

(defun extract-parameters-for-urlmethod (parameters)
  (let ((function-parameters nil)
	(require-parameters nil)
	(key-parameters nil)
	(all-parameters nil)
	(current-elt :function))
    (dolist (i parameters)
      (if (and (symbolp i) (equal (elt (symbol-name i) 0) #\&))
	  (let ((iname (symbol-name i)))
	    (cond
	      ((string-equal iname "&REQUIRE") (setf current-elt :require))
	      ((string-equal iname "&KEY")     (setf current-elt :key))
	      (t (error 'urlmethod-unknown-keyword :unknown iname :lambda-list parameters))))
	  (progn
	    (ecase current-elt
	      (:function (push i function-parameters))
	      (:require  (push i require-parameters))
	      (:key      (push i key-parameters)))
	    (push (parameter-name-from-url-specialization i) all-parameters))))
    (values (reverse function-parameters)
	    (reverse require-parameters)
	    (reverse key-parameters)
	    (reverse all-parameters))))

(defun define-urlmethod-function (method-name handlersym methodsym requestsym parameters)
  (unless (find method-name *url-methods-seen*)
    (push method-name *url-methods-seen*)
    (multiple-value-bind (function-parameters require-parameters key-parameters all-parameters) (extract-parameters-for-urlmethod parameters)
      (declare (ignore require-parameters key-parameters all-parameters))
      `(defun ,method-name (,handlersym ,methodsym ,requestsym ,@function-parameters)
	(araneida::call-url-method (araneida::select-url-method ,handlersym ,methodsym ,requestsym ',method-name)
	 ,handlersym ,methodsym ,requestsym (list ,@function-parameters))))))

(defun create-urlmethod (type method-name handlersym methodsym requestsym parameters body)
  (multiple-value-bind (function-parameters require-parameters key-parameters all-parameters) (extract-parameters-for-urlmethod parameters)
    (with-gensyms (method-info conflicting-signature-pos length-function-parameters methods-of-the-same-name)
      `(let* ((,method-info (list
			     ',method-name ,(ecase type (:tainted :tainted) (:untainted :untainted))
			     ',function-parameters
			     ',require-parameters ',key-parameters
			     ',all-parameters (lambda (,handlersym ,methodsym ,requestsym ,@all-parameters)
						  ,@body)))
	      (,conflicting-signature-pos (position-if (lambda (elt)
							 (araneida::equal-urlmethod-signature elt
											      ,method-info))
						       araneida::*url-methods*))
	      (,length-function-parameters ,(length function-parameters))
	      (,methods-of-the-same-name (remove-if (lambda (method)
						      (not (eql (araneida::method-method-name method) ',method-name)))
						    araneida::*url-methods*)))
	(unless (zerop (length ,methods-of-the-same-name))
	  (when (some (lambda (method)
			(not (equal (length (araneida::method-function-parameters method)) ,length-function-parameters)))
		      ,methods-of-the-same-name)
	    (error 'araneida::urlmethod-function-parameter-mismatch
		   :parameters ',parameters
		   :other-methods-info ',methods-of-the-same-name)))
	(if ,conflicting-signature-pos
	    (setf (elt araneida::*url-methods* ,conflicting-signature-pos) ,method-info)
	    (pushnew ,method-info *url-methods*))))))

;FIXME: add specialization for methodsym
(defmacro defurlmethod (method-name (handlersym methodsym requestsym &rest parameters) &body body)
  "Define a urlmehod. This is like a CLOS method, but the parameters are extracted from the url (eventually will work with POST as well)
The syntax is very similar to defmethod.

(defurlmethod method-name (handler-symbol method-symbol request-symbol [function-parameter...] [&require require-parameter...] [&key key-parameter...])
  [docstring]
  body)

*-symbol: names which will be bound to the handler, method, and request
function-parameter -- when the method is called like so (method-name handler method request a b c...), a b c are function-parameters
require-parameter: parameter-name  -- requires that the parameter named be present. Parameter will be bound to this name
                 | (parameter-name (function [function-parameter...]) -- requires that parameter be present and that it satisfy
                                                                        (apply #'function parameter-value function-parameters)

key-parameter: parameter-name -- bind the value of the parameter (if any) to this name. nil if no value
             | (parameter-name parameter-default-value) -- binds the value to parameter name, or parameter-default-value if no value

Please note that parameter-name is matched to parameters without regard to case.
There are numerous examples in test-server.lisp

Precendence algorithm is as follows:
Given a list of methods that all satisfy the current call, return the most specific one(s).
The algorithm for this is a bit different than for standard generic methods.

(unimplemented) If any have a specialized handler, they take precedence over unspecialized
(unimplemented) Of specialized handlers, class precedence is as for standard CLOS
(unimplemented) Specialized request-method takes precedence over unspecialized
 Methods with more required parameters take precedence over those with fewer
 Methods with more specialized required parameters take precedence over those with fewer
 Methods with more key parameters take precedence over those with fewer
 Tainted methods take precedence over untainted"
  `(progn
    ,(define-urlmethod-function method-name handlersym methodsym requestsym parameters)
    ,(create-urlmethod :untainted method-name handlersym methodsym requestsym parameters body)))

(defmacro deftaintedurlmethod (method-name (handlersym methodsym requestsym &rest parameters) &body body)
  "Just like defurlmethod, except the parameter values will be tainted"
  `(progn
    ,(define-urlmethod-function method-name handlersym methodsym requestsym parameters)
    ,(create-urlmethod :tainted method-name handlersym methodsym requestsym parameters body)))
