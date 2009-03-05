;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          asdf-config
;;;; Purpose:       An extension to the asdf build system that accounts for special
;;;;                parameters for a given module of code; this allows user or platform
;;;;                specific declaration of parameters, file locations, URLs, etc.
;;;;                This should have minimal external dependencies on utilities, etc so
;;;;                it can be widely used.
;;;; Programmer:    Ian S. Eslick   [ Copyright (c) MIT Media Lab, 2004 ]
;;;; Date Started:  October 2004

(defpackage :asdf-config
  (:use #:cl #:asdf)
  (:export #:defsystem-config
	   #:system-config
	   #:configure-op
	   #:initialize-op
	   #:set-parameter
	   #:set-parameters))

(in-package :asdf-config)

;; -- asdf-config --

;; allows us to specify configurations of packages that depend on non-lisp dependencies
;; such as files, net server resources, etc.  The notion of external resources should
;; be extendable as should the initialization procedures.
;;
;; Flow will typically be compile, load code.  Establish external deps (probe file, 
;; network op, etc).  The package spec will register handler functions to perform
;; inits and asdf will allow a namespace to be set by setup code.  A name can be
;; tested for existence and either have a default or be required.  The 'think root'
;; for the think system could be a shared dependency, etc.

;; Operations:
;; - configure - apply system-specific parameter settings
;;               verify existence of any data files
;;               home package
;; - initialize - Call load, configure then call special load function for module
;; - load - overload load for persistant-components

;; Components:
;; - parameter component (instead of file)
;;   (:parameter :my-param :default nil :required t :parameter *my-param*)
;; - configurable-component
;;   configure-op
;;   initialize-op 
;;   :initialization #'init-package
;; - testable-system
;;   :test-op

;; Interface:
;; defethod: (asdf:set-parameter :system :my-param form) -> delayed (setf *my-param* value)
;;   users call prior to (asdf:operate 'asdf:configure :system)
;; macro: (asdf:definitialize init-package :system-name body)
;;   implementers can get access to: package-root (in source)
;;                                   parameters (from users)
;;   other system defs
;;   'system is bound to component of system-name
;;   (asdf:system-root system) 
;;   (asdf:system-data-file system tag)
;;   (asdf:get-system :system-name)

;; Conditions:
;; - data-file-dependency-condition
;; - configuration-parameter-missing-condition

;; Questions:
;; - Use of logical paths standardized?
;;   configurable systems have a home within a standard logical heirarchy?
;;   how to ensure heirarchy is valid

(defmacro defsystem-config (name &body options)
  `(defsystem ,name ,@(append (list :class 'asdf-config:system-config) options)))

(defclass system-config (system)
  (;;(default-package :accessor system-config-default-package :initarg :default-package :initform nil)
   (parameters :accessor system-config-parameters :initarg :parameters :initform nil)
   (configured-p :accessor system-config-configured-p :initform nil)
   (initialization :accessor system-config-initialization :initarg :initialization :initform nil)
   (initialized-p :accessor system-config-initialized-p :initform nil)))

;; ---------------------------------------------------------------------------------
;; configure op - translate user parameter sets to actual setting inside the module

(defclass configure-op (operation) ())

(defmethod perform ((o configure-op) (c system-config))
  (mapc #'(lambda (param-record) (config-parameter c param-record))
	(system-config-parameters c)))

(defmethod perform :after ((o configure-op) (c system-config))
  (setf (system-config-configured-p c) t))

(defmethod perform ((o configure-op) (c component)) t)

(defmethod operation-done-p ((o configure-op) (c system-config))
  (system-config-configured-p c))

(defmethod component-depends-on ((o configure-op) (c component))
  "Operation must be loaded in order to configure"
  (let ((what-would-load-op-do
	 (cdr (assoc 'load-op (slot-value c 'asdf::in-order-to)))))
    (cons (list 'load-op (component-name c))
	  (mapcar (lambda (dep)
		    (if (eq (car dep) 'asdf:load-op)
			(cons 'configure-op (cdr dep))
		      dep))
		  what-would-load-op-do))))

;;	    (cons (list 'load-op (component-name c))
;;		  (call-next-method)))))

;; initialize op - load data files, etc
(defclass initialize-op (operation) ())

(defmethod perform ((o initialize-op) (c system-config))
  (format t "Initializing ~A by calling ~A~%" (component-name c)
	  (system-config-initialization c))
  (funcall (symbol-from-qualified-string 
	    (system-config-initialization c))))

(defmethod perform ((o initialize-op) (c component)) t)

(defmethod perform :after ((o initialize-op) (c system-config))
  (setf (system-config-initialized-p c) t))

(defmethod operation-done-p ((o initialize-op) (c system-config))
  (and (system-config-initialized-p c)
       (call-next-method)))

(defmethod component-depends-on ((o initialize-op) (c component))
  "Operation must be configured in order to initialize"
  (let ((what-would-load-op-do
	 (cdr (assoc 'load-op (slot-value c 'asdf::in-order-to)))))
;;    (cons (list 'initialize-op 
    (cons (list 'configure-op (component-name c))
	  (mapcar (lambda (dep)
		    (if (eq (car dep) 'load-op)
			(cons 'initialize-op (cdr dep))
		      dep))
		  what-would-load-op-do))))
;;	  (cons (list 'configure-op (component-name c))
;;		(call-next-method)))))

;; ---------------------------------------------------------------
;; Parameter setting and retrieving
(defvar *configuration-systems* (make-hash-table :test #'equalp))

(defun get-config-param (system name)
  "Get a value for system and name provided"
  (cdr (assoc (get-name name) (gethash (get-name system) *configuration-systems*) :test #'equalp)))

(defun set-config-param (system name value)
  "Set a value for system and name provided"
  (let ((sname (get-name system))
	(pname (get-name name)))
    (setf (gethash sname *configuration-systems*)
	  (acons pname value (gethash sname *configuration-systems*)))))

(defun get-name (name)
  (typecase name
    (string name)
    (symbol (symbol-name name))
    (t (error "Invalid name type provided in set-parameter; use a string or symbol."))))

;; ---------------------------------------------------------------
;; User interface
(defun set-parameter (system name value)
  "Users call this interface to associate a configuration
   name with a particular value."
  (set-config-param system name value)
  t)

(defun set-parameters (triples)
  (mapc #'(lambda (triple) (apply #'set-parameter triple)) triples)
  t)

;; ---------------------------------------------------------------
;; Helpers to setup parameters from configure-op
(define-condition configuration-error (operation-error)
  ((parameter :reader error-config-parameter :initarg :parameter)
   (param-doc-string :reader error-config-parameter-doc :initarg :parameter-doc))
  (:report (lambda (c s)
	     (format s "~@<Required parameter ~A \"~A\"~%for system ~A has not been set~@:>"
		     (error-config-parameter c) (error-config-parameter-doc c) (error-component c)))))
                        
;; We use the first name from the .asd file parameters option as
;; the external name people need to set prior to execution.  The
;; second symbol is a fully-qualified symbol name which will be set
;; when the user sets the named parameter.  The remainder are keywords
;; with options (see dbind below)
(defmethod config-parameter (system parameter-list)
  "During configuration operations this is used to pick a value for 
   all configured parameters."
  (if (null parameter-list)
      nil
    (destructuring-bind (name param-string &key required default) parameter-list
      (let ((value (get-config-param (component-name system) name))
	    (parameter (symbol-from-qualified-string param-string)))
	(format t "param: ~A value: ~A default: ~A~%" parameter value default)
	(when (null parameter) (error "Package for parameter ~A has not been created." param-string))
	(cond (value 
	       (format t "setting provided value: ~A <- ~A~%" parameter value)
	       (setf (symbol-value parameter) value))
	      (required 
	       (setf (symbol-value parameter) 
		     (restart-case (error 'configuration-error :component system :operation 'configure-op 
					  :parameter name :parameter-doc (documentation parameter 'variable))
				   (enter-value (value)
						:report "Specify a value."
						:interactive (lambda ()
							       (format t "~&Value to use: ")
							       (list (eval (read))))
						value)
				   (continue ()
					     :report (lambda (stream)
						       (format stream "Continue using default value: ~A " default))
					     default))))
	      (t (if (and (null default) (symbol-value parameter))
		     (format t "keeping value: ~A <- ~A~%" parameter (symbol-value parameter))
		   (progn
		     (format t "using default: ~A <- ~A~%" parameter (symbol-value parameter))
		     (setf (symbol-value parameter) default)))))))))

(defun symbol-from-qualified-string (string)
  (let ((pos (position #\: string)))
    (if (find-package (string-upcase 
		       (subseq string 0 pos)))
	(intern (string-upcase 
		 (subseq string (if (char= #\: (char string (1+ pos)))
				    (+ 2 pos)
				  (1+ pos))))
		(string-upcase 
		 (subseq string 0 pos)))
      nil)))

;; -------------------------------------------
;; Inspection, see what params are available

(defun describe-parameters (system-name)
  (let ((system (find-system system-name)))
    (if (eq (type-of system) 'system-config)
	(progn 
	  (format t "System has the following parameters and descriptions:~%")
	  (map nil #'print-parameter (system-config-parameters system)))
      (format t "Not a parameterized system.~%"))))

(defun print-parameter (param-list)
  (destructuring-bind (name param-string &key required default) param-list
    (let ((param (symbol-from-qualified-string param-string)))
      (format t "~A '\:~A', \"~A\" with ~A~%" 
	      (if required "Required" "Optional") (string-downcase name)
	      (if (null param) "<docs not loaded>" (documentation param 'variable)) 
	      (if default (format nil "default ~A." default) "no default.")))))
	    
;; ====================================================================================
;; Get dependencies - nice utility for pulling out all sources
;; ====================================================================================

(defparameter *visited* nil)
(defun get-lisp-source-filenames (system-name)
  (labels ((get-path (asdf-object)
		     (cond ((and (subtypep (type-of asdf-object) 'source-file)
				 (equal (source-file-type asdf-object (find-system system-name)) "lisp"))
			    (list (component-pathname asdf-object)))
			   ((subtypep (type-of asdf-object) 'module)
			    (mapcan #'get-path (module-components asdf-object))))))
    (setf *visited* nil)
    (mapcan #'get-path (get-all-sources system-name))))

(defun get-all-sources (system-name &key (reset-visited t))
 ;; Ensure the system is loaded in
;; (asdf:operate 'asdf:load-op system-name)
 ;; Then parse the dependency tree
  (when reset-visited (setf *visited* nil))
  (let ((system (find-system system-name)))
   (push system *visited*)
   (append
    (mapcan #'(lambda (dep)
		(let ((new (find-system dep)))
		  (when (not (member new *visited*))
;;		    (format t "visiting: ~A~%" new)
		    (copy-list (get-all-sources new :reset-visited nil)))))
            (mapcan #'(lambda (record)
			(destructuring-bind (op &rest systems) record
			  (declare (ignore op))
;;			  (format t "entry: ~A~%" record)
			  (copy-list systems)))
		    (component-depends-on (make-instance 'asdf:load-op) system)))
    (module-components system))))




