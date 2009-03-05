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

(in-package :asdf-config)

;; ASDF-CONFIG
;;
;; This extension to asdf was developed to simplify exporting a module or package 
;; to a user when that module depends on external resources that are user or machine
;; specific and want to be setup external to the system source code.  If the system
;; depends on certain data files being loaded or network systems being tested, a 
;; single top-level initialization function can be called after the system is configured.
;;
;; This package is complementary to other lisp abstractions in hiding system code from
;; machine or user specific configuration:
;; - default defparameter and defvar values (user can accept or override)
;; - Machine-specific paths can also be managed using logical pathnames as defaults and
;; requesting the user define the root directory for a logical filesystem.
;;
;; asdf-config adds two new operations to the asdf interface:
;; (asdf:oos 'asdf-config:configure-op ...) and
;; (asdf:oos 'asdf-config:initialize-op ...) 
;; and modifies the behavior of the load operation.
;; 
;; Operation behaviors:
;; - configure - apply system-specific parameter settings, verify the existence 
;;               of any data files and the home package for the system being configured.
;; - initialize - Ensure that the system is loaded and configured then call a registered
;;                load function for module
;; - load - overload load for persistant-components
;;
;; asdf-config also adds some components to supply data for the new operations:
;;
;; New asdf component:
;; - system-config
;;   Extends the basic system with new fields that provide a list of settable 
;;   configuration parameters and an initialization statement to be called
;;   via initialize-op
;;
;; System flow:
;; 1) System Developer extends the asdf-based system description using defsystem-config
;;    a) Provides a set of parameter names to export to the user and defparams or defvars
;;       to set within the system after it is loaded.  The user parameter names can be
;;       set to specific values prior to the loading of a system.
;;    b) Provide an initialization function to call (optionally) that uses the parameter
;;       setting to load files, test for network resources, etc.
;; 2) User loads asdf-config after asdf.lisp
;; 3) User then sets parameters in their personal or machine initialization files that
;;    customize the system to their preferences using 'asdf-config:set-parameters
;;    or individual calls to asdf-config:set-parameter with entries being a quoted 
;;    list of triples containing (:system :parameter-name value-expression)
;;
;; The value is a lisp value and cannot depend on names or values from within the 
;; system being configured (the package will have to convert strings or symbols
;; internally if necessary)
;;
;; A good example of asdf-config use is the downloadable system 'langutils' at:
;; http://www.media.mit.edu/~eslick/langutils
;; The following entries are from the author's init file and langutils.asd
;;
;; langutils.asd
;; ------------------
;; (defsystem-config :langutils
;;  :description "Natural Language Toolkit"
;;  :version "1.0"
;;  ... 
;;  :parameters ((:token-map "langutils::*default-token-map-file*")
;;		 (:lexicon "langutils::*default-lexicon-file*")
;;	         (:stems "langutils::*default-stems-file*")
;;		 (:lexical-rules "langutils::*default-lexical-rule-file*")
;;		 (:contextual-rules "langutils::*default-contextual-rule-file*")
;;		 (:stopwords "langutils::*default-stopwords-file*")
;;		 (:concise-stopwords "langutils::*default-concise-stopwords-file*"))
;;  :initialization "langutils::init-langutils")
;;
;; clinit.cl
;; -------------
;; (asdf-config:set-parameters
;;   '((:langutils :token-map "c:/Work/lisp/my-data/langutils-token-map.sexp")
;;     (:langutils :stopwords "c:/Work/lisp/my-data/langutils-custom-stopwords.txt")))
;;
;; Parameter entries have the form:
;;   (:parameter-name :my-param :default nil :required t :parameter *my-param*)
;; Initialization command is a string which is resolved into a function name AFTER
;;   the system has been loaded and the package and name are valid.
;;
;; Utility functions:
;; describe-parameters ( system-name ) - describes the parameters available in a loaded system
;;
;; get-lisp-source-filenames ( system-name ) - provides a list of paths for source 
;;     files for the system.  Can be slow as forces a parse of all dependencies and provides
;;     not just those for the package, but everything that system depends on.
;;
;; get-all-sources ( system-name &key (reset-visited t)) - Get all the source files for the
;;     package for all its dependencies.  reset-visited means that you force the dependency
;;     tracker to revisit all systems.  Returns asdf source file objects.
;; 
;; Features left to implement:
;; - Validate the existence of logical filesystems during configuration setting
;;   (systematic vs. per-system ad-hoc approach currently allowed via init routine)
;; - Validate the existence of external resources during initialization 
;;   (systematic vs. per-system ad-hoc approach currently allowed via init routine)
;; - More native support for logical filesystems
;;

(defclass system-config (system)
  (;;(default-package :accessor system-config-default-package :initarg :default-package :initform nil)
   (parameters :accessor system-config-parameters :initarg :parameters :initform nil)
   (configured-p :accessor system-config-configured-p :initform nil)
   (initialization :accessor system-config-initialization :initarg :initialization :initform nil)
   (initialized-p :accessor system-config-initialized-p :initform nil)))

(defmacro defsystem-config (name &body options)
  "This ensures that asdf-config entries will be properly parsed"
  `(defsystem ,name ,@(append (list :class 'asdf-config:system-config) options)))

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

;; ---------------------------------------------------------------------------------
;; initialize op - developer can load data files, check external deps, etc

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
    (cons (list 'configure-op (component-name c))
	  (mapcar (lambda (dep)
		    (if (eq (car dep) 'load-op)
			(cons 'initialize-op (cdr dep))
		      dep))
		  what-would-load-op-do))))

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
;; UTILITIES: Get dependencies
;; ====================================================================================

(defparameter *visited* nil)
(defun get-lisp-source-filenames (system-name)
  "Get the filenames for all the .lisp/.cl sources"
  (labels ((get-path (asdf-object)
		     (cond ((and (subtypep (type-of asdf-object) 'source-file)
				 (equal (source-file-type asdf-object (find-system system-name)) "lisp"))
			    (list (component-pathname asdf-object)))
			   ((subtypep (type-of asdf-object) 'module)
			    (mapcan #'get-path (module-components asdf-object))))))
    (setf *visited* nil)
    (mapcan #'get-path (get-all-sources system-name))))

(defun get-all-sources (system-name &key (reset-visited t))
 "Ensure the system is loaded in (asdf:operate 'asdf:load-op system-name).
  Then parse the dependency tree for all sources."
  (when reset-visited (setf *visited* nil))
  (let ((system (find-system system-name)))
   (push system *visited*)
   (append
    (mapcan #'(lambda (dep)
		(let ((new (find-system dep)))
		  (when (not (member new *visited*))
		    (copy-list (get-all-sources new :reset-visited nil)))))
            (mapcan #'(lambda (record)
			(destructuring-bind (op &rest systems) record
			  (declare (ignore op))
			  (copy-list systems)))
		    (component-depends-on (make-instance 'asdf:load-op) system)))
    (module-components system))))


   

  


