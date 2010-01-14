;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clos.lisp
;;;; Purpose:       File related utility functions
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;


;; ====================================
;; CLOS Specific Utilities and Macros
;; ====================================

(in-package :utils)

;; Syntactic sugar for defclass to cleanup defaults
(defmacro-exported defclass+ (name superclasses slots &rest options)
  ;
  ; Simplified defclass (behaves like defstruct)
  ; Can add a 'option' which prefixes all accessors with a string (:prefix "class-name-")
  ;
  (let* ((class-options (remove :prefix options :key #'car))
	 (prefix-list (find :prefix options :key #'car))
	 (prefix (when prefix-list (string-upcase (second prefix-list)))))
    `(defclass ,name ,superclasses
       ,(loop for slot in slots collect
	      (let* ((init-val-p (listp slot))
		     (slot-name  (if init-val-p (car slot) slot))
		     (accessor-name (if prefix-list 
					(intern-format "~A~A" prefix slot-name)
				      (intern-format "~A-~A" name slot-name))))
		`(,slot-name :accessor ,accessor-name
			     ,@(when init-val-p (list ':initform (cadr slot)))
			     :initarg ,(intern (symbol-name slot-name)
					       (find-package 'keyword)))))
       ,@class-options)))

(defmacro-exported defclass+-exported (name superclasses slots &rest options)
  `(progn
     (eval-when (compile load eval)
       (export ',name))
     (defclass+ ,name ,superclasses ,slots ,@options)))

;; CLOS 'SELF' UTILITIES
;;
;; These set of utilities are syntactic sugar that make clos
;; feel a bit more like home for those from the C++ or Java
;; worlds.  It also makes writing generic methods a little
;; simpler.  These are 'self'-anaphoric macros that assume
;; the variable 'self' is bound to the clos instance being
;; manipulated.  'it' is used in various macros to refer
;; to the slot value in the macro body.

(defmacro-exported defmethod+ (type method args &rest body)
  `(defmethod ,method ((self ,type) ,@args) ,@body))

(defmacro-exported defmethod+-exported (type method args &rest body)
  `(progn
     (eval-when (compile load eval)
       (export ',method))
     (defmethod ,method ((self ,type) ,@args) ,@body)))

(defmacro-exported with-slots+ (slots &rest body)
  (declare (special self))
  `(with-slots ,slots self ,@body))

(defmacro-exported with-accessors+ (slots &rest body)
  `(with-accessors ,slots self ,@body))

;; Copies values from src to target using
;; a list of accessors, if an accessor is a listp
;; it contains the accessor and the target expression
(defmacro-exported copy-slots ((src target) &rest alist)
  `(progn 
     ,@(loop for accessor in alist
	   collect
	     (if (listp accessor)
		 `(let ((it (,(car accessor) ,src)))
		    (setf (,(car accessor) ,target) ,(cadr accessor)))
	       `(setf (,accessor ,target) (,accessor ,src))))))

#+mop 
(defmacro-exported proxy-for-class-slots (class-name method-name slot-name &rest others)
  "This macro creates a method instance for the provided class that calls the
   same method on one or more of that classes slot elements.  Only works for
   methods where slot object type is the first argument.  Does not work for 
   multiple dispatch generic functions and does not yet work for keyword and
   rest or optional arguments."
  (assert (eq 0 (mod (length others) 2))) ;; even # of other pairs
  (let ((pairs (cons (list method-name slot-name)
		     (when others (group others 2))))
	(inst (gensym)))
    `(progn
       ,@(loop for pair in pairs collecting
	       (dbind (method slot) pair
		      `(defmethod ,method ((,inst ,class-name) 
					   ,@(cdr (mop:generic-function-lambda-list (symbol-function method))))
			 (funcall ,method-name 
				  (slot-value ,inst ,slot) 
				  ,@(cdr (mop:generic-function-lambda-list (symbol-function method))))))))))
#-mop
(defmacro-exported proxy-for-class-slots (class-name method-name slot-name &rest others)
  (error "MOP not supported in this lisp"))
				   

;; =============================================
;; CLOS INSTANCE HEIRARCHY UTILITIES
;; =============================================

;; NOTE: opportunity to create a nice macro to generate
;;       custom walkers

;; Walk a heirarchy of objects, keywords for functions
;; to walk the heirarchy and perform actions at leaves
;; or at each intermediate node
(defun-exported walk-heirarchy (root &key 
			    ;; Walk keywords
			    next-level
			    (order :before)
			    ;; Action keywords
			    (node-action nil)
			    (leaf-action nil)
			    (key nil))
  (labels ((walker (node)
	     ;; Call node action on all nodes before visiting children
	     (when (and node-action (eq order :before))
	       (funcall node-action 
			(if key 
			    (funcall key node) 
			  node)))
	     ;; If leaf call leaf routine
	     (aif (null (funcall next-level node)) ;; is leaf
		  (when leaf-action (funcall leaf-action
					     (if key
						 (funcall key node)
					       node)))
		  (mapcar #'walker (mklist (funcall next-level node))))
	     ;; Call node action on all nodes after visiting children
	     (when (and node-action (eq order :after))
	       (funcall node-action
			(if key
			    (funcall key node)
			  node)))))
    (walker root)))
	 
  


;; =============================================
;;
;;    GENERIC FUNCTION UTILITIES
;; 
;; =============================================

(defclass+-exported generic-function-family ()
  ((name nil)     ;; name of the family (table, etc)
   (members nil)  ;; generic functions belonging (names)
   (extends nil)  ;; inheritance from other familes
   (metadata nil)) ;; metadata describing the family
  (:prefix "gff-"))

;; NOTES:
;; - What happens if a family is extended (broadcast or recompile?)
;; - What is a family used for?
;;   - remap-slot - make all gfs work on a new object but redirect on the slot or object (:single or :multiple use)
;;   - generic-algorithm - in the :supports and :uses slots (can dispatch on objects that support familes?)
;;   - generic-datastruct - :supports
;;   - 

;;(defun make-gff ())



