;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          gentils.lisp
;;;; Purpose:       Main general utility functions for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: macros.lisp 10072 2004-10-01 04:01:58Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

(defmacro let-when ((var test-form) &body body)
  `(let ((,var ,test-form))
      (when ,var ,@body)))
  
(defmacro let-if ((var test-form) if-true &optional if-false)
  `(let ((,var ,test-form))
      (if ,var ,if-true ,if-false)))

;; Anaphoric macros

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body)
  `(aif2 ,test
         (progn ,@body)))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
         (aif2 ,test
               (progn ,@body)
               (setq ,flag nil))))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(defmacro mac (expr)
"Expand a macro"
  `(pprint (macroexpand-1 ',expr)))

(defmacro print-form-and-results (form)
  `(format t "~&~A --> ~S~%" (write-to-string ',form) ,form))


;;; Loop macros

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro with-each-stream-line ((var stream) &body body)
  (let ((eof (gensym))
	(eof-value (gensym))
	(strm (gensym)))
    `(let ((,strm ,stream)
	   (,eof ',eof-value))
      (do ((,var (read-line ,strm nil ,eof) (read-line ,strm nil ,eof)))
	  ((eql ,var ,eof))
	,@body))))

(defmacro with-each-file-line ((var file) &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,file :direction :input)
      (with-each-stream-line (,var ,stream)
	,@body))))


(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro mean (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
	  syms)
     ,@body))


(defmacro time-seconds (&body body)
  (let ((t1 (gensym)))
    `(let ((,t1 (get-internal-real-time)))
       (values
	(progn ,@body)
	(coerce (/ (- (get-internal-real-time) ,t1)
		   internal-time-units-per-second)
		'double-float)))))
  
(defmacro time-iterations (n &body body)
  (let ((i (gensym))
	(count (gensym)))
    `(progn
       (let ((,count ,n))
	 (format t "~&Test with ~d iterations: ~W" ,count (quote ,body))
	 (let ((t1 (get-internal-real-time)))
	   (dotimes (,i ,count)
	     ,@body)
	   (let* ((t2 (get-internal-real-time))
		  (secs (coerce (/ (- t2 t1)
				   internal-time-units-per-second)
				'double-float)))
	     (format t "~&Total time: ")
	     (print-seconds secs)
	     (format t ", time per iteration: ")
	     (print-seconds (coerce (/ secs ,n) 'double-float))))))))

(defmacro mv-bind (vars form &body body)
  `(multiple-value-bind ,vars ,form 
     ,@body))

;; From USENET
(defmacro deflex (var val &optional (doc nil docp))    
  "Defines a top level (global) lexical VAR with initial value VAL,
      which is assigned unconditionally as with DEFPARAMETER. If a DOC
      string is provided, it is attached to both the name |VAR| and the
      name *STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of
      kind 'VARIABLE. The new VAR will have lexical scope and thus may
      be shadowed by LET bindings without affecting its global value."
  (let* ((s0 (load-time-value (symbol-name '#:*storage-for-deflex-var-)))
	 (s1 (symbol-name var))
	 (p1 (symbol-package var))
	 (s2 (load-time-value (symbol-name '#:*)))
	 (backing-var (intern (concatenate 'string s0 s1 s2) p1)))
    `(progn
      (defparameter ,backing-var ,val ,@(when docp `(,doc)))
      ,@(when docp
	      `((setf (documentation ',var 'variable) ,doc)))
      (define-symbol-macro ,var ,backing-var))))

(defmacro def-cached-vector (name element-type)
  (let ((get-name (concat-symbol "get-" name "-vector"))
	(release-name (concat-symbol "release-" name "-vector"))
	(table-name (concat-symbol "*cached-" name "-table*"))
	(lock-name (concat-symbol "*cached-" name "-lock*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,table-name (make-hash-table :test 'equal))
       (defvar ,lock-name (kmrcl::make-lock ,name))
	 
	 (defun ,get-name (size)
	   (kmrcl::with-lock-held (,lock-name)
	     (let ((buffers (gethash (cons size ,element-type) ,table-name)))
	       (if buffers
		   (let ((buffer (pop buffers)))
		     (setf (gethash (cons size ,element-type) ,table-name) buffers)
		     buffer)
		 (make-array size :element-type ,element-type)))))
	 
	 (defun ,release-name (buffer)
	   (kmrcl::with-lock-held (,lock-name)
	     (let ((buffers (gethash (cons (array-total-size buffer)
					   ,element-type)
				     ,table-name)))
	       (setf (gethash (cons (array-total-size buffer)
				    ,element-type) ,table-name)
		 (cons buffer buffers))))))))

(defmacro def-cached-instance (name)
  (let* ((new-name (concat-symbol "new-" name "-instance"))
	 (release-name (concat-symbol "release-" name "-instance"))
	 (cache-name (concat-symbol "*cached-" name "-instance-table*"))
	 (lock-name (concat-symbol "*cached-" name "-instance-lock*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,cache-name nil)
       (defvar ,lock-name (kmrcl::make-lock ',name))
	 
	 (defun ,new-name ()
	   (kmrcl::with-lock-held (,lock-name)
	     (if ,cache-name
		 (pop ,cache-name)
		 (make-instance ',name))))
	 
	 (defun ,release-name (instance)
	   (kmrcl::with-lock-held (,lock-name)
	     (push instance ,cache-name))))))

(defmacro with-ignore-errors (&rest forms)
  `(progn
     ,@(mapcar
	(lambda (x) (list 'ignore-errors x))
	forms)))

(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
     (cond ((equal exp exp1)
	    (format t "~&Macro expansion:")
	    (pprint exp))
	   (t (format t "~&First step of expansion:")
	      (pprint exp1)
	      (format t "~%~%Final expansion:")
	      (pprint exp)))
     (format t "~%~%")
     (values)))

(defmacro defconstant* (sym value &optional doc)
  "Ensure VALUE is evaluated only once."
   `(defconstant ,sym (if (boundp ',sym)
			  (symbol-value ',sym)
			  ,value)
     ,@(when doc (list doc))))

(defmacro defvar-unbound (sym &optional (doc ""))
    "defvar with a documentation string."
    `(progn
      (defvar ,sym)
      (setf (documentation ',sym 'variable) ,doc)))

