;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Unspecified macros

(defmacro abbrev (short long)
  "creates a new macro being an abbreviation for long
   e.g (abbrev dbind multiple-value-bind)
       -> use dbind instead of multiple-value-bind"
  `(defmacro ,short (&rest args)
    `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  "creates abbreviations for a list of paired functions
   e.g. (abbrevs short1 long1
                 short2 long2
                 ...)"
  `(progn
    ,@(mapcar (lambda (pair)
		`(abbrev ,@pair))
	      (group names 2))))

(defmacro in (obj &rest choices)
  "tests if 'obj' is in the list of 'choices'
   use: (in obj 'a 'b 'c ...)
   expands into: (or (eql obj 'a)
                     (eql obj 'b)
                     ...)"
  (with-gensyms (insym)
    `(let ((,insym ,obj))
      (or ,@(loop for c in choices
		  collect `(eql ,insym ,c))))))

(defmacro inq (obj &rest choices)
  "like in, but quotes choices"
  `(in ,obj ,@(loop for c in choices
		    collect `',c)))


;;;condlet
(defmacro condlet (clauses &body body)
  "condlet allows you to locally bind variables based on conditions.
   syntax: (condlet (<condlet-clause>) forms*)
           <condlet-clause> ::= (predicate <condlet-binds>*)
           <condlet-binds> ::= variable | (variable form)
   semantics:
     - if one of the predicates yields true, the bindings belonging to it
       are established within the body
     - all variables are available in body. If a variable is not bound
       by a specific condition it's value is nil
  e.g.
     
       (condlet (((= 1 2) (x 1) (y 2))
	         ((= 1 1) (x 2) (y 1))
         	 (t (x 3) (z 3)))
         (list x y z)) 
     -> (2 1 nil)

"
  (let ((bodfn (gensym))
	(vars (loop for v in (remove-duplicates
			      (mapcar #'car (mappend #'cdr clauses)))
		    collect (cons v (gensym)))))
    `(labels ((,bodfn ,(mapcar #'car vars)
	       ,@body))
      (cond ,@(loop for cl in clauses
		    collect (condlet-clause vars cl bodfn))))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
		(let ,(condlet-binds vars cl)
		  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (loop for bindform in (cdr cl)
	collect (if (consp bindform)
		    (cons (cdr (assoc (car bindform) vars))
			  (cdr bindform)))))

;;;end condlet

(defmacro def-construct (name var1 var2)
  "creates a new datatype using a cons
   syntax: (def-construct name slot-info slot-info)
           slot-info ::= slot-name | (slot-name init-form)"
  (let ((var1-init (if (consp var1) (second var1) 'nil))
	(var2-init (if (consp var2) (second var2) 'nil)))
  `(progn
    
    (defun ,(symb 'make- name) (&optional v1 v2)
      (cons
       (if v1 v1 ,var1-init)
       (if v2 v2 ,var2-init)))

    ,(create-accessors var1 name 'car)
    ,(create-accessors var2 name 'cdr))))

(defun create-accessors (var name accessor)
  (let ((var-name (symb name '- (if (consp var)
				    (car var)
				    var))))
    `(progn
      (defun ,var-name (struct)
	(,accessor struct))
      (defun (setf ,var-name) (value struct)
	(setf (,accessor struct) value)))))
