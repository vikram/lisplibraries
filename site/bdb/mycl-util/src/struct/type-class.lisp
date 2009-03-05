;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

(defun fun-not-initalized (&rest args)
  (declare (ignore args))
  (error "function not initialized"))

(defmacro deftype-class (name (&rest funs) &key (special-var (symb (gensym))))
  (let ((funs (mapcar (lambda (f) (fun-infos name f)) funs)))
  
  `(progn
    (defvar ,special-var nil)

    ,(create-with-macro name special-var)

    (defun ,(symb 'make- name) ,(create-constructor-arglist funs)
      (make-array ,(length funs)
		  :initial-contents (list ,@(mapcar #'fun-init-name funs))))
    
    ,@(loop for i from 0
	    for fun in funs
	    collect `(defun ,(fun-name fun)
		      ,(extended-fun-args fun special-var)
		      (funcall (aref ,special-var ,i) ,@ (fun-args fun)))))))

(defun create-with-macro (name special-var)
  `(defmacro ,(symb 'with- name) (var &body body)
      `(let ((,',special-var ,var))
	(declare (special ,',special-var))
	,@body)))


(defun extended-fun-args (fun special-var)
  (append (fun-args fun)
	  `(&optional (,special-var ,special-var))))

(defun fun-init-name (f)
  (cdr (assoc 'name f)))

(defun fun-name (f)
  (cdr (assoc 'fun-name f)))

(defun fun-args (f)
  (cdr (assoc 'args f)))

(defun fun-default (f)
  (cdr (assoc 'default f)))


(defun create-constructor-arglist (funs)
  (if (null funs)
      (list)
      `(&key ,@(mapcar (lambda (f)
			 `(,(fun-init-name f) ,(fun-default f)))
		       funs))))

(defun fun-infos (type-name fun)
  (destructuring-bind (name args
			    &key
			    (fun-name (symb type-name '- name))
			    (default `#'fun-not-initalized)) fun
    (list (cons 'name name)
	  (cons 'args args)
	  (cons 'fun-name fun-name)
	  (cons 'default default))))

;;;
