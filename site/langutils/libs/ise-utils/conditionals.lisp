;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          conditionals.lisp
;;;; Purpose:       All conditional related utilities including boolean tests and control constructs
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :utils)

;; ------------------------------
;; Boolean conditionals utilities

(defmacro-exported neq (a b)
  `(not (eq ,a ,b)))

(defmacro-exported neql (a b)
  `(not (eql ,a ,b)))

(defmacro-exported nequal (a b)
  `(not (equal ,a ,b)))

(defmacro-exported error-on-null (exp &rest error-args)
  (with-gensyms (value)
    `(let ((,value ,exp))
       (if (null ,value)
	 (error ,@error-args)
	 ,value))))

;; -------------------------------------------------
;; Predicate generators - cheap filter, mapcon, etc

(defmacro-exported gen-everyp (pred)
  "Creates a predicate that ensures pred is satisfied for all elements in a list"
  `#'(lambda (list) (every ,pred list)))

;; -------------------------------------------------
;; Anaphoric control structures

(defmacro-exported aif (test-form then-form &optional else-form) 
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro-exported aif-ret (test-form else-form)
  `(let ((it ,test-form))
     (if it it ,else-form)))

(defmacro-exported aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro-exported awhen (test-form &body body) 
  `(aif ,test-form
	(progn ,@body)))

(defmacro-exported awhen2 (test &body body)
  `(aif2 ,test
         (progn ,@body)))

(defmacro-exported awhen-null (test-form default-value)
  `(aif ,test-form it ,default-value))

(defmacro-exported awhen0 (test-form when-zero-form)
  `(let ((it ,test-form))
     (if (= it 0)
	 ,when-zero-form
       it)))

(defmacro-exported awhile (expr &body body) 
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro-exported awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
         (aif2 ,test
               (progn ,@body)
               (setq ,flag nil))))))

(defmacro-exported aand (&rest args) 
  "Makes the prior element in an and available to the next"
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro-exported acond (&rest clauses) 
  "Anaphoric cond"
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (sym (gensym))
	  (it (intern "IT" *package*)))
      `(let ((,sym ,(car cl1)))
	 (if ,sym
	     (let ((,it ,sym))
	       (declare (ignorable ,it))
	       ,@(cdr cl1))
	   (acond ,@(cdr clauses)))))))

(defmacro-exported acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym))
	    (it (intern "IT" *package*)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((,it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(defmacro-exported acond-mv (&rest clauses)
  "If the second value of a multiple-value return from a
   clause is true, it is bound to the primary value"
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (val (gensym))
	  (win (gensym)))
      `(multiple-value-bind (,val ,win) ,(car cl1)
	 (if (or ,val ,win)
	     (let ((it ,val)) 
	       (declare (ignorable it))
	       ,@(cdr cl1))
	   (acond-mv ,@(cdr clauses)))))))

(defmacro-exported acond2-mv (&rest clauses)
  "If the primary value of a multiple-value return from a
   clause is true, 'it' is bound to the second value"
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (val (gensym))
	  (win (gensym)))
      `(multiple-value-bind (,win ,val) ,(car cl1)
	 (if ,win
	     (let ((it ,val)) 
	       (declare (ignorable it))
	       ,@(cdr cl1))
	   (acond2-mv ,@(cdr clauses)))))))


;;
;; Other Paul Graham utilities
;; 

(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                        (mapcar #'car 
                                (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                 ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))


(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                  (cons (cdr (assoc (car bindform) vars))
                        (cdr bindform))))
          (cdr cl)))

(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (?     ,?-case)
     (t     ,t-case)))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))
