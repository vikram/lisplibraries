;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          map.lisp
;;;; Purpose:       Extensions and variations of the map operator
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :utils)

(defmacro-exported map-> (fn start test-fn succ-fn)
  "A general map function to assemble a map operation over arbitrary sequences. 
   It takes an initial value 'start', a completion test function and a successor
   function which computes the next value"
  (let ((i (gensym))
	(result (gensym)))
    `(do ((,i ,start (funcall ,succ-fn ,i))
	  (,result nil))
	 ((funcall ,test-fn ,i) (nreverse ,result))
       (push (funcall ,fn ,i) ,result))))

(defmacro-exported mapa-b (fn a b &optional (step 1))
  "map over the integers from a to b"
  `(map-> ,fn ,a 
	  #'(lambda (x) (> x ,b))
	  #'(lambda (x) (+ x ,step))))

(defmacro-exported map0-n (fn n)
  "map over the integers from 0 to n"
  `(mapa-b ,fn 0 ,n))

(defun-exported collect (fn &rest lists)
  "cons together non-nil results"
  (labels ((rec (values lists)
		(if (null (car lists))
		    (nreverse values)
		  (rec (aif (apply fn (mapcar #'car lists))
			    (cons it values)
			    values)
		       (mapcar #'cdr lists)))))
    (rec nil lists)))
      

(eval-when (eval compile load)
  (proclaim '(inline last1 single append1 conc1))
  (proclaim '(optimize speed)))

(defun-exported last1 (lst)
  (car (last lst)))

(defun-exported single (lst)
  (and (consp lst) (not (cdr lst))))

(defun-exported append1 (lst obj) 
  (append lst (list obj)))

(defun-exported conc1 (lst obj)   
  (nconc lst (list obj)))

(defun-exported mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun-exported mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar-exported (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar 
             #'(lambda (&rest args) 
                 (apply #'rmapcar fn args))
             args)))

(defun-exported cars (list)
  (mapcar #'car list))

(defun-exported cdrs (list)
  (mapcar #'cdr list))

(defmacro-exported map-across (fn array)
  (with-gensyms (elt)
    `(loop for ,elt across ,array do
	   (funcall ,fn ,elt))))

(defmacro-exported map-across-n (fn array n)
  (with-gensyms (elt idx)
    `(loop for ,elt across ,array 
           for ,idx from 0 upto (1- ,n) do
	   (funcall ,fn ,elt))))

;;
;; Accumulation
;; 

(defmacro-exported accumulate-init (fn init &rest lists)
  `(accum ,fn ,init ,@lists))

(defmacro-exported accumulate-int (fn &rest lists)
  `(accum ,fn 0 ,@lists))

(defmacro-exported accumulate (fn &rest lists)
  `(accum ,fn nil ,@lists))

(defun-exported accum (fn init &rest lists)
  (labels ((cdrs (args) (mapcar #'cdr args))
           (cars (args) (mapcar #'car args))
	   (rec (val args)
		(if (or (null args)
			(and (consp args) (null (car args))))
		    val
		  (rec (apply fn val (cars args))
		       (cdrs args)))))
    (rec init lists)))

(defun-exported accumulate-list (fn list)
  "Map a binary operation over a list, accumulating
   the incremental results fn(fn(1,2),3)..."
  (if (= 1 (length list))
      (car list)
    (accumulate-init fn (car list) (cdr list))))

(defmacro-exported sum-list (list)
  `(apply #'+ ,list))

(defmacro-exported prod-list (list)
  `(apply #'* ,list))
