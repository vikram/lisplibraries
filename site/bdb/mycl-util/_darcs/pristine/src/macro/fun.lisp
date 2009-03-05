;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Function builders

(defmacro fn (expr)
  "builds a function taking one argument and applying this arg
   to included functions.
   syntax: (fn <expr>)
           <expr> ::= function | (operator <expr>*)
   semantics:
           <expr> is recursivle translated into:
                  (lambda (x) (f1
                                (fa x)
                                (fb x)
                                (fc x)))
                  where fa, fb, fc might be translated although 
                  if satisfie pattern: (operator <expr>*)
   e.g. (fn f1) -> #'f1
        (fn f1 fa fb) -> (lambda (x) (f1 (fa x) (fb x)))
        (fn f1 (f2 fa fb) fc) ->
          (lambda (x) 
            (f1 ((lambda (y) (f2 (fa y)
                                 (fb y)))
                          x)
                         (fc x))) 
                 --(in mind subst. lambda)-->
                   (lambda (x) (f1 (f2 (fa x)
                                       (fb x))
                                   (fc x)))
                      "
  `#',(rbuild expr))

(defun rbuild (expr)
  "recursivle builds the final lambda for 'fn'"
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
	  (build-compose (cdr expr))
	  (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (with-gensyms (g)
    `(lambda (,g)
      (,op ,@(mapcar (lambda (f)
		       `(,(rbuild f) ,g))
		     fns)))))

(defun build-compose (fns)
  (with-gensyms (g)
    `(lambda (,g)
      ,(labels ((rec (fns)
		     (if fns
			 `(,(rbuild (car fns))
			   ,(rec (cdr fns)))
			 g)))
	       (rec fns)))))


(defun fif (if then &optional else)
  "returns new function evaluating
     'if' to the applied arguments.
     if 'if' yields true, then 'then' is evaluated
     else 'else"
  #'(lambda (&rest args)
      (if (apply if args)
	  (apply then args)
	  (when else
	    (apply else args)))))

(defun fwhen (when then)
  (fif when then))

(defun funless (unless then)
  (fif (complement unless) then))

(defun fchain (fn-comb fn &rest fns)
  "uses fn-comb to a list of values returned by fns, where
   every fn in fns is called with x
   e.g. (fchain (lambda (x y) (and x y)) f1 f2 ... fn)
        creates a function like: 
          (lambda (x)
            (and (f1 x)
                 (f2 x)
                 ...
                 (fn x)))
        in truth it is more like:
          (lambda (x)
            (and (f1 x)
                 ((lambda (y) (and (f2 y)
                                   (...) y))
                  x)))

    works similiar to 
      (defun freduce (fn-comb fn &rest fns)
        (lambda (arg)
          (reduce (lambda (x fn-y)
 	      (funcall fn-comb
		       x
		       (funcall fn-y arg)))
	      fns :initial-value (funcall fn arg))))
    but builds one in comparison to freduce, fchain builds
    up a big function when called first instead of reducing on the list
    of functions.
"
  (if (null fns)
      fn
      (let ((chain (apply #'fchain fn-comb fns)))
	(lambda (x)
	  (funcall fn-comb
		   (funcall fn x)
		   (funcall chain x))))))

(defun fint (fn &rest fns)
  "intersection"
  (apply #'fchain
	 (lambda (x y) (and x y))
	 fn fns))

(defun funion (fn &rest fns)
  "union"
  (apply #'fchain
	 (lambda (x y) (or x y))
	 fn fns))

(defun lrec (rec &optional base)
  "simplifies building a recursive functions over lists and finally returns
   evaluation of base.

   rec has to be a fun with 2 args.
     (lambda (value cont) ...),
     where value is the car of the list
     and cont is a function used to continue recursion"
  (labels ((self (lst)
	     (if (null lst)
		 (if (functionp base)
		     (funcall base)
		     base)
		 (funcall rec (car lst)
			  (lambda ()
			    (self (cdr lst)))))))
    #'self))

(defun compose (&rest fns)
  "composing a number of functions.
   Every function, but the last one, must take exaclty one argument."
  (if fns
      (let ((fn1 (last1 fns))
	    (fns (butlast fns)))
	(lambda (&rest args)
	  (reduce #'funcall fns
		  :from-end t
		  :initial-value (apply fn1 args))))
      #'identity))

(defun curry (fun &rest init-args)
  (lambda (&rest args)
    (apply fun (append init-args args))))

(defun rcurry (fun &rest init-args)
  "reverse curry"
  (lambda (&rest args)
    (apply fun (append args init-args))))

(defmacro rlambda (name args &body body)
  "build a recursive 'anonymous' lambda"
  `(lambda (,args)
    (labels ((,name ,args ,@body)))
    (,name ,@args)))
