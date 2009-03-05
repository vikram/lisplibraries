;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Setf macros

(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
      (setf ,@(mapcan (lambda (a) `(,a ,gval))
		      args)))))

(defmacro nilf (&rest args)
  `(allf nil ,@args))

(defmacro tf (&rest args)
  `(allf t ,@args))

(defmacro togglef (&rest args)
  `(progn
    ,@(loop for a in args
	    collect `(toggle! ,a))))

(define-modify-macro toggle! () not)

(define-modify-macro concf () nconc)

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let (,@(mapcar #'list vars forms)
	   (,(car var) (,op ,access ,@args)))
      ,set)))

(defmacro sortf (op &rest places)
  (let* ((meths (loop for p in places
		     collect (multiple-value-list
			      (get-setf-expansion p))))
	 (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
		    (mapcan (lambda (m)
			      (append (first m)
				      (third m)))
			    meths)
		    (mapcan (lambda (m)
			      (append (second m)
				      (list (fifth m))))
			    meths))
      ,@(mapcon (lambda (rest)
		  (mapcar (lambda (arg)
			    `(unless (,op ,(car rest) ,arg)
			      (rotatef ,(car rest) ,arg)))
			  (cdr rest)))
		temps)
      ,@(mapcar #'fourth meths))))
