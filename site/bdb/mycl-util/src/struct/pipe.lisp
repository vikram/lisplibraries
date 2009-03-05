;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Pipes

(defmacro make-pipe (head tail)
  `(cons ,head (delay ,tail)))

(defun head (pipe) (first pipe))
(defun tail (pipe) (force (rest pipe)))

(defconstant empty-pipe nil)

(defun pipe-null-p (p)
  (or (eql p empty-pipe)
      (null p)))

(defun pipe-elt (pipe i)
  (if (= i 0)
      (head pipe)
      (pipe-elt (tail pipe) (1- i))))

(defun pipe-map (fn pipe)
  (if (pipe-null-p pipe)
      empty-pipe
      (make-pipe (funcall fn (head pipe))
		 (pipe-map fn (tail pipe)))))

(defun pipe-mapc (fn pipe)
  (unless (pipe-null-p pipe)
    (funcall fn (head pipe))
    (pipe-mapc fn (tail pipe))))

(defun pipe-mappend (fn pipe)
  (pipe-reduce #'pipe-append empty-pipe (pipe-map fn pipe)))

(defun pipe->list (pipe)
  (labels ((force (p)
	     (if (pipe-null-p p)
		 empty-pipe
		 (cons (head p)
		       (force (tail p))))))
    (force pipe)))

(defun pipe-filter (pred pipe)
  (cond ((pipe-null-p pipe) empty-pipe)
	((funcall pred (head pipe))
	 (make-pipe (head pipe)
		    (pipe-filter pred
				 (tail pipe))))
	(t (pipe-filter pred (tail pipe)))))

(defun pipe-reduce (fn s pipe)
  (if (pipe-null-p pipe)
      s
      (pipe-reduce fn
		   (funcall fn s (head pipe))
		   (tail pipe))))

(defun pipe-append (p1 p2)
  (cond ((pipe-null-p p1) p2)
	((pipe-null-p p2) p1)
	 (t (make-pipe (head p1)
		       (pipe-append (tail p1)
				    p2)))))

(defun pipe-permutations (p1)
  (if (pipe-null-p p1)
      (make-pipe nil empty-pipe)
      (pipe-mappend (lambda (e)
		      (pipe-map (lambda (p) (make-pipe e p))
				(pipe-permutations
				 (pipe-filter (lambda (f)
						(not (eq e f)))
					      p1))))
		    p1)))

(defun list->pipe (lst)
  (make-pipe (car lst)
	     (cdr lst)))

(defun integers (&optional (start 0) end)
  (if (or (null end)
	  (<= start end))
      (make-pipe start (integers (1+ start) end))
      nil))
