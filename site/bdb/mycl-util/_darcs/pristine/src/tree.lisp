;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Tree functions

(defun find-anywhere (item tree)
  (cond ((eql item tree) tree)
	((atom tree) nil)
	((find-anywhere item (first tree)))
	((find-anywhere item (rest tree)))))

(defun find-anywhere-if (pred tree)
  (cond ((funcall pred tree) tree)
	((atom tree) nil)
	((find-anywhere-if pred (first tree)))
	((find-anywhere-if pred (rest tree)))))

(defun tree-walk (fn tree)
  (cond ((atom tree) (funcall fn tree))
	(t (tree-walk fn (first tree))
	   (tree-walk fn (rest tree)))))

(defun prune (test tree)
  (labels ((rec (tree acc)
	     (cond ((null tree) (nreverse acc))
		   ((consp (car tree))
		    (rec (cdr tree)
			 (cons (rec (car tree) nil) acc)))
		   (t (rec (cdr tree)
			   (if (funcall test (car tree))
			       acc
			       (cons (car tree) acc)))))))
    (rec tree nil)))

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun tree-search (states goal-p successors combiner)
  (cond ((null states) nil)
	((funcall goal-p (first states)) (first states))
	(t (tree-search
	    (funcall combiner
		     (funcall successors (first states))
		     (rest states))
	    goal-p successors combiner))))

(defun tree-depth-first-search (start goal-p successors)
  (tree-search (list start) goal-p successors #'append))

(defun tree-breath-first-search (start goal-p successors)
  (tree-search (list start) goal-p successors #'prepend))

(defun tree-best-first-search (start goal-p successors cost-fn)
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun sorter (cost-fn)
  (lambda (new old)
    (merge 'list (sort new #'<) old #'< :key cost-fn)))

(defun tree-beam-search (start goal-p successors cost-fn beam-width)
  (tree-search (list start) goal-p successors
	       (lambda (old new)
		 (let ((sorted (funcall (sorter cost-fn) old new)))
		   (if (> beam-width (length sorted))
		       sorted
		       (subseq sorted 0 beam-width))))))

(defun tree-iter-wide-search (start goal-p successors cost-fn
			      &key (width 1) (max 100))
  (unless (> width max)
    (or (tree-beam-search start goal-p successors cost-fn width)
	(tree-iter-wide-search start goal-p successors cost-fn
			  :width (+ width 1) :max max))))
