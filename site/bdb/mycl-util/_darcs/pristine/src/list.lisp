;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;;
(in-package :mycl-util)

;;;; ** List functions

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  "returns the last element in list"
  (car (last lst)))

(defun single (lst)
  "predicate which returns true, if list has exactly one element"
  (and (consp lst)
       (not (cdr lst))))

(defun append1 (lst obj)
  "appends a new element to a list"
  (append lst (list obj)))

(defun nconc1 (lst obj)
  "destrucively appends a new element to a list"
  (nconc lst (list obj)))

(defun mklist (obj)
  "if not already a list, mklist will return a 
   new list with its param as element"
  (if (listp obj)
      obj
      (list obj)))

(defun longer (x y)
  "returns t if length of 1st list is longer than length of the 2nd one"
  (labels ((compare (x y)
	     (and (consp x)
		  (or (null y)
		      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
	(> (length x) (length y)))))

;(defun filter (fn lst)
;  "filters elements according to fn"
;  (let ((acc nil))
;    (dolist (x lst)
;      (let ((val (funcall fn x)))
;	(when val (push x acc))))
;    (nreverse acc)))

(setf (symbol-function 'filter) #'remove-if-not)

(defun group (source n)
  "groups every n elements together into new sublists.
   e.g. (group '(1 a 2 b) 2) -> ((1 a) (2 b))"
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source
	(rec source nil)
	nil)))

(defun find2 (fn lst)
  "finds the element and place in list where fn will not return nil"
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
	(if val
	    (values (car lst) val)
	    (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "tests if x comes before y in the specified list"
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "tests if x comes after y in the lst"
  (let ((rest (before y x lst :test test)))
    (and rest
	 (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  "test, if obj is a duplicate in lst"
  (member obj (cdr (member obj lst :test test))
	  :test test))

(defun split-if (fn lst)
  "splits the lst into 2 lists at the position where fn returns not nil"
  (let ((acc nil))
    (do ((src lst (cdr lst)))
	((or (null src)
	     (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst &key (test #'>))
  "finds the 'highest' value in lst according to fn. 
   Returns obj and score"
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj lst)
	  (let ((score (funcall fn obj)))
	    (when (funcall test score max)
	      (setq wins obj
		    max score))))
	(values wins max))))

(defun best (fn lst)
  "finds the 'best' object in lst according to fn"
  (if (null lst)
      nil
      (let ((wins (car lst)))
	(dolist (obj (cdr lst))
	  (if (funcall fn obj wins)
	      (setq wins obj)))
	wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
	    (max (funcall fn (car lst))))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (cond ((> score max)
		   (setq max score
			 result (list obj)))
		  ((= score max) (push obj result)))))
	(values (nreverse result) max))))

(defun map-> (fn start test-fn succ-fn)
  "maps all objects between start and test-fn with fn."
  (do ((i start (funcall succ-fn start))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mapa-b (fn a b &optional (step 1))
  "maps numbers from a to b"
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map1-n (fn n)
  "maps numbers from 1 to n"
  (mapa-b fn 1 n))

(defun map0-n (fn n)
  "maps numbers from 0 to n"
  (mapa-b fn 0 n))

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
	(push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  "recursively maps all objs in list and its sublists"
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
	     (lambda (&rest args)
		 (apply #'rmapcar fn args))
	     args)))

(defun cross-lists (fn xlist ylist)
  (mappend (lambda (y)
	     (mapcar (lambda (x)
		       (funcall fn x y))
		     xlist))
	   ylist))

(defun combine-all (xlist ylist)
  (cross-lists #'append xlist ylist))

(defun starts-with (list x &key (test #'eql))
  (and (consp list)
       (funcall test (car list) x)))

(defun permutations (lst)
  (if (null lst)
      (list nil)
      (mapcan (lambda (e)
		(mapcar (lambda (p) (cons e p))
			(permutations (remove e lst :count 1 :test #'eq))))
	      lst)))

(defun parition (pred list)
  (let ((yes-list nil)
	(no-list nil))
    (dolist (item list)
      (if (funcall pred item)
	  (push item yes-list)
	  (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun prepend (x y)
  (append y x))

(defun reuse-cons (x y x-y &key (test #'eql))
  (if (and (funcall test x (car x-y))
	   (funcall test y (cdr x-y)))
      x-y
      (cons x y)))

(defun find-all (item sequ &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequ
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequ
	     :test (complement test) keyword-args)))

(defun find-all-if (pred sequ &rest keyword-args &key &allow-other-keys)
  (apply #'remove-if (complement pred) sequ keyword-args))

(defun self-cons (x) (cons x x))
