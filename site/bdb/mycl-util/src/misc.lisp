;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Unspecified helper functions

(defun const (a)
  (lambda (&rest args)
    (declare (ignore args))
    a))

(defun const-values (&rest a)
  (lambda (&rest args)
    (declare (ignore args))
    (apply #'values a)))

(defun mkstr (&rest args)
  "writes args into a string and returns that string"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "creates a new symbol from args"
  (values (intern (apply #'mkstr args))))

(defun anon-symb (&rest args)
  "like symb, but doesn't intern new symbol"
  (make-symbol (apply #'mkstr args)))

(defun reread (&rest args)
  (values (read-from-string
	   (apply #'mkstr args))))

(defun one-of (list)
  "randomly choose one element from list"
  (list (random-elt list)))

(defun random-elt (list)
  (elt list
       (random (length list))))

(defun array-shift-left (array &key (start 0) (end (1- (length array)))
			 (replace nil))
  "- shifts all elements between start and end to the left
   - leftmost element (at :start) is removed from array
   - inserts a new value at end"
  (loop for i from (1+ start) to end
	for j from start
	do (setf (aref array j)
		 (aref array i))
	finally (progn (setf (aref array end) replace)
		       (return array))))

(defun array-shift-right (array &key (start 0) (end (1- (length array)))
			  (replace nil))
  "- shifts all elements between start and end to the right
   - rightmost element (at :end) is removed from array
   - inserts a new value at start"
  (loop for i from (1- end) downto start
	for j from end downto start
	do (setf (aref array j)
		 (aref array i))
	finally (progn (setf (aref array start) replace)
		       (return array))))

(defun vector-search (key vector &key (key-fn #'identity) (start 0)
		      (end (1- (length vector)))
		      (comparator (number-comparator)))
  "- uses binary search to find the index of key in vector.
   - vector must be sorted
   - returns : (nil index) if key not found and search stoped at index
               (index index) if key was found"
  (let* ((mid (round (+ start end) 2))
	 (k (funcall key-fn (aref vector mid))))
    (cond ((comp-= key k comparator) (values mid mid))
	  ((<= end start) (values nil start))
	  (t (let ((left (if (comp-< key k comparator)
			     start
			     (if (= start mid) end mid)))
		   (right (if (comp-> key k comparator)
			      end
			      (if (= end mid) start mid))))
	       (vector-search key vector :key-fn key-fn
			      :start left :end right
			      :comparator comparator))))))
