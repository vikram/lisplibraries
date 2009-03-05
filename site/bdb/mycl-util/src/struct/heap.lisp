;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Heap structure

(defstruct heap
  (array (make-array 5 :adjustable t))
  (size 0)
  (key-fn #'identity)
  (value-fn #'identity)
  (array-increment 3/4) 
  (comparator (number-comparator)))

(defun parent-index (i)
  (floor (- i 1) 2))

(defun compute-size (c heap)
  (floor (* c
	    (+ 1 (heap-array-increment heap)))))

(defun adjust-heap-array (size heap)
  (when (< (length (heap-array heap)) size)
    (setf (heap-array heap)
	  (adjust-array (heap-array heap)
			(compute-size size heap)))))

(defun heap-insert (item heap)
  (with-comparator (heap-comparator heap)
    (labels ((insert-at (pos)
	       (let ((p (parent-index pos))
		     (key (heap-key-fn heap)))
		 (if (and (> pos 0)
			  (comp-> (funcall key (aref (heap-array heap) p))
				  (funcall key item)))
		     (progn (setf (aref (heap-array heap) pos)
				  (aref (heap-array heap) p))
			    (insert-at p))
		     (setf (aref (heap-array heap) pos)
			   item)))))
      (adjust-heap-array (incf (heap-size heap)) heap)
      (insert-at (1- (heap-size heap))))))

(defun heap-empty-p (heap)
  (>= 0 (heap-size heap)))

(defun heap-front (heap)
  (if (heap-empty-p heap)
      (error "heap is empty")
      (funcall (heap-value-fn heap)
	       (aref (heap-array heap) 0))))

(defun heap-dequeue (heap)
  (unless (heap-empty-p heap)
    (let ((save (heap-front heap)))
      (setf (aref (heap-array heap) 0)
	    (aref (heap-array heap) (decf (heap-size heap))))
      ;;allow removed for begin garbage collected
      (setf (aref (heap-array heap) (heap-size heap)) 0)
      (heapify (heap-array heap) 0 (heap-size heap) (heap-key-fn heap)
		 (heap-comparator heap))
      save)))

(defun heapify (array node size key comp)
  (labels ((item-key (i)
	     (funcall key (aref array i))))
    (let ((left (+ 1 (* 2 node)))
	  (right (+ 2 (* 2 node))))
      (condlet (((and (< left size)
		      (comp-< (item-key left) (item-key node) comp)
		      (or (>= right size)
			  (comp-< (item-key left) (item-key right) comp)))
		 (smallest left))
		((and (< right size)
		      (comp-< (item-key right) (item-key node) comp))
		 (smallest right)))
	(array-swap array node smallest)
	(heapify array smallest size key comp))
      (values))))

(defun array-swap (array i1 i2)
  (let ((tmp (aref array i1)))
    (setf (aref array i1) (aref array i2)
	  (aref array i2) tmp)))

(defun is-in-heap (item heap)
  (with-comparator (heap-comparator heap)
    (loop for i below (heap-size heap)
	  for elem across (heap-array heap)
	  with key-fn = (heap-key-fn heap)
	  when (comp-= (funcall key-fn elem)
		       (funcall key-fn item))
	  do (return t)
	  finally (return nil))))
