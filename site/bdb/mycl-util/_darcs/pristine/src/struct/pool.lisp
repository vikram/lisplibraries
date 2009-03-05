;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Resource pools

;;by changing store, add-strategie and next-strategie you are able to
;;change behavior of a pool :)
(defstruct pool
  (count 0)
  (store (make-queue))
  (add-strategie #'enqueue)
  (next-strategie (lambda (q)
		    (let ((data (front q)))
		      (dequeue q)
		      (values data
			      t)))))

(defun empty-pool-p (pool)
  (= (pool-count pool) 0))

(defun pool-add (obj pool)
  "returns the number of elements of pool or nil if obj wasn't added to pool"
  (if (call-add-strategie obj pool)
      (incf (pool-count pool))
      nil))

(defun call-add-strategie (obj pool)
  (funcall (pool-add-strategie pool)
	   obj (pool-store pool)))

(defun pool-next (pool)
  "returns the next object from the pool"
  (unless (empty-pool-p pool)
    (multiple-value-bind (obj removed)
	(call-next-strategie pool)
      (when removed
	(decf (pool-count pool)))
      obj)))

(defun call-next-strategie (pool)
  (funcall (pool-next-strategie pool)
	   (pool-store pool)))

;;some special pool types ;)

(defun pool-make-max-sized (max-size pool)
  (let ((old-add-strategie (pool-add-strategie pool)))
    (setf (pool-add-strategie pool)
	  (lambda (obj store)
	    (when (< (pool-count pool) max-size)
	      (funcall old-add-strategie obj store))))
    pool))

(defun make-max-resource-pool (max-size)
  (pool-make-max-sized max-size (make-pool)))

(defun make-priority-pool (&key
			   (key-fn #'identity)
			   (value-fn #'identity)
			   (comparator (number-comparator)))
  (make-pool :store (make-heap :key-fn key-fn
			       :value-fn value-fn
			       :comparator comparator)
	     :add-strategie #'heap-insert
	     :next-strategie (lambda (store)
			       (values (heap-dequeue store)
				       t))))

(defun make-max-sized-priority-pool (max-size
				      &key
				      (key-fn #'identity)
				      (value-fn #'identity)
				      (comparator (number-comparator)))
  (pool-make-max-sized max-size
		       (make-priority-pool :key-fn key-fn
					   :value-fn value-fn
					   :comparator comparator)))
