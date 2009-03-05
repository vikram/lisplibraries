;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          queue.lisp
;;;; Purpose:       Various queue implementations with generic func interface.
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  September 2004

(in-package :utils)

;;;; Top level Queue creation

(defmacro-exported create-queue (&optional init-data &key (type 'list-queue))
  (let ((inst (gensym)))
    `(let ((,inst (make-instance ',type)))
       (initialize ,inst ,init-data))))

;;--------------------------------
;; List Queue Class
;;--------------------------------


(defclass-exported list-queue ()
  ((size :accessor list-queue-size)
   (head :accessor list-queue-head))
  (:documentation
   "Queue object, contains the head element and some stats"))

(defmethod initialize ((q list-queue) &rest rest)
  ;; NOTE: allow initialization from data
  (when (and (consp rest) (not (null (car rest)))) (error "No initialization allowed for type list-queue"))
  (setf (slot-value q 'head) nil)
  (setf (slot-value q 'size) 0)
  q)
    
(defclass-exported list-queue-elt ()
  ((pred :initarg :pred :accessor lqe-pred)
   (succ :initarg :succ :accessor lqe-succ)
   (data :initarg :data :accessor lqe-data))
  (:documentation
   "An element forming a queue, the provided element
    is treated as a head for push/pop operations"))

(defmethod-exported list-queue-empty-p (queue)
  (eq (list-queue-size queue) 0))

(eval-when (compile eval load)
  (export (list 'list-queue-size 'list-queue-empty-p 'list-queue-head)))


;; List interface
(defmethod-exported list-remove ((qe list-queue-elt))
  "Remove qe from its list"
  (with-slots (pred succ) qe
    (setf (lqe-succ pred) succ)
    (setf (lqe-pred succ) pred))
  qe)

(defmethod-exported list-insert ((qe (eql nil)) (new list-queue-elt))
  (with-slots (pred succ) new
    (setf pred new
	  succ new)
  new))

(defmethod-exported list-insert ((qe list-queue-elt) (new list-queue-elt))
  "Insert new in front of qe in the list"
  (with-slots (pred succ) qe
    (setf (lqe-succ new) qe
	  (lqe-pred new) pred
          (lqe-succ pred) new
	  pred new))
  new)


(defmethod-exported search-for-entry ((q list-queue-elt) item &key key (test #'eq))
  (if (funcall test (lqe-data q) (if key (funcall key item) item))
      q
    (search-for-entry (lqe-succ q) item :key key :test test)))


;; Queue Interface

(defmethod-exported peek ((q list-queue) &key (location :top))
  (with-slots (head) q
    (when head
      (ecase location
	(:top (lqe-data head))
	(:bottom (lqe-data (lqe-pred head)))))))

(defmethod-exported enqueue ((q list-queue) datum)
  (with-slots (head size) q
    (let ((new (make-instance 'list-queue-elt
			      :pred nil
			      :succ nil
			      :data datum)))
	(if (= size 0)
	    (with-slots (pred succ) new
	      (setf pred new)
	      (setf succ new)))
	(setf head (list-insert head new))
	(incf size))
    (lqe-data head)))
  

(defmethod-exported dequeue ((q list-queue))
  (with-slots (head size) q
    (unless (= size 0)
      (let ((record (list-remove (lqe-pred head))))
	(if (= size 1)
	    (setf head nil))
	(when record
	  (decf size)
	  (assert (>= size 0))
	  (lqe-data record))))))
	

(defmethod-exported pred ((q list-queue) &optional item)
  (with-slots (head) q
      (if item
	  (lqe-data (lqe-pred (search-for-entry head item)))
	(lqe-data (lqe-pred head)))))

(defmethod-exported succ ((q list-queue) &optional item)
  (with-slots (head) q
      (if item
	  (lqe-data (lqe-succ (search-for-entry head item)))
	(lqe-data (lqe-succ head)))))

;; Miscellaneous useful utilities

(defmethod-exported queue-element-to-front ((queue list-queue) element)
  (list-remove element)
  (list-insert (list-queue-head queue) element)
  (setf (list-queue-head queue) element))

(defmethod-exported map-queue ((q list-queue) fn &key reverse)
  "Traverse the queue from most to least recent push collecting
   the results of applying fn to the data in each queue position"
  (with-slots (head size) q
    (if (null head)
	nil
      (let ((next (if reverse #'lqe-pred #'lqe-succ))
	    (start (if reverse (lqe-pred head) head)))
	(loop for n from 1 to size
	  collecting (funcall fn (lqe-data start)) into list 
	  finally (return list) do
	  (setf start (funcall next start)))))))
