;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Unspecified structures

(defun queue-contents (q) (cdr q))

(defun make-queue ()
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  (setf (car q)
	(setf (rest (car q))
	      (cons item nil))))

(defun dequeue (q)
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)

(defun front (q)
  (first (queue-contents q)))

(defun in-queue (item q &key (key #'identity) (test #'eql))
  (member item (queue-contents q) :key key :test test))

(defun empty-queue-p (q)
  (null (queue-contents q)))

(defun queue-nconc (q list)
  (setf (car q)
	(last (setf (rest (car q)) list))))

;;tries
(defstruct trie (value nil) (arcs nil))

(defconstant trie-deleted "deleted")

(defun put-trie (key trie value)
  (setf (trie-value (find-trie key t trie)) value))

(defun get-trie (key trie)
  (let* ((key-trie (find-trie key nil trie))
	 (val (if key-trie (trie-value key-trie))))
    (if (or (null key-trie)
	    (eq val trie-deleted))
	(values nil nil)
	(values val t))))

(defun delete-trie (key trie)
  (put-trie key trie trie-deleted))

(defun find-trie (key extend? trie)
  (cond ((null trie) nil)
	((atom key)
	 (follow-arc key extend? trie))
	(t (find-trie (cdr key) extend?
		      (find-trie (car key) extend?
				 (find-trie "." extend? trie))))))

(defun follow-arc (component extend? trie)
  (let ((arc (assoc component (trie-arcs trie))))
    (cond ((not (null arc)) (cdr arc))
	  ((not extend?) nil)
	  (t (let ((new-trie (make-trie)))
	       (push (cons component new-trie)
		     (trie-arcs trie))
	       new-trie)))))

;; stack machina

(defclass stack-machine ()
  ((stack :accessor stack :initform (list))))

(defclass state () ())

(defmethod current-state ((m stack-machine)) (first (stack m)))
(defmethod push-state ((s state) (m stack-machine)) (push s (stack m)))
(defmethod pop-state ((m stack-machine)) (pop (stack m)))

(defmethod upd-current-state (fun (m stack-machine))
  (push-state (funcall fun (pop-state m)) m))

(defmethod upd-by-n-states (n fun (m stack-machine))
  (let ((states (loop for i from n above 0
		      for state = (pop-state m)
		      collect state)))
    (push-state (apply fun states))))