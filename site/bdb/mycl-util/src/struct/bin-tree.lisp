;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Binary trees

(defgeneric create-node (tree &rest args &key &allow-other-keys))

(defgeneric search-tree (tree key))

(defgeneric search-node (node key))

(defgeneric insert-tree (tree key value))

(defgeneric node-insert (node key value tree))

(defgeneric empty-p (tree))

(defgeneric clear-tree (tree))

(defgeneric delete-tree (tree key))

(defgeneric delete-from-node (node key tree))

(defgeneric tree-root (tree))

(defclass binary-tree ()
  ((root :accessor tree-root :initform nil)
   (node-type :reader node-type :initform 'binary-tree-node :initarg :type)
   (comparator :reader comparator :initform (number-comparator)
	       :initarg :comparator)))

(defclass binary-tree-node ()
  ((key :accessor key :initarg :key)
   (value :accessor value :initarg :value)
   (left :accessor left :initarg :left :initform nil)
   (right :accessor right :initarg :right :initform nil)))

(defmethod left ((node (eql nil))) nil)
(defmethod right ((node (eql nil))) nil)

(defclass avl-tree-node (binary-tree-node)
  ((height :accessor height :initarg :height :initform 0)))

(defun make-binary-tree (&optional (comparator (build-comparator #'= :< #'<)))
  (make-instance 'binary-tree :comparator comparator))

(defun make-avl-tree (&optional (comparator (build-comparator #'= :< #'<)))
  (make-instance 'binary-tree :comparator comparator
		 :type 'avl-tree-node))

;;general implementation
(defmethod create-node (tree &rest args &key &allow-other-keys)
  (apply #'make-instance (node-type tree) args))

(defmethod empty-p (tree)
  (not (tree-root tree)))

(defmethod search-tree (tree key)
  (when tree
    (with-comparator (comparator tree)
      (search-node (tree-root tree) key))))

(defmethod insert-tree (tree key value)
  (when tree
    (with-comparator (comparator tree)
      (setf (tree-root tree)
	    (if (empty-p tree)
		(create-node tree :key key :value value)
		(node-insert (tree-root tree) key value tree)))
      tree)))

(defmethod clear-tree (tree)
  (setf (tree-root tree) nil))

(defmethod delete-tree (tree key)
  (when (and tree (not (empty-p tree)))
    (with-comparator (comparator tree)
      (setf (tree-root tree)
	    (delete-from-node (tree-root tree) key tree)))
    tree))

;;binary-tree-node implementation

(defmethod create-node ((tree binary-tree-node) &rest args
			&key &allow-other-keys)
  (apply #'make-instance 'binary-tree-node args))

(defmethod node-insert ((node (eql nil)) key value tree)
  (create-node tree :key key :value value))

(defmethod search-node (node k)
  (when node
    (with-slots (left right key value) node
      (cond ((comp-= k key) value)
	    ((comp-< k key) (search-node left k))
	    (t (search-node right k))))))

(defmethod node-insert ((node binary-tree-node) k val tree)
  (with-slots (key value left right) node
    (cond ((comp-= k key) (setf value val))
	  ((comp-< k key) (setf left(node-insert left k val tree)))
	  (t (setf right (node-insert right k val tree))))
    node))

(defun replace-with-min (node tree)
  (let ((min-node (find-min-node (right node))))
    (setf (key node) (key min-node)
	  (value node) (value min-node)
	  (right node) (delete-from-node (right node)
					 (key min-node) tree))
    node))

(defmethod delete-from-node ((node binary-tree-node) k tree)
  (with-slots (key left right value) node
    (cond ((comp-= k key)
	   (cond ((and (not left) (not right)) nil)
		 ((not left) right)
		 ((not right) left)
		 (t (replace-with-min node tree))))
	  ((comp-< k key)
	   (setf left (delete-from-node left k tree))
	   node)
	  (t
	   (setf right (delete-from-node right k tree))
	   node))))

(defun find-min-node (node)
  (if-bind left (left node)
	   (find-min-node left)
	   node))

(defun print-tree (tree &optional (stream *standard-output*))
  (if-bind root (tree-root tree)
	   (print-nodes root 0 stream)
	   (format stream "<empty>"))
  (values))

(defun print-nodes (node depth stream)
  (when node
    (with-slots (left right key value) node
      (print-nodes right (+ depth 4) stream)
      (loop for i below depth do (princ " " stream))
      (format stream "+---~a~%" key)
      (print-nodes left (+ depth 4) stream)))
  (values))

;;avl-tree-node implementation

(defmethod create-node ((tree avl-tree-node) &rest args
			&key &allow-other-keys)
  (apply #'make-instance 'avl-tree-node args))

(defmethod node-insert ((node avl-tree-node) k val tree)
  (with-slots (key value left right) node
    (cond ((comp-= k key) (setf value val) node)
	  ((comp-< k key)
	   (setf left (node-insert left k val tree))
	   (apply-right-rotations (adjust-height node)))
	  (t
	   (setf right (node-insert right k val tree))
	   (apply-left-rotations (adjust-height node))))))

(defmethod delete-from-node ((node binary-tree-node) k tree)
  (with-slots (key left right value) node
    (cond ((comp-= k key)
	   (cond ((and (not left) (not right)) nil)
		 ((not left) right)
		 ((not right) left)
		 (t (apply-right-rotations
		     (adjust-height
		      (replace-with-min node tree))))))
	  ((comp-< k key)
	   (setf left (delete-from-node left k tree))
	   (apply-left-rotations (adjust-height node)))
	  (t (setf right (delete-from-node right k tree))
	     (apply-right-rotations (adjust-height node))))))

(defun apply-right-rotations (node)
  (when node
    (when-bind left (left node)
      (when (> (height-diff node) 1)
	(if (< (node-height (left left)) (node-height (right left)))
	    (setf node (double-rotate-right node))
	    (setf node (single-rotate-right node)))))
    node))

(defun apply-left-rotations (node)
  (when node
    (when-bind right (right node)
      (when (> (height-diff node) 1)
	(if (< (node-height (right right)) (node-height (left right)))
	    (setf node (double-rotate-left node))
	    (setf node (single-rotate-left node)))))
    node))

(defun node-height (n)
  (if n (height n) -1))

(defun height-diff (node)
  (abs (- (node-height (left node))
	  (node-height (right node)))))

(defun adjust-height (node)
  (when node
    (with-slots (left right height) node
      (setf height
	    (1+ (max (if left (height left) -1)
		     (if right (height right) -1))))
      node)))

(defun single-rotate-left (node)
  (let ((right (right node)))
    (setf (right node) (left right)
	  (left right) node)
    (adjust-height node)
    (adjust-height right)
    right))

(defun single-rotate-right (node)
  (let ((left (left node)))
    (setf (left node) (right left)
	  (right left) node)
    (adjust-height node)
    (adjust-height left)
    left))

(defun double-rotate-left (node)
  (with-slots (right) node
    (setf right (single-rotate-right right))
    (single-rotate-left node)))

(defun double-rotate-right (node)
  (with-slots (left) node
    (setf left (single-rotate-left left))
    (single-rotate-right node)))
