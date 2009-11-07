(in-package #:anardb.test)

(defconstant +chunk-array-length+ 1000)

(defdbclass chunk ()
  ((list)
   (array :type (simple-array float) :initform (make-array +chunk-array-length+ 
							  :element-type 'float :initial-element 0.0))))

(defdbclass linked-list ()
  ((next) 
   (prev) 
   (name :index :any :initform "no name")
   (value :index :any :initform nil)
   (chunk :initform (make-instance 'chunk))))

(defmethod drop :before ((chunk chunk))
  (setf (chunk-list chunk) nil))

(defmethod drop :before ((linked-list linked-list))
  (with-slots (prev next chunk)
      linked-list
    (setf (linked-list-next prev) next)
    (setf (linked-list-prev next) prev)
    (drop chunk)))


(defmethod initialize-instance :after ((linked-list linked-list) &rest initargs)
  (unless (slot-boundp linked-list 'next)
    (setf (linked-list-next linked-list) linked-list))
  (unless (slot-boundp linked-list 'prev)
    (setf (slot-value linked-list 'prev) (linked-list-next linked-list)))
  (setf (chunk-list (linked-list-chunk linked-list)) linked-list)
  linked-list)


