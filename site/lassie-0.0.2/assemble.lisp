(in-package :lassie.assembler)

;;; Interface

(defgeneric assemble-co-occurrence-matrix (assembler lister)
  (:documentation "Assemble MATRIX and remember how to perform the
same kind of activity on subsequent calls to ASSEMBLE-TERM-VECTOR and
ASSEMBLE-DOCUMENT-VECTOR."))

(defgeneric assemble-term-vector (assembler lister)
  (:documentation "Iterate over documents of LISTER and assemble a
term vector in the same way as the matrix was assembled previously."))

(defgeneric assemble-document-vector (assembler lister)
  (:documentation "Iterate over terms of LISTER and assemble a
document vector in the same way as the matrix was assembled
previously."))

;;;

(defclass lsa-assembler ()
  ((n-terms :initarg :n-terms :reader n-terms)
   (n-documents :initarg :n-documents :reader n-documents))
  (:documentation "The standard assembler that adds ..."))

(defun make-lsa-assembler (&key n-terms n-documents)
  (make-instance 'lsa-assembler :n-terms n-terms :n-documents n-documents))

(defmethod print-object :around ((assembler lsa-assembler) stream)
  (if *print-readably*
      (format stream "#.~S"
              `(make-lsa-assembler :n-terms ,(n-terms assembler)
                :n-documents ,(n-documents assembler)))
      (call-next-method)))

(defun incf-and-maybe-grow (matrix delta &rest indices)
  (assert (adjustable-array-p matrix))
  (unless (every #'< indices (array-dimensions matrix))
    (adjust-array matrix
                  (mapcar (lambda (i d)
                            (if (< i d)
                                d
                                (print (max (1+ i) (* 2 d)))))
                          indices (array-dimensions matrix))
                  :initial-element 0.0))
  (incf (apply #'aref matrix indices) delta))

(defmethod assemble-co-occurrence-matrix ((assembler lsa-assembler) lister)
  (let ((matrix (make-array '(0 0) :element-type 'single-float
                            :initial-element 0.0 :adjustable t))
        (max-row -1)
        (max-column -1))
    (funcall lister (lambda (row column &optional (delta 1.0))
                      (setf max-row (max row max-row))
                      (setf max-column (max column max-column))
                      (incf-and-maybe-grow matrix delta row column)))
    (setf (slot-value assembler 'n-terms) (1+ max-row))
    (setf (slot-value assembler 'n-documents) (1+ max-column))
    ;; We are going to work with this matrix a lot, make a new one
    ;; that's not adjustable.
    (let ((m (make-array (list (1+ max-row) (1+ max-column))
                         :element-type 'single-float)))
      (dotimes (row (1+ max-row))
        (dotimes (column (1+ max-column))
          (setf (aref m row column) (aref matrix row column))))
      m)))

(defun assemble-occurence-vector (lister size)
  "Return a vector of SIZE whose elements represent the frequency with
which their indices were listed by LISTER."
  (let ((v (make-array size :element-type 'single-float :initial-element 0.0)))
    (funcall lister (lambda (index &optional (delta 1.0))
                      (incf (aref v index) delta)))
    v))

(defmethod assemble-term-vector ((assembler lsa-assembler) lister)
  (declare (ignore lister))
  (error "Don't know how to assemble term vectors. Giving up."))

(defmethod assemble-document-vector ((assembler lsa-assembler) lister)
  (assemble-occurence-vector lister (n-terms assembler)))

;;;

(defclass ri-term-assembler (lsa-assembler)
  ()
  (:documentation "Terms are random indexed, documents are not."))

(defun make-ri-term-assembler ()
  (make-instance 'ri-term-assembler))

(defmethod print-object :around ((assembler ri-term-assembler) stream)
  (if *print-readably*
      (format stream "#.~S"
              `(make-instance 'ri-term-assembler :n-terms ,(n-terms assembler)
                :n-documents ,(n-documents assembler)))
      (call-next-method)))

(defmethod assemble-co-occurrence-matrix ((assembler ri-term-assembler) lister)
  (call-next-method
   assembler
   (lassie::compose-mappers
    lister
    (lambda (fn rows column)
      (loop with l = (/ (length rows) 2)
            for i below (* l 2)
            for weight = (if (< i l) 1.0 -1.0)
            do (funcall fn (aref rows i) column weight))))))

(defmethod assemble-document-vector ((assembler ri-term-assembler) lister)
  (assemble-occurence-vector
   (lassie::compose-mappers
    lister
    (lambda (fn rows)
      (loop with l = (/ (length rows) 2)
            for i below (* l 2)
            for weight = (if (< i l) 1.0 -1.0)
            do (funcall fn (aref rows i) weight))))
   (n-terms assembler)))
