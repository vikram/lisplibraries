(in-package :lassie)

(defun insert-into-sorted-vector (item vec &key
                                  (max-length (array-total-size vec))
                                  key
                                  (test #'>))
  "Insert ITEM into VECTOR while keeping it sorted by TEST. Extend the
vector if needed while respecting MAX-LENGTH"
  (let* ((len (length vec))
         (pos (or (position (if key (funcall key item) item)
                            vec :key key :test test)
                  len)))
    (when (< pos max-length)
      (when (< len max-length)
        (vector-push-extend nil vec))
      (replace vec vec :start1 (1+ pos) :start2 pos :end2 len)
      (setf (aref vec pos) item))
    vec))

(defun cosine-similarity (x y &key (n (length x)))
  (declare (type single-float-vector x y)
           (type fixnum n))
  (let ((sum1 0.0)
        (sum2 0.0)
        (sum3 0.0))
    (declare (type (single-float 0.0) sum1 sum2 sum3))
    (locally
        (declare (optimize (speed 3)))
      (loop for i below n do
            (let ((a (aref x i))
                  (b (aref y i)))
              (incf sum1 (* a b))
              (incf sum2 (* a a))
              (incf sum3 (* b b)))))
    (setq sum2 (sqrt sum2))
    (setq sum3 (sqrt sum3))
    (if (or (zerop sum2) (zerop sum3))
        0.0
        (/ sum1 sum2 sum3))))

(defun most-similar-documents (lsa document-features
                               &key (n 10) (test #'>) filter
                               (measure #'cosine-similarity))
  "Return a vector of index and similarity pairs of the - at most N -
documents whose features are most similar to DOCUMENT-FEATURES
according to the similarity MEASURE."
  (declare (type single-float-vector document-features)
           (type integer n)
           (optimize (speed 3)))
  (let* ((svd (the fsvd:svd (svd lsa)))
         (n-svs (length svd))
         (filter (if filter (coerce filter 'function) nil))
         (measure (coerce measure 'function)))
    (assert (not (zerop n-svs)))
    (assert (= (length document-features) n-svs))
    (let ((n-documents (length (fsvd:sv-right (aref svd 0))))
          (v (make-array n :fill-pointer 0))
          (features (make-array n-svs :element-type 'single-float)))
      (loop for i below n-documents do
            (when (or (null filter) (funcall filter i))
              (loop for j below n-svs do
                    (setf (aref features j)
                          (aref (fsvd:sv-right (aref svd j)) i)))
              (let ((similarity (funcall measure document-features features)))
                (insert-into-sorted-vector (cons i similarity) v
                                           :test test :key #'cdr))))
      v)))
