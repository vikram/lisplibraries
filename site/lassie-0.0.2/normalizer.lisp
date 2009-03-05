(in-package :lassie.assembler)

;;; Interface

(defgeneric normalize-matrix (normalizer matrix)
  (:documentation "Return the normalized MATRIX possibly destructively
and remember how to perform the same kind normalizations on subsequent
calls to NORMALIZE-TERM and NORMALIZE-DOCUMENT."))

(defgeneric normalize-term-vector (normalizer term-vector term)
  (:documentation "Return the normalized TERM-VECTOR. Possibly
desctructive."))

(defgeneric normalize-document-vector (normalizer document-vector document)
  (:documentation "Returned the normalized DOCUMENT-VECTOR. Possibly
desctructive."))

;;; Utilities

(defun sum-vector (vector)
  (loop for i below (length vector)
        sum (aref vector i)))

(defun n-rows (matrix)
  (array-dimension matrix 0))

(defun n-columns (matrix)
  (array-dimension matrix 1))

(defun sum-row (matrix row)
  (loop for column below (n-columns matrix)
        sum (aref matrix row column)))

(defun sum-column (matrix column)
  (loop for row below (n-rows matrix)
        sum (aref matrix row column)))

(defun sum-matrix (matrix)
  (loop for i below (array-total-size matrix)
        sum (row-major-aref matrix i)))

(defun norm (vector &key (power 2))
  (expt (loop for x across vector summing (expt (abs x) power))
        (/ power)))

(defun column-norm (matrix column &key (power 2))
  (expt (loop for row below (array-dimension matrix 0)
              summing (expt (abs (aref matrix row column)) power))
        (/ power)))

(defun normalize-vector (vector &key (power 2))
  (let ((norm (norm vector :power power)))
    (map-into vector (lambda (x)
                       (/ x norm))
              vector))
  vector)

(defun normalize-column (matrix column &key (power 2))
  (let ((norm (column-norm matrix column :power power)))
    (loop for row below (array-dimension matrix 0) do
          (setf (aref matrix row column) (/ (aref matrix row column) norm))))
  matrix)

(defun row-average (matrix row)
  (let ((sum 0.0)
        (n 0))
    (loop for column below (array-dimension matrix 1) do
          (incf sum (aref matrix row column))
          (incf n))
    (/ sum n)))

;;; TF-IDF normalizer

(defun term-frequency (matrix term document)
  "A normalized measure of how often TERM appears in DOCUMENT."
  (/ (aref matrix term document)
     (sum-column matrix term)))

(defun n-documents-with-term (matrix term)
  (loop for document below (n-columns matrix)
        count (not (zerop (aref matrix term document)))))

(defun inverse-document-frequency (matrix term)
  "Relative importance of TERM across MATRIX."
  (log (/ (n-columns matrix)
          (n-documents-with-term matrix term))))

(defclass tf-idf-normalizer ()
  ((idfs :initarg :idfs :reader idfs
         :documentation "The inverse document frequencies in the
originally assembled matrix.")))

(defun make-tf-idf-normalizer (&key idfs)
  (make-instance 'tf-idf-normalizer :idfs idfs))

(defmethod print-object :around ((normalizer tf-idf-normalizer) stream)
  (if *print-readably*
      (format stream "#.~S"
              `(make-tf-idf-normalizer :idfs ,(idfs normalizer)))
      (call-next-method)))

(defmethod normalize-matrix ((normalizer tf-idf-normalizer) matrix)
  (let* ((n-terms (n-rows matrix))
         (n-documents (n-columns matrix))
         (idfs (make-array n-terms)))
    (loop for term below n-terms do
          (setf (aref idfs term)
                (inverse-document-frequency matrix term)))
    (loop for document below n-documents do
          ;; We are updating MATRIX in place so SUM-DOCUMENT would
          ;; return the wrong answer if called on the changed document
          ;; column. Remember the right value and use to calculate the
          ;; TERM-FREQUENCY.
          (let ((sum-document (sum-column matrix document)))
            (loop for term below n-terms do
                  (let ((tf (/
                             ;; This value is yet unchanged.
                             (aref matrix term document)
                             sum-document))
                        (idf (aref idfs term)))
                    (setf (aref matrix term document)
                          (* tf idf))))))
    (setf (slot-value normalizer 'idfs) idfs)
    matrix))

(defmethod normalize-document-vector ((normalizer tf-idf-normalizer)
                                      document-vector document)
  (declare (ignore document))
  (let ((idfs (idfs normalizer))
        (sum-document (sum-vector document-vector)))
    (assert (= (length document-vector) (length idfs)))
    (loop for term below (length document-vector) do
          (let ((tf (/
                     ;; This value is yet unchanged.
                     (aref document-vector term)
                     sum-document))
                (idf (aref idfs term)))
            (setf (aref document-vector term)
                  (* tf idf)))))
  document-vector)

(defun test-tf-idf-normalizer ()
  (let* ((normalizer (make-instance 'tf-idf-normalizer))
         (matrix (normalize-matrix
                  normalizer
                  (make-array '(2 5)
                              :initial-contents '((4 3 2 3 0)
                                                  (1 0 0 2 1))))))
    (assert (> 0.01
               (abs (- (* 4/5 (log 5/4))
                       (aref matrix 0 0)))))
    (assert (> 0.01
               (abs (- (* 3/5 (log 5/4))
                       (aref matrix 0 3)))))
    (let ((doc (normalize-document-vector normalizer (vector 2 1) nil)))
      (assert (> 0.01
                 (abs (- (* 2/3 (log 5/4))
                         (aref doc 0)))))
      (assert (> 0.01
                 (abs (- (* 1/3 (log 5/3))
                         (aref doc 1))))))))

;;(test-tf-idf-normalizer)

;;; Pointwise mutual information normalizer

(defclass pmi-normalizer ()
  ((document-class-fn :initform (error "document-class-fn is required.")
                      :initarg :document-class-fn :reader document-class-fn)
   (term-total :initarg :term-total :reader term-total)
   (term-counts :initarg :term-counts :reader term-counts)
   (class-counts :initarg :class-counts :reader class-counts)
   (term-counts-per-class :initarg :term-counts-per-class
                          :reader term-counts-per-class)
   (term-total-per-class :initarg :term-total-per-class
                         :reader term-total-per-class)
   (n-documents :initarg :n-documents :reader n-documents)))

(defmethod normalize-matrix ((normalizer pmi-normalizer) matrix)
  (let ((term-total 0)
        (term-counts (make-hash-table))
        (class-counts (make-hash-table))
        (term-counts-per-class (make-hash-table))
        (term-total-per-class (make-hash-table))
        (n-terms (n-rows matrix))
        (n-documents (n-columns matrix))
        (document-class-fn (document-class-fn normalizer)))
    ;; Count everything
    (loop for document below n-documents do
          (let* ((document-class (funcall document-class-fn :index document))
                 (term-counts-in-class
                  (or (gethash document-class term-counts-per-class)
                      (setf (gethash document-class term-counts-per-class)
                            (make-hash-table)))))
            (incf (gethash document-class class-counts 0))
            (loop for term below n-terms do
                  (let ((x (aref matrix term document)))
                    (incf term-total x)
                    (incf (gethash term term-counts 0) x)
                    (incf (gethash term term-counts-in-class 0) x)
                    (incf (gethash document-class term-total-per-class 0) x)))))
    ;; Normalize the matrix
    (loop for document below n-documents do
          (let* ((document-class (funcall document-class-fn :index document))
                 (term-counts-in-class
                  (gethash document-class term-counts-per-class))
                 (term-total-in-class
                  (gethash document-class term-total-per-class))
                 (class-frequency (/ (gethash document-class class-counts 0)
                                     n-documents)))
            (loop for term below n-terms do
                  (let* ((term-in-class-frequency
                          (/ (gethash term term-counts-in-class 0)
                             term-total-in-class))
                         (term-frequency
                          (/ (gethash term term-counts 0) term-total))
                         (w (if (zerop term-in-class-frequency)
                                0.0
                                (log (/ term-in-class-frequency
                                        term-frequency class-frequency)))))
                    (setf (aref matrix term document)
                          (* (max 0.0 w)
                             (aref matrix term document)))))))
    (setf (slot-value normalizer 'term-total) term-total)
    (setf (slot-value normalizer 'term-counts) term-counts)
    (setf (slot-value normalizer 'class-counts) class-counts)
    (setf (slot-value normalizer 'term-counts-per-class) term-counts-per-class)
    (setf (slot-value normalizer 'term-total-per-class) term-total-per-class)
    (setf (slot-value normalizer 'n-documents) n-documents)
    matrix))

(defmethod normalize-document-vector ((normalizer pmi-normalizer)
                                      document-vector document)
  #+nil
  (let ((term-total (term-total normalizer))
        (term-counts (term-counts normalizer))
        (class-counts (class-counts normalizer))
        (term-counts-per-class (term-counts-per-class normalizer))
        (term-total-per-class (term-total-per-class normalizer))
        (n-documents (n-documents normalizer))
        (document-class-fn (document-class-fn normalizer)))
    (let* ((document-class (funcall document-class-fn :document document))
           (term-counts-in-class
            (gethash document-class term-counts-per-class))
           (term-total-in-class
            (gethash document-class term-total-per-class))
           (class-frequency (/ (gethash document-class class-counts 0)
                               n-documents)))
      (when document-class
        (loop for term below (length document-vector) do
              (let* ((term-in-class-frequency
                      (/ (gethash term term-counts-in-class 0)
                         term-total-in-class))
                     (term-frequency
                      (/ (gethash term term-counts 0) term-total))
                     (w (if (zerop term-in-class-frequency)
                            0.0
                            (log (/ term-in-class-frequency
                                    term-frequency class-frequency)))))
                (setf (aref document-vector term)
                      (* (if (plusp w) 1.0 0.0) #+nil (max 0.0 w)
                         (aref document-vector term))))))))
  document-vector)

(defun hash-table= (alist hash-table)
  (and (= (length alist)
          (hash-table-count hash-table))
       (every (lambda (x)
                (eql (cdr x) (gethash (car x) hash-table)))
              alist)))

(defun test-pmi-normalizer ()
  (flet ((document-class (&key index document)
           (cond ((eql index 0) :spam)
                 ((eql index 3) :ham)
                 (index :neutral)
                 (t document))))
    (let* ((normalizer (make-instance 'pmi-normalizer
                                      :document-class-fn #'document-class))
           (matrix (normalize-matrix
                    normalizer
                    (make-array '(2 5)
                                :initial-contents '((4 3 2 3 0)
                                                    (1 0 0 2 1))))))
      (assert (= 16 (term-total normalizer)))
      (assert (hash-table= '((0 . 12) (1 . 4))
                           (term-counts normalizer)))
      (assert (hash-table= '((:spam . 1) (:ham . 1) (:neutral . 3))
                           (class-counts normalizer)))
      (assert (hash-table= '((0 . 4) (1 . 1))
                           (gethash :spam (term-counts-per-class normalizer))))
      (assert (hash-table= '((0 . 3) (1 . 2))
                           (gethash :ham (term-counts-per-class normalizer))))
      (assert (hash-table= '((0 . 5) (1 . 1))
                           (gethash :neutral
                                    (term-counts-per-class normalizer))))
      (assert (hash-table= '((:spam . 5) (:ham . 5) (:neutral . 6))
                           (term-total-per-class normalizer)))
      (assert (= 5 (n-documents normalizer)))
      (print matrix)
      (print (normalize-document-vector normalizer (vector 1 1) :neutral)))))

;;(test-pmi-normalizer)

;;;

(defclass sign-normalizer ()
  ())

(defmethod normalize-matrix ((normalizer sign-normalizer) matrix)
  (loop for i below (array-total-size matrix) do
        (let ((x (row-major-aref matrix i)))
          (setf (row-major-aref matrix i)
                (cond ((plusp x) 1.0)
                      ((minusp x) -1.0)
                      (t 0.0)))))
  matrix)

(defmethod normalize-document-vector ((normalizer sign-normalizer)
                                      document-vector document)
  (declare (ignore document))
  (loop for i below (length document-vector) do
        (let ((x (aref document-vector i)))
          (setf (aref document-vector i)
                (cond ((plusp x) 1.0)
                      ((minusp x) -1.0)
                      (t 0.0)))))
  document-vector)

;;;

(defclass null-normalizer ()
  ())

(defun make-null-normalizer ()
  (make-instance 'null-normalizer))

(defmethod print-object :around ((normalizer null-normalizer) stream)
  (if *print-readably*
      (format stream "#.~S" '(make-null-normalizer))
      (call-next-method)))

(defmethod normalize-matrix ((normalizer null-normalizer) matrix)
  matrix)

(defmethod normalize-document-vector ((normalizer null-normalizer)
                                      document-vector document)
  (declare (ignore document))
  document-vector)

;;;

(defclass row-centering-normalizer ()
  ((row-averages :reader row-averages)))

(defmethod normalize-matrix ((normalizer row-centering-normalizer) matrix)
  (let ((averages (make-array (n-rows matrix))))
    (loop for term below (n-rows matrix) do
          (let ((average (row-average matrix term)))
            (setf (aref averages term) average)
            (loop for document below (n-columns matrix) do
                  (decf (aref matrix term document) average))))
    (setf (slot-value normalizer 'row-averages) averages)
    matrix))

(defmethod normalize-document-vector ((normalizer row-centering-normalizer)
                                      document-vector document)
  (declare (ignore document))
  (let ((averages (row-averages normalizer)))
    (loop for term below (length document-vector) do
          (decf (aref document-vector term) (aref averages term))))
  document-vector)

;;;

(defclass column-power-normalizer ()
  ((power :initform 2 :initarg :power :reader power)))

(defun make-column-power-normalizer (&key power)
  (make-instance 'column-power-normalizer :power power))

(defmethod print-object :around ((normalizer column-power-normalizer) stream)
  (if *print-readably*
      (format stream "#.~S"
              `(make-column-power-normalizer :power ,(power normalizer)))
      (call-next-method)))

(defmethod normalize-matrix ((normalizer column-power-normalizer) matrix)
  (let ((power (slot-value normalizer 'power)))
    (loop for document below (n-columns matrix) do
          (normalize-column matrix document :power power)))
  matrix)

(defmethod normalize-document-vector ((normalizer column-power-normalizer)
                                      document-vector document)
  (declare (ignore document))
  (let ((power (slot-value normalizer 'power)))
    (normalize-vector document-vector :power power)))
