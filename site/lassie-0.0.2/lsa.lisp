(in-package :lassie)

(deftype single-float-vector () '(simple-array single-float (*)))

(defclass lsa ()
  ((term-mapper
    :initarg :term-mapper
    :reader term-mapper
    :documentation "A mapper over all documents in which a given term occurs.")
   (document-mapper
    :initarg :document-mapper
    :reader document-mapper
    :documentation "A mapper over all terms that occur in a given document.")
   (term-indexer
    :initarg :term-indexer
    :reader term-indexer
    :documentation "Term indexer.")
   (document-indexer
    :initarg :document-indexer
    :reader document-indexer
    :documentation "Document indexer.")
   (assembler
    :initarg :assembler
    :reader assembler
    :documentation "Turns co-occurrences into a matrix, term and
document vectors.")
   (normalizer
    :initarg :normalizer
    :reader normalizer
    :documentation "Performs some last minute transformations on the
assembled matrix.")
   (svd
    :initarg :svd
    :reader svd
    :documentation "The singular value decomposition."))
  (:documentation "This is not much more than a convenience class that
remembers how the SVD was produced to be able to extract features
later, or just to know what a given row or column corresponds to."))

(defun save-lsa (lsa &key filename
                 (svd-filename (make-pathname :type "svd" :defaults filename)))
  "Save LSA to FILENAME and its svd to SVD-FILENAME."
  (with-open-file (s filename :direction :output
                   :if-does-not-exist :create :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-pretty* nil))
        (prin1 (list :document-mapper (document-mapper lsa)
                     :term-indexer (term-indexer lsa)
                     :document-indexer (document-indexer lsa)
                     :assembler (assembler lsa)
                     :normalizer (normalizer lsa))
               s))))
  (fsvd:save-svd (svd lsa) svd-filename)
  (values))

(defun load-lsa (&key filename
                 (svd-filename (make-pathname :type "svd" :defaults filename)))
  "Return the lsa loaded from FILENAME and SVD-FILENAME."
  (apply #'make-instance 'lsa :svd (fsvd:load-svd svd-filename)
         (with-open-file (s filename :direction :input)
           (with-standard-io-syntax
             (read s)))))

(defun lsa (&key term-mapper document-mapper
            term-lister document-lister
            (term-indexer (make-hashing-indexer))
            (document-indexer (make-hashing-indexer))
            (assembler (make-instance 'lsa-assembler))
            (normalizer (make-instance 'tf-idf-normalizer))
            supervisor
            learning-rate
            (normalization-factor 0.0))
  "Perform LSA and return the lsa object that contains the SVD and
remembers the mappers, indexers, ASSEMBLER and NORMALIZER for easy
querying later by for example DOCUMENT-FEATURES.

This fat function assembles the co-occurrence matrix by iterating over
all terms by TERM-LISTER and all documents by DOCUMENT-LISTER (either
may be NIL). If DOCUMENT-LISTER is provided then DOCUMENT-MAPPER is
employed to iterate over the terms of each document. Similarly
TERM-MAPPER complements TERM-LISTER. TERM-INDEXER and DOCUMENT-INDEXER
provide a - sometimes invertible - mapping from terms/documents to
indices.

After the initial construction the mappers and indexers are stored in
the LSA instance because they are needed to assemble term/document
vectors later.

Finally the co-occurrence matrix is decomposed into singular vector
pairs that define the feature spaces.

SUPERVISOR is a FSVD supervisor on which FSVD:SUPERVISE-SVD is invoked
to control iteration (see FSVD:SVD). The lsa instance being
constructed is passed as the :LSA argument to allow inspecting,
saving, etc."
  (let* ((lsa (make-instance 'lsa
                             :term-mapper term-mapper
                             :document-mapper document-mapper
                             :term-indexer term-indexer
                             :document-indexer document-indexer
                             :assembler assembler
                             :normalizer normalizer))
         (lister (make-encoded-term-document-lister term-mapper
                                                    document-mapper
                                                    term-indexer
                                                    document-indexer
                                                    term-lister
                                                    document-lister))
         (assembled-matrix (assemble-co-occurrence-matrix assembler lister))
         (matrix (normalize-matrix normalizer assembled-matrix)))
    (fsvd:svd matrix :learning-rate learning-rate
              :normalization-factor normalization-factor
              :supervisor (lambda (svd i &rest args)
                            (setf (slot-value lsa 'svd) svd)
                            (apply #'fsvd:supervise-svd supervisor svd i
                                   :lsa lsa args)))
    lsa))

;;;; Features

(defun n-features (lsa)
  "The number of features of LSA is the number of singular vector pairs."
  (length (lassie:svd lsa)))

(defun truncate-lsa (lsa n-features)
  "Return a copy of LSA that has only N-FEATURES."
  (make-instance 'lsa
                 :term-mapper (term-mapper lsa)
                 :document-mapper (document-mapper lsa)
                 :term-indexer (term-indexer lsa)
                 :document-indexer (document-indexer lsa)
                 :assembler (assembler lsa)
                 :normalizer (normalizer lsa)
                 :svd (subseq (svd lsa) 0 n-features)))

(defun term->vector (lsa term)
  "Turn TERM into a document vector."
  (normalize-term-vector
   (normalizer lsa)
   (assemble-term-vector (assembler lsa)
                         (curry-mapper (encode-mapper (term-mapper lsa)
                                                      (document-indexer lsa))
                                       term))
   term))

(defun document->vector (lsa document)
  "Turn DOCUMENT into a document vector."
  (normalize-document-vector
   (normalizer lsa)
   (assemble-document-vector (assembler lsa)
                             (curry-mapper (encode-mapper (document-mapper lsa)
                                                          (term-indexer lsa))
                                           document))
   document))

(defun inner* (vector1 vector2)
  (declare (type single-float-vector vector1 vector2))
  (let ((sum 0.0))
    (declare (optimize (speed 3)))
    (dotimes (i (length vector1))
      (incf sum (* (aref vector1 i) (aref vector2 i))))
    sum))

(defun coordinate (basis vector)
  "Return the length of the projection of VECTOR to BASIS."
  (declare (type single-float-vector basis vector))
  (let ((sum 0.0)
        (sum2 0.0))
    (declare (optimize (speed 3))
             (type single-float sum sum2))
    (dotimes (i (length vector))
      (declare (type fixnum i))
      (let ((b (aref basis i)))
        (incf sum (* b (aref vector i)))
        (incf sum2 (* b b))))
    (/ sum sum2)))

(defun update-residual (residual coordinate basis)
  (declare (type single-float-vector basis residual)
           (type single-float coordinate)
           (optimize (speed 3) (debug 0) (safety 0)))
  (dotimes (j (length residual))
    (declare (type fixnum j))
    (setf (aref residual j)
          (- (aref residual j)
             (* coordinate (aref basis j))))))

(defun extract-svd-features (svd vector fn)
  (declare (type fsvd:svd svd))
  (let ((residual (make-array (length vector) :element-type 'single-float))
        (v (make-array (length svd) :element-type 'single-float)))
    (replace residual vector)
    (dotimes (i (length svd))
      (declare (type fixnum i))
      (let* ((basis (funcall fn (aref svd i)))
             (coordinate (coordinate basis residual)))
        (declare (type single-float-vector basis residual)
                 (type single-float coordinate))
        (setf (aref v i) coordinate)
        (update-residual residual coordinate basis)))
    v))

(defun extract-lsa-features (lsa vector-or-index sv-own sv-other)
  (etypecase vector-or-index
    (fixnum
     (map 'vector (lambda (sv)
                    (aref (funcall sv-own sv) vector-or-index))
          (svd lsa)))
    (vector
     (extract-svd-features (svd lsa) vector-or-index sv-other))))

(defun term-vector-features (lsa vector-or-index)
  "Return the feature vector for the term given by term VECTOR or
INDEX."
  (extract-lsa-features lsa vector-or-index #'fsvd:sv-left #'fsvd:sv-right))

(defun document-vector-features (lsa vector-or-index)
  "Return the feature vector for the document given by document VECTOR
or INDEX."
  (extract-lsa-features lsa vector-or-index #'fsvd:sv-right #'fsvd:sv-left))

(defun term-features (lsa term)
  "Convenience function that returns the features of TERM after
turning into into a vector with LSA."
  (term-vector-features lsa (term->vector lsa term)))

(defun document-features (lsa document)
  "Convenience function that returns the features of DOCUMENT after
turning into into a vector with LSA."
  (document-vector-features lsa (document->vector lsa document)))

(defun construct-lsa-vector (lsa features sv-side)
  (let* ((n (length (funcall sv-side (aref (svd lsa) 0))))
         (v (make-array n :element-type 'single-float)))
    (dotimes (i (length features) v)
      (let ((a (aref features i)))
        (map-into v (lambda (x y)
                      (+ x (* a y)))
                  v
                  (funcall sv-side (aref (svd lsa) i)))))))

(defun construct-term-vector (lsa features)
  "Construct a term vector from FEATURES. Inverse of
TERM-VECTOR-FEATURES."
  (construct-lsa-vector lsa features #'fsvd:sv-right))

(defun construct-document-vector (lsa features)
  "Construct a document vector from FEATURES. Inverse of
DOCUMENT-VECTOR-FEATURES."
  (construct-lsa-vector lsa features #'fsvd:sv-left))
