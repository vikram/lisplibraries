;;; Assigning indices to terms and documents.

(in-package :lassie.indexer)

;;; Interface
;;;
;;; Indexers can also be printed readably.

(defgeneric ->index (indexer object &key allocate-new-index-p)
  (:documentation "Return an index representing OBJECT."))

(defgeneric <-index (indexer index)
  (:documentation "Return the object that is encoded to INDEX."))


;;; Counting indexer

(defstruct counting-indexer
  "Simply assigns a new index to every object."
  (count 0))

(defmethod ->index ((indexer counting-indexer) object &key allocate-new-index-p)
  (declare (ignore object))
  (when allocate-new-index-p
    (prog1 (counting-indexer-count indexer)
      (incf (counting-indexer-count indexer)))))

(defmethod <-index ((indexer counting-indexer) object)
  (declare (ignore object))
 nil)


;;; Hashing indexer

(defstruct (hashing-indexer (:constructor %make-hashing-indexer))
  (next-index 0)
  index->object
  object->index)

(defun make-hashing-indexer (&key (test #'equal))
  (%make-hashing-indexer :index->object (make-hash-table)
                         :object->index (make-hash-table :test test)))

(defmethod ->index ((indexer hashing-indexer) object &key allocate-new-index-p)
  (with-slots (index->object object->index next-index) indexer
    (cond ((gethash object object->index))
          (allocate-new-index-p
           (prog1
               next-index
             (setf (gethash next-index index->object) object)
             (setf (gethash object object->index) next-index)
             (incf next-index)))
          (t nil))))

(defmethod <-index ((indexer hashing-indexer) index)
  (with-slots (index->object) indexer
    (gethash index index->object)))

(defun hashing-indexer->alist (indexer)
  (loop for key being the hash-key in (hashing-indexer-index->object indexer)
        using (hash-value value)
        collect (cons key value)))

(defun alist->hashing-indexer (test alist)
  (let ((next-index 0)
        (index->object (make-hash-table))
        (object->index (make-hash-table :test test)))
    (loop for (index . object) in alist do
          (incf next-index)
          (setf (gethash index index->object) object)
          (setf (gethash object object->index) index))
    (%make-hashing-indexer :next-index next-index
                           :index->object index->object
                           :object->index object->index)))

(defmethod print-object :around ((indexer hashing-indexer) stream)
  (if *print-readably*
      (format stream "#.~S" `(alist->hashing-indexer
                              ',(hash-table-test
                                 (hashing-indexer-object->index indexer))
                              ',(hashing-indexer->alist indexer)))
      (call-next-method)))

(defun test-hashing-indexer ()
  (let* ((test 'equalp)
         (alist '((0 . "word") (1 . "stuff")))
         (indexer0 (alist->hashing-indexer test alist))
         (indexer (read-from-string
                   (let ((*print-readably* t))
                     (format nil "~S~%" indexer0)))))
    (assert (eq (hash-table-test (hashing-indexer-object->index indexer))
                test))
    (assert (equal alist (hashing-indexer->alist indexer)))))


;;; Random indexer

(defstruct (random-indexer (:constructor %make-random-indexer))
  length
  n
  object->index)

(defun make-random-indexer (&key length n (test #'equal))
  (%make-random-indexer :length length :n n
                        :object->index (make-hash-table :test test)))

(defun make-index-vector (n length)
  "Create a random index vector of LENGTH with N 1s and N -1s. It is
stored as a sparse vector (only the indices of non-zero elements where
the first N are +1 the rest are -1)."
  (assert (<= (* 2 n) length))
  (let ((v (make-array (* 2 n) :element-type 'fixnum)))
    (loop with i = 0
          while (< i (* 2 n)) do
          (let ((x (random length)))
            (unless (find x v)
              (setf (aref v i) x)
              (incf i))))
    v))

(defmethod ->index ((indexer random-indexer) object &key allocate-new-index-p)
  (with-slots (length n object->index) indexer
    (cond ((gethash object object->index))
          (allocate-new-index-p
           (let ((index (make-index-vector n length)))
             (setf (gethash object object->index) index)))
          (t nil))))

(defun random-indexer->alist (indexer)
  (loop for key being the hash-key in (random-indexer-object->index indexer)
        using (hash-value value)
        collect (cons key (coerce value '(simple-array t (*))))))

(defun alist->random-indexer (length n test alist)
  (let ((object->index (make-hash-table :test test)))
    (loop for (object . index) in alist do
          (setf (gethash object object->index)
                (coerce index '(simple-array fixnum (*)))))
    (%make-random-indexer :length length :n n :object->index object->index)))

(defmethod print-object :around ((indexer random-indexer) stream)
  (if *print-readably*
      (format stream "#.~S" `(alist->random-indexer
                              ,(random-indexer-length indexer)
                              ,(random-indexer-n indexer)
                              ',(hash-table-test
                                 (random-indexer-object->index indexer))
                              ',(random-indexer->alist indexer)))
      (call-next-method)))

(defun test-random-indexer ()
  (let* ((length 10)
         (n 1)
         (test 'equalp)
         (alist '((word . #(5 8)) (stuff . #(3 7))))
         (indexer0 (alist->random-indexer length n test alist))
         (indexer (read-from-string
                   (let ((*print-readably* t))
                     (format nil "~S~%" indexer0)))))
    (assert (= (random-indexer-length indexer) length))
    (assert (= (random-indexer-n indexer) n))
    (assert (eq (hash-table-test (random-indexer-object->index indexer))
                test))
    (assert (every (lambda (x y)
                     (and (equal (car x) (car y))
                          (equal (coerce (cdr x) 'list)
                                 (coerce (cdr y) 'list))))
                   alist (random-indexer->alist indexer)))))

#|

(test-hashing-indexer)
(test-random-indexer)

|#
