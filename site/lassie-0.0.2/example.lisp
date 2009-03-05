(defparameter *sentences*
  '(("Hello" "world")
    ("This" "is" "a" "small" "world")
    ("Small" "is" "beautiful")))

(defparameter *supervisor* (make-instance 'fsvd:limiting-supervisor
                                          :max-n-iterations 100
                                          :max-n-svs 3))

(defun map-sentence (function sentence)
  (map nil function sentence))

(defun list-documents (function)
  (map nil function *sentences*))

(defparameter *lsa*
  (lassie:lsa :document-mapper 'map-sentence
              :term-indexer (lassie.indexer:make-hashing-indexer)
              :document-indexer (lassie.indexer:make-hashing-indexer)
              :document-lister #'list-documents
              :assembler (make-instance 'lassie.assembler:lsa-assembler)
              :normalizer (make-instance 'lassie.assembler:tf-idf-normalizer)
              :supervisor *supervisor*
              :learning-rate 0.025))

(defparameter *this-directory* (make-pathname :name nil :type nil
                                              :defaults *load-truename*))

#+nil
(lassie:save-lsa *lsa* :filename (merge-pathnames "test.lsa" *this-directory*))

#+nil
(let ((lsa (lassie:load-lsa
            :filename (merge-pathnames "test.lsa" *this-directory*))))
  (lassie:save-lsa lsa
                   :filename (merge-pathnames "test2.lsa" *this-directory*)))
