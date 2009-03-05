(cl:defpackage :lassie.indexer
  (:use #:common-lisp)
  (:export #:->index
           #:<-index
           #:make-counting-indexer
           #:make-hashing-indexer
           #:make-random-indexer)
  (:documentation "Indexers provide a - sometimes reversible - mapping
from objects and indices. The word `index' is used here in a very
general sense, random indexers, for instance, map to a set of indices.
Within Lassie they are used in conjunction with assemblers that know
how to change the co-occurence matrix when encountering given an
index. They can be printed and read readably."))

(cl:defpackage :lassie.assembler
  (:use #:common-lisp)
  (:export #:assemble-co-occurrence-matrix
           #:assemble-term-vector
           #:assemble-document-vector
           #:lsa-assembler
           #:ri-term-assembler
           #:normalize-matrix
           #:normalize-term-vector
           #:normalize-document-vector
           #:tf-idf-normalizer
           #:pmi-normalizer
           #:sign-normalizer
           #:null-normalizer
           #:row-centering-normalizer
           #:column-power-normalizer)
  (:documentation "Different assemblers and normalizers that one plug
into Lassie. Assemblers to construct a co-occurence matrix or document
vector from a corpus, and normalizers to perform post processing on
the assembled data. Normalizers can be printed and read readably."))

(cl:defpackage :lassie
  (:use #:common-lisp #:lassie.indexer #:lassie.assembler)
  (:export #:lsa
           #:save-lsa
           #:load-lsa
           ;; LSA accessors
           #:term-mapper
           #:document-mapper
           #:term-indexer
           #:document-indexer
           #:normalizer
           #:svd
           ;; Vectorizing
           #:term->vector
           #:document->vector
           ;; Features
           #:n-features
           #:truncate-lsa
           #:term-vector-features
           #:document-vector-features
           #:term-features
           #:document-features
           #:construct-term-vector
           #:construct-document-vector
           ;; Extras
           #:cosine-similarity
           #:most-similar-terms
           #:most-similar-documents)
  (:documentation "The core functionality of Lassie."))
