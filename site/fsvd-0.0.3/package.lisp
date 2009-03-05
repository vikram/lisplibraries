(cl:defpackage :fsvd
  (:use #:common-lisp)
  (:export
   ;; Sparse matrix interface
   #:map-matrix
   #:height-of
   #:width-of
   #:size-of
   #:dense-row-index
   #:dense-column-index
   #:do-matrix
   #:do-matrix-macro-name
   ;; Core
   #:sv
   #:sv-left
   #:sv-right
   #:svd
   #:make-sv
   #:make-svd
   #:save-svd
   #:load-svd
   #:svd-value
   #:make-svd-approximator
   ;; Utilities
   #:approximation-rmse
   #:limiting-supervisor
   #:svd-in-progress
   #:max-n-iterations
   #:max-n-svs
   #:supervise-svd)
  (:documentation "This is a Common Lisp implementation of Simon
Funk's quasi svd as described at
http://sifter.org/~simon/journal/20061211.html. There is nothing quasi
about it when there are no missing elements in the original matrix and
the normalization factor is zero, but that requires a small learning
rate. Loss of orthogonality between singular vectors results from
using too large learning rates."))
