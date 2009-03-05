
(in-package :bdb-ext-playground)


(setf (symbol-function 'cbuf-writer)
      (make-cbuffer-writer #'cl-store:store))

(setf (symbol-function 'cbuf-reader)
      (make-cbuffer-reader #'cl-store:restore))
