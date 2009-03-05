
(defpackage #:gzip-stream 
  (:use :cl :flexi-streams :trivial-gray-streams)
  (:export #:gzip-output-stream #:gzip-input-stream
   #:gzip #:gunzip #:with-open-gzip-file
   #:make-gzip-input-stream #:make-gzip-output-stream
   #:asdf-install-extractor))

