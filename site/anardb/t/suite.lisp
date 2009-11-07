(defpackage #:anardb.test
  (:use #:anardb #:cl))
(in-package #:anardb.test)
(stefil:defsuite anardb)
(setf *store* (make-store))



(defun test-fresh-store ()
  (anardb::store-wipe *store*)
  (setf (anardb::store-version *store*) 0)
  (let ((dir (format nil "/tmp/anardb-test-~36r/" 
		     (mod (* (get-universal-time) (get-internal-run-time) (random most-positive-fixnum (make-random-state t))) most-positive-fixnum))))
    (assert (not (probe-file dir)))
    (store-init-dir *store* dir)))
