;;;; tests.lisp
;;;;
;;;; This is for simple prerelase sanity testing, not general
;;;; use. Please ignore.

(defpackage #:zs3-tests
  (:use #:cl #:zs3))

(in-package #:zs3-tests)

(setf *credentials* (file-credentials "~/.aws"))

(when (bucket-exists-p "zs3-tests")
  (delete-bucket "zs3-tests"))

(create-bucket "zs3-tests")

(put-file "/etc/printcap" "zs3-tests" "printcap")
(put-string "Hello, world" "zs3-tests" "hello")
(put-vector (octet-vector 8 6 7 5 3 0 9) "zs3-tests" "jenny")

(all-buckets)
(all-keys "zs3-tests")

(delete-object "zs3-tests" "printcap")
(delete-object "zs3-tests" "hello")
(delete-object "zs3-tests" "jenny")

(put-string "Hello, world" "zs3-tests" "hello" :start 1 :end 5)
(string= (get-string "zs3-tests" "hello")
         (subseq "Hello, world" 1 5))

(put-file "tests.lisp" "zs3-tests" "self" :start 1 :end 5)
(string= (get-string "zs3-tests" "self")
         ";;; ")

(defparameter *jenny* (octet-vector 8 6 7 5 3 0 9))
(put-vector *jenny* "zs3-tests" "jenny" :start 1 :end 6)

(equalp (get-vector "zs3-tests" "jenny")
        (subseq *jenny* 1 6))


(delete-object "zs3-tests" "hello")
(delete-object "zs3-tests" "self")
(delete-object "zs3-tests" "jenny")

(delete-bucket "zs3-tests")
