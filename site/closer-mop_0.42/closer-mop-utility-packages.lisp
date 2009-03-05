(in-package :cl-user)

(defpackage #:closer-common-lisp
  (:nicknames #:c2cl)
  (:use))

(let ((syms (nunion (loop for sym being the external-symbols of :common-lisp
                          if (find-symbol (symbol-name sym) :c2mop)
                          collect it
                          else collect sym)
                    (loop for sym being the external-symbols of :c2mop
                          collect sym))))
  (import syms :c2cl)
  (export syms :c2cl))

(defpackage #:closer-common-lisp-user
  (:nicknames #:c2cl-user)
  (:use #:closer-common-lisp))
