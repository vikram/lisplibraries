(defpackage :cxml-rng-system
  (:use :asdf :cl))
(in-package :cxml-rng-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem :cxml-rng
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "package")
     (:file "floats")
     (:file "unicode")
     (:file "nppcre")
     (:file "types")
     (:file "parse")
     (:file "validate")
     (:file "test")
     (:file "clex")
     (:file "compact"))
    :depends-on (:cxml :cl-ppcre :yacc :parse-number :cl-base64))
