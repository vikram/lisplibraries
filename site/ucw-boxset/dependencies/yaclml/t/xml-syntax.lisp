;;;; -*- lisp -*-

(in-package :it.bese.yaclml.test)

(enable-bracket-reader)

(def-suite :it.bese.yaclml.xml-syntax :in :it.bese.yaclml)

(in-suite :it.bese.yaclml.xml-syntax)

(defmacro yaclml= (str &body body)
  `(is (string= (format nil ,str) (with-yaclml-output-to-string ,@body))))

(test trivial
  {with-xml-syntax
    (yaclml= "<a~%/>" <a >)
    (yaclml= "<a foo=\"bar\"~%/>" <a :foo "bar">)
    (let ((var 42))
      (yaclml= "<a var=\"42\" eLiTe=\"caMel\" var=\"42\"~%  >body</a~%>"
                <a (@ "eLiTe" "caMel" :var var) (@) :var var "body">))})

(test number-of-evaluation
  {with-xml-syntax
    (let ((var 42))
      (yaclml= "<42 foo=\"foo\"~%  >body</42~%>"
                <(prog1 var
                   (setf var nil)) :foo t
                   "body">))})

