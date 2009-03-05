
(in-package :cl-user)

(asdf:oos 'asdf:load-op :qbook)
(asdf:oos 'asdf:load-op :cffi-util)

(defun make-doc ()
  (asdf:oos 'qbook:publish-op :cffi-util
            :generator (make-instance 'qbook:html-generator
                                      :output-directory "/home/main/proj/lisp/bdb-web/qbook/cffi-util/"
                                      :title "CFFI-Util Source")))

(make-doc)

