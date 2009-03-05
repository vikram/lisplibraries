
(in-package :cl-user)

(asdf:oos 'asdf:load-op :qbook)
(asdf:oos 'asdf:load-op :bdb-playground)

(defun make-doc ()
  (asdf:oos 'qbook:publish-op :bdb-playground
            :generator (make-instance 'qbook:html-generator
                                      :output-directory "/home/main/proj/lisp/bdb-web/qbook/bdb-playground/"
                                      :title "Berkley DB Playground")))

(make-doc)

