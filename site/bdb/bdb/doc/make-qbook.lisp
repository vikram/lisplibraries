
(in-package :cl-user)

(asdf:oos 'asdf:load-op :qbook)
(asdf:oos 'asdf:load-op :bdb)

(defun make-doc ()
  (asdf:oos 'qbook:publish-op :bdb
            :generator (make-instance 'qbook:html-generator
                                      :output-directory "/home/main/proj/lisp/bdb-web/qbook/bdb/"
                                      :title "Berkley DB CFFI-Bindings")))

(make-doc)

