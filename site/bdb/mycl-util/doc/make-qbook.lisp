
(in-package :cl-user)

(asdf:oos 'asdf:load-op :qbook)
(asdf:oos 'asdf:load-op :mycl-util)

(defun make-doc ()
  (asdf:oos 'qbook:publish-op :mycl-util
            :generator (make-instance 'qbook:html-generator
                                      :output-directory "/home/main/proj/lisp/bdb-web/qbook/mycl-util/"
                                      :title "Berkley DB CFFI-Bindings")))

(make-doc)

