
(asdf:oos 'asdf:load-op :ucw)
(asdf:oos 'asdf:load-op :ucw.aserve)
(asdf:oos 'asdf:load-op :ucw.araneida)
(asdf:oos 'asdf:load-op :ucw.mod-lisp)
(asdf:oos 'asdf:load-op :qbook)

(asdf:oos 'asdf:load-op :qbook)

(in-package :it.bese.ucw-user)

(qbook:publish-qbook "../examples/wiki.lisp"
                     (make-instance 'qbook:latex-generator
                                    :title "Creating a Wiki with UCW"
                                    :output-file "./pdf/wiki.tex"))

(qbook:publish-qbook "../examples/wiki.lisp"
                     (make-instance 'qbook:html-generator
                                    :title "Creating a Wiki with UCW"
                                    :output-directory "./html/wiki/"))
