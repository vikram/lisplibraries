(asdf:oos 'asdf:load-op :ucw)
(asdf:oos 'asdf:load-op :qbook)

(asdf:oos 'qbook:publish-op :ucw
          :generator (make-instance 'qbook:html-generator
                       :output-directory "./html/source/"
                       :title "UCW Source"))

(asdf:oos 'qbook:publish-op :ucw
          :generator (make-instance 'qbook:latex-generator
                       :output-file "./pdf/source.tex"
                       :title "UCW Source"))
