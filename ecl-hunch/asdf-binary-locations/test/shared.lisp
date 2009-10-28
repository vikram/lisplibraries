(in-package #:common-lisp-user)
(load "./asdf.compiled")
(load "../asdf-binary-locations.asd")
(asdf:oos 'asdf:load-op 'asdf-binary-locations)
