;;;; -*- coding:utf-8 -*-

(ext:without-package-lock ("GRAY" "COMMON-LISP" "CHARSET")
  (package:add-nickname "CHARSET""EXTERNAL-FORMAT")
  (asdf:oos 'asdf:load-op :mcclim))
(asdf:operate 'asdf:load-op :clim-listener)
(format t "~2%(clim-listener:run-listener)~2%")

