(defsystem :alexandria
  :version "0.0.0"
  :licence "Public Domain / 0-clause MIT"
  :components
  ((:static-file "LICENCE")
   (:static-file "tests.lisp")
   (:file "package")
   (:file "definitions" :depends-on ("package"))
   (:file "strings" :depends-on ("package"))
   (:file "errors" :depends-on ("package"))
   (:file "hash-tables" :depends-on ("package"))
   (:file "macros" :depends-on ("package" "strings"))
   (:file "control-flow" :depends-on ("package" "macros"))
   (:file "symbols" :depends-on ("package"))
   (:file "arrays" :depends-on ("package"))
   (:file "types" :depends-on ("package"))
   (:file "binding" :depends-on ("package"))
   (:file "functions" :depends-on ("package" "symbols" "macros"))
   (:file "lists" :depends-on ("package" "functions"))
   (:file "sequences" :depends-on ("package" "lists"))
   (:file "numbers" :depends-on ("package" "sequences"))))
