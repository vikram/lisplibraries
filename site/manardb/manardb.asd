(cl:in-package #:cl-user)

(asdf:defsystem manardb
  :version "0.1.20090911"
  :licence "LLGPL"
  :components
  ((:module :src
	    :components (
			 (:file "package")
			 (:file "widths" :depends-on ("package"))
			 (:file "utils" :depends-on ("package"))
			 (:file "struct" :depends-on ("utils" "widths"))
			 (:file "mop" :depends-on ("struct"))
			 (:file "mtagmap" :depends-on ("widths" "struct" "mop"))
			 (:file "class" :depends-on ("mop" "mtagmap"))
			 (:file "iterator" :depends-on ("class"))
			 (:file "types" :depends-on ("class"))
			 (:file "box" :depends-on ("types"))
			 (:file "fixed-string" :depends-on ("box"))
			 (:file "array" :depends-on ("types"))
			 (:file "finalize" :depends-on ("box"))
			 (:file "transaction" :depends-on ("finalize"))
			 (:file "gc" :depends-on ("finalize"))
			 (:file "rewrite-gc" :depends-on ("gc"))
			 )))
  :depends-on (alexandria osicat iterate closer-mop cl-irregsexp))



