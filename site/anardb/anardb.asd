(cl:in-package #:cl-user)
(asdf:defsystem anardb
  :version "0.1.20090604"
  :licence "LLGPL"
  :components
  ((:module :src
	    :components (
	    (:file "utils" :depends-on ("package"))
	    (:file "generics" :depends-on ("package"))
	    (:file "package")
	    (:file "system" :depends-on ("package"))
	    (:file "lock" :depends-on ("system"))
	    (:file "defdbclass" :depends-on ("utils" "generics"))
	    (:file "store" :depends-on ("defdbclass" "lock"))
	    (:file "transaction" :depends-on ("store")))))
  :depends-on (alexandria closer-mop))

