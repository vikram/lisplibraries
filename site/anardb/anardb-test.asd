(asdf:defsystem anardb-test
  :description "Tests for anardb"
  :author "MSI"
  :version "20090318"
  :licence "unknown"
  :depends-on (anardb stefil)
  :components ((:module t
			:components ((:file "suite") (:file "schema" :depends-on ("suite")) (:file "basic" :depends-on ("schema")) (:file "fork" :depends-on ("basic"))))))
 