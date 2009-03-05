(asdf:operate 'asdf:load-op :cxml-stp)
(asdf:operate 'asdf:load-op :atdoc)
(atdoc:generate-documentation '(:cxml-stp)
			      "/home/david/src/lisp/cxml-stp/doc/"
			      :index-title "cxml-stp API reference"
			      :heading "cxml-stp"
			      :css "cxml-stp.css")
