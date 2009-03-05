(asdf:operate 'asdf:load-op :closure-html)
(asdf:operate 'asdf:load-op :atdoc)
(atdoc:generate-documentation
 '(:chtml :hax)
 "/home/david/src/lisp/closure-html/doc/atdoc/"
 :index-title "Closure HTML API reference"
 :heading "Closure HTML"
 :css "cxml.css")
