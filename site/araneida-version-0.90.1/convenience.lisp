(in-package araneida)

(defmacro attach-hierarchy (parent-handler internal-url external-url &rest directives)
  "A convenience macro to aid in installing handlers to a parent handler.

Not-really-EBNF follows:
hierarchy-definition := ( attach-hierarchy parent-handler internal-url external-url
                                           {directive}* )

parent-handler := an instance of araneida:handler or children
external-url   := an araneida:url
internal-url   := an araneida:url

directive := ( url-component object-place
                             {directive-or-eval}* )

url-component  := a string that will be merged with the urls

object-place := handler-class-name
                | ( handler-class-name handler-class-instance )

handler-class-name := name of a class inheriting from araneida:handler
handler-class-instance := either an instance or a call that returns an instance of aforementioned class

directive-or-eval := either a directive or lisp commands

Each handler-class-name will have a function #'handler-class-name-url defined
that will return the external url for the class.

eval directives have _handler, _iurl, and _eurl defined as lexical variables. _handler is
the parent handler for that directive, _iurl is the internal url, while _eurl is the external url for that directive.
		  
Syntax is best explained in two examples:

(attach-hierarchy (http-listener-handler *listener*) *internal-url* *url*
		  (\"/hier\" root-h
			   (\"/foo\" foo-h)
			   (\"/bar\" bar-h)))

Given a dispatching handler ROOT-H and two handlers FOO-H and BAR-H,
this would install
root-h to *url*/hier/
foo-h  to *url*/hier/foo
bar-h  to *url*/hier/bar

With inexact matching.

In addition, (root-h-url) would return *url*/hier/, (foo-h-url) would return *url*/hier/foo, etc.

A more complicated example is here:

(araneida:attach-hierarchy (http-listener-handler *listener*) *internal-url* *url*
		  (\"/hier-advanced/\" hieradv-root-handler
				    (\"foo\" (hieradv-foo-handler (make-instance 'hieradv-foo-handler :moofoo \"mooofooo\")))
				    (\"bar\" hieradv-bar-handler)
				    (dolist (i '(\"yes\" \"no\" \"maybe\" \"so\"))
				      (install-handler _handler
						       (make-instance 'hieradv-xbase-handler
								      :mim i
								      :mimurl _eurl)
						       i t))))

The tricky bits are:
*url*/hier-advanced/foo goes to (make-instance 'hieradv-foo-handler :moofoo \"mooofooo\"). This is one way
to attach at URL to an object that takes parameters.

The DOLIST portion attaches *url*/hier-advanced/yes, *url*/hier-advanced/no. Providing \"yes\", \"no\", etc
as parameters to the instances of 'hieradv-xbase-handler, as well as the parent's external URL.

It's rare to need these advanced forms, but it sure makes things a lot easier to have them.

One note: having the same class attached at multiple points will result in the -url function having
undefined behavior. You'll get something, but you might wish you hadn't.

These examples are in the test code for araneida."
  (when (null directives)
    (error "No directives specified. Check the argument list. This is commonly caused by not having an internal AND an external
URL specified"))
  (once-only (parent-handler internal-url external-url)
    `(progn
      ,@(mapcan (lambda (directive)
		  (decode-hierarchy-directive parent-handler internal-url external-url directive t))
	      directives))))
