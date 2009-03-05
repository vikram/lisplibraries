#!/usr/bin/sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
  (setf *invoke-debugger-hook* (lambda (condition hook)
				 (declare (ignore hook))
				 #+sbcl(sb-debug:backtrace 20 *error-output*)
				 #+cmu(debug:backtrace 20 *error-output*)
				 (format *error-output* "Error: ~A~%" condition)
				 (quit)))
  ; load current directory's ASD file first
  (push '*default-pathname-defaults* asdf:*central-registry*)

  (asdf:oos 'asdf:load-op :araneida))

  ; allow forcing serve-event
  #+sbcl(when (string-equal (third sb-ext:*posix-argv*) "serve-event")
	  (delete :araneida-threads *features*))

#|
This script is the server side of the basic Araneida test suite

important bookmarks:
defvar *url*
defvar *listener*
defun get-random-text
:teststart
:testend
start-listening

urls have a bookmark:

url: <absolute url minus host>

for example: "url: /strict-match/"

|#

(defpackage "TEST-LISTENER"
  (:use "COMMON-LISP" "ARANEIDA"))

(in-package test-listener)
(defvar *quit* 0)

(defvar *url* (parse-urlstring "http://localhost:15840"))
(defvar *listener* (make-instance #+araneida-threads 'threaded-http-listener
				  #-araneida-threads 'serve-event-http-listener
				  :address #(0 0 0 0)
				  :default-hostname "localhost"
				  :port (url-port *url*)))

(defun get-random-text ()
  "Returns the random text passed at the command line.
If it can't be figured out, it defaults to '42'"
  (or #+sbcl(second sb-ext:*posix-argv*)
      #+cmu(fifth extensions:*command-line-strings*)
      "42"))

(defvar *uniquetext* (format nil "|RANDOMID=~A|" (get-random-text))
  "This unique identifier should be inserted into every page. This is so we know we're
not connecting to a stale session.")

(defun gencontent (string)
  (declare (type (or string cons) string))
  `(html (body
	  (h1 ,string)
	  (h1 ,*uniquetext*))))

(defmacro simplegen (string)
  (let ((string-val (gensym)))
    `(let ((,string-val ,string))
      (request-send-headers request)
      (html-stream (request-stream request)
       (gencontent ,string-val)))))

(defmacro my-install-handler (class url &optional (strict t))
  `(install-handler (http-listener-handler *listener*)
    (make-instance ',class)
    (urlstring (merge-url *url* ,url)) ,strict))
(defmacro install-handler-for (obj url &optional (strict t))
  `(install-handler (http-listener-handler *listener*)
    ,obj
    (urlstring (merge-url *url* ,url)) ,strict))

#| :teststart - the start of tests |# 

#|  -------- url: /handler   ------------------  |#
(defclass simple-handler (araneida:handler) ())
(defmethod handle-request-response ((handler simple-handler) method request)
  (simplegen "simple handler"))
(my-install-handler simple-handler "/handler")

#|  -------- url: /strict-match/   ------------------  |#
(defclass strict-matcher (araneida:handler) ())
(defmethod handle-request-response ((handler strict-matcher) method request)
  (simplegen "strict matcher"))
(my-install-handler strict-matcher "/strict-match/")

#|  -------- url: /loose-match/   ------------------  |#
(defclass loose-matcher (araneida:handler) ())
(defmethod handle-request-response ((handler loose-matcher) method request)
  (simplegen "loose matcher"))
(my-install-handler loose-matcher "/loose-match/" nil)

#|  -------- url: /html/html-stream   ------------------  |#
(defclass html-streamer (araneida:handler) ())
(defmethod handle-request-response ((handler html-streamer) method request)
  (request-send-headers request)
  (html-stream (request-stream request)
	       `(html (body
		       (h1 "testing")
		       (p ((a :href "/foo.html") "foo"))
		       (h1 ,*uniquetext*)))))
(my-install-handler html-streamer "/html/html-stream")

#|  -------- url: /html/html-escaped-stream   ------------------  |#
(defclass html-escaped-streamer (araneida:handler) ())
(defmethod handle-request-response ((handler html-escaped-streamer) method request)
  (request-send-headers request)
  (html-escaped-stream (request-stream request)
		       `(html (body
			       (h1 "testing & waiting")
			       (p ((a :href "/foo.html?a=1&b=2") "foo > bar"))
			       (h1 ,*uniquetext*)))))
(my-install-handler html-escaped-streamer "/html/html-escaped-stream")

#|  -------- url: /url/link   ------------------  |#
(defclass link-handler (araneida:handler) ())
(defmethod handle-request-response ((handler link-handler) method request)
  (simplegen (link (request-url request) :slays "everything but squid" :ow "yes")))
(my-install-handler link-handler "/url/link" nil)


#|  -------- url: /dispatch-handler/   ------------------  |#
; test a dispatching handler - and test to make sure it can handle its own response
(defclass dispatch-handler (araneida:dispatching-handler) ())
(defmethod handle-request-response :around ((handler dispatch-handler) method request)
  (or (call-next-method)
      (simplegen "dispatch handler")))
(defclass dispatch-handler-subhandler (araneida:handler) ())
(defmethod handle-request-response ((handler dispatch-handler-subhandler) method request)
  (simplegen "dispatch subhandler handler"))
(defmethod shared-initialize :after ((handler dispatch-handler) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (install-handler handler (make-instance 'dispatch-handler-subhandler) "subhandler" t))

(install-handler (http-listener-handler *listener*) (make-instance 'dispatch-handler)
		 (urlstring (merge-url *url* "/dispatch-handler/")) nil)

#|  -------- url: /error/   ------------------  |#
(defclass generate-error (araneida:handler)
  ((code :initarg :code :reader generate-error-code)))
(defmethod handle-request-response ((handler generate-error) method request)
  (error (generate-error-code handler) :client-message (format nil "Test error. ~A" *uniquetext*)))
(macrolet ((install-error-handler (error-name)
	     (let* ((symname (symbol-name error-name))
		    (error-sym (intern (format nil "HTTP-~A" symname) (find-package "ARANEIDA")))
		    (error-url (format nil "/error/~A" (string-downcase symname))))
	       `(install-handler-for (make-instance 'generate-error :code ',error-sym) ,error-url))))
  (install-error-handler bad-request)
  (install-error-handler unauthorized)
  (install-error-handler payment-required)
  (install-error-handler forbidden)
  (install-error-handler payment-required)
  (install-error-handler not-found)
  (install-error-handler method-not-allowed)
  (install-error-handler not-acceptable)
  (install-error-handler proxy-authentication-required)
  (install-error-handler request-time-out)
  (install-error-handler conflict)
  (install-error-handler gone)
  (install-error-handler length-required)
  (install-error-handler precondition-failed)
  (install-error-handler request-entity-too-large)
  (install-error-handler request-url-too-large)
  (install-error-handler unsupported-media-type)
  (install-error-handler internal-server-error)
  (install-error-handler not-implemented)
  (install-error-handler bad-gateway)
  (install-error-handler service-unavailable)
  (install-error-handler gateway-time-out)
  (install-error-handler version-not-supported))

#|  -------- url: /redirect/first   ------------------  |#
#|  -------- url: /redirect/second   ------------------  |#
(defclass redirect-first-handler (araneida:handler) ())
(defmethod handle-request-response ((handler redirect-first-handler) method request)
  (request-redirect request (merge-url *url* "/redirect/second")))
(my-install-handler redirect-first-handler "/redirect/first")

(defclass redirect-second-handler (araneida:handler) ())
(defmethod handle-request-response ((handler redirect-second-handler) method request)
  (simplegen "redirect second handler"))
(my-install-handler redirect-second-handler "/redirect/second")

#|  -------- url: /redirect-handler/first   ------------------  |#
#|  -------- url: /redirect-handler/second   ------------------  |#
; we don't have to subclass here for the redirect handler, it just helps with error checking
(defclass redirect-handler-first-handler (araneida:redirect-handler)
  ((araneida::location :initform (merge-url *url* "/redirect-handler/second"))))
(my-install-handler redirect-handler-first-handler "/redirect-handler/first")
(defclass redirect-handler-second-handler (araneida:handler) ())
(defmethod handle-request-response ((handler redirect-handler-second-handler) method request)
  (simplegen "redirect-handler second handler"))
(my-install-handler redirect-handler-second-handler "/redirect-handler/second")

#|  -------- url: /cookie/send   ------------------  |#
#|  -------- url: /cookie/receive   ------------------  |#
; SCORE! WWW::Mechanize cannot handle paths
(defclass cookie-send (araneida:handler) ())
(defmethod handle-request-response ((handler cookie-send) method request)
  (let ((cookies
	 (list
	  (cookie-string "simplecookie" "simple-value")
	  (cookie-string "ladencookie" "123ladenvalue123---" :comment "a comment!" :max-age 30))))
    (request-redirect request (merge-url *url* "/cookie/receive") :set-cookie cookies)))
(my-install-handler cookie-send "/cookie/send")
(defclass cookie-receive (araneida:handler) ())
(defmethod handle-request-response ((handler cookie-receive) method request)
  (let ((cookies '("simplecookie" "ladencookie")))
    (simplegen (format nil "cookie receive handler.<br>~%~:{unsafecookie ~A=~A<br>~%~}~%~:{safecookie ~A=~A<br>~%~}~%"
		       (mapcar (lambda (cookie-name) (list cookie-name (request-cookie request cookie-name)))
			       cookies)
		       (mapcar (lambda (cookie-name) (list cookie-name (request-safe-cookie request cookie-name nil)))
			       cookies)))))
(my-install-handler cookie-receive "/cookie/receive")


#|  -------- url: /conditional-get   ------------------  |#
(defclass conditional-get-handler (araneida:handler) ())
(defmethod handle-request-response ((handler conditional-get-handler) method request)
  (request-send-headers request
			:conditional t
			:last-modified (encode-universal-time 0 0 0 1 1 1990 0))
  (html-stream (request-stream request)
	       (gencontent "conditional get handler")))
(my-install-handler conditional-get-handler "/conditional-get")


#|  -------- url: /paramtest   ------------------  |#
(defclass paramtest-handler (araneida:handler) ())
(defmethod handle-request-response ((handler paramtest-handler) method request)
  (let ((params (url-query-alist (request-url request)))
	(foo (url-query-param (request-url request) "foo"))
	(foo-nocase (url-query-param (request-url request) "FOO" :case-sensitive nil))
	(foo-taint (tainted-url-query-param (request-url request) "foo"))
	(foo-taint-nocase (tainted-url-query-param (request-url request) "FOO" :case-sensitive nil)))
    (simplegen `(div
		 (p "foo-alist " ,(format nil (second (assoc "foo" params :test #'string-equal))))
		 (p "foo untaint case " ,(car foo))
		 (p "foo untaint nocase " ,(car foo-nocase))
		 (p "foo taint case " ,(untaint (lambda (x) x) (car foo-taint))) ; normally, this would check to make sure all the elements are something
		 (p "foo taint nocase " ,(untaint (lambda (x) x) (car foo-taint-nocase)))
		 (p "foo type taint case " ,(type-of (car foo-taint)))
		 (p "foo type taint nocase " ,(type-of (car foo-taint-nocase)))))))
(my-install-handler paramtest-handler "/paramtest" nil)


#|  -------- url: /with-paramtest   ------------------  |#
(defclass with-paramtest-handler (araneida:handler) ())
(defmethod handle-request-response ((handler with-paramtest-handler) method request)
  (with-url-params (foo bar) (request-url request)
    (simplegen `(div
		 (p "foo: " ,foo)
		 (p "bar: " ,(if bar bar "nil"))))))

(my-install-handler with-paramtest-handler "/with-paramtest" nil)

#|  -------- url: /with-tainted-paramtest   ------------------  |#
(defclass with-tainted-paramtest-handler (araneida:handler) ())
(defmethod handle-request-response ((handler with-tainted-paramtest-handler) method request)
  (with-tainted-url-params (foo bar) (request-url request)
    (simplegen `(div
		 (p "taint-foo: " ,(untaint #'identity foo))
		 (p "taint-bar: " ,(if bar (untaint #'identity bar) "nil"))))))

(my-install-handler with-tainted-paramtest-handler "/with-tainted-paramtest" nil)


#|  -------- url: /mime/text-plain   ------------------  |#
(defclass text-plain-handler (araneida:handler) ())
(defmethod handle-request-response ((handler text-plain-handler) method request)
  (send-file request "NEWS" :content-type "text/plain"))
(my-install-handler text-plain-handler "/mime/text-plain")


#|  -------- url: /authenticated/test   ------------------  |#
(defclass authenticated-test-handler (araneida:handler) ())
(defun authme (username password)
  (if (and (equal username "testuser")
	   (equal password "testpass"))
      1
      nil))
(defmethod handle-request-authentication ((handler authenticated-test-handler) method request)
  (funcall (araneida::make-basic-authentication-handler "testrealm" #'authme)
	   request nil))
(defmethod handle-request-response ((handler authenticated-test-handler) method request)
  (simplegen "authenticated test handler"))
(my-install-handler authenticated-test-handler "/authenticated/test")


#|  -------- url: /authenticated/test   ------------------  |#
(defclass authorization-test-handler (araneida:handler) ())
(defmethod handle-request-authorization ((handler authorization-test-handler) method request)
  (if (equal (first (request-header request :referer))
	     "http://example.com/foobar")
      t
      (signal 'http-unauthorized)))
(defmethod handle-request-response ((handler authorization-test-handler) method request)
  (simplegen "authorization test handler"))
(my-install-handler authorization-test-handler "/authorization/test")

#|  -------- url: /hier/*   ------------------  |#
(defclass hier-root-handler (araneida:dispatching-handler) ())
(defclass hier-foo-handler (araneida:handler) ())
(defclass hier-bar-handler (araneida:handler) ())

(attach-hierarchy (http-listener-handler *listener*) *url* *url*
		  ("/hier" hier-root-handler
			   ("/foo" hier-foo-handler)
			   ("/bar" hier-bar-handler)))

(defmethod handle-request-response ((handler hier-foo-handler) method request)
  (simplegen "hier foo handler"))
(defmethod handle-request-response ((handler hier-bar-handler) method request)
  (simplegen (format nil "hier bar handler: ~A." (urlstring (hier-bar-handler-url)))))


#|  -------- url: /hier-advanced/*   ------------------  |#
(defclass hieradv-root-handler (araneida:dispatching-handler) ())
(defclass hieradv-foo-handler (araneida:handler)
  ((moofoo :initarg :moofoo :reader moofoo)))
(defclass hieradv-bar-handler (araneida:handler) ())
(defmethod handle-request-response ((handler hieradv-foo-handler) method request)
  (simplegen (format nil "hier-advanced foo handler: ~A" (moofoo handler))))
(defmethod handle-request-response ((handler hieradv-bar-handler) method request)
  (simplegen "hier-advanced bar handler"))

(defclass hieradv-xbase-handler (araneida:handler)
  ((mim :initarg :mim :reader mim)
   (mimurl :initarg :mimurl :reader mimurl)))
(defmethod handle-request-response ((handler hieradv-xbase-handler) method request)
  (simplegen (format nil "hieradv xbase mim = ~A - ~A." (mim handler) (urlstring (mimurl handler)))))

(araneida:attach-hierarchy (http-listener-handler *listener*) *url* *url*
		  ("/hier-advanced/" hieradv-root-handler
				    ("foo" (hieradv-foo-handler (make-instance 'hieradv-foo-handler :moofoo "mooofooo")))
				    ("bar" hieradv-bar-handler)
				    (dolist (i '("yes" "no" "maybe" "so"))
				      (install-handler _handler
						       (make-instance 'hieradv-xbase-handler
								      :mim i
								      :mimurl _eurl)
						       i t))))

#|  -------- url: /parametermethod/basic   ------------------  |#
(defclass parametermethod-basic-handler (araneida:handler) ())

(defurlmethod basic-handler (handler method request
				     &key (greeting "Hello World!"))
  (declare (ignore handler method))
  (simplegen greeting))

(defmethod handle-request-response ((handler parametermethod-basic-handler) method request)
  (basic-handler handler method request))

(my-install-handler parametermethod-basic-handler "/parametermethod/basic" nil)

#|  -------- url: /parametermethod/taintedbasic   ------------------  |#
(defclass parametermethod-taintedbasic-handler (araneida:handler) ())

(deftaintedurlmethod taintedbasic-handler (handler method request
						   &key (greeting "Hello World!"))
  (declare (ignore handler method))
  (if (tainted-p greeting)
      (simplegen (untaint (lambda (x) x) greeting)) ; some day I'll put a real untaint function in here
      (simplegen "greeting was not tainted when it should've been!")))

(defmethod handle-request-response ((handler parametermethod-taintedbasic-handler) method request)
  (taintedbasic-handler handler method request))

(my-install-handler parametermethod-taintedbasic-handler "/parametermethod/taintedbasic" nil)


#|  -------- url: /parametermethod/funcparams   ------------------  |#
(defclass parametermethod-funcparams1-handler (araneida:handler) ())
(defclass parametermethod-funcparams2-handler (araneida:handler) ())

(defurlmethod funcparams-handler (handler method request
					  foo
					  &key (greeting "Hello World!"))
  (declare (ignore handler method))
  (simplegen (format nil "~A ~A" foo greeting)))

(defmethod handle-request-response ((handler parametermethod-funcparams1-handler) method request)
  (funcparams-handler handler method request "one"))
(defmethod handle-request-response ((handler parametermethod-funcparams2-handler) method request)
  (funcparams-handler handler method request "two"))

(my-install-handler parametermethod-funcparams1-handler "/parametermethod/funcparams1" nil)
(my-install-handler parametermethod-funcparams2-handler "/parametermethod/funcparams2" nil)

#|  -------- url: /parametermethod/specialize   ------------------  |#
(defclass parametermethod-specialize-handler (araneida:handler) ())

(defurlmethod specialize-handler (handler method request
					  &key (message "Hello yourself!"))
  (declare (ignore handler method))
  (simplegen message))

(defurlmethod specialize-handler (handler method request
					  &require (message (string-equal "goodbye")))
  (declare (ignore handler method message))
  (simplegen "The goodbye message!"))

(defmethod handle-request-response ((handler parametermethod-specialize-handler) method request)
  (specialize-handler handler method request))

(my-install-handler parametermethod-specialize-handler "/parametermethod/specialize" nil)

#|  -------- url: /parametermethod/noparams   ------------------  |#
(defclass parametermethod-noparams-handler (araneida:handler) ())

(defurlmethod noparams-handler (handler method request
					  &require foo)
  (declare (ignore handler method foo))
  (simplegen "noparam Foo provided"))

(defurlmethod noparams-handler (handler method request)					  
  (declare (ignore handler method))
  (simplegen "noparam No parameters given"))

(defmethod handle-request-response ((handler parametermethod-noparams-handler) method request)
  (noparams-handler handler method request))

(my-install-handler parametermethod-noparams-handler "/parametermethod/noparams" nil)

; These tests are done in reverse priority order. Makes it easier, actually.
; number of key parameters
#|  -------- url: /parametermethod/test1   ------------------  |#
(defclass parametermethod-prioritytest1-handler (araneida:handler) ())

(defurlmethod prioritytest1 (handler method request
				     &key (foo "foo") (bar "bar"))
  (declare (ignore handler method))
  (simplegen (format nil "test1 You should see this. Foo: ~A Bar: ~A" foo bar)))

(defurlmethod prioritytest1 (handler method request
				     &key (foo "foo"))
  (declare (ignore handler method))
  (simplegen (format nil "test1 You should NOT see this. Foo: ~A" foo)))

(defmethod handle-request-response ((handler parametermethod-prioritytest1-handler) method request)
  (prioritytest1 handler method request))
(my-install-handler parametermethod-prioritytest1-handler "/parametermethod/test1" nil)

; number of specialized requireds
#|  -------- url: /parametermethod/test2   ------------------  |#
(defclass parametermethod-prioritytest2-handler (araneida:handler) ())

(defurlmethod prioritytest2 (handler method request
				     &require (foo (string-equal "foo")) bar)
  (declare (ignore handler method))
  (simplegen (format nil "test2 You should see this. Foo: ~A Bar: ~A" foo bar)))

(defurlmethod prioritytest2 (handler method request
				     &require foo bar)
  (declare (ignore handler method))
  (simplegen (format nil "test2 When foo is \"foo\" you should NOT see this. Foo: ~A Bar: ~A" foo bar)))

(defmethod handle-request-response ((handler parametermethod-prioritytest2-handler) method request)
  (prioritytest2 handler method request))
(my-install-handler parametermethod-prioritytest2-handler "/parametermethod/test2" nil)

; number of requireds
#|  -------- url: /parametermethod/test3   ------------------  |#
(defclass parametermethod-prioritytest3-handler (araneida:handler) ())

(defurlmethod prioritytest3 (handler method request
				     &require foo bar)
  (declare (ignore handler method))
  (simplegen (format nil "test3 You should see this. Foo: ~A Bar: ~A" foo bar)))

(defurlmethod prioritytest3 (handler method request
				     &require foo
				     &key (bar "bozo"))
  (declare (ignore handler method))
  (simplegen (format nil "test3 When bar exists you should NOT see this. Foo: ~A Bar: ~A" foo bar)))

(defmethod handle-request-response ((handler parametermethod-prioritytest3-handler) method request)
  (prioritytest3 handler method request))
(my-install-handler parametermethod-prioritytest3-handler "/parametermethod/test3" nil)


; I managed to mess up parameter order once. This should make sure that things are working properly.
#|  -------- url: /parametermethod/paramorder   ------------------  |#
(defclass parametermethod-paramorder-handler (araneida:handler) ())

(defurlmethod paramorder (handler method request
				  &key (a "thedefault-a") (c "thedefault-c") (b "thedefault-b"))
  (declare (ignore handler method))
  (simplegen (format nil "paramorder a: ~A b: ~A c: ~A" a b c)))

(defmethod handle-request-response ((handler parametermethod-paramorder-handler) method request)
  (paramorder handler method request))
(my-install-handler parametermethod-paramorder-handler "/parametermethod/paramorder" nil)

#|  -------- url: /parametermethod/defaultkeyvalues   ------------------  |#
#|  -------- url: /parametermethod/defaultkeyvalues-tainted   ----------  |#
(defclass parametermethod-defaultkeyvalues-handler (araneida:handler) ())
(defclass parametermethod-defaultkeyvalues-tainted-handler (araneida:handler) ())

(defurlmethod defaultkeyvalues (handler method request
					&require r
					&key (a "thedefault-a") (c "thedefault-c") (b "thedefault-b"))
  (declare (ignore handler method))
  (simplegen (format nil "defaultkeyvalues r: ~A a: ~A b: ~A c: ~A" r a b c)))

(deftaintedurlmethod defaultkeyvalues-tainted (handler method request
						       &require r
						       &key (a "thedefault-a") (c "thedefault-c") (b "thedefault-b"))
  (declare (ignore handler method))
  (let ((r (untaint #'identity r))
	(a (untaint #'identity a))
	(c (untaint #'identity c))
	(b (untaint #'identity b)))
    (simplegen (format nil "defaultkeyvalues-tainted r: ~A a: ~A b: ~A c: ~A" r a b c))))

(defmethod handle-request-response ((handler parametermethod-defaultkeyvalues-handler) method request)
  (defaultkeyvalues handler method request))
(my-install-handler parametermethod-defaultkeyvalues-handler "/parametermethod/defaultkeyvalues" nil)

(defmethod handle-request-response ((handler parametermethod-defaultkeyvalues-tainted-handler) method request)
  (defaultkeyvalues-tainted handler method request))
(my-install-handler parametermethod-defaultkeyvalues-tainted-handler "/parametermethod/defaultkeyvalues-tainted" nil)

; check to make sure conditions are thrown when they should be
#|  -------- url: /parametermethod/conditioncheck   ------------------  |#
(defclass parametermethod-conditioncheck-handler (araneida:handler) ())

(defurlmethod fail-nomatch (handler method request
				    &require (dontmatchme (string-equal "dontmatchme")))
  (declare (ignore handler method request dontmatchme))
  "You will never see this.")

(defurlmethod fail-too-many-matches (handler method request
					     &key foo)
  (declare (ignore handler method request foo))
  "Match 1")

(defurlmethod fail-too-many-matches (handler method request
					     &key bar)
  (declare (ignore handler method request bar))
  "Match 2")


#+(and araneida-threads sb-thread) (defvar *my-mutex* (sb-thread:make-mutex :name "compile mutex"))
(let ((fail-unknown-keyword '(defurlmethod fail-unknown-keyword (handler method request
								 &require foo
								 &key bar
								 &quux quiver)
			      (declare (ignore handler method request))
			      "This shouldn't even compile"))
      (fail-function-parameter-mismatch '((defurlmethod fail-unknown-keyword (handler method request func1
								                      &require foo
								                      &key bar)
					    (declare (ignore handler method request func1 foo bar))
					    "This shouldn't even compile")
					  (defurlmethod fail-unknown-keyword (handler method request func1 func2
								                      &require foo
								                      &key bar)
					    (declare (ignore handler method request func1 func2 foo bar))
					    "This shouldn't even compile"))))
  (defmethod handle-request-response ((handler parametermethod-conditioncheck-handler) method request)
    (let (flag-nomatch flag-too-many-match flag-unknown-keyword flag-unknown-keyword-really-wrong flag-func-parameter-mismatch)
      (catch 'escape
	(handler-case (fail-nomatch handler method request)
	  (no-urlmethod () (throw 'escape nil)))
	(setf flag-nomatch t))
      (catch 'escape
	(handler-case (fail-too-many-matches handler method request)
	  (too-many-urlmethods-matched () (throw 'escape nil)))
	(setf flag-too-many-match t))
      (macrolet ((protect (&body body)
		   #+(and araneida-threads sb-thread) `(sb-thread:with-mutex (*my-mutex*) ,@body)
		   #-(and araneida-threads sb-thread) `(progn ,@body)))
	(protect
	 (catch 'escape
	   (handler-case (eval fail-unknown-keyword)
	     (urlmethod-unknown-keyword (c) (progn
					      (unless (equal (araneida::urlmethod-unknown-keyword-unknown c)
							     "&QUUX")
						(setf flag-unknown-keyword-really-wrong (araneida::urlmethod-unknown-keyword-unknown c)))
					      (throw 'escape nil))))
	   (setf flag-unknown-keyword t))
	 (catch 'escape
	   (handler-case (mapcar #'eval fail-function-parameter-mismatch)
	     (urlmethod-function-parameter-mismatch () (throw 'escape nil)))
	   (setf flag-func-parameter-mismatch t))))
      (if (or flag-nomatch
	      flag-too-many-match
	      flag-unknown-keyword
	      flag-unknown-keyword-really-wrong
	      flag-func-parameter-mismatch)
	  (if flag-unknown-keyword-really-wrong
	      (simplegen (format nil "Unknown keyword almost got it right, but was still horribly wrong: ~A"
				 flag-unknown-keyword-really-wrong))
	      (simplegen (format nil "Following failed: ~:[~;nomatch~] ~:[~;too-many-match~] ~:[~;unknown-keyword~] ~:[~;func-param-mismatch~]"
				 flag-nomatch flag-too-many-match flag-unknown-keyword flag-func-parameter-mismatch)))
	  (simplegen "all is well")))))
(my-install-handler parametermethod-conditioncheck-handler "/parametermethod/conditioncheck" nil)


#|  -------- url: /template   ------------------  |#
(deftemplate simple-span (contents)
  `(span ,contents))

(defclass simple-template (araneida:handler) ())
(defmethod handle-request-response ((handler simple-template) method request)
  (html-stream (request-stream request)
	       (simplegen (call-template 'simple-span "contents"))))
		 
(my-install-handler simple-template "/template")


#|  -------- url: /html/tag-basic   ------------------  |#
(defhtmltag coffee (attr content)
  (declare (ignore attr content))
  "c|_|")

(defhtmltag odd-even-list (attr content)
  `((ul ,@attr)
    ,@(let ((counter 0))
	(mapcar (lambda (elt)
		  (incf counter)
		  (destructure-html (tag attrs content) elt
		    `((,tag :class ,(if (oddp counter) "odd" "even") ,@attrs) ,@content)))
		content))))

(defclass html-basic-tag-handler (araneida:handler) ())
(defmethod handle-request-response ((handler html-basic-tag-handler) method request)
  (simplegen '(span "Mug of joe? " (coffee))))
(my-install-handler html-basic-tag-handler "/html/tag-basic")

(defclass html-advanced-tag-handler (araneida:handler) ())
(defmethod handle-request-response ((handler html-advanced-tag-handler) method request)
  (simplegen '(odd-even-list
	       (li "odd")
	       (li "even")
	       (li "odd")
	       (li "even"))))
(my-install-handler html-advanced-tag-handler "/html/tag-advanced")



#|  -------- url: /quit   ------------------  |#
(defclass quit-handler (araneida:handler) ())
(defmethod handle-request-response ((handler quit-handler) method request)
  (simplegen "quitting")
  (setf *quit* 1)
  (stop-listening *listener*))
(my-install-handler quit-handler "/quit")


#| :testend - the end of test servers |#
(start-listening *listener*)
(loop while (eql *quit* 0)
      do #+araneida-threads(sleep 1)
         #-araneida-threads(araneida::serve-event-http-listener-accept-one-request *listener*))
#+sbcl(sb-ext:quit)
#+cmu(extensions:quit)
