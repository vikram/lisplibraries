;;; -*- Lisp -*-
(defpackage :araneida-system (:use #:asdf #:cl))
(in-package :araneida-system)

;;; Define whether this lisp supports araneida threads

#+(or sb-thread allegro armedbear openmcl lispworks)
(pushnew :araneida-threads *features*)

#+(or sbcl cmu clisp digitool)
(pushnew :araneida-serve-event *features*)

(defsystem araneida
  :name "araneida"
  :perform (load-op :after (op araneida)
		    (pushnew :araneida cl:*features*))
  :depends-on (net-telent-date split-sequence
			       #+sbcl sb-bsd-sockets
			       #+#.(cl:if (asdf:find-system :parenscript cl:nil) '(and) '(or)) parenscript)
;  :weakly-depends-on (parenscript) ; whenever weakly-depends-on is merged in.... -- Alan Shields [18 November 2005]
  :version "0.90"
  :maintainer "Alan Shields <Alan-Shields@omrf.ouhsc.edu>"
  :components ((:file "taint")
	       (:file "defpackage" :depends-on ("taint"))
               (:module "utility"
                        :components ((:file "aif")
                                     (:file "lists")
                                     (:file "split")
                                     (:file "seqlet")
                                     (:file "base64" :depends-on ("seqlet"))
                                     (:file "strings")
				     (:file "rfc2109"))
			:depends-on ("defpackage"))
               (:module "compat"
                        :components
                        ((:file #+sbcl "compat-sbcl"
                                #+openmcl "compat-openmcl"
                                #+digitool "compat-mcl"
                                #+allegro "compat-allegro"
                                #+armedbear "compat-abcl"
                                #+cmu "compat-cmucl"
                                #+clisp "compat-clisp"
                                #+lispworks "compat-lispworks"))
                        :depends-on ("defpackage"))
	       (:module "doc"
			:components ((:html-file "examples")
				     (:static-file "example" 
						   :pathname "example.lisp")
				     (:html-file "faq")
				     (:html-file "handlers")
				     (:html-file "html")
				     (:html-file "index")
				     (:html-file "installation")
				     (:html-file "reference")
				     (:html-file "servers")
				     (:html-file "troubleshooting")
				     (:html-file "urls")
				     (:static-file "new-dispatch-model")
				     (:static-file "araneida"
						   :pathname "araneida.css")
				     (:static-file "PLAN")))
	       (:module "araneida-repl"
			:components ((:file "defpackage")
				     (:file "handler-utils" :depends-on ("defpackage")))
			:depends-on ("defpackage"))
	       (:module "main"
			:pathname ""
			:components ((:file "araneida")
				     (:file "macros")
				     (:file "html" :depends-on ("macros"))
				     (:file "templates")
				     (:file "url-class")
				     (:file "url" :depends-on ("url-class" "macros"))
				     (:file "exports"
					    :depends-on ("url" "handler"))
				     (:file "request-class")
				     (:file "request" :depends-on ("url" "request-class" "araneida" "exports" "html"))
				     (:file "auth" :depends-on ("request"))
				     (:file "define-page" :depends-on ("request"))
				     (:file "handler")
				     (:file "redirect-handler" :depends-on ("handler" ))
				     (:file "daemon"
					    :depends-on
					    ("url" "handler" "exports" "request"))
				     (:file "memoization")
				     (:file "http-error")
				     (:file "http-listener-class" :depends-on ("daemon"))
				     (:file "http-listener" :depends-on ("http-listener-class"))
				     #+araneida-serve-event
				     (:file "serve-event-http-listener"
					    :depends-on ("http-listener"))
				     (:file "static-file-handler" :depends-on ("handler" "request"))
				     (:file "file-request" :depends-on ("static-file-handler"))
				     #+araneida-threads
				     (:file "threaded-http-listener" :depends-on ("http-listener"))
				     (:file "reverse-proxy-listener" )
				     (:file "https-listener-class" 
					    :depends-on ("reverse-proxy-listener"))
				     ;; this file isn't used?
				     #+nil (:file "client" :depends-on ("request" "url"))
				     (:file "pattern-match" :depends-on ("memoization"))
				     (:static-file "NEWS")
				     ;; new and wonderful things!
				     (:file "convenience-support" :depends-on ("macros"))
				     (:file "convenience" :depends-on ("macros" "convenience-support"))
				     (:file "defurlmethod" :depends-on ("macros" "url")))
			:depends-on ("compat" "defpackage" "utility"))))

#||
(defsystem araneida-examples
  :depends-on (araneida)
  :components ((:file "defpackage")
               (:file "session")
               (:file "variables" :depends-on ("defpackage"))               
	       (:file "per-host" :pathname #.*per-host-configuration*
		      :depends-on ("variables"))
               (:file "main" :depends-on ("defpackage" "per-host"))))
||#
