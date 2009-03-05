;;;; -*- lisp -*-

(in-package :it.bese.yaclml.test)

(def-suite :it.bese.yaclml.tal :in :it.bese.yaclml)

(in-suite :it.bese.yaclml.tal)

(defparameter *test-generator*
  (make-instance 'file-system-generator
   :root-directories (list (make-pathname :directory (pathname-directory *load-truename*)
			                  :name nil :type nil :version :newest)
			   (make-pathname :directory (append (pathname-directory *load-truename*) (list "root-a"))
					  :name nil :type nil :version :newest)
			   (make-pathname :directory (append (pathname-directory *load-truename*) (list "root-b"))
					  :name nil :type nil :version :newest))))

(defun tal-compile&run (tal-string env)
  (with-output-to-string (*yaclml-stream*)
    (funcall (yaclml::compile-tal-string tal-string (find-package :it.bese.yaclml.test)) env *test-generator*)))

(test non-tal
  (is (string= "<a/>" (tal-compile&run "<a/>" nil)))
  (is (string= "<a><span/></a>" (tal-compile&run "<a><span/></a>" nil)))
  (is (string= "<pre>
  one

two
</pre>"
		   (tal-compile&run "

<pre>
  one

two
</pre>" nil))))

(test tal-when
  (is (string= "<a href=\"4\">OK1</a>" (tal-compile&run "<a xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\" href=\"4\" tal:when=\"(and (princ $c) $c)\">OK1</a>"
                                                        '((c . t)))))
  (is (string= "<a href=\"4\">OK2</a>" (tal-compile&run "<a xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\" href=\"4\" tal:when=\"(not $c)\">OK2</a>"
                                                         '((c . nil)))))
  (is (string= "<a href=\"4\">OK3</a>" (tal-compile&run "<a xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\" href=\"4\" tal:when=\"(not $c)\">OK3</a>"
							     nil))))

(test tal-replace
  (is (string= "OK!" (tal-compile&run "<a href=\"4\" xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\" tal:replace=\"$c\"></a>"
                                      '((c . "OK!"))))))

(test tal-content
  (is (string= "<a>OK!</a>" (tal-compile&run "<a xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\" tal:content=\"$c\">BAD BAD BAD</a>"
						 '((C . "OK!"))))))

(test tal-include
  (is (string= "FOO" 
	       (with-output-to-string (*yaclml-stream*)
		 (funcall (load-tal *test-generator* "inc1.tal") '((TO-INCLUDE . "inc2.tal")) *test-generator*))))
  (is (string= "foo is FOOPARAM, and bar is BARINCLUDED"
	       (with-output-to-string (*yaclml-stream*)
		 (funcall (load-tal *test-generator* "inc3.tal") '((FOO . "FOOPARAM")) *test-generator*))))
  (is (string= "OK"
	       (with-output-to-string (*yaclml-stream*)
		 (funcall (load-tal *test-generator* "inc6.tal") '() *test-generator*)))))

(test tal-env
  (let* ((e (make-tal-environment (cons 'foo 4)))
         (e1 (make-tal-environment (cons 'foo 5))))
    (is (= 4 (lookup-tal-variable 'foo e)))
    (is (= 5 (lookup-tal-variable 'foo e1)))
    (is (= 4 (lookup-tal-variable 'foo (extend-environment e e1))))
    (is (= 5 (lookup-tal-variable 'foo (extend-environment e1 e))))))
