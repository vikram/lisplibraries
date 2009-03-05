(in-package #:cl-user) 
(defpackage #:run-tests (:use #:cl))
(in-package #:run-tests)

(require 'rt)
(load "kmrcl.asd")
(load "kmrcl-tests.asd")
(asdf:oos 'asdf:test-op 'kmrcl)

(defun quit (&optional (code 0))
  "Function to exit the Lisp implementation. Copied from CLOCC's QUIT function."
    #+allegro (excl:exit code)
    #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
    #+(or cmu scl) (ext:quit code)
    #+cormanlisp (win32:exitprocess code)
    #+gcl (lisp:bye code)
    #+lispworks (lw:quit :status code)
    #+lucid (lcl:quit code)
    #+sbcl (sb-ext:quit :unix-status (typecase code (number code) (null 0) (t 1)))
    #+mcl (ccl:quit code)
    #-(or allegro clisp cmu scl cormanlisp gcl lispworks lucid sbcl mcl)
    (error 'not-implemented :proc (list 'quit code)))

(quit)
