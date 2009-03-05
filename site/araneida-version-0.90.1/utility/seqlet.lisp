(in-package :araneida)

(defmacro seqlet (binding-forms &body body)
  "Perform each of BINDING-FORMS sequentially in a lexical environment
in which all the variables in BINDING-FORMS are visible.  Stop if any
binding evaluates to NIL, setting all subsequent variables to NIL.
Evaluate the body in this environment"
  (let ((vars (mapcar #'car binding-forms)))
    (labels ((foo (forms)
                  (if forms
                      `((setf ,(caar forms) ,(cadar forms))
                        (when ,(caar forms) ,@(foo (cdr forms))))
                    '())))
      `(let ,vars
         ,@(foo binding-forms)
         ,@body))))

