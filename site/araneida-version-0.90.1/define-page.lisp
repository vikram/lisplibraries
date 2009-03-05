(in-package :araneida)

#|
(mapcar (l xml-for-function foo bar _ baz) (my-list-of-stuff))
=>
(mapcar (lambda (_) (xml-for-function foo bar _ baz)) (my-list-of-stuff))
|#

(defmacro l ((name &rest args))
  "Expands to an anonynmous function with one argument _ that calls NAME with whatever arguments are supplied - which may include _.  For example, (mapcar (l xml-for-function foo bar _ baz) (my-list-of-stuff)) => (mapcar (lambda (_) (xml-for-function foo bar _ baz)) (my-list-of-stuff))"
  `(lambda (_) (,name ,@args)))


;;; we don't want to evaluate the user-supplied form unless we have to.
;;; we don't know until page access time whether we have to.  So, we
;;; compile the supplied forms into a function: at runtime we choose
;;; whether or not to evaluate that function

(defun maybe-output-func (request last-modified headers output-form)
  (let ((if-mod-since (request-if-modified-since request 0)))
    (cond ((< if-mod-since last-modified)
	   (let ((o (funcall output-form)))
	     (apply #'request-send-headers
		    request :last-modified last-modified headers)
	     (html-stream (request-stream request) o)))
          (t
           (request-send-headers
            request :response-code 304 :response-text "Not modified")))))

(defmacro define-page ((url
                        &key patterns variable-lookup-fn changed-time headers)
                       &rest forms)
  (let* ((page-compile-time (get-universal-time))
         (mod-time-form
          (if changed-time
              `(max ,page-compile-time ,changed-time)
            page-compile-time)))
    `(progn
       (export-handler
        ,url
        (lambda (request rest-of-url) 
          (declare (ignorable request rest-of-url))
          (labels ((page ()
                     (rewrite-tree (progn ,@forms) ,patterns
                                   :variable-lookup-fn ,variable-lookup-fn)))
            (maybe-output-func request ,mod-time-form ,headers #'page)))))))

