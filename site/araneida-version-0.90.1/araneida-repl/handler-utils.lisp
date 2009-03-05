(in-package :araneida-repl)

(defun show-handler-hierarchy (handler &optional (stream *standard-output*) (depth 0))
  "Prints to stream a rough description of the handlers below this one. * on the end indicates an inexact match"
  (declare (type handler handler)
	   (type stream stream))
  (when (typep handler 'dispatching-handler)
    (map nil (lambda (subhandler)
					; each handler in the child-handlers looks like: ("urlportion" handler handler)
					; where there is one handler. If it's in the first position, it's inexact, if it's
					; in the second, it's exact
	       (let ((match (first subhandler))
		     (match-handler (or (second subhandler) (third subhandler))))
		 (loop for i below depth do (princ "   " stream))
		 (format stream "~A~:[~;*~] ~7,8@T(~A)~%"
			 match
			 (null (third subhandler))
			 (class-name (class-of match-handler)))
		 (show-handler-hierarchy match-handler stream (1+ depth))))
	 (child-handlers handler))))
  

(defun show-listener-handlers (http-listener &optional (stream *standard-output*))
  "Takes an http-listener and prints out all of the handlers contained within it"
  (declare (type http-listener http-listener)
	   (type stream stream))
  (show-handler-hierarchy (http-listener-handler http-listener) stream))
