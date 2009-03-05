(in-package :araneida)

(defun forcibly-close-stream (s)
  (let ((fd (sb-sys:fd-stream-fd s)))
    (multiple-value-bind (r e) (ignore-errors (close s) t)
      (unless r
	(format t "Unable to close fd ~A: ~A, trying harder ~%" fd e)
	(multiple-value-bind (r e) (ignore-errors (close s :abort t) t)
	  (unless r
	    (format t "still unable to close ~A: ~A, try harder ~%" fd e)
	    (multiple-value-bind (r e)
		(ignore-errors (sb-unix:unix-close fd) t)
	      (unless r
		(format t "Even unix-close failed on ~A:~A, giving up~%"
			fd e)))))))))
    
