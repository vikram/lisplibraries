(in-package #:anardb)

(defun getpid ()
  #.(or
     #+allegro `(excl.osi:getpid)
     #+sbcl (sb-posix:getpid)
     #+lispworks (system::getpid)
     #+ccl (ccl::getpid)
     #+clisp (system:process-id)
     `(parse-integer (first (last (pathname-directory (truename "/proc/self")))))))

(defun files-open-for-pid (pid)
  (loop for file in (directory (format nil "/proc/~D/fd/*" pid))
	for truename = (ignore-errors (truename file))
	when truename
	collect truename))

(define-condition fsync-error (file-error) ((original-error :initarg :original-error)))

(defun fsync (stream)
  (finish-output stream)
  (handler-case
      #.(or
	    #+(and allegro (not windows)) `(excl.osi:fsync stream)
	    #+sbcl `(sb-posix:fsync (sb-sys:fd-stream-fd stream))
	    #+ccl `(progn) ; finish-output already calls fsync
	    `(warn "This Lisp does not have fsync support; may not have safely written file: ~A" stream))
      (error (e)
	(error 'fsync-error :original-error e :pathname (pathname stream)))))


