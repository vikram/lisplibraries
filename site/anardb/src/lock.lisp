(in-package #:anardb)

(defun file-lock-filename (filename)
  (with-standard-io-syntax
    (format nil "~A.lock" filename)))

(defun file-locked-p (filename)
  (not (not (probe-file (file-lock-filename filename)))))

(defun stream-lock-pid (stream)
  (parse-integer (read-line stream)))

(defun file-lock-pid (filename)
  (let ((lock (file-lock-filename filename)))
    (with-open-file (stream lock)
      (stream-lock-pid stream))))
  
(defun file-lock-take (filename)
  (let ((lock (file-lock-filename filename)))
    (let ((stream (open lock :direction :output :if-exists :error :if-does-not-exist :create)) success)
      (unwind-protect
	   (progn
	     (princ (getpid) stream)
	     (fresh-line stream)
	     (fsync stream)

	     (setf success t))
	(unless success
	  (close stream :abort t)))
      (when success
	(assert (file-locked-by-this-process-p filename))
	stream))))

(defun file-locked-by-this-process-p (filename)
  (ignore-errors (eql (file-lock-pid filename) (getpid))))

(defun file-lock-drop (lock)
  (let ((pathname (pathname lock)))
    ;; Strangely have to use pathname because of https://bugs.launchpad.net/sbcl/+bug/406271
    (delete-file pathname))
  (ignore-errors (close lock))
  t)

(defun file-lock-decayed-p (filename)
  (let ((real-filename (ignore-errors (truename (file-lock-filename filename))))
	(owner-pid (ignore-errors (file-lock-pid filename))))
    (cond ((or (not real-filename) (not owner-pid))
	   t)
	  (t
	   (let (open-files)
	     (when (ignore-errors (setf open-files (files-open-for-pid owner-pid)) t)
	       (not
		(loop for file in open-files thereis
		      (equal (ignore-errors (truename file)) real-filename)))))))))

(defvar *check-break-delay* 1)
(defvar *hard-break-delay* 4000)

(defun file-lock-wait-then-maybe-break (filename 
				 &key 
				 (wait-delay (+ 0.1 (random 2.0)))
				 (break-delay *check-break-delay*)
				 (hard-break-delay *hard-break-delay*))
  (flet ((break-go ()
	   (when
	       (and (ignore-errors
		      (delete-file (file-lock-filename filename))
		      t))
	     (return-from file-lock-wait-then-maybe-break t))))
  (let (pid time write-time)
    (loop until (not (file-locked-p filename)) do
	  (let ((new-pid (ignore-errors (file-lock-pid filename)))
		(new-time (get-universal-time))
		(new-write-time (ignore-errors (file-write-date (file-lock-filename filename)))))
	    (cond 
	      ((or (not (and time new-time))
		   (not (and (eql pid new-pid)
			     (eql new-write-time write-time))))
	       (setf time new-time
		     pid new-pid
		     write-time new-write-time))
	      
	      ((> (- new-time time) hard-break-delay)
	       (warn "Breaking ACTIVE LOCK on ~A after ~D seconds of inactivity"
		     filename hard-break-delay)
	       (break-go))
	      ((and (> (- new-time time) break-delay) (file-lock-decayed-p filename))
	       (warn "Breaking old lock on ~A" filename)
	       (break-go))))
	  (sleep wait-delay))))
  (values))


(defmacro with-file-lock ((filename) &body body)
  (alexandria:with-unique-names (lock)
    `(let (,lock)
       (unwind-protect 
	    (progn 
	      (setf ,lock (file-lock-take ,filename))
	      (assert ,lock)
	      (when ,lock
		(locally ,@body)))
	 (when ,lock
	   (file-lock-drop ,lock))))))

  