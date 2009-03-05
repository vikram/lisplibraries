;;;
;;; This file contains property of Mu Aps.
;;; Copyright (c) 2005.  All rights reserved.
;;;

(in-package :muproc-test)

(defun kill-all ()
  (mapcar (lambda (p) (ignore-errors (muproc-kill p :just-die))) (muproc-all-processes)))

(deftest two-instances-1
    ;; verify that we can have multiple instances of the server
    (flet ((fn ()
	     (unwind-protect
	       (muprocn (5 :timeout)
		 (dolist (name '(:test1 :test2))
		   (muproc-spawn name #'muproc-test.gensrv1:start (list :port-name name)
				 :errorstream *trace-output*))
		 (sleep .2)    ; wait until the servers are ready
		 (let ((key :key)
		       (val 38))
		   (muproc-test.gensrv1:store* :test1 key val)
		   (muproc-test.gensrv1:store* :test2 key (* val 2))
		   (list
		    (muproc-test.gensrv1:lookup* :test1 key)
		    (muproc-test.gensrv1:lookup* :test2 key))))
		 (kill-all))))
      (fn))
  (38 76))

(deftest two-instances-2
    ;; verify that the values inserted by initialize operation 
    ;; are registered correctly
    (flet ((fn ()
	     (unwind-protect
		  (muprocn (5 :timeout)
		    (dolist (name '(:test1 :test2))
		      (muproc-spawn name #'muproc-test.gensrv1:start (list :port-name name)
				    :errorstream *trace-output*))
		    (sleep .2)	    ; wait until the servers are ready
		    (let ((key :key)
			  (val 38))
		      (muproc-test.gensrv1:store* :test1 key val)
		      (muproc-test.gensrv1:store* :test2 key (* val 2))
		      (mapcar (lambda (l) (sort l #'> :key #'cdr)) ;; we need well-defined ordering
			      (list
			       (muproc-test.gensrv1:all-values* :test1)
			       (muproc-test.gensrv1:all-values* :test2)))))
		 (kill-all))))
      (fn))
  (((:defkey . 77) (:key . 38))
   ((:defkey . 77) (:key . 76))))

(deftest two-instances-3
    ;; check that initargs are passed correctly
    (flet ((fn ()
	     (unwind-protect
		  (muprocn (5 :timeout)
		    (muproc-spawn :test1 #'muproc-test.gensrv1:start 
				  (list :port-name :test1 
					:initargs  '(:init1 20))
				  :errorstream *trace-output*)
		    (muproc-spawn :test2 #'muproc-test.gensrv1:start
				  (list :port-name :test2 
					:initargs (list :init2 (/ 20 2)))
				  :errorstream *trace-output*)

		    (sleep .2)	    ; wait until the servers are ready
		    (let ((key :key)
			  (val 38))
		      (muproc-test.gensrv1:store* :test1 key val)
		      (muproc-test.gensrv1:store* :test2 key (* val 2))
		      (mapcar 
		       (lambda (l) (sort l #'> :key #'cdr)) ;; we need well-defined ordering
		       (list
			(muproc-test.gensrv1:all-values* :test1)
			(muproc-test.gensrv1:all-values* :test2)))))
	       (kill-all))))
      (fn))
  (((:key . 38) (:init1 . 20))
   ((:key . 76) (:init2 . 10))))

(deftest default-instance-1
    ;; verify that default server instance operators work correctly
    (let ((instances '(:before :test1 :after)))
      (flet ((fn ()
	       (unwind-protect
		    (muprocn (5 :timeout)
		      (loop for nm in instances
			    for count from 1
			    do (muproc-spawn nm #'muproc-test.gensrv1:start
					     (list :port-name nm
						   :initargs (list nm count))
					     :errorstream *trace-output*))
		      (sleep .2)    ; wait until the servers are ready
		      (let ((key :key)
			    (val 38))
			(loop for nm in instances
			      for count from 1
			      do (muproc-test.gensrv1:store* nm key (* count val)))
			;; now use default server instance
			(muproc-test.gensrv1:store :in-default 999)
			;; collect state of each server instance
			(loop for nm in instances
			      collect (sort (muproc-test.gensrv1:all-values* nm)
					    #'> :key #'cdr)) ;; we need well-defined ordering
			))
		   (kill-all))))
	(fn)))
  (((:key . 38) (:before . 1))
   ((:in-default . 999) (:key . 76) (:test1 . 2))
   ((:key . 114) (:after . 3))))

(deftest default-instance-2
    (let ((instances '(:before :test1 :after)))
      (flet ((fn ()
	       (unwind-protect
		    (muprocn (5 :timeout)
		      (loop for nm in instances
			    for count from 1
			    do (muproc-spawn nm #'muproc-test.gensrv1:start
					     (list :port-name nm
						   :initargs (list nm count))
					     :errorstream *trace-output*))
		      (sleep .2)    ; wait until the servers are ready
		      (let ((key :key)
			    (val 38))
			(loop for nm in instances
			      for count from 1
			      do (muproc-test.gensrv1:store* nm key (* count val)))
			;; now use default server instance
			(muproc-test.gensrv1:store :in-default 999)
			;; collect state of each server instance
			(loop for nm in instances
			      collect (sort (muproc-test.gensrv1:all-values* nm)
					    #'> :key #'cdr)) ;; we need well-defined ordering
			))
		 (kill-all))))
	(fn)))
  (((:key . 38) (:before . 1))
   ((:in-default . 999) (:key . 76) (:test1 . 2))
   ((:key . 114) (:after . 3))))

(deftest cast-1
    ;; that we can use a server asynchronously
    (let ((instances '(:before :test1 :after)))
      (flet ((fn ()
		 (unwind-protect
		      (muprocn (5 :timeout)
			(loop for nm in instances
			      for count from 1
			      do (muproc-spawn nm #'muproc-test.gensrv1:start 
					       (list :port-name nm
						     :initargs (list nm count))))
			(sleep .2)  ; wait until the servers are ready
			(let ((key :key)
			      (val 38))
			  (loop for nm in instances
				for count from 1
				do (muproc-test.gensrv1:cast-store* nm key (* count val)))
			  ;; now use default server instance
			  (muproc-test.gensrv1:cast-store :in-default 999)
			  ;; collect state of each server instance
			  (loop for nm in instances
				collect (sort (muproc-test.gensrv1:all-values* nm)
					      #'> :key #'cdr)) ;; we need well-defined ordering
			  ))
		   (kill-all))))
	(fn)))
  (((:key . 38) (:before . 1))
   ((:in-default . 999) (:key . 76) (:test1 . 2))
   ((:key . 114) (:after . 3))))

(deftest port-name-1
    ;; that we can specify a different port-name for a server
    (let ((instances '(:before :test1 :after)))
      (sleep 1) ;; wait a bit
      (flet ((fn ()
		 (unwind-protect
		      (muprocn (5 :timeout)
			(loop for nm in instances
			      for count from 1
			      do (muproc-spawn nm #'muproc-test.gensrv1:start
					       (list :port-name (intern (format nil "PORT-~d" count)
									:keyword)
						     :initargs (list nm count))))
			(sleep .2)  ; wait until the servers are ready
			(let ((key :key)
			      (val 38))
			  (loop for nm in instances
				for count from 1
				do (muproc-test.gensrv1:cast-store* nm key (* count val)))
			  ;; now use default server instance
			  (muproc-test.gensrv1:cast-store :in-default 999)
			  ;; collect state of each server instance
			  (list
			   (loop for nm in instances
				 collect (muproc-test.gensrv1:port-name* nm))
			   (loop for nm in instances
				 collect (sort (muproc-test.gensrv1:all-values* nm)
					       #'> :key #'cdr))	;; we need well-defined ordering
			   )))
		   (kill-all))))
	(fn)))
  ((:port-1 :port-2 :port-3)
   (((:key . 38) (:before . 1))
    ((:in-default . 999) (:key . 76) (:test1 . 2))
    ((:key . 114) (:after . 3)))))

(deftest handle-term-1
    ;; verify that term/reason messages can be handled properly
    (labels ((term (id pid)
	       (muproc-link pid)
	       (muproc-exit id))
	     (fn ()
	       (unwind-protect
		    (muprocn (5 :timeout)
		      (let ((pid (muproc-spawn :test1 #'muproc-test.gensrv1:start
					       (list :port-name :test1))))
			(progn
			  (sleep .2)
			  (dolist (id '(one two three))
			    (muproc-spawn id #'term (list id pid)))
			  (sleep .5)
			  (sort (muproc-test.gensrv1:exits) #'string<))))
		 (kill-all))))
      (fn))
  (one three two))

