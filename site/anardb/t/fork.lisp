(in-package #:anardb.test)

(stefil:deftest create-long-list (&key (name 'many-transactions) (num-transactions 10))
  (declare (optimize debug safety))
  (with-transaction ()
    (drop (find-linked-list :name name)))
  (stefil:is (not (find-linked-list :name name)) "Drop failed on list ~A!" name)

  (let (once)
    (with-transaction ()
      (unless once
	(setf once t)
	(restart-transaction))
      (stefil:is (not (find-linked-list :name name)))))
  
  (flet ((check-our-list-okay (n &key (state :before))
	   (loop for node = (first (find-linked-list :name name :value 0)) then (linked-list-next node)
		 for i below n
		 do 
		 (let ((others (find-linked-list :name name :value i)))
		   (stefil:is (not (rest others)))
		   (stefil:is (eq (first others) node)))
		 (stefil:is (equal name (linked-list-name node)))
		 (test-linked-list-node-consistent node)
		 (stefil:is (= (linked-list-value node) i))
		 (stefil:is (= (linked-list-value (linked-list-prev node)) (mod (1- i) n))
			    "Linked list is not well linked ~A ~A: n=~D i=~D ~A"
			    state
			    name
			    n
			    i
			    (loop repeat (1+ n)
				  for n = node then (linked-list-prev n)
				  collect (linked-list-value n)))
		 (stefil:is (= (linked-list-value (linked-list-next node)) (mod (1+ i) n))))))
    (loop 
	  for n below num-transactions
	  for new = 
	  (with-transaction () 
	    (let ((new (make-linked-list :name name :value n)))
	      (check-our-list-okay (1+ n) :state :first)
	      new)) 
	  then 
	  (with-transaction ()
	    (check-our-list-okay n)
	    (let ((last (first (find-linked-list :name name :value (1- n)))))
	      (let ((new (make-linked-list :name name :value n :next (linked-list-next last) :prev last)))
		(setf (linked-list-next (linked-list-prev new)) new
		      (linked-list-prev (linked-list-next new)) new)
		(check-our-list-okay (1+ n) :state :after)
		(maybe-cause-trouble)
		new)))
	do 
	  (stefil:is (eq new (first (find-linked-list :name name :value n))))
	  (sleep (random 1.0))
	  (stefil:is (= (1+ n) (length (find-linked-list :name name)))))))
	
(defparameter *maybe-cause-trouble-sleep* nil)
(defparameter *maybe-cause-trouble-restart* 5)
(defparameter *maybe-cause-trouble-crash* nil)

(defun maybe-cause-trouble ()
  (flet ((inform (message)
	   (format *error-output* "~%~%Causing trouble: ~D: ~A~%~%" (anardb::getpid) message)
	   (finish-output *error-output*))
	 (occur (num)
	   (and num (zerop (random num)))))
    (when (occur *maybe-cause-trouble-sleep*)
      (inform "SLEEPING")
      (sleep 300))
    (when (occur *maybe-cause-trouble-restart*)
      (inform "RESTART TRANSACTION")
      (invoke-restart (find-restart 'restart-transaction)))
    (when (occur *maybe-cause-trouble-crash*)
      (inform "CRASHING!")
      (pprint stefil:*test-result-history*)
      (finish-output)
      #+allegro (excl:exit 1 :no-unwind t)
      #+sbcl (sb-ext:quit :recklessly-p t :unix-status 1)
      (error "failed to crash"))))

(stefil:deftest tiny-transaction (&key (num-transactions 100))
  (loop repeat num-transactions do
	(sleep (random 1.0))
	(with-transaction ()
	  (maybe-cause-trouble))))

(stefil:deftest modify-and-check-consistency (&key (num-transactions 10) (aref-index (random +chunk-array-length+)))
  (let ((old-table (make-hash-table)) new-table)
    (loop repeat num-transactions
	  do 
	  (sleep (random 1.0))
	  (with-transaction ()
	   (maybe-cause-trouble)
	   (setf new-table (make-hash-table))
	   (let ((cur-table (alexandria:copy-hash-table old-table)))
	     (loop for node in (retrieve-all-instances 'linked-list)
		   do 
		   (test-linked-list-node-consistent node)
		   (symbol-macrolet ((val (aref (chunk-array (linked-list-chunk node)) aref-index)))
		     (let ((old (gethash (store-object-id node) cur-table)))
		       (cond (old
			      (stefil:is (= val old) "The value ~A of element ~D in node ~A/~A (~A) unexpectedly changed from ~A"
					 val aref-index (linked-list-name node) (linked-list-value node) (store-object-id node) old)
			      (remhash (store-object-id node) cur-table))
			     (t (stefil:is (not (gethash (store-object-id node) old-table))))))
		     (let ((new-val (random most-positive-single-float)))
		       (setf val new-val)
		       (setf (gethash (store-object-id node) new-table) new-val))))
	     (stefil:is (not (alexandria:hash-table-values cur-table))))
	   (maybe-cause-trouble))
	  (setf old-table new-table))))

#-(and) ( 	(*maybe-cause-trouble-sleep* 1000)
		(*maybe-cause-trouble-restart* 100)
		(*maybe-cause-trouble-crash* 1000))

(defun run-one-fork-test (test-name &rest args)
  (let ((*maybe-cause-trouble-sleep* 100)
	(*maybe-cause-trouble-restart* 100)
	(*maybe-cause-trouble-crash* 100)

	(anardb::*hard-break-delay* 10)
	(anardb::*check-break-delay* 1))
    (pprint (apply test-name args))
    (pprint stefil:*test-result-history*)))


#+allegro
(defun print-backtrace-and-restart-transaction (e &optional *debugger-hook*)
  (with-standard-io-syntax
    (let ((*print-readably* nil)
	  (*print-miser-width* 40)
	  (*print-pretty* nil)
	  (tpl:*zoom-print-circle* t)
	  (tpl:*zoom-print-level* nil)
	  (tpl:*zoom-print-length* nil))
      (format *error-output* "~
~@<Error:~3I ~a~I~:@>~%~%"
	      e)
      (flet ((zoom (s)
	       (let ((*terminal-io* s)
		     (*standard-output* s))
		 (tpl:do-command :zoom
		   :from-read-eval-print-loop nil
		   :count t :all nil))))
	(zoom *error-output*))))
  (finish-output *error-output*)
  (typecase e
    (excl:interrupt-signal (excl:exit 2 :no-unwind t)))

  (let ((restart (find-restart 'restart-transaction)))
    (when restart (invoke-restart restart))))

(defun fork (test-name &rest args)
  #+allegro 
  (progn
    (let ((child (excl.osi:fork)))
      (when (zerop child)
	(let ((*debugger-hook* #'print-backtrace-and-restart-transaction))
	  (handler-bind ((error (lambda (e) (invoke-debugger e))))
	    (apply 'run-one-fork-test test-name args)))
	(excl:exit 0 :no-unwind t)))))

(defun run-fork-test (&optional (num 3))
  (test-fresh-store)
  (loop for i below num
	do (fork 'tiny-transaction))
  (loop for i below num
	do (fork 'create-long-list :name i))
  (loop for i below num
	do (fork 'modify-and-check-consistency :aref-index i))

  #+allegro (excl.osi:wait))
