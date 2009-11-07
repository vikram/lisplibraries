(in-package #:anardb.test)

(stefil:deftest test-linked-list-node-consistent (&optional (ll (make-linked-list)))
  (let ((oid (anardb:store-object-id ll)))
    (stefil:is (eq (chunk-list (linked-list-chunk ll)) ll))
    (stefil:is (eq ll (retrieve-instance-by-id 'linked-list oid)))
    (stefil:is (member ll (find-linked-list :name (linked-list-name ll))))))

(stefil:deftest make-linked-list (&rest args)
  (let ((ll (apply 'make-instance 'linked-list args)))
    (test-linked-list-node-consistent ll)
    ll))

(stefil:deftest test-store-wipe ()
  (anardb::store-wipe *store*)
  (setf (anardb::store-version *store*) 0)

  (stefil:is (not (retrieve-all-instances 'linked-list))))

(stefil:deftest test-store-save ()
  (test-store-wipe)
  (let ((names '(a b c d e f)))
    (loop for n in names
	  do (make-linked-list :name n))
    (flet ((check-present ()
	     (stefil:is (not (set-difference names (mapcar 'linked-list-name (retrieve-all-instances 'linked-list)))))
	     (loop for n in names
		   do (stefil:is (eq n (linked-list-name (first (find-linked-list :name n)))))))
	   (check-absent ()
	     (loop for n in names do (stefil:is (not (find-linked-list :name n))))))
      (check-present)
      (test-fresh-store)
      (check-present)
      (with-transaction ())
      (check-present)
      (test-store-wipe)
      (check-absent)
      (with-transaction ())
      (check-present))))


(stefil:deftest test-transaction-restart () 
  (with-transaction ()
    (anardb::store-wipe *store*))
  
  (let ((names '(a b c d e f)))
    (loop for n in names
	   do 
	  (let ((restarts 10))
	    (with-transaction ()
	      (stefil:is (not (find-linked-list :name n)))
	      (make-linked-list :name n)
	      (stefil:is (find-linked-list :name n))
	      (stefil:is (not (rest (find-linked-list :name n))))
	      (unless (zerop (decf restarts))
		(anardb:restart-transaction)))))))



(stefil:deftest test-transaction-restart-versus-drop () 
  (declare (optimize debug safety))
  (with-transaction ()
    (anardb::store-wipe *store*))
  (let ((name 'test-transaction-restart-versus-drop))
    (labels (
	     (fetch ()
	       (find-linked-list :name name))
	     (present (message)
	       (stefil:is (fetch) message))
	     (absent (message)
	       (stefil:is (not (fetch)) message)))
      (macrolet ((with-one-restart-transaction (args &body body)
		   (alexandria:with-unique-names (once)
		     `(let (,once)
			(with-transaction ,args
			  (multiple-value-prog1 (locally ,@body)
			    (unless ,once
			      (setf ,once t)
			      (restart-transaction))))))))
	(absent "first")
	(with-one-restart-transaction ()
	  (absent "before")
	  (make-linked-list :name name)
	  (present "after"))
	(present "after out")
	(with-one-restart-transaction ()
	  (present "before drop")
	  (drop (fetch))
	  (absent "after drop"))
	(absent "after drop out")
	(with-one-restart-transaction ()
	  (absent "after drop out again"))))))
