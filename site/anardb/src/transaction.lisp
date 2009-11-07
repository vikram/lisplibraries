(in-package #:anardb)

(defmacro assert-in-transaction (&optional (store '*store*))
  `(progn
     (assert (member ,store *transaction*) (*transaction*))
     (values)))

(defmacro assert-not-in-transaction (&optional (store '*store*))
  `(progn
     (assert (not (member ,store *transaction*)) (*transaction*))
     (values)))

(defun restart-transaction ()
  "Restart the innermost transaction"
  (warn "Restarting transaction")
  (invoke-restart (find-restart 'restart-transaction)))

(defmacro with-store-lock ((store) &body body)
  "Prevent the store from being updated.

Note that if you wait too long, the lock may be broken by another process.
See *hard-break-delay*."
  `(with-file-lock ((store-lockfile ,store))
     ,@body))

(defmacro with-on-escape-reset-store-version ((store) &body body)
  (alexandria:with-unique-names (normal-exit)
    `(let (,normal-exit)
       (unwind-protect (multiple-value-prog1
			   (locally ,@body)
			 (setf ,normal-exit t))
	 (unless ,normal-exit
	   (setf (store-version ,store) -1))))))

(defmacro with-transaction ((&key message (store '*store*) on-restart) &body body)
  "Protect changes to a store with a transaction.

If the transaction fails (because another process updated the store
before the changes could be saved) then the store will be rolled
forward and the BODY will be run again.

Rolls the store forward to the latest serialisation on start, if the
file serialisation is newer than the last one loaded.

Note that it will wipe unsaved changes to the store iff the last
serialisation loaded is older than the newest available
serialisation. Therefore it will not wipe unsaved changes if the store
is not backed with a directory, or no other process has written to the
store since the last load.

If with-transaction returns normally, then it is successful and the
changes have been committed to disk.
"
  (alexandria:with-unique-names (restart condition current-version old-version block real-store)
    `(let (,old-version (,real-store ,store))
       (block ,block
	 (tagbody
	    ,restart
	    (assert-not-in-transaction ,real-store)
	    (store-update ,real-store)
	    (assert (not (minusp (store-version ,real-store))))

	    (let ((,current-version (store-version ,real-store)))
	      (restart-bind ((restart-transaction 
			      (lambda () 
				,on-restart
				(assert (store-dir ,real-store))
				(setf (store-version ,real-store) -1)
				(go ,restart))))
		(return-from ,block 
		  (multiple-value-prog1
		      (with-on-escape-reset-store-version (,real-store) 
			(let ((*transaction* (cons ,real-store *transaction*))) 
			  ,@body))
		    (handler-bind 
			(((or file-error stream-error)
			  (lambda(,condition)
			    (unless (eql ,old-version ,current-version)
			      (warn "Transaction ~A restarted due to error saving (version ~A): ~A" ,message ,current-version ,condition)
			      (setf ,old-version ,current-version)
			      (restart-transaction)))))
		      (with-store-lock (,real-store)
			(store-update ,real-store)
			(unless (eql (store-version ,real-store) ,current-version)
			  (restart-transaction))
			(store-save ,real-store))))))))))))


