(in-package #:anardb)

(alexandria:define-constant +store-file-extension+ ".anardb"
  :test 'equal)

(defvar *store*)
(defvar *transaction* nil)
(defvar *store-serialised-objects*)

(defun dump-directory-versions (dir)
  (remove-if 'not (mapcar (lambda(x)(parse-integer (pathname-name x) :junk-allowed t)) 
			  (directory (concatenate 'string (force-string dir) "/*" +store-file-extension+)))))

(defun latest-version (versions)
  (apply 'max (remove-if 'not versions)))

(defun earliest-version (versions)
  (apply 'min (remove-if 'not versions)))

(defun dump-filename (dir version)
  (with-standard-io-syntax
    (format nil "~a/~d~a" dir version +store-file-extension+)))

(defun dump-directory-latest-version (dir)
  (let ((versions (dump-directory-versions dir)))
    (cond 
      ((not versions)
       nil)
      ((not (cdr versions))
       (car versions))
      (t
       (when (cddr versions)
	 (warn "More than two versions exist in ~s: ~s" dir versions))
       (earliest-version versions)))))


(defun store-object-mark-serialised (obj)
  (let ((old (gethash obj *store-serialised-objects*)))
    (cond (old (values old nil))
	  (t
	   (let ((lookup-form (store-object-make-lookup-form obj)))
	     (setf (gethash obj *store-serialised-objects*) lookup-form)
	     (values nil lookup-form))))))

(defun store-object-serialise-form (obj)
  (declare (optimize speed))
  (typecase obj
    (store-object
     (multiple-value-bind (form new-form)
	 (store-object-mark-serialised obj)
       (cond (form form)
	     ((eq *store* (store-object-store obj))
	      (store-object-make-serialise-form obj))
	     (t
	      new-form))))
    ((or standard-object structure-object)
     (make-load-form obj))
    ((or number string keyword (member t nil))
     obj)
    (array
     (assert (not (array-displacement obj)))
     (assert (not (array-has-fill-pointer-p obj)))
     (assert (not (cdr (array-dimensions obj))))
     `(make-array ',(array-dimensions obj) :element-type ',(array-element-type obj)
		  :initial-contents (list ,@(map 'list 'store-object-serialise-form obj))))
    (list
     `(list* ,@(loop 
		     while (consp obj)
		     collect (store-object-serialise-form (pop obj))) 
	     ,(store-object-serialise-form obj)))
    (symbol
     `',obj)
    (t
     (warn "Cannot safely serialise ~s; converting to string" obj)
     (with-standard-io-syntax (princ-to-string obj)))))


(defun store-wipe (store)
  (let ((*transaction* (cons store *transaction*)))
    (loop for table in (store-tables store)
	  do 
	  ;;; trickery to rapidly iterate over the values in the
	  ;;; hashtable while allowing for the fact that deleting one
	  ;;; may create or delete others
	  (loop for alist = (alexandria:hash-table-alist table)
		   while alist
		   do
		   (loop for (key . value) in alist
			 do (when (eq value (gethash key table))
			      (store-object-del value))))))


  (loop for table in (store-tables store)
	do (assert (zerop (hash-table-count table)) (table store)))
  (store-reset store))

(defvar *anardb-datastore-serialisation-done*)
(defun anardb-datastore-serialisation-end-marker ()
  (assert (not *anardb-datastore-serialisation-done*))
  (setf *anardb-datastore-serialisation-done* t))

(defun small-eval (form)
  (declare (optimize speed))

  ;; this is needed because  call-arguments-limit can be very low
  (typecase form
    (cons
     (case (first form)
       (list
	(loop for f in (rest form) collect (small-eval f)))
       (list*
	 (let ((remaining (rest form)))
	   (flet ((p ()
		    (small-eval (pop remaining))))
	     (declare (inline p))
	     (let* ((base (cons (p) nil)) (tail base))
	       (loop
		     (cond ((cdr remaining)
			    (setf (cdr tail) (cons (p) nil)
				  tail (cdr tail)))
			   (t
			    (setf (cdr tail) (p))
			    (return))))
	       base))))
       (make-array
	(destructuring-bind (dimensions &key (element-type t) initial-contents)
	    (rest form)
	  (let ((array (make-array (small-eval dimensions) :element-type (small-eval element-type))))
	    (assert (eq 'list (first initial-contents)))
	    (loop for f in (rest initial-contents) 
		  for i from 0
		  do (setf (aref array i) (small-eval f)))
	    array)))
       (sdo
	(destructuring-bind (class id &rest slot-values)
	    (rest form)
	  (let ((store-object (store-deserialise-create-object class id)))
	    (loop for (slot value) on slot-values by #'cddr do
		  (setf (slot-value store-object slot) (small-eval value)))
	    (store-object-add-indices store-object)
	    store-object)))
       (quote
	(second form))
       (t (eval form))))
    (t form)))

(defun store-deserialise (store stream)
  (let ((vars       
	 (mapcar 'dbclass-index-struct-var (store-classnames store)))
	vals)
    (progv
	vars
	(mapcar (lambda(classname) (funcall (dbclass-make-index-struct-name classname)))
		(store-classnames store))

      (with-standard-io-syntax
	(let ((*package* (find-package :anardb)) (*store* store) 
	      *anardb-datastore-serialisation-done*)

	  (loop for form =
		(read stream nil 'eof)
		until (eq form 'eof)
		do (small-eval form))

	  (unless *anardb-datastore-serialisation-done* 
	    (warn "Datastore serialisation does not have end marker")
	    (error 'end-of-file :stream stream))))

      (setf vals (mapcar 'symbol-value vars)))
    (map nil (lambda(var value)
	       (setf (symbol-value var) value))
	 vars
	 vals))
  (values))



(defun anardb-format-version (version &rest args)
  (declare (ignore args))
  (assert (>= 1 version)))

(defun store-serialise (store stream)
  (let ((*store-serialised-objects* (make-hash-table :test 'eq))
	(*store* store))
    (with-standard-io-syntax
      (let ((*package* #.*package*))
	(flet ((w (form)
		 (write form
			:stream stream :readably nil)
	       (terpri stream)))
	  (w `(anardb-format-version 1))

	  (loop for class in (store-classnames store) 
		for count = (hash-table-count (dbclass-index-table-value class))
		do (w `(dbclass-table-resize ',class ,count )))
	  (loop for table in (store-tables store) do
	      (loop for obj being the hash-values of table do
		    (unless (gethash obj *store-serialised-objects*)
		      (w (store-object-serialise-form obj)))))
	  (w `(progn
	      (setf (store-version *store*) ,(1+ (store-version store)))
	      (setf (store-next-id *store*) ,(store-next-id store))))
	  (w `(anardb-datastore-serialisation-end-marker))
	  
	  (finish-output stream))))))

(defun store-update (store)
  (when (store-dir store)
    (loop for version = 
	  (loop do
		(store-wait-for-save-to-finish store) 
		thereis
		(dump-directory-latest-version (store-dir store)))
	  until (eql (store-version store) version)
	  do
	  (handler-case 
	      (with-open-file (stream (store-filename store version) :if-does-not-exist nil)
		(when stream
		  (store-deserialise store stream)))
	    ((or file-error stream-error) (e)
	      (warn "Error updating store from ~A: ~A" (store-filename store version) e)))))
  (values))

(defun store-filename (store version)
  (dump-filename (store-dir store) version))

(defun store-next-version (store)
  (let ((new-version (1+ (store-version store))))
    (values (store-filename store new-version) new-version)))

(defun store-save (store)
  (when (store-dir store)
    (multiple-value-bind (filename new-version)
	(store-next-version store)
      (if (file-locked-by-this-process-p (store-lockfile store))
	  (ignore-errors (delete-file filename)) 					
					; We have the lock now!
	  (warn "Writing store file without lock ~A" filename))

      (with-open-file (stream filename
			      :if-exists :error :if-does-not-exist :create :direction :output)
	(store-serialise store stream)
	(fsync stream))
	
      (handler-case 
	  (when (ignore-errors (probe-file filename)) ; if we weren't subsequently deleted by someone else . . .
	    (delete-file (store-filename store (store-version store)))
	    (setf (store-version store) new-version))
	(file-error (e)
	  (warn "Error deleting old version ~A: ~A" (store-filename store (store-version store)) e)))))
  (values))

(defun store-lockfile (store) ;; actually a directory -- plan was to
			      ;; allow for posix locking and actually
			      ;; have a lock file, but now we just use
			      ;; dotlocks and so we call the directory
			      ;; the lockfile
  (concatenate 'string (force-string (store-dir store)) "/"))

(defun store-init-dir (store &optional (new-dir nil new-dir-p))
  "Create a new database in the file-system"
  (when new-dir-p
    (setf (store-dir store) new-dir))
  (assert (not (probe-file (store-dir store))))
  (ensure-directories-exist (store-dir store))
  (assert (= 0 (store-version store)))
  (with-open-file (stream (store-filename store 1) :if-does-not-exist :create :direction :output :if-exists :error)
    (store-serialise store stream))
  (values))

(defun store-reset (store &optional (new-dir nil new-dir-p))
  (when new-dir-p
    (setf (store-dir store) new-dir))
  (setf (store-version store) 0)
  store)

(defun store-wait-for-save-to-finish (store)
  (unless (file-locked-by-this-process-p (store-lockfile store))
    (file-lock-wait-then-maybe-break 
     (store-lockfile store))))




