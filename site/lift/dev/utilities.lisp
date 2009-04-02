(in-package #:lift)

;; stolen from metatilities
(defun form-symbol-in-package (package &rest names)
    "Finds or interns a symbol in package whose name is formed by concatenating the pretty printed representation of the names together."
    (with-standard-io-syntax 
      (let ((*package* package))
	(intern (format nil "~{~a~}" names)
		package))))
     
(defun form-symbol (&rest names)
    "Finds or interns a symbol in the current package whose name is formed by concatenating the pretty printed representation of the names together."
    (apply #'form-symbol-in-package *package* names))

(defun form-keyword (&rest names)
    "Finds or interns a symbol in the current package whose name is formed by concatenating the pretty printed representation of the names together."
    (apply #'form-symbol-in-package 
	   (load-time-value (find-package :keyword)) names))

;; borrowed from asdf
(defun pathname-sans-name+type (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME and TYPE components"
  (make-pathname :name nil :type nil :defaults pathname))

(defun pathname-has-device-p (pathname)
  (and (or (stringp pathname) (pathnamep pathname))
       (not (member (pathname-device pathname) '(nil :unspecific)))))

(defun pathname-has-host-p (pathname)
  (and (or (stringp pathname) (pathnamep pathname))
       (not (member (pathname-host pathname) '(nil :unspecific)))))

(defun relative-pathname (relative-to pathname &key name type)
  (let ((directory (pathname-directory pathname)))
    (when (eq (car directory) :absolute)
      (setf directory (copy-list directory)
	    (car directory) :relative))
    (merge-pathnames
     (make-pathname :name (or name (pathname-name pathname))
                    :type (or type (pathname-type pathname))
                    :directory directory
		    )
     relative-to)))

(defun directory-pathname-p (p)
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))

(defun directory-p (name)
  (let ((truename (probe-file name)))
    (and truename (directory-pathname-p name))))

(defun containing-pathname (pathspec)
  "Return the containing pathname of the thing to which 
pathspac points. For example:

    > \(containing-directory \"/foo/bar/bis.temp\"\)
    \"/foo/bar/\"
    > \(containing-directory \"/foo/bar/\"\)
    \"/foo/\"
"
  (make-pathname
   :directory `(,@(butlast (pathname-directory pathspec)
			   (if (directory-pathname-p pathspec) 1 0)))
   :name nil
   :type nil
   :defaults pathspec))

;; FIXME -- abstract and merge with unique-directory
(defun unique-filename (pathname &optional (max-count 10000))
  (let ((date-part (date-stamp)))
    (loop repeat max-count
       for index from 1
	 for name = 
	 (merge-pathnames 
	  (make-pathname
	   :name (format nil "~a-~a-~d" 
			 (pathname-name pathname)
			 date-part index))
	  pathname) do
	 (unless (probe-file name)
	   (return-from unique-filename name)))
    (error "Unable to find unique pathname for ~a; there are already ~:d similar files" pathname max-count)))
	    
;; FIXME -- abstract and merge with unique-filename
(defun unique-directory (pathname)
  (setf pathname (merge-pathnames pathname))
  (let* ((date-part (date-stamp))
	 (last-directory (first (last (pathname-directory pathname))))
	 (base-pathname (containing-pathname pathname))
	 (base-name (pathname-name last-directory))
	 (base-type (pathname-type last-directory)))
    (or (loop repeat 10000
	   for index from 1
	   for name = 
	   (merge-pathnames 
	    (make-pathname
	     :name nil
	     :type nil
	     :directory `(:relative 
			  ,(format nil "~@[~a-~]~a-~d~@[.~a~]" 
				   base-name date-part index base-type)))
	    base-pathname) do
	   (unless (probe-file name)
	     (return name)))
	(error "Unable to find unique pathname for ~a" pathname))))

(defun date-stamp (&key (datetime (get-universal-time)) (include-time? nil))
  (multiple-value-bind
	(second minute hour day month year day-of-the-week)
      (decode-universal-time datetime)
    (declare (ignore day-of-the-week))
    (let ((date-part (format nil "~d-~2,'0d-~2,'0d" year month day))
	  (time-part (and include-time? 
			  (list (format nil "-~2,'0d-~2,'0d-~2,'0d"
					hour minute second)))))
      (apply 'concatenate 'string date-part time-part))))


#+(or)
(date-stamp :include-time? t)	

;;; ---------------------------------------------------------------------------
;;; shared stuff
;;; ---------------------------------------------------------------------------	

(defgeneric get-class (thing &key error?)
  (:documentation "Returns the class of thing or nil if the class cannot be found. Thing can be a class, an object representing a class or a symbol naming a class. Get-class is like find-class only not as particular.")
  (:method ((thing symbol) &key error?)
           (find-class thing error?))
  (:method ((thing standard-object) &key error?)
           (declare (ignore error?))
           (class-of thing))
  (:method ((thing t) &key error?) 
           (declare (ignore error?))
           (class-of thing))
  (:method ((thing class) &key error?)
           (declare (ignore error?))
           thing))

(defun direct-subclasses (thing)
  "Returns the immediate subclasses of thing. Thing can be a class, object or symbol naming a class."
  (class-direct-subclasses (get-class thing)))

(defun map-subclasses (class fn &key proper?)
  "Applies fn to each subclass of class. If proper? is true, then
the class itself is not included in the mapping. Proper? defaults to nil."
  (let ((mapped (make-hash-table :test #'eq)))
    (labels ((mapped-p (class)
               (gethash class mapped))
             (do-it (class root)
               (unless (mapped-p class)
                 (setf (gethash class mapped) t)
                 (unless (and proper? root)
                   (funcall fn class))
                 (mapc (lambda (class)
                         (do-it class nil))
                       (direct-subclasses class)))))
      (do-it (get-class class) t))))

(defun subclasses (class &key (proper? t))
  "Returns all of the subclasses of the class including the class itself."
  (let ((result nil))
    (map-subclasses class (lambda (class)
                            (push class result))
                    :proper? proper?)
    (nreverse result)))

(defun superclasses (thing &key (proper? t))
  "Returns a list of superclasses of thing. Thing can be a class, object or symbol naming a class. The list of classes returned is 'proper'; it does not include the class itself."
  (let ((result (class-precedence-list (get-class thing))))
    (if proper? (rest result) result)))

#+(or)
;;?? remove
(defun direct-superclasses (thing)
  "Returns the immediate superclasses of thing. Thing can be a class, object or symbol naming a class."
  (class-direct-superclasses (get-class thing)))

(declaim (inline length-1-list-p)) 
(defun length-1-list-p (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defmacro defclass-property (property &optional (default nil default-supplied?))
  "Create getter and setter methods for 'property' on symbol's property lists." 
  (let ((real-name (intern (format nil "~:@(~A~)" property) :keyword)))
    `(progn
       (defgeneric ,property (symbol))
       (defgeneric (setf ,property) (value symbol))
       (defmethod ,property ((class-name symbol))
          (get class-name ,real-name ,@(when default-supplied? (list default))))
       (defmethod (setf ,property) (value (class-name symbol))
         (setf (get class-name ,real-name) value)))))

(defun parse-brief-slot (slot)
  (let* ((slot-spec 
	  (typecase slot
	    (symbol (list slot))
	    (list slot)
	    (t (error "Slot-spec must be a symbol or a list. `~s` is not." 
		      slot)))))
    (unless (null (cddr slot-spec))
      (error "Slot-spec must be a symbol or a list of length one or two. `~s` has too many elements." slot)) 
    `(,(first slot-spec) ,@(when (second slot-spec) 
				 `(:initform ,(second slot-spec))))))

(defun convert-clauses-into-lists (clauses-and-options clauses-to-convert)
  ;; This is useful (for me at least!) for writing macros
  (let ((parsed-clauses nil))
    (do* ((clauses clauses-and-options (rest clauses))
          (clause (first clauses) (first clauses)))
         ((null clauses))
      (if (and (keywordp clause)
               (or (null clauses-to-convert) (member clause clauses-to-convert))
               (not (length-1-list-p clauses)))
        (progn
          (setf clauses (rest clauses))
          (push (list clause (first clauses)) parsed-clauses))
        (push clause parsed-clauses)))
    (nreverse parsed-clauses)))

(defun remove-leading-quote (list)
  "Removes the first quote from a list if one is there."
  (if (and (consp list) (eql (first list) 'quote))
    (first (rest list))
    list))

(defun cleanup-parsed-parameter (parameter)
  (if (length-1-list-p parameter)
    (first parameter)
    parameter))

(defun ensure-string (it)
  (etypecase it
    (string it)
    (symbol (symbol-name it))))