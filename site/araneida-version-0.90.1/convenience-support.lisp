(in-package araneida)

; I really couldn't think of a better word than "root" for this. It means that it's in the
; first level of the hierarchy, and thus must include a directive to install itself. Simple enough.
(defun decode-hierarchy-directive (root-handler internal-url external-url this-directive root)
  (let ((url (first this-directive))
	(class (cond
		 ((symbolp (second this-directive)) (second this-directive))
		 ((consp   (second this-directive)) (first (second this-directive)))
		 (t (error "Unknown class designator: ~A" (second this-directive)))))
	(class-constructor (cond
		 ((symbolp (second this-directive)) `(make-instance ',(second this-directive)))
		 ((consp   (second this-directive)) (second (second this-directive)))
		 (t (error "Unknown class designator: ~A" (second this-directive)))))
	(sub-dirs (remove-if (lambda (possible-directive)
			       (if (not (stringp (car possible-directive)))
				   t))
			     (rest (rest this-directive))))
	(performs (remove-if (lambda (possible-directive)
			       (if (stringp (car possible-directive))
				   t))
			     (rest (rest this-directive)))))
    (let ((bind-url-func `(defun ,(intern (format nil "~A-URL" (symbol-name class))) ()
			   (araneida:append-url ,external-url ,url)))
	  (shared-initialize-func
	   (when (or sub-dirs performs)
	     (with-gensyms (handler)
	       `(defmethod shared-initialize :after ((,handler ,class) slot-names &rest initargs)
		 "Shared initialize as created by the macro attach-hierarchy"
		 (declare (ignorable ,handler slot-names initargs))
		 ,@(mapcar (lambda (sub)
			     (destructuring-bind (sub-url sub-class-place &rest etc) sub
			       (declare (ignore etc))
			       (let ((sub-class
				      (cond
					((symbolp sub-class-place) sub-class-place)
					((consp   sub-class-place) (first sub-class-place))
					(t (error "Unknown class designator: ~A" sub-class-place))))
				     (sub-class-constructor
				      (cond
					((symbolp sub-class-place) `(make-instance ',sub-class-place))
					((consp   sub-class-place) (second sub-class-place))
					(t (error "Unknown class designator: ~A" sub-class-place)))))
				 (declare (ignore sub-class)) ; might use it later - who knows?
				 `(araneida:install-handler ,handler ,sub-class-constructor ,sub-url nil))))
			   sub-dirs)
		 ,(when performs
			(let ((handlersym (intern "_HANDLER"))
			      (iurlsym (intern "_IURL"))
			      (eurlsym (intern "_EURL")))
			  `(let ((,handlersym ,handler)
				 (,eurlsym (araneida:append-url ,external-url ,url))
				 (,iurlsym (araneida:append-url ,internal-url ,url)))
			    (declare (ignorable ,handlersym ,eurlsym ,iurlsym))
			    ,@performs))))))))
      (append (list bind-url-func shared-initialize-func)
	      (when root
		`((araneida:install-handler ,root-handler
		                            ,class-constructor
		                            (araneida:append-url ,internal-url ,url)
                                            nil)))
	      (mapcan (lambda (sub) (decode-hierarchy-directive root-handler
								`(araneida:append-url ,internal-url ,url)
								`(araneida:append-url ,external-url ,url)
								sub nil))
		      sub-dirs)))))
