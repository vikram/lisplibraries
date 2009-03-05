;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cffi-util)

;;;; * defcfun extension
;;;;example usage:
;;;;
;;;;(defcfun* ("test" test :method t) :error-int
;;;;  (a :int)
;;;;  (b :out :uchar)
;;;;  (c :int :key)
;;;;  (p :pointer)
;;;;  (flag :flag (:create :truncate))
;;;;  (aflag :uint32 :const 0)
;;;;  (cl :uint32 :class (my-class value))
;;;;  (cl2 :uint32 :class any-slot)
;;;;  (cl3 :pointer :key :class handle))
;;;; ==>
;;;;(PROGN
;;;; (DEFCFUN ("test" %TEST)
;;;;          :INT
;;;;          (A :INT)
;;;;          (B :POINTER)
;;;;          (C :INT)
;;;;          (P :POINTER)
;;;;          (FLAG :UINT32)
;;;;          (AFLAG :UINT32)
;;;;          (CL :UINT32)
;;;;          (CL2 :UINT32)
;;;;          (CL3 :POINTER))
;;;; (DEFMETHOD TEST
;;;;            (A P (CL MY-CLASS) CL2 &KEY C CL3 CREATE TRUNCATE)
;;;;            (WITH-FOREIGN-OBJECTS ((B :UCHAR))
;;;;                                  (LET ((RET
;;;;                                         (%TEST A
;;;;                                                B
;;;;                                                C
;;;;                                                (IF (NOT P) (NULL-POINTER) P)
;;;;                                                (LOGIOR (IF CREATE 1 0)
;;;;                                                        (IF TRUNCATE 128 0))
;;;;                                                0
;;;;                                                (SLOT-VALUE CL 'VALUE)
;;;;                                                (SLOT-VALUE CL2 'ANY-SLOT)
;;;;                                                (IF
;;;;                                                 (OR (NOT CL3)
;;;;                                                     (NOT
;;;;                                                      (SLOT-VALUE CL3
;;;;                                                                  'HANDLE)))
;;;;                                                 (NULL-POINTER)
;;;;                                                 (SLOT-VALUE CL3 'HANDLE)))))
;;;;                                    (WHEN (STD-CHECK-ERROR RET)
;;;;                                      (VALUES (MEM-REF B :UCHAR)))))))


(defun std-check-error (code)
  (if (= code 0)
      t
      (error "error-code: ~A~%error-str: ~A"
	     code
	     (foreign-funcall "strerror" :int code :string))))

(defmacro defcfun* ((name fun-name &key
			  (error-fun 'std-check-error)
			  method)
		    ret-type &body arg-list)
  "if ret-type is keyword :error-int the cfunction will return
   an int indicating an error if ret!=0 and error-fun is called with using
   the returned value
   syntax: (defcfun* (name fun-name <:error-fun symbol> <:method t|nil>)
               ret-type
             args*)
           ctype ::= see cffi manual...
           args ::= (arg ctype [:key]) | (arg :out ctype) | 
                    (arg ctype :const value) |
                    (arg :flag flag-list
                         <:flag-type ctype>
                         <:default flag-list>
                         <:flag-system flag-system>)
                    (arg ctype :class class-args)
           flag-list ::= (flag-keyword*)
           class-args ::= slot-name | (class-name slot-name)
   params: flag-type: default is uint32 
           default: flags being set to t by default
           name: cfunction name
           fun-name: function name for lisp interface
           error-fun: function called if ret-type id of type :error-int
           ret-type: see defcfun
           arglist: see defcfun (if arglist contains :out keyword for
                                 several args, these will be returned
                                 using values... try macroexpand to find out
                                 more :P )"
  (let ((cfun-name (symb '% fun-name))
	(cffi-arglist (create-cffi-arglist arg-list))
	(lisp-arglist (create-lisp-arglist method arg-list))
	(out-arglist (create-out-arglist arg-list))
	(call-list (create-lisp-call-list arg-list))
	(real-ret-type (if (eq :error-int ret-type) :int ret-type)))
     `(progn
       (defcfun (,name ,cfun-name) ,real-ret-type
	 ,@cffi-arglist)

       (,(if method 'defmethod 'defun) ,fun-name ,lisp-arglist
	 ,(if (null out-arglist)
	      (if (eq :error-int ret-type)
		  `(,error-fun (,cfun-name ,@call-list))
		  `(,cfun-name ,@call-list))
	      `(with-foreign-objects ,(mapcar #'cdr out-arglist)
		(let ((ret (,cfun-name ,@call-list)))
		  ,(if (eq :error-int ret-type)
		       `(when (,error-fun ret)
			 (values ,@out-arglist))
		       `(values ,@(append (when (not (eq :void ret-type))
					    (list `ret))
					  out-arglist))))))))))

(defmacro defmethod* ((name fun-name &key (error-fun 'std-check-error))
		      ret-type &body args)
  `(defcfun* (,name ,fun-name :error-fun ,error-fun :method t)
      ,ret-type ,@args))


(defun unpack-flag (flag-info)
  (destructuring-bind (name &key
			    flag
			    (flag-type :uint32)
			    default
			    flag-system) flag-info
    (values name flag-type flag default flag-system)))

(defun flag-exp-list (flag-info)
  (multiple-value-bind (name type flags)
      (unpack-flag flag-info)
    flags))

(defun flag-exp-type (flag-info)
  (multiple-value-bind (name type)
      (unpack-flag flag-info)
    type))

(defun create-cffi-arglist (args)
  (mapcar (lambda (arg)
	    (cond ((member :out arg) `(,(car arg) :pointer))
		  ((member :class arg) `(,(car arg) ,(second arg)))
		  ((member :flag arg) `(,(car arg) ,(flag-exp-type arg)))
		  ((in (third arg) :const :key) `(,(car arg) ,(second arg)))
		  (t arg)))
	  args))

(defun keyword->symbol (k)
  (symb (symbol-name k)))

(defun create-std-arg (method arg-spec)
  (let ((class-desc (second (member :class arg-spec)))
	(name (car arg-spec)))
    (cond ((and method
		(listp class-desc)
		(not (null class-desc))) `(,name ,(first class-desc)))
	  ((and method
		(atom class-desc)) name)
	  ((and (not method)
		class-desc)
	   (error "using arg option :class without declaring method"))
	  (t name))))

(defun create-lisp-arglist (method args)
  (let* ((no-out-args (remove-if (curry #'member :out) args))
	 (flags (remove-if (complement (curry #'member :flag))
			   no-out-args))
	 (has-keys (or flags
		       (some (lambda (exp) (eq (third exp) :key))
			     no-out-args)))
	 (std-args (mapcar (curry #'create-std-arg method)
			   (remove-if (funion (curry #'member :flag)
					      (curry #'member :const)
					      (curry #'member :key))
				      no-out-args))))
    (if has-keys
	(nconc (nconc1 std-args '&key)
	       (mapcar #'car ;;keywords
		       (remove-if-not (lambda (exp) (eq (third exp) :key))
			       no-out-args))
	       (mapcar #'keyword->symbol ;;flags
		       (remove-duplicates (mappend #'flag-exp-list flags)
					  :test #'eq)))
	std-args)))

(defun create-lisp-call-list (args)
  (mapcar (lambda (arg)
	    (let ((name (car arg)))
	      (cond ((member :out arg) name)
		    ((member :flag arg) (create-flag-call arg))
		    ((member :const arg) (fourth arg))
		    ((member :class arg)
		     (let* ((class-desc (second (member :class arg)))
			    (slot-val `(slot-value ,name
					',(if (listp class-desc)
					      (second class-desc)
					      class-desc))))
		       (if (or (member :pointer arg)
			       (member :string arg))
			   `(if (or (not ,name) (not ,slot-val))
			     (null-pointer) ,slot-val)
			   slot-val)))
		    (t (if (or (member :pointer arg)
			       (member :string arg))
			   `(if (not ,name) (null-pointer) ,name)
			   name)))))
	  args))

(defun create-flag-call (flag)
  (multiple-value-bind (name type flags default flag-system)
      (unpack-flag flag)
    (with-flag-system (if flag-system
			  flag-system
			  *current-flag-system*)
      (macroexpand-1 ;;nice hack huh
       `(flags
	 ,@(nconc (mappend (rcurry #'list t) default)
		  (mappend (lambda (flag)
			     `(,flag ,(keyword->symbol flag)))
			   flags)))))))

(defun create-out-arglist (args)
  (mapcar (lambda (arg)
	    `(mem-ref ,@(remove :out arg)))
	  (remove-if (complement (curry #'member :out))
		     args)))