;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-
;;;
;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package :cxml-rng)

#+sbcl
(declaim (optimize (debug 2)))

(defparameter *keywords*
  '("attribute" "default" "datatypes" "div" "element" "empty" "external"
    "grammar" "include" "inherit" "list" "mixed" "namespace" "notAllowed"
    "parent" "start" "string" "text" "token"))

(defmacro double (x)
  `((lambda (x) (return (values x x))) ,x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Escape interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hex-stream (trivial-gray-streams:fundamental-character-input-stream)
    ((source :initarg :source :accessor stream-source)
     (buffer :initform (make-array 1 :adjustable t :fill-pointer 0)
	     :accessor stream-buffer)
     (pos :initform 0 :accessor stream-pos)))

(defmethod trivial-gray-streams:stream-file-position ((s hex-stream))
  (file-position (stream-source s)))

;; zzz geht das nicht besser?
(defmethod trivial-gray-streams:stream-read-char ((s hex-stream))
  (with-slots (source buffer pos) s
    (cond
      ((< pos (length buffer))
       (prog1
	 (elt buffer pos)
	 (incf pos)))
      (t
       (setf (fill-pointer buffer) 0)
       (setf pos 0)
       (flet ((slurp ()
		(let ((c (read-char source nil)))
		  (vector-push-extend c buffer)
		  c)))
	 (macrolet ((with-expectation (frob &body body)
		      (when (characterp frob)
			(setf frob `(eql (slurp) ,frob)))
		      `(let ((result ,frob))
			 (cond
			   (result
			    ,@(or body (list 'result)))
			   (t
			    (prog1
				(elt buffer 0)
			      (incf pos)))))))
	   (with-expectation
	       #\\
	     (with-expectation
		 #\x
	       (with-expectation
		   (loop
		      for d = (peek-char nil source)
		      while (eql d #\x)
		      do (slurp)
		      finally
			(return (eql (slurp) #\{)))
		 (with-expectation
		     (loop
			for result = 0 then (+ (* result 16) i)
			for d = (peek-char nil source nil)
			for i = (digit-char-p d 16)
			while i
			do
			  (slurp)
			finally
			  (return
			    (when (eql (slurp) #\})
			      (setf (fill-pointer buffer) 0)
			      (setf pos 0)
			      (code-char result))))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tokenization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nc-name-p (str)
  (and (cxml-types::namep str) (cxml::nc-name-p str)))

(cxml-clex:deflexer rng
    (
     ;; NCName
     (letter+extras
      (or (range #x0041 #x005A) (range #x0061 #x007A)
	  ;; just allow the rest of unicode, because clex can't deal with the
	  ;; complete definition of name-char:
	  (range #x00c0 #xd7ff)
	  (range #xe000 #xfffd)
	  (range #x10000 #x10ffff)))
     (digit (range #x0030 #x0039))	;ditto
     (name-start-char (or letter+extras #\_))
     (name-char (or letter+extras digit #\. #\- #\_ #\:))

     ;; some RNC ranges
     (char
      (or 9 10 13
	  (range 32 #xd7ff)
	  (range #xe000 #xfffd)
	  (range #x10000 #x10ffff)))
     (init-comment-char
      (or 9 32 33 34
	  ;; #\#
	  (range 36 #xd7ff)
	  (range #xe000 #xfffd)
	  (range #x10000 #x10ffff)))
     (comment-char
      (or 35 init-comment-char))
     (string-char
      (or 32 33
	  ;; #\"
	  (range 35 38)
	  ;; #\'
	  (range 40  #xd7ff)
	  (range #xe000 #xfffd)
	  (range #x10000 #x10ffff)))
     (space (or 9 10 13 32))
     (newline (or 10 13)))

  ((* space))

  ((and "##") (cxml-clex:begin 'documentation-line))
  ((and "##" newline))
  ((cxml-clex::in documentation-line newline) (cxml-clex:begin 'cxml-clex:initial))
  ((cxml-clex::in documentation-line comment-char)
   (return (values 'documentation-line cxml-clex:bag)))

  ((and #\# init-comment-char) (cxml-clex:begin 'comment))
  ((and #\# newline))
  ((cxml-clex::in comment newline) (cxml-clex:begin 'cxml-clex:initial))
  ((cxml-clex::in comment comment-char))

  ((and "'''" (* (or string-char #\' #\")) "'''")
   (return
     (values 'literal-segment (subseq cxml-clex:bag 3 (- (length cxml-clex:bag) 3)))))

  ((and #\' (* (or string-char #\")) #\')
   (when (or (find (code-char 13) cxml-clex:bag)
	     (find (code-char 10) cxml-clex:bag))
     (rng-error nil "disallowed newline in string literal"))
   (return
     (values 'literal-segment (subseq cxml-clex:bag 1 (- (length cxml-clex:bag) 1)))))

  ((and #\" #\" #\" (* (or string-char #\' #\")) #\" #\" #\")
   (return
     (values 'literal-segment (subseq cxml-clex:bag 3 (- (length cxml-clex:bag) 3)))))

  ((and #\" (* (or string-char #\')) #\")
   (when (or (find (code-char 13) cxml-clex:bag)
	     (find (code-char 10) cxml-clex:bag))
     (rng-error nil "disallowed newline in string literal"))
   (return
     (values 'literal-segment (subseq cxml-clex:bag 1 (- (length cxml-clex:bag) 1)))))

  ((and name-start-char (* name-char))
   (return
     (cond
       ((find cxml-clex:bag *keywords* :test #'equal)
	(let ((sym (intern (string-upcase cxml-clex:bag) :keyword)))
	  (values sym sym)))
       ((find #\: cxml-clex:bag)
	(let* ((pos (position #\: cxml-clex:bag))
	       (prefix (subseq cxml-clex:bag 0 pos))
	       (lname (subseq cxml-clex:bag (1+ pos ))))
	  (when (find #\: lname)
	    (rng-error "too many colons"))
	  (unless (and (nc-name-p prefix))
	    (rng-error nil "not an ncname: ~A" prefix))
	  (let ((ch (cxml-clex::getch)))
	    (cond
	      ((and (equal lname "") (eql ch #\*))
	       (values 'nsname prefix))
	      (t
	       (cxml-clex::backup ch)
	       (unless (and (nc-name-p lname))
		 (rng-error nil "not an ncname: ~A" lname))
	       (values 'cname (cons prefix lname)))))))
       (t
	(unless (nc-name-p cxml-clex:bag)
	  (rng-error nil "not an ncname: ~A" cxml-clex:bag))
	(values 'identifier cxml-clex:bag)))))

  ((and #\\ name-start-char (* name-char))
   (let ((str (subseq cxml-clex:bag 1)))
     (unless (nc-name-p str)
       (rng-error nil "not an ncname: ~A" cxml-clex:bag))
     (return (values 'identifier str))))

  (#\= (double '=))
  (#\{ (double '{))
  (#\} (double '}))
  (#\[ (double '[))
  (#\] (double ']))
  (#\, (double '|,|))
  (#\& (double '&))
  (#\| (double '|\||))
  (#\? (double '?))
  (#\* (double '*))
  (#\+ (double '+))
  (#\( (double '|(|))
  (#\) (double '|)|))
  ((and "|=") (double '|\|=|))
  ((and "&=") (double '&=))
  ((and ">>") (double '>>))
  (#\~ (double '~))
  (#\- (double '-)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing into S-Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or)
  (defmacro lambda* ((&rest args) &body body)
    (setf args (mapcar (lambda (arg) (or arg (gensym))) args))
    `(lambda (,@args)
       (declare (ignorable ,@args))
       ,@body))

  (defmacro lambda* ((&rest args) &body body)
    (setf args (mapcar (lambda (arg) (or arg (gensym))) args))
    `(lambda (&rest .args.)
       (unless (equal (length .args.) ,(length args))
	 (error "expected ~A, got ~A" ',args .args.))
       (destructuring-bind (,@args) .args.
	 (declare (ignorable ,@args))
	 ,@body)))

  (defun wrap-decls (decls content)
    (if decls
	`(,@(car decls)
	    ,(wrap-decls (cadr decls) content))
	content)))

(yacc:define-parser *compact-parser*
  (:start-symbol top-level)
  (:terminals (:attribute :default :datatypes :div :element :empty
			  :external :grammar :include :inherit :list
			  :mixed :namespace :notAllowed :parent :start
			  :string :text :token
			  = { } |,| & |\|| ? * + |(| |)| |\|=| &= ~ -
			  [ ] >>
			  identifier literal-segment cname nsname
			  documentation-line))
  #+debug (:print-first-terminals t)
  #+debug (:print-states t)
  #+debug (:print-lookaheads t)
  #+debug (:print-goto-graph t)
  (:muffle-conflicts (50 0))		;hrmpf

  (top-level (decl* pattern #'wrap-decls)
	     (decl* grammar-content*
		    (lambda (a b) (wrap-decls a `(with-grammar () ,@b)))))

  (decl* () (decl decl*))

  (decl (:namespace identifier-or-keyword = namespace-uri-literal
		    (lambda* (nil name nil uri)
		      `(with-namespace (:uri ,uri :name ,name))))
	(:default :namespace = namespace-uri-literal
		  (lambda* (nil nil nil uri)
		    `(with-namespace (:uri ,uri :default t))))
	(:default :namespace identifier-or-keyword = namespace-uri-literal
		  (lambda* (nil nil name nil uri)
		    `(with-namespace (:uri ,uri :name ,name :default t))))
	(:datatypes identifier-or-keyword = literal
		    (lambda* (nil name nil uri)
		      `(with-data-type (:name ,name :uri ,uri)))))

  (pattern (inner-pattern
	    (lambda* (p) `(without-annotations ,p))))

  (particle (inner-particle
	     (lambda* (p) `(without-annotations ,p))))

  (inner-pattern inner-particle
		 (particle-choice (lambda* (p) `(%with-annotations ,p)))
		 (particle-group (lambda* (p) `(%with-annotations ,p)))
		 (particle-interleave (lambda* (p) `(%with-annotations ,p)))
		 (data-except (lambda* (p) `(%with-annotations-group ,p))))

  (primary (:element name-class { pattern }
		     (lambda* (nil name nil pattern nil)
		       `(with-element (:name ,name) ,pattern)))
	   (:attribute name-class { pattern }
		       (lambda* (nil name nil pattern nil)
			 `(with-attribute (:name ,name) ,pattern)))
	   (:list { pattern }
		  (lambda* (nil nil pattern nil)
		    `(list ,pattern)))
	   (:mixed { pattern }
		   (lambda* (nil nil pattern nil)
		     `(mixed ,pattern)))
	   (identifier (lambda* (x)
			 `(ref ,x)))
	   (:parent identifier
		    (lambda* (nil x)
		      `(parent-ref ,x)))
	   (:empty)
	   (:text)
	   (data-type-name [params]
			   (lambda* (name params)
			     `(data :data-type ,name :params ,params)))
	   (data-type-name data-type-value
			   (lambda* (name value)
			     `(value :data-type ,name :value ,value)))
	   (data-type-value (lambda* (value)
			      `(value :data-type nil :value ,value)))
	   (:notallowed)
	   (:external any-uri-literal [inherit]
		      (lambda* (nil uri inherit)
			`(external-ref :uri ,uri :inherit ,inherit)))
	   (:grammar { grammar-content* }
		     (lambda* (nil nil content nil)
		       `(with-grammar () ,@content)))
	   (\( pattern \) (lambda* (nil p nil) p)))

  (data-except (data-type-name [params] - lead-annotated-primary
			       (lambda* (name params nil p)
				 `(data :data-type ,name
					:params ,params
					:except ,p))))

  (inner-particle (annotated-primary
		   (lambda* (p) `(%with-annotations-group ,p)))
		  (repeated-primary follow-annotations
				    (lambda* (a b)
				      `(progn
					 (%with-annotations ,a)
					 ,b))))

  (repeated-primary (annotated-primary *
				       (lambda* (p nil) `(zero-or-more ,p)))
		    (annotated-primary +
				       (lambda* (p nil) `(one-or-more ,p)))
		    (annotated-primary ?
				       (lambda* (p nil) `(optional ,p))))

  (annotated-primary (lead-annotated-primary follow-annotations
					     (lambda* (a b)
					       `(progn ,a ,b))))

  (annotated-data-except (lead-annotated-data-except follow-annotations
						     (lambda* (a b)
						       `(progn ,a ,b))))

  (lead-annotated-data-except data-except
			      (annotations data-except
					   (lambda* (a p)
					     `(with-annotations ,a ,p))))

  (lead-annotated-primary primary
			  (annotations primary
				       (lambda* (a p)
					 `(with-annotations ,a ,p)))
			  (\( inner-pattern \)
			      (lambda* (nil p nil) p))
			  (annotations \( inner-pattern \)
				       (lambda* (a nil p nil)
					 `(let-annotations ,a ,p))))

  (particle-choice (particle \| particle
			     (lambda* (a nil b) `(choice ,a ,b)))
		   (particle \| particle-choice
			     (lambda* (a nil b) `(choice ,a ,@(cdr b)))))

  (particle-group (particle \, particle
			    (lambda* (a nil b) `(group ,a ,b)))
		  (particle \, particle-group
			    (lambda* (a nil b) `(group ,a ,@(cdr b)))))

  (particle-interleave (particle \& particle
				 (lambda* (a nil b) `(interleave ,a ,b)))
		       (particle \& particle-interleave
				 (lambda* (a nil b) `(interleave ,a ,@(cdr b)))))

  (param (identifier-or-keyword = literal
				(lambda* (name nil value)
				  `(param ,name ,value)))
	 (annotations identifier-or-keyword = literal
		      (lambda* (a name nil value)
			`(with-annotations ,a (param ,name ,value)))))

  (grammar-content* ()
		    (member grammar-content* #'cons))

  (member annotated-component
	  annotated-element-not-keyword)

  (annotated-component component
		       (annotations component
				    (lambda* (a c)
				      `(with-annotations ,a ,c))))

  (component start
	     define
	     (:div { grammar-content* }
		   (lambda* (nil nil content nil)
		     `(with-div ,@content)))
	     (:include any-uri-literal [inherit] [include-content]
		       (lambda* (nil uri inherit content)
			 `(with-include (:inherit ,inherit :uri ,uri)
			    ,@content))))

  (include-content* ()
		    (include-member include-content* #'cons))

  (include-member annotated-include-component
		  annotation-element-not-keyword)

  (annotated-include-component include-component
			       (annotations include-component
					    (lambda* (a c)
					      `(with-annotations (,@a) ,c))))

  (include-component start
		     define
		     (:div { grammar-content* }
			   (lambda* (nil nil content nil)
			     `(with-div ,@content))))

  (start (:start assign-method pattern
		 (lambda* (nil method pattern)
		   `(with-start (:combine-method ,method) ,pattern))))

  (define (identifier assign-method pattern
		      (lambda* (name method pattern)
			`(with-definition (:name ,name :combine-method ,method)
			   ,pattern))))

  (assign-method (= (constantly nil))
		 (\|= (constantly "choice"))
		 (&= (constantly "interleave")))

  (name-class (inner-name-class (lambda (nc) `(without-annotations ,nc))))

  (inner-name-class (annotated-simple-nc
		     (lambda (nc) `(%with-annotations-choice ,nc)))
		    (nc-choice
		     (lambda (nc) `(%with-annotations ,nc)))
		    (annotated-nc-except
		     (lambda (nc) `(%with-annotations-choice ,nc))))

  (simple-nc (name (lambda* (n) `(name ,n)))
	     (ns-name (lambda* (n) `(ns-name ,n)))
	     (* (constantly `(any-name)))
	     (\( name-class \) (lambda* (nil nc nil) nc)))

  (follow-annotations ()
		      (>> annotation-element follow-annotations))

  (annotations #+nil ()
	       (documentations
		(lambda (e)
		  `(annotation :elements ,e)))
	       ([ annotation-attributes annotation-elements ]
		  (lambda* (nil a e nil)
		    `(annotation :attributes ,a :elements ,e)))
	       (documentations [ annotation-attributes annotation-elements ]
			       (lambda* (d nil a e nil)
				 `(annotation :attributes ,a
					      :elements ,(append e d)))))

  (annotation-attributes
   ((constantly '(annotation-attributes)))
   (foreign-attribute-name = literal annotation-attributes
			   (lambda* (name nil value rest)
			     `(annotation-attributes
			       (annotation-attribute ,name ,value)
			       ,@(cdr rest)))))

  (foreign-attribute-name prefixed-name)

  (annotation-elements ()
		       (annotation-element annotation-elements #'cons))

  (annotation-element (foreign-element-name annotation-attributes-content
					    (lambda (a b)
					      `(with-annotation-element
						   (:name ,a)
						 ,b))))

  (foreign-element-name identifier-or-keyword
			prefixed-name)

  (annotation-element-not-keyword (foreign-element-name-not-keyword
				   annotation-attributes-content
				   (lambda (a b)
				     `(with-annotation-element
					  (:name ,a)
					,b))))

  (foreign-element-name-not-keyword identifier prefixed-name)

  (annotation-attributes-content ([ nested-annotation-attributes
				     annotation-content ]))

  (nested-annotation-attributes
   ((constantly '(annotation-attributes)))
   (any-attribute-name = literal
		       nested-annotation-attributes
		       (lambda* (name nil value rest)
			 `(annotation-attributes
			   (annotation-attribute ,name ,value)
			   ,@(cdr rest)))))

  (any-attribute-name identifier-or-keyword prefixed-name)

  (annotation-content ()
		      (nested-annotation-element annotation-content #'cons)
		      (literal annotation-content #'cons))

  (nested-annotation-element (any-element-name annotation-attributes-content
					       (lambda (a b)
						 `(with-annotation-element
						      (:name ,a)
						    ,b))))

  (any-element-name identifier-or-keyword prefixed-name)

  (prefixed-name cname)

  (documentations (documentation)
		  (documentation documentations #'cons))

  (documentation documentation-line
		 (documentation-line documentation
				     (lambda (a b)
				       (concatenate 'string a b))))

  (annotated-nc-except (lead-annotated-nc-except
			follow-annotations
			(lambda (p a)
			  `(progn ,p ,a))))

  (lead-annotated-nc-except nc-except
			    (annotations nc-except
					 (lambda (a p)
					   `(with-annotations ,a ,p))))

  (annotated-simple-nc (lead-annotated-simple-nc
			follow-annotations
			(lambda (p a) `(progn ,p ,a))))

  (lead-annotated-simple-nc
   simple-nc
   (\( inner-name-class \) (lambda* (nil nc nil) nc))
   (annotations simple-nc
		(lambda (a nc) `(with-annotations ,a ,nc)))
   (annotations \( inner-name-class \)
		(lambda (a nc) `(let-annotations ,a ,nc))))

  (nc-except (ns-name - simple-nc
		      (lambda* (nc1 nil nc2) `(ns-name ,nc1 :except ,nc2)))
	     (* - simple-nc
		(lambda* (nil nil nc) `(any-name :except ,nc))))

  (nc-choice (annotated-simple-nc \| annotated-simple-nc
				  (lambda* (a nil b)
				    `(name-choice ,a ,b)))
	     (annotated-simple-nc \| nc-choice
				  (lambda* (a nil b)
				    `(name-choice ,a ,@(cdr b)))))

  (name identifier-or-keyword cname)

  (data-type-name cname :string :token)

  (data-type-value literal)
  (any-uri-literal literal)

  (namespace-uri-literal literal :inherit)

  (inherit (:inherit = identifier-or-keyword
		     (lambda* (nil nil x) x)))

  (identifier-or-keyword identifier keyword)

  ;; identifier ::= (ncname - keyword) | quotedidentifier
  ;; quotedidentifier ::= "\" ncname

  ;; (ns-name (ncname \:*))
  (ns-name nsname)

  (ncname identifier-or-keyword)

  (literal literal-segment
	   (literal-segment ~ literal
			    (lambda* (a nil b)
			      (concatenate 'string a b))))

  ;; literalsegment ::= ...

  (keyword :attribute :default :datatypes :div :element :empty :external
	   :grammar :include :inherit :list :mixed :namespace :notAllowed
	   :parent :start :string :text :token)

  ;; optional stuff
  ([data-type-name] () data-type-name)
  ([inherit] () inherit)
  ([params] () ({ params } (lambda* (nil p nil) p)))
  (params () (param params #'cons))
  ([include-content] () ({ include-content* }
			   (lambda* (nil content nil) content))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Conversion of sexps into SAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uncompact (list)
  (funcall (or (get (car list) 'uncompactor)
	       (error "no uncompactor for ~A" (car list)))
	   (cdr list)))

(defmacro define-uncompactor (name (&rest args) &body body)
  `(setf (get ',name 'uncompactor)
	 (lambda (.form.) (destructuring-bind ,args .form. ,@body))))

(defparameter *namespaces* '(("xml" . "http://www.w3.org/XML/1998/namespace")))
(defparameter *default-namespace* nil)
(defparameter *data-types*
  '(("xsd" . "http://www.w3.org/2001/XMLSchema-datatypes")))

(defun xor (a b)
  (if a (not b) b))

(defun lookup-prefix (prefix)
  (cdr (assoc prefix *namespaces* :test 'equal)))

(defun lookup-default ()
  (or *default-namespace* :inherit))

(defun lookup-data-type (name)
  (cdr (assoc name *data-types* :test 'equal)))

(define-uncompactor with-namespace ((&key uri name default) &body body)
  (when (xor (equal name "xml")
	     (equal uri "http://www.w3.org/XML/1998/namespace"))
    (rng-error nil "invalid redeclaration of `xml' namespace"))
  (when (equal name "xmlns")
    (rng-error nil "invalid declaration of `xmlns' namespace"))
  (let ((*namespaces* *namespaces*)
	(*default-namespace* *default-namespace*))
    (when name
      (when (lookup-prefix name)
	(rng-error nil "duplicate declaration of prefix ~A" name))
      (push (cons name uri) *namespaces*))
    (when default
      (when *default-namespace*
	(rng-error nil "default namespace already declared to ~A"
		   *default-namespace*))
      (push (cons "" uri) *namespaces*)
      (setf *default-namespace* uri))
    (if (and name (not (or (eq uri :inherit) (equal uri ""))))
	(cxml:with-namespace (name uri)
	  (mapc #'uncompact body))
	(mapc #'uncompact body))))

(define-uncompactor with-data-type ((&key name uri) &body body)
  (when (and (equal name "xsd")
	     (not (equal uri "http://www.w3.org/2001/XMLSchema-datatypes")))
    (rng-error nil "invalid redeclaration of `xml' namespace"))
  (when (and (lookup-data-type name) (not (equal name "xsd")))
    (rng-error nil "duplicate declaration of library ~A" name))
  (let ((*data-types* (acons name uri *data-types*)))
    (mapc #'uncompact body)))

(defparameter *annotation-attributes* nil)
(defparameter *annotation-elements* nil)
(defparameter *annotation-wrap* nil)

(defmacro with-element (name-and-args &body body)
  (destructuring-bind (prefix lname &rest args)
      (if (atom name-and-args)
	  (list nil name-and-args)
	  name-and-args)
    `(invoke-with-element ,prefix
			  ,lname
			  (lambda () ,@args)
			  (lambda () ,@body))))

(defun invoke-with-element (prefix lname args body)
  (if (and *annotation-attributes*
	   *annotation-wrap*)
      (cxml:with-element* (nil *annotation-wrap*)
	(let ((*annotation-wrap* nil))
	  (invoke-with-element prefix lname args body)))
      (let ((*annotation-wrap* nil))
	(cxml:with-element* (prefix lname)
	  (funcall args)
	  (when *annotation-attributes*
	    (uncompact *annotation-attributes*))
	  (dolist (elt *annotation-elements*)
	    (cxml:with-namespace
		("a" "http://relaxng.org/ns/compatibility/annotations/1.0")
	      (cxml:with-element* ("a" "documentation")
		(cxml:text elt))))
	  (funcall body)))))

(define-uncompactor with-grammar ((&optional) &body body)
  (with-element "grammar"
    (mapc #'uncompact body)))

(define-uncompactor with-start ((&key combine-method) &body body)
  (with-element (nil "start"
		 (cxml:attribute "combine" combine-method))
    (mapc #'uncompact body)))

(define-uncompactor ref (name)
  (with-element (nil "ref"
		     (cxml:attribute "name" name))))

(define-uncompactor parent-ref (name)
  (with-element (nil "parentRef"
		     (cxml:attribute "name" name))))

(define-uncompactor parent-ref (name)
  (with-element (nil "parentRef" (cxml:attribute "name" name))))

(defun ns-attribute (uri-or-inherit)
  (unless (eq uri-or-inherit :inherit)
    (cxml:attribute "ns" uri-or-inherit)))

(define-uncompactor external-ref (&key uri inherit)
  (let ((ns (if inherit (lookup-prefix inherit) (lookup-default))))
    (with-element (nil "externalRef"
		       (cxml:attribute "href" (munge-schema-ref uri))
		       (ns-attribute ns)))))

(defvar *elementp*)

(define-uncompactor with-element ((&key name) pattern)
  (with-element "element"
    (let ((*elementp* t))
      (uncompact name))
    (uncompact pattern)))

(define-uncompactor with-attribute ((&key name) pattern)
  (with-element "attribute"
    (let ((*elementp* nil))
      (uncompact name))
    (uncompact pattern)))

(define-uncompactor list (pattern)
  (with-element "list"
    (uncompact pattern)))

(define-uncompactor mixed (pattern)
  (with-element "mixed"
    (uncompact pattern)))

(define-uncompactor :empty ()
  (with-element "empty"))

(define-uncompactor :text ()
  (with-element "text"))

(defun uncompact-data-type (data-type)
  (case data-type
    (:string
     (cxml:attribute "datatypeLibrary" "")
     (cxml:attribute "type" "string"))
    (:token
     (cxml:attribute "datatypeLibrary" "")
     (cxml:attribute "type" "token"))
    (t
     (cxml:attribute "datatypeLibrary"
		     (lookup-data-type (car data-type)))
     (cxml:attribute "type" (cdr data-type)))))

(define-uncompactor data (&key data-type params except)
  (with-element (nil "data" (uncompact-data-type data-type))
    (mapc #'uncompact params)
    (when except
      (with-element "except"
	(uncompact except)))))

(define-uncompactor value (&key data-type value)
  (with-element (nil "value" (uncompact-data-type data-type))
    (cxml:text value)))

(define-uncompactor :notallowed ()
  (with-element "notAllowed"))

(define-uncompactor with-definition ((&key name combine-method) &body body)
  (with-element (nil "define"
		     (cxml:attribute "name" name)
		     (cxml:attribute "combine" combine-method))
    (mapc #'uncompact body)))

(define-uncompactor with-div (&body body)
  (with-element "div"
    (mapc #'uncompact body)))

(define-uncompactor any-name (&key except)
  (with-element "anyName"
    (when except
      (with-element "except"
	(uncompact except)))))

(define-uncompactor ns-name (nc &key except)
  (with-element (nil "nsName"
		     (ns-attribute (lookup-prefix nc)))
    (when except
      (with-element "except"
	(uncompact except)))))

(define-uncompactor name-choice (&rest ncs)
  (with-element "choice"
    (mapc #'uncompact ncs)))

(defun destructure-cname-like (x)
  (when (keywordp x)
    (setf x (find x *keywords* :test 'string-equal)))
  (when (atom x)
    (setf x (cons (if *elementp* "" nil)
		  x)))
  (values (lookup-prefix (car x))
	  (cdr x)))

(define-uncompactor name (x)
  (multiple-value-bind (uri lname) (destructure-cname-like x)
    (with-element (nil
		   "name"
		   (ns-attribute uri))
      (cxml:text lname))))

(define-uncompactor choice (&rest body)
  (with-element "choice"
    (mapc #'uncompact body)))

(define-uncompactor group (&rest body)
  (with-element "group"
    (mapc #'uncompact body)))

(define-uncompactor interleave (&rest body)
  (with-element "interleave"
    (mapc #'uncompact body)))

(define-uncompactor one-or-more (p)
  (with-element "oneOrMore"
    (uncompact p)))

(define-uncompactor optional (p)
  (with-element "optional"
    (uncompact p)))

(define-uncompactor zero-or-more (p)
  (with-element "zeroOrMore"
    (uncompact p)))

(defun munge-schema-ref (uri)
  (if (search "://" uri)
      (concatenate 'string "rnc+" uri)
      (concatenate 'string "rnc+://" uri)))

(defun rnc-uri-p (uri)
  (and (search "://" uri)
       (equal (mismatch "rnc+" uri) 4)))

(defun follow-rnc-uri (uri)
  (if (equal (mismatch "rnc+://" uri) 7)
      ;; rnc+://foo/bar
      (subseq uri 7)
      ;; rnc+file:///usr/foo/...
      (subseq uri 4)))

(define-uncompactor with-include ((&key inherit uri) &body body)
  (let ((ns (if inherit (lookup-prefix inherit) (lookup-default))))
    (with-element (nil "include"
		       (cxml:attribute "href" (munge-schema-ref uri))
		       (ns-attribute ns))
      (mapc #'uncompact body))))

(define-uncompactor with-annotations
    ((annotation &key attributes elements) &body body)
  (check-type annotation (member annotation))
  (let ((*annotation-attributes* attributes)
	(*annotation-elements* elements))
    (mapc #'uncompact body)))

(define-uncompactor without-annotations (&body body)
  (let ((*annotation-attributes* nil)
	(*annotation-elements* nil))
    (mapc #'uncompact body)))

;; zzz das kann weg
(define-uncompactor %with-annotations (&body body)
  (mapc #'uncompact body))

(define-uncompactor %with-annotations-group (&body body)
  (let ((*annotation-wrap* "group"))
    (mapc #'uncompact body)))

(define-uncompactor %with-annotations-choice (&body body)
  (let ((*annotation-wrap* "choice"))
    (mapc #'uncompact body)))

(define-uncompactor progn (a b)
  (when a (uncompact a))
  (when b (uncompact b)))

(define-uncompactor annotation-attributes (&rest attrs)
  (mapc #'uncompact attrs))

(define-uncompactor annotation-attribute (name value)
  (cxml:attribute* (car name) (cdr name) value))

(define-uncompactor param (name value)
  (with-element (nil "param"
		     (cxml:attribute "name" name))
    (cxml:text value)))

(define-uncompactor with-annotation-element ((&key name) &body attrs)
  (cxml:with-element name
    (mapc #'uncompact attrs)))

;;; zzz strip BOM
;;; zzz newline normalization: Wir lesen von einem character-stream, daher
;;; macht das schon das Lisp fuer uns -- je nach Plattform. Aber nicht richtig.
(defun uncompact-file-1 (stream)
  (handler-case
      (let ((lexer (make-rng-lexer
		    (make-instance 'hex-stream :source stream))))
	(yacc:parse-with-lexer
	 (lambda ()
	   (multiple-value-bind (cat sem) (funcall lexer)
	     #+nil (print (list cat sem))
	     (if (eq cat :eof)
		 nil
		 (values cat sem))))
	 *compact-parser*))
    (error (c)
      (rng-error nil
		 "failed to parse compact syntax at char ~A, ~A:~%  ~A"
		 (file-position stream)
		 (cxml::safe-stream-sysid stream)
		 c))))

(defun uncompact-file (input &optional stream)
  (let ((tree
	 (etypecase input
	   (pathname (with-open-file (s input) (uncompact-file-1 s)))
	   (stream (with-open-stream (s input) (uncompact-file-1 s))))))
    #+nil (print tree)
    (with-output-to-string (s)
      (cxml:with-xml-output
	  (if stream
	      (cxml:make-octet-stream-sink stream)
	      (cxml:make-character-stream-sink s))
	(cxml:with-namespace ("" "http://relaxng.org/ns/structure/1.0")
	  (uncompact tree))))))

(defun parse-compact (pathname)
  "@arg[pathname]{a pathname designator for a Relax NG compact file}
   @return{a parsed @class{schema}}
   @short{This function parses a Relax NG schema file in compact syntax}
   and returns a parsed representation of that schema.

   @see{parse-schema}
   @see{make-validator}"
  (parse-schema (named-string-xstream
		 (uncompact-file pathname)
		 (cxml::pathname-to-uri pathname))))

(defun test-compact ()
  (dolist (p (directory "/home/david/src/nxml-mode-20041004/schema/*.rnc"))
    (print p)
    (with-open-file (s (make-pathname :type "rng" :defaults p)
		       :direction :output
		       :if-exists :supersede)
      (uncompact-file p s))))

#+(or)
(compact)
