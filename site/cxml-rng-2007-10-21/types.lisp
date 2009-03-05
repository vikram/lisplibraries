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

(in-package :cxml-types)

(defstruct (param (:constructor make-param (name value)))
  "@short{A named data type parameter.}

   (With the XSD type library, parameters are known as restricting facets.)
   @see-constructor{make-param}
   @see{find-type}
   @see{cxml-rng:pattern-params}
   @see{cxml-rng:data}
   @see-slot{param-name}
   @see-slot{param-value}"
  name
  value)

(setf (documentation 'make-param 'function)
      "@arg[name]{parameter name, a string}
       @arg[value]{parameter value, a string}
       @return{a @class{param}}
       Create a data type parameter.
       @see{param-name}
       @see{param-value}")

(setf (documentation 'param-name 'function)
      "@arg[instance]{an instance of @class{param}}
       @return{a string}
       The data type parameter's name.
       @see{param-value}")

(setf (documentation 'param-value 'function)
      "@arg[instance]{an instance of @class{param}}
       @return{a string}
       The data type parameter's value.
       @see{param-name}")

(defclass data-type () ()
  (:documentation
   "@short{The abstract superclass of all types.}

    Each type belongs to a datatype library, named by a keyword.  In each
    library, the types are named by strings.

    @see-constructor{find-type}
    @see-slot{type-name}
    @see-slot{type-library}
    @see-slot{type-context-dependent-p}
    @see-slot{type-id-type}
    @see{parse}
    @see{equal-using-type}
    @see{lessp-using-type}
    @see{validp}"))

(defgeneric find-type (library name params)
  (:documentation
   "@arg[library]{datatype library, a keyword symbol}
    @arg[name]{the type's name, a string}
    @arg[params]{type parameters, a list of @class{param} instances}
    @return{an instance of @class{data-type}, or @code{nil}}
    @short{Look up the type named @em{name} in datatype library @em{library}.}

    Additional parameters (knows as restricting facets in XSD) can be passed
    to specify or restrict the type for the purposes of @fun{validp}.

    Return a type instance for this type and the additional parameters,
    @code{nil} if the type does not exist, or
    @code{:error} if the type exists, but the specified parameters are not
    valid for that type.

    @see{data-type}"))

(defgeneric type-library (type)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @return{library name, a keyword}
    @short{Return the name of the library this type belongs to.}

    @see{type-name}
    @see{type-context-dependent-p}
    @see{type-id-type}"))

(defgeneric type-name (type)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @return{type name, a string}
    @short{Return the name this type has within its library.}

    @see{type-library}
    @see{type-context-dependent-p}
    @see{type-id-type}"))

(defmethod find-type ((library t) name params)
  (declare (ignore name params))
  nil)

(defgeneric type-context-dependent-p (type)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @return{a boolean}
    @short{Return true if parsing and validation of values by this type
      depends on the validation context.}

    In this case, the optional @code{context} argument to @fun{parse} and
    @fun{validp} is required, and an error will be signalled if it is missing.

    @see{validation-context}
    @see{type-name}
    @see{type-library}
    @see{type-context-dependent-p}
    @see{type-id-type}"))

(defmethod type-context-dependent-p ((type data-type))
  nil)

(defgeneric type-id-type (type)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @return{one of @code{nil}, @code{:id}, @code{:idref}, or @code{:idrefs}}
    @short{Returns the @em{ID-type} of @code{type}.}

    The symbols @code{nil}, @code{:id}, @code{:idref}, or @code{:idrefs}
    represent the ID-types @em{null}, @em{ID},  @em{IDREF}, and @em{IDREFS},
    respectively, as defined by
    @a[http://relaxng.org/compatibility-20011203.html]{
      RELAX NG DTD Compatibility}.

    @see{type-name}
    @see{type-library}
    @see{type-context-dependent-p}"))

(defmethod type-id-type ((type data-type))
  nil)

(defgeneric equal-using-type (type u v)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @arg[u]{a parsed value as returned by @fun{parse}}
    @arg[v]{a parsed value as returned by @fun{parse}}
    @return{a boolean}
    @short{Compare the @emph{values} @code{u} and @code{v} using a
      data-type-dependent equality function.}

    @see{validp}"))

(defgeneric parse (type e &optional context)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @arg[e]{a string}
    @arg[context]{an instance of @class{validation-context}}
    @return{an object}
    @short{Parse string @code{e} and return a representation of its value
      as defined by the data type.}

    The @code{context} argument is required if @fun{type-context-dependent-p}
    is true for @code{type}, and will be ignored otherwise.

    @see{equal-using-type}
    @see{validp}"))

(defgeneric validp (type e &optional context)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @arg[e]{a string}
    @arg[context]{an instance of @class{validation-context}}
    @return{a boolean}
    @short{Determine whether a string is a valid lexical representation
    for a type.}

    The @code{context} argument is required if @fun{type-context-dependent-p}
    is true for @code{type}, and will be ignored otherwise.

    @see{parse}
    @see{equal-using-type}"))


;;; Validation context

(defclass validation-context () ()
  (:documentation
   "@short{This abstract class defines a protocol allowing data types
    to query the XML parser about its current state.}

    Some types are context dependent, as indicated by
    @fun{type-context-dependent-p}.  Those types need access to state
    computed by the XML parser implicitly, like namespace bindings or
    the Base URI.

    User-defined subclasses must implement methods
    for the functions @fun{context-find-namespace-binding} and
    @fun{context-find-unparsed-entity}.

    Two pre-defined validation context implementations are
    provided, one for use with SAX, the other based on Klacks."))

(defgeneric context-find-namespace-binding (context prefix)
  (:documentation
   "@arg[context]{an instance of @class{validation-context}}
    @arg[prefix]{name prefix, a string}
    @return{the namespace URI as a string, or NIL}
    @short{This function resolves a namespace prefix to a namespace URI in the
    current context.}
    All currently declared namespaces
    are taken into account, including those declared directly on the
    current element."))

(defgeneric context-find-unparsed-entity (context name)
  (:documentation
   "@arg[context]{an instance of @class{validation-context}}
    @arg[name]{entity name, a string}
    @return{@code{nil}, or a list of public id, system id, and notation name}
    This function looks for an unparsed entity in the current context."))

(defclass klacks-validation-context (validation-context)
  ((source :initarg :source :accessor context-source))
  (:documentation
   "A validation-context implementation that queries
    a klacks source for information about the parser's current state.
    @see-constructor{make-klacks-validation-context}"))

(defun make-klacks-validation-context (source)
  "@arg[source]{a @a[http://common-lisp.net/project/cxml/klacks.html]{
     klacks source}}
   @return{a @class{klacks-validation-context}}
   Create a validation-context that will query the given klacks source for
   the current parser context."
  (make-instance 'klacks-validation-context :source source))

(defmethod context-find-namespace-binding
    ((context klacks-validation-context) prefix)
  (klacks:find-namespace-binding prefix (context-source context)))

;; zzz nicht schoen.
(defmethod context-find-unparsed-entity
    ((context klacks-validation-context) name)
  (or (dolist (x (slot-value (context-source context)
			     'cxml::external-declarations))
	(when (and (eq (car x) 'sax:unparsed-entity-declaration)
		   (equal (cadr x) name))
	  (return t)))
      (dolist (x (slot-value (context-source context)
			     'cxml::internal-declarations))
	(when (and (eq (car x) 'sax:unparsed-entity-declaration)
		   (equal (cadr x) name))
	  (return t)))))

(defclass sax-validation-context-mixin (validation-context)
  ((stack :initform nil :accessor context-stack)
   (unparsed-entities :initform (make-hash-table :test 'equal)
		      :accessor unparsed-entities))
  (:documentation
   "@short{A class that implements validation-context as a mixin for
     user-defined SAX handler classes.}

    The mixin will record namespace information
    automatically, and the user's SAX handler can simply be passed as a
    validation context to data type functions."))

(defmethod sax:start-prefix-mapping :after
    ((handler sax-validation-context-mixin) prefix uri)
  (push (cons prefix uri) (context-stack handler)))

(defmethod sax:end-prefix-mapping :after
    ((handler sax-validation-context-mixin) prefix)
  (setf (context-stack handler)
	(remove prefix
		(context-stack handler)
		:count 1
		:key #'car
		:test #'equal)))

(defmethod sax:unparsed-entity-declaration
    ((context sax-validation-context-mixin)
     name public-id system-id notation-name)
  (setf (gethash name (unparsed-entities context))
	(list public-id system-id notation-name)))

(defmethod context-find-namespace-binding
    ((context sax-validation-context-mixin) prefix)
  (cdr (assoc prefix (context-stack context) :test #'equal)))

(defmethod context-find-unparsed-entity
    ((context sax-validation-context-mixin) name)
  (gethash name (unparsed-entities context)))


;;; Relax NG built-in type library

(defclass rng-type (data-type) ()
  (:documentation
   "@short{The class of Relax NG built-in types.}
    Relax NG defines two built-in data type: string and token.

    The Relax NG type library is named @code{:||}."))

(defmethod print-object ((object rng-type) stream)
  (print-unreadable-object (object stream :type t :identity nil)))

(defclass string-type (rng-type) ()
  (:documentation
   "@short{The Relax NG 'string' type.}
    This data type allows arbitrary strings and interprets them as-is.

    For this type, @fun{parse} will return any string unchanged, and
    @fun{equal-using-type} compares strings using @code{equal}."))

(defclass token-type (rng-type) ()
  (:documentation
   "@short{The Relax NG 'token' type.}
    This data type allows arbitrary strings and normalizes all whitespaces.

    For this type, @fun{parse} will return the string with leading and
    trailing whitespace removed, and remaining sequences of spaces
    compressed down to one space character each.

    A method for @fun{equal-using-type} compares strings using @code{equal}."))

(defmethod type-library ((type rng-type))
  :||)

(defvar *string-data-type* (make-instance 'string-type))
(defvar *token-data-type* (make-instance 'token-type))

(defmethod find-type ((library (eql :||)) name params)
  (cond
    ((eq name :probe) t)
    (params :error)
    ((equal name "string") *string-data-type*)
    ((equal name "token") *token-data-type*)
    (t nil)))

(defmethod equal-using-type ((type rng-type) u v)
  (equal u v))

(defmethod validp ((type rng-type) e &optional context)
  (declare (ignore e context))
  t)

(defmethod type-name ((type string-type)) "string")
(defmethod type-name ((type token-type)) "token")

(defmethod parse ((type string-type) e &optional context)
  (declare (ignore context))
  e)

(defmethod parse ((type token-type) e &optional context)
  (declare (ignore context))
  (normalize-whitespace e))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace*
    (format nil "~C~C~C~C"
	    (code-char 9)
	    (code-char 32)
	    (code-char 13)
	    (code-char 10))))

(defun normalize-whitespace (str)
  (cl-ppcre:regex-replace-all #.(format nil "[~A]+" *whitespace*)
			      (string-trim *whitespace* str)
			      " "))

(defun replace-whitespace (str)
  (cl-ppcre:regex-replace-all #.(format nil "[~A]" *whitespace*)
			      str
			      " "))


;;; DTD compatibility types

(defclass dtd-compatibility-type (data-type)
    ((chained-type :accessor chained-type))
  (:documentation
   "@short{The class of DTD Compatibility data types.}

    This library contains three types: ID, IDREF, and IDREFS.

    This type library is named
    @code{:|http://relaxng.org/ns/compatibility/datatypes/1.0|}."))

(defmethod print-object ((object dtd-compatibility-type) stream)
  (print-unreadable-object (object stream :type t :identity nil)))

(defclass id-type (dtd-compatibility-type) ()
  (:documentation
   "@short{The DTD compatibility 'ID' type.}

    For this type, @fun{parse} will return the string with leading and
    trailing whitespace removed.

    The resulting value must be an NCName.

    The ID-type of this data type is 'ID', ensuring that each value is
    only used for one element in a document.

    @see{xsd-id-type}"))

(defclass idref-type (dtd-compatibility-type) ()
  (:documentation
   "@short{The DTD compatibility 'IDREF' type.}

    For this type, @fun{parse} will return the string with leading and
    trailing whitespace removed.

    The resulting value must be an NCName.

    The ID-type of this data type is 'IDREF', ensuring that the value
    referenced must be declared as the ID of an element in the document.

    @see{xsd-idref-type}"))

(defclass idrefs-type (dtd-compatibility-type) ()
  (:documentation
   "@short{The DTD compatibility 'IDREFS' type.}

    Strings are valid for this data type they contain a whitespace-separated
    list of one or more NCNames.  @fun{parse} will return a list of these
    substrings.

    The ID-type of this data type is 'IDREFS', ensuring that each value
    referenced must be declared as the ID of an element in the document.

    @see{xsd-idrefs-type}"))

;; die Implementation dieser Typen deligieren wir einfach mal an die
;; entsprechenden XSD-Typen.
(defmethod initialize-instance :after ((instance id-type) &key)
  (setf (chained-type instance)
	(or (find-type :|http://www.w3.org/2001/XMLSchema-datatypes| "ID" nil)
	    (error "oops"))))

(defmethod initialize-instance :after ((instance idref-type) &key)
  (setf (chained-type instance)
	(or (find-type :|http://www.w3.org/2001/XMLSchema-datatypes| "IDREF" nil)
	    (error "oops"))))

(defmethod initialize-instance :after ((instance idrefs-type) &key)
  (setf (chained-type instance)
	(or (find-type :|http://www.w3.org/2001/XMLSchema-datatypes| "IDREFS" nil)
	    (error "oops"))))

(defmethod type-library ((type dtd-compatibility-type))
  :|http://relaxng.org/ns/compatibility/datatypes/1.0|)

(defmethod type-name ((type id-type)) "ID")
(defmethod type-name ((type idref-type)) "IDREF")
(defmethod type-name ((type idrefs-type)) "IDREFS")

;; default values set below
(declaim (special *id-type*))
(declaim (special *idref-type*))
(declaim (special *idrefs-type*))

(defmethod find-type
    ((library (eql :|http://relaxng.org/ns/compatibility/datatypes/1.0|||))
     name params)
  (cond
    ((eq name :probe) t)
    (params :error)
    ((equal name "ID") *id-type*)
    ((equal name "IDREF") *idref-type*)
    ((equal name "IDREFS") *idrefs-type*)
    (t nil)))

(defmethod validp ((type dtd-compatibility-type) e &optional context)
  (validp (chained-type type) e context))

(defmethod parse ((type dtd-compatibility-type) e &optional context)
  (parse (chained-type type) e context))

(defmethod type-id-type ((type dtd-compatibility-type))
  (type-id-type (chained-type type)))


;;; XML Schema Part 2: Datatypes Second Edition

(defparameter *xsd-types* (make-hash-table :test 'equal))

(defmacro defxsd
    ((class-name type-name) (&rest supers) (&rest slots) &rest args)
  `(progn
     (setf (gethash ,type-name *xsd-types*) ',class-name)
     (defclass ,class-name ,supers
	 ((type-name :initform ,type-name
		     :reader type-name
		     :allocation :class)
	  ,@slots)
       ,@args)))

(defgeneric patterns (data-type)
  (:documentation
   "@arg[data-type]{a subtype of @class{xsd-type}}
    @return{a list of strings}
    This slot reader returns a list of the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-pattern]{pattern facets}."))

(defmethod (setf patterns) :after (newval data-type)
  (slot-makunbound data-type 'compiled-patterns))

(defclass xsd-type (data-type)
  ((patterns :initform nil :accessor patterns)
   (compiled-patterns :accessor compiled-patterns))
  (:documentation
   "@short{The class of XML Schema built-in types.}

    Subclasses of xsd-type provide the built-in types of
    @a[http://www.w3.org/TR/xmlschema-2/]{
      XML Schema Part 2: Datatypes Second Edition}
    as specified in @a[http://relaxng.org/xsd-20010907.html]{Guidelines for
    using W3C XML Schema Datatypes with RELAX NG}.

    The XSD type library
    is named @code{:|http://www.w3.org/2001/XMLSchema-datatypes|}.

    @b{Parameters.} All XSD types accept regular expressions restricting
    the set of strings accepted by the type.  The pattern parameter is
    called @code{\"pattern\"}.  This parameter can be repeated to specify
    multiple regular expressions that must all match the data.
    As an initarg, specify @code{:pattern} with a list of regular expressions
    as an argument.

    @see-slot{patterns}"))

(defmethod initialize-instance :after ((instance xsd-type) &key patterns)
  (setf (patterns instance) (append (patterns instance) patterns)))

(defmethod print-object ((object xsd-type) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (describe-facets object stream)))

(defgeneric describe-facets (object stream)
  (:method-combination progn))

(defmethod describe-facets progn ((object xsd-type) stream)
  (format stream "~{ :pattern ~A~}" (patterns object)))

(defmethod type-library ((type xsd-type))
  :|http://www.w3.org/2001/XMLSchema-datatypes|)

(defun zip (keys values)
  (loop for key in keys for value in values collect key collect value))

(defgeneric parse-parameter (class-name type-name param-name value))

(defun parse-parameters (type-class params)
  (let ((patterns '())
	(args '()))
    (dolist (param params (values t patterns args))
      (let ((name (param-name param))
	    (value (param-value param)))
	(if (equal name "pattern")
	    (push value patterns)
	    (multiple-value-bind (key required-class)
		(case (find-symbol (param-name param) :keyword)
		  (:|length| (values :exact-length 'length-mixin))
		  (:|maxLength| (values :max-length 'length-mixin))
		  (:|minLength| (values :min-length 'length-mixin))
		  (:|minInclusive| (values :min-inclusive 'ordering-mixin))
		  (:|maxInclusive| (values :max-inclusive 'ordering-mixin))
		  (:|minExclusive| (values :min-exclusive 'ordering-mixin))
		  (:|maxExclusive| (values :max-exclusive 'ordering-mixin))
		  (:|totalDigits| (values :total-digits 'decimal-type))
		  (:|fractionDigits| (values :fraction-digits 'decimal-type))
		  (t (return nil)))
	      (unless (subtypep type-class required-class)
		(return nil))
	      (when (loop
		       for (k nil) on args by #'cddr
		       thereis (eq key k))
		(return nil))
	      (push (parse-parameter required-class
				     type-class
				     key
				     (normalize-whitespace value))
		    args)
	      (push key args)))))))

(defmethod find-type
    ((library (eql :|http://www.w3.org/2001/XMLSchema-datatypes|)) name params)
  (if (eq name :probe)
      t
      (let ((class (gethash name *xsd-types*)))
	(if class
	    (multiple-value-bind (ok patterns other-args)
		(parse-parameters class params)
	      (if ok
		  (apply #'make-instance
			 class
			 :patterns patterns
			 other-args)
		  :error))
	    nil))))

(defgeneric parse/xsd (type e context))

(defgeneric validp/xsd (type v context)
  (:method-combination and))

;; make CLOS happy:
(defmethod validp/xsd and ((type xsd-type) v context)
  (declare (ignore v context))
  t)

(defmethod validp ((type xsd-type) e &optional context)
  (not (eq :error (parse/xsd type e context))))

(defmethod parse ((type xsd-type) e &optional context)
  (let ((result (parse/xsd type e context)))
    (when (eq result :error)
      (error "not valid for data type ~A: ~S" type e))
    result))

;; Handle the whiteSpace "facet" before the subclass sees it.
;; If parsing succeded, check other facets by asking validp/xsd.
(defmethod parse/xsd :around ((type xsd-type) e context)
  (setf e (munge-whitespace type e))
  (unless (slot-boundp type 'compiled-patterns)
    (setf (compiled-patterns type)
	  (mapcar #'pattern-scanner (patterns type))))
  (if (every (lambda (pattern)
	       (cl-ppcre:all-matches pattern e))
	     (compiled-patterns type))
      (let ((result (call-next-method type e context)))
	(if (or (eq result :error) (validp/xsd type result context))
	    result
	    :error))
      :error))

(defgeneric munge-whitespace (type e))

(defmethod munge-whitespace ((type xsd-type) e)
  (normalize-whitespace e))


;;; ordering-mixin

(defgeneric min-exclusive (data-type)
  (:documentation
   "@arg[data-type]{an ordered data type}
    @return{an integer, or @code{nil}}
    This slot reader returns the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-minExclusive]{minExclusive facet},
    or @code{nil} if none was specified.
    @see{max-exclusive}
    @see{min-inclusive}
    @see{max-inclusive}"))

(defgeneric max-exclusive (data-type)
  (:documentation
   "@arg[data-type]{an ordered data type}
    @return{an integer, or @code{nil}}
    This slot reader returns the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-maxExclusive]{maxExclusive facet},
    or @code{nil} if none was specified.
    @see{min-exclusive}
    @see{min-inclusive}
    @see{max-inclusive}"))

(defgeneric min-inclusive (data-type)
  (:documentation
   "@arg[data-type]{an ordered data type}
    @return{an integer, or @code{nil}}
    This slot reader returns the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-minInclusive]{minInclusive facet},
    or @code{nil} if none was specified.
    @see{min-exclusive}
    @see{max-exclusive}
    @see{max-inclusive}"))

(defgeneric max-inclusive (data-type)
  (:documentation
   "@arg[data-type]{an ordered data type}
    @return{an integer, or @code{nil}}
    This slot reader returns the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-maxInclusive]{maxInclusive facet},
    or @code{nil} if none was specified.
    @see{min-exclusive}
    @see{max-exclusive}
    @see{min-inclusive}"))

(defclass ordering-mixin ()
    ((min-exclusive :initform nil
		    :initarg :min-exclusive
		    :accessor min-exclusive)
     (max-exclusive :initform nil
		    :initarg :max-exclusive
		    :accessor max-exclusive)
     (min-inclusive :initform nil
		    :initarg :min-inclusive
		    :accessor min-inclusive)
     (max-inclusive :initform nil
		    :initarg :max-inclusive
		    :accessor max-inclusive)))

(defmethod describe-facets progn ((object ordering-mixin) stream)
  (dolist (slot '(min-exclusive max-exclusive min-inclusive max-inclusive))
    (let ((value (slot-value object slot)))
      (when value
	(format stream " ~A ~A"
		(intern (symbol-name slot) :keyword)
		value)))))

(defmethod parse-parameter
    ((class-name (eql 'ordering-mixin)) type-name (param t) value)
  (parse (make-instance type-name) value nil))

(defgeneric lessp-using-type (type u v)
  (:documentation
   "@arg[type]{an ordered @class{data-type}}
    @arg[u]{a parsed value as returned by @fun{parse}}
    @arg[v]{a parsed value as returned by @fun{parse}}
    @return{a boolean}
    @short{Compare the @emph{values} @code{u} and @code{v} using a
      data-type-dependent partial ordering.}

    A method for this function is provided only by types that have a
    natural partial ordering.

    @see{equal-using-type}"))

(defun <-using-type (type u v)
  (lessp-using-type type u v))

(defun <=-using-type (type u v)
  (or (lessp-using-type type u v) (equal-using-type type u v)))

;; it's only a partial ordering, so in general this is not the opposite of <=
(defun >-using-type (type u v)
  (lessp-using-type type v u))

;; it's only a partial ordering, so in general this is not the opposite of <
(defun >=-using-type (type u v)
  (or (lessp-using-type type v u) (equal-using-type type v u)))

(defmethod validp/xsd and ((type ordering-mixin) v context)
  (declare (ignore context))
  (with-slots (min-exclusive max-exclusive min-inclusive max-inclusive) type
    (and (or (null min-exclusive) (>-using-type type v min-exclusive))
	 (or (null max-exclusive) (<-using-type type v max-exclusive))
	 (or (null min-inclusive) (>=-using-type type v min-inclusive))
	 (or (null max-inclusive) (<=-using-type type v max-inclusive)))))


;;; length-mixin

(defgeneric exact-length (data-type)
  (:documentation
   "@arg[data-type]{a data type supporting restrictions on value lengths}
    @return{an integer, or @code{nil}}
    This slot reader returns the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-length]{length facet},
    or @code{nil} if none was specified.
    @see{min-length}
    @see{max-length}"))

(defgeneric min-length (data-type)
  (:documentation
   "@arg[data-type]{a data type supporting restrictions on value lengths}
    @return{an integer, or @code{nil}}
    This slot reader returns the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-minLength]{minLength facet},
    or @code{nil} if none was specified.
    @see{exact-length}
    @see{max-length}"))

(defgeneric max-length (data-type)
  (:documentation
   "@arg[data-type]{a data type supporting restrictions on value lengths}
    @return{an integer, or @code{nil}}
    This slot reader returns the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-maxLength]{maxLength facet},
    or @code{nil} if none was specified.
    @see{exact-length}
    @see{min-length}"))

(defclass length-mixin ()
    ((exact-length :initform nil :initarg :exact-length :accessor exact-length)
     (min-length :initform nil :initarg :min-length :accessor min-length)
     (max-length :initform nil :initarg :max-length :accessor max-length)))

(defmethod describe-facets progn ((object length-mixin) stream)
  (dolist (slot '(exact-length min-length max-length))
    (let ((value (slot-value object slot)))
      (when value
	(format stream " ~A ~A"
		(intern (symbol-name slot) :keyword)
		value)))))

(defmethod parse-parameter
    ((class-name (eql 'length-mixin)) (type-name t) (param t) value)
  (parse (make-instance 'non-negative-integer-type) value nil))

;; extra-hack fuer die "Laenge" eines QName...
(defgeneric length-using-type (type u))
(defmethod length-using-type ((type length-mixin) e) (length e))

(defmethod validp/xsd and ((type length-mixin) v context)
  (declare (ignore context))
  (with-slots (exact-length min-length max-length) type
    (or (not (or exact-length min-length max-length))
	(let ((l (length-using-type type v)))
	  (and (or (null exact-length) (eql l exact-length))
	       (or (null min-length) (>= l min-length))
	       (or (null max-length) (<= l max-length)))))))


;;; enumeration-type

(defclass enumeration-type (xsd-type length-mixin)
    ((word-type :reader word-type)))

(defmethod initialize-instance :after ((type enumeration-type) &key)
  (setf (min-length type) (max* 1 (min-length type))))

(defmethod parse/xsd ((type enumeration-type) e context)
  (let ((wt (word-type type)))
    (loop
       for word in (cl-ppcre:split " " e)
       for v = (parse wt word context)
       collect v
       when (eq v :error) do (return :error))))



;;;; Primitive types

;;; duration

(defxsd (duration-type "duration") (xsd-type ordering-mixin)
  ()
  (:documentation
   "@short{The duration data type, representing a duration of time.}

    @b{Syntax.} This type accepts an ISO-like syntax.  For details refer to
    the @a[http://www.w3.org/TR/xmlschema-2/#duration]{specification}.

    @b{Implementation.} This type returns lists of the form
    @code{(years months days hours minutes seconds)}.  Each
    value can be @code{nil} or a number.  All values are integers
    except for @code{seconds}, which is a real.

    @b{Example.} @code{P1Y2M3DT10H30M}
    maps to @code{(1 2 3 10 30 nil)}

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}."))

(defmethod equal-using-type ((type duration-type) u v)
  (equal u v))

;; zzz das ist vielleicht ein bisschen zu woertlich implementiert
(defmethod lessp-using-type ((type duration-type) u v)
  (let ((dt (make-instance 'date-time-type)))
    (every (lambda (str)
	     (let ((s (parse dt str nil)))
	       (lessp-using-type dt
				 (datetime+duration s u)
				 (datetime+duration s v))))
	   '("1696-09-01T00:00:00Z"
	     "1697-02-01T00:00:00Z"
	     "1903-03-01T00:00:00Z"
	     "1903-07-01T00:00:00Z"))))

(defun datetime+duration (s d)
  (destructuring-bind (syear smonth sday shour sminute ssecond szone) s
    (destructuring-bind (dyear dmonth dday dhour dminute dsecond) d
      (setf dhour (or dhour 0))
      (setf dminute (or dminute 0))
      (setf dsecond (or dsecond 0))
      (labels ((floor3 (a low high)
		 (multiple-value-bind (u v)
		     (floor (- a low) (- high low))
		   (values u (+ low v))))
	       (maximum-day-in-month-for (yearvalue monthvalue)
		 (multiple-value-bind (m y)
		     (floor3 monthvalue 1 13)
		   (day-limit m (+ yearvalue y)))))
	(multiple-value-bind (carry emonth) (floor3 (+ smonth dmonth) 1 13)
	  (let ((eyear (+ syear dyear carry))
		(ezone szone))
	    (multiple-value-bind (carry esecond) (floor (+ ssecond dsecond) 60)
	      (multiple-value-bind (carry eminute)
		  (floor (+ sminute dminute carry) 60)
		(multiple-value-bind (carry ehour)
		    (floor (+ shour dhour carry) 24)
		  (let* ((mdimf (maximum-day-in-month-for eyear emonth))
			 (tmpdays (max 1 (min sday mdimf)))
			 (eday (+ tmpdays dday carry)))
		    (loop
		       (let* ((mdimf (maximum-day-in-month-for eyear emonth))
			      (carry
			       (cond
				 ((< eday 1)
				  (setf eday (+ eday mdimf))
				  -1)
				 ((> eday mdimf)
				  (setf eday (- eday mdimf))
				  1)
				 (t
				  (return))))
			      (tmp (+ emonth carry)))
			 (multiple-value-bind (y m)
			     (floor3 tmp 1 13)
			   (setf emonth m)
			   (incf eyear y))))
		    (list eyear emonth eday ehour eminute esecond
			  ezone)))))))))))

(defun scan-to-strings (&rest args)
  (coerce (nth-value 1 (apply #'cl-ppcre:scan-to-strings args)) 'list))

(defmethod parse/xsd ((type duration-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y m d tp h min s)
      (scan-to-strings "(?x)
                         ^(-)?            # minus
                         P(?:(\\d+)Y)?    # years
                         (?:(\\d+)M)?     # months
                         (?:(\\d+)D)?     # days
                         (T               # (time)
                           (?:(\\d+)H)?   # hours
                           (?:(\\d+)M)?   # minutes
                           (?:(\\d+(?:[.]\\d+)?)S)?   # seconds
                           )?$"
		       e)
    (if (and (or y m d h min s)
	     (or (null tp) (or h min s)))
	(let ((f (if minusp -1 1)))
	  (flet ((int (str)
		   (and str (* f (parse-integer str)))))
	    (list (int y) (int m) (int d) (int h) (int min)
		  (and s (* f (parse-number:parse-number s))))))
	:error)))


;;; dateTime

(defclass time-ordering-mixin (ordering-mixin) ())

(defxsd (date-time-type "dateTime") (xsd-type time-ordering-mixin)
  ()
  (:documentation
   "@short{The dateTime data type, representing a moment in time.}

    @b{Syntax.} This type accepts an ISO-like syntax.  For details refer to
    the @a[http://www.w3.org/TR/xmlschema-2/#dateTime]{specification}.

    @b{Implementation.} This type returns lists of the form
    @code{(year month day hour minute second timezone)}.  Each
    value is an integer, except except for @code{second}, which is a real, 
    and @code{timezone} which is a real or @code{nil}.
    A @code{timezone} of @code{nil} indicates UTC.

    @b{Example.} @code{2002-10-10T12:00:00-05:00}
    maps to @code{(2002 10 10 12 0 0 -5)}

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}.  The ordering is partial
    except within a timezone, see the spec for details."))

(defmethod equal-using-type ((type time-ordering-mixin) u v)
  (equal u v))

;; add zone-offset as a duration (if any), but keep a boolean in the
;; zone-offset field indicating whether there was a time-zone
(defun normalize-date-time (u)
  (destructuring-bind (year month day hour minute second zone-offset) u
    (let ((v (list year month day hour minute second (and zone-offset t))))
      (if zone-offset
	  (multiple-value-bind (h m)
	      (truncate zone-offset)
	    (datetime+timezone v h (* m 100)))
	  v))))

(defun datetime+timezone (d h m)
  (datetime+duration d (list 0 0 0 h m 0)))

(defmethod lessp-using-type ((type time-ordering-mixin) p q)
  (destructuring-bind (pyear pmonth pday phour pminute psecond pzone)
      (normalize-date-time p)
    (destructuring-bind (qyear qmonth qday qhour qminute qsecond qzone)
	(normalize-date-time q)
      (cond
	((and pzone (not qzone))
	 (lessp-using-type type p (datetime+timezone q 14 0)))
	((and (not pzone) qzone)
	 (lessp-using-type type (datetime+timezone p -14 0) q))
	(t
	 ;; zzz hier sollen wir <> liefern bei Feldern, die in genau einer
	 ;; der Zeiten fehlen.  Wir stellen aber fehlende Felder derzeit
	 ;; defaulted dar, koennen diese Situation also nicht feststellen.
	 ;; Einen Unterschied sollte das nur machen, wenn Werte verschiedener
	 ;; Datentypen miteinander verglichen werden.  Das bieten wir einfach
	 ;; nicht an.
	 (loop
	    for a in (list pyear pmonth pday phour pminute psecond)
	    for b in (list qyear qmonth qday qhour qminute qsecond)
	    do
	      (when (< a b)
		(return t))
	      (when (> a b)
		(return nil))))))))

(defun day-limit (m y)
  (cond
    ((and (eql m 2)
	  (or (zerop (mod y 400))
	      (and (zerop (mod y 4))
		   (not (zerop (mod y 100))))))
     29)
    ((eql m 2) 28)
    ((if (<= m 7) (oddp m) (evenp m)) 31)
    (t 30)))

(defun parse-time (minusp y m d h min s tz tz-sign tz-h tz-m
		       &key (start 0) end)
  (declare (ignore tz start end))		;zzz
  ;; parse into numbers
  (flet ((int (str)
	   (and str (parse-integer str)))
	 (num (str)
	   (and str (parse-number:parse-number str))))
    (setf (values y m d h min s tz-h tz-m)
	  (values (* (int y) (if minusp -1 1))
		  (int m) (int d) (int h) (int min)
		  (num s)
		  (int tz-h) (int tz-m))))
  (let ((day-limit (day-limit m y)))
    ;; check ranges
    (cond
      ((and y m d h min s
	    (plusp y)
	    (<= 1 m 12)
	    (<= 1 d day-limit)
	    (<= 0 h 24)
	    (<= 0 min 59)
	    ;; zzz sind leap seconds immer erlaubt?
	    (<= 0 s 60))
       ;; 24:00:00 must be canonicalized
       (when (and (eql h 24) (zerop min) (zerop s))
	 (incf h)
	 (incf d)
	 (when (> d day-limit)
	   (setf d 1)
	   (incf m)
	   (when (> m 12)
	     (incf y))))
       (let ((tz-offset
	      (when tz-h
		(* (if (equal tz-sign "-") -1 1)
		   (+ tz-h (/ tz-m 100))))))
	 (list (* y (if minusp -1 1)) m d h min s tz-offset)
	 ;; (subseq ... start end)
	 ))
      (t
       :error))))

(defmethod parse/xsd ((type date-time-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y m d h min s tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^(-)?                     # opt. minus
                          ((?:[1-9]\\d*)?\\d{4})      # year
                          -(\\d\\d)                   # month
                          -(\\d\\d)                   # day
                          T                         # (time)
                          (\\d\\d)                    # hour
                          :(\\d\\d)                   # minute
                          :(\\d+(?:[.]\\d+)?)        # second
                          (([+-])(\\d\\d):(\\d\\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time minusp y m d h min s tz tz-sign tz-h tz-m)))


;;; time

(defxsd (time-type "time") (xsd-type time-ordering-mixin)
  ()
  (:documentation
   "@short{The time data type, representing a time of day.}

    @b{Syntax.} This type accepts an ISO-like syntax.  For details refer to
    the @a[http://www.w3.org/TR/xmlschema-2/#dateTime]{specification}.

    @b{Implementation.} This type returns the same kind of lists as
    @class{date-time-type}, except that the fields @code{year},
    @code{month} and @code{day} are filled with dummy values from the
    Gregorian year AD 1.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}.  The ordering is partial
    except within a timezone, see the spec for details."))

(defmethod parse/xsd ((type time-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional h min s tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^(\\d\\d)                    # hour
                          :(\\d\\d)                   # minute
                          :(\\d+(?:[.]\\d+)?)        # second
                          (([+-])(\\d\\d):(\\d\\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time nil "1" "1" "1" h min s tz tz-sign tz-h tz-m
		:start 3)))


;;; date

(defxsd (date-type "date") (xsd-type time-ordering-mixin)
  ()
  (:documentation
   "@short{The date data type, representing a day of the year.}

    @b{Syntax.} This type accepts an ISO-like syntax.  For details refer to
    the @a[http://www.w3.org/TR/xmlschema-2/#date]{specification}.

    @b{Implementation.} This type returns the same kind of lists as
    @class{date-time-type}, except that the fields @code{hour},
    @code{minute} and @code{second} are filled with dummy values from the
    Gregorian year AD 1.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}.  The ordering is partial
    except within a timezone, see the spec for details."))

(defmethod parse/xsd ((type date-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y m d tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^(-)?                     # opt. minus
                          ((?:[1-9]\\d*)?\\d{4})      # year
                          -(\\d\\d)                   # month
                          -(\\d\\d)                   # day
                          (([+-])(\\d\\d):(\\d\\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time minusp y m d "0" "0" "0" tz tz-sign tz-h tz-m
		:end 3)))


;;; gYearMonth

(defxsd (year-month-type "gYearMonth") (xsd-type time-ordering-mixin)
  ()
  (:documentation
   "@short{The gYearMonth data type, representing the calendar month of a
    specific year.}

    @b{Syntax.} This type accepts an ISO-like syntax.  For details refer to
    the @a[http://www.w3.org/TR/xmlschema-2/#gYearMonth]{specification}.

    @b{Implementation.} This type returns the same kind of lists as
    @class{date-time-type}, except that the fields @code{day}, @code{hour},
    @code{minute} and @code{second} are filled with dummy values from the
    Gregorian year AD 1.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}.  The ordering is partial
    except within a timezone, see the spec for details."))

(defmethod parse/xsd ((type year-month-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y m)
      (scan-to-strings "(?x)
                          ^(-)?                     # opt. minus
                          ((?:[1-9]\\d*)?\\d{4})      # year
                          -(\\d\\d)                   # month
                          $"
		       e)
    (parse-time minusp y m "1" "0" "0" "0" nil nil nil nil
		:end 2)))


;;; gYear

(defxsd (year-type "gYear") (xsd-type time-ordering-mixin)
  ()
  (:documentation
   "@short{The gYear data type, representing a calendar year.}

    @b{Syntax.} This type accepts an ISO-like syntax.  For details refer to
    the @a[http://www.w3.org/TR/xmlschema-2/#gYear]{specification}.

    @b{Implementation.} This type returns the same kind of lists as
    @class{date-time-type}, except that the fields @code{month}, @code{day},
    @code{hour}, @code{minute} and @code{second} are filled with dummy values
    from the Gregorian year AD 1.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}.  The ordering is partial
    except within a timezone, see the spec for details."))

(defmethod parse/xsd ((type year-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^(-)?                     # opt. minus
                          ((?:[1-9]\\d*)?\\d{4})      # year
                          (([+-])(\\d\\d):(\\d\\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time minusp y "1" "1" "0" "0" "0" tz tz-sign tz-h tz-m
		:end 1)))


;;; gMonthDay

(defxsd (month-day-type "gMonthDay") (xsd-type time-ordering-mixin)
  ()
  (:documentation
   "@short{The gMonthDay data type, representing a calendar month and day.}

    @b{Syntax.} This type accepts an ISO-like syntax.  For details refer to
    the @a[http://www.w3.org/TR/xmlschema-2/#monthDay]{specification}.

    @b{Implementation.} This type returns the same kind of lists as
    @class{date-time-type}, except that the fields @code{year},
    @code{hour}, @code{minute} and @code{second} are filled with dummy values
    from the Gregorian year AD 1.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}.  The ordering is partial
    except within a timezone, see the spec for details."))

(defmethod parse/xsd ((type month-day-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional m d tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^--(\\d\\d)                 # month
                          -(\\d\\d)                   # day
                          (([+-])(\\d\\d):(\\d\\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time nil "1" m d "0" "0" "0" tz tz-sign tz-h tz-m
		:start 1 :end 3)))


;;; gDay

(defxsd (day-type "gDay") (xsd-type time-ordering-mixin)
  ()
  (:documentation
   "@short{The gDay data type, representing a calendar day.}

    @b{Syntax.} This type accepts an ISO-like syntax.  For details refer to
    the @a[http://www.w3.org/TR/xmlschema-2/#gDay]{specification}.

    @b{Implementation.} This type returns the same kind of lists as
    @class{date-time-type}, except that the fields @code{year}, @code{month},
    @code{hour}, @code{minute} and @code{second} are filled with dummy values
    from the Gregorian year AD 1.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}.  The ordering is partial
    except within a timezone, see the spec for details."))

(defmethod parse/xsd ((type day-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional d tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ---(\\d\\d)                   # day
                          (([+-])(\\d\\d):(\\d\\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time nil "1" "1" d "0" "0" "0" tz tz-sign tz-h tz-m
		:start 3 :end 4)))


;;; gMonth

(defxsd (month-type "gMonth") (xsd-type time-ordering-mixin)
  ()
  (:documentation
   "@short{The gMonth data type, representing a calendar month.}

    @b{Syntax.} This type accepts an ISO-like syntax.  For details refer to
    the @a[http://www.w3.org/TR/xmlschema-2/#gMonth]{specification}.

    @b{Implementation.} This type returns the same kind of lists as
    @class{date-time-type}, except that the fields @code{year}, @code{day},
    @code{hour}, @code{minute} and @code{second} are filled with dummy values
    from the Gregorian year AD 1.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}.  The ordering is partial
    except within a timezone, see the spec for details."))

(defmethod parse/xsd ((type month-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional m tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^--(\\d\\d)                 # month
                          (([+-])(\\d\\d):(\\d\\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time nil "1" m "1" "0" "0" "0" tz tz-sign tz-h tz-m
		:start 2 :end 3)))


;;; boolean

(defxsd (boolean-type "boolean") (xsd-type)
  ()
  (:documentation
   "@short{The boolean data type.}

    @b{Syntax.} \"1\", \"0\",  \"true\", or \"false\".
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#boolean]{specification}.

    @b{Implementation.} This type returns @code{t} or @code{nil}.

    @b{Parameters.} No parameters except for @fun{pattern} are available for
    this type."))

(defmethod parse/xsd ((type boolean-type) e context)
  (declare (ignore context))
  (case (find-symbol e :keyword)
    ((:|true| :|1|) t)
    ((:|false| :|0|) nil)))


;;; base64Binary

(defxsd (base64-binary-type "base64Binary") (xsd-type length-mixin)
  ()
  (:documentation
   "@short{The base64Binary data type.}

    @b{Syntax.} Normal Base64 syntax.
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#base64Binary]{specification}.

    @b{Implementation.} This type returns an @code{(unsigned-byte 8)}
    vector.

    @b{Parameters.} This type allows restrictions on the length of the octet
    vector through the parameters @slot{exact-length}, @slot{min-length}, and
    @slot{max-length}."))

(defmethod equal-using-type ((type base64-binary-type) u v)
  (equalp u v))

(defmethod parse/xsd ((type base64-binary-type) e context)
  (declare (ignore context))
  (if (cl-ppcre:all-matches
       "(?x)
        ^(([A-Za-z0-9+/][ ]?[A-Za-z0-9+/][ ]?[A-Za-z0-9+/]
                  [ ]?[A-Za-z0-9+/][ ]?)*
           (([A-Za-z0-9+/][ ]?[A-Za-z0-9+/][ ]?[A-Za-z0-9+/][ ]?[A-Za-z0-9+/])
             | ([A-Za-z0-9+/][ ]?[A-Za-z0-9+/][ ]?[AEIMQUYcgkosw048][ ]?=)
             | ([A-Za-z0-9+/][ ]?[AQgw][ ]?=[ ]?=)))?$"
       e)
      (handler-case
	  (cl-base64:base64-string-to-usb8-array e)
	(warning (c)
	  (error "unexpected failure in Base64 decoding: ~A" c)))
      :error))


;;; hexBinary

(defxsd (hex-binary-type "hexBinary") (xsd-type length-mixin)
  ()
  (:documentation
   "@short{The hexBinary data type.}

    @b{Syntax.} A sequence of two-digit hexadecimal numbers representing
    one octet each.
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#hexBinary]{specification}.

    @b{Implementation.} This type returns an @code{(unsigned-byte 8)}
    vector.

    @b{Parameters.} This type allows restrictions on the length of the octet
    vector through the parameters @slot{exact-length}, @slot{min-length}, and
    @slot{max-length}."))

(defmethod equal-using-type ((type hex-binary-type) u v)
  (equalp u v))

(defmethod parse/xsd ((type hex-binary-type) e context)
  (declare (ignore context))
  (if (evenp (length e))
      (let ((result
	     (make-array (/ (length e) 2) :element-type '(unsigned-byte 8))))
	(loop
	   for i from 0 below (length e) by 2
	   for j from 0
	   do
	     (setf (elt result j)
		   (handler-case
		       (parse-integer e :start i :end (+ i 2) :radix 16)
		     (error ()
		       (return :error))))
	   finally (return result)))
      :error))


;;; float

(defxsd (float-type "float") (xsd-type ordering-mixin)
  ()
  (:documentation
   "@short{The float data type.}

    @b{Syntax.} A floating-point number in a \"scientific notation\".
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#float]{specification}.

    @b{Implementation.} This type returns a @code{single-float} or, on
    implementations where Infinity and Nan cannot be represented as such,
    a special symbol that is treated as if it was Infinity or NaN by the
    built-in ordering.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}."))

(defmethod equal-using-type ((type float-type) u v)
  #+(or sbcl allegro) (= u v)
  #-(or sbcl allegro) (float= u v))

(defmethod lessp-using-type ((type float-type) u v)
  #+(or sbcl allegro) (< u v)
  #-(or sbcl allegro) (float< u v))

;; this one is more complex than would seem necessary, because too-large
;; and too-small values must be rounded to infinity rather than erroring out
(defun parse-float (e min max +inf -inf nan)
  (cond
    ((equal e "INF") +inf)
    ((equal e "-INF") -inf)
    ((equal e "Nan") nan)
    (t
     (destructuring-bind (&optional a b)
	 (scan-to-strings "^([^eE]+)(?:[eE]([^eE]+))?$" e)
       (if a
	   (let* ((mantissa (parse/xsd (make-instance 'decimal-type) a nil))
		  (exponent
		   (when b
		     (parse/xsd (make-instance 'integer-type) b nil))))
	     (if (or (eq mantissa :error) (eq exponent :error))
		 :error
		 (let ((ratio (* mantissa (expt 10 (or exponent 1)))))
		   (cond
		     ((< ratio min) -inf)
		     ((> ratio max) +inf)
		     (t (float ratio min))))))
	   :error)))))

;; zzz nehme hier an, dass single-float in IEEE single float ist.
;; Das stimmt unter LispWorks bestimmt wieder nicht.
(defmethod parse/xsd ((type float-type) e context)
  (declare (ignore context))
  (parse-float e
	       most-negative-single-float
	       most-positive-single-float
	       single-float-positive-infinity
	       single-float-negative-infinity
	       single-float-nan))


;;; decimal

(defgeneric fraction-digits (data-type)
  (:documentation
   "@arg[data-type]{a subtype of @class{decimal-type}}
    @return{an integer, or @code{nil}}
    This slot reader returns the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-fractionDigits]{fractionDigits facet},
    or @code{nil} if none was specified.
    @see{total-digits}"))

(defgeneric total-digits (data-type)
  (:documentation
   "@arg[data-type]{a subtype of @class{decimal-type}}
    @return{an integer, or @code{nil}}
    This slot reader returns the type's
    @a[http://www.w3.org/TR/xmlschema-2/#rf-totalDigits]{totalDigits facet},
    or @code{nil} if none was specified.
    @see{fraction-digits}"))

(defxsd (decimal-type "decimal") (xsd-type ordering-mixin)
  ((fraction-digits :initform nil
		    :initarg :fraction-digits
		    :accessor fraction-digits)
   (total-digits :initform nil
		 :initarg :total-digits
		 :accessor total-digits))
  (:documentation
   "@short{The decimal data type.}

    @b{Syntax.} A rational number, written using an optional decimal point
    and decimal places.
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#decimal]{specification}.

    @b{Implementation.} This type returns a @code{rational}.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}.

    In addition, the facets @slot{fraction-digits} @slot{total-digits}
    are recognized."))

(defmethod describe-facets progn ((object decimal-type) stream)
  (dolist (slot '(fraction-digits total-digits))
    (let ((value (slot-value object slot)))
      (when value
	(format stream " ~A ~A"
		(intern (symbol-name slot) :keyword)
		value)))))

(defmethod parse-parameter
    ((class-name (eql 'decimal-type))
     (type-name t)
     (param (eql :fraction-digits))
     value)
  (parse (make-instance 'non-negative-integer-type) value nil))

(defmethod parse-parameter
    ((class-name (eql 'decimal-type))
     (type-name t)
     (param (eql :total-digits))
     value)
  (parse (make-instance 'positive-integer-type) value nil))

(defmethod lessp-using-type ((type decimal-type) u v)
  (< u v))

(defmethod equal-using-type ((type decimal-type) u v)
  (= u v))

(defmethod validp/xsd and ((type decimal-type) v context)
  (declare (ignore context))
  (with-slots (fraction-digits total-digits) type
    (and (or (null fraction-digits)
	     (let* ((betrag (abs v))
		    (fraction (- betrag (truncate betrag)))
		    (scaled (* fraction (expt 10 fraction-digits))))
	       (zerop (mod scaled 1))))
	 (or (null total-digits)
	     (let ((scaled (abs v)))
	       (loop
		  until (zerop (mod scaled 1))
		  do (setf scaled (* scaled 10)))
	       (< scaled (expt 10 total-digits)))))))

(defmethod parse/xsd ((type decimal-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional a b)
      (scan-to-strings "^([+-]?\\d*)(?:[.](\\d+))?$" e)
    (if (plusp (+ (length a) (length b)))
	(+ (if (plusp (length a))
	       (parse-integer a)
	       0)
	   (if (plusp (length b))
	       (/ (parse-integer b) (expt 10 (length b)))
	       0))
	:error)))


;;; double

(defxsd (double-type "double") (xsd-type ordering-mixin)
  ()
  (:documentation
   "@short{The double data type.}

    @b{Syntax.} A floating-point number in a \"scientific notation\".
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#double]{specification}.

    @b{Implementation.} This type returns a @code{double-float} or, on
    implementations where Infinity and Nan cannot be represented as such,
    a special symbol that is treated as if it was Infinity or NaN by the
    built-in ordering.

    @b{Parameters.} This type is ordered and allows the parameters
    @slot{max-inclusive}, @slot{min-inclusive},
    @slot{max-exclusive}, and @slot{min-exclusive}."))

(defmethod equal-using-type ((type double-type) u v)
  #+(or sbcl allegro) (= u v)
  #-(or sbcl allegro) (float= u v))

(defmethod lessp-using-type ((type double-type) u v)
  #+(or sbcl allegro) (< u v)
  #-(or sbcl allegro) (float< u v))

;; zzz nehme hier an, dass double-float in IEEE double float ist.
;; Auch das ist nicht garantiert.
(defmethod parse/xsd ((type double-type) e context)
  (declare (ignore context))
  (parse-float e
	       most-negative-double-float
	       most-positive-double-float
	       double-float-positive-infinity
	       double-float-negative-infinity
	       double-float-nan))


;;; AnyURi

(defxsd (any-uri-type "anyURI") (xsd-type length-mixin)
  ()
  (:documentation
   "@short{The anyURI data type.}

    @b{Syntax.} An arbitrary string (!).
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#anyURI]{specification}.

    @b{Implementation.} This type returns a normalized string in which
    special characters have been escaped.

    @b{Parameters.} This type allows restrictions on the length of the
    normalized string through the parameters @slot{exact-length},
    @slot{min-length}, and @slot{max-length}."))

(defmethod equal-using-type ((type any-uri-type) u v)
  (equal u v))

(defmethod parse/xsd ((type any-uri-type) e context)
  (cxml-rng::escape-uri e))


;;; QName
;;; NOTATION

(defclass qname-like (xsd-type length-mixin) ())

(defxsd (qname-type "QName") (qname-like)
  ()
  (:documentation
   "@short{The QName data type.}

    @b{Syntax.} A Qualified Name, as per the \"Namespaces in XML\"
    specification.  The namespace prefix must be bound to a namespace URI
    in the context.
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#QName]{specification}.

    @b{Context dependent.} This type is context dependent and requires
    the @code{context} argument to @fun{parse} and @fun{validp}.

    @b{Implementation.} This type returns a structure with two components,
    the namespace URI and the local name.  fixme: and the original length.
    fixme: export this structure.

    @b{Parameters.} This type allows restrictions on the length of the
    original QName through the parameters @slot{exact-length},
    @slot{min-length}, and @slot{max-length}."))

(defxsd (notation-type "NOTATION") (qname-like)
  ()
  (:documentation
   "@short{The NOTATION data type.}

    @b{Syntax.} A qualified name.
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#NOTATION]{specification}.

    @b{Implementation.} This type is treated exactly like
    @class{qname-type}, as specified in
    @a[http://relaxng.org/xsd-20010907.html]{Guidelines for using W3C XML
    Schema Datatypes with RELAX NG}.

    @b{Parameters.} This type allows restrictions on the length of the
    original QName through the parameters @slot{exact-length},
    @slot{min-length}, and @slot{max-length}."))

(defstruct (qname (:constructor make-qname (uri lname length)))
  uri
  lname
  length)

(defmethod length-using-type ((type qname-like) e)
  (qname-length e))

(defmethod equal-using-type ((type qname-like) u v)
  (and (equal (qname-uri u) (qname-uri v))
       (equal (qname-lname u) (qname-lname v))))

(defun namep (str)
  (and (not (zerop (length str)))
       (cxml::name-start-rune-p (elt str 0))
       (every #'cxml::name-rune-p str)))

(defmethod type-context-dependent-p ((type qname-like))
  t)

(defmethod parse/xsd ((type qname-like) e context)
  (handler-case
      (if (namep e)
	  (multiple-value-bind (prefix local-name) (cxml::split-qname e)
	    (let ((uri (when prefix
			 (context-find-namespace-binding context prefix))))
	      (if (and prefix (not uri))
		  :error
		  (make-qname uri local-name (length e)))))
	  :error)
    (cxml:well-formedness-violation ()
      :error)))


;;; string

(defxsd (xsd-string-type "string") (xsd-type length-mixin)
  ()
  (:documentation
   "@short{The string data type.}

    @b{Syntax.} An arbitrary string.
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#string]{specification}.

    @b{Implementation.} Returns the string unchanged.  This is the only
    XSD type that does not normalize or replace whitespace.

    @b{Parameters.} This type allows restrictions on the length of the
    string through the parameters @slot{exact-length},
    @slot{min-length}, and @slot{max-length}."))

(defmethod equal-using-type ((type xsd-string-type) u v)
  (equal u v))

(defmethod munge-whitespace ((type xsd-string-type) e)
  e)

(defmethod parse/xsd ((type xsd-string-type) e context)
  e)


;;;;
;;;; Derived types
;;;;

;;; normalizedString

(defxsd (normalized-string-type "normalizedString") (xsd-string-type)
  ()
  (:documentation
   "@short{The normalizedString data type, derived from string.}

    @b{Syntax.} An arbitrary string.
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#normalizedString]{specification}.

    @b{Implementation.} Returns the string with whitespace replaced.

    I.e., each whitespace character is replaced by a space
    (character code 32), but multiple spaces, as well as
    leading and trailing spaces will still be returned.

    (This is the only XSD type that replaces whitespace in this way.)

    @b{Parameters.} This type allows restrictions on the length of the
    normalized string through the parameters @slot{exact-length},
    @slot{min-length}, and @slot{max-length}."))

(defmethod munge-whitespace ((type normalized-string-type) e)
  (replace-whitespace e))


;;; token

(defxsd (xsd-token-type "token") (normalized-string-type)
  ()
  (:documentation
   "@short{The token data type, derived from normalizedString.}

    @b{Syntax.} An arbitrary string.
    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#token]{specification}.

    @b{Implementation.} Returns the string with normalized whitespace.

    I.e., each whitespace character is replaced by a space
    (character code 32), multiple spaces are collapsed into one character,
    and leading and trailing spaces will be removed.

    (This is the standard behaviour of all XSD types with the exception of
    token's supertypes @class{string-type} and @class{normalized-string-type}.)

    @b{Parameters.} This type allows restrictions on the length of the
    normalized string through the parameters @slot{exact-length},
    @slot{min-length}, and @slot{max-length}."))

(defmethod munge-whitespace ((type xsd-token-type) e)
  (normalize-whitespace e))


;;; language

(defmacro precompile (pattern)
  `(load-time-value (list (pattern-scanner ,pattern))))

(defxsd (language-type "language") (xsd-token-type)
  ((patterns :initform (precompile "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*")))
  (:documentation
   "@short{The language data type, derived from token.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#language]{specification}.

    @b{Restrictions.} This type restricts its supertype @class{token-type}
    to strings of the pattern \"[a-zA-Z]{1,8@}(-[a-zA-Z0-9]{1,8@})*\".

    @b{Parameters and implementation.} Unchanged from the supertype."))


;;; Name

(defxsd (name-type "Name") (xsd-token-type)
  ((patterns :initform (precompile "\\i\\c*")))
  (:documentation
   "@short{The Name data type, derived from token.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#Name]{specification}.

    @b{Restrictions.} This type restricts its supertype @class{token-type}
    to strings of the pattern \"\\i\\c*\".

    @b{Parameters and implementation.} Unchanged from the supertype."))


;;; NCName

(defxsd (ncname-type "NCName") (name-type)
  ((patterns :initform (precompile "[\\i-[:]][\\c-[:]]*")))
  (:documentation
   "@short{The NCName data type, derived from Name.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#NCName]{specification}.

    @b{Restrictions.} This type restricts its supertype @class{name-type}
    to strings of the pattern \"[\\i-[:]][\\c-[:]]*\".

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod equal-using-type ((type ncname-type) u v)
  (equal u v))

(defmethod parse/xsd ((type ncname-type) e context)
  e)


;;; ID

(defxsd (xsd-id-type "ID") (ncname-type)
  ()
  (:documentation
   "@short{The ID data type, derived from NCName.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#ID]{specification}.

    @b{Restrictions.} None.

    @b{ID type.} This type has the ID-type 'ID'for the purposes of DTD
    compatibility. See @a[http://relaxng.org/xsd-20010907.html]{Guidelines
    for using W3C XML Schema Datatypes with RELAX NG}.

    @b{Parameters and implementation.} Unchanged from the supertype.

    @see{id-type}"))

(defmethod type-id-type ((type xsd-id-type))
  :id)


;;; IDREF

(defxsd (xsd-idref-type "IDREF") (xsd-id-type)
  ()
  (:documentation
   "@short{The IDREF data type, derived from ID.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#IDREF]{specification}.

    @b{Restrictions.} None.

    @b{ID type.} This type has the ID-type 'IDREF'for the purposes of DTD
    compatibility. See @a[http://relaxng.org/xsd-20010907.html]{Guidelines
    for using W3C XML Schema Datatypes with RELAX NG}.

    @b{Parameters and implementation.} Unchanged from the supertype.

    @see{idref-type}"))

(defmethod type-id-type ((type xsd-idref-type))
  :idref)


;;; IDREFS

(defxsd (xsd-idrefs-type "IDREFS") (enumeration-type)
  ((word-type :initform (make-instance 'xsd-idref-type)))
  (:documentation
   "@short{The IDREFS data type, an enumeration.}

    @b{Syntax.} A whitespace-separated sequence of @class{xsd-idref-type}
    values, with at least one element.

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#IDREFS]{specification}.

    @b{Implementation.} This type returns a list of the values as returned by
    @class{xsd-idref-type}.

    @b{ID type.} This type has the ID-type 'IDREFS'for the purposes of DTD
    compatibility. See @a[http://relaxng.org/xsd-20010907.html]{Guidelines
    for using W3C XML Schema Datatypes with RELAX NG}.

    @b{Parameters.} This type allows restrictions on the number of values
    through the parameters @slot{exact-length}, @slot{min-length}, and
    @slot{max-length}.

    @see{idrefs-type}"))

(defmethod type-id-type ((type xsd-idrefs-type))
  :idrefs)


;;; ENTITY

(defxsd (entity-type "ENTITY") (ncname-type)
  ()
  (:documentation
   "@short{The ENTITY data type, derived from NCName.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#ENTITY]{specification}.

    @b{Restrictions.} This type restricts its supertype @class{ncname-type}
    to names that have been declared as unparsed entities in the context.

    @b{Context dependent.} This type is context dependent and requires
    the @code{context} argument to @fun{parse} and @fun{validp}.

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod type-context-dependent-p ((type entity-type))
  t)

(defmethod parse/xsd ((type entity-type) e context)
  (if (context-find-unparsed-entity context e)
      e
      :error))


;;; ENTITIES

(defxsd (entities-type "ENTITIES") (enumeration-type)
  ((word-type :initform (make-instance 'entity-type)))
  (:documentation
   "@short{The ENTITIES data type, an enumeration.}

    @b{Syntax.} A whitespace-separated sequence of @class{entity-type}
    values, with at least one element.

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#ENTITIES]{specification}.

    @b{Implementation.} This type returns a list of the values as returned by
    @class{entity-type}.

    @b{Context dependent.} This type is context dependent and requires
    the @code{context} argument to @fun{parse} and @fun{validp}.

    @b{Parameters.} This type allows restrictions on the number of values
    through the parameters @slot{exact-length}, @slot{min-length}, and
    @slot{max-length}."))


;;; NMTOKEN

(defxsd (nmtoken-type "NMTOKEN") (xsd-token-type)
  ((patterns :initform (precompile "\\c+")))
  (:documentation
   "@short{The NMTOKEN data type, derived from token.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#NMTOKEN]{specification}.

    @b{Restrictions.} This type restricts its supertype @class{token-type}
    to strings of the pattern \"\\c+\".

    @b{Parameters and implementation.} Unchanged from the supertype."))


;;; NMTOKENS

(defxsd (nmtokens-type "NMTOKENS") (enumeration-type)
  ((word-type :initform (make-instance 'nmtoken-type)))
  (:documentation
   "@short{The NMTOKENS data type, an enumeration.}

    @b{Syntax.} A whitespace-separated sequence of @class{nmtoken-type}
    values, with at least one element.

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#NMTOKENS]{specification}.

    @b{Implementation.} This type returns a list of the values as returned by
    @class{nmtoken-type}.

    @b{Parameters.} This type allows restrictions on the number of values
    through the parameters @slot{exact-length}, @slot{min-length}, and
    @slot{max-length}."))


;;; integer

(defxsd (integer-type "integer") (decimal-type)
  ()
  (:documentation
   "@short{The integer data type, derived from decimal.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#integer]{specification}.

    @b{Syntax.} An integer, written it the decimal system without leading
    zeros.  No decimal point is permitted.

    @b{Implementation.} This type returns an @code{integer}.

    @b{Parameters and implementation.} Unchanged from the supertype."))

;; period is forbidden, so there's no point in letting decimal handle parsing
;; fixme: sind fuehrende nullen nun erlaubt oder nicht?  die spec sagt ja,
;; das pattern im schema nicht.
(defmethod parse/xsd ((type integer-type) e context)
  (declare (ignore context))
  (if (cl-ppcre:all-matches "^[+-]?(?:[1-9]\\d*|0)$" e)
      (parse-number:parse-number e)
      :error))


;;; nonPositiveInteger

(defxsd (non-positive-integer-type "nonPositiveInteger") (integer-type)
  ()
  (:documentation
   "@short{The nonPositiveInteger data type, derived from integer.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#nonPositiveInteger]{specification}.

    @b{Restrictions.} This type allows only values <= 0.

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defun min* (a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t (min a b))))

(defun max* (a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t (max a b))))

(defmethod initialize-instance :after ((type non-positive-integer-type) &key)
  (setf (max-inclusive type)
	(min* 0 (max-inclusive type))))


;;; nonPositiveInteger

(defxsd (negative-integer-type "negativeInteger") (non-positive-integer-type)
  ()
  (:documentation
   "@short{The negativeInteger data type, derived from nonPositiveInteger.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#negativeInteger]{specification}.

    @b{Restrictions.} This type allows only values < 0.

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type negative-integer-type) &key)
  (setf (max-inclusive type)
	(min* -1 (max-inclusive type))))


;;; long

(defxsd (long-type "long") (integer-type)
  ()
  (:documentation
   "@short{The long data type, derived from integer.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#long]{specification}.

    @b{Restrictions.} This type allows only values from the interval
    [-2^63, 2^63-1].

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type long-type) &key)
  (setf (max-inclusive type) (min* 9223372036854775807 (max-inclusive type)))
  (setf (min-inclusive type) (max* -9223372036854775808 (min-inclusive type))))


;;; int

(defxsd (int-type "int") (long-type)
  ()
  (:documentation
   "@short{The int data type, derived from long.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#int]{specification}.

    @b{Restrictions.} This type allows only values from the interval
    [-2^31, 2^31-1].

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type int-type) &key)
  (setf (max-inclusive type) (min* 2147483647 (max-inclusive type)))
  (setf (min-inclusive type) (max* -2147483648 (min-inclusive type))))


;;; short

(defxsd (short-type "short") (int-type)
  ()
  (:documentation
   "@short{The short data type, derived from int.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#short]{specification}.

    @b{Restrictions.} This type allows only values from the interval
    [-2^15, 2^15-1].

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type short-type) &key)
  (setf (max-inclusive type) (min* 32767 (max-inclusive type)))
  (setf (min-inclusive type) (max* -32768 (min-inclusive type))))


;;; byte

(defxsd (byte-type "byte") (short-type)
  ()
  (:documentation
   "@short{The byte data type, derived from short.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#byte]{specification}.

    @b{Restrictions.} This type allows only values from the interval
    [-128, 127].

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type byte-type) &key)
  (setf (max-inclusive type) (min* 127 (max-inclusive type)))
  (setf (min-inclusive type) (max* -128 (min-inclusive type))))


;;; nonNegativeInteger

(defxsd (non-negative-integer-type "nonNegativeInteger") (integer-type)
  ()
  (:documentation
   "@short{The nonNegativeInteger data type, derived from integer.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#nonNegativeInteger]{specification}.

    @b{Restrictions.} This type allows only values >= 0.

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type non-negative-integer-type) &key)
  (setf (min-inclusive type) (max* 0 (min-inclusive type))))


;;; unsignedLong

(defxsd (unsigned-long-type "unsignedLong") (non-negative-integer-type)
  ()
  (:documentation
   "@short{The unsignedLong data type, derived from nonNegativeInteger.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#unsignedLong]{specification}.

    @b{Restrictions.} This type allows only values from the interval
    [0, 2^64-1].

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type unsigned-long-type) &key)
  (setf (max-inclusive type) (min* 18446744073709551615 (max-inclusive type))))


;;; unsignedInt

(defxsd (unsigned-int-type "unsignedInt") (unsigned-long-type)
  ()
  (:documentation
   "@short{The unsignedInt data type, derived from unsignedLong.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#unsignedInt]{specification}.

    @b{Restrictions.} This type allows only values from the interval
    [0, 2^32-1].

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type unsigned-int-type) &key)
  (setf (max-inclusive type) (min* 4294967295 (max-inclusive type))))


;;; unsignedShort

(defxsd (unsigned-short-type "unsignedShort") (unsigned-int-type)
  ()
  (:documentation
   "@short{The unsignedShort data type, derived from unsignedInt.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#unsignedShort]{specification}.

    @b{Restrictions.} This type allows only values from the interval
    [0, 2^16-1].

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type unsigned-short-type) &key)
  (setf (max-inclusive type) (min* 65535 (max-inclusive type))))


;;; unsignedByte

(defxsd (unsigned-byte-type "unsignedByte") (unsigned-short-type)
  ()
  (:documentation
   "@short{The unsignedByte data type, derived from unsignedInt.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#unsignedByte]{specification}.

    @b{Restrictions.} This type allows only values from the interval
    [0, 255].

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type unsigned-byte-type) &key)
  (setf (max-inclusive type) (min* 255 (max-inclusive type))))


;;; positiveInteger

(defxsd (positive-integer-type "positiveInteger") (non-negative-integer-type)
  ()
  (:documentation
   "@short{The positiveInteger data type, derived from nonNegativeInteger.}

    C.f. the @a[http://www.w3.org/TR/xmlschema-2/#positiveInteger]{specification}.

    @b{Restrictions.} This type allows only values > 0.

    @b{Parameters and implementation.} Unchanged from the supertype."))

(defmethod initialize-instance :after ((type positive-integer-type) &key)
  (setf (min-inclusive type) (max* 1 (min-inclusive type))))


;;;; backpatch ID types

(defvar *id-type* (make-instance 'id-type))
(defvar *idref-type* (make-instance 'idref-type))
(defvar *idrefs-type* (make-instance 'idrefs-type))
