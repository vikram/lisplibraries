;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

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


;;;; Errors

(define-condition rng-error (simple-error)
  ((line-number :initarg :line-number :accessor rng-error-line-number)
   (column-number :initarg :column-number :accessor rng-error-column-number)
   (system-id :initarg :system-id :accessor rng-error-system-id))
  (:documentation
   "@short{The class of all validation and schema parsing errors.}

    Signalled while parsing a schema, this error signifies that the schema
    is incorrect (or not compatible with DTD Compatibility).  Signalled
    during validation, this error signifies that the document is invalid
    (or not sound).

    When parsing or validating with DTD Compatibility, check for
    @code{dtd-compatibility-error} to distinguish between
    correctness and compatibility or validity and soundness.

    @see-slot{rng-error-line-number}
    @see-slot{rng-error-column-number}
    @see-slot{rng-error-system-id}"))

(define-condition dtd-compatibility-error (rng-error)
  ()
  (:documentation
   "@short{The class of DTD compatibility errors.}

    Signalled while parsing a schema, this error signifies that the schema
    is not compatible (as opposed to incorrect).

    Signalled during validation, this error signifies that the document
    is not sound (as opposed to invalid)."))

(setf (documentation 'rng-error-line-number 'function)
      "@arg[instance]{an instance of @class{rng-error}}
       @return{an integer, or nil}
       Return the line number reported by the parser when the Relax NG error
       was detected, or NIL if not available.")

(setf (documentation 'rng-error-column-number 'function)
      "@arg[instance]{an instance of @class{rng-error}}
       @return{an integer, or nil}
       Return the column number reported by the parser when the Relax NG error
       was detected, or NIL if not available.")

(setf (documentation 'rng-error-system-id 'function)
      "@arg[instance]{an instance of @class{rng-error}}
       @return{a puri:uri, or nil}
       Return the System ID of the document being parsed when the Relax NG
       error was detected, or NIL if not available.")

(defvar *error-class* 'rng-error)

(defun rng-error (source fmt &rest args)
  "@unexport{}"
  (let ((s (make-string-output-stream)))
    (apply #'format s fmt args)
    (multiple-value-bind (line-number column-number system-id)
	(etypecase source
	  (null)
	  (klacks:source
	   (values (klacks:current-line-number source)
		   (klacks:current-column-number source)
		   (klacks:current-system-id source)))
	  (sax:sax-parser-mixin
	   (values (sax:line-number source)
		   (sax:column-number source)
		   (sax:system-id source))))
      (when (or line-number column-number system-id)
	(format s "~&  [ Error at line ~D, column ~D in ~S ]"
		line-number
		column-number
		system-id))
      (error *error-class*
	     :format-control "~A"
	     :format-arguments (list (get-output-stream-string s))
	     :line-number line-number
	     :column-number column-number
	     :system-id system-id))))


;;;; Parser

(defvar *datatype-library*)
(defvar *namespace-uri*)
(defvar *ns*)
(defvar *entity-resolver*)
(defvar *external-href-stack*)
(defvar *include-uri-stack*)
(defvar *include-body-p* nil)
(defvar *grammar*)

(defvar *debug* nil)

(defstruct (schema
	     (:constructor make-schema (start definitions)))
  "An instance of this class represents a Relax NG grammar that has
   been parsed and simplified.
   @see-slot{schema-start}
   @see-constructor{parse-schema}
   @see{make-validator}
   @see{serialize-schema} "
  (start (missing) :type pattern)
  (definitions (missing) :type list)
  (interned-start nil :type (or null pattern))
  (registratur nil :type (or null hash-table))
  (compatibility-table nil :type (or null compatibility-table)))

(setf (documentation 'schema-start 'function)
      "@arg[instance]{an instance of @class{schema}}
       @return{the start pattern, an instance of @class{pattern}}
       Reader function for the grammar's start pattern, from which all
       of the grammar's patters are reachable.")

(defmethod print-object ((object schema) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defun invoke-with-klacks-handler (fn source)
  (if *debug*
      (funcall fn)
      (handler-case
	  (funcall fn)
	(cxml:xml-parse-error (c)
	  (rng-error source "Cannot parse schema: ~A" c)))))

(defvar *validate-grammar* t)
(defvar *process-dtd-compatibility*)
(defparameter *relax-ng-grammar* nil)
(defparameter *compatibility-grammar* nil)

(defun flush ()
  (setf *relax-ng-grammar* nil)
  (setf *compatibility-grammar* nil))

(defun make-validating-source (input schema)
  "@arg[input]{a @code{source} or a stream designator}
   @arg[schema]{the parsed Relax NG @class{schema} object}
   @return{a klacks source}
   @short{This function creates a klacks source for @code{input} that validates
   events against @code{schema}.}

   Input can be a klacks source or any argument applicable to
   @code{cxml:make-source}.

   @see{parse-schema}
   @see{make-validator}"
  (klacks:make-tapping-source (if (typep input 'klacks:source)
				  input
				  (cxml:make-source input))
			      (make-validator schema)))

(defun make-schema-source (input)
  (let ((upstream (cxml:make-source input)))
    (if *validate-grammar*
	(let ((handler (make-validator *relax-ng-grammar*)))
	  (when *process-dtd-compatibility*
	    (setf handler
		  (cxml:make-broadcast-handler
		   handler
		   (multiple-value-bind (h v)
		       (make-validator *compatibility-grammar*)
		     (setf (validation-error-class v) 'dtd-compatibility-error)
		     h))))
	  (klacks:make-tapping-source upstream handler))
	upstream)))

(defun parse-schema (input &key entity-resolver (process-dtd-compatibility t))
  "@arg[input]{a string, pathname, stream, or xstream}
   @arg[entity-resolver]{a function of two arguments, or NIL}
   @arg[process-dtd-compatibility]{a boolean}
   @return{a parsed @class{schema}}
   @short{This function parses a Relax NG schema file in XML syntax}
   and returns a parsed representation of that schema.

   @code{input} can be any stream designator as understood by
   @code{cxml:make-source}.

   Note that namestrings are not valid arguments,
   because they would be interpreted as XML source code.  Use pathnames
   instead.

   @code{entity-resolver} can be passed as a function of two arguments.
   It is invoked for every entity referenced by the
   document with the entity's Public ID (a rod) and System ID (an
   URI object) as arguments.  The function may either return
   nil, CXML will then try to resolve the entity as usual.
   Alternatively it may return a Common Lisp stream specialized on
   @code{(unsigned-byte 8)} which will be used instead.

   If @code{process-dtd-compatibility} is true, the schema will be checked
   for @em{compatibility} with Relax NG DTD Compatibility, and default values
   will be recorded.  (Without @code{process-dtd-compatibility}, the schema
   will not be checked @em{compatibility}, and annotations for
   DTD Compatibility will be ignored like any other foreign element.)

   @see{parse-compact}
   @see{make-validator}"
  (when *validate-grammar*
    (unless *relax-ng-grammar*
      (let* ((*validate-grammar* nil)
	     (d (slot-value (asdf:find-system :cxml-rng)
			    'asdf::relative-pathname)))
	#+(or) (parse-compact (merge-pathnames "rng.rnc" d))
	(setf *relax-ng-grammar*
	      (parse-schema (merge-pathnames "rng.rng" d)))
	(setf *compatibility-grammar*
	      (parse-schema (merge-pathnames "compatibility.rng" d))))))
  (let ((*process-dtd-compatibility* process-dtd-compatibility))
    (klacks:with-open-source (source (make-schema-source input))
      (invoke-with-klacks-handler
       (lambda ()
	 (klacks:find-event source :start-element)
	 (let* ((*datatype-library* "")
		(*namespace-uri* "")
		(*entity-resolver* entity-resolver)
		(*external-href-stack* '())
		(*include-uri-stack* '())
		(*grammar* (make-grammar nil))
		(start (p/pattern source)))
	   (unless start
	     (rng-error nil "empty grammar"))
	   (setf (grammar-start *grammar*)
		 (make-definition :name :start :child start))
	   (check-pattern-definitions source *grammar*)
	   (check-recursion start 0)
	   (multiple-value-bind (new-start defns)
	       (finalize-definitions start)
	     (setf start (fold-not-allowed new-start))
	     (dolist (defn defns)
	       (setf (defn-child defn) (fold-not-allowed (defn-child defn))))
	     (setf start (fold-empty start))
	     (dolist (defn defns)
	       (setf (defn-child defn) (fold-empty (defn-child defn)))))
	   (multiple-value-bind (new-start defns)
	       (finalize-definitions start)
	     (check-start-restrictions new-start)
	     (dolist (defn defns)
	       (check-restrictions (defn-child defn)))
	     (let ((schema (make-schema new-start defns)))
	       (when *process-dtd-compatibility*
		 (check-schema-compatibility schema defns))
	       schema))))
       source))))


;;;; pattern structures

(defstruct pattern
  "@short{The superclass of all patterns.}
   Instances of this class represent elements in the \"simplified syntax\"
   of Relax NG.

   Patterns are documented for introspective purposes and are not meant to
   be modified by user code.

   The start pattern of a schema is available through @fun{schema-start}.

   @see{schema}"
  (nullable :uninitialized))

(defmethod print-object :around ((object pattern) stream)
  (if *debug*
      (let ((*print-circle* t))
	(call-next-method))
      (print-unreadable-object (object stream :type t :identity t))))

(defstruct (%parent (:include pattern) (:conc-name "PATTERN-"))
  child)

(defstruct (%named-pattern (:include %parent) (:conc-name "PATTERN-"))
  name)

(setf (documentation 'pattern-name 'function)
      "@arg[instance]{an instance of @class{pattern}}
       @return{a @class{name-class}}
       @short{Returns the @code{pattern}'s name class.}

       This slot describes the name allowed for the current element or
       attribute.

       @see{element}
       @see{attribute}")

(setf (documentation 'pattern-child 'function)
      "@arg[instance]{an instance of @class{pattern}}
       @return{an instance of @class{pattern}}
       @short{Returns the pattern's sub-pattern.}

       (Elements in the full Relax NG syntax allow more than one child
       pattern, but simplification normalizes the representation so that
       any such element has exactly one child.)

       @see{element}
       @see{attribute}
       @see{one-or-more}
       @see{list-pattern}
       @see{choice}")

(defstruct (element (:include %named-pattern))
  "@short{This pattern specifies that an element of a certain name class
   is required.}

   Its child pattern describes the attributes and child nodes
   of this element.
   @see-slot{pattern-name}
   @see-slot{pattern-child}")

(defstruct (attribute (:include %named-pattern)
		      (:conc-name "PATTERN-")
		      (:constructor make-attribute (default-value)))
  "@short{This pattern specifies that an attribute of a certain name class
   is required.}

   Its child pattern describes the type of the attribute's
   contents.
   @see-slot{pattern-name}
   @see-slot{pattern-child}"
  default-value)

(defstruct (%combination (:include pattern) (:conc-name "PATTERN-"))
  a b)

(setf (documentation 'pattern-a 'function)
      "@arg[instance]{an instance of @class{pattern}}
       @return{an instance of @class{pattern}}
       @short{Returns the first of two sub-patterns the pattern instance has.}

       (Elements in the full Relax NG syntax allow more than two child
       patterns, but simplification normalizes the representation so that
       any such element has exactly two children.)

       @see{pattern-b}
       @see{group}
       @see{interleave}
       @see{choice}")

(setf (documentation 'pattern-b 'function)
      "@arg[instance]{an instance of @class{pattern}}
       @return{an instance of @class{pattern}}
       @short{Returns the second of two sub-patterns the pattern instance has.}

       (Elements in the full Relax NG syntax allow more than two child
       patterns, but simplification normalizes the representation so that
       any such element has exactly two children.)

       @see{pattern-a}
       @see{group}
       @see{interleave}
       @see{choice}")

(defstruct (group
	    (:include %combination)
	    (:constructor make-group (a b)))
  "@short{This pattern specifies that two subpatterns are
   required at the current position in a specific order.}

   @see-slot{pattern-a}
   @see-slot{pattern-b}")
(defstruct (interleave
	    (:include %combination)
	    (:constructor make-interleave (a b)))
  "@short{This pattern specifies that two possible subpatterns are
   allowed to occur in any order at the current position.}

   @see-slot{pattern-a}
   @see-slot{pattern-b}")
(defstruct (choice
	    (:include %combination)
	    (:constructor make-choice (a b)))
  "@short{This pattern specifies that one of two possible subpatterns are
   allowed at the current position, given as its children.}

   @see-slot{pattern-a}
   @see-slot{pattern-b}")
(defstruct (after
	    (:include %combination)
	    (:constructor make-after (a b))))

(defstruct (one-or-more
	    (:include %parent)
	    (:constructor make-one-or-more (child)))
  "@short{This pattern specifies that its subpattern is
   allowed to occur at the current position one or more times.}

   @see-slot{pattern-child}")
(defstruct (list-pattern
	    (:include %parent)
	    (:constructor make-list-pattern (child)))
  "@short{This pattern specifies that a subpatterns is allowed multiple
   times a the current position, with whitespace as a separator.}

   @see-slot{pattern-child}")

(defstruct (ref
	    (:include pattern)
	    (:conc-name "PATTERN-")
	    (:constructor make-ref (target)))
  "@short{This pattern references another part of the pattern graph.}

   @code{ref} is the only pattern to introduce shared structure and
   circularity into the pattern graph, by referring to elements defined
   elsewhere.

   (@code{ref} patterns in the full Relax NG syntax can be used to refer
   to any pattern definition in the grammar.  Simplification normalizes
   the schema so that ref patterns only refer to definitions which have
   an @code{element} as their child.)

   @see-slot{pattern-element}"
  crdepth
  target)

(defun pattern-element (ref)
  "@arg[ref]{an instance of @class{ref}}
   @return{an instance of @class{element}}
   @short{Returns the ref pattern's target.}

   @code{ref} is the only pattern to introduce shared structure and
   circularity into the pattern graph, by referring to elements defined
   elsewhere.

   (@code{ref} patterns in the full Relax NG syntax can be used to refer
   to any pattern definition in the grammar.  Simplification normalizes
   the schema so that ref patterns only refer to definitions which have
   an @code{element} as their child.)"
  (defn-child (pattern-target ref)))

(defstruct (%leaf (:include pattern)))

(defstruct (empty (:include %leaf))
  "@short{This pattern specifies that nothing more is expected at the current
   position.}")

(defstruct (text (:include %leaf))
  "@short{This pattern specifies that text is expected here.}")

(defstruct (%typed-pattern (:include %leaf) (:conc-name "PATTERN-"))
  type)

(setf (documentation 'pattern-type 'function)
      "@arg[instance]{an instance of @class{pattern}}
       @return{a @class{cxml-types:data-type}}
       @short{Returns the data type expected at this position.}

       This type has already been parsed into an object.  Its name and
       the URI of its library can be queried from that object.

       @see{data}
       @see{value}
       @see{cxml-types:type-name}
       @see{cxml-types:type-library}")

(defstruct (value (:include %typed-pattern) (:conc-name "PATTERN-"))
  "@short{This pattern specifies that a specific value is expected as text
   here.}

   The value expected is @code{pattern-value}, parsed from
   @code{pattern-string} using @code{pattern-type}.

   @see-slot{pattern-type}
   @see-slot{pattern-value}
   @see-slot{pattern-string}"
  ns
  string
  value)

(setf (documentation 'pattern-string 'function)
      "@arg[instance]{an instance of @class{value}}
       @return{a string}
       @short{Returns the string expected at this position.}

       This string is the lexical representation expected, not parsed into
       a value object yet.  The parsed object is available as
       @fun{pattern-value}.

       @see{pattern-type}")

(setf (documentation 'pattern-value 'function)
      "@arg[instance]{an instance of @class{value}}
       @return{an object as returned by @fun{cxml-types:parse}}
       @short{Returns the value expected at this position.}

       This object is the result of parsing @fun{pattern-string} using
       @fun{pattern-type}.")

(defstruct (data (:include %typed-pattern) (:conc-name "PATTERN-"))
  "@short{This pattern specifies that text of a specific data type is
   expected.}

   The data type instance stored in the @code{pattern-type} slot takes into
   account additional paramaters, which can be retrieved using
   @code{pattern-params} in their original form.

   @see-slot{pattern-type}
   @see-slot{pattern-params}
   @see-slot{pattern-except}"
  params
  except)

(setf (documentation 'pattern-except 'function)
      "@arg[instance]{an instance of @class{data}}
       @return{a @class{pattern}, or @code{nil}}
       @short{Returns the @code{data} instance's @code{except} pattern.}

       In addition to a data type, @code{data} can specify that certain
       values are @em{not} permitted.  They are described using a pattern.

       If this slot is @code{nil}, no exception is defined.")

(setf (documentation 'pattern-params 'function)
      "@arg[instance]{an instance of @class{data}}
       @return{a list of @fun{cxml-types:param}}
       @short{The data type parameters for this data pattern.}

       (With the XSD type library, these are known as restricting facets.)")

(defstruct (not-allowed (:include %leaf))
  "@short{This pattern specifies that the part of the schema reached at
   this point is not valid.}")


;;;; non-pattern

(defstruct (grammar (:constructor make-grammar (parent)))
  (start nil)
  parent
  (definitions (make-hash-table :test 'equal)))

;; Clark calls this structure "RefPattern"
(defstruct (definition (:conc-name "DEFN-"))
  name
  combine-method
  head-p
  redefinition
  child)

(defstruct (compatibility-table (:conc-name "DTD-"))
  (elements (make-hash-table :test 'equal) :type hash-table))

(defstruct (dtd-member (:conc-name "DTD-"))
  (name (error "missing") :type name))

(defstruct (dtd-element
	     (:include dtd-member)
	     (:conc-name "DTD-")
	     (:constructor make-dtd-element (name)))
  (attributes (make-hash-table :test 'equal) :type hash-table))

(defstruct (dtd-attribute
	     (:include dtd-member)
	     (:conc-name "DTD-")
	     (:constructor make-dtd-attribute (name)))
  (default-value nil :type (or null string))
  (id-type :unknown :type (member :unknown nil :id :idref :idrefs))
  (value-declared-by nil :type list)
  (id-type-declared-by nil :type list))

(defun getname (name table)
  (gethash (list (name-uri name) (name-lname name)) table))

(defun (setf getname) (newval name table)
  (setf (gethash (list (name-uri name) (name-lname name)) table) newval))

(defun ensure-dtd-element (element compatibility-table)
  (let ((elements (dtd-elements compatibility-table))
	(element-name (pattern-name element)))
    (or (getname element-name elements)
	(setf (getname element-name elements)
	      (make-dtd-element element-name)))))

(defun ensure-dtd-attribute (attribute-name element table)
  (let* ((dtd-element (ensure-dtd-element element table))
	 (attributes (dtd-attributes dtd-element))
	 (a (getname attribute-name attributes)))
    (cond
      (a
       (values a t))
      (t
       (setf a (make-dtd-attribute attribute-name))
       (setf (getname attribute-name attributes) a)
       (values a nil)))))


;;; name-class

(defun missing ()
  (error "missing arg"))

(defstruct name-class
  "@short{The abstract superclass of all name-related classes.}

   Name classes represent sets of permissible names for an element or
   attribute.

   Names are pairs of namespace URI and local-name.

   @see{attribute}
   @see{element}")

(defstruct (any-name (:include name-class)
		     (:constructor make-any-name (except)))
  "@short{This name class allows any name.}

   Exceptions are given as @code{any-name-except}.

   @see-slot{any-name-except}"
  (except (missing) :type (or null name-class)))

(setf (documentation 'any-name-except 'function)
      "@arg[instance]{an instance of @class{any-name}}
       @return{a @class{name-class} or @code{nil}}

       Return the name class @em{not} allowed by this @code{any-name},
       or @code{nil} if there is no such exception.")

(defstruct (name (:include name-class)
		 (:constructor make-name (uri lname)))
  "@short{This name class allows only a specific name.}

   A specific namespace URI and local name are expected. 

   @see-slot{name-uri}
   @see-slot{name-lname}"
  (uri (missing) :type string)
  (lname (missing) :type string))

(setf (documentation 'name-uri 'function)
      "@arg[instance]{an instance of @class{name}}
       @return{a string}
       Return the expected namespace URI.")

(setf (documentation 'name-lname 'function)
      "@arg[instance]{an instance of @class{name}}
       @return{a string}
       Return the expected local name.")

(defstruct (ns-name (:include name-class)
		    (:constructor make-ns-name (uri except)))
  "@short{This name class allows all names in a specific namespace}, with
   possible exceptions.

   A specific namespace URI is expected. 

   Exceptions are given as @code{ns-name-except}.

   @see-slot{ns-name-uri}
   @see-slot{ns-name-except}"
  (uri (missing) :type string)
  (except (missing) :type (or null name-class)))

(setf (documentation 'ns-name-uri 'function)
      "@arg[instance]{an instance of @class{ns-name}}
       @return{a string}
       Return the expected namespace URI.")

(setf (documentation 'ns-name-except 'function)
      "@arg[instance]{an instance of @class{ns-name}}
       @return{a @class{name-class} or @code{nil}}

       Return the name class @em{not} allowed by this @code{ns-name},
       or @code{nil} if there is no such exception.")

(defstruct (name-class-choice (:include name-class)
			      (:constructor make-name-class-choice (a b)))
  "@short{This name class represents the union of two other name classes.}

   @see-slot{name-class-choice-a}
   @see-slot{name-class-choice-b}"
  (a (missing) :type name-class)
  (b (missing) :type name-class))

(setf (documentation 'name-class-choice-a 'function)
      "@arg[instance]{an instance of @class{name-class-choice}}
       @return{a @class{name-class}}
       Returns the 'first' of two name classes that are allowed.
       @see{name-class-choice-b}")

(setf (documentation 'name-class-choice-b 'function)
      "@arg[instance]{an instance of @class{name-class-choice}}
       @return{a @class{name-class}}
       Returns the 'second' of two name classes that are allowed.
       @see{name-class-choice-a}")

(defun simplify-nc-choice (values)
  (zip #'make-name-class-choice values))


;;;; parser

(defvar *rng-namespace* "http://relaxng.org/ns/structure/1.0")

(defun skip-foreign* (source)
  (loop
    (case (klacks:peek-next source)
      (:start-element (skip-foreign source))
      (:end-element (return)))))

(defun skip-to-native (source)
  (loop
    (case (klacks:peek source)
      (:start-element
	(when (equal (klacks:current-uri source) *rng-namespace*)
	  (return))
	(klacks:serialize-element source nil))
      (:end-element (return)))
    (klacks:consume source)))

(defun consume-and-skip-to-native (source)
  (klacks:consume source)
  (skip-to-native source))

(defun skip-foreign (source)
  (when (equal (klacks:current-uri source) *rng-namespace*)
    (rng-error source
	       "invalid schema: ~A not allowed here"
	       (klacks:current-lname source)))
  (klacks:serialize-element source nil))

(defun attribute (lname attrs)
  "@unexport{}"
  (let ((a (sax:find-attribute-ns "" lname attrs)))
    (if a
	(sax:attribute-value a)
	nil)))

(defparameter *whitespace*
    (format nil "~C~C~C~C"
	    (code-char 9)
	    (code-char 32)
	    (code-char 13)
	    (code-char 10)))

(defun ntc (lname source-or-attrs)
  ;; used for (n)ame, (t)ype, and (c)ombine, this also strips whitespace
  (let* ((attrs
	  (if (listp source-or-attrs)
	      source-or-attrs
	      (klacks:list-attributes source-or-attrs)))
	 (a (sax:find-attribute-ns "" lname attrs)))
    (if a
	(string-trim *whitespace* (sax:attribute-value a))
	nil)))

(defmacro with-library-and-ns (attrs &body body)
  `(invoke-with-library-and-ns (lambda () ,@body) ,attrs))

(defun invoke-with-library-and-ns (fn attrs)
  (let* ((dl (attribute "datatypeLibrary" attrs))
	 (ns (attribute "ns" attrs))
	 (*datatype-library* (if dl (escape-uri dl) *datatype-library*))
	 (*namespace-uri* (or ns *namespace-uri*))
	 (*ns* ns))
    ;; FIXME: Ganz boese gehackt -- gerade so, dass wir die Relax NG
    ;; Test-Suite bestehen.
    (when (and dl
	       (not (zerop (length *datatype-library*)))
	       ;; scheme pruefen, und es muss was folgen
	       (or (not (cl-ppcre:all-matches
			 "^[a-zA-Z][a-zA-Z0-9+.-]*:.+"
			 *datatype-library*))
		   ;; keine kaputten %te, keine #
		   (cl-ppcre:all-matches
		    "(%$|%.$|%[^0-9A-Fa-f][^0-9A-Fa-f]|#)"
		    *datatype-library*)))
      (rng-error nil "malformed datatypeLibrary: ~A" *datatype-library*))
    (funcall fn)))

(defun p/pattern (source)
  (let* ((lname (klacks:current-lname source))
	 (attrs (klacks:list-attributes source)))
    (with-library-and-ns attrs
      (case (find-symbol lname :keyword)
	(:|element|     (p/element source (ntc "name" attrs)))
	(:|attribute|   (p/attribute source (ntc "name" attrs)))
	(:|group|       (p/combination #'groupify source))
	(:|interleave|  (p/combination #'interleave-ify source))
	(:|choice|      (p/combination #'choice-ify source))
	(:|optional|    (p/optional source))
	(:|zeroOrMore|  (p/zero-or-more source))
	(:|oneOrMore|   (p/one-or-more source))
	(:|list|        (p/list source))
	(:|mixed|       (p/mixed source))
	(:|ref|         (p/ref source))
	(:|parentRef|   (p/parent-ref source))
	(:|empty|       (p/empty source))
	(:|text|        (p/text source))
	(:|value|       (p/value source))
	(:|data|        (p/data source))
	(:|notAllowed|  (p/not-allowed source))
	(:|externalRef| (p/external-ref source))
	(:|grammar|     (p/grammar source))
	(t (skip-foreign source))))))

(defun p/pattern+ (source)
  (let ((children nil))
    (loop
      (case (klacks:peek source)
	(:start-element
	  (let ((p (p/pattern source))) (when p (push p children))))
	(:end-element
	  (return))
	(t
	  (klacks:consume source))))
    (unless children
      (rng-error source "empty element"))
    (nreverse children)))

(defun p/pattern? (source)
  (let ((result nil))
    (loop
      (skip-to-native source)
      (case (klacks:peek source)
	(:start-element
	  (when result
	    (rng-error source "at most one pattern expected here"))
	  (setf result (p/pattern source)))
	(:end-element
	  (return))
	(t
	  (klacks:consume source))))
    result))

(defun p/element (source name)
  (klacks:expecting-element (source "element")
    (let ((elt (make-element)))
      (consume-and-skip-to-native source)
      (if name
	  (setf (pattern-name elt) (destructure-name source name))
	  (setf (pattern-name elt) (p/name-class source)))
      (skip-to-native source)
      (setf (pattern-child elt) (groupify (p/pattern+ source)))
      (make-ref (make-definition :name (gensym "ANONYMOUS") :child elt)))))

(defvar *attribute-namespace-p* nil)

(defun p/attribute (source name)
  (klacks:expecting-element (source "attribute")
    (let* ((dv
	    (when *process-dtd-compatibility*
	      (sax:find-attribute-ns
	       "http://relaxng.org/ns/compatibility/annotations/1.0"
	       "defaultValue"
	       (klacks:list-attributes source))))
	   (result (make-attribute (when dv (sax:attribute-value dv)))))
      (consume-and-skip-to-native source)
      (if name
	  (setf (pattern-name result)
		(let ((*namespace-uri* (or *ns* ""))
		      (*attribute-namespace-p* t))
		  (destructure-name source name)))
	  (setf (pattern-name result)
		(let ((*attribute-namespace-p* t))
		  (p/name-class source))))
      (skip-to-native source)
      (setf (pattern-child result)
	    (or (p/pattern? source) (make-text)))
      result)))

(defun p/combination (zipper source)
  (klacks:expecting-element (source)
    (consume-and-skip-to-native source)
    (funcall zipper (p/pattern+ source))))

(defun p/one-or-more (source)
  (klacks:expecting-element (source "oneOrMore")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-one-or-more (groupify children)))))

(defun p/zero-or-more (source)
  (klacks:expecting-element (source "zeroOrMore")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-choice (make-one-or-more (groupify children))
		   (make-empty)))))

(defun p/optional (source)
  (klacks:expecting-element (source "optional")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-choice (groupify children) (make-empty)))))

(defun p/list (source)
  (klacks:expecting-element (source "list")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-list-pattern (groupify children)))))

(defun p/mixed (source)
  (klacks:expecting-element (source "mixed")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-interleave (groupify children) (make-text)))))

(defun p/ref (source)
  (klacks:expecting-element (source "ref")
    (prog1
	(let* ((name (ntc "name" source))
	       (pdefinition
		(or (find-definition name)
		    (setf (find-definition name)
			  (make-definition :name name :child nil)))))
	  (make-ref pdefinition))
      (skip-foreign* source))))

(defun p/parent-ref (source)
  (klacks:expecting-element (source "parentRef")
    (prog1
	(let* ((name (ntc "name" source))
	       (grammar (grammar-parent *grammar*))
	       (pdefinition
		(or (find-definition name grammar)
		    (setf (find-definition name grammar)
			  (make-definition :name name :child nil)))))
	  (make-ref pdefinition))
      (skip-foreign* source))))

(defun p/empty (source)
  (klacks:expecting-element (source "empty")
    (skip-foreign* source)
    (make-empty)))

(defun p/text (source)
  (klacks:expecting-element (source "text")
    (skip-foreign* source)
    (make-text)))

(defun consume-and-parse-characters (source)
  ;; fixme
  (let ((tmp ""))
    (loop
      (multiple-value-bind (key data) (klacks:peek-next source)
	(case key
	  (:characters
	    (setf tmp (concatenate 'string tmp data)))
	  (:end-element (return)))))
    tmp))

(defun p/value (source)
  (klacks:expecting-element (source "value")
    (let* ((type (ntc "type" source))
	   (string (consume-and-parse-characters source))
	   (ns *namespace-uri*)
	   (dl *datatype-library*))
      (unless type
	(setf type "token")
	(setf dl ""))
      (let ((data-type
	     (cxml-types:find-type (and dl (find-symbol dl :keyword))
				   type
				   nil))
	    (vc (cxml-types:make-klacks-validation-context source)))
	(unless data-type
	  (rng-error source "type not found: ~A/~A" type dl))
	(make-value :string string
		    :value (cxml-types:parse data-type string vc)
		    :type data-type
		    :ns ns)))))

(defun p/data (source)
  (klacks:expecting-element (source "data")
    (let* ((type (ntc "type" source))
	   (params '())
	   (except nil))
      (loop
	 (multiple-value-bind (key uri lname)
	     (klacks:peek-next source)
	   uri
	   (case key
	     (:start-element
	      (case (find-symbol lname :keyword)
		(:|param| (push (p/param source) params))
		(:|except|
		  (setf except (p/except-pattern source))
		  (skip-to-native source)
		  (return))
		(t (skip-foreign source))))
	     (:end-element
	      (return)))))
      (setf params (nreverse params))
      (let* ((dl *datatype-library*)
	     (data-type (cxml-types:find-type
			 (and dl (find-symbol dl :keyword))
			 type
			 params)))
	(unless data-type
	  (rng-error source "type not found: ~A/~A" type dl))
	(when (eq data-type :error)
	  (rng-error source "params not valid for type: ~A/~A/~A"
		     type dl params))
	(make-data
	 :type data-type
	 :params params
	 :except except)))))

(defun p/param (source)
  (klacks:expecting-element (source "param")
    (let ((name (ntc "name" source))
	  (string (consume-and-parse-characters source)))
      (cxml-types:make-param name string))))

(defun p/except-pattern (source)
  (klacks:expecting-element (source "except")
    (with-library-and-ns (klacks:list-attributes source)
      (klacks:consume source)
      (choice-ify (p/pattern+ source)))))

(defun p/not-allowed (source)
  (klacks:expecting-element (source "notAllowed")
    (consume-and-skip-to-native source)
    (make-not-allowed)))

(defun safe-parse-uri (source str &optional base)
  (when (zerop (length str))
    (rng-error source "missing URI"))
  (let* ((compactp (rnc-uri-p str))
	 (str (if compactp (follow-rnc-uri str) str))
	 (uri
	  (handler-case
	      (if base
		  (puri:merge-uris str base)
		  (puri:parse-uri str))
	    (puri:uri-parse-error ()
	      (rng-error source "invalid URI: ~A" str)))))
    (when (and (eq (puri:uri-scheme uri) :file)
	       (puri:uri-fragment uri))
      (rng-error source "Forbidden fragment in URI: ~A" str))
    (values uri compactp)))

(defun named-string-xstream (str uri)
  (let ((xstream (cxml::string->xstream str)))
    (setf (cxml::xstream-name xstream)
	  (cxml::make-stream-name
	   :entity-name "main document"
	   :entity-kind :main
	   :uri uri))
    xstream))

(defun xstream-open-schema (uri compactp)
  (if compactp
      (named-string-xstream
       (uncompact-file
	;; fixme: Hier waere es schon, mit *entity-resolver* arbeiten
	;; zu koennen, aber der liefert binaere Streams.
	(open (cxml::uri-to-pathname uri)
	      :element-type 'character
	      :direction :input))
       uri)
      (cxml::xstream-open-extid* *entity-resolver* nil uri)))

(defun p/external-ref (source)
  (klacks:expecting-element (source "externalRef")
    (let* ((href
	    (escape-uri (attribute "href" (klacks:list-attributes source))))
	   (base (klacks:current-xml-base source)))
      (multiple-value-bind (uri compactp)
	  (safe-parse-uri source href base)
	(when (find uri *include-uri-stack* :test #'puri:uri=)
	  (rng-error source "looping include"))
	(prog1
	    (let* ((*include-uri-stack* (cons uri *include-uri-stack*))
		   (xstream (xstream-open-schema uri compactp)))
	      (klacks:with-open-source
		  (source (make-schema-source xstream))
		(invoke-with-klacks-handler
		 (lambda ()
		   (klacks:find-event source :start-element)
		   (let ((*datatype-library* ""))
		     (p/pattern source)))
		 source)))
	  (skip-foreign* source))))))

(defun p/grammar (source &optional grammar)
  (klacks:expecting-element (source "grammar")
    (consume-and-skip-to-native source)
    (let ((*grammar* (or grammar (make-grammar *grammar*)))
	  (includep grammar))
      (process-grammar-content* source)
      (unless (or includep (grammar-start *grammar*))
	(rng-error source "no <start> in grammar"))
      (unless includep
	(check-pattern-definitions source *grammar*)
	(defn-child (grammar-start *grammar*))))))

(defvar *include-start*)
(defvar *include-definitions*)

(defun process-grammar-content* (source &key disallow-include)
  (loop
    (multiple-value-bind (key uri lname) (klacks:peek source)
      uri
      (ecase key
	((:characters :comment)
	 (klacks:consume source))
	(:start-element
	 (with-library-and-ns (klacks:list-attributes source)
	   (case (find-symbol lname :keyword)
	     (:|start|
	       (process-start source))
	     (:|define| (process-define source))
	     (:|div| (process-div source))
	     (:|include|
	       (when disallow-include
		 (rng-error source "nested include not permitted"))
	       (process-include source))
	     (t
	      (skip-foreign source)))))
	(:end-element
	 (return))))))

(defun process-start (source)
  (klacks:expecting-element (source "start")
    (let* ((combine0 (ntc "combine" source))
	   (combine
	    (when combine0
	      (find-symbol (string-upcase combine0) :keyword)))
	   (child
	    (progn
	      (consume-and-skip-to-native source)
	      (p/pattern source)))
	   (pdefinition (grammar-start *grammar*)))
      (skip-foreign* source)
      ;; fixme: shared code with process-define
      (unless pdefinition
	(setf pdefinition (make-definition :name :start :child nil))
	(setf (grammar-start *grammar*) pdefinition))
      (when *include-body-p*
	(setf *include-start* pdefinition))
      (cond
	((defn-child pdefinition)
	 (ecase (defn-redefinition pdefinition)
	   (:not-being-redefined
	     (when (and combine
			(defn-combine-method pdefinition)
			(not (eq combine
				 (defn-combine-method pdefinition))))
	       (rng-error source "conflicting combine values for <start>"))
	     (unless combine
	       (when (defn-head-p pdefinition)
		 (rng-error source "multiple definitions for <start>"))
	       (setf (defn-head-p pdefinition) t))
	     (unless (defn-combine-method pdefinition)
	       (setf (defn-combine-method pdefinition) combine))
	     (setf (defn-child pdefinition)
		   (case (defn-combine-method pdefinition)
		     (:choice
		       (make-choice (defn-child pdefinition) child))
		     (:interleave
		       (make-interleave (defn-child pdefinition) child)))))
	   (:being-redefined-and-no-original
	     (setf (defn-redefinition pdefinition)
		   :being-redefined-and-original))
	   (:being-redefined-and-original)))
	(t
	  (setf (defn-child pdefinition) child)
	  (setf (defn-combine-method pdefinition) combine)
	  (setf (defn-head-p pdefinition) (null combine))
	  (setf (defn-redefinition pdefinition) :not-being-redefined))))))

(defun zip (constructor children)
  (cond
    ((null children)
      (rng-error nil "empty choice?"))
    ((null (cdr children))
      (car children))
    (t
      (destructuring-bind (a b &rest rest)
	  children
	(zip constructor (cons (funcall constructor a b) rest))))))

(defun choice-ify (children) (zip #'make-choice children))
(defun groupify (children) (zip #'make-group children))
(defun interleave-ify (children) (zip #'make-interleave children))

(defun find-definition (name &optional (grammar *grammar*))
  (gethash name (grammar-definitions grammar)))

(defun (setf find-definition) (newval name &optional (grammar *grammar*))
  (setf (gethash name (grammar-definitions grammar)) newval))

(defun process-define (source)
  (klacks:expecting-element (source "define")
    (let* ((name (ntc "name" source))
	   (combine0 (ntc "combine" source))
	   (combine (when combine0
		      (find-symbol (string-upcase combine0) :keyword)))
	   (child (groupify
		   (progn
		     (consume-and-skip-to-native source)
		     (p/pattern+ source))))
	   (pdefinition (find-definition name)))
      (unless pdefinition
	(setf pdefinition (make-definition :name name :child nil))
	(setf (find-definition name) pdefinition))
      (when *include-body-p*
	(push pdefinition *include-definitions*))
      (cond
	((defn-child pdefinition)
	  (case (defn-redefinition pdefinition)
	    (:not-being-redefined
	      (when (and combine
			 (defn-combine-method pdefinition)
			 (not (eq combine
				  (defn-combine-method pdefinition))))
		(rng-error source "conflicting combine values for ~A" name))
	      (unless combine
		(when (defn-head-p pdefinition)
		  (rng-error source "multiple definitions for ~A" name))
		(setf (defn-head-p pdefinition) t))
	      (unless (defn-combine-method pdefinition)
		(setf (defn-combine-method pdefinition) combine))
	      (setf (defn-child pdefinition)
		    (case (defn-combine-method pdefinition)
		      (:choice
			(make-choice (defn-child pdefinition) child))
		      (:interleave
			(make-interleave (defn-child pdefinition) child)))))
	    (:being-redefined-and-no-original
	      (setf (defn-redefinition pdefinition)
		    :being-redefined-and-original))
	    (:being-redefined-and-original)))
	(t
	  (setf (defn-child pdefinition) child)
	  (setf (defn-combine-method pdefinition) combine)
	  (setf (defn-head-p pdefinition) (null combine))
	  (setf (defn-redefinition pdefinition) :not-being-redefined))))))

(defun process-div (source)
  (klacks:expecting-element (source "div")
    (consume-and-skip-to-native source)
    (process-grammar-content* source)))

(defun reset-definition-for-include (defn)
  (setf (defn-combine-method defn) nil)
  (setf (defn-redefinition defn) :being-redefined-and-no-original)
  (setf (defn-head-p defn) nil))

(defun restore-definition (defn original)
  (setf (defn-combine-method defn) (defn-combine-method original))
  (setf (defn-redefinition defn) (defn-redefinition original))
  (setf (defn-head-p defn) (defn-head-p original)))

(defun process-include (source)
  (klacks:expecting-element (source "include")
    (let* ((href
	    (escape-uri (attribute "href" (klacks:list-attributes source))))
	   (base (klacks:current-xml-base source))
	   (*include-start* nil)
	   (*include-definitions* '()))
      (multiple-value-bind (uri compactp)
	  (safe-parse-uri source href base)
	(consume-and-skip-to-native source)
	(let ((*include-body-p* t))
	  (process-grammar-content* source :disallow-include t))
	(let ((tmp-start
	       (when *include-start*
		 (prog1
		     (copy-structure *include-start*)
		   (reset-definition-for-include *include-start*))))
	      (tmp-defns
	       (loop
		  for defn in *include-definitions*
		  collect
		  (prog1
		      (copy-structure defn)
		    (reset-definition-for-include defn)))))
	  (when (find uri *include-uri-stack* :test #'puri:uri=)
	    (rng-error source "looping include"))
	  (let* ((*include-uri-stack* (cons uri *include-uri-stack*))
		 (xstream (xstream-open-schema uri compactp)))
	    (klacks:with-open-source (source (make-schema-source xstream))
	      (invoke-with-klacks-handler
	       (lambda ()
		 (klacks:find-event source :start-element)
		 (let ((*datatype-library* ""))
		   (p/grammar source *grammar*)))
	       source))
	    (when tmp-start
	      (when (eq (defn-redefinition *include-start*)
			:being-redefined-and-no-original)
		(rng-error source "start not found in redefinition of grammar"))
	      (restore-definition *include-start* tmp-start))
	    (dolist (copy tmp-defns)
	      (let ((defn (gethash (defn-name copy)
				   (grammar-definitions *grammar*))))
		(when (eq (defn-redefinition defn)
			  :being-redefined-and-no-original)
		  (rng-error source "redefinition not found in grammar"))
		(restore-definition defn copy)))
	    nil))))))

(defun check-pattern-definitions (source grammar)
  (when (and (grammar-start grammar)
	     (eq (defn-redefinition (grammar-start grammar))
		 :being-redefined-and-no-original))
    (rng-error source "start not found in redefinition of grammar"))
  (loop for defn being each hash-value in (grammar-definitions grammar) do
	(when (eq (defn-redefinition defn) :being-redefined-and-no-original)
	  (rng-error source "redefinition not found in grammar"))
	(unless (defn-child defn)
	  (rng-error source "unresolved reference to ~A" (defn-name defn)))))

(defvar *any-name-allowed-p* t)
(defvar *ns-name-allowed-p* t)

(defun destructure-name (source qname)
  (multiple-value-bind (uri lname)
      (klacks:decode-qname qname source)
    (setf uri (or uri *namespace-uri*))
    (when (and *attribute-namespace-p*
	       (or (and (equal lname "xmlns") (equal uri ""))
		   (equal uri "http://www.w3.org/2000/xmlns")))
      (rng-error source "namespace attribute not permitted"))
    (make-name uri lname)))

(defun p/name-class (source)
  (klacks:expecting-element (source)
    (with-library-and-ns (klacks:list-attributes source)
      (case (find-symbol (klacks:current-lname source) :keyword)
	(:|name|
	  (let ((qname (string-trim *whitespace*
				    (consume-and-parse-characters source))))
	    (destructure-name source qname)))
	(:|anyName|
	  (unless *any-name-allowed-p*
	    (rng-error source "anyname not permitted in except"))
	  (klacks:consume source)
	  (prog1
	      (let ((*any-name-allowed-p* nil))
		(make-any-name (p/except-name-class? source)))
	    (skip-to-native source)))
	(:|nsName|
	  (unless *ns-name-allowed-p*
	    (rng-error source "nsname not permitted in except"))
	  (let ((uri *namespace-uri*)
		(*any-name-allowed-p* nil)
		(*ns-name-allowed-p* nil))
	    (when (and *attribute-namespace-p*
		       (equal uri "http://www.w3.org/2000/xmlns"))
	      (rng-error source "namespace attribute not permitted"))
	    (klacks:consume source)
	    (prog1
		(make-ns-name uri (p/except-name-class? source))
	      (skip-to-native source))))
	(:|choice|
	  (klacks:consume source)
	  (simplify-nc-choice (p/name-class* source)))
	(t
	  (rng-error source "invalid child in except"))))))

(defun p/name-class* (source)
  (let ((results nil))
    (loop
      (skip-to-native source)
      (case (klacks:peek source)
	(:characters
	 (klacks:consume source))
	(:start-element
	 (push (p/name-class source) results))
	(:end-element
	 (return))))
    (nreverse results)))

(defun p/except-name-class? (source)
  (skip-to-native source)
  (multiple-value-bind (key uri lname)
      (klacks:peek source)
    uri
    (if (and (eq key :start-element)
	     (string= (find-symbol lname :keyword) "except"))
	(p/except-name-class source)
	nil)))

(defun p/except-name-class (source)
  (klacks:expecting-element (source "except")
    (with-library-and-ns (klacks:list-attributes source)
      (klacks:consume source)
      (let ((x (p/name-class* source)))
	(if (cdr x)
	    (simplify-nc-choice x)
	    (car x))))))

(defun escape-uri (string)
  (with-output-to-string (out)
    (loop for c across (cxml::rod-to-utf8-string string) do
	  (let ((code (char-code c)))
	    ;; http://www.w3.org/TR/xlink/#link-locators
	    (if (or (>= code 127) (<= code 32) (find c "<>\"{}|\\^`"))
		(format out "%~2,'0X" code)
		(write-char c out))))))


;;;; unparsing

(defvar *definitions-to-names*)
(defvar *seen-names*)

(defun serialization-name (defn)
  (or (gethash defn *definitions-to-names*)
      (setf (gethash defn *definitions-to-names*)
	    (let ((name (if (gethash (defn-name defn) *seen-names*)
			    (format nil "~A-~D"
				    (defn-name defn)
				    (hash-table-count *seen-names*))
			    (defn-name defn))))
	      (setf (gethash name *seen-names*) defn)
	      name))))

(defun serialize-schema (schema sink)
  "@arg[schema]{a Relax NG @class{schema}}
   @arg[sink]{a SAX handler}
   @return{the result of @code{sax:end-document}}
   @short{This function serializes a parsed Relax NG back into XML syntax.}

   Note that the schema represented in memory has gone through simplification
   as is textually different from the original XML document.

   @see{parse-schema}"
  (cxml:with-xml-output sink
    (let ((*definitions-to-names* (make-hash-table))
	  (*seen-names* (make-hash-table :test 'equal)))
      (cxml:with-element "grammar"
	(cxml:with-element "start"
	  (serialize-pattern (schema-start schema)))
	(loop for defn being each hash-key in *definitions-to-names* do
	      (serialize-definition defn))))))

(defun serialize-pattern (pattern)
  (etypecase pattern
    (element
      (cxml:with-element "element"
	(serialize-name (pattern-name pattern))
	(serialize-pattern (pattern-child pattern))))
    (attribute
      (cxml:with-element "attribute"
	(serialize-name (pattern-name pattern))
	(serialize-pattern (pattern-child pattern))))
    (%combination
      (cxml:with-element
	  (etypecase pattern
	    (group "group")
	    (interleave "interleave")
	    (choice "choice"))
	(serialize-pattern (pattern-a pattern))
	(serialize-pattern (pattern-b pattern))))
    (one-or-more
      (cxml:with-element "oneOrMore"
	(serialize-pattern (pattern-child pattern))))
    (list-pattern
      (cxml:with-element "list"
	(serialize-pattern (pattern-child pattern))))
    (ref
      (cxml:with-element "ref"
	(cxml:attribute "name" (serialization-name (pattern-target pattern)))))
    (empty
      (cxml:with-element "empty"))
    (not-allowed
      (cxml:with-element "notAllowed"))
    (text
      (cxml:with-element "text"))
    (value
      (cxml:with-element "value"
	(let ((type (pattern-type pattern)))
	  (cxml:attribute "datatype-library"
			  (symbol-name (cxml-types:type-library type)))
	  (cxml:attribute "type" (cxml-types:type-name type)))
	(cxml:attribute "ns" (pattern-ns pattern))
	(cxml:text (pattern-string pattern))))
    (data
      (cxml:with-element "value"
	(let ((type (pattern-type pattern)))
	  (cxml:attribute "datatype-library"
			  (symbol-name (cxml-types:type-library type)))
	  (cxml:attribute "type" (cxml-types:type-name type)))
	(dolist (param (pattern-params pattern))
	  (cxml:with-element "param"
	    (cxml:attribute "name" (cxml-types:param-name param))
	    (cxml:text (cxml-types:param-value param))))
	(when (pattern-except pattern)
	  (cxml:with-element "except"
	    (serialize-pattern (pattern-except pattern))))))))

(defun serialize-definition (defn)
  (cxml:with-element "define"
    (cxml:attribute "name" (serialization-name defn))
    (serialize-pattern (defn-child defn))))

(defun serialize-name (name)
  (etypecase name
    (name
     (cxml:with-element "name"
       (cxml:attribute "ns" (name-uri name))
       (cxml:text (name-lname name))))
    (any-name
     (cxml:with-element "anyName"
       (when (any-name-except name)
	 (serialize-except-name (any-name-except name)))))
    (ns-name
     (cxml:with-element "anyName"
       (cxml:attribute "ns" (ns-name-uri name))
       (when (ns-name-except name)
	 (serialize-except-name (ns-name-except name)))))
    (name-class-choice
     (cxml:with-element "choice"
       (serialize-name (name-class-choice-a name))
       (serialize-name (name-class-choice-b name))))))

(defun serialize-except-name (spec)
  (cxml:with-element "except"
    (serialize-name spec)))


;;;; simplification

;;; 4.1 Annotations
;;;   Foreign attributes and elements are removed implicitly while parsing.

;;; 4.2 Whitespace
;;;   All character data is discarded while parsing (which can only be
;;;   whitespace after validation).
;;;
;;;   Whitespace in name, type, and combine attributes is stripped while
;;;   parsing.  Ditto for <name/>.

;;; 4.3. datatypeLibrary attribute
;;;   Escaping is done by p/pattern.
;;;   Attribute value defaulting is done using *datatype-library*; only
;;;   p/data and p/value record the computed value.

;;; 4.4. type attribute of value element
;;;   Done by p/value.

;;; 4.5. href attribute
;;;   Escaping is done by process-include and p/external-ref.
;;;
;;;   FIXME: Mime-type handling should be the job of the entity resolver,
;;;   but that requires xstream hacking.

;;; 4.6. externalRef element
;;;   Done by p/external-ref.

;;; 4.7. include element
;;;   Done by process-include.

;;; 4.8. name attribute of element and attribute elements
;;;   `name' is stored as a slot, not a child.  Done by p/element and
;;;    p/attribute.  

;;; 4.9. ns attribute
;;;    done by p/name-class, p/value, p/element, p/attribute

;;; 4.10. QNames
;;;    done by p/name-class

;;; 4.11. div element
;;;    Legen wir gar nicht erst an.

;;; 4.12. 4.13 4.14 4.15
;;;    beim anlegen

;;; 4.16
;;;    p/name-class
;;;    -- ausser der sache mit den datentypen

;;; 4.17, 4.18, 4.19
;;;    Ueber die Grammar-und Definition Objekte, wie von James Clark
;;;    beschrieben.
;;;
;;;    Dabei werden keine Umbenennungen vorgenommen, weil Referenzierung
;;;    durch Aufbei der Graphenstruktur zwischen ref und Definition
;;;    erfolgt und Namen dann bereits aufgeloest sind.  Wir benennen
;;;    dafuer beim Serialisieren um.

(defmethod check-recursion ((pattern element) depth)
  (check-recursion (pattern-child pattern) (1+ depth)))

(defmethod check-recursion ((pattern ref) depth)
  (when (eql (pattern-crdepth pattern) depth)
    (rng-error nil "infinite recursion in ~A"
	       (defn-name (pattern-target pattern))))
  (when (null (pattern-crdepth pattern))
    (setf (pattern-crdepth pattern) depth)
    (check-recursion (defn-child (pattern-target pattern)) depth)
    (setf (pattern-crdepth pattern) t)))

(defmethod check-recursion ((pattern %parent) depth)
  (check-recursion (pattern-child pattern) depth))

(defmethod check-recursion ((pattern %combination) depth)
  (check-recursion (pattern-a pattern) depth)
  (check-recursion (pattern-b pattern) depth))

(defmethod check-recursion ((pattern %leaf) depth)
  (declare (ignore depth)))

(defmethod check-recursion ((pattern data) depth)
  (when (pattern-except pattern)
    (check-recursion (pattern-except pattern) depth)))


;;;; 4.20

;;; %PARENT

(defmethod fold-not-allowed ((pattern element))
  (setf (pattern-child pattern) (fold-not-allowed (pattern-child pattern)))
  pattern)

(defmethod fold-not-allowed ((pattern %parent))
  (setf (pattern-child pattern) (fold-not-allowed (pattern-child pattern)))
  (if (typep (pattern-child pattern) 'not-allowed)
      (pattern-child pattern)
      pattern))

;;; %COMBINATION

(defmethod fold-not-allowed ((pattern %combination))
  (setf (pattern-a pattern) (fold-not-allowed (pattern-a pattern)))
  (setf (pattern-b pattern) (fold-not-allowed (pattern-b pattern)))
  pattern)

(defmethod fold-not-allowed ((pattern group))
  (call-next-method)
  (cond
    ;; remove if any child is not allowed
    ((typep (pattern-a pattern) 'not-allowed) (pattern-a pattern))
    ((typep (pattern-b pattern) 'not-allowed) (pattern-b pattern))
    (t pattern)))

(defmethod fold-not-allowed ((pattern interleave))
  (call-next-method)
  (cond
    ;; remove if any child is not allowed
    ((typep (pattern-a pattern) 'not-allowed) (pattern-a pattern))
    ((typep (pattern-b pattern) 'not-allowed) (pattern-b pattern))
    (t pattern)))

(defmethod fold-not-allowed ((pattern choice))
  (call-next-method)
  (cond
    ;; if any child is not allowed, choose the other
    ((typep (pattern-a pattern) 'not-allowed) (pattern-b pattern))
    ((typep (pattern-b pattern) 'not-allowed) (pattern-a pattern))
    (t pattern)))

;;; LEAF

(defmethod fold-not-allowed ((pattern %leaf))
  pattern)

(defmethod fold-not-allowed ((pattern data))
  (when (pattern-except pattern)
    (setf (pattern-except pattern) (fold-not-allowed (pattern-except pattern)))
    (when (typep (pattern-except pattern) 'not-allowed)
      (setf (pattern-except pattern) nil)))
  pattern)

;;; REF

(defmethod fold-not-allowed ((pattern ref))
  pattern)


;;;; 4.21

;;; %PARENT

(defmethod fold-empty ((pattern one-or-more))
  (call-next-method)
  (if (typep (pattern-child pattern) 'empty)
      (pattern-child pattern)
      pattern))

(defmethod fold-empty ((pattern %parent))
  (setf (pattern-child pattern) (fold-empty (pattern-child pattern)))
  pattern)

;;; %COMBINATION

(defmethod fold-empty ((pattern %combination))
  (setf (pattern-a pattern) (fold-empty (pattern-a pattern)))
  (setf (pattern-b pattern) (fold-empty (pattern-b pattern)))
  pattern)

(defmethod fold-empty ((pattern group))
  (call-next-method)
  (cond
    ;; if any child is empty, choose the other
    ((typep (pattern-a pattern) 'empty) (pattern-b pattern))
    ((typep (pattern-b pattern) 'empty) (pattern-a pattern))
    (t pattern)))

(defmethod fold-empty ((pattern interleave))
  (call-next-method)
  (cond
    ;; if any child is empty, choose the other
    ((typep (pattern-a pattern) 'empty) (pattern-b pattern))
    ((typep (pattern-b pattern) 'empty) (pattern-a pattern))
    (t pattern)))

(defmethod fold-empty ((pattern choice))
  (call-next-method)
  (if (typep (pattern-b pattern) 'empty)
      (cond
	((typep (pattern-a pattern) 'empty)
	  (pattern-a pattern))
	(t
	  (rotatef (pattern-a pattern) (pattern-b pattern))
	  pattern))
      pattern))

;;; LEAF

(defmethod fold-empty ((pattern %leaf))
  pattern)

(defmethod fold-empty ((pattern data))
  (when (pattern-except pattern)
    (setf (pattern-except pattern) (fold-empty (pattern-except pattern))))
  pattern)

;;; REF

(defmethod fold-empty ((pattern ref))
  pattern)


;;;; name class overlap

;;; fixme: memorize this stuff?

(defparameter !uri (string (code-char 1)))
(defparameter !lname "")

(defun classes-overlap-p (nc1 nc2)
  (flet ((both-contain (x)
	   (and (contains nc1 (car x) (cdr x))
		(contains nc2 (car x) (cdr x)))))
    (or (some #'both-contain (representatives nc1))
	(some #'both-contain (representatives nc2)))))

(defmethod representatives ((nc any-name))
  (cons (cons !uri !lname)
	(if (any-name-except nc)
	    (representatives (any-name-except nc))
	    nil)))

(defmethod representatives ((nc ns-name))
  (cons (cons (ns-name-uri nc) !lname)
	(if (ns-name-except nc)
	    (representatives (ns-name-except nc))
	    nil)))

(defmethod representatives ((nc name))
  (list (cons (name-uri nc) (name-lname nc))))

(defmethod representatives ((nc name-class-choice))
  (nconc (representatives (name-class-choice-a nc))
	 (representatives (name-class-choice-b nc))))


;;;; 7.1

(defun finalize-definitions (pattern)
  (let ((defns (make-hash-table)))
    (labels ((recurse (p)
	       (cond
		 ((typep p 'ref)
		  (let ((target (pattern-target p)))
		    (unless (gethash target defns)
		      (setf (gethash target defns) t)
		      (setf (defn-child target) (recurse (defn-child target))))
		    (if (typep (defn-child target) 'element)
			p
			(copy-pattern-tree (defn-child target)))))
		 (t
		  (etypecase p
		    (data
		     (when (pattern-except p)
		       (setf (pattern-except p) (recurse (pattern-except p)))))
		    (%parent
		     (setf (pattern-child p) (recurse (pattern-child p))))
		    (%combination
		     (setf (pattern-a p) (recurse (pattern-a p)))
		     (setf (pattern-b p) (recurse (pattern-b p))))
		    (%leaf))
		  p))))
      (values
       (recurse pattern)
       (loop
	  for defn being each hash-key in defns
	  collect defn)))))

(defun copy-pattern-tree (pattern)
  (labels ((recurse (p)
	     (let ((q (copy-structure p)))
	       (etypecase p
		 (data
		  (when (pattern-except p)
		    (setf (pattern-except q) (recurse (pattern-except p)))))
		 (%parent
		  (setf (pattern-child q) (recurse (pattern-child p))))
		 (%combination
		  (setf (pattern-a q) (recurse (pattern-a p)))
		  (setf (pattern-b q) (recurse (pattern-b p))))
		 ((or %leaf ref)))
	       q)))
    (recurse pattern)))

(defparameter *in-attribute-p* nil)
(defparameter *in-one-or-more-p* nil)
(defparameter *in-one-or-more//group-or-interleave-p* nil)
(defparameter *in-list-p* nil)
(defparameter *in-data-except-p* nil)
(defparameter *in-start-p* nil)

(defun check-start-restrictions (pattern)
  (let ((*in-start-p* t))
    (check-restrictions pattern)))

(defun content-type-max (a b)
  (if (and a b)
      (cond
	((eq a :empty) b)
	((eq b :empty) a)
	((eq a :complex) b)
	(:simple))
      nil))

(defun groupable-max (a b)
  (if (or (eq a :empty)
	  (eq b :empty)
	  (and (eq a :complex)
	       (eq b :complex)))
      (content-type-max a b)
      nil))

(defun assert-name-class-finite (nc)
  (etypecase nc
    ((or any-name ns-name)
     (rng-error nil "infinite attribute name class outside of one-or-more"))
    (name)
    (name-class-choice
     (assert-name-class-finite (name-class-choice-a nc))
     (assert-name-class-finite (name-class-choice-b nc)))))

(defmethod check-restrictions ((pattern attribute))
  (when *in-attribute-p*
    (rng-error nil "nested attribute not allowed"))
  (when *in-one-or-more//group-or-interleave-p*
    (rng-error nil "attribute not allowed in oneOrMore//group, oneOrMore//interleave"))
  (when *in-list-p*
    (rng-error nil "attribute in list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "attribute in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "attribute in start not allowed"))
  (let ((*in-attribute-p* t))
    (unless *in-one-or-more-p*
      (assert-name-class-finite (pattern-name pattern)))
    (values (if (check-restrictions (pattern-child pattern))
		:empty
		nil)
	    (list (pattern-name pattern))
	    nil)))

(defmethod check-restrictions ((pattern ref))
  (when *in-attribute-p*
    (rng-error nil "ref in attribute not allowed"))
  (when *in-list-p*
    (rng-error nil "ref in list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "ref in data/except not allowed"))
  (values :complex
	  nil
	  (list (pattern-name (defn-child (pattern-target pattern))))
	  nil))

(defmethod check-restrictions ((pattern one-or-more))
  (when *in-data-except-p*
    (rng-error nil "oneOrMore in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "one-or-more in start not allowed"))
  (let* ((*in-one-or-more-p* t))
    (multiple-value-bind (x a e textp)
	(check-restrictions (pattern-child pattern))
      (values (groupable-max x x) a e textp))))

(defmethod check-restrictions ((pattern group))
  (when *in-data-except-p*
    (rng-error nil "group in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "group in start not allowed"))
  (let ((*in-one-or-more//group-or-interleave-p*
	 *in-one-or-more-p*))
    (multiple-value-bind (x a e tp) (check-restrictions (pattern-a pattern))
      (multiple-value-bind (y b f tq) (check-restrictions (pattern-b pattern))
	(dolist (nc1 a)
	  (dolist (nc2 b)
	    (when (classes-overlap-p nc1 nc2)
	      (rng-error nil "attribute name overlap in group: ~A ~A"
			 nc1 nc2))))
	(values (groupable-max x y)
		(append a b)
		(append e f)
		(or tp tq))))))

(defmethod check-restrictions ((pattern interleave))
  (when *in-list-p*
    (rng-error nil "interleave in list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "interleave in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "interleave in start not allowed"))
  (let ((*in-one-or-more//group-or-interleave-p*
	 *in-one-or-more-p*))
    (multiple-value-bind (x a e tp) (check-restrictions (pattern-a pattern))
      (multiple-value-bind (y b f tq) (check-restrictions (pattern-b pattern))
	(dolist (nc1 a)
	  (dolist (nc2 b)
	    (when (classes-overlap-p nc1 nc2)
	      (rng-error nil "attribute name overlap in interleave: ~A ~A"
			 nc1 nc2))))
	(dolist (nc1 e)
	  (dolist (nc2 f)
	    (when (classes-overlap-p nc1 nc2)
	      (rng-error nil "element name overlap in interleave: ~A ~A"
			 nc1 nc2))))
	(when (and tp tq)
	  (rng-error nil "multiple text permitted by interleave"))
	(values (groupable-max x y)
		(append a b)
		(append e f)
		(or tp tq))))))

(defmethod check-restrictions ((pattern choice))
  (multiple-value-bind (x a e tp) (check-restrictions (pattern-a pattern))
    (multiple-value-bind (y b f tq) (check-restrictions (pattern-b pattern))
      (values (content-type-max x y)
	      (append a b)
	      (append e f)
	      (or tp tq)))))

(defmethod check-restrictions ((pattern list-pattern))
  (when *in-list-p*
    (rng-error nil "nested list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "list in data/except not allowed"))
  (let ((*in-list-p* t))
    (check-restrictions (pattern-child pattern)))
  (when *in-start-p*
    (rng-error nil "list in start not allowed"))
  :simple)

(defmethod check-restrictions ((pattern text))
  (when *in-list-p*
    (rng-error nil "text in list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "text in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "text in start not allowed"))
  (values :complex nil nil t))

(defmethod check-restrictions ((pattern data))
  (when *in-start-p*
    (rng-error nil "data in start not allowed"))
  (when (pattern-except pattern)
    (let ((*in-data-except-p* t))
      (check-restrictions (pattern-except pattern))))
  :simple)

(defmethod check-restrictions ((pattern value))
  (when *in-start-p*
    (rng-error nil "value in start not allowed"))
  :simple)

(defmethod check-restrictions ((pattern empty))
  (when *in-data-except-p*
    (rng-error nil "empty in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "empty in start not allowed"))
  :empty)

(defmethod check-restrictions ((pattern element))
  (unless (check-restrictions (pattern-child pattern))
    (rng-error nil "restrictions on string sequences violated")))

(defmethod check-restrictions ((pattern not-allowed))
  nil)


;;; compatibility restrictions

(defvar *in-element* nil)
(defvar *in-attribute* nil)
(defvar *in-choices* nil)
(defvar *in-default-value-p* nil)
(defvar *compatibility-table*)
(defvar *dtd-restriction-validator*)

(defun check-schema-compatibility (schema defns)
  (let* ((*error-class* 'dtd-compatibility-error)
	 (elements (mapcar #'defn-child defns))
	 (table (make-compatibility-table))
	 (*dtd-restriction-validator* (nth-value 1 (make-validator schema))))
    (setf (schema-compatibility-table schema) table)
    (let ((*compatibility-table* table))
      (mapc #'check-pattern-compatibility elements))
    (loop for elt1 being each hash-value in (dtd-elements table) do
	  (let ((nc1 (dtd-name elt1)))
	    (dolist (elt2 elements)
	      (let ((nc2 (pattern-name elt2)))
		(when (classes-overlap-p nc1 nc2)
		  (check-element-overlap-compatibility elt1 elt2)))))
	 ;; clean out ID type bookkeeping:
	 (let ((attributes (dtd-attributes elt1)))
	   (maphash (lambda (k v)
		      (when (eq (dtd-id-type v) :unknown)
			(if (dtd-default-value v)
			    (setf (dtd-id-type v) nil)
			    (remhash k attributes))))
		    attributes)))))

(defun check-element-overlap-compatibility (elt1 elt2)
  (unless
      (if (typep (pattern-name elt2) 'name)
	  ;; must both declare the same defaulted attributes
	  (loop
	     for a being each hash-value in (dtd-attributes elt1)
	     always (or (null (dtd-default-value a))
			(find elt2 (dtd-value-declared-by a))))
	  ;; elt1 has an attribute with defaultValue
	  ;; elt2 cannot have any defaultValue  ##
	  (loop
	     for a being each hash-value in (dtd-attributes elt1)
	     never (dtd-default-value a)))
    (rng-error nil "overlapping elements with and without defaultValue"))
  (unless
      (if (typep (pattern-name elt2) 'name)
	  ;; must both declare the same attributes with ID-type
	  (loop
	     for a being each hash-value in (dtd-attributes elt1)
	     always (or (eq (dtd-id-type a) :unknown)
			(find elt2 (dtd-id-type-declared-by a))))
	  ;; elt1 has an attribute with ID-type
	  ;; elt2 cannot have any ID-type  ##
	  (loop
	     for a being each hash-value in (dtd-attributes elt1)
	     always (eq (dtd-id-type a) :unknown)))
    (rng-error nil "overlapping elements with and without ID-type")))

(defun check-attribute-compatibility/default (pattern default-value)
  (unless (typep (pattern-name pattern) 'name)
    (rng-error nil "defaultValue declared in attribute without <name>"))
  (unless (typep (pattern-name *in-element*) 'name)
    (rng-error nil "defaultValue declared in element without <name>"))
  (let* ((hsx *dtd-restriction-validator*)
	 (derivation
	  (intern-pattern (pattern-child pattern) (registratur hsx))))
    (unless (value-matches-p hsx derivation default-value)
      (rng-error nil "defaultValue not valid")))
  (unless *in-choices*
    (rng-error nil "defaultValue declared outside of <choice>"))
  (dolist (choice *in-choices*
	   (rng-error nil "defaultValue in <choice>, but no <empty> found"))
    (when (or (typep (pattern-a choice) 'empty)
	      (typep (pattern-b choice) 'empty))
      (return)))
  (let ((a (ensure-dtd-attribute (pattern-name pattern)
				 *in-element*
				 *compatibility-table*))) 
    (cond
      ((null (dtd-default-value a))
       (setf (dtd-default-value a) default-value))
      ((not (equal (dtd-default-value a) default-value))
       (rng-error nil "inconsistent defaultValue declarations")))
    (push *in-element* (dtd-value-declared-by a))))

(defun check-attribute-compatibility/id (pattern default-value)
  (let* ((dt (pattern-type (pattern-child pattern)))
	 (id-type (cxml-types:type-id-type dt)))
    (when (and default-value (cxml-types:type-context-dependent-p dt))
      (rng-error nil
		 "defaultValue declared with context dependent type"))
    (when id-type
      (unless (typep (pattern-name pattern) 'name)
	(rng-error nil "defaultValue declared in attribute without <name>"))
      (unless (typep (pattern-name *in-element*) 'name)
	(rng-error nil "defaultValue declared in element without <name>"))
      (let ((a (ensure-dtd-attribute (pattern-name pattern)
				     *in-element*
				     *compatibility-table*))) 
	(cond
	  ((eq (dtd-id-type a) :unknown)
	   (setf (dtd-id-type a) id-type))
	  ((not (eq id-type (dtd-id-type a)))
	   (rng-error nil "inconsistent ID type attributes")))
	(push *in-element* (dtd-id-type-declared-by a))))))

(defmethod check-pattern-compatibility ((pattern attribute))
  (declare (optimize debug (speed 0) (space 0)))
  (let* ((*in-attribute* pattern)
	 (default-value (pattern-default-value pattern))
	 (*in-default-value-p* t))
    (when default-value
      (check-attribute-compatibility/default pattern default-value))
    (if (typep (pattern-child pattern) '(or data value))
	(check-attribute-compatibility/id pattern default-value)
	(check-pattern-compatibility (pattern-child pattern)))))

(defmethod check-pattern-compatibility ((pattern ref))
  nil)

(defmethod check-pattern-compatibility ((pattern one-or-more))
  (check-pattern-compatibility (pattern-child pattern)))

(defmethod check-pattern-compatibility ((pattern %combination))
  (check-pattern-compatibility (pattern-a pattern))
  (check-pattern-compatibility (pattern-b pattern)))

(defmethod check-pattern-compatibility ((pattern choice))
  (let ((*in-choices* (cons pattern *in-choices*)))
    (check-pattern-compatibility (pattern-a pattern))
    (check-pattern-compatibility (pattern-b pattern))))

(defmethod check-pattern-compatibility ((pattern list-pattern))
  (check-pattern-compatibility (pattern-child pattern)))

(defmethod check-pattern-compatibility ((pattern %leaf))
  nil)

(defmethod check-pattern-compatibility ((pattern data))
  (when (and *in-default-value-p*
	     (cxml-types:type-context-dependent-p (pattern-type pattern)))
    (rng-error nil "defaultValue declared with context dependent type"))
  (when (cxml-types:type-id-type (pattern-type pattern))
    (rng-error nil "ID type not a child of attribute")))

(defmethod check-pattern-compatibility ((pattern value))
  (when (and *in-default-value-p*
	     (cxml-types:type-context-dependent-p (pattern-type pattern)))
    (rng-error nil "defaultValue declared with context dependent type"))
  (when (cxml-types:type-id-type (pattern-type pattern))
    (rng-error nil "ID type not a child of attribute")))

(defmethod check-pattern-compatibility ((pattern element))
  (assert (null *in-element*))
  (let ((*in-element* pattern))
    (check-pattern-compatibility (pattern-child pattern))))
