(defpackage :cxml-rng
  (:use :cl)
  (:export #:rng-error
	   #:dtd-compatibility-error
	   #:rng-error-line-number
	   #:rng-error-column-number
	   #:rng-error-system-id

	   #:schema
	   #:schema-start

	   #:parse-schema
	   #:parse-compact
	   #:serialize-schema
	   #:make-validator
	   #:make-validating-source
	   #:make-dtd-compatibility-handler

	   #:pattern
	   #:element
	   #:attribute
	   #:group
	   #:interleave
	   #:choice
	   #:one-or-more
	   #:ref
	   #:empty
	   #:text
	   #:value
	   #:data
	   #:not-allowed
	   #:list-pattern

	   #:pattern-a
	   #:pattern-b
	   #:pattern-child
	   #:pattern-element
	   #:pattern-except
	   #:pattern-name
	   #:pattern-params
	   #:pattern-string
	   #:pattern-type
	   #:pattern-value

	   #:name-class
	   #:any-name
	   #:name
	   #:ns-name
	   #:name-class-choice

	   #:any-name-except
	   #:name-uri
	   #:name-lname
	   #:ns-name-uri
	   #:ns-name-except
	   #:name-class-choice-a
	   #:name-class-choice-b)
  (:documentation
   "@code{cxml-rng} implements @a[http://relaxng.org/spec-20011203.html]{
    Relax NG} schema validation for Closure XML.

    Support for @a[http://relaxng.org/compact-20021121.html]{Compact Syntax}
    and @a[http://relaxng.org/compatibility-20011203.html]{DTD Compatibility}
    is included.

    @begin[Example]{section}
    @begin{pre}(cxml:parse-file \"test.xml\"
                 (cxml-rng:make-validator
                  (cxml-rng:parse-schema #p\"test.rng\")))
    @end{pre}
    @end{section}
    @begin[Classes]{section}
    @aboutclass{schema}
    @aboutclass{rng-error}
    @aboutclass{dtd-compatibility-error}
    @end{section}
    @begin[Parsing and validating]{section}
    @aboutfun{parse-schema}
    @aboutfun{parse-compact}
    @aboutfun{make-validator}
    @aboutfun{make-dtd-compatibility-handler}
    @aboutfun{serialize-grammar}
    @end{section}
    @begin[Grammar introspection]{section}
    The following classes and function are exported so that users can
    take a peek at the internals of the parsed and simplified grammar.

    @aboutfun{schema-start}
    @aboutclass{attribute}
    @aboutclass{choice}
    @aboutclass{data}
    @aboutclass{element}
    @aboutclass{empty}
    @aboutclass{group}
    @aboutclass{interleave}
    @aboutclass{list-pattern}
    @aboutclass{not-allowed}
    @aboutclass{one-or-more}
    @aboutclass{pattern}
    @aboutclass{ref}
    @aboutclass{text}
    @aboutclass{value}
    @aboutfun{pattern-child}
    @aboutfun{pattern-a}
    @aboutfun{pattern-b}
    @aboutfun{pattern-name}
    @aboutfun{pattern-element}
    @aboutfun{pattern-type}
    @aboutfun{pattern-string}
    @aboutfun{pattern-value}
    @aboutfun{pattern-params}
    @aboutfun{pattern-except}
    @end{section}"))

(defpackage :cxml-types
  (:use :cl)
  (:export #:param
	   #:make-param
	   #:param-name
	   #:param-value

	   #:data-type
	   #:find-type
	   #:type-library
	   #:type-name
	   #:type-context-dependent-p
	   #:type-id-type
	   #:parse
	   #:equal-using-type
	   #:lessp-using-type
	   #:validp
	   #:validation-context
	   #:sax-validation-context-mixin
	   #:klacks-validation-context
	   #:make-klacks-validation-context
	   #:context-find-namespace-binding
	   #:context-find-unparsed-entity
	   #:rng-type
	   #:token-type
	   #:string-type

	   #:dtd-compatibility-type
	   #:id-type
	   #:idref-type
	   #:idrefs-type

	   #:xsd-type
	   #:patterns
	   #:min-exclusive
	   #:max-exclusive
	   #:min-inclusive
	   #:max-inclusive
	   #:min-length
	   #:max-length
	   #:exact-length
	   #:fraction-digits
	   #:total-digits

	   #:duration-type
	   #:date-time-type
	   #:time-type
	   #:date-type
	   #:year-month-type
	   #:year-type
	   #:month-day-type
	   #:day-type
	   #:month-type
	   #:boolean-type
	   #:base64-binary-type
	   #:hex-binary-type
	   #:float-type
	   #:decimal-type
	   #:double-type
	   #:any-uri-type
	   #:qname-type
	   #:notation-type
	   #:xsd-string-type
	   #:normalized-string-type
	   #:xsd-token-type
	   #:language-type
	   #:name-type
	   #:ncname-type
	   #:xsd-id-type
	   #:xsd-idref-type
	   #:xsd-idrefs-type
	   #:entity-type
	   #:entities-type
	   #:nmtoken-type
	   #:nmtokens-type
	   #:integer-type
	   #:non-positive-integer-type
	   #:negative-integer-type
	   #:long-type
	   #:int-type
	   #:short-type
	   #:byte-type
	   #:non-negative-integer-type
	   #:unsigned-long-type
	   #:unsigned-int-type
	   #:unsigned-short-type
	   #:unsigned-byte-type
	   #:positive-integer-type)
  (:documentation
   "@code{cxml-types} defines an extensible interface for XML-related
    data types as required for use in Relax NG validation.

    It includes Relax NG's minimal built-in type library, which is named
    @code{:||} and defines the types \"string\" and \"token\".

    In addition, it implements the built-in types of
    @a[http://www.w3.org/TR/xmlschema-2/]{XML Schema Datatypes}
    as specified in @a[http://relaxng.org/xsd-20010907.html]{Guidelines for
    using W3C XML Schema Datatypes with RELAX NG}.  The XSD type library
    is named @code{:|http://www.w3.org/2001/XMLSchema-datatypes|}.

    The types defined by @a[http://relaxng.org/compatibility-20011203.html]{
      RELAX NG DTD Compatibility}
    are available through the data type library named
    @code{:|http://relaxng.org/ns/compatibility/datatypes/1.0|}.

    @begin[Example]{section}
    @begin{pre}
* (setf ttt (cxml-types:find-type :|| \"token\"))
#<CXML-TYPES:TOKEN-TYPE {1002D16B71@}>
* (cxml-types:parse ttt \"a b\")
\"a b\"
* (cxml-types:parse ttt \"a     b\")
\"a b\"
* (cxml-types:equal-using-type ttt ** *)
T
    @end{pre}
    @end{section}
    @begin[Type instances]{section}
      Each type, together with its parameters, is represented by an
      instance of @code{data-type}.  The generic function @fun{find-type},
      defined for each library, creates type instances.  A type's properties
      are accessible using @fun{type-name}, @fun{type-library}, and
      @fun{type-context-dependent-p}.

      @aboutclass{data-type}
      @aboutclass{rng-type}
      @aboutclass{xsd-type}
      @aboutfun{find-type}
      @aboutfun{type-name}
      @aboutfun{type-library}
      @aboutfun{type-context-dependent-p}
    @end{section}
    @begin[Using types]{section}
      Types allow strings to be tested for validity and equality.
      @fun{validp} checks whether a string can be parsed.  If it is valid,
      @fun{parse} will compute the string's @emph{value}, and return a
      Lisp object of a type-specific class as a representation of that value.
      Values returned by @fun{parse} can be compared for equality using
      @fun{equal-using-type}.  Some types also define a partial ordering,
      which can be queried using @fun{lessp-using-type}.

      @aboutfun{validp}
      @aboutfun{parse}
      @aboutfun{equal-using-type}
      @aboutfun{lessp-using-type}
    @end{section}
    @begin[The validation context]{section}
      Some types are context dependent, as indicated by
      @fun{type-context-dependent-p}.  Those types need access to state
      computed by the XML parser implicitly, like namespace bindings or
      the Base URI.

      An abstract class @class{validation-context} is defined that
      users of this API can implement a subclass of
      to define methods for the generic functions listed below.

      In addition, two pre-defined validation context implementations are
      provided, one for use with SAX, the other based on Klacks.

      @aboutclass{validation-context}
      @aboutclass{sax-validation-context-mixin}
      @aboutclass{klacks-validation-context}
      @aboutfun{context-find-namespace-binding}
      @aboutfun{context-find-unparsed-entity}
    @end{section}
    @begin[Relax NG built-in types]{section}
      The following primitive types are defined by Relax NG:

      @aboutclass{string-type}
      @aboutclass{token-type}
    @end{section}
    @begin[DTD compatibility types]{section}
      The following primitive types are defined by Relax NG DTD
      Compatibility:

      @aboutclass{id-type}
      @aboutclass{idref-type}
      @aboutclass{idrefs-type}
    @end{section}
    @begin[Primitive XSD built-in types]{section}
      The following primitive types are part of the XSD built-in data type
      library:

      @aboutclass{duration-type}
      @aboutclass{date-time-type}
      @aboutclass{time-type}
      @aboutclass{date-type}
      @aboutclass{year-month-type}
      @aboutclass{year-type}
      @aboutclass{month-day-type}
      @aboutclass{day-type}
      @aboutclass{month-type}
      @aboutclass{boolean-type}
      @aboutclass{base64-binary-type}
      @aboutclass{hex-binary-type}
      @aboutclass{float-type}
      @aboutclass{decimal-type}
      @aboutclass{double-type}
      @aboutclass{any-uri-type}
      @aboutclass{qname-type}
      @aboutclass{notation-type}
      @aboutclass{xsd-string-type}
    @end{section}
    @begin[Enumerated XSD built-in types]{section}
      The following types are part of the XSD built-in data type
      library, and are defined as derived types through enumeration.
      Relax NG does not implement the enumeration facet, so although these
      types are described as \"derived\", they are implemented directly.

      @aboutclass{xsd-idrefs-type}
      @aboutclass{entities-type}
      @aboutclass{nmtokens-type}
    @end{section}
    @begin[Derived XSD built-in types]{section}
      The following types are part of the XSD built-in data type
      library, and are defined as derived types through restriction.

      @aboutclass{normalized-string-type}
      @aboutclass{xsd-token-type}
      @aboutclass{language-type}
      @aboutclass{name-type}
      @aboutclass{ncname-type}
      @aboutclass{xsd-id-type}
      @aboutclass{xsd-idref-type}
      @aboutclass{entity-type}
      @aboutclass{nmtoken-type}
      @aboutclass{integer-type}
      @aboutclass{non-positive-integer-type}
      @aboutclass{negative-integer-type}
      @aboutclass{long-type}
      @aboutclass{int-type}
      @aboutclass{short-type}
      @aboutclass{byte-type}
      @aboutclass{non-negative-integer-type}
      @aboutclass{unsigned-long-type}
      @aboutclass{unsigned-int-type}
      @aboutclass{unsigned-short-type}
      @aboutclass{unsigned-byte-type}
      @aboutclass{positive-integer-type}
    @end{section}"))
