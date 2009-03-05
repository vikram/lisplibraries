;;;; Trivial HTML output from Lisp
;;; $Id: htout.lisp,v 1.1 2005/08/23 20:49:44 gwking Exp $
;;;
;;; htout.lisp is copyright 1999-2002 by me, Tim Bradshaw, and may
;;; be used for any purpose whatsoever by anyone. It has no warranty
;;; whatsoever. I would appreciate acknowledgement if you use it in
;;; anger, and I would also very much appreciate any feedback or bug
;;; fixes.

;;; This should be *identical* to the version in pkg.lisp
;;;

(defpackage :org.tfeb.tml
  (:nicknames :org.tfeb.htout
   ;; backwards compatibility: deprecated!
   :tml :htout)
  (:use :cl)
  ;; WITH-HTML-OUTPUT &co
  (:export #:with-html-output           ;basic macro, and shorthands:
           #:htm                        ;reenter html mode
           #:fmt                        ;format in html
           #:esc                        ;escaped string
           #:str                        ;just a string
           #:lfd                        ;linefeed in html
           #:define-empty-tags          ;define a tag to have no content
           #:*html-element-predicate*   ;to tell if a symbol is an elt name
           #:*constant-html-predicate*) ;to tell if an object is constant
  ;; Utilities
  (:export #:emit-tag                   ;emit a tag
           #:escape-string)             ;implement ESC, useful in itself
  ;; TML
  (:export #:tml-parse 
           #:tml-open-elements
           #:tml->html))


(in-package :org.tfeb.tml)
(provide :org.tfeb.htout)               ;so people can use it standalone


;;;; Trivial HTML output from Lisp
;;;
;;; This is fragile, but the right idea.
;;;
;;; the trick of distinguishing HTML elt names by being keywords might
;;; want to be generalised somewhat (but it's a reasonable approach I
;;; think). It could obviously be infinitely generalised to XML but I
;;; don't want to bother with that.
;;;
;;; the listification of tags with attributes to get evaluation is a bit
;;; fragile -- ((:a href "foo") ...) breaks -- do we care?
;;;
;;; it might be cool to define compiler macros for some things to inline 
;;; more code?
;;;
;;; I'm not sure about all these shorthands.
;;;
;;; There is now fairly fancy constant HTML detection.
;;;
;;; The semantics of lisp forms (particularly variables) in HTML is
;;; somewhat weird and maybe should be addressed: in particular
;;; compound forms are evaluated for side-effect (presumably to print
;;; something), while symbols are evaluated for value which is then
;;; printed.  Except, maybe, in attribute lists...
;;;
;;; The constant detection stuff depends somewhat on CONSTANTP having
;;; sensible semantics, in particular that (CONSTANTP x) -> T => (EVAL
;;; x) succeeds.  This is not always true for at least CMUCL in the
;;; presence of DEFCONSTANT, since the values of things defined with
;;; DEFCONSTANT are not available at compile time.  I am pretty sure
;;; this is a bug in CMUCL, but it might be a deficiency in the spec.
;;; This is dealt with by having a variable which holds the actual
;;; predicate used, whose default value checks for unbound symbols.
;;;

;;; Known bugs and issues:
;;;
;;; EMIT-TAG doesn't deal with %'s when generating TML. It's not clear
;;; that ESCAPE-STRING is up to this.
;;;
;;; EMIT-TAG needs to be reworked in general.
;;;
;;; The notion of what is an element and when you can know 
;;; needs to be sorted out.
;;;
;;; The LANGUAGE option may not fully work.  One issue, again, is that
;;; the escaping stuff needs to be language dependent.  LANGUAGE
;;; should probably be OUTPUT-LANGUAGE or something.
;;;

(defmacro with-html-output ((var &optional stream &rest kwd/attr-args)
                            &body html)
  ;; Generate code which will write HTML from a Lispy representation.
  ;;
  ;; VAR will be bound to the stream to which output is printed.
  ;; STREAM if given is the stream, oitherwise VAR is rebound to its
  ;; own current value.  Any remaining arguments are keyword/value
  ;; pais which are given to HTMLIFY-FORMS.  The only currently
  ;; significant one is ATTRIBUTE-ARGUMENTS, which, if given as T,
  ;; modifies the representation of attribues so that HTML looks like
  ;; (tag attributes . body) instead of ((tag . attributes) . body),
  ;; which is more regular for machines to generate but slightly
  ;; harder for humans to type if most elts have no attributes.
  ;;
  ;; The undocumented (except here) LANGUAGE argument controls
  ;; the output language.  This may or may not completely work
  ;; (I've used it for creating TML output).
  ;; 
  ;; Note: the `prefered' syntax for arguments to this macro is now
  ;; (var stream &key ...), so, for instance 
  ;; (x x :attribute-arguments t).  However it supports the old 
  ;; (var stream &optional attribute-arguments-p) syntax as a special case.
  ;; See the hack with DESTRUCTURING-BIND below.  
  ;; Thanks to Rob Warnock for forcing me to make it backwards compatible.
  ;;
  ;; The expansion returns no values (this is also a change, it used
  ;; to return random grut).
  (destructuring-bind (&rest keyword-arguments &key &allow-other-keys)
      (if (= (length kwd/attr-args) 1)
          `(:attribute-arguments ,(first kwd/attr-args))
        kwd/attr-args)
    `(let ((,var ,(or stream var)))
       (macrolet ((htm (&body forms)
                    `(with-html-output (,',var 
                                        ,',var 
                                        ,@',keyword-arguments)
                       ,@forms))
                  (fmt (format-string &rest args)
                    `(format ,',var ,format-string ,@args))
                  (lfd (&optional (n 1))
                    (if (= n 1)
                        `(terpri ,',var)
                      `(loop repeat ,n
                             do
                             (terpri ,',var))))
                  (esc (string &optional map)
                    (let ((mname (make-symbol "MAP")))
                      `(let ((,mname ,map))
                         (write-sequence 
                          (if ,mname
                              (escape-string ,string ,mname)
                            (escape-string ,string))
                          ,',var))))
                  (str (string)
                    `(write-sequence ,string ,',var)))
         ,@(apply #'htmlify-forms html var keyword-arguments))
       (values))))


(defvar *empty-table* 
    (make-hash-table))

(defun empty-tag-p (tag)
  (values (gethash tag *empty-table*)))

(defmacro define-empty-tags (&rest tags)
  (warn "DEFINE-EMPTY-TAGS is deprecated, you probably should not use it")
  `(loop for tag in ',tags
       do (setf (gethash tag *empty-table*) tag)
       finally (return ',tags)))

(dolist (tag '(:br ':hr))
  (setf (gethash tag *empty-table*) tag))

(defvar *html-element-predicate* #'keywordp
  "Function called on a symbol to determine if it is an HTML element
or a Lisp function or macro.  The default value is #'KEYWORDP, meaning
that a symbol names an HTML element if it is a keyword symbol.
Something that satisfies this must be a constant in the sense of 
*CONSTANT-HTML-PREDICATE*.")

(declaim (inline html-element-p))
(defun html-element-p (sym)
  ;; Note that the predicate may only return true if SYM is a
  ;; constant
  (funcall *html-element-predicate* sym))

(defvar *constant-html-predicate*
 ;; The complexity below is to get round what I think is
 ;; a bug in CMUCL: CONSTANTP can be true of symbols
 ;; which are not bound - typically things defined with
 ;; DEFCONSTANT at compile time.  Really the test should
 ;; just be CONSTANTP, I think.
 ;;
 ;; Note that all of this assumes a null lexical
 ;; environment, which is safe.  We can't really assume
 ;; anything better because there might be things which
 ;; are constant in the current lexical environment but
 ;; which we don't know how to evaluate.
  #'(lambda (x)
      (if (symbolp x)
          (and (constantp x)
               (boundp x))
        (constantp x)))
  "Function called on an object to determine if it is a compile-time
constant, when considered as HTML.  This has much the same intention
as CONSTANTP, but it allows for some extra per-implementation
cleverness in case CONSTANTP doesn't work right, for instance in the
case of variables defined with DEFCONSTANT whose value may not be
available at compile-time.  If this returns true, then PRINC should
produce a reasonable representation of the object in HTML.")

(declaim (inline constant-html-p))
(defun constant-html-p (x)
  (funcall *constant-html-predicate* x))

(defun htmlify-forms (forms stream-var &rest keyword-arguments
                            &key &allow-other-keys)
  ;; Returns a list of expressions which emit FORMS on STREAM-VAR for
  ;; WITH-HTML-OUTPUT.  This works by accumulating forms into a list,
  ;; trying as hard as possible to make them strings, then collapsing
  ;; contiguous sequences of strings into one large string, which is
  ;; written out in one go.  Anything that can't be resolved into a
  ;; string at macroexpansion time is evaluated at run time in the
  ;; normal way.  HTMLIFY-ONE-FORM does most of the work.
  (let* ((results '()))
    (dolist (form forms)
      (apply #'htmlify-one-form form #'(lambda (x)
                                         (push x results))
             stream-var keyword-arguments))
    ;; Now, RESULTS is a (backwards) list of strings or expressions.
    ;; We want to find all the sequences of strings and concatenate them.
    ;; This is sort-of reduction, but not...
    (loop with strings = '()
          and forms = '()
          for result in results
          do (if (stringp result)
                 (push result strings)
               (progn
                 (when strings
                   (push `(write-sequence ,(format nil "~{~A~}" strings)
                                          ,stream-var)
                         forms)
                   (setf strings '()))
                 (push result forms)))
          finally
          (when strings 
            (push `(write-sequence ,(format nil "~{~A~}" strings)
                                   ,stream-var)
                  forms))
          (return forms))))

(defgeneric htmlify-one-form (form collector stream-var &key &allow-other-keys)
  ;; Process one form for WITH-HTML-OUTPUT.  FORM is the form to
  ;; process, COLLECTOR is a function which collects the result of
  ;; processing FORM STREAM-VAR is the name of the variable which will
  ;; be bound to the output stream in the expanded code.  KW args come
  ;; from WITH-HTML-OUTPUT.  Methods on this GF do the work depending
  ;; on the class of FORM. The return values, if any, arge ignored: to
  ;; collect a result you call COLLECTOR on it - This lets recursive
  ;; calls collect into the same flat list of things to emit.  which
  ;; can then be collapsed.  Collected strings are collapsed, anything
  ;; else is left as a form to evaluate at run time.
  )

(defmethod htmlify-one-form ((form cons) collector stream-var &key
                             (attribute-arguments nil)
                             (language ':html))
  ;; This is the complicated case - either a bit of HTML to be
  ;; emitted, or a compound Lisp form.  ATTRIBUTE-ARGUMENTS changes
  ;; the HTML representation of attributes from ((:foo :x 1 ...) ...)
  ;; to (:foo (:x 1 ...) ...)  which is more regular but a bit harder
  ;; to type in the trivial case of no attributes - (:foo ...) vs
  ;; (:foo () ...).
  (macrolet ((collect (form)
               `(funcall collector ,form)))
    (if (let ((eltish (first form)))
          ;; This is the ultimate Lisp code: bindings in the test of a 
          ;; conditional..
          (or (html-element-p eltish)
              (and (not attribute-arguments)
                   (consp eltish)
                   (html-element-p (first eltish)))))
        ;; This is an HTML form, not random Lisp code
        (multiple-value-bind (tag attributes body)
            (if attribute-arguments
                ;; New-style: (:foo (:x 1) ...)
                (destructuring-bind (tag attributes . body) form
                  (values tag attributes body))
              ;; Old style: ((:foo :x 1) ...), or (:foo ...)
              (destructuring-bind (elt . body) form
                (values (if (consp elt) (first elt) elt)
                        (if (consp elt) (rest elt) '())
                        body)))
              ;; Sanity checks: warn on empty tags with non-null bodies and
              ;; check for mutant attribute lists
              (when (and (empty-tag-p tag)
                         (not (null body)))
                (warn "Ignoring body of empty tag ~S" tag))
              (unless (listp attributes)
                (error "Mutant attribute `list' ~S" attributes))
              ;; See *CONSTANT-HTML-PREDICATE* above for what this means
              (let ((constant (every *constant-html-predicate* attributes))
                    (empty (or (empty-tag-p tag)
                                (null body))))
                ;; If CONSTANT is true this is a constant element,
                ;; but it may have a non-constant body.  Note that we already
                ;; know that the tag is a constant, because HTML-ELEMENT-P
                ;; is required to only return true if it is.
                (collect
                 (if constant
                     (emit-tag tag nil
                               :type (if empty ':empty ':open)
                               :language language
                               :attribute-plist (if attributes
                                                    ;; Yes, EVAL.
                                                    (eval `(list ,@attributes))
                                                  '()))
                   `(emit-tag ',tag ,stream-var
                              :type ,(if empty ':empty ':open)
                              :language ',language
                              :attribute-plist
                              ;; (list ...) because the forms in ...
                              ;; must be evaluated
                              ,(if attributes `(list ,@attributes) '()))))
                (dolist (b body)
                  (htmlify-one-form 
                   b collector stream-var
                   :attribute-arguments attribute-arguments
                   :language language))
                (unless empty
                  (collect
                   (if constant
                       (emit-tag tag nil :type :close :language language)
                     `(emit-tag ',tag ,stream-var 
                                :type :close
                                :language ',language))))))
      ;; This is a general Lisp form: collect it for evaluation
      (collect form))))

(defmethod htmlify-one-form ((form symbol) collector stream-var &key
                             (language ':html))
  (macrolet ((collect (form)
               `(funcall collector ,form)))
    (if (html-element-p form)
        (collect (emit-tag form nil :type :empty :language language))
      (collect `(princ ,form ,stream-var)))))

(defmethod htmlify-one-form ((form string) collector stream-var &key)
  (declare (ignore stream-var))
  (funcall collector form))

(defmethod htmlify-one-form ((form character) collector stream-var &key)
  (declare (ignore stream-var))
  (funcall collector (string form)))

(defmethod htmlify-one-form ((form t) collector stream-var &key
                             (language ':html))
  ;; Is this sensible?  Is it safe to assume constantness?
  ;; Is the element thing right?
  (macrolet ((collect (form)
               `(funcall collector ,form)))
    (if (not (constant-html-p form))
        (progn
          (warn "Seen a non-constant ~S, wasn't expecting this" (type-of form))
          (collect `(princ ,form ,stream-var)))
      (collect (if (html-element-p form)
                   (emit-tag form nil :type :empty :language language)
                 (with-output-to-string (out)
                   (princ form out)))))))


(defvar *html-escape-map*
    '((#\< . "&lt;")
      (#\> . "&gt;")
      (#\& . "&amp;")))

(defvar *sgml/xml-attval-quote-map*
  '((#\' . "&#39;")
    (#\" . "&#34;")))

(defun escape-string (string &optional (map *html-escape-map*))
  ;; escape the characters in MAP in STRING.  This is an easy way of
  ;; doing it but I haven't thought abut making it efficient.
  (declare (type string string))
  (if (not (find-if #'(lambda (c)
                        (assoc c map))
                    string))
      string
      (with-output-to-string (o)
        (loop for prev = 0 then (1+ found)
            for found = (position-if #'(lambda (c)
                                         (assoc c map))
                                     string
                                     :start prev)
            while found
            do
              (write-sequence string o :start prev :end found)
              (write-sequence (cdr (assoc (char string found) map)) o)
            finally
              (write-sequence string o :start prev :end (length string))))))

(define-compiler-macro escape-string (&whole form string &optional 
                                             (map *html-escape-map*))
  ;; Note that this can effectively wire in the compile-time value of
  ;; *HTML-ESCAPE-MAP*.
  (if (and (stringp string) (eq map *html-escape-map*))
      (escape-string string map)
    form))

(defgeneric emit-tag (tag stream &key 
                                 type language
                                 attribute-plist attribute-alist)
  ;; Emit a tag on stream.  This is exported.
  ;; LANGUAGE says what language to use, the interesting value at
  ;; present being :XML (:XHTML is equivalent), which says to use XML
  ;; empty tag conventions.  Default is :SGML, :HTML is equivalent.
  ;; There should be a better protocol for dealing with languages.
  ;; if STREAM is NIL then this returns a string with the output, otherwise
  ;; returns no values.
  ;; 
  ;; Methods on this GF know about symbols for tags and attribute
  ;; names, but prints anything else with the ~A formatting directive
  ;; or PRINC.  So if you want to control how your own tags are
  ;; printed make sure they aren't symbols and define a printer for
  ;; them.
  ;;
  ;; This whole thing really needs to be redone in some more
  ;; principled way,
  (:argument-precedence-order stream tag)
  ;; This method defaults KW args - this is really intended as a
  ;; wrapping method to default args...
  (:method :around (tag stream &key
                        (type :open)
                        (language ':sgml)
                        (attribute-plist '())
                        (attribute-alist '()))
   (call-next-method tag stream :type type :language language
                     :attribute-plist attribute-plist
                     :attribute-alist attribute-alist)))

(defmethod emit-tag (tag (stream null) &key
                         type language
                         attribute-plist attribute-alist)
  (with-output-to-string (out)
    (emit-tag tag out :type type :language language
              :attribute-plist attribute-plist
              :attribute-alist attribute-alist)))

(defmethod emit-tag ((tag t) stream &key
                     type language attribute-plist attribute-alist)
  ;; This is the method that does the work, except in the case of TML.
  (case language
    ;; Check the language. There should be a better language protocol
    ((:sgml :html :xml :xhtml))
    ((:tml)
     (error "Can't emit TML from general tags, need symbols"))
    (otherwise
     (error "Unknown language ~S" language)))
  (ecase type
    ((:open :empty)
     (princ "<" stream)
     (princ tag stream)
     ;; It is OK to provide both alist and plist (but probably
     ;; strange)
     (when attribute-plist
       (loop for tail = attribute-plist then (cddr tail)
           for att = (first tail) and val = (second tail)
           while tail
           do
             (format stream " ~A~@[='~A'~]"
                     (typecase att
                       (symbol (symbol-name att))
                       (t att))
                     (and val 
                          (escape-string (format nil "~A" val)
                                         *sgml/xml-attval-quote-map*)))))
     (when attribute-alist
       (loop with a and v
           for av in attribute-alist
           do
             (typecase av
               (cons 
                (setf a (first av) 
                      v (second av)))
               (t (setf a av
                        v nil)))
             (format stream " ~A~@[='~A'~]"
                     (typecase a
                       (symbol (symbol-name a))
                       (t a))
                     (and v
                          (escape-string (format nil "~A" v)
                                         *sgml/xml-attval-quote-map*)))))
     (princ (if (and (member language '(:xml :xhtml))
                     (eql type ':empty)) "/>" ">") stream))
    ((:close)
     (princ "</" stream)
     (princ tag stream)
     (princ ">" stream)))
  (values))

(defmethod emit-tag ((tag symbol) stream &key
                     type language attribute-plist attribute-alist)
  ;; I hate this passing-down-of-keywords stuff.  &REST and APPLY
  ;; could avoid it, but ...
  (case language
    ((:tml)
     ;; TML needs to know about the symbol so it has the option
     ;; of printing package prefixes
     ;; We should deal with % chars in attributes here.
     (ecase type
       (:open
        (format stream "<~S~{ ~S ~S~}~:{ ~S ~S~}|" tag
                attribute-plist attribute-alist))
       (:empty
                (format stream "<~S~{ ~S ~S~}~:{ ~S ~S~}>" tag
                attribute-plist attribute-alist))
       (:close
        (princ ">" stream)))
     (values))
    (otherwise
     (emit-tag (symbol-name tag) stream 
               :type type :language language
               :attribute-plist attribute-plist
               :attribute-alist attribute-alist))))

(defmethod emit-tag ((tag list) stream &key 
                     type language attribute-plist attribute-alist)
  (emit-tag (first tag) stream 
            :type type :language language
            :attribute-plist (if (not (null attribute-plist))
                                 ;; ? order
                                 (append (rest tag) attribute-plist)
                               (rest tag))
            :attribute-alist attribute-alist))

#||
(defun count-numbers (n w &optional (s *standard-output*))
  (with-html-output (s)
    (:html
     (:head (:title 
             (fmt "Numbers from zero below ~R" n)))
     (:body
      (:h1  (fmt "Numbers from zero below ~R" n))
      ;; Forms beginning with non-keyword symbols are code to be evaluated.
      (lfd)
      (:p "Table border width "
          (princ w s))
      ;; isolated keywords are empty tags.
      :br
      (lfd)
      ;; empty tags with attributes need this slightly crufty syntax, 
      ;; and also need to be defined as empty. 
      ((:hr :noshade))
      (:center
       ;; the values of atttributes are evaluated (in fact the whole 
       ;; attribute list is, but attribute names asre keywords).
       ((:table :border w
                :width "90%")
        (:tbody                         ;html 4, bah.
         (:tr
          ((:th :align :left) "English")
          ((:th :align :right) "Arabic")
          ((:th :align :right) "Roman"))
         ;; you can leap into Lisp...
         (dotimes (i n)
           (let ((c (if (evenp i) "blue" "white")))
             ;; ... and then back into HTML: the local HTML macro is shorthand
             ;; for WITH-HTML-OUTPUT to the same stream.
             (htm
              ((:tr :bgcolor c)
               ((:td :align :left)
                (fmt "~R" i))
               ((:td :align :right)
                (fmt "~D" i))
               ((:td :align :right)
                (if (zerop i)
                    (fmt "")
                    (fmt "~:@R" i))))
              (lfd)))))))
       ((:hr :noshade))))))

(defun create-blank-page (s title)
  (with-html-output (s)
    (:html
     (:head
      (:title (esc title))
      (lfd))
     (:body
      (:h1 (esc title))
      (lfd)
      "<!-- Body here -->"
      (lfd)))))
||#