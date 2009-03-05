;; -*- lisp -*-

(in-package :it.bese.yaclml)

;;;; * YACLML - Programmatic HTML Generation

;;;; The programmatic interface is a collection of Common Lisp macros
;;;; designed to make embedding HTML in lisp code easy. It was created
;;;; with the following goals in mind:

;;;; - The code for creating HTML should look and act like regular lisp
;;;;   code.

;;;; - Given what we know about HTML and the ratio of static to dynamic
;;;;   text in a typical web page it's important to constant fold as much
;;;;   as possible.

;;;; - Tags should be easily definable and should be able to perform
;;;;   arbitrary computations at both run time and compile time.

;;;; ** Using YACLML Tag Macros

;;;; You use YACLML tags just like regular macros, any attributes are
;;;; passed in like keyword arguments. YACLML examines its args (at
;;;; compile time) and distinguishes between the keyword arguments
;;;; which become attributes and everything else, which becomes the
;;;; tag's body. Tags all have the following syntax:

;;;;  ( tag-name [ :keyword value ] * . body )

;;;; ** Tag Attributes

;;;; The name of the attribute will be the result of string-downcase'ing
;;;; the symbol-name specified in the macro. Depending on the runtime value
;;;; returned executing the value specified in the macro call three things
;;;; can happen:

;;;; NIL - The attribute will be ignored

;;;; T   - The attribute will be printed with (string-downcase name) as
;;;;       the value.

;;;; anything else - The result of evaluating the value will be
;;;;                 printed (via PRINC) as the value of the
;;;;                 attribute.

;;;; If the need ever arises to have an HTML attribute whose value is
;;;; T or NIL it is necessary to return the string \"T\" or \"NIL\"
;;;; and not the symbol T or NIL.

;;;; ** The Tag Body

;;;; Every element of the tag body is processed in order: if it is a
;;;; form it is executed at runtime and must explicitly print to the
;;;; stream *yaclml-stream* if it needs to generate output, if it is a
;;;; string its value will be printed to *yaclml-stream* at run time.

;;;; ** Examples

;;;;   ;; Assuming *yaclml-stream* is bound to *standard-output*

;;;;   (<:a :href \"http://foo.com\" \"foo.com\")
;;;;   => 
;;;;   <a href=\"http://foo.com\">foo.com</a>

;;;;   (<:br)
;;;;   =>
;;;;   <br/>

;;;;   (<:td \"whatever\")
;;;;   =>
;;;;   <td>whatever</td>

(defvar *yaclml-stream* t
  "The stream to which tags are printed.")

(defvar *yaclml-indent* t
  "When T (must be set while compiling yaclml code) the generated
  HTML is indented.")

(defvar %yaclml-indentation-depth% 0)

(defmacro with-yaclml-stream (stream &body body)
  "Evaluate BODY with *yaclml-stream* bound to STREAM."
  `(let ((*yaclml-stream* ,stream))
     (declare (special *yaclml-stream*))
     ,@body))

(defmacro with-yaclml-output-to-string (&body body)
  "Evaluate BODY with *yaclml-stream* bound to a string stream, return the string."
  (with-unique-names (output)
    `(with-output-to-string (,output)
       (with-yaclml-stream ,output
         ,@body))))

(defvar %yaclml-code% nil
  "The list of currently collected code this yaclml macro should
   expand into.")

(defvar %yaclml-custom-attributes% nil
  "The list of collected custom attributes in the most recent attribute-bind.
   Custom attributes are defined with (@ name value ...) or (@ list-of-name-value-pairs).")

(defvar *expanders* (make-hash-table :test 'eql)
  "Hash table mapping expanders to the expander function.")

(defvar *expander-macros* (make-hash-table :test 'eql)
  "Hash table mapping expander macros to theri macre functions.")

(defun yaclml-constant-p (thing)
  "Returns T if THING is, as far as yaclml is concerned, a run time
  constant."
  (or (stringp thing)
      (characterp thing)
      (numberp thing)
      (keywordp thing)))

(defun emit-princ (&rest items)
  "Emit to the current yaclml-code a form which will, at runtime,
   princ ITEM. If (yaclml-constant-p ITEM) is true the princ will
   be done at compile time."
  (dolist (item items %yaclml-code%)
    (push (cond
            ((stringp item)
             item)
            ((keywordp item)
             (string-downcase (princ-to-string item)))
            ((yaclml-constant-p item)
             (princ-to-string item))
            (t `(princ ,item *yaclml-stream*)))
          %yaclml-code%)))

(defun emit-html (&rest items)
  "Like EMIT-PRINC but escapes html chars in item."
  (dolist (item items %yaclml-code%)
    (if (yaclml-constant-p item)
        (push (escape-as-html (princ-to-string item)) %yaclml-code%)
        (push `(emit-attribute-value ,item) %yaclml-code%))))
  
(defun emit-code (&rest forms)
  "Emit to the current yaclml-code CODE. This means that whatever
   CODE is it will be run, and it's result will be ignored, at
   runtime."
  (setf %yaclml-code% (nconc forms %yaclml-code%)))

(defmacro emit-attribute (name value)
  (rebinding (value)
    `(case ,value
      ((t)
       (princ #\Space *yaclml-stream*)
       (princ ,name *yaclml-stream*)
       (princ "=\"" *yaclml-stream*)
       (princ ,name *yaclml-stream*)
       (princ #\" *yaclml-stream*))
      ((nil) nil)
      (t
       (princ #\Space *yaclml-stream*)
       (princ ,name *yaclml-stream*)
       (princ "=\"" *yaclml-stream*)
       (emit-attribute-value ,value)
       (princ #\" *yaclml-stream*)))))

(defun emit-princ-attribute (name value)
  (unless (stringp name)
    (setf name (string-downcase (princ-to-string name))))
  (emit-code
   (rebinding (value)
     `(case ,value
       ((t)
        (princ ,(concatenate 'string " " name "=\"" name "\"")
         *yaclml-stream*))
       ((nil) nil)
       (t
        (princ ,(concatenate 'string " " name "=\"") *yaclml-stream*)
        (emit-attribute-value ,value)
        (princ "\"" *yaclml-stream*))))))

(defun emit-attribute-value (value)
  (if (listp value)
      (iter (for el in value)
            (unless (first-time-p)
              (princ #\Space *yaclml-stream*))
            (write-as-html (princ-to-string el) :stream *yaclml-stream*))
      (write-as-html (princ-to-string value) :stream *yaclml-stream*)))

(defun emit-princ-attributes (attributes)
  ;; convert the list of attributes at compile time to the format we expect when attributes are provided at runtime.
  ;; this way this conversion only happens at compile time, not the other way around.
  (%emit-princ-attributes (iter (for (name . value) in attributes)
                                (nconcing (list name value))))
  (dolist (custom-attributes %yaclml-custom-attributes%)
    (if (rest custom-attributes)
        (%emit-princ-attributes custom-attributes)
        (emit-code `(iter (for (name value) on ,(first custom-attributes) :by #'cddr)
                          (unless (stringp name)
                            (setf name (string-downcase (string name))))
                          (emit-attribute name value))))))

(defun %emit-princ-attributes (attributes)
  "Assuming attributes is a list of (name1 value1 name2 value2 ...), emit
   the code necessary to print them at runtime. If VALUE is a
   list every element will be concatenated separated by a space to
   form the final string value of the attribute.

If the value of any of the attributes is NIL it will be ignored.

If a value is the symbol T the name of the attribute will be used
as the value."
  (unless (evenp (length attributes))
    (error "The number of elements in the attribute list is not even"))
  (iter (for (key value) on attributes :by #'cddr)
        (cond
          ((eql t value)
           ;; according to xhtml thoses attributes which in html are
           ;; specified without a value should just use the attribute
           ;; name as the xhtml value
           (emit-princ " " key "=\"" key "\""))
          ((eql nil value) nil)
          ((yaclml-constant-p value)
           (progn
             (emit-princ " " key "=\"")
             (emit-html value)
             (emit-princ "\"")))
          (t
           (if (and (consp value)
                    (eql 'cl:concatenate (first value))
                    (consp (cdr value))
                    (eql 'cl:string (second value)))
               ;; a call to concatenate can be dealt with specially
               (progn
                 (emit-princ " " key "=\"")
                 (dolist (val (cddr value))
                   (emit-princ val)))
               (emit-princ-attribute key value)))))
  %yaclml-code%)

(defun emit-indentation ()
  (when *yaclml-indent*
    (emit-princ #\Newline)
    (emit-princ (make-string %yaclml-indentation-depth% :initial-element #\Space))))

(defun emit-open-tag (name attributes)
  "Emit the code required to print an open tag whose name is NAME and
with the attributes attributes."
  (incf %yaclml-indentation-depth% 2)
  (emit-princ "<")
  (emit-princ name)
  (emit-princ-attributes attributes)
  (emit-indentation)
  (emit-princ ">"))

(defun emit-close-tag (name)
  "Emit the code required to print a close tag whose name is NAME."
  (decf %yaclml-indentation-depth% 2)
  (emit-princ "</" name)
  (emit-indentation)
  (emit-princ ">"))

(defun emit-empty-tag (name attributes)
  "Emit the code required to print an empty tag with name NAME and a
attributes ATTRIBUTES."
  (emit-princ "<" name)
  (emit-princ-attributes attributes)
  (emit-indentation)
  (emit-princ "/>"))

(defun emit-body (body)
  "Traverse body and emit the corresponding code. Every form in body
is analyzed according to the following rules:

cons whose car is not a known expander - code which should be included
with no further analysis.

cons whose car is a known expander - simply call the expander function
with the cdr of the cons as the arg.

yaclml-constant-p - print the constant (after escape-as-html) to
*yaclml-stream*.

cons whose car is YACLML-QUOTE - emit-body on every element of the
cdr.
"
  (dolist (form body)
    (emit-form form)))

(defun emit-form (form)
  "Emits the code to print FORM."
  (if (consp form)
      (let ((op (car form)))
        (cond
          ((gethash op *expander-macros*)
           (emit-form (funcall (gethash op *expander-macros*) (cdr form))))
          ((gethash op *expanders*)
           (funcall (gethash op *expanders*) (cdr form)))
          ((eql 'yaclml-quote op)
           (dolist (b (cdr form))
             (emit-form b)))
          ((eql 'cl:progn op)
           (dolist (b (cdr form))
             (emit-form b)))
          (t (emit-code form))))
      (if (yaclml-constant-p form)
          (emit-princ (escape-as-html (princ-to-string form)))
          (emit-code form))))

(defmacro deftag (name attributes &body body)
  "Define a new tag.

ATTRIBUTES should be an attribute-spec (see parse-attributes and
attribute-bind).

BODY is simply the body of the expander lambda.

Within the BODY the functions EMIT-CODE, EMIT-PRINC and EMIT-HTML can
be used to generate code. EMIT-CODE should be passed lisp code which
will be executed at runtime."
  (with-unique-names (contents)
    `(progn
       (setf (gethash ',name *expanders*)
             (lambda (,contents)
               (handler-bind ((unrecognized-attribute (lambda (c)
                                                        (setf (tag c) ,contents))))
                 (attribute-bind ,attributes ,contents
                   ,@body))))
       (defmacro ,name (&rest contents)
         (let ((%yaclml-code% nil)
	       (%yaclml-indentation-depth% 0))
           ;; build tag's body
	   (funcall (gethash ',name *expanders*) contents)
           (setf %yaclml-code% (nreverse %yaclml-code%))
	   ;; now that we've generated the code we can fold the
	   ;; strings in yaclml-code and princ them, leaving any other
	   ;; forms as they are.
           `(progn ,@(mapcar (lambda (form)
                               (if (stringp form)
                                   `(write-string ,form *yaclml-stream*)
				   form))
                             (fold-strings %yaclml-code%))
             (values)))))))

(defmacro deftag-macro (name attributes &body body)
  "Define a new YACLML tag macro.

Tag macros, like regular macros, expand into other YACLML tag
forms which are recursivly processed."
  (let ((contents (gensym))
        (doc-string (if (stringp (first body))
                        (pop body)
                        nil)))
    `(progn
       (setf (gethash ',name *expander-macros*)
             (lambda (,contents)
               (handler-bind ((unrecognized-attribute (lambda (c)
                                                        (setf (tag c) ,contents))))
                 (attribute-bind ,attributes ,contents
                   ,@body))))
       (defmacro ,name (&rest ,contents) ,doc-string
         (funcall (gethash ',name *expander-macros*) ,contents))
       ',name)))

(defmacro def-simple-xtag (name)
  "Convience macro for defing tags which accept any kind of attribute
and just wrap the body in an xml tag."
  `(deftag ,name (&allow-other-attributes other-attributes &body body)
     (if body
         (progn
           (emit-open-tag ,(string-downcase (string name)) other-attributes)
           (emit-body body)
           (emit-close-tag ,(string-downcase (string name))))
         (emit-empty-tag ,(string-downcase (string name)) other-attributes))))

(defmacro wrap-in-tag ((tag-name &rest tag-attributes) &body body)
  (with-unique-names (tname)
    `(let ((,tname ,(string-downcase (string tag-name))))
       (emit-open-tag ,tname (list ,@(loop
                                        for (k v) on tag-attributes by #'cddr
                                        collect `(cons ,k ,v))))
       (prog1
          (progn ,@body)
         (emit-close-tag ,tname)))))

(defvar *xml-reader-open-char* #\<)
(defvar *xml-reader-close-char* #\>)

(defmacro enable-xml-syntax ()
  "Enable xml reader syntax for the file being compiled or loaded.

You may consider using (enable-bracket-reader):
  {with-xml-syntax
    <foo :attr \"bar\" (lisp) >}

Syntax examples:
  <foo :attribute \"bar\" (call lisp code) >

  <(progn 33) :bar 42 (@ \"cAMeL\" \"eLitE-<>\") \"body-<>\" >
==>
  <33 bar=\"42\" cAMeL=\"eLitE-&lt;&gt;\"
    >body-&lt;&gt;</33
  >

  <\"foo\" :bar 42>
==>
  <foo bar=\"42\"/>"
  '(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-xml-syntax)))

(defun %enable-xml-syntax ()
  (set-macro-character *xml-reader-open-char* #'xml-reader-open nil *readtable*))

(defun xml-reader-open (s char)
  "Emit XML elements into *yaclml-stream*, use keyword parameters
for attributes and rest parameters for nested XML elements or
normal lisp code. See enable-xml-syntax for more details."
  ;;; (attila) this code here makes sure not to use two unread-char calls
  ;;; as it's unspecified by the standard. and therefore the ugliness here...
  ;;; we create a fake package, read the token in there, delete the package
  ;;; and analize what was read.
  (let ((fake-package (make-package (gensym)))
        (symbol nil)
        (symbol-name nil)
        (next-char nil)
        ;; simple-version means that the xml tag name is a symbol, e.g. <foo
        ;; otherwise it's a <"foo" or a <(foo). unless it's a symbol it is
        ;; evaluated and the result is used as the tag name.
        (simple-version t))
    (unwind-protect
         (unread-char char s)                    ; read the entire token
         (setf symbol (with-standard-io-syntax   ; turn ourself off
                          (let ((*package* fake-package))
                            (read s t nil t))))
         (setf symbol-name (string-downcase (symbol-name symbol)))
         (delete-package fake-package))
    ;;(format t "Read in symbol ~S, symbol-package is ~S~%" symbol (symbol-package symbol)) ; TODO debug code
    (setf next-char (peek-char nil s t nil t))
    (if (and (string= symbol-name "<")
             (or (eq next-char #\( )
                 (eq next-char #\" )))
        (setf simple-version nil)
        (when (and (symbol-package symbol)
                   (not (eq (symbol-package symbol)
                            fake-package)))
          ;;(format t "Bailing out with ~S~%" symbol) ; TODO debug code
          (return-from xml-reader-open symbol)))
    ;;(format t "We've got a hit, simple-version? ~S~%" simple-version) ; TODO debug code
    (let ((*readtable* (copy-readtable)))
      (set-syntax-from-char *xml-reader-close-char* #\) *readtable*)
      (labels ((writer (form)
                 (if (stringp form)
                     `(write-string ,form *yaclml-stream*)
                     form))
               (emitter (form)
                 (mapcar #'writer (fold-strings (nreverse form)))))
        (let* ((list (let ((result (read-delimited-list #\> s t)))
                       ;;(format t "The delimited list is ~S~%" result) ; TODO debug code
                       result))
               (head (if simple-version
                         (subseq symbol-name 1) ; drop the "<" from the symbol-name
                         (car list)))
               (tag-name (if (or (stringp head) (consp head))
                             head
                             (string-downcase (princ-to-string head))))
               (%yaclml-code% nil)
               (%yaclml-indentation-depth% 0))
          (attribute-bind
              (&allow-other-attributes other-attributes &body body)
              (if simple-version
                  list
                  (cdr list))
            (let* ((protect nil)
                   (emittable-attributes
                    (iter (for attribute on other-attributes by 'cddr)
                          (if (eq (first attribute) :with-unwind-protect)
                              (setf protect (second attribute))
                              (collect (cons (string-downcase (string (first attribute)))
                                             (second attribute)))))))
              (if body
                  (let* ((open-code)
                         (body-code)
                         (close-code)
                         (rebind-tag-name-p (consp tag-name))
                         (original-tag-name tag-name))
                    (when rebind-tag-name-p
                      (setf tag-name (gensym "TAG-NAME")))
                    (let ((%yaclml-code% '()))
                      (emit-open-tag tag-name emittable-attributes)
                      (setf open-code %yaclml-code%))
                    (let ((%yaclml-code% '()))
                      (emit-body body)
                      (setf body-code %yaclml-code%))
                    (let ((%yaclml-code% '()))
                      (emit-close-tag tag-name)
                      (setf close-code %yaclml-code%))
                    (if protect
                        (if rebind-tag-name-p
                            (emit-code `(let ((,tag-name ,original-tag-name))
                                         ,@(emitter open-code)
                                         (unwind-protect
                                              (progn ,@(emitter body-code))
                                           ,@(emitter close-code))))
                            (emit-code `(progn
                                         ,@(emitter open-code)
                                         (unwind-protect
                                              (progn ,@(emitter body-code))
                                           ,@(emitter close-code)))))
                        (if rebind-tag-name-p
                            (emit-code `(let ((,tag-name ,original-tag-name))
                                         ,@(emitter open-code)
                                         ,@(emitter body-code)
                                         ,@(emitter close-code)))
                            (emit-code `(progn
                                         ,@(emitter open-code)
                                         ,@(emitter body-code)
                                         ,@(emitter close-code))))))
                  (emit-empty-tag tag-name emittable-attributes))))
          `(progn
            ,@(emitter %yaclml-code%)
            (values)))))))

(defun with-xml-syntax ()
  (lambda (handler)
    (%enable-xml-syntax)
    `(progn ,@(funcall handler))))

;; Copyright (c) 2002-2005, Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
