;; -*- lisp -*-

(in-package :it.bese.yaclml)

;;;; * ATTRIBUTE-BIND

;;;; This macro serves the same purpose as destructuring-bind but
;;;; allows for a different syntax.

(defmacro attribute-bind (attribute-spec attribute-values &body body)
  "Evaluate BODY with the values in LIST bound according to ATTRIBUTE-SPEC.

ATTRIBUTE-SPEC has the following form:

 ( required-args* [ &attribute attributes* ] 
                  [ &allow-other-attributes others ]
                  [ &body body ] )

The symbols in REQUIRED-ARGS will be positionaly bound to the
values in LIST. After the required args have been consumed any
keyword value pairs will be consumed and bound to the
corresponding attributes (binding form is just like &key in
regular lambda lists, but only keyword symbols are allowed).

If &allow-other-attributes is present than OTHERS will be bound
to a list containing all the attributes in LIST which don't have
a corresponding &attribute variable.

if &body is present then BODY will be bound to anything remaining
in LIST after attribute parsing is complete."
  (destructuring-bind (locals attrs flags allow-other-attributes body-var)
      (parse-attribute-spec attribute-spec)
    (with-unique-names (element)
      (rebinding (attribute-values)
        `(let ,(append (remove-if #'null (append locals
                                                 attrs
                                                 flags
                                                 (list allow-other-attributes)
                                                 (list body-var)))
                       (list '%yaclml-custom-attributes%))
           (declare (ignorable %yaclml-custom-attributes%))
           ,(when body-var
              `(declare (ignorable ,body-var)))
           ,@(loop
                for local in locals
                collect `(setf ,local (pop ,attribute-values)))
           (setf ,attribute-values (iter (for el in ,attribute-values)
                                         (if (and (listp el)
                                                  (symbolp (first el))
                                                  (string= "@" (symbol-name (first el))))
                                             (push (rest el) %yaclml-custom-attributes%)
                                             (collect el))))           
           (iterate
            (while (and (consp ,attribute-values)
                        (keywordp (car ,attribute-values))))
            (let ((,element (pop ,attribute-values)))
              (case ,element
                ,@(loop
                     for attr in attrs
                     ;; NB: ATTR is (symbol-to-bind-to default-value),
                     ;; we want to match against the keyword whose
                     ;; string name is (symbol-name symbol-to-bind-to),
                     ;; hence the intern.
                     collect `(,(intern (string (car attr)) :keyword) (setf ,(car attr) (pop ,attribute-values))))
                ,@(loop
                     for flag in flags
                     collect `(,(intern (string flag) :keyword) (setf ,flag t)))
                (t
                 ,(if allow-other-attributes
                      `(progn
                         (push ,element ,allow-other-attributes)
                         (push (pop ,attribute-values) ,allow-other-attributes))
                      `(error 'unrecognized-attribute :attribute ,element))))))
           ,(when (null body-var)
              `(when ,attribute-values
                (warn "Ignoring extra elements in body: ~S" ,attribute-values)))
           ,(when body-var
              `(setf ,body-var ,attribute-values))
           ,(when allow-other-attributes
              `(setf ,allow-other-attributes (nreverse ,allow-other-attributes)))
           ,@(if (and (consp body)
                      (consp (car body))
                      (eql 'declare (car (car body))))
                 `((locally ,@body))
                 body))))))

(define-condition unrecognized-attribute (error)
  ((attribute :accessor attribute :initarg :attribute)
   (tag :accessor tag :initarg :tag :initform nil))
  (:report (lambda (c s)
             (if (tag c)
                 (format s "Unrecognized attribute ~S." (attribute c))
                 (format s "Unrecognized attribute ~S in ~S." (attribute c) (tag c))))))

(defun parse-attribute-spec (attribute-spec)
  "Parse an attribute spec into required args, attribute args,
  other args and the body arg."
  (let* ((required '())
         (attrs '())
         (flags '())
         (body-var nil)
         (allow-other-attributes nil)
         (put (lambda (item)
                (push item required))))
    (dolist (attr attribute-spec)
      ;; the #'string= tom-follery (god i love that word) is so that
      ;; the & symbols can be read in from any package. we're kinda
      ;; faking keywords...
      (if (symbolp attr)
	  (cond
	    ((string= attr '&attribute)
	     (setf put (lambda (item)
			 (if (listp item)
			     (case (length item)
			       (1 (push (list (first item) nil) attrs))
			       (2 (push item attrs))
			       (t (error "Bad &attribute spec: ~S" item)))
			     (push (list item nil) attrs)))))
	    ((string= attr '&flag)
	     (setf put (lambda (item)
			 (push item flags))))
	    ((string= attr '&body)
	     (setf put (lambda (item)
			 (setf body-var item))))
	    ((string= attr '&allow-other-attributes)
	     (setf put (lambda (item)
			 (setf allow-other-attributes item))))
	    (t (funcall put attr)))
	  (funcall put attr)))
    (list (nreverse required) (nreverse attrs) (nreverse flags) allow-other-attributes body-var)))

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
