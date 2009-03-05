;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-
;;;
;;; An implementation of James Clark's algorithm for RELAX NG validation.
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

(defvar *empty* (make-empty))
(defvar *not-allowed* (make-not-allowed))

(defmacro ensuref (key table value)
  `(ensure-hash ,key ,table (lambda () ,value)))

(defun ensure-hash (key table fn)
  (or (gethash key table)
      (setf (gethash key table) (funcall fn))))


(defun make-validator (schema &optional handler)
  "@arg[schema]{the parsed Relax NG @class{schema} object}
   @arg[handler]{an additional SAX handler to broadcast events to}
   @return{a SAX handler}
   @short{This function creates a validation handler for @code{schema}},
    to be used for validation of a document against that schema.

   The validation handler processes SAX events and can be used with any
   function generating such events, in particular with cxml:parse-file.

   Events will be passed on unchanged to @code{handler}.

   This validator does @em{not} perform DTD compatibility processing.
   (Specify a DTD compatibility handler as the second argument to this
   function instead.)

   @see{parse-schema}
   @see{make-validating-source}
   @see{make-dtd-compatibility-handler}"
  (let* ((table (ensure-registratur schema))
	 (start (schema-interned-start schema))
	 (validator
	  (make-instance 'validator
			 :registratur table
			 :current-pattern start))
	 (wrapper
	  (make-instance 'text-normalizer :chained-handler validator)))
    (when handler
      (setf wrapper (cxml:make-broadcast-handler wrapper handler)))
    (values wrapper validator)))

(defun make-dtd-compatibility-handler (schema handler)
  "@arg[schema]{the parsed Relax NG @class{schema} object}
   @arg[handler]{an additional SAX handler to broadcast events to}
   @return{a SAX handler}
   @short{This function creates a handler for DTD Compatibility processing}
   according to @code{schema}.

   The validation handler processes SAX events and can be used with any
   function generating such events, in particular with cxml:parse-file.

   Compatibility processing consists of two steps: Infoset modification
   for default values, and soundness checking for attributes with an
   ID-type.

   In @code{sax:start-element}, infoset modification will be performed as
   specified for DTD compatibility.  This entails addition of attributes
   according to their defaultValue, and addition (and, when the element ends,
   removal) of suitable namespace declarations if no prefix has been declared
   for the defaulted attribute yet.

   Also in @code{sax:start-element}, the handler checks that no ID is declared
   more than once.  Before the end of the document, the handler checks that
   all IDs referred to by attributes with ID-types IDREF or IDREFS have been
   declared.

   @see{parse-schema}
   @see{make-validator}"
  (make-instance 'dtd-compatibility-handler
		 :compatibility-table (schema-compatibility-table schema)
		 :handlers (list handler)))


;;;; CONTAINS

(defgeneric contains (nc uri lname))

(defmethod contains ((nc any-name) uri lname)
  (let ((except (any-name-except nc)))
    (if except
        (not (contains except uri lname))
        t)))

(defmethod contains ((nc ns-name) uri lname)
  (and (equal (ns-name-uri nc) uri)
       (let ((except (ns-name-except nc)))
         (if except
             (not (contains except uri lname))
             t))))

(defmethod contains ((nc name) uri lname)
  (and (equal (name-uri nc) uri)
       (equal (name-lname nc) lname)))

(defmethod contains ((nc name-class-choice) uri lname)
  (or (contains (name-class-choice-a nc) uri lname)
      (contains (name-class-choice-b nc) uri lname)))


;;;; COMPUTE-NULLABLE

(defun finalize-pattern (p)
  (setf (pattern-nullable p) (compute-nullable p))
  p)

(defun nullable (pattern)
  (let ((np (pattern-nullable pattern)))
    (check-type np boolean)		;initialized by intern-pattern
    np))

(defgeneric compute-nullable (pattern))

(defmethod compute-nullable ((pattern group))
  (and (nullable (pattern-a pattern))
       (nullable (pattern-b pattern))))

(defmethod compute-nullable  ((pattern interleave))
  (and (nullable (pattern-a pattern))
       (nullable (pattern-b pattern))))

(defmethod compute-nullable ((pattern choice))
  (or (nullable (pattern-a pattern))
      (nullable (pattern-b pattern))))

(defmethod compute-nullable ((pattern one-or-more))
  (nullable (pattern-child pattern)))

(defmethod compute-nullable ((pattern element)) nil)
(defmethod compute-nullable ((pattern attribute)) nil)
(defmethod compute-nullable ((pattern list-pattern)) nil)
(defmethod compute-nullable ((pattern value)) nil)
(defmethod compute-nullable ((pattern data)) nil)
(defmethod compute-nullable ((pattern not-allowed)) nil)
(defmethod compute-nullable ((pattern after)) nil)

(defmethod compute-nullable ((pattern empty)) t)
(defmethod compute-nullable ((pattern text)) t)


;;;; VALIDATOR

(defclass validator (sax:default-handler
		     cxml-types:sax-validation-context-mixin)
  ((current-pattern :initarg :current-pattern :accessor current-pattern)
   (after-start-tag-p :accessor after-start-tag-p)
   (pending-text-node :initform nil :accessor pending-text-node)
   (registratur :initarg :registratur :accessor registratur)
   (validation-error-class :initform 'rng-error
			   :initarg :validation-error-class
			   :accessor validation-error-class)
   (open-start-tag\'-cache :initform (make-hash-table :test 'equal)
			   :reader open-start-tag\'-cache)
   (close-start-tag\'-cache :initform (make-hash-table)
			    :reader close-start-tag\'-cache)
   (end-tag\'-cache :initform (make-hash-table) :reader end-tag\'-cache)
   (non-element\'-cache :initform (make-hash-table)
			:reader non-element\'-cache)
   (mixed-text\'-cache :initform (make-hash-table)
		       :reader mixed-text\'-cache)))

(defun advance (hsx pattern message &rest args)
  (when (typep pattern 'not-allowed)
    (let ((*error-class* (validation-error-class hsx)))
      (rng-error hsx "~?,~%was expecting ~A"
		 message
		 args
		 (replace-scary-characters
		  (with-output-to-string (s)
		    (let ((*print-level* nil))
		      (expectation (current-pattern hsx) s)))))))
  (setf (current-pattern hsx) pattern))

;; make sure slime doesn't die
(defun replace-scary-characters (pattern)
  (let ((str (write-to-string pattern
			      :circle t
			      :escape nil
			      :pretty nil)))
    (loop
       for c across str
       for i from 0
       when (>= (char-code c) 128)
       do (setf (elt str i) #\?))
    str))

(defmethod sax:characters ((hsx validator) data)
  (assert (null (pending-text-node hsx))) ;parser must be normalize
  (if (after-start-tag-p hsx)
      (setf (pending-text-node hsx) data)
      (unless (whitespacep data)
	;; we already saw an element sibling, so discard whitespace
	(advance hsx
		 (mixed-text\' hsx (current-pattern hsx))
		 "text node not valid")))
  (setf (after-start-tag-p hsx) nil))

(defmethod sax:start-element ((hsx validator) uri lname qname attributes)
  (declare (ignore qname))
  (when (pending-text-node hsx)
    ;; text node was the previous child, and we're in element content.
    ;; process non-whitespace now; discard whitespace completely
    (let ((data (pending-text-node hsx)))
      (unless (whitespacep data)
	(advance hsx
		 (mixed-text\' hsx (current-pattern hsx))
		 "text node")))
    (setf (pending-text-node hsx) nil))
  (setf attributes
	(remove-if (cxml::compose #'cxml::xmlns-attr-p #'sax:attribute-qname)
		   attributes))
  (let* ((p0 (current-pattern hsx))
	 (p1 (open-start-tag\' hsx p0 uri lname))
	 (p2 (progn
	       (advance hsx p1 "element ~A (~A) not valid" lname uri)
	       (attributes\' hsx p1 attributes)))
	 (p3 (progn
	       (advance hsx p2 "attributes not valid")
	       (close-start-tag\' hsx p2))))
    (advance hsx p3 "attributes not valid")
    (setf (after-start-tag-p hsx) t)))

(defmethod sax:end-element ((hsx validator) uri lname qname)
  (declare (ignore uri lname qname))
  (when (after-start-tag-p hsx)
    ;; nothing at all?  pretend we saw whitespace.
    (sax:characters hsx ""))
  (when (pending-text-node hsx)
    ;; text node was the only child?
    ;; process it and handle whitespace specially
    (let* ((current (current-pattern hsx))
	   (data (pending-text-node hsx))
	   (next (text-only\' hsx current data)))
      (advance hsx
	       (if (whitespacep data)
		   (intern-choice hsx current next)
		   next)
	       "text node not valid"))
    (setf (pending-text-node hsx) nil))
  (advance hsx
	   (end-tag\' hsx (current-pattern hsx))
	   "end of element not valid"))

(defun eat (ok)
  (if ok *empty* *not-allowed*))


;;;; TEXT-ONLY' / NON-ELEMENT'

(defun text-only\' (handler pattern data)
  (data\' handler
	  (non-element\' handler pattern)
	  data))

(defgeneric non-element\' (handler pattern))

(defmethod non-element\' :around (hsx (pattern pattern))
  (ensuref pattern (non-element\'-cache hsx) (call-next-method)))

(defmethod non-element\' (hsx (pattern choice))
  (intern-choice hsx
		 (non-element\' hsx (pattern-a pattern))
                 (non-element\' hsx (pattern-b pattern))))

(defmethod non-element\' (hsx (pattern interleave))
  (let ((a (pattern-a pattern))
        (b (pattern-b pattern)))
    (intern-choice hsx
                   (intern-interleave hsx (non-element\' hsx a) b)
                   (intern-interleave hsx a (non-element\' hsx b)))))

(defmethod non-element\' (hsx (pattern group))
  (let* ((a (pattern-a pattern))
         (b (pattern-b pattern))
         (p (intern-group hsx (non-element\' hsx a) b)))
    (if (nullable a)
        (intern-choice hsx p (non-element\' hsx b))
        p)))

(defmethod non-element\' (hsx (pattern after))
  (intern-after hsx
                (non-element\' hsx (pattern-a pattern))
                (pattern-b pattern)))

(defmethod non-element\' (hsx (pattern one-or-more))
  (let ((child (pattern-child pattern)))
    (intern-group hsx
                  (non-element\' hsx child)
		  (intern-zero-or-more hsx child))))

(defmethod non-element\' (hsx (pattern element))
  *not-allowed*)

(defmethod non-element\' (hsx pattern)
  pattern)


;;;; DATA'

(defgeneric data\' (handler pattern data))

(defmethod data\' (hsx (pattern choice) data)
  (intern-choice hsx
		 (data\' hsx (pattern-a pattern) data)
                 (data\' hsx (pattern-b pattern) data)))

(defmethod data\' (hsx (pattern interleave) data)
  (let ((a (pattern-a pattern))
        (b (pattern-b pattern)))
    (intern-choice hsx
                   (intern-interleave hsx (data\' hsx a data) b)
                   (intern-interleave hsx a (data\' hsx b data)))))

(defmethod data\' (hsx (pattern group) data)
  (let* ((a (pattern-a pattern))
         (b (pattern-b pattern))
         (p (intern-group hsx (data\' hsx a data) b)))
    (if (nullable a)
        (intern-choice hsx p (data\' hsx b data))
        p)))

(defmethod data\' (hsx (pattern after) data)
  (intern-after hsx
                (data\' hsx (pattern-a pattern) data)
                (pattern-b pattern)))

(defmethod data\' (hsx (pattern one-or-more) data)
  (let ((child (pattern-child pattern)))
    (intern-group hsx
                  (data\' hsx child data)
		  (intern-zero-or-more hsx child))))

(defmethod data\' (hsx (pattern text) data)
  (declare (ignore data))
  pattern)

(defmethod data\' (hsx (pattern value) data)
  (let ((data-type (pattern-type pattern)))
    (eat (cxml-types:equal-using-type
	  data-type
	  (pattern-value pattern)
	  (cxml-types:parse data-type data hsx)))))

(defmethod data\' (hsx (pattern data) data)
  (eat (and (cxml-types:validp (pattern-type pattern) data hsx)
	    (let ((except (pattern-except pattern)))
	      (not (and except (nullable (data\' hsx except data))))))))

(defmethod data\' (hsx (pattern list-pattern) data)
  (eat (nullable (list\' hsx (pattern-child pattern) (words data)))))

(defmethod data\' (hsx pattern data)
  (declare (ignore pattern data))
  *not-allowed*)

(defun list\' (hsx pattern words)
  (dolist (word words)
    (setf pattern (data\' hsx pattern word)))
  pattern)

(defun words (str)
  (cl-ppcre:split #.(format nil "[~A]+" *whitespace*)
		  (string-trim *whitespace* str)))


;;;; MIXED-TEXT'

(defgeneric mixed-text\' (handler pattern))

(defmethod mixed-text\' :around (hsx (pattern pattern))
  (ensuref pattern (mixed-text\'-cache hsx) (call-next-method)))

(defmethod mixed-text\' (hsx (pattern choice))
  (intern-choice hsx
		 (mixed-text\' hsx (pattern-a pattern))
                 (mixed-text\' hsx (pattern-b pattern))))

(defmethod mixed-text\' (hsx (pattern interleave))
  (let ((a (pattern-a pattern))
        (b (pattern-b pattern)))
    (intern-choice hsx
                   (intern-interleave hsx (mixed-text\' hsx a) b)
                   (intern-interleave hsx a (mixed-text\' hsx b)))))

(defmethod mixed-text\' (hsx (pattern group))
  (let* ((a (pattern-a pattern))
         (b (pattern-b pattern))
         (p (intern-group hsx (mixed-text\' hsx a) b)))
    (if (nullable a)
        (intern-choice hsx p (mixed-text\' hsx b))
        p)))

(defmethod mixed-text\' (hsx (pattern after))
  (intern-after hsx
                (mixed-text\' hsx (pattern-a pattern))
                (pattern-b pattern)))

(defmethod mixed-text\' (hsx (pattern one-or-more))
  (let ((child (pattern-child pattern)))
    (intern-group hsx
                  (mixed-text\' hsx child)
		  (intern-zero-or-more hsx child))))

(defmethod mixed-text\' (hsx (pattern text))
  pattern)

(defmethod mixed-text\' (hsx pattern)
  (declare (ignore pattern))
  *not-allowed*)


;;;; INTERN

(defgeneric intern-choice (handler a b))
(defmethod intern-choice (hsx a (b not-allowed)) a)
(defmethod intern-choice (hsx (a not-allowed) b) b)
(defmethod intern-choice (hsx a b)
  (ensuref (list 'choice a b)
	   (registratur hsx)
	   (let ((table (make-hash-table)))
	     (labels ((record (p)
			(cond
			  ((typep p 'choice)
			   (record (pattern-a p))
			   (record (pattern-b p)))
			  (t
			   (setf (gethash p table) t)))))
	       (record a))
	     (labels ((eliminate (p)
			(cond
			  ((typep p 'choice)
			   (intern-choice hsx
					  (eliminate (pattern-a p))
					  (eliminate (pattern-b p))))
			  ((gethash p table)
			   *not-allowed*)
			  (t
			   p))))
	       (let ((x (eliminate b)))
		 (if (typep x 'not-allowed)
		     a
		     (finalize-pattern (make-choice a x))))))))

(defgeneric intern-group (handler a b))
(defmethod intern-group (hsx (a pattern) (b not-allowed)) b)
(defmethod intern-group (hsx (a not-allowed) (b pattern)) a)
(defmethod intern-group (hsx a (b empty)) a)
(defmethod intern-group (hsx (a empty) b) b)
(defmethod intern-group (hsx a b)
  (ensuref (list 'group a b)
	   (registratur hsx)
	   (finalize-pattern (make-group a b))))

(defgeneric intern-interleave (handler a b))
(defmethod intern-interleave (hsx (a pattern) (b not-allowed)) b)
(defmethod intern-interleave (hsx (a not-allowed) (b pattern)) a)
(defmethod intern-interleave (hsx a (b empty)) a)
(defmethod intern-interleave (hsx (a empty) b) b)
(defmethod intern-interleave (hsx a b)
  (ensuref (list 'interleave a b)
	   (registratur hsx)
	   (finalize-pattern (make-interleave a b))))

(defgeneric intern-after (handler a b))
(defmethod intern-after (hsx (a pattern) (b not-allowed)) b)
(defmethod intern-after (hsx (a not-allowed) (b pattern)) a)
(defmethod intern-after (hsx a b)
  (ensuref (list 'after a b)
	   (registratur hsx)
	   (finalize-pattern (make-after a b))))

(defgeneric intern-one-or-more (handler c))
(defmethod intern-one-or-more (hsx (c not-allowed)) c)
(defmethod intern-one-or-more (hsx c)
  (ensuref (list 'one-or-more c)
	   (registratur hsx)
	   (finalize-pattern (make-one-or-more c))))


;;;; ENSURE-REGISTRATUR

(defvar *seen-elements*)

(defun ensure-registratur (grammar)
  (or (schema-registratur grammar)
      (setf (schema-registratur grammar)
	    (let ((table (make-hash-table :test 'equal))
		  (*seen-elements* '())
		  (done-elements '()))
	      (setf (schema-interned-start grammar)
		    (intern-pattern (schema-start grammar) table))
	      (loop
		 for elements = *seen-elements*
		 while elements do
		   (setf *seen-elements* nil)
		   (dolist (pattern elements)
		     (unless (find pattern done-elements)
		       (push pattern done-elements)
		       (setf (pattern-child pattern)
			     (intern-pattern (pattern-child pattern) table)))))
	      table))))

;;; FIXME: misnamed.  we don't really intern the originals pattern yet.

(defgeneric intern-pattern (pattern table))

(defmethod intern-pattern ((pattern element) table)
  (let ((copy (ensuref (list 'element pattern)
		       table
		       (copy-structure pattern))))
    (pushnew copy *seen-elements*)
    copy))

(defmethod intern-pattern :around ((pattern pattern) table)
  (finalize-pattern (call-next-method)))

(defmethod intern-pattern ((pattern %parent) table)
  (let ((c (intern-pattern (pattern-child pattern) table)))
    (if (eq c (pattern-child pattern))
	pattern
	(let ((copy (copy-structure pattern)))
	  (setf (pattern-child copy) c)
	  copy))))

(defmethod intern-pattern ((pattern %combination) table)
  (let ((a (intern-pattern (pattern-a pattern) table))
	(b (intern-pattern (pattern-b pattern) table)))
    (if (and (eq a (pattern-a pattern)) (eq b (pattern-b pattern)))
	pattern
	(let ((copy (copy-structure pattern)))
	  (setf (pattern-a copy) a)
	  (setf (pattern-b copy) b)
	  copy))))

(defmethod intern-pattern ((pattern data) table)
  (let ((e (when (pattern-except pattern)
	     (intern-pattern (pattern-except pattern) table))))
    (if (eq e (pattern-except pattern))
	pattern
	(let ((copy (copy-structure pattern)))
	  (setf (pattern-except copy) e)
	  copy))))

(defmethod intern-pattern ((pattern ref) table)
  (intern-pattern (defn-child (pattern-target pattern)) table))

(defmethod intern-pattern ((pattern empty) table)
  *empty*)

(defmethod intern-pattern ((pattern not-allowed) table)
  *not-allowed*)

(defmethod intern-pattern ((pattern %leaf) table)
  pattern)


;;;; APPLY-AFTER

(defgeneric apply-after (handler fn pattern))

(defmethod apply-after (hsx fn (pattern after))
  (intern-after hsx
                (pattern-a pattern)
                (funcall fn (pattern-b pattern))))

(defmethod apply-after (hsx fn (pattern choice))
  (intern-choice hsx
                 (apply-after hsx fn (pattern-a pattern))
                 (apply-after hsx fn (pattern-b pattern))))

(defmethod apply-after (hsx fn (pattern not-allowed))
  (declare (ignore hsx fn))
  pattern)


;;;; OPEN-START-TAG'

(defgeneric open-start-tag\' (handler pattern uri lname))

(defmethod open-start-tag\' :around (hsx (pattern pattern) uri lname)
  (ensuref (list pattern uri lname)
	   (open-start-tag\'-cache hsx)
	   (call-next-method)))

(defmethod open-start-tag\' (hsx (pattern choice) uri lname)
  (intern-choice hsx
                 (open-start-tag\' hsx (pattern-a pattern) uri lname)
                 (open-start-tag\' hsx (pattern-b pattern) uri lname)))

(defmethod open-start-tag\' (hsx (pattern element) uri lname)
  (if (contains (pattern-name pattern) (or uri "") lname)
      (intern-after hsx (pattern-child pattern) *empty*)
      *not-allowed*))

(defmethod open-start-tag\' (hsx (pattern interleave) uri lname)
  (intern-choice hsx
                 (apply-after
                  hsx
                  (lambda (p) (intern-interleave hsx p (pattern-b pattern)))
                  (open-start-tag\' hsx (pattern-a pattern) uri lname))
                 (apply-after
                  hsx
                  (lambda (p) (intern-interleave hsx (pattern-a pattern) p))
                  (open-start-tag\' hsx (pattern-b pattern) uri lname))))

(defun intern-zero-or-more (hsx c)
  (intern-choice hsx (intern-one-or-more hsx c) *empty*))

(defmethod open-start-tag\' (hsx (pattern one-or-more) uri lname)
  (let ((c (intern-zero-or-more hsx (pattern-child pattern))))
    (apply-after hsx
                 (lambda (p) (intern-group hsx p c))
                 (open-start-tag\' hsx (pattern-child pattern) uri lname))))

(defmethod open-start-tag\' (hsx (pattern group) uri lname)
  (let ((x (apply-after hsx
                        (lambda (p)
                          (intern-group hsx p (pattern-b pattern)))
                        (open-start-tag\' hsx (pattern-a pattern) uri lname))))
    (if (nullable (pattern-a pattern))
        (intern-choice hsx
                       x
                       (open-start-tag\' hsx (pattern-b pattern) uri lname))
        x)))

(defmethod open-start-tag\' (hsx (pattern after) uri lname)
  (apply-after hsx
	       (lambda (p)
		 (intern-after hsx p (pattern-b pattern)))
	       (open-start-tag\' hsx (pattern-a pattern) uri lname)))

(defmethod open-start-tag\' (hsx pattern uri lname)
  (declare (ignore hsx pattern uri lname))
  *not-allowed*)


;;;; ATTRIBUTES'

(defun attributes\' (handler pattern attributes)
  (dolist (a attributes)
    (setf pattern (attribute\' handler pattern a))
    (advance handler pattern "attribute not valid: ~A" a))
  pattern)

(defgeneric attribute\' (handler pattern attribute))

(defmethod attribute\' (hsx (pattern after) a)
  (intern-after hsx
		(attribute\' hsx (pattern-a pattern) a)
		(pattern-b pattern)))

(defmethod attribute\' (hsx (pattern choice) a)
  (intern-choice hsx
		 (attribute\' hsx (pattern-a pattern) a)
		 (attribute\' hsx (pattern-b pattern) a)))

(defmethod attribute\' (hsx (pattern group) a)
  (intern-choice hsx
		 (intern-group hsx
			       (attribute\' hsx (pattern-a pattern) a)
			       (pattern-b pattern))
		 (intern-group hsx
			       (pattern-a pattern)
			       (attribute\' hsx (pattern-b pattern) a))))

(defmethod attribute\' (hsx (pattern interleave) a)
  (intern-choice hsx
		 (intern-interleave hsx
				    (attribute\' hsx (pattern-a pattern) a)
				    (pattern-b pattern))
		 (intern-interleave hsx
				    (pattern-a pattern)
				    (attribute\' hsx (pattern-b pattern) a))))

(defmethod attribute\' (hsx (pattern one-or-more) a)
  (intern-group hsx
		(attribute\' hsx (pattern-child pattern) a)
		(intern-zero-or-more hsx (pattern-child pattern))))

(defmethod attribute\' (hsx (pattern attribute) a)
  (eat (and (contains (pattern-name pattern)
		      (or (sax:attribute-namespace-uri a) "")
		      (sax:attribute-local-name a))
	    (value-matches-p hsx
			     (pattern-child pattern)
			     (sax:attribute-value a)))))

(defun value-matches-p (hsx pattern value)
  (or (and (nullable pattern) (whitespacep value))
      (nullable (text-only\' hsx pattern value))))

(defun whitespacep (str)
  (zerop (length (string-trim *whitespace* str))))

(defmethod attribute\' (hsx pattern a)
  (declare (ignore hsx pattern a))
  *not-allowed*)


;;;; CLOSE-START-TAG'

(defgeneric close-start-tag\' (handler pattern))

(defmethod close-start-tag\' :around (hsx (pattern pattern))
  (ensuref pattern (close-start-tag\'-cache hsx) (call-next-method)))

(defmethod close-start-tag\' (hsx (pattern after))
  (intern-after hsx
		(close-start-tag\' hsx (pattern-a pattern))
		(pattern-b pattern)))

(defmethod close-start-tag\' (hsx (pattern choice))
  (intern-choice hsx
		(close-start-tag\' hsx (pattern-a pattern))
		(close-start-tag\' hsx (pattern-b pattern))))

(defmethod close-start-tag\' (hsx (pattern group))
  (intern-group hsx
		(close-start-tag\' hsx (pattern-a pattern))
		(close-start-tag\' hsx (pattern-b pattern))))

(defmethod close-start-tag\' (hsx (pattern interleave))
  (intern-interleave hsx
		     (close-start-tag\' hsx (pattern-a pattern))
		     (close-start-tag\' hsx (pattern-b pattern))))

(defmethod close-start-tag\' (hsx (pattern one-or-more))
  (intern-one-or-more hsx (close-start-tag\' hsx (pattern-child pattern))))

(defmethod close-start-tag\' (hsx (pattern attribute))
  (declare (ignore hsx))
  *not-allowed*)

(defmethod close-start-tag\' (hsx pattern)
  (declare (ignore hsx))
  pattern)


;;;; END-TAG\'

(defgeneric end-tag\' (handler pattern))

(defmethod end-tag\' :around (hsx (pattern pattern))
  (ensuref pattern (end-tag\'-cache hsx) (call-next-method)))

(defmethod end-tag\' (hsx (pattern choice))
  (intern-choice hsx
		 (end-tag\' hsx (pattern-a pattern))
		 (end-tag\' hsx (pattern-b pattern))))

(defmethod end-tag\' (hsx (pattern after))
  (if (nullable (pattern-a pattern))
      (pattern-b pattern)
      *not-allowed*))

(defmethod end-tag\' (hsx pattern)
  (declare (ignore hsx pattern))
  *not-allowed*)


;;;; TEXT NORMALIZER

;;; FIXME: cxml should do that

;;; FIXME: since we ignore PI, CDATA, and comment events, we should probably
;;; discard them properly.

(defclass text-normalizer (cxml:sax-proxy)
  ((pending-text-node :initform (make-string-output-stream)
		      :accessor pending-text-node)))

(defmethod sax:characters ((handler text-normalizer) data)
  (write-string data (pending-text-node handler)))

(defun flush-pending (handler)
  (let ((str (get-output-stream-string (pending-text-node handler))))
    (unless (zerop (length str))
      (sax:characters (cxml:proxy-chained-handler handler) str))))

(defmethod sax:start-element :before
    ((handler text-normalizer) uri lname qname attributes)
  (declare (ignore uri lname qname attributes))
  (flush-pending handler))

(defmethod sax:end-element :before
    ((handler text-normalizer) uri lname qname)
  (declare (ignore uri lname qname))
  (flush-pending handler))


;;;; EXPECTATION, DESCRIBE-NAME

(defgeneric expectation (pattern stream))
(defgeneric describe-name (name-class stream))

(defmethod expectation ((pattern after) s)
  (expectation (pattern-a pattern) s))

(defmethod expectation ((pattern group) s)
  (cond
    ;; zzz: for better error messages with attributes we should probably
    ;; have a separate attribute-expectation function
    ((typep (pattern-a pattern) 'attribute)
     (pprint-logical-block (s nil)
       (expectation (pattern-a pattern) s)
       (when (typep (pattern-a pattern) 'attribute)
	 (format s "~:@_and ")
	 (expectation (pattern-b pattern) s))))
    (t
     (expectation (pattern-a pattern) s))))

(defmethod expectation ((pattern attribute) s)
  (pprint-logical-block (s nil)
    (write-string "an attribute " s)
    (describe-name (pattern-name pattern) s)
    (format s "~:@_with a value of ")
    (expectation (pattern-child pattern) s)))

(defmethod expectation ((pattern choice) s)
  (pprint-logical-block (s nil)
    (expectation (pattern-a pattern) s)
    (format s "~:@_or ")
    (expectation (pattern-b pattern) s)))

(defmethod expectation ((pattern element) s)
  (pprint-logical-block (s nil)
    (write-string "an element " s)
    (describe-name (pattern-name pattern) s)))

(defmethod expectation ((pattern data) s)
  (format s "a text node of type ~A" (pattern-type pattern)))

(defmethod expectation ((pattern interleave) s)
  (pprint-logical-block (s nil)
    (expectation (pattern-a pattern) s)
    (format s "~:@_interleaved with ")
    (expectation (pattern-b pattern) s)))

(defmethod expectation ((pattern list-pattern) s)
  (pprint-logical-block (s nil)
    (format s "a whitespace separated list of:~:@_")
    (expectation (pattern-child pattern) s)))

(defmethod expectation ((pattern not-allowed) s)
  (write-string "nothing" s))

(defmethod expectation ((pattern one-or-more) s)
  (pprint-logical-block (s nil)
    (format s "one or more of:~:@_")
    (expectation (pattern-child pattern) s)))

(defmethod expectation ((pattern text) s)
  (write-string "whitespace" s))

(defmethod expectation ((pattern value) s)
  (format s "a text node of type ~A and value ~S"
	  (pattern-type pattern)
	  (pattern-value pattern)))

(defmethod expectation ((pattern empty) s)
  (write-string "nothing more" s))

(defmethod describe-name ((nc name) s)
  (format s "named ~S, in the namespace ~S"
	  (name-lname nc)
	  (name-uri nc)))

(defmethod describe-name ((nc any-name) s)
  (pprint-logical-block (s nil)
    (write-string "of any name" s)
    (when (any-name-except nc)
      (format s "~:@_except ")
      (describe-name (any-name-except nc) s))))

(defmethod describe-name ((nc ns-name) s)
  (pprint-logical-block (s nil)
    (format s "with a name in the namespace ~S" (ns-name-uri nc))
    (when (ns-name-except nc)
      (format s "~:@_except for ")
      (describe-name (ns-name-except nc) s))))

(defmethod describe-name ((nc name-class-choice) s)
  (pprint-logical-block (s nil)
    (describe-name (name-class-choice-a nc) s)
    (format s "~:@_or ")
    (describe-name (name-class-choice-b nc) s)))


;;;; DTD-COMPATIBILITY-HANDLER

(defclass dtd-compatibility-handler
    (cxml:broadcast-handler cxml-types:sax-validation-context-mixin)
    ((compatibility-table :initarg :compatibility-table
			  :accessor compatibility-table)
     (extra-namespaces :initform nil :accessor extra-namespaces)
     (seen-ids :initform (make-hash-table :test 'equal) :accessor seen-ids)
     (seen-idrefs :initform nil :accessor seen-idrefs)))

(defmethod sax:start-element
    ((hsx dtd-compatibility-handler) uri lname qname attributes)
  (declare (ignore qname))
  (push nil (extra-namespaces hsx))
  (let ((dtd-element
	 (gethash (list (or uri "") lname)
		  (dtd-elements
		   (compatibility-table hsx))))
	(*error-class* 'dtd-compatibility-error))
    (when dtd-element
      (loop for a being each hash-value in (dtd-attributes dtd-element) do
	   (setf attributes (process-dtd-attribute hsx a attributes)))))
  (call-next-method hsx uri lname qname attributes))

(defmethod sax:end-element :before
    ((hsx dtd-compatibility-handler) uri lname qname)
  (declare (ignore uri lname qname))
  (dolist (c (pop (extra-namespaces hsx)))
    (dolist (next (cxml:broadcast-handler-handlers hsx))
      (sax:end-prefix-mapping next (car c)))))

(defmethod sax:end-document :before ((hsx dtd-compatibility-handler))
  (let ((*error-class* 'dtd-compatibility-error))
    (dolist (id (seen-idrefs hsx))
      (unless (gethash id (seen-ids hsx))
	(rng-error nil "referenced ID ~A not defined" id)))))

(defun process-dtd-attribute (hsx a attributes)
  (let* ((uri (name-uri (dtd-name a)))
	 (lname (name-lname (dtd-name a)))
	 (b (find-if (lambda (b)
		       (and (equal (or (sax:attribute-namespace-uri b) "") uri)
			    (equal (sax:attribute-local-name b) lname)))
		     attributes)))
    (cond
      (b
       (let ((ids (cl-ppcre:split #.(format nil "[~A]+" *whitespace*)
				  (sax:attribute-value b))))
	 (when ids
	   (case (dtd-id-type a)
	     ((nil))
	     (:id
	      (when (cdr ids)
		(rng-error hsx "more than one token in ID: ~A" ids))
	      (let ((id (car ids)))
		(when (gethash id (seen-ids hsx))
		  (rng-error hsx "multiple declarations for ID: ~A" id))
		(setf (gethash id (seen-ids hsx)) t)))
	     ((:idref :idrefs)
	      (setf (seen-idrefs hsx) (append ids (seen-idrefs hsx))))))))
      (t
       (when (dtd-default-value a)
	 (let ((prefix
		(flet ((uri-to-prefix (stack)
			 (car (find uri stack :key #'cdr :test #'equal))))
		  (or (uri-to-prefix (cxml-types::context-stack hsx))
		      (some #'uri-to-prefix (extra-namespaces hsx))))))
	   (unless prefix
	     (setf prefix
		   (loop
		      for i from 0
		      for name = (format nil "ns-~D" i)
		      while (find name
				  (cxml-types::context-stack hsx)
				  :key #'car
				  :test #'equal)
		      finally (return name)))
	     (when sax:*include-xmlns-attributes*
	       (push (sax:make-attribute
		      :namespace-uri "http://www.w3.org/2000/xmlns/"
		      :local-name prefix
		      :qname (format nil "xmlns:~A" prefix)
		      :value uri
		      :specified-p nil)
		     attributes))
	     (push (cons prefix uri) (car (extra-namespaces hsx)))
	     (dolist (next (cxml:broadcast-handler-handlers hsx))
	       (sax:start-prefix-mapping next prefix uri)))
	   (push (sax:make-attribute :namespace-uri uri
				     :local-name lname
				     :qname (format nil "~A:~A" prefix lname)
				     :value (dtd-default-value a)
				     :specified-p nil)
		 attributes))))))
  attributes)


;;;;

(finalize-pattern *empty*)
(finalize-pattern *not-allowed*)
