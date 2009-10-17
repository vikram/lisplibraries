;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.parser)

;;;; Generic list/symbol pattern matching
;;;; (Maybe use CL-Unification at some point)

(defun wildcard-sym-p (x)
  (and (symbolp x)
       (let ((name (symbol-name x)))
         (and (> (length name) 0)
              (char= (aref name 0) #\?)))))

(defun template-wildcards (template)
  (let (res)
    (labels ((collect (x)
               (etypecase x
                 (symbol (when (wildcard-sym-p x) (push x res)))
                 (list (dolist (xi x) (collect xi))))))
      (collect template)
      (sort (remove-duplicates res) #'string<))))

(defun match-p (form template &key verbose (check-consistency t))
  "Pattern matcher for lists/symbols, to detect AST patterns.
FORM and TEMPLATE must both be trees. Symbols starting with #\?
in TEMPLATE are wildcards, bound to list or symbol of FORM.
Returns MATCH-P, BINDINGS; the latter is alist of wildcard-form pairs."
  (when check-consistency
    (check-consistent-template template))
  (let (bindings)
    (labels ((match-forms (form template)
               (cond ((wildcard-sym-p template) ;; wildcards symbol
                      (push (cons template form) bindings))
                     ((and (listp form) (listp template) ;; two lists
                           (= (length form) (length template)))
                      (loop for fi in form for ti in template
                          do (match-forms fi ti)))
                     ((and (symbolp form) ;; two symbols
                           (symbolp template)
                           (eq form template)))
                     (t
                      (when verbose (warn "Mismatch: ~A != ~A" form template))
                      (return-from match-p nil)))))

      (match-forms form template)
      
      (loop for sublist on bindings
          for (key . val) = (car sublist)
          for next-entry = (find key (cdr sublist) :key #'car)
          when next-entry
          unless (equal val (cdr next-entry))
          do (when verbose (warn "Wildcard mismatch for ~A: ~A != ~A"
                                 key val (cdr next-entry)))
             (return-from match-p nil))
      
      (values t bindings))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *inside-match-check-template* nil))

(define-compiler-macro match-p (&whole whole form template
                                &key verbose (check-consistency t))
  (declare (ignore form verbose))
  (when (and check-consistency
             (not *inside-match-check-template*)
             (listp template)
             (eq (car template) 'quote))
    (check-consistent-template (second template)))
  whole)

(defun check-consistent-template (template)
  (unless *inside-match-check-template*
    (let ((*inside-match-check-template* t))
      (let ((x (car template)))
        (when (and (symbolp x)
                   (eq x (find-symbol (symbol-name x) :clpython.ast)))
          (whereas ((ast-pattern (clpython.parser::get-ast-pattern x)))
            (unless (match-p template ast-pattern)
              (warn "Attempt to use template of the form ~A. This template ~
differs in structure from the template for ~A ast nodes, which is: ~A"
                    template x ast-pattern))))))))

(defmacro with-matching ((form template &key (must-hold t) (check-consistency t))
                         &body body)
  (when check-consistency
    (check-consistent-template template))
  (let ((wildcards (template-wildcards template)))
    `(multiple-value-bind (.match-p .bindings)
         (match-p ,form ',template :check-consistency nil)
       ,@(unless wildcards
           `((declare (ignore .bindings))))
       ,@(when must-hold
           `((assert .match-p () 
               "Form does not match required pattern: ~S != ~S" ,form ',template)))
       (when .match-p
         (let ,(loop for wc in wildcards
                   collect `(,wc (cdr (assoc ',wc .bindings))))
           (declare (ignorable ,@wildcards))
           ,@body)))))

(defmacro with-perhaps-matching ((form template &rest args) &body body)
  `(with-matching (,form ,template :must-hold nil ,@args)
     ,@body))


