;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; * Macro helpers

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym ,(mkstr s '-))))
    ,@body))

(defmacro with-unique-names (names &body body)
  `(with-gensyms ,names ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for name in names collect (gensym (mkstr name '-)))))
    ``(let (,,(mapcar (lambda (gensym name) ``(,',gensym ,,name))
		      gensyms names))
       ,(let ,(mapcar (lambda (name gensym) `(,name ',gensym))
		      names gensyms)
	     ,@body))))
