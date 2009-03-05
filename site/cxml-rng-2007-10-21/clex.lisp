;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLEX; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: A flex like scanner generator for Common LISP
;;;   Created: 1997-10-12
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1997-1999 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;

;;; Changes

;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  2007-04-29  DFL     - Represent RANGE directly to cope with character
;;                        set sizes typical for Unicode.
;;                      - Disable *full-table-p* by default.
;;                      - Added SBCL case to the CMUCL workarounds.

(defpackage :cxml-clex
  (:use :cl :runes)
  (:export
   #:deflexer #:backup #:begin #:initial #:bag))

(in-package :cxml-clex)

;;; NOTE -- It turns out that this code is a magintude slower under CMUCL
;;; compared to CLISP or ACL. Probably they do not have a good implementation of
;;; bit vectors.

;;; We encode our FSA's directly as linked datastructures; A state is represented by:

(defstruct (state (:type vector))
  (final 0)
  transitions                           ;simple alist of (sigma . next-state)
  id                                    ;numeric id of state
  eps-transitions)                      ;list of all states reached by epsilon (empty transitions)

(defun destructure-range (x)
  (if (listp x)
      (values (car x) (cadr x))
      (values x x)))

(defun range- (a b)
  (multiple-value-bind (amin amax) (destructure-range a)
    (multiple-value-bind (bmin bmax) (destructure-range b)
      (incf amax)
      (incf bmax)
      (let ((result nil))
	(flet ((range* (min max)
		 (when (< min max)
		   (push (list min (1- max)) result))))
	  (range* amin (min bmin amax))
	  (range* (max amin bmax) amax))
	result))))

(defun ranges-range (aa b)
  (mapcan (lambda (a) (range- a b)) aa))

(defun ranges- (aa b)
  (dolist (l b)
    (setf aa (ranges-range aa l)))
  aa)

(defun partition-range (a pos)
  (multiple-value-bind (min max) (destructure-range a)
    (if (and (< min pos) (<= pos max))
	(list (list min (1- pos))
	      (list pos max))
	(list a))))

(defun code (x)
  (typecase x
    (integer x)
    (character (char-code x))))

(defun parse-range (range)
  (if (listp range)
      (list (code (car range)) (code (cadr range)))
      (list (code range) (code range))))

(defun state-add-link (this range that)
  "Add a transition to state `this'; reading `range' proceeds to `that'."
  (cond ((eq range 'eps)
         (pushnew that (state-eps-transitions this)))
    (t
     (let ((new (list (parse-range range))))
       (dolist (k (state-transitions this)
		(push (cons new that) (state-transitions this)))
	 (when (eq (cdr k) that)
	   (setf new (ranges- new (car k))) ;avoid duplicates
	   (setf (car k) (append new (car k)))
	   (return nil)))
       ;; split existing ranges to remove overlap
       (dolist (k (state-transitions this))
	 (flet ((doit (pos)
		  (setf (car k)
			(mapcan (lambda (l)
				  (partition-range l pos))
				(car k)))))
	   (dolist (n new)
	     (doit (car n))
	     (doit (1+ (cadr n))))))))))

;;; When constructing FSA's from regular expressions we abstract by the notation
;;; of FSA's as boxen with an entry and an exit state.

(defstruct fsa
  start                                 ;entry state
  end)                                  ;exit state

(defun fsa-empty ()
  "Accepts the empty word."
  (let ((q (make-state)))
    (make-fsa :start q :end q)))

(defun fsa-trivial (char)
  "Accepts the trivial word consisting out of exactly one `char'."
  (let ((q0 (make-state))
        (q1 (make-state)))
    (state-add-link q0 char q1)
    (make-fsa :start q0 :end q1)))

(defun fsa-concat (a1 a2)
  "Concatenation of `a1' and `a2'. Hence `a1 a2'."
  (state-add-link (fsa-end a1) 'eps (fsa-start a2))
  (make-fsa :start (fsa-start a1)
            :end   (fsa-end   a2)))

(defun fsa-iterate (a)
  "Iteration of `a'. Hence `a*'"
  (let ((q0 (make-state))
        (q1 (make-state)))
    (state-add-link q0 'eps (fsa-start a))
    (state-add-link q0 'eps q1)
    (state-add-link q1 'eps q0)
    (state-add-link (fsa-end a) 'eps q1)
    (make-fsa :start q0 :end q1)))

(defun fsa-branch (&rest as)
  "Alternation of a0..an; Hence `a0 | a1 | ... | an'."
  (let ((q0 (make-state))
        (q1 (make-state)))
    (dolist (a as)
      (state-add-link q0 'eps (fsa-start a))
      (state-add-link (fsa-end a) 'eps q1))
    (make-fsa :start q0 :end q1)))

;;;; ----------------------------------------------------------------------------------------------------
;;;;  Converting regular expressions to (ND)FSA 
;;;;

;;; However we choose here a Lispy syntax for regular expressions:

;;; a                   singelton
;;; (and a0 .. an)      concatation
;;; (or a0 .. an)       alternation
;;; (* a)               iteration

;;; Further the abbrevs.:
;;;   (+ a) == (and a (* a))
;;;   (? a) == (or a (and))
;;;   (a0 ... an) == (and a0 ... an)

;;; When a string embeded into a regular expression is seen, the list
;;; of characters is spliced in. So formally:
;;;   (a0 .. ai "xyz" aj .. an) == (a0 .. ai #\x #\y #\z aj .. an)
;;;
;;; This is useful for matching words:
;;;   "foo" --> (and "foo") --> (and #\f #\o #\o) == The word 'foo'
;;; or for denoting small sets:
;;;   (or "+-") --> (or #\+ #\-) == One of '+' or '-'

(defun loose-eq (x y)
  (cond ((eq x y))
        ((and (symbolp x) (symbolp y))
         (string= (symbol-name x) (symbol-name y)))))

(defun regexp->fsa (term)
  (setf term (regexp-expand-splicing term))
  (cond ((and (atom term) (not (stringp term)))
         (fsa-trivial term))
        ((loose-eq (car term) 'RANGE)
         (fsa-trivial (cdr term)))
        ((loose-eq (car term) 'AND) (regexp/and->fsa term))
        ((loose-eq (car term) 'OR)  (regexp/or->fsa term))
        ((loose-eq (car term) '*)   (fsa-iterate (regexp->fsa (cadr term))))
        ((loose-eq (car term) '+)   (regexp->fsa `(AND ,(cadr term) (* ,(cadr term)))))
        ((loose-eq (car term) '?)   (regexp->fsa `(OR (AND) ,(cadr term))))
        (t 
         (regexp->fsa `(AND .,term))) ))

(defun regexp/or->fsa (term)
  ;; I optimize here a bit: I recognized, that ORs are mainly just
  ;; (large) sets of characters. The extra epsilon transitions are not
  ;; neccessary on single atoms, so I omit them here. -- This reduces the
  ;; number of states quite a bit in the first place.
  (let ((q0 (make-state))
        (q1 (make-state)))
    (dolist (a (cdr term))
      (cond ((atom a)
             (state-add-link q0 a q1))
            ((let ((a (regexp->fsa a)))
               (state-add-link q0 'eps (fsa-start a))
               (state-add-link (fsa-end a) 'eps q1)))))
    (make-fsa :start q0 :end q1)))

(defun regexp/and->fsa (term)
  (cond ((null (cdr term)) (fsa-empty))
        ((null (cddr term)) (regexp->fsa (cadr term)))
        ((fsa-concat (regexp->fsa (cadr term)) (regexp->fsa `(and .,(cddr term)))))))

(defun regexp-expand-splicing (term)
 (cond ((consp term)
        (mapcan #'(lambda (x)
                    (cond ((stringp x) (coerce x 'list))
                          ((list x))))
                term))
       (t term)))

;;;; ----------------------------------------------------------------------------------------------------
;;;;  Converting a ND-FSA to a D-FSA
;;;;

;;; Since we have to compare and unionfy sets of states a lot, I use bit-vectors
;;; to represent these sets for speed. However let me abstract that a bit:

;;; (All these are defined as macros simply for speed. Inlining would be an
;;; option here, when it would be reliable. With defining macros I enforce
;;; inlining).

(defmacro make-empty-set (n)
  "Create the empty set on the domain [0,n)."
  `(make-array ,n :element-type 'bit :initial-element 0))

(defmacro nset-put (bag new)
  "Destructively calculate bag = bag U {new}."
  `(setf (sbit (the (simple-array bit (*)) ,bag) (the fixnum ,new)) 1))

(defmacro element-of-set-p (elm set)
  "Determine whether `elm' is element of the set `set'."
  `(eq 1 (sbit (the (simple-array bit (*)) ,set) (the fixnum ,elm))))

(defmacro set-size (set)
  "Return the upper bound of the domain of `set'."
  `(length ,set))

(defmacro do-bits ((var set &optional result) &body body)
  "Iterate body with `var' over all elements of `set'."
  (let ((g/set (gensym)))
    `(let ((,g/set ,set))
       (dotimes (,var (set-size ,g/set) ,result)
         (when (element-of-set-p ,var ,g/set)
           ,@body)))))

;;; Since the sets we defined above only take non-negative integers, we have to
;;; number our states. This is done once by NUMBER-STATES.

(defun number-states (starts)
  "Number all state reachable form `starts', continuosly from 0. Each state got
   it's number stuck into the `id' slot.
   Returns two values: `n' the number of states and `tab' a table to lookup a
   state given the number it got attached to."
  (let ((n 0)
        (tab (make-array 0 :adjustable t :fill-pointer 0 :initial-element nil)))
    (labels ((walk (x)
               (unless (state-id x)
                 (vector-push-extend x tab 300)
                 (setf (state-id x) (prog1 n (incf n)))
                 (dolist (tr (state-transitions x))
                   (walk (cdr tr)))
                 (dolist (y (state-eps-transitions x))
                   (walk y)))))
      (dolist (s starts) (walk s))
      (values n tab))))

;;; We need to calculate the epsilon closure of a given state. Due to the
;;; precise workings of our algorithm below, we only need this augmenting
;;; version.

(defun fsa-epsilon-closure/set (x state-set)
  "Augment the epsilon closure of the state `state' into `state-set'."
  (unless (element-of-set-p (state-id x) state-set)
    (nset-put state-set (state-id x))
    (dolist (k (state-eps-transitions x))
      (fsa-epsilon-closure/set k state-set))))

(defun ndfsa->dfsa (starts)
  (let ((batch nil)
        (known nil))
    (multiple-value-bind (n tab) (number-states starts)
      (labels ((name-state-set (state-set)
                 (or (cdr (assoc state-set known :test #'equal))
                     (let ((new (make-state)))
                       (push (cons state-set new) known)
                       (push state-set batch)
                       new)))
               (add-state-set (state-set)
                 (let ((new-tr (make-hash-table :test 'equal))
                       (new-tr-real nil)
                       (name   (name-state-set state-set))
                       (new-final  0))
                   (do-bits (s0 state-set)
                     (let ((s (aref tab s0))) 
                       (setf new-final (max new-final (state-final s)))
                       (dolist (tr (state-transitions s))
                         (let ((to (cdr tr)))
                           (dolist (z (car tr))
                             (let ((looked (gethash z new-tr)))
                               (if looked
                                   (fsa-epsilon-closure/set to looked)
                                 (let ((sts (make-empty-set n)))
                                   (fsa-epsilon-closure/set to sts)
                                   (setf (gethash z new-tr) sts)))))))))
                   (do ((q (frob2 new-tr) (cddr q)))
                       ((null q))
                     (let ((z (car q))
                           (to (cadr q)))
                       (push (cons z (name-state-set to)) new-tr-real)))
                   (setf (state-transitions name) new-tr-real
                         (state-final name) new-final))))
        (prog1
            (mapcar #'(lambda (s)
                        (name-state-set (let ((sts (make-empty-set n)))
                                          (fsa-epsilon-closure/set s sts)
                                          sts)))
                    starts)
          (do ()
              ((null batch))
            (add-state-set (pop batch)))) ))))

(defun frob2 (res &aux res2)
  (maphash (lambda (z to)
	     (do ((p res2 (cddr p)))
		 ((null p)
		  (setf res2 (list* (list z) to res2)))
	       (when (equal to (cadr p))
		 (setf (car p) (cons z (car p)))
		 (return))))
	   res)
  res2)

;;;; ----------------------------------------------------------------------------------------------------
;;;;  API
;;;;

;;; Features to think about:
;;; - case insensitive scanner
;;; - compression of tables
;;; - debugging aids
;;; - non-interactive high speed scanning?
;;; - make BAG a macro? So that non used bags are not considered?
;;; - REJECT?
;;; - support for include?
;;; - support for putting back input?
;;; - count lines/columns? Track source?
;;; - richer set of regexp primitives e.g. "[a-z]" style sets
;;; - could we offer complement regexp?
;;; - trailing context
;;; - sub-state stacks?
;;; - user variables to include ['global' / 'lexical']
;;; - identifing sub-expression of regexps (ala \(..\) and \n)
;;;

#-(OR CMU SBCL GCL)
(defun loadable-states-form (starts) 
  `',starts)  

#+(OR CMU SBCL GCL)
;; Leider ist das CMUCL so dumm, dass es scheinbar nicht faehig ist die
;; selbstbezuegliche Structur ',starts in ein FASL file zu dumpen ;-(
;; Deswegen hier dieser read-from-string Hack.
(defun loadable-states-form (starts)
  `(LET ((*PACKAGE* (FIND-PACKAGE ',(package-name *package*))))
        (READ-FROM-STRING ',(let ((*print-circle* t)
                                  (*print-readably* t)
                                  (*print-pretty* nil))
                              (prin1-to-string starts)))))

;;;; ----------------------------------------------------------------------------------------------------
;;;;

(defun parse-char-set (string i)
  (let ((res nil)
        (complement-p nil))
    (incf i)                            ;skip '['
    ;;the first char is special
    (cond ((char= (char string i) #\]) (incf i) (push #\] res))
          ((char= (char string i) #\^) (incf i) (setq complement-p t))
          ((char= (char string i) #\-) (incf i) (push #\- res)))
    (do ()
        ((char= (char string i) #\])
         (values (if complement-p (cons 'cset res) (cons 'set res)) (+ i 1)))
      (cond ((char= (char string (+ i 1)) #\-)
             ;;it's a range
             (push (cons (char string i) (char string (+ i 2))) res)
             (incf i 3))
            (t
             ;;singleton
             (push (char string i) res)
             (incf i))))))

;;;; ------------------------------------------------------------------------------------------

(defparameter *full-table-p* nil)

(defun mungle-transitions (trs)
  (if *full-table-p*
      (let ((res (make-array 256 :initial-element nil)))
        (dolist (tr trs)
          (dolist (range (car tr))
            (loop
	       for code from (car range) to (cadr range)
	       do (setf (aref res code) (cdr tr)))))
        res)
    trs))

(defun over-all-states (fun starts)
  ;; Apply `fun' to each state reachable from starts.
  (let ((yet nil))
    (labels ((walk (q)
               (unless (member q yet)
                 (push q yet)
                 (let ((trs (state-transitions q)))
                   (funcall fun q)
                   (dolist (tr trs)
                     (walk (cdr tr)))))))
      (mapc #'walk starts))))

(defmacro deflexer (name macro-defs &rest rule-defs)
  (let ((macros nil) starts clauses (n-fin 0))
    (dolist (k macro-defs)
      (push (cons (car k) (sublis macros (cadr k))) macros))
    ;;canon clauses -- each element of rule-defs becomes (start expr end action)
    (setq rule-defs
      (mapcar #'(lambda (x)
                  (cond ((and (consp (car x)) (string-equal (caar x) :in))
                         (list (cadar x) (sublis macros (caddar x)) (progn (incf n-fin) n-fin) (cdr x)))
                        ((list 'initial (sublis macros (car x)) (progn (incf n-fin) n-fin) (cdr x)))))
              (reverse rule-defs)))
    ;;collect all start states in alist (<name> . <state>)
    (setq starts (mapcar #'(lambda (name)
                             (cons name (make-state)))
                         (remove-duplicates (mapcar #'car rule-defs))))
    ;;build the nd-fsa's
    (dolist (r rule-defs)
      (destructuring-bind (start expr end action) r
        (let ((q0 (cdr (assoc start starts)))
              (fsa (regexp->fsa `(and ,expr))))
          ;;link start state
          (state-add-link q0 'eps (fsa-start fsa))
          ;;mark final state
          (setf (state-final (fsa-end fsa)) end)
          ;; build a clause for CASE
          (push `((,end) .,action) clauses))))
    ;; hmm... we have to sort the final states after building the dfsa
    ;; or introduce fixnum identifier and instead of union take the minimum 
    ;; above in ndfsa->dfsa.
    (progn
     (mapcar #'(lambda (x y) (setf (cdr x) y))
             starts (ndfsa->dfsa (mapcar #'cdr starts))))
    ;;(terpri)(princ `(,(number-states starts) states))(finish-output)
    (let ((n 0))
      (over-all-states (lambda (state)
                         (incf n)
                         (setf (state-transitions state)
                               (mungle-transitions (state-transitions state))))
                       (mapcar #'cdr starts))
      (format T "~&~D states." n))
    `(DEFUN ,(intern (format nil "MAKE-~A-LEXER" name)) (INPUT)
        (LET* ((STARTS ,(loadable-states-form starts))
               (SUB-STATE 'INITIAL)
               (STATE NIL)
               (LOOK-AHEAD NIL)
               (BAGG/CH (MAKE-ARRAY 100 :FILL-POINTER 0 :ADJUSTABLE T
				    :ELEMENT-TYPE 'CHARACTER))
               (BAGG/STATE (MAKE-ARRAY 100 :FILL-POINTER 0 :ADJUSTABLE T))
               (CH NIL))
              #'(LAMBDA ()
                   (BLOCK NIL
                     (LABELS ((BEGIN (X)
                                     (SETQ SUB-STATE X))
                              (BACKUP (CH)
                                      (COND ((STRINGP CH)
                                             (WHEN (> (LENGTH CH) 0)
                                                   (PUSH (CONS 0 CH) LOOK-AHEAD)))
                                            (T (PUSH CH LOOK-AHEAD))))
                              (PUSH* (CH STATE)
                                     (VECTOR-PUSH-EXTEND CH BAGG/CH 10)
                                     (VECTOR-PUSH-EXTEND STATE BAGG/STATE 10) )
                              (POP*/CH ()
                                       (LET ((FP (LENGTH BAGG/CH)))
                                            (PROG1 (CHAR BAGG/CH (1- FP))
                                                   (SETF (FILL-POINTER BAGG/STATE) (1- FP))
                                                   (SETF (FILL-POINTER BAGG/CH) (1- FP)))))
                              (TOS*/STATE ()
                                          (AREF BAGG/STATE (1- (LENGTH BAGG/STATE))) )
                              (EMPTY*? ()
                                       (= (LENGTH BAGG/CH) 0))
                              (REWIND* ()
                                       (SETF (FILL-POINTER BAGG/CH) 0)
                                       (SETF (FILL-POINTER BAGG/STATE) 0) )
                              (STRING* ()
                                       (COPY-SEQ BAGG/CH))
                              (GETCH ()
                                     (COND ((NULL LOOK-AHEAD) (READ-CHAR INPUT NIL NIL))
                                           ((CONSP (CAR LOOK-AHEAD))
                                            (LET ((S (CDAR LOOK-AHEAD)))
                                                 (PROG1
                                                  (CHAR S (CAAR LOOK-AHEAD))
                                                  (INCF (CAAR LOOK-AHEAD))
                                                  (WHEN (= (CAAR LOOK-AHEAD) (LENGTH S))
                                                        (POP LOOK-AHEAD)))))
                                           (T (POP LOOK-AHEAD)) ))
                              ,(if *full-table-p*
                                 `(FIND-NEXT-STATE (STATE CH)
                                    (IF (CHARACTERP CH)
                                        (SVREF (STATE-TRANSITIONS STATE) (CHAR-CODE CH))
                                        NIL))
                                 `(FIND-NEXT-STATE (STATE CH)
                                     (WHEN ch
				       (BLOCK FOO
					 (DOLIST (K (STATE-TRANSITIONS STATE))
					   (DOLIST (Q (CAR K))
					     (WHEN (<= (CAR Q) (CHAR-CODE CH) (CADR q))
					       (RETURN-FROM FOO (CDR K))))))))) )
                             (DECLARE (INLINE BACKUP GETCH FIND-NEXT-STATE)
				      (IGNORABLE #'BEGIN))
                             (TAGBODY 
                              START (SETQ STATE (CDR (ASSOC SUB-STATE STARTS)))
                                    (WHEN (NULL STATE)
                                          (ERROR "Sub-state ~S is not defined." SUB-STATE))
                                    (REWIND*)
                              LOOP  (SETQ CH (GETCH))
                                    (LET ((NEXT-STATE (FIND-NEXT-STATE STATE CH)) )
                                         (COND ((NULL NEXT-STATE)
                                                (BACKUP CH)
                                                (DO ()
                                                    ((OR (EMPTY*?) (NOT (EQ 0 (TOS*/STATE)))))
                                                    (BACKUP (POP*/CH)))
                                                (COND ((AND (EMPTY*?) (NULL CH))
                                                       (RETURN :EOF))
                                                      ((EMPTY*?)
                                                       (ERROR "oops at ~A: ~S ~S"
							      (file-position (cxml-rng::stream-source INPUT))
							      ch
							      (mapcar #'car (state-transitions state))))
                                                      (T
                                                       (LET ((HALTING-STATE (TOS*/STATE)))
                                                            (LET ((BAG* NIL))
                                                                 (SYMBOL-MACROLET ((BAG (IF BAG*
                                                                                         BAG*
                                                                                         (SETF BAG* (STRING*)))))
                                                                   (CASE HALTING-STATE
                                                                         ,@clauses)))
                                                            (GO START)))))
                                               (T
                                                (PUSH* CH (STATE-FINAL NEXT-STATE))
                                                (SETQ STATE NEXT-STATE)
                                                (GO LOOP))))))))))))
