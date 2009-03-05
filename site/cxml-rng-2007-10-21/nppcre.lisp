;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2002-2005, Dr. Edmund Weitz. All rights reserved.

;;; hacked for XSD regular expressions by David Lichteblau in 2007:

;;;  - no comments and extended stuff
;;;  - no (?
;;;  - no greedyness modifier
;;;  - fewer and different backslash-escapes: \i \I \c \C \.
;;;  - character set substraction: [foo-[bar]]
;;;  - no ^ and $, but always wrap those around the complete parse tree
;;;  - ...

;;; Derived from:
;;; /usr/local/cvsrep/cl-ppcre/lexer.lisp,v 1.24 2005/04/01 21:29:09 edi Exp
;;; and
;;; /usr/local/cvsrep/cl-ppcre/parser.lisp,v 1.21 2005/08/03 21:11:27 edi Exp

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *standard-optimize-settings* '(optimize)))

(defvar *in-pattern-parser-p* nil)

(defvar *convert-char-class-to-hash* #'cl-ppcre::convert-char-class-to-hash)

;;; zzz Evil hack!

(format t "Patching CL-PPCRE::CONVERT-CHAR-CLASS-TO-HASH~%")
(setf (fdefinition 'cl-ppcre::convert-char-class-to-hash)
      (lambda (list)
	(when *in-pattern-parser-p*
	  (setf list (mapcan (lambda (x)
			       (if (symbolp x)
				   (symbol-value x)
				   x))
			     list)))
	(funcall *convert-char-class-to-hash* list)))

(defun signal-ppcre-syntax-error (fmt &rest args)
  (error "invalid pattern: ~?" fmt args))

(defun signal-ppcre-syntax-error* (pos fmt &rest args)
  (error "invalid pattern at ~D: ~?" pos fmt args))

(defmacro maybe-coerce-to-simple-string (string)
  (let ((=string= (gensym)))
    `(let ((,=string= ,string))
      (cond ((simple-string-p ,=string=)
              ,=string=)
            (t
              (coerce ,=string= 'simple-string))))))

(defun map-char-to-special-char-class (chr lexer)
  (declare #.*standard-optimize-settings*)
  "Maps escaped characters like \"\\d\" to the tokens which represent
their associated character classes."
  (case chr
    (#\. '\.)
    (#\s '\\s) (#\i '\\i) (#\c '\\c) (#\d '\\d) (#\w '\\w)
    (#\S '^s) (#\I '^i) (#\C '^c) (#\D '^d) (#\W '^w)
    (#\p
     (unless (eql (next-char lexer) #\{)
       (signal-ppcre-syntax-error "Missing open brace after \\p"))
     (let* ((bag (loop
		    for c = (next-char lexer)
		    for last = (eql c #\})
		    and done = nil then last
		    until done
		    unless c do
		      (signal-ppcre-syntax-error
		       "Missing close brace after \\p")
		    collect c))
	    (bag (coerce (list* #\p #\{ bag) 'string)))
       (or (find-symbol bag 'cxml-types)
	   (signal-ppcre-syntax-error "Invalid character property: ~A"
				      bag))))))

(locally
  (declare #.*standard-optimize-settings*)
  (defstruct (lexer (:constructor make-lexer-internal))
    "LEXER structures are used to hold the regex string which is
currently lexed and to keep track of the lexer's state."
    (str ""
         :type string
         :read-only t)
    (len 0
         :type fixnum
         :read-only t)
    (pos 0
         :type fixnum)
    (last-pos nil
              :type list)))

(defun make-lexer (string)
  (declare (inline make-lexer-internal)
           #-genera (type string string))
  (make-lexer-internal :str (maybe-coerce-to-simple-string string)
                       :len (length string)))

(declaim (inline end-of-string-p))
(defun end-of-string-p (lexer)
  (declare #.*standard-optimize-settings*)
  "Tests whether we're at the end of the regex string."
  (<= (lexer-len lexer)
      (lexer-pos lexer)))

(declaim (inline looking-at-p))
(defun looking-at-p (lexer chr)
  (declare #.*standard-optimize-settings*)
  "Tests whether the next character the lexer would see is CHR.
Does not respect extended mode."
  (and (not (end-of-string-p lexer))
       (char= (schar (lexer-str lexer) (lexer-pos lexer))
              chr)))

(declaim (inline next-char-non-extended))
(defun next-char-non-extended (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns the next character which is to be examined and updates the
POS slot. Does not respect extended mode."
  (cond ((end-of-string-p lexer)
          nil)
        (t
          (prog1
            (schar (lexer-str lexer) (lexer-pos lexer))
            (incf (lexer-pos lexer))))))

(defun next-char (lexer)
  (declare #.*standard-optimize-settings*)
  (next-char-non-extended lexer))

(declaim (inline fail))
(defun fail (lexer)
  (declare #.*standard-optimize-settings*)
  "Moves (LEXER-POS LEXER) back to the last position stored in
\(LEXER-LAST-POS LEXER) and pops the LAST-POS stack."
  (unless (lexer-last-pos lexer)
    (signal-ppcre-syntax-error "LAST-POS stack of LEXER ~A is empty" lexer))
  (setf (lexer-pos lexer) (pop (lexer-last-pos lexer)))
  nil)

(defun get-number (lexer &key (radix 10) max-length no-whitespace-p)
  (declare #.*standard-optimize-settings*)
  "Read and consume the number the lexer is currently looking at and
return it. Returns NIL if no number could be identified.
RADIX is used as in PARSE-INTEGER. If MAX-LENGTH is not NIL we'll read
at most the next MAX-LENGTH characters. If NO-WHITESPACE-P is not NIL
we don't tolerate whitespace in front of the number."
  (when (or (end-of-string-p lexer)
            (and no-whitespace-p
                 (not (find (schar (lexer-str lexer) (lexer-pos lexer))
			    "0123456789"))))
    (return-from get-number nil))
  (multiple-value-bind (integer new-pos)
      (parse-integer (lexer-str lexer)
                     :start (lexer-pos lexer)
                     :end (if max-length
                            (let ((end-pos (+ (lexer-pos lexer)
                                              (the fixnum max-length)))
                                  (lexer-len (lexer-len lexer)))
                              (if (< end-pos lexer-len)
                                end-pos
                                lexer-len))
                            (lexer-len lexer))
                     :radix radix
                     :junk-allowed t)
    (cond ((and integer (>= (the fixnum integer) 0))
            (setf (lexer-pos lexer) new-pos)
            integer)
          (t nil))))

(declaim (inline make-char-from-code))
(defun make-char-from-code (number error-pos)
  (declare #.*standard-optimize-settings*)
  "Create character from char-code NUMBER. NUMBER can be NIL
which is interpreted as 0. ERROR-POS is the position where
the corresponding number started within the regex string."
  ;; only look at rightmost eight bits in compliance with Perl
  (let ((code (logand #o377 (the fixnum (or number 0)))))
    (or (and (< code char-code-limit)
             (code-char code))
        (signal-ppcre-syntax-error*
         error-pos
         "No character for hex-code ~X"
         number))))

(defun unescape-char (lexer)
  (declare #.*standard-optimize-settings*)
  "Convert the characters(s) following a backslash into a token
which is returned. This function is to be called when the backslash
has already been consumed. Special character classes like \\W are
handled elsewhere."
  (when (end-of-string-p lexer)
    (signal-ppcre-syntax-error "String ends with backslash"))
  (let ((chr (next-char-non-extended lexer)))
    (case chr
      ;; the following five character names are 'semi-standard'
      ;; according to the CLHS but I'm not aware of any implementation
      ;; that doesn't implement them
      ((#\t)
        #\Tab)
      ((#\n)
        #\Newline)
      ((#\r)
        #\Return)
      (otherwise
        ;; all other characters aren't affected by a backslash
        chr))))

(defun convert-substraction (r s)
  (flet ((rangify (x)
	   (etypecase x
	     (character `((:range ,x ,x)))
	     (list (assert (eq (car x) :range)) (list x))
	     (symbol (copy-list (symbol-value x))))))
    (ranges- (mapcan #'rangify r) (mapcan #'rangify s))))

(defun collect-char-class (lexer)
  (declare #.*standard-optimize-settings*)
  "Reads and consumes characters from regex string until a right
bracket is seen. Assembles them into a list \(which is returned) of
characters, character ranges, like \(:RANGE #\\A #\\E) for a-e, and
tokens representing special character classes."
  (let ((start-pos (lexer-pos lexer))         ; remember start for error message
        hyphen-seen
        last-char
        list)
    (flet ((handle-char (c)
             "Do the right thing with character C depending on whether
we're inside a range or not."
             (cond ((and hyphen-seen last-char)
                     (setf (car list) (list :range last-char c)
                           last-char nil))
                   (t
                     (push c list)
                     (setq last-char c)))
             (setq hyphen-seen nil)))
      (loop for first = t then nil
            for c = (next-char-non-extended lexer)
            ;; leave loop if at end of string
            while c
            do (cond
                 ((char= c #\\)
                   ;; we've seen a backslash
                   (let ((next-char (next-char-non-extended lexer)))
                     (case next-char
                       ((#\. #\i #\I #\c #\C #\d #\D #\w #\W #\s #\S #\p)
                         ;; a special character class
                         (push (map-char-to-special-char-class next-char lexer)
			       list)
                         ;; if the last character was a hyphen
                         ;; just collect it literally
                         (when hyphen-seen
                           (push #\- list))
                         ;; if the next character is a hyphen do the same
                         (when (looking-at-p lexer #\-)
                           (incf (lexer-pos lexer))
                           (when (looking-at-p lexer #\[)
			     (incf (lexer-pos lexer))
			     (return-from collect-char-class
			       (prog1
				   (convert-substraction
				    (nreverse list)
				    (collect-char-class lexer))
				 (unless
				     (eql (next-char-non-extended lexer) #\])
				   (signal-ppcre-syntax-error*
				    start-pos
				    "Missing right bracket to close character class")))))
			   (push #\- list))
                         (setq hyphen-seen nil))
                       (otherwise
                         ;; otherwise unescape the following character(s)
                         (decf (lexer-pos lexer))
                         (handle-char (unescape-char lexer))))))
                 (first
                   ;; the first character must not be a right bracket
                   ;; and isn't treated specially if it's a hyphen
                   (handle-char c))
                 ((char= c #\])
                   ;; end of character class
                   ;; make sure we collect a pending hyphen
                   (when hyphen-seen
                     (setq hyphen-seen nil)
                     (handle-char #\-))
                   ;; reverse the list to preserve the order intended
                   ;; by the author of the regex string
                   (return-from collect-char-class (nreverse list)))
                 ((and hyphen-seen (char= c #\[))
		  (return-from collect-char-class
		    (prog1
			(convert-substraction
			 (nreverse list)
			 (collect-char-class lexer))
		      (unless (eql (next-char-non-extended lexer) #\])
			(signal-ppcre-syntax-error*
			 start-pos
			 "Missing right bracket to close character class")))))
                 ((and (char= c #\-)
                       last-char
                       (not hyphen-seen))
                   ;; if the last character was 'just a character'
                   ;; we expect to be in the middle of a range
                   (setq hyphen-seen t))
                 ((char= c #\-)
                   ;; otherwise this is just an ordinary hyphen
                   (handle-char #\-))
                 (t
                   ;; default case - just collect the character
                   (handle-char c))))
      ;; we can only exit the loop normally if we've reached the end
      ;; of the regex string without seeing a right bracket
      (signal-ppcre-syntax-error*
       start-pos
       "Missing right bracket to close character class"))))

(defun get-quantifier (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns a list of two values (min max) if what the lexer is looking
at can be interpreted as a quantifier. Otherwise returns NIL and
resets the lexer to its old position."
  ;; remember starting position for FAIL and UNGET-TOKEN functions
  (push (lexer-pos lexer) (lexer-last-pos lexer))
  (let ((next-char (next-char lexer)))
    (case next-char
      ((#\*)
        ;; * (Kleene star): match 0 or more times
        '(0 nil))
      ((#\+)
        ;; +: match 1 or more times
        '(1 nil))
      ((#\?)
        ;; ?: match 0 or 1 times
        '(0 1))
      ((#\{)
        ;; one of
        ;;   {n}:   match exactly n times
        ;;   {n,}:  match at least n times
        ;;   {n,m}: match at least n but not more than m times
        ;; note that anything not matching one of these patterns will
        ;; be interpreted literally - even whitespace isn't allowed
        (let ((num1 (get-number lexer :no-whitespace-p t)))
          (if num1
            (let ((next-char (next-char-non-extended lexer)))
              (case next-char
                ((#\,)
                  (let* ((num2 (get-number lexer :no-whitespace-p t))
                         (next-char (next-char-non-extended lexer)))
                    (case next-char
                      ((#\})
                        ;; this is the case {n,} (NUM2 is NIL) or {n,m}
                        (list num1 num2))
                      (otherwise
                        (fail lexer)))))
                ((#\})
                  ;; this is the case {n}
                  (list num1 num1))
                (otherwise
                  (fail lexer))))
            ;; no number following left curly brace, so we treat it
            ;; like a normal character
            (fail lexer))))
      ;; cannot be a quantifier
      (otherwise
        (fail lexer)))))

(defun get-token (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns and consumes the next token from the regex string (or NIL)."
  ;; remember starting position for UNGET-TOKEN function
  (push (lexer-pos lexer)
        (lexer-last-pos lexer))
  (let ((next-char (next-char lexer)))
    (cond (next-char
            (case next-char
              ;; the easy cases first - the following six characters
              ;; always have a special meaning and get translated
              ;; into tokens immediately
              ((#\))
                :close-paren)
              ((#\|)
                :vertical-bar)
              ((#\?)
                :question-mark)
              ((#\.)
                :everything)
              ((#\+ #\*)
                ;; quantifiers will always be consumend by
                ;; GET-QUANTIFIER, they must not appear here
                (signal-ppcre-syntax-error*
                 (1- (lexer-pos lexer))
                 "Quantifier '~A' not allowed"
                 next-char))
              ((#\{)
                ;; left brace isn't a special character in it's own
                ;; right but we must check if what follows might
                ;; look like a quantifier
                (let ((this-pos (lexer-pos lexer))
                      (this-last-pos (lexer-last-pos lexer)))
                  (unget-token lexer)
                  (when (get-quantifier lexer)
                    (signal-ppcre-syntax-error*
                     (car this-last-pos)
                     "Quantifier '~A' not allowed"
                     (subseq (lexer-str lexer)
                             (car this-last-pos)
                             (lexer-pos lexer))))
                  (setf (lexer-pos lexer) this-pos
                        (lexer-last-pos lexer) this-last-pos)
                  next-char))
              ((#\[)
                ;; left bracket always starts a character class
                (cons  (cond ((looking-at-p lexer #\^)
                               (incf (lexer-pos lexer))
                               :inverted-char-class)
                             (t
                               :char-class))
                       (collect-char-class lexer)))
              ((#\\)
                ;; backslash might mean different things so we have
                ;; to peek one char ahead:
                (let ((next-char (next-char-non-extended lexer)))
                  (case next-char
                    ((#\. #\i #\I #\c #\C #\d #\D #\w #\W #\s #\S #\p)
                      ;; these will be treated like character classes
                      (map-char-to-special-char-class next-char lexer))
                    (otherwise
                      ;; in all other cases just unescape the
                      ;; character
                      (decf (lexer-pos lexer))
                      (unescape-char lexer)))))
              ((#\()
	       :open-paren)
              (otherwise
                ;; all other characters are their own tokens
                next-char)))
          ;; we didn't get a character (this if the "else" branch from
          ;; the first IF), so we don't return a token but NIL
          (t
            (pop (lexer-last-pos lexer))
            nil))))

(declaim (notinline unget-token))	;FIXME: else AVER in GET-TOKEN
(defun unget-token (lexer)
  (declare #.*standard-optimize-settings*)
  "Moves the lexer back to the last position stored in the LAST-POS stack."
  (if (lexer-last-pos lexer)
    (setf (lexer-pos lexer)
            (pop (lexer-last-pos lexer)))
    (error "No token to unget \(this should not happen)")))

(declaim (inline start-of-subexpr-p))
(defun start-of-subexpr-p (lexer)
  (declare #.*standard-optimize-settings*)
  "Tests whether the next token can start a valid sub-expression, i.e.
a stand-alone regex."
  (let* ((pos (lexer-pos lexer))
         (next-char (next-char lexer)))
    (not (or (null next-char)
             (prog1
               (member (the character next-char)
                       '(#\) #\|)
                       :test #'char=)
               (setf (lexer-pos lexer) pos))))))

(defun group (lexer)
  (declare #.*standard-optimize-settings*)
  "Parses and consumes a <group>.
The productions are: <group> -> \"(\"<regex>\")\"
                                <legal-token>
Will return <parse-tree> or (<grouping-type> <parse-tree>) where
<grouping-type> is one of six keywords - see source for details."
  (let ((open-token (get-token lexer)))
    (cond ((eq open-token :open-paren)
	   (let* ((open-paren-pos (car (lexer-last-pos lexer)))
		  (reg-expr (reg-expr lexer))
		  (close-token (get-token lexer)))
	     (unless (eq close-token :close-paren)
	       ;; the token following <regex> must be the closing
	       ;; parenthesis or this is a syntax error
	       (signal-ppcre-syntax-error*
		open-paren-pos
		"Opening paren has no matching closing paren"))
	     (list :register reg-expr)))
          (t
            ;; this is the <legal-token> production; <legal-token> is
            ;; any token which passes START-OF-SUBEXPR-P (otherwise
            ;; parsing had already stopped in the SEQ method)
            open-token))))

(defun greedy-quant (lexer)
  (declare #.*standard-optimize-settings*)
  "Parses and consumes a <greedy-quant>.
The productions are: <greedy-quant> -> <group> | <group><quantifier>
where <quantifier> is parsed by the lexer function GET-QUANTIFIER.
Will return <parse-tree> or (:GREEDY-REPETITION <min> <max> <parse-tree>)."
  (let* ((group (group lexer))
         (token (get-quantifier lexer)))
    (if token
      ;; if GET-QUANTIFIER returned a non-NIL value it's the
      ;; two-element list (<min> <max>)
      (list :greedy-repetition (first token) (second token) group)
      group)))

(defun quant (lexer)
  (declare #.*standard-optimize-settings*)
  (greedy-quant lexer))

(defun seq (lexer)
  (declare #.*standard-optimize-settings*)
  "Parses and consumes a <seq>.
The productions are: <seq> -> <quant> | <quant><seq>.
Will return <parse-tree> or (:SEQUENCE <parse-tree> <parse-tree>)."
  (flet ((make-array-from-two-chars (char1 char2)
           (let ((string (make-array 2
                                     :element-type 'character
                                     :fill-pointer t
                                     :adjustable t)))
             (setf (aref string 0) char1)
             (setf (aref string 1) char2)
             string)))
    ;; Note that we're calling START-OF-SUBEXPR-P before we actually try
    ;; to parse a <seq> or <quant> in order to catch empty regular
    ;; expressions
    (if (start-of-subexpr-p lexer)
      (let ((quant (quant lexer)))
        (if (start-of-subexpr-p lexer)
          (let* ((seq (seq lexer))
                 (quant-is-char-p (characterp quant))
                 (seq-is-sequence-p (and (consp seq)
                                         (eq (first seq) :sequence))))
            (cond ((and quant-is-char-p
                        (characterp seq))
                    (make-array-from-two-chars seq quant))
                  ((and quant-is-char-p
                        (stringp seq))
                    (vector-push-extend quant seq)
                    seq)
                  ((and quant-is-char-p
                        seq-is-sequence-p
                        (characterp (second seq)))
                    (cond ((cddr seq)
                            (setf (cdr seq)
                                    (cons
                                     (make-array-from-two-chars (second seq)
                                                                quant)
                                     (cddr seq)))
                            seq)
                          (t (make-array-from-two-chars (second seq) quant))))
                  ((and quant-is-char-p
                        seq-is-sequence-p
                        (stringp (second seq)))
                    (cond ((cddr seq)
                            (setf (cdr seq)
                                    (cons
                                     (progn
                                       (vector-push-extend quant (second seq))
                                       (second seq))
                                     (cddr seq)))
                            seq)
                          (t 
                            (vector-push-extend quant (second seq))
                            (second seq))))
                  (seq-is-sequence-p
                    ;; if <seq> is also a :SEQUENCE parse tree we merge
                    ;; both lists into one to avoid unnecessary consing
                    (setf (cdr seq)
                            (cons quant (cdr seq)))
                    seq)
                  (t (list :sequence quant seq))))
          quant))
      :void)))
  
(defun reg-expr (lexer)
  (declare #.*standard-optimize-settings*)
  "Parses and consumes a <regex>, a complete regular expression.
The productions are: <regex> -> <seq> | <seq>\"|\"<regex>.
Will return <parse-tree> or (:ALTERNATION <parse-tree> <parse-tree>)."
  (let ((pos (lexer-pos lexer)))
    (case (next-char lexer)
      ((nil)
        ;; if we didn't get any token we return :VOID which stands for
        ;; "empty regular expression"
        :void)
      ((#\|)
        ;; now check whether the expression started with a vertical
        ;; bar, i.e. <seq> - the left alternation - is empty
        (list :alternation :void (reg-expr lexer)))
      (otherwise
        ;; otherwise un-read the character we just saw and parse a
        ;; <seq> plus the character following it
        (setf (lexer-pos lexer) pos)
        (let* ((seq (seq lexer))
               (pos (lexer-pos lexer)))
          (case (next-char lexer)
            ((nil)
              ;; no further character, just a <seq>
              seq)
            ((#\|)
              ;; if the character was a vertical bar, this is an
              ;; alternation and we have the second production
              (let ((reg-expr (reg-expr lexer)))
                (cond ((and (consp reg-expr)
                            (eq (first reg-expr) :alternation))
                        ;; again we try to merge as above in SEQ
                        (setf (cdr reg-expr)
                                (cons seq (cdr reg-expr)))
                        reg-expr)
                      (t (list :alternation seq reg-expr)))))
            (otherwise
              ;; a character which is not a vertical bar - this is
              ;; either a syntax error or we're inside of a group and
              ;; the next character is a closing parenthesis; so we
              ;; just un-read the character and let another function
              ;; take care of it
              (setf (lexer-pos lexer) pos)
              seq)))))))

(defun reverse-strings (parse-tree)
  (declare #.*standard-optimize-settings*)
  (cond ((stringp parse-tree)
          (nreverse parse-tree))
        ((consp parse-tree)
          (loop for parse-tree-rest on parse-tree
                while parse-tree-rest
                do (setf (car parse-tree-rest)
                           (reverse-strings (car parse-tree-rest))))
          parse-tree)
        (t parse-tree)))

(defun parse-pattern (string)
  (declare #.*standard-optimize-settings*)
  "Translate the regex string STRING into a parse tree."
  (let* ((*in-pattern-parser-p* t)
	 (lexer (make-lexer string))
         (parse-tree (reverse-strings (reg-expr lexer))))
    ;; check whether we've consumed the whole regex string
    (if (end-of-string-p lexer)
	`(:sequence :start-anchor ,parse-tree :end-anchor)
	(signal-ppcre-syntax-error*
	 (lexer-pos lexer)
	 "Expected end of string"))))

(defmethod pattern-scanner ((str string))
  (cl-ppcre:create-scanner (parse-pattern str)))

(defmethod pattern-scanner ((scanner function))
  scanner)
