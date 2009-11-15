;;;; -*- coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:               string.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             clisp
;;;;USER-INTERFACE:     clisp
;;;;DESCRIPTION
;;;;
;;;;    This module exports string functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-01-30 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
;;;;
;;;;    This file is part of PJB Clisp Utilities.
;;;;
;;;;    This  program is  free software;  you can  redistribute  it and/or
;;;;    modify it  under the  terms of the  GNU General Public  License as
;;;;    published by the Free Software Foundation; either version 2 of the
;;;;    License, or (at your option) any later version.
;;;;
;;;;    This program  is distributed in the  hope that it  will be useful,
;;;;    but  WITHOUT ANY WARRANTY;  without even  the implied  warranty of
;;;;    MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a  copy of the GNU General Public License
;;;;    along with  this program; see the  file COPYING; if  not, write to
;;;;    the Free  Software Foundation, Inc.,  59 Temple Place,  Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;*****************************************************************************


(DEFINE-PACKAGE "COM.INFORMATIMAGO.CLISP.STRING"
  (:DOCUMENTATION "This module exports string functions.")
  (:FROM "COMMON-LISP" :IMPORT :ALL)
  (:FROM "REGEXP"      :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.LIST"   :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING" :IMPORT UNSPLIT-STRING)
  (:EXPORT "SPLIT-STRING" "UNSPLIT-STRING"
           "STRING-MATCH" "STRING-TO-NUMBER"
           "CAPITALIZATION" "REPLACE-REGEXP-IN-STRING"
           "SUBSTRING"))



;; We have our own implementation of SPLIT-STRING using REGEXP,
;; specific to CLISP.

(DEFPARAMETER SPLIT-STRING-DEFAULT-SEPARATORS
  (FORMAT NIL "[ ~C~C~C~C~C]\\+"
          (CODE-CHAR 9) (CODE-CHAR 10) (CODE-CHAR 11) (CODE-CHAR 12)
          (CODE-CHAR 13))
  "The default separators for split-string (HT, LF, VT, FF, CR, SP)") ;;SPLIT-STRING-DEFAULT-SEPARATORS


(DEFUN SPLIT-STRING (STRING &OPTIONAL SEPARATORS)
  "
NOTE:   This implementation uses he REGEXP package.
"
  (UNLESS SEPARATORS (SETQ SEPARATORS SPLIT-STRING-DEFAULT-SEPARATORS))
  (LET ((RESULT (REGEXP:REGEXP-SPLIT SEPARATORS STRING)))
    (IF (STRING= "" (CAR RESULT))
        (SETQ RESULT (CDR RESULT)))
    (IF (STRING= "" (CAR (LAST RESULT)))
        (SETQ RESULT (NBUTLAST RESULT)))
    RESULT))

;; But we inherit UNSPLIT-STRING from COM.INFORMATIMAGO.COMMON-LISP.STRING.
;;



(DEFUN STRING-MATCH (REGEXP STRING &KEY (START 0) (END NIL)
                     (CASE-SENSITIVE NIL)
                     (EXTENDED NIL)
                     (NEWLINE NIL)
                     (NOSUB NIL))
  "An alias for REGEXP:MATCH."
  (REGEXP:MATCH REGEXP STRING
                :START START :END END
                :IGNORE-CASE (NOT CASE-SENSITIVE)
                :EXTENDED EXTENDED
                :NEWLINE NEWLINE :NOSUB NOSUB)
  ) ;;STRING-MATCH



(DEFVAR *CASE-FOLD-SEARCH* NIL
  "Whether searches and matches should ignore case.
Used by: REPLACE-REGEXP-IN-STRING.
") ;;*CASE-FOLD-SEARCH*



;;; CAPITALIZATION:
;;;
;;; 0  NIL
;;; 1   T
;;;
;;; 0  Upcase
;;; 1  Lowcase
;;; 2  Nocase
;;; 3  Special
;;;
;;; STATE:   (BOW,U/L/N/S)
;;;   Initial state: (NIL SP)
;;;
;;;   ((NIL UP) UP) --> (NIL UP) NOT NO-2C-WORD
;;;   ((NIL UP) LO) --> (NIL LO) NOT NO-2C-WORD NOT ALL-UPCASE
;;;   ((NIL UP) NO) --> (NIL NO) NOT NO-2C-WORD 
;;;   ((NIL UP) SP) --> (NIL SP)
;;;   ((NIL LO) UP) --> (NIL UP) NOT NO-2C-WORD NOT ALL-LOCASE NOT ALL-CAPITA 
;;;   ((NIL LO) LO) --> (NIL LO) NOT NO-2C-WORD
;;;   ((NIL LO) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   ((NIL LO) SP) --> (NIL SP)
;;;   ((NIL NO) UP) --> (NIL UP) NOT NO-2C-WORD
;;;   ((NIL NO) LO) --> (NIL LO) NOT NO-2C-WORD
;;;   ((NIL NO) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   ((NIL NO) SP) --> (NIL SP)
;;;   ((NIL SP) UP) --> ( T  UP) NOT ALL-LOCASE  
;;;   ((NIL SP) LO) --> ( T  LO) NOT ALL-UPCASE  NOT ALL-CAPITA
;;;   ((NIL SP) NO) --> ( T  NO)
;;;   ((NIL SP) SP) --> (NIL SP)
;;;   (( T  UP) UP) --> (NIL UP) NOT NO-2C-WORD NOT ALL-LOCASE NOT ALL-CAPITA
;;;   (( T  UP) LO) --> (NIL LO) NOT NO-2C-WORD NOT ALL-UPCASE  
;;;   (( T  UP) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   (( T  UP) SP) --> (NIL SP)
;;;   (( T  LO) UP) --> (NIL UP) NOT NO-2C-WORD NOT ALL-LOCASE
;;;   (( T  LO) LO) --> (NIL LO) NOT NO-2C-WORD NOT ALL-UPCASE  
;;;   (( T  LO) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   (( T  LO) SP) --> (NIL SP)
;;;   (( T  NO) UP) --> (NIL UP) NOT NO-2C-WORD NOT ALL-LOCASE NOT ALL-CAPITA 
;;;   (( T  NO) LO) --> (NIL LO) NOT NO-2C-WORD NOT ALL-UPCASE  
;;;   (( T  NO) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   (( T  NO) SP) --> (NIL SP)
;;;    ( T  SP) is impossible.

(DEFPARAMETER +CAPITALIZATION-TRANSITIONS+
  (MAKE-ARRAY '(2 4 4)
              :INITIAL-CONTENTS
              '((( (0 0 3)
                  (0 1 3 0)
                  (0 2 3)
                  (0 3) )
                 ( (0 0 3 1 2)
                  (0 1 3)
                  (0 2 3)
                  (0 3) )
                 ( (0 0 3)
                  (0 1 3)
                  (0 2 3)
                  (0 3) )
                 ( (1 0 1)
                  (1 1 0 2)
                  (1 2)
                  (0 3) ))
                (( (0 0 3 1 2)
                  (0 1 3 0)
                  (0 2 3)
                  (0 3) )
                 ( (0 0 3 1)
                  (0 1 3 0)
                  (0 2 3)
                  (0 3) )
                 ( (0 0 3 1 2)
                  (0 1 3 0)
                  (0 2 3)
                  (0 3) )
                 ( (0 0) ;; impossible state
                  (0 1)
                  (0 2)
                  (0 3) ))))) ;;+CAPITALIZATION-TRANSITIONS+



(DEFUN CAPITALIZATION (STRING)
  "
RETURN:  :LOWER :UPPER :CAPITALIZED or :WHATEVER
"
  (LET ((ALL-UPCASE 0)
        (ALL-LOCASE 1)
        (ALL-CAPITA 2)
        (NO-2C-WORD 3)
        (RESULT     (MAKE-ARRAY '(4) :INITIAL-ELEMENT T))
        (STATE      (CONS 0 3)) )
    (MAP NIL (LAMBDA (CH)
               (LET ((NEW-STATE (AREF +CAPITALIZATION-TRANSITIONS+
                                      (CAR STATE) (CDR STATE)
                                      (COND 
                                        ((NOT (ALPHA-CHAR-P CH)) 3)
                                        ((UPPER-CASE-P CH)       0)
                                        ((LOWER-CASE-P CH)       1)
                                        (T                       2)))))
                 (SETF (CAR STATE) (POP NEW-STATE))
                 (SETF (CDR STATE) (POP NEW-STATE))
                 (MAPC (LAMBDA (SYM) (SETF (AREF RESULT SYM) NIL)) NEW-STATE)
                 ))
         STRING)
    (COND ((AREF RESULT NO-2C-WORD) :WHATEVER)
          ((AREF RESULT ALL-UPCASE) :UPPER)
          ((AREF RESULT ALL-LOCASE) :LOWER)
          ((AREF RESULT ALL-CAPITA) :CAPITALIZED)
          (T                        :WHATEVER))
    )) ;;CAPITALIZATION





(DEFUN EMACS-BUGGED-STRING-CAPITALIZE (STRING)
  "
The string-capitalized that emacs implements in its replace-regexp-in-string
which is not even its capitalize (which is correct)!
Namely, it seems to  touch only the first character of each word.
"
  (DO ((RESULT (COPY-SEQ STRING))
       (I 0 (1+ I))
       (SP T)
       (CH) )
      ((<= (LENGTH RESULT) I) RESULT)
    (SETQ CH (CHAR RESULT I))
    (IF SP
        (WHEN (ALPHA-CHAR-P CH)
          (SETF (CHAR RESULT I) (CHAR-UPCASE CH))
          (SETQ SP NIL))
        (WHEN (NOT (ALPHANUMERICP CH))
          (SETQ SP T))))
  ) ;;EMACS-BUGGED-STRING-CAPITALIZE



(DEFUN REPLACE-REGEXP-IN-STRING
    (REGEXP REP STRING
     &OPTIONAL (FIXEDCASE NIL) (LITERAL NIL) (SUBEXP 0) (START 0)
     &KEY (CASE-SENSITIVE (NOT *CASE-FOLD-SEARCH*))
     (EXTENDED NIL) (NEWLINE NIL) (NOSUB NIL))
  "
NOTE:       emacs regexps are a mix between POSIX basic regexps
            and POSIX extended regexps.
            By default we'll use basic POSIX regexps, to keep '\\(...\\)'
            at the cost of the '+' repetition. The key parameters are
            passed to REGEXP:MATCH if specific behavior is needed.
            (We're not entirely compatible with emacs, but it's emacs which
            is wrong and we'll have to use another regexp package in emacs).

Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.
When REP is a function it's passed the while match 0, even if SUBEXP is not 0.

To replace only the first match (if any), make REGEXP match up to \'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"

If second arg FIXEDCASE is non-nil, do not alter case of replacement text.
Otherwise maybe capitalize the whole text, or maybe just word initials,
based on the replaced text.
If the replaced text has only capital letters
and has at least one multiletter word, convert NEWTEXT to all caps.
Otherwise if all words are capitalized in the replaced text,
capitalize each word in NEWTEXT.

If third arg LITERAL is non-nil, insert NEWTEXT literally.
Otherwise treat `\' as special:
  `\&' in NEWTEXT means substitute original matched text.
  `\N' means substitute what matched the Nth `\(...\)'.
       If Nth parens didn't match, substitute nothing.
  `\\' means insert one `\'.
Case conversion does not apply to these substitutions.

FIXEDCASE and LITERAL are optional arguments.

The optional fifth argument SUBEXP specifies a subexpression ;
it says to replace just that subexpression with NEWTEXT,
rather than replacing the entire matched text.
This is, in a vague sense, the inverse of using `\N' in NEWTEXT ;
`\N' copies subexp N into NEWTEXT, but using N as SUBEXP puts
NEWTEXT in place of subexp N.
This is useful only after a regular expression search or match,
since only regular expressions have distinguished subexpressions.
"
;;; REP       function(\0) --> NEWTEXT
;;; REP       string       --> NEWTEXT
;;;
;;; FIXEDCASE   T         identity
;;; FIXEDCASE   NIL       replaced text capitalization -> replacement
;;;
;;; LITERAL     T         identity
;;; LITERAL     NIL       substitute \&, \N, \\ and \x.
;;;
;;; SUBEXP      N         replaces only \N instead of \0
  (DO ((DONE NIL)
       (PIECES '())
       (POS 0)
       (REPLACEMENT)
       (REPLACED-MATCH)
       (MATCHES))
      (DONE
       (PROGN (PUSH (SUBSEQ STRING POS) PIECES)
              (APPLY (FUNCTION CONCATENATE) 'STRING (NREVERSE PIECES))))
    (SETQ MATCHES (MULTIPLE-VALUE-LIST
                   (REGEXP:MATCH REGEXP STRING
                                 :START START
                                 :IGNORE-CASE (NOT CASE-SENSITIVE)
                                 :EXTENDED EXTENDED
                                 :NEWLINE NEWLINE
                                 :NOSUB NOSUB)))
    (IF (AND MATCHES (CAR MATCHES))
        (PROGN
          ;; -1- Find the replacement:
          (SETQ REPLACEMENT
                (IF (FUNCTIONP REP)
                    (FUNCALL REP (REGEXP:MATCH-STRING STRING (CAR MATCHES)))
                    REP))
          ;; -2- Process FIXEDCASE
          (WHEN (OR (< SUBEXP 0) (<= (LENGTH MATCHES) SUBEXP))
            (ERROR "Argument out of range SUBEXP=~A." SUBEXP))
          (SETQ REPLACED-MATCH (NTH SUBEXP MATCHES))
          (UNLESS FIXEDCASE
            (LET ((CAP (CAPITALIZATION
                        (REGEXP:MATCH-STRING STRING REPLACED-MATCH))) )
              (SETQ REPLACEMENT
                    (FUNCALL
                     (COND
                       ((EQ CAP :UPPER) (FUNCTION STRING-UPCASE))
                       ((EQ CAP :LOWER) (FUNCTION IDENTITY))
                       ;;                That's what emacs does...
                       ((EQ CAP :CAPITALIZED)
                        (FUNCTION EMACS-BUGGED-STRING-CAPITALIZE))
                       (T               (FUNCTION IDENTITY)))
                     REPLACEMENT))))
          ;; -3- Process LITERAL
          (UNLESS LITERAL
            ;; substitute \&, \N and \\.
            (SETQ REPLACEMENT
                  (REPLACE-REGEXP-IN-STRING
                   "\\\\\\(.\\)"
                   (LAMBDA (SUBSTR)
                     (COND
                       ((CHAR= (CHAR SUBSTR 1) (CHARACTER "&"))
                        (REGEXP:MATCH-STRING STRING (CAR MATCHES)) )
                       ((DIGIT-CHAR-P (CHAR SUBSTR 1))
                        (LET ((N (PARSE-INTEGER SUBSTR :START 1)))
                          (IF (<= (LENGTH MATCHES) N)
                              SUBSTR ;; How coherent emacs is!
                              (REGEXP:MATCH-STRING STRING (NTH N MATCHES)))) )
                       ((CHAR= (CHARACTER "\\") (CHAR SUBSTR 1))
                        (SUBSEQ SUBSTR 1) )
                       (T
                        (ERROR "Invalid use of '\\' in replacement text ~W."
                               SUBSTR) )))
                   REPLACEMENT T T)) )
          ;; -4- Replace.
          (PUSH (SUBSEQ STRING POS
                        (REGEXP:MATCH-START (NTH SUBEXP MATCHES))) PIECES)
          (PUSH REPLACEMENT PIECES)
          (SETQ START
                (IF (= 0 (LENGTH REGEXP))
                    (1+ START)
                    (REGEXP:MATCH-END (CAR MATCHES))))
          (SETQ POS (REGEXP:MATCH-END (NTH SUBEXP MATCHES)))
          (SETQ DONE (<= (LENGTH STRING) START)) )
        (PROGN
          (SETQ DONE T) ))
    )) ;;REPLACE-REGEXP-IN-STRING




;;; (defun rris-result (test-case res)
;;;   "Emacs"
;;;   (if (or (and (eq :error res) (eq res (car test-case)))
;;;           (string= (car test-case) res))
;;;     (insert (format "Ok  %-50S --> %S\n" (cdr test-case) res))
;;;     (insert
;;;      (format "Unexpected result for %S\n    expected %S\n    got      %S\n"
;;;              (cdr test-case) (car test-case) res))))


(defun rris-result (test-case res)
  "Common-Lisp"
  (if (or (and (eq :error res) (eq res (car test-case)))
          (string= (car test-case) res))
      (format t "Ok  ~50W --> ~W~%" (cdr test-case) res)
      (format t "Unexpected result for ~W~%    expected ~W~%    got      ~W~%"
              (cdr test-case) (car test-case) res))) ;;rris-result



(defun rris-test ()
  "
Test cases for REPLACE-REGEXP-IN-STRING
"
  (let ((*case-fold-search* t))
    (do* ((test-cases
           ;; We use basic POSIX regexps, so no 're+'.
           ;;  result      regexp      replac      string      fix lit sub start
           '( ("xy"        ""          "x"         "y"         t   t)
             ("xyxyxyxy"  ""          "x"         "yyyy"      t   t)
             (""          "."         "x"         ""          t   t)
             ("x"         "."         "x"         "y"         t   t)
             ("xxxx"      "."         "x"         "yyyy"      t   t)
             ("xxxx"      "."         "x"         "yaya"      t   t)
             ("good"      "a"         "bad"       "good"      t   t)
             ("good"      "bad"       "good"      "bad"       t   t)
             ("good rest" "bad"       "good"      "bad rest"  t   t)
             ("rest good" "bad"       "good"      "rest bad"  t   t)
             ("good"      "bad"  (lambda (x) "good") "bad"    t   t)
             ("good rest" "bad"  (lambda (x) "good") "bad rest" t   t)
             ("rest good" "bad"  (lambda (x) "good") "rest bad" t   t)
             ("test"      "r\\(e\\)g" "good"      "test"     nil nil 2)
             (:error      "r\\(e\\)g" "good"      "reg"      nil nil 2)
             ("rgoodg"    "r\\(e\\)g" "good"      "reg"      nil nil 1)
             ("BOC NEW VALUE hoc" "pattern"   "nEW VAlue" "BOC PATTERN hoc")
             ("BOC nEW VAlue hoc" "pattern"   "nEW VAlue" "BOC pattern hoc")
             ("BOC new value hoc" "pattern"   "new value" "BOC pattern hoc")
             ("BOC NEW VAlue hoc" "pattern"   "nEW VAlue" "BOC Pattern hoc")
             ("BOC New Value hoc" "pattern"   "new value" "BOC Pattern hoc")
             ("BOC nEW VAlue hoc" "pattern"   "nEW VAlue" "BOC pATteRN hoc")
             ("BOC New1value hoc" "pattern"   "new1value" "BOC Pattern hoc")
             ("BOC New-Value hoc" "pattern"   "new-value" "BOC Pattern hoc")
             ("rrrr-www-bb rr-w-bbb"
              "\\(bb*\\) \\(ww*\\) \\(rr*\\)"
              "\\3-\\2-\\1" "bb www rrrr bbb w rr")
             ("\\4-www-bb \\4-w-bbb"
              "\\(bb*\\) \\(ww*\\) \\(rr*\\)"
              "\\4-\\2-\\1" "bb www rrrr bbb w rr")
             (:error "blue" "\\\\b\\l\\&" "blue box and bluetooth")
             ("\\bblue box and \\bbluetooth"
              "blue" "\\\\b\\&" "blue box and bluetooth")
             )
           (cdr test-cases))
          (test-case (car test-cases) (car test-cases))
          (tc test-case test-case)
          (all-ok t))
         ((null test-cases) all-ok)
      (when (listp (nth 2 tc))
        (setq tc (copy-seq tc))
        (setf (nth 2 tc) (coerce (nth 2 tc) 'function)))
      (let ((res (handler-case
                     (apply (function replace-regexp-in-string) (cdr tc))
                   (error () :error))) )
        (if (eq :error res)
            (unless (eq res (car test-case)) (setq all-ok nil))
            (unless (string= (car test-case) res) (setq all-ok nil)))
        (rris-result  test-case res))))) ;;rris-test



;;  (rris-test)
;;; (let ((start 0) (case-sensitive nil) (extended nil) (newline nil) (nosub nil))
;;;       (REGEXP:MATCH "blue" "blue box and bluetooth"
;;;                     :START START :IGNORE-CASE (NOT CASE-SENSITIVE)
;;;                     :EXTENDED EXTENDED  :NEWLINE NEWLINE :NOSUB NOSUB) )

;; (replace-regexp-in-string "blue" "\\\\b\\X\\&" "blue box and bluetooth")


(DEFUN STRING-TO-NUMBER (STRING &KEY (BASE 10) (START 0) (END NIL))
  "
DO:         Converts the string to a number.
RETURN:     A number.
"
  ;; PARSE-INTEGER is for integers...
  (LET ((RESULT  (WITH-INPUT-FROM-STRING
                     (STREAM STRING :START START :END END)
                   (LET ((*READ-BASE* BASE)) (READ STREAM)))))
    (UNLESS (NUMBERP RESULT)
      (ERROR "Expected a number, not ~S." RESULT))
    RESULT)
  ) ;;STRING-TO-NUMBER

;;;; string.lisp                      --                     --          ;;;;
