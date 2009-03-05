;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          strings.lisp
;;;; Purpose:       Strings utility functions for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: strings.lisp 11323 2006-11-29 17:31:47Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002-2006 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


(in-package #:kmrcl)

;;; Strings

(defmacro string-append (outputstr &rest args)
  `(setq ,outputstr (concatenate 'string ,outputstr ,@args)))

(defun list-to-string (lst)
  "Converts a list to a string, doesn't include any delimiters between elements"
  (format nil "~{~A~}" lst))

(defun count-string-words (str)
  (declare (simple-string str)
	   (optimize (speed 3) (safety 0) (space 0)))
  (let ((n-words 0)
	(in-word nil))
    (declare (fixnum n-words))
    (do* ((len (length str))
	  (i 0 (1+ i)))
	((= i len) n-words)
      (declare (fixnum i))
      (if (alphanumericp (schar str i))
	  (unless in-word
	    (incf n-words)
	    (setq in-word t))
	(setq in-word nil)))))

;; From Larry Hunter with modifications
(defun position-char (char string start max)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (fixnum start max) (simple-string string))
  (do* ((i start (1+ i)))
       ((= i max) nil)
    (declare (fixnum i))
    (when (char= char (schar string i)) (return i))))

(defun position-not-char (char string start max)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (fixnum start max) (simple-string string))
  (do* ((i start (1+ i)))
       ((= i max) nil)
    (declare (fixnum i))
    (when (char/= char (schar string i)) (return i))))

(defun delimited-string-to-list (string &optional (separator #\space)
						  skip-terminal)
  "split a string with delimiter"
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))
	   (type string string)
	   (type character separator))
  (do* ((len (length string))
	(output '())
	(pos 0)
	(end (position-char separator string pos len)
	     (position-char separator string pos len)))
       ((null end)
	(if (< pos len)
	    (push (subseq string pos) output)
	    (when (or (not skip-terminal) (zerop len))
	      (push "" output)))
	(nreverse output))
    (declare (type fixnum pos len)
	     (type (or null fixnum) end))
    (push (subseq string pos end) output)
    (setq pos (1+ end))))


(defun list-to-delimited-string (list &optional (separator " "))
  (format nil (concatenate 'string "~{~A~^" (string separator) "~}") list))

(defun string-invert (str)
  "Invert case of a string"
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 0))
           (simple-string str))
  (let ((up nil) (down nil))
    (block skip
      (loop for char of-type character across str do
	    (cond ((upper-case-p char)
		   (if down (return-from skip str) (setf up t)))
		  ((lower-case-p char)
		   (if up   (return-from skip str) (setf down t)))))
      (if up (string-downcase str) (string-upcase str)))))

(defun add-sql-quotes (s)
  (substitute-string-for-char s #\' "''"))

(defun escape-backslashes (s)
  (substitute-string-for-char s #\\ "\\\\"))

(defun substitute-string-for-char (procstr match-char subst-str)
  "Substitutes a string for a single matching character of a string"
  (substitute-chars-strings procstr (list (cons match-char subst-str))))

(defun string-substitute (string substring replacement-string)
  "String substitute by Larry Hunter. Obtained from Google"
  (let ((substring-length (length substring))
	(last-end 0)
	(new-string ""))
    (do ((next-start
	  (search substring string)
	  (search substring string :start2 last-end)))
	((null next-start)
	 (concatenate 'string new-string (subseq string last-end)))
      (setq new-string
	(concatenate 'string
	  new-string
	  (subseq string last-end next-start)
	  replacement-string))
      (setq last-end (+ next-start substring-length)))))

(defun string-trim-last-character (s)
  "Return the string less the last character"
  (let ((len (length s)))
    (if (plusp len)
	(subseq s 0 (1- len))
	s)))

(defun nstring-trim-last-character (s)
  "Return the string less the last character"
  (let ((len (length s)))
    (if (plusp len)
	(nsubseq s 0 (1- len))
	s)))

(defun string-hash (str &optional (bitmask 65535))
  (let ((hash 0))
    (declare (fixnum hash)
	     (simple-string str))
    (dotimes (i (length str))
      (declare (fixnum i))
      (setq hash (+ hash (char-code (char str i)))))
    (logand hash bitmask)))

(defun is-string-empty (str)
  (zerop (length str)))

(defvar *whitespace-chars* '(#\space #\tab #\return #\linefeed
			     #+allegro #\%space
			     #+lispworks #\No-Break-Space))

(defun is-char-whitespace (c)
  (declare (character c) (optimize (speed 3) (safety 0)))
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Return)
      (char= c #\Linefeed)
      #+allegro (char= c #\%space)
      #+lispworks (char= c #\No-Break-Space)))

(defun is-string-whitespace (str)
  "Return t if string is all whitespace"
  (every #'is-char-whitespace str))

(defun string-right-trim-whitespace (str)
  (string-right-trim *whitespace-chars* str))

(defun string-left-trim-whitespace (str)
  (string-left-trim *whitespace-chars* str))

(defun string-trim-whitespace (str)
  (string-trim *whitespace-chars* str))

(defun replaced-string-length (str repl-alist)
  (declare (simple-string str)
	   (optimize (speed 3) (safety 0) (space 0)))
    (do* ((i 0 (1+ i))
	  (orig-len (length str))
	  (new-len orig-len))
	 ((= i orig-len) new-len)
      (declare (fixnum i orig-len new-len))
      (let* ((c (char str i))
	     (match (assoc c repl-alist :test #'char=)))
	(declare (character c))
	(when match
	  (incf new-len (1- (length
			     (the simple-string (cdr match)))))))))

(defun substitute-chars-strings (str repl-alist)
  "Replace all instances of a chars with a string. repl-alist is an assoc
list of characters and replacement strings."
  (declare (simple-string str)
	   (optimize (speed 3) (safety 0) (space 0)))
  (do* ((orig-len (length str))
	(new-string (make-string (replaced-string-length str repl-alist)))
	(spos 0 (1+ spos))
	(dpos 0))
      ((>= spos orig-len)
       new-string)
    (declare (fixnum spos dpos) (simple-string new-string))
    (let* ((c (char str spos))
	   (match (assoc c repl-alist :test #'char=)))
      (declare (character c))
      (if match
	  (let* ((subst (cdr match))
		 (len (length subst)))
	    (declare (fixnum len)
		     (simple-string subst))
	    (dotimes (j len)
	      (declare (fixnum j))
	      (setf (char new-string dpos) (char subst j))
	      (incf dpos)))
	(progn
	  (setf (char new-string dpos) c)
	  (incf dpos))))))

(defun escape-xml-string (string)
  "Escape invalid XML characters"
  (substitute-chars-strings string '((#\& . "&amp;") (#\< . "&lt;"))))

(defun make-usb8-array (len)
  (make-array len :element-type '(unsigned-byte 8)))

(defun usb8-array-to-string (vec &key (start 0) end)
  (declare (type (simple-array (unsigned-byte 8) (*)) vec)
	   (fixnum start))
  (unless end
    (setq end (length vec)))
  (let* ((len (- end start))
	 (str (make-string len)))
    (declare (fixnum len)
	     (simple-string str)
	     (optimize (speed 3) (safety 0)))
    (do ((i 0 (1+ i)))
	((= i len) str)
      (declare (fixnum i))
      (setf (schar str i) (code-char (aref vec (the fixnum (+ i start))))))))

(defun string-to-usb8-array (str)
  (declare (simple-string str))
  (let* ((len (length str))
	 (vec (make-usb8-array len)))
    (declare (fixnum len)
	     (type (simple-array (unsigned-byte 8) (*)) vec)
	     (optimize (speed 3)))
    (do ((i 0 (1+ i)))
	((= i len) vec)
      (declare (fixnum i))
      (setf (aref vec i) (char-code (schar str i))))))

(defun concat-separated-strings (separator &rest lists)
  (format nil (concatenate 'string "~{~A~^" (string separator) "~}")
	  (append-sublists lists)))

(defun only-null-list-elements-p (lst)
  (or (null lst) (every #'null lst)))

(defun print-separated-strings (strm separator &rest lists)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
		     (compilation-speed 0)))
  (do* ((rest-lists lists (cdr rest-lists))
	(list (car rest-lists) (car rest-lists))
	(last-list (only-null-list-elements-p (cdr rest-lists))
		   (only-null-list-elements-p (cdr rest-lists))))
       ((null rest-lists) strm)
    (do* ((lst list (cdr lst))
	  (elem (car lst) (car lst))
	  (last-elem (null (cdr lst)) (null (cdr lst))))
	 ((null lst))
      (write-string elem strm)
      (unless (and last-elem last-list)
	(write-string separator strm)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-prefixed-number-string (fn-name type &optional doc)
    `(defun ,fn-name (num pchar len)
       ,@(when (stringp doc) (list doc))
       (declare (optimize (speed 3) (safety 0) (space 0))
                (fixnum len)
                (,type num))
       (when pchar
         (incf len))
       (do* ((zero-code (char-code #\0))
           (result (make-string len :initial-element #\0))
           (minus? (minusp num))
           (val (if minus? (- num) num)
                (nth-value 0 (floor val 10)))
           (pos (1- len) (1- pos))
           (mod (mod val 10) (mod val 10)))
         ((or (zerop val) (minusp pos))
          (when pchar
            (setf (schar result 0) pchar))
          (when minus? (setf (schar result (if pchar 1 0)) #\-))
          result)
       (declare (,type val) 
                (fixnum mod zero-code pos)
                (boolean minus?)
                (simple-string result))
       (setf (schar result pos) (code-char (the fixnum (+ zero-code mod))))))))

(def-prefixed-number-string prefixed-fixnum-string fixnum
 "Outputs a string of LEN digit with an optional initial character PCHAR.
Leading zeros are present. LEN must be a fixnum.")

(def-prefixed-number-string prefixed-integer-string integer
 "Outputs a string of LEN digit with an optional initial character PCHAR.
Leading zeros are present. LEN must be an integer.")

(defun integer-string (num len)
  "Outputs a string of LEN digit with an optional initial character PCHAR.
Leading zeros are present."
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type fixnum len)
	   (type integer num))
  (do* ((zero-code (char-code #\0))
	(result (make-string len :initial-element #\0))
	(minus? (minusp num))
	(val (if minus? (- 0 num) num)
	     (nth-value 0 (floor val 10)))
	(pos (1- len) (1- pos))
	(mod (mod val 10) (mod val 10)))
      ((or (zerop val) (minusp pos))
       (when minus? (setf (schar result 0) #\-))
       result)
    (declare (fixnum mod zero-code pos) (simple-string result) (integer val))
    (setf (schar result pos) (code-char (+ zero-code mod)))))

(defun fast-string-search (substr str substr-length startpos endpos)
  "Optimized search for a substring in a simple-string"
  (declare (simple-string substr str)
	   (fixnum substr-length startpos endpos)
	   (optimize (speed 3) (space 0) (safety 0)))
  (do* ((pos startpos (1+ pos))
	(lastpos (- endpos substr-length)))
       ((> pos lastpos) nil)
    (declare (fixnum pos lastpos))
    (do ((i 0 (1+ i)))
	((= i substr-length)
	 (return-from fast-string-search pos))
      (declare (fixnum i))
      (unless (char= (schar str (+ i pos)) (schar substr i))
	(return nil)))))

(defun string-delimited-string-to-list (str substr)
  "splits a string delimited by substr into a list of strings"
  (declare (simple-string str substr)
	   (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)
		     (debug 0)))
  (do* ((substr-len (length substr))
	(strlen (length str))
	(output '())
	(pos 0)
	(end (fast-string-search substr str substr-len pos strlen)
	     (fast-string-search substr str substr-len pos strlen)))
       ((null end)
	(when (< pos strlen)
	  (push (subseq str pos) output))
	(nreverse output))
    (declare (fixnum strlen substr-len pos)
	     (type (or fixnum null) end))
    (push (subseq str pos end) output)
    (setq pos (+ end substr-len))))

(defun string-to-list-skip-delimiter (str &optional (delim #\space))
  "Return a list of strings, delimited by spaces, skipping spaces."
  (declare (simple-string str)
	   (optimize (speed 0) (space 0) (safety 0)))
  (do* ((results '())
	(end (length str))
	(i (position-not-char delim str 0 end)
	   (position-not-char delim str j end))
	(j (when i (position-char delim str i end))
	   (when i (position-char delim str i end))))
       ((or (null i) (null j))
	(when (and i (< i end))
	  (push (subseq str i end) results))
	(nreverse results))
    (declare (fixnum end)
	     (type (or fixnum null) i j))
    (push (subseq str i j) results)))

(defun string-starts-with (start str)
  (and (>= (length str) (length start))
       (string-equal start str :end2 (length start))))

(defun count-string-char (s c)
  "Return a count of the number of times a character appears in a string"
  (declare (simple-string s)
	   (character c)
	   (optimize (speed 3) (safety 0)))
  (do ((len (length s))
       (i 0 (1+ i))
       (count 0))
      ((= i len) count)
    (declare (fixnum i len count))
    (when (char= (schar s i) c)
      (incf count))))

(defun count-string-char-if (pred s)
  "Return a count of the number of times a predicate is true
for characters in a string"
  (declare (simple-string s)
	   (type (or function symbol) pred)
	   (optimize (speed 3) (safety 0) (space 0)))
  (do ((len (length s))
       (i 0 (1+ i))
       (count 0))
      ((= i len) count)
    (declare (fixnum i len count))
    (when (funcall pred (schar s i))
      (incf count))))


;;; URL Encoding

(defun non-alphanumericp (ch)
  (not (alphanumericp ch)))

(defvar +hex-chars+ "0123456789ABCDEF")
(declaim (type simple-string +hex-chars+))

(defun hexchar (n)
  (declare (type (integer 0 15) n))
  (schar +hex-chars+ n))

(defconstant* +char-code-lower-a+ (char-code #\a))
(defconstant* +char-code-upper-a+ (char-code #\A))
(defconstant* +char-code-0+ (char-code #\0))
(declaim (type fixnum +char-code-0+ +char-code-upper-a+
	       +char-code-0))

(defun charhex (ch)
  "convert hex character to decimal"
  (let ((code (char-code (char-upcase ch))))
    (declare (fixnum ch))
    (if (>= code +char-code-upper-a+)
	(+ 10 (- code +char-code-upper-a+))
	(- code +char-code-0+))))

(defun binary-sequence-to-hex-string (seq)
  (let ((list (etypecase seq
                (list seq)
                (sequence (map 'list #'identity seq)))))
    (string-downcase (format nil "~{~2,'0X~}" list))))

(defun encode-uri-string (query)
  "Escape non-alphanumeric characters for URI fields"
  (declare (simple-string query)
	   (optimize (speed 3) (safety 0) (space 0)))
  (do* ((count (count-string-char-if #'non-alphanumericp query))
	(len (length query))
	(new-len (+ len (* 2 count)))
	(str (make-string new-len))
	(spos 0 (1+ spos))
	(dpos 0 (1+ dpos)))
      ((= spos len) str)
    (declare (fixnum count len new-len spos dpos)
	     (simple-string str))
    (let ((ch (schar query spos)))
      (if (non-alphanumericp ch)
	  (let ((c (char-code ch)))
	    (setf (schar str dpos) #\%)
	    (incf dpos)
	    (setf (schar str dpos) (hexchar (logand (ash c -4) 15)))
	    (incf dpos)
	    (setf (schar str dpos) (hexchar (logand c 15))))
	(setf (schar str dpos) ch)))))

(defun decode-uri-string (query)
  "Unescape non-alphanumeric characters for URI fields"
  (declare (simple-string query)
	   (optimize (speed 3) (safety 0) (space 0)))
  (do* ((count (count-string-char query #\%))
	(len (length query))
	(new-len (- len (* 2 count)))
	(str (make-string new-len))
	(spos 0 (1+ spos))
	(dpos 0 (1+ dpos)))
      ((= spos len) str)
    (declare (fixnum count len new-len spos dpos)
	     (simple-string str))
    (let ((ch (schar query spos)))
      (if (char= #\% ch)
	  (let ((c1 (charhex (schar query (1+ spos))))
		(c2 (charhex (schar query (+ spos 2)))))
	    (declare (fixnum c1 c2))
	    (setf (schar str dpos)
		  (code-char (logior c2 (ash c1 4))))
	    (incf spos 2))
	(setf (schar str dpos) ch)))))


(defun uri-query-to-alist (query)
  "Converts non-decoded URI query to an alist of settings"
  (mapcar (lambda (set)
            (let ((lst (kmrcl:delimited-string-to-list set #\=)))
              (cons (first lst) (second lst))))
          (kmrcl:delimited-string-to-list
           (kmrcl:decode-uri-string query) #\&)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +unambiguous-charset+
    "abcdefghjkmnpqrstuvwxyz123456789ABCDEFGHJKLMNPQSRTUVWXYZ")
  (defconstant* +unambiguous-length+ (length +unambiguous-charset+)))

(defun random-char (&optional (set :lower-alpha))
  (ecase set
    (:lower-alpha
     (code-char (+ +char-code-lower-a+ (random 26))))
    (:lower-alphanumeric
     (let ((n (random 36)))
       (if (>= n 26)
	   (code-char (+ +char-code-0+ (- n 26)))
	 (code-char (+ +char-code-lower-a+ n)))))
    (:upper-alpha
     (code-char (+ +char-code-upper-a+ (random 26))))
    (:unambiguous
     (schar +unambiguous-charset+ (random +unambiguous-length+)))
    (:upper-lower-alpha
     (let ((n (random 52)))
       (if (>= n 26)
	   (code-char (+ +char-code-upper-a+ (- n 26)))
	 (code-char (+ +char-code-lower-a+ n)))))))


(defun random-string (&key (length 10) (set :lower-alpha))
  "Returns a random lower-case string."
  (declare (optimize (speed 3)))
  (let ((s (make-string length)))
    (declare (simple-string s))
    (dotimes (i length s)
      (setf (schar s i) (random-char set)))))


(defun first-char (s)
  (declare (simple-string s))
  (when (and (stringp s) (plusp (length s)))
    (schar s 0)))

(defun last-char (s)
  (declare (simple-string s))
  (when (stringp s)
    (let ((len (length s)))
      (when (plusp len))
      (schar s (1- len)))))

(defun ensure-string (v)
  (typecase v
    (string v)
    (character (string v))
    (symbol (symbol-name v))
    (otherwise (write-to-string v))))

(defun string-right-trim-one-char (char str)
  (declare (simple-string str))
  (let* ((len (length str))
	 (last (1- len)))
    (declare (fixnum len last))
    (if (char= char (schar str last))
	(subseq str 0 last)
      str)))


(defun string-strip-ending (str endings)
  (if (stringp endings)
      (setq endings (list endings)))
  (let ((len (length str)))
    (dolist (ending endings str)
      (when (and (>= len (length ending))
		 (string-equal ending
			       (subseq str (- len
					      (length ending)))))
	(return-from string-strip-ending
	  (subseq str 0 (- len (length ending))))))))


(defun string-maybe-shorten (str maxlen)
  (string-elide str maxlen :end))

(defun string-elide (str maxlen position)
  (declare (fixnum maxlen))
  (let ((len (length str)))
    (declare (fixnum len))
    (cond
     ((<= len maxlen)
      str)
     ((<= maxlen 3)
      "...")
     ((eq position :middle)
      (multiple-value-bind (mid remain) (truncate maxlen 2)
	(let ((end1 (- mid 1))
	      (start2 (- len (- mid 2) remain)))
	  (concatenate 'string (subseq str 0 end1) "..." (subseq str start2)))))
     ((or (eq position :end) t)
      (concatenate 'string (subseq str 0 (- maxlen 3)) "...")))))

(defun shrink-vector (str size)
  #+allegro
  (excl::.primcall 'sys::shrink-svector str size)
  #+cmu
  (lisp::shrink-vector str size)
  #+lispworks
  (system::shrink-vector$vector str size)
  #+sbcl
  (sb-kernel:shrink-vector str size)
  #+scl
  (common-lisp::shrink-vector str size)
  #-(or allegro cmu lispworks sbcl scl)
  (setq str (subseq str 0 size))
  str)

(defun lex-string (string &key (whitespace '(#\space #\newline)))
  "Separates a string at whitespace and returns a list of strings"
  (flet ((is-sep (char) (member char whitespace :test #'char=)))
    (let ((tokens nil))
      (do* ((token-start
             (position-if-not #'is-sep string)
             (when token-end
               (position-if-not #'is-sep string :start (1+ token-end))))
            (token-end
             (when token-start
               (position-if #'is-sep string :start token-start))
             (when token-start
               (position-if #'is-sep string :start token-start))))
           ((null token-start) (nreverse tokens))
        (push (subseq string token-start token-end) tokens)))))

(defun split-alphanumeric-string (string)
  "Separates a string at any non-alphanumeric chararacter"
  (declare (simple-string string)
	   (optimize (speed 3) (safety 0)))
  (flet ((is-sep (char)
	   (declare (character char))
	   (and (non-alphanumericp char)
		(not (char= #\_ char)))))
    (let ((tokens nil))
      (do* ((token-start
             (position-if-not #'is-sep string)
             (when token-end
               (position-if-not #'is-sep string :start (1+ token-end))))
            (token-end
             (when token-start
               (position-if #'is-sep string :start token-start))
             (when token-start
               (position-if #'is-sep string :start token-start))))
           ((null token-start) (nreverse tokens))
        (push (subseq string token-start token-end) tokens)))))


(defun trim-non-alphanumeric (word)
  "Strip non-alphanumeric characters from beginning and end of a word."
  (declare (simple-string word)
	   (optimize (speed 3) (safety 0) (space 0)))
  (let* ((start 0)
	 (len (length word))
	 (end len))
    (declare (fixnum start end len))
    (do ((done nil))
	((or done (= start end)))
      (if (alphanumericp (schar word start))
	  (setq done t)
	(incf start)))
    (when (> end start)
      (do ((done nil))
	  ((or done (= start end)))
	(if (alphanumericp (schar word (1- end)))
	    (setq done t)
	  (decf end))))
    (if (or (plusp start) (/= len end))
	(subseq word start end)
      word)))


(defun collapse-whitespace (s)
  "Convert multiple whitespace characters to a single space character."
  (declare (simple-string s)
	   (optimize (speed 3) (safety 0)))
  (with-output-to-string (stream)
    (do ((pos 0 (1+ pos))
	 (in-white nil)
	 (len (length s)))
	((= pos len))
      (declare (fixnum pos len))
      (let ((c (schar s pos)))
	(declare (character c))
	(cond
	 ((kl:is-char-whitespace c)
	  (unless in-white
	    (write-char #\space stream))
	  (setq in-white t))
	 (t
	  (setq in-white nil)
	  (write-char c stream)))))))

(defun string->list (string)
  (let ((eof (list nil)))
    (with-input-from-string (stream string)
      (do ((x (read stream nil eof) (read stream nil eof))
           (l nil (cons x l)))
          ((eq x eof) (nreverse l))))))
