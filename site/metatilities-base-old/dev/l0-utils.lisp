(in-package #:metatilities)

#-(or openmcl digitool ccl) ; already has this
(defun fixnump (arg)
  "Same as (typep arg 'fixnum).  A lot of Explorer code was written using this,
and it's easier to implement it than to change them all."
  (typep arg 'fixnum))

;;;
;;;   MACROS
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  ;;; NOTE: can't use WITH-UNIQUE-NAMES here
  ;;; XXX This is a lousy name.  Don't export.
  (defmacro with-standard-printing (&body forms &aux (package (gensym "PACKAGE")))
    "Similar to WITH-STANDARD-IO-SYNTAX, but doesn't change packages."
    `(let ((,package *package*))
       (with-standard-io-syntax
         (let ((*package* ,package))
           ,@forms))))
  
  ) ; eval-always


;;;
;;;   PREDICATES
;;;

#-(or digitool openmcl ccl)
(defun neq (left right)
  (not (eq left right)))

#-(or digitool openmcl ccl)
(declaim (inline neq))

#-(or digitool openmcl ccl)
(define-compiler-macro neq (left right)
  `(not (eq ,left ,right)))

;;;
;;;   FORMING SYMBOLS
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun form-symbol-in-package (package &rest names)
    "Finds or interns a symbol in package whose name is formed by concatenating the pretty printed representation of the names together."
    (with-standard-printing
      (intern (format nil "~{~a~}" names)
              package)))
      
  (defun form-symbol (&rest names)
    "Finds or interns a symbol in the current package whose name is formed by concatenating the pretty printed representation of the names together."
    (with-standard-printing
      (apply #'form-symbol-in-package *package* names)))
  
  (defun form-keyword (&rest names)
    "Finds or interns a symbol in the keyword package whose name is formed by concatenating the pretty printed representation of the names together."
    (with-standard-printing
      (apply #'form-symbol-in-package (load-time-value (find-package :keyword))
             names)))
  
  (defun form-uninterned-symbol (&rest names)
    "Creates and returns an uninterned symbol whose name is formed by concatenating the pretty printed representation of the names together."
    (with-standard-printing
      (make-symbol (format nil "~{~a~}" names))))
  
  ) ; eval-always


(defun current-load-file ()
  "Returns (if possible) the value of the file currently being loaded or from which
code is currently being evaluated."
  
  #+allegro excl:*source-pathname*
  #+digitool (if *load-pathname* 
               *load-pathname*
               ;; This makes it work in a fred buffer...
               ccl:*loading-file-source-file*)
  #-(or lucid allegro Genera Explorer digitool)
  *load-pathname*)

(defmacro with-unique-names ((&rest vars) &body body)
  "Binds the symbols in VARS to gensyms.  cf with-gensyms."
  (assert (every #'symbolp vars) () "Can't rebind an expression.")
  `(let ,(mapcar #'(lambda (x) `(,x (gensym* ',x))) vars)
     ,@body))

(defun ensure-list (x)
  "If `x` is a list then ensure-list returns it. Otherwise, this returns a singleton list containing `x`."
  (if (listp x) x (list x)))

(defun ensure-function (thing)
  (typecase thing
    (function thing)
    (symbol (symbol-function thing))))

;;; newsym
;;;
;;; Sometimes it's nice to have your gensyms mean something when
;;; you're reading the macroexpansion of some form.  The problem
;;; is that if you give a prefix to GENSYM it remains the prefix
;;; until you change it.  

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; the eval-when is because the newsym function is used in expanding
  ;; `with-variables' and other macros below.
  
  (defvar *newsym-counter* 0
    "Counter used by NEWSYM for generating print names.")
  
  (defun newsym (&optional (prefix "X"))
    "Create a new uninterned symbol whose print name begins with `prefix', which
may be a string or a symbol.  This differs from `gensym' in that the prefix is
not sticky."
    (unless (stringp prefix)
      (setf prefix (string prefix)))
    (make-symbol (format nil "~a~4,'0d" prefix (incf *newsym-counter*)))))


(defun export-exported-symbols (from-package to-package)
  "Make the exported symbols in from-package be also exported from to-package."
  (use-package from-package to-package)
  (do-external-symbols (sym (find-package from-package))
    (export sym to-package)))


(defgeneric length-at-least-p (thing length)
  (:documentation "Returns true if thing has no fewer than length elements in it."))


(defmethod length-at-least-p ((thing sequence) length)
  (>= (length thing) length))


(defmethod length-at-least-p ((thing cons) length)
  (let ((temp thing))
    (loop repeat (1- length)
          while temp do
          (setf temp (rest temp)))
    (not (null temp))))


(defgeneric length-at-most-p (thing length)
  (:documentation "Returns true if thing has no more than length elements in it."))


(defmethod length-at-most-p ((thing sequence) length)
  (<= (length thing) length))


(defmethod length-at-most-p ((thing cons) length)
  ;;?? cf. length-at-least-p, this seems similar
  (let ((temp thing))
    (loop repeat length
          while temp do
          (setf temp (rest temp)))
    (null temp)))


(declaim (inline length-1-list-p))
(defun length-1-list-p (x) 
  "Is x a list of length 1? Note that this is better than the naive \(= \(length x\) 1\) because we don't need to traverse the entire list..."
  (and (consp x) (null (cdr x))))


(defun nearly-zero-p (x &optional (threshold 0.0001))
  "Returns true if `x` is within threshold of 0d0."
  (declare (optimize (speed 3) (space 3) (debug 0) (safety 0))
           (dynamic-extent x threshold))
  ;; ABS conses
  (if (< 0.0 x)
    (> threshold x)
    (> x threshold)))

#+Test
(timeit (:report t)
        (loop repeat 100000 do
              (nearly-zero-p 10.1)
              (nearly-zero-p 0.00001)
              (nearly-zero-p -0.00001)))


(defun nearly-equal-p (x y threshold)
  "Returns true if x and y are within threshold of each other."
  (declare (optimize (speed 3) (space 3) (debug 0) (safety 0))
           (dynamic-extent x y threshold)
           (type double-float x y threshold))
  (let ((temp 0.0d0))
    (declare (type double-float temp)
             (dynamic-extent temp))
    (cond ((> x y)
           (setf temp (the double-float (- x y)))
           (< temp threshold))
          (t
           (setf temp (the double-float (- y x)))
           (< temp threshold)))))

#+Test
(timeit (:report t)
        (loop repeat 100000 do
              (nearly-equal-p 10.1 10.2 0.0001)
              (nearly-equal-p 10.2342345 10.234234 0.0001)))

;;; dotted-pair-p

(defun dotted-pair-p (putative-pair)
  "Returns true if and only if `putative-pair` is a dotted-list. I.e., if `putative-pair` is a cons cell with a non-nil cdr."
  (and (consp putative-pair)
       (cdr putative-pair)
       (not (consp (cdr putative-pair)))))

#+No
;;?? move to test suite 
(deftestsuite test-dotted-pair-p ()
  ()
  (:tests
   ((ensure (dotted-pair-p '(a . b))))
   ((ensure (not (dotted-pair-p '(a b)))))
   ((ensure (not (dotted-pair-p :a))))
   ((ensure (not (dotted-pair-p '(a b . c)))))
   ((ensure (not (dotted-pair-p nil))))))

(defun apply-if-exists (function package &rest args)
  "If the function `function` can be found in `package`, then apply it 
to `args`.

Returns nil if `package` does not exist or if `function` does not name a 
function in `package`. Otherwise, returns whatever `function` returns."
  (call-if-exists 'apply function package args))
				      
(defun funcall-if-exists (function package &rest args)
  "If the function `function` can be found in `package`, then funcall it 
on `args`.

Returns nil if `package` does not exist or if `function` does not name a 
function in `package`. Otherwise, returns whatever `function` returns."
  (call-if-exists 'funcall function package args))

(defun call-if-exists (call-with function package args)
  "If the function `function` can be found in `package`, then call it 
with `args`.

Returns nil if `package` does not exist or if `function` does not name a 
function in `package`. Otherwise, returns whatever `function` returns."
  (let ((package (find-package package)))
    (when package
      (let ((symbol (find-symbol (etypecase function
				   (string function)
				   (symbol (symbol-name function)))
				 package)))
	(when (and symbol (fboundp symbol))
	  (if (eq call-with 'funcall)
	      (apply #'funcall symbol args)
	      (apply #'apply symbol args)))))))

(defun iterate-over-indexes (symbol-counts fn &optional (direction :left))
  "Apply fn to lists of indexes generated from symbol counts. The counting is
done so that the first symbol varies most quickly unless the optional direction
parameter is set to :right."
  (let* ((dimension (length symbol-counts))
         (current-thing (make-list dimension :initial-element 0))
         (index-start (ecase direction (:right (1- dimension)) (:left 0)))
         (increment (ecase direction (:right -1) (:left 1)))
         (index-test (ecase direction 
                       (:right (lambda (i) (>= i 0))) 
                       (:left (lambda (i) (< i dimension))))) 
         (index index-start))
    (loop for i from 0 to 
	 (1- (reduce #'* (remove-if (complement #'plusp) symbol-counts)))
          do
          (funcall fn current-thing)
          (loop while (and (funcall index-test index)
                           (>= (incf (elt current-thing index)) 
                               (elt symbol-counts index))) do
                (setf (elt current-thing index) 0)
                (setf index (+ index increment))
                finally 
                (setf index index-start)))))

#+Experimental
(defun ioi (lists fn &optional (direction :left))
  (iterate-over-indexes
   (mapcar #'length lists)
   (lambda (indexes)
     (funcall fn (mapcar #'elt lists indexes)))
   direction))

#+Example
(ioi '((:a :b) (:g) (:d :e :f)) #'print :right)
