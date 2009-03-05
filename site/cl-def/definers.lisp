;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(defun function-definer-option-transformer (name)
  (bind ((name (find-symbol "TRANSFORM-FUNCTION-DEFINER-OPTIONS"
                            (symbol-package (if (consp name)
                                                (second name)
                                                name)))))
    (when (and name
               (fboundp name))
      (fdefinition name))))

(defun function-like-definer-declarations (-options-)
  (if (getf -options- :debug)
      (progn
        (when (getf -options- #\o)
          (warn "Ignoring 'O'ptimize flag because 'D'ebug was also specified"))
        '((declare (optimize (speed 0) (debug 3)))))
      (when (getf -options- :optimize)
        '((declare (optimize (speed 3) (debug 0) (safety 2)))))))

(defun function-like-definer (-definer- def-macro-name -whole- -environment- -options-)
  (declare (ignore -environment- -definer-))
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (args (pop body)))
    (awhen (function-definer-option-transformer name)
      (setf -options- (funcall it -options-)))
    (bind (((:values body declarations documentation) (parse-body body :documentation #t :whole -whole-))
           (outer-declarations (function-like-definer-declarations -options-)))
      `(progn
         ,@(when (getf -options- :inline)
                 `((declaim (inline ,name))))
         ,@(when (getf -options- :debug)
                 `((declaim (notinline ,name))))
         (locally
             ,@outer-declarations
           ,@(when (getf -options- :export)
                   `((eval-when (:compile-toplevel :load-toplevel :execute)
                       (export ',name))))
           (,def-macro-name ,name ,args
             ,@(when documentation
                     (list documentation))
             ,@declarations
             ,@body))))))

(def (definer e :available-flags "ioed") function ()
  (function-like-definer -definer- 'defun -whole- -environment- -options-))

(def (definer e :available-flags "eod") method ()
  (function-like-definer -definer- 'defmethod -whole- -environment- -options-))

(defun defmethods-like-definer (actual-definer -whole- -options-)
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (outer-declarations (function-like-definer-declarations -options-)))
    `(locally
         ,@outer-declarations
       ,@(when (getf -options- :export)
               `((export ',name)))
       ,@(iter (for entry :in body)
               (when (eq (first entry) :method)
                 (pop entry))
               (collect `(,actual-definer ,name ,@entry))))))

(def (definer e :available-flags "eod") methods ()
  (defmethods-like-definer 'defmethod -whole- -options-))

(def (definer e :available-flags "eod") macro ()
  (function-like-definer -definer- 'defmacro -whole- -environment- -options-))

(def (definer e :available-flags "eod") compiler-macro ()
  (function-like-definer -definer- 'define-compiler-macro -whole- -environment- -options-))

(def (definer e :available-flags "e") symbol-macro (name expansion &optional documentation)
  (check-type name symbol)
  (with-standard-definer-options name
    `(progn
       (define-symbol-macro ,name ,expansion)
       (setf (documentation ',name 'variable) ,documentation))))

(def (definer e :available-flags "eod") generic ()
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (outer-declarations (function-like-definer-declarations -options-)))
    `(locally
         ,@outer-declarations
       ,@(when (getf -options- :export)
               `((export ',name)))
       (defgeneric ,name ,@body))))

(defun extract-function-name (spec)
  "Useful for macros that want to emulate the functional interface for functions
like #'eq and 'eq."
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(def (definer :available-flags "e") type (name args &body forms)
  (with-standard-definer-options name
    `(deftype ,name ,args
       ,@forms)))

(def (definer :available-flags "eas") class (name supers slots &rest options)
  "Example that exports all the class name and all the readers, writers and slot names:
    (def (class eas) foo \(bar baz)
     \(\(slot1 :reader readerr)
      \(slot2 :writer writerr :accessor accessorr))
     \(:metaclass fofofo))"
  (with-class-definer-options name slots
    `(defclass ,name ,supers
       ,slots
       ,@options)))

(def (definer :available-flags "eas") condition (name supers slots &rest options)
  "See the CLASS definer."
  (with-class-definer-options name slots
    `(define-condition ,name ,supers
       ,slots
       ,@options)))

(defun %reevaluate-constant (name value &key (test 'eql))
  (if (not (boundp name))
      value
      (let ((old (symbol-value name))
            (new value))
        (if (not (constantp name))
            (prog1 new
              (cerror "Try to redefine the variable as a constant."
                      "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
            (if (funcall test old new)
                old
                (prog1 new
                  (cerror "Try to redefine the constant."
                          "~@<~S is an already defined constant whose value ~
                           ~S is not equal to the provided initial value ~S ~
                           under ~S.~:@>" name old new test)))))))

(def (definer e :available-flags "e") constant (name initial-value &optional documentation)
  "Use like: (def (constant e :test #'string=) alma \"korte\")"
  (check-type name symbol)
  (bind ((test (getf -options- :test ''eql)))
    (with-standard-definer-options name
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defconstant ,name (%reevaluate-constant ',name ,initial-value :test ,test)
           ,@(when documentation `(,documentation)))))))

(def (definer e :available-flags "e") load-time-constant (name initial-value &optional documentation)
  (check-type name symbol)
  (bind ((variable-name (format-symbol *package* "%%%~A" name)))
    (with-standard-definer-options name
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defvar ,variable-name)
           (setf (documentation ',name 'variable) ,documentation)
           (unless (boundp ',variable-name)
             (setf ,variable-name ,initial-value)))
         (define-symbol-macro ,name (load-time-value ,variable-name))))))

(def (definer e :available-flags "e") special-variable (name &optional value documentation)
  "Uses defvar/defparameter based on whether a value was provided or not, and accepts :documentation definer parameter for value-less defvars."
  (assert (not (and documentation (getf -options- :documentation))) () "Multiple documentations for ~S" -whole-)
  (setf documentation (or documentation (getf -options- :documentation)))
  (bind ((has-value? (> (length -whole-) 3)))
    (with-standard-definer-options name
      `(progn
        ,@(when documentation
            `((setf (documentation ',name 'variable) ,documentation)))
        (defvar ,name)
        (makunbound ',name)
        ,@(when has-value?
            `((setf ,name ,value)))))))

(def (definer e :available-flags "o") constructor (class-name* &body body)
  (let ((key-args (when (listp class-name*)
                    (rest class-name*)))
        (class-name (if (listp class-name*)
                        (first class-name*)
                        class-name*)))
    (bind ((declarations (function-like-definer-declarations -options-)))
      `(locally
           ,@declarations
         ;; TODO this is a bad idea: a headache for macro writing macros...
         ;; use -self- instead. same for print-object and friends...
         (defmethod initialize-instance :after ((-self- ,class-name) &key ,@key-args)
           ,@body)))))

(def (definer e) print-object (&whole whole class-name* &body body)
  "Define a PRINT-OBJECT method using PRINT-UNREADABLE-OBJECT.
  An example:
  (def print-object parenscript-dispatcher ; could be (parenscript-dispatcher :identity nil)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream printing)
    (bind ((args (ensure-list class-name*))
           ((class-name &key (identity t) (type t) with-package (muffle-errors t)) args)
           ((:values body declarations documentation) (parse-body body :documentation #t :whole whole)))
      `(defmethod print-object ((-self- ,class-name) ,stream)
         ,@(when documentation
             (list documentation))
         ,@declarations
         (print-unreadable-object (-self- ,stream :type ,type :identity ,identity)
           (let ((*standard-output* ,stream))
             (block ,printing
               (,@(if muffle-errors
                      `(handler-bind ((error (lambda (error)
                                               (declare (ignore error))
                                               (write-string "<<error printing object>>")
                                               (return-from ,printing)))))
                      `(progn))
                  (let (,@(when with-package `((*package* ,(find-package with-package)))))
                    ,@body)))))
         ;; primary PRINT-OBJECT methods are supposed to return the object
         -self-))))

(defun simple-lambda-list-p (args)
  (parse-lambda-list args
                     (lambda (kind name entry &optional external-name default)
                       (declare (ignore name entry external-name default))
                       (when (member kind '(&rest &optional &key &allow-other-keys))
                         (return-from simple-lambda-list-p #f))))
  #t)

;; TODO it should check if the &key and &optional args of the macro part were provided and
;; only forward them if when they were. otherwise let the function's default forms kick in.
;; currently you need to C-c C-c all usages if the default values changed incompatibly.
(defun expand-with-macro (name args body -options- flat must-have-args)
  (unless (or (not flat)
              (simple-lambda-list-p args))
    (error "Can not generate a flat with-macro when using &rest, &optional or &key in its lambda list. Use with-macro* for that."))
  (with-unique-names (fn with-body)
    (with-standard-definer-options name
      (bind ((call-funcion-name (format-symbol *package* "CALL-~A" name))
             (inner-arguments 'undefined))
        (labels ((process-body (form)
                   (if (consp form)
                       (cond
                         ((eq (first form) '-body-)
                          (unless (or (eq inner-arguments 'undefined)
                                      (equal inner-arguments (rest form)))
                            (error "Used -BODY- multiple times and they have different argument lists: ~S, ~S" inner-arguments (rest form)))
                          (setf inner-arguments (rest form))
                          ;; use an flet instead `(funcall ,fn ,@inner-arguments) so that #'-body- is also possible
                          `(,(first form) ,@(mapcar (lambda (el)
                                                      (first (ensure-list el)))
                                                    (rest form))))
                         ((and (eq (first form) 'function)
                               (eq (second form) '-body-)
                               (length= 2 form))
                          ;; shut up if there's a #'-body- somewhere
                          (setf inner-arguments nil)
                          form)
                         (t
                          (iter (for entry :first form :then (cdr entry))
                                (collect (process-body (car entry)) :into result)
                                (cond
                                  ((consp (cdr entry))
                                   ;; nop, go on looping
                                   )
                                  ((cdr entry)
                                   (setf (cdr (last result)) (cdr entry))
                                   (return result))
                                  (t (return result))))))
                       form)))
          (setf body (process-body body))
          (when (eq inner-arguments 'undefined)
            (simple-style-warning "You probably want to have at least one (-body-) form in the body of a WITH-MACRO to invoke the user provided body...")
            (setf inner-arguments nil))
          (bind ((args-to-remove-from-fn ())
                 (fn-args args)
                 (inner-arguments/macro-body ())
                 (inner-arguments/fn-body ()))
            (dolist (el inner-arguments)
              (if (consp el)
                  (progn
                    (unless (and (length= 2 el)
                                 (notany (lambda (part)
                                           (or (not (symbolp part))
                                               (not (symbolp part))
                                               (member part '(&rest &optional &key &allow-other-keys))))
                                         el))
                      (error "The arguemnts used to invoke (-body- foo1 foo2) may only contain symbols, or (with-macro-body-name lexically-visible-name) pairs denoting variables that are \"transferred\" from the call site in the with-macro into the lexical scope of the user provided body."))
                    (push (second el) args-to-remove-from-fn)
                    (push (first el) inner-arguments/macro-body)
                    (push (second el) inner-arguments/fn-body))
                  (progn
                    (push el inner-arguments/macro-body)
                    (push `(quote ,el) inner-arguments/fn-body))))
            (bind ()
              (dolist (arg args-to-remove-from-fn)
                (removef fn-args arg))
              (bind (((:values funcall-list rest-variable-name) (lambda-list-to-funcall-list fn-args))
                     (body-fn-name (format-symbol *package* "~A-BODY" name)))
                `(progn
                   (defun ,call-funcion-name (,fn ,@fn-args)
                     (declare (type function ,fn))
                     ,@(function-like-definer-declarations -options-)
                     (flet ((-body- (,@inner-arguments/macro-body)
                              (funcall ,fn ,@inner-arguments/macro-body)))
                       (declare (inline -body-))
                       (block ,name
                         ,@body)))
                   (defmacro ,name (,@(when (or args must-have-args)
                                            (bind ((macro-args (lambda-list-to-lambda-list-with-quoted-defaults
                                                                args)))
                                              (if flat
                                                  macro-args
                                                  (list macro-args))))
                                    &body ,with-body)
                     `(,',call-funcion-name
                       (named-lambda ,',body-fn-name ,(list ,@inner-arguments/fn-body)
                         ,@,with-body)
                       ,,@funcall-list
                       ,@,rest-variable-name)))))))))))

(def (definer e :available-flags "eod") with-macro (name args &body body)
  "(def with-macro with-foo (arg1 arg2)
     (let ((*zyz* 42)
           (local 43))
       (do something)
       (-body- local)))
   Example:
   (with-foo arg1 arg2
     (...))"
  (expand-with-macro name args body -options- #t #f))

(def (definer e :available-flags "eod") with-macro* (name args &body body)
  "(def with-macro* with-foo (arg1 arg2 &key alma)
     (let ((*zyz* 42)
           (local 43))
       (do something)
       (-body- local)))
   Example:
   (with-foo (arg1 arg2 :alma alma)
     (...))"
  (expand-with-macro name args body -options- #f #t))

(def (definer e :available-flags "e") with/without (name)
  (bind ((variable-name (concatenate-symbol "*" name "*"))
         (with-macro-name (concatenate-symbol "with-" name))
         (without-macro-name (concatenate-symbol "without-" name)))
    `(progn
       ,@(when (getf -options- :export)
               `((export '(,variable-name ,with-macro-name ,without-macro-name))))
       (defvar ,variable-name)
       (defmacro ,with-macro-name (&body forms)
         `(let ((,',variable-name #t))
            ,@forms))
       (defmacro ,without-macro-name (&body forms)
         `(let ((,',variable-name #f))
            ,@forms)))))

(def (definer e :available-flags "e") boolean-slot (class-name flag-slot-name slot-name)
  "Example:
\(def class clazz ()
  ((%flags :initform 0)))
\(def boolean-slot clazz %flags visible)
allocates a bit index in the %FLAGS slot of the class CLAZZ and generates a reader and a writer for accessing that bit."
  (bind ((accessor-name (symbolicate slot-name '#:-p))
         (slot-name->index (get class-name 'flag-slot-indices))
         (flag-index (aif (assoc (list flag-slot-name slot-name) slot-name->index :test #'equal)
                          (cdr it)
                          (bind ((max-index (or (iter (for (nil . index) :in slot-name->index)
                                                      (maximizing index))
                                                0))
                                 (index (1+ max-index))
                                 (flag-count-limit (truncate (cl:log most-positive-fixnum 2))))
                            (assert (< (1+ max-index) flag-count-limit) ()
                                    "The number or flag slots in ~S / ~S doesn't fit in a fixnum that can store ~S flags"
                                    class-name flag-slot-name flag-count-limit)
                            (push (cons (list flag-slot-name slot-name) index) (get class-name 'flag-slot-indices))
                            index))))
    (declare (type fixnum flag-index))
    (with-standard-definer-options accessor-name
      `(progn
         (def (method o) ,accessor-name ((instance ,class-name))
           (not (zerop (logand (the fixnum (slot-value instance ',flag-slot-name)) ,(expt 2 flag-index)))))
         (def (method o) (setf ,accessor-name) (new-value (instance ,class-name))
           (bind ((slot-value (slot-value instance ',flag-slot-name)))
             (declare (type fixnum slot-value))
             (setf (slot-value instance ',flag-slot-name)
                   (if new-value
                       (logior (the fixnum slot-value) ,(expt 2 flag-index))
                       (logand (the fixnum slot-value) ,(lognot (expt 2 flag-index))))))
           new-value)))))
