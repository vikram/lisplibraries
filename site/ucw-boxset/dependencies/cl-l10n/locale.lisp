;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

;; TODO
;;  use LC_COLLATE to define locale-uppercase and friends
;;  Test on windows.
;;  Parsers (money)
;;  locale aliases?
;;  Optimizing print-time
;;  Handle _ and - in time directives (see date --help)
;;  Compile locales into fasl files.

(in-package :cl-l10n )

(defvar *locale-path* 
  (merge-pathnames (make-pathname :directory '(:relative "locales"))
                   (asdf:component-pathname (asdf:find-system :cl-l10n))))

(defvar *locale* nil
  "Either a locale or a list of locales in which case resources will be looked for in each locale in order.")

(defun current-locale ()
  (declare (inline current-locale))
  (if (consp *locale*)
      (car *locale*)
      *locale*))

(defvar *locales* (make-hash-table :test #'equal)
  "Hash table containing all loaded locales keyed on name (eg. \"af_ZA\")")

;; Conditions
(define-condition locale-error (error)
  ((mesg :accessor mesg :initarg :mesg :initform "Unknown."))
  (:report (lambda (obj stream) (cl:format stream "~A" (mesg obj)))))

(defun locale-error (string &rest args)
  (error 'locale-error :mesg (apply #'cl:format nil string args)))

;; Classes
(defclass locale ()
  ((locale-name :accessor locale-name :initarg :name 
                :initform (required-arg :name))
   (title :accessor title :initarg :title :initform nil)
   (printers :accessor printers :initarg :printers :initform nil)
   (parsers :accessor parsers :initarg :parsers :initform nil)
   (source :accessor source :initarg :source :initform nil)
   (language :accessor language :initarg :language :initform nil)
   (territory :accessor territory :initarg :territory :initform nil)
   (revision :accessor revision :initarg :revision :initform nil)
   (date :accessor date :initarg :date :initform nil)
   (categories :accessor categories :initarg :categories
               :initform (make-hash-table :test #'equal))))

(defmethod print-object ((obj locale) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (locale-name obj) stream)))

(defclass category ()
  ((category-name :accessor category-name :initform (required-arg :name) 
                  :initarg :name)
   (vals :accessor vals :initform (make-hash-table :test #'equal)
         :initarg :vals)))

(defmethod print-object ((obj category) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (category-name obj) stream)))


(declaim (inline get-locale))
(defun get-locale (name)
  (gethash name *locales*))

(defun (setf get-locale) (new-val name)
  (setf (gethash name *locales*)
        new-val))

(defgeneric get-category (locale name)
  (:documentation "Find category called NAME in locale LOCALE.")
  (:method ((locale locale) (name string))
    (gethash name (categories locale))))

(defmethod (setf get-category) ((new-val category) (locale locale) (name string)) 
  (setf (gethash name (categories locale))
        new-val))

(defgeneric category-value (category key)
  (:documentation "Lookup attribute named by string KEY in category CATEGORY.")
  (:method ((category category) (key string))
    (gethash key (vals category))))

(defmethod (setf category-value) ((new-val t) (category category) (key string))
  (setf (gethash key (vals category))
        new-val))

(defun locale-value (locale cat key)
  (awhen (get-category locale cat)
    (category-value it key)))

;; Getters
(defmacro defgetter (key cat &key (wrap '#'identity))
  (let ((name (intern-concat (list "LOCALE-" (substitute #\- #\_ (string-upcase key))))))
    `(progn 
       (defun ,name (&optional (locale (current-locale)))
         (let ((locale (locale locale)))
           (when locale
             (funcall ,wrap (locale-value locale ,cat ,key)))))
       (export ',name))))

(defun parse-car-or-val (x)
  (values (parse-integer (if (consp x) (car x) x))))

(defgetter "int_curr_symbol" "LC_MONETARY")
(defgetter "currency_symbol" "LC_MONETARY")
(defgetter "mon_decimal_point" "LC_MONETARY")
(defgetter "mon_thousands_sep" "LC_MONETARY")
(defgetter "mon_grouping" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "positive_sign" "LC_MONETARY")
(defgetter "negative_sign" "LC_MONETARY")
(defgetter "int_frac_digits" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "frac_digits" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "p_cs_precedes" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "p_sep_by_space" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "n_cs_precedes" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "n_sep_by_space" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "p_sign_posn" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "n_sign_posn" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "decimal_point" "LC_NUMERIC")
(defgetter "thousands_sep" "LC_NUMERIC")
(defgetter "grouping" "LC_NUMERIC" :wrap 'parse-car-or-val)
(defgetter "abday" "LC_TIME")
(defgetter "day" "LC_TIME")
(defgetter "abmon" "LC_TIME")
(defgetter "mon" "LC_TIME")
(defgetter "d_t_fmt" "LC_TIME")
(defgetter "d_fmt" "LC_TIME")
(defgetter "t_fmt" "LC_TIME")
(defgetter "am_pm" "LC_TIME")
(defgetter "t_fmt_ampm" "LC_TIME")
(defgetter "date_fmt" "LC_TIME")
(defgetter "yesexpr" "LC_MESSAGES")
(defgetter "noexpr" "LC_MESSAGES")
(defgetter "height" "LC_PAPER")
(defgetter "width" "LC_PAPER")
(defgetter "name_fmt" "LC_NAME")
(defgetter "name_gen" "LC_NAME")
(defgetter "name_mr" "LC_NAME")
(defgetter "name_mrs" "LC_NAME")
(defgetter "name_miss" "LC_NAME")
(defgetter "name_ms" "LC_NAME")
(defgetter "postal_fmt" "LC_ADDRESS")
(defgetter "tel_int_fmt" "LC_TELEPHONE")
(defgetter "measurement" "LC_MEASUREMENT")


;; EOF
