;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :cl-l10n)

(defparameter *ignore-categories*
  (list "LC_CTYPE" "LC_COLLATE"))

(defvar *resource-package* nil
  "Resource files will be loaded into this package. I suggest to create a package called 'lang'
and set/bind this variable to it before calling cl-l10n. Then all the defined resources will be in
this new package and you can refer to them with lang:resource-name so it's easy to search, etc.

An example:

(defpackage :lang
    (:use
      :cl
      :arnesi
      :bind
      :iterate
      :cl-l10n))")

(defmacro with-resource-package (package &body body)
  `(let ((*resource-package* (find-package ,package)))
    ,@body))

(defparameter *common-resource-file-loaded-p* nil)

(defparameter *language->default-locale-name* (make-hash-table :test #'equal)
  "This map specifies what is the default locale for locale specifications without a region (i.e. en_US for en)")

(deftype locale-descriptor ()
  `(or locale string symbol))

(defun canonical-locale-name-from (locale &optional (signal-fn #'error))
  (check-type locale locale-descriptor)
  (if (typep locale 'locale)
      (locale-name locale)
      (let ((name locale))
        (when (and (not (null name))
                   (symbolp name))
          (setf name (symbol-name name)))
        (let* ((parts (split "_" name))
               (count (list-length parts))
               (first-length (length (first parts)))
               (second-length (length (second parts))))
          (when (> count 2)
            (funcall signal-fn "Locale variants are not yet supported")
            (return-from canonical-locale-name-from nil))
          (when (or (< first-length 2)
                    (and (> count 1)
                         (or (> second-length 3)
                             (< second-length 2))))
            (funcall signal-fn "~A is not a valid locale name (examples: en_GB, en_US, en)" locale)
            (return-from canonical-locale-name-from nil))
          (let ((language (string-downcase (first parts)))
                (region (when (> count 1)
                          (second parts))))
            (if (> count 1)
                (concatenate 'string language "_" region)
                (aif (gethash language *language->default-locale-name*)
                     it
                     (concatenate 'string language "_" (string-upcase language)))))))))

;; set up the default region mappings while loading
(eval-when (:load-toplevel :execute)
  (iter (for (language locale) in '(("en" "en_US")
                                    ("English" "en_US")))
        (setf (gethash language *language->default-locale-name*)
              (canonical-locale-name-from locale)))
  (iter (for (language locale) in '(("posix" "POSIX")))
        (setf (gethash language *language->default-locale-name*) locale))
  (values))

;; Add a restart here?
(defun locale (loc-name &key (use-cache t) (errorp t) (loader nil))
  "Find locale named by the specification LOC-NAME. If USE-CACHE
is non-nil forcefully reload the locale from *locale-path* else
the locale is first looked for in *locales*. If ERRORP is non-nil
signal a warning rather than an error if the locale file cannot be found.
If LOADER is non-nil skip everything and call loader with LOC-NAME."
  (unless loc-name
    (return-from locale nil))
  (if (typep loc-name 'locale)
      loc-name
      (let ((name (canonical-locale-name-from
                   (aif (position #\. loc-name)
                        (subseq loc-name 0 it)
                        loc-name)
                   #'warn)))
        (acond ((and (not name) (not errorp)) nil)
               ((and use-cache (get-locale name)) it)
               (loader (setf (get-locale name) (funcall loader name)))
               ((probe-file (merge-pathnames *locale-path* name))
                (prog1
                    (setf (get-locale name) (load-locale name))
                  (load-resource name)))
               (t (funcall (if errorp #'error #'warn)
                           "Can't find locale ~A." name))))))

(defvar *locale-type* 'locale
  "The class of loaded locales.")

(defvar *category-type* 'category
  "The class of loaded categories")

(defun load-locale (name)
  (l10n-logger.debug "Trying to load locale ~A" name)
  (let ((locale-file (merge-pathnames *locale-path* name)))
    (l10n-logger.info "Loading locale from ~A" locale-file)
    (let ((locale (make-instance *locale-type* :name name)))
      (with-input-from-file (stream locale-file :external-format (encoding-keyword-to-native :us-ascii))
        (multiple-value-bind (escape comment) (munge-headers stream)
          (loop for header = (next-header stream)
                while header do
                (when-bind cat (make-category locale header 
                                              (parse-category header stream
                                                              escape comment))
                  (setf (get-category locale header) cat)))))
      (add-printers locale)
      (add-parsers locale)
      locale)))

(defun load-resource (name)
  (l10n-logger.debug "Trying to load resource ~A" name)
  (let ((resource-file (merge-pathnames (make-pathname :directory
                                                       '(:relative :up "resources")
                                                       :name name
                                                       :type "lisp")
                                        *locale-path*)))
    (awhen (probe-file resource-file)
      (when (pathname-name it)
        (l10n-logger.debug "Resource found at ~A" it)
        (if *resource-package*
            (let ((*package* *resource-package*))
              (l10n-logger.info "Loading resource ~A into ~A" it *resource-package*)
              (load it))
            (progn
              (l10n-logger.debug "*resource-package* is not set, skipped loading ~A" it)
              (warn "*resource-package* is not set, skipped loading resource file ~A" it))))))
  (unless *common-resource-file-loaded-p*
    (setf *common-resource-file-loaded-p* t)
    (load-resource "common")))

(defun reload-resources ()
  (let ((common-was-loaded-p *common-resource-file-loaded-p*))
    (iter (for (name nil) :in-hashtable *locales*)
          (load-resource name))
    (when common-was-loaded-p
      (load-resource "common"))))

(defun load-all-locales (&key (path *locale-path*) (ignore-errors nil) (use-cache nil))
  "Load all locale found in pathname designator PATH."
  (let ((*locale-path* path))
    (dolist (x (list-directory *locale-path*))
      (when (and (not (directory-pathname-p x)) (pathname-name x))
        (let ((locale (pathname-name x)))
          (with-simple-restart (continue "Ignore locale ~A." x)
            (handler-bind ((error (lambda (&optional c)
                                    (when ignore-errors
                                      (warn "Failed to load locale ~S, Ignoring." locale)
                                      (invoke-restart (find-restart 'continue c))))))
              (locale locale :use-cache use-cache))))))))

(defvar *default-thousands-sep* #\,)

(defun thousands-sep-char (sep)
  (if (> (length sep) 0)
      (schar sep 0)
      *default-thousands-sep*))

(defun create-number-fmt-string (locale no-ts)
  ;; TODO: Quick workaround for buggy format in openmcl which prints the
  ;; commachar even when the : modifier is not present.
  #+openmcl (when no-ts (return-from create-number-fmt-string "~A~D~{~A~}"))
  (cl:format nil "~~A~~,,'~A,~A~A~~{~~A~~}" 
             (thousands-sep-char (locale-thousands-sep locale))
             (if (minusp (locale-grouping locale)) 3 (locale-grouping locale))
             (if no-ts "D" ":D")))

(defun get-descriptors (minusp locale)
  (if minusp 
      (values (locale-n-sep-by-space locale)
              (= 1 (locale-n-cs-precedes locale))
              (locale-n-sign-posn locale)
              (locale-negative-sign locale))
      (values (locale-p-sep-by-space locale)
              (= 1 (locale-p-cs-precedes locale))
              (locale-p-sign-posn locale)
              (locale-positive-sign locale))))

(defun create-money-fmt-string (locale no-ts minusp)
  (multiple-value-bind (sep-by-space prec spos sign) 
      (get-descriptors minusp locale)
    (let ((sym-sep (if (zerop sep-by-space) "" " ")))
      (with-output-to-string (stream)
        ;; sign and sign separator
        (when (or* (= spos 0 1 3))
          (princ (if (zerop spos) "(" sign) stream)
          (when (= 2 sep-by-space)
            (princ #\Space stream)))
        ;; Sym and seperator
        (princ "~A" stream)
        (when prec
          (princ sym-sep stream))
        ;; Actual number
        ;; TODO: workaround for buggy format in openmcl
        ;; (see create-number-fmt-string above)
        #+openmcl (when no-ts (write-string "~D~{~A~}" stream))
        (unless #+openmcl no-ts #-openmcl nil
          (cl:format stream "~~,,'~A,~A~A~~{~~A~~}"
                   (thousands-sep-char (locale-mon-thousands-sep locale))
                   (if (minusp (locale-mon-grouping locale)) 3 (locale-mon-grouping locale))
                   (if no-ts "D" ":D")))
        (unless prec
          (princ sym-sep stream))
        (princ "~A" stream)
        (when (or* (= spos 0 2 4))
          (when (= 2 sep-by-space)
            (princ #\Space stream))
          (princ (if (zerop spos) ")" sign) stream))))))

(defun add-printers (locale)
  "Creates monetary and numeric format strings for locale LOCALE."
  (when (and (get-category locale "LC_MONETARY")
             (get-category locale "LC_NUMERIC"))
    ;; otherwise its an include locale (tranlit* etc)
    (setf (printers locale)
          (nconc (list :number-no-ts
                       (create-number-fmt-string locale t))
                 (list :number-ts
                       (create-number-fmt-string locale nil))
                 (list :money-p-no-ts
                       (create-money-fmt-string locale t nil))
                 (list :money-p-ts
                       (create-money-fmt-string locale nil nil))
                 (list :money-n-no-ts
                       (create-money-fmt-string locale t t))
                 (list :money-n-ts
                       (create-money-fmt-string locale nil t))
                 (printers locale)))))

(defun day-element-p (x)
  (member x '(#\d #\e)))

(defun month-element-p (x)
  (member x '(#\m #\b #\B)))

(defun year-element-p (x)
  (member x '(#\y #\Y)))

(defun element-type (char)
  (cond ((day-element-p char) 'day)
        ((month-element-p char) 'month)
        ((year-element-p char) 'year)))

(defvar date-dividers '(#\\ #\/ #\-))

;; FIXME
;; this effort definitely doesn't cover
;; every single case but it will do for now.
(defun locale-date-month-order (locale)
  (let ((fmt (locale-d-fmt locale)))
    (cond ((string= fmt "%D") '(month day year))
          ((string= fmt "%F") '(year month day))
          (t (compute-order fmt)))))

(defun compute-order (fmt)
  (let ((res nil))
    (loop for char across fmt 
          with perc = nil 
          with in-dot = nil do
          (cond ((char= char #\%) (setf perc (not perc)))
                ((member char date-dividers) nil)
                ((and perc (char= char #\.))  (setf in-dot t))
                ((and perc in-dot (char= char #\1))  
                 (setf in-dot nil))
                (perc (unless (char= char #\E)
                        ;; some locales (eg lo_LA) have this funny E before
                        ;; various time format designators. Debian 
                        ;; treats this as if it wasn't there so neither do we.
                        (let ((val (element-type char)))
                          (when val (push val res))
                          (setf perc nil))))))
    (nreverse res)))

(defun add-parsers (locale)
  (when (get-category locale "LC_TIME")
    (destructuring-bind (first second third)
        (locale-date-month-order locale)
      (setf (parsers locale)
            (list `((noon-midn) (weekday) ,first (date-divider) ,second (date-divider) ,third (noon-midn))
                  `((weekday) ,first (date-divider) ,second (date-divider) ,third hour (time-divider) minute
                    (time-divider) (secondp) (am-pm) (date-divider) (zone))
                  `(hour (time-divider) minute (time-divider) (secondp) (am-pm) (weekday) ,first (date-divider) 
                         (secondp) (date-divider) ,third (date-divider) (zone)))))))

(defvar *category-loaders*
  '(("LC_IDENTIFICATION" . load-identification)
    ("LC_MONETARY" . load-category)
    ("LC_NUMERIC" . load-category)
    ("LC_TIME" . load-category)
    ("LC_MESSAGES" . load-category)
    ("LC_PAPER" . load-category)
    ("LC_TELEPHONE" . load-category)
    ("LC_MEASUREMENT" . load-category)
    ("LC_NAME" . load-category)
    ("LC_ADDRESS" . load-category))
  "Map of category names to the function which will load them.")

(defun get-loader (name)
  (cdr (assoc name *category-loaders* :test #'string=)))

(defun make-category (locale name vals)
  (awhen (get-loader name)
    (funcall it locale name vals)))

(defgeneric load-category (locale name vals)
  (:documentation "Load a category for LOCALE using VALS.")
  (:method ((locale locale) (name string) (vals category))
    vals)
  (:method ((locale locale) (name string) (vals cons))
    (let ((cat (make-instance *category-type* :name name)))
      (dolist (x vals)
        (setf (category-value cat (car x)) (cdr x)))
      cat)))

(defvar *id-vals* 
  '(("title" . title)
    ("source" . source)
    ("language" . language)
    ("territory" . territory)
    ("revision" . revision)
    ("date" . date)
    ("categories" . categories)))

(defun load-identification (locale name vals)
  (declare (ignore name))
  (dolist (x *id-vals*)
    (aif (cdr (assoc (car x) vals :test #'string=))
         (setf (slot-value locale (cdr x)) 
               (remove #\" it)))))

(defun line-comment-p (line comment)
  (or (string= line "")
      (and (> (length line) 0)
           (char= (schar line 0) comment))))
      

(defun copy-category (cat line)
  (let ((from (trim (subseq line (position #\Space line))
                    (cons #\" *whitespace*))))
    (handler-case (let* ((locale (locale from)))
                    (or (get-category locale cat)
                        (locale-error "No category ~A in locale ~A." 
                                      cat from)))
      (error (c) (locale-error "Unable to copy Category ~A from ~A. ~A."
                               cat from c)))))

(defun parse-category (name stream escape comment)
  (let ((end (strcat "END " name))
        (ret nil))
    (loop for line = (read-line stream nil stream)
          until (eq line stream) do
      (cond ((line-comment-p line comment))
            ((search end line) (return-from parse-category ret))
            ((search "END" line) 
             (locale-error "End of wrong block reached ~S. Expected ~S." 
                    line end))
            ((and (> (length line) 3) (search "copy" line :end2 4))
             (return-from parse-category 
               (copy-category name line)))
            (t (push (get-value line stream escape) ret))))))

(defun munge-headers (stream)
  (let ((escape #\\) (comment-char #\#))
    (loop for line = (read-line stream nil stream)
          for i from 1 do
          ;; HACK We assume that if the escape and comment
          ;; lines don't appear right away that they don't exist
          ;; This is to work around lispworks being unable
          ;; to unread a line of text character by character.
      (cond ((> i 3) (return nil))
            ((line-comment-p line comment-char))
            ((search "escape_char" line)
             (setf escape 
                   (schar (cdr (get-value line stream escape)) 0)))
            ((search "comment_char" line)
             (setf comment-char
                   (schar (cdr (get-value line stream escape)) 0)))))
    (values escape comment-char)))

(defun get-full-line (line stream escape)
  (let ((length (length line)))
    (if (char= (elt line (1- length)) escape)
        (let ((next-line (read-line stream nil stream)))
          (if (eq next-line stream)
              (locale-error "EOF Looking for next line of ~A." line)
              (get-full-line (concatenate 
                              'string 
                              (subseq line 0 (1- length))
                              (trim next-line))
                             stream
                             escape)))
        line)))

(defun real-value (string)
  (loop for char across string
        with in-special = nil
        with result = ()
        with special-val = () do
        (cond ((eql char #\") nil) ;;ignore
              ((eql char #\<) (setf in-special t))
              ((and in-special (eql char #\>))
               (push (code-char 
                      (parse-integer (coerce (cdr (nreverse special-val)) 'string)
                                     :radix 16))
                     result)
               (setf in-special nil 
                     special-val nil))
              (in-special (push char special-val))
              (t (push char result)))
        finally (return (coerce (nreverse result)
                                #-lispworks 'string 
                                #+lispworks 'lw:text-string))))

(defvar *split-scanner* 
  (cl-ppcre:create-scanner '(:char-class #\;)))
                         
(defun parse-value (val)
  (let ((all-vals (split *split-scanner* val)))
    (if (singlep all-vals)
        (real-value (car all-vals))
        (mapcar #'real-value all-vals))))

(defun get-value (line stream escape)
  "Return a cons containing the key of line and its value. 
   Honors lines ending with ESCAPE"
  (let* ((line (get-full-line line stream escape))
         (first-space (position-if #'(lambda (x)
                                       (or* (char= x #\Space #\Tab))) 
                                   line)))
    (if (null first-space)
        (locale-error "No Space in line ~A." line)
        (cons (trim (subseq line 0 first-space))
              (parse-value (trim (subseq line first-space)))))))

(defun next-header (stream)
  (loop for line = (read-line stream nil stream)
        until (eq line stream) do
    (if (and (> (length line) 3) (search "LC_" line :end2 3)
             (notany #'(lambda (x)
                         (search x line :test #'string=))
                     *ignore-categories*))
        (return-from next-header (trim line)))))

(defun set-locale (locale-des)
  (setf *locale* (if (listp locale-des)
                     (loop for locale in locale-des
                           collect (locale locale))
                     (locale locale-des))))

(defun normalize-locale-list (locales)
  "Makes sure that the locale list contains the default locales. E.g. a single
en_GB is quite useless when it only contains overrides based on the default locale of
the en language (which is en_US). So we insert en_US right after en_GB. Also
removes duplicate locales."
  (macrolet ((collectedp (locale)
               `(find ,locale result :test #'eq)))
    (iter (for locale in locales)
          (for index from 0)
          (for default-locale = (locale (gethash (language locale) *language->default-locale-name*)))
          (if default-locale
              (unless (collectedp locale)
                (collect locale :into result)
                (unless (collectedp default-locale)
                  (collect default-locale :into result)))
              (unless (collectedp locale)
                (collect locale :into result)))
          (finally (return result)))))

(defmacro with-locale (locale &body body)
  (rebinding (locale)
    `(let ((*locale* (if (consp ,locale)
                         ,locale
                         (list (locale ,locale)))))
      ,@body)))

(defun load-default-locale ()
  (set-locale (get-default-locale)))

(defun get-default-locale () 
  (macrolet ((try (name)
               `(awhen (getenv ,name)
                 (locale it :errorp nil))))
    (or (try "CL_LOCALE")
        (try "LC_CTYPE")
        (try "LANG")
        (locale "POSIX" :errorp nil))))

(eval-when (:load-toplevel :execute)
  (load-default-locale))

;; EOF
