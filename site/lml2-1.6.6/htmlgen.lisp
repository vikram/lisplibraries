;; -*- mode: common-lisp; package: lml2 -*-
;;
;; $Id$
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA
;; copyright (c) 2003 Kevin Rosenberg
;;
;; Main changes from Allegro version:
;;    - Support XHTML end tags
;;    - lowercase symbol names for attributes
;;    - Add custom tags such as :jscript, :insert-file, :load-file, :nbsp
;;    - removal of if* macro
;;    - Add attribute conditions
;;    - Automatic conversion to strings for attribute values
;;    - Convert some comments to function doc strings
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by
;; the Free Software Foundation, as clarified by the LLGPL


(in-package #:lml2)


(defstruct (html-process (:type list) (:constructor
                                       make-html-process (key has-inverse
                                                              macro special
                                                              print
                                                              name-attr
                                                              )))
  key           ; keyword naming this tag
  has-inverse   ; t if the / form is used
  macro         ; the macro to define this
  special       ; if true then call this to process the keyword and return
                ; the macroexpansion
  print         ; function used to handle this in html-print
  name-attr     ; attribute symbols which can name this object for subst purposes
  )


(defparameter *html-process-table*
    (make-hash-table :test #'equal) ; #'eq is accurate but want to avoid rehashes
  )

(defmacro html (&rest forms &environment env)
  (post-process-html-forms
   (process-html-forms forms env)))

(defun post-process-html-forms (input-forms)
  "KMR: Walk through forms and combine write-strings"
  (let (res strs last-stream)
    (flet ((flush-strings ()
             (when strs
               (push `(write-string ,strs ,last-stream) res)
               (setq strs nil)
               (setq last-stream nil))))
      (do* ((forms input-forms (cdr forms))
            (form (car forms) (car forms)))
           ((null forms)
            (flush-strings)
            (nreverse res))
        (cond
          ((atom form)
           (flush-strings)
           (push form res))
          ((and (eq (car form) 'cl:write-string)
                (stringp (cadr form)))
           (if strs
               (if (eq last-stream (third form))
                   (setq strs (concatenate 'string strs (second form)))
                   (progn
                     (flush-strings)
                     (setq strs (second form))
                     (setq last-stream (third form))))
               (progn
                 (setq strs (second form))
                 (setq last-stream (third form)))))
          (t
           (flush-strings)
           (push (post-process-html-forms form) res)))))))


(defmacro html-out-stream-check (stream)
  ;; ensure that a real stream is passed to this function
  (let ((s (gensym)))
  `(let ((,s ,stream))
     (unless (streamp ,s)
       (error "html-stream must be passed a stream object, not ~s" ,s))
    ,s)))


(defmacro html-stream (stream &rest forms)
  ;; set output stream and emit html
  `(let ((*html-stream* (html-out-stream-check ,stream))) (html ,@forms)))


(defun process-html-forms (forms env)
  (let (res)
    (flet ((do-ent (ent args argsp body)
             ;; ent is an html-process object associated with the
             ;;     html tag we're processing
             ;; args is the list of values after the tag in the form
             ;;     ((:tag &rest args) ....)
             ;; argsp is true if this isn't a singleton tag  (i.e. it has
             ;;     a body) .. (:tag ...) or ((:tag ...) ...)
             ;; body is the body if any of the form
             ;;
             (let (spec)
               (cond
                ((setq spec (html-process-special ent))
                 ;; do something different
                 (push (funcall spec ent args argsp body) res))
                ((null argsp)
                 ;; singleton tag, just do the set
                 (push `(,(html-process-macro ent) :set) res)
                 nil)
                (t
                 (cond ((equal args '(:unset))
                        ;; ((:tag :unset)) is a special case.
                        ;; that allows us to close off singleton tags
                        ;; printed earlier.
                        (push `(,(html-process-macro ent) :unset) res)
                        nil)
                       (t
                        ;; some args
                        (push `(,(html-process-macro ent)
                                ,args
                                ,(process-html-forms body env))
                              res)
                        nil)))))))


      (do* ((xforms forms (cdr xforms))
            (form (car xforms) (car xforms)))
          ((null xforms))

        (setq form (macroexpand form env))

        (if (atom form)
            (cond
             ((keywordp form)
              (let ((ent (gethash form *html-process-table*)))
                (if (null ent)
                    (error "unknown html keyword ~s" form)
                  (do-ent ent nil nil nil))))
             ((stringp form)
              ;; turn into a print of it
              (push `(write-string ,form *html-stream*) res))
             (t
              (push form res)))
          (let ((first (car form)))
            (cond
             ((keywordp first)
              ;; (:xxx . body) form
              (let ((ent (gethash first
                                  *html-process-table*)))
                (if (null ent)
                    (error "unknown html keyword ~s" form)
                  (do-ent ent nil t (cdr form)))))
             ((and (consp first) (keywordp (car first)))
              ;; ((:xxx args ) . body)
              (let ((ent (gethash (car first)
                                  *html-process-table*)))
                (if (null ent)
                    (error "unknown html keyword ~s" form)
                  (do-ent ent (cdr first) t (cdr form)))))
             (t
              (push form res)))))))
    `(progn ,@(nreverse res))))


(defun html-atom-check (args open close body)
  (when (and args (atom args))
    (let ((ans (case args
                 (:set `(write-string  ,open *html-stream*))
                 (:unset `(write-string  ,close *html-stream*))
                 (t (error "illegal arg ~s to ~s" args open)))))
      (if (and ans body)
          (error "can't have a body form with this arg: ~s" args)
        ans))))

(defun html-body-form (open close body)
  ;; used when args don't matter
  `(progn (write-string  ,open *html-stream*)
          ,@body
          (write-string  ,close *html-stream*)))


(defun attribute-name-string (name)
  (etypecase name
    (symbol (string-downcase (symbol-name name)))
    (string name)))

(defun process-attributes (args)
  (flet ((write-attribute-name-forms (name)
           `((write-char #\space *html-stream*)
             (write-string ,(attribute-name-string name)
                           *html-stream*)))
         (write-separator-forms ()
           '((write-char #\= *html-stream*)
             (write-char #\" *html-stream*))))
    (do* ((xx args (cddr xx))
          (res)
          (name (first xx) (first xx))
          (value (second xx) (second xx)))
        ((null xx)
         (nreverse res))
      (case name
        (:fformat
         (unless (and (listp value)
                      (>= (length value) 2))
           (error ":fformat must be given a list at least 2 elements"))
         (mapcar (lambda (f) (push f res))
                 (write-attribute-name-forms (first value)))
         (mapcar (lambda (f) (push f res))
                 (write-separator-forms))
         (push `(fformat *html-stream* ,(second value) ,@(cddr value))
               res)
         (push `(write-char #\" *html-stream*) res))
      (:format
       (unless (and (listp value) (>= (length value) 2))
         (error ":format must be given a list at least 2 elements"))
       (mapcar (lambda (f) (push f res))
               (write-attribute-name-forms (first value)))
       (push `(prin1-safe-http-string
               (format nil ,(second value) ,@(cddr value)))
             res))
      (:optional
       (let ((eval-if (gensym "EVAL-IF-")))
         (push `(let ((,eval-if ,(second value)))
                  (when ,eval-if
                     ,@(write-attribute-name-forms (first value))
                     (prin1-safe-http-string ,eval-if)))
               res)))
      (:if
          (unless (and (listp value)
                       (>= (length value) 3)
                       (<= (length value) 4))
            (error ":if must be given a list with 3 and 4 elements"))
          (let ((eval-if (gensym "EVAL-IF-")))
            (push `(let ((,eval-if ,(second value)))
                     ,@(write-attribute-name-forms (first value))
                     (prin1-safe-http-string
                      (if ,eval-if
                          ,(third value)
                        ,(fourth value))))
                  res)))
      (:when
          (unless (and (listp value)
                       (= (length value) 3))
            (error ":when must be given a list with 3 elements"))
        (push `(when ,(second value)
                 ,@(write-attribute-name-forms (first value))
                 (prin1-safe-http-string ,(third value)))
              res))
      (t
       (mapcar (lambda (f) (push f res))
               (write-attribute-name-forms name))
       (push `(prin1-safe-http-string ,value) res))))))

(defun html-body-key-form (string-code has-inv args body)
  ;; do what's needed to handle given keywords in the args
  ;; then do the body
  (when (and args (atom args))
    ;; single arg
    (return-from html-body-key-form
      (case args
        (:set (if has-inv
                  `(write-string  ,(format nil "<~a>" string-code)
                                  *html-stream*)
                `(write-string  ,(format nil "<~a />" string-code)
                                *html-stream*)))
        (:unset (when has-inv
                  `(write-string  ,(format nil "</~a>" string-code)
                                  *html-stream*)))
        (t (error "illegal arg ~s to ~s" args string-code)))))

  (unless (evenp (length args))
    (warn "arg list ~s isn't even" args))


  (if args
      `(progn (write-string ,(format nil "<~a" string-code)
                            *html-stream*)

              ,@(process-attributes args)

              ,(unless has-inv `(write-string " /" *html-stream*))
              (write-string ">" *html-stream*)
              ,@body
              ,(when (and body has-inv)
                 `(write-string ,(format nil "</~a>" string-code)
                                *html-stream*)))
    (if has-inv
        `(progn (write-string ,(format nil "<~a>" string-code)
                              *html-stream*)
                ,@body
                ,(when body
                   `(write-string ,(format nil "</~a>" string-code)
                                  *html-stream*)))
      `(progn (write-string ,(format nil "<~a />" string-code)
                            *html-stream*)))))



(defun princ-http (val)
  ;; print the given value to the http stream using ~a
  (format *html-stream* "~a" val))

(defun prin1-http (val)
  ;; print the given value to the http stream using ~s
  (format *html-stream* "~s" val))


(defun princ-safe-http (val)
  (emit-safe *html-stream* (format nil "~a" val)))

(defun prin1-safe-http (val)
  (emit-safe *html-stream* (format nil "~s" val)))


(defun prin1-safe-http-string (val)
  ;; used only in a parameter value situation
  ;;
  ;; if the parameter value is the symbol with the empty print name
  ;; then turn this into a singleton object.  Thus || is differnent
  ;; than "".
  ;;
  ;; print the contents inside a string double quotes (which should
  ;; not be turned into &quot;'s
  ;; symbols are turned into their name
  ;;
  ;; non-string and non-symbols are written to a string and quoted

  (unless (and (symbolp val)
               (equal "" (symbol-name val)))
    (write-char #\= *html-stream*)
    (when (not (or (stringp val)
                   (symbolp val)))
      (setq val (write-to-string val)))
    (if (or (stringp val)
            (and (symbolp val)
                 (setq val (string-downcase
                            (symbol-name val)))))
        (progn
          (write-char #\" *html-stream*)
          (emit-safe *html-stream* val)
          (write-char #\" *html-stream*))
      (prin1-safe-http val))))


(defun emit-safe (stream string)
  "Send the string to the http response stream watching out for
  special html characters and encoding them appropriately."
  (do* ((i 0 (1+ i))
        (start i)
        (end (length string)))
      ((>= i end)
       (when (< start i)
         (write-sequence string stream :start start :end i)))

    (let* ((ch (char string i))
           (cvt (case ch
                  (#\< "&lt;")
                  (#\> "&gt;")
                  (#\& "&amp;")
                  (#\" "&quot;"))))
      (when cvt
         ;; must do a conversion, emit previous chars first
        (when (< start i)
          (write-sequence string stream :start start :end i))
        (write-string cvt stream)
        (setq start (1+ i))))))



(defun html-print-list (list-of-forms stream &key unknown)
  ;; html print a list of forms
  (dolist (x list-of-forms)
    (html-print-subst x nil stream unknown)))


(defun html-print-list-subst (list-of-forms subst stream &key unknown)
  ;; html print a list of forms
  (dolist (x list-of-forms)
    (html-print-subst x subst stream unknown)))


(defun html-print (form stream &key unknown)
  (html-print-subst form nil stream unknown))


(defun html-print-subst (form subst stream unknown)
  ;; Print the given lhtml form to the given stream
  (assert (streamp stream))


  (let* ((attrs)
         (attr-name)
         (name)
         (possible-kwd (cond
                        ((atom form) form)
                        ((consp (car form))
                         (setq attrs (cdar form))
                         (caar form))
                        (t (car form))))
         print-handler
         ent)
    (when (keywordp possible-kwd)
      (if (null (setq ent (gethash possible-kwd *html-process-table*)))
          (if unknown
              (return-from html-print-subst
                (funcall unknown form stream))
            (error "unknown html tag: ~s" possible-kwd))
        ;; see if we should subst
        (when (and subst
                   attrs
                   (setq attr-name (html-process-name-attr ent))
                   (setq name (getf attrs attr-name))
                   (setq attrs (html-find-value name subst)))
          (return-from html-print-subst
            (if (functionp (cdr attrs))
                (funcall (cdr attrs) stream)
              (html-print-subst
               (cdr attrs)
               subst
               stream
               unknown)))))

      (setq print-handler
        (html-process-print ent)))

    (cond
     ((atom form)
      (cond
       ((keywordp form)
        (funcall print-handler ent :set nil nil nil nil stream))
       ((stringp form)
        (write-string form stream))
       (t
        (princ form stream))))
     (ent
      (funcall print-handler
               ent
               :full
               (when (consp (car form)) (cdr (car form)))
               form
               subst
               unknown
               stream))
     (t
      (error "Illegal form: ~s" form)))))


(defun html-find-value (key subst)
  ; find the (key . value) object in the subst list.
  ; A subst list is an assoc list ((key . value) ....)
  ; but instead of a (key . value) cons you may have an assoc list
  ;
  (let ((to-process nil)
        (alist subst))
    (loop
      (do* ((entlist alist (cdr entlist))
            (ent (car entlist) (car entlist)))
          ((null entlist) (setq alist nil))
        (cond
         ((consp (car ent))
          ;; this is another alist
          (when (cdr entlist)
            (push (cdr entlist) to-process))
          (setq alist ent)
          (return))                     ; exit do*
         ((equal key (car ent))
          (return-from html-find-value ent))))

      (when (null alist)
         ;; we need to find a new alist to process
        (if to-process
            (setq alist (pop to-process))
          (return))))))

(defun html-standard-print (ent cmd args form subst unknown stream)
  ;; the print handler for the normal html operators
  (ecase cmd
    (:set ; just turn it on
     (format stream "<~a>" (html-process-key ent)))
    (:full ; set, do body and then unset
     (let (iter)
       (if args
           (cond
            ((and (setq iter (getf args :iter))
                  (setq iter (html-find-value iter subst)))
              ;; remove the iter and pre
             (setq args (copy-list args))
             (remf args :iter)
             (funcall (cdr iter)
                      (cons (cons (caar form)
                                  args)
                            (cdr form))
                      subst
                      stream)
             (return-from html-standard-print))
            (t
             (format stream "<~a" (html-process-key ent))
             (do ((xx args (cddr xx)))
                 ((null xx))
                                        ; assume that the arg is already escaped
                                        ; since we read it
                                        ; from the parser
               (format stream " ~a=\"~a\"" (car xx) (cadr xx)))
             (format stream ">")))
         (format stream "<~a>" (html-process-key ent)))
       (dolist (ff (cdr form))
         (html-print-subst ff subst stream unknown)))
     (when (html-process-has-inverse ent)
       ;; end the form
       (format stream "</~a>" (html-process-key ent))))))








;; --  defining how html tags are handled. --
;;
;; most tags are handled in a standard way and the def-std-html
;; macro is used to define such tags
;;
;; Some tags need special treatment and def-special-html defines
;; how these are handled.  The tags requiring special treatment
;; are the pseudo tags we added to control operations
;; in the html generator.
;;
;;
;; tags can be found in three ways:
;;  :br                 - singleton, no attributes, no body
;;  (:b "foo")          - no attributes but with a body
;;  ((:a href="foo") "balh")  - attributes and body
;;



(defmacro def-special-html (kwd fcn print-fcn)
  ;; kwd - the tag we're defining behavior for.
  ;; fcn - function to compute the macroexpansion of a use of this
  ;;       tag. args to fcn are:
  ;;            ent - html-process object holding info on this tag
  ;;            args - list of attribute-values following tag
  ;;            argsp - true if there is a body in this use of the tag
  ;;            body - list of body forms.
  ;; print-fcn - function to print an lhtml form with this tag
  ;;        args to fcn are:
  ;;            ent - html-process object holding info on this tag
  ;;            cmd - one of :set, :unset, :full
  ;;            args - list of attribute-value pairs
  ;;            subst - subsitution list
  ;;            unknown - function to call for unknown tags
  ;;            stream - stream to write to
  ;;
  `(setf (gethash ,kwd *html-process-table*)
     (make-html-process ,kwd nil nil ,fcn ,print-fcn nil)))


(defmacro named-function (name &body body)
  (declare (ignore name))
  `(function ,@body))


(def-special-html :newline
    (named-function html-newline-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        (when body
          (error "can't have a body with :newline -- body is ~s" body))
        `(terpri *html-stream*)))

  (named-function html-newline-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (if (eq cmd :set)
          (terpri stream)
        (error ":newline in an illegal place: ~s" form)))))

(def-special-html :princ
    (named-function html-princ-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        `(progn ,@(mapcar #'(lambda (bod)
                              `(princ-http ,bod))
                          body))))

  (named-function html-princ-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (format stream "~a" (cadr form))
        (error ":princ must be given an argument")))))

(def-special-html :princ-safe
    (named-function html-princ-safe-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        `(progn ,@(mapcar #'(lambda (bod)
                              `(princ-safe-http ,bod))
                          body))))
  (named-function html-princ-safe-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (emit-safe stream (format nil "~a" (cadr form)))
        (error ":princ-safe must be given an argument")))))

(def-special-html :prin1
    (named-function html-prin1-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        `(progn ,@(mapcar #'(lambda (bod)
                              `(prin1-http ,bod))
                          body))))
  (named-function html-prin1-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent args unknown subst))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (format stream "~s" (cadr form))
        (error ":prin1 must be given an argument")))))

(def-special-html :prin1-safe
    (named-function html-prin1-safe-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        `(progn ,@(mapcar #'(lambda (bod)
                              `(prin1-safe-http ,bod))
                          body))))
  (named-function html-prin1-safe-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent subst unknown))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (emit-safe stream (format nil "~s" (cadr form)))
        (error ":prin1-safe must be given an argument")))))

(def-special-html :comment
    (named-function html-comment-function
      (lambda (ent args argsp body)
        ;; must use <!--   --> syntax
        (declare (ignore ent args argsp))
        `(progn (write-string "<!--" *html-stream*)
                (html ,@body)
                (write-string "-->" *html-stream*))))
  (named-function html-comment-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent cmd args subst unknown))
      (format stream "<!--~a-->" (cadr form)))))



(defmacro def-std-html (kwd has-inverse name-attrs)
  (let ((mac-name (intern (format nil "~a-~a" :with-html kwd)))
        (string-code (string-downcase (string kwd))))
    `(progn (setf (gethash ,kwd *html-process-table*)
              (make-html-process ,kwd ,has-inverse
                                     ',mac-name
                                     nil
                                     #'html-standard-print
                                     ',name-attrs))
            (defmacro ,mac-name (args &rest body)
              (html-body-key-form ,string-code ,has-inverse args body)))))



(def-std-html :a        t nil)
(def-std-html :abbr     t nil)
(def-std-html :acronym  t nil)
(def-std-html :address  t nil)
(def-std-html :applet   t nil)
(def-std-html :area    nil nil)

(def-std-html :b        t nil)
(def-std-html :base     nil nil)
(def-std-html :basefont nil nil)
(def-std-html :bdo      t nil)
(def-std-html :bgsound  nil nil)
(def-std-html :big      t nil)
(def-std-html :blink    t nil)
(def-std-html :blockquote  t nil)
(def-std-html :body      t nil)
(def-std-html :br       nil nil)
(def-std-html :button   nil nil)

(def-std-html :caption  t nil)
(def-std-html :center   t nil)
(def-std-html :cite     t nil)
(def-std-html :code     t nil)
(def-std-html :col      nil nil)
(def-std-html :colgroup nil nil)

(def-std-html :dd        t nil)
(def-std-html :del       t nil)
(def-std-html :dfn       t nil)
(def-std-html :dir       t nil)
(def-std-html :div       t nil)
(def-std-html :dl        t nil)
(def-std-html :dt        t nil)

(def-std-html :em        t nil)
(def-std-html :embed     t nil)

(def-std-html :fieldset        t nil)
(def-std-html :font        t nil)
(def-std-html :form        t :name)
(def-std-html :frame        t nil)
(def-std-html :frameset        t nil)

(def-std-html :h1        t nil)
(def-std-html :h2        t nil)
(def-std-html :h3        t nil)
(def-std-html :h4        t nil)
(def-std-html :h5        t nil)
(def-std-html :h6        t nil)
(def-std-html :head        t nil)
(def-std-html :hr        nil nil)
(def-std-html :html        t nil)

(def-std-html :i     t nil)
(def-std-html :iframe     t nil)
(def-std-html :ilayer     t nil)
(def-std-html :img     nil :id)
(def-std-html :input     nil nil)
(def-std-html :ins     t nil)
(def-std-html :isindex    nil nil)

(def-std-html :kbd      t nil)
(def-std-html :keygen   nil nil)

(def-std-html :label    t nil)
(def-std-html :layer    t nil)
(def-std-html :legend   t nil)
(def-std-html :li       t nil)
(def-std-html :link     nil nil)
(def-std-html :listing  t nil)

(def-std-html :map      t nil)
(def-std-html :marquee  t nil)
(def-std-html :menu     t nil)
(def-std-html :meta     nil nil)
(def-std-html :multicol t nil)

(def-std-html :nobr     t nil)
(def-std-html :noembed  t nil)
(def-std-html :noframes t nil)
(def-std-html :noscript t nil)

(def-std-html :object   t nil)
(def-std-html :ol       t nil)
(def-std-html :optgroup t nil)
(def-std-html :option   t nil)

(def-std-html :p        t nil)
(def-std-html :param    t nil)
(def-std-html :plaintext  nil nil)
(def-std-html :pre      t nil)

(def-std-html :q        t nil)

(def-std-html :s        t nil)
(def-std-html :samp     t nil)
(def-std-html :script   t nil)
(def-std-html :select   t nil)
(def-std-html :server   t nil)
(def-std-html :small    t nil)
(def-std-html :spacer   nil nil)
(def-std-html :span     t :id)
(def-std-html :strike   t nil)
(def-std-html :strong   t nil)
(def-std-html :style    t nil)
(def-std-html :sub      t nil)
(def-std-html :sup      t nil)

(def-std-html :table    t :name)
(def-std-html :tbody    t nil)
(def-std-html :td       t nil)
(def-std-html :textarea  t nil)
(def-std-html :tfoot    t nil)
(def-std-html :th       t nil)
(def-std-html :thead    t nil)
(def-std-html :title    t nil)
(def-std-html :tr       t nil)
(def-std-html :tt       t nil)

(def-std-html :u        t nil)
(def-std-html :ul       t nil)

(def-std-html :var      t nil)

(def-std-html :wbr      nil nil)

(def-std-html :xmp      t nil)




;;; KMR Local Additions

(def-special-html :jscript
    (named-function html-comment-function
      (lambda (ent args argsp body)
        ;; must use <!--   --> syntax
        (declare (ignore ent args argsp))
        `(progn
          #+ignore
          (write-string "<script language=\"JavaScript\" type=\"text/javascript\">" *html-stream*)
          (write-string "<script type=\"text/javascript\">" *html-stream*)
          (write-char #\newline *html-stream*)
          (write-string "// <![CDATA[" *html-stream*)
          (write-char #\newline *html-stream*)
          (html ,@body)
          (write-char #\newline *html-stream*)
          (write-string "// ]]>" *html-stream*)
          (write-char #\newline *html-stream*)
          (write-string "</script>" *html-stream*))))
  (named-function html-comment-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent cmd args subst unknown))
      (format stream "<script language=\"JavaScript\" type=\"text/javascript\">~%// <![CDATA[~%~A~%// ]]>~%</script>"
              (cadr form)))))

(def-special-html :nbsp
    (named-function html-nbsp-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        (when body
          (error "can't have a body with :nbsp -- body is ~s" body))
        `(write-string "&nbsp;" *html-stream*)))

  (named-function html-nbsp-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (if (eq cmd :set)
          (write-string "&nbsp;" stream)
        (error ":nbsp in an illegal place: ~s" form)))))


(def-special-html :load-file
    (named-function html-nbsp-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        (unless body
          (error "must have a body with :load-file"))
        `(progn ,@(mapcar #'(lambda (bod)
                              `(lml-load ,bod))
                          body))))

  (named-function html-nbsp-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent unknown subst stream args))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (lml-load (cadr form))
        (error ":load-file must be given an argument")))))

(def-special-html :insert-file
    (named-function html-nbsp-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        (unless body
          (error "must have a body with :insert-file"))
        `(progn ,@(mapcar #'(lambda (bod)
                              `(insert-file ,bod))
                          body))))

  (named-function html-nbsp-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent unknown subst stream args))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (insert-file (cadr form))
        (error ":insert-file must be given an argument")))))

(def-special-html :write-string
    (named-function html-write-string-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        (if (= (length body) 1)
            `(write-string ,(car body) *html-stream*)
          `(progn ,@(mapcar #'(lambda (bod)
                                `(write-string ,bod *html-stream*))
                            body)))))

  (named-function html-write-string-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (write-string (cadr form) stream)
          (error ":write-string must be given an argument")))))

(def-special-html :write-char
    (named-function html-write-char-function
      (lambda (ent args argsp body)
        (declare (ignore ent args argsp))
        `(progn ,@(mapcar #'(lambda (bod)
                              `(write-char ,bod *html-stream*))
                          body))))

  (named-function html-write-char-print-function
    (lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (write-char (cadr form) stream)
          (error ":write-char must be given an argument")))))



