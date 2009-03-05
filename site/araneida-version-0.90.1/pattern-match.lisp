(in-package :araneida)

(defvar *flatten-p*)

(defmacro define-patterns (name &body body)
  `(defparameter ,name (cons ,(symbol-package name) (quote ,@body))))

;;; rewriting trees

(memo::define-memo-function pattern-match (list pattern)
  "Matches the LIST against PATTERN.  PATTERN may contain any number of
:any symbols, which denote ``at least zero of anything at this point''."
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (let ((p1 (car pattern))
        (l1 (car list)))
    (cond
     ((and (not p1) (not l1)) t)
     ((or (not p1) (not l1)) nil)
     ((eq p1 :any) (or (pattern-match list (cdr pattern) )
                       (pattern-match (cdr list) (cdr pattern) )
                       (pattern-match (cdr list) pattern)))
     ((or (string= (symbol-name l1)  (symbol-name p1))
          (subtypep l1 p1) )
      (pattern-match (cdr list) (cdr pattern) ))
     (t nil))))

#|
(pattern-match '(a b c d e f) '(a :any d :any c :any))

The source tree and replacement trees are made of elements.  Syntactically
they are equivalent, but in practice parameters are only meaningful in the
replacement tree

element := string
         = ((name attributes*) element*)
         = parameter
         = variable

parameter is a symbol whose print name starts with "?"
variable is a symbol whose print name starts with "*"

|#

(defun parameter-p (p)
  (and (symbolp p) (eql (elt (symbol-name p) 0) #\?)))

(defun variable-p (p)
  (and (symbolp p) (eql (elt (symbol-name p) 0) #\*)))

(defun wild-parameter-p (p)
  (and (parameter-p p)
       (let ((name (symbol-name p)))
         (eql (elt name (1- (length name))) #\*))))

(defun simple-el-p (el)  
  (or (not (consp el)) (stringp el) (parameter-p el)))
(defun compound-el-p (el)
  (not (simple-el-p el)))

;;; accessors for the bits of an element
(defun el-head (el)
  (if (compound-el-p el) (car el)))

(defun el-name (el)
  (if (compound-el-p el) (caar el)))

(defun el-attributes (el)
  (if (compound-el-p el)
    (loop for i on (cdar el) by #'cddr
          collect (list (car i) (cadr i)))))

(defun el-body (el)
  (if (compound-el-p el)
    (cdr el)))

(defun parameter-as-list (p)
  ;; XXX should assert that p is a parameter
  (if (eq (intern "?*") p) '(*)
    (mapcar #'intern (split (subseq (symbol-name p) 1) nil '(#\/)))))

;;; conveniences so that callers can type (b "foo") as a shorthand for
;;; ((b) "foo")

(defun canonical-el (el)
  (cond ((simple-el-p el) el)
        ((atom (car el)) (cons (list (car el)) (cdr el)))
        (t el)))

(defun canonical-tree (el)
  (let ((c (canonical-el el)))
    (if (simple-el-p c) c
      (cons (el-head c) (mapcar #'canonical-tree (el-body c))))))

;;; returns a list of tree (usually) or a list of trees (when asked for *)
(defun sub-tree (tree parameter)
  (labels ((sub-aux (tree pattern)
                    (cond ((not pattern) (list tree))
                          ((eql (car pattern) '*) (el-body tree))
                          (t (sub-aux (find (car pattern) (el-body tree)
                                            :key #'el-name)
                                      (cdr pattern))))))
    (let ((ret (sub-aux tree (parameter-as-list parameter))))
      (if (car ret) ret (list "")))))


;;; replace-bits-in always gets a single fully formed element as an
;;; argument, but it returns a list of elements and expects its caller
;;; (usually itself) to interpolate them into a bigger list.  If it is
;;; called by rewrite-tree, the interpolation doesn't happen; it just
;;; uses the car

(defun mapappend (f &rest lists)
  (apply #'append (apply #'mapcar f lists)))

(defparameter *variable-lookup-fn* nil)

(defun variable-lookup (name)
  (if *variable-lookup-fn*
      (funcall *variable-lookup-fn* name)))


(defun replace-bits-in (context template parameter-tree rules)
  (cond ((atom template) template)
        (t (loop for el in template
                 if (parameter-p el)
                 append (mapappend (lambda (x)
                                     (rewrite-tree-aux context x rules))
                                   (sub-tree parameter-tree el))
                 else if (variable-p el)
                 append (rewrite-tree-aux context (variable-lookup el) rules)
                 else collect (replace-bits-in context el parameter-tree rules)
                 ))))


(defun rewrite-tree-aux-flatten (context tree rules)
  (if (simple-el-p tree) (list tree) 
    (let* ((el (canonical-el tree))
           (context (cons (el-name el) context))
           (rule (assoc context rules :test #'pattern-match))
           (action (and rule (second rule)))
           (args (and rule (cddr rule))))
      (labels ((rewrite-body (el context rules)
                             (loop for i in (el-body el)
                                   append (rewrite-tree-aux context i rules))))
        (cond ((and rule (eq action :rewrite))
               (replace-bits-in context
                                (mapcar #'canonical-tree args) el rules))
              ((and rule (eq action :prefunc))
               (funcall (car args) el
                        (lambda (&optional (el el))
                          (rewrite-body el context rules))))
              ((and rule (eq action :postfunc))
               (apply (car args) (rewrite-body el context rules)))
              ((eq action :preserve)
               (list (cons (el-head el) (rewrite-body el context rules))))
              ((not rule)
               (rewrite-body el context rules))
              (t (error (format nil "Unknown stylesheet action ~A" action)))
              )))))

(defun rewrite-tree-aux-no-flatten (context tree rules)
  (if (simple-el-p tree) (list tree) 
    (let* ((el (canonical-el tree))
           (context (cons (el-name el) context))
           (rule (assoc context rules :test #'pattern-match))
           (action (and rule (second rule)))
           (args (and rule (cddr rule))))
      (labels ((rewrite-body (el context rules)
                             (loop for i in (el-body el)
                                   append (rewrite-tree-aux context i rules))))
        (cond ((and rule (eq action :rewrite))
               (replace-bits-in context
                                (mapcar #'canonical-tree args) el rules))
              ((and rule (eq action :prefunc))
               (funcall (car args) el
                        (lambda ()
                          (rewrite-body el context rules))))
              ((and rule (eq action :postfunc))
               (apply (car args) (rewrite-body el context rules)))
              ((not rule)
               (list (cons (el-head el) (rewrite-body el context rules))))
              (t (error (format nil "Unknown stylesheet action ~A" action)))
              )))))

(defun rewrite-tree-aux (context tree rules)
  (if *flatten-p*
      (rewrite-tree-aux-flatten context tree rules)
    (rewrite-tree-aux-no-flatten context tree rules)))
  
(defun rewrite-tree (tree rules &key variable-lookup-fn (flatten-p t) (context '(t)))
  "Transform TREE (describing an XML document fragment) according to
RULES.  Optional CONTEXT argument says which of the RULES to match on.
See also PATTERN-MATCH documentation"
  (car (rewrite-trees tree rules
                      :variable-lookup-fn variable-lookup-fn
                      :flatten-p flatten-p :context context)))

(defun rewrite-trees (tree rules &key variable-lookup-fn (flatten-p t) (context '(t)))
  (let ((*package* (car rules))
        (*print-circle* nil)
        (*variable-lookup-fn* variable-lookup-fn)
        (*flatten-p* flatten-p))
    (rewrite-tree-aux context (canonical-tree tree) (cdr rules))))

(defun xml-attr (&rest args)
  (declare (optimize (speed 3)))
  (labels ((flatten (a) (if (consp a)
                            (apply #'s. (mapcar #'flatten a))
                          a)))
    ;; surely somewhere there's a function that takes a list of
    ;; strings to concatenate?  or maybe this isn't slow really
    (let ((r (apply #'concatenate 'string
                    (loop for a on args by #'cddr
                          append (list " " (symbol-name (car a))
                                       "=\"" (flatten (cadr a)) "\"")))))
      (the simple-string r))))

;;; XXX this is basically a straight rip-off of the HTML function
;;; elsewhere in the package - and probably an old version of it, at
;;; that.  (It hasn't seen much use). The two should be coalesced.

(defun xml (things &key (case :preserve))
  "Format supplied argument as XML.  Argument may be a string (returned unchanged) or a list of (tag content) where tag may be (tagname attrs). "
  (cond ((and (consp things) (not (stringp (car things))))
         (let* ((tag (if (consp (car things)) (caar things) (car things)))
                (ctag (cond ((eql case :upcase) (string-upcase tag))
                            ((eql case :downcase) (string-downcase tag))
                            (t tag)))
                (attrs (if (consp (car things)) (cdar things) ()))
                (content (cdr things)))
           (if t #+nil (not (empty-element-p tag))
               ;; this isn't line noise.  Honest
               (format nil "<~A~A>~{~A~}</~A>~:[~;
~]"
                       ctag (xml-attr attrs)
                       (mapcar (lambda (x) (xml x :case case)) content) ctag
                       t #+nil (or 1 (member tag newline )))
             (format nil "<~A ~A>" ctag (xml-attr attrs)))))
        ((consp things)
         (apply #'concatenate 'string (mapcar #'princ-to-string things)))
        (t
         things)))

