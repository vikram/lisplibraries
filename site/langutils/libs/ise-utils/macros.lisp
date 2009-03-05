;; 
;; Basic Macros from Paul Graham, etc
;;

(in-package :utils)

(defmacro-exported mac1 (expr) 
  `(pprint (macroexpand-1 ',expr)))


(defmacro-exported when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro-exported when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

(defun-exported pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(defmacro-exported defanaph (name &optional &key calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
         (body (case rule
                 (:all   `(anaphex1 args '(,opname)))
                 (:first `(anaphex2 ',opname args))
                 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

(defun-exported anaphex1 (args call)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex1 (cdr args)
                      (append call (list sym)))))
    call))

(defun-exported anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@(cdr args))))

(defun-exported anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

(defun-exported dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
                         (if (consp (car b))
                             (car b)
                             b))
                     binds)
        ,(dbind-ex (mapcan #'(lambda (b)
                               (if (consp (car b))
                                   (cdr b)))
                           binds)
                   body))))


;;
;; Creating local environments
;; 

(defmacro-exported with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
               (mapcan 
                 #'(lambda (pat)
                     (incf row)
                     (setq col -1)
                     (mapcar #'(lambda (p)
                                 `(,p (aref ,gar 
                                            ,row 
                                            ,(incf col))))
                              pat))
                 pats))
         ,@body))))

(defmacro-exported with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
                         `(,(car p) (aref ,gar ,@(cdr p))))
                     pat)
         ,@body))))

(defmacro-exported with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (slot-value ,gs ',f)))
                     fields)
         ,@body))))

;;(defmacro with-places (pat seq &body body)
;;  (let ((gseq (gensym)))
;;    `(let ((,gseq ,seq))
;;       ,(wplac-ex (destructuring-bind pat gseq #'atom) body))))

(defun-exported wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b)
                                     (if (consp (car b))
                                         (car b)
                                         b))
                                 binds)
        ,(wplac-ex (mapcan #'(lambda (b)
                               (if (consp (car b))
                                   (cdr b)))
                           binds)
                   body))))

(defmacro-exported propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro-exported propmacros (&rest props)
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p))
               props)))

(defun-exported expand-n (paramlist body)
  (let ((syms (mapcar #'car paramlist)))
    (apply #'mapcar (lambda (&rest subs) 
		      (let ((newbody (copy-tree body)))
			(loop for sym in syms and sub in subs do
			  (nsubst sub sym newbody))
			newbody)) paramlist)))

(defmacro-exported letn ((paramlist assignment) &body body)
  "A shortcut for long multiple let assignments.  The first variable in the let assignment form will be replaced with subsequent symbols and also evaluated. Example:
(letn (((x1 x2 x3) (y1 y2 y3))
       (x1 (format nil \"~A is ~A\" 'x1 'y1)))
      (loop for var in (list x1 x2 x3) do (print var)))
prints: 
\"X1 is Y1\"
\"X2 is Y2\"
\"X3 is Y3\""
  `(let (,@(expand-n paramlist assignment))
     ,@body))
