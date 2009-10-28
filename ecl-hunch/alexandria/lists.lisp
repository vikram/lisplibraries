(in-package :alexandria)

(defun alist-plist (alist)
  "Returns a property list containing the same keys and values as the
association list ALIST in the same order."
  (let (plist)
    (dolist (pair alist)
      (push (car pair) plist)
      (push (cdr pair) plist))
    (nreverse plist)))

(defun plist-alist (plist)
  "Returns an association list containing the same keys and values as the
property list PLIST in the same order."
  (let (alist)
    (do ((tail plist (cddr tail)))
        ((endp tail) (nreverse alist))
      (push (cons (car tail) (cadr tail)) alist))))

(defun malformed-plist (plist)
  (error "Malformed plist: ~S" plist))

(defmacro doplist ((key val plist &optional values) &body body)
  "Iterates over elements of PLIST. BODY can be preceded by
declarations, and is like a TAGBODY. RETURN may be used to terminate
the iteration early. If RETURN is not used, returns VALUES."
  (multiple-value-bind (forms declarations) (parse-body body)
    (with-gensyms (tail loop results)
      `(block nil
         (flet ((,results ()
                  (let (,key ,val)
                    (declare (ignorable ,key ,val))
                    (return ,values))))
           (let* ((,tail ,plist)
                  (,key (if ,tail
                            (pop ,tail)
                            (,results)))
                 (,val (if ,tail
                           (pop ,tail)
                           (malformed-plist ',plist))))
            (declare (ignorable ,key ,val))
            ,@declarations
            (tagbody
               ,loop
               ,@forms
               (setf ,key (if ,tail
                              (pop ,tail)
                              (,results))
                     ,val (if ,tail
                              (pop ,tail)
                              (malformed-plist ',plist)))
               (go ,loop))))))))

(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by the first
argument.")

(define-modify-macro nconcf (&rest lists) nconc
  "Modify-macro for NCONC. Concatenates LISTS to place designated by the first
argument.")

(define-modify-macro unionf (list) union
  "Modify-macro for UNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place.")

(define-modify-macro nunionf (list) nunion
  "Modify-macro for NUNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place. May modify
either argument.")

(defun circular-list (&rest elements)
  "Creates a circular list of ELEMENTS."
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

(defun circular-list-p (object)
  "Returns true if OBJECT is a circular list, NIL otherwise."
  (and (listp object)
       (do ((fast object (cddr fast))
            (slow (cons (car object) (cdr object)) (cdr slow)))
           (nil)
         (unless (and (consp fast) (listp (cdr fast)))
           (return nil))
         (when (eq fast slow)
           (return t)))))

(defun circular-tree-p (object)
  "Returns true if OBJECT is a circular tree, NIL otherwise."
  (labels ((circularp (object seen)
             (and (consp object)
                  (do ((fast (cons (car object) (cdr object)) (cddr fast))
                       (slow object (cdr slow)))
                      ((or (not (consp fast)) (not (consp (cdr slow))))
                       (do ((tail object (cdr tail)))
                           ((not (consp tail))
                            nil)
                         (let ((elt (car tail)))
                           (circularp elt (cons object seen)))))
                    (when (or (eq fast slow) (member slow seen))
                      (return-from circular-tree-p t))))))
    (circularp object nil)))

(defun proper-list-p (object)
  "Returns true if OBJECT is a proper list."
  (cond ((not object)
         t)
        ((consp object)
         (do ((fast object (cddr fast))
              (slow (cons (car object) (cdr object)) (cdr slow)))
             (nil)
           (unless (and (listp fast) (consp (cdr fast)))
             (return (and (listp fast) (not (cdr fast)))))
           (when (eq fast slow)
             (return nil))))
        (t
         nil)))

(deftype proper-list ()
  "Type designator for proper lists. Implemented as a SATISFIES type, hence
not recommended for performance intensive use. Main usefullness as a type
designator of the expected type in a TYPE-ERROR."
  `(and list (satisfies proper-list-p)))

(defun lastcar (list)
  "Returns the last element of LIST. Signals a type-error if LIST is not a
proper list."
  (do ((last list fast)
       (fast list (cddr fast))
       (slow (cons (car list) (cdr list)) (cdr slow)))
      (nil)
    (when (endp fast)
      (return (cadr last)))
    (when (endp (cdr fast))
      (return (car fast)))
    (when (eq fast slow)
      (error 'type-error
             :datum list
             :expected-type '(and list (not circular-list))))))

(defun (setf lastcar) (object list)
  "Sets the last element of LIST. Signals a type-error if LIST is not a proper
list."
  (do ((last list fast)
       (fast list (cddr fast))
       (slow (cons (car list) (cdr list)) (cdr slow)))
      (nil)
    (when (endp fast)
      (return (setf (cadr last) object)))
    (when (endp (cdr fast))
      (return (setf (car fast) object)))
    (when (eq fast slow)
      (error 'type-error
             :datum list
             :expected-type '(and list (not circular-list))))))

(defun make-circular-list (length &key initial-element)
  "Creates a circular list of LENGTH with the given INITIAL-ELEMENT."
  (let ((cycle (make-list length :initial-element initial-element)))
    (nconc cycle cycle)))

(deftype circular-list ()
  "Type designator for circular lists. Implemented as a SATISFIES type, so not
recommended for performance intensive use. Main usefullness as the
expected-type designator of a TYPE-ERROR."
  `(satisfies circular-list-p))

(defun ensure-car (thing)
  "If THING is a CONS, its CAR is returned. Otherwise THING is returned."
  (if (consp thing)
      (car thing)
      thing))

(defun ensure-cons (cons)
  "If CONS is a cons, it is returned. Otherwise returns a fresh cons with CONS
  in the car, and NIL in the cdr."
  (if (consp cons)
      cons
      (cons cons nil)))

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))

(defun remove-from-plist (plist &rest keys)
  "Returns a propery-list with same keys and values as PLIST, except that keys
in the list designated by KEYS and values corresponding to them are removed.
The returned property-list may share structure with the PLIST, but PLIST is
not destructively modified. Keys are compared using EQ."
  (declare (optimize (speed 3)))
  ;; FIXME: possible optimization: (remove-from-plist '(:x 0 :a 1 :b 2) :a)
  ;; could return the tail without consing up a new list.
  (loop for (key . rest) on plist by #'cddr
        do (assert rest () "Expected a proper plist, got ~S" plist)
        unless (member key keys :test #'eq)
        collect key and collect (first rest)))

(defun delete-from-plist (plist &rest keys)
  "Just like REMOVE-FROM-PLIST, but this version may destructively modify the
provided plist."
  ;; FIXME: should not cons
  (apply 'remove-from-plist plist keys))

(define-modify-macro remove-from-plistf (&rest keys) remove-from-plist)
(define-modify-macro delete-from-plistf (&rest keys) delete-from-plist)

(declaim (inline sans))
(defun sans (plist &rest keys)
  "Alias of REMOVE-FROM-PLIST for backward compatibility."
  (apply #'remove-from-plist plist keys))

(defun mappend (function &rest lists)
  "Applies FUNCTION to respective element(s) of each LIST, appending all the
all the result list to a single list. FUNCTION must return a list."
  (loop for results in (apply #'mapcar function lists)
        append results))

(defun setp (object &key (test #'eql) (key #'identity))
  "Returns true if OBJECT is a list that denotes a set, NIL otherwise. A list
denotes a set if each element of the list is unique under KEY and TEST."
  (and (listp object)
       (let (seen)
         (dolist (elt object t)
           (let ((key (funcall key elt)))
             (if (member key seen :test test)
                 (return nil)
                 (push key seen)))))))

(defun set-equal (list1 list2 &key (test #'eql) (key nil keyp))
  "Returns true if every element of LIST1 matches some element of LIST2 and
every element of LIST2 matches some element of LIST1. Otherwise returns false."
  (let ((keylist1 (if keyp (mapcar key list1) list1))
        (keylist2 (if keyp (mapcar key list2) list2)))
    (and (dolist (elt keylist1 t)
           (or (member elt keylist2 :test test)
               (return nil)))
         (dolist (elt keylist2 t)
           (or (member elt keylist1 :test test)
               (return nil))))))

(defun map-product (function list &rest more-lists)
  "Returns a list containing the results of calling FUNCTION with one argument
from LIST, and one from each of MORE-LISTS for each combination of arguments.
In other words, returns the product of LIST and MORE-LISTS using FUNCTION.

Example:

 (map-product 'list '(1 2) '(3 4) '(5 6)) => ((1 3 5) (1 3 6) (1 4 5) (1 4 6)
                                              (2 3 5) (2 3 6) (2 4 5) (2 4 6))
"
  (labels ((%map-product (f lists)
             (let ((more (cdr lists))
                   (one (car lists)))
               (if (not more)
                   (mapcar f one)
                   (mappend (lambda (x)
                              (%map-product (curry f x) more))
                            one)))))
    (%map-product (if (functionp function)
                      function
                      (fdefinition function))
                  (cons list more-lists))))

(defun flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))
