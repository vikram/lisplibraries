(in-package :araneida)

;; (foldr (lambda (x y) (string-append x "; " y)) '(" 3" "2" "1"))
;; => 3; 2; 1
;; (foldr (lambda (x y) (string-append "(" x "; " y ")")) '(" 3" "2" "1"))
;; => ( 3; (2; 1))

(defun foldr (func list)
  "Fold FUNC over LIST, starting at the right-hand end"
  (cond ((null (cdr list))
         (car list))
	(t 
	 (funcall func (car list) (foldr func (cdr list) )))))

;; (foldl (lambda (x y) (string-append x "; " y)) '(" 3" "2" "1"))
;; => 3; 2; 1
;; (foldl (lambda (x y) (string-append "(" x "; " y ")")) '(" 3" "2" "1"))
;; => (( 3; 2); 1)

(defun foldl (f lst) (foldr (lambda (y x) (funcall f x y)) (reverse lst)))

(defun join (delimiter elements)
  "Returns a new string consisting of ELEMENTS with DELIMITERs between them.  Elements are converted to strings as if by PRINC-TO-STRING"
  (with-output-to-string (o)
    (loop for front on elements
	  if (> (length front) 1)
	  do (progn (princ (car front) o) (princ delimiter o))
	  else
	  do (princ (car front) o))))

(defun choose-best (list test &key (key 'identity))
  "Effectively equivalent to (elt (sort list test :key key) 0) but potentially faster"  
  (loop for el in list
        for so-far = el then (if (funcall test (funcall key el)
                                          (funcall key so-far))
                                 el
                               so-far)
        finally (return so-far)))

#|
(choose-best '(#\1 #\2 #\1 #\8 #\3 #\0) #'> :key #'char-code) => 8
|#
