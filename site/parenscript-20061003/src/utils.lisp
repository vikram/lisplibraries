(in-package :js)

(defun list-join (list elt)
  (let (res)
    (dolist (i list)
      (push i res)
      (push elt res))
    (pop res)
    (nreverse res)))

;;; wie herrlich effizient
(defun list-to-string (list)
  (reduce #'(lambda (str1 &optional (str2 ""))
              (concatenate 'string str1 str2))
          list))

(defun append-to-last (form elt)
  (cond ((stringp form)
	 (concatenate 'string form elt))
	((consp form)
	 (let ((last (last form)))
	   (if (stringp (car last))
	       (rplaca last (concatenate 'string (car last) elt))
	       (append-to-last (car last) elt))
	   form))
	(t (error "unsupported form ~S" form))))

(defun prepend-to-first (form elt)
  (cond ((stringp form)
	 (concatenate 'string elt form))
	((consp form)
	 (let ((first (first form)))
	   (if (stringp first)
	       (rplaca form (concatenate 'string elt first))
	       (prepend-to-first first elt))
	   form))
	(t (error "unsupported form ~S" form))))

(defun string-join (strings elt)
  (list-to-string (list-join strings elt)))

(defun val-to-string (val)
  (cond ((stringp val) val)
	((symbolp val) (string-downcase (symbol-name val)))
	(t (princ-to-string val))))

(defun string-split (string separators)
  (do ((len (length string))
       (i 0 (1+ i))
       (last 0)
       res)
      ((= i len)
       (nreverse (if (> i last)
		     (cons (subseq string last i) res)
		     res)))
    (when (member (char string i) separators)
      (push (subseq string last i) res)
      (setf last (1+ i)))))


