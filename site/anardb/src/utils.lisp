(in-package #:anardb)

(declaim (inline force-class))
(defun force-class (class)
  (typecase class
    (class class)
    (t (find-class class))))

(declaim (inline force-symbol))
(defun force-symbol (sym)
  (etypecase sym
    (symbol sym)
    (class (class-name sym))))

(declaim (inline force-string))
(defun force-string (s)
  (with-standard-io-syntax
    (write-to-string s :escape nil :readably nil :pretty nil)))

(defun all-subclasses (class)
  (let ((class (force-class class)))
    (append (list class)
	    (mapcan 'all-subclasses (closer-mop:class-direct-subclasses class)))))


