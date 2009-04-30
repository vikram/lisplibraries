(in-package #:metatilities)

;;; whitespace-p

(defparameter +whitespace-characters+
  (list #\Space #\Newline #\Tab #\Page #\Null #\Linefeed)
  "A list of characters that should be treated as whitespace. See, 
for example, [whitespacep][].")

(defun whitespacep (char)
  "Returns true if `char` is an element of [+whitespace-characters+][]
and nil otherwise."
  (not (null (find char +whitespace-characters+ :test #'char=))))

(defun string-starts-with (string prefix &key test)
  "Returns true if `string` starts with `prefix`.

Use the keyword argument `test` (which defaults to `char=`) to check
each character."
  (setf test (or (and test (ensure-function test)) #'char=))
  (let ((mismatch (mismatch prefix string :test test)))
    (or (not mismatch) (= mismatch (length prefix)))))

(defun string-ends-with (string suffix &key test)
  "Returns true if `string` starts with `prefix`.

Use the keyword argument `test` (which defaults to `eql`) to check
each character."
  (setf test (or (and test (ensure-function test)) #'char=))
  (let ((mm 0))
    (loop for end1 from (1- (length string)) downto 0
	 for end2 from (1- (length suffix)) downto 0
       while (funcall test (aref string end1) (aref suffix end2)) do
	 (incf mm))
    (= mm (length suffix))))

(defun string-trim-if (predicate string &key (start 0) (end (length string)))
  (let ((end (1- end)))
    (loop for ch across string 
       while (funcall predicate ch) do (incf start))
    (when (< start end)
      (loop for ch = (aref string end)
         while (funcall predicate ch) do (decf end)))
    (subseq string start (1+ end))))

(defun strip-whitespace (string &key (start 0) (end (length string)))
  (string-trim-if
   #'whitespacep string :start start :end end))



#| OR

(defun string-starts-with (string prefix &key ignore-case-p)
  (declare (type string string prefix))
  (let ((prelen (length prefix)))
    (when (<= prelen (length string))
      (if ignore-case-p
        (string-equal string prefix :end1 prelen)
        (string= string prefix :end1 prelen)))))

;; not cribbed from Wilbur --cas
(defun string-ends-with (string suffix &key ignore-case-p)
  (declare (type string string suffix))
  (let ((suflen (length suffix))
        (strlen (length string)))
    (when (< suflen (length string))
      (if ignore-case-p
        (string-equal string suffix :start1 (- strlen suflen))
        (string= string suffix :start1 (- strlen suflen))))))

|#