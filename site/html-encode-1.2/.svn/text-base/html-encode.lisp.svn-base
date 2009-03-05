;;;; See the LICENSE file for licensing information.

(defpackage :html-encode
  (:use :common-lisp)
  (:export :encode-for-pre :encode-for-tt :encode-for-http
	   :encode-for-argument :automatically-encode-non-entities))
(in-package :html-encode)

(defun encode-for-tt (string &key with-line-numbers first-char-nbsp)
  (let ((pos 0) (end (length string))
        (char nil) (last-was-newline nil))
    (flet ((next-char ()
             (prog1
                 (setf char (when (> end pos)
                              (prog1
                                  (schar string pos)
                                (incf pos))))
               (when char (setf last-was-newline (eql char #\newline))))))
      (values
       (with-output-to-string (out)
        (block nil
          (tagbody
             (unless first-char-nbsp
               (next-char)
               (go process-char))
           escape-spaces
             (next-char)
             (when (eql char #\Space)
               (write-string "&nbsp;" out)
               (go escape-spaces))
           process-char
             (case char
               ((nil) (return))
               ((#\Newline)
                (write-string "<br>" out)
                (when with-line-numbers
                  (write-string (funcall with-line-numbers) out))
                  (go escape-spaces))
               ((#\&)
                (write-string "&amp;" out))
               ((#\<)
                (write-string "&lt;" out))
               ((#\>)
                (write-string "&gt;" out))
               ((#\Tab)
                (write-string "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" out))
               ((#\Space)
                (write-char #\Space out)
                (go escape-spaces))
               ((#\Linefeed #\Return))
               (t
                (write-char char out)))
             (next-char)
             (go process-char))))
       last-was-newline))))


(defun encode-for-pre (string)
  (declare (simple-string string))
  (let ((output (make-array (truncate (length string) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (with-output-to-string (out output)
      (loop for char across string
            do (case char
                 ((#\&) (write-string "&amp;" out))
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 (t (write-char char out)))))
    (coerce output 'simple-string)))

(defun encode-for-argument (string)
  (declare (simple-string string))
  (let ((output (make-array (truncate (length string) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (with-output-to-string (out output)
      (loop for char across string
            do (case char
                 ((#\&) (write-string "&amp;" out))
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
		 ((#\") (write-string "&quot;" out))
                 (t (write-char char out)))))
    (coerce output 'simple-string)))

(defun encode-for-http (string)
  (declare (simple-string string))
  (with-output-to-string (out)
    (loop for char across string
          do (write-char char out)
          when (char= char #\>)
          do (write-char #\Newline out))))

(defun automatically-encode-non-entities (string)
  (with-output-to-string (stream)
    (let ((pos 0)
	  (max-pos (1- (length string)))
	  (buffer nil))
      (flet ((collect-five ()
	       (loop repeat 5 while (< pos max-pos) do (incf pos) do (push (elt string pos) buffer))
	       (setf buffer (nreverse buffer))))
	(loop while (<= pos max-pos)
	      if (eql (elt string pos) #\&)
	      do (progn
		   (write-char (elt string pos) stream)
		   (collect-five)
		   (if (not (find #\; buffer))
		       (progn
			 (write-string "amp;" stream)
			 (loop for char in buffer do (write-char char stream))
			 (setf buffer nil))
		       (progn
			 (loop for char in buffer do (write-char char stream))
			 (setf buffer nil))))
	      else do (write-char (elt string pos) stream)
	      do (incf pos))))))