;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** IO functions

(defun readlist (&rest args)
  (values (read-from-string
	   (concatenate 'string "("
			(apply #'read-line args)
			")"))))

(defun prompt (&rest args)
  "prompts for args and reads answear"
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  "very simple REPL"
  (loop
   (let ((in (apply #'prompt args)))
     (if (funcall quit in)
	 (return)
	 (format *query-io* "~A~%" (funcall fn in))))))

(defun interactive-interpreter (&key (prompt "#> ")
				(print #'print)
				(eval #'eval)
				(read #'read)
				(input *standard-input*)
				(output *standard-output*)
				(quit-fn #'null))
  "simple configurable REPL"
  (let (* ** *** - + ++ +++ / // /// vals)
    (loop
     (fresh-line output)
     (if (stringp prompt)
	 (princ prompt)
	 (funcall prompt))
     (setf - (funcall read input))
     (when (funcall quit-fn -)
       (return (values)))
     (setf vals (multiple-value-list (funcall eval -)))
     (setf +++ ++
	   /// //
	   *** (first ///)
	   ++ +
	   // /
	   ** (first //)
	   + -
	   / vals
	   * (first /))
     (dolist (value vals)
       (funcall print value output)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  (lambda ()
    (format t ctl-string (incf num))))
