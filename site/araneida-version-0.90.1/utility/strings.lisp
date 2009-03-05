(in-package :araneida)

(defun s. (&rest args)
  "Concatenate ARGS as strings"
  (declare (optimize (speed 3)))
  (let ((*print-pretty* nil))
    (with-output-to-string (out)
      (dolist (arg args)
        (princ arg out)))))

(defun remove-if-empty (strings)
  (remove-if (lambda (x) (= (length x) 0)) strings))

