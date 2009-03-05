
(defun s. (&rest args)
  "Concatenate ARGS as strings"
  (apply #'concatenate 'string (mapcar #'princ-to-string args)))



