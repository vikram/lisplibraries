#+cmu (setq ext:*gc-verbose* nil)

(require :lml2)
(in-package :lml2)
(let ((cwd (parse-namestring (lml-cwd))))
  (process-dir cwd))
(lml-quit)
