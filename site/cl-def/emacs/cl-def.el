;;; -*- encoding: utf-8 -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(provide 'cl-def)

;; usage example in your init.el:
;;
;; (add-to-list 'load-path (expand-file-name "~/workspace/cl-def/etc/"))
;;
;; (require 'cl-def)

(defun cl-def-lisp-mode-hook ()
  (font-lock-add-keywords
   nil `(("(\\(def\\)[ 	\n]+(\\([^) 	\n]+\\).*?)[ 	\n(]+\\(.*?\\)[ 	\n)]+"
          (1 font-lock-keyword-face)
          (2 font-lock-type-face)
          (3 font-lock-function-name-face))
         ("(\\(def\\)[ 	\n]+\\(.*?\\)[ 	\n(]+\\(.*?\\)[ 	\n)]+"
          (1 font-lock-keyword-face)
          (2 font-lock-type-face)
          (3 font-lock-function-name-face)))
   t))

(add-hook 'lisp-mode-hook 'cl-def-lisp-mode-hook)
