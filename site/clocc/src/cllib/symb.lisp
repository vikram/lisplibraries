;;; Symbols & Keywords
;;;
;;; Copyright (C) 1997-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: symb.lisp,v 1.9 2004/12/22 17:08:52 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/symb.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(export '(symbol-concat +kwd+ kwd keyword-concat read-key keyword= kill-symbol
          reset-package))

;;(defmacro symbol-concat (&rest args)
;;  (let ((lst (mapcar (lambda (zz) (if (stringp zz) zz `(string ,zz))) args)))
;;    `(intern (concatenate 'string ,@lst))))

(defsubst symbol-concat (&rest args)
  "Concatenate objects into a symbol."
  (intern (apply #'concatenate 'string (mapcar #'string args))))

(defconst +kwd+ package (find-package :keyword) "The KEYWORD package.")

(declaim (ftype (function ((or symbol string)) (values symbol symbol)) kwd))
(defsubst kwd (sy-st)
  "Convert the argument, symbol or string, to a keyword."
  (declare (type (or symbol string) sy-st))
  (when (symbolp sy-st) (unintern sy-st) (setq sy-st (symbol-name sy-st)))
  (intern sy-st +kwd+))

(declaim (ftype (function (&rest t) (values symbol symbol)) keyword-concat))
(defsubst keyword-concat (&rest args)
  "Concatenate objects into a keyword."
  (kwd (apply #'concatenate 'string (mapcar #'string args))))

(declaim (ftype (function (stream) (values symbol)) read-key))
(defsubst read-key (stream)
  "Read the symbol and make it a keyword."
  (declare (stream stream))
  (let ((*package* +kwd+)) (read stream)))

(defun keyword= (key1 key2 &key (suffix1 nil suff1p) (suffix2 nil suff2p))
  "Check whether the two keywords with the suffixes are the same."
  (eq (if suff1p (keyword-concat (code key1) (code suffix1)) (code key1))
      (if suff2p (keyword-concat (code key2) (code suffix2)) (code key2))))

(defun kill-symbol (id)
  "Kill symbol: unboud, unintern, remove properties."
  (makunbound id)
  (fmakunbound id)
  (setf (symbol-plist id) nil)
  (let ((p (symbol-package id)))
    (if p
        (unintern id p)
        (warn "~S: Killing dead symbol ~S" 'kill-symbol id))))

(defun reset-package (package &key (out *standard-output*) delete (terpri t))
  "Kill all symbols in the package with KILL-SYMBOL."
  (let ((count 0))
    (when out (format out "~&;; Cleaning ~A..." package) (force-output out))
    (do-symbols (s package)
      (when (eq package (symbol-package s))
        (kill-symbol s) (incf count)))
    (when delete
      (delete-package package))
    (when out
      (format out "done (~:D symbol~:P killed)" count)
      (when terpri (terpri out)))
    count))

(provide :cllib-symb)
;;; file symb.lisp ends here
