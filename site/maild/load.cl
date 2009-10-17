;; $Id: load.cl,v 1.23 2007/05/30 14:09:22 dancy Exp $

(in-package :user)

(defparameter *source-files*
    '(
      "version" "utils" "sasl" "auth" "dns" "security" "lex"
      "emailaddr" "log" "ipaddr" "blacklist" 
      "recips" "aliases" "headers" "rewrite" 
      "lock" "smtp" "queue" "mailer" 
      "greylist"
      "deliver" "deliver-smtp" 
      "queue-process" "checkers"
      "input" 
      "smtp-server" "smtp-server-checkers" 
      "bounce" "www"
      "maild" "rep-server"))


(defun compile-sources (&key load)
  (let ((excl::*break-on-warnings* t))
    (with-compilation-unit ()
      (dolist (file *source-files*)
	(compile-file-if-needed (concatenate 'string file ".cl"))
	(if load (load (concatenate 'string file ".fasl")))))))
  
(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi)
  (use-package :socket)
  (require :regexp2))


(eval-when (load eval)
  (load "config.cl")
  (compile-sources :load t))

(defun build ()
  (compile-sources)
  (generate-executable 
   "maild" 
   (append '("config.cl" :acldns :locale :srecord :regexp2
	     #-(version>= 8 1)
	     :mcombin ;; for tracing
	     )
	   (mapcar #'(lambda (f) 
		       (concatenate 'string f ".fasl"))
		   *source-files*)))
  (chmod "maild/maild" #o4555)) ;; setuid
