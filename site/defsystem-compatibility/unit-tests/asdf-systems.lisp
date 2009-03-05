(in-package #:common-lisp-user)

(defpackage :defsystem-compatibility-asdf-test (:use #:asdf #:cl))
(in-package :defsystem-compatibility-asdf-test)

;;; ---------------------------------------------------------------------------

(defsystem l0-system
  :components (:module "l0" 
                       :components
                       ((:file "base-file-0")
                        (:file "base-file-1" 
                               :depends-on ("base-file-0")))))

(defsystem l1-system
  :components (:module "l1" 
                       :components
                       ((:file "file-0")
                        (:file "file-1" 
                               :depends-on ("file-0"))))
  :depends-on (l0-system))
