;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file contains the ASDF system definitions for Erlisp.  To load Erlisp,
;;;; make sure this file is accessible through one of the directories in
;;;; ASDF:*CENTRAL-REGISTRY* and then simply load the ASDF system named :ERLISP
;;;; in your favorite Common Lisp implementation:
;;;;
;;;;    (asdf:operate 'asdf:load-op :erlisp)
;;;;
;;;; To run Erlisp's test suite, you'll need to have Marco Baringer's FiveAM
;;;; installed and accessible through ASDF:*CENTRAL-REGISTRY*.  Then just
;;;; evuluate:
;;;;
;;;;    (asdf:operate 'asdf:test-op :erlisp)
;;;;
;;;;----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :erlisp.asdf)
    (defpackage :erlisp.asdf
      (:use :common-lisp :asdf))))

(in-package :erlisp.asdf)

(defsystem :erlisp
  :components ((:module :src
                :components ((:file "package")
                             (:file "mailbox"
                                    :depends-on ("package"))
                             (:file "compatibility"
                                    :depends-on ("package"))
                             (:file "node"
                                    :depends-on ("package"))
                             (:file "process"
                                    :depends-on ("package" "mailbox" "node"
                                                 "compatibility"))
                             (:file "matcher"
                                    :depends-on ("package"))
                             (:file "messaging"
                                    :depends-on ("package" "matcher"
                                                 "mailbox" "process"))))))

(defsystem :erlisp.test
  :components ((:module :test
                :components ((:file "suite")
                             (:file "mailbox"
                                    :depends-on ("suite"))
                             (:file "node"
                                    :depends-on ("suite"))
                             (:file "process"
                                    :depends-on ("suite"))
                             (:file "matcher"
                                    :depends-on ("suite"))
                             (:file "messaging"
                                    :depends-on ("suite")))))
  :depends-on (:erlisp :FiveAM)
  :in-order-to ((compile-op (load-op :erlisp))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :erlisp))))
  (asdf:oos 'asdf:load-op :erlisp.test)
  (let ((5am-run! (intern "RUN!" "5AM"))) ; 5am:run!
    (funcall 5am-run! :erlisp)))

