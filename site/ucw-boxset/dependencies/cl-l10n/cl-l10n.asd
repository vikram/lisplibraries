;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-user)

(defpackage #:cl-l10n.system
  (:use #:cl #:asdf))

(in-package #:cl-l10n.system)

(defsystem cl-l10n
  :name "CL-L10N"
  :author "Sean Ross <sross@common-lisp.net>"
  :maintainer "Sean Ross <sross@common-lisp.net>"
  :version "0.3.10"
  :description "Portable CL Locale Support"
  :long-description "Portable CL Package to support localization"
  :licence "MIT"
  :components ((:file "package")
               (:file "parse-number" :depends-on ("package"))
               (:file "utils" :depends-on ("package"))
               (:file "locale" :depends-on ("utils"))
               (:file "load-locale" :depends-on ("locale"))
               (:file "printers" :depends-on ("load-locale"))
               (:file "parsers" :depends-on ("printers" "parse-number"))
               (:file "parse-time" :depends-on ("load-locale"))
               (:file "i18n" :depends-on ("printers"))
               (:module :languages
                        :components ((:file "common")
                                     (:file "english" :depends-on ("common"))
                                     (:file "hungarian" :depends-on ("common")))
                        :depends-on ("package" "utils")))
  :depends-on (:arnesi :iterate :cl-ppcre :cl-fad))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-l10n))))
  (provide 'cl-l10n))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-l10n))))
  (oos 'load-op :cl-l10n-tests)
  (oos 'test-op :cl-l10n-tests))

(defsystem cl-l10n-tests
  :depends-on (#:rt #:cl-l10n)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-l10n-tests))))
  (eval (read-from-string "(regression-test:do-tests)")))

;; EOF
