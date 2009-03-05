;;; -*- lisp -*-

;;;; ASDF system definition file for UCW


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :it.bese.ucw.system)
    (defpackage :it.bese.ucw.system
      (:nicknames :ucw.system)
      (:use :common-lisp :asdf))))

(in-package :it.bese.ucw.system)

(defsystem :UCW
  :description "UnCommon Web - A Common Lisp Web Application Framework."
  :long-description "A continuation based, component oriented
dynamic web application framework written in Common Lisp."
  :author "Marco Baringer <mb@bese.it>"
  :licence "BSD (sans advertising clause)"
  :version "0.3.9"
  :components
  ((:module :src
    :components ((:file "packages")
		 (:file "config" :depends-on ("packages" "vars"))
                 (:file "helpers" :depends-on ("packages" ))
                 (:file "loggers" :depends-on ("packages" "vars"))
                 (:file "vars" :depends-on ("packages"))
                 (:file "control" :depends-on ("config" :backend :rerl))
                 (:module :backend
                  :components ((:file "accept"))
                  :depends-on ("packages" "loggers" :rerl))
                 (:module :rerl
                  :components ((:file "conditions" :depends-on ("protocol"))
                               (:file "backtracking" :depends-on ("standard-classes"))
                               (:file "protocol")
                               (:file "request-loop-error" :depends-on ("conditions"))
                               (:file "standard-classes" :depends-on ("protocol"
                                                                      "standard-vars"))
                               (:file "standard-action" :depends-on ("protocol"
                                                                     "standard-classes"))
                               (:file "standard-application" :depends-on ("protocol"
                                                                          "standard-classes"
                                                                          "standard-vars"))
                               (:module :standard-component
                                        :components ((:file "standard-component" :depends-on ("standard-component-class"))
                                                     (:file "control-flow" :depends-on ("standard-component"))
                                                     (:file "standard-component-class")
                                                     (:file "transactions" :depends-on ("standard-component")))
                                        :depends-on ("backtracking"
                                                     "protocol"
                                                     "request-loop-error"
                                                     "standard-application"
                                                     "standard-action"
                                                     "standard-classes"
                                                     "standard-vars"))
                               (:file "standard-dispatcher" :depends-on ("request-loop-error"))
                               (:file "standard-request-context" :depends-on ("protocol"
                                                                              "standard-classes"
                                                                              "standard-vars"
                                                                              :standard-component))
                               (:file "standard-server" :depends-on ("protocol"
                                                                     "request-loop-error"
                                                                     "standard-classes"))
                               (:file "standard-session" :depends-on ("protocol"
                                                                      "standard-classes"
                                                                      "standard-session-frame"
                                                                      "standard-vars"))
                               (:file "standard-session-frame" :depends-on ("protocol"
                                                                            "backtracking"
                                                                            "standard-classes"
                                                                            "standard-vars"
                                                                            "request-loop-error"))
                               (:file "standard-vars")
			       (:module :modular-application
					:components ((:file "modular-application")
						     (:file "cookie-module")
						     (:file "security-module"))
					:serial t
					:depends-on ("protocol"
						     "standard-application"
						     "standard-request-context")))
                  :depends-on ("packages" "loggers" "helpers" "vars"))
                 (:module :components
                  :components ((:file "user-login" :depends-on ("form"
                                                                "status-bar"
                                                                "window"))
                               (:file "container")
                               (:file "cached")
                               (:file "error" :depends-on ("ucw-inspector" "window"))
                               (:file "form" :depends-on ("widget"))
                               (:file "login")
                               (:file "message")
			       (:file "meta-refresh-component")
                               (:file "task")
                               (:file "option-dialog" :depends-on ("template"))
                               (:file "range-view" :depends-on ("template"))
                               (:file "redirect")
                               (:file "status-bar")
                               (:file "tabbed-pane" :depends-on ("container" "template"))
                               (:file "template")
                               (:file "transaction-mixin")
                               (:file "ucw-inspector")
                               (:file "widget" :depends-on ("html-element"))
			       (:file "html-element")
                               (:file "window"))
                  :depends-on (:rerl "packages" "loggers" :yaclml))
                 (:module :yaclml
                  :components ((:file "tal")
                               (:file "ucw-tags")
                               (:file "yaclml"))
                  :depends-on ("packages" "loggers" :rerl)))))
  :properties ((version "0.3.9"))
  :depends-on (:arnesi :yaclml :swank :iterate :parenscript :cl-ppcre
               :trivial-sockets :rfc2109 :net-telent-date :cl-fad))

;; UCW applications included in ucw itself

(defsystem :ucw.admin
    :components ((:module :src
                  :pathname "src/admin/"
                  :components ((:file "admin")
                               (:file "admin-inspector"))))
    :depends-on (:ucw))

(defsystem :ucw.examples
    :components ((:module :examples
		  :components ((:file "examples")
			       (:file "counter" :depends-on ("examples"))
                               (:file "cache" :depends-on ("examples"))
                               (:file "forms" :depends-on ("examples"))
			       (:file "sum" :depends-on ("examples")))))
    :depends-on (:ucw))

;; Backends

(defsystem :ucw.httpd
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "common")
                             (:file "httpd" :depends-on ("common"))
                             (:file "multithread-httpd" :depends-on ("httpd")))))
  :depends-on (:ucw :rfc2388 :puri :split-sequence))

(defsystem :ucw.mod-lisp
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "mod-lisp"))))
  :depends-on (:ucw :ucw.httpd))

(defsystem :ucw.aserve
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "aserve" :depends-on ("aserve-locator"))
                             (:file "aserve-locator"))))
  :depends-on (:ucw :aserve :cl-ppcre))

(defsystem :ucw.araneida
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "common")
                             (:file "araneida" :depends-on ("common")))))
  :depends-on (:ucw :araneida :rfc2388))

;; ucw l10n

(defsystem :ucw.l10n
  :components ((:module :src
			:pathname "src/rerl/modular-application/"
			:components ((:file "l10n-module"))))
  :depends-on (:ucw :cl-l10n))

(defsystem :ucw.examples.l10n
    :components ((:module :examples :components ((:file "l10n"))))
    :depends-on (:ucw :ucw.l10n :ucw.examples))

;; This ensures that we're loading the right versions of arnesi and
;; yaclml (add similar code for rfc2388, mod_lisp, aserve et al. when
;; they have them).

(defun ensure-system-has-feature
    (system-name version-string &optional (hint ""))
  (let* ((features (asdf:component-property (asdf:find-system system-name) :features))
         (message (format nil "UCW requires the ~A feature of system ~S.
~S currently only provides ~S.~%~A"
                          version-string system-name system-name features hint)))
    (unless (member version-string features :test #'string-equal)
      (error message))))

(defmethod perform :after ((op t) (system (eql (find-system :ucw))))
  (ensure-system-has-feature :arnesi "cc-interpreter"
                             "Try pull'ing the latest arnesi or send an email to bese-devel@common-lisp.net")
  (ensure-system-has-feature :arnesi "getenv"
                             "Try pull'ing the latest arnesi or send an email to bese-devel@common-lisp.net")
  (ensure-system-has-feature :yaclml "v0.5.2"
                             "Try pull'ing the latest yaclml or send an email to bese-devel@common-lisp.net"))

(let ((config-loaded-p nil))
  (defmethod perform :around ((o load-op) (c (eql (find-system :ucw))))
    (progv (list
            (read-from-string "arnesi:*call/cc-returns*")
            ;; If you want the walker to warn about undefined variables and
            ;; functions change this to T. Since this code "breaks" (sort of)
            ;; loading ucw with asdf on sbcl we leave it off by default.
            (read-from-string "arnesi:*warn-undefined*"))
        (list nil nil)
      (call-next-method)
      (unless config-loaded-p
        (setf config-loaded-p t)
        (funcall (read-from-string "ucw::load-config-file"))))))

(defmethod perform :after ((op t) (system (eql (find-system :ucw.examples))))
  (in-package :ucw-user))

;;; Export the variables in the ucw.system package, so that between
;;; (asdf:find-system :ucw) and (asdf:oos 'asdf:load-op :ucw) users
;;; get the chance to set these variables when loading UCW
;;; programmatically. For more details on the variables themselves see
;;; src/vars.lisp

(macrolet ((def (&rest names)
               `(progn
                 ,@(loop for name in names
                         collect `(defvar ,name)
                         collect `(export ',name)))))
  (def
    *ucw-config-file*
    *ucw-swank-port*
    *ucw-backend-type*
    *ucw-backend-host*
    *ucw-backend-port*
    *defucw-server-class*
    *ucw-applications-directory*
    *ucw-systems*
    *ucw-applications*
    *ucw-log-root-directory*
    *ucw-log-level*
    *ucw-compile-time-log-level*))

;;;; * Introduction

;;;; This is the source code to UnCommon Web (aka UCW), "the UnCommon
;;;; Web Application Framework"

;;;;@include "src/packages.lisp"

;;;;@include "src/loggers.lisp"

;;;;@include "src/helpers.lisp"

;;;;@include "src/vars.lisp"


;; Copyright (c) 2003-2006 Edward Marco Baringer
;; Copyright (c) 2006 Luca Capello http://luca.pca.it <luca@pca.it>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Luca Capello, Edward Marco Baringer, nor
;;    BESE, nor the names of its contributors may be used to endorse
;;    or promote products derived from this software without specific
;;    prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
