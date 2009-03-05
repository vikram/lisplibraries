
;;;; $Id: usocket.asd 298 2007-09-17 22:08:26Z ehuelsmann $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.3.5/usocket.asd $

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:usocket-system
    (:use #:cl #:asdf))

(in-package #:usocket-system)

(defsystem usocket
    :name "usocket"
    :author "Erik Enge & Erik Huelsmann"
    :version "0.3.5"
    :licence "MIT"
    :description "Universal socket library for Common Lisp"
    :depends-on (:split-sequence
                 #+sbcl :sb-bsd-sockets)
    :components ((:file "package")
                 (:file "usocket"
                        :depends-on ("package"))
                 (:file "condition"
                        :depends-on ("usocket"))
                 #+clisp (:file "clisp" :pathname "backend/clisp"
                                :depends-on ("condition"))
                 #+cmu (:file "cmucl" :pathname "backend/cmucl"
                              :depends-on ("condition"))
                 #+scl (:file "scl" :pathname "backend/scl"
                              :depends-on ("condition"))
                 #+(or sbcl ecl) (:file "sbcl" :pathname "backend/sbcl"
                                        :depends-on ("condition"))
                 #+lispworks (:file "lispworks" :pathname "backend/lispworks"
                                    :depends-on ("condition"))
                 #+openmcl (:file "openmcl" :pathname "backend/openmcl"
                                  :depends-on ("condition"))
                 #+allegro (:file "allegro" :pathname "backend/allegro"
                                  :depends-on ("condition"))
                 #+armedbear (:file "armedbear" :pathname "backend/armedbear"
                                                :depends-on ("condition"))
                 ))
