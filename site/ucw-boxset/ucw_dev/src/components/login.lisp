;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Login Component

(defclass login ()
  ((username :accessor login.username
             :initform nil)
   (password :accessor login.password
             :initform nil)
   (message :accessor login.message
            :initform nil
            :initarg :message
            :documentation "A message which will be presented to
 the user before the login box."))
  (:documentation "Generic login (input username and password) component.

This component, which must be embedded in another component,
presents the user with a simple two fielded login form.

When the user attempts a login the action try-login is called,
try-login calls the generic function check-credentials passing it
the login component. If check-credentials returns true then the
login-successful action is called, otherwise the message slot of
the login component is set (to a generic \"bad username\"
message).

The default implementaion of login-successful simply answers t,
no default implementation of check-credentials is
provided. Developers should use sub-classes of login for which
all the required methods have been definined.")
  (:metaclass standard-component-class))

(defmethod render ((l login))
  (<:div :id "ucw-login"
    (when (login.message l)
      (<:div :id "ucw-login-message" (<:as-html (login.message l))))
    (<ucw:form :action (try-login l) :method "post"
    (<:table
     (<:tr (<:td :align "right" "Username")
           (<:td :align "left"  (<ucw:input :name "username"
                                            :type "text"
					    :accessor (login.username l)
					    :size 10)))
     (<:tr (<:td :align "right" "Password")
           (<:td :align "left"  (<ucw:input :name "password"
                                            :type "password"
					    :accessor (login.password l)
					    :size 10)))
     (<:tr (<:td :align "center" :colspan 2
                 (<:input :type "submit" :value "login")))))))

(defgeneric check-credentials (login)
  (:documentation "Returns T if LOGIN is valid."))

(defaction login-successful ((l login))
  (answer t))

(defaction try-login ((l login))
  (if (check-credentials l)
      (login-successful l)
      (setf (login.message l) "Bad username/password.")))

;; Copyright (c) 2003-2005 Edward Marco Baringer
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
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
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
