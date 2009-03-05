;;; -*- lisp -*-

;;;; * Default UCW startup file

;;;; This file is meant to be loaded by ucwctl(1), but you can use it
;;;; as a general "startup ucw" file as well.  In this case, you
;;;; should customize this script to load/prepare your application.

(in-package :common-lisp-user)

;;;; * CMUCL specific hacks

;; this isn't strictly necessary, but scheduling feels very coarse
;; without startup-idle-and-top-level-loops, leading to answer delays
;; of about 1s per request.
#+cmu
(unless (find-if
         #'(lambda (proc)
             (string= (mp:process-name proc) "Top Level Loop"))
         (mp:all-processes))
  (mp::startup-idle-and-top-level-loops))

;;;; Load up UCW itself plus the examples
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :ucw)
  (asdf:oos 'asdf:load-op :ucw.admin)
  (asdf:oos 'asdf:load-op :ucw.examples)
  #+(or (and sbcl sb-unicode) (and clisp unicode))
  (asdf:oos 'asdf:load-op :ucw.examples.l10n))

;;;; Tell UCW which apps to serve
(setf ucw.system:*ucw-applications* '(ucw-user::*example-application*
                                      #+(or (and sbcl sb-unicode) (and clisp unicode))
                                      ucw-user::*l10n-example-application*
                                      ucw::*admin-application*))

;;;; Let there be swank
(ucw:start-swank)

;;;; Finally startup the server
(ucw:create-server)


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
