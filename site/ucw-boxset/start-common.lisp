;;; -*- lisp -*-

;;;; * Startup file for ucw_boxset

;;;; This file is a modified version of ucw's start.lisp. It is
;;;; designed to be simple to start and skips ucw's entire
;;;; configuration system. NB: This file will also override
;;;; asdf:*central-registry*.

(in-package :common-lisp-user)

(write-line "*** ")
(write-line "*** ")
(write-line "*** ")
(write-line "*** Starting UCW boxset, if something goes wrong try loading it into a clean lisp (reading lisp files in UTF-8 encoding)")
(write-line "***   sbcl --no-sysinit --no-userinit --load start.lisp")
(write-line "***   clisp -norc -Efile UTF-8 start.lisp")
(write-line "*** ")
(write-line "*** ")
(write-line "*** ")
(terpri)

(eval-when (:load-toplevel :compile-toplevel :execute)
  #+sbcl(require :asdf)

  (unless (find-package :asdf)
    (load (merge-pathnames #P"dependencies/asdf/asdf.lisp" *load-truename*)))
  ;; make sure it exists no matter what, so we don't need read time magic
  (export (intern "*SOURCE-TO-TARGET-MAPPINGS*" (find-package :asdf)) (find-package :asdf)))

(defparameter *ucw-boxset-home-directory*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defun reread-ucw-boxset-asdf-registry ()
  "Go through all dirs in the boxset home and register them into the asdf:*central-registry*."
  (flet ((push-all (systems-dir)
           (dolist (dir-candidate
                     (directory (concatenate 'string (namestring systems-dir) "*/") #+openmcl :directories #+openmcl t))
             ;; skip dirs starting with a _
             (let ((name (car (last (pathname-directory dir-candidate)))))
               (unless (equal #\_ (elt name 0))
                 (pushnew dir-candidate asdf:*central-registry* :test 'equal))))))
    ;; here, we probably may even (setf asdf:*central-registry* nil)
    (push-all (merge-pathnames #P"dependencies/"
                               *ucw-boxset-home-directory*))))

(reread-ucw-boxset-asdf-registry)

(pushnew *ucw-directory* asdf:*central-registry* :test 'equal)

(when (find-package :asdf-binary-locations)
  (write-line "*** ASDF Binary Locations detected, we'll try to disable it for Slime/Swank")
  (terpri)
  ;; turn off asdf-binary-locations for slime if it's happened to be loaded
  (pushnew (list (merge-pathnames #P"slime/" *ucw-boxset-home-directory*) nil)
           asdf:*source-to-target-mappings* :test #'equal))

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
  ;; TODO this is here until the l10n integration patch reaches both repos
  (ignore-errors
    (asdf:oos 'asdf:load-op :ucw.examples.l10n)))

;;;; Tell UCW which apps to serve
(setf ucw.system:*ucw-applications* (append (list 'ucw-user::*example-application*
                                                  'ucw-user::*l10n-example-application*
                                                  'ucw::*admin-application*)
                                            (let ((shared-counter-app (find-symbol "*SHARED-COUNTER-APPLICATION*" (find-package :ucw-user))))
                                              (when shared-counter-app
                                                (list shared-counter-app)))))

;;;; * Finally startup the server
(ucw:create-server)

(terpri)
(write-line "*** UCW Started. Point your browser to http://127.0.0.1:8080/")
(terpri)

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
