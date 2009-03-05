;;; -*- lisp -*-

;;;; * Configuration functions

;;;; Functions to load a config file and/or the applications, setting
;;;; the `*ucw-systems*' and `*ucw-applications*' variables, according
;;;; to `*ucw-applications-directory*'.


(in-package :it.bese.ucw)

(defun prepare-applications-variables (directory)
  "Set the `*ucw-systems*' and `*ucw-applications*' variables according to
the ASDF file names present in DIRECTORY.

NB: it is assumed that `(readtable-case *readtable*)' is always :UPCASE."
  (let ((files (cl-fad:list-directory directory)))
    (dolist (file files)
      (when (equal "asd" (pathname-type file))
	(push (pathname-name file) *ucw-systems*)
	(push `(symbol-value
		(find-symbol
		 ,(concatenate 'string "*"
			       (string-upcase (pathname-name file))
			       "-APPLICATION*")
		 (find-package
		  ,(string-upcase (pathname-name file)))))
	      *ucw-applications*)))))

(defun load-applications-systems (&optional (applications-list *ucw-systems*))
  "Load the applications systems in APPLICATIONS-LIST.

APPLICATIONS-LIST defaults to `*ucw-systems*'."
  (dolist (name applications-list)
    (asdf:oos 'asdf:load-op name)))

(defun load-applications (&optional (applications-directory *ucw-applications-directory*))
  "Load the ASDF applications present in APPLICATIONS-DIRECTORY.

APPLICATION-DIRECTORY defaults to `*ucw-applications-directory*'."
  (let ((directory (when applications-directory
                     (cl-fad:directory-exists-p applications-directory))))
    (if directory
      (progn
        (pushnew directory asdf:*central-registry* :test #'equal)
        (prepare-applications-variables directory))
      (when applications-directory
        (error "~S is not a valid directory" applications-directory)))
    (when *ucw-systems*
      (load-applications-systems))))

(defun load-config-file (&optional (config-file *ucw-config-file*))
  "Load the config file and then proceed to load the applications.

CONFIG-FILE defaults to `*ucw-config-file*'."
  (when config-file
    (load config-file))
  (load-applications))

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
