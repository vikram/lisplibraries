;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

;;; $Revision: 1.4 $
;;; Copyright © 2001 Paul Foley (mycroft@actrix.gen.nz)
;;; All rights reserved.  Use and verbatim redistribution permitted.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.
#+CMU (ext:file-comment "$Header: /cvsroot/homepage/db/db-pkg.lisp,v 1.4 2002/12/17 08:13:02 anoncvs Exp $")

(in-package #:common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:berkeley-db)
    #+Allegro
    (load "/usr/local/lib/dbx.so")
    #+CMU
    (load-foreign "/usr/local/lib/dbx.so"
		  :libraries '("/usr/local/lib/libdb-4.1.so"))
    #+LispWorks
    (fli:register-module :dbx :real-name "/usr/local/lib/dbx.so")
    #+LispWorks
    (fli:register-module :bdb :real-name "/usr/local/lib/libdb-4.so")))

(defpackage #:berkeley-db		; "Modern mode" abomination
  (:use #:common-lisp #+CMU #:alien)
  (:export #:db-error #:db-old-version-error #:db-deadlock-error
	   #:db-fatal-error #:db-error-errno
	   #:db #:cursor #:environment #:transaction
	   #:db-p #:cursor-p #:environment-p #:transaction-p
	   #:*environment* #:*transaction*
	   #:db-plist #:environment-plist
	   #:db-version
	   #:db-create-environment #:db-open-environment
	   #:db-remove-environment
	   #:db-create #:db-open #:db-set-flags #:db-remove #:db-rename
	   #:db-cursor #:db-copy-cursor
	   #:db-close #:db-associate #:db-set-feedback
	   #:db-put #:db-append #:db-get #:db-cursor-put #:db-cursor-get
	   #:db-key-exists #:db-count
	   #:db-delete #:db-clear
	   #:db-sync #:db-checkpoint #:db-lock-detect
	   #:db-set-deadlock-detection #:db-set-encryption
	   #:db-add-data-directories #:db-set-log-directory
	   #:db-set-tmp-directory
	   #:db-begin-transaction #:db-commit-transaction
	   #:db-abort-transaction
	   #:with-environment #:with-database #:with-databases
	   #:with-cursor #:with-transaction
	   #:mapdb #:do-database))
