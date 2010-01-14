;;; Gray streams
;;;
;;; Copyright (C) 1999-2003 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: gray.lisp,v 1.11 2003/09/25 03:47:28 rtoy Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/gray.lisp,v $

(eval-when (compile load eval)
  #-(or allegro clisp cmu lispworks openmcl sbcl scl)
  (error 'not-implemented :proc "Gray streams")
;;  (require :port-ext (translate-logical-pathname "clocc:src;port;ext"))
  #+cmu
  (unless (ignore-errors (find-class 'ext:fundamental-input-stream))
    ;; If CMUCL has WITHOUT-PACKAGE-LOCKS, it's better to REQUIRE Gray
    ;; streams because it does the necessary magic to load it without
    ;; package-lock errors.
    (if (find-symbol "WITHOUT-PACKAGE-LOCKS" "EXT")
	(require 'gray-streams)
	(load "library:subsystems/gray-streams-library"))))

(in-package #+allegro :excl
            #+(and clisp      lisp=cl)  :ext
            #+(and clisp (not lisp=cl)) :lisp
            #+(or cmu scl) :ext
            #+lispworks :stream
            #+openmcl :ccl
            #+sbcl :sb-gray)

(let ((cl-user::gray-symbols
       '(;; Classes
	 fundamental-stream fundamental-input-stream fundamental-output-stream
         fundamental-character-stream fundamental-binary-stream
         fundamental-character-input-stream fundamental-character-output-stream
         fundamental-binary-input-stream fundamental-binary-output-stream
         ;; character input
         stream-read-char stream-unread-char stream-read-char-no-hang
         stream-peek-char stream-listen stream-read-line stream-clear-input
         ;; character output
         stream-write-char stream-line-column stream-start-line-p
         stream-write-string stream-terpri stream-fresh-line
         stream-finish-output stream-force-output stream-clear-output
         stream-advance-to-column
         ;; Binary streams
         stream-read-byte stream-write-byte)))
  (import cl-user::gray-symbols :port)
  (export cl-user::gray-symbols :port))

(provide :port-gray)
;;; file gray.lisp ends here
