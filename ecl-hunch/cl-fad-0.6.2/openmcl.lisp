;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CCL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-fad/openmcl.lisp,v 1.5 2008/03/12 00:10:43 edi Exp $

;;; Copyright (c) 2004-2008, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :ccl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((%rmdir-symbol (find-symbol "%RMDIR" :ccl)))
    (unless (and %rmdir-symbol (fboundp %rmdir-symbol))
      (pushnew :no-%rmdir *features*))))

#+:no-%rmdir
(defun %rmdir (name)
  (with-cstrs ((n name))
    (#_rmdir n)))

(defun delete-directory (path)
  (let* ((namestring (native-translated-namestring path)))
    (when (%realpath namestring)
      (let* ((err (%rmdir namestring)))
        (or (eql 0 err) (signal-file-error err path))))))

(export 'delete-directory)
