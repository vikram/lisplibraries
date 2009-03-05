;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: BERKELEY-DB -*-

;;; $Revision: 1.7 $
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
#+CMU (ext:file-comment "$Header: /cvsroot/homepage/db/db-cmu.lisp,v 1.7 2002/12/17 08:12:59 anoncvs Exp $")

(in-package #:berkeley-db)

(import '(ext:finalize ext:cancel-finalization ext:required-argument
	  sys:sap-int))

#+CMU
(setf (getf ext:*herald-items* :berkeley-db) '("    Berkeley DB"))

#+CMU
(deftype pointer () 'sys:system-area-pointer)
#-CMU
(deftype pointer () 't)

(defconstant +null+ (sys:int-sap 0))

(def-alien-type db-type
  (enum nil (:btree 1) :hash :recno :queue :unknown))

#+CMU
(def-alien-type DBT			; needed by DB-ASSOCIATE
  (struct nil
    (data sys:system-area-pointer)
    (size c-call:unsigned-int)
    (ulen c-call:unsigned-int)
    (dlen c-call:unsigned-int)
    (doff c-call:unsigned-int)
    (flags c-call:unsigned-int)))

#+CMU
(defun naturalize-string (sap size)
  (declare (type sys:system-area-pointer sap)
	   (type (integer 0 #.most-positive-fixnum) size))
  (let ((string (make-string size)))
    (sys:without-gcing
     (kernel:copy-from-system-area sap 0 string
				   (* vm:vector-data-offset vm:word-bits)
				   (* size vm:byte-bits)))
    string))
