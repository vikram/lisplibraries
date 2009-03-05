;;;; ACL compatibility macro
;;;; similar in spirit to Alain Picard's cmucl-ffi-compat.lisp.
;;;;
;;;; Copyright (C) 2002 knowledgeTools GmbH. All rights reserved.
;;;; Author: David Lichteblau <david@knowledgetools.de>
;;;; License: See db.lisp.

(in-package #:berkeley-db)

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

(defpackage #:c-call
  (:export #:int #:unsigned-int #:c-string))

(defpackage #:ext
  (:export #:finalize #:cancel-finalization #:required-argument))

(defvar *finalizations* (make-hash-table :weak-keys t))

(defun ext:finalize (object fn)
  (let ((fp (excl:schedule-finalization object fn)))
    (push fp (gethash object *finalizations* '()))
    nil))

(defun ext:cancel-finalization (object)
  (let ((fp (gethash object *finalizations*)))
    (if fp
	(mapc #'excl:unschedule-finalization fp)
	(cerror "continue" "no finalizer defined for ~S" object)))
  nil)

(defun ext:required-argument ()
  (error "An argument was required, but not provided."))

(defmacro def-alien-type (type bar)
  (declare (ignore foo bar))
  ;; fixme: this is a workaround for enum DB-TYPE, which we handle manually
  ;; Might make sense to either implement enums properly or avoid them.
  (assert (eq type 'db-type))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (excl:without-package-locks
   (export 'sys::system-area-pointer :sys)
   (export 'sys::int-sap :sys)
   (export 'sys::sap-int :sys)
   (defun sys::int-sap (address)
     address)
   (defun sys::sap-int (sap)
     sap)))

(defun naturalize-string (str size)
  (etypecase str
    (string str)
    (integer (excl:native-to-string str :length size))))

(defun translate-type (spec)
  (ecase spec
    (sys:system-area-pointer '((* :void) integer))
    (c-call:unsigned-int '(:unsigned-int integer))
    ((c-call:int db-type) '(:int integer))
    (c-call:c-string '((* :char)))))

(defun process-arglist (vars)
  "Take CMUCL-style DEF-ALIEN-ROUTINE parameter specification VARS and return
  as values:
    (0) DFC-ARGS: DEFINE-FOREIGN-CALL's argument specification
    (1) LISP-ARGS: parameters of Lisp wrapper function (excludes output args)
    (2) PLACES: variable names for internal arrays (for output arguments only)
    (3) TRANSFORMERS: forms which return the actual ffi call arguments"
  (let ((dfc-args '())
	(lisp-args '())
	(places '())
	(transformers '()))
    (dolist (var vars)
      (destructuring-bind (name type &optional out) var
	(let ((typespec (translate-type type)))
	  (ecase out
	    (:out
	      (push `(,name (:array ,(car typespec))) dfc-args)
	      (push name places))
	    ((nil)
	      (push `(,name ,@typespec) dfc-args)
	      (push name lisp-args))))
	(push (case type
		(db-type		;workaround
		  `(ecase ,name (:btree 1) (:hash 2) (:recno 3) (:queue 4)))
		(t
		  `(or ,name 0)))
	      transformers)))
    (values (reverse dfc-args)
	    (reverse lisp-args)
	    (reverse places)
	    (reverse transformers))))

(defmacro def-alien-routine ((c-name lisp-name) return-type &rest vars)
  (multiple-value-bind (dfc-args lisp-args places transformers)
      (process-arglist vars)
    (let ((internal-name (make-symbol c-name))
	  (place-maker '(make-array 1
			 :element-type '(unsigned-byte 32)
			 :initial-element 0)))
      `(progn
	 (ff:def-foreign-call (,internal-name ,c-name)
	     ,dfc-args
	   :returning ,(translate-type return-type)
	   :strings-convert t)
	 (defun ,lisp-name (,@lisp-args)
	   ;; fixme: we want ACL to convert strings for us, but sometime we
	   ;; also pass foreign strings directly (in particular, 0), which
	   ;; gives a warning.  We just suppress the warning here...
	   (handler-bind ((excl::foreign-strings-convert-warning
			   #'muffle-warning))
	     (let (,@(mapcar (lambda (place) `(,place ,place-maker)) places))
	       (values
		(,internal-name ,@transformers)
		,@(mapcar (lambda (place) `(elt ,place 0)) places)))))))))
