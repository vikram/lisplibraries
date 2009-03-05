;;;;   -*- Mode: lisp; Package: berkeley-db; Syntax: Common-lisp -*-
;;
;; Copyright (C) 2002 Memetrics Pty. Ltd.
;; All rights reserved.
;;
;; Author:  $Author: kooks $
;; email:   Alain.Picard@memetrics.com
;;
;; Version: $Id: cmucl-ffi-compat.lisp,v 1.4 2002/08/16 01:01:01 kooks Exp $
;;
;;;; Commentary:
;;
;; This is a compatibility package for the FFI glue in db-cmu.lisp for
;; the Xanalys Lispworks. This does not cover all of CMUCL's FFI
;; functionality (far from it!); but only what is strictly necessary
;; to get db-cmu.lisp to compile under lispworks.
;;
;; Warranty:  Same non-warranty as in db.lisp by Paul Foley:
;; AS IS, with no express or implied warranty.  Read db.lisp
;; for details.
;;
;;;; Code:

(defconstant +db-lw-version+ "$Revision: 1.4 $"
  "$Id: cmucl-ffi-compat.lisp,v 1.4 2002/08/16 01:01:01 kooks Exp $
   Report bugs to: Alain.Picard@memetrics.com")


;; We must "fake out" certain packages.
;; Only the exported symbols are simulated and should
;; be relied on.
;;

;;;; C-CALL package

(defpackage c-call
  (:export int unsigned-int c-string))


;;;; EXT package

(defpackage ext
  (:export finalize cancel-finalization required-argument))

(in-package ext)

(declaim (inline finalize))
(defun finalize (object fn)
  (declare (ignore fn))
  (hcl:flag-special-free-action object))

(declaim (inline cancel-finalization))
(defun cancel-finalization (object)
  (hcl:flag-not-special-free-action object))

(defun required-argument ()
  (error "An argument was required, but not provided."))


;;;; SYSTEM package

;; LW also provides a SYSTEM package.  Thank god it doesn't
;; use the symbols SYSTEM-AREA-POINTER and INT-SAP.  :-)

(in-package :sys)
(export '(system-area-pointer int-sap sap-int))

(defun system:int-sap (address)
  (fli:make-pointer :address address :type :void))

(defun system:sap-int (sap)
  (fli:pointer-address sap))



;;;;  BERKELEY DB Compatibilty macros

(in-package "BERKELEY-DB")
(use-package :fli)

;; We don't provide all alien types; only enum, since that's the
;; only one used by BDB.
(defmacro def-alien-type  (name enum-values)
  (assert (eq 'enum (first enum-values)))
  (assert (eq nil (second enum-values)))
  `(define-c-enum ,name ,@(nthcdr 2 enum-values)))

(def-alien-type db-type
    (enum nil (:btree 1) :hash :recno :queue :unknown))

(define-foreign-type c-call:c-string (in-out-spec)
  (ecase in-out-spec
    ;; Those marked with ??? are not used by BDB, and I'm not sure
    ;; they would actually work.
    (:in-out '(:reference (:reference :ef-mb-string) :allow-null t))	; ???
    (:out    '(:reference (:reference  :ef-mb-string) :allow-null t)) ; ???
    (:return '(:reference-return :ef-mb-string :allow-null t))
    (:in     '(:reference-pass :ef-mb-string :allow-null t))))

(define-foreign-type system:system-area-pointer (in-out-spec)
  (ecase in-out-spec
    (:in-out '(:reference (:pointer void) :allow-null t))
    (:out    '(:reference-return (:pointer :void) :allow-null t))
    (:in     '(:pointer :void))))

(define-foreign-type c-call:int (in-out-spec)
  (ecase in-out-spec
    ((or :in :return)  :int)
    ((or :in-out :out) '(:reference-return :int))))

(define-foreign-type c-call:unsigned-int (in-out-spec)
  (ecase in-out-spec
    ((or :in :return)  '(:unsigned :int))
    ((or :in-out :out) '(:reference-return (:unsigned :int)))))

(defconstant +wrapped-types+
  '(c-call:int c-call:unsigned-int
    c-call:c-string sys:system-area-pointer)
  "A list of all types which need special treatment when
   we see an IN-OUT-SPEC.")


(defmacro def-alien-routine ((c-name lisp-name) return-type &rest slots)
    (if (some #'output-slot-p slots)
	`(define-complex-alien-routine (,c-name ,lisp-name) ,return-type ,@slots)
	`(define-simple-alien-routine (,c-name ,lisp-name) ,return-type ,@slots)))


(defmacro define-simple-alien-routine ((c-name lisp-name) return-type &rest slots)
  "This macro translates a CMUCL def-alien-routine into a LW DEFINE-FOREIGN-FUNCTION."
  `(define-foreign-function (,lisp-name ,c-name)
    (,@(mapcar #'canonicalize-slot slots))
    :calling-convention :cdecl
    :language :c
    :result-type ,(canonicalize-return-type return-type)))

(defun canonicalize-return-type (return-type)
  (list return-type :return))

(defmacro define-complex-alien-routine ((c-name lisp-name) return-type &rest slots)
  "Translates a `complex' alien routine; i.e. one which has 1 or more :out
   parameters and for which we must wrap the `simple' translation in a layer
   which swizzles the lambda list appropriately for CMUCL calling convention."
  (let ((orig-lambda-list      (mapcar #'first slots))
	(modified-lambda-list  (mapcar #'first (remove-if-not #'input-slot-p slots)))
	(new-func-name (make-raw-func-symbol lisp-name)))
    `(progn
      (define-simple-alien-routine (,c-name ,new-func-name ) ,return-type ,@slots)
      (defun ,lisp-name ,modified-lambda-list
	(funcall (function ,new-func-name)
		 ,@(mapcar #'(lambda (arg) (find arg modified-lambda-list)) orig-lambda-list))))))

(defun make-raw-func-symbol (symbol)
  (intern (concatenate 'string
		       "%-RAW-"
		       (symbol-name symbol))))

(defun canonicalize-slot (slot)
  (destructuring-bind (name type in-out-spec &rest ignore) (append slot '(:in))
    (declare (ignore ignore))
    (if (member type +wrapped-types+)
	`(,name (,type ,in-out-spec))
	`(,name ,type))))

(defun output-slot-p (raw-slot)
  (member (car (last raw-slot)) '(:out :in-out)))

(defun input-slot-p (raw-slot)
  (or (= 2 (length raw-slot))
      (member (car (last raw-slot)) '(:in :in-out))))


;;;; Pieces of missing functionality

;; This is how we do the finalization.  CMUCL accepts a function
;; to be run; so this is not a general solution.  I simply inspected
;; the code and saw that all he wants to do is warn and reclaim
;; foreign resources.

(eval-when (:load-toplevel :execute)
  (hcl:add-special-free-action 'warn-on-unclosed-db-object))

(defun warn-on-unclosed-db-objects (object)
  ;; This action only does something on
  ;; one of the `db' types of :berkeley-db.
  ;; It should not interfere with any other action list
  ;; in an LW image.
  (when (member (type-of object)
		'(db cursor environment transaction))
    (warn "Aborting or closing lost ~A: ~A" (type-of object) object))
  (typecase object
    (db          (%db-close object 0))
    (cursor      (%db-cursor-close object))
    (environment (%db-env-close object 0))
    (transaction (%db-txn-abort (db-txn-%sap object)))))

(defun naturalize-string (sap size)
  (fli:convert-from-foreign-string sap :length size))

;;; DB-LW.LISP ends here



; int DB_ENV->set_flags(DB_ENV *dbenv, u_int32_t flags, int onoff);

#+foo
(def-alien-routine ("dbx_env_set_flags" %db-env-flags) c-call:int
  (handle sys:system-area-pointer)
  (flags c-call:unsigned-int)
  (onoff c-call:int))