;; -*- lisp -*-

(in-package :it.bese.ucw-user)

;;;; First we'll define the main component for our application. It
;;;; will hold the current value and a boolean specifying whether we
;;;; want to accept negative values or not.

(defcomponent counter (template-component)
  ;; two slots, both are backtracked.
  ((value :accessor value
          :initarg :value
	  :initform 0)
   (allow-negatives :accessor allow-negatives
                    :initarg :allow-negatives
		    :initform nil))
  (:default-backtrack #'identity)
  (:default-initargs :template-name "ucw/examples/counter.tal"))

;;;; This action will just increment the current value of the counter.

(defaction increment ((c counter))
  (incf (value c)))

;;;; This action will decrement the counter. However if the user tries
;;;; to give the counter a negative value we ask for if they're
;;;; sure. We present them with the option of not asking this question
;;;; again (the :forever option).

(defaction decrement ((c counter))
  (when (and (zerop (value c))
             (not (allow-negatives c)))
    ;; the option-dialog component returns the value associated with
    ;; whatever answer the user chose.
    (case (call 'option-dialog
                :message "Do you really want to allow negative values?"
                :options '((:once-only . "Yes, but just this time.")
                           (:forever .   "Yes, now and forever.")
                           (:no .        "No"))
                :confirm t)
      (:no ;; they don't really want to decrement, do nothing.
       (return-from decrement nil))
      (:forever ;; don't ask this question again
       (setf (allow-negatives c) t))))
  (decf (value c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003-2005 Edward Marco Baringer
;;; All rights reserved. 
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;; 
;;;  - Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;;  - Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;    of its contributors may be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
