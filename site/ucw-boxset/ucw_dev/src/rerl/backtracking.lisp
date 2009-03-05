;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Backtracking 

;;;; Places

(defmacro make-place (form &optional (copyer '#'identity))
  "Create a new place around FORM."
  (with-unique-names (v)
    `(make-instance 'place
                    :getter (lambda () ,form)
                    :setter (lambda (,v) (setf ,form ,v))
                    :copyer ,copyer
                    :form ',form)))

(defmethod place ((place place))
  "Returns the current value of PLACE."
  (etypecase (place.getter place)
    (function (funcall (place.getter place)))
    (arnesi::closure/cc
     (with-call/cc
       (funcall (place.getter place))))))

(defmethod (setf place) (value (place place))
  "Set the value of PLACE to VALUE."
  (etypecase (place.setter place)
    (function (funcall (place.setter place) value))
    (arnesi::closure/cc
     (with-call/cc
       (funcall (place.setter place) value)))))

(defmethod clone-place-value ((place place) &optional (value (place place)))
  "Return a copy of the current value in PLACE."
  (funcall (place.copyer place) value))

;;;; Backtracking

(defconstant +unbound-value+ '|#<unbound-value>|)

(defmethod save-backtracked ((frame standard-session-frame))
  "For every place in FRAME we want to back track save (in frame's
  BACKTRACKS) a copy of the current value of the backtrack
  places. This involes iterating over FRAME's backtracks and
  calling the copyer on the result of copying the getter."
  (dolist (bt-cons (frame.backtracks frame))
    (setf (car bt-cons) (place (cdr bt-cons)))))

(defmethod reinstate-backtracked ((frame standard-session-frame))
  "Reset all the values of the backtracked places in FRAME to the
  values they had the last FRAME was handled."
  (dolist* ((value . place) (frame.backtracks frame))
    (setf (place place) (clone-place-value place value))))

(defun clone-backtracks (backtracks)
  "BACKTRACKS is a list of backtrack objects, we create a new list of
  backtracks with the same getter, setter and value but a new copy of
  value."
  (copy-alist backtracks))

(defmethod backtrack ((frame session-frame) place &optional (value (place place)))
  (push (cons value place) (frame.backtracks frame))
  t)

(defmethod backtrack-slot ((frame standard-session-frame) object slot-name &optional (copyer #'identity))
  (backtrack frame
             (make-instance 'place
                            :getter (lambda ()
                                      (if (slot-boundp object slot-name)
                                          (slot-value object slot-name)
                                          +unbound-value+))
                            :setter (lambda (v)
                                      (if (eql +unbound-value+ v)
                                          (slot-makunbound object slot-name)
                                          (setf (slot-value object slot-name) v)))
                            :copyer copyer
                            :form `(slot-value ,object ,slot-name))))

;; Copyright (c) 2003-2005 Edward Marco Baringer
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
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
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
