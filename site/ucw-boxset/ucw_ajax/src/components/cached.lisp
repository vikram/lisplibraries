;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Caching Component

(defcomponent cached-component ()
  ((cached-output :accessor cached-output :initform nil
                  :documentation "A string holding the output to
                  use for this component. This string will be
                  written directly to the html stream and is
                  changed by the REFRESH-COMPONENT-OUTPUT
                  method." )
   (timeout :accessor timeout :initarg :timeout
            :documentation "An value specifying how often this
            component needs to be refreshed. The exact
            interpretation of the value depends on the type of
            caching used class."))
  (:documentation "Component which caches its output.

The component caching API is built around the generic functions
COMPONENT-DIRTY-P and REFRESH-COMPONENT-OUTPUT and a method on
RENDER, see the respective docstrings for more details.

Do not use CACHED-COMPONENT directly, use one its subclasses."))

(defgeneric component-dirty-p (component)
  (:documentation "Returns T is COMPONENT's cache is invalid."))

(defgeneric update-cache (component)
  (:documentation "Update COMPONENT's cache variables after a refresh."))
 
(defmethod render :wrapping ((c cached-component))
  (when (component-dirty-p c)
    (setf (cached-output c)
          (with-output-to-string (yaclml:*yaclml-stream*)
            (call-next-method))))
  (write-sequence (cached-output c) (html-stream (context.response *context*))))

;;;; ** Timeout cache component

(defcomponent timeout-cache-component (cached-component)
  ((last-refresh :accessor last-refresh :initform nil
                 :documentation "The time, exrpessed as a
                 universal time, when the component was last rendered."))
  (:default-initargs
   :timeout (* 30 60 60))
  (:documentation "Render the component at most every TIMEOUT seconds."))

(defmethod component-dirty-p ((c timeout-cache-component))
  (if (null (last-refresh c))
      ;; hasn't been rendered yet.
      t
      (< (last-refresh c)
         (- (get-universal-time) (timeout c)))))

(defmethod (setf cached-output) :after (output (c timeout-cache-component))
  (declare (ignore output))
  (setf (last-refresh c) (get-universal-time)))

;;;; ** Num hits cache component

(defcomponent num-hits-cache-component (cached-component)
  ((hits-since-refresh :accessor hits-since-refresh
                       :initform nil
                       :documentation "Number of views since last refresh."))
  (:default-initargs :timeout 10)
  (:documentation "Render the component every TIMEOUT views."))

(defmethod component-dirty-p ((n num-hits-cache-component))
  (if (null (hits-since-refresh n))
      t
      (< (timeout n) (hits-since-refresh n))))

(defmethod render :after ((n num-hits-cache-component))
  (incf (hits-since-refresh n)))

(defmethod (setf cached-output) :after (output (n num-hits-cache-component))
  (declare (ignore output))
  (setf (hits-since-refresh n) 0))

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
