;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Error Message

(defclass error-message (simple-window-component)
  ((message :accessor message :initarg :message :initform "ERROR [no message specified]"))
  (:documentation "Generic component for showing server side
 error messages.")
  (:metaclass standard-component-class))

(defmethod window-component.title ((err error-message))
  (concatenate 'string "ERROR: " (message err)))

(defmethod render ((err error-message))
  (<:h1 :style "color: #cc3333;" "ERROR")
  (<:p :style "color: #cc3333;" (<:as-html (message err))))

;;;; ** Error Component (Error with Backtrace)

(defclass error-component (error-message)
  ((condition :accessor error.condition :initarg :condition :initform nil)
   (backtrace :accessor error.backtrace :initarg :backtrace))
  (:documentation "Generic component for showing server side
 error conditions. Unlike ERROR-MESSAGE this component also
 attempts to display a backtrace.")
  (:default-initargs
    :place (make-place (context.window-component *context*))
    :javascript '((:js
                   (defun toggle-display (id)
                     (let ((element (document.get-element-by-id id)))
                       (with-slots (display) (slot-value element 'style)
                         (setf display (if (= display "none")
                                           "block"
                                           "none"))))))))
  (:metaclass standard-component-class))

(defmethod window-component.title ((err error-component))
  (format nil "ERROR: ~S"  (error.condition err)))

(defmethod render ((err error-component))
  (<:h1 :style "text-align: center; width: 100%; color: #cc3333;" "ERROR")
  (<:h2 :style "color: #ff0000;"
    (<:tt 
    (inspect-anchor (frame.window-component (context.current-frame *context*))
		    (error.condition err)
		    (message err))))
  (<:h2 :style "text-align: center; width: 100%; color: #cc3333;" "BACKTRACE")
  (loop 
     for frame in (error.backtrace err)
     for index upfrom 0
     for div-id = (concatenate 'string "frame-" (princ-to-string index) "-details")
     do (<:div
	  (<:p (<:button :onclick (js:js-inline* `(toggle-display ,div-id)) "[ + ]")
	       " "
	       (<:as-html
		 (let ((desc (backtrace-frame-description frame))
		       (max-length 50))
		   (if (stringp desc)
		       (if (< max-length (length desc))
			   (concatenate 'string (subseq desc 0 (min max-length (length desc))) "...")
			   desc)
		       desc))))
	  (<:div :style "display: none;" :id div-id
	    (<:table :border 1
	      (<:tr
	        (<:td :valign "top" "Description")
		(<:td (<:as-html (backtrace-frame-description frame))))
	      (<:tr
	        (<:td :valign "top" "Locals")
		(<:td
		 (if (backtrace-frame-locals frame)
		     (<:table
		       (<:tr
			 (<:th "Name")
			 (<:th "Value"))
		       (loop
			  with *print-level* = 4
			  with *print-length* = 4
			  with *print-circle* = t
			  for local in (backtrace-frame-locals frame)
			  do (<:tr
			       (<:td :valign "top"
			         (inspect-anchor (frame.window-component (context.current-frame *context*))
						 (getf local :name) #|  "name" |# ))
			       (<:td :valign "top"
				 (inspect-anchor (frame.window-component (context.current-frame *context*))
						 (getf local :value) #|  "value" |# )))))
		     (<:as-html "None."))))
	      (<:tr
	        (<:td :valign "top" "Source")
		(<:td (<:as-html (backtrace-frame-source-location frame)))))))))

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
