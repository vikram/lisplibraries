;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Range View

(defclass range-view (template-component)
  ((offset :initarg :offset
           :accessor range-view.offset
           :initform 0
           :backtrack t
           :documentation "Whcih of the windows we're currently looking at.")
   (windows :reader range-view.windows :initform '())
   (window-size :accessor range-view.window-size :initform 20 :initarg :window-size))
  (:default-initargs :template-name "range-view.tal")
  (:documentation "Component for showing the user a set of data one \"window\" at a time.

The data set is presented one \"window\" at a time with links to
the the first, previous, next and last window. Each window shows
at most WINDOW-SIZE elements of the data. The data is passed to
the range-view at instance creation time via the :DATA initarg.

The generic function RENDER-RANGE-VIEW-ITEM is used to render
each item of DATA.

In order to change the rendering of the single elements of a
range view developer's should create a sub class of RANGE-VIEW
and define their RENDER-RANGE-VIEW-ITEM methods on that.")
  (:metaclass standard-component-class))

(defun partition-into-windows (data window-size)
  (iterate
    (with windows = '())
    (with current-window = '())
    (for index upfrom 1)
    (for ele in data)
    (push (cons index ele) current-window)    
    (when (zerop (mod index window-size))
      (push (nreverse current-window) windows)
      (setf current-window '()))
    (finally (when current-window
               (push (nreverse current-window) windows)))
    (finally (return (nreverse windows)))))

(defmethod shared-initialize :after ((range range-view) slot-names
                                     &key data window-size &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value range 'windows)
        (partition-into-windows data window-size)))

(defmethod range-view.current-window ((range range-view))
  (nth (range-view.offset range) (range-view.windows range)))

(defmethod template-component-environment nconc ((range range-view))
  (let ((current-window (range-view.current-window range))
        current-window-number)
    (make-standard-environment
     `((items . ,(mapcar (lambda (item-cons)
                           (tal-env 'index (car item-cons)
                                    'item (cdr item-cons)))
                         current-window))
       (windows . ,(loop 
                      for page-number upfrom 1
                      for w in (range-view.windows range)
                      when (eq w current-window)
                        do (setf current-window-number page-number)
                      collect (tal-env 'num page-number 'selected (eq current-window w))))
       (current-window-number . ,current-window-number)
       (nextp . ,(range-view.have-next-p range))
       (previousp . ,(range-view.have-previous-p range))
       (num-windows . ,(length (range-view.windows range)))))))

(defmethod range-view.current-window-items ((range range-view))
  (mapcar #'cdr (range-view.current-window range)))

(defmethod range-view.have-previous-p ((view range-view))
  "Returns true if VIEW has a window before the current one."
  (and (range-view.windows view)
       (not (zerop (range-view.offset view)))))

(defmethod range-view.have-next-p ((view range-view))
  "Returns true if VIEW has a window after the current one."
  (with-slots (offset windows)
      view
    (and windows (< offset (1- (length windows))))))

(defgeneric render-range-view-item (range-view item)
  (:documentation "Render a single element of a range-view.")
  (:method ((range-view range-view) (item t))
    "Standard implementation of RENDER-RANGE-VIEW-ITEM. Simply
applies ITEM to princ (via <:as-html)."
    (declare (ignore range-view))
    (<:as-html item)))

(defaction scroll-start ((range range-view))
  (setf (range-view.offset range) 0))

(defaction scroll-end ((range range-view))
  (setf (range-view.offset range) (1- (length (range-view.windows range)))))
  
(defaction scroll-forward ((view range-view) &optional (n 1))
  (with-slots (offset windows)
      view
    (incf offset n)
    (when (<= (length windows) offset)
      (scroll-end view))))

(defaction scroll-backward ((range range-view) &optional (n 1))
  (with-slots (offset)
      range
    (decf offset n)
    (when (minusp offset)
      (setf offset 0))))

(defaction scroll-to-page ((range range-view) window-number)
  (setf (range-view.offset range) window-number))

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
