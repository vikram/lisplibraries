(in-package #:defsystem-compatibility)

#| simple-header

http://www.opensource.org/licenses/mit-license.php

Copyright (c) 2004-2006 Gary Warren King, metabang.com

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
Author: Gary King

DISCUSSION

|#

(in-package #:defsystem-compatibility)

;;; We use this to keep track of nestings for ASDF components.
(defparameter *relative-pathname* "")

;;; ---------------------------------------------------------------------------

(defmethod registered-systems* ((system-definer (eql :asdf)))
  (let ((result nil))
    (maphash 
     (lambda (k v)
       (declare (ignore v))
       (push (ensure-system-name* :asdf k) result))
     asdf::*defined-systems*)
    result))

;;; ---------------------------------------------------------------------------

(defmethod loaded-systems* ((system-definer (eql :asdf)))
  (let ((result nil))
    (maphash 
     (lambda (name time.system)
       (let ((system (cdr time.system)))
         (when (gethash 'asdf::load-op (asdf::component-operation-times system))
           (push (ensure-system-name* :asdf name) result))))
     asdf::*defined-systems*)
    (values result)))

(defmethod associated-test-system* ((system-definer (eql :asdf)) system-name)
  (declare (ignore system-definer system-name))
  (values nil)) 

(defmethod system-sub-systems* ((system-definer (eql :asdf)) system-name)
  (collect-system-dependencies system-name))

(defmethod top-level-system-p* ((system-definer (eql :asdf)) system-name)
  (declare (ignore system-name))
  (values t))

(defmethod system-name-for-display* ((system-definer (eql :asdf)) system-name)
  (symbol-name system-name))

(defmethod system-source-file* ((system-definer (eql :asdf)) system-name)
  (let ((system (find-system system-name)))
    (make-pathname 
     :type "asd"
     :name (safe-slot-value system 'asdf::name)
     :defaults (safe-slot-value system 'asdf::relative-pathname))))

(defmethod ensure-system-name* ((system-definer (eql :asdf)) system-name)
  (intern (ensure-string (asdf::coerce-name system-name)) "KEYWORD"))

(defmethod ensure-system-name* ((system-definer (eql :asdf)) (system asdf:system))
  (safe-slot-value system 'asdf::name))

(defmethod filename-looks-like-system-file-p* ((system-definer (eql :asdf)) filename)
  (string-equal "asd" (pathname-type filename)))

(defmethod system-dependencies* ((system-definer (eql :asdf)) system-name)
  (mapcar
    (lambda (x)
      (intern (ensure-string x) "KEYWORD"))
    (cdadr (assoc 'asdf::load-op 
                  (safe-slot-value (asdf:find-system system-name) 
                                   'asdf::in-order-to)))))

(defun safe-slot-value (thing name)
  (when (slot-boundp thing name)
    (slot-value thing name)))



