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

(defmethod available-systems* ((system-definer (eql :asdf)))
  ;; this only handles sysdef-central-registry-search type search
  (let ((result nil)
        (*load-verbose* nil)
        (*load-print* nil))
    (dolist (dir asdf:*central-registry*)
      (let* ((defaults (eval dir)))
        (dolist (file (directory (make-pathname
                                  :defaults defaults :version :newest
                                  :name :wild :type "asd" :case :local)))
          (pushnew (pathname-name file) result :test #'string-equal))))
    (remove-if 
     (lambda (s)
       (not (ignore-errors (select-system-definer s))))
     (nreverse result))))
   
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
  (declare (ignore system-name))
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
  (intern (ensure-string (asdf::coerce-name system-name)) '#:keyword))

(defmethod ensure-system-name* ((system-definer (eql :asdf)) (system asdf:system))
  (safe-slot-value system 'asdf::name))

(defmethod filename-looks-like-system-file-p* ((system-definer (eql :asdf)) filename)
  (string-equal "asd" (pathname-type filename)))

(defmethod system-dependencies* ((system-definer (eql :asdf)) system-name)
  (loop for system in
       (cdadr (assoc 'asdf::load-op 
		     (safe-slot-value (asdf:find-system system-name) 
				      'asdf::in-order-to)))
       collect (if *ignore-errors?* 
		(or (ignore-errors (ensure-system-name system)) system)
		(ensure-system-name system))))

;;; ---------------------------------------------------------------------------

(defun safe-slot-value (thing name)
  (when (slot-boundp thing name)
    (slot-value thing name)))

#+(and ASDF GLU-GENERIC-LOAD-UTILS)
;; a bit weird
(defmethod ensure-system-name* ((system-definer (eql :glu)) (system asdf:system))
  (user::canonicalize-glu-system-name 
   (ensure-system-name* :asdf system)))

;;; ---------------------------------------------------------------------------

(defmethod extend-relative-pathname ((thing asdf:component))
  (multiple-value-bind (new kind)
      (compute-relative-pathname-extension thing)
    (when new
      (case kind
	(:absolute new)
	(:relative 
	 (make-pathname
	  :directory (append (pathname-directory *relative-pathname*)
			     (if (consp new) new (list new)))))))))

(defmethod compute-relative-pathname-extension ((thing asdf:component))
  (values (safe-slot-value thing 'asdf::relative-pathname) :relative))

(defmethod compute-relative-pathname-extension ((thing asdf:system))
  (values (safe-slot-value thing 'asdf::relative-pathname) :absolute))

(defmethod compute-relative-pathname-extension ((thing asdf:module))
  (let ((relative-pathname (safe-slot-value thing 'asdf::relative-pathname)))
    (values 
     (if relative-pathname
	 (rest (pathname-directory relative-pathname))
	 (list (safe-slot-value thing 'asdf::name)))
     :relative)))

(defmethod %map-system-files :around
    ((thing asdf:component) function 
     system-closure? include-pathname? include-non-source?)
  (let ((*relative-pathname* (or (extend-relative-pathname thing)
				 *relative-pathname*)))
    (call-next-method)))

(defmethod %map-system-files ((module asdf:module) function 
                              system-closure? include-pathname? include-non-source?)
  (mapc (lambda (thing)
	  ;;(format t "~%        ~A" thing)
	  (%map-system-files 
	   thing function system-closure? 
	   include-pathname? include-non-source?)) 
	(safe-slot-value module 'asdf::components)))

;;; ---------------------------------------------------------------------------

(defmethod %map-system-files ((system asdf:system) function 
                              system-closure? include-pathname? include-non-source?)
  (when system-closure?
    (dolist (system (collect-system-dependencies system))
      (%map-system-files 
       (find-system system) function system-closure? include-pathname? include-non-source?)))
  
  ;(format t "~%~A" system)
  (let ((*relative-pathname* (safe-slot-value system 'asdf::relative-pathname)))
    (mapc (lambda (thing)
            ;(format t "~%  ~A" thing)
            (%map-system-files 
             thing function system-closure? include-pathname? include-non-source?)) 
          (safe-slot-value system 'asdf::components))))

;;; ---------------------------------------------------------------------------

(defmethod %map-system-files ((file asdf:source-file) function  
                             system-closure? include-pathname? include-non-source?)
  (declare (ignore system-closure? include-non-source?))
  (handle-file file function include-pathname?))

;;; ---------------------------------------------------------------------------

(defmethod %map-system-files ((file asdf:static-file) function  
                              system-closure? include-pathname? include-non-source?)
  (declare (ignore system-closure?))
  (when include-non-source?
    (handle-file file function include-pathname?)))

;;; ---------------------------------------------------------------------------
  
(defun handle-file (file function include-pathname?)
  (let* ((full-name
          (make-pathname :name (safe-slot-value file 'asdf::name)
                         :defaults *relative-pathname*
                         :type (asdf:source-file-type 
                                file (safe-slot-value file 'asdf::parent))))
         (true-name (probe-file full-name)))
    ;;?? GWK 27 Mar 2006 - hack to get around probably correct allegro behavior!
    ;; problem is that sometimes the asdf::name is, e.g., tinaa.css so we get
    ;; a pathname with name "tinaa.css" and type "nil" and we don't know how 
    ;; to handle that downstream...
    (when true-name
      (setf true-name (namestring true-name))
      (if include-pathname?
        (funcall function true-name)
        (funcall function (pathname-name+type full-name))))))

#+(and ASDF OTHER)
;;?? this one doesn't use probe-file
(defmethod %map-system-files ((file asdf:source-file) function  
                              system-closure? include-pathname?)
  (let* ((full-name
          (make-pathname :name (safe-slot-value file 'asdf::name)
                         :defaults *relative-pathname*
                         :type (asdf:source-file-type 
                                file (safe-slot-value file 'asdf::parent))))
         (true-name full-name))
    (when true-name
      (if include-pathname?
        (funcall function true-name)
        (funcall function (make-pathname 
                           :name (safe-slot-value file 'asdf::name)
                           :type (asdf:source-file-type 
                                  file (safe-slot-value file 'asdf::parent))))))))

;;; ---------------------------------------------------------------------------

(defmethod system-property* 
           ((system-definer (eql :asdf)) system-name property-name &key (no-error? t))
  (let ((system (find-system system-name))
        (property-name (or 
                        (find-symbol (string-upcase (symbol-name property-name)) '#:asdf)
                        property-name)))
    (cond ((slot-exists-p system property-name)
           (safe-slot-value system property-name))
          ((getf (safe-slot-value system 'asdf::properties) property-name)
           ;; return it
           )
          ((not no-error?)
           (error "Property ~A does not exist for the ~A system named ~A"
                  property-name system-definer system-name)))))

;;; ---------------------------------------------------------------------------

(defmethod select-system-definer ((system-name asdf:system))
  :asdf)

