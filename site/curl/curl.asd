;;; -*-  Lisp -*-
;********************************************************
; file:        curl.asd                                  
; description: System definition for curl.               
; date:        Sun Mar  6 2005 - 10:29                   
; author:      Liam M. Healy <cl@healy.washington.dc.us>
; modified:    Mon Aug 15 2005 - 23:12
;********************************************************
;;; $Id: $

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :uffi)
  ;; (clc:clc-require :uffi)
  )

(defpackage #:curl (:use cl asdf))
(in-package #:curl)

;;; we also have a shared library with some .o files in it

(format t "~&starting")

(defclass unix-dso (module) ())
(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defmethod asdf::input-files ((operation compile-op) (dso unix-dso))
  (mapcar #'component-pathname (module-components dso)))

(defmethod output-files ((operation compile-op) (dso unix-dso))
  (let ((dir (component-pathname dso)))
    (list
     (make-pathname :type #-darwin "so" 
                          #+darwin "dylib"
		    :name (car (last (pathname-directory dir)))
		    :directory (butlast (pathname-directory dir))
		    :defaults dir))))

(defmethod perform :after ((operation compile-op) (dso unix-dso))
  (let ((dso-name (unix-name (car (output-files operation dso)))))
    (unless (zerop
	     (run-shell-command
	      "gcc ~A -o ~S ~{~S ~}"
	      #-(or x86-64 darwin)
	      "-fPIC -shared -lcurl"
	      #+x86-64
	      ;; For some reason, SBCL x86-64 gets a segmentation violation
	      ;; unless compiled -g
	      "-g -fPIC -shared -lcurl"
              #+darwin
              "-dynamiclib -lcurl" 
	      dso-name
	      (mapcar #'unix-name
		      (mapcan (lambda (c)
				(output-files operation c))
			      (module-components dso)))))
      (error 'operation-error :operation operation :component dso))))

;;; if this goes into the standard asdf, it could reasonably be extended
;;; to allow cflags to be set somehow
(defmethod output-files ((op compile-op) (c c-source-file))
  (list 
   (make-pathname :type "o" :defaults
		  (component-pathname c))))
(defmethod perform ((op compile-op) (c c-source-file))
  (unless
      (= 0 (run-shell-command "gcc ~A -o ~S -c ~S"
			      #-darwin "-fPIC -shared -lcurl"
			      #+darwin "-dynamiclib"
			      (unix-name (car (output-files op c)))
			      (unix-name (component-pathname c))))
    (error 'operation-error :operation op :component c)))

(defmethod perform ((operation load-op) (c c-source-file))
  t)
  
;;; Load the .so library
(defmethod perform ((o load-op) (c unix-dso))
  (let ((co (make-instance 'compile-op)))
    (let ((filename (car (output-files co c))))
      (uffi:load-foreign-library filename))))

(defsystem curl
    :version "0.10"
    :depends-on (uffi)
    :components
    ((:unix-dso "clcurl"
		:components ((:c-source-file "curl")))
     (:file "curl" :depends-on ("clcurl"))))

(defmethod perform :after ((o load-op) (c (eql (find-system :curl))))
  (provide 'curl))

(defmethod perform ((o test-op) (c (eql (find-system :curl))))
  (operate 'load-op 'curl)
  (operate 'test-op 'curl))

(unuse-package :asdf)
