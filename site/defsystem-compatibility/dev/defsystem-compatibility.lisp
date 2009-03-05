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

(defvar *ignore-errors?* nil)

(defvar *pathname-separator* nil)

(define-condition system-definer-not-found-error (error)
                  ((system-name :initform nil :initarg :system-name
                                :reader system-name))
  (:report (lambda (c s)
             (format s "No system definer found for '~S'" (system-name c)))))

;;; ---------------------------------------------------------------------------
;;; compatibility
;;; ---------------------------------------------------------------------------

(defparameter *prefered-system-definer*
  (or #+asdf :asdf
      #+:glu-generic-load-utils :glu))

(defmethod find-system* ((system-definer (eql :asdf)) system-name)
  (declare (ignorable system-definer system-name))
  #+asdf
  (asdf:find-system (ensure-system-name* :asdf system-name) nil))

(defmethod find-system* ((system-definer (eql :glu)) system-name)
  (declare (ignorable system-definer system-name))
  #+:GLU-GENERIC-LOAD-UTILS
  (let ((system (ensure-system-name* :glu system-name)))
    (when (and (symbolp system)
               (getf (symbol-plist system) :glu-system))
      system)))

(defparameter *prefered-definer-for-systems* (make-hash-table))

(defmethod select-system-definer ((system-name symbol))
  (select-system-definer% (intern (symbol-name system-name) '#:keyword)))

(defmethod select-system-definer ((system-name string))
  (select-system-definer% (intern (string-upcase system-name) '#:keyword)))

(defmethod select-system-definer ((system-name (eql nil)))
  (select-system-definer% nil))

(defun select-system-definer% (system-name)
  (let ((definer (gethash system-name *prefered-definer-for-systems*)))
    (or definer
        (setf (gethash system-name *prefered-definer-for-systems*)
              (let ((asdf-system (ignore-errors
                                  (find-system* :asdf system-name)))
                    (glu-system (ignore-errors
                                 (find-system* :glu system-name))))
                (cond ((eq system-name nil)
                       *prefered-system-definer*)
                      ((and asdf-system (eq *prefered-system-definer* :asdf))
                       :asdf)
                      ((and glu-system (eq *prefered-system-definer* :glu))
                       :glu)
                      (asdf-system
                       :asdf)
                      (glu-system
                       :glu)
                      (t
                       (error 'system-definer-not-found-error
                              :system-name system-name))))))))

#+(and asdf glu-generic-load-utils)
;; a bit weird
(defmethod ensure-system-name* ((system-definer (eql :glu)) (system asdf:system))
  (user::canonicalize-glu-system-name 
   (ensure-system-name* :asdf system)))

;;; ---------------------------------------------------------------------------
;;; level-0
;;; ---------------------------------------------------------------------------

(defun registered-systems ()
  "Returns a list of every system that has been registered / defined for the default system-definer."
  (registered-systems* (select-system-definer nil)))

;;; ---------------------------------------------------------------------------

(defun available-systems ()
  "Returns a list of the names of all of the systems that could be found for the default system definer."
  (available-systems* (select-system-definer nil)))

;;; ---------------------------------------------------------------------------

(defun loaded-systems ()
  "Returns a list of the names of the systems that are currently loaded for the default system-definer."
  (loaded-systems* (select-system-definer nil)))

;;; ---------------------------------------------------------------------------

(defun find-system (system-name)
  "Returns the system definition of system-name for its system-definer or nil if no such system exists."
  (find-system* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------

(defun associated-test-system (system-name)
  "Returns the name of the system that should be used to test system-name or nil if no such system exists."
  (associated-test-system* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------

(defun system-sub-systems (system-name)
  "Returns the complete list of the systems on which system-name depends; compare with system-dependencies."
  (system-sub-systems* (select-system-definer system-name) 
                       (ensure-system-name system-name)))

;;; ---------------------------------------------------------------------------

(defun system-property (system-name property-name &key (no-error? t))
  "Returns the value of the named property-name of system-name. If no-error? is nil, then an error will be signaled if property-name does not exist in the system."
  (system-property* (select-system-definer system-name)
                   (ensure-system-name system-name)
                   property-name :no-error? no-error?))

;;; ---------------------------------------------------------------------------

(defun system-source-file (system-name)
  "Returns the pathname of the system definition of system-name for its system-definer."
  (system-source-file* (select-system-definer system-name) 
                       (ensure-system-name system-name)))

;;; ---------------------------------------------------------------------------

(defun top-level-system-p (system-name)
  "Returns true if, metaphorically speaking, system-name is a system unto itself. Usually, this means that there are no systems that depends on the system but it could also be that the system is used by other systems but is also coherent in and of itself."
  (top-level-system-p* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------

(defun system-name-for-display (system-name)
  "Returns a 'nice name' for system-name. If no such special name is available, then the system-name is used."
  (system-name-for-display* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------

;;?? hacky implementation
(defun ensure-system-name (system-name)
  "Returns the canonical representation of the system's name."
  (if *ignore-errors?*
      (or (ignore-errors (ensure-system-name*
			  (select-system-definer system-name) system-name))
	  system-name)
      (ensure-system-name*
       (select-system-definer system-name) system-name)))

;;; ---------------------------------------------------------------------------

(defun filename-looks-like-system-file-p (filename)
  "Returns true if filename looks like a system file for the defaults system-definer. This is based only on the syntax if the file's name, not on the contents of the file."
  (filename-looks-like-system-file-p* (select-system-definer nil) filename))

;;; ---------------------------------------------------------------------------

(defun system-dependencies (system-name)
  "Returns a list of the systems on which system-name depends directly."
  (if *ignore-errors?* 
      (ignore-errors       
	(system-dependencies* (select-system-definer system-name) system-name))
      (system-dependencies* (select-system-definer system-name) system-name)))

;;; ---------------------------------------------------------------------------
;;; level-1
;;; ---------------------------------------------------------------------------

(defun system-source-directory (system-name)
  "Returns the pathname of the directory in which system-name lives."
  (make-pathname :name nil
                 :type nil
                 :defaults (system-source-file system-name)))

;;; ---------------------------------------------------------------------------

(defun system-loaded-p (system-name)
  (member (ensure-system-name system-name) (loaded-systems)))

;;; ---------------------------------------------------------------------------

(defun collect-system-dependencies 
    (systems &key (ignore-errors? *ignore-errors?*))
  "What's in a name...? This form attempts to find and load all of the referenced
system files starting with any systems in systems. It does not include the systems
themselves unless they are syb-systems of some other system in the list."
  (let ((systems-processed nil)
        (result nil)
	(*ignore-errors?* ignore-errors?))
    (labels ((compute-system-closure (system &optional (save? t))
               (setf system (ensure-system-name system))
               (unless (member system systems-processed)
                 (push system systems-processed)
                 (when save?
                   (push system result))
                 (dolist (sub-system (system-dependencies system))
                   (when save?
                     (push sub-system result))
                   (compute-system-closure sub-system)))))
      (setf systems-processed nil)
      (dolist (system (if (consp systems) systems (list systems)))
        (compute-system-closure system nil)))
    (remove-duplicates result)))

;;; ---------------------------------------------------------------------------

(defun map-system-dependencies (systems fn)
  ;; this is weak in that it cones up the whole list and then maps that 
  ;; but efficiency is probably not a big concern here...
  (mapc fn (collect-system-dependencies systems)))

;;; ---------------------------------------------------------------------------

(defmethod map-system-files ((system-names list) function &key 
                             (system-closure? nil) (include-pathname? t)
                             (include-non-source? nil))
  (let ((seen (make-hash-table :test #'equal)))
    (labels ((seen-p (thing)
               (gethash thing seen))
             (look-at (thing)
               (setf (gethash thing seen) t)))
      (dolist (system (if system-closure? 
                        (append 
                         (collect-system-dependencies system-names)
                         system-names)
                        system-names))
        (let ((system (find-system system)))
          (%map-system-files 
           system
           (lambda (thing)
             (unless (seen-p thing)
               (look-at thing)
               (funcall function thing)))
           system-closure?
           include-pathname?
           include-non-source?))))))

;;; ---------------------------------------------------------------------------

(defmethod map-system-files ((system-name symbol) function &rest args
                             &key (system-closure? nil) (include-pathname? t)
                             (include-non-source? nil))
  (declare (dynamic-extent args)
           (ignore system-closure? include-pathname? include-non-source?))
  (apply #'map-system-files (list system-name) function args))

;;; ---------------------------------------------------------------------------

(defmethod map-system-files ((system-name string) function &rest args
                             &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'map-system-files (list (intern system-name)) function args))

;;; ---------------------------------------------------------------------------

(defun collect-system-files (system/s &key 
                                      (system-closure? nil)
                                      (include-pathname? nil)
                                      (include-non-source? nil)
                                      (include-files? t)
                                      (options nil)
                                      )
  "Returns a list of the files in system/s. See map-system-files for a discussion of the arguments."
  (declare (ignorable include-pathname? include-associates? include-files? options))
  (let ((result nil))
    (map-system-files system/s 
                      (lambda (f)
                        (push f result))
                      :system-closure? system-closure?
                      :include-pathname? include-pathname?
                      :include-non-source? include-non-source?
                      ; :include-files? include-files?
                      ; :options options
                      )
    (values (nreverse result))))

;;; ---------------------------------------------------------------------------

(defparameter *pathname-cache* (make-hash-table :test #'equal))

;;?? very expensive
;;?? and not always correct (since multiple systems can contain the same name+type)
(defun pathname-for-system-file (system filename)
  "Returns the full pathname in system for filename (which is just the name+type)."
  (let ((name (pathname-name filename))
        (type (pathname-type filename)))
    (multiple-value-bind (value found?)
                         (gethash (cons name type) *pathname-cache*)
      (cond ((and (not (null found?))
                  (probe-file value))
             (values value))
            (t
             (let ((result 
                    (block look-for-it
                      (map-system-files 
                       system
                       (lambda (pathname)
                         (when (and (string-equal (pathname-name pathname) name)
                                    (string-equal (pathname-type pathname) type))
                           (return-from look-for-it pathname)))
                       :include-pathname? t
                       :system-closure? t
                       :include-non-source? t)
                      
                      ;; no dice
                      (values nil))))
               (when result
                 (setf (gethash (cons name type) *pathname-cache*) result))))))))

;;; ---------------------------------------------------------------------------

(defun system-signature (system-name)
  (ensure-pathname-separator-defined)
  (let ((source-file (system-source-file system-name))
	(source-directory (system-source-directory system-name)))
    (flet ((tag-for-file (path)
             (let ((name (namestring (enough-namestring path source-directory))))
               (when (and (plusp (length name))
                          (char= (aref name 0) (aref *pathname-separator* 0)))
                 (setf name (subseq name 1)))
               (cons name
                     (file-write-date path)))))
      (let ((result nil))
        (map-system-files 
         system-name
         (lambda (path)
           (push (tag-for-file path) result)) 
         :include-pathname? t
         :system-closure? nil
         :include-non-source? nil)
        (append (list (tag-for-file source-file))
                (nreverse result))))))

;;; ---------------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------------

(defun ensure-string (x)
  (typecase x
    (string x)
    (t (format nil "~A" x))))

(defun ensure-pathname-separator-defined ()
  (unless *pathname-separator*
    (setf *pathname-separator* (metatilities:physical-pathname-directory-separator))))

(defun system-relative-pathname (system pathname)
  (let ((directory (pathname-directory pathname)))
    (when (eq (car directory) :absolute)
      (setf (car directory) :relative))
    (merge-pathnames
     (make-pathname :name (pathname-name pathname)
                    :type (pathname-type pathname)
                    :directory directory)
     (dsc:system-source-directory system))))