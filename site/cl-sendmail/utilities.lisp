;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utilities.lisp: General utilities useful for sendmail
;;;; Copyright (C) 2006 Robert Marlow <bobstopper@bobturf.org>
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :sendmail)


(defun split-string (splitter string segment-size)
  (let* ((string-length (length string))
         (out-string
          (make-string (+ string-length
                          (* (floor (/ string-length segment-size))
                             (length splitter))))))
    (do* ((i 0 (1+ i))
          (j 0 (1+ j))
          (newline-p nil (= (mod i segment-size) 0)))
         ((>= i string-length))
      (when newline-p
        (do ((k 0 (1+ k)))
            ((>= k (length splitter)))
          (setf (char out-string j) (char splitter k))
          (incf j)))
      (setf (char out-string j) (char string i)))
    out-string))


(defun read-file (pathname)
  "Reads a file and returns an unsigned byte array holding the contents"
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((out (make-array (file-length in)
			   :element-type '(unsigned-byte 8))))
      (read-sequence out in)
      out)))

