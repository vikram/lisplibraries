;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; * Debug helper code

(defmacro debug-only (&body body)
  (when *load-with-debug-p*
    `(progn ,@body)))

;;;; * Miscalaneous helper code

(defun new-random-key (hash-table key-length)
  (iter (for key = (strcat "_" (random-string key-length)))
        (for (values value foundp) = (gethash key hash-table))
        (while foundp)
        (finally (return key))))

(defun insert-with-new-key (hash-table key-length value)
  "helper method. generates random strings of length key-length until
  it finds one that isn't a key in hash-table and sets value to
  that. returns the new id."
  (let ((key (new-random-key hash-table key-length)))
    (setf (gethash key hash-table) value)
    key))

(defstruct (shared-hashtable-entry (:conc-name shsh-))
  (table nil)
  (lock nil)
  (last-purge-time nil :type (or null integer))
  (access-count-until-purge nil :type (or null integer)))

(defmacro define-shared-hashtable (name &rest args &key purge-interval-secs purge-interval-access
                                        purge-interval-size &allow-other-keys)
  (let ((entry-name (intern-concat (list "*" name "*")))
        (with-lock-held-name (intern-concat (list "WITH-LOCK-HELD-ON-" name))))
    (remf-keywords args :purge-interval-size :purge-interval-secs :purge-interval-access)
    `(progn
      (defparameter ,entry-name
        (make-shared-hashtable-entry :table (make-hash-table ,@args)
                                     :lock (make-lock ,(string-downcase (string entry-name)))
                                     :last-purge-time (get-universal-time)
                                     :access-count-until-purge ,purge-interval-access))
      (defmacro ,with-lock-held-name (&body body)
        `(with-lock-held ((shsh-lock ,',entry-name))
          ,@body))
      (defmacro ,(intern-concat (list "ENSURE-" name "-VALUE")) (key &body body)
        (with-unique-names (now entry value table)
          (declare (ignorable now))
          (rebinding (key)
            ;; TODO we should use a read-write lock here
            `(,',with-lock-held-name
              (let* ((,entry ,',entry-name)
                     (,table (shsh-table ,entry))
                     (,value (gethash ,key ,table)))
                ,,(when purge-interval-size
                        ``(when (> (hash-table-count (shsh-table ,entry)) ,',purge-interval-size)
                           (clrhash ,table)))
                ,,(when purge-interval-secs
                        ``(let ((,now (get-universal-time)))
                           (when (> (+ ,',purge-interval-secs (shsh-last-purge-time ,entry)) now)
                             (setf (shsh-last-purge-time ,entry) now)
                             (clrhash ,table))))
                ,,(when purge-interval-access
                        ``(when (minusp (decf (shsh-access-count-until-purge ,entry)))
                           (setf (shsh-access-count-until-purge ,entry) ,',purge-interval-access)
                           (clrhash ,table)))
                (unless ,value
                  (setf ,value (progn ,@body))
                  (when ,value
                    (setf (gethash ,key ,table) ,value)))
                ,value))))))))

;;;; ** Simple URL manipulation

;;;; May be replaced with something better (puri?) should the need
;;;; arise.

(defclass uri ()
  ((scheme   :initarg :scheme   :initform nil :accessor uri.scheme)
   (host     :initarg :host     :initform nil :accessor uri.host)
   (port     :initarg :port     :initform nil :accessor uri.port)
   (path     :initarg :path     :initform nil :accessor uri.path)
   (query    :initarg :query    :initform nil :accessor uri.query)
   (fragment :initarg :fragment :initform nil :accessor uri.fragment)))

(defprint-object (uri uri :identity nil)
  (write-uri uri *standard-output*))

(defun write-uri-sans-query (uri stream)
  "Write URI to STREAM, only write scheme, host and path."
  (with-slots (scheme host path)
      uri
    (when scheme
      (write-string scheme stream)
      (write-string "://" stream))
    (when host
      (write-string host stream)
      (write-string "/" stream))
    
    (if (or (position #\? path)
            (position #\= path)
            (position #\+ path))
        (loop
           for char across path
           do (case char
                (#\? (write-string "%3F" stream))
                (#\= (write-string "%3D" stream))
                (#\+ (write-string "%2B" stream))
                (t (write-char char stream))))
        (write-string path stream))))

(defun write-uri (uri stream)
  (with-slots (scheme host path query fragment)
      uri
    (write-uri-sans-query uri stream)
    (when query
      (flet ((write-query-part (name value)
               (write-as-uri name stream)
               (write-char #\= stream)
               (write-as-uri value stream)))
        (write-string "?" stream)
        (write-query-part (car (first query))
                          (cdr (first query)))
        (iterate
          (for (name . value) in (cdr query))
          (write-char #\& stream)
          (write-query-part name value))))
    (when fragment
      (write-char #\# stream)
      (write-string fragment stream))))

(defun print-uri-to-string (uri)
  (with-output-to-string (string)
    (write-uri uri string)))

(defun print-uri-to-string-sans-query (uri)
  (with-output-to-string (string)
    (write-uri-sans-query uri string)))

(defun read-from-client-string (str &rest opt-args)
  "Read a string from an untrusted source (don't eval anything)."
  (let ((*read-eval* nil))
    (apply #'read-from-string str opt-args))) ;;*read-eval*=nil


;;;
;;; enable-js-sharpqute-reader, enable-sharpquote<>-reader and with-sharpquote<>-syntax
;;;

(defmacro enable-js-sharpquote-reader ()
  "Enable quote reader for the rest of the file (being loaded or compiled).
#\"my i18n text\" parts will be replaced by a ucw.i18n.lookup call for the string.
Be careful when using in different situations, because it modifies *readtable*."
  `(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (set-dispatch-macro-character
     #\# #\"
     (lambda (s c1 c2)
       (declare (ignore c2))
       (unread-char c1 s)
       (let ((key (read s)))
         `(ucw.i18n.lookup ,key))))))

(defmacro enable-sharpquote<>-reader ()
  "Enable quote reader for the rest of the file (being loaded or compiled).
#\"my i18n text\" parts will be replaced by a lookup-resource call for the string.
Be careful when using in different situations, because it modifies *readtable*."
  ;; The standard sais that *readtable* is restored after loading/compiling a file,
  ;; so we make a copy and alter that. The effect is that it will be enabled
  ;; for the rest of the file being processed.
  `(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharpquote<>-reader)))

(defun with-sharpquote<>-syntax ()
  "To be used with the curly reader from arnesi: {with-sharpquote<>-reader (foo #\"locale-specific\") }"
  (lambda (handler)
    (%enable-sharpquote<>-reader)
    `(progn ,@(funcall handler))))

(defun %enable-sharpquote<>-reader ()
  (set-dispatch-macro-character
   #\# #\"
   (lambda (s c1 c2)
     (declare (ignore c2))
     (unread-char c1 s)
     (let ((key (read s)))
       (if (ends-with key "<>")
           `(multiple-value-bind (str foundp) (lookup-resource ,(subseq key 0 (- (length key) 2)) nil)
             (if foundp
                 (<:as-html str)
                 (<:span :class +missing-resource-css-class+
                         (<:as-html str))))
           `(lookup-resource ,key nil))))))

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
