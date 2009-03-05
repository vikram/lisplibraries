;; -*- lisp -*-

;;;; ** Parsing Accept-* headers

;;;; currently we only deal with Accept-Language

(in-package :it.bese.ucw)

(defun make-string-buffer ()
  (make-array 2 :element-type 'character :adjustable t :fill-pointer 0))

(defun without-chars (string &optional (char-bag '(#\Space #\Tab #\Newline #\Linefeed)))
  (with-output-to-string (new-string)
    (loop for char across string
          unless (member char char-bag)
            do (write-char char new-string))))

(defun parse-accept-language-header (header-string)
  (ucw.backend.dribble "Parsing Accept header ~S." header-string)
  (let ((accepts '())
        (lan (make-string-buffer))
        (scr (make-string-buffer))
        (q (make-string-buffer))
        (index 0)
        (header (without-chars header-string)))
    (labels ((next-char ()
               (unless (<= (length header) index)
                 (aref header index)))
             (read-next-char ()
               (prog1
                   (next-char)
                 (incf index)))
             (next-accept ()
               (push (list lan scr q) accepts)
               (setf lan (make-string-buffer)
                     scr (make-string-buffer)
                     q (make-string-buffer))))
      (loop
         with state = :lan
         for char = (read-next-char)
         while char
         do (case state
              (:lan
               (case char
                 (#\- (setf state :scr))
                 (#\; (setf state :q))
                 (#\, (setf state :lan)
                      (next-accept))
                 (t (vector-push-extend char lan)
                    (unless (next-char)
                      (next-accept)))))
              (:scr
               (case char
                 (#\; (setf state :q))
                 (#\, (setf state :lan) (next-accept))
                 (t (vector-push-extend char scr)
                    (unless (next-char)
                      (next-accept)))))
              (:q
               (read-next-char)
               (loop
                  for char = (read-next-char)
                  while char
                  until (char= #\, char)
                  do (vector-push-extend char q))
               (setf state :lan)
               (next-accept)))))
    (sort (mapcar (lambda (acc)
                    (when (string= "" (first acc))
                      (error "Badly formatted accept-languages header."))
                    (let ((quality (if (not (string= "" (third acc)))
                                       (parse-float (third acc))
                                       1)))
                      (if (string= "" (second acc))
                          (list (first acc) quality)
                          (list (concatenate 'string (first acc) "_" (string-upcase (second acc)))
                                quality))))
                  (nreverse accepts))
          #'> :key #'second)))

;; Copyright (c) 2003-2006 Edward Marco Baringer
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
