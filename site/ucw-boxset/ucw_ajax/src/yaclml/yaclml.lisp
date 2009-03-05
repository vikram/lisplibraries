;;;; -*- lisp -*-

(in-package :it.bese.ucw)

(defmethod render-template ((context request-context) template-name environment)
  (let ((*print-pretty* nil)) 
    (ucw.component.render.dribble
     "Rendering template ~S in environment ~S" template-name environment))
  (if-bind generator
      (application.tal-generator (context.application context))
    (if-bind truename
        (template-truename generator template-name)
      (%render-template context generator truename environment)
      (progn
        (cerror "Retry rendering the template." "Can't find a template named ~S." template-name)
        (render-template context template-name environment)))
    (error "No known generator for the current application.")))

(defmethod %render-template ((context request-context) generator truename environment)
  (let ((yaclml:*uri-to-package* (cons (cons "http://common-lisp.net/project/ucw/core"
                                             (find-package :it.bese.ucw.tags))
                                       yaclml:*uri-to-package*)))
    (restart-case
        (funcall (load-tal generator truename) environment generator)
      (retry ()
        :report (lambda (stream)
                  (format stream "Retry rendering ~A." truename))
        (return-from %render-template (%render-template context generator truename environment)))))
           (ucw.component.render.dribble "Template rendered."))

(defmethod preprocess-template (template-name environment &optional (application *default-application*))
  (aif (application.tal-generator application)
       (if-bind truename (template-truename it template-name)
           (let ((yaclml:*uri-to-package* (cons (cons "http://common-lisp.net/project/ucw/core"
                                                      (find-package :it.bese.ucw.tags))
                                                yaclml:*uri-to-package*)))
             (yaclml::preprocess-tal it truename))
           (progn
             (cerror "Retry rendering the template." "Can't find a template named ~S." template-name)
             (render-template *context* template-name environment)))
       (error "No known generator for the current application.")))

(defun add-session-id (url)
  "Add a session-id parametet to URL unless one is already present."
  (when (position #\# url)
    (error "Can't handle ~S.

Adding session ids to links with section parts is not yet supported."
           url))
  (flet ((already-have-session-id ()
           (let ((param-name-offset (search +session-parameter-name+ url)))
             (if param-name-offset
                 (let ((=-offset (+ param-name-offset
                                    (length +session-parameter-name+))))
                   (and (< =-offset (length url))
                        (char= #\= (aref url =-offset))))
                 nil))))
    (if (already-have-session-id)
        url
        (with-output-to-string (new-url)
          (write-sequence url new-url)
          (if (position #\? url)
              (write-char #\& new-url)
              (write-char #\? new-url))
          (write-sequence +session-parameter-name+ new-url)
          (write-char #\= new-url)
          (write-sequence (session.id (context.session *context*)) new-url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003-2005 Edward Marco Baringer
;;; All rights reserved. 
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;; 
;;;  - Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;;  - Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;    of its contributors may be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
