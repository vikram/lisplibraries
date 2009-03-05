;;;; -*- lisp -*-

(in-package :it.bese.ucw-user)

(eval-always
  (ucw::import-cl-l10n-symbols (find-package :it.bese.ucw-user)))

(enable-sharpquote-reader)

(defpackage :it.bese.ucw.lang
  (:nicknames :lang)
  (:use :cl
        :ucw
        :arnesi
        :iterate
        :cl-l10n))

(eval-always
  (with-resource-package :lang
    ;; make sure that at least one locale is loaded so that some functions
    ;; get defined and we don't get warnings
    (locale "en_US")
    ;; and reload the standard cl-l10n resources into our package
    ;; so that the common symbols get created (plural-of, etc)
    (reload-resources)))

(in-package :lang)

(eval-always
  ;; and now we define our own resources. this should be in a separate file
  ;; in more complex situation. but an eval-when will do here...
  (defresources en
    (this-is-your-locale-preference "This is the locale preference list we extracted from your browser's request:")
    (plural-message<> (word)
                      (<:as-html (lang:with-indefinit-article word :capitalize-first-letter t)
                                 ", many "
                                 (lang:plural-of word))))

  (defresources hu
    (this-is-your-locale-preference "A böngésző beállításai alapján összeállított lokalizációs preferencia lista a következő:")
    (plural-message<> (word)
                      (<:as-html (with-indefinit-article word :capitalize-first-letter t)
                                 ", "
                                 (lang:plural-of word)))))

(in-package :it.bese.ucw-user)

(defparameter *l10n-example-application*
  (make-instance 'l10n-application
                 :url-prefix "/l10n/"
                 :resource-package :lang
                 :default-locale "en"
                 ;;:accepted-locales (list "it_IT" "en")
                 :tal-generator
                 (make-instance 'l10n-tal-generator
                                :root-directories
                                (list (merge-pathnames (make-pathname :directory '(:relative "wwwroot" "ucw" "examples" "l10n"))
                                                       (asdf:component-pathname (asdf:find-system :ucw)))))))

(defcomponent l10n-example-window (simple-window-component)
  ((body :component
         (list-container :orientation :vertical
                         :contents (list (make-instance 'l10n-plural-component)
                                         (make-instance 'l10n-example-tal-component)))
         :accessor body-of))
  (:default-initargs :title "UCW l10n example"
    :stylesheet (list "/ucw/ucw.css" "/ucw/examples/examples.css"))
  (:render (self)
           (render (body-of self))))

(defentry-point "" (:application *l10n-example-application*) ()
  (call 'l10n-example-window))

(defcomponent l10n-example-tal-component (template-component)
  ()
  (:default-initargs :template-name "l10n-example.tal"))

(defcomponent l10n-plural-component (widget-component)
  ((input-field :initform (make-instance 'string-field) :accessor input-field-of))
  (:render (self)
           (<:p (<:as-html #"this-is-your-locale-preference" " ")
                (iter:iter (iter:for locale :in (ensure-list cl-l10n::*locale*))
                           (unless (iter:first-time-p)
                             (<:as-html ", "))
                           (<:as-html (locale-name locale))))
           (<ucw:simple-form
            (<:table (<:tr (<:td (render (input-field-of self))
                                 (<ucw:simple-submit "pluralize!"))
                           (<:td (awhen (string-trim '(#\Space #\Tab) (value (input-field-of self)))
                                   (unless (zerop (length it))
                                     (lang:plural-message<> it)))))))))

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
