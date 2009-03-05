;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n.system)

(defpackage #:cl-l10n 
  (:use #:cl #:cl-ppcre #:cl-fad #:arnesi #:iterate)
  (:shadow cl:format cl:formatter)
  (:shadowing-import-from :cl-fad
                          #:copy-stream #:copy-file)
  (:export #:locale-name
           #:category-name
           #:locale
           #:current-locale
           #:category
           #:locale-error
           #:get-category
           #:locale-value
           #:load-all-locales
           #:normalize-locale-list
           #:get-locale
           #:set-locale
           #:with-locale
           #:*locale-path*
           #:*locale*
           #:load-default-locale
           #:parser-error
           
           #:format-number
           #:print-number
           #:format-money
           #:print-money
           #:format-time
           #:print-time
           #:parse-number

           #:*float-digits*
           #:shadow-format
           #:parse-time

           #:*resource-package*
           #:with-resource-package
           #:reload-resources
           
           #:capitalize-first-letter
           #:capitalize-first-letter!

           #:lookup-resource
           #:lookup-resource-without-fallback
           #:localize
           #:resource-missing
           #:defresources
           #:enable-sharpquote-reader
           #:with-sharpquote-reader
           #:lookup-first-matching-resource

           #:consonantp
           #:vowelp
           #:high-vowel-p
           #:low-vowel-p
           #:last-vowel-of
           #:starts-with-consonant-p
           #:starts-with-vowel-p
           
           #:english-plural-of
           #:english-indefinit-article-for
           #:hungarian-plural-of
           ))

           ;; attila: these symbols are very frequent and cause a lot of headaches
           ;; when integrating cl-l10n into other projects. they should
           ;; be renamed to *p and *-p if we really want to export them
           ;;#:month #:day #:year #:hour #:minute #:second
           ;;#:date-divider #:time-divider #:weekday #:noon-midn
           ;;#:secondp #:am-pm #:zone
