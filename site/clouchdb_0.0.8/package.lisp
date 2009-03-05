;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2007 Peter Eddy. All rights reserved.

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(cl:in-package :cl-user)

(defpackage :clouchdb
  (:use :cl :drakma :flexi-streams :s-base64)
  (:export :*scheme*
	   :*host*
	   :*port*
	   :*db-name*
           :*document-update-fn*
           :*document-fetch-fn*
           :as-keyword-symbol
           :as-field-name-string
	   :db-existential-error
	   :db-does-not-exist
	   :db-already-exists
	   :doc-error
	   :id-or-revision-conflict
	   :id-missing
	   :document-missing
	   :document-to-json
           :json-to-document
	   :document-as-hash
           :encode-document
	   :set-connection
	   :with-connection
	   :document-properties
	   :document-property
           :document-id
           :query-document
           :set-document-property
	   :list-dbs
	   :create-db
	   :delete-db
	   :create-temp-db
	   :create-temp-db-name
	   :with-temp-db
	   :get-db-info
	   :get-all-documents
	   :get-document
	   :put-document
	   :post-document
	   :create-document
	   :bulk-document-update
	   :delete-document
	   :create-view
	   :delete-view
	   :invoke-view
	   :ad-hoc-view))
