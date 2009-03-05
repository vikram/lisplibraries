;;;;
;;;; Copyright (c) 2008 Zachary Beane, All Rights Reserved
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;
;;;; package.lisp

(defpackage #:zs3
  (:use #:cl)
  ;; In documentation order and grouping:
  ;; Credentials
  (:export #:*credentials*
           #:access-key
           #:secret-key
           #:file-credentials)
  ;; Buckets
  (:export #:all-buckets
           #:creation-date
           #:name
           #:all-keys
           #:bucket-exists-p
           #:create-bucket
           #:delete-bucket
           #:bucket-location)
  ;; Bucket queries
  (:export #:query-bucket
           #:continue-bucket-query
           #:bucket-name
           #:keys
           #:common-prefixes
           #:prefix
           #:marker
           #:delimiter
           #:truncatedp
           #:last-modified
           #:etag
           #:size
           #:owner)
  ;; Objects
  (:export #:get-object
           #:get-vector
           #:get-string
           #:get-file
           #:put-object
           #:put-vector
           #:put-string
           #:put-file
           #:put-stream
           #:copy-object
           #:delete-object
           #:delete-objects
           #:delete-all-objects
           #:object-metadata)
  ;; Access Control
  (:export #:get-acl
           #:put-acl
           #:grant
           #:acl-eqv
           #:*all-users*
           #:*aws-users*
           #:*log-delivery*
           #:acl-email
           #:acl-person
           #:me
           #:make-public
           #:make-private)
  ;; Logging
  (:export #:enable-logging-to
           #:disable-logging-to
           #:enable-logging
           #:disable-logging
           #:logging-setup)
  ;; Misc.
  (:export #:*use-ssl*
           #:make-post-policy
           #:head
           #:authorized-url
           #:resource-url)
  ;; Util
  (:export #:octet-vector
           #:now+
           #:now-
           #:file-etag
           #:parameters-alist
           #:clear-redirects)
  ;; Conditions
  (:export #:slow-down
           #:no-such-bucket
           #:no-such-key
           #:access-denied
           #:signature-mismatch
           #:precondition-failed
           #:invalid-bucket-name
           #:bucket-exists
           #:too-many-buckets
           #:ambiguous-grant
           #:bucket-not-empty
           #:invalid-logging-target
           #:key-too-long
           #:request-time-skewed
           #:operation-aborted)
  (:shadow #:method))



