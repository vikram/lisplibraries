;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/packages.lisp,v 1.47 2006/09/30 13:20:25 edi Exp $

;;; Copyright (c) 2004-2006, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-user)

(defpackage #:tbnl
  (:use #:cl #:url-rewrite)
  (:shadow #:url-encode)
  ;; see ASDF system definition
  (:import-from :tbnl-asd #:*tbnl-version*)
  (:export #:*backtrace*
           #:*body*
           #:*catch-errors-p*
	   #:*command*
           #:*content-types-for-url-rewrite*
           #:*debug-mode*
           #:*error*
           #:*default-content-type*
           #:*default-handler*
           #:*file-upload-hook*
           #:*http-error-handler*
           #:*default-log-level*
           #:*dispatch-table*
           #:*lisp-errors-log-level*
           #:*lisp-warnings-log-level*
           #:*listener*
           #:*log-lisp-backtraces-p*
           #:*log-lisp-errors-p*
           #:*log-lisp-warnings-p*
           #:*log-prefix*
           #:*reply*
           #:*request*
           #:*rewrite-for-session-urls*
           #:*session*
           #:*session-cookie-name*
           #:*session-max-time*
           #:*session-removal-hook*
           #:*show-access-log-messages*
           #:*show-lisp-backtraces-p*
           #:*show-lisp-errors-p*
           #:*tbnl-port*
           #:*tmp-directory*
           #:*use-apache-log*
           #:*use-user-agent-for-sessions*
           #:*use-remote-addr-for-sessions*
           #:*tbnl-default-external-format*
           #:+http-authorization-required+
           #:+http-forbidden+
           #:+http-internal-server-error+
           #:+http-moved-permanently+
           #:+http-moved-temporarily+
           #:+http-not-modified+
           #:+http-not-found+
           #:+http-ok+
           #:+http-continue+
           #:+http-switching-protocols+
           #:+http-ok+
           #:+http-created+
           #:+http-accepted+
           #:+http-non-authoritative-information+
           #:+http-no-content+
           #:+http-reset-content+
           #:+http-partial-content+
           #:+http-multiple-choices+
           #:+http-moved-permanently+
           #:+http-moved-temporarily+
           #:+http-see-other+
           #:+http-not-modified+
           #:+http-use-proxy+
           #:+http-temporary-redirect+
           #:+http-bad-request+
           #:+http-authorization-required+
           #:+http-payment-required+
           #:+http-forbidden+
           #:+http-not-found+
           #:+http-method-not-allowed+
           #:+http-not-acceptable+
           #:+http-proxy-authentication-required+
           #:+http-request-time-out+
           #:+http-conflict+
           #:+http-gone+
           #:+http-length-required+
           #:+http-precondition-failed+
           #:+http-request-entity-too-large+
           #:+http-request-uri-too-large+
           #:+http-unsupported-media-type+
           #:+http-requested-range-not-satisfiable+
           #:+http-expectation-failed+
           #:+http-internal-server-error+
           #:+http-not-implemented+
           #:+http-bad-gateway+
           #:+http-service-unavailable+
           #:+http-gateway-time-out+
           #:+http-version-not-supported+
           #:+latin-1+
           #:+utf-8+
           #:authorization
           #:aux-request-value
           #:content-length
           #:content-type
           #:cookie-domain
           #:cookie-expires
           #:cookie-in
           #:cookies-in
           #:cookie-name
           #:cookie-out
           #:cookies-out
           #:cookie-path
           #:cookie-value
           #:cookie-secure
           #:create-folder-dispatcher-and-handler
           #:create-prefix-dispatcher
           #:create-regex-dispatcher
           #:create-static-file-dispatcher-and-handler
           #:default-dispatcher
           #:define-easy-handler
           #:delete-aux-request-value
           #:delete-session-value
           #:dispatch-easy-handlers
           #:dispatch-request
           #:do-sessions
           #:escape-for-html
           #:get-backtrace
           #:get-parameter
           #:get-parameters
           #:handle-if-modified-since
           #:handle-static-file
           #:raw-post-data
           #:header-in
           #:headers-in
           #:header-out
           #:headers-out
           #:http-token-p
           #:host
           #:log-file
           #:log-message
           #:log-message*
           #:mod-lisp-id
           #:no-cache
           #:parameter
           #:post-parameter
           #:post-parameters
           #:query-string
           #:read-from-string*
           #:real-remote-addr
           #:recompute-request-parameters
           #:redirect
           #:rfc-1123-date
           #:referer
           #:remote-addr
           #:remote-port
           #:request-method
           #:request-uri
           #:require-authorization
           #:reset-sessions
           #:return-code
           #:script-name
           #:server-addr
           #:server-port
           #:server-protocol
           #:session-counter
           #:session-max-time
           #:session-remote-addr
           #:session-string
           #:session-user-agent
           #:session-value
           #:set-cookie
           #:set-cookie*
           #:ssl-session-id
           #:send-headers
           #:start-session
           #:start-tbnl
           #+:lispworks #:start-server
           #:stop-tbnl
           #+:lispworks #:stop-server
           #:tbnl-araneida-handler
           #:tbnl-handler-done
           #:url-decode
           #:url-encode
           #:user-agent))

(defpackage #:tbnl-contrib
  (:use #:cl #:tbnl))
