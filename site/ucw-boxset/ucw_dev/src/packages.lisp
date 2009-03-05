;;; -*- lisp -*-

;;;; * The Packages

(defpackage :it.bese.ucw
  (:nicknames :ucw)
  (:use :common-lisp
        :it.bese.ucw.system
        :it.bese.arnesi
        :it.bese.yaclml
        :iterate
        :trivial-sockets)
  (:export
   ;; backend classes
   #:mod-lisp-backend
   #:multithread-mod-lisp-backend
   #:aserve-backend
   #:araneida-backend
   #:httpd-backend
   #:multithread-httpd-backend
   ;; random configuration options
   #:*inspect-components*
   #:external-format-for
   ;; rerl protocol
   #:*default-server*
   #:standard-server
   #:startup-server
   #:shutdown-server
   #:restart-server
   #:server.backend
   #:server.applications
   #:debug-on-error
   #:*debug-on-error*

   #:*context*
   #:context.window-component
   #:context.request
   #:context.response
   #:context.session
   #:with-dummy-context
   #:make-request-context
   
   #:startup-application
   #:shutdown-application
   #:restart-application
   #:register-application
   #:unregister-application
   #:*default-application*
   #:standard-application
   #:cookie-session-application
   #:cookie-session-request-context

   ;; modular application and modules
   #:modular-application
   #:modular-application-mixin
   #:cookie-session-application-module
   #:secure-application-module
   #:l10n-application-module
   #:user-track-module
   #:application.online-users
   #:effective-context-definition
   
   ;; accessing the request/response objects
   #:mime-part-p
   #:mime-part-headers
   #:mime-part-body
   
   #:request
   #:response
   #:html-stream
   #:close-request
   #:get-header
   #:get-parameter
   #:map-parameters
   
   #:send-headers
   #:send-response
   #:serve-sequence
   ;; backtracking
   #:backtrack
   #:backtrack-slot
   ;; components
   #:defcomponent
   #:compute-url
   #:update-url
   #:standard-component-class
   #:component
   #:parent
   #:widget-component
   #:standard-component
   #:template-component
   #:template-component-environment
   #:simple-template-component
   #:show
   #:show-window
   #:html-element
   #:widget-component
   #:inline-widget-component
   #:css-class
   #:css-style
   #:child-components
   
   ;; windows
   #:window-component
   #:simple-window-component
   #:window-component.icon
   #:window-component.stylesheet
   #:window-component.javascript
   #:window-component.title
   #:window-component.content-type
   
   ;; generic componet actions
   #:refresh-component
   #:ok
   #:meta-refresh
   
   ;; error message component
   #:error-message
   #:error-component
   
   ;; login component
   #:login
   #:login.username
   #:login.password
   #:try-login
   #:check-credentials
   #:login-successful

   ;; info-message component
   #:info-message

   ;; option dialog component
   #:option-dialog
   #:respond

   ;; container component
   #:container
   #:switching-container
   #:list-container
   #:make-list-container
   #:component-at
   #:add-component
   #:remove-component
   #:clear-container
   #:container.current-component-key
   #:container.current-component
   #:container.key-test
   #:container.contents
   #:switch-component
   #:find-component
   #:initialize-container

   ;; inspector
   #:ucw-inspector
   #:inspect-anchor
   
   ;; forms
   #:form-field
   #:generic-html-input
   #:dom-id
   #:value
   #:client-value
   #:tabindex
   
   #:simple-form

   #:string-field
   #:textarea-field
   #:number-field
   #:integer-field
   #:select-field
   #:password-field
   #:checkbox-field
   #:file-upload-field
   #:alist-select-field
   #:hash-table-select-field
   #:plist-select-field
   #:submit-button
   #:radio-group
   #:value-widget
   #:in-field-string-field
   #:date-field
   #:date-ymd
   #:date-in-range-validator
   #:dmy-date-field
   #:mdy-date-field

   #:validator
   #:validators
   #:generate-javascript
   #:generate-javascript-check
   #:javascript-check
   #:generate-javascript-valid-handler
   #:javascript-valid-handler
   #:generate-javascript-invalid-handler
   #:javascript-invalid-handler
   #:validp
   #:is-an-integer-validator
   #:number-range-validator  
   #:length-validator
   #:min-length
   #:max-length
   #:not-empty-validator
   #:string=-validator
   #:integer-range-validator
   #:regex-validator
   #:regex
   #:e-mail-address-validator
   #:phone-number-validator
  
   ;; range-view component
   #:range-view
   #:render-range-view-item
   #:range-view.current-window
   #:range-view.current-window-items
   #:range-view.windows
   #:range-view.have-next-p
   #:range-view.have-previous-p
   
   ;; the date picker component
   #:generic-date-picker
   #:dropdown-date-picker
   #:date-picker.year
   #:date-picker.day
   #:date-picker.month
   #:date-picker.partial-date-p
   #:date-picker.complete-date-p

   #:redirect-component
   #:send-redirect
   
   ;; the tabbed-pane component
   #:tabbed-pane

   ;; the task component
   #:task-component
   #:start

   ;; status bar component
   #:status-bar
   #:add-message
   #:show-message

   ;; cache
   #:cached-component
   #:cached-output
   #:timeout
   #:component-dirty-p
   #:refresh-component-output
   #:timeout-cache-component
   #:num-hits-cache-component
   
   ;; transactions
   #:transaction-mixin
   #:open-transaction
   #:close-transaction

   ;; secure application
   #:secure-application-mixin
   #:secure-application-p
   #:application-find-user
   #:application-check-password
   #:application-authorize-call
   #:on-authorization-reject
   #:session-user
   #:session-authenticated-p
   #:user-login
   #:login-user
   #:logout-user
   #:exit-user
   
   ;; actions
   #:defaction
   #:defentry-point
   #:self
   #:call
   #:call-component
   #:call-as-window
   #:answer
   #:answer-component
   #:jump
   #:jump-to-component
   #:component.place
   #:make-place
   #:action-href
   #:action-href-body

   ;; disptachers
   #:minimal-dispatcher
   #:simple-dispatcher
   #:url-dispatcher
   #:regexp-dispatcher
   #:action-dispatcher
   #:tal-dispatcher
   #:parenscript-dispatcher

   #:with-request-params
   
   ;; session
   #:get-session-value
   #:session.value
   #:make-new-session
   #:with-session-variables
   
   #:register-action
   #:register-callback
   #:make-new-callback
   ;; yaclml/tal
   #:*ucw-tal-root*
   #:render
   #:render-template

   ;; publishing files, directories and other "stuff"
   #:publish-directory

   ;; Helper functions
   #:read-from-client-string

   ;; Control utilities
   #:start-swank
   #:create-server
   #:hello
   #:bye-bye))

(defpackage :it.bese.ucw-user
  (:nicknames :ucw-user)
  (:use :common-lisp
        :it.bese.ucw
        :it.bese.arnesi
        :it.bese.yaclml))

(defpackage :it.bese.ucw.tags
  (:documentation "UCW convience tags.")
  (:use)
  (:nicknames #:<ucw)
  (:export
   #:render-component
   #:a
   #:area
   #:form
   #:input
   #:button
   #:select
   #:option
   #:textarea

   #:integer-range-select
   #:month-day-select
   #:month-select

   #:text
   #:password
   #:submit
   #:simple-form
   #:simple-submit
   #:hidden
   #:script))

;;;;@include "rerl/protocol.lisp"

;;;; * Components

;;;;@include "components/login.lisp"

;;;;@include "components/error.lisp"

;;;;@include "components/message.lisp"

;;;;@include "components/option-dialog.lisp"

;;;;@include "components/range-view.lisp"

;;;;@include "components/redirect.lisp"

;;;;@include "components/tabbed-pane.lisp"

;;;;@include "components/task.lisp"

;;;;@include "components/ucw-inspector.lisp"

;;;; * Meta Components

;;;;@include "components/widget.lisp"

;;;;@include "components/window.lisp"

;;;;@include "components/template.lisp"

;;;;@include "components/container.lisp"

;;;; * Standard RERL Implementation

;;;;@include "rerl/standard-server.lisp"

;;;;@include "rerl/standard-application.lisp"

;;;;@include "rerl/standard-session.lisp"

;;;;@include "rerl/cookie-session.lisp"

;;;;@include "rerl/standard-session-frame.lisp"

;;;;@include "rerl/standard-action.lisp"

;;;;@include "rerl/backtracking.lisp"

;;;;@include "rerl/request-loop-error.lisp"

;;;;@include "rerl/conditions.lisp"

;;;;@include "rerl/standard-vars.lisp"

;;;; ** Standard Component

;;;; * The Backends

;;;;@include "backend/httpd.lisp"

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
