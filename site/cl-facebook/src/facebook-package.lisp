(defpackage :cl-facebook
  (:nicknames #:fb #:facebook)
  (:use :cl)
  (:export
   ;; conditions
   #:facebook-error
   #:facebook-net-error
   #:facebook-server-error
   #:feed-action-limit-error
   ;; utility
   #:generate-signature
   #:verify-form-params
   #:decode-form-params
   #:decode-form-uids
   ;; facebook-session properties
   #:api-key
   #:secret
   #:session-key
   #:uid
   ;; misc
   #:profile-uri
   ;; methods for calling facebook API
   #:establish-facebook-session
   #:make-session
   #:session-request

   #:execute-query

   #:publish-action-of-user
   #:tpublish-story-to-user

   #:set-profile-markup
   #:get-profile-markup

   #:send-notification-request
   #:send-notification
   #:get-notifications
   )
  )

(defpackage :cl-facebook-user
  (:nicknames :facebook-user :fb-user)
  (:use :cl :cl-facebook))
