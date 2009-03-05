(defpackage "ARANEIDA"
  (:export
   ;; client
   ;; with-url-stream
   ;; daemon
   :install-serve-event-handlers :remove-serve-event-handlers
   :define-page
   ;; exports (deprecated)
   :export-server :export-handler :output-apache-conf   
   ;; listeners
   :http-listener :threaded-http-listener :serve-event-http-listener
   :https-listener :threaded-https-listener :serve-event-https-listener
   :reverse-proxy-listener-mixin  :reverse-proxy-translations
   :serve-event-reverse-proxy-listener 
   :threaded-reverse-proxy-listener 
   :http-listener-handler :apache-conf-segment
   :start-listening :stop-listening :listening-p
   :*restart-on-handler-errors*
   ;; file-request
   :*content-types* :read-mime-types :file-request-handler :static-file-handler
   :send-file
   ;; send files with Parenscript integration
   #+parenscript :css-file #+parenscript :js-file
   ;; handler
   :install-handler :uninstall-handler :find-handler
   :child-handlers :handler :dispatching-handler
   :response-sent :handle-request
   :handle-request-authorization :handle-request-response
   :handle-request-authentication
   :handle-request-logging
   ;; http-error
   :http-error :http-error-code :http-error-message :http-error-client-message
   :http-bad-request   :http-unauthorized   :http-payment-required
   :http-forbidden   :http-not-found   :http-method-not-allowed
   :http-not-acceptable   :http-proxy-authentication-required
   :http-request-time-out   :http-conflict   :http-gone
   :http-length-required   :http-precondition-failed
   :http-request-entity-too-large   :http-request-url-too-large
   :http-unsupported-media-type   :http-internal-server-error
   :http-not-implemented   :http-bad-gateway   :http-service-unavailable
   :http-gateway-time-out   :http-version-not-supported
   ;; redirect-handler
   :redirect-handler :redirect-location
   ;; html
   :html :html-escape :html-stream :html-escaped-stream :search-html-tree
   :htmlp :destructure-html :defhtmltag
   ;; pattern-match
   :define-patterns :rewrite-tree
   ;; server
   :server-base-url :export-server :server
   ;; request
   :request :request-url :request-user :request-original-url :request-method
   #+nil :request-socket			;not actually used anywhere
   :request-unhandled-part
   :request-stream :request-session :request-headers
   :request-body :request-unparsed-body
   :request-base-url :request-path-info :request-header
   :request-condition :request-cookie
   :cookie-not-found
   :request-safe-cookie :request-cookies :request-safe-cookies :cookie-string
   :request-send-headers :request-send-error :request-redirect   
   :body-param :body-params :request-if-modified-since
   :dispatch-request
   :request-handled-by
   ;; url
   :url :url-scheme :copy-url :internet-url :url-username :url-password
   :url-endpoint :url-host :url-port :url-path :url-query :url-fragment
   :urlstring :urlstring-unescape
   :http-url :https-url :httplike-url :urlstring-escape
   :link
   :using-untainted-values :*warn-when-using-untainted-values*
   :url-query-alist :tainted-url-query-alist
   :url-query-param :tainted-url-query-param
   :url-query-string-from-alist
   :with-url-params :with-tainted-url-params
   :parse-urlstring :make-url :merge-url :append-url
   ;; utilities
   :split				;give in to the dark side, luke
   ;; for CLISP
   :host-serve-events
   ;; pass along taint, untaint, and tainted-p
   :taint :untaint :tainted-p
   ;; convenience functions
   :attach-hierarchy
   ;; urlmethods and conditions
   :defurlmethod :deftaintedurlmethod
   :urlmethod-error :urlmethod-compile-error
   :no-urlmethod :too-many-urlmethods-matched
   :urlmethod-unknown-keyword :urlmethod-function-parameter-mismatch
   ;; template utility functions
   :deftemplate :call-template
   :trace-template :untrace-template
   ;; Compatibility with TBNL, etc
   :*araneida-mode*
   )
  (:use "SPLIT-SEQUENCE"
	"CL-TAINT"
        #+sbcl "SB-MOP"
        #+openmcl "OPENMCL-MOP"
        #+allegro "ACL-MOP"
        "COMMON-LISP" )) ;; PAR: removed "SB-BSD-SOCKETS"