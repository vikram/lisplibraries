;********************************************************
; file:        curl.lisp                                 
; description: CL interface to libcurl library.          
; date:        Tue Jan 18 2005 - 22:27                   
; author:      Liam M. Healy <cl@healy.washington.dc.us>
; modified:    Wed Feb 23 2005 - 12:21
;********************************************************

(in-package :curl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :uffi)
  (export '(set-option perform return-string finish get-information
	    initialize-for-returning-string with-connection-returning-string
	    set-send-string)))

;;; See /usr/include/curl/*.h for interface.

;;;;****************************************************************************
;;;; Curl functions
;;;;****************************************************************************

;;; Initialize for connection(s) returning strings.
(def-function ("curl_init_write_string" initialize-for-returning-string)
    ()
  :returning (* :char))

(def-function ("curl_perform" perform-int)
    ((connection (* :char)))
  :returning :int)

(def-function ("curl_return_string" return-string-int)
    ((connection (* :char)))
  :returning :cstring)

;;; Delete the memory allocated for the string; only need be done at the end.
(def-function ("curl_free_string" delete-string)
    ((connection (* :char)))
  :returning :void)

;;; Close the Curl session
(def-function ("curl_finish" finish)
    ((connection (* :char)))
  :returning :void)

;;;;****************************************************************************
;;;; Higher level functions
;;;;****************************************************************************

(defun perform (connection)
  "Perform the CURL action."
  (return-error-check (perform-int connection)))

(defun return-string (connection)
  "The string returned by the last CURL transaction."
  (copy-seq (convert-from-cstring (return-string-int connection))))

;;;;****************************************************************************
;;;; Convenience macro
;;;;****************************************************************************

(defmacro with-connection-returning-string
    ((&key cookies reassure) &body body)
  "Establish a network connection, and return the final string.
   If reassure is a non-nil, print out information at the start
   and end of the connection."
  (let ((sym (gensym)))
    `(let ((,sym (initialize-for-returning-string)))
       (flet ((set-option (option value) (set-option ,sym option value))
	      (perform () (perform ,sym))
	      (return-string () (return-string ,sym))
	      (set-send-string (string) (set-send-string ,sym string)))
	 #-allegro
	 (declare (ignorable (function set-option)
			     (function perform)
			     (function return-string)
			     (function set-send-string)))
	 ,(when reassure
	    `(format *terminal-io* "Connecting to ~a ~a..."
		     ,(third (find :url body :key #'second :test #'string-equal))
		     ,reassure)
	    '(force-output *terminal-io*))
	 ,(when cookies
	    (if (stringp cookies)
		`(set-option :cookiefile ,cookies)
		'(set-option :cookiefile "nonsense.cookies")))
	 ,@body
	 (prog1
	     (copy-seq (return-string))
	   (delete-string ,sym)
	   (finish ,sym)
	   (when ,reassure
	     (format *terminal-io* " done")
	     (force-output *terminal-io*)))))))

;;;;****************************************************************************
;;;; Return status
;;;;****************************************************************************

(defparameter *error-codes*
  '(ok
    unsupported-protocol		; 1
    failed-init				; 2
    url-malformat			; 3
    url-malformat-user			; 4
    couldnt-resolve-proxy		; 5
    couldnt-resolve-host		; 6
    couldnt-connect			; 7
    ftp-weird-server-reply		; 8
    ftp-access-denied			; 9
    ftp-user-password-incorrect		; 10
    ftp-weird-pass-reply		; 11
    ftp-weird-user-reply		; 12
    ftp-weird-pasv-reply		; 13
    ftp-weird-227-format		; 14
    ftp-cant-get-host			; 15
    ftp-cant-reconnect			; 16
    ftp-couldnt-set-binary		; 17
    partial-file			; 18
    ftp-couldnt-retr-file		; 19
    ftp-write-error			; 20
    ftp-quote-error			; 21
    http-returned-error			; 22
    write-error				; 23
    malformat-user			; 24 - user name is illegally specified
    ftp-couldnt-stor-file		; 25 - failed FTP upload 
    read-error				; 26 - could open/read from file
    out-of-memory			; 27
    operation-timeouted			; 28 - the timeout time was reached
    ftp-couldnt-set-ascii		; 29 - TYPE A failed
    ftp-port-failed			; 30 - FTP PORT operation failed
    ftp-couldnt-use-rest		; 31 - the REST command failed
    ftp-couldnt-get-size		; 32 - the SIZE command failed
    http-range-error			; 33 - RANGE "command" didn't work
    http-post-error			; 34
    ssl-connect-error			; 35 - wrong when connecting with SSL
    bad-download-resume			; 36 - couldn't resume download
    file-couldnt-read-file		; 37
    ldap-cannot-bind			; 38
    ldap-search-failed			; 39
    library-not-found			; 40
    function-not-found			; 41
    aborted-by-callback			; 42
    bad-function-argument		; 43
    bad-calling-order			; 44
    http-port-failed			; 45 - HTTP interface operation failed
    bad-password-entered		; 46 - my-getpass() returns fail
    too-many-redirects			; 47 - catch endless re-direct loops
    unknown-telnet-option		; 48 - user specified an unknown option
    telnet-option-syntax		; 49 - Malformed telnet option
    obsolete				; 50 - removed after 7.7.3
    ssl-peer-certificate		; 51 - peer's certificate wasn't ok
    got-nothing				; 52 - when this is a specific error
    ssl-engine-notfound			; 53 - SSL crypto engine not found
    ssl-engine-setfailed		; 54 - can not set SSL crypto engine as default
    send-error				; 55 - failed sending network data
    recv-error				; 56 - failure in receiving network data
    share-in-use			; 57 - share is in use
    ssl-certproblem			; 58 - problem with the local certificate
    ssl-cipher				; 59 - couldn't use specified cipher
    ssl-cacert				; 60 - problem with the CA cert (path?)
    bad-content-encoding		; 61 - Unrecognized transfer encoding
    ldap-invalid-url			; 62 - Invalid LDAP URL
    filesize-exceeded			; 63 - Maximum file size exceeded
    ftp-ssl-failed			; 64 - Requested FTP SSL level failed
    last)
  "Error codes returned from CURL functions." )

(defun return-error-check (result)
  (if (zerop result)
      t					; sucess
      (if (plusp result)
	  (error "CURL error ~d (~a)" result (nth result *error-codes*))
	  (error "No CURL handle."))))

;;;;****************************************************************************
;;;; Set curl options
;;;;****************************************************************************

(defconstant +long-opt+ 0)
(defconstant +objectpoint-opt+ 10000)
(defconstant +functionpoint-opt+ 20000)
(defconstant +offt-opt+ 30000)

(defparameter *option-codes*
  `((FILE . ,(+ +objectpoint-opt+ 1))
    (WRITEDATA . ,(+ +objectpoint-opt+ 1))
    (URL . ,(+ +objectpoint-opt+ 2))
    (PORT . ,(+ +long-opt+ 3))
    (PROXY . ,(+ +objectpoint-opt+ 4))
    (PROXYUSERPWD . ,(+ +objectpoint-opt+ 6))
    (RANGE . ,(+ +objectpoint-opt+ 7))
    (INFILE . ,(+ +objectpoint-opt+ 9))
    (READDATA . ,(+ +objectpoint-opt+ 9))
    (ERRORBUFFER . ,(+ +objectpoint-opt+ 10))
    (WRITEFUNCTION . ,(+ +functionpoint-opt+ 11))
    (READFUNCTION . ,(+ +functionpoint-opt+ 12))
    (TIMEOUT . ,(+ +long-opt+ 13))
    (INFILESIZE . ,(+ +long-opt+ 14))
    (POSTFIELDS . ,(+ +objectpoint-opt+ 15))
    (REFERER . ,(+ +objectpoint-opt+ 16))
    (FTPPORT . ,(+ +objectpoint-opt+ 17))
    (USERAGENT . ,(+ +objectpoint-opt+ 18))
    (LOW-SPEED-LIMIT . ,(+ +long-opt+ 19))
    (LOW-SPEED-TIME . ,(+ +long-opt+ 20))
    (RESUME-FROM . ,(+ +long-opt+ 21))
    (COOKIE . ,(+ +objectpoint-opt+ 22))
    (HTTPHEADER . ,(+ +objectpoint-opt+ 23))
    (HTTPPOST . ,(+ +objectpoint-opt+ 24))
    (SSLCERT . ,(+ +objectpoint-opt+ 25))
    (SSLCERTPASSWD . ,(+ +objectpoint-opt+ 26))
    (SSLKEYPASSWD . ,(+ +objectpoint-opt+ 26))
    (CRLF . ,(+ +long-opt+ 27))
    (QUOTE . ,(+ +objectpoint-opt+ 28))
    (WRITEHEADER . ,(+ +objectpoint-opt+ 29))
    (HEADERDATA . ,(+ +objectpoint-opt+ 29))
    (COOKIEFILE . ,(+ +objectpoint-opt+ 31))
    (SSLVERSION . ,(+ +long-opt+ 32))
    (TIMECONDITION . ,(+ +long-opt+ 33))
    (TIMEVALUE . ,(+ +long-opt+ 34))
    (CUSTOMREQUEST . ,(+ +objectpoint-opt+ 36))
    (STDERR . ,(+ +objectpoint-opt+ 37))
    (POSTQUOTE . ,(+ +objectpoint-opt+ 39))
    (WRITEINFO . ,(+ +objectpoint-opt+ 40))
    (VERBOSE . ,(+ +long-opt+ 41))
    (HEADER . ,(+ +long-opt+ 42))
    (NOPROGRESS . ,(+ +long-opt+ 43))
    (NOBODY . ,(+ +long-opt+ 44))
    (FAILONERROR . ,(+ +long-opt+ 45))
    (UPLOAD . ,(+ +long-opt+ 46))
    (POST . ,(+ +long-opt+ 47))
    (FTPLISTONLY . ,(+ +long-opt+ 48))
    (FTPAPPEND . ,(+ +long-opt+ 50))
    (NETRC . ,(+ +long-opt+ 51))
    (FOLLOWLOCATION . ,(+ +long-opt+ 52))
    (TRANSFERTEXT . ,(+ +long-opt+ 53))
    (PUT . ,(+ +long-opt+ 54))
    (PROGRESSFUNCTION . ,(+ +functionpoint-opt+ 56))
    (PROGRESSDATA . ,(+ +objectpoint-opt+ 57))
    (AUTOREFERER . ,(+ +long-opt+ 58))
    (PROXYPORT . ,(+ +long-opt+ 59))
    (POSTFIELDSIZE . ,(+ +long-opt+ 60))
    (HTTPPROXYTUNNEL . ,(+ +long-opt+ 61))
    (INTERFACE . ,(+ +objectpoint-opt+ 62))
    (KRB4LEVEL . ,(+ +objectpoint-opt+ 63))
    (SSL-VERIFYPEER . ,(+ +long-opt+ 64))
    (CAINFO . ,(+ +objectpoint-opt+ 65))
    (MAXREDIRS . ,(+ +long-opt+ 68))
    (FILETIME . ,(+ +objectpoint-opt+ 69))
    (TELNETOPTIONS . ,(+ +objectpoint-opt+ 70))
    (MAXCONNECTS . ,(+ +long-opt+ 71))
    (CLOSEPOLICY . ,(+ +long-opt+ 72))
    (FRESH-CONNECT . ,(+ +long-opt+ 74))
    (FORBID-REUSE . ,(+ +long-opt+ 75))
    (RANDOM-FILE . ,(+ +objectpoint-opt+ 76))
    (EGDSOCKET . ,(+ +objectpoint-opt+ 77))
    (CONNECTTIMEOUT . ,(+ +long-opt+ 78))
    (HEADERFUNCTION . ,(+ +functionpoint-opt+ 79))
    (HTTPGET . ,(+ +long-opt+ 80))
    (SSL-VERIFYHOST . ,(+ +long-opt+ 81))
    (COOKIEJAR . ,(+ +objectpoint-opt+ 82))
    (SSL-CIPHER-LIST . ,(+ +objectpoint-opt+ 83))
    (HTTP-VERSION . ,(+ +long-opt+ 84))
    (FTP-USE-EPSV . ,(+ +long-opt+ 85))
    (SSLCERTTYPE . ,(+ +objectpoint-opt+ 86))
    (SSLKEY . ,(+ +objectpoint-opt+ 87))
    (SSLKEYTYPE . ,(+ +objectpoint-opt+ 88))
    (SSLENGINE . ,(+ +objectpoint-opt+ 89))
    (SSLENGINE-DEFAULT . ,(+ +long-opt+ 90))
    (DNS-USE-GLOBAL-CACHE . ,(+ +long-opt+ 91))
    (DNS-CACHE-TIMEOUT . ,(+ +long-opt+ 92))
    (PREQUOTE . ,(+ +objectpoint-opt+ 93))
    (DEBUGFUNCTION . ,(+ +functionpoint-opt+ 94))
    (DEBUGDATA . ,(+ +objectpoint-opt+ 95))
    (COOKIESESSION . ,(+ +long-opt+ 96))
    (CAPATH . ,(+ +objectpoint-opt+ 97))
    (BUFFERSIZE . ,(+ +long-opt+ 98))
    (NOSIGNAL . ,(+ +long-opt+ 99))
    (SHARE . ,(+ +objectpoint-opt+ 100))
    (PROXYTYPE . ,(+ +long-opt+ 101))
    (ENCODING . ,(+ +objectpoint-opt+ 102))
    (PRIVATE . ,(+ +objectpoint-opt+ 103))
    (HTTP200ALIASES . ,(+ +objectpoint-opt+ 104))
    (UNRESTRICTED-AUTH . ,(+ +long-opt+ 105))
    (FTP-USE-EPRT . ,(+ +long-opt+ 106))
    (HTTPAUTH . ,(+ +long-opt+ 107))
    (SSL-CTX-FUNCTION . ,(+ +functionpoint-opt+ 108))
    (SSL-CTX-DATA . ,(+ +objectpoint-opt+ 109))
    (FTP-CREATE-MISSING-DIRS . ,(+ +long-opt+ 110))
    (PROXYAUTH . ,(+ +long-opt+ 111))
    (FTP-RESPONSE-TIMEOUT . ,(+ +long-opt+ 112))
    (IPRESOLVE . ,(+ +long-opt+ 113))
    (MAXFILESIZE . ,(+ +long-opt+ 114))
    (INFILESIZE-LARGE . ,(+ +offt-opt+ 115))
    (RESUME-FROM-LARGE . ,(+ +offt-opt+ 116))
    (MAXFILESIZE-LARGE . ,(+ +offt-opt+ 117))
    (NETRC-FILE . ,(+ +objectpoint-opt+ 118))
    (FTP-SSL . ,(+ +long-opt+ 119))
    (POSTFIELDSIZE-LARGE . ,(+ +offt-opt+ 120))
    (TCP-NODELAY . ,(+ +long-opt+ 121))))

(defun option-lookup (symbol)
  "Find the numeric code for the CURL option."
  (rest
   (assoc
    symbol
    *option-codes* :test #'string-equal)))

;;; Not tested
(def-function ("curl_set_option_long" set-option-long)
    ((connection (* :char))
     (option :int)
     (value :long))
  :returning :int)

(def-function ("curl_set_option_string" set-option-string)
    ((connection (* :char))
     (option :int)
     (value :cstring))
  :returning :int)

(defun set-option (connection option value)
  "Set the CURL option."
  (let ((optnum (option-lookup option)))
    (unless optnum (error "Failed to find option ~a" option))
    (cond ((< +long-opt+ optnum +objectpoint-opt+) ; long
	   (let ((val
		  (if (integerp value)
		      value
		      ;; Allow passing T for 1 and NIL for 0,
		      ;; following the boolean convention of C
		      (if value 1 0))))
	     (return-error-check
	      (set-option-long connection optnum val))))
	  ((< +objectpoint-opt+ optnum +functionpoint-opt+) ; object
	   ;; Most "object"s are strings; they ones that aren't
	   ;; we can't handle.
	   (return-error-check
	    (set-option-string
	     connection optnum
	     (convert-to-cstring
	      (etypecase value
		(string value)
		(symbol (string value))
		(pathname (namestring value)))))))
	  ((< +functionpoint-opt+ optnum +offt-opt+)
	   (error "Can't handle callbacks yet."))
	  ((< +offt-opt+ optnum)
	   (error "Can't handle offt yet.")))))

;;;;****************************************************************************
;;;; Get information
;;;;****************************************************************************

(defconstant +string-info+ #x100000)
(defconstant +long-info+   #x200000)
(defconstant +double-info+ #x300000)

(defparameter *information-codes*
  `((EFFECTIVE-URL    . ,(+ +string-info+ 1))
    (RESPONSE-CODE    . ,(+ +long-info+ 2))
    (TOTAL-TIME       . ,(+ +double-info+ 3))
    (NAMELOOKUP-TIME  . ,(+ +double-info+ 4))
    (CONNECT-TIME     . ,(+ +double-info+ 5))
    (PRETRANSFER-TIME . ,(+ +double-info+ 6))
    (SIZE-UPLOAD      . ,(+ +double-info+ 7))
    (SIZE-DOWNLOAD    . ,(+ +double-info+ 8))
    (SPEED-DOWNLOAD   . ,(+ +double-info+ 9))
    (SPEED-UPLOAD     . ,(+ +double-info+ 10))
    (HEADER-SIZE      . ,(+ +long-info+ 11))
    (REQUEST-SIZE     . ,(+ +long-info+ 12))
    (SSL-VERIFYRESULT . ,(+ +long-info+ 13))
    (FILETIME         . ,(+ +long-info+ 14))
    (CONTENT-LENGTH-DOWNLOAD . ,(+ +double-info+ 15))
    (CONTENT-LENGTH-UPLOAD . ,(+ +double-info+ 16))
    (STARTTRANSFER-TIME . ,(+ +double-info+ 17))
    (CONTENT-TYPE     . ,(+ +string-info+ 18))
    (REDIRECT-TIME    . ,(+ +double-info+ 19))
    (REDIRECT-COUNT   . ,(+ +long-info+ 20))
    (PRIVATE          . ,(+ +string-info+ 21))
    (HTTP-CONNECTCODE . ,(+ +long-info+ 22))
    (HTTPAUTH-AVAIL   . ,(+ +long-info+ 23))
    (PROXYAUTH-AVAIL  . ,(+ +long-info+ 24))))

(def-function ("curl_get_information_string" get-information-string)
    ((connection (* :char))
     (info :int)
     (value (* :cstring)))
  :returning :int)

(def-function ("curl_get_information_long" get-information-long)
    ((connection (* :char))
     (info :int)
     (value (* :long)))
  :returning :int)

(def-function ("curl_get_information_double" get-information-double)
    ((connection (* :char))
     (info :int)
     (value (* :double)))
  :returning :int)

(defun get-information (connection info)
  "Get the requested information after the transfer."
  (let ((code (rest (assoc info *information-codes* :test #'string-equal))))
    (unless code (error "Unrecognized information request."))
    (cond
      ((< +string-info+ code +long-info+)
       (with-foreign-object (ans :cstring)
	 (return-error-check (get-information-string connection code ans))
	 (deref-pointer ans :cstring)))
      ((< +long-info+ code +double-info+)
       (with-foreign-object (ans :long)
	 (return-error-check (get-information-long connection code ans))
	 (deref-pointer ans :long)))
      ((< +double-info+ code)
       (with-foreign-object (ans :double)
	 (return-error-check (get-information-double connection code ans))
	 (deref-pointer ans :double))))))

;;;;****************************************************************************
;;;; Send string
;;;;****************************************************************************

(def-function ("curl_set_read_string" set-read-string-int)
    ((connection (* :char))
     (value :cstring))
  :returning :int)

(defun set-send-string (connection string)
  "Set a string to be sent."
  ;; What if it's not a post?
  (set-option connection :postfieldsize (length string))
  (set-read-string-int connection (convert-to-cstring string)))

;;;;****************************************************************************
;;;; Subcode enumerations
;;;;****************************************************************************

;;; Drawbacks of using def-enum:
;;; symbols used need to be in this package, can't use keywords
;;; and therefore need to be exported.  They are difficult to
;;; use as return codes because lookup is not easy.

(defparameter *suboptions*
  ;; curl_infotype CURLINFO_*
  '((infotype-text . 0)
    (infotype-header-in . 1)
    (infotype-header-out . 2)
    (infotype-data-in . 3)
    (infotype-data-out . 4)
    (infotype-end . 5)
    ;; curl_proxytype
    (proxytpe-http . 0)
    (proxytpe-socks4 . 4)
    (proxytpe-socks5 . 5)
    ;; curl_ftpssl
    (ftpssl-none . 1)
    (ftpssl-try . 2)
    (ftpssl-control . 3)
    (ftpssl-all . 4)
    (ftpssl-last . 5)
    (http-version-none . 1)
    (http-version-1-0 . 2)
    (http-version-1-1 . 3)
    (http-version-last . 4)
    (netrc-option-ignored . 1)
    (netrc-option-optional . 2)
    (netrc-option-reuired . 3)
    (netrc-option-last . 4)
    (sslversion-default . 1)
    (sslversion-tlsv1 . 2)
    (sslversion-tlsv2 . 3)
    (sslversion-tlsv3 . 4)
    (timecond-none . 1)
    (timecond-ifmodsince . 2)
    (timecond-ifunmodsince . 3)
    (timecond-lastmod . 4)
    (timecond-last . 5)
    (formoption-nothing . 1)
    (formoption-copyname . 2)
    (formoption-ptrname . 3)
    (formoption-namelength . 4)
    (formoption-copycontents . 5)
    (formoption-ptrcontents . 6)
    (formoption-contentslength . 7)
    (formoption-filecontent . 8)
    (formoption-array . 9)
    (formoption-obsolete . 10)
    (formoption-file . 11)
    (formoption-buffer . 12)
    (formoption-bufferptr . 13)
    (formoption-bufferlength . 14)
    (formoption-contenttype . 15)
    (formoption-contentheader . 16)
    (formoption-filename . 17)
    (formoption-end . 18)
    (formoption-obsolete2 . 19)
    (closepolicy-none . 1)
    (closepolicy-oldest . 2)
    (closepolicy-least-recently-used . 3)
    (closepolicy-least-traffic . 4)
    (closepolicy-slowest . 5)
    (closepolicy-callback . 6)
    (closepolicy-last . 7)
    (lock-data-none . 1)
    (lock-data-share . 2)
    (lock-data-cookie . 3)
    (lock-data-dns . 4)
    (lock-data-ssl-session . 5)
    (lock-data-connect . 6)
    (lock-data-last . 7)
    ;; curl_lock_access
    (lock-access-none . 1)
    (lock-access-shared . 2)
    (lock-access-single . 3)
    (lock-access-last . 4)
    ;; CURLSHoption
    (shoption-none . 1)
    (shoption-share . 2)
    (shoption-unshare . 3)
    (shoption-lockfunc . 4)
    (shoption-unlockfunc . 5)
    (shoption-userdata . 6)
    (shoption-last . 7)))

;;; CURLSHcode is an output code?

;;; CURLversion
(defconstant +curl-version+ 2)
