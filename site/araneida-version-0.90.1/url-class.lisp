(in-package :araneida)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic URL class

(defclass url ()
  ((string :initarg :string :accessor url-string)         ; original string
   (unparsed :initarg :unparsed :accessor url-unparsed)   ; that bit of it we haven't sussed yet
   (scheme :initform nil :initarg :scheme :accessor url-scheme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mailto urls don't follow RFC 1738

(defclass mailto-url (url)
  ((username :initarg :username :initform nil :accessor url-username)
   (host     :initarg :host     :initform nil :accessor url-host)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; URL class for "Common Internet Scheme Syntax" (RFC 1738)

;;;    While the syntax for the rest of the URL may vary depending on the
;;;    particular scheme selected, URL schemes that involve the direct use
;;;    of an IP-based protocol to a specified host on the Internet use a
;;;    common syntax for the scheme-specific data:

;;;         //<user>:<password>@<host>:<port>/<url-path>

;;; RFC1738 says that <url-path> should not include the leading "/"
;;; I think that's nuts and is going to make it impossible to deal
;;; sanely with relative URLs

;;; Fortunately, 2396 agrees with me.

(defclass internet-url (url)
  ((username :initarg :username :initform nil :accessor url-username)
   (password :initarg :password :initform nil :accessor url-password)
   (host     :initarg :host     :initform nil :accessor url-host)
   (port     :initarg :port     :initform nil :accessor url-port)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP url according to RFC 1738 doesn't allow username/password.
;;; Perverse or what?  we ignore that because it's easier to allow them
;;; and probably more useful too

(defclass httplike-url (internet-url)
  ((path     :initarg :path    :initform "/" :accessor url-path)
   (query    :initarg :query   :initform nil :accessor url-query)
   (fragment :initarg :fragment :initform nil :accessor url-fragment)))

(defclass http-url (httplike-url)
  ((scheme :initform "http")
   (port :initform 80)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTPS

(defclass https-url (httplike-url)
  ((scheme :initform "https")
   (port :initform 443)))

;;; FTP doesn't really have query and fragment.  Never mind

(defclass ftp-url (httplike-url)
  ((scheme :initform "ftp")
   (port :initform 21)))
