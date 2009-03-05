;; -*- mode: common-lisp; package: net.aserve.client -*-
;;
;; client.cl
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2004 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;
;; $Id: client.cl,v 1.51 2006/06/08 00:42:43 jkf Exp $

;; Description:
;;   http client code.

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


;; this will evolve into the http client code but for now it's
;; just some simple stuff to allow us to test aserve
;;







(in-package :net.aserve.client)


#-allegro
(defun md5-init ()
  (md5:make-md5-state))
#-allegro
(defun md5-update (state sequence &key (start 0) (end (length sequence)))
  (md5:update-md5-state state sequence :start start :end end))
#-allegro
(defun md5-final (state &key return)
  (declare (ignore return))
  (format nil
          "~{~(~2,'0X~)~}"
          (coerce (md5:finalize-md5-state state) 'list)))



(defclass client-request ()
  ((uri   	;; uri we're accessing
    :initarg :uri
    :accessor client-request-uri)

   (method	; :get, :put, etc
    :initarg :method
    :accessor client-request-method)
   
   (headers ; alist of  ("headername" . "value") or (:headername . "value")
    :initform nil
    :initarg :headers
    :accessor client-request-headers)
   (response-code    ; response code (an integer)
    :initform nil
    :accessor client-request-response-code)
   (socket  ; the socket through which we'll talk to the server
    :initarg :socket
    :accessor client-request-socket)
   (protocol 
    ; the protocol value returned by the web server
    ; note, even if the request is for http/1.0, apache will return
    ; http/1.1.  I'm not sure this is kosher.
    :accessor client-request-protocol)
   (response-comment  ;; comment passed back with the response
    :accessor client-request-response-comment)
   ;
   (bytes-left  ;; indicates how many bytes in response left
    ; value is nil (no body)
    ;          integer (that many bytes left, not chunking)
    ;		:unknown - read until eof, not chunking
    ;		:chunking - read until chunking eof
    :accessor client-request-bytes-left
    :initform nil)
   
   (cookies  ;; optionally a cookie jar for hold received and sent cookies
    :accessor client-request-cookies
    :initarg :cookies
    :initform nil)
   ))


(defclass digest-authorization ()
  ((username :initarg :username
	     :initform ""
	     :reader digest-username)
   
   (password :initarg :password
	     :initform ""
	     :reader digest-password)
   
   (realm    :initform ""
	     :accessor digest-realm)
   
   (uri       :initform nil
	      :accessor digest-uri)
   
   (qop	     :initform nil
	     :accessor digest-qop)
   
   (nonce    :initform ""
	     :accessor digest-nonce)
   
   ; we sent unique cnonce each time
   (nonce-count :initform "1"
		:reader digest-nonce-count)

   (cnonce   :initform nil
	     :accessor digest-cnonce)
   
   (opaque   :initform nil
	     :accessor digest-opaque)
   
   (response :initform nil
	     :accessor digest-response)
   
   ))


(defvar crlf (make-array 2 :element-type 'character
			 :initial-contents '(#\return #\linefeed)))

(defmacro with-better-scan-macros (&body body)
  ;; define the macros for scanning characters in a string
  `(macrolet ((collect-to (ch buffer i max &optional downcasep eol-ok)
		;; return a string containing up to the given char
		`(let ((start ,i))
		   (loop
		     (if* (>= ,i ,max)
			then ,(if* eol-ok
				 then `(return (buf-substr start ,i ,buffer ,downcasep))
				 else `(fail)))
		     (if* (eql ,ch (schar ,buffer ,i)) 
			then (return (buf-substr start ,i ,buffer ,downcasep)))
		     (incf ,i)
		     )))
	      
	      (collect-to-eol (buffer i max)
		;; return a string containing up to the given char
		`(let ((start ,i))
		   (loop
		     (if* (>= ,i ,max) 
			then (return (buf-substr start ,i ,buffer)))
		     (let ((thisch (schar ,buffer ,i)))
		       (if* (eql thisch #\return)
			  then (let ((ans (buf-substr start ,i ,buffer)))
				 (incf ,i)  ; skip to linefeed
				 (return ans))
			elseif (eql thisch #\linefeed)
			  then (return (buf-substr start ,i ,buffer))))
		     (incf ,i)
		     )))
	      
	      (skip-to-not (ch buffer i max &optional (errorp t))
		;; skip to first char not ch
		`(loop
		   (if* (>= ,i ,max) 
		      then ,(if* errorp 
			       then `(fail)
			       else `(return)))
		   (if* (not (eql ,ch (schar ,buffer ,i)))
		      then (return))
		   (incf ,i)))
	      
	      (buf-substr (from to buffer &optional downcasep)
		;; create a string containing [from to }
		;;
		`(let ((res (make-string (- ,to ,from))))
		   (do ((ii ,from (1+ ii))
			(ind 0 (1+ ind)))
		       ((>= ii ,to))
		     (setf (schar res ind)
		       ,(if* downcasep
			   then `(char-downcase (schar ,buffer ii))
			   else `(schar ,buffer ii))))
		   res)))
     
     ,@body))


(defun do-http-request (uri 
			&rest args
			&key 
			(method  :get)
			(protocol  :http/1.1)
			(accept "*/*")
			content
			content-type
			query
			(format :text) ; or :binary
			cookies ; nil or a cookie-jar
			(redirect 5) ; auto redirect if needed
			(redirect-methods '(:get :head))
			basic-authorization  ; (name . password)
			digest-authorization ; digest-authorization object
			keep-alive   ; if true, set con to keep alive
			headers	    ; extra header lines, alist
			proxy	    ; naming proxy server to access through
			proxy-basic-authorization  ; (name . password)
			user-agent
			(external-format *default-aserve-external-format*)
			ssl		; do an ssl connection
			skip-body ; fcn of request object
			timeout  
			
			;; internal
			recursing-call ; true if we are calling ourself
			)
  
  ;; send an http request and return the result as four values:
  ;; the body, the response code, the headers and the uri 
  (let ((creq (make-http-client-request 
	       uri  
	       :method method
	       :protocol protocol
	       :accept  accept
	       :content content
	       :content-type content-type
	       :query query
	       :cookies cookies
	       :basic-authorization basic-authorization
	       :digest-authorization digest-authorization
	       :keep-alive keep-alive
	       :headers headers
	       :proxy proxy
	       :proxy-basic-authorization proxy-basic-authorization
	       :user-agent user-agent
	       :external-format external-format
	       :ssl ssl
	       :timeout timeout
	       )))

    (unwind-protect
	(let (new-location) 
	  
	  (loop
	    (read-client-response-headers creq)
	    ;; if it's a continue, then start the read again
	    (if* (not (eql 100 (client-request-response-code creq)))
	       then (return)))
	  
	  (if* (and (member (client-request-response-code creq)
			    '(#.(net.aserve::response-number *response-found*)
			      #.(net.aserve::response-number *response-moved-permanently*)
			      #.(net.aserve::response-number *response-temporary-redirect*)
			      #.(net.aserve::response-number *response-see-other*))
			    :test #'eq)
		    redirect
		    (member method redirect-methods :test #'eq)
		    (if* (integerp redirect)
		       then (> redirect 0)
		       else t))		; unrestricted depth
	     then
		  (setq new-location
		    (cdr (assoc :location (client-request-headers creq)
				:test #'eq))))
	
	  (if* (and digest-authorization
		    (equal (client-request-response-code creq)
			   #.(net.aserve::response-number 
			      *response-unauthorized*))
		    (not recursing-call))
	     then ; compute digest info and retry
		  (if* (compute-digest-authorization 
			creq digest-authorization)
		     then (client-request-close creq)
			  (return-from do-http-request
			    (apply #'do-http-request
				   uri
				   :recursing-call t
				   args))))
		  
		  
	  
	  (if* (and (null new-location) 
		    ; not called when redirecting
		    (if* (functionp skip-body)
		       then (funcall skip-body creq)
		       else skip-body))
	     then
		  (return-from do-http-request
		    (values 
		     nil		; no body
		     (client-request-response-code creq)
		     (client-request-headers  creq)
		     (client-request-uri creq))))
	  
	  ;; read the body of the response
	  (let ( #+ignore (atype (if* (eq format :text) ; JSC: We do not need to set this all over and over
			  then 'character
			  else '(unsigned-byte 8)))
		ans
		res
		(start 0)
		(end nil)
		body)
	    
	    (loop
	      (if* (null ans)
		 then (setq ans (make-array 1024 :element-type '(unsigned-byte 8)) ; was atype)
			    start 0))
		
	      (setq end (client-request-read-sequence ans creq :start start))
	      (if* (zerop end)
		 then			; eof
		      (return))
	      (if* (eql end 1024)
		 then			; filled up
		      (push ans res)
		      (setq ans nil)
		 else (setq start end)))
	    
	    ;; we're out with res containing full arrays and 
	    ;; ans either nil or holding partial data up to but not including
	    ;; index start
	    
	    (if* res
	       then			; multiple items
		    (let* ((total-size (+ (* 1024 (length res)) start))
			   (bigarr (make-array total-size :element-type '(unsigned-byte 8)) ; was atype)
                             ))
		      (let ((sstart 0))
			(dolist (arr (reverse res))
			  (replace bigarr arr :start1 sstart)
			  (incf sstart (length arr)))
			(if* ans 
			   then		; final one 
				(replace bigarr ans :start1 sstart)))
		      
		      (setq body bigarr))
	       else			; only one item
		    (if* (eql 0 start)
		       then		; nothing returned
			    (setq body "")
		       else (setq body (subseq ans 0 start))))
	    
	    (if* new-location
	       then			; must do a redirect to get to the real site
		    (client-request-close creq)
		    (apply #'do-http-request
			   (puri:merge-uris new-location uri)
			   :redirect
			   (if* (integerp redirect)
			      then (1- redirect)
			      else redirect)
			   args)
	       else
		    (values 
		     (if (eq format :text)
                         (let ((result (make-string (length body))))
                           (map-into result #'code-char body)
                           result)
                         body)
		     (client-request-response-code creq)
		     (client-request-headers  creq)
		     (client-request-uri creq)))))
      
      ;; protected form:
      (client-request-close creq))))


    
		
		
		
		
		
		
		      



(defun make-http-client-request (uri &key 
				     (method  :get)  ; :get, :post, ....
				     (protocol  :http/1.1)
				     keep-alive 
				     (accept "*/*") 
				     cookies  ; nil or a cookie-jar
				     basic-authorization
				     digest-authorization
				     content
				     content-length 
				     content-type
				     query
				     headers
				     proxy
				     proxy-basic-authorization
				     user-agent
				     (external-format 
				      *default-aserve-external-format*)
				     ssl
				     timeout
				     )
  

  (declare (ignorable timeout))
  
  (let (host sock port fresh-uri scheme-default-port)
    ;; start a request 
  
    ; parse the uri we're accessing
    (if* (not (typep uri 'puri:uri))
       then (setq uri (puri:parse-uri uri)
		  fresh-uri t))
    
    ; make sure it's an http uri
    (case (or (puri:uri-scheme uri) :http)
      (:http nil)
      (:https (setq ssl t))
      (t (error "Can only do client access of http or https uri's, not ~s" uri)))
  
    ; make sure that there's a host
    (if* (null (setq host (puri:uri-host uri)))
       then (error "need a host in the client request: ~s" uri))

    (setq scheme-default-port
      (case (or (puri:uri-scheme uri) (if* ssl 
					    then :https
					    else :http))
	(:http 80)
	(:https 443)))
    
    ; default the port to what's appropriate for http or https
    (setq port (or (puri:uri-port uri) scheme-default-port))
    
    (if* proxy
       then ; sent request through a proxy server
	    (assert (stringp proxy) (proxy) 
	      "proxy value ~s should be a string" proxy)
	    (multiple-value-bind (phost pport)
		(net.aserve::get-host-port proxy)
	      (if* (null phost)
		 then (error "proxy arg should have form \"foo.com\" ~
or \"foo.com:8000\", not ~s" proxy))
	      
	      (setq sock (acl-compat.socket:make-socket :remote-host phost
					     :remote-port pport
					     :format :bivalent
					     :type net.aserve::*socket-stream-type*
					     :nodelay t
					     )))
       else (setq sock 
	      (acl-compat.socket:make-socket :remote-host host
				  :remote-port port
				  :format :bivalent
				  :type 
				  net.aserve::*socket-stream-type*
				  :nodelay t
					     
				  ))
	    (if* ssl
	       then (setq sock
		      (funcall 'acl-compat.socket::make-ssl-client-stream sock)))
	    )

    #+(and allegro (version>= 6 0))
    (let ((ef (find-external-format external-format)))
      #+(and allegro (version>= 6)) (net.aserve::warn-if-crlf ef)
      (setf (stream-external-format sock) ef))

    #+allegro
    (if* net.aserve::*watch-for-open-sockets*
       then (schedule-finalization 
	     sock 
	     #'net.aserve::check-for-open-socket-before-gc))
    
    #+io-timeout
    (if* (integerp timeout)
       then (socket:socket-control 
		 sock 
		 :read-timeout timeout
		 :write-timeout timeout))
	    
    
    (if* query
       then (case method
	      ((:get :put)  ; add info the uri
	       ; must not blast a uri we were passed
	       (if* (not fresh-uri)
		  then (setq uri (puri:copy-uri uri)))
	       (setf (puri:uri-query uri) (query-to-form-urlencoded
					      query
					      :external-format
					      external-format)))
	      (:post 	; make the content
	       (if* content
		  then (error "Can't specify both query ~s and content ~s"
			      query content))
	       (setq content (query-to-form-urlencoded
			      query :external-format external-format)
		     content-type "application/x-www-form-urlencoded"))))
		 
    
    (net.aserve::format-dif :xmit sock "~a ~a ~a~a"
			    (string-upcase (string method))
			    (if* proxy
			       then (puri:render-uri uri nil)
			       else (uri-path-etc uri))
			    (string-upcase (string protocol))
			    crlf)

    ; always send a Host header, required for http/1.1 and a good idea
    ; for http/1.0
    (if*  (not (eql scheme-default-port  port))
       then (net.aserve::format-dif :xmit sock "Host: ~a:~a~a" host port crlf)
       else (net.aserve::format-dif :xmit  sock "Host: ~a~a" host crlf))
    
    ; now the headers
    (if* keep-alive
       then (net.aserve::format-dif :xmit
				    sock "Connection: Keep-Alive~a" crlf))

    (if* accept
       then (net.aserve::format-dif :xmit
				    sock "Accept: ~a~a" accept crlf))

    ; content can be a nil, a single vector or a list of vectors.
    ; canonicalize..
    (if* (and content (atom content)) then (setq content (list content)))
    
    (if* content
       then (let ((computed-length 0))
	      (dolist (content-piece content)
		(typecase content-piece
                  ;;added for paserve - in some lisps (e.g. mcl)
                  ;;strings are not character arrays
		  ((or (array character (*)) (array base-char (*)))
		   (if* (null content-length)
		      then (incf computed-length
                                 #+allegro
				 (native-string-sizeof 
				  content-piece
				  :external-format external-format)
                                 #-allegro
                                 (length content-piece))))
		 
		  ((array (unsigned-byte 8) (*)) 
		   (if* (null content-length)
		      then (incf computed-length (length content-piece))))
		  (t (error "Illegal content array: ~s" content-piece))))
	      
	      (if* (null content-length)
		 then (setq content-length computed-length))))
    
	    
    
    (if* content-length
       then (net.aserve::format-dif :xmit
				    sock "Content-Length: ~s~a" content-length crlf))
    
	    
    (if* cookies 
       then (let ((str (compute-cookie-string uri
					      cookies)))
	      (if* str
		 then (net.aserve::format-dif :xmit
					      sock "Cookie: ~a~a" str crlf))))

    (if* basic-authorization
       then (net.aserve::format-dif :xmit sock "Authorization: Basic ~a~a"
				    (base64-encode
				     (format nil "~a:~a" 
					     (car basic-authorization)
					     (cdr basic-authorization)))
				    crlf))
    
    (if* proxy-basic-authorization
       then (net.aserve::format-dif :xmit sock "Proxy-Authorization: Basic ~a~a"
				    (base64-encode
				     (format nil "~a:~a"
					     (car proxy-basic-authorization)
					     (cdr proxy-basic-authorization)))
				    crlf))

    (if* (and digest-authorization
	      (digest-response digest-authorization))
       then ; put out digest info
	    (net.aserve::format-dif 
	     :xmit sock
	     "Authorization: Digest username=~s, realm=~s, nonce=~s, uri=~s, qop=~a, nc=~a, cnonce=~s, response=~s~@[, opaque=~s~]~a"
	     (digest-username digest-authorization)
	     (digest-realm digest-authorization)
	     (digest-nonce digest-authorization)
	     (digest-uri digest-authorization)
	     (digest-qop digest-authorization)
	     (digest-nonce-count digest-authorization)
	     (digest-cnonce digest-authorization)
	     (digest-response digest-authorization)
	     (digest-opaque digest-authorization)
	     crlf))
	     
				    
				    

    (if* user-agent
       then (if* (stringp user-agent)
	       thenret
	     elseif (eq :aserve user-agent)
	       then (setq user-agent net.aserve::*aserve-version-string*)
	     elseif (eq :netscape user-agent)
	       then (setq user-agent "Mozilla/4.7 [en] (WinNT; U)")
	     elseif (eq :ie user-agent)
	       then (setq user-agent "Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0)")
	       else (error "Illegal user-agent value: ~s" user-agent))
	    (net.aserve::format-dif :xmit
				    sock "User-Agent: ~a~a" user-agent crlf))

    (if* content-type
       then (net.aserve::format-dif :xmit sock "Content-Type: ~a~a"
				    content-type
				    crlf))
    (if* headers
       then (dolist (header headers)
	      (net.aserve::format-dif :xmit sock "~a: ~a~a" 
				      (car header) (cdr header) crlf)))
    

    (write-string crlf sock)  ; final crlf
    
    ; send out the content if there is any.
    ; this has to be done differently so that if it looks like we're
    ; going to block doing the write we start another process do the
    ; the write.  
    (if* content
       then ; content can be a vector a list of vectors
	    (if* (atom content) then (setq content (list content)))
	    (dolist (cont content)
	      (net.aserve::if-debug-action 
	       :xmit
	       (format net.aserve::*debug-stream*
		       "client sending content of ~d characters/bytes"
		       (length cont)))
	      (write-sequence cont sock)))
    
    
    (force-output sock)
    
    (make-instance 'client-request
      :uri uri
      :socket sock
      :cookies cookies
      :method method
      )))


(defun uri-path-etc (uri)
  ;; return the string form of the uri path, query and fragment
  (let ((nuri (puri:copy-uri uri)))
    (setf (puri:uri-scheme nuri) nil)
    (setf (puri:uri-host nuri) nil)
    (setf (puri:uri-port nuri) nil)
    (if* (null (puri:uri-path nuri))
       then (setf (puri:uri-path nuri) "/"))
    
    (puri:render-uri nuri nil)))
    
    
(defmethod read-client-response-headers ((creq client-request))
  ;; read the response and the headers
  (let ((buff (get-header-line-buffer))
	(buff2 (get-header-line-buffer))
	(pos 0)
	len
	(sock (client-request-socket creq))
	(headers)
	protocol
	response
	comment
	val
	)
    (unwind-protect
	(with-better-scan-macros
	    (if* (null (setq len (read-socket-line sock buff (length buff))))
	       then ; eof getting response
		    (error "premature eof from server"))
	  (macrolet ((fail ()
		       `(let ((i 0))
			  (error "illegal response from web server: ~s"
				 (collect-to-eol buff i len)))))
	    (setq protocol (collect-to #\space buff pos len))
	    (skip-to-not #\space buff pos len)
	    (setq response (collect-to #\space buff pos len nil t))
	    ; some servers don't return a comment, so handle that
	    (skip-to-not #\space buff pos len nil)
	    (setq comment (collect-to-eol buff pos len)))

	  (if* (equalp protocol "HTTP/1.0")
	     then (setq protocol :http/1.0)
	   elseif (equalp protocol "HTTP/1.1")
	     then (setq protocol :http/1.1)
	     else (error "unknown protocol: ~s" protocol))
      
	  (setf (client-request-protocol creq) protocol)
      
	  (setf (client-request-response-code creq) 
	    (quick-convert-to-integer response))
      
	  (setf (client-request-response-comment creq) comment)
      

	  ; now read the header lines
	  (setq headers (net.aserve::compute-client-request-headers sock))
	  
      
	  (setf (client-request-headers creq) headers)
	  
	  ;; do cookie processing
	  (let ((jar (client-request-cookies creq)))
	    (if* jar
	       then ; do all set-cookie requests
		    (let (prev cs)
		      ; Netscape v3 web server bogusly splits set-cookies
		      ; over multiple set-cookie lines, so we look for
		      ; incomplete lines (those ending in #\;) and combine
		      ; them with the following set-cookie
		      (dolist (headval headers)
			(if* (eq :set-cookie (car headval))
			   then (if* prev 
				   then (setq prev (concatenate 'string
						     prev (cdr headval)))
				   else (setq prev (cdr headval)))
				
				(if* (not (eql #\; (last-character prev)))
				   then (push prev cs)
					(setq prev nil))
					
			 elseif prev
			   then (push prev cs)
				(setq prev nil)))
		      
		      (if* prev
			 then (push prev cs))
		      
		      (dolist (cc (nreverse cs))
			(save-cookie (client-request-uri creq)
				     jar
				     cc)))))
	  
	  
	  (if* (eq :head (client-request-method creq))
	     then  ; no data is returned for a head request
		  (setf (client-request-bytes-left creq) 0)
	   elseif (equalp "chunked" (client-response-header-value 
				     creq :transfer-encoding))
	     then ; data will come back in chunked style
		  (setf (client-request-bytes-left creq) :chunked)
		  (acl-compat.socket:socket-control (client-request-socket creq)
					 :input-chunking t)
	   elseif (setq val (client-response-header-value
			     creq :content-length))
	     then ; we know how many bytes are left
		  (setf (client-request-bytes-left creq) 
		    (quick-convert-to-integer val))
	   elseif (not (equalp "keep-alive"
			       (client-response-header-value
				creq :connection)))
	     then ; connection will close, let it indicate eof
		  (setf (client-request-bytes-left creq) :unknown)
	     else ; no data in the response
		  nil)
	  
		  
	  
	  creq  ; return the client request object
	  )
      (progn (put-header-line-buffer buff2 buff)))))
		  


(defmethod client-request-read-sequence (buffer
					 (creq client-request)
					 &key
					 (start 0)
					 (end (length buffer)))
  ;; read the next (end-start) bytes from the body of client request, handling
  ;;   turning on chunking if needed
  ;;   return index after last byte read.
  ;;   return 0 if eof
  (let ((bytes-left (client-request-bytes-left creq))
	(socket (client-request-socket creq))
	(last start))
    (if* (integerp bytes-left)
       then ; just a normal read-sequence
	    (if* (zerop bytes-left)
	       then 0  ; eof
	       else (let ((ans (net.aserve::rational-read-sequence buffer 
						       socket :start start
					      :end (+ start 
						      (min (- end start) 
							   bytes-left)))))
		      (if* (eql ans start)
			 then 0  ; eof
			 else (net.aserve::if-debug-action :xmit
					       (write-sequence 
						buffer 
						net.aserve::*debug-stream*
						:start start
						:end
						ans))
			      (setf (client-request-bytes-left creq)
				(- bytes-left (- ans start)))
			      ans)))
     elseif (or (eq bytes-left :chunked)
		(eq bytes-left :unknown))
       then (handler-case (do ((i start (1+ i))
			       (stringp (stringp buffer))
			       (debug-on (member :xmit 
						 net.aserve::*debug-current*
						 :test #'eq)))
			      ((>= i end) (setq last end))
			    (setq last i)
			    (let ((ch (if* stringp
					 then (read-char socket nil nil)
					 else (read-byte socket nil nil))))
			      (if* (null ch)
				 then (return)
				 else (if* debug-on
					 then (write-char
					       (if* (characterp ch) 
						  then ch
						  else (code-char ch))
					       net.aserve::*debug-stream*))
				      (setf (aref buffer i) ch))))
	      (acl-compat.excl::socket-chunking-end-of-file
		  (cond)
		(declare (ignore cond))
		; remember that there is no more data left
		(setf (client-request-bytes-left creq) :eof)
		nil))
	    ; we return zero on eof, regarless of the value of start
	    ; I think that this is ok, the spec isn't completely clear
	    (if* (eql last start) 
	       then 0 
	       else last)
     elseif (eq bytes-left :eof)
       then 0
       else (error "socket not setup for read correctly")
	    )))
  

(defmethod client-request-close ((creq client-request))
  (let ((sock (client-request-socket creq)))
    (if* sock
       then (setf (client-request-socket creq) nil)
	    (ignore-errors (force-output sock))
	    (ignore-errors (close sock)))))


(defun quick-convert-to-integer (str)
  ; take the simple string and convert it to an integer
  ; it's assumed to be a positive number
  ; no error checking is done.
  (let ((res 0))
    (dotimes (i (length str))
      (let ((chn (- (char-code (schar str i)) #.(char-code #\0))))
	(if* (<= 0 chn 9)
	   then (setq res (+ (* 10 res) chn)))))
    res))


(defmethod client-response-header-value ((creq client-request)
					 name &key parse)
  ;; return the value associated with the given name
  ;; parse it too if requested
  (if* (stringp name)
     then (error "client-response headers are now named by symbols, not strings"))
  
  (let ((val (cdr (assoc name (client-request-headers creq) :test #'eq))))
    (if* (and parse val)
       then (net.aserve::parse-header-value val)
       else val)))

    
  


(defun read-socket-line (socket buffer max)
  ;; read the next line from the socket.
  ;; the line may end with a linefeed or a return, linefeed, or eof
  ;; in any case don't put that the end of line characters in the buffer
  ;; return the number of characters in the buffer which will be zero
  ;; for an empty line.
  ;; on eof return nil

  ;; JSC Note: This function is only used for reading headers. Therefore we
  ;;           are safe in always doing CODE->CHAR conversions here.

  (let ((i 0))
    (loop
      (let* ((ch (read-byte socket nil nil))
             (ch (and ch (code-char ch))))
	(if* (null ch)
	   then ; eof from socket
		(if* (> i 0)
		   then ; actually read some stuff first
			(return i)
		   else (return nil) ; eof
			)
	 elseif (eql ch #\return)
	   thenret ; ignore
	 elseif (eql ch #\linefeed)
	   then ; end of the line,
		(return i)
	 elseif (< i max)
	   then ; ignore characters beyone line end
		(setf (schar buffer i) ch)
		(incf i))))))
		
		
    
      
;; buffer pool for string buffers of the right size for a header
;; line

(defvar *response-header-buffers* nil)

(defun get-header-line-buffer ()
  ;; return the next header line buffer
  (let (buff)
    (acl-compat.excl::atomically
      (acl-compat.excl::fast (setq buff (pop *response-header-buffers*))))
    (if* buff
       thenret
       else (make-array 400 :element-type 'character))))

(defun put-header-line-buffer (buff &optional buff2)
  ;; put back up to two buffers
  (acl-compat.mp:without-scheduling
    (push buff *response-header-buffers*)
    (if* buff2 then (push buff2 *response-header-buffers*))))




(defun compute-digest-authorization (creq da)
  ;; compute the digest authentication info, if such is present
  ;; return true if did the authentication thing
  (let ((val (cdr (assoc :www-authenticate (client-request-headers creq))))
	(params))
    
    
    (if* (not (and val
		   (null (mismatch "digest " val :end2 7 :test #'char-equal))))
       then ; not a digest authentication
	    (return-from compute-digest-authorization nil))
    
    (setq params (net.aserve::parse-header-line-equals 
		  val #.(length "digest ")))
    
    
    (setf (digest-opaque da) (cdr (assoc "opaque" params :test #'equal)))
    
    (let ((md (md5-init))
	  (qop (cdr (assoc "qop" params :test #'equalp)))
	  (ha1)
	  (ha2))
      
      (setf (digest-qop da) qop)
      
      (md5-update md (digest-username da))
      (md5-update md ":")
      (md5-update md (setf (digest-realm da)
		       (or (cdr (assoc "realm" params :test #'equalp)) "")))
      (md5-update md ":")
      (md5-update md (digest-password da))
      (setq ha1 (md5-final md :return :hex))
      
      ; compute a2
      
      (setq md (md5-init))
      (md5-update md (string-upcase
		      (symbol-name (client-request-method creq))))
      (md5-update md ":")
      ; this is just a part of the whole uri but should be enough I hope
      (md5-update md (setf (digest-uri da) 
		       (uri-path-etc (client-request-uri creq))))

      (if* (equal "auth-int" qop)
	 then (error "auth-int digest not supported"))
      
      (setq ha2 (md5-final md :return :hex))
      
      
      
      
      ; calculate response
      
      (setq md (md5-init))
      
      (md5-update md ha1)
      (md5-update md ":")
      (md5-update md (setf (digest-nonce da)
		       (or (cdr (assoc "nonce" params :test #'equalp))
			   "")))
      (md5-update md ":")
      (if* qop
	 then (md5-update md (digest-nonce-count da))
	      (md5-update md ":")
	      (md5-update md (setf (digest-cnonce da)
			       (format nil "~x" (+ (ash (get-universal-time) 5)
						 (random 34567)))))
	      (md5-update md ":")
	      (md5-update md qop)
	      (md5-update md ":"))
      (md5-update md ha2)
      
      (setf (digest-response da) (md5-final md :return :hex))

      t
      )))
	      
	      
	      
      
	
			       
      
      
      
      
      
      
    
    
    

	    
	    
    

    

;;;;; cookies

(defclass cookie-jar ()
  ;; holds all the cookies we've received
  ;; items is a alist where each item has the following form:
  ;; (hostname cookie-item ...)
  ;; 
  ;; where hostname is a string that must be the suffix
  ;;	of the requesting host to match
  ;; path is a string that must be the prefix of the requesting host
  ;;	to match
  ;;  
  ;;
  ((items :initform nil
	  :accessor cookie-jar-items)))

(defmethod print-object ((jar cookie-jar) stream)
  (print-unreadable-object (jar stream :type t :identity t)
    (format stream "~d cookies" (length (cookie-jar-items jar)))))

;* for a given hostname, there will be only one cookie with
; a given (path,name) pair
;
(defstruct cookie-item 
  path      ; a string that must be the prefix of the requesting host to match
  name	    ; the name of this cookie
  value	    ; the value of this cookie
  expires   ; when this cookie expires
  secure    ; t if can only be sent over a secure server
  )


(defmethod save-cookie (uri (jar cookie-jar) cookie)
  ;; we've made a request to the given host and gotten back
  ;; a set-cookie header with cookie as the value 
  ;; jar is the cookie jar into which we want to store the cookie
  
  (let* ((pval (car (net.aserve::parse-header-value cookie t)))
	 namevalue
	 others
	 path
	 domain
	 )
    (if* (consp pval)
       then ; (:param namevalue . etc)
	    (setq namevalue (cadr pval)
		  others (cddr pval))
     elseif (stringp pval)
       then (setq namevalue pval)
       else ; nothing here
	    (return-from save-cookie nil))
    
    ;; namevalue has the form name=value
    (setq namevalue (net.aserve::split-on-character namevalue #\=
						    :count 1))
    
    ;; compute path
    (setq path (cdr (net.aserve::assoc-paramval "path" others)))
    (if* (null path)
       then (setq path (or (puri:uri-path uri) "/"))
       else ; make sure it's a prefix
	    (if* (not (net.aserve::match-head-p 
		       path (or (puri:uri-path uri) "/")))
	       then ; not a prefix, don't save
		    (return-from save-cookie nil)))
    
    ;; compute domain
    (setq domain (cdr (net.aserve::assoc-paramval "domain" others)))
    
    (if* domain
       then ; one is given, test to see if it's a substring
	    ; of the host we used
	    (if* (null (net.aserve::match-tail-p domain 
						 (puri:uri-host uri)))
	       then (return-from save-cookie nil))
       else (setq domain (puri:uri-host uri)))
    
    
    (let ((item (make-cookie-item
		 :path path
		 :name  (car namevalue)
		 :value (or (cadr namevalue) "")
		 :secure (net.aserve::assoc-paramval "secure" others)
		 :expires (cdr (net.aserve::assoc-paramval "expires" others))
		 )))
      ; now put in the cookie jar
      (let ((domain-vals (assoc domain (cookie-jar-items jar) :test #'equal)))
	(if* (null domain-vals)
	   then ; this it the first time for this host
		(push (list domain item) (cookie-jar-items jar))
	   else ; this isn't the first
		; check for matching path and name
		(do* ((xx (cdr domain-vals) (cdr xx))
		     (thisitem (car xx) (car xx)))
		    ((null xx)
		     )
		  (if* (and (equal (cookie-item-path thisitem)
				   path)
			    (equal (cookie-item-name thisitem)
				   (car namevalue)))
		     then ; replace this one
			  (setf (car xx) item)
			  (return-from save-cookie nil)))
		
		; no match, must insert based on the path length
		(do* ((prev nil xx)
		      (xx (cdr domain-vals) (cdr xx))
		      (thisitem (car xx) (car xx))
		      (length (length path)))
		    ((null xx)
		     ; put at end
		     (if* (null prev) then (setq prev domain-vals))
		     (setf (cdr prev) (cons item nil)))
		  (if* (>= (length (cookie-item-path thisitem)) length)
		     then ; can insert here
			  (if* prev
			     then (setf (cdr prev)
				    (cons item xx))
				  
			     else ; at the beginning
				  (setf (cdr domain-vals)
				    (cons item (cdr domain-vals))))
			  (return-from save-cookie nil))))))))
		  
      

(defparameter cookie-separator
    ;; useful for separating cookies, one per line
    (make-array 10 :element-type 'character
		:initial-contents '(#\return
				    #\linefeed 
				    #\C
				    #\o
				    #\o
				    #\k
				    #\i
				    #\e
				    #\:
				    #\space)))

(defmethod compute-cookie-string (uri (jar cookie-jar))
  ;; compute a string of the applicable cookies.
  ;;
  (let ((host (puri:uri-host uri))
	(path (or (puri:uri-path uri) "/"))
	res
	rres)
    
    (dolist (hostval (cookie-jar-items jar))
      (if* (net.aserve::match-tail-p (car hostval)
				     host)
	 then ; ok for this host
	      (dolist (item (cdr hostval))
		(if* (net.aserve::match-head-p (cookie-item-path item)
					       path)
		   then ; this one matches
			(push item res)))))
    
    (if* res
       then ; have some cookies to return
	    #+ignore (dolist (item res)
		       (push (cookie-item-value item) rres)
		       (push "=" rres)
		       (push (cookie-item-name item) rres)
		       (push semicrlf rres))
	    
	    (dolist (item res)
	      (push (cookie-item-value item) rres)
	      (push "=" rres)
	      (push (cookie-item-name item) rres)
	      (push cookie-separator rres))
	    
	    (pop rres) ; remove first seperator
	    (apply #'concatenate 'string  rres))))

(defun last-character (string)
  ;; return the last non blank character, or nil
  (do ((i (1- (length string)) (1- i))
       (ch))
      ((< i 0) nil)
    (setq ch (schar string i))
    (if* (eql #\space ch) 
       thenret
       else (return ch))))

			   
   
    
    
  





    
  
	    
  
		 




		      
			
					   
  
			
		      
  


		
		

	    
    
	      
    
    
    
      
    
  
  
