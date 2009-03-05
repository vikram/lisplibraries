(in-package :araneida)

(defun make-basic-authentication-handler (realm lookup-user)
  "Returns a function suitable for use as a :authentication handler
for REALM.  The called should supply a function LOOKUP-USER taking
two arguments NAME and PASSWORD and returning some unique user identifier"
  (lambda (r arg-string)
    (declare (ignore arg-string))
    (seqlet ((auth (car (request-header r :authorization)))
             (user (apply lookup-user
                          (split (base64-decode (cadr (split auth))) nil '(#\:)))))
      (cond (user
             (setf (request-user r) user))
            (t
             (format (request-stream r) "HTTP/1.0 401 Unauthorized~%WWW-Authenticate: Basic realm=~D~%Content-type: text/html~%~%<h1>Unauthorized</h1><p>Either your browser does not support HTTP basic authentication or you have supplied an incorrect username/password for this page.  Sorry"
                     realm)
             (close (request-stream r))
	     (signal 'response-sent)))
      t)))
             
    




  