(in-package :facebook-user)

(defvar *fb-api-key* (progn (format t "API Key: ") (read-line))
  "Facebook API key for the intramurals application")

(defvar *fb-secret* (progn (format t "FB Secret: ") (read-line))
  "Facebook secret for the intramurals application")

(defparameter *auth-token* nil
  "Authorization token.")

(defvar *session* (establish-facebook-session
		   *fb-api-key* *fb-secret* *auth-token*))