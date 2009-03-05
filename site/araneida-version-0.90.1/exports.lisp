(in-package :araneida)

;;; The list of registered URL handlers, and functions for
;;; manipulating it

;; List of (method match url-prefix handler) 
(defvar *authentication-handlers* (list nil))
(defvar *authorization-handlers* (list nil))
(defvar *response-handlers* (list nil))
(defvar *error-handlers* (list nil))
(defvar *log-handlers* (list nil))

(defun export-method (export) (first export))
(defun export-match (export) (second export))
(defun export-prefix (export) (third export))
;(defun export-handler (export) (fourth export))
(defun export-needs-discriminator-p (export) (fifth export))

(defun handlers-for-stage (stage)
  (case stage
    (:authentication *authentication-handlers*)
    (:authorization *authorization-handlers*)
    (:response *response-handlers*)
    (:error *error-handlers*)
    (:log *log-handlers*)
    (t stage)))

(defun export-eq (e1 e2)
  (and (eql (export-method e1) (export-method e2))
       (eql (export-match e1) (export-match e2))
       (string= (export-prefix e1) (export-prefix e2))))

(defun string-prefix-length (short-string long-string)
  (let ((short-length (length short-string)))
    (if (> short-length (length long-string))
         nil
      (let ((m (mismatch long-string short-string)))
        (cond ((eql m nil) short-length)
              ((< m short-length) nil)
              (t m))))))

(defun matching-export-method-p (export method)
  (let ((m (export-method export)))
    (or (eql m t) (eql m method))))

(defun matching-export-p (export urlstring method)
  "Is EXPORT a valid handler for URLSTRING MATCH PREFIX?"
  (and (matching-export-method-p export method)
       (if (eql (export-match export) :prefix)
           (string-prefix-length (export-prefix export) urlstring)
           (string=  (export-prefix export) urlstring))))

;;; Precondition: both e1 and e2 satisfy matching-export-p
;;; - the longer prefix is preferred over the shorter one
;;; - :exact match favoured over :component over :prefix, 
;;; - exact method export selected over T (default) export
(defun export-better-p (e1 e2)
  (labels ((match (arg) (case arg (:exact 0) (:component 1) (:prefix 2))))
    (let ((l (- (length (export-prefix e1)) (length (export-prefix e2)))))
      (cond ((> l 0) t)
            ((< l 0) nil)
            (t
             (let ((m1 (match (export-match e1)))
                   (m2 (match (export-match e2))))
               (cond ((< m1 m2) t)
                     ((> m1 m2) nil)
                     ((and (not (eql t (export-method e1)))
                           (eql t (export-method e2))) t)
                     (t nil))))))))

(defvar *root-handler*)
(defun export-handler (url handler
                       &key
                       (match :prefix)
                       (method t)
                       needs-discriminator
                       (stage :response))
  "Add HANDLER to the list of exported URL handlers.  HANDLER may be a
function designator, which will be called with arguments REQUEST and
REST-OF-URL, or a list whose car designates a function and whose cdr will
be passed to it as aditional arguments.  Keyword :MATCH may be :PREFIX
or :EXACT.  *ROOT-HANDLER* must be bound to the root handler for some 
appropriate listener."
  (let ((handlers (handlers-for-stage stage))
        (export (list
                 method match (urlstring url) handler needs-discriminator)))
    (install-handler *root-handler* (make-instance 'legacy-handler)
		     (urlstring url) (eql match :exact))
    (aif (find export handlers :test #'export-eq)
         (rplacd it (cdr export))
         (nconc handlers (list export)))))

(defun find-export (url-string stage method)
  (let ((handlers (handlers-for-stage stage))
        (best nil))
    (loop for export in handlers
          if (and (matching-export-p export url-string method)
                  (or (not best) (export-better-p export best)))
          do (setf best export))
    (if best
        best
        (list nil nil nil nil))))
    

#+nil
(defvar *exported-servers* '())
#+nil
(defvar *default-server* nil
  "Server for requests without a Host header (e.g. HTTP/0.9")

#+nil
(defun export-server (server)
  (unless *default-server* (setf *default-server* server))
  (or (find server *exported-servers* :test #'server-equal-p)
      (push server *exported-servers*)))

#+nil
(defun server-for-url (url)
  (find (merge-url url "/")
        *exported-servers* :key #'server-base-url :test #'url-equal-p))

#+nil
(defun server-for-name-colon-port (name-colon-port-string)
  (let* ((colon-pos (position #\: name-colon-port-string))
         (name (subseq name-colon-port-string 0 colon-pos))
         (port-s (when colon-pos
                   (subseq name-colon-port-string
                           (1+ (length name)) nil)))
         (port (if port-s (parse-integer port-s) 80)))
    (or (find-if (lambda (s) (and (string-equal name (server-name s))
                                  (eql port (server-port s))))
                 *exported-servers*)
        (error "Unable to find exported server ~A" name))))


;;; exported
#+nil
(defun output-apache-conf (&optional (stream t))
  "Output a chunk of text suitable for including in an Apache configuration,
which comprises a VirtualHost section with ProxyPass for each exported server"
  (dolist (i  *exported-servers*)
    (output-apache-conf-segment i stream)))


