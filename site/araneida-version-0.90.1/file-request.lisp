(in-package :araneida)

;;; XXX deprecated - see static-file-handler.lisp

;;; XXX should fix to stop people requestiong ../../.. to get out of the
;;; document root.  In practice Apache won't allow it anyway, but better
;;; to be sure

(defun file-request-handler (r arg-string root
                               &optional (directoryindex "index.html"))
  (let* ((file (if (or (eql 0 (length arg-string))
                       (eql (elt arg-string (1- (length arg-string))) #\/))
                   (s. arg-string directoryindex)
                 arg-string))
         (extension (subseq file (aif  (position #\. file :from-end t)
				       (1+ it)
				       (length file))))
         (content-type (cadr (assoc extension *content-types* :test #'string=)))
         (fnam (merge-pathnames file root)))
    (if (and (not (wild-pathname-p fnam)) (probe-file fnam))
        (with-open-file (in fnam :direction :input)
          (if (ignore-errors (peek-char nil in nil))
              (send-file r fnam :content-type (or content-type "text/plain"))))
	(let ((error-message (format nil "Can't find file ~S" fnam)))
	  (request-send-error r 404 :log-message error-message :client-message error-message)))))

