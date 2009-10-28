;; this file must be stored in the same directory
;; where all the lisp libraries are stored 
;; (every library in its own subdirectory)

(require 'asdf)

(flet
    ((reg (relative-lib-dir)
       (let ((lib-dir
              (directory-namestring 
               (merge-pathnames relative-lib-dir
                                (load-time-value
                                 (or #.*compile-file-pathname* 
                                     *load-pathname*))))))
         (print lib-dir)
         (pushnew lib-dir
                  asdf:*central-registry*
                  :test #'equalp))))
  (reg "alexandria/")
  (reg "asdf-binary-locations/")
  (reg "babel_0.2.0/")
  (reg "cffi-080926/")
  (reg "chunga-0.4.3/")
  (reg "cl-base64-3.3.2/")
  (reg "cl-fad-0.6.2/") 
  (reg "cl-ppcre-2.0.1/")
  (reg "cl-who-0.11.0/")
  (reg "cl+ssl/")
  (reg "flexi-streams-1.0.7/")
  (reg "hunchentoot-0.15.7/")
  (reg "md5-1.8.5/")
  (reg "rfc2388/")
  (reg "trivial-features_0.1/")
  (reg "trivial-gray-streams/")
  (reg "url-rewrite-0.1.1/"))

(asdf:operate 'asdf:load-op :asdf-binary-locations)
