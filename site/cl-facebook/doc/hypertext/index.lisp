(require :cl-who)

(use-package :cl-who)

(defun gen-index ()
  (with-open-file (stream "./index.html" :direction :output :if-exists :supersede)
    (with-html-output (stream)
      (:html
       (:head
	(:title "CL-Facebook")
	(:link :rel "stylesheet" :type "text/css" :href "style.css"))
       (:body
	(:div 
	 :class "header"
	 (:h1 "CL-Facebook"))
	(:p (:em "A simple facebook library for common-lisp."))
	;; DEV DOWNLOAD
	(:h4 "Download the source")
	(:p "Get the source from darcs:")
	(:pre "darcs get http://common-lisp.net/project/cl-facebook/darcs/cl-facebook/")
	;; MAILING LISTS
	(:h4 "Mailing list")
	(:p
	 "Post problems, inquiries, patches to the "
	 (:a :href "/cgi-bin/mailman/listinfo/cl-facebook-devel" "cl-facebook-devel mailing list")
	 ".  Most development discussion goes on through the mailing list.")
	(:pre "darcs get http://common-lisp.net/project/cl-facebook/darcs/cl-facebook/")
      )))))