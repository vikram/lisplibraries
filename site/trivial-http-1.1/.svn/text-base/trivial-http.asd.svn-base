;;;; Silly emacs, this is -*- Lisp -*- (or thereabouts)

;;this is necessary due to a bug in SBCL
#+sbcl
(require :sb-bsd-sockets)

(defsystem trivial-http
    :name "trivial-http"
    :author "Brian Mastenbrook"
    :licence "MIT"
    :description "Trivial support for HTTP GET and POST."
    :depends-on (:trivial-sockets)
    :components ((:file "trivial-http")))