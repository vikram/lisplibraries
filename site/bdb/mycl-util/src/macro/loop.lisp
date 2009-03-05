;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Simple looping

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
    ,@body))

(defmacro till (test &body body)
  `(do ()
       ((,test))
    ,@body))

(defmacro forever (&body body)
  `(do ()
       (nil)
    ,@body))
