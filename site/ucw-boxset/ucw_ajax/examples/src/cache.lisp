;; -*- lisp -*-

(in-package :it.bese.ucw-user)

(defcomponent cache-example ()
  ())

(defmethod render ((c cache-example))
  (<:p "Last effective rendering at: " (<:as-html (get-universal-time))))

(defcomponent timeout-cache-example (cache-example timeout-cache-component)
  ()
  (:render (self)
           (<:p (<:as-html (format nil "Will timeout every ~A seconds, try reloading" (timeout self))))
           (call-next-method))
  (:default-initargs :timeout 10))

(defcomponent hits-cache-example (cache-example num-hits-cache-component)
  ()
  (:render (self)
           (<:p (<:as-html (format nil "Will timeout every ~A reloads, try reloading" (timeout self))))
           (call-next-method))
  (:default-initargs :timeout 5))


