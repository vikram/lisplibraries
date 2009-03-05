(in-package :araneida)

(defparameter *araneida-release*
  (asdf:component-version (asdf:find-system 'araneida)))

(defvar *araneida-product-tokens*
  (format nil "Araneida/~A" *araneida-release*))
