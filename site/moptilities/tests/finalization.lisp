(in-package #:moptilities-test)

(defclass class-for-finalization-test ()
  ())

(defmethod when-finalized ((self class-for-finalization-test))
  (incf *test-scratchpad*))

#|
;; this doesn't work because setting thing to nil only removes the 'local' name for 
;; thing and doesn't set the slot value...

(deftestsuite finalization-test (moptilities-test)
  ((thing (make-instance 'class-for-finalization-test)))
  (:setup (setf *test-scratchpad* 0)))

(addtest (finalization-test)
  test-caring
  (care-when-finalized thing)
  (setf thing nil)
  (metatilities:collect-garbage)
  (ensure-same *test-scratchpad* 1))
|#

(deftestsuite finalization-test (moptilities-test)
  ()
  (:setup (setf *test-scratchpad* 0)))

(addtest (finalization-test)
  test-caring
  (let ((it (make-instance 'class-for-finalization-test)))
    (care-when-finalized it)
    (setf it nil)
    (metatilities:collect-garbage)
    (metatilities:collect-garbage)
    (ensure-same *test-scratchpad* 1)))

(addtest (finalization-test)
  test-not-caring
  (let ((it (make-instance 'class-for-finalization-test)))
    (identity it)                       ; for effect and in hopes of fooling the compiler
    (setf it nil)
    (metatilities:collect-garbage)
    (metatilities:collect-garbage)
    (ensure-same *test-scratchpad* 0)))
