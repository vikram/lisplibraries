(in-package #:lift)

#+(or)
(run-tests :suite 'equality-test)

(deftestsuite equality-test (lift-test)
  ())

(deftestsuite equality-test-1 (equality-test)
  ()
  (:equality-test '=)
  (:documentation "Ensure that equality-test is inherited"))

(addtest (equality-test-1) test-1
	 (ensure-same *lift-equality-test* '= :test #'equal))

(deftestsuite equality-test-2 (equality-test-1)
  ())

(addtest (equality-test-2) test-1
	 (ensure-same *lift-equality-test* '= :test #'equal))

(deftestsuite equality-test-3 (equality-test-1)
  ()
  (:equality-test 'equalp))

(addtest (equality-test-3) test-1
	 (ensure-same *lift-equality-test* 'equalp :test #'equal))

(deftestsuite test-equality-test-works (equality-test)
  ()
  (:equality-test 'equalp)
  (:setup
   (ensure-same #(1 2 4) #(1 2 4))))

(addtest (test-equality-test-works)
  test-1
  (ensure-same #(1 2 4) #(1 2 4)))


