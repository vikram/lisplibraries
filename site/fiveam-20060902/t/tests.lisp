;;;; -*- lisp -*-

(in-package :it.bese.FiveAM)

(in-suite :it.bese.FiveAM)

(def-suite test-suite :description "Suite for tests which should fail.")

(defmacro with-test-results ((results test-name) &body body)
  `(let ((,results (with-*test-dribble* nil (run ',test-name))))
     ,@body))

;;;; Test the checks

(test (is1 :suite test-suite)
  (is (plusp 1))
  (is (< 0 1))
  (is (not (plusp -1)))
  (is (not (< 1 0)))
  (is-true t)
  (is-false nil))

(test (is2 :suite test-suite)
  (is (plusp 0))
  (is (< 0 -1))
  (is (not (plusp 1)))
  (is (not (< 0 1)))
  (is-true nil)
  (is-false t))

(test is
  (with-test-results (results is1)
    (is (= 6 (length results)))
    (is (every #'test-passed-p results)))
  (with-test-results (results is2)
    (is (= 6 (length results)))
    (is (every #'test-failure-p results))))

(test signals/finishes
  (signals error
    (error "an error"))
  (finishes
   (signals error
    (error "an error"))))

(test pass
  (pass))

(test (fail1 :suite test-suite)
  (fail "This is supposed to fail"))

(test fail
  (with-test-results (results fail1)
    (is (= 1 (length results)))
    (is (test-failure-p (first results)))))

;;;; non top level checks

(test foo-bar
  (let ((state 0))
    (is (= 0 state))
    (is (= 1 (incf state)))))

;;;; Test dependencies

(test (ok :suite test-suite)
  (pass))

(test (not-ok :suite test-suite)
  (fail "This is supposed to fail."))

(test (and1 :depends-on (and ok not-ok) :suite test-suite)
  (fail))

(test (and2 :depends-on (and ok) :suite test-suite)
  (pass))

(test dep-and 
  (with-test-results (results and1)
    (is (= 3 (length results)))
    ;; we should have one skippedw one failed and one passed
    (is (some #'test-passed-p results))
    (is (some #'test-skipped-p results))
    (is (some #'test-failure-p results)))
  (with-test-results (results and2)
    (is (= 2 (length results)))
    (is (every #'test-passed-p results))))

(test (or1 :depends-on (or ok not-ok) :suite test-suite)
  (pass))

(test (or2 :depends-on (or not-ok ok) :suite test-suite)
  (pass))

(test dep-or
  (with-test-results (results or1)
    (is (= 2 (length results)))
    (is (every #'test-passed-p results)))
  (with-test-results (results or2)
    (is (= 3 (length results)))
    (is (= 2 (length (remove-if-not #'test-passed-p results))))))

(test (not1 :depends-on (not not-ok) :suite test-suite)
  (pass))

(test (not2 :depends-on (not ok) :suite test-suite)
  (fail))

(test not
  (with-test-results (results not1)
    (is (= 2 (length results)))
    (is (some #'test-passed-p results))
    (is (some #'test-failure-p results)))
  (with-test-results (results not2)
    (is (= 2 (length results)))
    (is (some #'test-passed-p results))
    (is (some #'test-skipped-p results))))

(test (nested-logic :depends-on (and ok (not not-ok) (not not-ok))
                    :suite test-suite)
  (pass))

(test dep-nested
  (with-test-results (results nested-logic)
    (is (= 3 (length results)))
    (is (= 2 (length (remove-if-not #'test-passed-p results))))
    (is (= 1 (length (remove-if-not #'test-failure-p results))))))

(test (circular-0 :depends-on (and circular-1 circular-2 or1) 
                  :suite test-suite)
  (fail "we depend on a circular dependency, we should not be tested."))

(test (circular-1 :depends-on (and circular-2)
                  :suite test-suite)
  (fail "we have a circular depednency, we should not be tested."))

(test (circular-2 :depends-on (and circular-1)
                  :suite test-suite)
  (fail "we have a circular depednency, we should not be tested."))

(test circular
  (signals circular-dependency
    (run 'circular-0))
  (signals circular-dependency
    (run 'circular-1))
  (signals circular-dependency
    (run 'circular-2)))

(test gen-integer
  (for-all ((a (gen-integer)))
    (is (integerp a))))

(test for-all-guarded
  (for-all ((less (gen-integer))
            (more (gen-integer) (< less more)))
    (is (< less more))))

(test gen-float
  (macrolet ((test-gen-float (type)
               `(for-all ((unbounded (gen-float :type ',type))
                          (bounded   (gen-float :type ',type :bound 42)))
                  (is (typep unbounded ',type))
                  (is (typep bounded ',type))
                  (is (<= (abs bounded) 42)))))
    (test-gen-float single-float)
    (test-gen-float short-float)
    (test-gen-float double-float)
    (test-gen-float long-float)))

(test gen-character
  (for-all ((c (gen-character)))
    (is (characterp c)))
  (for-all ((c (gen-character :code (gen-integer :min 32 :max 40))))
    (is (characterp c))
    (member c (list #\Space #\! #\" #\# #\$ #\% #\& #\' #\())))

(test gen-string
  (for-all ((s (gen-string)))
    (is (stringp s)))
  (for-all ((s (gen-string :length (gen-integer :min 0 :max 2))))
    (is (<= (length s) 2)))
  (for-all ((s (gen-string :elements (gen-character :code (gen-integer :min 0 :max 0))
                           :length (constantly 2))))
    (is (= 2 (length s)))
    (is (every (curry #'char= #\Null) s))))

(defun dummy-mv-generator ()
  (lambda ()
    (list 1 1)))

(test for-all-destructuring-bind
  (for-all (((a b) (dummy-mv-generator)))
    (is (= 1 a))
    (is (= 1 b))))
