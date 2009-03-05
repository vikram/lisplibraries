(in-package #:moptilities-test)

(defclass test-copy-template-1 ()
  ((tct-a 
    :initform :tct-a
    :initarg :tct-a
    :accessor tct-a)
   (tct-b 
    :initform :tct-b
    :initarg :tct-b
    :accessor tct-b)))

(defclass test-copy-template-2 (test-copy-template-1)
  ((tct-c 
    :initform :tct-c
    :initarg :tct-c
    :accessor tct-c)
   (tct-d 
    :initform :tct-d
    :initarg :tct-d
    :accessor tct-d
    :allocation :class)))

(deftestsuite test-copy-template (moptilities-test)
  ())

(addtest (test-copy-template)
  test-1
  (let* ((a (make-instance 'test-copy-template-1
			   :tct-a 1
			   :tct-b 2))
	 (b (copy-template a)))
    (ensure-same (tct-a a) (tct-a b))
    (ensure-same (tct-b a) (tct-b b))))

(addtest (test-copy-template)
  test-class-slots
  (let* ((a (make-instance 'test-copy-template-2
			   :tct-b 3
			   :tct-c 4
			   :tct-d 'd))
	 (b (copy-template a)))
    (ensure-same (tct-a a) (tct-a b))
    (ensure-same (tct-b a) (tct-b b))
    (ensure-same (tct-c a) (tct-c b))
    (ensure-same (tct-d a) (tct-d b))))
    