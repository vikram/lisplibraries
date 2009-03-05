(defclass my-limiting-supervisor (fsvd:limiting-supervisor)
  ()
  (:documentation "Like LIMITING-SUPERVISOR but take initialization of
SV pairs into our own hands."))

(defun make-v (length)
  (let ((r (make-array length :element-type 'single-float)))
    (loop for i below length do
          (setf (aref r i) (+ 0.05 (random 0.015))))
    r))

(defmethod fsvd:supervise-svd ((supervisor my-limiting-supervisor) svd iteration
                               &key base-approximator clip matrix approximation)
  (declare (ignore base-approximator clip approximation))
  (values (call-next-method)
          (if (null iteration)
              (let ((left (make-v (fsvd:height-of matrix :densep t)))
                    (right (make-v (fsvd:width-of matrix :densep t))))
                (list :sv (fsvd:make-sv :left left :right right)))
              nil)))

(defun test-svd (n-svs n-iterations learning-rate contents)
  (let* ((m (make-array (list (length contents)
                              (length (first contents)))
                        ;; Declare the type to help us silence
                        ;; compilation warnings coming from the
                        ;; compilation of inner training loop.
                        :element-type (list* 'member (apply #'append contents))
                        :initial-contents contents))
         (svd (fsvd:svd m :learning-rate learning-rate
                        :normalization-factor 0.0
                        :supervisor (make-instance
                                     'my-limiting-supervisor
                                     :max-n-iterations n-iterations
                                     :max-n-svs n-svs))))
    (format t "~S~%" svd)
    (dotimes (row (fsvd:height-of m))
      (dotimes (column (fsvd:width-of m))
        (format t "~,3F " (fsvd:svd-value svd row column)))
      (terpri))))

(test-svd 1 30 0.1 '((1 2 3 4 5)
                     (2 4 6 8 10)
                     (3 6 9 12 15)))

(test-svd 1 30 0.1 '((10 20 30 40 50)
                     (20 40 60 80 100)
                     (30 60 90 120 150)))

(test-svd 1 30 0.1 '((100 200 300 400 500)
                     (200 400 600 800 1000)
                     (300 600 900 1200 1500)))

(test-svd 1 30 0.1 '((10000 20000 30000 40000 50000)
                     (20000 40000 60000 80000 100000)
                     (30000 60000 90000 120000 150000)))

(test-svd 1 30 0.1 '((0.1 0.2 0.3 0.4 0.5)
                     (0.2 0.4 0.6 0.8 1.0)
                     (0.3 0.6 0.9 1.2 1.5)))

(test-svd 1 30 0.1 '((0.01 0.02 0.03 0.04 0.05)
                     (0.02 0.04 0.06 0.08 0.10)
                     (0.03 0.06 0.09 0.12 0.15)))

(test-svd 2 30 0.1 '((1 2 3 4 5)
                     (2 3 4 5 10)
                     (3 6 9 12 15)))

;;; The same two with missing values.

(test-svd 1 30 0.1 '((1 2 3 nil 5)
                     (2 4 6 8 10)
                     (3 nil nil 12 15)))

(test-svd 2 30 0.1 '((1 2 3 4 nil)
                     (2 3 4 5 10)
                     (nil 6 nil 12 15)))
