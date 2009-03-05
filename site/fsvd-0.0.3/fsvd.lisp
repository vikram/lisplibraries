(in-package :fsvd)

;;; Possibly sparse matrix interface

(defgeneric height-of (matrix &key densep)
  (:documentation "Return the number of rows of MATRIX. If DENSEP
return the number of non-empty rows."))

(defgeneric width-of (matrix &key densep)
  (:documentation "Return the number of columns of MATRIX. If DENSEP
return the number of non-empty columns."))

(defgeneric size-of (matrix)
  (:documentation "Return the number known cells in MATRIX. This is an
upper limit for the dense indices produced by MAP-MATRIX."))

(defgeneric dense-row-index (matrix row)
  (:documentation "Number those rows that are not empty from 0. Return
NIL for empty rows.")
  (:method ((matrix t) row)
    row))

(defgeneric dense-column-index (matrix column)
  (:documentation "Number those columns that are not empty from 0.
Return NIL for empty columns.")
  (:method ((matrix t) column)
    column))

(defgeneric map-matrix (function matrix)
  (:documentation "Call FUNCTION for each non-empty cell of MATRIX.
FUNCTION is of four parameters: ROW, COLUMN, VALUE and DENSE-INDEX
where DENSE-INDEX is akin to a row major index except it doesn't skip
over empty cells. DENSE-INDEX is always less than the SIZE-OF the
MATRIX."))

(defmacro do-matrix (((row column value dense-index) matrix)
                     &body body)
  "A simple, inefficient implementation of the macro interface to
iterate over MATRIX."
  `(map-matrix (lambda (,row ,column ,value ,dense-index)
                 ,@body)
    ,matrix))

(defgeneric do-matrix-macro-name (matrix)
  (:method ((matrix t))
    'do-matrix)
  (:documentation "Return the name of the macro that provides a
hopefully efficient way to iterate over MATRIX. See DO-MATRIX for an
example."))

;;; Vector utilities

(deftype single-float-vector () '(simple-array single-float (*)))
(deftype 2d-single-float-array () '(simple-array single-float (* *)))

(defun make-v (length initial-element)
  (make-array length :element-type 'single-float
              :initial-element initial-element))

;;; Reading and writing float arrays

#+sbcl
(progn

(defun sync->fd (fd-stream)
  (force-output fd-stream)
  (let ((fd (sb-impl::fd-stream-fd fd-stream)))
    (sb-unix:unix-lseek fd (file-position fd-stream) sb-unix:l_set)))

(defun sync<-fd (fd-stream)
  (let ((fd (sb-impl::fd-stream-fd fd-stream)))
    (file-position fd-stream
                   (sb-unix:unix-lseek fd 0 sb-unix:l_incr))))

(defun write-float-array (array fd-stream)
  (declare (type (simple-array single-float (*)) array))
  (sync->fd fd-stream)
  (let ((fd (sb-impl::fd-stream-fd fd-stream)))
    (sb-unix:unix-write fd
                        (sb-sys:vector-sap array)
                        0
                        (* 4 (length array))))
  (sync<-fd fd-stream))

(defun read-float-array (array fd-stream)
  (declare (type (simple-array single-float (*)) array))
  (sync->fd fd-stream)
  (let* ((l (* 4 (length array)))
         (l2 (sb-unix:unix-read (sb-impl::fd-stream-fd fd-stream)
                                (sb-sys:vector-sap array)
                                l)))
    (sync<-fd fd-stream)
    (unless (= l l2)
      (error "Read only ~S bytes out of ~S~%" l2 l))))

(defun write-float (float stream)
  (declare (type single-float float))
  (let ((v (make-array 1 :element-type 'single-float
                       :initial-contents (list float))))
    (write-byte (sb-sys:sap-ref-32 (sb-sys:vector-sap v) 0) stream)))

)

#+allegro
(progn

(defun write-float-array (array stream)
  (declare (type (simple-array single-float (*)) array)
           (type excl:simple-stream stream))
  (excl:write-vector array stream))

(defun read-float-array (array stream)
  (declare (type (simple-array single-float (*)) array)
           (type excl:simple-stream stream))
  (let* ((l (* 4 (length array)))
         (l2 (excl:read-vector array stream)))
    (unless (= l l2)
      (error "Read only ~S bytes out of ~S~%" l2 l))))

)

;;; Singular value decomposition

(defstruct sv
  "SV is a pair of vectors. They are not of unit lenght. The singular
value is implicitly defined as the product of their euclidean norms."
  (left nil :type (simple-array single-float *))
  (right nil :type (simple-array single-float *)))

(defun create-sv (height width &optional (value 0.1))
  (make-sv :left (make-v height value) :right (make-v width value)))

(deftype svd ()
  "SVD consists of n SVs - pairs of left and right vector - of the
same sizes. The matrix SVD represents in this form is obtained by
summing pairwise the outer products of these vectors."
  `(simple-array sv (*)))

(defun make-svd ()
  "Create an empty SVD."
  (make-array 0 :element-type 'sv :initial-element (create-sv 0 0)))

(defun compact-sv (sv matrix)
  "Take SV that uses sparse indices of MATRIX and turn it into one
that uses dense indices."
  (flet ((compact (target source map)
           (loop for i below (length source) do
                 (let ((dense-i (funcall map matrix i)))
                   (when dense-i
                     (setf (aref target dense-i) (aref source i)))))))
    (let ((new-sv (create-sv (height-of matrix :densep t)
                             (width-of matrix :densep t))))
      (compact (sv-left new-sv) (sv-left sv) #'dense-row-index)
      (compact (sv-right new-sv) (sv-right sv) #'dense-column-index)
      new-sv)))

(defun expand-sv (sv matrix)
  "Take SV that uses dense indices of MATRIX and turn it into one that
uses normal indices."
  (flet ((expand (target source map)
           (loop for i below (length target) do
                 (let ((dense-i (funcall map matrix i)))
                   (when dense-i
                     (setf (aref target i) (aref source dense-i)))))))
    (let ((new-sv (create-sv (height-of matrix :densep nil)
                             (width-of matrix :densep nil))))
      (expand (sv-left new-sv) (sv-left sv) #'dense-row-index)
      (expand (sv-right new-sv) (sv-right sv) #'dense-column-index)
      new-sv)))

(defun save-svd (svd filename)
  "Write the content of SVD to FILENAME in a reasonably compact form."
  (ensure-directories-exist filename)
  (with-open-file (stream filename :direction :output
                   :if-does-not-exist :create
                   :if-exists :supersede)
    (let ((sv (if (zerop (length svd))
                  (create-sv 0 0)
                  (aref svd 0))))
      (format stream "~S~%" (list :length (length svd)
                                  :left-size (length (sv-left sv))
                                  :right-size (length (sv-right sv)))))
    (loop for sv across svd do
          (write-float-array (sv-left sv) stream)
          (write-float-array (sv-right sv) stream)))
  (values))

(defun load-svd (filename)
  "Return the SVD loaded from FILENAME."
  (with-open-file (stream filename)
    (destructuring-bind (&key length left-size right-size)
        (read-from-string (read-line stream))
      (let ((svd (make-array length :element-type 'sv)))
        (loop for i below length do
              (let ((sv (create-sv left-size right-size)))
                (read-float-array (sv-left sv) stream)
                (read-float-array (sv-right sv) stream)
                (setf (aref svd i) sv)))
        svd))))

(defun svd-value (svd row column &key (base-value 0.0) (clip #'identity))
  "Return the value of the matrix represented by SVD at ROW and
COLUMN. Start the summation from BASE-VALUE and CLIP the current sum
to some valid range if any after every pass."
  (let* ((clip (coerce clip 'function))
         (sum (funcall clip base-value)))
    (locally
        (declare (type svd svd)
                 (type single-float sum))
      (loop for sv across svd do
            (setf sum (funcall clip (+ sum
                                       (* (aref (sv-left sv) row)
                                          (aref (sv-right sv) column)))))))
    sum))

(defun append-to-svd (svd sv)
  (adjust-array svd (1+ (length svd)) :initial-element sv))

;;; This is the performance critical loop. Compile it for each run.
(defmacro train/epoch (&key matrix approximation
                       normalization-factor clip do-matrix)
  `(lambda (left right learning-rate)
     (declare (type single-float-vector left right)
              (type single-float learning-rate)
              (inline ,clip)
              (optimize (speed 3)))
     (,do-matrix ((row column value index) ,matrix)
       (let* ((l (aref left row))
              (r (aref right column))
              (err (- value (,clip (+ (aref ,approximation index)
                                      (* l r))))))
         (setf (aref right column) (+ r
                                      (* learning-rate
                                         (- (* err l)
                                            (* ,normalization-factor r)))))
         (setf (aref left row) (+ l
                                  (* learning-rate
                                     (- (* err r)
                                        (* ,normalization-factor l)))))))))

;;; Well, this is not so critical as it is only used when starting
;;; from an existing SVD and when going from one sv to the next in
;;; training.
(defmacro add-sv-to-approximation (&key matrix clip do-matrix)
  `(lambda (sv approximation)
     (declare (type single-float-vector approximation)
              (inline ,clip)
              (optimize (speed 3)))
     (let* ((sv (expand-sv sv ,matrix))
            (left (sv-left sv))
            (right (sv-right sv)))
       (,do-matrix ((row column value index2) ,matrix)
         (declare (ignorable value))
         (setf (aref approximation index2)
               (,clip (+ (aref approximation index2)
                         (* (aref left row)
                            (aref right column))))))
       ,matrix)))

(defun svd-1 (matrix &key trainer svd supervisor learning-rate0)
  (declare (type svd svd))
  ;; We work with sparse indices to avoid having to map indices and
  ;; compact SV then necessary.
  (let (sv0)
    (flet ((supervise (svd iteration)
             (multiple-value-bind (continuep parameters)
                 (funcall supervisor svd iteration)
               (when continuep
                 (destructuring-bind (&key learning-rate sv)
                     parameters
                   (when sv
                     (setf sv0 (expand-sv sv matrix)))
                   (when learning-rate
                     (setf learning-rate0 learning-rate))))
               continuep)))
      (when (supervise svd nil)
        (unless sv0
          (setf sv0 (create-sv (height-of matrix :densep nil)
                               (width-of matrix :densep nil)
                               0.1)))
        (loop for i upfrom 0 do
              (funcall trainer (sv-left sv0) (sv-right sv0) learning-rate0)
              (unless (supervise (append-to-svd svd (compact-sv sv0 matrix)) i)
                (return)))
        (compact-sv sv0 matrix)))))

(defun svd (matrix &key (svd (make-svd)) (base-approximator (constantly 0.0))
            (learning-rate 0.001) (normalization-factor 0.02)
            supervisor (clip 'identity))
  "Approximate the single-float MATRIX with a quasi singular value
decomposition. Each SV of an SVD consists of a left and a right
vector. The sum of the outer products of the left and right vectors of
its consituent SVs is the approximation provided by an SVD. This SVD
is quasi because while the rank of the approximation is N, the
singular values are not explicit.

The sparse matrix interface must be supported on MATRIX.

BASE-APPROXIMATOR is a function of row and column. Its values are
effectively subtracted from those of MATRIX. Don't forget to supply
the same BASE-APPROXIMATOR to MAKE-SVD-APPROXIMATOR and
SVD-VALUE.

CLIP is a symbol that is fbound to a function that takes a single
single-float and returns it clamped into some valid range or leaves it
alone.

To make training fast, a new trainer function is compiled for each SVD
call using CLIP and DO-MATRIX-MACRO-NAME for MATRIX.

LEARNING-RATE controls the how much weights are drawn towards to
optimum at each step. By default the LEARNING-RATE is scaled by 1/MAE*
where MAE* is the MAE mean avarage error. To avoid normalization and
to have finer grained control one can return learning rate from the
supervisor.

The Tikhonov NORMALIZATION-FACTOR penalizes large weights.

After each iteration on a SV and also before adding a new SV
SUPERVISE-SVD is invoked on SUPERVISOR. The return value being NIL
indicates that the supervisor wants to stop. See SUPERVISE-SVD for
details."
  (let* ((approximation (make-array (size-of matrix)
                                    :element-type 'single-float))
         (trainer (compile nil
                           (eval
                            (list 'train/epoch :matrix matrix
                                  :approximation approximation
                                  :normalization-factor normalization-factor
                                  :clip clip
                                  :do-matrix (do-matrix-macro-name matrix)))))
         (adder (compile nil
                         (eval
                          (list 'add-sv-to-approximation
                                :matrix matrix :clip clip
                                :do-matrix (do-matrix-macro-name matrix)))))
         (clip (coerce clip 'function)))
    (map-matrix
     (lambda (row column value i)
       (declare (ignore value))
       (setf (aref approximation i)
             (funcall clip (funcall base-approximator row column))))
     matrix)
    (flet ((supervise (svd i)
             (supervise-svd supervisor svd i
                            :base-approximator base-approximator
                            :clip clip :matrix matrix
                            :approximation approximation))
           (add (sv)
             (funcall adder sv approximation)))
      (loop for sv across svd do
            (add sv))
      (loop for n upfrom (length svd) do
            (let* ((mean-error (approximation-me matrix approximation))
                   (sv (svd-1 matrix :trainer trainer :svd svd
                              :supervisor #'supervise
                              :learning-rate0 (/ learning-rate
                                                 (float mean-error 0.0)))))
              (unless sv
                (return svd))
              (add sv)
              (setf svd (append-to-svd svd sv)))))))

(defun make-svd-approximator (svd &key matrix (max-n (length svd))
                              (base-approximator (constantly 0))
                              (clip #'identity))
  "Return a function of (ROW COLUMN) parameters that approximates
MATRIX by SVD. The BASE-VALUE for SVD-VALUE is produced by
BASE-APPROXIMATOR for the given coordinates, while CLIP is simply
passed on. The returned function automatically translates to dense
coordinates to query the SVD."
  (let ((svd (subseq svd 0 max-n)))
    (lambda (row column)
      (let ((base-value (funcall base-approximator row column))
            (row (dense-row-index matrix row))
            (column (dense-column-index matrix column)))
        (svd-value svd row column :base-value base-value :clip clip)))))

;;; Utilities

(defun approximation-me (matrix dense-approximation)
  (let ((sum 0.0d0)
        (n 0))
    (fsvd:do-matrix ((row column value index) matrix)
      (declare (ignore row column))
      (unless (zerop value)
        (incf sum (abs (- value (aref dense-approximation index))))
        (incf n)))
    (if (zerop n)
        1.0
        (/ sum n))))

(defun approximation-rmse (matrix dense-approximation)
  (let ((sum 0.0d0)
        (n 0))
    (fsvd:do-matrix ((row column value index) matrix)
      (declare (ignore row column))
      (incf sum (expt (- value (aref dense-approximation index)) 2))
      (incf n))
    (sqrt (/ sum n))))

(defclass limiting-supervisor ()
  ((svd-in-progress :initform (make-svd) :accessor svd-in-progress)
   (max-n-iterations :initform nil :initarg :max-n-iterations
                     :accessor max-n-iterations)
   (max-n-svs :initform nil :initarg :max-n-svs :accessor max-n-svs)
   (trace-stream :initform *trace-output* :initarg :trace-stream
                 :accessor trace-stream))
  (:documentation "Construct an instance, keep it around and while the
SVD is in progress inspect/save SVD-IN-PROGRESS, or set MAX-N-SVS as
you see how the learning is going."))

(defgeneric supervise-svd (supervisor svd iteration &key base-approximator clip
                                      matrix approximation
                                      &allow-other-keys)
  (:method ((supervisor function) svd iteration &rest args)
    (apply supervisor svd iteration args))
  (:method ((supervisor symbol) svd iteration &rest args)
    (apply supervisor svd iteration args))
  (:method ((supervisor limiting-supervisor) svd iteration &key
            base-approximator clip matrix approximation)
    (declare (ignore base-approximator clip))
    (with-slots (svd-in-progress max-n-iterations max-n-svs trace-stream)
        supervisor
      (when (null iteration)
        (format trace-stream "With ~S SVs, RMSE: ~S~%" (length svd)
                (approximation-rmse matrix approximation))
        (force-output))
      (setf svd-in-progress svd)
      (and (or (null max-n-svs)
               (<= (+ (length svd) (if iteration 0 1)) max-n-svs))
           (or (null iteration) (null max-n-iterations)
               (< iteration max-n-iterations)))))
  (:documentation "This is invoked from SVD on its SUPERVISOR
argument. If ITERATION is NIL then a new SV is about to be added and
upon rejecting that and returning NIL the decomposition is finished.
When ITERATION is not NIL, it is a non-negative integer that is the
index of the current iteration on the last SV of SVD. MATRIX,
BASE-APPROXIMATOR, CLIP are passed through verbatim from the SVD call.
APPROXIMATION is a single-float vector that parallels MATRIX with
dense indices (see MAP-MATRIX). APPROXIMATION is updated when about to
start on a new SV.

Its second return value is a list conforming to (&KEY LEARNING-RATE
SV) that can be used to change the learning rate dynamically and to
initialize or change the compact representation of the current SV."))

;;; Simplistic implementation of the FSVD matrix interface for 2D arrays

(defmethod height-of ((array array) &key densep)
  (declare (ignore densep))
  (array-dimension array 0))

(defmethod width-of ((array array) &key densep)
  (declare (ignore densep))
  (array-dimension array 1))

(defmethod size-of ((array array))
  (array-total-size array))

(defmethod map-matrix (function (array array))
  (loop for row below (height-of array) do
        (loop for column below (width-of array) do
              (when (aref array row column)
                (funcall function row column (aref array row column)
                         (array-row-major-index array row column))))))

(defun declarationp (form)
  (and (listp form)
       (eq 'declare (first form))))

(defun split-body (body)
  "Return the declarations and the rest of the body as separate lists."
  (let ((pos (position-if-not #'declarationp body)))
    (values (subseq body 0 pos)
            (subseq body pos))))

(defmacro do-array-matrix (((row column value dense-index) matrix)
                           &body body)
  (let ((width (gensym))
        (%matrix (gensym)))
    (multiple-value-bind (declarations body) (split-body body)
      `(let* ((,%matrix ,matrix)
              (,dense-index 0)
              (,width (the fixnum (width-of ,%matrix))))
         (declare (type (integer 0 #.(1- most-positive-fixnum)) ,dense-index))
         (dotimes (,row (the fixnum (height-of ,%matrix)))
           (dotimes (,column ,width)
             (let ((,value (aref ,%matrix ,row ,column)))
               ,@declarations
               (when ,value
                 ,@body)
               (incf ,dense-index))))))))

(defmethod do-matrix-macro-name ((array array))
  'do-array-matrix)
