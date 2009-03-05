(in-package :zlib-test)

(defparameter *tmp-dir*    
  #+(or :win32 :mswindows :windows) "c:\\temp\\"
  #-(or :win32 :mswindows :windows) "/tmp/")

(defparameter *tmp-file*
  (merge-pathnames *tmp-dir* "cl-zlib-test.gz"))

(defun gen-test-strings (&optional  (lines 100))
  (let ((strs))
    (dotimes (i lines (nreverse strs))
      (setf strs
            (cons (format nil "This is the ~:r line of the compressed file." i)
                  strs)))))

(defun write-lines (lines &optional (testfilename *tmp-file*))
  (let ((chars
         (with-gz (file testfilename :direction :output)
           (format t "Writing compressed file '~a'...~%" testfilename)
           (reduce
            #'(lambda (cnt line)
                (+ cnt (gz-write-line file (format nil "~a~%" line))))
            lines
            :initial-value 0)))
        (fsize (with-open-file (in testfilename)
                 (file-length in))))
      (format t "~d chars written; compressed size: ~d; ratio: ~d%~%"
	      chars
	      fsize
	      (round (* 100 (/ fsize chars))))
      (values chars fsize)))

(defun read-lines (&optional (lines 100) (testfilename *tmp-file*))
  (with-gz (file testfilename :direction :input)
    (let ((strs '()))
      (dotimes (i lines)
        (setf strs (cons (gz-read-line file) strs)))
      (nreverse strs))))

(defun compress-str1 (&optional (str "asdfgh"))
  (compress-string str))

(defun uncompress-str1 (&rest args)
  (apply #' uncompress-string args))

(defun memzip ()
  (let ((a (uffi:allocate-foreign-object :unsigned-char 1000))  ; Source array
        (al (uffi:allocate-foreign-object :long))               ; Source length
        (b  (uffi:allocate-foreign-object :unsigned-char 2000)) ; Destination array
        (bl (uffi:allocate-foreign-object :long))               ; Destination length
        (ca '())  ; data to compress
        (ua '())) ; uncompressed data
    (unwind-protect
         (progn
           (setf (uffi:deref-pointer al :int) 1000
                 (uffi:deref-pointer bl :int) 2000)
           (dotimes (i 2000) (setf (uffi:deref-array b :unsigned-char i) 0))
           ;; Fill input with not-so-random data
           (dotimes (i 1000)
             (let ((data (mod i 7)))
               (setf (uffi:deref-array a :int i) data
                     ca (cons data ca))))
           ;; Compress (note scalar source length)
           (zlib::%gz-compress b bl a 1000)

           (format t "Compressed length: ~d~%" (uffi:deref-pointer bl :long))

           ;; Decompress
           (dotimes (i 1000) (setf (uffi:deref-array a :int i) 0))
           (zlib::%gz-uncompress a al b (uffi:deref-pointer bl :long))
           (format t "Uncompressed length: ~d~%" (uffi:deref-pointer al :long))
           (dotimes (i 1000)
             (setf ua (cons (uffi:deref-array a :int i) ua)))
           (values ca ua))
      (progn
        (uffi:free-foreign-object a)
        (uffi:free-foreign-object al)
        (uffi:free-foreign-object b)
        (uffi:free-foreign-object bl)))))

;;; Tests

(def-suite zlib-suite :description "ZLIB tests.")

(in-suite zlib-suite)

(test write-lines1
  "Test gzip file creation."
  (multiple-value-bind (chars fsize) (write-lines (gen-test-strings))
    (is (= 5301 chars))
    (is (= 447 fsize))))

(test read-lines1
  "Test reading lines from the file."
  :depends-on '(write-lines1)
  (write-lines (gen-test-strings))
  (is (equal (read-lines) (gen-test-strings))))

(test read-lines2
  "Test reading line from the file with various buffer size."
  :depends-on '(read-lines1)
  (let ((lines (gen-test-strings 1)))
    (write-lines lines)
    (dotimes (bufsize (length (car lines)))
      (let ((zlib::*read-buffer-length* (+ (if (> 1 bufsize) 2 1) bufsize)))
        (is (equal (read-lines 1) lines))))))

(test compress-str1
  "Test string compression."
  (is (equal
       (mapcar #'char-code (coerce (compress-str1) 'list))
       '(120 156 75 44 78 73 75 207 0 0 8 131 2 110))))

(test uncompress-str1
  "Test string uncompression with default buffer size."
  (is (string= "qwertyuio"
               (uncompress-str1 (compress-str1 "qwertyuio")))))

(test uncompress-str2
  "Test string uncompression with custom buffer size."
  (is (string= "qwertyuio"
               (uncompress-str1 (compress-str1 "qwertyuio") 4096))))

(test memzip1
  "Test in-memory compression and decompression."
  (multiple-value-bind (compressed uncompressed)
      (memzip)
    (is (equal compressed uncompressed))))

;;;

(defun zlib-test ()
  (run! 'zlib-suite))