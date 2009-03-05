;; -*- lisp -*-

(in-package :rfc2388.test)

(def-suite :rfc2388)

(in-suite :rfc2388)

(defparameter *test-data-dir*
  (make-pathname :directory
		 (append (pathname-directory
                          #.(asdf:component-pathname (asdf:find-system :rfc2388.test)))
                  '("test" "data"))))

(defun data-file (filename)
  (merge-pathnames filename *test-data-dir*))

(test parse-key-values
  (macrolet ((test-key-values (bind string &body body)
               `(destructuring-bind ,bind
                    (rfc2388::parse-key-values ,string)
                  ,@body)))
    (test-key-values ((foo . bar)) "foo=bar"
      (is (string= "foo" foo))
      (is (string= "bar" bar)))

    (test-key-values ((one . two) (three . four))
                      "one=two;three=four"
      (is (string= "one" one))
      (is (string= "two" two))
      (is (string= "three" three))
      (is (string= "four" four)))

    (test-key-values ((k1 . v1) (k2 . v2) (k3 . v3) (k4 . v4))
                     "1=\"2\";1=2 ;1=2 ; 1=2"
      (is-true (every (lambda (key)
                        (string= "1" key))
                      (list k1 k2 k3 k4)))
      (is-true (every (lambda (value)
                        (string= "2" value))
                      (list v1 v2 v3 v4))))))

(test parse-header-value
  (multiple-value-bind (form-data attributes)
      (rfc2388::parse-header-value "form-data")
    (is (string= "form-data" form-data))
    (is (null attributes)))
  (multiple-value-bind (form-data attributes)
      (rfc2388::parse-header-value "form-data;")
    (is (string= "form-data" form-data))
    (is (null attributes)))
  (multiple-value-bind (form-data attributes)
      (rfc2388::parse-header-value "form-data; a=b")
    (is (string= "form-data" form-data))
    (is (string= "a" (caar attributes)))
    (is (string= "b" (cdar attributes))))
  (multiple-value-bind (form-data attributes)
      (rfc2388::parse-header-value "form-data; a=b ; c=\"d\"")
    (is (string= "form-data" form-data))
    (destructuring-bind ((a . b) (c . d))
        attributes
      (is (string= "a" a))
      (is (string= "b" b))
      (is (string= "c" c))
      (is (string= "d" d)))))

(test as-ascii-char
  (is (char= #\Space (rfc2388::as-ascii-char 32)))
  (is (char= #\Tab (rfc2388::as-ascii-char 9)))
  (is (char= #\! (rfc2388::as-ascii-char 33)))
  (is (char= #\: (rfc2388::as-ascii-char 58)))
  (is (char= #\a (rfc2388::as-ascii-char 97)))
  (is (char= #\A (rfc2388::as-ascii-char 65))))

(test empty-data
  (with-input-from-file (mime (data-file "mime1") :element-type '(unsigned-byte 8))
    (is-true
     (rfc2388::read-until-next-boundary mime
                                        (rfc2388::ascii-string-to-boundary-array "12345678")
                                        (lambda (byte)
                                          (declare (ignore byte))
                                          (fail)))))
  (with-input-from-file (mime (data-file "mime2") :element-type '(unsigned-byte 8))
    (is-false
     (rfc2388::read-until-next-boundary mime
                                        (rfc2388::ascii-string-to-boundary-array "12345678")
                                        (lambda (byte)
                                          (fail "Read char byte ~D (~C), why?" byte (code-char byte)))))))

(test hello-world
  (with-output-to-string (hello-world)
    (with-input-from-file (mime (data-file "mime3") :element-type '(unsigned-byte 8))
      (is-true
       (rfc2388::read-until-next-boundary mime
                                          (rfc2388::ascii-string-to-boundary-array "12345678")
                                          (lambda (byte)
                                            (write-char (code-char byte) hello-world))))
      (is (string= "hello, world!" (get-output-stream-string hello-world)))))
  (with-output-to-string (hello-world)
    (with-input-from-file (mime (data-file "mime4") :element-type '(unsigned-byte 8))
      (is-true
       (rfc2388::read-until-next-boundary mime
                                          (rfc2388::ascii-string-to-boundary-array "12345678")
                                          (lambda (byte)
                                            (declare (ignore byte))
                                            (fail))))
      (is-false
       (rfc2388::read-until-next-boundary mime
                                          (rfc2388::ascii-string-to-boundary-array "12345678")
                                          (lambda (byte)
                                            (write-char (code-char byte) hello-world))))
      (is (string= "
hello, world!" (get-output-stream-string hello-world))))))

(test parse-header
  (with-input-from-file (header (data-file "header1")
                                :element-type '(unsigned-byte 8))
    (multiple-value-bind (found-header header-name header-value)
        (rfc2388::read-next-header header)
      (is-true found-header)
      (is (string= "foo" header-name))
      (is (string= "bar" header-value)))
    (is-false (rfc2388::read-next-header header))))

(defun simple-test-callback (partial-mime-part)
  (setf (content partial-mime-part)
        (make-array 10
                    :element-type '(unsigned-byte 8)
                    :adjustable t
                    :fill-pointer 0))
  (values
   (lambda (byte)
     (vector-push-extend byte (content partial-mime-part)))
   (lambda (mime-part)
     mime-part)))

(defun string-to-vector (string)
  (map 'vector #'char-code string))

(test read-mime
  (with-input-from-file (mime (data-file "mime5") :element-type '(unsigned-byte 8))
    (read-mime mime "--AaB03x" #'simple-test-callback)
    (pass))
  (with-input-from-file (mime (data-file "mime5") :element-type '(unsigned-byte 8))
    (let ((parts (read-mime mime "--AaB03x" #'simple-test-callback)))
      (let ((larry (first parts)))
        (is (equalp (content larry) (string-to-vector "Larry"))))
      (let ((file1 (second parts)))
        (is (equalp (content file1) (string-to-vector "file1.txt")))
        (is (string= "text/plain" (content-type (second parts)))))
      (is (= 2 (length parts))))))

(test read-mime-multipart
  (with-input-from-file (mime (data-file "mime6") :element-type '(unsigned-byte 8))
    (read-mime mime "AaB03x" #'simple-test-callback)
    (pass))
  (with-input-from-file (mime (data-file "mime6") :element-type '(unsigned-byte 8))
    (let ((parts (read-mime mime "AaB03x" #'simple-test-callback)))
      (is (= 3 (length parts)))
      (destructuring-bind (file1 file2 larry)
          parts
        (is (equalp (content larry) (string-to-vector "Larry")))
        (is (string= "form-data" (header-value (get-header larry "Content-Disposition"))))
        (is (equalp (content file1) (string-to-vector "file1.txt")))
        (is (equalp (content file2) (string-to-vector "file2.gif")))))))

(test read-mime-multipart2
  (with-input-from-file (mime (data-file "mime7") :element-type '(unsigned-byte 8))
    (let ((parts (read-mime mime "AaB03x" #'simple-test-callback)))
      (is (= 1 (length parts)))
      (destructuring-bind (files)
          parts
        (is (string= "form-data" (header-value (get-header files "Content-Disposition"))))
        (is (equalp (content files) (string-to-vector "----AaB03")))))))

(test read-binary
  (with-input-from-file (mime (data-file "mime8") :element-type '(unsigned-byte 8))
    (let ((parts (read-mime mime "----------hUrrH2HCA6fHrlQsvCv5qD" #'simple-test-callback)))
      (is (= 4 (length parts)))
      (destructuring-bind (s f a file) parts
 	(is (equalp (string-to-vector "wTWkJQflmGAAAtiuGQjZfdliukKmDMrVxzXziwGq") (content s)))
 	(is (equalp (string-to-vector "NkPeoCRHHdAUgcTAWYkw") (content f)))
 	(is (equalp (string-to-vector "xovkAWwneq") (content a)))	; Won't do harm, might be useful.
	(is (string= "form-data" (header-value (get-header file "Content-Disposition"))))
	(is (string= "application/x-macbinary" (header-value (get-header file "Content-Type"))))
	(is (equalp (content file)
		    (make-array 512 :element-type '(unsigned-byte 8)
				:initial-contents (nconc (loop for x from 0 to 255 collecting x)
							 (loop for x from 255 downto 0 collecting x)))))))))

(test random-junk
  (for-all ((random-byte-buffer (gen-buffer :length (gen-integer :min (expt 2 0) :max (expt 2 4))
                                            :elements (gen-one-element
                                                       (char-code #\-)
                                                       10
                                                       13
                                                       (char-code #\Space)))
                                (not (search "----------hUrrH2HCA6fHrlQsvCv5qD"
                                             random-byte-buffer))))
    (with-output-to-file (mime (data-file "mime9")
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
      (flet ((%line (data)
               (write-sequence (string-to-vector data) mime)
               (write-byte 13 mime)
               (write-byte 10 mime)))
        (%line "------------hUrrH2HCA6fHrlQsvCv5qD")
        (%line "Content-Disposition: form-data; name=\"IujzYaQDEj\"; filename=\"foo.bin\"") ;
        (%line "Content-Type: application/octet-stream")
        (%line "")
        (write-sequence random-byte-buffer mime)
        (write-byte 13 mime)
        (write-byte 10 mime)
        (%line "------------hUrrH2HCA6fHrlQsvCv5qD--")))
    (with-input-from-file (mime (data-file "mime9") :element-type '(unsigned-byte 8))
      (let ((parts (read-mime mime "----------hUrrH2HCA6fHrlQsvCv5qD" #'simple-test-callback)))
        (is (= 1 (length parts)))
        (destructuring-bind (file) parts
          (is (= (length random-byte-buffer) (length (content file)))
              "Wrote ~D bytes, got ~D back." (length random-byte-buffer) (length (content file)))
          (loop
             for index upfrom 0 below (min (length random-byte-buffer)
                                           (length (content file)))
             do (when (/= (aref random-byte-buffer index)
                          (aref (content file) index))
                  (fail 
                   "Bytes at offset ~D differ (length: ~D; on-disk: ~D; returned: ~D)"
                   index
                   (length random-byte-buffer)
                   (aref random-byte-buffer index)
                   (aref (content file) index))))
          (is (string= "form-data" (header-value (get-header file "Content-Disposition"))))
          (is (string= "application/octet-stream" (header-value (get-header file "Content-Type")))))))))

;; Copyright (c) 2003 Janis Dzerins
;; Copyright (c) 2005 Edward Marco Baringer
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
