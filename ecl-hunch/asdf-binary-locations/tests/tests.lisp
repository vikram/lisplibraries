#|

Wow, these tests sure are long winded... (and confusing)

|#

(in-package #:asdf-binary-locations-tests)

(deftestsuite asdf-binary-locations-test () 
  ((op (make-instance 
	'compile-op
	:pathname (system-relative-pathname 'asdf-binary-locations "dev/")))
   (fasl-type (pathname-type (compile-file-pathname ""))))
  (:equality-test #'string=)
  (:dynamic-variables
   (*source-to-target-mappings* nil)
   (*default-toplevel-directory* 
    (system-relative-pathname 'asdf-binary-locations "scratch/"))
   (*centralize-lisp-binaries* nil)
   (*include-per-user-information* nil)
   (*map-all-source-files* nil))
  (:setup 
   (load (system-relative-pathname 'asdf-binary-locations-test 
				   "tests/abl-test-system.asd"))))

(deftestsuite centralize-binaries (asdf-binary-locations-test)
  ())

(addtest (centralize-binaries)
  try-nil
  (let ((c (find-component-using-path :abl-test-system '("dev" "main"))))
    (ensure-same
     (namestring
      (first
       (output-files 
	(make-instance 
	 'compile-op
	 :pathname (system-relative-pathname 'abl-test-system  "dev"))
	c)))
     (namestring
      (system-relative-pathname 
       'abl-test-system 
       (concatenate 'string "/dev/"
		    (implementation-specific-directory-name)
		    "/main." fasl-type))))))

(addtest (centralize-binaries)
  try-true
  (let ((c (find-component-using-path :abl-test-system '("dev" "main")))
	(*centralize-lisp-binaries* t))
    (ensure-same
     (namestring
      (first
       (output-files 
	(make-instance 
	 'compile-op
	 :pathname (system-relative-pathname 'abl-test-system  "dev"))
	c)))
     (namestring
       (concatenate 
	'string
	(namestring *default-toplevel-directory*)
	(implementation-specific-directory-name)
	(namestring (system-relative-pathname 'abl-test-system  "dev"))
	"/main." fasl-type)))))

;;;;

(deftestsuite map-all-source-files (asdf-binary-locations-test)
  ())

(addtest (map-all-source-files)
  try-nil
  (let ((c (find-component-using-path :abl-test-system '("dev" "test-source")))
	(*map-all-source-files* nil))
    (ensure-same
     (namestring
      (first
       (output-files 
	(make-instance 
	 'compile-op
	 :pathname (system-relative-pathname 'abl-test-system  "dev"))
	c)))
     (namestring
      (system-relative-pathname 
       'abl-test-system 
       (concatenate 'string "/dev/" "/test-source.xxx"))))))

(addtest (map-all-source-files)
  try-true
  (let ((c (find-component-using-path :abl-test-system
				      '("dev" "test-source")))
	(*map-all-source-files* t))
    (ensure-same
     (namestring
      (first
       (output-files 
	(make-instance 
	 'compile-op
	 :pathname (system-relative-pathname 'abl-test-system  "dev"))
	c)))
     (namestring
      (system-relative-pathname 
       'abl-test-system 
       (concatenate 'string "/dev/"	
		    (implementation-specific-directory-name)
		    "/test-source.xxx"))))))

;;;;

(defmethod find-component-using-path ((system symbol) path)
  (find-component-using-path (find-system system) path))

(defmethod find-component-using-path ((component component) path)
  (cond ((null path) component)
	(t
	 (let ((next (find (first path)
			   (module-components component)
			   :test #'string-equal :key #'component-name)))
	   (find-component-using-path next (rest path))))))



