(cl:eval-when (:load-toplevel :compile-toplevel :execute)
  (loop for dir in (directory "/prj/encore/packages/netstatus/addons/*/*.asd")
	do 
	(pushnew (make-pathname :directory (pathname-directory dir)) asdf:*central-registry* :test 'equal))
  (pushnew "/prj/encore/packages/anardb/" asdf:*central-registry* :test 'equal))

(asdf:operate 'asdf:load-op 'anardb-test)

(setf *random-state* (make-random-state t (+ (get-internal-real-time) (get-universal-time))))

