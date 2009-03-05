(in-package #:defsystem-compatibility)

(defgeneric available-systems* (system-definer)
  (:documentation "Returns a list of systems that are available for the system-definer."))

(defgeneric associated-test-system* (system-definer system-name)
  (:documentation "Returns the name of the system that should be used to test system-name or nil if no such system exists."))

(defgeneric ensure-system-name* (system-definer system)
  (:documentation "Returns the canonical representation of the system's name."))

(defgeneric filename-looks-like-system-file-p* (system-definer filename)
  (:documentation "Returns true if filename looks like a system file for the system-definer. This is based only on the syntax if the file's name, not on the contents of the file."))

(defgeneric find-system* (system-definer system-name)
  (:documentation "Returns the system definition of system-name for the system-definer or nil if no such system exists."))

(defgeneric loaded-systems* (system-definer)
  (:documentation "Returns a list of the names of the systems that are currently loaded with the system-definer."))

(defgeneric map-system-files (system-name/s function &key 
                                            system-closure? 
                                            include-pathname?
                                            include-non-source?)
  (:documentation "Applies function to each of the files named in system-name/s. If system-closure? is true, then function is applied to the files of the systems that this system depends on. If incude-pathname? is true, the the function is applied to complete pathnames; otherwise the function is applied only to the name+type of each file. If include-non-source? is true, then all files are included. If it is nil \(the default\), then only source files will be included."))

(defgeneric registered-systems* (system-definer)
  (:documentation "Returns a list of every system that has been registered / defined for the system-definer."))

(defgeneric system-dependencies* (system-definer system-name)
  (:documentation "Returns a list of the systems on which system-name depends directly."))

(defgeneric system-loaded-p* (system-definer system-name)
  (:documentation "Returns true if and only if `system-name` has been loaded into this Lisp image."))


(defgeneric system-name-for-display* (system-definer system-name)
  (:documentation "Returns a 'nice name' for system-name. If no such special name is available, then the system-name is used."))

(defgeneric system-property* (system-definer system-name property-name &key no-error?)
  (:documentation "Returns the value of the named property-name of system-name. If no-error? is nil, then an error will be signaled if property-name does not exist in the system."))

(defgeneric system-source-file* (system-definer system-name)
  (:documentation "Returns the pathname of the system definition of system-name for system-definer."))

(defgeneric system-sub-systems* (system-definer system-name)
  (:documentation "Returns the complete list of the systems on which system-name depends; compare with system-dependencies*."))

(defgeneric top-level-system-p* (system-definer system-name)
  (:documentation "Returns true if, metaphorically speaking, system-name is a system unto itself. Usually, this means that there are no systems that depends on the system but it could also be that the system is used by other systems but is also coherent in and of itself."))


