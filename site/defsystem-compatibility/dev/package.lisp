(in-package #:common-lisp-user)

(defpackage #:defsystem-compatibility
  (:use #:common-lisp)
  (:nicknames #:dsc)
  (:import-from #:metatilities
                #:pathname-name+type)
  (:export 
   #:available-systems
   #:associated-test-system
   #:collect-system-dependencies
   #:collect-system-files
   #:ensure-system-name
   #:filename-looks-like-system-file-p
   #:find-system
   #:loaded-systems
   #:map-system-files
   #:map-system-dependencies
   #:pathname-for-system-file
   #:registered-systems
   #:system-dependencies
   #:system-loaded-p
   #:system-name-for-display
   #:system-property
   #:system-signature
   #:system-source-directory
   #:system-source-file
   #:system-sub-systems
   #:top-level-system-p
   #:pathname-name+type
   
   #:system-relative-pathname
   
   #:system-definer-not-found-error
   #:system-name))
