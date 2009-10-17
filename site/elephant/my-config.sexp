
;; OSX Defaults 
#+(and (or sbcl allegro openmcl lispworks) (not (or mswindows windows)) (or macosx darwin))
((:compiler . :gcc)
 (:berkeley-db-version . "4.5")
 (:berkeley-db-include-dir . "/usr/local/BerkeleyDB.4.5/include/")
 (:berkeley-db-lib-dir . "/usr/local/BerkeleyDB.4.5/lib/")
 (:berkeley-db-lib . "/usr/local/BerkeleyDB.4.5/lib/libdb-4.5.dylib")
 (:berkeley-db-deadlock . "/usr/local/BerkeleyDB.4.5/bin/db_deadlock")
 (:berkeley-db-cachesize . 20971520)
 (:berkeley-db-map-degree2 . t)
 (:clsql-lib-paths . nil)
 (:prebuilt-libraries . nil))

