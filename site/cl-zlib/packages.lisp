(cl:defpackage #:org.chip.aar.zlib
  (:nicknames #:zlib)
  (:use #:cl #:uffi)
  (:export #:zcompress
           #:zuncompress
	   #:compress-string
           #:uncompress-string
	   #:gz-open
           #:gz-close
           #:gz-read
           #:gz-write
	   #:gz-read-line
           #:gz-write-line
	   #:with-gz
           #+allegro
           #:man
           ))

(cl:defpackage #:org.chip.aar.zlib-test
  (:nicknames #:zlib-test)
  (:use #:cl #:zlib #:it.bese.fiveam)
  (:export #:zlib-test))

