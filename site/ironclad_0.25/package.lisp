(cl:defpackage :ironclad
  (:use :cl)
  (:nicknames :crypto)
  (:shadow null)
  (:export
   ;; referencing multiple-octet values in an octet vector (SETF-able)
   #:ub16ref/be #:ub16ref/le #:ub32ref/be #:ub32ref/le #:ub64ref/le #:ub64ref/be

   ;; hash functions
   #:digest-sequence #:digest-stream #:digest-file
   #:make-digest #:copy-digest #:update-digest #:produce-digest

   ;; HMACs
   #:make-hmac #:update-hmac #:hmac-digest
   ;; CMACs
   #:make-cmac #:update-cmac #:cmac-digest

   ;; introspection
   #:cipher-supported-p #:list-all-ciphers
   #:digest-supported-p #:list-all-digests
   #:mode-supported-p #:list-all-modes
   #:block-length #:digest-length #:key-lengths

   ;; high-level block cipher operators
   #:make-cipher #:encrypt #:decrypt #:encrypt-in-place #:decrypt-in-place

   ;; arguments to (MAKE-CIPHER ... :MODE X)
   #:ecb #:cbc #:ctr #:ofb #:cfb #:stream

   ;; public-key encryption operations
   #:make-public-key #:make-private-key
   #:sign-message #:verify-signature
   #:encrypt-message #:decrypt-message

   ;; signatures
   #:make-dsa-signature

   ;; public-key slot readers
   #:dsa-key-p #:dsa-key-q #:dsa-key-g #:dsa-key-y #:dsa-key-x
   #:dsa-signature-r #:dsa-signature-s

   ;; conditions
   #:ironclad-error #:initialization-vector-not-supplied
   #:invalid-initialization-vector #:invalid-key-length
   #:unsupported-cipher #:unsupported-mode #:unsupported-digest
   #:insufficient-buffer-space #:invalid-padding
   #:key-not-supplied

   ;; utilities
   #:ascii-string-to-byte-array #:byte-array-to-hex-string
   #:octets-to-integer #:integer-to-octets #:expt-mod

   ;; streams
   #:make-octet-input-stream #:make-octet-output-stream
   #:get-output-stream-octets

   #:make-digesting-stream))
