(in-package :crypto-tests)

(rtest:deftest verify-key.bad-cipher
  (handler-case (crypto::verify-key :error
                                    (make-array 0
                                                :element-type '(unsigned-byte 8)))
    (crypto:unsupported-cipher () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest verify-key.bad-key0
  (handler-case (crypto::verify-key :aes "")
    (type-error () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest verify-key.bad-key1
  (handler-case (crypto::verify-key :aes nil)
    (crypto:key-not-supplied () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest unprovided-key
  (handler-case
      (crypto:make-cipher :blowfish :mode :ecb
                          :initialization-vector (make-array 8 :element-type '(unsigned-byte 8)))
    (crypto:key-not-supplied () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest block-length.known-ciphers
  (dolist (name (crypto:list-all-ciphers) :ok)
    (unless (crypto:block-length name)
      (return :error)))
  :ok)

(rtest:deftest block-length.bad-cipher
  (crypto:block-length :error)
  nil)

(rtest:deftest key-lengths.known-ciphers
  (dolist (name (crypto:list-all-ciphers) :ok)
    (unless (crypto:key-lengths name)
      (return :error)))
  :ok)

(rtest:deftest key-lengths.bad-cipher
  (crypto:key-lengths :error)
  nil)

#.(loop for cipher in (crypto:list-all-ciphers)
        collect `(rtest:deftest ,cipher
                   (run-test-vector-file ',cipher *cipher-tests*) t) into forms
        finally (return `(progn ,@forms)))

(rtest:deftest clean-symbols.ciphers
    (loop with n-ciphers = (length (crypto:list-all-ciphers))
     for s being each symbol of :crypto
     when (crypto::%find-cipher s)
     count s into computed-n-ciphers
     finally (return (= n-ciphers computed-n-ciphers)))
  t)
