(in-package #:common-lisp-user)

(defpackage #:metabang.utilities
  (:nicknames #:metatilities)
  (:use #:common-lisp)

  ;; just a little bit of mop
  (:import-from #+allegro          #:mop
		#+clisp            #:clos
		#+ecl              #:clos
		#+lispworks        #:clos
		#+(or mcl openmcl) #:ccl
		#+cmu              #:clos-mop
		#+sbcl             #:sb-mop
		#:class-direct-subclasses
		#:class-precedence-list
		#:class-finalized-p
		#:finalize-inheritance)
  (:export 
   #:class-direct-subclasses
   #:class-precedence-list
   #:class-finalized-p
   #:finalize-inheritance)
   
  #+asdf
  (:import-from #:asdf
                #:load-op #:compile-op #:test-op 
                #:operate #:oos)
  #+asdf
  (:export
   #:load-op #:compile-op #:test-op 
   #:operate #:oos)
  
  #+(or openmcl digitool)     ; ??? I think this is the right thing...
  (:shadow #:line #:copy-file
           #:lock-owner #:lock-name
           #:selected? #:whitespacep)
  
  (:intern
   #:depth)

  (:export
   #:copy-file)

  (:export 
   #:neq)
  
  #-(or allegro clisp)
  (:import-from #+lispworks #:mp
                #+(or openmcl digitool)       #:ccl
                #+cmu       #:system
                #+sbcl      #:sb-sys

                #:without-interrupts)
  
  ;; these are stand-ins for those that will come from cl-containers
  (:export
   #:element
   #:element-type
   #:filter
   #:parent
   #:tag
   #:size
   #:root
   #:next-element
   #:total-size
   #:argmax
   #:argmin
   #:best-item
   #:filter
   )

  (:export
   #:apply-if-exists
   #:funcall-if-exists

   #:defclass-property
   #:deprecated
   #:once-only
   #:with-variables
   #:eval-always
   #:with-atomic-execution
   #:handler-bind*

   #:file-newer-than-file-p
   #:pathname-without-name+type
   #:with-stream-from-specifier
   #:relative-pathname
   #:directory-pathname-p
   #:ensure-directory-pathname
   #:with-input
   #:with-output
   #:map-lines
   #:map-forms
   #:collect-lines
   #:collect-forms
   #:map-matching-files
   #:collect-matching-files

   #:defclass-brief 
   #:defclass*
   #:defcondition

   #:copy-array
   #:linearize-array
   #:maparray! 
   #:maparray
   #:array-row
  
   #:without-interrupts
  
   #:linearize-array
   #:copy-array
   #:maparray
   
   #:fixnump

   #:set-equal
   
   #:constant-expression-p 
  
   #:muffle-redefinition-warnings

   #:nyi
   #:deprecated
   #:with-unique-names

   

   #:defun*
   #:defmethod*
   #:*add-check-types*
   #:*optimizations-to-ignore*
   
   #:delegates-to
   
   #:funcall-if
   
   #:*file-if-exists*
   #:with-new-file
  
   #:with-gensyms
   
   #:invalid-stream-specifier-error

   #:pathname-samep
   #:physical-pathname-directory-separator

   #:map-lines
   #:day->string 
   
   #:month->string

   #:+minutes-per-hour+
   #:+seconds-per-hour+
   #:+seconds-per-minute+
   #:+usual-days-per-year+
   #:+hours-per-day+
   #:+seconds-per-day+
   
   #:format-date
   #:days-in-month
   #:day-of-year
   #:leap-year-p
 
   #:dotted-pair-p
   #:iterate-over-indexes
   
   #:form-symbol-in-package
   #:form-symbol
   #:form-keyword
   #:form-uninterned-symbol
   
   #:current-load-file
   #:with-unique-names
   
   #:ensure-list 
   #:newsym
   #:export-exported-symbols
   
   #:length-at-most-p
   #:length-at-least-p
   #:length-1-list-p
   
   #:nearly-zero-p
   #:nearly-equal-p
   
   #:samep
   #:nearly-samep
   #:*samep-test*
   
   #:set-equal
   
   #:is-interface-available-p
   #:is-default-interface-available-p
   #:default-interface
   
   #:quit-lisp
   #:quit-lisp*
   
   #:inspect-thing*
   #:inspect-thing
   #:inspect-things
   
   #:total-bytes-allocated*
   #:total-bytes-allocated

   #:gc-time

   #:collect-garbage*
   #:collect-garbage

   #:make-load-form*

   #:*development-mode* 
   #:*use-native-debugger*

   #:with-progress-bar 
   #:progress
   #:make-progress-bar
   #:progress-bar-value
   #:close-progress-bar
   #:with-process-message
   #:handle-errors
   #:gui-error
   #:gui-warn
   #:interface-beep
   #:sound-note
   #:stop-notes
   #:select-instrument
   #:make-color*
   #:make-scaled-color
   #:make-gray
   #:y-or-n-question
   #:choose-file-question
   #:choose-new-file-question
   #:choose-directory-question
   #:choose-item-question
   #:query-user-for-string
   #:query-user-for-integer
   #:choose-item-from-pup
   #:choose-item-from-pup-no-singletons
   #:make-ui-point*
   #:process-parameters
   #:put-item-on-clipboard
   #:inspect-things
   #:prompt-for
   
   #:parse-brief-slot
   #:class-copyable-p
  

   #:+whitespace-characters+
   #:whitespacep
   #:string-starts-with
   #:string-ends-with
   #:string-trim-if
   #:strip-whitespace
   ))
