(in-package :cl-user)

(defpackage #:closer-mop
  (:use #:common-lisp #:lispworks)
  (:nicknames #:c2mop)

  (:shadow
   #:defclass #:defgeneric #:defmethod
   #:ensure-generic-function
   #:standard-class #:standard-generic-function)
  (:export
   #:defclass #:defgeneric #:defmethod
   #:ensure-generic-function
   #:standard-class #:standard-generic-function)

  (:import-from #:clos

   #:direct-slot-definition
   #:effective-slot-definition
   #-lispworks #:eql-specializer
   #:forward-referenced-class
   #-lispworks #:funcallable-standard-class
   #+lispworks5.0 #:funcallable-standard-object
   #:metaobject
   #:slot-definition
   #-lispworks #:specializer
   #:standard-accessor-method
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-reader-method
   #:standard-slot-definition
   #:standard-writer-method

   #-lispworks4.3 #:accessor-method-slot-definition
   #:add-dependent
   #:add-direct-method
   #:add-direct-subclass
   #:class-default-initargs
   #:class-direct-default-initargs
   #:class-direct-slots
   #:class-direct-subclasses
   #:class-direct-superclasses
   #:class-finalized-p
   #:class-precedence-list
   #:class-prototype
   #:class-slots
   #-lispworks #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #-lispworks #:compute-default-initargs
   #:compute-discriminating-function
   #:compute-effective-method
   #:compute-effective-slot-definition
   #:compute-slots
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:ensure-class
   #:ensure-class-using-class
   #:ensure-generic-function-using-class
   #-lispworks #:eql-specializer-object
   #:extract-lambda-list
   #:extract-specializer-names
   #:finalize-inheritance
   #-lispworks #:find-method-combination
   #-lispworks #:funcallable-standard-instance-access
   #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #-lispworks #:intern-eql-specializer
   #-lispworks #:make-method-lambda
   #:map-dependents
   #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #-lispworks4.3 #:reader-method-class
   #:remove-dependent
   #:remove-direct-method
   #:remove-direct-subclass
   #:set-funcallable-instance-function
   #:slot-boundp-using-class
   #:slot-definition-allocation
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-location
   #:slot-definition-name
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-type
   #:slot-makunbound-using-class
   #:slot-value-using-class
   #-lispworks #:specializer-direct-generic-functions
   #:specializer-direct-methods
   #-lispworks #:standard-instance-access
   #:update-dependent
   #:validate-superclass
   #-lispworks4.3 #:writer-method-class)

  (:export
   #:direct-slot-definition
   #:effective-slot-definition
   #:eql-specializer
   #+lispworks #:eql-specializer*
   #:forward-referenced-class
   #:funcallable-standard-class
   #:funcallable-standard-object
   #:metaobject
   #:slot-definition
   #-lispworks #:specializer
   #:standard-accessor-method
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-reader-method
   #:standard-slot-definition
   #:standard-writer-method

   #:ensure-finalized
   #:ensure-method
   #:fix-slot-initargs
   #:required-args

   #-lispworks4.3 #:accessor-method-slot-definition
   #:add-dependent
   #:add-direct-method
   #:add-direct-subclass
   #:class-default-initargs
   #:class-direct-default-initargs
   #:class-direct-slots
   #:class-direct-subclasses
   #:class-direct-superclasses
   #:class-finalized-p
   #:class-precedence-list
   #:class-prototype
   #:class-slots
   #-lispworks #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #-lispworks #:compute-default-initargs
   #:compute-discriminating-function
   #:compute-effective-method
   #:compute-effective-slot-definition
   #:compute-slots
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:ensure-class
   #:ensure-class-using-class
   #:ensure-generic-function-using-class
   #:eql-specializer-object
   #:extract-lambda-list
   #:extract-specializer-names
   #:finalize-inheritance
   #:find-method-combination
   #-lispworks #:funcallable-standard-instance-access
   #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #:intern-eql-specializer
   #+lispworks #:intern-eql-specializer*
   #:make-method-lambda
   #:map-dependents
   #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #-lispworks4.3 #:reader-method-class
   #:remove-dependent
   #:remove-direct-method
   #:remove-direct-subclass
   #:set-funcallable-instance-function
   #:slot-boundp-using-class
   #:slot-definition-allocation
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-location
   #:slot-definition-name
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-type
   #:slot-makunbound-using-class
   #:slot-value-using-class
   #:specializer-direct-generic-functions
   #:specializer-direct-methods
   #-lispworks #:standard-instance-access
   #:update-dependent
   #:validate-superclass
   #-lispworks4.3 #:writer-method-class))
