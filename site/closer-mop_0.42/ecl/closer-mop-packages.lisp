(in-package :cl-user)

(defpackage #:closer-mop
  (:use #:common-lisp #:lispworks)
  (:nicknames #:c2mop)

  (:shadow #:defgeneric #:ensure-generic-function #:find-method
   #:remove-method #:standard-generic-function)
  (:export #:defgeneric #:ensure-generic-function #:find-method
   #:remove-method #:standard-generic-function)

  (:import-from #:clos

   #:direct-slot-definition
   #:effective-slot-definition
   #-ecl #:eql-specializer
   #:forward-referenced-class
   #-ecl #:funcallable-standard-class
   #-ecl #:funcallable-standard-object
   #-ecl #:metaobject
   #:slot-definition
   #-ecl #:specializer
   #-ecl #:standard-accessor-method
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #-ecl #:standard-reader-method
   #:standard-slot-definition
   #-ecl #:standard-writer-method

   #-ecl #:accessor-method-slot-definition
   #-ecl #:add-dependent
   #-ecl #:add-direct-method
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
   #-ecl #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #:compute-default-initargs
   #:compute-discriminating-function
   #:compute-effective-method
   #:compute-effective-slot-definition
   #:compute-slots
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:ensure-class
   #:ensure-class-using-class
   #:ensure-generic-function-using-class
   #-ecl #:eql-specializer-object
   #-ecl #:extract-lambda-list
   #-ecl #:extract-specializer-names
   #:finalize-inheritance
   #-ecl #:find-method-combination
   #:funcallable-standard-instance-access
   #:generic-function-argument-precedence-order
   #-ecl #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #-ecl #:intern-eql-specializer
   #-ecl #:make-method-lambda
   #-ecl #:map-dependents
   #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #-ecl #:reader-method-class
   #-ecl #:remove-dependent
   #-ecl #:remove-direct-method
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
   #-ecl #:specializer-direct-generic-functions
   #-ecl #:specializer-direct-methods
   #:standard-instance-access
   #-ecl #:update-dependent
   #-ecl #:validate-superclass
   #-ecl #:writer-method-class)

  (:export
   #:direct-slot-definition
   #:effective-slot-definition
   #:eql-specializer
   #:forward-referenced-class
   #-ecl #:funcallable-standard-class
   #-ecl #:funcallable-standard-object
   #-ecl #:metaobject
   #:slot-definition
   #-ecl #:specializer
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

   #:accessor-method-slot-definition
   #-ecl #:add-dependent
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
   #-ecl #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #:compute-default-initargs
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
   #:eql-specializer-object*
   #:extract-lambda-list
   #:extract-specializer-names
   #:finalize-inheritance
   #-ecl #:find-method-combination
   #:funcallable-standard-instance-access
   #:generic-function-argument-precedence-order
   #-ecl #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #:intern-eql-specializer
   #:intern-eql-specializer*
   #-ecl #:make-method-lambda
   #-ecl #:map-dependents
   #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #:reader-method-class
   #-ecl #:remove-dependent
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
   #:standard-instance-access
   #-ecl #:update-dependent
   #-ecl #:validate-superclass
   #:writer-method-class))
