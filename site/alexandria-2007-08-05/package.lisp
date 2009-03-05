(defpackage :alexandria.0.dev
  (:nicknames :alexandria)
  (:use :cl)
  (:export 
   ;; Binding constructs
   #:if-let
   #:if-let*
   #:when-let
   #:when-let*
   ;; Definitions
   #:define-constant
   ;; Control flow
   #:cswitch
   #:eswitch
   #:switch
   #:whichever
   #:xor
   ;; Hash tables
   #:alist-hash-table
   #:copy-hash-table
   #:hash-table-alist
   #:hash-table-keys
   #:hash-table-plist
   #:hash-table-values
   #:maphash-keys
   #:maphash-values
   #:plist-hash-table
   ;; Functions
   #:compose
   #:conjoin
   #:curry
   #:disjoin
   #:ensure-function
   #:multiple-value-compose
   #:named-lambda
   #:rcurry
   ;; Lists
   #:alist-plist
   #:appendf
   #:circular-list
   #:circular-list-p
   #:circular-tree-p
   #:ensure-car
   #:ensure-cons
   #:ensure-list
   #:flatten
   #:lastcar
   #:make-circular-list
   #:map-product
   #:mappend
   #:nunionf
   #:plist-alist
   #:proper-list
   #:proper-list-p
   #:remove-from-plist
   #:remove-from-plistf
   #:delete-from-plist
   #:delete-from-plistf
   #:set-equal
   #:setp
   #:unionf
   ;; Numbers
   #:binomial-coefficient
   #:clamp
   #:count-permutations
   #:factorial
   #:gaussian-random
   #:iota   
   #:lerp
   #:map-iota
   #:maxf   
   #:mean   
   #:median
   #:minf
   #:standard-deviation
   #:subfactorial
   #:variance
   ;; Arrays
   #:array-index
   #:array-length
   #:copy-array
   ;; Sequences
   #:copy-sequence
   #:deletef
   #:emptyp
   #:ends-with
   #:ends-with-subseq
   #:first-elt
   #:last-elt
   #:map-combinations
   #:map-derangements
   #:map-permuations
   #:proper-sequence
   #:random-elt
   #:removef
   #:rotate
   #:sequence-of-length-p
   #:shuffle
   #:starts-with
   #:starts-with-subseq
   ;; Macros
   #:once-only
   #:parse-body
   #:with-gensyms
   #:with-unique-names
   ;; Symbols
   #:ensure-symbol
   #:format-symbol
   #:make-gensym-list
   #:make-keyword
   ;; Strings
   #:string-designator
   ;; Types
   #:of-type
   #:type=
   ;; Errors
   #:required-argument
   ))
