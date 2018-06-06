(in-package #:cl-user)


(defpackage :cl-data-structures
  (:use #:common-lisp #:iterate #:alexandria
        #:serapeum #:metabang-bind)
  (:nicknames #:cl-ds)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:across
   #:add
   #:add!
   #:aggregate
   #:apply-layer
   #:argument-out-of-bounds
   #:assert-one-dimension
   #:at
   #:become-functional
   #:become-lazy
   #:become-mutable
   #:become-transactional
   #:clone
   #:consume-back
   #:consume-front
   #:define-validation-for-fields
   #:delay
   #:delayed
   #:dimensionality
   #:dimensionality-error
   #:drop-back
   #:drop-front
   #:empty-clone
   #:empty-container
   #:erase
   #:erase!
   #:erase-if
   #:erase-if!
   #:expression
   #:field
   #:force
   #:found
   #:freeze!
   #:frozenp
   #:functional
   #:functionalp
   #:fundamental-assignable-forward-range
   #:fundamental-assignable-range
   #:fundamental-bidirectional-range
   #:fundamental-container
   #:fundamental-forward-range
   #:fundamental-forward-range
   #:fundamental-modification-operation-status
   #:fundamental-random-access-range
   #:fundamental-range
   #:hash-content
   #:hash-content-hash
   #:hash-content-location
   #:ice-error
   #:initialization-error
   #:initialization-out-of-bounds
   #:insert
   #:invalid-argument
   #:key-value-range
   #:lazy
   #:make-delay
   #:make-from-traversable
   #:make-state
   #:melt!
   #:mod-bind
   #:mutable
   #:mutablep
   #:name
   #:near
   #:not-implemented
   #:operation-not-allowed
   #:out-of-bounds
   #:peek-back
   #:peek-front
   #:put
   #:put!
   #:read-arguments
   #:read-bounds
   #:read-class
   #:read-value
   #:reset!
   #:size
   #:take-out
   #:take-out!
   #:textual-error
   #:too-many-dimensions
   #:transaction
   #:transactional
   #:transactionalp
   #:traversable
   #:traverse
   #:traverse-through
   #:unexpected-argument
   #:update
   #:update!
   #:update-if
   #:update-if!
   #:validate-field
   #:validate-fields
   #:value
   #:whole-range
   #:xpr))


(defpackage :cl-data-structures.meta
  (:use #:common-lisp #:iterate #:alexandria
        #:serapeum #:metabang-bind)
  (:nicknames #:cl-ds.meta)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:add!-function
   #:add-function
   #:destructive-counterpart
   #:erase!-function
   #:erase-function
   #:erase-if!-function
   #:erase-if-function
   #:full-bucket-p
   #:functional-add-function
   #:functional-counterpart
   #:functional-erase-function
   #:functional-erase-if-function
   #:functional-insert-function
   #:functional-put-function
   #:functional-take-out-function
   #:functional-update-function
   #:functional-update-if-function
   #:grow-bucket
   #:grow-function
   #:grow-bucket!
   #:grow-function
   #:insert!-function
   #:insert-function
   #:make-bucket
   #:make-bucket-from-multiple
   #:map-bucket
   #:null-bucket
   #:null-bucket-p
   #:position-modification
   #:put!-function
   #:put-function
   #:shrink-bucket
   #:shrink-bucket!
   #:shrink-function
   #:take-out!-function
   #:take-out-function
   #:update!-function
   #:update-function
   #:update-if!-function
   #:destructive-function
   #:functional-function
   #:update-if-function))
