(in-package #:cl-user)


(defpackage :cl-data-structures
  (:use #:common-lisp #:docstample #:docstample.mechanics)
  (:nicknames #:cl-ds)
  (:export
   #:*documentation*
   #:add
   #:add!
   #:add-function
   #:argument-out-of-bounds
   #:at
   #:become-functional
   #:become-lazy
   #:become-mutable
   #:become-transactional
   #:erase
   #:erase!
   #:found
   #:functional
   #:functionalp
   #:fundamental-container
   #:fundamental-modification-operation-status
   #:transform-bucket
   #:position-modification
   #:initialization-error
   #:initialization-out-of-bounds
   #:insert
   #:insert-function
   #:invalid-argument
   #:lazy
   #:make-bucket
   #:mod-bind
   #:mutable
   #:mutablep
   #:not-implemented
   #:out-of-bounds
   #:read-arguments
   #:read-bounds
   #:read-class
   #:read-value
   #:size
   #:textual-error
   #:transaction
   #:transactional
   #:transactionalp
   #:update
   #:update!
   #:update-function
   #:value))


(in-package #:cl-ds)
(defparameter *documentation* (docstample:make-accumulator))
