(in-package #:cl-user)


(defpackage :cl-data-structures
  (:use #:common-lisp #:docstample #:docstample.mechanics)
  (:nicknames #:cl-ds)
  (:export
   #:add
   #:add!
   #:add!-function
   #:add-function
   #:argument-out-of-bounds
   #:at
   #:become-functional
   #:become-lazy
   #:become-mutable
   #:become-transactional
   #:destructive-function
   #:erase
   #:erase!
   #:erase!-function
   #:found
   #:functional
   #:functionalp
   #:fundamental-container
   #:fundamental-modification-operation-status
   #:grow-bucket
   #:grow-function
   #:initialization-error
   #:initialization-out-of-bounds
   #:insert
   #:insert!-function
   #:insert-function
   #:invalid-argument
   #:lazy
   #:make-bucket
   #:mod-bind
   #:mutable
   #:mutablep
   #:not-implemented
   #:out-of-bounds
   #:position-modification
   #:read-arguments
   #:read-bounds
   #:read-class
   #:read-value
   #:shrink-bucke
   #:size
   #:textual-error
   #:transaction
   #:transactional
   #:transactionalp
   #:update
   #:update!
   #:update!-function
   #:update-function
   #:value
   #:*documentation*))


(in-package #:cl-ds)
(defparameter *documentation* (docstample:make-accumulator))
