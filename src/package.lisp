(in-package #:cl-user)


(defpackage :cl-data-structures
  (:use #:common-lisp #:docstample #:docstample.mechanics)
  (:nicknames #:cl-ds)
  (:export
   #:*documentation*
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
   #:destructive-function
   #:erase
   #:erase!
   #:erase!-function
   #:erase-function
   #:erase-if
   #:erase-if!
   #:erase-if!-function
   #:erase-if-function
   #:found
   #:functional
   #:functional-add-function
   #:functional-erase-function
   #:functional-erase-if-function
   #:functional-function
   #:functional-insert-function
   #:functional-update-function
   #:functionalp
   #:fundamental-container
   #:fundamental-modification-operation-status
   #:grow-bucket
   #:grow-bucket!
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
   #:shrink-bucket
   #:shrink-bucket!
   #:shrink-function
   #:size
   #:textual-error
   #:transaction
   #:transactional
   #:transactionalp
   #:update
   #:update!
   #:update!-function
   #:update-function
   #:value))


(in-package #:cl-ds)
(docstample:define-accumulated-docs *documentation*)
