(in-package #:cl-user)


(defpackage :cl-data-structures
  (:use #:common-lisp #:docstample #:docstample.mechanics)
  (:nicknames #:cl-ds)
  (:export
   ;; generic functions
   #:at
   #:erase
   #:erase!
   #:add
   #:add!
   #:value
   #:found
   #:insert
   #:emptyp
   #:size
   #:update
   #:update!
   #:become-functional
   #:become-mutable
   #:become-transactional
   #:become-lazy
   #:mutablep
   #:functionalp
   #:transactionalp
   ;; trait classes
   #:fundamental-container
   #:fundamental-modification-operation-status
   #:functional
   #:transactional
   #:lazy
   #:mutable

   #:data-structure-condition
   #:argument-out-of-bounds
   #:not-implemented

   #:mod-bind
   #:transaction
   #:*documentation*))

(in-package #:cl-ds)
(defvar *documentation* nil)
