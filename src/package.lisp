(in-package #:cl-user)


(defpackage :cl-data-structures
  (:use #:common-lisp)
  (:nicknames #:cl-ds)
  (:export
   ;; generic functions
   #:at
   #:erase
   #:erase!
   #:add
   #:add!
   #:insert
   #:emptyp
   #:size
   #:update
   #:update!
   #:become-functional
   #:become-mutable
   #:become-transactional
   #:mutablep
   #:functionalp
   #:transactionalp
   ;; trait classes
   #:fundamental-container
   #:functional
   #:transactional
   #:mutable

   #:data-structure-condition
   #:argument-out-of-bounds))
