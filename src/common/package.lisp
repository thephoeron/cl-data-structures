(in-package #:cl-user)


(defpackage :cl-data-structures.common
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common)
  (:export
   #:access-content
   #:assignable-forward-tree-range
   #:assignable-tree-range
   #:close-queue
   #:defmethod-with-peek-stack
   #:defmethod-with-stack
   #:dict-content
   #:dict-content-location
   #:dict-content-value
   #:eager-modification-operation-status
   #:empty-eager-modification-operation-status
   #:force-version
   #:forward-lazy-range
   #:forward-tree-range
   #:hash-content-hash
   #:hash-content-location
   #:hash-dict-content-value
   #:lazy-bidirectional-range
   #:lazy-box-container
   #:lazy-random-access-range
   #:make-dict-content
   #:make-eager-modification-operation-status
   #:make-hash-content
   #:make-hash-dict-content
   #:make-lazy-range
   #:read-found
   #:read-store-value
   #:read-value
   #:single-element-p))
