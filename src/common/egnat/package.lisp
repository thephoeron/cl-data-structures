(in-package #:cl-user)


(defpackage :cl-data-structures.common.egnat
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind
        #:cl-data-structures.common.abstract)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common.egnat)
  (:export
   #:access-root
   #:access-size
   #:distance
   #:fundamental-egnat-container
   #:get-value
   #:make-egnat-tree
   #:mutable-egnat-container
   #:prune-subtrees
   #:read-margin
   #:read-near
   #:same
   #:select-children
   #:traverse-impl))
