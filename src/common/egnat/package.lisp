(in-package #:cl-user)


(defpackage :cl-data-structures.common.egnat
  (:use #:common-lisp #:cl-data-structures.aux-package)
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
