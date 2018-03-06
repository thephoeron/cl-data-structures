(in-package #:cl-user)


(defpackage :cl-data-structures.common.egnat
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind
        #:cl-data-structures.common.abstract)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common.egnat)
  (:export
   #:distance
   #:fundamental-egnat-container
   #:get-value
   #:prune-subtrees
   #:read-margin
   #:read-near
   #:select-children
   #:traverse-impl))
