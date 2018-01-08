(in-package #:cl-user)


(defpackage :cl-data-structures.common.abstract
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common.abstract)
  (:export
   #:acquire-ownership
   #:enclose-finalizer
   #:fundamental-ownership-tagged-object
   #:make-ownership-tag
   #:make-tagged-node
   #:read-ownership-tag
   #:reset-ownership-tag
   #:tagged-node
   #:tagged-node-lock
   #:tagged-node-ownership-tag))
