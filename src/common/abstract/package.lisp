(in-package #:cl-user)


(defpackage :cl-data-structures.common.abstract
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common.abstract)
  (:export
   #:acquire-ownership
   #:read-ownership-tag
   #:fundamental-ownership-tagged-object
   #:make-tagged-node
   #:make-ownership-tag
   #:tagged-node
   #:tagged-node-lock
   #:tagged-node-ownership-tag))
