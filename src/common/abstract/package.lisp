(in-package #:cl-user)


(defpackage :cl-data-structures.common.abstract
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in #:into #:sum)
  (:nicknames #:cl-ds.common.abstract)
  (:export
   #:acquire-ownership
   #:enclose-finalizer
   #:fundamental-ownership-tagged-object
   #:make-ownership-tag
   #:make-tagged-node
   #:read-ownership-tag
   #:tagged-node
   #:write-ownership-tag))
