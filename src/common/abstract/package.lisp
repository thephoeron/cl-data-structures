(cl:in-package #:cl-user)


(defpackage :cl-data-structures.common.abstract
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.common.abstract)
  (:export
   #:acquire-ownership
   #:enclose-finalizer
   #:fundamental-ownership-tagged-object
   #:make-ownership-tag
   #:make-tagged-node
   #:define-tagged-untagged-node
   #:tagged-struct-node
   #:read-ownership-tag
   #:tagged-node
   #:replica
   #:write-ownership-tag))
