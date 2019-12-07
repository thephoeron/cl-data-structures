(cl:in-package #:cl-user)


(defpackage :cl-data-structures.common.skip-list
  (:use #:common-lisp
        #:cl-data-structures.common.abstract
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.common.skip-list)
  (:export
   #:pointers
   #:size
   #:maximum-level
   #:make-skip-list-node
   #:level
   #:skip-list-node
   #:make-skip-list-node
   #:skip-list-node-pointers
   #:fundamental-skip-list
   #:access-size
   #:read-pointers
   #:skip-list-node-content
   #:skip-list-node-level
   #:skip-list-node-at
   #:locate-node))
