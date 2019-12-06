(cl:in-package #:cl-user)


(defpackage :cl-data-structures.common.skip-list
  (:use #:common-lisp
        #:cl-data-structures.common.abstract
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.common.skip-list)
  (:export
   #:skip-list-node
   #:make-skip-list-node
   #:skip-list-node-pointers
   #:skip-list-node-content
   #:locate-node))
