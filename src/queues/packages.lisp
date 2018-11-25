(in-package #:cl-user)


(defpackage :cl-data-structures.queues
  (:use #:cl
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.queues)
  (:export
   #:access-size
   #:fundamental-functional-queue
   #:fundamental-mutable-queue
   #:fundamental-transactional-queue
   #:fundamental-queue))

(defpackage :cl-data-structures.queues.2-3-tree
  (:use #:cl
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.queues.2-3-tree)
  (:export
   #:functional-2-3-queue
   #:make-functional-2-3-queue
   #:make-mutable-2-3-queue
   #:make-transactional-2-3-queue
   #:mutable-2-3-queue
   #:transactional-2-3-queue))
