(in-package #:cl-user)


(defpackage :cl-data-structures.queues
  (:use #:cl
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.queues)
  (:export #:fundamental-queue
           #:fundamental-transactional-queue
           #:fundamental-mutable-queue
           #:fundamental-functional-queue
           #:access-size))


(defpackage :cl-data-structures.queues.2-3-tree
  (:use #:cl
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.queues.2-3-tree)
  (:export #:make-mutable-2-3-queue
           #:mutable-2-3-queue))
