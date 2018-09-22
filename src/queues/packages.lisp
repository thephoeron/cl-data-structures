(in-package #:cl-user)


(defpackage :cl-data-structures.queues
  (:use #:cl
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.queues)
  (:export #:fundamental-queue
           #:fundamental-mutable-queue
           #:fundamental-functional-queue
           #:access-size))


(defpackage :cl-data-structures.queues.2-3-tree
  (:use #:cl
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.queues.2-3-tree))
