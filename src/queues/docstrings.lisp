(in-package #:cl-data-structures.queues)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (class fundamental-queue
    (:description "Fundamental base class of all queues."))

  (class fundamental-mutable-queue
    (:description "Fundamental base class of all mutable queues."))

  (class fundamental-functional-queue
    (:description "Fundamental base class of all functional queues.")))
