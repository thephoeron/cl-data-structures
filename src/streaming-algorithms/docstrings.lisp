(in-package #:cl-data-structures.streaming-algorithms)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function estimated-set-cardinality
    (:description "Calculates estimated set cardinality using HyperLogLog algorithm. This requires only a constant ammount of memory.")))
