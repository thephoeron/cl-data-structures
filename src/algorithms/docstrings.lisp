(in-package #:cl-data-structures.algorithms)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function group-by
    (:description "Groups RANGE into partitions according to TEST. This does not change content of RANGE, but it will force aggregation to be performed for every group independly.")))
