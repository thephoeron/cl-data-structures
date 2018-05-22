(in-package #:cl-ds.utils)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type half-matrix
    (:description "Matrix container suitable for storing symetric data (like distances). Does not store diagonal values.")))
