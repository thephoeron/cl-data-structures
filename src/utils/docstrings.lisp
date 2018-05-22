(in-package #:cl-ds.utils)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type half-matrix
    (:description "Matrix container suitable for storing symetric data (like distances). Does not store diagonal values."))

  (function mref
    (:description "Matrix reference. Accessor for values in matrices."))

  (function lower-bound
    (:description "Find position in the VECTOR of the first element not larger then ELEMENT.")))
