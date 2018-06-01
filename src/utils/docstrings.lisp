(in-package #:cl-ds.utils)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type half-matrix
    (:description "Matrix container suitable for storing symetric data (like distances). Does not store diagonal values."))

  (function mref
    (:description "Matrix reference. Accessor for values in matrices."))

  (function if-else
    (:description "Construct function out of PREDICATE function, TRUE function and FALSE function. Checks if PREDICATE returns true, if yes: pass arguments to the TRUE function, if no: pass arguments to the FALSE function."
     :returns "Function."))

  (function lower-bound
    (:description "Find position in the VECTOR of the first element not larger then ELEMENT.")))
