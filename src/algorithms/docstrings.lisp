(in-package #:cl-data-structures.algorithms)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type aggregation-function
    (:description "Base class of all aggregation functions. Aggregation functions work by applying destructive change to the state for each element in range."))

  (function accumulate
    (:description "Like CL:REDUCE but works on all traversable objects."))

  (function count
    (:description "Counts number of elements. Usefull mostly in conjuction with GROUP-BY."
     :see-also (group-by)))

  (function without
    (:description "Layer function. Creates range that skips elements that return T when passed to the PREDICATE function through key function."
     :arguments ((predicate "Test used to check if element should be skipped.")
                 (key "Key function used to extract value for predicate.")
                 (range "Range argument."))))

  (function group-by
    (:description "Groups RANGE into partitions according to the TEST. This does not change content of RANGE, but it will force aggregation to be performed on every group independently."
     :arguments ((range "Range that is supposed to be groupped.")
                 (key "Key function, used to extract value for TEST")
                 (test "Test for inner hashtable (either eq, eql or equal).")))))
