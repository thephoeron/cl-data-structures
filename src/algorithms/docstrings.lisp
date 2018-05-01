(in-package #:cl-data-structures.algorithms)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function accumulate
    (:description "Like CL:REDUCE but works on all traversable containers."))

  (function count
    (:description "Counts number of elements. Usefull mostly in conjuction with GROUP-BY."
     :see-also (group-by)))

  (function group-by
    (:description "Groups RANGE into partitions according to the TEST. This does not change content of RANGE, but it will force aggregation to be performed on every group independently."
     :arguments ((range "Range that is supposed to be groupped.")
                 (key "Key function, used to extract value for TEST")
                 (test "Test for inner hashtable (either eq, eql or equal).")))))
