(in-package #:cl-data-structures.algorithms)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function accumulate
    (:description "Like CL:REDUCE but works on all traversable objects."))

  (function hash-join
    (:description "Joins multiple ranges into one using JOIN-FUNCTION."))

  (function to-vector
    (:description "Collects all elements into cl:vector."
     :arguments-and-values ((range "Object to aggregate accross.")
                            (key "Key function used to extract value for vector.")
                            (element-type ":element-type for result vector."))))

  (function on-each
    (:description "Creates new range by applying FUNCTION to each element of the RANGE.")
    (:notes "Works almost like cl:map-and-friends, but it is lazy evaluated. FUNCTION is called only when required."))

  (function count
    (:description "Counts number of elements. Usefull mostly in conjuction with GROUP-BY."
     :see-also (group-by)))

  (function chain
    (:description "Joins multiple ranges sequentially into one."))

  (function summary
    (:description "Summary is a function that allows to perform multiple aggregations in one form."
     :arguments ((range "Range to aggregate.")
                 (forms "Lists describing way to invoke function. First element of list is label used to identify value in the result range, second is aggregation function designator, the rest is list of arguments that should be passed to the function, with range being replaced by the keyword :range."))
     :returns "Range of results. Use cl-ds:at with label to extract result of each individual aggregation form."))

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
