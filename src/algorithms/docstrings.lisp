(in-package #:cl-data-structures.algorithms)
(eval-always
  (scribble:configure-scribble :package :cl-data-structures.algorithms)
  (named-readtables:in-readtable :scribble))


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function accumulate
    (:description "Like CL:REDUCE but works on all traversable objects."))

  (function hash-join
    (:description "Joins multiple ranges into one using JOIN-FUNCTION."))

  (function split-into-chunks
    (:description "Divides aggregation process into partitions upto size."
     :examples [(let ((data (cl-ds.alg:to-vector (cl-ds.alg:split-into-chunks #(1 2 3 4 5 6) 2))))
                  (prove:is (cl-ds:size data) 3)
                  (prove:is (cl-ds:at data 0) #(1 2) :test 'equalp)
                  (prove:is (cl-ds:at data 1) #(3 4) :test 'equalp)
                  (prove:is (cl-ds:at data 2) #(5 6) :test 'equalp))]))

  (function to-vector
    (:description "Collects all elements into CL:VECTOR."
     :arguments-and-values ((range "Object to aggregate accross.")
                            (key "Key function used to extract value for vector.")
                            (element-type ":element-type for result vector.")
                            (force-copy "Pass NIL to allow returning vector passed as RANGE."))))

  (function on-each
    (:description "Creates new range by applying FUNCTION to each element of the RANGE.")
    (:notes "Works almost like cl:map-and-friends, but lazy evaluated."))

  (function count-elements
    (:description "Counts number of elements. Usefull mostly in conjuction with GROUP-BY."
     :examples [(let ((data #(1 2 3 4 5)))
                  (prove:is (length data) (cl-ds.alg:count-elements data))
                  (prove:is 3 (cl-ds:at (cl-ds.alg:count-elements (cl-ds.alg:group-by data :key #'evenp))
                                        nil)))]
     :see-also (group-by)))

  (function hash-join
    (:description "Joins multiple ranges using hash join algorithm."
     :examples [(let ((result (sort (cl-ds.alg:hash-join #(1 2 3 4) #'identity
                                                         (list (cl-ds:field :data #(1 2 3)
                                                                            :key #'identity))
                                                         #'<
                                                         :key #'first))))
                  (prove:is (length result) 3)
                  (map nil (lambda (x) (prove:is (first x) (second x))) result))]))

  (function chain
    (:description "Joins multiple ranges sequentially into one."))

  (function summary
    (:description "Summary is a function that allows to perform multiple aggregations in one form."
     :arguments ((range "Range to aggregate.")
                 (forms "Lists describing way to invoke function. First element of list is label used to identify value in the result range, second is aggregation function designator, the rest is list of arguments that should be passed to the function, with range being replaced by the keyword :range."))
     :returns "Range of results. Use cl-ds:at with label to extract result of each individual aggregation form."))

  (function only
    (:description "Layer funciton. Creates range that skips elements that return NIL when passed to the PREDICATE function through key function."
     :arguments ((predicate "Test used to check if element should be skipped.")
                 (key "Key function used to extract value for predicate.")
                 (range "Range argument."))))

  (function without
    (:description "Layer function. Creates range that skips elements that return T when passed to the PREDICATE function through key function."
     :arguments ((predicate "Test used to check if element should be skipped.")
                 (key "Key function used to extract value for predicate.")
                 (range "Range argument."))))

  (function group-by
    (:description "Groups RANGE into partitions according to the TEST. This does not change content of RANGE, but it will force aggregation to be performed on every group independently."
     :arguments ((range "Range that is supposed to be groupped.")
                 (key "Key function, used to extract value for TEST")
                 (test "Test for inner hashtable (either eq, eql or equal)."))
     :examples [(let* ((data #(1 2 3 4 5 6 7 8 9 10))
                       (sums (cl-ds.alg:accumulate #'+ (cl-ds.alg:group-by data :key #'evenp))))
                  (prove:is (cl-ds:size sums) 2)
                  (prove:is (cl-ds:at sums t) 30)
                  (prove:is (cl-ds:at sums nil) 25))])))
