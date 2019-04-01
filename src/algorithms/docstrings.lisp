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

  (function distinct
    (:description "Returns forward range that skips elements that were already seen."))

  (function split-into-chunks
    (:description "Divides aggregation process into partitions upto size."
     :returns "split-into-chunks-proxy range subclass."
     :examples [(let ((data (cl-ds.alg:to-vector (cl-ds.alg:split-into-chunks #(1 2 3 4 5 6) 2))))
                  (prove:is (cl-ds:size data) 3)
                  (prove:is (cl-ds:at data 0) #(1 2) :test 'equalp)
                  (prove:is (cl-ds:at data 1) #(3 4) :test 'equalp)
                  (prove:is (cl-ds:at data 2) #(5 6) :test 'equalp))]))

  (function partition-if
    (:description "Groups consecutive elements in the range into partition if TEST called on previous value in the range and the current value in the range returns non-NIL, creates new partition otherwise. This does not change content of the RANGE, but it will force aggregation to be performed on every group independently."
     :arguments ((range "Input range.")
                 (test "Function of two arguments used to check if elements belong the same partition."))))

  (function to-vector
    (:description "Collects all elements into CL:VECTOR."
     :returns "cl:vector with content of the range."
     :notes ("There is no way to know ahead of time how large vector will be created, and therefore multiple reallocations may be performed during aggregation. User can supply :SIZE to mitigate that."
             "To avoid copying in case when RANGE is also a vector, pass NIL as :FORCE-COPY.")
     :arguments ((range "The object to aggregate.")
                 (:key "Key function used to extract value for vector.")
                 (:element-type ":ELEMENT-TYPE for the result vector.")
                 (:size "Initial size of the internal vector. Supplie to minimize memory allocations count.")
                 (:force-copy "Pass NIL to allow returning vector passed as RANGE."))))

  (function on-each
    (:description "Creates new range by applying FUNCTION to each element of the RANGE."
     :returns "Another range."
     :notes "Works almost like cl:map-and-friends, but lazily evaluated values.."))

  (function count-elements
    (:description "Counts number of elements. Usefull mostly in conjuction with GROUP-BY."
     :returns "Integer with count of elements."
     :examples [(let ((data #(1 2 3 4 5)))
                  (prove:is (length data) (cl-ds.alg:count-elements data))
                  (prove:is 3 (cl-ds:at (cl-ds.alg:count-elements (cl-ds.alg:group-by data :key #'evenp))
                                        nil)))]
     :see-also (group-by)))

  (function hash-join
    (:description "Joins multiple ranges using hash join algorithm."
     :returns "FORWARD-RANGE"
     :examples [(let ((result (cl-ds.alg:hash-join #(1 2 3 4) #'identity
                                                   (list (cl-ds:field :data #(1 2 3)
                                                                      :key #'identity)))))
                  (map nil (lambda (x) (prove:is (first x) (second x))) result))]))

  (function chain
    (:description "Joins multiple ranges sequentially into one."
     :returns "Another range."))

  (function shuffled-range
    (:description "Creates range of shuffled integers from FROM, to TO."
     :exceptional-situations ("Raises type-error if from or to is not integer."
                              "TO must be greater then FROM, otherwise incompatible-arguments error is signaled.")
     :returns "FORWARD-RANGE"))

  (function summary
    (:description "Summary is a macro that allows to perform multiple aggregations in one form."
     :examples [(let ((result (cl-ds.alg:summary (cl-ds:iota-range :to 250)
                                :min (cl-ds.alg:accumulate #'min)
                                :max (cl-ds.alg:accumulate #'max))))
                  (prove:is (cl-ds:at result :min) 0)
                  (prove:is (cl-ds:at result :max) 249))]
     :arguments ((range "Range to aggregate.")
                 (forms "Way to invoke function in the form of the plist. Key is a label used to identify value in the result range, second is aggregation function form (function and the function arguments). The range will be inserted as the first argument in the aggregation function call by default, or in the place of any symbol with name '_'."))
     :returns "Range of results. Use cl-ds:at with label to extract result of each individual aggregation form."
     :notes "Currently, this macro does support only the single stage aggregation functions."))

  (function only
    (:description "Layer funciton. Creates range that skips elements that return NIL when passed to the PREDICATE function through key function."
     :arguments ((range "Range argument.")
                 (predicate "Test used to check if element should be skipped.")
                 (key "Key function used to extract value for predicate."))
     :returns "Either forward, bidirectional or random-access range, depending on the RANGE."))

  (function without
    (:description "Layer function. Creates range that skips elements that return T when passed to the PREDICATE function through key function."
     :arguments ((range "Range argument.")
                 (predicate "Test used to check if an element should be skipped.")
                 (key "Key function used to extract an value for the predicate."))
     :returns "Either forward, bidirectional or random-access range, depending on the RANGE."))

  (function flatten-lists
    (:description "Layer function. Flattens each list in the input range to the atoms."
     :arguments ((range "Input range.")
                 (key "Function used to extract lists from elements of the RANGE. Defaults to CL:IDENTITY."))
     :returns "FORWARD-RANGE"))

  (function latch
    (:description "Combines primary range with multiple latch ranges. Returned range contains elements picked from the primary range, where, on a corresponding positions, each of the latch ranges contains a non-nil value."
     :arguments-and-values ((range "Primary input range.")
                            (latch "Range with boolean values.")
                            (more-latches "Ranges with booleans values."))
     :returns "Another range."))

  (function zip
    (:description "Combines multiple ranges into single range by applying function length wise."))

  (function repeat
    (:description "Layer function. Constructs new range from the RANGE. New range is cyclic, and will reset to initial position once end is reached when calling the CONSUME-FRONT function. This happens always by default, or can be limited to a number of times by suppling optional TIMES argument. This function can be therefore used to go over the same range multiple times in the aggregation function."
     :arguments ((range "Input range used to construct the result.")
                 (times "How many times range will be repeated? Unlimited by default."))
     :exceptional-situations ("Will raise type-error when TIMES is not of the type (or null positive-integer)")
     :returns "FORWARD-RANGE"))

  (function restrain-size
    (:description "Layer function. Constructs new range from the RANGE. New range contains limit on how many times consume-front can be called on it before returning (values nil nil) effectivly reducing size of itself."
     :arguments ((range "Input range used to construct the result.")
                 (size "What should be limit on the new range?"))
     :returns "FORWARD-RANGE"
     :exceptional-situations "Will raise type-error when SIZE is not of the type non-negative-integer."))

  (function extremum
    (:description "Aggregation function. Find extremum (first value that would occur if whole range was sorted according to the FN). This can be used to find either the maximum or the minium."
     :arguments ((range "Input range.")
                 (fn "Comparsion function.")
                 (key "Function used to extract values from the elements in the RANGE.")
                 (value-key "Like KEY, but using this instead will preserve the complete element in the result. This argument can be used in combination with KEY, in which case KEY is applied before the VALUE-KEY."))
     :returns "Extremum"))

  (function extrema
    (:description "Aggregation function. Find extrema (both minimum and maximum) in the RANGE, according to the FN comparsion function."
     :arguments ((range "Input range.")
                 (fn "Comparsion function.")
                 (key "Function used to extract values from the elements in the RANGE.")
                 (value-key "Like KEY, but using this instead will preserve the complete element in the result. This argument can be used in combination with KEY, in which case KEY is applied before the VALUE-KEY."))
     :returns "Dotted pair. First value is the extremum that would occur as first element in the sequence sorted according to the FN, second value is the element that would occur last."))

  (function cartesian
    (:description "Combines ranges into one range that contains result of FUNCTION application on cartesian combination of all elements in the input ranges."
     :arguments ((function "Function used to combine input ranges.")
                 (range "First input range.")
                 (more-ranges "All other ranges."))
     :exceptional-situations "Will raise type-errors if any of arguments is of the wrong type."
     :returns "FORWARD-RANGE"))

  (function group-by
    (:description "Groups RANGE into partitions according to the TEST. This does not change content of the RANGE, but will force aggregation to be performed on every group independently."
     :arguments ((range "Range that is supposed to be groupped.")
                 (key "Key function, used to extract value for TEST")
                 (test "Test for inner hashtable (either eq, eql or equal)."))
     :examples [(let* ((data #(1 2 3 4 5 6 7 8 9 10))
                       (sums (cl-ds.alg:accumulate (cl-ds.alg:group-by data :key #'evenp) #'+)))
                  (prove:is (cl-ds:size sums) 2)
                  (prove:is (cl-ds:at sums t) 30)
                  (prove:is (cl-ds:at sums nil) 25))])))
