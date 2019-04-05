(in-package #:cl-data-structures.algorithms)
(eval-always
  (scribble:configure-scribble :package :cl-data-structures.algorithms)
  (named-readtables:in-readtable :scribble))


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function accumulate
    (:description "Like CL:REDUCE but works on all traversable objects."))

  (function cumulative-accumulate
    (:description ""))

  (function distinct
    (:description "Returns forward range that skips elements that were already seen."))

  (function split-into-chunks
    (:description "Divides aggregation process into partitions upto size."
     :returns "Instance of SPLIT-INTO-CHUNKS-PROXY range subclass."
     :examples [(let ((data (cl-ds.alg:to-vector (cl-ds.alg:split-into-chunks #(1 2 3 4 5 6) 2))))
                  (prove:is (cl-ds:size data) 3)
                  (prove:is (cl-ds:at data 0) #(1 2) :test 'equalp)
                  (prove:is (cl-ds:at data 1) #(3 4) :test 'equalp)
                  (prove:is (cl-ds:at data 2) #(5 6) :test 'equalp))]))

  (function partition-if
    (:description "Groups consecutive elements in the range into a partition if TEST called on the previous value in the range and the current value in the range returns non-NIL, creates new partition otherwise. This does not change the content of the RANGE, but it will force aggregation to be performed on every group independently."
     :arguments ((range "An input range.")
                 (test "A function of two arguments used to check if elements belong to the same partition."))))

  (function to-vector
    (:description "Collects all elements into a CL:VECTOR."
     :returns "CL:VECTOR with the content of the RANGE."
     :notes ("There is no way to know ahead of time how large vector will be created, and therefore multiple reallocations may be performed during aggregation. A user can supply :SIZE to mitigate that."
             "To avoid copying in the case when RANGE is also a vector, pass NIL as :FORCE-COPY.")
     :arguments ((range "Object to aggregate.")
                 (:key "Key function used to extract value to the result vector.")
                 (:element-type ":ELEMENT-TYPE for the result vector.")
                 (:size "Initial size of the internal vector. Supplie to minimize memory allocations count.")
                 (:force-copy "When NIL, TO-VECTOR called with CL:VECTOR is allowed to return the input."))))

  (function on-each
    (:description "Creates a new range by applying the FUNCTION to each element of the RANGE."
     :returns "FUNDAMENTAL-FORWARD-RANGE instance."
     :notes "Works almost like cl:map-and-friends, but lazily evaluates content."))

  (function count-elements
    (:description "Counts the number of elements. Useful mostly in conjunction with a GROUP-BY."
     :returns "Integer."
     :examples [(let ((data #(1 2 3 4 5)))
                  (prove:is (length data) (cl-ds.alg:count-elements data))
                  (prove:is 3 (cl-ds:at (cl-ds.alg:count-elements (cl-ds.alg:group-by data :key #'evenp))
                                        nil)))]
     :see-also (group-by)))

  (function hash-join
    (:description "Joins multiple ranges using the hash join algorithm."
     :returns "FUNDAMENTAL-FORWARD-RANGE instance."
     :examples [(let ((result (cl-ds.alg:hash-join #(1 2 3 4) #'identity
                                                   (list (cl-ds:field :data #(1 2 3)
                                                                      :key #'identity)))))
                  (map nil (lambda (x) (prove:is (first x) (second x))) result))]))

  (function chain
    (:description "Sequentially concatenate multiple ranges into one."
     :exceptional-situations ("Raises TYPE-ERROR if any of the input ranges is not (OR CL:SEQUENCE FUNDAMENTAL-FORWARD-RANGE).")
     :returns "FUNDAMENTAL-FORWARD-RANGE instance."))

  (function shuffled-range
    (:description "Creates a range of shuffled integers from FROM, to TO."
     :exceptional-situations ("Raises TYPE-ERROR if FROM or TO is not an integer."
                              "TO must be greater then FROM, otherwise the incompatible-arguments error is signaled.")
     :returns "FUNDAMENTAL-FORWARD-RANGE instance."))

  (function summary
    (:description "The summary is a macro allowing performance of multiple aggregations in one function call."
     :examples [(let ((result (cl-ds.alg:summary (cl-ds:iota-range :to 250)
                                :min (cl-ds.alg:accumulate #'min)
                                :max (cl-ds.alg:accumulate #'max))))
                  (prove:is (cl-ds:at result :min) 0)
                  (prove:is (cl-ds:at result :max) 249))]
     :arguments ((range "Range to aggregate.")
                 (forms "Description of function invocation in the form of the plist. Key is a label used to identify value in the result range, a value is an aggregation function form (function and the function arguments). The range will be inserted as the first argument in the aggregation function call by default, or in the place of any symbol with name '_' if such symbol is present."))
     :returns "Range of results. Use cl-ds:at with label to extract result of each individual aggregation form."
     :notes ("Currently, this macro does support only the single stage aggregation functions."
             "Particularly useful when the iteration over the range requires considerable time alone and therefore repeating it should be avoided for efficiency sake.")))

  (function only
    (:description "A layer function. Creates a range that skips elements that PREDICATE (KEY element) => NIL."
     :arguments ((range "Input range.")
                 (predicate "Test used to check if element should be skipped.")
                 (key "Key function used to extract a value for predicate."))
     :returns "Either forward, bidirectional or random-access range, depending on the RANGE."))

  (function without
    (:description "A layer function. Creates a range that skips elements that PREDICATE (KEY element) => T."
     :arguments ((range "Input range.")
                 (predicate "Test used to check if an element should be skipped.")
                 (key "Key function used to extract a value for the predicate."))
     :returns "Either forward, bidirectional or random-access range, depending on the RANGE."))

  (function flatten-lists
    (:description "A layer function. Flattens each list in the input range to the atoms."
     :arguments ((range "Input range.")
                 (key "Function used to extract lists from elements of the RANGE. Defaults to CL:IDENTITY."))
     :returns "FUNDAMENTAL-FORWARD-RANGE instance."))

  (function latch
    (:description "Combines primary range with multiple latch ranges. The returned range contains elements picked from the primary range, where, on corresponding positions, each of the latch ranges contains a non-nil value."
     :arguments-and-values ((range "Primary input range.")
                            (latch "Range with boolean values.")
                            (more-latches "Ranges with boolean values."))
     :returns "FUNDAMENTAL-FORWARD-RANGE instance."))

  (function zip
    (:description "Combines multiple ranges into a single range by applying FUNCTION lengthwise."))

  (function repeat
    (:description "A layer function. Constructs new range from the RANGE. The new range is cyclic and will reset to initial position once the end is reached when calling the CONSUME-FRONT function. This happens always by default, and can be limited to a number of times by supplying optional TIMES argument. This function can be therefore used to go over the same range multiple times in the aggregation function."
     :arguments ((range "Input range used to construct the result.")
                 (times "How many times the range will be repeated? Unlimited by default."))
     :exceptional-situations ("Will raise the TYPE-ERROR when TIMES is not of the type (OR NULL POSITIVE-INTEGER).")
     :returns "FUNDAMENTAL-FORWARD-RANGE instance."))

  (function restrain-size
    (:description "A layer function. Constructs new range from the RANGE. New range contains a limit on how many times consume-front can be called on it before returning (values nil nil), effectively reducing size of the RANGE."
     :arguments ((range "Input range used to construct the result.")
                 (size "What should be the limit on the new range?"))
     :returns "FUNDAMENTAL-FORWARD-RANGE instance."
     :exceptional-situations "Will raise a type-error when the SIZE is not of the type non-negative-integer."))

  (function extremum
    (:description "An aggregation function. Finds the extremum (the first value that would occur if the whole range was sorted according to the FN). This can be used to find either the maximum or the minimum."
     :arguments ((range "Input range.")
                 (fn "Comparsion function.")
                 (key "Function used to extract values from the elements in the RANGE.")
                 (value-key "Like KEY, but using this instead will preserve the complete element in the result. This argument can be used in combination with KEY, in which case KEY is applied before the VALUE-KEY."))
     :notes ("Shadows alexandria:extremum.")
     :returns "Single extremum value."))

  (function extrema
    (:description "An aggregation function. Finds extrema (both minimum and maximum) in the RANGE, according to the FN comparsion function."
     :arguments ((range "Input range.")
                 (fn "Comparsion function.")
                 (key "Function used to extract values from the elements in the RANGE.")
                 (value-key "Like KEY, but using this instead will preserve the complete element in the result. This argument can be used in combination with KEY, in which case KEY is applied before the VALUE-KEY."))
     :returns "Dotted pair. The first value is the extremum that would occur as the first element in the sequence sorted according to the FN, second value is an element that would occur as the last."))

  (function cartesian
    (:description "Combines RANGES into a singular range that contains results of FUNCTION application on cartesian combination of all elements in the input RANGES."
     :arguments ((function "Function used to combine input ranges.")
                 (range "First input range.")
                 (more-ranges "All other ranges."))
     :exceptional-situations "Will raise a TYPE-ERROR if any of the RANGES is of a wrong type."
     :returns "FUNDAMENTAL-FORWARD-RANGE instance."))

  (function group-by
    (:description "Groups RANGE into partitions according to the TEST. This does not change the content of the RANGE, but will force aggregation to be performed on every group independently."
     :arguments ((range "Range that is supposed to be groupped.")
                 (key "Key function, used to extract value for TEST.")
                 (test "A test for inner hashtable (either eq, eql or equal)."))
     :examples [(let* ((data #(1 2 3 4 5 6 7 8 9 10))
                       (sums (cl-ds.alg:accumulate (cl-ds.alg:group-by data :key #'evenp) #'+)))
                  (prove:is (cl-ds:size sums) 2)
                  (prove:is (cl-ds:at sums t) 30)
                  (prove:is (cl-ds:at sums nil) 25))])))
