(cl:in-package #:cl-data-structures.math)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function average
    (:description "Calculate average of elements in the range."
     :arguments ((range "Object to aggregate.")
                 (:key "Function used to extract value from RANGE element."))))

  (function harmonic-average
    (:description "Calculate harmonic average of elements in the range"
     :exceptional-situations "Will return 0 if at least one elements is zero."
     :arguments ((range "Object to aggregate.")
                 (:key "Function used to extract value from RANGE element."))))

  (function geometric-average
    (:description "Calculate geometric average of elements in the range"
     :arguments ((range "Object to aggregate.")
                 (:key "Function used to extract value from RANGE element."))))

  (function array-average
    (:description "Calculate element wise average of arrays in the range."
     :arguments ((range "Object to aggregate.")
                 (key "Function used to extract value from element that shall be passed to average function."))
     :returns "SIMPLE-ARRAY holding results."))

  (function array-harmonic-average
    (:description "Calculate element wise harmonic average of arrays in the range."
     :arguments ((range "Object to aggregate.")
                 (:key "Function used to extract array from RANGE element."))
     :returns "SIMPLE-ARRAY holding results."))

  (function array-geometric-average
    (:description "Calculate element wise geometric average of arrays in the range."
     :arguments ((range "Object to aggregate.")
                 (:key "Function used to extract array from RANGE element."))
     :returns "SIMPLE-ARRAY holding results."))

  (function fast-map
    (:description "Embedds finite metric set into euclid space using fast-map algorithm.."
     :returns "SIMPLE-ARRAY containing with embeddings with all elements."))

  (function median-absolute-deviation
    (:description "Calculates MAD estimator for element in the RANGE."
     :arguments ((range "Object to aggregate")
                 (:key "Function used to extract array from RANGE element."))
     :notes "Roboost scale estimator."))

  (function moving-average
    (:description "Calculates moving-average for elements in the RANGE."
     :returns "Range of all moving-averages."
     :arguments ((range "Object to process.")
                 (key "Function used to extract values from the RANGE."))))

  (function hodges-lehmann-estimator
    (:description "Calculates Hodges-Lehman estimator for range."
     :arguments ((range "Object to aggregate")
                 (key "Function used to extract value from element.")
                 (parallel "Will use lparallel for calculations if T."))
     :notes ("This estimator has high breakdown point."
             "Roboost expected value estimator."
             "Time complexity greater then cubic. Memory complexity quadratic."
             "Use bootstrapping for more scalable solution.")
     :see-also (bootstrap)
     :thread-safety "Will ues lparallel if PARALLEL is T."))

  (function sum
    (:description "Calculates sum of all elements in the range."
     :returns "A total sum of all elements in the array."))

  (function array-sum
    (:description "Calculates elementwise sum of all arrays in the range."))

  (function gini-impurity
    (:description "Calculates Gini impurity of the content range."
     :exceptional-situations ("Will signal TYPE-ERROR when HASH-TABLE is not of the type CL:HASH-TABLE."
                              "Will signal TYPE-ERROR when KEY is not a function."
                              "Will signal TYPE-ERROR when COUNT-FN is not a function."
                              "Will signal exception as would CL:MAKE-HASH-TABLE function when TEST is invalid.")
     :returns "Numerical value representing the Gini impurity."
     :arguments ((range "Object to aggregate.")
                 (:key "Function used to extract value from element.")
                 (:size "Size passed to the MAKE-HASH-TABLE function.")
                 (:test "Test for the hash table.")
                 (:count-fn "Function used to extract count of elements from the element in the RANGE."))))

  (function entropy
    (:description "Calculates Shannon information entropy of the elements in the range (using natural logarithm)."
     :arguments ((range "Object to aggregate.")
                 (:key "Function used to extract value from element.")
                 (:size "Size passed to MAKE-HASH-TABLE function.")
                 (:test "Test for the hash table.")
                 (:count-fn "Function used to extract number of elements. Defaults to constantly 1."))
     :exceptional-situations ("Will signal TYPE-ERROR when KEY is not a function."
                              "Will signal TYPE-ERROR when COUNT-FN is not a function."
                              "Will signal exception as would CL:MAKE-HASH-TABLE function when TEST is invalid.")
     :notes ("Uses e as a logarithm base.")
     :returns "Numerical value represting Shannon entropy."))

  (function bootstrap
    (:description "Changes aggregation into bootstrapping schema using percentail method. Reduces the ammount of resources needed to obtain the value."
     :arguments ((range "Data for aggregation.")
                 (sample-size "Size of the single sample.")
                 (samples-count "Total number of samples drawn.")
                 (confidence "Confidence value. High confidence results in wide estimate bounds.")
                 (key "Key passed to SORT function.")
                 (compare "Function passed to SORT function.")
                 (parallel "Will use lparallel to evaluate samples if T. Defaults to T."))
     :thread-safety "Will process each sample in the lparallel task if PARALLEL is T."
     :notes "Don't attempt to mix lparallel used in bootstrap with lparallel used in the aggregation function"))

  (function simple-linear-regression
    (:description "Matches linear function to RANGE using least squares method."
     :arguments ((x-key "Function used to extract argument from range.")
                 (y-key "Function used to extract result from range."))
     :returns "Function object. Call with argument to obtain expected result. Pass to beta0 function to obtain beta0 value. Pass to beta1 to obtain beta1 value."))

  (function variance
    (:description "Calculates variance."
     :returns "Number representing variance."
     :arguments ((range "Data to aggregate.")
                 (key "Function used to extract values from elements.")
                 (biased "Boolean. Should result be biased?"))
     :notes "Name conflict with alexandria:variance."))

  (function mutual-information
    (:description "Calculates mutual-information between FIELD and COMPARATIVE-FIELDS. Elements in each FIELD should be EQUAL comparable."
     :returns "Range. Provides access to values for each field by quering cl-ds:at with label of field."))

  (function moments
    (:description "Calculate statistical moments in the range."
     :arguments ((range "Object to aggregate.")
                 (from "Positive number. Lowest moment to calculate.")
                 (count "Positive number. How many moments to calculate.")
                 (about "Number. Value around which moments are calculated.")
                 (key "Function used to extract values from elements."))
     :returns "Range. Query it with CL-DS:AT function passing rank of moment that you want to obtain.")))
