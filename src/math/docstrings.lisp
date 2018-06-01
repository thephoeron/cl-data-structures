(in-package #:cl-data-structures.math)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function average
    (:description "Calculate average of elements in the range."
     :arguments ((range "Object to aggregate.")
                 (key "Function used to extract value from element that shall be passed to average function."))))

  (function median-absolute-deviation
    (:description "Calculates MAD estimator for range."
     :arguments ((range "Object to aggregate")
                 (key "Function used to extract value from element."))))

  (function hodges-lehman-estimator
    (:description "Calculates Hodges-Lehman estimator for range."
     :arguments ((range "Object to aggregate")
                 (key "Function used to extract value from element.")
                 (parallel "Will use lparallel for calculations if T."))
     :notes ("This estimator has high breakdown point."
             "Time complexity greater then cubic. Memory complexity quadratic."
             "Use bootstrapping for more scalable solution.")
     :see-also (bootstrap)
     :thread-safety "Will ues lparallel if PARALLEL is T"))

  (function bootstrap
    (:description "Changes aggregation into bootstrapping schema using percentail method. Reduces time needed to calculate functions with expensive complexity."
     :arguments ((range "Data for aggregation.")
                 (sample-size "Size of the single sample.")
                 (samples-count "Total number of samples drawn.")
                 (confidence "Confidence value. High confidence results in wide estimate bounds.")
                 (key "Key passed to SORT function.")
                 (compare "Function passed to SORT function.")
                 (parallel "Will use lparallel to evaluate samples if T. Defaults to T."))
     :thread-safety "Will use lparallel to samples if PARALLEL is T."))

  (function simple-linear-regression
    (:description "Matches linear function to RANGE using least squares method."))

  (function statistical-summary
    (:description "Calculates classical statistical values (average, variance, skewness, kurtosis)."))

  (function mutual-information
    (:description "Calculates mutual-information between FIELD and COMPARATIVE-FIELDS. Elements in each FIELD should be EQUAL comparable."
     :returns "Range, allowing to access value for each field by quering cl-ds:at with label of field."))

  (function moments
    (:description "Calculate statistical moments in the range."
     :arguments ((range "Object to aggregate.")
                 (from "Positive number. Lowest moment to calculate.")
                 (count "Positive number. How many moments to calculate.")
                 (about "Number. Value around which moments are calculated.")))))
