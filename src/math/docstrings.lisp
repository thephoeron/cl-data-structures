(in-package #:cl-data-structures.math)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function
   average
   (:description "Calculate average of elements in the range."
    :arguments ((range "Object to aggregate.")
                (key "Function used to extract value from element that shall be passed to average function."))))

  (function
   median-absolute-deviation
   (:description "Calculates MAD estimator for range."
    :arguments ((range "Object to aggregate")
                (key "Function used to extract value from element."))))

  (function
   hodges-lehman-estimator
   (:description "Calculates Hodges-Lehman estimator for range."
    :arguments ((range "Object to aggregate")
                (key "Function used to extract value from element."))
    :notes ("This estimator has high breakdown point."
            "Time complexity greater then cubic. Memory complexity quadratic."
            "Use bootstrapping for more scalable solution.")
    :see-also (bootstrap)))

  (function
   bootstrap
   (:description "Changes aggregation into bootstrapping schema. Draws multiple samples, applies aggregation to each of those, and finally uses statistical-summary on the result. Reduces time needed to calculate functions with expensive complexity."))

  (function
   simple-linear-regression
   (:description "Matches linear function to RANGE using least squares method."))

  (function
   statistical-summary
   (:description "Calculates classical statistical values (average, variance, skewness, kurtosis)."))

  (function
   moments
   (:description "Calculate statistical moments in the range."
    :arguments ((range "Object to aggregate.")
                (from "Positive number. Lowest moment to calculate.")
                (count "Positive number. How many moments to calculate.")
                (about "Number. Value around which moments are calculated.")))))
