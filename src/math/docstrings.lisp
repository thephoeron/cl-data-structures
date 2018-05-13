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
            "Use approximated version of this function for more scalable solution.")
    :see-also (approximated-hodges-lehman-estimator)))

  (function
   approximated-hodges-lehman-estimator
   (:description "Estimates Hodges-Lehman estimator for range as average from samples."
    :arguments ((range "Object to aggregate")
                (sample-size "Size of the sample to draw for calculation.")
                (samples-count "How many samples should be drawn?")
                (key "Function used to extract value from element."))
    :notes ("This estimator has high breakdown point.")))

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
