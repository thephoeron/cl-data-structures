(in-package #:cl-data-structures.utils.metric)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function levenshtein-metric
    (:description "Calculates the Levenshtein distance."
     :returns "Non-negative fixnum representing distance."
     :arguments ((str1 "String.")
                 (str2 "String."))))

  (function svr-metric
    (:description "Calculates the subvector representation based metric."
     :returns "Single-float (between 0.0 and 1.0) representing distance."
     :arguments ((a "Vector.")
                 (b "Vector."))
     :notes "Content of a and b vectors must be comparable using EQUAL."))

  (function hellinger-metric
    (:description "Calculates hellinger distance between two distributions, both represented as histograms."
     :arguments ((q "Frequency vector.")
                 (p "Frequency vector."))
     :returns "Hellinger distance."))

  (function earth-mover-metric
            (:description "Calculates earth mover distance between two distributions, both represented as histograms."
             :arguments ((a "Frequency vector.")
                         (b "Frequency vector."))
             :returns "Earth mover distance between two histograms.")))
