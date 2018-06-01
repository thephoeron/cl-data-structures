(in-package #:cl-data-structures.utils.metric)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function levenshtein-metric
    (:description "Calculates the Levenshtein distance."
     :returns "Non-negative fixnum representing distance."
     :arguments ((str1 "String.")
                 (str2 "String."))))

  (function hellinger-metric
    (:description "Calculates hellinger distance between two distributions, both represented as histogram."
     :arguments ((q "Frequency vector.")
                 (p "Frequency vector."))
     :returns "Hellinger distance.")))
