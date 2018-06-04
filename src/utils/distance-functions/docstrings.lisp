(in-package #:cl-data-structures.utils.distance)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function bhattacharyya-distance
    (:description "Calculates Bhattacharyya distance between two distributions represented as frequency vector."
     :returns "Bhattacharyya as float."
     :arguments ((h1 "Frequency vector representing distribution.")
                 (h2 "Frequency vector representing distribution.")))))
