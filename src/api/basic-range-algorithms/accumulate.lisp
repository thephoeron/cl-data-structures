(in-package #:cl-data-structures)


(defgeneric accumulate (function range &key key initial-value)
  (:generic-function-class accumulate-function)
  (:method (function (range fundamental-range) &key (key #'identity) (initial-value nil))
    (apply-aggregation-function range #'accumulate
                                :key key
                                :initial-value initial-value)))

