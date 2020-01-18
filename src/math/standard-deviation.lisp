(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    standard-deviation
    standard-deviation-function

    (:range around &key key biased)
    (:range around &key (key #'identity) (biased t))

    (%count %sum %biased %average)

    ((setf %count 0
           %average around
           %sum 0
           %biased biased))

    ((element)
     (incf %count)
     (incf %sum (expt (- element %average) 2)))

    ((sqrt (/ %sum %count))))
