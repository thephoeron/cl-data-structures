(in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    count-elements
    count-elements-function

    (:range)
    (:range)

    (%count)

    ((setf %count 0))

    ((element)
     (incf %count))

    (%count))
