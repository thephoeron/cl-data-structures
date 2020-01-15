(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function variance variance-function
    (:range around &key key biased)
    (:range around &key (key #'identity) (biased t))

    (%count %sum %biased %average)

    ((&key biased average &allow-other-keys)
     (setf %count 0
           %average average
           %sum 0
           %biased biased))

    ((element)
     (incf %count)
     (incf %sum (expt (- element %average) 2)))

    ((/ %sum %count)))
