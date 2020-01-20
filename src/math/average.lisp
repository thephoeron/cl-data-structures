(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    average average-function

    (:range &key key sum count)
    (:range &key (key #'identity) (sum 0) (count 0))

    ((%sum number) (%count integer))

    ((setf %sum sum
           %count count))

    ((element)
     (incf %count)
     (incf %sum element))

    ((/ %sum %count)))


(cl-ds.alg.meta:define-aggregation-function
    harmonic-average harmonic-average-function

    (:range &key key)
    (:range &key (key #'identity))

    ((%sum number) (%count integer) (%zero boolean))

    ((setf %sum 0
           %count 0
           %zero nil))

    ((element)
     (incf %count)
     (if (zerop element)
         (setf %zero t)
         (incf %sum (/ 1 element))))

    ((if %zero
         0.0
         (/ %count %sum))))
