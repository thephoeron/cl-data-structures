(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    frequency frequency-function
    (:range &key key test normalize)
    (:range &key (key #'identity) (test 'eql) (normalize t))

    ((%total-count integer) (%sub-counts hash-table) %normalize)

    ((setf %total-count 0
           %normalize normalize
           %sub-counts (make-hash-table :test test)))

    ((element)
     (incf %total-count)
     (incf (the integer (gethash element %sub-counts 0)) 1))

    ((when %normalize
       (iterate
         (for (key value) in-hashtable %sub-counts)
         (setf (gethash key %sub-counts)
               (coerce (/ value %total-count) 'single-float))))
     (make-hash-table-range %sub-counts)))
