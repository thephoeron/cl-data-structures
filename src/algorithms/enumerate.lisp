(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    enumerate enumerate-function

  (:range &key key test size)
  (:range &key
          (test 'eql)
          (key #'identity)
          (size 16))

  (%table %number)

  ((setf %table (make-hash-table :test test :size size)))

  ((element)
   (ensure (gethash element %table)
     (prog1 %number (incf %number))))

  (%table))
