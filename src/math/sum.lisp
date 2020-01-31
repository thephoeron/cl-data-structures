(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function sum sum-function
  (:range &key key sum)
  (:range &key (key #'identity) (sum 0))
  ((%sum number))
  ((setf %sum sum))
  ((element)
   (incf %sum element))
  (%sum))


(cl-ds.alg.meta:define-aggregation-function array-sum array-sum-function
  (:range &key key sum)
  (:range &key (key #'identity) (sum nil))
  ((%sum (or null array)))
  ((if (arrayp sum)
       (setf %sum (copy-array sum))
       (setf %sum nil)))
  ((element)
   (if (null %sum)
       (setf %sum (copy-array element))
       (iterate
         (for i from 0 below (array-total-size %sum))
         (incf (row-major-aref %sum i) (row-major-aref element i)))))
  (%sum))
