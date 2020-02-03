(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    enumerate enumerate-function

  (:range &key key test size number)
  (:range &key (test 'eql) (key #'identity)
   (number 0) (size 16))

  (%table %number)

  ((check-type number integer)
   (setf %table (make-hash-table :test test :size size)
         %number number))

  ((element)
   (ensure (gethash element %table)
     (prog1 %number (incf %number))))

  (%table))
