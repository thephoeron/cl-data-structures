(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    absolute-value-norm absolute-value-norm-function

  (:range &key key)

  (:range &key (key #'identity))

  (%sum)

  ((&key &allow-other-keys)
   (setf %sum 0))

  ((element)
   (incf %sum (abs element)))

  (%sum))
