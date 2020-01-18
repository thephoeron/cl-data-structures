(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    to-list to-list-function

  (:range &key key)
  (:range &key (key #'identity))

  (%list)

  ((setf %list '()))

  ((element)
   (push element %list))

  ((nreverse %list)))
