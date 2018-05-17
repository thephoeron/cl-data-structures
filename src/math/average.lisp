(in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    average average-function

  (:range &key key)
  (:range &key (key #'identity))

  (%sum %count)

  ((&key &allow-other-keys)
   (setf %sum 0
         %count 0))
  ((element)
   (incf %count)
   (incf %sum element))

  ((/ %sum %count)))


(cl-ds.alg.meta:define-aggregation-function
    harmonic-average harmonic-average-function

  (:range &key key)
  (:range &key (key #'identity))

  (%sum %count)

  ((&key &allow-other-keys)
   (setf %sum 0
         %count 0))
  ((element)
   (incf %count)
   (incf %sum (/ 1 element)))

  ((/ %count %sum)))
