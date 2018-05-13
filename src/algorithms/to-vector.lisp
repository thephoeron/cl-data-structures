(in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    to-vector to-vector-function

  (:range &key key element-type force-copy)
  (:range &key (key #'identity) (element-type t) (force-copy t))

  (%vector)

  ((&key element-type &allow-other-keys)
   (setf %vector (make-array 16 :adjustable t
                                :fill-pointer 0
                                :element-type element-type)))
  ((element)
   (vector-push-extend element %vector))

  (%vector))


(defmethod to-vector ((range vector)
                      &key
                        (key #'identity)
                        (element-type t)
                        (force-copy t))
  (if (and (not force-copy)
           (subtypep element-type (array-element-type range))
           (eq key #'identity))
      range
      (call-next-method)))
