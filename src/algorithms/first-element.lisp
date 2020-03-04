(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    first-element first-element-function

  (:range &key key)
  (:range &key (key #'identity))

  (%value %bound)

  ((setf %value nil
         %bound nil))

  ((element)
   (unless %bound
     (setf %bound t
           %value element)))

  (%value))


(cl-ds.alg.meta:define-aggregation-function
    last-element last-element-function

  (:range &key key)
  (:range &key (key #'identity))

  (%value)

  ((setf %value nil))

  ((element)
   (setf %value element))

  (%value))
