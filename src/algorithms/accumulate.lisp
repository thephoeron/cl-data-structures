(in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    accumulate accumulate-function

  (fn :range &key key initial-value)
  (fn :range &key (key #'identity) (initial-value nil initial-value-bound))

  (%value %fn %first-iteration %initial-value-present)

  ((&key fn (initial-value nil initial-value-bound))
   (setf %value initial-value
         %fn fn
         %first-iteration t
         %initial-value-present initial-value-bound))

  ((element)
   (if %first-iteration
       (setf %value (if %initial-value-present
                        (funcall %fn %value element)
                        element)
             %first-iteration nil)
       (setf %value (funcall %fn %value element))))

  (%value))
