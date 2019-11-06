(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    rate rate-function
    (:range test &key key positive total)
    (:range test &key (key #'identity) (positive 0) (total 0))

    (%positive %total %test)

    ((&key test positive total &allow-other-keys)
     (ensure-functionf test)
     (setf %positive positive
           %test test
           %total total))

    ((element)
     (incf %total)
     (when (funcall %test element)
       (incf %positive)))

    ((/ %positive %total)))
