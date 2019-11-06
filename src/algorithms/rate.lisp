(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    rate rate-function
    (:range test &key key)
    (:range test &key (key #'identity))

    (%positive %total %test)

    ((&key test &allow-other-keys)
     (ensure-functionf test)
     (setf %positive 0
           %test test
           %total 0))

    ((element)
     (incf %total)
     (when (funcall %test element)
       (incf %positive)))

    ((/ %positive %total)))
