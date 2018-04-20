(in-package #:cl-data-structures.math)


(defclass standard-deviation-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric standard-deviation (range &key key biased)
  (:generic-function-class standard-deviation-function)
  (:method (range &key key (biased t))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'standard-deviation
                                               :key key
                                               :biased biased)))


(defmethod cl-ds.alg.meta:multi-aggregation-stages ((fn standard-deviation-function)
                                                    &rest all
                                                    &key key biased
                                                    &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :average (range) (average range :key key))
        #'sqrt))
