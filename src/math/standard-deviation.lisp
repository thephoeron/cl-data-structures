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
                                                    &key
                                                    &allow-other-keys)
  (let ((variance-stages (apply #'cl-ds.alg.meta:multi-aggregation-stages
                                #'variance
                                all)))
    (setf (rest (last variance-stages))
          (list (lambda (x &key &allow-other-keys)
                  (sqrt x))))
    variance-stages))
