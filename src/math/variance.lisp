(in-package #:cl-data-structures.math)


(defclass variance-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric variance (range &key key biased)
  (:generic-function-class variance-function)
  (:method (range &key key (biased t))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'variance
                                               :key key
                                               :biased biased)))


(defmethod cl-ds.alg.meta:multi-aggregation-stages ((fn variance-function)
                                                    &rest all
                                                    &key key biased
                                                    &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :average (range &rest all)
          (declare (ignore all))
          (average range :key key))
        (cl-ds.alg.meta:reduce-stage :variance (list* (if biased 0 -1) 0)
            (prev next &key average &allow-other-keys)
          (symbol-macrolet ((count (car prev))
                            (sum (cdr prev)))
            (incf count)
            (incf sum (expt (- next average) 2))
            prev))
        (lambda (&key variance &allow-other-keys)
          (/ (cdr variance)
             (car variance)))))
