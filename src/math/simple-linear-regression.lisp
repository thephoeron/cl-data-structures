(in-package #:cl-data-structures.math)


(defclass simple-linear-regression (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric simple-linear-regression (range x-key y-key)
  (:generic-function-class simple-linear-regression)
  (:method (range x-key y-key)
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'simple-linear-regression
                                               :x-key x-key
                                               :y-key y-key)))


(defstruct linear-regression-state
  (xx 0.0) (yy 0.0) (xy 0.0))


(defmethod cl-ds.alg.meta:multi-aggregation-stages ((fn simple-linear-regression)
                                                    &rest all
                                                    &key x-key y-key
                                                    &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :average-y (range &rest all)
          (declare (ignore all))
          (average range :key y-key))
        (cl-ds.alg.meta:stage :average-x (range &rest all)
          (declare (ignore all))
          (average range :key x-key))
        (cl-ds.alg.meta:reduce-stage :stats (make-linear-regression-state)
            (state element &key average-x average-y &allow-other-keys)
          (bind (((:slots xx yy xy) state)
                 (x (funcall x-key element))
                 (y (funcall y-key element)))
            (incf yy (expt (- y average-y) 2))
            (incf xy (* (- y average-y) (- x average-x)))
            (incf xx (expt (- x average-x) 2))
            state))
        (lambda (&key stats average-y average-x &allow-other-keys)
          (bind (((:slots xx yy xy) stats)
                 (beta1 (/ xy xx))
                 (beta0 (- average-y (* beta1 average-x))))
            (list beta1 beta0)))))
