(cl:in-package #:cl-data-structures.math)


(defclass simple-linear-regression (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric simple-linear-regression (range x-key y-key)
  (:generic-function-class simple-linear-regression)
  (:method (range x-key y-key)
    (cl-ds.alg.meta:apply-range-function
     range #'simple-linear-regression
     :x-key x-key
     :y-key y-key)))


(defstruct linear-regression-state
  (xx 0.0) (yy 0.0) (xy 0.0))


(defclass linear-regression-fit (c2mop:funcallable-standard-object)
  ((%beta1 :initarg :beta1
           :accessor beta1)
   (%beta0 :initarg :beta0
           :accessor beta0))
  (:metaclass c2mop:funcallable-standard-class))


(defmethod initialize-instance :after ((obj linear-regression-fit) &rest rest)
  (declare (ignore rest))
  (c2mop:set-funcallable-instance-function obj (lambda (x)
                                                 (+ (* (beta1 obj) x)
                                                    (beta0 obj)))))


(defmethod cl-ds.alg.meta:multi-aggregation-stages ((fn simple-linear-regression)
                                                    &rest all
                                                    &key x-key y-key
                                                    &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :averages (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:summary range
            :average-x (average :key x-key)
            :average-y (average :key y-key)))

        (cl-ds.alg.meta:reduce-stage :stats (make-linear-regression-state)
            (state element &key averages &allow-other-keys)
          (bind (((:slots xx yy xy) state)
                 (x (funcall x-key element))
                 (y (funcall y-key element))
                 (average-x (cl-ds:at averages :average-x))
                 (average-y (cl-ds:at averages :average-y)))
            (incf yy (expt (- y average-y) 2))
            (incf xy (* (- y average-y) (- x average-x)))
            (incf xx (expt (- x average-x) 2))
            state))

        (lambda (&key stats averages &allow-other-keys)
          (bind (((:slots xx yy xy) stats)
                 (beta1 (/ xy xx))
                 (average-x (cl-ds:at averages :average-x))
                 (average-y (cl-ds:at averages :average-y))
                 (beta0 (- average-y (* beta1 average-x))))
            (make 'linear-regression-fit :beta0 beta0 :beta1 beta1)))))
