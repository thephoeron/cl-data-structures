 (in-package #:cl-data-structures.statistics)


(defclass simple-linear-regression (cl-ds.alg:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric simple-linear-regression (range x-key y-key)
  (:generic-function-class simple-linear-regression)
  (:method (range x-key y-key)
    (cl-ds.alg:apply-aggregation-function range
                                          #'simple-linear-regression
                                          :x-key x-key
                                          :y-key y-key)))


(defmethod cl-ds.alg:multi-aggregation-stages ((fn simple-linear-regression)
                                               &rest all
                                               &key x-key y-key
                                               &allow-other-keys)
  (declare (ignore all))
  `((:average-y . ,(lambda (range)
                     (cl-ds.stat:average range :key y-key)))
    (:average-x . ,(lambda (range)
                     (cl-ds.stat:average range :key x-key)))))


(defstruct linear-regression-state
  (xx 0.0) (yy 0.0) (xy 0.0)
  average-x average-y x-key y-key)


(defmethod cl-ds:make-state ((function simple-linear-regression)
                             &rest all
                             &key x-key y-key average-x average-y)
  (declare (ignore all))
  (make-linear-regression-state :x-key x-key
                                :y-key y-key
                                :average-x average-x
                                :average-y average-y))


(defmethod cl-ds:aggregate ((function simple-linear-regression)
                            state
                            element)
  (check-type state linear-regression-state)
  (bind (((:slots xx yy xy x-key y-key average-x average-y) state)
         (x (funcall x-key element))
         (y (funcall y-key element)))
    (incf yy (expt (- y average-y) 2))
    (incf xy (* (- y average-y) (- x average-x)))
    (incf xx (expt (- x average-x) 2))))


(defmethod cl-ds.alg:state-result ((function simple-linear-regression)
                                   state)
  (check-type state linear-regression-state)
  (bind (((:slots xx yy xy average-x average-y) state)
         (beta1 (/ xy xx))
         (beta0 (- average-y (* beta1 average-x))))
    (values beta1 beta0)))
