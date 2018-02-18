(in-package #:cl-data-structures.math)


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
                     (average range :key y-key)))
    (:average-x . ,(lambda (range)
                     (average range :key x-key)))))


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
    (list beta1 beta0)))


(defclass simple-linear-regression-with-error (cl-ds.alg:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric simple-linear-regression-with-error (range x-key y-key)
  (:generic-function-class simple-linear-regression-with-error)
  (:method (range x-key y-key)
    (cl-ds.alg:apply-aggregation-function range
                                          #'simple-linear-regression-with-error
                                          :x-key x-key
                                          :y-key y-key)))


(defstruct linear-regression-errors
  x-key y-key (count 0) (sum-of-error-squares 0.0) regres)


(defmethod cl-ds:make-state ((function simple-linear-regression-with-error)
                             &rest all
                             &key x-key y-key regres)
  (declare (ignore all))
  (make-linear-regression-errors :x-key x-key
                                 :y-key y-key
                                 :regres regres))


(defmethod cl-ds.alg:multi-aggregation-stages ((function simple-linear-regression-with-error)
                                               &rest all
                                               &key x-key y-key)
  (declare (ignore all))
  (append (cl-ds.alg:multi-aggregation-stages #'simple-linear-regression
                                              :x-key x-key
                                              :y-key y-key)
          `((:regres . ,(lambda (range)
                           (simple-linear-regression range x-key y-key))))))


(defmethod cl-ds.alg:aggregate ((function simple-linear-regression-with-error)
                                state
                                element)
  (check-type state linear-regression-errors)
  (bind (((:slots x-key y-key count sum-of-error-squares regres) state)
         ((beta1 beta0) regres)
         (x (funcall x-key element))
         (y (funcall y-key element))
         (fit (+ (* x beta1) beta0))
         (er (expt (- fit y) 2)))
    (incf count)
    (incf sum-of-error-squares er)))


(defmethod cl-ds.alg:state-result ((function simple-linear-regression-with-error)
                                   state)
  (check-type state linear-regression-errors)
  (bind (((:slots regres count sum-of-error-squares) state))
    (unless (> count 2)
      (error 'cl-ds:out-of-bounds
             :text "Can't calculate error for linear regression because range does not contain at least 3 element."
             :value count
             :bounds (list 3 :infinity)))
    `(,@regres ,(/ sum-of-error-squares (- count 2)))))
