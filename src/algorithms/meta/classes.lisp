(cl:in-package #:cl-data-structures.algorithms.meta)


(defclass range-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))


(defclass layer-function (range-function)
  ()
  (:metaclass funcallable-standard-class))


(defclass transformation!-function (range-function)
  ()
  (:metaclass funcallable-standard-class))


(defclass aggregation-function (range-function)
  ()
  (:metaclass funcallable-standard-class))
