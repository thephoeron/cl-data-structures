(in-package #:cl-data-structures.algorithms)


(defclass count-elements-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric count-elements (range)
  (:generic-function-class count-elements-function)
  (:method (range)
    (apply-aggregation-function #'count-elements range)))


(defmethod cl-ds.alg.meta:make-state ((function count-elements-function) &rest all)
  (list 0))


(defmethod aggregate ((function count-elements-function) state element)
  (incf (car state))
  state)


(defmethod state-result ((function count-elements-function) state)
  (car state))


