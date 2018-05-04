(in-package #:cl-data-structures.algorithms)


(defclass to-vector-function (cl-ds.alg.meta:aggregation-function)
  ()
  (:metaclass c2mop:funcallable-standard-class))


(defgeneric to-vector (range &key key element-type)
  (:generic-function-class to-vector-function)
  (:method (range &key (key #'identity) (element-type t))
    (apply-aggregation-function range #'to-vector
                                :key key
                                :element-type element-type)))


(defmethod make-state ((function to-vector-function) &key element-type &allow-other-keys)
  (make-array 16 :adjustable t :fill-pointer 0 :element-type element-type))


(defmethod state-result ((function to-vector-function) state)
  (adjust-array state (fill-pointer state)))


(defmethod aggregate ((function to-vector-function) state element)
  (vector-push-extend element state))
